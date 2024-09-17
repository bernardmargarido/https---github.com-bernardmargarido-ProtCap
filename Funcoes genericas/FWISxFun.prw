/*----------------------------------------------------------------------*\
 * FWISxFun for AdvPL
 * Copyright (C) 2015  Felipe Raposo <feliperaposo@gmail.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
\*----------------------------------------------------------------------*/

#include "protheus.ch"
#include "aarray.ch"
#include "json.ch"
#include "WISStatus.ch"
#define DEBUG    .F.

Static cPathSrv    := "\WIS\" + cFilAnt + "\"
Static cEnvServer  := upper(GetEnvServer())

// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Materiais
// Função   : WISAPost
// Descrição: Envia Json para o webservice WIS.
// Retorno  : Retorno do webservice.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 09/03/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function WISAPost(cResource, oJson, cDest, lEnvNull, bWhile)

Local aRet       := {}
Local lRet       := .T.
Local nCount     := 0
Local lGrava     := .T.
Local nX

Local cStsCode   := ""
Local cHeadRet   := ""
Local cRetPost   := ""

Local aRetFile   := {}
Local aFilesAux  := {}
Local nTamHash   := 0

// Limite de posts que serão feitos.
Default bWhile := {|nCount, cRetPost| nCount  <= 20}

// Trata a pasta onde serão gravados o Json e o retorno do webservice.
Default cDest := ProcName(1)
cDest := StrTran(StrTran(AllTrim(cDest), "/", ""), "\", "")
FwMakeDir(cPathSrv + cDest)

// Verifica se já há arquivos não processados na pasta.
aRetFile := Directory(cPathSrv + cDest + "\*_PostRet*.htm")
For nX := 1 to len(aRetFile)
	aAdd(aRet, {"", "", cPathSrv + cDest + "\" + aRetFile[nX, 1], "000"})
	nTamHash := at("_POSTRET", upper(aRetFile[nX, 1])) - 1

	aFilesAux := Directory(cPathSrv + cDest + "\" + left(aRetFile[nX, 1], nTamHash) + "_PostEnv*.*")
	If !empty(aFilesAux)
		aTail(aRet)[1] := cPathSrv + cDest + "\" + aFilesAux[1, 1]
	Endif

	aFilesAux := Directory(cPathSrv + cDest + "\" + left(aRetFile[nX, 1], nTamHash) + "_HeadRet*.*")
	If !empty(aFilesAux)
		aTail(aRet)[2] := cPathSrv + cDest + "\" + aFilesAux[1, 1]
	Endif
Next nX

// Faz quantos posts forem necessários, limitando a quantidade de posts.
Do While lRet
	aRetFile := {}
	cRetPost := U_WISPost(cResource, oJson, cDest, @cStsCode, @cHeadRet, lEnvNull, aRetFile)
	nCount++

	If cStsCode = "200"
		lRet := Eval(bWhile, nCount, cRetPost, cStsCode, cHeadRet)
		If lRet
			aAdd(aRet, aRetFile)
		Endif
	Else
		lRet   := .F.
		lGrava := DEBUG .or. !(cRetPost = "NENHUM")
		U_WISProc(aRetFile, .T., lGrava)
	Endif
EndDo

Return aRet


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Materiais
// Função   : WISProc
// Descrição: Move os arquivos para a pasta de processados.
// Retorno  : Lógico, indicando se a função foi executada com sucesso.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 09/03/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function WISProc(aFiles, lErro, lGrava)

Local lRet       := .T.
Local cProcessed := ""
Local nX

Local cFileDrv   := ""
Local cFilePath  := ""
Local cFileName  := ""
Local cFileExt   := ""

// Pasta para onde os arquivos serão movidos.
Default lErro  := .F.
Default lGrava := .T.
If lErro
	cProcessed := "Erro\"
Else
	cProcessed := "Processed\"
Endif

// Move os arquivos.
For nX := 1 to len(aFiles)
	If lGrava
		SplitPath(aFiles[nX], @cFileDrv, @cFilePath, @cFileName, @cFileExt)
		If file(cFileDrv + cFilePath + cFileName + cFileExt)
			FwMakeDir(cFileDrv + cFilePath + cProcessed)
			If fRename(cFileDrv + cFilePath + cFileName + cFileExt, cFileDrv + cFilePath + cProcessed + cFileName + cFileExt) < 0
				lRet := .F.
			Endif
		Endif
	Else
		fErase(aFiles[nX])
	Endif
Next nX

Return lRet


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Materiais
// Função   : WISPost
// Descrição: Envia Json para o webservice WIS.
// Retorno  : Retorno do webservice.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 09/03/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function WISPost(cResource, oJson, cDest, cStsCode, cHeadRet, lEnvNull, aRetFile, lGravaArq)

Local cRet       := ""
Local cJson      := ""
Local nRetry     := 0
Local nSeconds   := 0
Local dDate      := dDataBase
Local nStart     := 0
Local cHash      := StrTran(Time(), ":", "") + "_" + CriaTrab(nil, .F.)
Local cStsVer    := ""

Default lGravaArq  := SuperGetMV("WIS_DEBUG", NIL, .F.)

// Limpa variáveis de retorno do POST.
cStsCode := ""
cHeadRet := ""

//Inicializacao das variaveis
lSimulacao := U_WISSimu()

If lGravaArq
	aRetFile  := {"", "", "", ""}
Endif

// Trata a pasta onde serão gravados o Json e o retorno do webservice.
If lGravaArq
	Default cDest := ProcName(1)
	cDest := StrTran(StrTran(AllTrim(cDest), "/", ""), "\", "")
	FwMakeDir(cPathSrv + cDest)
	If !IsInCallStack("U_WISAPost")
		cDest += "\" + dtos(date()); FwMakeDir(cPathSrv + cDest)
	Endif
Endif

// Tratamento de usuário e senha.
If oJson[#"usuario"] == "%cUser%"
	oJson[#"usuario"] := cWisUser
	oJson[#"senha"]   := cWisPass
Endif

// Transforma o objeto em string Json.
cJson := ToJson(oJson, lEnvNull)

// Se for teste, não faz integração com nenhum servidor.
If lSimulacao
	// Simula um POST realizado com sucesso, com retorno SUCCESS.
	cHeadRet := "HTTP/1.1 200 OK" + CRLF + "Server: simulação Protheus" + CRLF + "IP: " + RTrim(GetServerIP())
	cRet     := "SUCCESS"
Else
	// Tenta enviar Json 3 vezes.
	dDate  := Date()
	nStart := Seconds()
	Do While nRetry < 3 .and. empty(cHeadRet)
		cRet := HttpPost(cWisURL + cResource, "", cJson, nWisTimeOut, aHeadOut, @cHeadRet)
		nRetry ++
	EndDo

	// Calcula quanto tempo levou para recuperar o retorno do webservice.
	nSeconds := ((Date() - dDate) * 86400) + Seconds() - nStart
Endif

// Grava o Json de envio.
If lGravaArq
	aRetFile[1] := cPathSrv + cDest + "\" + cHash + "_PostEnv_Json.log"
	MemoWrite(aRetFile[1], "resource: " + cWisURL + cResource + CRLF + cJson)
Endif

// Verifica se o POST foi realizado com sucesso.
If ValType(cHeadRet) == "C"
	cRet     := DecodeUTF8(AllTrim(cRet))
	cStsCode := left(cHeadRet, at(chr(13), cHeadRet) - 1)
	cStsVer  := left(cStsCode, at(" ", cHeadRet) - 1)
	cStsCode := SubStr(cStsCode, at(" ", cHeadRet) + 1)

	// Grava o resultado da busca no servidor.
	If lGravaArq
		aRetFile[2] := cPathSrv + cDest + "\" + cHash + "_HeadRet_" + cValToChar(nSeconds) + "s.log"
		MemoWrite(aRetFile[2], cHeadRet)
		If ValType(cRet) == "C"
			aRetFile[3] := cPathSrv + cDest + "\" + cHash + "_PostRet_" + cValToChar(nSeconds) + "s.htm"
			MemoWrite(aRetFile[3], cRet)
		Endif
		aRetFile[4] := cStsCode
	Endif
Else
	cHeadRet := ""
Endif

If ValType(cRet) <> "C"
	cRet := ""
Endif

Return cRet


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Materiais
// Função   : WISStru
// Descrição: Explode a estrutura do produto a ser enviado para o WIS.
// Retorno  : Estrutura.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 20/05/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function WISStru(cProduto, nQtde, aEstrutura)

Local aEstrutAux := {}
Local nX

// Variável privada necessária para função Estrut().
Private nEstru     := 0

// Explode o primeiro nível da estrutura.
// Returno -> {{nil, nil, PRODUTO, QTDE}}
aEstrutAux := Estrut(cProduto, nQtde, .T. /* Somente primeiro nível */, .F. /* Explode pré-estrutura */)

// Verifica se o produto tem sub-níveis.
Default aEstrutura := {}
For nX := 1 to len(aEstrutAux)
	// Se produto tem montagem automática, explode a sua estrutura também.
	SB1->(dbSetOrder(1))  // B1_FILIAL, B1_COD.
	SB1->(dbSeek(xFilial() + aEstrutAux[nX, 3], .F.))
	If SB1->B1_XGEROP == "S"
		U_WISStru(aEstrutAux[nX, 3], aEstrutAux[nX, 4], aEstrutura)
	Else
		aAdd(aEstrutura, {aEstrutAux[nX, 3], aEstrutAux[nX, 4], 0, 0})
	Endif
Next nX

Return aEstrutura


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Materiais
// Função   : WISSimu
// Descrição: Explode a estrutura do produto a ser enviado para o WIS.
// Retorno  : Estrutura.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 20/05/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function WISSimu()
	Static cWisURL
	Static lSimulacao  := .F.
	Static cWisUser    := ""
	Static cWisPass    := ""
	Static nWisTimeOut := 0
	Static aHeadOut    := {}

	// Configuração do servidor webservice WIS.
	cWisURL      := SuperGetMV("WIS_URL",, "")     // URL.
	cWisUser     := SuperGetMV("WIS_USR",, "")     // Usuário.
	cWisPass     := SuperGetMV("WIS_PSW",, "")     // Senha.
	nWisTimeOut  := SuperGetMV("WIS_TO",,  10)     // Time-out.

	If Empty(aHeadOut)
		aAdd(aHeadOut, 'Content-Type: application/json; charset=utf-8')
	EndIf

	// Se não for servidor de produção, faz alguns tratamentos a parte.
	If !U_Producao()
		If !("hml" $ cWisURL .or. "tst" $ cWisURL .or. "https://protcaphml.seniorcloud.com.br/interfacewis" $ cWisURL .or. "https://cdepihml.seniorcloud.com.br/interfacewis" $ cWisURL)
			// Não permite que outros ambientes utilizem o servidor 192.168.2.144 (WIS produção).
			lSimulacao := .T.
			cWisURL    := ""
		Endif
	Endif
Return lSimulacao
