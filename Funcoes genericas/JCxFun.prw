/*----------------------------------------------------------------------*\
 * JCxFun for AdvPL
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
#define DEBUG    .T.

// Configuração do servidor webservice Jotacom.
Static lSimulacao   := .F.
Static cURLServ     := ""
Static nJCTimeOut   := 0
Static aHeadStr     := {}
Static cJCApiKey    := ""
Static cJCApiTok    := ""
Static cPathSrv     := "\Jotacom\"

// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Materiais
// Função   : JCGet
// Descrição: Envia um GET para o webservice Jotacom.
// Retorno  : Retorno do webservice.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 15/05/16 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function JCGet(cResource, cGetParms, cDest, cStsCode, cHeadRet, aRetFile)
Return JCCon("GET", cResource, cGetParms, nil, cDest, @cStsCode, cHeadRet, aRetFile)


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Materiais
// Função   : JCPost
// Descrição: Envia um POST para o webservice Jotacom.
// Retorno  : Retorno do webservice.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 15/05/16 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function JCPost(cResource, cGetParms, cPost, cDest, cStsCode, cHeadRet, aRetFile)
Return JCCon("POST", cResource, cGetParms, cPost, cDest, @cStsCode, cHeadRet, aRetFile)


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Materiais
// Função   : JCCon
// Descrição: Envia um GET ou POST Json para o webservice Jotacom.
// Retorno  : Retorno do webservice.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 15/05/16 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
Static Function JCCon(cOpc, cResource, cGetParms, cPost, cDest, cStsCode, cHeadRet, aRetFile)

Local cRet       := ""
Local lGravaArq  := DEBUG
Local cFileRec   := ""

Local bValHttp
Local nRetry     := 0
Local nSeconds   := 0
Local dDate      := dDataBase
Local nStart     := 0
Local cHash      := StrTran(Time(), ":", "") + "_" + CriaTrab(nil, .F.)
Local cStsVer    := ""

// Limpa variáveis de retorno do POST.
cStsCode := ""
cHeadRet := ""

// Define se os arquivos de envio e retorno serão gravados no servidor.
If !lSimulacao .and. (ValType(aRetFile) == "A" .or. cOpc = "G")
	lGravaArq := .T.
Endif
If lGravaArq
	aRetFile  := {"", "", "", ""}
Endif

// Alimenta as variáveis estáticas.
If empty(cURLServ)
	cURLServ      := SuperGetMV("JC_URLWS",,   "www.protcap.com.br")        // URL Jotacom.
	nJCTimeOut    := SuperGetMV("JC_WAITIME",, 10)                          // Time-out.
	cJCApiKey     := SuperGetMV("JC_KEYWS",,   "API-User: protcap")         // API Key.
	cJCApiTok     := SuperGetMV("JC_TOKENWS",, "API-Pass: protheusAccess")  // API Token.
	aAdd(aHeadStr, 'Content-Type:application/json; charset=utf-8')
	aAdd(aHeadStr, cJCApiKey)
	aAdd(aHeadStr, cJCApiTok)
Endif

// Trata a pasta onde serão gravados o Json e o retorno do webservice.
If lGravaArq
	Default cDest := "\" + ProcName(1)
	cDest := StrTran(AllTrim(cDest), "/", "\")
	cDest += "\" + dtos(date())
	FWMakeDir(cPathSrv + cDest)
Endif

// Se for teste, não faz integração com nenhum servidor.
If lSimulacao
	// Simula um POST realizado com sucesso, com retorno SUCCESS.
	cHeadRet := "HTTP/1.1 200 OK" + CRLF + "Server: simulação Protheus" + CRLF + "IP: " + RTrim(GetServerIP())
	cRet     := "SUCCESS"
Else
	// Tenta enviar Json 3 vezes.
	dDate  := Date()
	nStart := Seconds()
	If cOpc = "G"
		bValHttp := {|| HttpGet(cURLServ + cResource, cGetParms, nJCTimeOut, aHeadStr, @cHeadRet)}
	Else
		bValHttp := {|| HttpPost(cURLServ + cResource, cGetParms, cPost, nJCTimeOut, aHeadStr, @cHeadRet)}
	Endif
	Do While nRetry < 3 .and. empty(cHeadRet)
		cRet := Eval(bValHttp)
		nRetry ++
	EndDo

	// Calcula quanto tempo levou para recuperar o retorno do webservice.
	nSeconds := ((Date() - dDate) * 86400) + Seconds() - nStart
Endif

// Grava o Json de envio.
If lGravaArq
	aRetFile[1] := cPathSrv + cDest + "\" + cHash + "_PostEnv_Json.log"
	cFileRec := "resource: " + cURLServ + cResource + CRLF + "Parametros: " + cGetParms + CRLF
	If DEBUG
		cFileRec += "Header:"
		aEval(aHeadStr, {|cHeader| cFileRec += CRLF + cHeader})
	Endif
	MemoWrite(aRetFile[1], cFileRec)
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
