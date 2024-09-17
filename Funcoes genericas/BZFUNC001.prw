#INCLUDE "PROTHEUS.CH"
#INCLUDE "AARRAY.CH"
#INCLUDE "JSON.CH"
/***********************************************************************************/
/*/{Protheus.doc} BZFUNC001
@description Funcao que monta o JSON de alteração de cadastro e grava na tabela muro
Z12. 
Chamado por PE de alteração dos cadastros que integram com a ASTREIN SSACAD.
Obs: Futuramente mudar para campos fixos no parametro, transformar e array e utilizar
a funcao SX3 para tratar o tipo do campo
@type  Function
@author Michihiko Tanimoto
@since 15/12/2020
/*/
/***********************************************************************************/
User Function BZFUNC001(_cEndPoint,_cAlias,_nRecno)
	Local _aArea    := GetArea()
	Local _cCodEmp  := GetNewPar("BZ_EMPASTR","BEPI")
	Local _aaArray  := {}
	Local _cJson    := ""
	Local _cError   := ""
	Local _cStatus  := ""  //Nao integrado 2 = Integrado com Sucesso e 3 = erro de integração
	Local _cChave   := ""
	Local _nX
	Local _cIdProc
	Local _aCposAstren
	Local _cTypeSX3
	Local _cAux		:= ""

	dbSelectArea("Z22")
	Z22->(dbSetOrder(1))    //Z22_FILIAL+Z22_EPOINT+Z22_ALIAS
	Z22->(dbSeek(xFilial("Z22")+PADR( UPPER( _cEndPoint ) ,30 ) + _cAlias)) //ENVIOATUALIZARITEMLOTE
	While Z22->Z22_EPOINT == PADR( UPPER( _cEndPoint ) ,30 ) .AND. Z22->Z22_ALIAS == _cAlias
		If _cAlias=="SB1"  //Materiais
			_aCposAstren := StrTokArr2( Alltrim(Z22_CAMPOS),"," )
			dbSelectArea(_cAlias)
			(_cAlias)->(dbGoto(_nRecno))              //Posiciono no registro que sera enviado

			_aaArray := Array(#)     // Inicia um Vetor Associativo
			// Define valores do cabecalho
			_aaArray[#"empresa"]  := _cCodEmp
			_aaArray[#"filial"]   := ""
			_aaArray[#"codigoERP"]:= (_cAlias)->(B1_COD)
			_aaArray[#"idSSACAD"] := Iif( FieldPos("B1_XIDASTR") > 0,(_cAlias)->(B1_XIDASTR),"")      //So existe o campo XIDASTR na tabela da empresa 01
			_aaArray[#"tipoPD"]   := "Material"
			// Inicia outro Vetor Associativo das caracteristicas do produto (campos)
			_aaArray[#"valores"]  := Array(Len(_aCposAstren))
			For _nX := 1 To Len(_aCposAstren)
				// Inicia outro Vetor Associativo das caracteristicas do produto (campos)
				_aaArray[#"valores"][_nX] := Array(#)
				_cTypeSX3 := FWSX3Util():GetFieldType(  _aCposAstren[_nX] ) //Retorna o tipo do campo cType - Caracter - Tipo do campo no SX3 (C,L,D,M,N)
				If  FieldPos(_aCposAstren[_nX])
					Do Case 
						Case _cTypeSX3 == "D"
						_aaArray[#"valores"][_nX][#"CARACTERISTICA"]   := _aCposAstren[_nX]
						_aaArray[#"valores"][_nX][#"CONTEUDO"]         := dToC(SB1->&(_aCposAstren[_nX]))
						Case _cTypeSX3 == "N"
						_aaArray[#"valores"][_nX][#"CARACTERISTICA"]   := _aCposAstren[_nX]
						_aaArray[#"valores"][_nX][#"CONTEUDO"]         := cValToChar(SB1->&(_aCposAstren[_nX]))
						Case _cTypeSX3 == "L"
						_aaArray[#"valores"][_nX][#"CARACTERISTICA"]   := _aCposAstren[_nX]
						_aaArray[#"valores"][_nX][#"CONTEUDO"]         := IIF(SB1->&(_aCposAstren[_nX]), ".T.", ".F.")
						OtherWise
						_aaArray[#"valores"][_nX][#"CARACTERISTICA"]   := _aCposAstren[_nX]
						_aaArray[#"valores"][_nX][#"CONTEUDO"]         := SB1->&(_aCposAstren[_nX])
					EndCase
				EndIf
			Next _nX

			//Transforma o array em JSON
			_cJson := "[" + ToJson(_aaArray)+ "]"

			_cIdProc:= "0013"   //Alteracao de produto no ERP para envio a Astrein
			_cError := ""
			_cStatus:= "1"  //Nao integrado 2 = Integrado com Sucesso e 3 = erro de integração
			_cChave := "SB1"+SB1->(B1_FILIAL+B1_COD)    //Defini como chave o Alias e o conteudo dos campos de indice
			//Funcão para gravar nova integração na tabela Z12  
			If (BZFUNC001_ATUZ12(_cIdProc,_cChave,_cJson,_cError,_cStatus,3))
				(_cAlias)->(dbGoto(_nRecno))              //Posiciono no registro que sera enviado
				RecLock(_cAlias,.F.)
				(_cAlias)->B1_MSEXP := DTOS(ddatabase)       //Atualiza como ja integrado
				MsUnLock()
			EndIf
		ElseIf _cAlias=="SA1"
			_aCposAstren := StrTokArr2( Alltrim(Z22_CAMPOS),"," )
			dbSelectArea(_cAlias)
			(_cAlias)->(dbGoto(_nRecno))              //Posiciono no registro que sera enviado

			_aaArray := Array(#)     // Inicia um Vetor Associativo
			// Define valores do cabecalho
			_aaArray[#"empresa"]  := _cCodEmp
			_aaArray[#"filial"]   := ""
			_aaArray[#"codigoERP"]:= (_cAlias)->(A1_COD+A1_LOJA)
			_aaArray[#"idSSACAD"] := Iif( FieldPos("A1_XIDASTR") > 0,(_cAlias)->(A1_XIDASTR),"")      //So existe o campo XIDASTR na tabela da empresa 01
			_aaArray[#"tipoPD"]   := "Cliente"
			// Inicia outro Vetor Associativo das caracteristicas do produto (campos)
			_aaArray[#"valores"]  := Array(Len(_aCposAstren))
			For _nX := 1 To Len(_aCposAstren)
				// Inicia outro Vetor Associativo das caracteristicas do produto (campos)
				_aaArray[#"valores"][_nX] := Array(#)
				_cTypeSX3 := FWSX3Util():GetFieldType(  _aCposAstren[_nX] ) //Retorna o tipo do campo cType - Caracter - Tipo do campo no SX3 (C,L,D,M,N)
				If  FieldPos(_aCposAstren[_nX])
					Do Case 
						Case _cTypeSX3 == "D"
						_aaArray[#"valores"][_nX][#"CARACTERISTICA"]   := _aCposAstren[_nX]
						_aaArray[#"valores"][_nX][#"CONTEUDO"]         := dToC((_cAlias)->&(_aCposAstren[_nX]))
						Case _cTypeSX3 == "N"
						_aaArray[#"valores"][_nX][#"CARACTERISTICA"]   := _aCposAstren[_nX]
						_aaArray[#"valores"][_nX][#"CONTEUDO"]         := cValToChar((_cAlias)->&(_aCposAstren[_nX]))
						Case _cTypeSX3 == "L"
						_aaArray[#"valores"][_nX][#"CARACTERISTICA"]   := _aCposAstren[_nX]
						_aaArray[#"valores"][_nX][#"CONTEUDO"]         := IIF((_cAlias)->&(_aCposAstren[_nX]), ".T.", ".F.")
						OtherWise
						_aaArray[#"valores"][_nX][#"CARACTERISTICA"]   := _aCposAstren[_nX]
						_aaArray[#"valores"][_nX][#"CONTEUDO"]         := IIF(_aCposAstrein[_nX]=='A1_COD',(_cAlias)->(A1_COD+A1_LOJA),(_cAlias)->&(_aCposAstren[_nX]))
					EndCase
				EndIf
			Next _nX

			//Transforma o array em JSON
			_cJson := "[" + ToJson(_aaArray)+ "]"

			_cIdProc:= "0014"   //Alteracao de produto no ERP para envio a Astrein
			_cError := ""
			_cStatus:= "1"  //Nao integrado 2 = Integrado com Sucesso e 3 = erro de integração
			_cChave := _cAlias+(_cAlias)->(A1_FILIAL+A1_COD+A1_LOJA)    //Defini como chave o Alias e o conteudo dos campos de indice
			If (BZFUNC001_ATUZ12(_cIdProc,_cChave,_cJson,_cError,_cStatus,3))
				(_cAlias)->(dbGoto(_nRecno))              //Posiciono no registro que sera enviado
				RecLock(_cAlias,.F.)
				(_cAlias)->A1_MSEXP := Dtos(ddatabase)        //Atualiza como ja integrado
				MsUnLock()
			EndIf
		ElseIf _cAlias=="SA2"
			_aCposAstren := StrTokArr2( Alltrim(Z22_CAMPOS),"," )
			dbSelectArea("SA2")
			SA2->(dbGoto(_nRecno))              //Posiciono no registro que sera enviado

			_aaArray := Array(#)     // Inicia um Vetor Associativo
			// Define valores do cabecalho
			_aaArray[#"empresa"]  := _cCodEmp
			_aaArray[#"filial"]   := ""
			_aaArray[#"codigoERP"]:= SA2->(A2_COD)
			_aaArray[#"idSSACAD"] := Iif( FieldPos("A2_XIDASTR") > 0,SA2->(A2_XIDASTR),"")      //So existe o campo XIDASTR na tabela da empresa 01
			_aaArray[#"tipoPD"]   := "Fornecedor"
			// Inicia outro Vetor Associativo das caracteristicas do produto (campos)
			_aaArray[#"valores"]  := Array(Len(_aCposAstren))
			For _nX := 1 To Len(_aCposAstren)
				// Inicia outro Vetor Associativo das caracteristicas do produto (campos)
				_aaArray[#"valores"][_nX] := Array(#)
				_cTypeSX3 := FWSX3Util():GetFieldType(  _aCposAstren[_nX] ) //Retorna o tipo do campo cType - Caracter - Tipo do campo no SX3 (C,L,D,M,N)
				If  FieldPos(_aCposAstren[_nX])
					Do Case 
						Case _cTypeSX3 == "D"
						_aaArray[#"valores"][_nX][#"CARACTERISTICA"]   := _aCposAstren[_nX]
						_aaArray[#"valores"][_nX][#"CONTEUDO"]         := dToC(SA2->&(_aCposAstren[_nX]))
						Case _cTypeSX3 == "N"
						_aaArray[#"valores"][_nX][#"CARACTERISTICA"]   := _aCposAstren[_nX]
						_aaArray[#"valores"][_nX][#"CONTEUDO"]         := cValToChar(SA2->&(_aCposAstren[_nX]))
						Case _cTypeSX3 == "L"
						_aaArray[#"valores"][_nX][#"CARACTERISTICA"]   := _aCposAstren[_nX]
						_aaArray[#"valores"][_nX][#"CONTEUDO"]         := IIF(SA2->&(_aCposAstren[_nX]), ".T.", ".F.")
						OtherWise
						_aaArray[#"valores"][_nX][#"CARACTERISTICA"]   := _aCposAstren[_nX]
						_aaArray[#"valores"][_nX][#"CONTEUDO"]         := SA2->&(_aCposAstren[_nX])
					EndCase
				EndIf
			Next _nX

			//Transforma o array em JSON
			_cJson := "[" + ToJson(_aaArray)+ "]"

			_cIdProc:= "0015"   //Alteracao de produto no ERP para envio a Astrein
			_cError := ""
			_cStatus:= "1"  //Nao integrado 2 = Integrado com Sucesso e 3 = erro de integração
			_cChave := "SA2"+SA2->(A2_FILIAL+A2_COD+A2_LOJA)    //Defini como chave o Alias e o conteudo dos campos de indice
			If (BZFUNC001_ATUZ12(_cIdProc,_cChave,_cJson,_cError,_cStatus,3))
				(_cAlias)->(dbGoto(_nRecno))              //Posiciono no registro que sera enviado
				RecLock(_cAlias,.F.)
				(_cAlias)->A2_MSEXP := Dtos(ddatabase)        //Atualiza como ja integrado
				MsUnLock()
			EndIf
		Else
			_cAux := _cAlias
			If _cAlias $ "X5*X6"
				_cAlias := "SX5"
			EndIf

			dbSelectArea(_cAlias)
			&(_cAlias)->(dbGoto(_nRecno))              //Posiciono no registro que sera enviado

			_aaArray := Array(#)     // Inicia um Vetor Associativo
			// Define valores do cabecalho
			_aaArray[#"divisao"]        := "1"
			_aaArray[#"empresa"]        := _cCodEmp
			_aaArray[#"tipoPD"]         := Alltrim(Z22->Z22_CAMPOS)
			_aaArray[#"caracteristica"] := Z22->Z22_CARACT      //So existe o campo XIDASTR na tabela da empresa 01
			_aaArray[#"conteudoLongo"]  := Rtrim((_cAlias)->&(Z22->Z22_DESLON))+":"+Rtrim(_cCodEmp)
			_aaArray[#"conteudoCurto"]  := Rtrim((_cAlias)->&(Z22->Z22_DESCUR))+":"+Rtrim(_cCodEmp)
			_aaArray[#"bloquear"]       := "N"

			//Transforma o array em JSON
			_cJson := ToJson(_aaArray)

			_cIdProc:= "0016"   //Alteracao de produto no ERP para envio a Astrein
			_cError := ""
			_cStatus:= "1"  //|1=Nao integrado|2=Integrado com Sucesso|3=erro de integração|
			If Alltrim(Z22->Z22_CAMPOS) == "Cliente PJ"
				_cAux := "PJ" + _cAux
			ElseIf Alltrim(Z22->Z22_CAMPOS) == "Cliente PF"
				_cAux := "PF" + _cAux
			EndIf
			
			_cChave := _cAux + (_cAlias)->(&(IIF(Substr(_cAlias,1,1)=="S",Substr(_cAlias,2,2),_cAlias)+"_FILIAL")+&(Z22->Z22_DESCUR))    //Define como chave o Alias e o conteudo dos campos de indice

			BZFUNC001_ATUZ12(_cIdProc,_cChave,_cJson,_cError,_cStatus,3)
		EndIf
		Z22->(dbSkip())
	End
	RestArea(_aArea)

Return(Nil)
/***********************************************************************************/
/*/{Protheus.doc} BZFUNC001_ATUZ12
@description Grava integração na tabela muro Z12 para ser processada pelo BZAPI002 (JOB)
@type  Static Function
@author Michihiko Tanimoto
@since 16/11/2020
/*/
/***********************************************************************************/
Static Function BZFUNC001_ATUZ12(_cIdProc,_cChave,_cJson,_cError,_cStatus,_nOpc)
	Local _aArea    := GetArea()
	Local _lRet     := .T.
	Local _oMonitor := ProtMonitor():New()
	Default _nOpc   := 3

	_oMonitor:cIdProc   := _cIdProc
	_oMonitor:cChave    := _cChave
	_oMonitor:cStatus   := _cStatus
	_oMonitor:cJSon     := _cJson
	_oMonitor:nQtdReg   := 1
	_oMonitor:nOpc      := _nOpc
	_oMonitor:cFunName  := ProcName(2)

	If _oMonitor:GrvMonitor()
		_lRet := .T.
	Else
		_lRet := .F.
	EndIf

	RestArea(_aArea)
Return _lRet 
