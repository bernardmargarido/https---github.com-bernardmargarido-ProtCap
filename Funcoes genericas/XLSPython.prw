#INCLUDE "PROTHEUS.CH"
#INCLUDE "MSOBJECT.CH"
#INCLUDE "AARRAY.CH"
#INCLUDE "JSON.CH"

User Function XLSPython()
Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} XLSPython
Classe para geracao de Relatorios em Excel via API Python

@author  Guilherme Santos
@since   02/04/2024
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Class XLSPython
	Data cJson
	Data cURLPython
	Data cURLDown
	Data cMsgErro

	Method New()
	Method SetJson(cJson)
	Method SendData()
	Method GetURLDown()
	Method GetErro()
EndClass
//-------------------------------------------------------------------
/*/{Protheus.doc} New
Construtor do Objeto

@author  Guilherme Santos
@since   02/04/2024
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method New() Class XLSPython
	::cJson			:= ""
	::cURLPython	:= SuperGetMV("BZ_URLXLSP", NIL, "http://172.16.0.43:5000/gera_xlsx")	//URL da API em Python
	::cURLDown		:= ""
	::cMsgErro		:= ""
Return Self
//-------------------------------------------------------------------
/*/{Protheus.doc} SetJson
Metodo para atribuicao do Json do Relatorio

@author  Guilherme Santos
@since   02/04/2024
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method SetJson(cJson) Class XLSPython
	Local nLenMax	:= 10000000
	Local nLoops 	:= 0
	Local nCont		:= 0
	Local nPosIni	:= 0

	::cJson	:= ""

	If Len(cJson) > nLenMax
		nLoops 	:= Int(Len(cJson) / nLenMax)
		nLoops 	+= If(Mod(Len(cJson), nLenMax) > 0, 1, 0)
		nPosIni := 1

		For nCont := 1 to nLoops
			::cJson += EncodeUTF8(Substr(cJson, nPosIni, nLenMax))
			nPosIni += nLenMax
		Next nCont
	Else
		::cJson := EncodeUTF8(cJson, "cp1252")
	EndIf

Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} SendData
Metodo para Consumo da API de Geracao do XLS em Python

@author  Guilherme Santos
@since   02/04/2024
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method SendData() Class XLSPython
	Local lRetorno 	:= .T.
	Local cRetorno 	:= ""
	Local cRetGet	:= ""
	Local nTimeOut	:= 120
	Local aHeader	:= {}
	Local oResult 	:= NIL

	Aadd(aHeader, 'Content-Type: application/json;charset=utf-8')

	cRetorno := HttpPost(::cURLPython, NIL, ::cJson, nTimeOut, aHeader, @cRetGet)

	If Empty(cRetorno)
		lRetorno 	:= .F.
		::cMsgErro 	:= "Erro ao conectar ao Serviço de Relatórios."
	Else
		oResult 	:= FromJson(cRetorno)		//Funcao customizada (json.prw) - traduzida pela include json.ch

		If Empty(oResult)
			lRetorno 	:= .F.
			::cMsgErro 	:= DecodeUTF8(cRetorno)
		Else
			::cURLDown 	:= oResult[#"download_link"]
		EndIf
	EndIf

Return lRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} GetURLDown
Metodo para retorno da URL de Download do Relatorio em Excel

@author  Guilherme Santos
@since   02/04/2024
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method GetURLDown() Class XLSPython
Return ::cURLDown
//-------------------------------------------------------------------
/*/{Protheus.doc} GetErro
Metodo para retorno da Mensagem de Erro

@author  Guilherme Santos
@since   03/04/2024
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method GetErro() Class XLSPython
Return ::cMsgErro
