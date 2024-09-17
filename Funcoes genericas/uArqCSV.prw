#INCLUDE "PROTHEUS.CH"
#INCLUDE "MSOBJECT.CH"
//-------------------------------------------------------------------
/*/{Protheus.doc} uArqCSV
Funcao Generica para Compilacao

@author		Guilherme Santos
@since		08/06/2018
@version	12.1.17
/*/
//-------------------------------------------------------------------
User Function uArqCSV()
Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} uArqCSV
Classe para Manipulacao de Arquivos em Formato CSV

@type 		class
@author		Guilherme Santos
@since		08/06/2018
@version	12.1.17
/*/
//-------------------------------------------------------------------
Class uArqCSV From uArqTxt
	/*
	-------------------------------------------------------------------
		Propriedades Recebidas por Heranca da Class uArqTxt
	-------------------------------------------------------------------
	Data aDados
	Data cArquivo
	Data cNomeArq
	Data cPath
	Data nHandle
	-------------------------------------------------------------------
	*/
	Data aHeader				//Cabecalho
	Data aDetails				//Detalhes dos Registros
	Data aTrailer				//Rodape

	Data cIdHeader				//ID do Header
	Data cIdDetail				//ID do Detalhe
	Data cIdTrailer				//ID do Rodape
	/*
	-------------------------------------------------------------------
		Metodos Recebidos por Heranca da Class uArqTxt
	-------------------------------------------------------------------
	Method New(cPath, cNomeArq)
	Method Open(nModo)
	Method Close()
	Method Write(cTexto)
	Method Use()
	Method Free()
	Method GoTop()
	Method GoBottom()
	Method Skip()
	Method Bof()
	Method Eof()
	Method ReadLn()
	Method Read()
	Method Create()
	Method Exists()
	Method Erase()
	Method Rename(cNewName)
	Method GetName()
	-------------------------------------------------------------------
	*/
	Method New(cPath, cNomeArq, cIdHeader, cIdDetail, cIdTrailer)
	Method ReadLn()
	Method SetHeader()
	Method SetDetail()
	Method SetTrailer()
	Method GetPosField(cField)
	Method GetFieldValue(cField)
EndClass
//-------------------------------------------------------------------
/*/{Protheus.doc} New
Construtor da Classe uArqCSV

@author		Guilherme Santos
@since		08/06/2018
@version	12.1.17
@param		cPath, string, Caminho do Arquivo para Importacao
/*/
//-------------------------------------------------------------------
Method New(cPath, cNomeArq, cIdHeader, cIdDetail, cIdTrailer) Class uArqCSV

	_Super:New(cPath, cNomeArq)

	::aHeader 		:= {}
	::aDetails		:= {}
	::aTrailer		:= {}
	::cIdHeader		:= cIdHeader
	::cIdDetail		:= cIdDetail
	::cIdTrailer	:= cIdTrailer
Return Self
//-------------------------------------------------------------------
/*/{Protheus.doc} ReadLn
Leitura da Linha do Arquivo CSV

@author		Guilherme Santos
@since		08/06/2018
@version	12.1.17
/*/
//-------------------------------------------------------------------
Method ReadLn() Class uArqCSV
	Local cBuffer 	:= _Super:ReadLn()
	Local nRetorno	:= 0

	Do Case
	Case Substr(cBuffer, 1, Len(::cIdHeader)) == ::cIdHeader
		::SetHeader(cBuffer)
		nRetorno := 1
	Case Substr(cBuffer, 1, Len(::cIdDetail)) == ::cIdDetail
		::SetDetail(cBuffer)
		nRetorno := 2
	Case Substr(cBuffer, 1, Len(::cIdTrailer)) == ::cIdTrailer
		::SetTrailer(cBuffer)
		nRetorno := 3
	EndCase

Return nRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} SetHeader
Atribuicao dos dados do Cabecalho do Arquivo CSV

@author		Guilherme Santos
@since		08/06/2018
@version	12.1.17
/*/
//-------------------------------------------------------------------
Method SetHeader(cBuffer) Class uArqCSV
	::aHeader := StrTokArr2(cBuffer, ";", .T.)
Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} SetDetail
Atribuicao dos dados dos Detalhes do Arquivo CSV

@author		Guilherme Santos
@since		08/06/2018
@version	12.1.17
/*/
//-------------------------------------------------------------------
Method SetDetail(cBuffer) Class uArqCSV
	::aDetails := StrTokArr2(cBuffer, ";", .T.)
Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} SetTrailer
Atribuicao dos dados do Rodape do Arquivo CSV

@author		Guilherme Santos
@since		08/06/2018
@version	12.1.17
/*/
//-------------------------------------------------------------------
Method SetTrailer(cBuffer) Class uArqCSV
	::aTrailer := StrTokArr2(cBuffer, ";", .T.)
Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} GetPosField
Retorna a Posicao do Campo no Array de Detalhes

@author		Guilherme Santos
@since		08/06/2018
@version	12.1.17
/*/
//-------------------------------------------------------------------
Method GetPosField(cField)  Class uArqCSV
	Local nPosField := AScan(::aHeader, {|x| AllTrim(x) == cField})
Return nPosField
//-------------------------------------------------------------------
/*/{Protheus.doc} GetPosField
Retorna o Conteudo do Campo de Detalhe informado

@author		Guilherme Santos
@since		08/06/2018
@version	12.1.17
/*/
//-------------------------------------------------------------------
Method GetFieldValue(cField) Class uArqCSV
	Local nPosField	:= ::GetPosField(cField)
	Local cRetorno	:= If(nPosField > 0, ::aDetails[nPosField], "")
Return cRetorno
