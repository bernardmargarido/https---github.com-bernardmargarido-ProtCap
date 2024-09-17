#INCLUDE "PROTHEUS.CH"

#DEFINE		XNAME		01
#DEFINE		XTYPE 		02
#DEFINE		XLENGTH		03
#DEFINE		XDECIMAL	04
#DEFINE		XPICTURE	05
//-------------------------------------------------------------------
/*/{Protheus.doc} TREPXGEN
Funcao Generica para Compilacao

@author  Guilherme Santos
@since   08/11/2018
@version 12.1.17
/*/
//-------------------------------------------------------------------
User Function TREPXGEN()
Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} uTRepGEN
Classe Generica para Impressao de Relatorios em TReport

@author  Guilherme Santos
@since   08/11/2018
@version 12.1.17
/*/
//-------------------------------------------------------------------
Class uTRepGEN
	Data cQuery
	Data cTabRep
	Data cTitle
	Data oReport
	Data oSection
	Data aFields		//01-Nome, 02-Tipo, 03-Tamanho, 04-Decimal, 05-Picture
	
	Method New(cQuery, cTitle, cFunction, cDescr, aFields) Constructor
	Method Dialog()
	Method Print()
	Method SetFields()
EndClass
//-------------------------------------------------------------------
/*/{Protheus.doc} New
Construtor do Objeto

@author  Guilherme Santos
@since   08/11/2018
@version 12.1.17
/*/
//-------------------------------------------------------------------
Method New(cQuery, cTitle, cFunction, cDescr, aFields) Class uTRepGEN
	Default aFields	:= {}

	::cQuery 	:= cQuery
	::cTabRep	:= ""
	::cTitle	:= cTitle
	::oReport 	:= TReport():New(cFunction, cTitle, "", {|| Self:Print()}, cDescr)
	::oSection	:= NIL
	::aFields	:= aFields
Return Self
//-------------------------------------------------------------------
/*/{Protheus.doc} Dialog
Dialog de Impressao do TReport

@author  Guilherme Santos
@since   08/11/2018
@version 12.1.17
/*/
//-------------------------------------------------------------------
Method Dialog() Class uTRepGEN
	::oReport:PrintDialog()
Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} Print
Impressao do Relatorio

@author  Guilherme Santos
@since   08/11/2018
@version 12.1.17
/*/
//-------------------------------------------------------------------
Method Print() Class uTRepGEN
	Local aArea		:= GetArea()
	Local lRetorno 	:= .T.

	::cTabRep	:= GetNextAlias()
	::oSection	:= TRSection():New(::oReport, ::cTitle, {::cTabRep})

	DbUseArea(.T., "TOPCONN", TcGenQry(NIL, NIL, ::cQuery), ::cTabRep, .T., .T.)

	If (::cTabRep)->(Eof())
		lRetorno := .F.
		Aviso("TREPXGEN", "Sem dados para impressão...", {"Fechar"})
	Else
		//Preparacao dos Campos para Impressao
		::SetFields()

		//Impressao do Relatorio
		::oSection:Print()
	EndIf

	If Select(::cTabRep) > 0
		(::cTabRep)->(DbCloseArea())
	EndIf

	RestArea(aArea)

Return lRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} SetFields
Preparacao dos Campos para Impressao

@author  Guilherme Santos
@since   08/11/2018
@version 12.1.17
/*/
//-------------------------------------------------------------------
Method SetFields() Class uTRepGEN
	Local nField	:= 0
	Local nPosFld	:= 0
	Local cCampo	:= ""

	For nField := 1 to (::cTabRep)->(FCount())
		nPosFld := Ascan(::aFields, {|x| Upper(AllTrim(x[XNAME])) == Upper(AllTrim((::cTabRep)->(FieldName(nField))))})

		If nPosFld > 0
			If ::aFields[nPosFld][XTYPE] $ "|D|N|"
				TCSetField(::cTabRep, (::cTabRep)->(FieldName(nField)), ::aFields[nPosFld][XTYPE], ::aFields[nPosFld][XLENGTH], ::aFields[nPosFld][XDECIMAL])
			EndIf

			TRCell():New(::oSection, (::cTabRep)->(FieldName(nField)), ::cTabRep, (::cTabRep)->(FieldName(nField)), ::aFields[nPosFld][XPICTURE], ::aFields[nPosFld][XLENGTH] + ::aFields[nPosFld][XDECIMAL],/*lPixel*/,/*{|| code-block de impressao }*/,/*cAlign*/,/*lLineBreak*/,/*cHeaderAlign*/,/*lCellBreak*/,/*nColSpace*/,/*lAutoSize*/,/*nClrBack*/,/*nClrFore*/, .T. /*lBold*/)
		Else
			cCampo := (::cTabRep)->(FieldName(nField))

			If !Empty(GetSx3Cache(cCampo, "X3_CAMPO"))
				//Ajusta o formato do Campo da Query
				If GetSX3Cache(cCampo, "X3_TIPO") $  "|D|N|"
					TCSetField(::cTabRep, cCampo, GetSX3Cache(cCampo, "X3_TIPO"), GetSX3Cache(cCampo, "X3_TAMANHO"), GetSX3Cache(cCampo, "X3_DECIMAL"))
				EndIf

				TRCell():New(::oSection, cCampo, ::cTabRep, GetSX3Cache(cCampo, "X3_TITULO"), GetSX3Cache(cCampo, "X3_PICTURE"), GetSX3Cache(cCampo, "X3_TAMANHO"),/*lPixel*/,/*{|| code-block de impressao }*/,/*cAlign*/,/*lLineBreak*/,/*cHeaderAlign*/,/*lCellBreak*/,/*nColSpace*/,/*lAutoSize*/,/*nClrBack*/,/*nClrFore*/, .T. /*lBold*/)
			Else
				TRCell():New(::oSection, (::cTabRep)->(FieldName(nField)), ::cTabRep, (::cTabRep)->(FieldName(nField)), /*nTamanho*/, /*nDecimal*/, /*lPixel*/,/*{|| code-block de impressao }*/,/*cAlign*/,/*lLineBreak*/,/*cHeaderAlign*/,/*lCellBreak*/,/*nColSpace*/,/*lAutoSize*/,/*nClrBack*/,/*nClrFore*/, .T. /*lBold*/)
			EndIf
		EndIf
	Next nField

Return NIL
