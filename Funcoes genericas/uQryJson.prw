#INCLUDE "PROTHEUS.CH"
#INCLUDE "MSOBJECT.CH"

User Function uQryJson()
Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} uQryJson
Classe para Converter o Resultado de uma Query em Json

@author  Guilherme Santos
@since   02/04/2024
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Class uQryJson
	Data cTabQry
	Data cQuery
	Data cJson
	Data aFields
	Data aResults

	Method New()
	Method SetQuery(cQuery)
	Method SetJson(cCpoAct)
	Method GetJson()
	Method GetFldName()
EndClass
//-------------------------------------------------------------------
/*/{Protheus.doc} New
Construtor do Objeto

@author  Guilherme Santos
@since   02/04/2024
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method New() Class uQryJson
	::cTabQry	:= ""
	::cQuery	:= ""
	::cJson		:= ""
	::aFields	:= {}
	::aResults	:= {}
Return Self
//-------------------------------------------------------------------
/*/{Protheus.doc} SetQuery
Metodo para atribuicao da Query que sera executada

@author  Guilherme Santos
@since   02/04/2024
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method SetQuery(cQuery) Class uQryJson
	::cQuery := cQuery
Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} SetJson
Metodo para Construcao do Json de Retorno

@author  Guilherme Santos
@since   02/04/2024
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method SetJson(cCpoAct) Class uQryJson
	Local lRetorno 	:= .T.
	Local nField	:= 0
	Local nLenCol	:= 0
	Local cFldName	:= ""
	Default cCpoAct	:= ""

	::cJson		:= ""
	::aResults	:= {}
	::cTabQry	:= MPSysOpenQuery(::cQuery)

	If (::cTabQry)->(Eof())
		lRetorno := .F.
	Else
		::cJson += "["

		While !(::cTabQry)->(Eof())

			nLenCol	:= (::cTabQry)->(FCount())
			::cJson += "{"

			For nField := 1 to nLenCol

				cFldName := ::GetFldName((::cTabQry)->(FieldName(nField)))

				If Empty(cFldName)
					cFldName := (::cTabQry)->(FieldName(nField))
				EndIf

				::cJson += '"' + cFldName + '": '

				//Tratamento para o Tipo do Campo
				Do Case
				Case ValType((::cTabQry)->(FieldGet(nField))) == "N"
					::cJson += AllTrim(Str((::cTabQry)->(FieldGet(nField))))
				Case ValType((::cTabQry)->(FieldGet(nField))) == "C"

					//Remove os acentos se o campo foi incluido no PB2_ACENTO
					If Upper((::cTabQry)->(FieldName(nField))) $ Upper(cCpoAct)
						::cJson += '"' + U_BzNoAcento(AllTrim((::cTabQry)->(FieldGet(nField)))) + '"'
					Else
						::cJson += '"' + AllTrim((::cTabQry)->(FieldGet(nField))) + '"'
					EndIf
				EndCase

				If nField < nLenCol
					::cJson += ","
				EndIf
			Next nField

			::cJson += "}"

			(::cTabQry)->(DbSkip())

			If !(::cTabQry)->(Eof())
				::cJson += ","
			EndIf
		End
		::cJson += "]"
	EndIf

	If Select(::cTabQry) > 0
		(::cTabQry)->(DbCloseArea())
	EndIf

Return lRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} GetJson
Metodo para Retorno do Json

@author  Guilherme Santos
@since   02/04/2024
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method GetJson() Class uQryJson
Return ::cJson
//-------------------------------------------------------------------
/*/{Protheus.doc} GetFldName
Retorna o Nome do Campo de Acordo com o Dicionario do Protheus - SX3

@author  Guilherme Santos
@since   02/04/2024
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method GetFldName(cField) Class uQryJson
	Local cRetorno	:= ""
	Local nPosName	:= AScan(::aFields, {|x| AllTrim(x[01]) == cField})

	If nPosName == 0
		cRetorno := AllTrim(GetSx3Cache(cField, "X3_TITULO"))
		cRetorno := StrTran(cRetorno, " ", "_")

		Aadd(::aFields, {cField, cRetorno})
	Else
		cRetorno := ::aFields[nPosName][02]
	EndIf

Return cRetorno
