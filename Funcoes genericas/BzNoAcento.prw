#INCLUDE 'Protheus.ch'

//-------------------------------------------------------------------
/*/{Protheus.doc} BzNoAcento
Remove acentos do texto. Usado para substituir caracteres especiais.
@author  João Leão
@since   19/12/2018
@version 12.1.17
/*/
//-------------------------------------------------------------------
user function BzNoAcento(cString)

Local cChar	:= ""
Local cVogal	:= "aeiouAEIOU"
Local cAgudo	:= "áéíóú"+"ÁÉÍÓÚ"
Local cCircu	:= "âêîôû"+"ÂÊÎÔÛ"
Local cTrema	:= "äëïöü"+"ÄËÏÖÜ"
Local cCrase	:= "àèìòù"+"ÀÈÌÒÙ"
Local cTio		:= "ãõÃÕ"
Local cCecid	:= "çÇ"
Local cMaior	:= "&lt;"
Local cMenor	:= "&gt;"
Local cEComer   := "&"
Local cApostr	:= "'"
Local cAspas	:= '"'
Local cSinMai	:= '>'
Local cSinMen	:= '<'

Local nX		:= 0
Local nY		:= 0

If ValType(cString) == "C"
	If !Empty(cString)
		For nX:= 1 To Len(cString)
			cChar:=SubStr(cString, nX, 1)
			IF cChar$cAgudo+cCircu+cTrema+cCecid+cTio+cCrase
				nY:= At(cChar,cAgudo)
				If nY > 0
					cString := StrTran(cString,cChar,SubStr(cVogal,nY,1))
				EndIf
				nY:= At(cChar,cCircu)
				If nY > 0
					cString := StrTran(cString,cChar,SubStr(cVogal,nY,1))
				EndIf
				nY:= At(cChar,cTrema)
				If nY > 0
					cString := StrTran(cString,cChar,SubStr(cVogal,nY,1))
				EndIf
				nY:= At(cChar,cCrase)
				If nY > 0
					cString := StrTran(cString,cChar,SubStr(cVogal,nY,1))
				EndIf
				nY:= At(cChar,cTio)
				If nY > 0
					cString := StrTran(cString,cChar,SubStr("aoAO",nY,1))
				EndIf
				nY:= At(cChar,cCecid)
				If nY > 0
					cString := StrTran(cString,cChar,SubStr("cC",nY,1))
				EndIf
			Endif
		Next

		If cSinMai $ cString
			cString := strTran( cString, cSinMai, "" )
		EndIf

		If cSinMen $ cString
			cString := strTran( cString, cSinMen, "" )
		EndIf

		If cMaior$ cString
			cString := strTran( cString, cMaior, "" )
		EndIf

		If cMenor$ cString
			cString := strTran( cString, cMenor, "" )
		EndIf

		If cEComer$ cString
			cString := strTran( cString, cEComer, "E" )
		EndIf

		If cApostr $ cString
			cString := StrTran(cString, cApostr, "")
		EndIf

		If cAspas $ cString
			cString := StrTran(cString, cAspas, "")
		EndIf

		cString := StrTran( cString, CRLF, " " )

		For nX:=1 To Len(cString)
			cChar:=SubStr(cString, nX, 1)
			If (Asc(cChar) < 32 .Or. Asc(cChar) > 123) .and. !(cChar $ '|/_@.')
				cString:=StrTran(cString,cChar," ")
			Endif
		Next nX

		cString := decodeUtf8(cString)
		cString := encodeUtf8(cString)
	EndIf
Else
	cString := ""
EndIf

Return cString
