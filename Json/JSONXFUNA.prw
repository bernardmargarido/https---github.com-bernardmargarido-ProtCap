#include 'totvs.ch'

/* 51 chars - strings extras "x" e "y" para excessoes */
Static aInd      := {"a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","0","1","2","3","4","5","6","7","8","9","A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z"}
Static nDeslocar := 1

User Function SmObjClone(oObject)
Local uRet
Local aKeys
Local nI
Local cClassname
Local cKey
Local cType

If (cType := ValType(oObject)) == "O"
	cClassname := GetClassName(oObject)
	uRet := &(cClassname+"():New()")

	aKeys := ClassDataArr(oObject)
	
	For nI := 1 to Len(aKeys)
		If Valtype(cKey := aKeys[nI, 1]) == "C"
			Eval(&("{ |oObj, uValue| oObj:"+cKey+" := uValue }"), uRet, SmObjClone(aKeys[nI, 2]))
		EndIf
	Next
ElseIf cType == "A"
	uRet := {}
	For nI := 1 to Len(oObject)
		aAdd(uRet, SmObjClone(oObject[nI]))
	Next
Else
	uRet := oObject
EndIf

Return uRet

User Function SmObj2JSON(oObject)

Return Jsonfy(oObject, ":", ",")

Static Function Jsonfy(oObject, cDiscrim, cComma)
Local cRet
Local bKey
Local bValue
Local aKeys
Local aValues
Local nI
Local cClassname
Local cKey
Local uValue
Local cType

If (cType := ValType(oObject)) == "O"
	cRet := "{" 
	If (cClassname := GetClassName(oObject)) == "HASHMAP"
		aKeys := oObject:aKeys
		aValues := oObject:aValues
		bKey := {|| aKeys[nI]}
		bValue := {|| aValues[nI]}
	Else
		aKeys := ClassDataArr(oObject)
		aValues := aKeys
		bKey := {|| aKeys[nI, 1]}
		bValue := {|| aValues[nI, 2]}
	Endif
	
	For nI := 1 to Len(aKeys)
		If Valtype(cKey := Eval(bKey)) == "C"
			If nI > 1
				cRet += cComma
			EndIf
			cRet += cKey+cDiscrim+SmObj2JSON(Eval(bValue))
		EndIf
	Next
	If cClassname != "HASHMAP"
		cRet += '___languageorign:"ADVPL",___classname:"'+cClassName+'"'
	EndIf
	cRet += "}"
ElseIf cType == "C"
	cRet := SmDoScape(oObject, .T.)
ElseIf cType == "A"
	If Len(oObject) == 2 ;
		.And. ValType(oObject[1]) == "D" .And. ValType(oObject[2]) == "C" ;
		.And. IsNumeric(oObject[2])
		//
		cRet := '"'+SmJSONDate(oObject)+'"'
	Else
	
		cRet := "["
		For nI := 1 to Len(oObject)
			If nI > 1
				cRet += cComma
			EndIf
			cRet += SmObj2JSON(oObject[nI])
		Next
		cRet += "]"
	EndIf
ElseIf cType == "D"
	cRet := '"'+SmJSONDate(oObject)+'"'
ElseIf cType == "U"
	cRet := "null"
ElseIf cType == "L"
	cRet := IIF(oObject, "true", "false")
Else
	cRet := cValtoChar(oObject)
EndIf

Return cRet

User Function SmJSON2Obj(cObject0, oObject, cError, lUTF8, lCaseSensitive)
Local cObject := Alltrim(cObject0)
Local bError
Default lCaseSensitive := .F.

If Asc(Left(cObject, 1)) == 239 //Ignorar byte order mark (não tratar, por enquanto)
	cObject := SubStr(cObject, 4)
EndIf

If Left(cObject, 1) $ '{["0123456789.tTfFnN'
	bError := ErrorBlock({|oError| DoError(oError, @cError) })
	cError := ""
	BEGIN SEQUENCE
		oObject := JsonParse(cObject, ":", ",",, lUTF8, lCaseSensitive)
	END SEQUENCE
	ErrorBlock(bError)
Else
	cError := "Json inválido!"
EndIf

Return Empty(cError)

Static Function DoError(oError, cError)

cError := oError:ErrorStack
break

Return

Static Function JsonParse(cObject, cDiscrim, cComma, nIni, lUTF8, lCaseSensitive)
	Static cBlank := " "+chr(13)+chr(10)+chr(9)+chr(8)
	Local nA, nI
	Local nState := 0
	Local lQuoted := .F.
	Local cName   := ""
	Local cError
	Local uValue
	Local uRet
	
	Default nIni := 1
	Default lUTF8 := .T.

	If Empty(cObject)
		cError := "Empty string!"
	ElseIf (cChar := SubStr(cObject, nIni, 1)) == "{"
		uRet := HashMap():New()
		uRet:lCaseSensitive := lCaseSensitive

		//0 - idle, 1 - naming, 2 - ending naming, 3 - initing valueing, 4 - ending value
		For nA := nIni+1 To Len(cObject)
			cChar := SubStr(cObject, nA, 1)
			If nState == 0
				If cChar == "}"
					Exit 
				ElseIf !(cChar $ cBlank)
					nState := 1
					If cChar == '"'
						lQuoted := .T.
					ElseIf !AcceptChar(cChar)
						cError := "Sintax error at "+SubStr(cObject, nA)
						Exit
					Else
						cName := cChar
					EndIf
				EndIf 
			ElseIf nState == 1
				If cChar == '"'
					If lQuoted
						lQuoted := .F.
						nState := 2
					Else
						cError := "Sintax error at "+SubStr(cObject, nA)
						Exit
					Endif
				ElseIf lQuoted
					cName += cChar
				ElseIf cChar $ cBlank
					nState := 2
				ElseIf cChar == cDiscrim
					nState := 3
				ElseIf !AcceptChar(cChar)
					cError := "Sintax error at "+SubStr(cObject, nA)
					Exit
				Else
					cName += cChar
				EndIf
			ElseIf nState == 2
				If cChar == cDiscrim
					nState := 3
				ElseIf !(cChar $ cBlank)
					 UserException("Sintax error at "+SubStr(cObject, nA))
				EndIf
			ElseIf nState == 3
				If !(cChar $ cBlank)
					uValue := JsonParse(cObject, cDiscrim, cComma, @nA, lUTF8, lCaseSensitive)
					uRet:Put(cName, uValue)
					cName := ""
					If (cChar := SubStr(cObject, nA, 1)) == cComma
						nState := 0
					Else
						nState := 4
					EndIf
				EndIf
			ElseIf nState == 4
				If !(cChar $ cBlank)
					If cChar == "}"
						Exit
					ElseIf cChar == cComma
						nState := 0
					Else
						cError := "Sintax error at "+SubStr(cObject, nA)
						Exit
					EndIf
				EndIf
			EndIf
		Next
		If Empty(cError)
			If uRet:IsKeyEqual("___languageorign", "ADVPL") ;
				.And. ValType(cName := uRet:Get("___classname")) == "C"
				//
				cName += "():New()"
				uValue := &(cName)
				For nA := 1 To Len(uRet:aKeys)
					If ValType(uRet:aKeys[nA]) != "U"
						Eval(&("{| oObj, uVal| oObj:"+uRet:aKeys[nA]+" := uVal }") ;
							, uValue, uRet:aValues[nA])
					Endif
				Next
				uRet := uValue
			EndIf
		EndIf
	ElseIf cChar == "["
		uRet := {}
		For nA := nIni+1 To Len(cObject)
			cChar := SubStr(cObject, nA, 1)
			//0 - idle, 1 - finishing valuing
			If cChar == "]"
				Exit
			ElseIf nState == 0
				If !(cChar $ cBlank)
					aAdd(uRet, JsonParse(cObject, cDiscrim, cComma, @nA, lUTF8, lCaseSensitive))
					nState := 1
				EndIf
			ElseIf nState == 1
				If !(cChar $ cBlank)
					If cChar == cComma
						nState := 0
					EndIf
				EndIf
			EndIf
		Next
	ElseIf cChar == '"'
		uRet := ""
		For nA := nIni+1 To Len(cObject)
			cChar := SubStr(cObject, nA, 1)
			If cChar == '"'
				Exit
			ElseIf cChar == "\"
				nA++
				cChar := SubStr(cObject, nA, 1)
				If cChar == "r"
					uRet += CHR(13) //Carriage Return
				ElseIf cChar == "n"
					uRet += CHR(10) //new line
				ElseIf cChar == "t"
					uRet += CHR(9) //Tab
				ElseIf cChar == "b"
					uRet += CHR(8) //backslash
				ElseIf cChar == "f"
					uRet += CHR(12) //formfeed
				ElseIf cChar == "u"
					nA++
					//cChar := HEX2Conv(SubStr(cObject, nA, 4), 2) //converte de hexa para utf8
					If SubStr(cObject, nA, 2) == "00"
						uRet += Chr(HEX2Conv(SubStr(cObject, nA+2, 2)))
					ElseIf lUTF8
						uRet += Chr(HEX2Conv(SubStr(cObject, nA, 2))) + Chr(HEX2Conv(SubStr(cObject, nA + 2, 2)))
					Else
						uRet += DecodeUTF8("\u" + SubStr(cObject, nA, 4))
					Endif
					nA += 3
				Else
					uRet += cChar
				EndIf
			Else
				uRet += cChar
			EndIf
		Next
		//Verifica se está no formato de data/hora próprio de json/xml
		uValue := U_REFind("^(\d{4}-(?:0\d|1[012])-(?:[012]\d|3[01])T(?:[01]\d|2[0-3])(?:\:[0-5]\d){2})(?:(\.\d{1,3})?((?:\+|-)(?:[01]\d|2[0-3]):[0-5]\d)?)?", uRet) 
		//uValue := U_REFind("^(\d{4}(?:-\d\d){2}T\d\d(?:\:\d\d){2})(?:(\.\d{1,3})?((?:\+|-)\d\d:\d\d)?)?$", uRet)
		If Len(uValue) > 0
			uRet := U_SmANSIDate(uValue[1,2,1,1],,2) //Retorna a
		ElseIf lUTF8
			uRet := DecodeUTF8(uRet)
		Endif
	ElseIf cChar $ 'tTfFnN'
		uRet := ""
		For nA := nIni To Len(cObject)
			cChar := Upper(SubStr(cObject, nA, 1))
			If !(cChar $ cBlank)
				If cChar $ "TRUEFALSN" //Letras de true, false e null
					If Len(uRet) >= 5 //O maior match possível é 5 (false)
						cError := "Sintax error at "+SubStr(cObject, nIni)
						Exit
					Else
						uRet += cChar
					EndIf
				ElseIf cChar $ "]},"
					Exit
				Else
					cError := "Sintax error at "+SubStr(cObject, nIni)
					Exit
				EndIf
			ElseIf Len(uRet) > 0
				Exit
			EndIf
		Next
		If uRet == "TRUE"
			uRet := .T.
		ElseIf uRet == "FALSE"
			uRet := .F.
		ElseIf uRet == "NULL
			uRet := nil
		Else
			cError := "Sintax error at "+SubStr(cObject, nIni)
		Endif
		nA--
	ElseIf IsDigit(cChar) .Or. cChar = "." .Or. cChar == "-"
		uRet := ""
		IF cChar == "-"
			uRet := "-"
			nIni++
			cChar := SubStr(cObject, nIni, 1)
		EndIf
		IF cChar == "."
			uRet += "0"
		EndIf
		For nA := nIni To Len(cObject)
			cChar := Upper(SubStr(cObject, nA, 1))
			If IsDigit(cChar)
				uRet += cChar
			ElseIf cChar == "."
				If lQuoted
					cError := "Sintax error at "+SubStr(cObject, nIni)
					Exit
				Else
					uRet += cChar
					lQuoted := .T.
				Endif
			ElseIf cChar $ "]},"
				Exit
			ElseIf cChar $ cBlank
				Exit
			Else
				cError := "Sintax error at "+SubStr(cObject, nIni)
				Exit
			EndIf
		Next
		uRet := Val(uRet)
		nA--
	Else
		cError := "Sintax error at "+SubStr(cObject, nIni)
	EndIf
	If !Empty(cError)
		UserException(cError)
	EndIf
	nIni := nA
Return uRet

User Function AcceptChar(cChar)

return IsAlpha(cChar) .Or. IsDigit(cChar) .Or. cChar $ "_"

User Function SmRemScape(cString, lRemoveQuotes)
Local cAux := cString
Local cRet := ""
Local cChr
Local nI
Default lRemoveQuotes := .f.

If lRemoveQuotes .and. Left(cAux, 1) == '"'
	cAux := SubStr(cAux, 2)
	cAux := Left(cAux, Len(cAux) - 1)
EndIf

For nI := 1  To Len(cAux)
	If (cChr := SubStr(cAux, nI, 1)) == "\"
		nI++
		If (cChr := SubStr(cAux, nI, 1)) == "r"
			cRet += CHR(13) //Carriage Return
		ElseIf cChr == "n"
			cRet += CHR(10) //new line
		ElseIf cChr == "t"
			cRet += CHR(9) //Tab
		ElseIf cChr == "b"
			cRet += CHR(8) //backslash
		ElseIf cChr == "f"
			cRet += CHR(12) //formfeed
		ElseIf cChr == "u"
			cChr := HEX2Conv(SubStr(cString, nI, 4), 2) //converte de hexa para utf8 
			nI += 2
			cRet += DecodeUTF8(cChr)
		Else
			cRet += cChr
		EndIf
	Else
		cRet += cChr
	EndIf
Next

Return cRet

User Function SmDoScape(cString, lAddQuotes)
Local cAux := cString
Local cRet := ""
Local cChr
Local nI
Default lAddQuotes := .f.

For nI := 1  To Len(cAux)
	If (cChr := SubStr(cAux, nI, 1)) == CHR(13)
		cRet += CHR(13) //Carriage Return
	ElseIf cChr == CHR(10) //new line
	ElseIf cChr == CHR(9) //Tab
	ElseIf cChr == CHR(8) //backslash
	ElseIf cChr == CHR(12) //formfeed
	ElseIf Asc(cChr) > 127
		cChr := EncodeUTF8(cChr)
		cRet += cChr
	ElseIf cChr $ "\"+'"'
		cRet += "\"+cChr
	Else
		cRet +=  cChr
	EndIf
Next

If lAddQuotes
	cRet := '"' + cRet + '"'
EndIf

Return cRet

/*/{Protheus.doc} HEX2Conv
Executa uma conversão de hexadecimal para tratamento de caracteres de escape no estilo de parâmetros HTML

@author thiago.santos
@since 08/05/2014
@version 1.0

@param cHex, character, String hexadecimal
@param nType, numeric, tipo de conversão a efetuar (1 - Hexa para Dec, 3 - dec para hexa (2 caracares), 1 - dec para hexa, vários caracteres)

@return character, string binária correspondente
/*/
Static Function HEX2Conv( cHex, nType )
static cHexa := "123456789ABCDEF"
Local nL := Len( cHex )
Local nRes := 0
Local nExp := 0
Local nAux
Local uRet
Local ni
Default nType := 0

If nType == 0
	uRet := 0
	For ni := nL to 1 STEP -1
		uRet += At(Upper(SubStr(cHex, ni, 1)), cHexa) * (16^nExp)
		nExp++
	Next
ElseIf nType == 3
	uRet := ""
	For ni := nL to 1 step -1
		nRes := asc(SubStr(cHex, ni, 1))
		nAux := int(nRes / 16)
		uRet += IIF(nAux == 0, "0", SubStr(cHexa, nAux, 1))
		nAux := int(nRes % 16)
		uRet += IIF(nAux == 0, "0", SubStr(cHexa, nAux, 1))
	Next
Else
	uRet := ""
	For ni := 1 to nL step 2
		nRes := HEX2Conv(SubStr(cHex, ni, 2))
		If nType == 1 .or. nRes > 0
			uRet += chr(nRes) 
		EndIf
		nExp++
	Next
EndIf

Return uRet

/*/{Protheus.doc} Hexa2Str
Transforma uma sequência de caracteres hexadecimais na string binária correspondente.

@author thiago.santos
@since 08/05/2014
@version 1.0

@param cInfo, character, sequência de caracteres hexadecimais

@return character, string binária correspondente
/*/
Static Function Hexa2Str(cInfo)
Local cHexa := "0123456789ABCDEF"
Local cRet := ""
Local nI

For nI := 1 to Len(cInfo) STEP 2
	cRet += CHR((At(SubStr(cInfo, nI, 1), cHexa)- 1)*16 + (At(SubStr(cInfo, nI+1, 1), cHexa)- 1)) 
Next

Return cRet

User Function RC4Decrypt(cInfo, cChave, lHexa)
Local cInfo0
Local cError
Local oExt
Local cRet
Default lHexa := .F.

If FindFunction("RC4Crypt")
	cInfo0 := IIf(lHexa, Hexa2Str(cInfo), cInfo)
	SmSafeExec("RC4Crypt", {cInfo0, cChave, .F.}, @cRet)
Else
	oExt := SmGetExtSrv()
	cRet := oExt:RC4Decrypt(cInfo, cChave, @cError)
EndIf

Return cRet

User Function SmRC4Crypt(cInfo, cChave, lHexa)
Local cRet
Default lHexa := .F.

If FindFunction("RC4Crypt")
	SmSafeExec("RC4Crypt", {cInfo, cChave, lHexa}, @cRet)
Else
	oExt := SmGetExtSrv()
	cRet := oExt:RC4Encrypt(cInfo, cChave)
Endif

Return cRet

/*/{Protheus.doc} SmJSONDate
Transforma uma data no formato esperado por JSON ou XML

@author thiago.santos
@since 08/05/2014
@version 1.0

@param dData, date, data a transformar
@param lFuso, boolean, determina se as datas serão convertidas considerando fusohorário

@return character, a data convertida
/*/
User Function SmJSONDate(dData0, lFuso, cHora)
Local dData
Local cRet
Default lFuso := .F.

If ValType(dData0) == "A"
	dData := dData0
	cHora := Left(dData[2], 2)+":"+SubStr(dData[2], 3, 2)+":"+Right(dData[2], 2)
	dData := dData[1]
Else
	dData := dData0
EndIf
Default cHora := "00:00:00"

cRet := cRet := Padl(Year(dData), 4, "0")+"-"+Padl(Month(dData), 2, "0")+"-"+Padl(Day(dData), 2, "0")
cRet += "T"+cHora //Necessário incluir hora e fuso horário para bater com sintaxe da data
If lFuso
	cRet += "-3.00"
EndIf

Return cRet

/*/{Protheus.doc} SmObj2Xml
Transforma um objeto em xml. Atualmente, parâmetros de tag não são suportados

@author thiago.santos
@since 08/05/2014
@version 1.0

@param oObject, object, objeto a transformar em xml
@param lFuso, boolean, determina se as datas serão convertidas considerando fusohorário

@return character, o xml gerado a partir do objeto
/*/
User Function SmObj2Xml(oObject, lFuso)
Local cRet
Local cAux
Local bKey
Local bValue
Local aKeys
Local aValues
Local nI
Local cClassname
Local cKey
Local uValue
Local cType

If (cType := ValType(oObject)) == "O"
	cRet := "" 
	If (cClassname := GetClassName(oObject)) == "HASHMAP"
		aKeys := oObject:aKeys
		aValues := oObject:aValues
		bKey := {|| aKeys[nI]}
		bValue := {|| aValues[nI]}
	Else
		aKeys := ClassDataArr(oObject)
		aValues := aKeys
		bKey := {|| aKeys[nI, 1]}
		bValue := {|| aValues[nI, 2]}
	Endif
	
	
	For nI := 1 to Len(aKeys)
		If Valtype(cKey := Eval(bKey)) == "C"
			If Empty(cAux := SmObj2Xml(Eval(bValue), lFuso))
				cRet += "<"+cKey+"/>"
			Else
				cRet += "<"+cKey+">"+cAux+"</"+cKey+">"
			EndIf
		EndIf
	Next
	If cClassname != "HASHMAP"
		cRet += "<___languageorign>ADVPL</___languageorign>><___classname>"+cClassName+"</___classname>"
	EndIf
ElseIf cType == "C"
	cRet := SmXmlScape(oObject)
ElseIf cType == "A"
	cRet := ""
	For nI := 1 to Len(oObject)
		cRet += SmObj2Xml(oObject[nI], lFuso)
	Next
ElseIf cType == "D"
	cRet := SmJSONDate(oObject, lFuso) //Data em Xml é no mesmo padrão que em JSON
ElseIf cType == "U"
	cRet := "" //Valor nulo vai ser tratado como em branco
ElseIf cType == "L"
	cRet := IIF(oObject, "true", "false")
Else
	cRet := Any2Str(oObject) //Conversão de demais tipos
EndIf

Return cRet

/*/{Protheus.doc} SmXmlScape
Trata caracteres problemáticos de um texto xml

@author thiago.santos
@since 08/05/2014
@version 1.0

@param cTexto, character, Texto a tratar

@return character, texto tratado
/*/
User Function SmXmlScape(cTexto)
Local cRet := StrTran(ctexto, "&", "&amp;")

cRet := StrTran(cRet, '"', "&quot;")
cRet := StrTran(cRet, "'", "&apos;")
cRet := StrTran(cRet, "<", "&lt;")
cRet := StrTran(cRet, ">", "&gt;")

Return cRet

/*/{Protheus.doc} SmHM2Http
Transforma um HashMap em parâmetros para HTTP (GET ou POST).

O primeiro nível de chaves é tratado como os parâmetros passados. O segundo nível, se for um objeto,
é assumido como um JSON ou como um XML e, portanto, é devidamente convertido a caracter e tem os
caracteres problemáticos tratados, para formar o valor dos parâmetros
	
@author thiago.santos
@since 08/05/2014
@version 1.0

@param oHM, object, HashMap que será serializado em parâmetros

@return character, a mensagem de expiração do Sistema
/*/
User Function SmHM2Http(oHM, lFuso, nXmlJson)
Local cRet       := ""
Local cKey
Local nI
Default nXmlJson := 1

For nI := 1 To Len(oHM:aKeys)
	If !Empty(cKey := oHM:aKeys[nI])
		If !Empty(cRet)
			cRet += "&"
		EndIf
		cRet += cKey+'='
		If nXmlJson == 1
			cRet += EscapeGet(SmObj2Xml(oHM:aValues[nI], lFuso))
		Else
			cRet += EscapeGet(SmObj2Json(oHM:aValues[nI], lFuso))
		EndIf
	EndIf
Next

Return cRet

/*/{Protheus.doc} smEncode
Criptografia em codigo numerico baseado no logaritmo e potencia com substituicao por caracteres pre-definidos em vetor.

@author Antonio C Ferreira (acferreira)
@since 05/06/2014
@version 1.0

@param cCodigo, codigo a ser criptografado.
@param cEncode, resultado da criptografia, deve ser passado por referencia.
@param cMensagem, mensagem de erro do processo da rotina, deve ser passado por referencia.

@return boolean, Verdadeiro ou Falso.
/*/

User Function smEncode(cCodigo, cEncode, cMensagem)

Local nTam     := 0
Local nMeio    := 0
Local cCodigo1 := ""
Local cCodigo2 := ""
Local cAux01   := ""
Local cAux02   := ""
Local nAux01   := 0
Local nAux02   := 0

cCodigo   := Alltrim(cCodigo)
cMensagem := ""

Begin Sequence

    nTam := Len(cCodigo)
    
    If  (nTam > 16)
        cMensagem := "O código deve ser menor ou igual a 16 dígitos!"
        Break
    EndIf

    If  !( IsDigit(cCodigo) )
        cMensagem := "O código deve ser de conteúdo numérico somente!"
        Break
    EndIf
    
    If  (nTam > 8)  //Caso seja grande o numero divide no meio e obtem o encode das duas metades recursivamente.
        nMeio := Int(nTam/2)
        nMeio += If(((nTam/2)-nMeio) > 0, 1, 0)
         
        cCodigo1 := Left(cCodigo, nMeio)
        cCodigo2 := SubStr(cCodigo, (nMeio+1))
        
        If  !( smEncode(cCodigo1, @cAux01, @cMensagem) ) .Or. !( smEncode(cCodigo2, @cAux02, @cMensagem) )
            Break
        EndIf     
        
        cEncode :=  cAux01 + "y" + cAux02 //% define a divisao no codigo
        Break  
    EndIf
    
    cAux01 := StrTran(Alltrim(Str((Log(Val(cCodigo))))),".","x")  //Tratamento para ponto.
    
    //Passa para cada 2 digitos do codigo resultante
    Do  While !( Empty(cAux01) )

        cAux02 := Alltrim(Left(cAux01+"0",2))  //Caso seja um digito, adiciona o Zero no final
        If  (Len(cAux01) <= 2)
            cAux01 := ""
        Else    
            cAux01 := Right(cAux01,Len(cAux01)-2)
        EndIf    
        
        If  ("x" $ cAux02)  //Posso ter 2P como P2
            If  (Left(cAux02,1) != "x")
                cAux01 := "x" + cAux01  //Devolve para o proximo processamento
                cAux02 := Left(cAux02,1)
            Else
                cEncode += cAux02  //Assume o * e o proximo digito sem codificar.
                Loop    
            EndIf
        EndIf    
            
        nAux01 := Val(cAux02)
        nAux02 := 0
            
        If  (nAux01 >= 50)  //Limite de caracteres da criptografia
            nAux02 := (nAux01 - 50) + nDeslocar  //nDeslocar representa a posicao real dentro de aInd que contem 51 chars sendo o primeiro o zero.
            nAux01 := 50 + nDeslocar             //caso de zero o nDeslocar posicione no primeiro char de aInd.
        Else
            nAux01 := nAux01 + nDeslocar    
        EndIf
            
        //Exemplo.: Codigo 76 => 50+1 e 26+1 => Z e B => ZB => Decodificando => 51 e 27 => 78 - 2 => 76
        //          Codigo 46 => 46+1 => V => Decodificando => 47 - 1 => 46
        cEncode += aInd[nAux01]
        cEncode += Alltrim(If(nAux02 > 0, aInd[nAux02], ""))
    
    EndDo
        

End Sequence

If  !( Empty(cMensagem) )
    ConOut("smEncode: " + cMensagem)
    Aviso("Mensagem do Usuário", cMensagem, {"ok"})
EndIf

Return Empty(cMensagem)



/*/{Protheus.doc} smDecode
DeCriptografia em codigo numerico baseado no logaritmo e potencia com substituicao por caracteres pre-definidos em vetor.

@author Antonio C Ferreira (acferreira)
@since 05/06/2014
@version 1.0

@param cCodigo, codigo a ser decriptografado.
@param cEncode, resultado da decriptografia, deve ser passado por referencia.
@param cMensagem, mensagem de erro do processo da rotina, deve ser passado por referencia.

@return boolean, Verdadeiro ou Falso.
/*/

User Function smDecode(cCodigo, cDecode, cMensagem)

Local nPos     := 0
Local nTam     := 0
Local cCodigo1 := ""
Local cCodigo2 := ""
Local cAux01   := ""
Local cAux02   := ""
Local cResult  := ""

Begin Sequence

    nPos := At("y", cCodigo)
    
    If  (nPos > 0)
        cCodigo1 := Left(cCodigo, nPos-1)
        cCodigo2 := SubStr(cCodigo, nPos+1)
        
        If  !( smDecode(cCodigo1, @cAux01, @cMensagem) ) .Or. !( smDecode(cCodigo2, @cAux02, @cMensagem) )
            Break
        EndIf
        
        cDecode := cAux01 + cAux02
        Break
    EndIf     

    nTam   := Len(cCodigo)
    cAux01 := cCodigo
    
    Do  While !( Empty(cAux01) )
        cAux02 := Left(cAux01,1)
        cAux01 := Right(cAux01,Len(cAux01)-1)
        
        If  (cAux02 == 'x')
            cResult += "."
            
            cAux02 := Left(cAux01,1)
            cAux01 := Right(cAux01,Len(cAux01)-1)
            
            cResult += cAux02
            
            Loop
        EndIf
        
        nPos := Ascan(aInd, cAux02)
        
        If  (nPos == 51)  //Ultima posicao vai somar com a proxima posicao
            cAux02 := Left(cAux01,1)
            cAux01 := Right(cAux01,Len(cAux01)-1)
            
            nPos := Ascan(aInd, cAux02) + 50
        EndIf
        
        nPos -= 1  //Subtrai o deslocado do aInd.
        
        cResult += StrZero(nPos,2)
            
    EndDo
    
    cDecode := Alltrim(Str(Int(Exp(Val(cResult)))))
    
End Sequence

If  !( Empty(cMensagem) )
    ConOut("smDecode: " + cMensagem)
    Aviso("Mensagem do Usuário", cMensagem, {"ok"})
EndIf

Return Empty(cMensagem)

User Function SmXF(oParent, aCond, nC)
Local aParent := GetXmlArr(oParent)
Local lFound  := .F.
Local nCount
Local nJ
Local nI
Local nK
Local aChild
Local oRet
Local aComp
Default nC := 1
aComp := StrToKarr(aCond[nC], "=")

For nJ := 1 to Len(aParent)
	nCount := IIF(ValType(aParent[nJ]) == "A", Len(aParent[nJ]), XmlChildCount(aParent[nJ]))
	For nI := 1 To nCount
		aChild := GetXmlArr(XmlGetChild(aParent[nJ], nI))
		For nK := 1 To Len(aChild)
			If !(Upper(aComp[1]) == Upper(aChild[nK]:RealName)) //Verifica se o nome real bate
				Loop
			EndIf
			If Len(aComp) > 1 .And. !(aComp[2] == aChild[nK]:Text) //Verifica se o valor bate
				Loop
			EndIf
			If Len(aCond) > nC .And. ValType(SmXF(aChild[nK], aCond, nC+1)) == "U" //Verifica se as próximas condições são atendidas em alguma filha
				Loop
			EndIf
			lFound := .T.
			Exit
		Next
		If lFound
			oRet := aParent[nJ]
			Exit
		EndIf
	Next
	If lFound
		Exit
	EndIf
Next

Return oRet

User Function SmTrimJson(uInfo)
Local uRet

If ValType(uInfo) == "C"
	uRet := Alltrim(uInfo)
Else
	uRet := uInfo
EndIf

Return SmObj2Json(uRet)