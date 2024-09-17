#INCLUDE "PROTHEUS.CH"
//-------------------------------------------------------------------
/*/{Protheus.doc} FCOMXFUN
Fonte para Rotinas Genericas de Compras

@author  Guilherme Santos
@since   11/11/2020
@version 12.1.25
/*/
//-------------------------------------------------------------------
User Function FCOMXFUN()
Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} FCOMXUSR
Retorna os Compradores dos Grupos de Compras Pre-Definidos

@author  Guilherme Santos
@since   11/11/2020
@version 12.1.25
/*/
//-------------------------------------------------------------------
User Function FCOMXUSR()
	Local aGrpCom	:= StrToArray(SuperGetMV("PC_GRPCOMP", NIL, "|000001|000002|"), "|")
	Local cQuery	:= ""
	Local cTabQry	:= ""
	Local nI		:= 0
	Local cRetorno	:= ""

	If !Empty(aGrpCom)
		cQuery += "SELECT	SAJ.AJ_GRCOM" + CRLF
		cQuery += ",		SAJ.AJ_USER " + CRLF
		cQuery += "FROM		" + RetSqlName("SAJ") + " SAJ" + CRLF
		cQuery += "			INNER JOIN" + CRLF
		cQuery += "			" + RetSqlName("SY1") + " SY1" + CRLF
		cQuery += "			ON SY1.Y1_FILIAL = '" + xFilial("SY1") + "'" + CRLF
		cQuery += "			AND SY1.Y1_USER = SAJ.AJ_USER" + CRLF
		cQuery += "			AND SY1.D_E_L_E_T_ = ''" + CRLF
		cQuery += "WHERE	SAJ.AJ_FILIAL = '" + xFilial("SAJ") + "'" + CRLF
		cQuery += "AND		SAJ.AJ_GRCOM IN ("

		For nI := 1 to Len(aGrpCom)
			If nI > 1
				cQuery += ","
			EndIf
			cQuery += "'" + aGrpCom[nI] + "'"
		Next nI
		cQuery += ")" + CRLF

		cQuery += "AND		SAJ.D_E_L_E_T_ = ''" + CRLF
		cQuery += "ORDER BY SAJ.AJ_GRCOM, SAJ.AJ_USER" + CRLF

		cTabQry := MpSysOpenQuery(cQuery)

		If !(cTabQry)->(Eof())
			While !(cTabQry)->(Eof())
				cRetorno += "|" + (cTabQry)->AJ_USER

				(cTabQry)->(DbSkip())
			End
		EndIf

		If Select(cTabQry) > 0
			(cTabQry)->(DbCloseArea())
		EndIf
	EndIf

Return cRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} FCOMVLD
Validacao do Fornecedor e Produto no Pedido de Compra

@author  Guilherme Santos
@since   17/11/2020
@version 12.1.25
/*/
//-------------------------------------------------------------------
User Function FCOMVLD()
	Local aGrupo 	:= UsrGrComp(RetCodUsr())
	Local cGrpCom	:= ""
	Local cCampo 	:= ReadVar()
	Local lRetorno	:= .T.
	Local lTransf	:= If(cA120Forn == "999999", .T., .F.)
	Local cTpProd	:= SuperGetMV("PC_TPPROD", NIL, "|ME|PA|PI|MP|") //Tipos de Produtos que apenas os grupos de compras podem incluir em pedidos de compras

	If !lTransf
		Do Case

			Case cCampo == "M->C7_LOJA" .OR. cCampo == "CA120LOJ"
				//Validacao retirada do campo X3_VLDUSER
				lRetorno := U_M270VLD()
			Case cCampo == "M->C7_PRODUTO"
				cGrpCom := SuperGetMV("PC_GRPCOMP", NIL, "|000001|000002|")

				If Ascan(aGrupo, {|x| AllTrim(x) $ cGrpCom}) == 0 .And. SB1->B1_TIPO $ cTpProd
					Aviso("FCOMVLD", "Tipo de Produto Inválido" + CRLF + "Tipos inválidos: ME, MP, PA e PI", {"Fechar"})
					lRetorno := .F.
				EndIf
			EndCase
	EndIf

Return lRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} FCOMXARM
Retorna o Armazem padrao para o Pedido de Compra

@author  Guilherme Santos
@since   21/12/2020
@version 12.1.25
/*/
//-------------------------------------------------------------------
User Function FCOMXARM(cFilPed, cProduto)
	Local lWMSSythex	:= .F.
	Local lArm95		:= .T.
	Local cArmTmp		:= "95"
	Local aGrupo 		:= UsrGrComp(RetCodUsr())
	Local cGrpCom		:= ""
	Local cRetorno 		:= "01"

	Default cFilPed		:= xFilial("SC7")
	Default cProduto	:= M->C7_PRODUTO

	lWMSSythex	:= SuperGetMv("BZ_WMS", NIL, "", cFilPed) == "WIS"
	lArm95		:= SuperGetMV("BZ_WMSARM", NIL, .F., cFilPed)
	cArmTmp		:= SuperGetMv("BZ_WMSATMP", NIL, "95", cFilPed)
	cGrpCom		:= SuperGetMV("PC_GRPCOMP", NIL, "|000001|000002|", cFilPed)
	cRetorno	:= Posicione("SB1", 1, xFilial("SB1") + cProduto, "B1_LOCPAD")

	If lWMSSythex .AND. lArm95 .AND. !Empty(cArmTmp)
		//Somente para os grupos de compradores de produtos de revenda
		If Ascan(aGrupo, {|x| AllTrim(x) $ cGrpCom}) > 0
			cRetorno := cArmTmp
		EndIf
	EndIf

Return cRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} FCOMXSA5
Inclusao da Amarração do Produto x Fornecedor com base na tabela XT4
do SmartdocsX

@author  Guilherme Santos
@since   25/01/2021
@version 12.1.25
/*/
//-------------------------------------------------------------------
User Function FCOMXSA5()
	Local cQuery 	:= ""
	Local cTabQry 	:= ""

	cQuery += "SELECT	XT4.XT4_FILIAL" + CRLF
	cQuery += ",		XT4.XT4_CODFOR" + CRLF
	cQuery += ",		XT4.XT4_LOJFOR" + CRLF
	cQuery += ",		SA2.A2_NOME" + CRLF
	cQuery += ",		XT4.XT4_CODPRD" + CRLF
	cQuery += ",		SB1.B1_DESC" + CRLF

	cQuery += "FROM	" + RetSqlName("XT4") + " XT4" + CRLF
	cQuery += "		INNER JOIN" + CRLF
	cQuery += "		" + RetSqlName("SA2") + " SA2" + CRLF
	cQuery += "		ON SA2.A2_FILIAL = '" + xFilial("SA2") + "'" + CRLF
	cQuery += "		AND SA2.A2_COD = XT4.XT4_CODFOR" + CRLF
	cQuery += "		AND SA2.A2_LOJA = XT4.XT4_LOJFOR" + CRLF
	cQuery += "		AND	SA2.D_E_L_E_T_ = ''" + CRLF
	cQuery += "		INNER JOIN" + CRLF
	cQuery += "		" + RetSqlName("SB1") + " SB1" + CRLF
	cQuery += "		ON SB1.B1_FILIAL = ''" + CRLF
	cQuery += "		AND SB1.B1_COD = XT4.XT4_CODPRD" + CRLF
	cQuery += "		AND SB1.D_E_L_E_T_ = ''" + CRLF
	cQuery += "WHERE	XT4.D_E_L_E_T_ = ''" + CRLF
	cQuery += "AND NOT EXISTS (	SELECT	SA5.A5_FORNECE" + CRLF
	cQuery += "					,		SA5.A5_LOJA" + CRLF
	cQuery += "					,		SA5.A5_PRODUTO" + CRLF
	cQuery += "					FROM " + RetSqlName("SA5") + " SA5" + CRLF
	cQuery += "					WHERE SA5.A5_FILIAL = '" + xFilial("SA5") + "'" + CRLF
	cQuery += "					AND	SA5.A5_FORNECE = XT4.XT4_CODFOR" + CRLF
	cQuery += "					AND SA5.A5_LOJA = XT4.XT4_LOJFOR" + CRLF
	cQuery += "					AND SA5.A5_PRODUTO = XT4.XT4_CODPRD" + CRLF
	cQuery += "					AND SA5.D_E_L_E_T_ = '')" + CRLF

	cTabQry := MpSysOpenQuery(cQuery)

	While !(cTabQry)->(Eof())
		RecLock("SA5", .T.)
			SA5->A5_FILIAL 	:= xFilial("SA5")
			SA5->A5_FORNECE := (cTabQry)->XT4_CODFOR
			SA5->A5_LOJA 	:= (cTabQry)->XT4_LOJFOR
			SA5->A5_NOMEFOR := (cTabQry)->A2_NOME
			SA5->A5_PRODUTO := (cTabQry)->XT4_CODPRD
			SA5->A5_NOMPROD := (cTabQry)->B1_DESC
			SA5->A5_STATUS	:= "X"
		MsUnlock()

		(cTabQry)->(DbSkip())
	End

	If Select(cTabQry) > 0
		(cTabQry)->(DbCloseArea())
	EndIf

Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} FCOMXDOC
Visualizacao de documentos vinculados ao Pedido de Compra

@author  Guilherme Santos
@since   04/02/2021
@version 12.1.25
/*/
//-------------------------------------------------------------------
User Function FCOMXDOC(cAlias, nReg, nOpcao)
	Local cNumPed	:= ""
	Local cQuery	:= ""
	Local cTabQry	:= ""
	Local lContinua	:= .T.

	Do Case
	Case cAlias == "SF1"
		cQuery += "SELECT	SD1.D1_PEDIDO" + CRLF
		cQuery += "FROM		" + RetSqlName("SD1") + " SD1" + CRLF
		cQuery += "WHERE	SD1.D1_FILIAL = '" + SF1->F1_FILIAL + "'" + CRLF
		cQuery += "AND		SD1.D1_DOC = '" + SF1->F1_DOC + "'" + CRLF
		cQuery += "AND		SD1.D1_SERIE = '" + SF1->F1_SERIE + "'" + CRLF
		cQuery += "AND		SD1.D1_TIPO = '" + SF1->F1_TIPO + "'" + CRLF
		cQuery += "AND		SD1.D1_FORNECE = '" + SF1->F1_FORNECE + "'" + CRLF
		cQuery += "AND		SD1.D1_LOJA = '" + SF1->F1_LOJA + "'" + CRLF
		cQuery += "AND		SD1.D_E_L_E_T_ = ''" + CRLF
		cQuery += "GROUP BY SD1.D1_PEDIDO" + CRLF

		cTabQry := MpSysOpenQuery(cQuery)

		If !(cTabQry)->(Eof())
			cNumPed := (cTabQry)->D1_PEDIDO
		EndIf

		If Select(cTabQry) > 0
			(cTabQry)->(DbCloseArea())
		EndIf

	Case cAlias == "SE2"
		If AllTrim(SE2->E2_TIPO) == "NF"
			cQuery += "SELECT	SD1.D1_PEDIDO" + CRLF
			cQuery += "FROM		" + RetSqlName("SD1") + " SD1" + CRLF
			cQuery += "			INNER JOIN" + CRLF
			cQuery += "			" + RetSqlName("SF1") + " SF1" + CRLF
			cQuery += "			ON SF1.F1_FILIAL = SD1.D1_FILIAL" + CRLF
			cQuery += "			AND SF1.F1_DOC = SD1.D1_DOC" + CRLF
			cQuery += "			AND SF1.F1_PREFIXO = '" + SE2->E2_PREFIXO + "'" + CRLF
			cQuery += "			AND SF1.F1_SERIE = SD1.D1_SERIE" + CRLF
			cQuery += "			AND SF1.F1_FORNECE = SD1.D1_FORNECE" + CRLF
			cQuery += "			AND SF1.F1_LOJA = SD1.D1_LOJA" + CRLF
			cQuery += "			AND SF1.F1_TIPO = SD1.D1_TIPO" + CRLF
			cQuery += "			AND SF1.D_E_L_E_T_ = ''" + CRLF

			cQuery += "WHERE	SD1.D1_FILIAL = '" + SE2->E2_FILIAL + "'" + CRLF
			cQuery += "AND		SD1.D1_DOC = '" + SE2->E2_NUM + "'" + CRLF
			cQuery += "AND		SD1.D1_TIPO = 'N'" + CRLF
			cQuery += "AND		SD1.D1_FORNECE = '" + SE2->E2_FORNECE + "'" + CRLF
			cQuery += "AND		SD1.D1_LOJA = '" + SE2->E2_LOJA + "'" + CRLF
			cQuery += "AND		SD1.D_E_L_E_T_ = ''" + CRLF
			cQuery += "GROUP BY SD1.D1_PEDIDO" + CRLF

			cTabQry := MpSysOpenQuery(cQuery)

			If !(cTabQry)->(Eof())
				cNumPed := (cTabQry)->D1_PEDIDO
			EndIf

			If Select(cTabQry) > 0
				(cTabQry)->(DbCloseArea())
			EndIf
		Else
			Aviso("FCOMXDOC", "Consulta disponível apenas para Titulos do Tipo NF.", {"Fechar"})
			lContinua := .F.
		EndIf
	EndCase

	If lContinua
		If Empty(cNumPed)
			Aviso("FCOMXDOC", "Pedido de Compra não localizado.", {"Fechar"})
		Else
			DbSelectArea("SC7")
			DbSetOrder(1)		//C7_FILIAL, C7_NUM

			If SC7->(DbSeek(xFilial("SC7") + cNumPed))
				MsDocument("SC7", SC7->(Recno()), 4)
			Else
				Aviso("FCOMXDOC", "Pedido de Compra não localizado.", {"Fechar"})
			EndIf
		EndIf
	EndIf

Return NIL

//-------------------------------------------------------------------
/*/{Protheus.doc} FCOMXHST
Consulta ao Historico do Pedido de Compra

@author  Guilherme Santos
@since   24/02/2021
@version 12.1.25
/*/
//-------------------------------------------------------------------
User Function FCOMXHST(cAlias, nReg, nOpcao)
	Local cNumPed	:= ""
	Local cQuery	:= ""
	Local cTabQry	:= ""
	Local lContinua	:= .T.

	Do Case
	Case cAlias == "SF1"
		cQuery += "SELECT	SD1.D1_PEDIDO" + CRLF
		cQuery += "FROM		" + RetSqlName("SD1") + " SD1" + CRLF
		cQuery += "WHERE	SD1.D1_FILIAL = '" + SF1->F1_FILIAL + "'" + CRLF
		cQuery += "AND		SD1.D1_DOC = '" + SF1->F1_DOC + "'" + CRLF
		cQuery += "AND		SD1.D1_SERIE = '" + SF1->F1_SERIE + "'" + CRLF
		cQuery += "AND		SD1.D1_TIPO = '" + SF1->F1_TIPO + "'" + CRLF
		cQuery += "AND		SD1.D1_FORNECE = '" + SF1->F1_FORNECE + "'" + CRLF
		cQuery += "AND		SD1.D1_LOJA = '" + SF1->F1_LOJA + "'" + CRLF
		cQuery += "AND		SD1.D_E_L_E_T_ = ''" + CRLF
		cQuery += "GROUP BY SD1.D1_PEDIDO" + CRLF

		cTabQry := MpSysOpenQuery(cQuery)

		If !(cTabQry)->(Eof())
			cNumPed := (cTabQry)->D1_PEDIDO
		EndIf

		If Select(cTabQry) > 0
			(cTabQry)->(DbCloseArea())
		EndIf

	Case cAlias == "SE2"
		If AllTrim(SE2->E2_TIPO) == "NF"
			cQuery += "SELECT	SD1.D1_PEDIDO" + CRLF
			cQuery += "FROM		" + RetSqlName("SD1") + " SD1" + CRLF
			cQuery += "			INNER JOIN" + CRLF
			cQuery += "			" + RetSqlName("SF1") + " SF1" + CRLF
			cQuery += "			ON SF1.F1_FILIAL = SD1.D1_FILIAL" + CRLF
			cQuery += "			AND SF1.F1_DOC = SD1.D1_DOC" + CRLF
			cQuery += "			AND SF1.F1_PREFIXO = '" + SE2->E2_PREFIXO + "'" + CRLF
			cQuery += "			AND SF1.F1_SERIE = SD1.D1_SERIE" + CRLF
			cQuery += "			AND SF1.F1_FORNECE = SD1.D1_FORNECE" + CRLF
			cQuery += "			AND SF1.F1_LOJA = SD1.D1_LOJA" + CRLF
			cQuery += "			AND SF1.F1_TIPO = SD1.D1_TIPO" + CRLF
			cQuery += "			AND SF1.D_E_L_E_T_ = ''" + CRLF

			cQuery += "WHERE	SD1.D1_FILIAL = '" + SE2->E2_FILIAL + "'" + CRLF
			cQuery += "AND		SD1.D1_DOC = '" + SE2->E2_NUM + "'" + CRLF
			cQuery += "AND		SD1.D1_TIPO = 'N'" + CRLF
			cQuery += "AND		SD1.D1_FORNECE = '" + SE2->E2_FORNECE + "'" + CRLF
			cQuery += "AND		SD1.D1_LOJA = '" + SE2->E2_LOJA + "'" + CRLF
			cQuery += "AND		SD1.D_E_L_E_T_ = ''" + CRLF
			cQuery += "GROUP BY SD1.D1_PEDIDO" + CRLF

			cTabQry := MpSysOpenQuery(cQuery)

			If !(cTabQry)->(Eof())
				cNumPed := (cTabQry)->D1_PEDIDO
			EndIf

			If Select(cTabQry) > 0
				(cTabQry)->(DbCloseArea())
			EndIf
		Else
			Aviso("FCOMXHST", "Consulta disponível apenas para Titulos do Tipo NF.", {"Fechar"})
			lContinua := .F.
		EndIf
	EndCase

	If lContinua
		If Empty(cNumPed)
			Aviso("FCOMXHST", "Pedido de Compra não localizado.", {"Fechar"})
		Else
			DbSelectArea("SC7")
			DbSetOrder(1)		//C7_FILIAL, C7_NUM

			If SC7->(DbSeek(xFilial("SC7") + cNumPed))
				//Posiciona no Pedido de Compra e chama a tela de consulta do historico
				U_PROM316B()
			Else
				Aviso("FCOMXHST", "Pedido de Compra não localizado.", {"Fechar"})
			EndIf
		EndIf
	EndIf

Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} FCOMXFRN
Retorna as informacoes do Fornecedor para a tela de liberacao de 
documentos de acordo com o campo informado no parametro

@author  Guilherme Santos
@since   28/05/2021
@version 12.1.25
/*/
//-------------------------------------------------------------------
User Function FCOMXFRN(cField)
	Local cFornece 	:= ""
	Local cLojaFor	:= ""
	Local cRetorno 	:= ""

	If AllTrim(SCR->CR_TIPO) $ "PC|IP|AE"
		Do Case
		Case cField == "CR_XCODFOR"
			cRetorno := Posicione("SC7", 1, SCR->CR_FILIAL + AllTrim(SCR->CR_NUM), "C7_FORNECE")
		Case cField == "CR_XLOJFOR"
			cRetorno := Posicione("SC7", 1, SCR->CR_FILIAL + AllTrim(SCR->CR_NUM), "C7_LOJA")
		Case cField == "CR_XNOMFOR"
			cFornece := Posicione("SC7", 1, SCR->CR_FILIAL + AllTrim(SCR->CR_NUM), "C7_FORNECE")
			cLojaFor := Posicione("SC7", 1, SCR->CR_FILIAL + AllTrim(SCR->CR_NUM), "C7_LOJA")

			If !Empty(cFornece) .AND. !Empty(cLojaFor)
				cRetorno := Posicione("SA2", 1, xFilial("SA2") + cFornece + cLojaFor, "A2_NOME")
			EndIf
		EndCase
	ElseIf AllTrim(SCR->CR_TIPO) == "CP"
		Do Case
		Case cField == "CR_XCODFOR"
			cRetorno := Posicione("SC3", 1, SCR->CR_FILIAL + AllTrim(SCR->CR_NUM), "C3_FORNECE")
		Case cField == "CR_XLOJFOR"
			cRetorno := Posicione("SC3", 1, SCR->CR_FILIAL + AllTrim(SCR->CR_NUM), "C3_LOJA")
		Case cField == "CR_XNOMFOR"
			cFornece := Posicione("SC3", 1, SCR->CR_FILIAL + AllTrim(SCR->CR_NUM), "C3_FORNECE")
			cLojaFor := Posicione("SC3", 1, SCR->CR_FILIAL + AllTrim(SCR->CR_NUM), "C3_LOJA")

			If !Empty(cFornece) .AND. !Empty(cLojaFor)
				cRetorno := Posicione("SA2", 1, xFilial("SA2") + cFornece + cLojaFor, "A2_NOME")
			EndIf
		EndCase
	EndIf

Return cRetorno
