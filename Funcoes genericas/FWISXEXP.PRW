#INCLUDE "PROTHEUS.CH"
//-------------------------------------------------------------------
/*/{Protheus.doc} FWISXEXP
Exportacao do cadastro de produtos para o WIS

@author  Guilherme Santos
@since   11/07/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
User Function FWISXEXP()
	Local cTxtAviso	:= "Esta rotina exportará o Cadastro selecionado para integração no WIS. Selecione a Opção desejada:"
	Local nOpcao	:= 0

	nOpcao := Aviso("FWISXEXP", cTxtAviso, {"Produtos", "Clientes", "Fornecedores", "Transportadoras", "Fechar"})

	Do Case
	Case nOpcao == 1
		Processa({|| FWISXEXP(nOpcao)}, "Exportando cadastro de Produtos...")
	Case nOpcao == 2
		Processa({|| FWISXEXP(nOpcao)}, "Exportando cadastro de Clientes...")
	Case nOpcao == 3
		Processa({|| FWISXEXP(nOpcao)}, "Exportando cadastro de Fornecedores...")
	Case nOpcao == 4
		Processa({|| FWISXEXP(nOpcao)}, "Exportando cadastro de Transportadoras...")
	EndCase

Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} FWISXEXP
Exportacao do cadastro selecionado para o WIS

@author  Guilherme Santos
@since   11/07/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
Static Function FWISXEXP(nOpcao)
	Local cQuery 	:= ""
	Local cTabQry	:= GetNextAlias()
	Local lFltCGC	:= SuperGetMV("WIS_FLTCGC", NIL, .T.)		//Filtra CNPJ em branco
	Local aParamBox	:= {}
	Local aRetParam	:= {}
	Local cProdIni	:= Space(TamSX3("B1_COD")[1])
	Local cProdFin	:= Space(TamSX3("B1_COD")[1])
	Local lContinua := .T.

	ProcRegua(0)

	IncProc("Selecionando registros...")

	Do Case
	Case nOpcao == 1

		aAdd(aParamBox, {1, "Produto de",  cProdIni,,, "SB1",,, .F.})
		aAdd(aParamBox, {1, "Produto até", cProdFin,,, "SB1",,, .F.})

		If ParamBox(aParamBox, "Informe os parâmetros", @aRetParam,,,,,,,, .F., .F.)
			cQuery += "SELECT 	B1_COD" + CRLF
			cQuery += "FROM 	" + RetSqlName("SB1") + CRLF
			cQuery += "WHERE 	B1_FILIAL = '" + xFilial("SB1") + "'" + CRLF
			cQuery += "AND		B1_COD BETWEEN '" + aRetParam[1] + "' AND '" + aRetParam[2] + "'" + CRLF
			cQuery += "AND 		D_E_L_E_T_ = ''" + CRLF
		Else
			lContinua := .F.
		EndIf
	Case nOpcao == 2

		cQuery += "SELECT	SA1.A1_FILIAL" + CRLF
		cQuery += ",		SA1.A1_COD" + CRLF
		cQuery += ",		SA1.A1_LOJA " + CRLF
		cQuery += "FROM		" + RetSqlName("SA1") + " SA1" + CRLF
		cQuery += "WHERE	SA1.A1_FILIAL = '" + xFilial("SA1") + "'" + CRLF
		cQuery += "AND		SA1.A1_MSBLQL <> '1'" + CRLF
		cQuery += "AND		SA1.D_E_L_E_T_ = ''" + CRLF
		If lFltCGC
			cQuery += "AND		SA1.A1_CGC <> ''" + CRLF
		EndIf
		cQuery += "ORDER BY SA1.A1_FILIAL, SA1.A1_COD, SA1.A1_LOJA" + CRLF

	Case nOpcao == 3

		cQuery += "SELECT	SA2.A2_FILIAL" + CRLF
		cQuery += ",		SA2.A2_COD" + CRLF
		cQuery += ",		SA2.A2_LOJA " + CRLF
		cQuery += "FROM		" + RetSqlName("SA2") + " SA2" + CRLF
		cQuery += "WHERE	SA2.A2_FILIAL = '" + xFilial("SA2") + "'" + CRLF
		cQuery += "AND		SA2.A2_MSBLQL <> '1'" + CRLF
		cQuery += "AND		SA2.D_E_L_E_T_ = ''" + CRLF
		If lFltCGC
			cQuery += "AND		SA2.A2_CGC <> ''" + CRLF
		EndIf
		cQuery += "ORDER BY SA2.A2_FILIAL, SA2.A2_COD, SA2.A2_LOJA" + CRLF

	Case nOpcao == 4

		cQuery += "SELECT	SA4.A4_FILIAL" + CRLF
		cQuery += ",		SA4.A4_COD" + CRLF
		cQuery += "FROM		" + RetSqlName("SA4") + " SA4" + CRLF
		cQuery += "WHERE	SA4.A4_FILIAL = '" + xFilial("SA4") + "'" + CRLF
		cQuery += "AND		SA4.A4_MSBLQL <> '1'" + CRLF
		If lFltCGC
			cQuery += "AND		SA4.A4_CGC <> ''" + CRLF
		EndIf
		cQuery += "AND		SA4.D_E_L_E_T_ = ''" + CRLF
		cQuery += "ORDER BY SA4.A4_FILIAL, SA4.A4_COD" + CRLF

	EndCase

	If lContinua
		DbUseArea(.T., "TOPCONN", TcGenQry(NIL, NIL, cQuery), cTabQry, .T., .T.)

		While !(cTabQry)->(Eof())

			Do Case
			Case nOpcao == 1
				IncProc("Exportando " + (cTabQry)->B1_COD)

				DbSelectArea("SB1")
				DbSetOrder(1)

				If SB1->(DbSeek(xFilial("SB1") + (cTabQry)->B1_COD))
					U_WISSB1()
				EndIf

			Case nOpcao == 2
				IncProc("Exportando " + (cTabQry)->A1_COD + (cTabQry)->A1_LOJA)

				DbSelectArea("SA1")
				DbSetOrder(1)

				If SA1->(DbSeek((cTabQry)->A1_FILIAL + (cTabQry)->A1_COD + (cTabQry)->A1_LOJA))
					U_WISSA1(.F.)
				EndIf

			Case nOpcao == 3

				IncProc("Exportando " + (cTabQry)->A2_COD + (cTabQry)->A2_LOJA)

				DbSelectArea("SA2")
				DbSetOrder(1)

				If SA2->(DbSeek((cTabQry)->A2_FILIAL + (cTabQry)->A2_COD + (cTabQry)->A2_LOJA))
					U_WISSA2(.F.)
				EndIf

			Case nOpcao == 4

				IncProc("Exportando " + (cTabQry)->A4_COD)

				DbSelectArea("SA4")
				DbSetOrder(1)

				If SA4->(DbSeek((cTabQry)->A4_FILIAL + (cTabQry)->A4_COD))
					U_WISSA4()
				EndIf

			EndCase

			(cTabQry)->(DbSkip())
		End

		If Select(cTabQry) > 0
			(cTabQry)->(DbCloseArea())
		EndIf
	EndIf



Return NIL
