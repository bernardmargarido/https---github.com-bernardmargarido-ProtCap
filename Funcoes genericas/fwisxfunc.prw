#INCLUDE "PROTHEUS.CH"
#INCLUDE "WISSTATUS.CH"
//-------------------------------------------------------------------
/*/{Protheus.doc} WISXSC6
Ajusta as quantidades do Pedido de Venda de acordo com as quantidades
separadas no WIS

@author  Guilherme Santos
@since   23/10/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
User Function WISXSC6(cPedido)
	Local aArea		:= GetArea()
	Local aAreaSC5	:= SC5->(GetArea())
	Local aAreaSC6	:= SC6->(GetArea())
	Local aAreaSC9	:= SC9->(GetArea())
	Local cMsgAviso	:= ""
	Local nOpcao	:= 0
	Local lRetorno 	:= .T.
	Local lContinua := .F.

	cMsgAviso += "As quantidades separadas no WIS são diferentes das quantidades do Pedido de Venda." + CRLF
	cMsgAviso += "Deseja ajustar o Pedido de acordo com as quantidades separadas?" + CRLF
	cMsgAviso += "Selecione uma das opções: Desmembrar / Eliminar Residuos / Cancelar" + CRLF

	nOpcao := Aviso("WISXSC6", cMsgAviso, {"Desmembrar", "Eliminar Residuos", "Cancelar"})

	If nOpcao == 1 .OR. nOpcao == 2
		If SoftLock("SC5")
			//Habilita o semaforo
			MsAguarde({|| lContinua := U_FESTX001()}, "Habilitando semaforo para liberação de estoque...")

			If lContinua
				If nOpcao == 1
					MsAguarde({|| lRetorno := U_PVDESWIS(cPedido)}, "Desmembrando Pedido e ajustando quantidades...")
				Else
					MsAguarde({|| lRetorno := AjustaPV(cPedido)}, "Ajustando quantidades separadas do Pedido de Venda e Eliminando Residuos...")
				EndIf

				//Pedido ajustado
				If lRetorno
					RecLock("SC5", .F.)
						SC5->C5_XWIS := "4"
					MsUnLock()
				EndIf

				//Libera o semaforo
				U_FESTX002()
			Else
				Aviso("WISXSC6", "Erro ao habilitar o semaforo para liberação de estoque." + CRLF + "Tente novamente em alguns minutos.", {"Fechar"})
			EndIf
			SC5->(MsUnlock())
		Else
			Help(" ", 1, "Help", "WISXSC6", "Pedido em manutenção por outro usuário.", 3, 0)
		EndIf
	EndIf

	RestArea(aAreaSC9)
	RestArea(aAreaSC6)
	RestArea(aAreaSC5)
	RestArea(aArea)

Return lRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} AjustaPV
Ajusta as quantidades do Pedido de Venda de acordo com as quantidades
separadas no WIS

@author  Guilherme Santos
@since   23/10/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
Static Function AjustaPV(cPedido)
	Local aCabec		:= {}
	Local aItens		:= {}
	Local aAux			:= {}
	Local nField		:= 0
	Local nPosQtV		:= SC6->(FieldPos("C6_QTDVEN"))
	Local nPosQtE		:= SC6->(FieldPos("C6_XQTEXP"))
	Local nPosWIS		:= SC5->(FieldPos("C5_XWIS"))
	Local aParam		:= {}
	Local aRetPar		:= {}
	Local cMotivo		:= "02"
	Local lRetorno 		:= .T.
	Local cCodUsr		:= RetCodUsr()
	Private lMsErroAuto	:= .F.

	Aadd(aParam, {1, "Motivo Cancelamento", cMotivo, NIL, NIL, "ZC7", , , .F.})

	If ParamBox(aParam, "Informe os parâmetros", @aRetPar,,,,,,,, .F., .F.)
		cMotivo := MV_PAR01

		DbSelectArea("SC5")
		DbSetOrder(1)		//C5_FILIAL, C5_NUM

		If SC5->(DbSeek(xFilial("SC5") + cPedido))
			//Monta o Array com os Campos do Cabecalho
			For nField := 1 to SC5->(FCount())
				If nField == nPosWIS
					Aadd(aCabec, { SC5->(FieldName(nField)), "4", NIL})		//Conferido
				Else
					Aadd(aCabec, { SC5->(FieldName(nField)), SC5->(FieldGet(nField)), NIL})
				EndIf
			Next nField

			DbSelectArea("SC6")
			DbSetOrder(1)		//C6_FILIAL, C6_NUM

			If SC6->(DbSeek(SC5->C5_FILIAL + SC5->C5_NUM))
				While !SC6->(Eof()) .AND. SC5->C5_FILIAL + SC5->C5_NUM == SC6->C6_FILIAL + SC6->C6_NUM

					aAux := {}

					For nField := 1 to SC6->(FCount())
						If !AllTrim(SC6->(FieldName(nField))) $ "|C6_VALOR|"
							If nField == nPosQtV .AND. SC6->C6_QTDVEN <> SC6->C6_XQTEXP
								If SC6->C6_XQTEXP == 0
									Aadd(aAux, { SC6->(FieldName(nField)), SC6->(FieldGet(nField)), NIL})
									Aadd(aAux, {"AUTDELETA", "S", SC6->C6_ITEM})
								Else
									Aadd(aAux, { SC6->(FieldName(nField)), SC6->(FieldGet(nPosQtE)), NIL})
								EndIf

								Aadd(aAux, { "C6_XUSRCAN", cCodUsr, NIL})
								Aadd(aAux, { "C6_XDTCANC", dDataBase, NIL})
								Aadd(aAux, { "C6_XMOTCAN", cMotivo, NIL})
								Aadd(aAux, { "C6_XHORCAN", Time(), NIL})
							Else
								If !Empty(SC6->(FieldGet(nField)))
									Aadd(aAux, { SC6->(FieldName(nField)), SC6->(FieldGet(nField)), NIL})
								EndIf
							EndIf
						EndIf
					Next nField

					Aadd(aItens, aAux)

					SC6->(DbSkip())
				End

				MsExecAuto({|x, y, z| MATA410(x, y, z)}, aCabec, aItens, 4)

				If lMsErroAuto
					lRetorno := .F.
					cMsgErro := MostraErro("\system\", "MATA410.LOG") + CRLF
					Aviso("WISXSC6", cMsgErro, {"Fechar"})
				Else
					DbSelectArea("ZC4")
					DbSetOrder(1)
					
					RecLock("ZC4", .T.)
						ZC4->ZC4_FILIAL := xFilial("ZC4")
						ZC4->ZC4_TAB    := "SC6"
						ZC4->ZC4_FILORI := SC5->C5_FILIAL
						ZC4->ZC4_REGORI := SC5->C5_NUM
						ZC4->ZC4_MOT    := cMotivo
						ZC4->ZC4_DATA   := dDataBase
					MsUnLock()

					U_HistPV(SC5->C5_NUM, "8", "Ajuste PV - Retorno WIS - Eliminação de resíduo")
					Aviso("WISXSC6", "Pedido " + SC5->C5_NUM + " ajustado com sucesso.", {"Fechar"})
				EndIf
			EndIf
		EndIf
	Else
		Aviso("WISXSC6", "A informação do Motivo é obrigatória. Pedido não ajustado.", {"Fechar"})
		lRetorno := .F.
	EndIf

Return lRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} WHENSC5
Valida a alteracao no campo do SC5

@author  Guilherme Santos
@since   25/11/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
User Function WHENSC5(cCampo)
	Local lRetorno 		:= .T.
	Local lAltera		:= IsInCallStack("A410Altera")
	Local lWMSSythex	:= SuperGetMV("BZ_WMS",, "") == "WIS"
	Local lWMSWinthr 	:= SuperGetMV("BZ_WMS",, "") == "WINTHOR"
	Local lFuncXFld		:= SuperGetMV("BZ_FUNXFLD",, .F.)

	//Valida os campos que estarão disponíveis para alteração.
	//A alteração só será permitida por fontes customizados. As outras rotinas não poderão alterar o pedido.
	If lAltera .And. lFuncXFld
		ZAX->(DBSetOrder(1)) //ZAX_FILIAL, ZAX_FUNCTI, ZAX_FIELD, R_E_C_N_O_, D_E_L_E_T_
		If !(ZAX->(DBSeek(xFilial("ZAX") + PadR(FUNNAME(), TamSX3("ZAX_FUNCTI")[1]) + cCampo)))
			If !(ZAX->(DBSeek(xFilial("ZAX") + PadR(FUNNAME(), TamSX3("ZAX_FUNCTI")[1]))))
				lRetorno := .F.
			Else
				If ! AllTrim(ZAX->ZAX_FIELD) == "*"
					lRetorno := .F.
				EndIf
			EndIf
		EndIf
	EndIf

	If (lWMSSythex .OR. lWMSWinthr) .And. lRetorno
		If lAltera
			If Type("l410Auto") == "U" .OR. !l410Auto
				If SC5->C5_XWIS $ "|2|4|"
					If !(cCampo $ SuperGetMV("BZ_XCPOSC5", NIL, "|C5_VOLUME1|C5_ESPECI1|C5_TPFRETE|C5_TRANSP|C5_MENNOTA|C5_XOBSFAT|C5_XOBSLOG|C5_NATUREZ|C5_XDESMEM|C5_XESP|C5_REDESP|C5_ESTRDP1|C5_CMURDP1|C5_TFRDP1|C5_XTPPREV|C5_XDTPFAT|C5_CONDPAG|C5_FRETE|C5_XEND|C5_XCEP|C5_XBAIRRO|C5_XCIDADE|C5_XEST|"))		//Campos para Alteracao
						lRetorno := .F.
					EndIf
				EndIf
			EndIf
		EndIf
	EndIf

Return lRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} WHENSC6
Valida a alteracao no campo do SC6

@author  Guilherme Santos
@since   25/11/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
User Function WHENSC6(cCampo)
	Local lRetorno 		:= .T.
	Local lAltera		:= IsInCallStack("A410Altera")
	Local lInclui		:= IsInCallStack("A410Inclui")
	Local lWMSSythex	:= SuperGetMV("BZ_WMS",, "") == "WIS"
	Local lWMSWinthr 	:= SuperGetMV("BZ_WMS",, "") == "WINTHOR"
	Local lFuncXFld		:= SuperGetMV("BZ_FUNXFLD",, .F.)

	//Valida os campos que estarão disponíveis para alteração.
	//A alteração só será permitida por fontes customizados. As outras rotinas não poderão alterar o pedido.
	If lAltera .And. lFuncXFld
		ZAX->(DBSetOrder(1)) //ZAX_FILIAL, ZAX_FUNCTI, ZAX_FIELD, R_E_C_N_O_, D_E_L_E_T_
		If !(ZAX->(DBSeek(xFilial("ZAX") + PadR(FUNNAME(), TamSX3("ZAX_FUNCTI")[1]) + cCampo)))
			If !(ZAX->(DBSeek(xFilial("ZAX") + PadR(FUNNAME(), TamSX3("ZAX_FUNCTI")[1]))))
				lRetorno := .F.
			Else
				If ! AllTrim(ZAX->ZAX_FIELD) == "*"
					lRetorno := .F.
				EndIf
			EndIf
		EndIf
	EndIf

	If (lWMSSythex .OR. lWMSWinthr) .And. lRetorno
		If lAltera
			If Type("l410Auto") == "U" .OR. !l410Auto
				If SC5->C5_XWIS $ "|2|4|"
					If !(cCampo $ SuperGetMV("BZ_XCPOSC6", NIL, "|C6_NUMPCOM|C6_ITEMPC|C6_XOBS|C6_PRCVEN|"))					//Campos para Alteracao
						lRetorno := .F.
					EndIf
				EndIf
			EndIf
		EndIf
	EndIf

	If lInclui .Or. lAltera
		If M->C5_TIPO == "D"
			If (cCampo $ SuperGetMV("BZ_XCPDSC6", NIL, "C6_PRCVEN|")) //Campos bloqueados para pedido tipo Devolução de Compras
				lRetorno := .F.
			EndIf
		EndIf
	EndIf

Return lRetorno

//-------------------------------------------------------------------
/*/{Protheus.doc} VldXWIS
Valida a alteracao manual do campo C5_XWIS

@author  Guilherme Santos
@since   17/02/2020
@version 12.1.25
/*/
//-------------------------------------------------------------------
User Function VldXWIS()
	Local lRetorno 		:= .T.
	Local lInclui		:= IsInCallStack("A410Inclui")
	Local lAltera		:= IsInCallStack("A410Altera")
	Local lWMSSythex 	:= SuperGetMV("BZ_WMS",, "") == "WIS"

	If lWMSSythex
		If Type("l410Auto") == "U" .OR. !l410Auto
			If lInclui
				If M->C5_XWIS $ "|" + WMS_C5_PICK + "|" + WMS_C5_CONF + "|" + WMS_C5_FATUR + "|"
					lRetorno := .F.
					Help(" ", 1, "Help", "VLDXWIS", "Não é permitida a alteração manual do Status do WIS para: " + "2-Picking / 4-Separado / 5-Faturado", 3, 0)
				EndIf
			EndIf
			If lAltera
				If SC5->C5_XWIS <> M->C5_XWIS .AND. M->C5_XWIS $ "|" + WMS_C5_PICK + "|" + WMS_C5_CONF + "|" + WMS_C5_FATUR + "|"
					lRetorno := .F.
					Help(" ", 1, "Help", "VLDXWIS", "Não é permitida a alteração manual do Status do WIS para: " + "2-Picking / 4-Separado / 5-Faturado", 3, 0)
				EndIf
			EndIf
		EndIf
	EndIf

Return lRetorno
