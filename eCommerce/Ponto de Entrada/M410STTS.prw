#include "protheus.ch"
#include "topconn.ch"
#include "WISStatus.ch"
#include "tbiconn.ch"

Static cDepart    := ""
Static lJob       := (GetRemoteType() == -1)

// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : M410STTS
// Descrição: Ponto de entrada executado após a gravação do pedido de vendas, em
//            todas as rotinas de inclusao, alteracao e exclusao de pedido de
//            vendas. Fonte:
//            tdn.totvs.com/pages/releaseview.action?pageId=6784155
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 27/11/13 | Felipe Raposo     | Inclusão de função para gravar informações de
//          |                   | impostos no cabeçalho e nos itens do pedido.
// ---------+-------------------+------------------------------------------------
User Function M410STTS()

Local aArea		:= GetArea()
Local aAreaSC5	:= SC5->(GetArea())
Local aAreaSC6	:= SC6->(GetArea())
Local aAreaSA1   := SA1->(GetArea())
Local lExclui    := IsInCallStack("A410Deleta")
Local lPedTMK    := IsInCallStack("U_TMKVFim")
Local cNumAtend  := SC5->C5_XATEND
Local cFilAtend  := SC5->C5_FILIAL
Local lNetSup    := !empty(SC5->C5_XIDVTV3)
Local nVlrMin	:= 0
Local lBloqueio	:= .F.
Local cMotivo	:= ""
Local lContrato	:= .F.
Local cContrato	:= ""
Local lVlrMin	:= SuperGetMv("PC_VLRMIN", NIL, .F.)		//Tratamento para Valor Minimo Frete CIF
Local lEnvTrk    := SuperGetMV("PC_ENVTRK",, .F.)  // Envia tracker de pedido aos contatos do cliente
Local cPedOrig	:= ""
Local nPMPNew	:= ""
Local lBlqPMP	:= .F.
Local cMsg		:= ""
Local lPMPPgto	:= SuperGetMV("PC_PMPGTO", , .F.)
Local cPedido	:= SC5->C5_NUM
Local nValMer	:= 0
Local nFrtCal	:= 0
Local nOpcao	:= 2
Local lExecAuto	:= .F.
Local lFine		:= ExistBlock("FINE410S")
Local cNBlqTPV	:= SuperGetMV("PC_NBLQTPV",,"BEP|BP1|DFQ|SRQ|RC2")	//Tipos de pedidos que nao bloqueia o pedido

// Se pedido vier do call-center, marca como inclusão de pedido.
Private INCLUI := If(IsInCallStack("TMKA271"), .T., INCLUI)

U_DebugMsg("Início do M410STTS (" + SC5->(C5_FILIAL + "-" + C5_NUM) + ")", "I")

If INCLUI .Or. ALTERA
	//Grava o número do pedido original
	If Empty(SC5->C5_XPEDORI)
		If RecLock("SC5", .F.)
			Do Case
				Case !empty(SC5->C5_XPEDPAI)
					U_PedOrig(SC5->C5_FILIAL, SC5->C5_XPEDPAI, @cPedOrig)
					If !Empty(cPedOrig)
						SC5->C5_XPEDORI := cPedOrig
					Else
						SC5->C5_XPEDORI := SC5->C5_NUM
					EndIf
				Case empty(SC5->C5_XPEDORI)
					SC5->C5_XPEDORI := SC5->C5_NUM
				Otherwise
					SC5->C5_XPEDORI := SC5->C5_NUM
			End Case
			SC5->(MSUnlock())
		EndIf
	EndIf

	If Empty(SC5->C5_CLIENT) .Or. Empty(SC5->C5_LOJAENT)
		If RecLock("SC5", .F.)
			SC5->C5_CLIENT := SC5->C5_CLIENTE
			SC5->C5_LOJAENT := SC5->C5_LOJACLI
			SC5->(MsUnlock())
		EndIf
	EndIf

	// Grava histórico do pedido.
	If INCLUI .And. !(IsInCallStack("A410PCopia"))
		If empty(SC5->C5_XPEDPAI)
			U_HistPV(SC5->C5_NUM, "1", "Pedido " + SC5->C5_NUM, "Valor de mercadorias: R$ " + AllTrim(Transform(SC5->C5_XVALMER, PesqPict('SC5', 'C5_XVALMER'))))
		Else
			U_HistPV(SC5->C5_NUM, "1", "Pedido desmembrado do " + SC5->C5_XPEDPAI, "Valor de mercadorias: R$ " + AllTrim(Transform(SC5->C5_XVALMER, PesqPict('SC5', 'C5_XVALMER'))))
		Endif
	ElseIf ALTERA
		// Se for desmembramento de pedido, não precisa gravar log aqui.
		If !IsInCallStack("U_PVDesmem")
			nValMer := U_XVALMERC(SC5->C5_FILIAL, SC5->C5_NUM)
			U_HistPV(SC5->C5_NUM, "2", "", "Valor de mercadorias: R$ " + AllTrim(Transform(nValMer, PesqPict('SC5', 'C5_XVALMER'))))
		Endif
	EndIf

	// Grava totalizadores específicos nas tabelas SC5/SC6.
	If !lPedTMK
		U_GrvImpPV()
		U_DebugMsg("U_GrvImpPV executado.")
	Endif

	// Verifica o crédito e o estoque do pedido em thread separada.
	U_PCrdEst(cEmpAnt, cFilAnt, cModulo, cPedido, .F.)

	// Grava no campo C5_MEMPAD informações tributárias da DANFE.
	U_RET_SM4(SC5->C5_NUM)
	U_DebugMsg("U_RET_SM4 executado.")

	// Preenche o departamento de vendas e a natureza do cliente no pedido.
	If INCLUI .And. Empty(Alltrim(SC5->C5_NATUREZ))// Grava a natureza do cliente no pedido.
		SA1->(dbSetOrder(1))  // A1_FILIAL, A1_COD, A1_LOJA.
		SA1->(msSeek(xFilial() + SC5->(C5_CLIENTE + C5_LOJACLI), .F.))

		RecLock("SC5", .F.)
		SC5->C5_NATUREZ := SA1->A1_NATUREZ
		SC5->(msUnlock())
	Endif

	//Não avalia na geração do Pedido pois já foi liberado através do orçamento
	If !IsInCallStack("U_PROMA030")
		//Bloqueio das Rotinas especificas de inclusao e alteracao dos Pedidos de Venda
		If IsInCallStack("U_PROMA065")
			DBSelectArea("ZAX")
			DbSetOrder(1)		//ZAX_FILIAL, ZAX_FUNCTI, ZAX_FIELD

			If ZAX->(DbSeek(xFilial("ZAX") + PadR(FunName(), TamSx3("ZAX_FUNCTI")[1])))
				If !(SC5->C5_XTIPO $ cNBlqTPV)	//Nao bloqueia para pedido de descarte
					If !Empty(ZAX->ZAX_USRLIB) .AND. AllTrim(ZAX->ZAX_USRLIB) != "*"
						RecLock("SC5", .F.)
							SC5->C5_XBLQCON := "D"
						MsUnlock()
						If INCLUI
							U_HistPV(SC5->C5_NUM, '7', 'Bloqueio de pedido por Regra', "Inclusão de PV Outros Tipos")
						EndIf

						If ALTERA
							U_HistPV(SC5->C5_NUM, '7', 'Bloqueio de pedido por Regra', "Alteração de PV Outros Tipos")
						EndIf

						Aviso("M410STTS", "Pedido Bloqueado para Reserva de Estoque, Separação e Faturamento. Liberação necessária pelo(s) usuário(s) " + AllTrim(ZAX->ZAX_USRLIB), {"Fechar"})
					EndIf
				EndIf
			EndIf
		Else
			If lVlrMin .AND. !lNetSup

				lBloqueio 	:= .F.
				lBlqPMP		:= .F.
				nPMPNew		:= 0
				cMotivo		:= ""
				lContrato	:= .F.
				cContrato	:= ""

				DbSelectArea("SC6")
				DbSetOrder(1)		//C6_FILIAL, C6_NUM, C6_ITEM

				If SC6->(DbSeek(SC5->C5_FILIAL + SC5->C5_NUM))
					While !SC6->(Eof()) .AND. SC5->C5_FILIAL + SC5->C5_NUM == SC6->C6_FILIAL + SC6->C6_NUM
						//Verifica se existe contrato para o Cliente + Produto
						U_MA020Ctr(SC5->C5_CLIENTE, SC5->C5_LOJACLI, SC6->C6_PRODUTO, NIL, @cContrato, NIL, .T., NIL)

						If !Empty(cContrato)
							lContrato := .T.
							Exit
						EndIf

						SC6->(DbSkip())
					End
				EndIf

				//Verifica se o orçamento tem valor de frete calculado
				SUA->(DBSetOrder(1))
				If SUA->(DBSeek(xFilial("SC5") + SC5->C5_XATEND))
					nFrtCal := SUA->UA_XFRTCAL
				EndIf

				If SC5->C5_TIPO == "N" .AND. (SC5->C5_TPFRETE == "C" .AND. Empty(SC5->C5_REDESP) .And. (SC5->C5_FRETE == 0 .Or. SC5->C5_FRETE < nFrtCal)) .AND. SC5->C5_XTIPO $ "|VAO|VNO|VTR|VZF|" .AND. !lContrato
					DbSelectArea("SA1")
					DbSetOrder(1)

					If SA1->(DbSeek(xFilial("SA1") + SC5->C5_CLIENTE + SC5->C5_LOJACLI))
						nVlrMin := U_MinFrete(SA1->A1_EST, SA1->A1_COD_MUN)
					EndIf

					If nVlrMin > 0
						If SC5->C5_XVALMER < nVlrMin
							lBloqueio 	:= .T.
							cMotivo		:= "Bloqueio por Valor Minimo para Pedidos - Frete CIF. Valor do Pedido: " + AllTrim(Str(SC5->C5_XVALMER, 9, 2)) + " Vlr.Min.: " + AllTrim(Str(nVlrMin, 9, 2)) + CRLF
						EndIf
					Else
						lBloqueio := .T.
						cMotivo := "Bloqueio por Valor Minimo para Orçamentos - Frete CIF. Valor Minimo para o Municipio " + SA1->A1_COD_MUN + " / Estado " + SA1->A1_COD_MUN + " não cadastrado." + CRLF
					EndIf
				EndIf
			EndIf

			//Bloqueio por prazo médio da condição de pagamento
			If ALTERA .And. !lNetSup .AND. SC5->C5_TIPO == "N" .AND. SC5->C5_XTIPO $ "|VAO|VNO|VTR|VZF|"
				//Avalia alteração do Prz. Méd. de Pagto
				nPMPNew := U_xPMPgto(SC5->C5_XTOTAL, SC5->C5_CONDPAG, SC5->C5_EMISSAO)
				If nPMPNew > SC5->C5_XPMPCAL .And. nPMPNew > SC5->C5_XPMPGTO
					RecLock("SC5", .F.)
						SC5->C5_XPMPCAL := nPMPNew
					SC5->(MSUnlock())
					If lPMPPgto
						lBlqPMP := .T.
						cMotivo := "Bloqueio por alteração do prazo médio de pagamento de " + cValToChar(SC5->C5_XPMPCAL) + " para " + cValToChar(nPMPNew) + "." + CRLF
					EndIf
				ElseIf nPMPNew > SC5->C5_XPMPGTO .And. Empty(SC5->C5_XREGRA) //Se o PMP novo não teve liberação e é maior que o PMP do cliente, bloqueia.
					RecLock("SC5", .F.)
						SC5->C5_XPMPCAL := nPMPNew
					SC5->(MSUnlock())
					If lPMPPgto
						lBlqPMP := .T.
						cMotivo := "Bloqueio por prazo médio de pagamento acima do permitido." + CRLF
					EndIf
				EndIf
			EndIf

			If lBloqueio .Or. lBlqPMP
				If Type("l410Auto") == "U"
					If lBloqueio
						cMsg := "Pedido não atinge o Valor do Ticket Mínimo por Frete. " + CRLF
					EndIf
					If lBlqPMP
						cMsg += "Alteração na condição de pagamento. " + CRLF
					EndIf
					cMotivo := cMsg + cMotivo
					Aviso("M410STTS", cMotivo, {"Fechar"})
				ElseIf Type("l410Auto") <> "U" .AND. l410Auto
					cMotivo += "Pedido desmembrado." + CRLF
				EndIf

				If SC5->C5_XBLQCON != "C" //Bloqueio por alteração sobressai os outros
					RecLock("SC5", .F.)
						If lBloqueio .And. lBlqPMP
							SC5->C5_XBLQCON := "5"
						ElseIf lBloqueio .And. !lBlqPMP
							SC5->C5_XBLQCON := "3"
						ElseIf !lBloqueio .And. lBlqPMP
							SC5->C5_XBLQCON := "7"
						EndIf
						SC5->C5_XOBSBLQ	:= SC5->C5_XOBSBLQ + CRLF + cMotivo + CRLF + UsrRetName(RetCodUsr()) + " - Data: " + DtoC(dDatabase) + " - " + Time() + CRLF
					MsUnlock()
				EndIf

				// Elimino a regra condicional pois gerou um novo bloqueio no orçamento.
				DbSelectArea("ZC8")
				DbSetOrder(4) //ZC8_FILIAL, ZC8_PEDIDO
				Do While DbSeek(xFilial("ZC8") + SC5->C5_NUM)
					RecLock("ZC8", .F.)
					DbDelete()
					msUnlock()
				EndDo
			EndIf
		EndIf
	EndIf
Endif

If lExclui
	U_HistPV(SC5->C5_NUM, "8")

	//INICIO: CONTROLE DE FCI
	If SC5->C5_TIPO $ "N|D" .And. !Empty(SC6->C6_FCICOD)

		cQuery := " SELECT "+ CRLF
		cQuery += " 	ZAK.R_E_C_N_O_ AS ZAKRECNO "+ CRLF
		cQuery += " FROM "+RetSqlName("ZAK")+" ZAK (NOLOCK) "+ CRLF
		cQuery += " WHERE "+ CRLF
		cQuery += " 	ZAK.ZAK_FILIAL = '"+xFilial("ZAK")+"' "+ CRLF
		cQuery += " 	AND ZAK.ZAK_PROD = '"+SC6->C6_PRODUTO+"' "+ CRLF
		cQuery += " 	AND ZAK.ZAK_CODFCI = '"+SC6->C6_FCICOD+"' "+ CRLF
		cQuery += " 	AND ZAK.D_E_L_E_T_ = ' ' "+ CRLF

		cTMP1 := MPSysOpenQuery(cQuery)

		If (cTMP1)->(!EOF())
			DbSelectArea("ZAK")
			DbGoTo((cTMP1)->ZAKRECNO)
			RecLock("ZAK",.F.)
				ZAK->ZAK_SALDO := ZAK->ZAK_SALDO + SC6->C6_QTDVEN
				ZAK->(MsUnlock())
		EndIf

		(cTMP1)->(DbCloseArea())
	EndIf
	//FIM: CONTROLE DE FCI
Endif

//finaliza a agenda do operador
if INCLUI .And. lPedTMK
	u_gravaAgenda(cFilAtend,cNumAtend,"P")
endIf

//adiciona o cliente na carteira NetSuprimentos
if INCLUI .and. lNetSup
	cartWeb()
endIf

// Seleciona os contatos para envio de tracker do pedido
If lEnvTrk
	If INCLUI .And. !lNetSup
		If !lJob
			If SC5->C5_TRANSP <> "000010" //Cliente Retira
				If SC5->C5_XTIPO $ 'VAO*VNO*VTR*VZF' //Tipos somente de vendas
					If Empty(SC5->C5_XPEDPAI) //Pedidos desmembrado não envia o passo de pedido emitido.
						U_PROMA430()
					Else
						ZATPAI()
					EndIf
				EndIf
			Endif
		Endif
	Endif
Endif

If (INCLUI .Or. ALTERA) .And. Left(SC5->C5_XTIPO, 1) == "V"
	//Gravação de custo médio
	SC6->(DBSetOrder(1)) //C6_FILIAL, C6_NUM
	If SC6->(DBSeek(xFilial("SC6") + cPedido))
		While SC6->(!EOF()) .And. SC6->(C6_FILIAL + C6_NUM) == xFilial("SC5") + cPedido
			If Empty(SC6->C6_XCUSMED)
				RecLock("SC6", .F.)
				SC6->C6_XCUSMED := U_xCusMed(SC6->C6_FILIAL, SC6->C6_PRODUTO, SC6->C6_LOCAL, .T.)[1]
				SC6->(MSUnlock())
				SUB->(DBSetOrder(3)) //UB_FILIAL, UB_NUMPV, UB_ITEMPV, R_E_C_N_O_, D_E_L_E_T_
				If !Empty(cNumAtend) .And. SUB->(DBSeek(xFilial("SUB") + cNumAtend + SC6->C6_ITEM))
					If Empty(SUB->UB_XCUSMED)
						RecLock("SUB",.F.)
							SUB->UB_XCUSMED := U_xCusMed(SUB->UB_FILIAL, SUB->UB_PRODUTO, SUB->UB_LOCAL, .T.)[1]
						SUB->(MsUnlock())
					EndIf
				EndIf
			EndIf
			SC6->(DBSkip())
		EndDo
	EndIf
EndIf

If (ALTERA .Or. lExclui) .And. u_ARMESPECIAL()

	cQuery := " SELECT DISTINCT "+ CRLF
	cQuery += " 	SC6.C6_XNUMCTR "+ CRLF
	cQuery += " 	,SC6.C6_XITCTRL "+ CRLF
	cQuery += " FROM "+RetSqlName("SC6")+" SC6 (NOLOCK) "+ CRLF
	cQuery += " WHERE "+ CRLF
	cQuery += " 	SC6.C6_FILIAL = '"+xFilial("SC6")+"' "+ CRLF
	cQuery += " 	AND SC6.C6_NUM = '"+SC5->C5_NUM+"' "+ CRLF
	cQuery += " 	AND SC6.D_E_L_E_T_ = ' ' "+ CRLF

	cTMP1 := MPSysOpenQuery(cQuery)

	If (cTMP1)->(!EOF()) .And. !Empty((cTMP1)->C6_XNUMCTR) .And. !Empty((cTMP1)->C6_XITCTRL)
		// Atualiza saldo do processo
		U_MA670SLD((cTMP1)->C6_XNUMCTR, (cTMP1)->C6_XITCTRL, .T.)
	EndIf

	(cTMP1)->(DbCloseArea())
EndIf

If (INCLUI .OR. ALTERA) .AND. !IsInCallStack("U_PVDesmem") .AND. SC5->C5_XTPPREV <> "2"	//.AND. Empty(SC5->C5_XPEDPAI) .AND. !U_ChkDesm(SC5->C5_NUM)
	//Atualiza a previsao de Faturamento do Pedido (Não atualizar pedidos desmembrados)
	lAtuPrevIni := if(ValType(lAtuPrevIni) <> "U",lAtuPrevIni,.f.)
	U_PrevEntr(2, .F., "", 0, NIL, NIL, NIL, NIL, SC5->C5_NUM, SC5->C5_TRANSP,(ALTERA .AND. lAtuPrevIni))
EndIf

//Transferencias
If IsInCallStack("U_MA065002")
	//Atualiza o Pedido de Compra na Unidade de Destino
	MsAguarde({|| U_PROMA066(SC5->C5_FILIAL, SC5->C5_NUM, lExclui)}, "Atualizando Pedido de Compra na Unidade de Destino da Transfencia...")
EndIf

If INCLUI .Or. ALTERA
	If RecLock("SC5", .F., .F., .T., .F.)
		SC5->C5_XDTALTE := Date()
		SC5->C5_XHRALTE := Time()
		SC5->(MsUnlock())
	EndIf
EndIf

If lFine
	lExecAuto := Type("l410Auto") <> "U" .AND. l410Auto

	If INCLUI
		nOpcao := 3
	ElseIf ALTERA
		nOpcao := 4
	Else
		nOpcao := 5
	EndIf

	ExecBlock("FINE410S",.F.,.F., {lExecAuto, nOpcao})
EndIf

U_DebugMsg("Fim do M410STTS (" + SC5->(C5_FILIAL + "-" + C5_NUM) + ")", "F")

RestArea(aAreaSA1)
RestArea(aAreaSC6)
RestArea(aAreaSC5)
RestArea(aArea)

Return


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : PCrdEst
// Descrição: Verifica o crédito e o estoque do pedido.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 28/11/17 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function PCrdEst(cRPCEmp, cRPCFil, cRPCMod, cPedido, lNewJob)

Local cEstoque   := ""
Local cObserv    := ""
Local lJobInt	 := ""

U_DebugMsg("Início do PCrdEst (" + cRPCFil + "-" + cPedido + ")", "I")

// Abre o ambiente.
Default lNewJob := .T.
If lNewJob
	U_DebugMsg("PCrdEst - abrindo módulo " + cRPCMod + " - filial " + cRPCEmp + "/" + cRPCFil + "...")
	Prepare Environment empresa cRPCEmp filial cRPCFil modulo cRPCMod
Endif

lJobInt	 := SuperGetMV("WIS_JOBINT", NIL, .T.)

// Posiciona o pedido de venda.
SC5->(dbSetOrder(1))  // C5_FILIAL, C5_NUM.
If SC5->(dbSeek(xFilial() + cPedido, .F.))
	// Se for inclusão de pedido website, envia automaticamente para o WIS.
	If empty(SC5->C5_XIDVTV3)
		// Analisa o crédito do pedido.
		U_MA060Crd(.T., .F.)
		U_DebugMsg("U_MA060Crd executado.")

		// Se o pedido já está no WIS, refaz a liberação do pedido.
		If SC5->(C5_XWIS == WMS_C5_PICK .or. C5_XWIS == WMS_C5_CONF .or. C5_XWIS == WMS_C5_ERRO)
			U_LibPV(SC5->C5_NUM, .T., .T.)
			U_DebugMsg("U_LibPV executado.")
		Endif
	Else
		// Preenche o campo xCanal.
		Reclock("SC5",.F.)
		SC5->C5_XCANAL := "01"
		SC5->C5_XTIPOPV := "1"
		// Preenche condições de pagamento e liberação de crédito
		SC5->C5_CONDPAG := "888"  // Condição de pagamento negociada (tipo 9).
		SC5->C5_XDTLIB	:= Date()
		If Empty(SC5->C5_XCRDLIB)
			SC5->C5_XCONLIB := "888"
			SC5->C5_XCRDLIB := 'L'
			SC5->C5_XVLDCRD := Date() + 365
			SC5->C5_XVLRLIB := SC5->C5_XTOTAL
			SC5->C5_XUSRLIB := "Sistema"
		EndIf

		// Remove a transportadora se o campo C5_XCDMUNE estiver em branco. Obrigando a emissão a digitar novamente a transportadora.
		If Empty(SC5->C5_XCDMUNE)
			SC5->C5_TRANSP := ""
			SC5->C5_REDESP := ""
		EndIf
		SC5->(MsUnlock())

		// Acerta os campos de pagamento do pedido.
		PVWebPG()
		U_DebugMsg("PVWebPG executado.")

		// Verifica se não há outro pedido com o mesmo número NetSuprimentos.
		If PWebExcl(SC5->C5_NUM, SC5->C5_XIDVTV3, @cObserv)
			// Analisa o crédito do pedido.
			U_MA060Crd(.T., .F.)
			U_DebugMsg("U_MA060Crd executado.")

			// Libera o pedido.
			U_LibPV(SC5->C5_NUM, .T., .T.)
			U_DebugMsg("U_LibPV executado.")

			// Envia para o WMS.
			If SuperGetMV("PC_WEBWMS",, .T.)
				If lJobInt
					U_WISJINC("/interfacewis/entrada/pedido", "SC5", SC5->(Recno()), "U_WISSC5", SC5->C5_FILIAL, WMS_PV_ENVIAR)
				Else
					U_WISSC5(WMS_PV_ENVIAR, .T.)
				EndIf
				U_DebugMsg("U_WISSC5 executado.")
			Endif
		Else
			U_HistPV(SC5->C5_NUM, "4", "Pedido duplicado", cObserv, "Sistema")
		Endif
	Endif

	// Acerta o status do pedido quanto ao estoque.
	U_EstPV(.T., SC5->C5_NUM,, @cEstoque)
	RecLock("SC5", .F.)
	SC5->C5_XESTOQ := cEstoque
	SC5->(msUnLock())
	U_DebugMsg("U_EstPV executado.")

	// Tratamento da comissão nos itens do pedido de venda.
	PComItem()
	U_DebugMsg("PComItem executado.")
Endif

// Fecha o ambiente.
If lNewJob
	U_DebugMsg("PCrdEst - fechando ambiente...")
	Reset Environment
Endif

U_DebugMsg("Fim do PCrdEst (" + cRPCFil + "-" + cPedido + ")", "F")

Return


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : PWebExcl
// Descrição: Verifica se o pedido NetSuprimentos não está duplicado.
// Retorno  : Lógico, indicando se o pedido é exclusivo.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 05/09/16 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
Static Function PWebExcl(cPedido, cIDVTex, cObserv)

Local lRet       := .T.
Local cAlias     := Alias()
Local aArea      := {}

Local cQuery     := ""
Local cAliasTRB  := ""

// Salva áreas de trabalho.
If empty(cAlias)
	cAlias := "SX3"
	dbSelectArea(cAlias)
Endif
aArea := sGetArea()

cQuery := "select top 1 SC5.C5_NUM PEDIDO "
cQuery += "from " + RetSQLName("SC5") + " SC5 with (noLock) "
cQuery += "where SC5.D_E_L_E_T_ = ' ' "
cQuery += "and SC5.C5_FILIAL  = '" + xFilial("SC5") + "' "
cQuery += "and SC5.C5_XIDVTV3 = '" + cIDVTex + "' "
cQuery += "and SC5.C5_NUM     <> '" + cPedido + "' "
cAliasTRB := MPSysOpenQuery(cQuery)

If (cAliasTRB)->(!eof())
	lRet := .F.
	cObserv := "Pedido NetSuprimentos duplicado: " + (cAliasTRB)->PEDIDO
Endif
(cAliasTRB)->(dbCloseArea())

// Restaura areas de trabalho.
(cAlias)->(sRestArea(aArea))
dbSelectArea(cAlias)

Return lRet


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : PVWebPG
// Descrição: Acerta vencimentos dos títulos de pedidos web.
// Retorno  : Lógico
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 03/06/16 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
Static Function PVWebPG()

Local dVazio     := stod("")
Local cOrderB2B  := RTrim(SC5->C5_XIDVTV3)
Local cGetParms  := ""
Local oPedido, aPagData[0], aPagamentos[0]
Local dEmissao   := dVazio
Local cSequence  := ""
Local dRecebIni  := dVazio
Local dRecebPar  := dVazio
Local cStsCode   := ""

Local nParcelas  := 0
Local nVlrPed    := 0
Local aRateio    := {}
Local nRateio    := 0

Local aParcelas  := {}
Local nX, nY, nZ, nPar
Local nDiasAcr	 := 0

// Busca o pedido na B2B.
FWJsonDeserialize(U_VTEXGet("PROT", "/api/oms/pvt/orders/" + cOrderB2B, cGetParms, "orders\PROT\" + cOrderB2B, @cStsCode), @oPedido)

// Emissão do pedido.
dEmissao  := stod(StrTran(Left(oPedido:creationDate, 10), "-", ""))
nVlrPed   := oPedido:value / 100
cSequence := oPedido:sequence

// Pega os pagamentos efetuados nessa venda.
aPagData := oPedido:paymentData:transactions

// Rateia valores dos pagamentos.
For nY := 1 to len(aPagData)
	If aPagData[nY]:isActive
		aPagamentos := aPagData[nY]:payments
		For nZ := 1 to len(aPagamentos)
			aAdd(aRateio, aPagamentos[nZ]:value)
		Next nZ
	Endif
Next nY

If len(aRateio) > 0
	aRateio := U_Rateio(nVlrPed, aRateio,, 2)
EndIf

// Calcula a data de recebimento de cada pagamento.
nRateio := 1
For nY := 1 to len(aPagData)
	If aPagData[nY]:isActive
		aPagamentos := aPagData[nY]:payments
		For nZ := 1 to len(aPagamentos)
			dRecebIni := DataValida(dEmissao + 35)
			nParcelas := max(1, aPagamentos[nZ]:installments)

			// Quebra o valor entre as parcelas.
			aParcPag := array(nParcelas); aFill(aParcPag, 1)
			aParcPag := U_Rateio(aRateio[nRateio], aParcPag,, 2)
			nRateio++

			// Adiciona as parcelas.
			For nX := 1 to len(aParcPag)
				dRecebPar := DataValida(MonthSum(dRecebIni, nX - 1))
				nPar := aScan(aParcelas, {|x| x[1] == dRecebPar})
				If nPar = 0
					aAdd(aParcelas, {dRecebPar, aParcPag[nX]})
				Else
					aParcelas[nPar, 2] += aParcPag[nX]
				Endif
			Next nX
		Next nZ
	Endif
Next nY

// Se houver mais que quatro parcelas, joga todo o valor na quarta, com o último vencimento.
aSort(aParcelas,,, {|x, y| x[1] < y[1]})
If len(aParcelas) > 4
	For nX := 5 to len(aParcelas)
		aParcelas[4, 1] := aParcelas[nX, 1]
		aParcelas[4, 2] += aParcelas[nX, 2]
	Next nX
Endif

// Acerta o dia de recebimento, de acordo com o período.
For nX := 1 to len(aParcelas)
	If day(aParcelas[nX, 1]) < 16
		aParcelas[nX, 1] := FirstDay(aParcelas[nX, 1]) + 19  // Dia 20 do mês corrente.
	Else
		aParcelas[nX, 1] := LastDay(aParcelas[nX, 1]) + 05   // Dia 05 do mês posterior.
	Endif
Next nX

//Atualiza as datas do array se já estiverem ultrapassadas. João 27/01/2022
If Len(aParcelas) >= 1
	If aParcelas[1,1] < Date()
		nDiasAcr := DateDiffDay(Date(),aParcelas[1,1])
		For nX := 1 To Len(aParcelas)
			aParcelas[nX][1] := DaySum(aParcelas[nX][1], nDiasAcr)
		Next nX
	EndIf
EndIf

// Acerta as parcelas no pedido.
RecLock("SC5", .F.)
SC5->C5_XIDVTEX := val(cSequence)
SC5->C5_DATA1   := If(len(aParcelas) >= 1, aParcelas[1, 1], dVazio)
SC5->C5_PARC1   := If(len(aParcelas) >= 1, aParcelas[1, 2], 0)
SC5->C5_DATA2   := If(len(aParcelas) >= 2, aParcelas[2, 1], dVazio)
SC5->C5_PARC2   := If(len(aParcelas) >= 2, aParcelas[2, 2], 0)
SC5->C5_DATA3   := If(len(aParcelas) >= 3, aParcelas[3, 1], dVazio)
SC5->C5_PARC3   := If(len(aParcelas) >= 3, aParcelas[3, 2], 0)
SC5->C5_DATA4   := If(len(aParcelas) >= 4, aParcelas[4, 1], dVazio)
SC5->C5_PARC4   := If(len(aParcelas) >= 4, aParcelas[4, 2], 0)
SC5->(msUnLock())

Return


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Fabio Luiz Gesser
// Modulo   : Faturamento / Call Center
// Função   : PComItem
// Descrição: Tratamento da Comissão de vendedores interno/externo nos itens do pedido
//            A3_TIPO = I-Vendedor Interno / E=Vendedor Externo
//            Quando o tipo do vendedor = externo, pegar a comissão do produto se existir,
//            caso contrario pegar do vendedor.
// Retorno  : Nenhum
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 21/02/17 | Fabio Gesser      | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
Static Function PComItem()

Local nComis := 0

If !empty(SC5->C5_VEND1)
	DbSelectArea("SA3")
	SA3->(dbSetOrder(1))  // A3_FILIAL, A3_COD.
	If SA3->(dbSeek(xFilial() + SC5->C5_VEND1, .F.))
		SC6->(dbSetOrder(1))  // C6_FILIAL, C6_NUM, C6_ITEM, C6_PRODUTO.
		SC6->(dbSeek(xFilial() + SC5->C5_NUM, .F.))
		Do While SC6->(!eof() .and. C6_FILIAL + C6_NUM == xFilial() + SC5->C5_NUM)

			If SA3->A3_TIPO == 'E'
				// Verifico se existe comissão no produto, se existir pego a comissão caso contratio pego do vendedor.
				SB1->(dbSetOrder(1))  // B1_FILIAL, B1_COD.
				SB1->(dbSeek(xFilial() + SC6->C6_PRODUTO, .F.))
				If SB1->B1_COMIS > 0
					nComis := SB1->B1_COMIS
				Else
					nComis := SA3->A3_COMIS
				Endif
			Else  // Quando tipo de vendedor estiver vazio, pego percentual do interno
				nComis := SA3->A3_COMIS
			Endif

			// Gravo a comissão no item do pedido
			RecLock("SC6", .F.)
			SC6->C6_COMIS1 := nComis
			SC6->(msUnLock())

			SC6->(Dbskip())
		EndDo

		// Zero a comissão no cabeçalho do pedido pois estou tratando no item do pedido de venda
		RecLock("SC5", .F.)
		SC5->C5_COMIS1 := 0
		SC5->(msUnLock())
	Endif
Endif

Return

// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : João Leão
// Modulo   : Faturamento / Call Center
// Função   : gravaAgenda
// Descrição: Encerra a agenda do orçamento passado no parâmetro.
// Retorno  : Nenhum
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 21/02/18 | João Leão         | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
user function gravaAgenda(cFilAtend,cNumAtend,cTipo)
Local aZAF  := ZAF->(getArea())
Local aArea := getArea()

ZAF->(DbSetOrder(2))  // ZAF_FILIAL, ZAF_NUMORC, ZAF_ATEND.
if ZAF->(DbSeek(cFilAtend + cNumAtend + "2"))
	RecLock("ZAF",.F.)
		ZAF->ZAF_ATEND  := "1"
		ZAF->ZAF_DTARET := dDatabase
		ZAF->ZAF_HORRET := Time()
		ZAF->ZAF_USRRET := __cUserId
		if cTipo == "P"
			ZAF->ZAF_HISTOR := AllTrim(ZAF->ZAF_HISTOR) + CRLF + "Data " + dToc(dDataBase) + CRLF + "Agenda encerrada pois foi gerado o pedido " + SUA->UA_NUMSC5
		elseIf cTipo == "C"
			ZAF->ZAF_HISTOR := AllTrim(ZAF->ZAF_HISTOR) + CRLF + "Data: " + Dtoc(dDataBase) + CRLF + "Encerramento da agenda via cancelamento total de orçamento"
		endIf
	ZAF->(MsUnLock())
endif

restArea(aZAF)
restArea(aArea)

Return

// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : João Leão
// Modulo   : Faturamento / Call Center
// Função   : cartWeb
// Descrição: Adiciona o cliente Net na carteira Web.
// Retorno  : Nenhum
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 13/03/18 | João Leão         | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
static function cartWeb()

local aZA6       := ZA6->(getArea())
local aArea      := getArea()
local cCartWeb   := superGetMV("PC_CARTWEB", ,"000157")
local cQuery     := ""
local cAliasTemp := ""
local cFiliais	 := ""

u_Filiais(@cFiliais)

//Pesquisa se o cliente não existe carteira
cQuery := "SELECT ZA7_CART CART" + CRLF
cQuery += "FROM " + retSQLName("ZA7") + " ZA7 WITH(NOLOCK) " + CRLF
cQuery += "WHERE ZA7.D_E_L_E_T_ = ' ' " + CRLF
cQuery += "AND ZA7_FILIAL IN (" + cFiliais + ") " + CRLF
cQuery += "AND ZA7_CLIENT = '" + SC5->C5_CLIENTE + "' " + CRLF
cQuery += "AND ZA7_LOJA = '" + SC5->C5_LOJACLI + "' " + CRLF
cQuery += "AND ZA7_DEPVEN = '01' " + CRLF
cAliasTemp := MPSysOpenQuery(cQuery)

if (cAliasTemp)->(eof())
	(cAliasTemp)->(dbCloseArea())

	//Localiza o operador da carteira web
	cQuery := "SELECT ZA6_FILIAL FILIAL, ZA6_COD COD " + CRLF
	cQuery += "FROM " + retSQLName("ZA6") + " ZA6 WITH (NOLOCK) " + CRLF
	cQuery += "WHERE ZA6.D_E_L_E_T_ = '' " + CRLF
	cQuery += "AND ZA6_FILIAL IN (" + cFiliais + ") " + CRLF
	cQuery += "AND ZA6_COD = '" + cCartWeb + "' " + CRLF
	cAliasTemp := MPSysOpenQuery(cQuery)

	//Grava o cliente na carteira web
	if (cAliasTemp)->(!eof())
		ZA6->(dbSetOrder(1))//ZA6_FILIAL, ZA6_COD
		if ZA6->(dbSeek((cAliasTemp)->(FILIAL + COD)))
			RecLock("ZA7",.T.)
				ZA7->ZA7_FILIAL := ZA6->ZA6_FILIAL
				ZA7->ZA7_CLIENT := SC5->C5_CLIENTE
				ZA7->ZA7_LOJA   := SC5->C5_LOJACLI
				ZA7->ZA7_OPERAD := ZA6->ZA6_OPERAD
				ZA7->ZA7_NOPER  := ZA6->ZA6_NOPER
				ZA7->ZA7_CART   := ZA6->ZA6_COD
				ZA7->ZA7_DEPVEN := ZA6->ZA6_DEPVEN
			ZA7->(msUnlock())
			//Incrementa o contador de clientes
			RecLock("ZA6",.F.)
				ZA6->ZA6_QTDCLI := ZA6->ZA6_QTDCLI + 1
			ZA6->(msUnlock())
		endIf
	endIf
endIf

(cAliasTemp)->(dbCloseArea())

restArea(aZA6)
restArea(aArea)

return

// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Victor Dessunte
// Modulo   : Faturamento / Call Center
// Função   : ZATPAI
// Descrição: Grava ZAT sem a necessidade de selecionar o contato.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------

Static Function ZATPAI()

Local aAreaSC5 	:= SC5->(GetArea())
Local aZat		:= {}
Local _nX		:= 0

	ZAT->(DbSetOrder(1))  // ZAT_FILIAL + ZAT_PEDIDO + ZAT_CONTAT
	ZAT->(DbSeek(xFilial("ZAT") + SC5->C5_XPEDPAI))
	While ZAT->(!EOF()) .And. xFilial("ZAT") == ZAT->ZAT_FILIAL .And. SC5->C5_XPEDPAI == ZAT->ZAT_PEDIDO
		AADD(aZAT,{ZAT->ZAT_FILIAL,ZAT->ZAT_CONTAT})
		ZAT->(dbSkip())
	End
	For _nX := 1 To Len(aZAT)
		RecLock("ZAT",.T.)
		ZAT->ZAT_FILIAL := aZAT[_nX,1]
		ZAT->ZAT_CONTAT := aZAT[_nX,2]
		ZAT->ZAT_PEDIDO := SC5->C5_NUM
		ZAT->ZAT_ETAPA  := '01'  // Pedido incluído
		ZAT->ZAT_ENVIA	:= '2'   // Marca como enviado para não enviar pedidos desmembrados.
		ZAT->(MsUnLock())
	Next _nX

RestArea(aAreaSC5)

Return
