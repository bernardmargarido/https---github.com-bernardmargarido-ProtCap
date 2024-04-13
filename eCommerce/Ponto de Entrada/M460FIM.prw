#include "protheus.ch"

// Definição dos status das amostras.
#define SOLICITACAO     "1"
#define APROVADA        "2"
#define PENDENTE        "3"
#define FECHADA         "0"

Static nTamRef    := val(Substr(GetMv("MV_MASCGRD"), 1, 2))

// ##############################################################################
// Projeto  : PROT-CAP
// Autor    : Eduardo Zanardo
// Modulo   : Faturamento
// Função   : M460FIM
// Descrição: P.E. chamado após a gravação da NF de saída, e fora da transação.
// Retorno  : Lógico, validando ou não a operação.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descrição
// ---------+-------------------+------------------------------------------------
// 09/06/08 | Eduardo Zanardo   | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function M460FIM()

Local lRet       := .T.
Local cAlias     := Alias()
Local aArea      := {}

Local lDevol     := (SF2->F2_TIPO $ "DB")
Local aPedidos   := {}
Local aSolAmost  := {}
Local aAmostra   := {}
Local cTipoFec   := ""
Local cMsgNfs    := ""
Local cMsg001    := " - PRESTADOR CADASTRADO NA CPOM"
Local cMsg002    := " - O SERVICO PRESTADO NAO SE CARACTERIZA COMO CESSAO DE MAO DE OBRA, DESQUALIFICANDO A RETENCAO DE INSS"
Local lFatur     := .T.
Local lPedWeb    := .F.
Local nX
Local _nDiasPR	 := SuperGetMV("BZ_XDIASPR",,20)  // Dias para protesto padrao no titulo a receber
Local cAtuCpo1	 := SuperGetMV("M460FIM01",,"N") // Atualiza campo A1_ULTCOM ? S/N

// Guarda posição das tabelas.
If empty(cAlias)
	cAlias := "SX3"
	dbSelectArea(cAlias)
Endif
aArea := sGetArea()
sGetArea(aArea, "SA1")
sGetArea(aArea, "SC5")
sGetArea(aArea, "SC6")
sGetArea(aArea, "SD2")
sGetArea(aArea, "SE1")
sGetArea(aArea, "SF2")

// Posiciona o cliente.
If cAtuCpo1 == "S"
	If !lDevol
		SA1->(dbSetOrder(1))  // A1_FILIAL, A1_COD, A1_LOJA.
		If SA1->(msSeek(xFilial() + SF2->(F2_CLIENTE + F2_LOJA), .F.))
			//Nesse momento, há um erro no ERP em que não está sendo grava a data da última compra (A1_ULTCOM)
			//Por esse motivo, está criado a atualização abaixo, até que se resolva o problema, a pedido do
			//gestor Rafael Basso. João Leão 25/10/2021
			If RecLock("SA1",.F.)
					SA1->A1_ULTCOM := SF2->F2_EMISSAO
				SA1->(MsUnLock())
			EndIf
		EndIf
	Endif
EndIf

// Posiciona pedido de vendas.
SD2->(dbSetOrder(3))  // D2_FILIAL, D2_DOC, D2_SERIE, D2_CLIENTE, D2_LOJA, D2_COD, D2_ITEM.
SD2->(dbSeek(xFilial() + SF2->(F2_DOC + F2_SERIE + F2_CLIENTE + F2_LOJA), .F.))
SC5->(dbSetOrder(1))  // C5_FILIAL, C5_NUM.
SC5->(dbSeek(xFilial() + SD2->D2_PEDIDO, .F.))

// Grava o campo F2_XDEPVEN
If !Empty(SC5->C5_XDEPVEN)
	RecLock("SF2",.F.)
		Replace SF2->F2_XDEPVEN With SC5->C5_XDEPVEN
	SF2->(msUnlock())
EndIf

lPedWeb := !empty(SC5->C5_XIDVTV3)

// Grava o pedido Vtex
If lPedWeb
	U_EcVldNF(SF2->F2_DOC,SF2->F2_SERIE,SC5->C5_XIDVTV3)
	/*
	RecLock("SF2",.F.)
		Replace SF2->F2_XIDVTV3 With SC5->C5_XIDVTV3
	SF2->(msUnlock())
	*/

EndIf
// Ponto de entrada para integração de pedidos NetSuprimentos (web).
If !lDevol
	If ExistBlock("ADM460FIM")
		ExecBlock("ADM460FIM", .F., .F.)
	Endif
Endif

// Gravo etapa de pedido faturado para envio de tracker
If !lPedWeb .And. !lDevol
	DbSelectArea("ZAT")
	ZAT->(DbSetOrder(1))  // ZAT_FILIAL + ZAT_PEDIDO + ZAT_CONTAT
	ZAT->(DbSeek(xFilial("ZAT") + SC5->C5_NUM))
	While ZAT->(!Eof()) .And. xFilial("ZAT") == ZAT->ZAT_FILIAL .And. SC5->C5_NUM == ZAT->ZAT_PEDIDO
		RecLock("ZAT",.F.)
		ZAT->ZAT_ETAPA := '03'  // Pedido faturado
		ZAT->ZAT_ENVIA := '1'   // Muda flag para enviar status
		ZAT->(MsUnLock())

		ZAT->(DbSkip())
	End
Endif

// Posiciona o título a receber para atualização.
SE1->(dbSetOrder(2))  // E1_FILIAL, E1_CLIENTE, E1_LOJA, E1_PREFIXO, E1_NUM, E1_PARCELA.
If SE1->(dbSeek(xFilial() + SF2->(F2_CLIENTE + F2_LOJA + F2_PREFIXO + F2_DOC), .F.))
	Do While SE1->(!eof() .and. E1_FILIAL + E1_CLIENTE + E1_LOJA + E1_PREFIXO + E1_NUM == xFilial() + SF2->(F2_CLIENTE + F2_LOJA + F2_PREFIXO + F2_DOC))
		RecLock("SE1", .F.)
		SE1->E1_XCIDADE := SC5->C5_XCIDADE
		SE1->E1_XEST    := SC5->C5_XEST
		SE1->E1_XPEDCOM := SC5->C5_X_OC
		SE1->E1_XDIASPR := _nDiasPR

		If !lDevol
			If SE1->(FieldPos("E1_XFUNCAT")) > 0
				SE1->E1_XFUNCAT := SC5->C5_XFUNCAT
			Endif

			DO CASE
				CASE lPedWeb
					SE1->E1_PORTADO := "900"
					SE1->E1_SITUACA := "N"
				CASE AllTrim(SA1->A1_BCO1) $ "999#900"
					SE1->E1_PORTADO := "900"
					SE1->E1_SITUACA := "0"
				CASE AllTrim(SA1->A1_BCO1) == "000"
					SE1->E1_PORTADO := "   "
					SE1->E1_SITUACA := "0"
			ENDCASE
		Endif

		//Campos relacionados a venda com cartao
		SE1->E1_XADMFIN := SC5->C5_XADMFIN
		SE1->E1_XCARTAO := SC5->C5_XCARTAO
		SE1->E1_XTPCART := SC5->C5_XTPCART
		SE1->E1_XNSUCAR := SC5->C5_XNSUCAR
		SE1->E1_XAUTORI := SC5->C5_XAUTORI

		SE1->(msUnlock())
		SE1->(dbSkip())
	EndDo
EndIf

//Grava titulo a pagar
GeraE2AdmF()

// Grava mensagens no pedido de venda quando for NFSe
If SF2->(Alltrim(F2_ESPECIE) == "RPS")
	SC6->(dbSetOrder(4))  // C6_FILIAL, C6_NOTA, C6_SERIE.
	If SC6->(dbSeek(xFilial("SC6") + SF2->(F2_DOC + F2_SERIE), .F.))
		SC5->(dbSetOrder(1))  // C5_FILIAL, C5_NUM.
		SC5->(dbSeek(xFilial("SC5") + SC6->C6_NUM, .F.))
		cMsgNfs := " Ped.venda: " + SC5->C5_NUM
		If !Empty(SC5->C5_X_OC)
			cMsgNfs += " - Ord.compra: " + AllTrim(SC5->C5_X_OC)
		EndIf

		SE1->(dbSetOrder(2))  // E1_FILIAL, E1_CLIENTE, E1_LOJA, E1_PREFIXO, E1_NUM, E1_PARCELA, E1_TIPO.
		If SE1->(dbSeek(xFilial("SE1") + SF2->(F2_CLIENTE + F2_LOJA + F2_PREFIXO + F2_DOC), .F.))
			cMsgNfs += " - Vencimento: " + dtoc(SE1->E1_VENCREA) + " - R$ " + AllTrim(Transform(SE1->E1_VALOR, PesqPict("SE1","E1_VALOR")))
		Endif

		If SF2->F2_EST == "SP" // Dentro do estado.
			cMsgNfs += cMsg001 + cMsg002
		Else
			cMsgNfs += cMsg002
		EndIf

		If ! (cMsgNfs $ SC5->C5_MENNOTA)
			RecLock("SC5", .F.)
				Replace SC5->C5_MENNOTA With AllTrim(SC5->C5_MENNOTA) + cMsgNfs
			SC5->(MsUnlock())
		EndIf
	Endif
Endif

// Não pega notas de devolução e beneficiamento.
If !lDevol
	// Processa os pedidos de venda da nota.
	SD2->(dbSetOrder(3))  // D2_FILIAL, D2_DOC, D2_SERIE, D2_CLIENTE, D2_LOJA, D2_COD, D2_ITEM.
	SD2->(dbSeek(xFilial() + SF2->(F2_DOC + F2_SERIE + F2_CLIENTE + F2_LOJA), .F.))
	Do While SD2->(!eof() .and. D2_FILIAL + D2_DOC + D2_SERIE + D2_CLIENTE + D2_LOJA == xFilial() + SF2->(F2_DOC + F2_SERIE + F2_CLIENTE + F2_LOJA))
		SC5->(dbSetOrder(1))  // C5_FILIAL, C5_NUM.
		SC5->(dbSeek(xFilial() + SD2->D2_PEDIDO, .F.))
		SC6->(dbSetOrder(1))  // C6_FILIAL, C6_NUM, C6_ITEM, C6_PRODUTO.
		SC6->(dbSeek(xFilial() + SD2->(D2_PEDIDO + D2_ITEMPV + D2_COD), .F.))

		// Verifica as reservas do pedido.
		If aScan(aPedidos, SC5->C5_NUM) == 0
			aAdd(aPedidos, SC5->C5_NUM)
			U_VerB2Res(SC5->C5_NUM)

			// Grava log de alteração do pedido.
			U_HistPV(SC5->C5_NUM, "9", "Gerada NF " + SF2->(F2_SERIE + "/" + F2_DOC))
		Endif

		// Analisa a amostra.
		If !empty(SC5->C5_XSOLAMO)
			ZB3->(dbSetOrder(1))  // ZB3_FILIAL, ZB3_COD.
			If ZB3->(dbSeek(xFilial() + SC5->C5_XSOLAMO, .F.))
				If aScan(aSolAmost, ZB3->ZB3_COD) == 0
					aAdd(aSolAmost, ZB3->ZB3_COD)
				Endif

				ZB5->(dbSetOrder(1))  // ZB5_FILIAL, ZB5_COD.
				ZB5->(dbSeek(xFilial() + ZB3->ZB3_TIPO, .F.))

				// Atualiza o item da amostra.
				ZB4->(dbSetOrder(1))  // ZB4_FILIAL, ZB4_COD, ZB4_ITEM, ZB4_PROD.
				If ZB4->(dbSeek(xFilial() + SC5->C5_XSOLAMO + SC6->(C6_XITEMAM + C6_PRODUTO), .F.))
					RecLock("ZB4", .F.)
					ZB4->ZB4_QTDFAT += SD2->D2_QUANT
					If ZB5->ZB5_FECHAM == "A" .and. ZB4->(ZB4_QTDE <= ZB4_QTDFAT) // ZB5_FECHAM -> A-Automático.
						ZB4->ZB4_STATUS := FECHADA
					Else
						ZB4->ZB4_STATUS := PENDENTE
					Endif
					ZB4->(msUnLock())
				Endif
			Endif
		Else
			// Se for pedido de venda com duplicata, fecha a solicitação de amostra em aberto.
			SF4->(dbSetOrder(1))  // F4_FILIAL, F4_CODIGO.
			If SF4->(dbSeek(xFilial() + SD2->D2_TES, .F.) .and. F4_DUPLIC = "S")
				// Verifica se há amostra em aberto do produto, no cliente.
				cTipoFec := "M"  // Somente amostras com fechamento manual.
				aAmostra := U_MA090Cli(SD2->D2_CLIENTE, SD2->D2_LOJA, .F., left(SD2->D2_COD, nTamRef), cTipoFec)[1]

				For nX := 1 to len(aAmostra)
					ZB3->(dbGoTo(aAmostra[nX, 1]))
					ZB4->(dbGoTo(aAmostra[nX, 2]))

					RecLock("ZB4", .F.)
					ZB4->ZB4_STATUS := FECHADA
					ZB4->(msUnLock())

					// Atualiza o status do cabeçalho da solicitação de amostra.
					U_MA090Sts()
				Next nX
			Endif
		Endif
		SD2->(dbSkip())
	EndDo

	// Amarra a NF com a solicitação de amostra.
	ZB3->(dbSetOrder(1))  // ZB3_FILIAL, ZB3_COD.
	For nX := 1 to len(aSolAmost)
		If ZB3->(dbSeek(xFilial() + aSolAmost[nX], .F.))
			// Posiciona o tipo de amostra.
			ZB5->(dbSetOrder(1))  // ZB5_FILIAL, ZB5_COD.
			ZB5->(dbSeek(xFilial() + ZB3->ZB3_TIPO, .F.))

			// Verifica se todos os itens estão faturados.
			ZB4->(dbSetOrder(1))  // ZB4_FILIAL, ZB4_COD, ZB4_ITEM, ZB4_PROD.
			ZB4->(dbSeek(xFilial() + ZB3->ZB3_COD, .F.))
			Do While ZB4->(!eof() .and. ZB4_FILIAL + ZB4_COD == xFilial() + ZB3->ZB3_COD)
				If ZB4->(ZB4_STATUS <> FECHADA .and. ZB4_QTDE > ZB4_QTDFAT)
					// Marca como não faturado completamente.
					lFatur := .F.
					Exit
				Endif
				ZB4->(dbSkip())
			EndDo

			// Se a solicitação de amostra foi faturada 100%, grava o número da NF.
			If lFatur
				RecLock("ZB3", .F.)
				ZB3->ZB3_DOC    := SF2->F2_DOC
				ZB3->ZB3_SERIE  := SF2->F2_SERIE
				ZB3->ZB3_DATANF := SF2->F2_EMISSAO
				If ZB5->ZB5_FECHAM == "A"  // Automático.
					ZB3->ZB3_STATUS := FECHADA
				Endif
				ZB3->(msUnLock())
			Endif
		Endif
	Next nX
Endif

// Envia email com NF de saída com produtos que estejam no armazém 91 (avarias).
SaiAvar()

// Envia email com NF de saída com produtos que estejam tenham DIFAL.
SaiDifal()

// Restaura areas de trabalho.
(cAlias)->(sRestArea(aArea))
dbSelectArea(cAlias)

// Grava número do contrato no item. Grava o número da Campanha Comercial
if !lDevol .and. ! (SF2->F2_CLIENTE $ "999999|888888")
	grvCtrIt()
endIf

Return lRet


// ##############################################################################
// Projeto  : PROT-CAP
// Autor    : Tiago Melo
// Modulo   : Compras
// Função   : SaiAvar
// Descrição: Seleciona a NF de saída com produtos do armazém 91 (avarias)
// Retorno  : Nenhum.
// ---------+---------------------------+----------------------------------------
// Data     | Autor                     | Descrição
// ---------+---------------------------+----------------------------------------
// 13/01/15 | Tiago Melo                | Desenvolvimento da rotina
// ---------+---------------------------+----------------------------------------
Static Function SaiAvar()

Local cTo       := SuperGetMV("PC_MAILLOG",, "")  // Para quem será enviado o email.
Local cAssunto  := ""
Local cMsgMail  := ""
Local cQry      := ""
Local cAliasTRB := ""
Local cProduto  := ""
Local nCont     := 0

If !empty(cTo)
	cQry := "SELECT SD2.R_E_C_N_O_ SD2RecNo  " + CRLF
	cQry += "FROM " + RetSqlName("SD2")+" SD2 " + CRLF

	cQry += "WHERE SD2.D_E_L_E_T_ = '' " + CRLF
	cQry += "and SD2.D2_FILIAL  = '" + xFilial("SD2") + "' " + CRLF
	cQry += "and SD2.D2_DOC     = '" + SF2->F2_DOC + "' " + CRLF
	cQry += "and SD2.D2_SERIE   = '" + SF2->F2_SERIE + "' " + CRLF
	cQry += "and SD2.D2_CLIENTE = '" + SF2->F2_CLIENTE + "' " + CRLF
	cQry += "and SD2.D2_LOJA    = '" + SF2->F2_LOJA + "' " + CRLF
	cQry += "and SD2.D2_LOCAL   = '91' " + CRLF
	cQry += "and SD2.D2_TES     <> '' " + CRLF  // Somente notas classificadas.
	cQry += "ORDER BY D2_COD, D2_LOCAL "

	cAliasTRB  := GetNextAlias()
	dbUseArea(.T., "TOPCONN", TCGenQry(,, cQry), cAliasTRB, .F., .T.)

	cMsgMail := U_Txt2HTML("NF de saída " + StrTran(SF2->F2_FILIAL + " " + SF2->F2_SERIE + "/" + SF2->F2_DOC, " ", "&nbsp;") + " com produtos do armazém 91.") + "<br/>"
	If SF2->F2_TIPO $ "DB"
		cMsgMail  += "Fornecedor: " + SF2->F2_CLIENTE + "/" + SF2->F2_LOJA + " - " + U_Txt2HTML(Posicione("SA2", 1, xFilial("SA2") + SF2->F2_CLIENTE + SF2->F2_LOJA, + " " + "rtrim(A2_NOME)")) + "<br/>"
		cAssunto  := "Armazém 91 - Saída de devolução/beneficiamento"
	Else
		cMsgMail  += "Cliente: " + SF2->F2_CLIENTE + "/" + SF2->F2_LOJA + " - " + U_Txt2HTML(Posicione("SA1", 1, xFilial("SA1") + SF2->F2_CLIENTE + SF2->F2_LOJA, + " " + "rtrim(A1_NOME)")) + "<br/>"
		cAssunto  := "Armazém 91 - Saída"
	Endif
	cMsgMail += U_Txt2HTML("Emissão: " + dtoc(SF2->F2_EMISSAO)) + "<br/>"
	cMsgMail += U_Txt2HTML("Usuário: " + cUserName) + "<br/><br/>"

	// Cabeçalho da tabela.
	cMsgMail += "<table style='border:1px solid #002a5c'>"
	cMsgMail += " <tr>"
	cMsgMail += "  <th style='border:1px solid #002a5c; width:80%; font-weight:bold; text-align:left'>Produto</th>"
	cMsgMail += "  <th style='border:1px solid #002a5c; width:20%; font-weight:bold; text-align:left'>Quantidade</th>"
	cMsgMail += " </tr>"

	Do While (cAliasTRB)->(!eof())
		SD2->(dbGoTo((cAliasTRB)->SD2RecNo))

		// Monta e-mail.
		cProduto := StrTran(SD2->D2_COD + " - " + U_Txt2HTML(Posicione("SB1", 1 , xFilial("SB1") + SD2->D2_COD, "rtrim(B1_DESC)")), " ", "&nbsp;")
		cMsgMail += " <tr>"
		cMsgMail += "  <td style='border:1px solid #002a5c; text-align:left'>" + cProduto + "</td>"
		cMsgMail += "  <td style='border:1px solid #002a5c; text-align:right'>" + Transform(SD2->D2_QUANT, "@E 999,999,999.99") + "</td>"
		cMsgMail += " </tr>"
		nCont++

		// Próximo registro.
		(cAliasTRB)->(dbSkip())
	EndDo
	(cAliasTRB)->(dbCloseArea())

	cMsgMail += "</table>"

	// Envia email.
	If nCont > 0
		U_EnvEMail(nil, cTo, cAssunto, cMsgMail)
	EndIf
Endif

Return


// ##############################################################################
// Projeto  : PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Compras
// Função   : SaiAvar
// Descrição: Seleciona a NF de saída com produtos do armazém 91 (avarias)
// Retorno  : Nenhum.
// ---------+---------------------------+----------------------------------------
// Data     | Autor                     | Descrição
// ---------+---------------------------+----------------------------------------
// 29/12/15 | Felipe Raposo             | Desenvolvimento da rotina
// ---------+---------------------------+----------------------------------------
Static Function SaiDifal()

Local cTo        := SuperGetMV("PC_MAILDIF",, "fatur.difal@protcap.com.br")
Local cAssunto   := ""
Local cMsgMail   := ""

Local nTamFTItem := TamSX3("FT_ITEM")[1]
Local cProduto   := ""
Local aDIFAL     := {}
Local aICMS      := {0, 0, 0, 0}
Local nX

Local cMailFat	:= ""

PswOrder(1)
If PswSeek(__cUserId, .T.)
   cMailFat := ";" + PswRet()[1][14]
EndIf

If !empty(cTo)
	// Processa os pedidos de venda da nota.
	SD2->(dbSetOrder(3))  // D2_FILIAL, D2_DOC, D2_SERIE, D2_CLIENTE, D2_LOJA, D2_COD, D2_ITEM.
	SD2->(dbSeek(xFilial() + SF2->(F2_DOC + F2_SERIE + F2_CLIENTE + F2_LOJA), .F.))
	Do While SD2->(!eof() .and. D2_FILIAL + D2_DOC + D2_SERIE + D2_CLIENTE + D2_LOJA == xFilial() + SF2->(F2_DOC + F2_SERIE + F2_CLIENTE + F2_LOJA))
		SC5->(DbSetOrder(1)) // Indice 1 - C5_FILIAL+C5_NUM
		SC5->(DbSeek(xFilial('SC5')+SD2->D2_PEDIDO))
		If SD2->D2_DIFAL > 0 .And. Empty(SC5->C5_XIDVTV3)
			SFT->(dbSetOrder(1))  // FT_FILIAL, FT_TIPOMOV, FT_SERIE, FT_NFISCAL, FT_CLIEFOR, FT_LOJA, FT_ITEM, FT_PRODUTO.
			SFT->(dbSeek(xFilial() + "S" + SD2->(D2_SERIE + D2_DOC + D2_CLIENTE + D2_LOJA + PadR(D2_ITEM, nTamFTItem) + D2_COD), .F.))
			aICMS[1] += SD2->D2_VALICM
			aICMS[2] += SFT->FT_ICMSCOM
			aICMS[3] += SFT->FT_DIFAL
			aICMS[4] += SFT->FT_VFCPDIF

			aAdd(aDIFAL, {SD2->(RecNo()), SFT->(RecNo())})
		Endif
		SD2->(dbSkip())
	EndDo

	// Se há produtos com DIFAL, envia e-mail.
	If !empty(aDIFAL)
		cAssunto := "NF de saída " + SF2->F2_FILIAL + " " + SF2->F2_SERIE + "/" + SF2->F2_DOC + " com DIFAL."

		cMsgMail := U_Txt2HTML("Origem " + SF2->F2_FILIAL + " " + FWFilialName(nil, SF2->F2_FILIAL, 1)) + "<br/>"
		cMsgMail += U_Txt2HTML("Destino: " + SF2->F2_EST) + "<br/>"
		cMsgMail += U_Txt2HTML("NF de saída ") + StrTran(SF2->F2_SERIE + "/" + SF2->F2_DOC, " ", "&nbsp;") + "<br/>"
		If SF2->F2_TIPO $ "DB"
			cMsgMail += U_Txt2HTML("Fornecedor: " + SF2->F2_CLIENTE + "/" + SF2->F2_LOJA + " - " + Posicione("SA2", 1, xFilial("SA2") + SF2->F2_CLIENTE + SF2->F2_LOJA, + " " + "rtrim(A2_NOME)")) + "<br/>"
		Else
			cMsgMail += U_Txt2HTML("Cliente: " + SF2->F2_CLIENTE + "/" + SF2->F2_LOJA + " - " + Posicione("SA1", 1, xFilial("SA1") + SF2->F2_CLIENTE + SF2->F2_LOJA, + " " + "rtrim(A1_NOME)")) + "<br/>"
		Endif
		cMsgMail += U_Txt2HTML("Emissão: " + dtoc(SF2->F2_EMISSAO)) + "<br/>"
		cMsgMail += U_Txt2HTML("Usuário: " + cUserName) + "<br/><br/>"

		cMsgMail += "<table>"
		cMsgMail += " <tr>"
		cMsgMail += "  <td style='text-align:left'>ICMS</td>"
		cMsgMail += "  <td style='text-align:right'>R$ " + Transform(aICMS[1], "@E 999,999,999.99") + "</td>"
		cMsgMail += " </tr>"
		cMsgMail += " <tr>"
		cMsgMail += "  <td style='text-align:left'>ICMS complementar</td>"
		cMsgMail += "  <td style='text-align:right'>R$ " + Transform(aICMS[2], "@E 999,999,999.99") + "</td>"
		cMsgMail += " </tr>"
		cMsgMail += " <tr>"
		cMsgMail += "  <td style='text-align:left'>ICMS complementar dest.</td>"
		cMsgMail += "  <td style='text-align:right'>R$ " + Transform(aICMS[3], "@E 999,999,999.99") + "</td>"
		cMsgMail += " </tr>"
		If aICMS[4] > 0
			cMsgMail += " <tr>"
			cMsgMail += "  <td style='text-align:left'>FECP</td>"
			cMsgMail += "  <td style='text-align:right'>R$ " + Transform(aICMS[4], "@E 999,999,999.99") + "</td>"
			cMsgMail += " </tr>"
		Endif
		cMsgMail += "</table><br/><br/>"

		// Cabeçalho da tabela.
		cMsgMail += "<table style='border:1px solid #002a5c'>"
		cMsgMail += " <tr>"
		cMsgMail += "  <th style='border:1px solid #002a5c; font-weight:bold; text-align:left'>It</th>"
		cMsgMail += "  <th style='border:1px solid #002a5c; font-weight:bold; text-align:left'>Produto</th>"
		cMsgMail += "  <th style='border:1px solid #002a5c; font-weight:bold; text-align:right'>Quantidade</th>"
		cMsgMail += "  <th style='border:1px solid #002a5c; font-weight:bold; text-align:right'>Vlr Unit</th>"
		cMsgMail += "  <th style='border:1px solid #002a5c; font-weight:bold; text-align:right'>Total</th>"
		cMsgMail += "  <th style='border:1px solid #002a5c; font-weight:bold; text-align:center' colspan='2'>ICMS</th>"
		cMsgMail += "  <th style='border:1px solid #002a5c; font-weight:bold; text-align:right'>ICMS Compl</th>"
		cMsgMail += "  <th style='border:1px solid #002a5c; font-weight:bold; text-align:right'>ICMS Dest</th>"
		cMsgMail += "  <th style='border:1px solid #002a5c; font-weight:bold; text-align:right'>FECP</th>"
		cMsgMail += " </tr>"

		For nX := 1 to len(aDIFAL)
			SD2->(dbGoTo(aDIFAL[nX, 1]))
			SFT->(dbGoTo(aDIFAL[nX, 2]))
			cProduto := StrTran(SD2->D2_COD + " - " + U_Txt2HTML(Posicione("SB1", 1 , xFilial("SB1") + SD2->D2_COD, "rtrim(B1_DESC)")), " ", "&nbsp;")

			cMsgMail += " <tr>"
			cMsgMail += "  <td style='border:1px solid #002a5c; text-align:left'>" + SD2->D2_ITEM + "</td>"
			cMsgMail += "  <td style='border:1px solid #002a5c; text-align:left'>" + cProduto + "</td>"
			cMsgMail += "  <td style='border:1px solid #002a5c; text-align:right'>"    + Transform(SD2->D2_QUANT,   "@E 999,999,999.99") + "</td>"
			cMsgMail += "  <td style='border:1px solid #002a5c; text-align:right'>R$ " + Transform(SD2->D2_PRCVEN,  "@E 999,999,999.99") + "</td>"
			cMsgMail += "  <td style='border:1px solid #002a5c; text-align:right'>R$ " + Transform(SD2->D2_TOTAL,   "@E 999,999,999.99") + "</td>"
			cMsgMail += "  <td style='border:1px solid #002a5c; text-align:right'>"    + Transform(SD2->D2_PICM,    "@E 999") + "%</td>"
			cMsgMail += "  <td style='border:1px solid #002a5c; text-align:right'>R$ " + Transform(SD2->D2_VALICM,  "@E 999,999,999.99") + "</td>"
			cMsgMail += "  <td style='border:1px solid #002a5c; text-align:right'>R$ " + Transform(SFT->FT_ICMSCOM, "@E 999,999,999.99") + "</td>"
			cMsgMail += "  <td style='border:1px solid #002a5c; text-align:right'>R$ " + Transform(SFT->FT_DIFAL,   "@E 999,999,999.99") + "</td>"
			cMsgMail += "  <td style='border:1px solid #002a5c; text-align:right'>R$ " + Transform(SFT->FT_VFCPDIF, "@E 999,999,999.99") + "</td>"
			cMsgMail += " </tr>"
		Next nX

		cMsgMail += "</table>"

		cTo += cMailFat
		// Envia email.
		U_EnvEMail(nil, cTo, cAssunto, cMsgMail)
	Endif
Endif

Return

// ##############################################################################
// Projeto  : PROT-CAP
// Autor    : Fabricio Romera
// Modulo   : Faturamento
// Função   : GeraE2AdmF
// Descrição: Gera titulo a pagar referente taxa da administradora do cartao
//			  Deve estar posicionado na nota fiscal SF2
// Retorno  : Nenhum.
// ---------+---------------------------+----------------------------------------
// Data     | Autor                     | Descrição
// ---------+---------------------------+----------------------------------------
// 15/01/18 | Fabricio Romera           | Desenvolvimento da rotina
// ---------+---------------------------+----------------------------------------
Static Function GeraE2AdmF()
Local aArea 	:= GetArea()
Local aAreaSE1	:= SE1->(GetArea())
Local aAreaSF2	:= SF2->(GetArea())
Local aAreaSE4	:= SE4->(GetArea())
Local aAreaSC5	:= SC5->(GetArea())
Local aAreaSAE	:= SAE->(GetArea())

Local lRet 		:= .T.
Local nValAdm 	:= 0
Local aVetorSE2 := {}
Local cNumSA2 	:= ""
Local cCondPg	:= SF2->F2_COND
Local nValTit	:= 0
Local cHistE2   := "Tx Cartao - Titulo: "
Local nSldTax	:= 0
Local nSldTit	:= 0
Local nVlrPar	:= 0
Local cNatSE2	:= SuperGetMV("PC_NATTXCC", NIL, "021102    ")

//Verifica se forma de pagamento é cartao
DbSelectArea("SE4")
SE4->(DbSetOrder(1))
If SE4->(DbSeek(xFilial("SE4")+cCondPg))
	If SE4->E4_XFPGTO <> "3" .AND. SE4->E4_XFPGTO <> "4"
		lRet := .F.
	EndIf
EndIf

If lRet
	//Posiciona no Pedido para buscar valor passado em cartao de credito
	DbSelectArea("SC5")
	SC5->(dbSetOrder(1))  // C5_FILIAL, C5_NUM.
	If SC5->(dbSeek(xFilial("SC5") + SD2->D2_PEDIDO))
		If SC5->C5_XVALCAR > 0 .AND. !Empty(SC5->C5_XADMFIN)
			nValTit	:= SC5->C5_XVALCAR
			cAdmFin	:= SC5->C5_XADMFIN

			//Calcula valor da taxa administrativa
			DbSelectArea("SAE")
			DBSetOrder(1)

			If SAE->(DbSeek(xFilial("SAE") + cAdmFin))
				If SAE->AE_TAXA > 0
					nValAdm := A410Arred(nValTit * SAE->AE_TAXA / 100, "E2_VALOR")
					nSldTax := nValAdm
					nSldTit := nValTit
					cNumSA2 := L070IncSA2() //Funcao padrao que retorna codigo do forn/ou o inclui se nao existir
				Else
					lRet := .F.
				EndIf
			Else
				lRet := .F.
			EndIf
		Else
			lRet := .F.
		EndIf
	EndIf
EndIf

If lRet
	DbSelectArea("SE1")
	SE1->(dbSetOrder(2))  // E1_FILIAL, E1_CLIENTE, E1_LOJA, E1_PREFIXO, E1_NUM, E1_PARCELA.

	If SE1->(dbSeek(xFilial("SE1") + SF2->F2_CLIENTE + SF2->F2_LOJA + SF2->F2_PREFIXO + SF2->F2_DOC))

		While !SE1->(Eof()) .AND. lRet .AND. xFilial("SE1") + SE1->E1_CLIENTE + SE1->E1_LOJA + SE1->E1_PREFIXO + SE1->E1_NUM == xFilial("SE1") + SF2->F2_CLIENTE + SF2->F2_LOJA + SF2->F2_PREFIXO + SF2->F2_DOC

			If AllTrim(SE1->E1_TIPO) == "NF" .AND. SE1->E1_CLIENTE == SF2->F2_CLIENTE .AND. SE1->E1_LOJA == SF2->F2_LOJA
				//Calcula o Valor da Parcela de Acordo com a Proporcao da Parcela atual para o Valor Total
				nSldTit -= SE1->E1_VALOR

				//Se não tem mais saldo de Titulo
				If nSldTit == 0
					nVlrPar := nSldTax
					nSldTax := 0
				Else
					nVlrPar := A410Arred(SE1->E1_VALOR * SAE->AE_TAXA / 100, "E2_VALOR")
					nSldTax -= nVlrPar
				EndIf

				aVetorSE2 := {	{"E2_PREFIXO",	SE1->E1_PREFIXO													, NIL},;
								{"E2_NUM",		SE1->E1_NUM														, NIL},;
								{"E2_PARCELA",	SE1->E1_PARCELA													, NIL},;
								{"E2_TIPO",		SE1->E1_TIPO													, NIL},;
								{"E2_FORNECE",	cNumSA2															, NIL},;
								{"E2_LOJA",		"01  "															, NIL},;
								{"E2_NATUREZ",	cNatSE2															, NIL},;
								{"E2_EMISSAO",	dDataBase														, NIL},;
								{"E2_VENCTO",	SE1->E1_VENCTO													, NIL},;
								{"E2_VENCREA",	SE1->E1_VENCREA													, NIL},;
								{"E2_VALOR",	nVlrPar															, NIL},;
								{"E2_HIST",		cHistE2 + AllTrim(SE1->E1_NUM) + "/" + AllTrim(SE1->E1_PARCELA)	, NIL},;
								{"E2_XCARTAO",	SE1->E1_XCARTAO													, NIL},;
								{"E2_XTPCART",	SE1->E1_XTPCART													, NIL},;
								{"E2_XNSUCAR",	SE1->E1_XNSUCAR													, NIL},;
								{"E2_XADMFIN",	SE1->E1_XADMFIN													, NIL},;
								{"E2_XAUTORI",	SE1->E1_XAUTORI													, NIL}}

				lMsErroAuto := .F.

				//Faz a inclusao do contas a pagar via ExecAuto
				MSExecAuto({|x,y,z| Fina050(x,y,z)}, aVetorSE2, , 3)

				//Verifica erros durante a execucao da rotina automatica.
				If lMsErroAuto
					MostraErro()
					lRet := .F.
					Exit
				Endif
			EndIf

			SE1->(DbSkip())
		End
	Else
    	lRet := .F.
    EndIf
EndIf

RestArea(aAreaSF2)
RestArea(aAreaSE1)
RestArea(aAreaSE4)
RestArea(aAreaSAE)
RestArea(aAreaSC5)
RestArea(aArea)

Return lRet

//-------------------------------------------------------------------
/*/{Protheus.doc} grvCtrIt
Grava número do contrato nos itens da nota

@author  João Leão
@since   25/09/2018
@version 12.1.17
/*/
//-------------------------------------------------------------------
static function grvCtrIt()

local aArea		:= getArea()
local aSD2		:= SD2->(getArea())
local aSC5		:= SC5->(getArea())
local aSC6		:= SC6->(getArea())

SD2->(dbSetOrder(3))  // D2_FILIAL, D2_DOC, D2_SERIE, D2_CLIENTE, D2_LOJA, D2_COD, D2_ITEM.
if SD2->(dbSeek(xFilial("SD2") + SF2->(F2_DOC + F2_SERIE + F2_CLIENTE + F2_LOJA), .F.))
	Do While SD2->(!eof() .and. D2_FILIAL + D2_DOC + D2_SERIE + D2_CLIENTE + D2_LOJA == xFilial("SF2") + SF2->(F2_DOC + F2_SERIE + F2_CLIENTE + F2_LOJA))
		SC5->(dbSetOrder(1))  // C5_FILIAL, C5_NUM.
		if SC5->(dbSeek(xFilial() + SD2->D2_PEDIDO, .F.))
			SC6->(dbSetOrder(1))  // C6_FILIAL, C6_NUM, C6_ITEM, C6_PRODUTO.
			if SC6->(dbSeek(xFilial() + SD2->(D2_PEDIDO + D2_ITEMPV + D2_COD), .F.))
				recLock("SD2", .F.)
				if !empty(SC6->C6_XCONTR)
						SD2->D2_XCONTR := SC6->C6_XCONTR
				EndIf

				If !Empty(SC6->C6_XACAO)
					SD2->D2_XACAO  := SC6->C6_XACAO
				endIf

					SD2->(msUnlock())
			endIf
		endIf
		SD2->(dbSkip())
	endDo
endIf

restArea(aSC5)
restArea(aSC6)
restArea(aSD2)
restArea(aArea)

return
