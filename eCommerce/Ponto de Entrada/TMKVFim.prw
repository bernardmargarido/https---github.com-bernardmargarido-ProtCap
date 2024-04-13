#include "protheus.ch"
#include "topconn.ch"
#include "tbiconn.ch"
#include "tbicode.ch"
#include "rptdef.ch"

// Valores dos combos tipo operacao.
#DEFINE FATURAMENTO  "1"
#DEFINE ORCAMENTO    "2"
#DEFINE ATENDIMENTO  "3"

Static nAliqPCC   := SuperGetMV("MV_TXPIS") + SuperGetMV("MV_TXCOFIN")
Static aEntidade  := MsRelation()
Static cTransRed  := SuperGetMV("PC_TRANRED",, "")  // Transportadora de redespacho.

// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : TMKVFim
// Descrição: Ponto de entrada no final da gravacao do atendimento do Call-center.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 13/11/13 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function TMKVFim()
Local lRet       := .T.
Local cAlias     := Alias()
Local aArea      := {}
Local nItem      := 0

Local lFatur     := (SUA->UA_OPER == FATURAMENTO) .and. !IsInCallStack("U_ZB010JRun")
Local nCustoIt   := 0
Local nPrcLiqIt  := 0

Local cOrdCom    := ""
Local nCustoTot  := 0
Local nPrcLiqTot := 0

Local lNetSupr   := !empty(SUA->UA_XIDVTV3)
Local lSiteProt  := LEFT(SUA->UA_XPEDLV,2) == "JC"
Local lPrevTransp	:= SuperGetMV("BZ_PRZENTR", NIL, .T.) // calcula e grava os campos de prazo de entrega de transporte

//Local aRet800		:= ''
//Local lEnvioFup		:= .T.
//Local lFollowup		:= SuperGetMv("PC_FOLLOWU",,.F.)

// Guarda posição das tabelas.
If empty(cAlias)
	cAlias := "SX3"
	dbSelectArea(cAlias)
Endif
aArea := sGetArea()
sGetArea(aArea, "SA1")
sGetArea(aArea, "SUB")

U_DebugMsg("Início do TMKVFim", "I")

//Grava a origem do Orcamento
If SUA->(FieldPos("UA_XDTA")) > 0
	If Empty(SUA->UA_XINTEDI)
		RecLock("SUA", .F.)
			SUA->UA_XDTA := U_fGetOrig(IsInCallStack("U_PROMA611"))
		MsUnlock()
	EndIf
EndIf


//Manual / Automatico
If SUA->(FieldPos("UA_XDTAM")) > 0
	If Empty(SUA->UA_XINTEDI)
		RecLock("SUA", .F.)
		If Empty(SUA->UA_XDTAM)
			SUA->UA_XDTAM	:= If(SUA->UA_XDTA == "000", "M", "A")		//TGV - Manual
		Else
			SUA->UA_XDTAM	:= "M"
		EndIf
		MsUnlock()
	EndIf
EndIf

// Corrige o cálculo de frete, utilizado no padrão.
// O padrão está jogando o frete inteiro no primeiro item.
If SUA->UA_FRETE > 0
	MaFisAlt("NF_FRETE", 0)
	MaFisAlt("NF_FRETE", SUA->UA_FRETE)
Endif

// Grava o número do orçamento no log de Similaridade
If !Empty(SUA->UA_XIDSIMI)
	ZFB->(DBSetOrder(1)) //ZFB_FILIAL, ZFB_IDSIMI, ZFB_ORCAME
	If ZFB->(DBSeek(xFilial("ZFB") + SUA->UA_XIDSIMI))
		If ZFB->ZFB_ORCAME != SUA->UA_NUM
			If RecLock("ZFB", .F., .F., .T., .F.)
				ZFB->ZFB_ORCAME := SUA->UA_NUM
				ZFB->(MsUnlock())
			EndIf
		EndIf
	EndIf
EndIf

// Grava os impostos e rentabilidade do atendimento.
For nItem := 1 to len(aCols)
	// Verifica se a linha está apagada.
	If aTail(aCols[nItem])
		If !MaFisRet(nItem, "IT_DELETED")
			MaFisDel(nItem, .T.)
		Endif
	Else
		SUB->(dbSetOrder(1))  // UB_FILIAL, UB_NUM, UB_ITEM, UB_PRODUTO.
		SUB->(dbSeek(xFilial() + SUA->UA_NUM + GdFieldGet("UB_ITEM", nItem), .F.))

		// Grava os dados.
		If MaFisRet(nItem, "IT_PRODUTO") == SUB->UB_PRODUTO

			SB1->(dbSetOrder(1))  // B1_FILIAL, B1_COD.
			SB1->(msSeek(xFilial() + SUB->UB_PRODUTO, .F.))

			RecLock("SUB", .F.)

			// A base de ICMS já é gravada no campo padrão UB_BASEICM.
			SUB->UB_XALQICM := MaFisRet(nItem, "IT_ALIQICM")
			SUB->UB_XVALICM := MaFisRet(nItem, "IT_VALICM")
			SUB->UB_XREDICM := MaFisRet(nItem, "IT_PREDIC")

			SUB->UB_XBASSOL := MaFisRet(nItem, "IT_BASESOL")
			SUB->UB_XVALSOL := MaFisRet(nItem, "IT_VALSOL")

			SUB->UB_XBASIPI := MaFisRet(nItem, "IT_BASEIPI")
			SUB->UB_XIPI    := MaFisRet(nItem, "IT_ALIQIPI")
			SUB->UB_XVALIPI := MaFisRet(nItem, "IT_VALIPI")

			SUB->UB_XBASPCC := MaFisRet(nItem, "IT_BASECF2")
			SUB->UB_XALQPS2 := MaFisRet(nItem, "IT_ALIQPS2")
			SUB->UB_XVALPIS := MaFisRet(nItem, "IT_VALPS2")
			SUB->UB_XALQCF2 := MaFisRet(nItem, "IT_ALIQCF2")
			SUB->UB_XVALCOF := MaFisRet(nItem, "IT_VALCF2")

			SUB->UB_XALQCMP := MaFisRet(nItem, "IT_ALIQCMP")
			SUB->UB_XVALCMP := MaFisRet(nItem, "IT_VALCMP")
			SUB->UB_XBASDES := MaFisRet(nItem, "IT_BASEDES")
			SUB->UB_XDIFAL  := MaFisRet(nItem, "IT_DIFAL")
			SUB->UB_XALFCMP := MaFisRet(nItem, "IT_ALFCCMP")
			SUB->UB_XVFCPDI := MaFisRet(nItem, "IT_VFCPDIF")

			SUB->UB_XVALFRE := MaFisRet(nItem, "IT_FRETE")

			// Calcula custo e rentabilidade da linha.
			nCustoIt  := SUB->UB_QUANT * round(SB1->B1_CUSTD * (1 - ((SB1->B1_XICMF + nAliqPCC) / 100)), 8)              // Custo líquido.
			nPrcLiqIt := SUB->(UB_VLRITEM - UB_XVALICM - UB_XVALPIS - UB_XVALCOF - UB_XVALCMP - UB_XDIFAL - UB_XVFCPDI)  // Preço líquido.
			SUB->UB_XVLRLIQ := nPrcLiqIt
			SUB->UB_XCUSTO  := nCustoIt
			SUB->UB_XRENTA  := min(max(If(nPrcLiqIt = 0, 0, (1 - (nCustoIt / nPrcLiqIt)) * 100), -999.99), 999.99)

			// Pedidos NetSuprimentos.
			If INCLUI .and. lNetSupr
				SUB->UB_PRCTAB	:= SUB->UB_VRUNIT
			Endif

			SUB->UB_XCUSMED := u_xCusMed(SUB->UB_FILIAL, SUB->UB_PRODUTO, SUB->UB_LOCAL, .T.)[1]

			SUB->(msUnLock())

			// Armazena dados para calcular rentabilidade total do atendimento.
			nCustoTot  += nCustoIt
			nPrcLiqTot += nPrcLiqIt

			// Atualiza valores do item do pedido de venda.
			If lFatur
				SC6->(dbSetOrder(1))  // C6_FILIAL, C6_NUM, C6_ITEM, C6_PRODUTO.
				If SC6->(dbSeek(xFilial() + SUB->(UB_NUMPV + UB_ITEMPV + UB_PRODUTO), .F.))
					RecLock("SC6", .F.)

					If !empty(SUB->UB_XDESCR)
						SC6->C6_DESCRI  := SUB->UB_XDESCR
					Endif
					SC6->C6_XOBS    := SUB->UB_XOBS
					SC6->C6_NUMPCOM := SUB->UB_XORDCOM
					SC6->C6_ITEMPC  := SUB->UB_XITEMOC
					SC6->C6_XCONTR  := SUB->UB_XCONTR

					SC6->C6_XBASICM := SUB->UB_BASEICM
					SC6->C6_XALQICM := SUB->UB_XALQICM
					SC6->C6_XVALICM := SUB->UB_XVALICM
					SC6->C6_XREDICM := SUB->UB_XREDICM

					SC6->C6_XBASSOL := SUB->UB_XBASSOL
					SC6->C6_XVALSOL := SUB->UB_XVALSOL

					SC6->C6_XBASIPI := SUB->UB_XBASIPI
					SC6->C6_XALQIPI := SUB->UB_XIPI
					SC6->C6_XVALIPI := SUB->UB_XVALIPI

					SC6->C6_XBASPCC := SUB->UB_XBASPCC
					SC6->C6_XALQPS2 := SUB->UB_XALQPS2
					SC6->C6_XVALPIS := SUB->UB_XVALPIS
					SC6->C6_XALQCF2 := SUB->UB_XALQCF2
					SC6->C6_XVALCOF := SUB->UB_XVALCOF

					SC6->C6_XALQCMP := SUB->UB_XALQCMP
					SC6->C6_XVALCMP := SUB->UB_XVALCMP
					SC6->C6_XBASDES := SUB->UB_XBASDES
					SC6->C6_XDIFAL  := SUB->UB_XDIFAL
					SC6->C6_XALFCMP := SUB->UB_XALFCMP
					SC6->C6_XVFCPDI := SUB->UB_XVFCPDI

					SC6->C6_XVALFRE := SUB->UB_XVALFRE

					// Calcula custo e rentabilidade da linha.
					SC6->C6_XVLRLIQ := SUB->UB_XVLRLIQ
					SC6->C6_XCUSTO  := SUB->UB_XCUSTO
					SC6->C6_XRENTA  := SUB->UB_XRENTA

					// Pedidos NetSuprimentos.
					If INCLUI .and. lNetSupr
						SC6->C6_PRUNIT  := SC6->C6_PRCVEN
					Endif

					SC6->C6_XACAO	:= SUB->UB_XACAO
					SC6->C6_XCUSMED := SUB->UB_XCUSMED

					SC6->(msUnLock())

					// Armazena a ordem de compra para gravar no SC5.
					If AllTrim(SUB->UB_XORDCOM) <> "" .and. !(AllTrim(SUB->UB_XORDCOM) $ cOrdCom)
						cOrdCom += AllTrim(SUB->UB_XORDCOM) + "/"
					Endif
				Else
					Help(,, 'Help',, "Erro na gravação da rentabilidade - item PV " + SUB->(UB_NUMPV + "-" + UB_ITEMPV + " - " + rtrim(UB_PRODUTO)) + ".", 1, 0)
					lRet := .F.
					Exit
				Endif
			Endif
		Else
			Help(,, 'Help',, "Erro na gravação da rentabilidade - item " + SUB->(UB_ITEM + " - " + rtrim(UB_PRODUTO)) + ".", 1, 0)
			lRet := .F.
			Exit
		Endif
	Endif
Next nItem
U_DebugMsg("Gravação dos itens finalizada.")

If lRet
	// Calcula rentabilidade do atendimento total.
	RecLock("SUA", .F.)
	SUA->UA_VLRLIQ  := MaFisRet(nil, "NF_TOTAL")
	SUA->UA_VALMERC := MaFisRet(nil, "NF_VALMERC")
	SUA->UA_VALBRUT := MaFisRet(nil, "NF_VALMERC")
	SUA->UA_XDESCZF := MaFisRet(nil, "NF_DESCZF")
	SUA->UA_XVALCMP := MaFisRet(nil, "NF_VALCMP")
	SUA->UA_XDIFAL  := MaFisRet(nil, "NF_DIFAL")
	SUA->UA_XVFCPDI := MaFisRet(nil, "NF_VFCPDIF")
	SUA->UA_XBASSOL := MaFisRet(nil, "NF_BASESOL")
	SUA->UA_XVALSOL := MaFisRet(nil, "NF_VALSOL")
	SUA->UA_XVLRLIQ := nPrcLiqTot
	SUA->UA_XCUSTO  := nCustoTot
	SUA->UA_XRENTAB := min(max(If(nPrcLiqTot = 0, 0, (1 - (nCustoTot / nPrcLiqTot)) * 100), -999.99), 999.99)
	If SUA->UA_XRENTAO = 0
		SUA->UA_XRENTAO := SUA->UA_XRENTAB
	Endif

	// Grava a data limite corretamente.
	If IsInCallStack("U_PROMA030") .and. SUA->UA_DTLIM <> M->UA_DTLIM
		SUA->UA_DTLIM := M->UA_DTLIM
	Endif

	// Atendimentos NetSuprimentos e Site ProtCap caem no departamento 01.
	If lNetSupr .Or. lSiteProt
		SUA->UA_XDEPVEN := "01"
	Endif

	SUA->(msUnLock())

	// Grava dados no pedido de venda.
	If lFatur
		sGetArea(aArea, "SC5")
		sGetArea(aArea, "SC6")

		// Posiciona o pedido de venda, para atualização.
		SC5->(dbSetOrder(1))  // C5_FILIAL, C5_NUM.
		If SC5->(dbSeek(xFilial() + SUA->UA_NUMSC5, .F.))
			RecLock("SC5", .F.)

			SC5->C5_XATEND  := SUA->UA_NUM
			SC5->C5_XOPER   := SUA->UA_OPERADO
			SC5->C5_XCONTAT := SUA->UA_CODCONT
			SC5->C5_XTIPO   := SUA->UA_XTIPO
			SC5->C5_XDTPFAT := SUA->UA_XDTPFAT
			SC5->C5_XFATBZL := SUA->UA_XFATBZL
			SC5->C5_XTPPREV := SUA->UA_XTPPREV

			If SUA->UA_TPFRETE == "F" .And. SUA->UA_XTFRDP1 == "2" .and. !empty(cTransRed) // 1=Retira / 2=Redespacho. Somente FOB é Redespacho.
				sGetArea(aArea, "SA4")
				SA4->(dbSetOrder(1))  // A4_FILIAL, A4_COD.
				If SA4->(dbSeek(xFilial() + SUA->UA_TRANSP, .F.))
					// Transportadora de redespacho da PROT-CAP / CIF.
					SC5->C5_TRANSP  := cTransRed
					SC5->C5_TPFRETE := "C"

					// Redespacha para a transportadora da cotação.
					SC5->C5_REDESP  := SUA->UA_TRANSP
					SC5->C5_TFRDP1  := SUA->UA_TPFRETE
					SC5->C5_ESTRDP1 := SA4->A4_EST
					SC5->C5_CMURDP1 := SA4->A4_COD_MUN
				Endif
			Endif

			SC5->C5_XEND    := SUA->UA_ENDENT
			SC5->C5_XBAIRRO := SUA->UA_BAIRROE
			SC5->C5_XCDMUNE := SUA->UA_XCDMUNE
			SC5->C5_XCIDADE := SUA->UA_MUNE
			SC5->C5_ESTPRES := SUA->UA_ESTE
			SC5->C5_XEST    := SUA->UA_ESTE
			SC5->C5_XCEP    := SUA->UA_CEPE
			SC5->C5_XENDC   := SUA->UA_ENDCOB
			SC5->C5_XBAIRRC := SUA->UA_BAIRROC
			SC5->C5_XMUNC   := SUA->UA_MUNC
			SC5->C5_XESTC   := SUA->UA_ESTC
			SC5->C5_XCEPC   := SUA->UA_CEPC
			SC5->C5_XOBSLOG := SUA->UA_XOBSLOG
			SC5->C5_TIPLIB  := "2"  // 2 = Por pedido.
			SC5->C5_XHORA   := Time() // Grava hora no pedido de venda.
			if lPrevTransp
				SC5->C5_XDTENTI := U_PrevTransp(SC5->C5_FILIAL,SC5->C5_NUM, 1)
				SC5->C5_XDTENTF := U_PrevTransp(SC5->C5_FILIAL,SC5->C5_NUM, 2)
			endif
			// Pedido VTEX.
			SC5->C5_XIDVTEX := SUA->UA_XIDVTEX
			SC5->C5_XIDVTV3 := SUA->UA_XIDVTV3
			SC5->C5_XPEDLV  := SUA->UA_XPEDLV

			SC5->C5_XVALFAT := MaFisRet(nil, "NF_BASEDUP")
			SC5->C5_XVALMER := SUA->UA_VALMERC
			SC5->C5_XDESCZF := SUA->UA_XDESCZF
			SC5->C5_XTOTAL  := SUA->UA_VLRLIQ

			SC5->C5_XBASICM := MaFisRet(nil, "NF_BASEICM")
			SC5->C5_XVALICM := MaFisRet(nil, "NF_VALICM")

			SC5->C5_XBASSOL := SUA->UA_XBASSOL
			SC5->C5_XVALSOL := SUA->UA_XVALSOL

			SC5->C5_XBASIPI := MaFisRet(nil, "NF_BASEIPI")
			SC5->C5_XVALIPI := MaFisRet(nil, "NF_VALIPI")

			SC5->C5_XBASPCC := MaFisRet(nil, "NF_BASECF2")
			SC5->C5_XVALPIS := MaFisRet(nil, "NF_VALPS2")
			SC5->C5_XVALCOF := MaFisRet(nil, "NF_VALCF2")

			SC5->C5_XVALCMP := SUA->UA_XVALCMP
			SC5->C5_XDIFAL  := SUA->UA_XDIFAL
			SC5->C5_XVFCPDI := SUA->UA_XVFCPDI

			SC5->C5_XVLRLIQ := SUA->UA_XVLRLIQ
			SC5->C5_XCUSTO  := SUA->UA_XCUSTO
			SC5->C5_XRENTA  := SUA->UA_XRENTAB

			SC5->C5_X_OC    := left(cOrdCom, len(cOrdCom) - 1)
			SC5->C5_XGRPATE := SUA->UA_XGRPATE
			SC5->C5_XCONTR  := SUA->UA_XCONTR
			SC5->C5_XDEPVEN := SUA->UA_XDEPVEN
			SC5->C5_XPMPGTO	:= SUA->UA_XPMPGTO
			SC5->C5_XPMPCAL	:= SUA->UA_XPMPCAL

			//Adequação Norma 2020.006 - SEFAZ - Indicativo do Intermediador/Marketplace (indIntermed)
			If Empty(SUA->UA_XIDVTV3)
				SC5->C5_INDPRES := "3" //3-Operação não presencial, teleatendimento
			Else
				SC5->C5_INDPRES := "2" //2-Operação não presencial, pela internet
				A1U->(DBSetOrder(2)) //A1U_FILIAL, A1U_CGC
				If A1U->(DBSeek(xFilial("A1U") + "19812763000165")) //CNPJ da NetSuprimentos
					SC5->C5_CODA1U := A1U->A1U_CODIGO
				EndIf
			EndIf

			//Grava a origem do Orcamento
			If SC5->(FieldPos("C5_XDTA")) > 0
				SC5->C5_XDTA := SUA->UA_XDTA
			EndIf

			//Manual / Automatico
			If SC5->(FieldPos("C5_XDTAM")) > 0
				SC5->C5_XDTAM := SUA->UA_XDTAM
			EndIf

			SC5->(msUnlock())

			If Empty(SC5->C5_XPEDPAI) .AND. !U_ChkDesm(SC5->C5_NUM) .AND. SC5->C5_XTPPREV <> "2"
				//Atualiza a previsao de Faturamento do Pedido (nao atualizar pedidos desmembrados)
				U_PrevEntr(2, .F., "", 0, NIL, NIL, NIL, NIL, SC5->C5_NUM, SC5->C5_TRANSP)
			EndIf

			// Copia os objetos de conhecimento do atendimento para o pedido.
			CopiaAC9()

			// Executa ponto de entrada no final da gravação do pedido de venda.
			U_M410STTS()
		Else
			Help(,, 'Help',, "Erro na gravação de dados do pedido " + SUA->UA_NUMSC5 + ".", 1, 0)
			lRet := .F.
		Endif
	Endif
	U_DebugMsg("Gravação do cabeçalho finalizada.")
Endif

//Follow-up de clientes, geração da tabela P04
/*If lRet .And. lFollowup .And. !lFatur .And. (Empty(SUA->UA_XBLQCON) .Or. SUA->UA_XBLQCON == '1') .And. !IsInCallStack("TGCANCELA") .and. !IsInCallStack("U_PROMA611")
	SZ8->(dbSetOrder(1))
	If SZ8->(dbSeek(xFilial("SZ8")+SUA->UA_CLIENTE+SUA->UA_LOJA))
		If SZ8->Z8_FUP == 'N'
			lEnvioFup := .F.
		ENDIF
	EndIf
	If lEnvioFup
		aRet800 := u_PROMA800('1',cFilant,SUA->UA_NUM)
		If Len(aRet800) == 0
			Help(,,'Help',,'Registro de follow-up não gerado, não será enviado a aprovação ao cliente',1,0)
		Else
			U_PROMR020(,,AllTrim(aRet800[1,3])) //Grava html no diretório do Sharepoint
		EndIf
	EndIf
EndIf*/

U_DebugMsg("Fim do TMKVFim", "F")

// Restaura areas de trabalho.
(cAlias)->(sRestArea(aArea))
dbSelectArea(cAlias)

Return


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : CopiaAC9
// Descrição: Copia os objetos de conhecimento do atendimento para o pedido.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 25/09/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
Static Function CopiaAC9()
Local cEntidade  := ""
Local cChaveEnt  := ""
Local bChaveEnt  := {|| nX := aScan(aEntidade, {|x| x[1] == cEntidade}), If(nX = 0, Eval(bChaveSX2), MaBuildKey(cEntidade, aEntidade[nX, 2]))}
Local bChaveSX2  := {|| &(cEntidade + "->(" + Posicione("SX2", 1, cEntidade, "X2_UNICO") + ")")}

Local cQuery     := ""
Local cAliasTRB  := GetNextAlias()
Local nX

// Localiza a chave única do atendimento.
cEntidade := "SUA"
cChaveEnt := Eval(bChaveEnt)

cQuery := "select AC9.R_E_C_N_O_ AC9RecN, AC9_CODOBJ CODOBJ " + CRLF
cQuery += "from " + RetSQLName("AC9") + " AC9 " + CRLF
cQuery += "where AC9.D_E_L_E_T_ = ' ' " + CRLF
cQuery += "and AC9.AC9_FILIAL = '" + xFilial("AC9") + "' " + CRLF
cQuery += "and AC9.AC9_FILENT = '" + xFilial(cEntidade) + "' " + CRLF
cQuery += "and AC9.AC9_ENTIDA = '" + cEntidade + "' " + CRLF
cQuery += "and AC9.AC9_CODENT = '" + cChaveEnt + "' " + CRLF
dbUseArea(.T., "TOPCONN", TCGenQry(,, cQuery), cAliasTRB, .F., .T.)

// Grava a amarração dos objetos do atendimento no pedido.
If (cAliasTRB)->(!eof())
	cEntidade := "SC5"
	cChaveEnt := Eval(bChaveEnt)
	Do While (cAliasTRB)->(!eof())
		RecLock("AC9", .T.)
		AC9->AC9_FILIAL := xFilial("AC9")
		AC9->AC9_FILENT := xFilial(cEntidade)
		AC9->AC9_ENTIDA := cEntidade
		AC9->AC9_CODENT := cChaveEnt
		AC9->AC9_CODOBJ := (cAliasTRB)->CODOBJ
		AC9->(msUnLock())

		// Próximo registro.
		(cAliasTRB)->(dbSkip())
	EndDo
Endif
(cAliasTRB)->(dbCloseArea())

Return
