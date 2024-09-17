/*----------------------------------------------------------------------*\
 * FWISxFunB for AdvPL
 * Copyright (C) 2015  Felipe Raposo <feliperaposo@gmail.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
\*----------------------------------------------------------------------*/

#include "protheus.ch"
#include "aarray.ch"
#include "json.ch"
#include "WISStatus.ch"
#include "fileio.ch"

// Definições de vendas.
#define LOC_VENDA       "01"  // Armazém de vendas.

// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Materiais
// Função   : WISPVHst
// Descrição: Consulta status de pedidos no webservice WIS e atualiza histórico.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 11/03/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function WISPVHst()
Local cRet			:= ""
Local cAlias		:= Alias()
Local aArea			:= {}
Local nX			:= 0
Local oJson			:= 0
Local aStatusPV		:= {}
Local cStsCode   	:= ""
Local cCodSit    	:= ""
Local cUltCod    	:= ""
Local cOcorLog   	:= ""
Local cDescLog   	:= ""
Local dDataLog   	:= stod("")
Local cHoraLog   	:= ""
Local cWISDep		:= SuperGetMV("WIS_DEP", NIL, "")

If SuperGetMV("BZ_WMS",, "") == "WIS"
	// Guarda posição das tabelas.
	If empty(cAlias)
		cAlias := "SX3"
		dbSelectArea(cAlias)
	Endif
	aArea := sGetArea()

	If SC5->(!eof())
		oJson := Array(#)
		oJson[#"usuario"]          := "%cUser%"
		oJson[#"senha"]            := "%cPass%"
		oJson[#"nu_pedido_origem"] := SC5->C5_NUM

		// Envia o Json para o webservice.
		cRet := U_WISPost("/saida/status/pedido", oJson, "SC5c", @cStsCode)

		// Se deu certo, trata o Json retornado.
		If cStsCode = "200"
			aStatusPV := FromJson(cRet)
			If ValType(aStatusPV) == "A"
				For nX := 1 to len(aStatusPV)
					cOcorLog := "4"  // 4-Logistica.
					cCodSit  := cValToChar(aStatusPV[nX][#"cd_situacao"])
					
					//Filtra Somente os Pedidos da Unidade Logada no Sistema
					//O retorno do WIS esta vindo com os status dos pedidos das duas unidades (01 e 02)
					If Val(aStatusPV[nX][#"cd_deposito"]) == val(cWISDep)
						// Não trata situações 61 (carga montada) e 64 (aberto).
						If cCodSit <> WMS_PV_CARGA .and. cCodSit <> WMS_PV_ABERTO
							cDescLog := cCodSit + " - " + Capital(Rtrim(aStatusPV[nX][#"ds_situacao"]))
							cHoraLog := aStatusPV[nX][#"dt_situacao"]
							If empty(cHoraLog)
								cHoraLog := dtoc(Date()) + " " + Time()
							Endif
							dDataLog := ctod(left(cHoraLog, at(" ", cHoraLog) - 1))
							cHoraLog := SubStr(cHoraLog, at(" ", cHoraLog) + 1)

							U_HistPV(SC5->C5_NUM, cOcorLog, cDescLog,, "WMS/WIS", dDataLog, cHoraLog)
							cUltCod := cCodSit
						Endif
					Endif
				Next nX

				If cUltCod == WMS_PV_CANCEL  // Pedido cancelado.
					// Se o pedido foi cancelado no WIS, atualiza o status no Protheus.
					If SC5->C5_XWIS <> WMS_C5_CANCEL
						RecLock("SC5", .F.)
						SC5->C5_XWIS := WMS_C5_CANCEL
						SC5->(msUnlock())
					Endif

				ElseIf cUltCod == WMS_PV_CONFERIDO  // Pedido conferido.
					// Se estiver marcado como conferido, busca no WIS o status.
					If empty(SC5->C5_NOTA)
						U_WISSep(SC5->C5_NUM, .F.)
					Endif

				ElseIf (cUltCod == WMS_PV_SEP_INI .or. cUltCod == WMS_PV_CARGA .or. cUltCod == WMS_PV_ABERTO) .and. SC5->(C5_XWIS == " " .or. C5_XWIS == WMS_C5_ENVIAR .or. C5_XWIS == WMS_C5_ERRO)
					RecLock("SC5", .F.)
					SC5->C5_XWIS := If(empty(SC5->C5_NOTA), WMS_C5_PICK, WMS_C5_FATUR)
					SC5->(msUnlock())
				Endif
			Endif
		Endif
	Endif

	// Restaura areas de trabalho.
	(cAlias)->(sRestArea(aArea))
	dbSelectArea(cAlias)
Endif

Return


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Materiais
// Função   : WISSep
// Descrição: Consulta de pedidos separados, aptos a faturar, no webservice WIS.
// Retorno  : Matriz -> {pedidos separados, pedidos excluídos}.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 13/03/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function WISSep(cPedidoP, lExibeMsg)
Local aPedSep    := {0, 0}
Local lRet       := .T.
Local cRet       := ""
Local cAlias     := Alias()
Local aArea      := {}
Local cMsg       := ""
Local nX, nY
Local nIt, nSubIt

Local oJson, aSeparado[0], aDetalhe[0]
Local cStsCode   := ""
Local cOcorLog   := ""

Local cMsgErro   := ""
Local lDevol     := .F.
Local cPedido    := ""
Local cCliente   := ""
Local cCNPJCli   := ""
Local cCliEntr   := ""
Local cCodSit    := ""
Local nVolumes   := 0
Local cEspecie   := ""

Local cItem      := ""
Local cSubItem   := ""
Local cProduto   := ""
Local aItem      := {}
Local aEstrut    := {}
Local nQtdSep    := 0
Local nQtdExp    := 0
Local nPesoL     := 0
Local nPesoB     := 0
Local cPesoL     := SuperGetMv("PC_C5PESOL",, "(SB1->B1_PESO * SC6->C6_QTDVEN)")
Local cPesoB     := SuperGetMv("PC_C5PESOB",, "(SB1->B1_PESO * SC6->C6_QTDVEN) * 1.05")  // Soma 5% ao peso líquido.

Local lDifSep	:= .F.
Local cWisSeq	:= ""
Local lPorta	:= SC5->(FieldPos("C5_XWISPOR")) > 0
Local cPortaExp	:= ""

If SuperGetMV("BZ_WMS",, "") == "WIS"
	Default lExibeMsg := IsInCallStack("msAguarde")

	// Guarda posição das tabelas.
	If empty(cAlias)
		cAlias := "SX3"
		dbSelectArea(cAlias)
	Endif
	aArea := sGetArea()

	oJson := Array(#)
	oJson[#"usuario"] := "%cUser%"
	oJson[#"senha"]   := "%cPass%"
	Default cPedidoP := ""
	If !empty(cPedidoP)
		oJson[#"nu_pedido_origem"] := cPedidoP
	Endif

	If lExibeMsg
		msProcTxt("Consultando WIS...")
		ProcessMessages()
	Endif

	// Envia o Json para o webservice.
	cRet := U_WISPost("/saida/expedicao", oJson, "Sep", @cStsCode)

	// Se deu certo, trata o Json retornado.
	If cStsCode = "200"
		aSeparado := FromJson(cRet)
		If ValType(aSeparado) == "A"
			For nX := 1 to len(aSeparado)
				cMsgErro := ""
				cPedido  := aSeparado[nX][#"nu_pedido_origem"]
				cCliente := aSeparado[nX][#"cd_cliente"]
				cCNPJCli := cValToChar(aSeparado[nX][#"cd_cnpj_cliente"])
				cCodSit  := cValToChar(aSeparado[nX][#"cd_situacao"])
				nVolumes := aSeparado[nX][#"qt_volumes"]
				cEspecie := "CAIXA"
				cWisSeq	 := StrZero(aSeparado[nX][#"nu_pedido"], TamSX3("C5_XWISSEQ")[1])		//Sequencial WIS

				If lPorta
					cPortaExp := AllTrim(Str(aSeparado[nX][#"cd_porta"]))
				EndIf

				aDetalhe := aSeparado[nX][#"detalhe"]
				Default aDetalhe := {}
				lDifSep	 := .F.

				// Posiciona o pedido.
				SC5->(dbSetOrder(1))  // C5_FILIAL, C5_NUM.
				If SC5->(dbSeek(xFilial() + cPedido, .F.))

					//So processa se o Status for mais novo que o ultimo envio para o WIS (Picking)
					If cWisSeq > SC5->C5_XWISSEQ

						// Indica se o pedido é de devolução para fornecedor.
						lDevol := (aScan({'D', 'B'}, SC5->C5_TIPO) > 0)

						// Posiciona cliente ou fornecedor.
						cCliEntr := SC5->(C5_CLIENTE + If(empty(C5_LOJAENT), C5_LOJACLI, C5_LOJAENT))
						If lDevol
							SA2->(dbSetOrder(1))  // A2_FILIAL, A2_COD, A2_LOJA.
							SA2->(msSeek(xFilial() + cCliEntr, .F.))
							lRet := (val(SA2->A2_CGC) = val(cCNPJCli))
						Else
							SA1->(dbSetOrder(1))  // A1_FILIAL, A1_COD, A1_LOJA.
							SA1->(msSeek(xFilial() + cCliEntr, .F.))
							lRet := (val(SA1->A1_CGC) = val(cCNPJCli) .and. cCliente = SC5->C5_CLIENTE)
						Endif

						If lRet
							If lExibeMsg
								msProcTxt("Acertando pedido " + cPedido + "...")
								ProcessMessages()
							Endif

							If cCodSit == WMS_PV_CANCEL  // Pedido cancelado.
								If SC5->C5_XWIS <> WMS_C5_NAOENVIA
									RecLock("SC5",.F.)
									SC5->C5_XWIS    := WMS_C5_CANCEL
									SC5->C5_VOLUME1 := nVolumes
									SC5->C5_ESPECI1 := cEspecie
									SC5->C5_XWISSEQ := cWisSeq
									SC5->(msUnlock())

									U_HistPV(SC5->C5_NUM, "4", "Status WIS - " + cCodSit, "Pedido cancelado no WIS", "WMS/WIS")
								Endif
								aPedSep[2]++

							ElseIf len(aDetalhe) > 0  // Verifica se o WIS retornou item do pedido.
								nPesoL := 0
								nPesoB := 0
								aItem  := {}
								SC6->(dbSetOrder(1))  // C6_FILIAL, C6_NUM, C6_ITEM, C6_PRODUTO.
								SC6->(dbSeek(xFilial() + SC5->C5_NUM, .F.))
								Do While SC6->(!eof() .and. C6_FILIAL + C6_NUM == xFilial() + SC5->C5_NUM)
									aEstrut := {}
									If !empty(SC6->C6_NUMOP)
										aEstrut := U_WISStru(SC6->C6_PRODUTO, SC6->C6_QTDVEN)
									Endif
									If empty(aEstrut)
										aAdd(aEstrut, {SC6->C6_PRODUTO, SC6->C6_QTDVEN, 0, 0})
									Endif
									aAdd(aItem, {SC6->C6_ITEM, SC6->C6_PRODUTO, aEstrut})

									// Pesos líquido e bruto do pedido.
									SB1->(dbSetOrder(1))  // B1_FILIAL, B1_COD.
									SB1->(msSeek(xFilial() + SC6->C6_PRODUTO, .F.))
									nPesoL += &(cPesoL)
									nPesoB += &(cPesoB)

									SC6->(dbSkip())
								EndDo

								// Processa os itens retornados pelo WIS.
								For nY := 1 to len(aDetalhe)
									cItem    := aDetalhe[nY][#"nu_item_corp"]
									cSubItem := SubStr(cItem, 3, 2)
									cItem    := left(cItem, 2)
									cProduto := PadR(aDetalhe[nY][#"cd_produto"], TamSX3("C6_PRODUTO")[1])
									nQtdSep  := aDetalhe[nY][#"qt_separar"]
									nQtdExp  := aDetalhe[nY][#"qt_expedida"]

									SC6->(dbSetOrder(1))  // C6_FILIAL, C6_NUM, C6_ITEM, C6_PRODUTO.
									If SC6->(dbSeek(xFilial() + SC5->C5_NUM + cItem, .F.))
										nIt := aScan(aItem, {|x| x[1] == cItem})
										If nIt > 0
											aEstrut := aItem[nIt, 3]
											nSubIt  := aScan(aEstrut, {|x| x[1] == cProduto})
											If nSubIt > 0
												// Calcula quantidade proporcional do produto original (produto acabado) de acordo com a separação da matéria prima (produto empenhado).
												// Qtde relativa = (qtde separada / qtde empenhada) * qtde do produto orig.
												aEstrut[nSubIt, 3] += (nQtdSep / aEstrut[nSubIt, 2]) * SC6->C6_QTDVEN
												aEstrut[nSubIt, 4] += (nQtdExp / aEstrut[nSubIt, 2]) * SC6->C6_QTDVEN
											ElseIf nQtdExp > 0
												// Item separado a mais do que consta no pedido.
												// Talvez a estrutura ou a OP do produto tenha sido alterada após o envio do pedido para o WIS.
												aAdd(aEstrut, {cProduto, 0, 0, 0})
												cMsgErro := "O produto '" + cItem + "/" + cProduto + "' (qtde " + cValToChar(nQtdExp) + ") foi separado e não consta no pedido de venda. Favor verificar."
											Endif
										Endif
									ElseIf nQtdExp > 0
										// Item separado a mais do que consta no pedido.
										// Não deve acontecer, já que o Protheus não permite alterar pedido que esteja em separação.
										cMsgErro := "O produto '" + cItem + "/" + cProduto + "' (qtde " + cValToChar(nQtdExp) + ") foi separado e não consta no pedido de venda. Favor verificar."
									Endif
								Next nY

								// Confere se os itens foram separados corretamente.
								For nIt := 1 to len(aItem)
									SC6->(dbSetOrder(1))  // C6_FILIAL, C6_NUM, C6_ITEM, C6_PRODUTO.
									If SC6->(dbSeek(xFilial() + SC5->C5_NUM + aItem[nIt, 1] + aItem[nIt, 2], .F.))
										nQtdSep := 0
										nQtdExp := 0
										aEstrut := aItem[nIt, 3]
										If len(aEstrut) > 0
											nQtdSep := SC6->C6_QTDVEN
											nQtdExp := SC6->C6_QTDVEN
											For nSubIt := 1 to len(aEstrut)
												nQtdSep := min(nQtdSep, aEstrut[nSubIt, 3])
												nQtdExp := min(nQtdExp, aEstrut[nSubIt, 4])
											Next nSubIt
										Endif

										If SC6->(C6_XQTSEP <> nQtdSep .or. C6_XQTEXP <> nQtdExp)
											RecLock("SC6", .F.)
											SC6->C6_XQTSEP := nQtdSep
											SC6->C6_XQTEXP := nQtdExp
											SC6->(msUnLock())
										Endif

										If SC6->C6_QTDVEN <> SC6->C6_XQTEXP
											cMsgErro := "O produto '" + SC6->C6_ITEM + "/" + SC6->C6_PRODUTO + "' (qtde " + cValToChar(SC6->C6_QTDVEN) + ") não foi separado. Favor verificar."
											lDifSep  := .T.
										Endif
									Endif
								Next nIt

								RecLock("SC5", .F.)
								If nVolumes <> 0
									SC5->C5_VOLUME1 := nVolumes
								Endif
								SC5->C5_ESPECI1 := cEspecie
								SC5->C5_PESOL   := nPesoL
								SC5->C5_PBRUTO  := nPesoB
								If !empty(cMsgErro)
									SC5->C5_XWIS := WMS_C5_ERRO
								ElseIf cCodSit == WMS_PV_SEP_INI .or. cCodSit == WMS_PV_CONFERIDO  // Separação iniciada ou pedido conferido.
									SC5->C5_XWIS := WMS_C5_CONF
									aPedSep[1]++
								Endif
								If lPorta .AND. !Empty(cPortaExp)
									SC5->C5_XWISPOR := cPortaExp
								EndIf
								SC5->(msUnlock())

								If !empty(cMsgErro)
									cOcorLog := "4"  // 4-Logistica.
									U_HistPV(SC5->C5_NUM, cOcorLog, "Status WIS - " + cCodSit, cMsgErro, "WMS/WIS")
									MsgAlert(cMsgErro, "Atenção - pedido " + SC5->C5_NUM)
								Endif
							Endif
						Endif
					
						If lRet .AND. lDifSep .AND. !Empty(cPedidoP)
							If !IsInCallStack("U_PROMM020") .AND. MsgYesNo("Executa o ajuste de quantidades para o Pedido " + SC5->C5_NUM + "?", "WISSep")
								U_WISXSC6(SC5->C5_NUM)
							EndIf
						EndIf
					Endif
				Endif
			Next nX
		Endif
	Endif

	// Restaura areas de trabalho.
	(cAlias)->(sRestArea(aArea))
	dbSelectArea(cAlias)

	cMsg := "Total de pedidos processados " + cValToChar(aPedSep[1] + aPedSep[2]) + ":" + CRLF +;
	"  Pedidos separados:  " + cValToChar(aPedSep[1]) + CRLF +;
	"  Pedidos cancelados: " + cValToChar(aPedSep[2])
	If lExibeMsg
		MsgInfo(cMsg, "Atenção")
	Endif
Endif

Return aPedSep


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Materiais
// Função   : WISNFEnt
// Descrição: Consulta status da notas fiscais de entrada.
// Retorno  : Matriz -> {notas aceitas, notas rejeitadas}.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 18/03/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function WISNFEnt(cNotaP)
	Local aArea			:= GetArea()
	Local aAreaSD1		:= SD1->(GetArea())
	Local aAreaSF1		:= SF1->(GetArea())
	Local aDetalhe		:= {}
	Local aEstrut		:= {}
	Local aFilWIS		:= U_WISDPFIL()
	Local aItem			:= {}
	Local aNFProc		:= {}
	Local aReceb		:= {}
	Local cStsCode   	:= ""

	Local cAgenda		:= ""
	Local cFilBkp		:= cFilAnt
	Local cCodSitIt 	:= ""
	Local cCodSitNF 	:= ""
	Local cItem     	:= ""
	Local cMsgErro		:= ""
	Local cNota			:= ""
	Local cProdConf 	:= ""
	Local cProduto		:= ""
	Local cRet			:= ""

	Local nX			:= 0
	Local nY			:= 0
	Local nIt			:= 0
	Local nQtdConf  	:= 0
	Local nQtdAvar  	:= 0
	Local nNFProc		:= 0
	Local nSubIt		:= 0
	Local nTamF1Chv  	:= TamSX3("F1_DOC")[1] + TamSX3("F1_SERIE")[1] + TamSX3("F1_FORNECE")[1] + TamSX3("F1_LOJA")[1]
	Local nTotConf		:= 0
	Local nTotRej		:= 0

	Local lDebug		:= .F.
	Local lDevMont 		:= .F.
	Local lMsAguarde	:= IsInCallStack("msAguarde")

	Local oJson			:= NIL

	Local aItensNFE		:= {}
	Local nPosFab		:= 0
	Local lDevFab		:= .F.	//Determina se e NF de Devolucao de Beneficiamento da Fabrica (Empresa 12 / Filial 11-Ivai)
	Local nItem			:= 0
	Local lContinua		:= .T.
	Local aItConf		:= {}

	Default cNotaP 		:= ""

	If SuperGetMV("BZ_WMS",, "") == "WIS"
		oJson := Array(#)
		oJson[#"usuario"] := "%cUser%"
		oJson[#"senha"]   := "%cPass%"
	EndIf
	If !Empty(cNotaP)
		oJson[#"nu_nota"] := cNotaP
	Endif

	If lMsAguarde
		msProcTxt("Consultando WIS...")
		ProcessMessages()
	Endif

	If lDebug
		cRet := U_WISRDTXT("D:\retorno.json")

		If !Empty(cRet)
			cStsCode := "200"
		EndIf
	Else
		//Envia o Json para o webservice.
		cRet := U_WISPost("/saida/recebimento", oJson, "NFEntr", @cStsCode)
	EndIf

	// Se deu certo, trata o Json retornado.
	If cStsCode = "200"
		aReceb := FromJson(cRet)
		If ValType(aReceb) == "A"
			//Armazena os numeros de NFs e somente a ultima agenda para processar somente o ultimo retorno
			For nX := 1 to Len(aReceb)
				cNota 	:= PadR(aReceb[nX][#"nu_doc_erp"], nTamF1Chv)
				cAgenda	:= aReceb[nX][#"cd_agenda"]
				nNFProc	:= aScan(aNFProc, {|x| x[01] == cNota})

				If nNFProc > 0
					If cAgenda > aNFProc[nNFProc][02]
						aNFProc[nNFProc][02] := cAgenda
					EndIf
				Else
					Aadd(aNFProc, {cNota, cAgenda})
				EndIf
			Next nX
			//Processamento dos registros e gravacao dos dados da NF
			For nX := 1 to Len(aReceb)
				cNota 		:= PadR(aReceb[nX][#"nu_doc_erp"], nTamF1Chv)
				cAgenda		:= aReceb[nX][#"cd_agenda"]
				cFilAnt		:= aFilWis[AScan(aFilWIS, {|x| x[02] == aReceb[nX][#"cd_deposito"]})][01]

				//Processa somente o ultimo retorno do Recebimento
				If AScan(aNFProc, {|x| x[1] + x[2] == cNota + cAgenda}) > 0
					nY 			:= 1
					aDetalhe	:= aReceb[nX][#"detalhe"]
					Default aDetalhe := {}

					If lMsAguarde
						msProcTxt("Acertando documento " + cNota + "...")
						ProcessMessages()
					Endif

					// Posiciona a nota.
					SF1->(dbSetOrder(1))  // F1_FILIAL, F1_DOC, F1_SERIE, F1_FORNECE, F1_LOJA, F1_TIPO.
					If SF1->(dbSeek(xFilial() + cNota, .F.))
						If SF1->F1_XWIS == WMS_F1_FINALIZ
							If !IsBlind()
								MsgAlert("Nota fiscal " + cNota + " já finalizada.", "Atenção")
							EndIf
						Else
							If !Empty(aDetalhe)
								//Ordena o Array na Ordem dos Itens da NF
								ASort(aDetalhe,,, {|x, y| x[#"nu_item_corp"] < y[#"nu_item_corp"]})

								//Determina se e NF de Devolucao de Beneficiamento da Fabrica (Empresa 12 / Filial 11-Ivai)
								lDevFab := U_FabrBzl(SF1->F1_TIPO, SF1->F1_FORNECE, SF1->F1_LOJA, SF1->F1_SERIE, SF1->F1_DOC, @aItensNFE)

								If lDevFab
									For nItem := 1 to Len(aDetalhe)
										cItem		:= PadR(aDetalhe[nItem][#"nu_item_corp"], TamSX3("D1_ITEM")[1])
										nPosFab		:= AScan(aItensNFE, {|x| x[03] == cItem})

										If nPosFab > 0
											DbSelectArea("SD1")
											DbSetOrder(1)

											SD1->(DbGoTo(aItensNFE[nPosFab][04]))

											If aItensNFE[nPosFab][04] == SD1->(Recno())
												cCodSitNF := WMS_F1_OK
												cProdConf := PadR(aDetalhe[nItem][#"cd_produto"], TamSX3("D1_COD")[1])
												cCodSitIt := cValToChar(aDetalhe[nItem][#"cd_situacao"])
												nQtdConf  := aDetalhe[nItem][#"qt_conferido"]
												nQtdAvar  := aDetalhe[nItem][#"qt_avaria"]

												If cCodSitIt == "9"  // Conferido.
													If cProdConf <> aItensNFE[nPosFab][01]
														cMsgErro	:= "O produto " + cProdConf + " não é o produto (" + aItensNFE[nPosFab][01] + ") enviado para conferencia."
														lContinua	:= .F.
													ElseIf nQtdConf > aItensNFE[nPosFab][2]
														cMsgErro  := "A quantidade conferida (" + AllTrim(Str(nQtdConf)) + ") do produto " + cProdConf + " é maior que a quantidade (" + AllTrim(Str(aItensNFE[nPosFab][02])) + ") da Ordem de Produção."
														lContinua := .F.
													EndIf
												Else
													cMsgErro  := "Erro na conferência do produto '" + cItem + "/" + cProdConf + "' (qtde " + cValToChar(nQtdConf) + ") - cod. situação " + cCodSitIt + ". Favor verificar."
													lContinua := .F.
												EndIf
											Else
												cMsgErro := "Item " + cItem + " não localizado na NF " + cNota + "."
												lContinua := .F.
											EndIf
										Else
											cMsgErro := "Item " + cItem + " não localizado na NF " + cNota + "."
											lContinua := .F.
										EndIf

										//Nao encontrou o item retornado nos itens enviados
										If lContinua
											RecLock("SD1", .F.)
												SD1->D1_QTDCONF := nQtdConf + nQtdAvar
												SD1->D1_XQTAVAR := nQtdAvar
												SD1->D1_XWIS    := WMS_F1_OK
											SD1->(msUnlock())
										
											Aadd(aItConf, SD1->D1_ITEM)
										Else
											If cCodSitIt == "28"
												cCodSitNF := WMS_F1_CANCEL
											Else
												cCodSitNF := WMS_F1_ERRO
											EndIf

											If !IsBlind()
												MsgAlert(cMsgErro, "Atenção")
											EndIf
											cMsgErro := ""

										EndIf
									Next nItem

									If Len(aItConf) <> Len(aItensNFE)
										RecLock("SF1", .F.)
											SF1->F1_XWIS := WMS_F1_ERRO
										SF1->(msUnlock())

										If !IsBlind()
											MsgAlert("Quantidade de Itens conferida não confere com a quantidade enviada.", "Atenção")
										EndIf
									Else
										If SF1->F1_XWIS <> cCodSitNF
											RecLock("SF1", .F.)
												SF1->F1_XWIS := cCodSitNF
											SF1->(msUnlock())
										ElseIf cCodSitNF == "1"
											RecLock("SF1", .F.)
												SF1->F1_XWIS := WMS_F1_FINALIZ
											SF1->(msUnlock())
										Endif

										//Tratar transferencia entre armazens
										U_SF1TrfFab(.F., aItensNFE)
									EndIf
								Else
									aItem := {}
									SD1->(dbSetOrder(1))  // D1_FILIAL, D1_DOC, D1_SERIE, D1_FORNECE, D1_LOJA, D1_COD, D1_ITEM.
									SD1->(dbSeek(xFilial("SD1") + SF1->(F1_DOC + F1_SERIE + F1_FORNECE + F1_LOJA), .F.))
									While SD1->(!Eof() .AND. D1_FILIAL + D1_DOC + D1_SERIE + D1_FORNECE + D1_LOJA == xFilial("SD1") + SF1->(F1_DOC + F1_SERIE + F1_FORNECE + F1_LOJA))
										lDevMont := .F.
										If SD1->D1_TIPO == "D"
											SD2->(dbSetOrder(3))  // D2_FILIAL, D2_DOC, D2_SERIE, D2_CLIENTE, D2_LOJA, D2_COD, D2_ITEM.
											If SD2->(dbSeek(xFilial("SD1") + SD1->(D1_NFORI + D1_SERIORI + D1_FORNECE + D1_LOJA + D1_COD + D1_ITEMORI), .F.))
												SC6->(dbSetOrder(1))  // C6_FILIAL, C6_NUM, C6_ITEM, C6_PRODUTO.
												lDevMont := SC6->(dbSeek(xFilial("SC6") + SD2->(D2_PEDIDO + D2_ITEMPV), .F.) .AND. !Empty(SC6->C6_NUMOP))
											Endif
										Endif
										aEstrut := {}
										If lDevMont
											aEstrut := U_WISStru(SD1->D1_COD, SD1->D1_QUANT)
										Endif
										If empty(aEstrut)
											aAdd(aEstrut, {SD1->D1_COD, SD1->D1_QUANT, 0, 0})
										Endif
										aAdd(aItem, {SD1->D1_ITEM, SD1->D1_COD, aEstrut})

										SD1->(dbSkip())
									End

									nY := 1

									While nY <= Len(aDetalhe)
										// Processa os itens da nota.
										cCodSitNF := WMS_F1_OK
										cItem     := PadR(aDetalhe[nY][#"nu_item_corp"], TamSX3("D1_ITEM")[1])
										cProdConf := PadR(aDetalhe[nY][#"cd_produto"], TamSX3("D1_COD")[1])
										cCodSitIt := cValToChar(aDetalhe[nY][#"cd_situacao"])
										nQtdConf  := 0
										nQtdAvar  := 0

										While nY <= Len(aDetalhe) .AND. cItem == PadR(aDetalhe[nY][#"nu_item_corp"], TamSX3("D1_ITEM")[1]) .AND. cProdConf == PadR(aDetalhe[nY][#"cd_produto"], TamSX3("D1_COD")[1])

											nQtdConf  += aDetalhe[nY][#"qt_conferido"]
											nQtdAvar  += aDetalhe[nY][#"qt_avaria"]
											nY++
										End

										If cCodSitIt == "9"  // Conferido.
											nIt := aScan(aItem, {|x| x[1] == cItem})
											If nIt > 0
												cProduto := aItem[nIt, 2]
												SD1->(dbSetOrder(1))  // D1_FILIAL, D1_DOC, D1_SERIE, D1_FORNECE, D1_LOJA, D1_COD, D1_ITEM.
												If SD1->(dbSeek(xFilial() + SF1->(F1_DOC + F1_SERIE + F1_FORNECE + F1_LOJA) + cProduto + cItem, .F.))
													aEstrut := aItem[nIt, 3]
													nSubIt  := aScan(aEstrut, {|x| x[1] == cProdConf})
													If nSubIt > 0
														// Calcula quantidade proporcional do produto original (produto acabado) de acordo com a conferência da matéria prima (produto empenhado).
														// Qtde relativa = (qtde conferida / qtde empenhada) * qtde do produto orig.
														aEstrut[nSubIt, 3] += (nQtdConf / aEstrut[nSubIt, 2]) * SD1->D1_QUANT
														aEstrut[nSubIt, 4] += (nQtdAvar / aEstrut[nSubIt, 2]) * SD1->D1_QUANT
													ElseIf cProdConf == cProduto
														aEstrut := {{SD1->D1_COD, SD1->D1_QUANT, nQtdConf, nQtdAvar}}
													ElseIf nQtdConf > 0
														// Item conferido a mais do que consta no pedido.
														// Talvez a estrutura ou a OP do produto tenha sido alterada após o envio do pedido para o WIS.
														cMsgErro := "O produto '" + cItem + "/" + cProdConf + "' (qtde " + cValToChar(nQtdConf) + ") foi conferido e não consta na nota de entrada. Favor verificar."
													Endif
												Endif
											Endif
										Else
											cMsgErro  := "Erro na conferência do produto '" + cItem + "/" + cProdConf + "' (qtde " + cValToChar(nQtdConf) + ") - cod. situação " + cCodSitIt + ". Favor verificar."
										Endif
									End

									If empty(cMsgErro)
										For nIt := 1 to len(aItem)
											cItem    := aItem[nIt, 1]
											cProduto := aItem[nIt, 2]
											aEstrut  := aItem[nIt, 3]

											SD1->(dbSetOrder(1))  // D1_FILIAL, D1_DOC, D1_SERIE, D1_FORNECE, D1_LOJA, D1_COD, D1_ITEM.
											If SD1->(dbSeek(xFilial("SD1") + SF1->(F1_DOC + F1_SERIE + F1_FORNECE + F1_LOJA) + cProduto + cItem, .F.))
												nQtdConf := 0
												nQtdAvar := 0
												If len(aEstrut) > 0
													nQtdConf := aEstrut[1, 3]
													nQtdAvar := aEstrut[1, 4]
													For nSubIt := 2 to len(aEstrut)
														nQtdConf := min(nQtdConf, aEstrut[nSubIt, 3])
														nQtdAvar := min(nQtdAvar, aEstrut[nSubIt, 4])
													Next nSubIt
												Endif

												RecLock("SD1", .F.)
												SD1->D1_QTDCONF := nQtdConf + nQtdAvar
												SD1->D1_XQTAVAR := nQtdAvar
												SD1->D1_XWIS    := WMS_F1_OK
												SD1->(msUnlock())
											Else
												If !IsBlind()
													MsgAlert("Item " + cItem + " [" + cProduto + "] não encontrado na nota fiscal.", "NF " + cNotaP)
												EndIf
												cCodSitNF := WMS_F1_ERRO
												Exit
											Endif
										Next nIt
									Else
										If cCodSitIt == "28"
											cCodSitNF := WMS_F1_CANCEL
										Else
											cCodSitNF := WMS_F1_ERRO
										EndIf
										If !IsBlind()
											MsgAlert(cMsgErro, "Atenção")
										EndIf
										cMsgErro := ""
									Endif

									If SF1->F1_XWIS <> cCodSitNF
										RecLock("SF1", .F.)
										SF1->F1_XWIS := cCodSitNF
										SF1->(msUnlock())
									Endif

									If cCodSitNF == WMS_F1_OK
										nTotConf++

										// Se a notas estiver classificada, efetua a transferência automática de armazéns.
										If SF1->F1_STATUS = "A"
											U_SF1TrfVen()
										Endif
									Else
										nTotRej++
										If !empty(cNotaP)
											If !IsBlind()
												MsgAlert("Divergência de quantidade foi reportada na conferência WIS.", "NF " + cNotaP)
											EndIf
										Endif
									Endif
								EndIf
							EndIf
						EndIf
					Else
						If !IsBlind()
							MsgAlert("Nota fiscal " + cNota + " não encontrada. Verifique se a NF foi excluida no ERP.", "Atenção")
						EndIf
					Endif
				EndIf
			Next nX
		EndIf
	EndIf

	If lMsAguarde
		cMsg := "Total de notas processadas " + cValToChar(nTotConf + nTotRej) + ":" + CRLF +;
		"  Notas conferidas: " + cValToChar(nTotConf) + CRLF +;
		"  Notas rejeitadas: " + cValToChar(nTotRej)

		MsgInfo(cMsg, "Atenção")
	Endif

	RestArea(aAreaSF1)
	RestArea(aAreaSD1)
	RestArea(aArea)

	cFilAnt := cFilBkp

Return NIL
// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Materiais
// Função   : WISCEst
// Descrição: Consulta estoque no webservice WIS.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 11/03/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function WISCEst(cProduto, cLocal, lExibe)
Local cRet       := ""
Local cAlias     := Alias()
Local aArea      := {}
Local lMsAguarde := IsInCallStack("msAguarde") .and. Type("oText") = "O"
Local aEstoqDisp := {}
Local nQtdeWis   := 0
Local nX

Local oJson, aEstoqWIS[0]
Local cStsCode   := ""

Local oDialg, oGrpTot
Local aCabec     := {}
Local aTam       := {}
Local nSaldoSB2  := 0

If SuperGetMV("BZ_WMS",, "") == "WIS"
	// Guarda posição das tabelas.
	If empty(cAlias)
		cAlias := "SX3"
		dbSelectArea(cAlias)
	Endif
	aArea := sGetArea()

	Default cProduto := SB2->B2_COD
	Default cLocal   := SB2->B2_LOCAL
	Default lExibe   := .F.

	// Posiciona tabela.
	SB2->(dbSetOrder(1))   // B2_FILIAL, B2_COD, B2_LOCAL.
	If SB2->(dbSeek(xFilial() + PadR(cProduto, TamSX3("B1_COD")[1]) + cLocal, .F.))
		If U_WISSimu()
			nQtdeWis := SB2->B2_QATU
			aAdd(aEstoqDisp, {"Armazém de testes (simulação)", Transform(nQtdeWis, "@E 999,999,999"), ""})
		Else
			oJson := Array(#)
			oJson[#"usuario"]          := "%cUser%"
			oJson[#"senha"]            := "%cPass%"
			oJson[#"cd_produto"]       := SB2->B2_COD
			oJson[#"ds_area_erp" ]     := SB2->B2_LOCAL

			If lMsAguarde
				msProcTxt("Consultando estoque [" + SB2->(B2_COD + "/" + B2_LOCAL) + "]...")
				ProcessMessages()
			Endif

			// Envia o Json para o webservice.
			cRet := U_WISPost("/saida/estoque", oJson, "SB2c", @cStsCode)

			// Se deu certo, trata o Json retornado.
			If cStsCode = "200"
				// Grava log de alterações do pedido.
				aEstoqWIS := FromJson(cRet)
				If ValType(aEstoqWIS) == "A"
					For nX := 1 to len(aEstoqWIS)
						aAdd(aEstoqDisp, {aEstoqWIS[nX][#"ds_area_armaz"], Transform(aEstoqWIS[nX][#"qt_estoque"], "@E 999,999,999"), ""})
						nQtdeWis += aEstoqWIS[nX][#"qt_estoque"]
					Next nX
				Endif
			Else
				Default cRet := ""
			Endif
		Endif
	Endif

	// Exibe o estoque do WIS.
	If lExibe
		DEFINE MSDIALOG oDialog TITLE "Estoque WIS - " + SB2->(B2_COD + "/" + B2_LOCAL) FROM 0, 0 TO 300, 580 PIXEL

		// Grid com os motivos.
		aCabec  := {"Armazém", "Disponível", ""}
		aTam    := {240, 30, 0}
		oGdServ := TWBrowse():New(0, 0, 0, 0,, aCabec, aTam, oDialg,,,,,,,,,,,,,,,,,, .T., .T.)
		oGdServ:Align := CONTROL_ALIGN_ALLCLIENT
		If empty(aEstoqDisp)
			aAdd(aEstoqDisp, {"Não há estoque disponível", "0", ""})
		Endif
		oGdServ:SetArray(aEstoqDisp)
		oGdServ:bLine := {|| aEstoqDisp[oGdServ:nAt]}
		oGdServ:nAt   := 1

		// Grupo de motivos.
		oGrpTot := TScrollBox():New(oDialg, 0, 0, 16, 0, .F., .F., .T.)
		oGrpTot:Align := CONTROL_ALIGN_BOTTOM

		// Saldo disponível no Protheus.
		nSaldoSB2 := SaldoSB2()
		@ 02, 001 SAY "Disp. Protheus"  OF oGrpTot PIXEL
		@ 02, 040 MSGET nSaldoSB2 SIZE 040, 008 of oGrpTot picture "@E 999,999,999" WHEN .F. PIXEL

		// Total disponível no WIS.
		@ 02, 101 SAY "Total disp. WIS" OF oGrpTot PIXEL
		@ 02, 140 MSGET nQtdeWis  SIZE 040, 008 of oGrpTot picture "@E 999,999,999" WHEN .F. PIXEL

		tButton():New(02, 253, "Sair", oGrpTot, {|| oDialog:End()}, 35, 10,,,, .T.)

		ACTIVATE MSDIALOG oDialog CENTERED
	Endif

	// Restaura areas de trabalho.
	(cAlias)->(sRestArea(aArea))
	dbSelectArea(cAlias)
Endif

Return nQtdeWis


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : WISInvE
// Descrição: Consulta inventário estático no WIS.
// Retorno  : Lógico, indicando se a função foi executada com sucesso.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 22/07/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function WISInvE()
Local oProcess
Local aParamBox  := {}
Local aRetParam  := {}

If SuperGetMV("BZ_WMS",, "") == "WIS"
	// Abre parâmetros para o usuário.
	aAdd(aParamBox, {2, "Documento",      "1-Usa do WIS", {"1-Usa do WIS", "2-Doc. informado"}, 70,, .T.})
	aAdd(aParamBox, {1, "Doc. informado", CriaVar("B7_DOC",  .T.),,,, "mv_par01 = '2'",, .F.})
	aAdd(aParamBox, {1, "Data invent.",   CriaVar("B7_DATA", .T.),,,,, 50, .T.})
	If ParamBox(aParamBox, "Informe os parâmetros", @aRetParam,,,,,,,, .F., .F.)
		If aRetParam[1] = '2' .and. empty(aRetParam[2])
			MsgAlert("Informe um número de documento", "Atenção")
		Else
			oProcess  := MsNewProcess():New({|lEnd| WISInvE(oProcess, aRetParam)}, "Inventário estático")
			oProcess:Activate()
		Endif
	Endif
Endif

Return

Static Function WISInvE(oProcess, aRetParam)
Local aRet       := {}
Local lErro      := .F.
Local nX

Local oJson
Local cStsCode   := ""
Local aRetJson   := {}
Local cJson      := ""
Local nArqs      := 0

Local lDocWIS    := (aRetParam[1] = '1')
Local cDocumento := aRetParam[2]
Local dInvent    := aRetParam[3]

Local nHdl       := 0
Local nTamArq    := 0

Local aInvent    := {}
Local aReg       := {}
Local nReg       := 0
Local nQtdeRegs  := 0
Local cPorcent   := ""
Local cMsgProc   := ""

Local nTamSB1    := TamSX3("B8_PRODUTO")[1]
Local nTamSB7    := TamSX3("B7_DOC")[1]
Local cProduto   := ""
Local nQtdeInv   := 0

Private lMsErroAuto := .F.

// Acerta a barra de progressão superior.
oProcess:SetRegua1(1)
oProcess:IncRegua1("Consultando WIS...")

oJson := Array(#)
oJson[#"usuario"] := "%cUser%"
oJson[#"senha"]   := "%cPass%"

// Envia o Json para o webservice.
aRet := U_WISAPost("/saida/inventario/estatico", oJson, "SB7", nil, {|nCount, cRetPost, cStsCode, cHeadRet| !(cRetPost = "NENHUMA ")})

// Acerta a barra de progressão superior.
nArqs := len(aRet)
oProcess:SetRegua1(nArqs + 1)

// Se deu certo, trata o Json retornado.
For nX := 1 to nArqs
	lErro    := .F.
	aRetJson := aRet[nX]
	oProcess:IncRegua1("Arquivo " + cValToChar(nX) + " de " + cValToChar(nArqs) + "...")

	oProcess:SetRegua2(2)
	oProcess:IncRegua2("Abrindo arquivo...")
	nHdl := fOpen(aRetJson[3], FO_EXCLUSIVE)
	If nHdl > 0
		fSeek(nHdl, 0, 0)
		nTamArq := fSeek(nHdl, 0, 2)
		fSeek(nHdl, 0, 0)
		cJson := Space(nTamArq)
		fRead(nHdl, @cJson, nTamArq)
		fClose(nHdl)

		cStsCode := aRetJson[4]

		// Processa os registros do arquivo.
		If (cJson = "NENHUMA ")
			lErro := .T.
		Else
			aInvent := FromJson(cJson)
			If ValType(aInvent) == "A"
				nQtdeRegs := len(aInvent)
				oProcess:SetRegua2(nQtdeRegs)
				For nReg := 1 to nQtdeRegs
					aReg       := aInvent[nReg]
					cPorcent   := Transform((nReg / nQtdeRegs) * 100, "@E 999.9") + "%"
					If lDocWIS
						cDocumento := "WIS" + StrZero(aReg[#'nu_inventario'], nTamSB7 - 3)
					Endif
					cProduto   := PadR(aReg[#'cd_produto'], nTamSB1)
					nQtdeInv   := aReg[#'qt_produto']
					SB1->(dbSetOrder(1))  // B1_FILIAL, B1_COD.
					SB1->(msSeek(xFilial() + cProduto, .F.))
					cMsgProc := "[" + cDocumento + "] - [" + cProduto + "] - qtde [" + cValToChar(nQtdeInv) + "] " + cPorcent
					oProcess:IncRegua2(cMsgProc)

					// Insere inventário, considerando os lotes.
					If !U_Invent(cDocumento, dInvent, cProduto, LOC_VENDA, nQtdeInv)
						lErro := .T.
						Exit
					Endif
				Next nReg
				oProcess:IncRegua2("Encerrando arquivo...")
			Else
				lErro := .T.
			Endif
		Endif

		// Move arquivos para pasta de processados.
		U_WISProc(aRetJson, lErro)
	Else
		lErro := .T.
	Endif
Next nX

oProcess:IncRegua2("Fim")
oProcess:IncRegua1("Fim")

If nArqs == 0
	MsgInfo("Nenhum arquivo processado")
Else
	MsgInfo("Total de arquivos processados: " + cValToChar(nArqs))
Endif

Return


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Materiais
// Função   : WISMovto
// Descrição: Consulta movimentos de estoque.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 26/08/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function WISMovto()
Local oProcess
If SuperGetMV("BZ_WMS",, "") == "WIS"
	If MsgYesNo("Essa rotina irá ler todos os ajustes de inventário cíclico do WIS e efetuar os ajustes no Protheus. Deseja continuar?", "Atenção")
		oProcess := MsNewProcess():New({|lEnd| WISMovto(oProcess)}, "Inventário cíclico")
		oProcess:Activate()
	Endif
Endif

Return

Static Function WISMovto(oProcess)
Local aRet       := {}
Local lErro      := .F.
Local lGrava     := .T.
Local nX

Local oJson
//Local cStsCode   := ""
Local aRetJson   := {}
Local cJson      := ""
Local nArqs      := 0
Local nMovtos    := 0

Local nHdl       := 0
Local nTamArq    := 0

Local aMovimento := {}
Local aReg       := {}
Local nReg       := 0
Local nQtdeRegs  := 0
Local cPorcent   := ""
Local cMsgProc   := ""

Local nSD3Movto  := 0
Local nSD3Item   := 0
Local aSD3Movto  := {}
Local nTamSB1    := TamSX3("B8_PRODUTO")[1]
Local dEmissao   := ctod("")
Local dDlMes     := GetMV("MV_ULMES")
Local cProduto   := ""
Local nQtde      := 0
Local cLocalOri  := ""
Local cLocalDes  := ""
Local cTpMovto   := ""

Private lMsErroAuto := .F.

// Acerta a barra de progressão superior.
oProcess:SetRegua1(1)
oProcess:IncRegua1("Consultando WIS...")

oJson := Array(#)
oJson[#"usuario"] := "%cUser%"
oJson[#"senha"]   := "%cPass%"

// Envia o Json para o webservice.
aRet := U_WISAPost("/saida/movimento", oJson, "SD3", nil, {|nCount, cRetPost, cStsCode, cHeadRet| !(cRetPost = "NENHUMA ")})

// Acerta a barra de progressão superior.
nArqs := len(aRet)
oProcess:SetRegua1(nArqs + 1)

// Se deu certo, trata o Json retornado.
For nX := 1 to nArqs
	lErro    := .F.
	aRetJson := aRet[nX]
	oProcess:IncRegua1("Arquivo " + cValToChar(nX) + " de " + cValToChar(nArqs) + "...")

	oProcess:SetRegua2(4)
	oProcess:IncRegua2("Lendo arquivo...")
	nHdl := fOpen(aRetJson[3], FO_EXCLUSIVE)
	If nHdl > 0
		oProcess:IncRegua2("Lendo arquivo...")
		fSeek(nHdl, 0, 0)
		nTamArq := fSeek(nHdl, 0, 2)
		fSeek(nHdl, 0, 0)
		cJson := Space(nTamArq)
		fRead(nHdl, @cJson, nTamArq)
		fClose(nHdl)

		Begin Transaction

		// Processa os registros do arquivo.
		If (cJson = "NENHUMA ")
			lErro  := .T.
			lGrava := .F.
		Else
			lGrava := .T.
			oProcess:IncRegua2("Lendo arquivo...")
			aMovimento := FromJson(cJson)
			If ValType(aMovimento) == "A"
				aSD3Movto := {}
				nQtdeRegs := len(aMovimento)
				oProcess:SetRegua2(nQtdeRegs)
				For nReg := 1 to nQtdeRegs
					aReg      := aMovimento[nReg]
					cPorcent  := Transform((nReg / nQtdeRegs) * 100, "@E 999.9") + "%"

					dEmissao  := ctod(left(aReg[#'dt_movimento'], 10))
					cProduto  := PadR(aReg[#'cd_produto'], nTamSB1)
					nQtde     := abs(aReg[#'qt_movimentacao'])
					cLocalOri := aReg[#'ds_area_erp_origem']
					cLocalDes := aReg[#'ds_area_erp_destino']
					cTpMovto  := aReg[#'tp_movimento']

					cMsgProc := "[" + cProduto + "] - qtde [" + cValToChar(nQtde) + "] " + cPorcent
					oProcess:IncRegua2(cMsgProc); ProcessMessages()

					If cTpMovto == "I" .and. dEmissao > dDlMes // Inventário cíclico executado após o último fechamento.
						SB1->(dbSetOrder(1))  // B1_FILIAL, B1_COD.
						SB1->(msSeek(xFilial() + cProduto, .F.))

						// Transfere o produto de armazém.
						nSD3Movto := aScan(aSD3Movto, {|x| x[1] = dEmissao})
						If nSD3Movto == 0
							aAdd(aSD3Movto, {dEmissao, {}})
							nSD3Movto := len(aSD3Movto)
						Endif

						nSD3Item := aScan(aSD3Movto[nSD3Movto, 2], {|x| x[1] == cProduto .and. x[3] == cLocalDes .and. x[4] == cLocalOri})
						If nSD3Item == 0
							aAdd(aSD3Movto[nSD3Movto, 2], {cProduto, nQtde, cLocalOri, cLocalDes})
						Else
							// Se houver algum movimento inverso, subtrai a quantidade.
							aSD3Movto[nSD3Movto, 2, nSD3Item, 2] -= nQtde
							If aSD3Movto[nSD3Movto, 2, nSD3Item, 2] == 0
								// Se zerou a movimentação, exclui o item.
								aDel(aSD3Movto[nSD3Movto, 2], nSD3Item)
								aSize(aSD3Movto[nSD3Movto, 2], len(aSD3Movto[nSD3Movto, 2]) - 1)
							ElseIf aSD3Movto[nSD3Movto, 2, nSD3Item, 2] < 0
								// Se a movimentação ficou negativa, inverte a origem e destino.
								aSD3Movto[nSD3Movto, 2, nSD3Item, 2] *= -1
								aSD3Movto[nSD3Movto, 2, nSD3Item, 3] := cLocalOri
								aSD3Movto[nSD3Movto, 2, nSD3Item, 4] := cLocalDes
							Endif
						Endif
					Endif
				Next nReg

				// Transfere os produtos para armazém de perdas.
				For nSD3Movto := 1 to len(aSD3Movto)
					If !empty(aSD3Movto[nSD3Movto, 2])
						oProcess:IncRegua2("Efetuando a transferência...")
						If U_TransfArm(aSD3Movto[nSD3Movto, 2], aSD3Movto[nSD3Movto, 1], "Inventário cíclico no WIS.", .T.)
							nMovtos += len(aSD3Movto[nSD3Movto, 2])
						Else
							lErro := .T.
						Endif
					Endif
				Next nSD3Movto
			Else
				lErro := .T.
			Endif
		Endif

		// Move arquivos para pasta de processados.
		U_WISProc(aRetJson, lErro, lGrava)

		End Transaction
	Else
		lErro := .T.
	Endif
Next nX

oProcess:IncRegua2("Fim")
oProcess:IncRegua1("Fim")

If nArqs == 0
	MsgInfo("Nenhum arquivo processado")
Else
	MsgInfo("Total de arquivos processados: " + cValToChar(nArqs) + CRLF + "Total de produtos movimentados: " + cValToChar(nMovtos))
Endif

Return
//-------------------------------------------------------------------
/*/{Protheus.doc} WISDPFIL
Retorna o relacionamentos entre as Unidades e os depositos do WIS

@author  Guilherme Santos
@since   16/03/2020
@version 12.1.25
/*/
//-------------------------------------------------------------------
User Function WISDPFIL()
	Local aRetorno 	:= {}
	Local aFilWIS	:= {}
	Local nX		:= 0

	U_Filiais(NIL, aFilWIS)

	For nX := 1 to Len(aFilWIS)
		If !Empty(SuperGetMV("WIS_DEP", NIL, "", aFilWIS[nX]))
			Aadd(aRetorno, {aFilWIS[nX], SuperGetMV("WIS_DEP", NIL, "", aFilWIS[nX])})
		EndIf
	Next nX

Return aRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} WISRDTXT
Le um arquivo texto e retorna o conteudo

@author  Guilherme Santos
@since   02/03/2021
@version 12.1.25
/*/
//-------------------------------------------------------------------
User Function WISRDTXT(cArquivo)
	Local cRetorno	:= ""
	Local oArqJson	:= uArqTxt():New("", "", cArquivo)

	If oArqJson:Exists() .AND. oArqJson:Use() .AND. oArqJson:Open(0)		//Abre o arquivo Somente Leitura + Exclusivo
		While !oArqJson:Eof()
			cRetorno += oArqJson:ReadLn()

			oArqJson:Skip()
		End

		oArqJson:Close()
		oArqJson:Free()
	EndIf

Return cRetorno
