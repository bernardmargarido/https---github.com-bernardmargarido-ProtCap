#include "protheus.ch"
#include "protcap.ch"
#include "msgraphi.ch"
#include "fileio.ch"
#include "fwbrowse.ch"

// Definições do status de estoque do pedido de venda.
#define AGUARDANDO    "1"
#define DISPONIVEL    "2"
#define RESERVADO     "3"
#define RES_PARCIAL   "4"
#define NAO_MOVIMENTA "9"

//Definicoes Array Previsao Entrega
#DEFINE XDTPREV		1
#DEFINE XSALDO		2
#DEFINE XFILCDS		3
#DEFINE XLEADTM		4
#DEFINE	XATRCOM		5

Static cStartPath := GetSrvProfString("StartPath", "")
Static cEmailNeg  := SuperGetMV("PC_MAILNEG",, "")
Static nBZPedMin  := SuperGetMV("BZ_PEDMIN",, 0)

Static nCliAtiv   := SuperGetMv("PC_CLIATIV",,90)
Static nCliDorm   := SuperGetMv("PC_CLIDORM",,365)
Static nTamPrd    := TamSX3("B1_COD")[1]
Static cMascGrd   := GetMv("MV_MASCGRD")
Static nTamRef    := val(Substr(cMascGrd, 1, 2))
Static nTamLin    := val(Substr(cMascGrd, 4, 2))
Static nTamCol    := val(Substr(cMascGrd, 7, 2))
Static aFiliais   := {}

Static nAliqPCC   := SuperGetMV("MV_TXPIS") + SuperGetMV("MV_TXCOFIN")
Static nArredPrc  := 2

Static nTamSC0    := TamSX3("C0_NUM")[1]
Static nTamDoc    := TamSX3("B7_DOC")[1]
Static nTamProd   := TamSX3("B7_COD")[1]
Static nTamLoc    := TamSX3("B7_LOCAL")[1]

Static cPictCNPJ  := "@R 99.999.999/9999-99"
Static cPictCPF   := "@R 999.999.999-99"
Static cPictQtd   := "@E 999,999,999.9"
Static cPictVlr   := "@E 999,999,999.99"
Static cPictPer   := "@E 999.9"

Static lCrescente := .T.
Static nUltCol	  := 0

// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Materiais
// Função   : ProdCpo
// Descrição: Retorna o campo de uma estrutura de produto.
// Retorno  : Valor do campo.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 27/09/13 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function ProdCpo(cProduto, cCampoPrd)
Local xRet		:= CriaVar(cCampoPrd, .F.)
Local aArea 	:= GetArea()
Local aAreaSB1	:= SB1->(GetArea())
Local aAreaSB4	:= SB4->(GetArea())
Local aAreaSB5	:= SB5->(GetArea())
Local nFieldPos  := 0
Local cCampoAux  := ""
Local lGrade     := .F.
Local aSB5SB4Cpo := U_SB5xSB4()
Local nSB5SB4Cpo := 0

Default cProduto   := SB1->B1_COD
cProduto := PadR(cProduto, nTamPrd)

// Busca o produto no buffer primeiro.
Static aProdCpo := {"", 0, 0, 0}
If aProdCpo[1] == cProduto
	SB1->(dbGoTo(aProdCpo[2]))
	SB4->(dbGoTo(aProdCpo[3]))
	SB5->(dbGoTo(aProdCpo[4]))
	lGrade := (aProdCpo[3] > 0)
Else
	SB1->(dbSetOrder(1))  // B1_FILIAL, B1_COD.
	SB1->(msSeek(xFilial() + cProduto, .T.))
	SB4->(dbSetOrder(1))  // B4_FILIAL, B4_COD.
	lGrade := SB4->(msSeek(xFilial() + left(cProduto, nTamRef), .F.))
	SB5->(dbSetOrder(1))  // B5_FILIAL, B5_COD.
	SB5->(msSeek(xFilial() + cProduto, .T.))
	aProdCpo := {cProduto, SB1->(RecNo()), If(lGrade, SB4->(RecNo()), 0), SB5->(RecNo())}
Endif

// Busca o campo na tabela apropriada.
If cCampoPrd = "B5_"
	// Se tiver de/para de campo no SB4, pega o campo do SB4.
	If lGrade
		nSB5SB4Cpo := aScan(aSB5SB4Cpo, {|x| x[3] = cCampoPrd})
	Endif
	If nSB5SB4Cpo > 0
		nFieldPos := aSB5SB4Cpo[nSB5SB4Cpo, 2]
		xRet      := SB4->(FieldGet(nFieldPos))
	Else
		nFieldPos := SB5->(FieldPos(cCampoPrd))
		xRet      := SB5->(FieldGet(nFieldPos))
	Endif
Else
	If lGrade
		cCampoAux := PadR("B4_" + SubStr(cCampoPrd, 4), 10)
		nFieldPos := SB4->(FieldPos(cCampoAux))
	Endif
	If nFieldPos > 0
		xRet      := SB4->(FieldGet(nFieldPos))
	Else
		nFieldPos := SB1->(FieldPos(cCampoPrd))
		xRet      := SB1->(FieldGet(nFieldPos))
	Endif
Endif

RestArea(aAreaSB5)
RestArea(aAreaSB4)
RestArea(aAreaSB1)
RestArea(aArea)

Return xRet


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Materiais
// Função   : ValidPrc
// Descrição: Valida a digitação de fator e preço de venda no cadastro do produto.
// Retorno  : Lógico.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 14/06/17 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function ValidPrc()
Local lRet       := .T.
Local cReadVar   := AllTrim(Upper(ReadVar()))

If IsInCallStack("MATA550")
	// Busca os dados para o cálculo do preço ou fator.
	DO CASE
		CASE cReadVar == "M->B4_CUSTD"

			M->B4_PRV1 := CalcPrc(M->B4_CUSTD, M->B4_XFATOR1)
			M->B4_PRV2 := CalcPrc(M->B4_CUSTD, M->B4_XFATOR2)
			M->B4_PRV3 := CalcPrc(M->B4_CUSTD, M->B4_XFATOR3)

		CASE SubStr(cReadVar,1,12) == "M->B4_XFATOR"

			cAux := SubStr(cReadVar,13,1)

			&("M->B4_PRV" + cAux) := CalcPrc(M->B4_CUSTD, &(cReadVar))

		CASE SubStr(cReadVar,1,9) == "M->B4_PRV"

			cAux := SubStr(cReadVar,10,1)

			&("M->B4_XFATOR" + cAux) := CalcFator(M->B4_CUSTD, &(cReadVar))
	ENDCASE
Else
	// Busca os dados para o cálculo do preço ou fator.
	DO CASE
		CASE cReadVar == "M->B1_CUSTD"

			FwFldPut("B1_CUSTD"	, M->B1_CUSTD)
			FwFldPut("B5_XCUSTD", FwFldGet("B1_CUSTD"))

			FwFldPut("B5_PRV1", CalcPrc(FwFldGet("B1_CUSTD"), FwFldGet("B5_XFATOR1")) )
			FwFldPut("B5_PRV2", CalcPrc(FwFldGet("B1_CUSTD"), FwFldGet("B5_XFATOR2")) )
			FwFldPut("B5_PRV3", CalcPrc(FwFldGet("B1_CUSTD"), FwFldGet("B5_XFATOR3")) )

		CASE cReadVar == "M->B5_XCUSTD"

			FwFldPut("B5_XCUSTD", M->B5_XCUSTD)
			FwFldPut("B1_CUSTD"	, FwFldGet("B5_XCUSTD"))

			FwFldPut("B5_PRV1", CalcPrc(FwFldGet("B5_XCUSTD"), FwFldGet("B5_XFATOR1")) )
			FwFldPut("B5_PRV2", CalcPrc(FwFldGet("B5_XCUSTD"), FwFldGet("B5_XFATOR2")) )
			FwFldPut("B5_PRV3", CalcPrc(FwFldGet("B5_XCUSTD"), FwFldGet("B5_XFATOR3")) )

		CASE SubStr(cReadVar,1,12) == "M->B5_XFATOR"

			cAux := SubStr(cReadVar,13,1)

			cCpoFat := "B5_XFATOR" + cAux
			cCpoPrv := "B5_PRV" + cAux

			FwFldPut(cCpoFat, &(cReadVar))
			FwFldPut(cCpoPrv, CalcPrc(FwFldGet("B5_XCUSTD"), FwFldGet(cCpoFat)) )

		CASE SubStr(cReadVar,1,9) == "M->B5_PRV"

			cAux := SubStr(cReadVar,10,1)

			cCpoFat := "B5_XFATOR" + cAux
			cCpoPrv := "B5_PRV" + cAux

			FwFldPut(cCpoPrv, &(cReadVar))
			FwFldPut(cCpoFat, CalcFator(FwFldGet("B5_XCUSTD"), FwFldGet(cCpoPrv)) )
	ENDCASE
EndIf

Return lRet

// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Wilson A. Silva Jr
// Modulo   : Materiais
// Função   : CalcPrc
// Descrição: Calcula o preco de venda atraves do fator e custo.
// Retorno  :
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 05/12/18 | Wilson A. Silva Jr| Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
Static Function CalcPrc(nCustStd, nFator)
Return IIF(nFator == 0, 0, nCustStd * (1 + (nFator / 100)))

// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Wilson A. Silva Jr
// Modulo   : Materiais
// Função   : CalcFator
// Descrição: Calcula o fator atraves do preco de venda e custo.
// Retorno  :
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 05/12/18 | Wilson A. Silva Jr| Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
Static Function CalcFator(nCustStd, nValor)
Return IIF(nValor == 0, 0, ((nValor / nCustStd) - 1) * 100)

// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Materiais
// Função   : ProdPrv
// Descrição: Retorna o preço de venda de um produto vs cliente.
// Retorno  : Matriz com P1, P2 e P3.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 20/05/14 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function ProdPrv(cProduto, cCliente, cLoja, lProspect)
Local aRet       := {0, 0, 0}

Default cProduto   := SB1->B1_COD

// Pega o produto que será usado como base para o cálculo dos preços.
If len(rtrim(cProduto)) > nTamRef
	cProduto := left(cProduto, nTamPrd)
Else
	cProduto := left(cProduto, nTamRef)
Endif

// Pega o preço de lista.
aRet[1] := U_ProdCpo(cProduto, "B5_PRV1")
aRet[2] := U_ProdCpo(cProduto, "B5_PRV2")
aRet[3] := U_ProdCpo(cProduto, "B5_PRV3")

// Verifica se há desconto do produto.
DA1->(dbSetOrder(2))  // DA1_FILIAL, DA1_CODPRO, DA1_CODTAB, DA1_ITEM.
If DA1->(dbSeek(xFilial() + left(cProduto, nTamRef), .F.))
	If DA1->DA1_PRCMAX > 0
		aRet[1] := DA1->DA1_PRCMAX
	Else
		aRet[1] *= (1 + (DA1->DA1_PERDES / 100))
	Endif
	aRet[2] *= (1 + (DA1->DA1_PERDES / 100))
	aRet[3] *= (1 + (DA1->DA1_PERDES / 100))
Endif

// Se o cliente foi informado, verifica se ele possui acréscimo de preço.
If !empty(cCliente) .and. !empty(cLoja)
	Default lProspect := .F.
	If lProspect
		SUS->(dbSetOrder(1))  // US_FILIAL, US_COD, US_LOJA.
		If SUS->(msSeek(xFilial() + cCliente + cLoja, .F.) .and. US_XACRESC <> 0)
			aRet[1] *= 1 + (SUS->US_XACRESC / 100)
			aRet[2] *= 1 + (SUS->US_XACRESC / 100)
			aRet[3] *= 1 + (SUS->US_XACRESC / 100)
		Endif
	Else
		SA1->(dbSetOrder(1))  // A1_FILIAL, A1_COD, A1_LOJA.
		If SA1->(msSeek(xFilial() + cCliente + cLoja, .F.) .and. A1_XACRESC <> 0)
			aRet[1] *= 1 + (SA1->A1_XACRESC / 100)
			aRet[2] *= 1 + (SA1->A1_XACRESC / 100)
			aRet[3] *= 1 + (SA1->A1_XACRESC / 100)
		Endif
	Endif
Endif

// Arredonda os preços.
aRet[1] := round(aRet[1], nArredPrc)
aRet[2] := round(aRet[2], nArredPrc)
aRet[3] := round(aRet[3], nArredPrc)

Return aRet


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Fernando Amorim
// Modulo   : Faturamento
// Função   : PrcVen
// Descrição: Engatilha o preço de venda de um produto no pedido de venda.
// Retorno  : Preço de venda calculado.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 06/08/10 | Fernando Amorim   | Desenvolvimento da rotina.
// 06/04/15 | Felipe Raposo     | Alteração para considerar os contratos.
// ---------+-------------------+------------------------------------------------
User Function PrcVen()
Local nPrcVen    := 0
Local nCusto     := 0
Local cCliente   := ""
Local nPrcCtr    := 0
Local nAliqIcm   := 0
Local nRedICMS   := 0

If M->C5_TIPO <> 'D'
	SB1->(dbSetOrder(1))  // B1_FILIAL, B1_COD.
	If ReadVar() == "M->C6_PRODUTO"
		SB1->(msSeek(xFilial() + M->C6_PRODUTO, .F.))
	Else
		SB1->(msSeek(xFilial() + GdFieldGet("C6_PRODUTO"), .F.))
	Endif

	// Pega a alíquota de ICMS.
	If MaFisFound("IT", n) .and. MaFisRet(n, "IT_PRODUTO") == SB1->B1_COD
		If MaFisRet(n, "IT_BASEICM") > 0
			nAliqIcm := MaFisRet(n, "IT_ALIQICM")
			nRedICMS := MaFisRet(n, "IT_PREDIC")
		Endif
	Else
		MaFisSave()
		MaFisEnd()
		SF4->(dbSetOrder(1))  // F4_FILIAL, F4_CODIGO.
		SF4->(msSeek(xFilial() + GdFieldGet("C6_TES"), .F.))
		cCliente := If(empty(M->C5_CLIENT), M->C5_CLIENTE, M->C5_CLIENT)
		MaFisIni(cCliente, M->C5_LOJAENT, If(M->C5_TIPO $ "DB", "F", "C"), M->C5_TIPO, M->C5_TIPOCLI,,,,, "MATA461")
		MaFisAdd(SB1->B1_COD, SF4->F4_CODIGO, 1, 1, 0, "", "", 0, 0, 0, 0, 0, 1, 0, SB1->(RecNo()), SF4->(RecNo()))
		If MaFisRet(1, "IT_BASEICM") > 0
			nAliqIcm := MaFisRet(1, "IT_ALIQICM")
			nRedICMS := MaFisRet(1, "IT_PREDIC")
		Endif
		MaFisEnd()
		MaFisRestore()
	Endif

	// Pedidos de transferência.
	If (M->C5_XTIPO == "TST" .or. M->C5_XTIPO == "TRN" .or. M->C5_XTIPO == "DTR")
		// Calcula o preço baseado no custo do último fechamento.
		SB9->(dbSetOrder(1))  // B9_FILIAL, B9_COD, B9_LOCAL, B9_DATA.
		If SB9->(dbSeek(xFilial() + SB1->B1_COD + ERP_LOC_VENDAS + dtos(SuperGetMV("MV_ULMES")), .F.) .and. B9_CM1 > 0 .and. B9_XCUSCON <> '2')  // B9_XCUSCON - Custo conciliado -> 1=Sim / 2=Não.
			nCusto := SB9->(If(B9_QINI <> 0 .and. B9_VINI1 > 0, B9_VINI1 / B9_QINI, B9_CM1))
		Else
			// Calcula o preço sem PIS/COFINS, com ICMS.
			nCusto := round(SB1->B1_CUSTD * (1 - ((SB1->B1_XICMF + nAliqPCC) / 100)), 8)
		Endif
		nPrcVen := round(nCusto / (1 - (nAliqIcm / 100)), 8)
	Else
		// Pega preço de contrato.
		nPrcCtr := U_MA020Ctr(M->C5_CLIENTE, M->C5_LOJACLI, SB1->B1_COD, nAliqIcm,,,, nRedICMS)
		If nPrcCtr > 0
			nPrcVen := nPrcCtr
		Else
			nPrcVen := U_ProdPrv(SB1->B1_COD, M->C5_CLIENTE, M->C5_LOJACLI)[1]
		Endif
	Endif

	// Atualiza preço de venda na tela.
	nPrcVen := round(nPrcVen, nArredPrc)
	U_AtuCamp("C6_PRCVEN", nPrcVen)
Endif

Return nPrcVen


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Fernando Amorim
// Modulo   : Faturamento
// Função   : PrcCom
// Descrição: Engatilha o preço de compra de um produto no pedido de compra.
// Retorno  : Preço de compra calculado.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 06/08/10 | Fernando Amorim   | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function PrcCom()
Local nCusto     := 0
Local nPrcCom    := 0
Local nAliqICM   := 0
Local nAliqIPI   := 0
Local cFilForn   := right(ca120Loj, 2)
Local lZFManaus  := FWSM0Util():GetSM0Data(cEmpAnt, cFilAnt, {"M0_ESTENT"})[1][2]  == "AM"

SB1->(dbSetOrder(1))  // B1_FILIAL, B1_COD.
SB1->(msSeek(xFilial() + GdFieldGet("C7_PRODUTO"), .F.))

// Verifica se é pedido de transferência.
If cA120Forn = "999999"
	// Retira o ICMS do preço unitário.
	If MaFisFound("IT", n)
		nAliqICM := MaFisRet(n, "IT_ALIQICM")
	ElseIf GdFieldGet("C7_PICM") > 0
		nAliqICM := GdFieldGet("C7_PICM")
	Else
		nAliqICM := SB1->B1_XICMF
	Endif

	// Calcula o preço sem PIS/COFINS, com ICMS.
	SB9->(dbSetOrder(1))  // B9_FILIAL, B9_COD, B9_LOCAL, B9_DATA.
	If SB9->(dbSeek(xFilial(, cFilForn) + SB1->B1_COD + ERP_LOC_VENDAS + dtos(SuperGetMV("MV_ULMES")), .F.) .and. B9_CM1 > 0 .and. B9_XCUSCON <> '2')  // B9_XCUSCON - Custo conciliado -> 1=Sim / 2=Não.
		nCusto := SB9->(If(B9_QINI <> 0 .and. B9_VINI1 > 0, B9_VINI1 / B9_QINI, B9_CM1))
	Else
		nCusto := round(SB1->B1_CUSTD * (1 - ((SB1->B1_XICMF + nAliqPCC) / 100)), 8)
	Endif

	If lZFManaus .AND. !(SB1->B1_ORIGEM $ '1*2')// Quando Manaus não aplica o ICMS no cálculo do preço somente para produtos que não são importados
		nPrcCom := round(nCusto, 8)
	Else
		nPrcCom := round(nCusto / (1 - (nAliqICM / 100)), 8)
	EndIf
Endif

// Pega preço de compra padrão, sem o IPI.
If nPrcCom = 0
	// Pega os impostos do pedido.
	If MaFisFound("IT", n)
		nAliqIPI := MaFisRet(n, "IT_ALIQIPI")
		nAliqICM := MaFisRet(n, "IT_ALIQICM")
		nRedICMS := MaFisRet(n, "IT_PREDIC")
	ElseIf GdFieldGet("C7_IPI") > 0
		nAliqIPI := GdFieldGet("C7_IPI")
		nAliqICM := GdFieldGet("C7_PICM")
		nRedICMS := 0
	Else
		nAliqIPI := SB1->B1_IPI
		nAliqICM := SB1->B1_XICMF
		nRedICMS := 0
	Endif

	// Retira o IPI do preço unitário.
	nPrcCom := round(SB1->B1_CUSTD / (1 + (nAliqIPI / 100)), 8)

	// Calcula diferencial de ICMS.
	nPrcCom := U_DifICMS(nPrcCom, SB1->B1_XICMF, 0, nAliqICM, nRedICMS)
Endif

// Atualiza preço de compra na tela.
nPrcCom := round(nPrcCom, nArredPrc)
U_AtuCamp("C7_PRECO", nPrcCom)

Return nPrcCom


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento
// Função   : DifICMS
// Descrição: Calcula o preço considerando o diferencial de ICMS.
// Retorno  : Preço calculado.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 03/07/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function DifICMS(nPreco, nICMSOr, nRedOr, nAlqICMS, nRedICMS, nAlqPCCOr, nAlqPCCRet, lRenta)
Local nPrcRet		:= 0
Local nMarkup		:= 0
Local lRNTPCC		:= SuperGetMV("BZ_RNTPCCB", NIL, .T.)		//Retira o PCC da base do ICMS - Renta por Marca

Default nRedOr		:= 0
Default nAlqICMS	:= 0
Default nRedICMS	:= 0
Default nAlqPCCOr	:= nAliqPCC
Default nAlqPCCRet	:= nAlqPCCOr
Default lRenta		:= .F.

If lRenta .AND. lRNTPCC
	//No Relatorio Renta por Marca, retira primeiro o PCC da base do ICMS
	nMarkup := nAlqPCCOr
	nPrcRet	:= Round(nPreco * (1 - (nMarkup / 100)), 8)

	// PCC Retido
	nMarkup := nAlqPCCRet
	nPrcRet := round(nPrcRet / (1 - (nMarkup / 100)), 8)

	// Se não houver redução de base de ICMS, considera 100% da base.
	nRedOr   := If(nRedOr == 0,   1, nRedOr / 100)
	nRedICMS := If(nRedICMS == 0, 1, nRedICMS / 100)

	// Preço líquido.
	nMarkup := nICMSOr * nRedOr
	nPrcRet := round(nPrcRet  * (1 - (nMarkup / 100)), 8)

	// Preço com o ICMS corrigido.
	nMarkup := nAlqICMS * nRedICMS
	nPrcRet := round(nPrcRet / (1 - (nMarkup / 100)), 8)
Else
	// Se não houver redução de base de ICMS, considera 100% da base.
	nRedOr   := If(nRedOr == 0,   1, nRedOr / 100)
	nRedICMS := If(nRedICMS == 0, 1, nRedICMS / 100)

	// Preço líquido.
	nMarkup := ((nICMSOr * nRedOr) + nAlqPCCOr)
	nPrcRet := round(nPreco  * (1 - (nMarkup / 100)), 8)

	// Preço com o ICMS corrigido.
	nMarkup := ((nAlqICMS * nRedICMS) + nAlqPCCRet)
	nPrcRet := round(nPrcRet / (1 - (nMarkup / 100)), 8)
EndIf

Return nPrcRet


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Materiais
// Função   : PVForCli
// Descrição: Retorna o conteúdo de um campo do cliente de um pedido de venda.
// Retorno  : Conteúdo do campo.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 16/10/14 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function PVForCli(cCampo, cPedido)
Local xRet

Default cPedido    := SC5->C5_NUM

// Posiciona o pedido.
If cPedido <> SC5->C5_NUM
	SC5->(dbSetOrder(1))  // C5_FILIAL, C5_NUM.
	SC5->(dbSeek(xFilial() + cPedido, .F.))
Endif

// Posiciona cliente/prospect.
If SC5->C5_TIPO $ "D|B"
	SA2->(dbSetOrder(1))  // A2_FILIAL, A2_COD, A2_LOJA.
	If SA2->(msSeek(xFilial() + SC5->(C5_CLIENTE + C5_LOJACLI), .F.))
		cCampo := "A2_" + SubStr(cCampo, 4)
		xRet := SA2->(&cCampo)
	Endif
Else
	SA1->(dbSetOrder(1))  // A1_FILIAL, A1_COD, A1_LOJA.
	If SA1->(msSeek(xFilial() + SC5->(C5_CLIENTE + C5_LOJACLI), .F.))
		cCampo := "A1_" + SubStr(cCampo, 4)
		xRet := SA1->(&cCampo)
	Endif
Endif

Return xRet


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : PVFis
// Descrição: Carrega os impostos de um pedido de vendas, usando as funções de
//            impostos MATXFIS.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 13/11/13 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function PVFis(cPedido, nTipo)
// Rotina baseada na rotina padrão Ma410Impos.
Local nX
Local nY

Local lSaldo     := .F.

Local aFisGetSC5 := {}
Local aFisGetSC6 := {}
Local nPosCpo    := 0

Local nPItem     := 0
Local nPProduto  := 0
Local nPQtdVen   := 0
Local nPPrUnit   := 0
Local nPPrcVen   := 0
Local nPTotal    := 0
Local nPValDesc  := 0
Local nPLocal    := 0
Local nPDtEntr   := 0
Local nPTES      := 0
Local nPCFOP     := 0
Local nPNfOri    := 0
Local nPSerOri   := 0
Local nPItemOri  := 0
Local nPIdentB6  := 0
Local nPLote     := 0
Local nPSubLote  := 0
Local nPClasFis  := 0
Local nPSuframa  := 0

Local aDupl      := {}
Local aVencto    := {}
Local aEntr      := {}
Local aDuplTmp   := {}
Local nAcerto    := 0
Local nPosEntr   := 0

Local cProduto   := ""
Local nTotDesc   := 0
Local nQuant     := 0
Local nQtdEnt    := 0
Local nPrcLista  := 0
Local nValMerc   := 0
Local nDesconto  := 0
Local nAcresUnit := 0
Local nAcresFin  := 0
Local nQtdPeso   := 0

Local nRecOri    := 0
Local nItem      := 0
Local nPropLot   := 0
Local lDtEmi     := SuperGetMv("MV_DPDTEMI", .F., .T.)
Local cTpDpInd   := GetNewPar("MV_TPDPIND", "1")
Local dDataCnd   := ctod("")

Local lIVAAju    := .F.
Local lUsaVenc   := .F.
Local lRastroLot := .F.
Local lRastro	 := ExistBlock("MAFISRASTRO")

Local nAcresTot  := 0
Local cCliPed    := ""
Local lCfo       := .F.
Local aTransp    := {"", ""}
Local aSaldos    := {}
Local aInfLote   := {}
Local aNfOri     := {}
Local cEstado    := SuperGetMv("MV_ESTADO")
Local cTesVend   := SuperGetMv("MV_TESVEND",, "")
Local nlValor    := 0
Local aLinha     := {}

Local aAreaSA1 := SA1->(GetArea())
Local aAreaSA2 := SA2->(GetArea())
Local aAreaSA4 := SA4->(GetArea())
Local aAreaSB2 := SB2->(GetArea())
Local aAreaSB8 := SB8->(GetArea())
Local aAreaSC5 := SC5->(GetArea())
Local aAreaSC6 := SC6->(GetArea())
Local aAreaSC9 := SC9->(GetArea())
Local aAreaSE4 := SE4->(GetArea())
Local aAreaSF3 := SF3->(GetArea())
Local aAreaSF4 := SF4->(GetArea())

Local aFldSC5	:= FWSX3Util():GetListFieldsStruct("SC5", .T.)
Local aFldSC6	:= FWSX3Util():GetListFieldsStruct("SC6", .T.)
Local nSC5		:= 0
Local nSC6		:= 0
Local cLoteVct	:= SuperGetMv('MV_LOTVENC')

Default cPedido := SC5->C5_NUM
Default nTipo   := 1

SC5->(dbSetOrder(1))  // C5_FILIAL, C5_NUM.
If SC5->(dbSeek(xFilial() + cPedido, .F.))
	// Inicializa as variáveis de memória.
	RegToMemory("SC5", .F., .F.)

	// Montagem do aHeader/aCols.
	aHeader := {}; aCols := {}; n := 0
	If nTipo == 1  // Por pedido (SC6).
		FillGetDados(4, "SC6", 1, xFilial("SC6") + SC5->C5_NUM, {|| C6_FILIAL + C6_NUM},,,,,,,, aHeader, aCols)
	ElseIf nTipo == 2  // Por liberação de pedido (SC9).
		FillGetDados(3, "SC6", 1, xFilial("SC6") + SC5->C5_NUM, {|| C6_FILIAL + C6_NUM},,,,,,,, aHeader, aCols)
		aLinha := aCols[1]

		aCols := {}; n := 0
		SC9->(dbSetOrder(1))  // C9_FILIAL, C9_PEDIDO, C9_ITEM, C9_SEQUEN, C9_PRODUTO.
		SC9->(dbSeek(xFilial() + SC5->C5_NUM, .F.))
		Do While SC9->(!eof() .and. C9_FILIAL + C9_PEDIDO == xFilial() + SC5->C5_NUM)
			aAdd(aCols, aClone(aLinha)); n++
			For nX := 1 to len(aHeader)
				aTail(aCols)[nX] := SC6->(FieldGet(FieldPos(aHeader[nX, 2])))
			Next nX
			GdFieldPut("C6_QTDVEN",  SC9->C9_QTDLIB)
			GdFieldPut("C6_PRCVEN",  SC9->C9_PRCVEN)
			GdFieldPut("C6_VALOR",   Round(SC9->(C9_QTDLIB * C9_PRCVEN), TamSX3("C6_VALOR")[2]))
			If !empty(SC9->C9_LOTECTL)
				SB8->(dbSetOrder(3))  // B8_FILIAL, B8_PRODUTO, B8_LOCAL, B8_LOTECTL, B8_NUMLOTE, B8_DTVALID.
				SB8->(msSeek(xFilial() + SC9->(C9_PRODUTO + C9_LOCAL + C9_LOTECTL + C9_NUMLOTE), .F.))

				GdFieldPut("C6_LOTECTL", SC9->C9_LOTECTL)
				GdFieldPut("C6_NUMLOTE", SC9->C9_NUMLOTE)
				GdFieldPut("C6_CLASFIS", SB8->B8_XCST)
			Endif

			// Próximo item.
			SC9->(dbSkip())
		EndDo
	Endif

	// Busca a posição das variáves locais.
	nPItem    := GdFieldPos("C6_ITEM")
	nPProduto := GdFieldPos("C6_PRODUTO")
	nPQtdVen  := GdFieldPos("C6_QTDVEN")
	nPPrUnit  := GdFieldPos("C6_PRUNIT")
	nPPrcVen  := GdFieldPos("C6_PRCVEN")
	nPTotal   := GdFieldPos("C6_VALOR")
	nPValDesc := GdFieldPos("C6_VALDESC")
	nPLocal   := GdFieldPos("C6_LOCAL")
	nPDtEntr  := GdFieldPos("C6_ENTREG")
	nPTES     := GdFieldPos("C6_TES")
	nPCFOP    := GdFieldPos("C6_CF")
	nPNfOri   := GdFieldPos("C6_NFORI")
	nPSerOri  := GdFieldPos("C6_SERIORI")
	nPItemOri := GdFieldPos("C6_ITEMORI")
	nPIdentB6 := GdFieldPos("C6_IDENTB6")
	nPLote    := GdFieldPos("C6_LOTECTL")
	nPSubLote := GdFieldPos("C6_NUMLOTE")
	nPClasFis := GdFieldPos("C6_CLASFIS")
	nUsado    := len(aHeader)
	dDataCnd  := M->C5_EMISSAO

	// Busca referências no SC5.
	aFisGetSC5 := {}

	For nSC5 := 1 to Len(aFldSC5)
		cValid := GetSX3Cache(aFldSC5[nSC5][1], "X3_VALID") + GetSX3Cache(aFldSC5[nSC5][1], "X3_VLDUSER")
		If 'MAFISGET("' $ cValid
			nPosIni 	:= AT('MAFISGET("', cValid) + 10
			nLen		:= AT('")', Substr(cValid, nPosIni, Len(cValid) - nPosIni)) - 1
			cReferencia := Substr(cValid, nPosIni, nLen)
			aAdd(aFisGetSC5, {cReferencia, aFldSC5[nSC5][1], MaFisOrdem(cReferencia)})
		Endif
		If 'MAFISREF("' $ cValid
			nPosIni		:= AT('MAFISREF("', cValid) + 10
			nLen		:= AT('","MT410",', Substr(cValid, nPosIni, Len(cValid) - nPosIni)) - 1
			cReferencia	:= Substr(cValid, nPosIni, nLen)
			aAdd(aFisGetSC5, {cReferencia, aFldSC5[nSC5][1], MaFisOrdem(cReferencia)})
		Endif
	Next nSC5

	aSort(aFisGetSC5,,, {|x, y| x[3] < y[3]})

	// Busca referências no SC6.
	aFisGetSC6 := {}

	For nSC6 := 1 to Len(aFldSC6)
		cValid := GetSX3Cache(aFldSC6[nSC6][1], "X3_VALID") + GetSX3Cache(aFldSC6[nSC6][1], "X3_VLDUSER")
		If 'MAFISGET("' $ cValid
			nPosIni 	:= AT('MAFISGET("', cValid) + 10
			nLen		:= AT('")', Substr(cValid, nPosIni, Len(cValid) - nPosIni)) - 1
			cReferencia := Substr(cValid, nPosIni, nLen)
			aAdd(aFisGetSC6, {cReferencia, aFldSC6[nSC6][1], MaFisOrdem(cReferencia)})
		Endif
		If 'MAFISREF("' $ cValid
			nPosIni		:= AT('MAFISREF("', cValid) + 10
			nLen		:= AT('","MT410",', Substr(cValid, nPosIni, Len(cValid) - nPosIni)) - 1
			cReferencia	:= Substr(cValid, nPosIni, nLen)
			aAdd(aFisGetSC6, {cReferencia, aFldSC6[nSC6][1], MaFisOrdem(cReferencia)})
		Endif
	Next nSC6

	aSort(aFisGetSC6,,, {|x, y| x[3] < y[3]})

	SA4->(dbSetOrder(1))  // A4_FILIAL, A4_COD.
	If SA4->(msSeek(xFilial() + M->C5_TRANSP, .F.))
		aTransp[01] := SA4->A4_EST
		aTransp[02] := If(SA4->(FieldPos("A4_TPTRANS")) > 0, SA4->A4_TPTRANS, "")
	Endif

	// Inicializa a função fiscal.

	// A consultoria tributária, por meio da resposta à consulta nº 268/2004, determinou a aplicação
	// das seguintes alíquotas nas notas fiscais de venda emitidas pelo vendedor remetente:
	//  1) no caso previsto na letra "a" (venda para SP e entrega no PR)
	//     - aplicação da alíquota interna do Estado de São Paulo, visto que a operação entre o
	//       vendedor remetente e o adquirente originário é interna.
	//  2) no caso previsto na letra "b" (venda para o DF e entrega no PR)
	//     - aplicação da alíquota interestadual prevista para as operações com o Paraná, ou seja,
	//       12%, visto que a circulação da mercadoria se dá entre os Estado de São Paulo e do Paraná.
	//  3) no caso previsto na letra "c" (venda para o RS e entrega no SP)
	//     - aplicação da alíquota interna do Estado de São Paulo, uma vez que se considera interna a
	//       operação, quando não se comprovar a saída da mercadoria do território do Estado de São
	//       Paulo, conforme previsto no art. 36, § 4º do RICMS/SP;
	If cEstado == 'SP'
		If !empty(M->C5_CLIENT) .and. M->C5_CLIENT <> M->C5_CLIENTE
			If aScan(aCols, {|x| !x[nUsado + 1] .and. x[nPTES] $ Alltrim(cTesVend)}) > 0
				lCfo := .T.
			Endif

			If lCfo
				dbSelectArea(If(M->C5_TIPO $ "DB", "SA2", "SA1"))
				dbSetOrder(1)
				msSeek(xFilial() + M->(C5_CLIENTE + C5_LOJAENT), .F.)
				If If(M->C5_TIPO $ "DB", SA2->A2_EST, SA1->A1_EST) == 'SP'
					cCliPed := M->C5_CLIENTE
				Else
					cCliPed := M->C5_CLIENT
				Endif
			Endif
		Endif
	Endif
	If empty(cCliPed)
		cCliPed := If(empty(M->C5_CLIENT), M->C5_CLIENTE, M->C5_CLIENT)
	Endif

	MaFisEnd()
	MaFisIni(cCliPed, M->C5_LOJAENT, If(M->C5_TIPO $ "DB", "F", "C"), M->C5_TIPO, M->C5_TIPOCLI,,,,, "MATA461",,,,,,,, aTransp)

	// Realiza alterações de referências do SC5.
	dbSelectArea("SC5")
	For nX := 1 to len(aFisGetSC5)
		If !Empty(&("M->" + Alltrim(aFisGetSC5[nX, 2])))
			MaFisAlt(aFisGetSC5[nX, 1], &("M->" + Alltrim(aFisGetSC5[nX, 2])),, .F.)
		Endif
	Next nX

	// Agrega os itens para a função fiscal.
	If nPTotal > 0 .And. nPValDesc > 0 .And. nPPrUnit > 0 .And. nPProduto > 0 .And. nPQtdVen > 0 .And. nPTes > 0
		For nX := 1 to len(aCols)
			nQtdPeso := 0
			If !aCols[nX, nUsado + 1]
				nItem++

				// Posiciona registros.
				cProduto := aCols[nX, nPProduto]
				MatGrdPrRf(@cProduto)
				SB1->(dbSetOrder(1))  // B1_FILIAL, B1_COD.
				SB1->(msSeek(xFilial() + cProduto, .F.))
				SB2->(dbSetOrder(1))  // B2_FILIAL, B2_COD, B2_LOCAL.
				SB2->(msSeek(xFilial() + aCols[nX, nPProduto] + aCols[nX, nPLocal], .F.))
				SC6->(dbSetOrder(1))  // C6_FILIAL, C6_NUM, C6_ITEM, C6_PRODUTO.
				SC6->(dbSeek(xFilial() + M->C5_NUM + aCols[nX, nPItem] + aCols[nX, nPProduto], .F.))
				SF4->(dbSetOrder(1))  // F4_FILIAL, F4_CODIGO.
				SF4->(msSeek(xFilial() + aCols[nX, nPTES], .F.))

				// Dados do item.
				If lSaldo .and. nPItem > 0
					nQtdEnt := If(!left(SC6->C6_BLQ, 1) $ "RS" .and. empty(SC6->C6_BLOQUEI), SC6->C6_QTDENT, SC6->C6_QTDVEN)
					nQuant  -= nQtdEnt
				Else
					lSaldo  := .F.
				Endif
				nQuant    := aCols[nX, nPQtdVen]
				nValMerc  := If(aCols[nX, nPQtdVen] == 0, aCols[nX, nPTotal], If(lSaldo, nQuant * aCols[nX, nPPrcVen], aCols[nX, nPTotal]))
				nPrcLista := aCols[nX, nPPrUnit]
				nAcresUnit:= a410Arred(aCols[nX, nPPrcVen] * M->C5_ACRSFIN / 100, "D2_PRCVEN")
				nAcresFin := a410Arred(nQuant * nAcresUnit, "D2_TOTAL")
				nValMerc  += nAcresFin
				nDesconto := a410Arred(nQuant * nPrcLista, "D2_DESCON") - nValMerc
				nDesconto := If(nDesconto <= 0, aCols[nX, nPValDesc], nDesconto)
				nDesconto := max(0, nDesconto)
				nPrcLista += nAcresUnit
				nValMerc  += nDesconto
				nQtdPeso  := nQuant * SB1->B1_PESO

				// Totalizador geral.
				nAcresTot += nAcresFin

				// Verifica a data de entrega para as duplicatas.
				If (nPDtEntr > 0)
					If (dDataCnd > aCols[nX, nPDtEntr] .And. !Empty(aCols[nX, nPDtEntr]))
						dDataCnd := aCols[nX, nPDtEntr]
					Endif
				Else
					dDataCnd := M->C5_EMISSAO
				Endif

				// Tratamento de IVA ajustado.
				If (SB1->B1_IVAAJU == '1' .and. Rastro(cProduto, "S"))
					lIVAAju := IF(lRastro, (lRastroLot := ExecBlock("MAFISRASTRO", .F., .F.)), Rastro(cProduto, "S"))
				Endif
				If lIVAAju
					SC6->(dbSetOrder(1))  // C6_FILIAL, C6_NUM, C6_ITEM, C6_PRODUTO.
					SC6->(dbSeek(xFilial() + M->C5_NUM, .F.))
					lUsaVenc := (!Empty(SC6->C6_LOTECTL + SC6->C6_NUMLOTE) .or. (cLoteVct == 'S'))
					aSaldos  := SldPorLote(aCols[nX, nPProduto], aCols[nX, nPLocal], aCols[nX, nPQtdVen], 0, SC6->C6_LOTECTL, SC6->C6_NUMLOTE, SC6->C6_LOCALIZ, SC6->C6_NUMSERI,,,, lUsaVenc,,, dDataBase)

					For nY := 1 to Len(aSaldos)
						nPropLot := aSaldos[nY, 5]
						If lRastroLot
							SB8->(dbSetOrder(5))  // B8_FILIAL, B8_PRODUTO, B8_LOTECTL, B8_NUMLOTE, B8_DTVALID.
							If SB8->(msSeek(xFilial() + cProduto + aSaldos[nX, 01], .F.))
								aAdd(aInfLote, {SB8->B8_DOC, SB8->B8_SERIE, SB8->B8_CLIFOR, SB8->B8_LOJA, nPropLot})
							Endif
						Else
							SB8->(dbSetOrder(2))  // B8_FILIAL, B8_NUMLOTE, B8_LOTECTL, B8_PRODUTO, B8_LOCAL, B8_DTVALID.
							If SB8->(msSeek(xFilial() + aSaldos[nX, 02] + aSaldos[nX, 01], .F.))
								aAdd(aInfLote, {SB8->B8_DOC, SB8->B8_SERIE, SB8->B8_CLIFOR, SB8->B8_LOJA, nPropLot})
							Endif
						Endif

						If !Empty(aInfLote)
							SF3->(dbSetOrder(4))  // F3_FILIAL, F3_CLIEFOR, F3_LOJA, F3_NFISCAL, F3_SERIE.
							If SF3->(dbSeek(xFilial() + aInfLote[nY, 03] + aInfLote[nY, 04] + aInfLote[nY, 01] + aInfLote[nY, 02], .F.))
								aAdd(aNfOri, {SF3->F3_ESTADO, SF3->F3_ALIQICM, aInfLote[nY, 05]})
							Endif
						Endif
					Next nY
				Endif

				If nPIdentB6 <> 0 .and. !Empty(aCols[nX, nPIdentB6])
					SD1->(dbSetOrder(4))  // D1_FILIAL, D1_NUMSEQ.
					If SD1->(dbSeek(xFilial() + aCols[nX, nPIdentB6], .F.))
						nRecOri := SD1->(Recno())
					Endif
				ElseIf nPNfOri > 0 .and. nPSerOri > 0 .and. nPItemOri > 0
					If !Empty(aCols[nX, nPNfOri]) .And. !Empty(aCols[nX, nPItemOri])
						SD1->(dbSetOrder(1))  // D1_FILIAL, D1_DOC, D1_SERIE, D1_FORNECE, D1_LOJA, D1_COD, D1_ITEM.
						If SD1->(dbSeek(xFilial() + aCols[nX, nPNfOri] + aCols[nX, nPSerOri] + M->(C5_CLIENTE + C5_LOJACLI) + aCols[nX, nPProduto] + aCols[nX, nPItemOri], .F.))
							nRecOri := SD1->(Recno())
						Endif
					Endif
				Endif

				// Agrega os itens para a função fiscal.
				MaFisAdd(cProduto, aCols[nX, nPTES], nQuant, nPrcLista, nDesconto, "", "", nRecOri, 0, 0, 0, 0, nValMerc, 0, SB1->(RecNo()), SF4->(RecNo()),;
				If(nPItem > 0, aCols[nX, nPItem], ""), 0, 0, aCols[nX, nPCFOP],,,, aCols[nX, nPLote], aCols[nX, nPSubLote],,, aCols[nX, nPClasFis])

				// Tratamento do IVA ajustado.
				If lIVAAju
					For nY := 1 to len(aNfOri)
						MaFisAddIT("IT_ANFORI2", {aNfOri[nY, 1], aNfOri[nY, 2], aNfOri[nY, 3], 0}, nItem, nY == 1)
					Next nY
					aSaldos := {}
					aNfOri  := {}
				Endif

				// Cálculo do ISS.
				If (M->C5_INCISS == "N" .And. M->C5_TIPO == "N")
					If (SF4->F4_ISS == "S")
						nPrcLista := a410Arred(nPrcLista / (1 - (MaAliqISS(nItem) / 100)), "D2_PRCVEN")
						nValMerc  := a410Arred(nValMerc  / (1 - (MaAliqISS(nItem) / 100)), "D2_PRCVEN")
						MaFisAlt("IT_PRCUNI",  nPrcLista, nItem)
						MaFisAlt("IT_VALMERC", nValMerc,  nItem)
					Endifç
				Endif

				// Altera peso para calcular frete.
				MaFisAlt("IT_PESO",    nQtdPeso,  nItem)
				MaFisAlt("IT_PRCUNI",  nPrcLista, nItem)
				MaFisAlt("IT_VALMERC", nValMerc,  nItem)

				// Analise da rentabilidade.
				If SF4->F4_DUPLIC == "S"
					nTotDesc += MaFisRet(nItem, "IT_DESCONTO")
				Else
					If cTpDpInd == "1"
						nTotDesc += MaFisRet(nItem, "IT_DESCONTO")
					Endif
				Endif
			Endif
		Next nX
	Endif

	// Indica os valores do cabecalho.
	MaFisAlt("NF_FRETE",    M->C5_FRETE)
	MaFisAlt("NF_VLR_FRT",  M->C5_VLR_FRT)
	MaFisAlt("NF_SEGURO",   M->C5_SEGURO)
	MaFisAlt("NF_AUTONOMO", M->C5_FRETAUT)
	MaFisAlt("NF_DESPESA",  M->C5_DESPESA)

	// Indenização por valor.
	If M->C5_DESCONT > 0
		MaFisAlt("NF_DESCONTO", min(MaFisRet(nil, "NF_VALMERC") - 0.01, nTotDesc + M->C5_DESCONT),/*nItem*/,/*lNoCabec*/,/*nItemNao*/, cTpDpInd == "2")
	Endif

	If M->C5_PDESCAB > 0
		MaFisAlt("NF_DESCONTO", a410Arred(MaFisRet(nil, "NF_VALMERC") * M->C5_PDESCAB / 100, "C6_VALOR") + MaFisRet(nil, "NF_DESCONTO"))
	Endif

	// Realiza alterações de referências do SC6.
	dbSelectArea("SC6")
	If Len(aFisGetSC6) > 0
		For nX := 1 to Len(aCols)
			If !aCols[nX, nUsado + 1]
				For nY := 1 to len(aFisGetSC6)
					nPosCpo := GdFieldPos(aFisGetSC6[ny, 2])
					If nPosCpo > 0
						If !Empty(aCols[nX, nPosCpo])
							MaFisAlt(aFisGetSC6[ny, 1], aCols[nX, nPosCpo], nX, .F.)
						Endif
					Endif
				Next nX
			Endif
		Next nY
	Endif

	// Realiza alterações de referências do SC5 suframa.
	nPSuframa := aScan(aFisGetSC5, {|x| x[1] == "NF_SUFRAMA"})
	If !Empty(nPSuframa)
		dbSelectArea("SC5")
		If !Empty(&("M->" + Alltrim(aFisGetSC5[nPSuframa, 2])))
			MaFisAlt(aFisGetSC5[nPSuframa, 1], (&("M->"+Alltrim(aFisGetSC5[nPSuframa, 2])) == "1"), nItem, .F.)
		Endif
	Endif

	MaFisWrite(1)

	// Calcula os venctos conforme a condição de pagto.
	If !M->C5_TIPO == "B"
		If lDtEmi
			SE4->(dbSetOrder(1))  // E4_FILIAL, E4_CODIGO.
			SE4->(msSeek(xFilial() + M->C5_CONDPAG, .F.))
			If ((SE4->E4_TIPO == "9" .and. !(INCLUI .or. ALTERA)) .or. SE4->E4_TIPO <> "9")
				nAcerto := 0
				nlValor := MaFisRet(nil, "NF_BASEDUP")

				aDupl := Condicao(nlValor, M->C5_CONDPAG, MaFisRet(nil, "NF_VALIPI"), dDataCnd, MaFisRet(nil, "NF_VALSOL"),,, nAcresTot)
				If Len(aDupl) > 0
					For nX := 1 To Len(aDupl)
						nAcerto += aDupl[nX, 2]
					Next nX
					aDupl[Len(aDupl), 2] += MaFisRet(nil, "NF_BASEDUP") - nAcerto

					aVencto := aClone(aDupl)
					For nX := 1 To Len(aDupl)
						aDupl[nX, 2] := Transform(aDupl[nX, 2], PesqPict("SE1", "E1_VALOR"))
					Next nX
				Endif
			Else
				aDupl   := {{Ctod(""), Transform(MaFisRet(nil, "NF_BASEDUP"), PesqPict("SE1", "E1_VALOR"))}}
				aVencto := {{dDataBase, MaFisRet(nil, "NF_BASEDUP")}}
			Endif
		Else
			nItem := 0
			For nX := 1 to Len(aCols)
				If Len(aCols[nX])==nUsado .Or. !aCols[nX, nUsado+1]
					nItem++
					If nPDtEntr > 0
						nPosEntr := Ascan(aEntr,{|x| x[1] == aCols[nX, nPDtEntr]})
						If nPosEntr == 0
							aAdd(aEntr, {aCols[nX, nPDtEntr], MaFisRet(nItem, "IT_BASEDUP"), MaFisRet(nItem, "IT_VALIPI"), MaFisRet(nItem, "IT_VALSOL")})
						Else
							aEntr[nPosEntr, 2] += MaFisRet(nItem, "IT_BASEDUP")
							aEntr[nPosEntr, 3] += MaFisRet(nItem, "IT_VALIPI")
							aEntr[nPosEntr, 4] += MaFisRet(nItem, "IT_VALSOL")
						Endif
					Endif
				Endif
			Next

			SE4->(dbSetOrder(1))  // E4_FILIAL, E4_CODIGO.
			SE4->(msSeek(xFilial() + M->C5_CONDPAG, .F.))
			If (SE4->E4_TIPO <> "9")
				For nY := 1 to len(aEntr)
					nAcerto := 0
					nlValor := aEntr[nY, 2]

					aDuplTmp := Condicao(nlValor, M->C5_CONDPAG, aEntr[nY, 3], aEntr[nY, 1], aEntr[nY, 4],,, nAcresTot)
					If len(aDuplTmp) > 0
						For nX := 1 To Len(aDuplTmp)
							nAcerto += aDuplTmp[nX, 2]
						Next nX
						aDuplTmp[Len(aDuplTmp), 2] += aEntr[nY, 2] - nAcerto

						aVencto := aClone(aDuplTmp)
						For nX := 1 To Len(aDuplTmp)
							aDuplTmp[nX, 2] := Transform(aDuplTmp[nX, 2], PesqPict("SE1", "E1_VALOR"))
						Next nX
						aEval(aDuplTmp, {|x| aAdd(aDupl, {aEntr[nY, 1], x[1], x[2]})})
					Endif
				Next
			Else
				aDupl   := {{Ctod(""), Transform(MaFisRet(nil, "NF_BASEDUP"), PesqPict("SE1", "E1_VALOR"))}}
				aVencto := {{dDataBase, MaFisRet(nil, "NF_BASEDUP")}}
			Endif
		Endif
	Else
		aDupl   := {{Ctod(""), Transform(0, PesqPict("SE1", "E1_VALOR"))}}
		aVencto := {{dDataBase, 0}}
	Endif

	If len(aDupl) == 0
		aDupl   := {{Ctod(""), Transform(MaFisRet(nil, "NF_BASEDUP"), PesqPict("SE1", "E1_VALOR"))}}
		aVencto := {{dDataBase, MaFisRet(nil, "NF_BASEDUP")}}
	Endif
Endif

// Restaura posição das tabelas.
RestArea(aAreaSA1)
RestArea(aAreaSA2)
RestArea(aAreaSA4)
RestArea(aAreaSB2)
RestArea(aAreaSB8)
RestArea(aAreaSC5)
RestArea(aAreaSC6)
RestArea(aAreaSC9)
RestArea(aAreaSE4)
RestArea(aAreaSF3)
RestArea(aAreaSF4)

Return


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : TMKFis
// Descrição: Carrega os impostos de um pedido de vendas, usando as funções de
//            impostos MATXFIS.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 09/01/14 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function TMKFis(cAtend)
// Rotina baseada na rotina padrão Ma410Impos.
Local cAlias     := Alias()
Local aArea      := {}
Local nX, nY

Local aFisGetSUA := {}
Local aFisGetSUB := {}
Local nPosCpo    := 0

Local nPItem     := 0
Local nPProduto  := 0
Local nPQtdVen   := 0
Local nPPrUnit   := 0
Local nPPrcVen   := 0
Local nPTotal    := 0
Local nPValDesc  := 0
Local nPLocal    := 0
Local nPDtEntr   := 0
Local nPTES      := 0
Local nPCFOP     := 0
Local nPNfOri    := 0
Local nPSerOri   := 0
Local nPItemOri  := 0
Local nPIdentB6  := 0
Local nPSuframa  := 0

Local aDupl      := {}
Local aVencto    := {}
Local aEntr      := {}
Local aDuplTmp   := {}
Local nAcerto    := 0
Local nPosEntr   := 0

Local cProduto   := ""
Local nTotDesc   := 0
Local nQuant     := 0
Local nPrcLista  := 0
Local nValMerc   := 0
Local nDesconto  := 0
Local nQtdPeso   := 0

Local nRecOri    := 0
Local nItem      := 0
Local nPropLot   := 0
Local lDtEmi     := SuperGetMv("MV_DPDTEMI", .F., .T.)
Local cTpDpInd   := GetNewPar("MV_TPDPIND", "1")
Local dDataCnd   := ctod("")

Local nAcresTot  := 0
Local lCfo       := .F.
Local aTransp    := {"", ""}
Local aSaldos    := {}
Local aInfLote   := {}
Local aNfOri     := {}
Local cEstado    := SuperGetMv("MV_ESTADO")
Local cTesVend   := SuperGetMv("MV_TESVEND",, "")
Local nlValor    := 0

Local aFldSUA	:= FWSX3Util():GetListFieldsStruct("SUA", .T.)
Local aFldSUB	:= FWSX3Util():GetListFieldsStruct("SUB", .T.)
Local nSUA		:= 0
Local nSUB		:= 0
Local nPos		:= 0

Default cAtend   := SUA->UA_NUM

// Guarda posição das tabelas.
If empty(cAlias)
	cAlias := "SX3"
	dbSelectArea(cAlias)
Endif
aArea := sGetArea()
sGetArea(aArea, "SX3")
sGetArea(aArea, "SA1")
sGetArea(aArea, "SA2")
sGetArea(aArea, "SA4")
sGetArea(aArea, "SB2")
sGetArea(aArea, "SB8")
sGetArea(aArea, "SE4")
sGetArea(aArea, "SF3")
sGetArea(aArea, "SF4")
sGetArea(aArea, "SUA")
sGetArea(aArea, "SUB")

SUA->(dbSetOrder(1))  // UA_FILIAL, UA_NUM.
If SUA->(dbSeek(xFilial() + cAtend, .F.))
	// Inicializa as variáveis de memória.
	RegToMemory("SUA", .F., .F.)

	// Montagem do aHeader/aCols.
	aHeader := {}; aCols := {}; n := 0
	FillGetDados(4, "SUB", 1, xFilial("SUB") + SUA->UA_NUM, {|| UB_FILIAL + UB_NUM},,,,,,,, aHeader, aCols)

	// Busca a posição das variáves locais.
	nPItem    := GdFieldPos("UB_ITEM")
	nPProduto := GdFieldPos("UB_PRODUTO")
	nPQtdVen  := GdFieldPos("UB_QUANT")
	nPPrUnit  := GdFieldPos("UB_PRCTAB")
	nPPrcVen  := GdFieldPos("UB_VRUNIT")
	nPTotal   := GdFieldPos("UB_VLRITEM")
	nPValDesc := GdFieldPos("UB_VALDESC")
	nPLocal   := GdFieldPos("UB_LOCAL")
	nPDtEntr  := GdFieldPos("UB_DTENTRE")
	nPTES     := GdFieldPos("UB_TES")
	nPCFOP    := GdFieldPos("UB_CF")
	nUsado    := len(aHeader)
	dDataCnd  := M->UA_EMISSAO

	// Busca referências no SUA.
	aFisGetSUA := {}

	For nSUA := 1 to Len(aFldSUA)
		cValid := 		cValid := GetSX3Cache(aFldSUA[nSUA][1], "X3_VALID") + GetSX3Cache(aFldSUA[nSUA][1], "X3_VLDUSER")
		If 'MAFISGET("' $ cValid
			nPosIni 	:= AT('MAFISGET("', cValid) + 10
			nLen		:= AT('")', Substr(cValid, nPosIni, Len(cValid) - nPosIni)) - 1
			cReferencia := Substr(cValid, nPosIni, nLen)
			aAdd(aFisGetSUA, {cReferencia, aFldSUA[nSUA][1], MaFisOrdem(cReferencia)})
		Endif
		If 'MAFISREF("' $ cValid
			nPosIni		:= AT('MAFISREF("', cValid) + 10
			nLen		:= AT('","MT410",', Substr(cValid, nPosIni, Len(cValid) - nPosIni)) - 1
			cReferencia	:= Substr(cValid, nPosIni, nLen)
			aAdd(aFisGetSUA, {cReferencia, aFldSUA[nSUA][1], MaFisOrdem(cReferencia)})
		Endif
	Next nSUA

	aSort(aFisGetSUA,,, {|x, y| x[3] < y[3]})

	// Busca referências no SUB.
	aFisGetSUB := {}

	For nSUB := 1 to Len(aFldSUB)
		cValid := 		cValid := GetSX3Cache(aFldSUB[nSUB][1], "X3_VALID") + GetSX3Cache(aFldSUB[nSUB][1], "X3_VLDUSER")
		If 'MAFISGET("' $ cValid
			nPosIni 	:= AT('MAFISGET("', cValid) + 10
			nLen		:= AT('")', Substr(cValid, nPosIni, Len(cValid) - nPosIni)) - 1
			cReferencia := Substr(cValid, nPosIni, nLen)
			aAdd(aFisGetSUB, {cReferencia, aFldSUB[nSUB][1], MaFisOrdem(cReferencia)})
		Endif
		If 'MAFISREF("' $ cValid
			nPosIni		:= AT('MAFISREF("', cValid) + 10
			nLen		:= AT('","MT410",', Substr(cValid, nPosIni, Len(cValid) - nPosIni)) - 1
			cReferencia	:= Substr(cValid, nPosIni, nLen)
			aAdd(aFisGetSUB, {cReferencia, aFldSUB[nSUB][1], MaFisOrdem(cReferencia)})
		Endif
	Next nSUB

	aSort(aFisGetSUB,,, {|x, y| x[3] < y[3]})

	SA4->(dbSetOrder(1))  // A4_FILIAL, A4_COD.
	If SA4->(msSeek(xFilial() + M->UA_TRANSP, .F.))
		aTransp[01] := SA4->A4_EST
		aTransp[02] := If(SA4->(FieldPos("A4_TPTRANS")) > 0, SA4->A4_TPTRANS, "")
	Endif

	// Inicializa a função fiscal.
	MaFisEnd()
	MaFisIni(M->UA_CLIENTE, M->UA_LOJA, "C", "N", M->UA_TIPOCLI,,,,, "MATA461",,, If(M->UA_PROSPEC, M->(UA_CLIENTE + UA_LOJA), ""),,,,, aTransp)

	// Realiza alterações de referências do SUA.
	dbSelectArea("SUA")
	For nX := 1 to len(aFisGetSUA)
		If !Empty(&("M->" + Alltrim(aFisGetSUA[nX, 2])))
			MaFisAlt(aFisGetSUA[nX, 1], &("M->" + Alltrim(aFisGetSUA[nX, 2])),, .F.)
		Endif
	Next nX

	// Agrega os itens para a função fiscal.
	If nPTotal > 0 .And. nPValDesc > 0 .And. nPPrUnit > 0 .And. nPProduto > 0 .And. nPQtdVen > 0 .And. nPTes > 0
		For nX := 1 to len(aCols)
			nQtdPeso := 0
			If !aCols[nX, nUsado + 1]
				nItem++

				// Posiciona registros.
				cProduto := aCols[nX, nPProduto]
				MatGrdPrRf(@cProduto)
				SB1->(dbSetOrder(1))  // B1_FILIAL, B1_COD.
				SB1->(msSeek(xFilial() + cProduto, .F.))
				SB2->(dbSetOrder(1))  // B2_FILIAL, B2_COD, B2_LOCAL.
				SB2->(msSeek(xFilial() + aCols[nX, nPProduto] + aCols[nX, nPLocal], .F.))
				SUB->(dbSetOrder(1))  // UB_FILIAL, UB_NUM, UB_ITEM, UB_PRODUTO.
				SUB->(dbSeek(xFilial() + M->UA_NUM + aCols[nX, nPItem] + aCols[nX, nPProduto], .F.))
				SF4->(dbSetOrder(1))  // F4_FILIAL, F4_CODIGO.
				SF4->(msSeek(xFilial() + aCols[nX, nPTES], .F.))

				// Dados do item.
				nQuant    := aCols[nX, nPQtdVen]
				nValMerc  := aCols[nX, nPTotal]
				nPrcLista := aCols[nX, nPPrUnit]
				nDesconto := a410Arred(nQuant * nPrcLista, "D2_DESCON") - nValMerc
				nDesconto := If(nDesconto <= 0, aCols[nX, nPValDesc], nDesconto)
				nDesconto := max(0, nDesconto)
				nValMerc  += nDesconto
				nQtdPeso  := nQuant * SB1->B1_PESO

				// Verifica a data de entrega para as duplicatas.
				If (nPDtEntr > 0)
					If (dDataCnd > aCols[nX, nPDtEntr] .And. !Empty(aCols[nX, nPDtEntr]))
						dDataCnd := aCols[nX, nPDtEntr]
					Endif
				Else
					dDataCnd := M->UA_EMISSAO
				Endif

				// Agrega os itens para a função fiscal.
				MaFisAdd(cProduto, aCols[nX, nPTES], nQuant, nPrcLista, nDesconto, "", "", nRecOri, 0, 0, 0, 0, nValMerc, 0, SB1->(RecNo()), SF4->(RecNo()),;
				If(nPItem > 0, aCols[nX, nPItem], ""), 0, 0, aCols[nX,nPCFOP])

				// Altera peso para calcular frete.
				MaFisAlt("IT_PESO",    nQtdPeso,  nItem)
				MaFisAlt("IT_PRCUNI",  nPrcLista, nItem)
				MaFisAlt("IT_VALMERC", nValMerc,  nItem)

				// Analise da rentabilidade.
				If SF4->F4_DUPLIC == "S"
					nTotDesc += MaFisRet(nItem, "IT_DESCONTO")
				Else
					If cTpDpInd == "1"
						nTotDesc += MaFisRet(nItem, "IT_DESCONTO")
					Endif
				Endif
			Endif
		Next nX
	Endif

	// Indica os valores do cabecalho.
	MaFisAlt("NF_FRETE",    M->UA_FRETE)
	MaFisAlt("NF_DESPESA",  M->(UA_ACRECND + UA_DESPESA))

	// Indenização por valor.
	If M->UA_DESCONT > 0
		MaFisAlt("NF_DESCONTO", min(MaFisRet(nil, "NF_VALMERC") - 0.01, nTotDesc + M->UA_DESCONT),/*nItem*/,/*lNoCabec*/,/*nItemNao*/, cTpDpInd == "2")
	Endif

	// Realiza alterações de referências do SUB.
	dbSelectArea("SUB")
	If Len(aFisGetSUB) > 0
		For nX := 1 to Len(aCols)
			If !aCols[nX, nUsado + 1]
				For nY := 1 to len(aFisGetSUB)
					nPosCpo := GdFieldPos(aFisGetSUB[ny, 2])
					If nPosCpo > 0
						If !Empty(aCols[nX, nPosCpo])
							MaFisAlt(aFisGetSUB[ny, 1], aCols[nX, nPosCpo], nX, .F.)
						Endif
					Endif
				Next nX
			Endif
		Next nY
	Endif

	// Realiza alterações de referências do UA_SUFRAMA.
	nPSuframa := aScan(aFisGetSUA, {|x| x[1] == "NF_SUFRAMA"})
	If !Empty(nPSuframa)
		dbSelectArea("SUA")
		If !Empty(&("M->" + Alltrim(aFisGetSUA[nPSuframa, 2])))
			MaFisAlt(aFisGetSUA[nPSuframa, 1], (&("M->"+Alltrim(aFisGetSUA[nPSuframa, 2])) == "1"), nItem, .F.)
		Endif
	Endif

	MaFisWrite(1)

	// Calcula os venctos conforme a condição de pagto.
	If lDtEmi
		SE4->(dbSetOrder(1))  // E4_FILIAL, E4_CODIGO.
		SE4->(msSeek(xFilial() + M->UA_CONDPG, .F.))
		If ((SE4->E4_TIPO == "9" .and. !(INCLUI .or. ALTERA)) .or. SE4->E4_TIPO <> "9")
			nAcerto := 0
			nlValor := MaFisRet(nil, "NF_BASEDUP")

			aDupl := Condicao(nlValor, M->UA_CONDPG, MaFisRet(nil, "NF_VALIPI"), dDataCnd, MaFisRet(nil, "NF_VALSOL"),,, nAcresTot)
			If Len(aDupl) > 0
				For nX := 1 To Len(aDupl)
					nAcerto += aDupl[nX, 2]
				Next nX
				aDupl[Len(aDupl), 2] += MaFisRet(nil, "NF_BASEDUP") - nAcerto

				aVencto := aClone(aDupl)
				For nX := 1 To Len(aDupl)
					aDupl[nX, 2] := Transform(aDupl[nX, 2], PesqPict("SE1", "E1_VALOR"))
				Next nX
			Endif
		Else
			aDupl   := {{Ctod(""), Transform(MaFisRet(nil, "NF_BASEDUP"), PesqPict("SE1", "E1_VALOR"))}}
			aVencto := {{dDataBase, MaFisRet(nil, "NF_BASEDUP")}}
		Endif
	Else
		nItem := 0
		For nX := 1 to Len(aCols)
			If Len(aCols[nX])==nUsado .Or. !aCols[nX, nUsado+1]
				nItem++
				If nPDtEntr > 0
					nPosEntr := Ascan(aEntr,{|x| x[1] == aCols[nX, nPDtEntr]})
					If nPosEntr == 0
						aAdd(aEntr, {aCols[nX, nPDtEntr], MaFisRet(nItem, "IT_BASEDUP"), MaFisRet(nItem, "IT_VALIPI"), MaFisRet(nItem, "IT_VALSOL")})
					Else
						aEntr[nPosEntr, 2] += MaFisRet(nItem, "IT_BASEDUP")
						aEntr[nPosEntr, 3] += MaFisRet(nItem, "IT_VALIPI")
						aEntr[nPosEntr, 4] += MaFisRet(nItem, "IT_VALSOL")
					Endif
				Endif
			Endif
		Next

		SE4->(dbSetOrder(1))  // E4_FILIAL, E4_CODIGO.
		SE4->(msSeek(xFilial() + M->UA_CONDPG, .F.))
		If (SE4->E4_TIPO <> "9")
			For nY := 1 to len(aEntr)
				nAcerto := 0
				nlValor := aEntr[nY, 2]

				aDuplTmp := Condicao(nlValor, M->UA_CONDPG, aEntr[nY, 3], aEntr[nY, 1], aEntr[nY, 4],,, nAcresTot)
				If len(aDuplTmp) > 0
					For nX := 1 To Len(aDuplTmp)
						nAcerto += aDuplTmp[nX, 2]
					Next nX
					aDuplTmp[Len(aDuplTmp), 2] += aEntr[nY, 2] - nAcerto

					aVencto := aClone(aDuplTmp)
					For nX := 1 To Len(aDuplTmp)
						aDuplTmp[nX, 2] := Transform(aDuplTmp[nX, 2], PesqPict("SE1", "E1_VALOR"))
					Next nX
					aEval(aDuplTmp, {|x| aAdd(aDupl, {aEntr[nY, 1], x[1], x[2]})})
				Endif
			Next
		Else
			aDupl   := {{Ctod(""), Transform(MaFisRet(nil, "NF_BASEDUP"), PesqPict("SE1", "E1_VALOR"))}}
			aVencto := {{dDataBase, MaFisRet(nil, "NF_BASEDUP")}}
		Endif
	Endif

	If len(aDupl) == 0
		aDupl   := {{Ctod(""), Transform(MaFisRet(nil, "NF_BASEDUP"), PesqPict("SE1", "E1_VALOR"))}}
		aVencto := {{dDataBase, MaFisRet(nil, "NF_BASEDUP")}}
	Endif
Endif

// Restaura posição das tabelas.
(cAlias)->(sRestArea(aArea))
dbSelectArea(cAlias)

Return


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : GrvImpPV
// Descrição: Grava os impostos do pedido de venda nas tabelas SC5 e SC6.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 27/11/13 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function GrvImpPV()
Local nItem      := 0
Local nCustoIt   := 0
Local nPrcLiqIt  := 0
Local nCustoTot  := 0
Local nPrcLiqTot := 0
Local cOrdCom    := ""
Local cContrato  := ""
Local dVigCrt    := ctod("")

Local lRentaNeg  := .F.
Local cAssunto   := ""
Local cMsgMail   := ""
Local cTabItens  := ""
Local lMotor     := IsInCallStack("U_ZB010JRun")

Local aArea		:= GetArea()
Local aAreaSB1	:= SB1->(GetArea())
Local aAreaSB2	:= SB2->(GetArea())
Local aAreaSB8	:= SB8->(GetArea())
Local aAreaSC5	:= SC5->(GetArea())
Local aAreaSC6	:= SC6->(GetArea())
Local aAreaSC9	:= SC9->(GetArea())

// Guarda a situação corrente das funções fiscais.
MaFisSave()

// Carrega os impostos.
U_PVFis()

// Monta tabela de itens para ser enviada por e-mail.
cTabItens := "<table>"
cTabItens += " <tr>"
cTabItens += "  <th style='border:1px solid #002a5c; border-collapse:collapse; width:05%; font-weight:bold; text-align:left'>It</th>"
cTabItens += "  <th style='border:1px solid #002a5c; border-collapse:collapse; width:30%; font-weight:bold; text-align:left'>Produto</th>"
cTabItens += "  <th style='border:1px solid #002a5c; border-collapse:collapse; width:15%; font-weight:bold; text-align:left'>Qtde</th>"
cTabItens += "  <th style='border:1px solid #002a5c; border-collapse:collapse; width:20%; font-weight:bold; text-align:right'>Vlr Unit</th>"
cTabItens += "  <th style='border:1px solid #002a5c; border-collapse:collapse; width:20%; font-weight:bold; text-align:right'>Total</th>"
cTabItens += "  <th style='border:1px solid #002a5c; border-collapse:collapse; width:10%; font-weight:bold; text-align:right'>Rentab.</th>"
cTabItens += " </tr>"

// Grava os dados dos itens.
SC6->(dbSetOrder(1))  // C6_FILIAL, C6_NUM, C6_ITEM, C6_PRODUTO.
SC6->(dbSeek(xFilial() + SC5->C5_NUM, .F.))
Do While SC6->(!eof() .and. C6_FILIAL + C6_NUM == xFilial() + SC5->C5_NUM)
	nItem++

	SB1->(dbSetOrder(1))  // B1_FILIAL, B1_COD.
	SB1->(msSeek(xFilial() + SC6->C6_PRODUTO, .F.))

	RecLock("SC6", .F.)

	SC6->C6_XBASICM := MaFisRet(nItem, "IT_BASEICM")
	SC6->C6_XALQICM := MaFisRet(nItem, "IT_ALIQICM")
	SC6->C6_XVALICM := MaFisRet(nItem, "IT_VALICM")

	SC6->C6_XBASSOL := MaFisRet(nItem, "IT_BASESOL")
	SC6->C6_XVALSOL := MaFisRet(nItem, "IT_VALSOL")

	SC6->C6_XBASIPI := MaFisRet(nItem, "IT_BASEIPI")
	SC6->C6_XALQIPI := MaFisRet(nItem, "IT_ALIQIPI")
	SC6->C6_XVALIPI := MaFisRet(nItem, "IT_VALIPI")

	SC6->C6_XBASPCC := MaFisRet(nItem, "IT_BASECF2")
	SC6->C6_XALQPS2 := MaFisRet(nItem, "IT_ALIQPS2")
	SC6->C6_XVALPIS := MaFisRet(nItem, "IT_VALPS2")
	SC6->C6_XALQCF2 := MaFisRet(nItem, "IT_ALIQCF2")
	SC6->C6_XVALCOF := MaFisRet(nItem, "IT_VALCF2")

	SC6->C6_XALQCMP := MaFisRet(nItem, "IT_ALIQCMP")
	SC6->C6_XVALCMP := MaFisRet(nItem, "IT_VALCMP")
	SC6->C6_XBASDES := MaFisRet(nItem, "IT_BASEDES")
	SC6->C6_XDIFAL  := MaFisRet(nItem, "IT_DIFAL")
	SC6->C6_XALFCMP := MaFisRet(nItem, "IT_ALFCCMP")
	SC6->C6_XVFCPDI := MaFisRet(nItem, "IT_VFCPDIF")

	SC6->C6_XVALFRE := MaFisRet(nItem, "IT_FRETE")

	// Calcula custo e rentabilidade da linha.
	nCustoIt  := SC6->C6_QTDVEN * round(SB1->B1_CUSTD * (1 - ((SB1->B1_XICMF + nAliqPCC) / 100)), 8)           // Custo líquido.
	nPrcLiqIt := SC6->(C6_VALOR - C6_XVALICM - C6_XVALPIS - C6_XVALCOF - C6_XVALCMP - C6_XDIFAL - C6_XVFCPDI)  // Preço líquido.
	SC6->C6_XVLRLIQ := nPrcLiqIt
	SC6->C6_XCUSTO  := nCustoIt
	SC6->C6_XRENTA  := min(100, max(-100, If(nPrcLiqIt = 0, 0, (1 - (nCustoIt / nPrcLiqIt)) * 100)))

	// Armazena a ordem de compra para gravar no SC5.
	If AllTrim(SC6->C6_NUMPCOM) <> "" .and. !(AllTrim(SC6->C6_NUMPCOM) $ cOrdCom)
		cOrdCom += AllTrim(SC6->C6_NUMPCOM) + "/"
	Endif

	SC6->(msUnlock())

	// Adiciona linha na tabela.
	cTabItens += " <tr>"
	cTabItens += "  <th style='border:1px solid #002a5c; border-collapse:collapse; font-weight:normal; text-align:left'>" + SC6->C6_ITEM + "</th>"
	cTabItens += "  <th style='border:1px solid #002a5c; border-collapse:collapse; font-weight:normal; text-align:left'>" + U_Txt2HTML(SC6->C6_PRODUTO, .T.) + "</th>"
	cTabItens += "  <th style='border:1px solid #002a5c; border-collapse:collapse; font-weight:normal; text-align:right'>" + Transform(SC6->C6_QTDVEN, cPictQtd) + "</th>"
	cTabItens += "  <th style='border:1px solid #002a5c; border-collapse:collapse; font-weight:normal; text-align:right'>R$ " + Transform(SC6->C6_PRCVEN, cPictVlr) + "</th>"
	cTabItens += "  <th style='border:1px solid #002a5c; border-collapse:collapse; font-weight:normal; text-align:right'>R$ " + Transform(SC6->C6_VALOR,  cPictVlr) + "</th>"
	cTabItens += "  <th style='border:1px solid #002a5c; border-collapse:collapse; font-weight:normal; text-align:right'>" + Transform(SC6->C6_XRENTA, cPictPer) + "%</th>"
	cTabItens += " </tr>"
	lRentaNeg := lRentaNeg .or. SC6->C6_XRENTA < 0  // Define se há algum item com rentabilidade negativa.

	// Totalizadores auxiliares para o cálculo da rentabilidade total.
	nCustoTot  += nCustoIt
	nPrcLiqTot += nPrcLiqIt

	SC6->(dbSkip())
EndDo

// Finaliza tabela de itens do pedido.
cTabItens += "</table>"

// Grava o campo de valor total do pedido de venda.
RecLock("SC5", .F.)

SC5->C5_XVALFAT := MaFisRet(nil, "NF_BASEDUP")
SC5->C5_XVALMER := MaFisRet(nil, "NF_VALMERC")
SC5->C5_XDESCZF := MaFisRet(nil, "NF_DESCZF")
SC5->C5_XTOTAL  := MaFisRet(nil, "NF_TOTAL")

SC5->C5_XBASICM := MaFisRet(nil, "NF_BASEICM")
SC5->C5_XVALICM := MaFisRet(nil, "NF_VALICM")

SC5->C5_XBASSOL := MaFisRet(nil, "NF_BASESOL")
SC5->C5_XVALSOL := MaFisRet(nil, "NF_VALSOL")

SC5->C5_XBASIPI := MaFisRet(nil, "NF_BASEIPI")
SC5->C5_XVALIPI := MaFisRet(nil, "NF_VALIPI")

SC5->C5_XBASPCC := MaFisRet(nil, "NF_BASECF2")
SC5->C5_XVALPIS := MaFisRet(nil, "NF_VALPS2")
SC5->C5_XVALCOF := MaFisRet(nil, "NF_VALCF2")

SC5->C5_XVALCMP := MaFisRet(nil, "NF_VALCMP")
SC5->C5_XDIFAL  := MaFisRet(nil, "NF_DIFAL")
SC5->C5_XVFCPDI := MaFisRet(nil, "NF_VFCPDIF")

SC5->C5_XVLRLIQ := nPrcLiqTot
SC5->C5_XCUSTO  := nCustoTot
SC5->C5_XRENTA  := min(100, max(-100, If(nPrcLiqTot = 0, 0, (1 - (nCustoTot / nPrcLiqTot)) * 100)))

SC5->C5_X_OC    := left(cOrdCom, len(cOrdCom) - 1)

SC5->(MsUnLock())

// Restaura a situação anterior das funções fiscais.
MaFisRestore()

// Se tiver algum item com margem negativa, mandar e-mail para o Leonardo.
If !lMotor .and. lRentaNeg .and. !empty(cEmailNeg) .and. SC5->C5_XVALFAT > 0 .and. SC5->C5_TIPO = "N"
	cAssunto := "Pedido (" + SC5->C5_FILIAL + "-" + rtrim(FWFilialName(nil, SC5->C5_FILIAL, 1)) + ") " + SC5->C5_NUM + " - item com rentabilidade baixa"
	cMsgMail := "O pedido " + SC5->C5_NUM + " foi gravado com algum item com rentabilidade negativa. " + CRLF + CRLF
	cMsgMail += "Cliente: " + SC5->(C5_CLIENTE + "/" + C5_LOJACLI) + " - " + rtrim(U_PVForCli("A1_NOME", SC5->C5_NUM)) + CRLF
	U_MA020Ctr(SC5->C5_CLIENTE, SC5->C5_LOJACLI,,, @cContrato, @dVigCrt, .F.)
	If !empty(cContrato)
		cMsgMail += "Contrato.: " + cContrato + " - vigência " + dtoc(dVigCrt) + CRLF
	ElseIf !empty(SC5->C5_XFUNCAT)
		cMsgMail += "Cliente de " + SC5->C5_XFUNCAT + " - " + RTrim(Posicione("SX5", 1, xFilial("SX5") + "X0" + SC5->C5_XFUNCAT, "X5Descri()")) + CRLF
	Endif
	cMsgMail += "Tipo.....: " + SC5->C5_XTIPO + " - " + rtrim(Posicione("SZF", 1, xFilial("SZF") + SC5->C5_XTIPO, "ZF_DESC")) + CRLF + CRLF
	cMsgMail += "Total do pedido: R$ " + Transform(SC5->C5_XVALMER, cPictVlr) + " (rentabilidade " + Transform(SC5->C5_XRENTA, cPictPer) + "%)" + CRLF + CRLF
	cMsgMail := U_Txt2HTML(cMsgMail, .T.)
	cMsgMail += cTabItens
	U_EnvEMail(nil, cEmailNeg, cAssunto, cMsgMail,, .T.)
Endif

// Restaura areas de trabalho.
RestArea(aArea)
RestArea(aAreaSB1)
RestArea(aAreaSB2)
RestArea(aAreaSB8)
RestArea(aAreaSC5)
RestArea(aAreaSC6)
RestArea(aAreaSC9)

Return


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : LibPV
// Descrição: Refaz a liberação de itens de pedido de venda.
// Retorno  : Nenhum
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 16/09/14 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function LibPV(cPedido, lEstorna, lLibera, cEstoque)
Local lRet       := .T.
Local cAlias     := Alias()
Local aArea      := {}

Local lAlterado  := .F.
Local cAliasSQL  := ""
Local lEnd       := .F.

Default cPedido    := SC5->C5_NUM
Default lEstorna   := .T.
Default lLibera    := .T.

// Salva áreas de trabalho.
If empty(cAlias)
	cAlias := "SX3"
	dbSelectArea(cAlias)
Endif
aArea := sGetArea()
sGetArea(aArea, "SB2")
sGetArea(aArea, "SB8")
sGetArea(aArea, "SC5")
sGetArea(aArea, "SC6")

If SoftLock("SC5")
	// Estorna os itens já liberados antes de efetuar a liberação do pedido.
	If lEstorna
		// Executa query para buscar os itens a serem estornados.
		cAliasSQL := GetNextAlias()
		BeginSql Alias cAliasSQL
			%noparser%
			select SC5.R_E_C_N_O_ SC5RecNo, SC6.R_E_C_N_O_ SC6RecNo, SC9.R_E_C_N_O_ SC9RecNo,
			SB2.R_E_C_N_O_ SB2RecNo, SB8.R_E_C_N_O_ SB8RecNo
			from %Table:SC9% SC9
			inner join %Table:SC5% SC5 on SC5.%NotDel%
			and SC5.C5_FILIAL  = %xFilial:SC5%
			and SC5.C5_NUM     = SC9.C9_PEDIDO
			inner join %Table:SC6% SC6 on SC6.%NotDel%
			and SC6.C6_FILIAL  = %xFilial:SC6%
			and SC6.C6_NUM     = SC9.C9_PEDIDO
			and SC6.C6_ITEM    = SC9.C9_ITEM
			left  join %Table:SB2% SB2 on SB2.%NotDel%
			and SB2.B2_FILIAL  = %xFilial:SB2%
			and SB2.B2_COD     = SC9.C9_PRODUTO
			and SB2.B2_LOCAL   = SC9.C9_LOCAL
			left  join %Table:SB8% SB8 on SB8.%NotDel%
			and SB8.B8_FILIAL  = %xFilial:SB8%
			and SB8.B8_PRODUTO = SC9.C9_PRODUTO
			and SB8.B8_LOCAL   = SC9.C9_LOCAL
			and SB8.B8_DTVALID = SC9.C9_DTVALID
			and SB8.B8_LOTECTL = SC9.C9_LOTECTL
			and SB8.B8_NUMLOTE = SC9.C9_NUMLOTE
			where SC9.%NotDel%
			and SC9.C9_FILIAL  = %xFilial:SC9%
			and SC9.C9_PEDIDO  = %Exp:cPedido%
			and SC9.C9_BLCRED  not in ('09', '10', 'ZZ')
			and SC9.C9_NFISCAL = ' '
			and SC6.C6_QTDVEN  > 0
			order by %order:SC9,1%
		EndSql

		// Estorna todos os itens retornados pela query.
		Do While (cAliasSQL)->(!eof())
			SC5->(dbGoto((cAliasSQL)->SC5RecNo))
			SC6->(dbGoto((cAliasSQL)->SC6RecNo))
			SC9->(dbGoto((cAliasSQL)->SC9RecNo))
			SB2->(dbGoto((cAliasSQL)->SB2RecNo))
			SB8->(dbGoto((cAliasSQL)->SB8RecNo))

			If SC9->(!deleted() .and. empty(C9_NFISCAL) .and. aScan({"09", "10", "ZZ"}, C9_BLCRED) == 0)
				SC9->(A460Estorna())
				lAlterado := .T.
			Endif

			(cAliasSQL)->(dbSkip())
		EndDo
		(cAliasSQL)->(dbCloseArea())
	Endif

	// Efetua a liberação do pedido (gera SC9).
	If lLibera
		// Posiciona tabelas.
		SC5->(dbSetOrder(1))  // C5_FILIAL, C5_NUM.
		If SC5->(dbSeek(xFilial() + cPedido, .F.))

			// Gera OP automaticamente.
			If SC5->C5_CLIENTE <> "999999"  // Não gera OP para pedidos de transferência.
				lRet := U_ProManOP(cPedido)
				lAlterado := .T.
			Endif

			Begin Transaction

			// Estorna reserva para pedido NetSuprimento.
			If lRet
				If !empty(SC5->C5_XIDVTEX)
					U_SC0Exc(SC5->C5_XIDVTEX)
				Endif
			Endif

			If lRet
				Private MV_PAR01 := 1  // Ordem -> 1=Por pedido / 2=Data de entrega.
				Private MV_PAR02 := SC5->C5_NUM
				Private MV_PAR03 := SC5->C5_NUM
				Private MV_PAR04 := SC5->C5_CLIENTE
				Private MV_PAR05 := SC5->C5_CLIENTE
				Private MV_PAR06 := stod("")
				Private MV_PAR07 := stod("20491231")
				Private MV_PAR08 := 1    // 1=Libera crédito e estoque.
				Private MV_PAR09 := SC5->C5_LOJACLI
				Private MV_PAR10 := SC5->C5_LOJACLI
				Private lTransf  := .F.  // Transfere locais para a liberação.
				Private lLiber   := (SC5->C5_TIPLIB = "1")  // Libera somente produtos com saldo em estoque.
				Private lSugere  := .T.  // Sugere quantidade liberada.

				a440Proces("SC5", SC5->(RecNo()), 1, @lEnd)
				lAlterado := .T.
			Endif

			End Transaction
		Endif
	Endif

	// Processa campos somente se houve alguma alteração na liberação do pedido.
	If lAlterado
		// Classifica a liberação do pedido (C5_LIBEROK).
		MaLiberOk({cPedido})

		// Acerta o status do pedido quanto ao estoque.
		U_EstPV(.T., SC5->C5_NUM,, @cEstoque)
		RecLock("SC5", .F.)
		SC5->C5_XESTOQ := cEstoque
		SC5->(msUnLock())

		// Verifica as reservas do pedido.
		U_VerB2Res(cPedido)
	Endif
	SC5->(MsUnlock())
Else
	Help(" ", 1, "Help", "LibPV", "Pedido em manutenção por outro usuário.", 3, 0)
EndIf

// Restaura areas de trabalho.
(cAlias)->(sRestArea(aArea))
dbSelectArea(cAlias)

Return lRet


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : EstPV
// Descrição: Verifica se o pedido tem todos os itens em estoque.
// Retorno  : Lógico, indicando se o pedido tem todos os itens em estoque.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 20/02/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function EstPV(lVerSC9, cPedido, cItem, cEstoque)
Local lRet       := .T.
Local cAlias     := Alias()
Local aArea      := {}
Local cAliasSQL  := ""
Local nLiber     := 0
Local nEstDisp   := 0

Default lVerSC9    := .F.
Default cPedido    := SC5->C5_NUM
Default cItem      := ""

// Salva áreas de trabalho.
If empty(cAlias)
	cAlias := "SX3"
	dbSelectArea(cAlias)
Endif
aArea := sGetArea()
sGetArea(aArea, "SC5")
sGetArea(aArea, "SC6")
sGetArea(aArea, "SC9")

cAliasSQL := GetNextAlias()

// Procura algum item sem saldo em estoque.
If lVerSC9
	// Procura algum item bloqueado por crédito ou estoque.
	BeginSql Alias cAliasSQL
		%noparser%
		select
		(SC6.C6_QTDVEN - SC6.C6_QTDENT) SaldoPV,
		isnull((
			select sum(SC9.C9_QTDLIB)
			from %Table:SC9% SC9
			where SC9.%NotDel%
			and SC9.C9_FILIAL  = %xFilial:SC9%
			and SC9.C9_PEDIDO  = SC6.C6_NUM
			and SC9.C9_ITEM    = SC6.C6_ITEM
			and (SC9.C9_NFISCAL = ' ' and SC9.C9_BLCRED = ' ' and SC9.C9_BLEST = ' ')
		), 0) QtdLib,
		isnull(SB2.B2_QATU - SB2.B2_RESERVA, 0) EstDisp

		from %Table:SC6% SC6

		left  join %Table:SB2% SB2 on SB2.%NotDel%
		and SB2.B2_FILIAL  = %xFilial:SB2%
		and SB2.B2_COD     = SC6.C6_PRODUTO
		and SB2.B2_LOCAL   = SC6.C6_LOCAL

		where SC6.%NotDel%
		and SC6.C6_FILIAL  = %xFilial:SC6%
		and SC6.C6_NUM     = %Exp:cPedido%
		and SC6.C6_BLQ     not in ('R', 'S')
		and %Exp:cItem%    in ('', SC6.C6_ITEM)

		order by QtdLib desc, EstDisp desc
	EndSql

	// Verifica se há algum item sem estoque.
	If (cAliasSQL)->(eof())
		lRet := .F.
	Else
		Do While (cAliasSQL)->(!eof())
			// Verifica se há alguma liberação.
			If (cAliasSQL)->QtdLib > 0
				nLiber++
			Endif

			// Verifica se a linha não está 100% liberada.
			If (cAliasSQL)->(SaldoPV <> QtdLib)
				lRet := .F.
				If (cAliasSQL)->EstDisp > 0
					nEstDisp++
					Exit
				Endif
			Endif

			(cAliasSQL)->(dbSkip())
		EndDo
	Endif
	(cAliasSQL)->(dbCloseArea())
Else
	BeginSql Alias cAliasSQL
		%noparser%
		select
		(SC6.C6_QTDVEN - SC6.C6_QTDENT) SaldoPV,
		(
			isnull(SB2.B2_QATU - SB2.B2_RESERVA, 0) +
			isnull((
				select sum(SC9.C9_QTDLIB)
				from %Table:SC9% SC9
				where SC9.%NotDel%
				and SC9.C9_FILIAL  = %xFilial:SC9%
				and SC9.C9_PEDIDO  = SC6.C6_NUM
				and SC9.C9_ITEM    = SC6.C6_ITEM
				and (SC9.C9_NFISCAL = ' ' and SC9.C9_BLCRED = ' ' and SC9.C9_BLEST = ' ')
			), 0)
		) EstDisp

		from %Table:SC5% SC5

		inner join %Table:SC6% SC6 on SC6.%NotDel%
		and SC6.C6_FILIAL  = %xFilial:SC6%
		and SC6.C6_NUM     = SC5.C5_NUM

		left  join %Table:SB2% SB2 on SB2.%NotDel%
		and SB2.B2_FILIAL  = %xFilial:SB2%
		and SB2.B2_COD     = SC6.C6_PRODUTO
		and SB2.B2_LOCAL   = SC6.C6_LOCAL

		where SC5.%NotDel%
		and SC5.C5_FILIAL  = %xFilial:SC5%
		and SC5.C5_NUM     = %Exp:cPedido%
		and SC6.C6_BLQ     not in ('R', 'S')
		and %Exp:cItem%    in ('', SC6.C6_ITEM)

		order by EstDisp desc
	EndSql

	// Verifica se há algum item sem estoque.
	If (cAliasSQL)->(eof())
		lRet := .F.
	Else
		Do While (cAliasSQL)->(!eof())
			// Verifica se a linha tem estoque disponível.
			If (cAliasSQL)->(SaldoPV > EstDisp)
				lRet := .F.
				If (cAliasSQL)->EstDisp > 0
					nEstDisp++
					Exit
				Endif
			Endif

			(cAliasSQL)->(dbSkip())
		EndDo
	Endif
	(cAliasSQL)->(dbCloseArea())
Endif

// Define o status do pedido de venda.
If lRet .and. lVerSC9
	cEstoque := RESERVADO
ElseIf nLiber > 0 .and. lVerSC9
	cEstoque := RES_PARCIAL
ElseIf nEstDisp > 0
	cEstoque := DISPONIVEL
Else
	cEstoque := AGUARDANDO
Endif

// Restaura areas de trabalho.
(cAlias)->(sRestArea(aArea))
dbSelectArea(cAlias)

Return lRet


// ##############################################################################
// Projeto  : PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento
// Função   : MovQtdEst
// Descrição: Verifica se o pedido movimenta estoque.
// Retorno  : Lógico, indicando se o pedido movimenta estoque ou não.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 12/12/14 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function PVMovEst(cPedido)
Local lRet       := .F.
Local cAlias     := Alias()
Local aArea      := {}

Local cQuery     := ""
Local cAliasTop  := ""

// Salva áreas de trabalho.
If empty(cAlias)
	cAlias := "SC5"
	dbSelectArea(cAlias)
Endif
aArea := sGetArea()

Default cPedido := SC5->C5_NUM

cQuery := "select top 1 SC6.R_E_C_N_O_ SC6RecNo "
cQuery += "from " + RetSQLName("SC6") + " SC6 "
cQuery += "inner join " + RetSQLName("SF4") + " SF4 on SF4.D_E_L_E_T_ = ' ' "
cQuery += "and SF4.F4_FILIAL  = '" + xFilial("SF4") + "' "
cQuery += "and SF4.F4_CODIGO  = SC6.C6_TES "
cQuery += "and SF4.F4_ESTOQUE = 'S' "
cQuery += "where SC6.D_E_L_E_T_ = ' ' "
cQuery += "and SC6.C6_FILIAL  = '" + xFilial("SC6") + "' "
cQuery += "and SC6.C6_NUM     = '" + cPedido + "' "
cQuery += "and SC6.C6_QTDVEN  <> 0 "
cAliasTop := MPSysOpenQuery(cQuery)
lRet := (cAliasTop)->(!eof() .and. SC6RecNo > 0)
(cAliasTop)->(dbCloseArea())

// Restaura areas de trabalho.
(cAlias)->(sRestArea(aArea))
dbSelectArea(cAlias)

Return lRet


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : VerB2Res
// Descrição: Verifica reserva de produto, e corrige, se necessário.
// Retorno  : Nenhum
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 08/01/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function VerB2Res(cPedido)
Local cAlias     := Alias()
Local aArea      := {}
Local cAliasSQL  := GetNextAlias()

Local aLogSB2    := {}
Local aLogSB8    := {}
Local aLogSC6    := {}
Local nHlLog     := 0
Local cAgora     := ""
Local nX

If !U_Producao()
	Sleep(15 * 1000)  // Espera 15 segundos.
Endif
U_DebugMsg("Início do U_VerB2Res()", "I")

Default cPedido := SC5->C5_NUM

// Guarda o alias selecionado.
If empty(cAlias)
	cAlias := "SX3"
	dbSelectArea(cAlias)
Endif
aArea := sGetArea()
sGetArea(aArea, "SB2")
sGetArea(aArea, "SC6")

// *****************************************************************
// Acerta os itens com erro de reserva.
// *****************************************************************
BeginSql Alias cAliasSQL
	%noparser%
	select a.*

	from
	(
		select distinct SB2.R_E_C_N_O_ SB2RecNo,
		SB2.B2_FILIAL, SB2.B2_COD, SB2.B2_LOCAL, SB2.B2_RESERVA, SB2.B2_QPEDVEN,
		SC6Lib.C6_QTDLIB, SC9Lib.C9_QTDLIB, SC0Lib.C0_QUANT,
		(isnull(SC9Lib.C9_QTDLIB, 0) + isnull(SC0Lib.C0_QUANT, 0)) RES_CALC,
		(isnull(SC6Lib.C6_QTDLIB, 0) - isnull(SC9Lib.C9_QTDLIB, 0)) PVEN_CALC

		from %Table:SC6% SC6 with (noLock)

		inner join %Table:SB2% SB2 with (noLock) on SB2.%NotDel%
		and SB2.B2_FILIAL  = SC6.C6_FILIAL
		and SB2.B2_COD     = SC6.C6_PRODUTO
		and SB2.B2_LOCAL   = SC6.C6_LOCAL

		left join (
			select SC6.C6_FILIAL, SC6.C6_PRODUTO, SC6.C6_LOCAL, sum(SC6.C6_QTDVEN - SC6.C6_QTDENT) C6_QTDLIB

			from %Table:SC6% SC6 with (noLock)

			inner join %Table:SF4% SF4 with (noLock) on SF4.%NotDel%
			and SF4.F4_FILIAL  = %xFilial:SF4%
			and SF4.F4_CODIGO  = SC6.C6_TES
			and SF4.F4_ESTOQUE = 'S'

			where SC6.%NotDel%
			and SC6.C6_QTDVEN <> SC6.C6_QTDENT
			and SC6.C6_BLQ not in ('R', 'S')

			group by SC6.C6_FILIAL, SC6.C6_PRODUTO, SC6.C6_LOCAL
		) SC6Lib on SC6Lib.C6_FILIAL = SB2.B2_FILIAL
		and SC6Lib.C6_PRODUTO = SB2.B2_COD
		and SC6Lib.C6_LOCAL   = SB2.B2_LOCAL

		left join (
			select SC9.C9_FILIAL, SC9.C9_PRODUTO, SC9.C9_LOCAL,
			sum(SC9.C9_QTDLIB) C9_QTDLIB

			from %Table:SC9% SC9 with (noLock)

			inner join %Table:SC6% SC6 with (noLock) on SC6.%NotDel%
			and SC6.C6_FILIAL  = SC9.C9_FILIAL
			and SC6.C6_NUM     = SC9.C9_PEDIDO
			and SC6.C6_ITEM    = SC9.C9_ITEM

			inner join %Table:SF4% SF4 with (noLock) on SF4.%NotDel%
			and SF4.F4_FILIAL  = %xFilial:SF4%
			and SF4.F4_CODIGO  = SC6.C6_TES
			and SF4.F4_ESTOQUE = 'S'

			where SC9.%NotDel%
			and SC9.C9_BLCRED  = ' '
			and SC9.C9_BLEST   = ' '
			and SC9.C9_NFISCAL = ' '

			group by SC9.C9_FILIAL, SC9.C9_PRODUTO, SC9.C9_LOCAL
		) SC9Lib on SC9Lib.C9_FILIAL = SB2.B2_FILIAL
		and SC9Lib.C9_PRODUTO = SB2.B2_COD
		and SC9Lib.C9_LOCAL   = SB2.B2_LOCAL

		left join (
			select SC0.C0_FILIAL, SC0.C0_PRODUTO, SC0.C0_LOCAL,
			sum(SC0.C0_QUANT + SC0.C0_QTDPED) C0_QUANT

			from %Table:SC0% SC0 with (noLock)
			where SC0.%NotDel%

			group by SC0.C0_FILIAL, SC0.C0_PRODUTO, SC0.C0_LOCAL
		) SC0Lib on SC0Lib.C0_FILIAL = SB2.B2_FILIAL
		and SC0Lib.C0_PRODUTO = SB2.B2_COD
		and SC0Lib.C0_LOCAL   = SB2.B2_LOCAL

		where SC6.%NotDel%
		and SC6.C6_FILIAL  = %xFilial:SC6%
		and SC6.C6_NUM     = %Exp:cPedido%
	) a

	where
	a.B2_RESERVA <> a.RES_CALC or
	a.B2_QPEDVEN <> a.PVEN_CALC

	order by a.B2_FILIAL, a.B2_COD, a.B2_LOCAL
EndSql

// Acerta a reserva do produto, se necessário.
Do While (cAliasSQL)->(!eof())

	// Corrige o problema.
	SB2->(dbGoTo((cAliasSQL)->SB2RecNo))
	RecLock("SB2", .F.)
	SB2->B2_RESERVA := (cAliasSQL)->RES_CALC
	SB2->B2_QPEDVEN := (cAliasSQL)->PVEN_CALC
	SB2->(msUnLock())

	// Adiciona o produto no log de erro.
	(cAliasSQL)->(aAdd(aLogSB2, {SB2RecNo, B2_RESERVA, B2_QPEDVEN, RES_CALC, PVEN_CALC}))

	// Próximo registro.
	(cAliasSQL)->(dbSkip())
EndDo
(cAliasSQL)->(dbCloseArea())

// Monta log SB2.
If !empty(aLogSB2)
	// Cria o arquivo se não existir.
	Static cSB2Log := "\SB2Prob.log"
	cAgora := dtoc(date()) + " " + time() + " " + cUserName +  " - "
	If !file(cSB2Log)
		MemoWrite(cSB2Log, cAgora + "Arquivo criado." + CRLF)
	Endif

	// Exibe mensagem no console.
	cMsg := CRLF + cAgora + " - Erro no SB2 - Pedido " + cFilAnt + "/" + cPedido + CRLF

	// Monta pilha de chamadas.
	nX := 1
	Do While !empty(ProcName(nX)) .and. nX < 20
		cMsg += cAgora + "     - Função " + ProcName(nX) + " (linha " + cValToChar(ProcLine(nX)) + ")" + CRLF
		nX++
	EndDo

	// Lista linhas corrigidas.
	For nX := 1 to len(aLogSB2)
		SB2->(dbGoTo(aLogSB2[nX, 1]))
		cMsg += cAgora + "     - Produto " + SB2->(B2_FILIAL + " " + B2_COD + "/" + B2_LOCAL) + CRLF
		cMsg += cAgora + "       -  Reserva  " + cValToChar(aLogSB2[nX, 2]) + " (calculada " + cValToChar(aLogSB2[nX, 4]) + ")" + CRLF
		cMsg += cAgora + "       -  Ped. Ven " + cValToChar(aLogSB2[nX, 3]) + " (calculada " + cValToChar(aLogSB2[nX, 5]) + ")" + CRLF
	Next nX

	// Grava log de erro no arquivo.
	nHlLog := fOpen(cSB2Log, FO_READWRITE)  // Vai para o final do arquivo.
	If nHlLog > 0
		fSeek(nHlLog, 0, FS_END)  // Vai para o fim do arquivo.
		fWrite(nHlLog, cMsg, len(cMsg))
		fClose(nHlLog)
	Endif
Endif
U_DebugMsg("Acerto de SB2 ok.")

// *****************************************************************
// Acerta os itens com erro de empenho.
// *****************************************************************
BeginSql Alias cAliasSQL
	%noparser%
	select a.*
	from (
		select distinct SB8.R_E_C_N_O_ SB8RecNo, SB8.B8_EMPENHO,
		isnull((
			select sum(SD4.D4_QUANT)
			from %Table:SD4% SD4 with (noLock)
			where SD4.%NotDel%
			and SD4.D4_FILIAL  = %xFilial:SD4%
			and SD4.D4_COD     = SB8.B8_PRODUTO
			and SD4.D4_LOCAL   = SB8.B8_LOCAL
			and SD4.D4_LOTECTL = SB8.B8_LOTECTL
			and SD4.D4_NUMLOTE = SB8.B8_NUMLOTE
			and SD4.D4_DTVALID = SB8.B8_DTVALID
		), 0) EMP_CALC

		from %Table:SC6% SC6 with (noLock)

		inner join %Table:SB8% SB8 with (noLock) on SB8.%NotDel%
		and SB8.B8_FILIAL  = %xFilial:SB8%
		and SB8.B8_PRODUTO = SC6.C6_PRODUTO
		and SB8.B8_LOCAL   = SC6.C6_LOCAL

		where SC6.%NotDel%
		and SC6.C6_FILIAL = %xFilial:SC6%
		and SC6.C6_NUM    = %Exp:cPedido%
	) a
	where round(a.B8_EMPENHO, 2) <> round(a.EMP_CALC, 2)
EndSql

// Acerta a reserva do produto, se necessário.
Do While (cAliasSQL)->(!eof())

	// Corrige o problema.
	SB8->(dbGoTo((cAliasSQL)->SB8RecNo))
	RecLock("SB8", .F.)
	SB8->B8_EMPENHO := (cAliasSQL)->EMP_CALC
	SB8->(msUnLock())

	// Adiciona o produto no log de erro.
	(cAliasSQL)->(aAdd(aLogSB8, {SB8RecNo, B8_EMPENHO, EMP_CALC}))

	// Próximo registro.
	(cAliasSQL)->(dbSkip())
EndDo
(cAliasSQL)->(dbCloseArea())

// Monta log SB8.
If !empty(aLogSB8)
	// Cria o arquivo se não existir.
	Static cSB8Log := "\SB8Prob.log"
	cAgora := dtoc(date()) + " " + time() + " " + cUserName +  " - "
	If !file(cSB8Log)
		MemoWrite(cSB8Log, cAgora + "Arquivo criado." + CRLF)
	Endif

	// Exibe mensagem no console.
	cMsg := CRLF + cAgora + " - Erro no SB8 - Pedido " + cFilAnt + "/" + cPedido + CRLF

	// Monta pilha de chamadas.
	nX := 1
	Do While !empty(ProcName(nX)) .and. nX < 20
		cMsg += cAgora + "     - Função " + ProcName(nX) + " (linha " + cValToChar(ProcLine(nX)) + ")" + CRLF
		nX++
	EndDo

	// Lista linhas corrigidas.
	For nX := 1 to len(aLogSB8)
		SB8->(dbGoTo(aLogSB8[nX, 1]))
		cMsg += cAgora + "     - Lote " + SB8->(B8_FILIAL + B8_PRODUTO + B8_LOCAL + dtos(B8_DTVALID) + B8_LOTECTL + B8_NUMLOTE) + CRLF
		cMsg += cAgora + "       -  Empenho  " + cValToChar(aLogSB8[nX, 2]) + " (calculada " + cValToChar(aLogSB8[nX, 3]) + ")" + CRLF
	Next nX

	// Grava log de erro no arquivo.
	nHlLog := fOpen(cSB8Log, FO_READWRITE)  // Vai para o final do arquivo.
	If nHlLog > 0
		fSeek(nHlLog, 0, FS_END)  // Vai para o fim do arquivo.
		fWrite(nHlLog, cMsg, len(cMsg))
		fClose(nHlLog)
	Endif
Endif
U_DebugMsg("Acerto de SB8 ok.")

// *****************************************************************
// Acerta os itens com erro de quantidade liberada.
// *****************************************************************
BeginSql Alias cAliasSQL
	%noparser%
	select a.*
	from (
		select SC6.R_E_C_N_O_ SC6RecNo, SC6.C6_QTDEMP, SC6.C6_QTDENT,
		isnull((
				select sum(C9_QTDLIB) C9Count
				from %Table:SC9% SC9 with (noLock)
				where SC9.%NotDel%
				and SC9.C9_FILIAL  = %xFilial:SC9%
				and SC9.C9_PEDIDO  = SC6.C6_NUM
				and SC9.C9_ITEM    = SC6.C6_ITEM
				and SC9.C9_NFISCAL = ' '
		), 0) C9QTDEMP,
		isnull((
				select sum(C9_QTDLIB) C9Count
				from %Table:SC9% SC9 with (noLock)
				where SC9.%NotDel%
				and SC9.C9_FILIAL  = %xFilial:SC9%
				and SC9.C9_PEDIDO  = SC6.C6_NUM
				and SC9.C9_ITEM    = SC6.C6_ITEM
				and SC9.C9_NFISCAL <> ' '
		), 0) C9QTDENT

		from %Table:SC6% SC6 with (noLock)

		left join %Table:SF4% SF4 with (noLock) on SF4.%NotDel%
		and SF4.F4_FILIAL = %xFilial:SF4%
		and SF4.F4_CODIGO = SC6.C6_TES
		and SF4.F4_ESTOQUE = 'S'

		where SC6.%NotDel%
		and SC6.C6_FILIAL  = %xFilial:SC6%
		and SC6.C6_NUM     = %Exp:cPedido%
	) a
	where SC6RecNo > 0
	and (
		   a.C6_QTDEMP <> isnull(a.C9QTDEMP, 0)
		or a.C6_QTDENT <> isnull(a.C9QTDENT, 0)
	)
EndSql

// Acerta a reserva do produto, se necessário.
Do While (cAliasSQL)->(!eof())

	// Corrige o problema.
	SC6->(dbGoTo((cAliasSQL)->SC6RecNo))
	RecLock("SC6", .F.)
	SC6->C6_QTDEMP := (cAliasSQL)->C9QTDEMP
	SC6->C6_QTDENT := (cAliasSQL)->C9QTDENT
	SC6->(msUnLock())

	// Adiciona o produto no log de erro.
	(cAliasSQL)->(aAdd(aLogSC6, {SC6RecNo, C6_QTDEMP, C6_QTDENT, C9QTDEMP, C9QTDENT}))

	// Próximo registro.
	(cAliasSQL)->(dbSkip())
EndDo
(cAliasSQL)->(dbCloseArea())

// Monta log SC6.
If !empty(aLogSC6)
	// Cria o arquivo se não existir.
	Static cSC6Log := "\SC6Prob.log"
	cAgora := dtoc(date()) + " " + time() + " " + cUserName +  " - "
	If !file(cSC6Log)
		MemoWrite(cSC6Log, cAgora + "Arquivo criado." + CRLF)
	Endif

	// Exibe mensagem no console.
	cMsg := CRLF + cAgora + " - Erro no SC6 - Pedido " + cFilAnt + "/" + cPedido + CRLF

	// Monta pilha de chamadas.
	nX := 1
	Do While !empty(ProcName(nX)) .and. nX < 20
		cMsg += cAgora + "     - Função " + ProcName(nX) + " (linha " + cValToChar(ProcLine(nX)) + ")" + CRLF
		nX++
	EndDo

	// Lista linhas corrigidas.
	For nX := 1 to len(aLogSC6)
		SC6->(dbGoTo(aLogSC6[nX, 1]))
		cMsg += cAgora + "     - Pedido " + SC6->(C6_FILIAL + " " + C6_NUM + "/" + C6_ITEM + " - " + C6_PRODUTO) + CRLF
		cMsg += cAgora + "       -  Qtde empenhada " + cValToChar(aLogSC6[nX, 2]) + " (calculada " + cValToChar(aLogSC6[nX, 4]) + ")" + CRLF
		cMsg += cAgora + "       -  Qtde entregue  " + cValToChar(aLogSC6[nX, 3]) + " (calculada " + cValToChar(aLogSC6[nX, 5]) + ")" + CRLF
	Next nX

	// Grava log de erro no arquivo.
	nHlLog := fOpen(cSC6Log, FO_READWRITE)  // Vai para o final do arquivo.
	If nHlLog > 0
		fSeek(nHlLog, 0, FS_END)  // Vai para o fim do arquivo.
		fWrite(nHlLog, cMsg, len(cMsg))
		fClose(nHlLog)
	Endif
Endif
U_DebugMsg("Acerto de SC6 ok.")
U_DebugMsg("Fim do U_VerB2Res()", "F")

// Restaura areas de trabalho.
(cAlias)->(sRestArea(aArea))
dbSelectArea(cAlias)

Return


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Mauro Sano
// Modulo   : Faturamento / Call Center
// Função   : ProManOP
// Descrição: Geração e baixa automática de OPs.
// Retorno  : Nenhum
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 18/05/13 | Mauro Sano        | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function ProManOP(cPedido)
Local cAlias     := Alias()
Local aArea      := {}

Local nRecNoSC6  := 0
Local cNumOp     := ""
Local cItemOp    := ""
Local cSeqOP     := StrZero(1, TamSX3("C2_SEQUEN")[1])
Local aAutoOP    := {}
Local nNecess    := 0

Local cPrfLot    := AllTrim(SuperGetMV("PC_PRFLOTN",, "LOTE"))
Local nTamLot    := TamSx3("B8_LOTECTL")[1]

Private lMsErroAuto := .F.
Default cPedido     := SC5->C5_NUM

// Salva áreas de trabalho.
If empty(cAlias)
	cAlias := "SX3"
	dbSelectArea(cAlias)
Endif
aArea := sGetArea()
sGetArea(aArea, "SC5")
sGetArea(aArea, "SC6")

SC6->(dbSetOrder(1))  // C6_FILIAL, C6_NUM, C6_ITEM, C6_PRODUTO.
If SC6->(dbSeek(xFilial() + cPedido, .F.))
	Do While SC6->(!eof() .and. C6_FILIAL + C6_NUM == xFilial() + cPedido) .and. !lMsErroAuto

		nRecNoSC6 := SC6->(Recno())
		SB1->(dbSetOrder(1))  // B1_FILIAL, B1_COD.
		If SB1->(msSeek(xFilial() + SC6->C6_PRODUTO, .F.) .and. B1_XGEROP == "S")

			// Necessidade a ser produzida.
			nNecess := SC6->C6_QTDVEN

			// Verifica se o pedido já possui quantidade reservada.
			SC9->(dbSetOrder(1))  // C9_FILIAL, C9_PEDIDO, C9_ITEM, C9_SEQUEN, C9_PRODUTO.
			SC9->(dbSeek(xFilial() + SC6->(C6_NUM + C6_ITEM), .F.))
			Do While SC9->(!eof() .and. C9_FILIAL + C9_PEDIDO + C9_ITEM == xFilial() + SC6->(C6_NUM + C6_ITEM))
				If SC9->(empty(C9_BLCRED) .and. empty(C9_BLEST) .and. empty(C9_NFISCAL))
					nNecess -= SC9->C9_QTDLIB
				Endif

				// Próximo item.
				SC9->(dbSkip())
			EndDo

			// Verifica se o produto possui estoque disponível antes de produzir.
			If nNecess > 0
				SB2->(dbSetOrder(1))  // B2_FILIAL, B2_COD, B2_LOCAL.
				If SB2->(msSeek(xFilial() + SC6->(C6_PRODUTO + C6_LOCAL), .F.))
					nNecess -= SaldoSB2()
				Endif
			Endif

			If nNecess > 0
				// Verifica se existe OP gerada e já produzida para esse pedido.
				SC2->(dbSetOrder(1))  // C2_FILIAL, C2_NUM, C2_ITEM, C2_SEQUEN, C2_ITEMGRD.
				If !empty(SC6->C6_NUMOP) .and. SC2->(dbSeek(xFilial() + SC6->(C6_NUMOP + C6_ITEMOP) + cSeqOP, .F.) .and. !empty(SC2->C2_DATRF))
					// Se a OP foi baixada, porém não existe saldo suficiente.
					If !U_EstPV(.F., SC6->C6_NUM, SC6->C6_ITEM)
						// Limpa campos pra gerar nova OP.
						RecLock("SC6", .F.)
						SC6->C6_NUMOP  := ""
						SC6->C6_ITEMOP := ""
						SC6->(msUnLock())
					Endif
				Endif

				// Gera OP.
				SC2->(dbSetOrder(1))  // C2_FILIAL, C2_NUM, C2_ITEM, C2_SEQUEN, C2_ITEMGRD.
				If !empty(SC6->C6_NUMOP) .and. SC2->(dbSeek(xFilial() + SC6->(C6_NUMOP + C6_ITEMOP) + cSeqOP, .F.) .and. C2_PRODUTO == SC6->C6_PRODUTO .and. C2_QUANT == SC6->C6_QTDVEN)
					cNumOp  := SC6->C6_NUMOP
					cItemOp := SC6->C6_ITEMOP
				Else
					cNumOp  := GetNumSC2()
					cItemOp := StrZero(1, TamSX3("C2_ITEM")[1])
					aAutoOP := {}
					aAdd(aAutoOP, {'C2_NUM',     cNumOp, nil})
					aAdd(aAutoOP, {'C2_ITEM',    cItemOp, nil})
					aAdd(aAutoOP, {'C2_SEQUEN',  cSeqOP, nil})
					aAdd(aAutoOP, {'C2_PRODUTO', SC6->C6_PRODUTO, nil})
					aAdd(aAutoOP, {'C2_LOCAL',   SC6->C6_LOCAL, nil})
					aAdd(aAutoOP, {'C2_QUANT',   nNecess, nil})
					aAdd(aAutoOP, {'C2_UM',      SB1->B1_UM, nil})
					aAdd(aAutoOP, {'C2_DATPRI',  dDatabase, nil})
					aAdd(aAutoOP, {'C2_DATPRF',  dDatabase, nil})
					aAdd(aAutoOP, {'C2_REVI',    ' ', nil})
					aAdd(aAutoOP, {'C2_TPOP',    'F', nil})
					aAdd(aAutoOP, {'AUTEXPLODE', 'S', nil})

					msExecAuto({|x, y| MATA650(x, y)}, aAutoOP, 3)
					U_DebugMsg("Pedido " + SC5->C5_NUM + " - OP gerada " + cNumOp + cItemOp)

					If lMsErroAuto
						MostraErro()
						If __lSX8
							RollBackSX8()
						Endif
					Else
						If __lSX8
							ConfirmSX8()
						Endif

						SC6->(dbGoTo(nRecNoSC6))
						RecLock("SC6", .F.)
						SC6->C6_NUMOP  := cNumOp
						SC6->C6_ITEMOP := cItemOp
						SC6->(msUnlock())
					Endif
				Endif

				// Baixa a OP.
				If !lMsErroAuto
					SC2->(dbSetOrder(1))  // C2_FILIAL, C2_NUM, C2_ITEM, C2_SEQUEN, C2_ITEMGRD.
					If SC2->(dbSeek(xFilial() + SC6->(C6_NUMOP + C6_ITEMOP) + cSeqOP, .F.) .and. empty(SC2->C2_DATRF))
						aAutoOP := {}
						aAdd(aAutoOP, {'AUTPRTOTAL', 'S', nil})
						aAdd(aAutoOP, {'D3_OP',      cNumop + cItemOp + cSeqOP, nil})
						aAdd(aAutoOP, {'D3_COD',     SC2->C2_PRODUTO, nil})
						aAdd(aAutoOP, {'D3_DOC',     SC6->C6_NUM, nil})
						aAdd(aAutoOP, {'D3_QUANT',   SC2->C2_QUANT, nil})
						aAdd(aAutoOP, {'D3_TM',      '011', nil})
						aAdd(aAutoOP, {'D3_LOCAL',   SC2->C2_LOCAL, nil})
						aAdd(aAutoOP, {'D3_EMISSAO', dDatabase, nil})
						aAdd(aAutoOP, {'D3_UM',      SB1->B1_UM, nil})
						aAdd(aAutoOP, {'D3_TIPO',    SB1->B1_TIPO, nil})
						aAdd(aAutoOP, {'D3_CF',      'PR0', nil})
						aAdd(aAutoOP, {'D3_CHAVE',   'R0', nil})
						aAdd(aAutoOP, {'D3_XTIPO',   'G', nil})
						aAdd(aAutoOP, {'D3_PARCTOT', 'T', nil})
						If Rastro(SC2->C2_PRODUTO,"S") .OR. Rastro(SC2->C2_PRODUTO,"L")
							aAdd(aAutoOP, {'D3_LOTECTL', cPrfLot + StrZero(1, nTamLot - len(cPrfLot)), nil})
						EndIf
						aAdd(aAutoOP, {'D3_USUARIO', cUserName, nil})

						msExecAuto({|x, y| MATA250(x, y)}, aAutoOP, 3)
						U_DebugMsg("Pedido " + SC5->C5_NUM + " - OP produzida " + cNumOp + cItemOp)

						If lMsErroAuto
							MostraErro()
							If __lSX8
								RollBackSX8()
							Endif
						Else
							If __lSX8
								ConfirmSX8()
							Endif
						Endif
					Endif
				Endif
			Endif
		Endif

		SC6->(dbGoTo(nRecNoSC6))
		SC6->(dbSkip())
	EndDo
Endif

// Restaura areas de trabalho.
(cAlias)->(sRestArea(aArea))
dbSelectArea(cAlias)

Return !lMsErroAuto


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : ConPDSA1
// Descrição:
// Retorno  : Lógico, indicando se o usuário confirmou a tela.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 21/02/14 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function ConPdSA1()
Local lRet       := .F.
Local nLin, nCol

Local oDlgBusca, oGrBusca, oGrRes, oGrCli, oGrCon, oGrBut

Local oGetFil
Local cFilCli    := "1"

Local oGdCli, oGdCon

Local oGetOrdem
Local cOrdem     := ""
Local aComboText := {}
Local cChave     := Space(200)
Local lIncBlq    := .F.
Local bBusca     := {|| msAguarde({|| BuscaSA1(oGdCli, lBusProsp, cFilCli, cOrdem, cChave, lIncBlq)}, "Aguarde", "Pesquisando cliente/contato...", .F.), oGdCli:SetFocus(), .T.}
Local lBusProsp  := .T.

Local bProspect
Local bPosTab    := {|aTab| lProspect := (aTab[1] == "SUS"), If(lProspect, SUS->(dbGoTo(aTab[2])), SA1->(dbGoTo(aTab[2])))}
Local bOK        := {|| eval(bPosTab, oGdCli:Cargo[oGdCli:nAt]), lRet := .T., oDlgBusca:End()}
Local bCancel    := {|| lRet := .F., oDlgBusca:End()}

// Variáveis do usuário logado no sistema.
Static cCodSU7    := ""
Static cTpCart    := ""

// Configuração do usuário.
If empty(cCodSU7)
	SU7->(dbSetOrder(4))  // U7_FILIAL, U7_CODUSU.
	If SU7->(dbSeek(xFilial() + __cUserId, .F.))
		cCodSU7   := SU7->U7_COD
		cTpCart   := TkPosto(SU7->U7_COD, "U0_XCART")
	Endif
Endif

// Cria a variável lProspect, se não existir.
If Type("lProspect") <> "L"
	If IsInCallStack(FunName())
		_SetNamedPrvt("lProspect", .F., FunName())
	ElseIf IsInCallStack("U_" + FunName())
		_SetNamedPrvt("lProspect", .F., "U_" + FunName())
	Endif
	lBusProsp := .F.
Endif

If Type("lProspect") <> "L"
	MsgAlert("Variável privada lProspect não encontrada. Favor entrar em contato com o administrador do sistema.", "Atenção")
Else
	// Monta tela de entrada.
	DEFINE MSDIALOG oDlgBusca TITLE "Busca de clientes" FROM 0, 0 TO 400, 1050 PIXEL Style DS_MODALFRAME

	// Grupo de dados da busca.
	oGrBusca := TScrollArea():New(oDlgBusca, 0, 0, 30, 0, .F., .F.)
	oGrBusca:Align := CONTROL_ALIGN_TOP

	nLin := 002; nCol := 002
	bBloco     := {|x| If(ValType(x) <> 'U', (cOrdem := x, lIncBlq := (cOrdem = '2'), cOrdem), cOrdem)}
	aComboText := {"1=Nome", "2=CNPJ/CPF", "3=E-mail", "4=Contato", "5=E-mail de contato", "6=Código Grupo Cliente", "7=Grupo Cliente", "8=Centro de Custo"}
	oGetOrdem  := TComboBox():New(nLin, nCol, bBloco, aComboText, 62, 12, oGrBusca, 0,, /*bValid*/, /*CLR_DEFAULT*/, /*CLR_DEFAULT*/, .T.)
	@ nLin, nCol + 63 MSGET cChave SIZE 355, 10 of oGrBusca VALID Eval(bBusca) PIXEL

	nLin += 13
	bBloco     := {|x| If(ValType(x) <> 'U', cFilCli := x, cFilCli)}
	aComboText := {"1=Somente " + FWFilialName(nil, cFilAnt, 1), "2=Todas filiais"}
	oGetFil    := TComboBox():New(nLin, nCol, bBloco, aComboText, 90, 12, oGrBusca, 0,, bBusca, /*CLR_DEFAULT*/, /*CLR_DEFAULT*/, .T.,,,, {|| cTpCart <> "1"})
	nCol += 360

	// Busca clientes bloqueados.
	TCheckBox():New(nLin, nCol, ' ', {|u| If(ValType(u) = "U", lIncBlq, (lIncBlq := u, Eval(bBusca), lIncBlq))}, oGrBusca, 7, 21,,,,,,,, .T.)
	@ nLin, nCol + 10 SAY "Busca bloqueados" OF oGrBusca PIXEL

	// Grupo de dados dos clientes / contatos.
	oGrRes := TScrollArea():New(oDlgBusca, 0, 0, 0, 0, .F., .F.)
	oGrRes:Align := CONTROL_ALIGN_ALLCLIENT

	// Grupo de dados dos clientes.
	oGrCli := TScrollArea():New(oGrRes, 0, 0, 0, 0, .F., .F.)
	oGrCli:Align := CONTROL_ALIGN_ALLCLIENT

	// Grid dos clientes.
	aCabec := {"Filial", "Status", "Situação", "Grupo", "Descr.Grupo", "Código", "Loja", "C.Custo", "Est", "Municipio", "CNPJ", "Nome"}
	oGdCli := TWBrowse():New(0, 0, 0, 0,, aCabec, {15, 30, 30, 25, 60, 25, 20, 60, 20, 60, 60, 100}, oGrCli)
	oGdCli:Align := CONTROL_ALIGN_ALLCLIENT
	oGdCli:bChange    := {|| BuscaSU5(oGdCon, oGdCli:Cargo[oGdCli:nAt])}
	oGdCli:blDblClick := bOK

	// Grupo de dados dos contatos.
	oGrCon := TScrollArea():New(oGrRes, 0, 0, 70, 0, .F., .F.)
	oGrCon:Align := CONTROL_ALIGN_BOTTOM

	// Grid dos contatos.
	aCabec := {"Contato", "Nome", "Função", "Tel", "E-Mail", "Observação"}
	oGdCon := TWBrowse():New(0, 0, 0, 0,, aCabec, {30, 70, 70, 40, 100, 200}, oGrCon)
	oGdCon:Align := CONTROL_ALIGN_ALLCLIENT
	oGdCon:blDblClick := bOK

	// Grupo dos botões.
	oGrBut := TScrollArea():New(oDlgBusca, 0, 0, 15, 0, .F., .F.)
	oGrBut:Align := CONTROL_ALIGN_BOTTOM

	If lBusProsp
		bProspect := {|| If(FWExecView("Prospect", "VIEWDEF.TMKA260", 3, /*oDlg*/, {|| .T.}, /*bOk*/, /*nPercReducao*/) = 0, Eval(bBusca), nil)}
		tButton():New(2, 002, "Prospect",  oGrBut, bProspect, 35, 10,,,, .T.)
	Endif

	tButton():New(2, 349, "Confirmar", oGrBut, bOK,       35, 10,,,, .T.)
	tButton():New(2, 386, "Cancelar",  oGrBut, bCancel,   35, 10,,,, .T.)

	// Atualiza lista de clientes/contatos.
	BuscaSA1(oGdCli, lBusProsp, cFilCli, cOrdem, cChave)

	ACTIVATE MSDIALOG oDlgBusca centered
	dbSelectArea("SA1")
Endif

Return lRet


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : BuscaSA1
// Descrição:
// Retorno  : Lógico, indicando se o processamento foi executado com sucesso.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 21/02/14 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
Static Function BuscaSA1(oGdCli, lBusProsp, cFilCli, cOrdem, cChave, lIncBlq)
Local lRet       := .T.
Local aResult    := {}
Local aTABRecNo  := {}
Local cStatus    := ""

Local cQuery     := ""
Local cAliasTop  := ""

If !empty(cChave)
	// Busca cliente.
	cQuery := "select a.* " + CRLF
	cQuery += "from ( " + CRLF
	cQuery += "  select 'SA1' TAB, SA1.R_E_C_N_O_ TABRecNo, isnull(ZA7.ZA7_FILIAL, SA1.A1_XEMPRE) EMPRESA, " + CRLF
	cQuery += "  SA1.A1_FILIAL FILIAL, SA1.A1_COD CODIGO, SA1.A1_LOJA LOJA, SA1.A1_CGC CNPJ, SA1.A1_NOME NOME, SA1.A1_EMAIL EMAIL " + CRLF
	cQuery += "  ,ISNULL(ACY.ACY_GRPVEN,' ') AS CODGRP "+ CRLF
	cQuery += "  ,ISNULL(ACY.ACY_DESCRI,' ') AS DESCGRP "+ CRLF
	cQuery += ",ISNULL(RTRIM(ZW_CHVEDI),' ') AS CHVEDI " + CRLF

	cQuery += "  from " + RetSQLName("SA1") + " SA1 with (noLock) " + CRLF

	cQuery += "	LEFT JOIN " + RetSQLName("SZW") + " SZW WITH(NOLOCK) " + CRLF
	cQuery += "		ON SZW.D_E_L_E_T_ = '' " + CRLF
	cQuery += "		AND ZW_FILIAL = '" + xFilial("SZW") + "' " + CRLF
	cQuery += "		AND ZW_CLIENTE = A1_COD " + CRLF
	cQuery += "		AND ZW_LOJA = A1_LOJA " + CRLF

	// Filtra a empresa.
	If cFilCli = "1" .or. cTpCart == "1"  // cTpCart -> 1=Restrito.
		cQuery += "  inner join " + RetSQLName("ZA7") + " ZA7 with (noLock) " + CRLF
	Else
		cQuery += "  left  join " + RetSQLName("ZA7") + " ZA7 with (noLock) " + CRLF
	Endif
	cQuery += "  on ZA7.D_E_L_E_T_ = ' '  " + CRLF
	cQuery += "  and ZA7.ZA7_FILIAL = '" + xFilial("ZA7") + "' " + CRLF
	cQuery += "  and ZA7.ZA7_CLIENT = SA1.A1_COD " + CRLF
	cQuery += "  and ZA7.ZA7_LOJA   = SA1.A1_LOJA " + CRLF
	cQuery += "  and ZA7.ZA7_DEPVEN IN ('01','02') " + CRLF
	If cTpCart == "1"  // 1-Restrito.
		cQuery += "  and ZA7.ZA7_OPERAD = '" + cCodSU7 + "' " + CRLF
	Endif

	cQuery += "  left join " + RetSQLName("ACY") + " ACY with (noLock) " + CRLF
	cQuery += "  on ACY.D_E_L_E_T_ = ' '  " + CRLF
	cQuery += "  and ACY.ACY_FILIAL = '" + xFilial("ACY") + "' " + CRLF
	cQuery += "  and ACY.ACY_GRPVEN = SA1.A1_GRPVEN " + CRLF

	cQuery += "  where SA1.D_E_L_E_T_ = ' ' " + CRLF
	cQuery += "  and SA1.A1_FILIAL  = '" + xFilial("SA1") + "' " + CRLF
	If cOrdem <> "2" .and. !lIncBlq  // 2=CNPJ/CPF.
		cQuery += "  and SA1.A1_MSBLQL  <> '1' " + CRLF
	Endif

	// Busca prospect também.
	If lBusProsp .And. cOrdem != "8"
		cQuery += "  union " + CRLF

		cQuery += "  select 'SUS' TAB, SUS.R_E_C_N_O_ TABRecNo, SUS.US_MSFIL EMPRESA, " + CRLF
		cQuery += "  SUS.US_FILIAL FILIAL, SUS.US_COD, SUS.US_LOJA, SUS.US_CGC, SUS.US_NOME, SUS.US_EMAIL " + CRLF
		cQuery += "  ,' ' AS CODGRP "+ CRLF
		cQuery += "  ,' ' AS DESCGRP "+ CRLF
		cQuery += "  ,' ' AS CHVEDI "+ CRLF
		cQuery += "  from " + RetSQLName("SUS") + " SUS with (noLock) " + CRLF
		cQuery += "  where SUS.D_E_L_E_T_ = ' ' " + CRLF
		cQuery += "  and SUS.US_FILIAL  = '" + xFilial("SUS") + "' " + CRLF
		cQuery += "  and SUS.US_CODCLI  = '' " + CRLF  // Não lista prospects convertidos em cliente.
		If cOrdem <> "2" .and. !lIncBlq  // 2=CNPJ/CPF.
			cQuery += "  and SUS.US_MSBLQL  <> '1' " + CRLF
		Endif
	Endif
	cQuery += ") a " + CRLF
	cQuery += "where a.TABRecNo > 0 " + CRLF

	cChave := StrTran(cChave, "'", "''")
	If cOrdem == "1"  // 1=Nome.
		cQuery += "and upper(a.NOME) like '" + AllTrim(upper(cChave)) +  "%' " + CRLF
		cQuery += "order by a.NOME, a.CODIGO, a.LOJA " + CRLF
	ElseIf cOrdem == "2"  // 2=CNPJ/CPF.
		cChave := StrTran(cChave, ".", "")
		cChave := StrTran(cChave, "/", "")
		cChave := StrTran(cChave, "-", "")
		cQuery += "and a.CNPJ like '" + AllTrim(cChave) +  "%' " + CRLF
		cQuery += "order by a.CNPJ, a.CODIGO, a.LOJA " + CRLF
	ElseIf cOrdem == "3"  // 3=E-mail.
		cQuery += "and upper(a.EMAIL) like '" + AllTrim(upper(cChave)) + "%' " + CRLF
		cQuery += "order by a.EMAIL, a.CODIGO, a.LOJA " + CRLF
	ElseIf cOrdem == "4" .Or. cOrdem == "5" // 4=Contato. // 5=E-mail do contato.
		cQuery += "and exists ( " + CRLF
		cQuery += "  select top 1 null " + CRLF
		cQuery += "  from " + RetSQLName("AC8") + " AC8 with (noLock) " + CRLF
		cQuery += "  inner join " + RetSQLName("SU5") + " SU5 with (noLock) on SU5.D_E_L_E_T_ = ' ' " + CRLF
		cQuery += "  and SU5.U5_FILIAL  = '" + xFilial("SU5") + "' " + CRLF
		cQuery += "  and SU5.U5_CODCONT = AC8.AC8_CODCON " + CRLF
		cQuery += "  where AC8.D_E_L_E_T_ = ' ' " + CRLF
		cQuery += "  and AC8.AC8_FILIAL = '" + xFilial("AC8") + "' " + CRLF
		cQuery += "  and AC8.AC8_ENTIDA = a.TAB " + CRLF
		cQuery += "  and AC8.AC8_FILENT = a.FILIAL " + CRLF
		cQuery += "  and AC8.AC8_CODENT = a.CODIGO + a.LOJA " + CRLF
		If cOrdem == "4"  // 4=Contato.
			cQuery += "  and upper(SU5.U5_CONTAT) like '" + AllTrim(upper(cChave)) + "%' " + CRLF
		ElseIf cOrdem == "5"  // 5=E-mail do contato.
			cQuery += "  and upper(SU5.U5_EMAIL)  like '" + AllTrim(upper(cChave)) + "%' " + CRLF
		Endif
		cQuery += ") " + CRLF
		cQuery += "order by a.CODIGO, a.LOJA " + CRLF
	ElseIf cOrdem == "6" // 6=Codigo Grupo Cliente
		cQuery += "and a.CODGRP = '" + AllTrim(cChave) + "' " + CRLF
		cQuery += "order by a.CODGRP, a.DESCGRP, a.CODIGO, a.LOJA " + CRLF
	ElseIf cOrdem == "7" // 7=Grupo Cliente
		cQuery += "and upper(a.DESCGRP) like '" + AllTrim(upper(cChave)) +  "%' " + CRLF
		cQuery += "order by a.DESCGRP, a.CODGRP, a.CODIGO, a.LOJA " + CRLF
	ElseIf cOrdem == "8"
		cQuery += "	AND UPPER(a.CHVEDI) LIKE '" + AllTrim(Upper(cChave)) +  "%' " + CRLF
		cQuery += "order by a.CODIGO, a.LOJA, a.CHVEDI " + CRLF
	Endif

	cAliasTop := MPSysOpenQuery(cQuery)
	Do While (cAliasTop)->(!eof())
		// Armazena posição da tabela.
		aAdd(aTABRecNo, {(cAliasTop)->TAB, (cAliasTop)->TABRecNo})

		// Adiciona os resultados na tela.
		If (cAliasTop)->TAB == "SA1"
			SA1->(dbGoTo((cAliasTop)->TABRecNo))

			// Outros dados do cliente.
			DO CASE
				CASE SA1->A1_MSBLQL == "1" // Bloqueado -> 1-Sim / 2-Não.
					cSituacao := "BLOQUEADO"
				CASE SA1->A1_RISCO == "E"
					cSituacao := "SUSPENSO"
				OTHERWISE
					cSituacao := "LIBERADO"
			ENDCASE

			DO CASE
				CASE SA1->A1_ULTCOM = SToD("")
					cStatus := "NOVO"
				CASE SA1->A1_ULTCOM >= (dDatabase - nCliAtiv)
					cStatus := "ATIVO"
				CASE SA1->A1_ULTCOM < (dDatabase - nCliAtiv) .And. SA1->A1_ULTCOM >= (dDatabase - nCliDorm)
					cStatus := "INATIVO"
				OTHERWISE
					cStatus := "DORMENTE"
			ENDCASE

			SA1->(aAdd(aResult, {(cAliasTop)->EMPRESA, cStatus, cSituacao, (cAliasTop)->CODGRP, (cAliasTop)->DESCGRP, A1_COD, A1_LOJA, (cAliasTop)->CHVEDI,A1_EST, A1_MUN, StrTran(Transform(A1_CGC, PicPes(A1_PESSOA)), "%C", ""), A1_NOME}))
		Else
			SUS->(dbGoTo((cAliasTop)->TABRecNo))
			SUS->(aAdd(aResult, {(cAliasTop)->EMPRESA, "Prospect", "Liberado", "", "", US_COD, US_LOJA, "", US_EST, US_MUN, StrTran(Transform(US_CGC, PicPes("J")), "%C", ""), US_NOME}))
		Endif

		// Próximo item.
		(cAliasTop)->(dbSkip())
	EndDo
	(cAliasTop)->(dbCloseArea())
Endif

// Atualiza a tela.
If empty(aResult)
	aAdd(aTABRecNo, {"", 0})
	aAdd(aResult, {cFilAnt, "", "", "", "", "", "", "", "", "", "", ""})
Endif
oGdCli:Cargo := aTABRecNo
oGdCli:SetArray(aResult)
oGdCli:bLine := {|| aResult[oGdCli:nAt]}
oGdCli:nAt   := 1
Eval(oGdCli:bChange)
oGdCli:Refresh()

Return lRet


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : BuscaSU5
// Descrição:
// Retorno  : Lógico, indicando se o processamento foi executado com sucesso.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 21/02/14 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
Static Function BuscaSU5(oGdCon, aChave)
Local lRet       := .T.
Local aResult    := {}

Local cTabOri    := aChave[1]
Local nRecNo     := aChave[2]

Local cQuery     := ""
Local cAliasTop  := ""

// Busca contatos.
If nRecNo > 0
	cQuery := "select SU5.R_E_C_N_O_ TABRecNo "
	cQuery += "from " + RetSQLName("AC8") + " AC8 with (noLock) "
	cQuery += "inner join " + RetSQLName("SU5") + " SU5 with (noLock) on SU5.D_E_L_E_T_ = ' ' "
	cQuery += "and SU5.U5_FILIAL  = '" + xFilial("SU5") + "' "
	cQuery += "and SU5.U5_CODCONT = AC8.AC8_CODCON "
	cQuery += "where AC8.D_E_L_E_T_ = ' ' "
	cQuery += "and AC8.AC8_FILIAL = '" + xFilial("AC8") + "' "
	cQuery += "and AC8.AC8_ENTIDA = '" + cTabOri + "' "
	cQuery += "and AC8.AC8_FILENT = '" + xFilial(cTabOri) + "' "
	If cTabOri == "SA1"
		SA1->(dbGoTo(nRecNo))
		cQuery += "and AC8.AC8_CODENT = '" + SA1->(A1_COD + A1_LOJA) + "' "
	Else
		SUS->(dbGoTo(nRecNo))
		cQuery += "and AC8.AC8_CODENT = '" + SUS->(US_COD + US_LOJA) + "' "
	Endif
	cQuery += "order by AC8.AC8_FILIAL, AC8.AC8_CODCON "

	cAliasTop := MPSysOpenQuery(cQuery)
	Do While (cAliasTop)->(!eof())

		SU5->(dbGoTo((cAliasTop)->TABRecNo))
		aAdd(aResult, {SU5->U5_CODCONT, AllTrim(SU5->U5_CONTAT), AllTrim(Posicione("SUM", 1, xFilial("SUM") + SU5->U5_FUNCAO, "UM_DESC")),;
		If(empty(SU5->U5_DDD), "", "(" + SU5->U5_DDD + ") ") + AllTrim(SU5->U5_FCOM1), lower(AllTrim(SU5->U5_EMAIL)), AllTrim(SU5->U5_OBS)})

		// Próximo item.
		(cAliasTop)->(dbSkip())
	EndDo
	(cAliasTop)->(dbCloseArea())
Endif

// Atualiza a tela.
If empty(aResult)
	aAdd(aResult, {"", "", "", "", "", ""})
Endif
oGdCon:SetArray(aResult)
oGdCon:bLine := {|| aResult[oGdCon:nAt]}
oGdCon:nAt   := 1
oGdCon:Refresh()

Return lRet


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : ConPDSA4
// Descrição:
// Retorno  : Lógico, indicando se o usuário confirmou a tela.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 24/02/14 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function ConPdSA4()
Local lRet       := .F.
Local nLin, nCol

Local oDlgBusca
Local oGrBusca
Local oGrCli
Local oGrBut

Local oGdTrp

Local oGetOrdem, oGetBusca
Local cOrdem     := "1"
Local aComboText := {"1=CNPJ/CPF", "2=Nome", "3=E-mail"}
Local cChave     := Space(200)
Local bBusca     := {|| msAguarde({|| BuscaSA4(oGdTrp, cOrdem, cChave)}, "Aguarde", "Pesquisando transportadora...", .F.), If(empty(cChave), oGetBusca:SetFocus(), oGdTrp:SetFocus()), .T.}

Local bOK        := {|| nRec := oGdTrp:Cargo[oGdTrp:nAt], If(nRec > 0, (SA4->(dbGoTo(nRec)), lRet := .T., oDlgBusca:End()), nil)}
Local bCancel    := {|| lRet := .F., oDlgBusca:End()}

// Monta tela de entrada.
DEFINE MSDIALOG oDlgBusca TITLE "Busca de transportadora" FROM 0, 0 TO 400, 840 PIXEL Style DS_MODALFRAME

// Grupo de dados da busca.
oGrBusca := TScrollArea():New(oDlgBusca, 0, 0, 18, 0, .F., .F.)
oGrBusca:Align := CONTROL_ALIGN_TOP

nLin := 002; nCol := 002
bBloco     := {|x| If(ValType(x) <> 'U', cOrdem := x, cOrdem)}
aComboText := {"1=Nome", "2=CNPJ/CPF", "3=E-mail", "4=Própria"}
oGetOrdem  := TComboBox():New(nLin,nCol,bBloco,aComboText, 60,/*nAuxHeight*/,oGrBusca,,, bBusca,/*CLR_DEFAULT*/,/*CLR_DEFAULT*/,.T.)
@ nLin, nCol + 62 MSGET oGetBusca VAR cChave SIZE 354, 010 of oGrBusca VALID Eval(bBusca) PIXEL

// Grupo de dados da transportadora.
oGrCli := TScrollArea():New(oDlgBusca, 0, 0, 0, 0, .F., .F.)
oGrCli:Align := CONTROL_ALIGN_ALLCLIENT

// Grid das transportadoras.
aCabec := {"Código", "Nome", "Município", "UF", "CNPJ", "E-mail", "Nome"}
oGdTrp := TWBrowse():New(0, 0, 0, 0,, aCabec, {25, 80, 35, 20, 60, 100, 100}, oGrCli)
oGdTrp:Align := CONTROL_ALIGN_ALLCLIENT
oGdTrp:blDblClick := bOK

// Grupo dos botões.
oGrBut := TScrollArea():New(oDlgBusca, 0, 0, 15, 0, .F., .F.)
oGrBut:Align := CONTROL_ALIGN_BOTTOM

tButton():New(2, 349, "Confirmar", oGrBut, bOK,     35, 10,,,, .T.)
tButton():New(2, 386, "Cancelar",  oGrBut, bCancel, 35, 10,,,, .T.)

// Atualiza lista de clientes/contatos.
BuscaSA4(oGdTrp, cOrdem, cChave)
oGetBusca:SetFocus()

ACTIVATE MSDIALOG oDlgBusca centered
dbSelectArea("SA4")

Return lRet


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : BuscaSA4
// Descrição:
// Retorno  : Lógico, indicando se o usuário confirmou a tela.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 21/02/14 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
Static Function BuscaSA4(oGdTrp, cOrdem, cChave)
Local lRet      := .T.
Local aResult   := {}
Local aTABRecNo := {}
Local lProp     := (cOrdem = "4")
Local cQuery    := ""
Local cAliasTop := ""
Local lRedesp   := M->UA_TPFRETE == "F" .And. M->UA_XTFRDP1 == "2"

// Busca a transportadora.
If !empty(cChave) .or. lProp
	cQuery := "select SA4.R_E_C_N_O_ TABRecNo " + CRLF
	cQuery += "from " + RetSQLName("SA4") + " SA4 with (noLock) " + CRLF
	If lRedesp
		cQuery += "inner join " + RetSQLName("ZAG") + " ZAG ON " + CRLF
		cQuery += "SA4.A4_EST = ZAG.ZAG_EST " + CRLF
		cQuery += "and SA4.A4_COD_MUN = ZAG.ZAG_CODMUN " + CRLF
		cQuery += "and ZAG.D_E_L_E_T_ = ' ' " + CRLF
		cQuery += "and ZAG.ZAG_FILIAL = '" +xFilial("ZAG")+ "' " + CRLF
		cQuery += "and ZAG.ZAG_REGMET = 'S' " + CRLF
	EndIf
	cQuery += "where SA4.D_E_L_E_T_ = ' ' " + CRLF
	cQuery += "and SA4.A4_FILIAL  = '" + xFilial("SA4") + "' " + CRLF
	cQuery += "and SA4.A4_MSBLQL  <> '1' " + CRLF  // 1-Bloqueadas.

	If cOrdem == "1"  // 1=Nome.
		cQuery += "and upper(SA4.A4_NOME) like '%" + AllTrim(upper(cChave)) +  "%' " + CRLF
		cQuery += "order by SA4.A4_NOME, SA4.A4_COD " + CRLF
	ElseIf cOrdem == "2"  // 2=CNPJ/CPF.
		cChave := StrTran(cChave, ".", "")
		cChave := StrTran(cChave, "/", "")
		cChave := StrTran(cChave, "-", "")
		cQuery += "and SA4.A4_CGC like '" + AllTrim(cChave) +  "%' " + CRLF
		cQuery += "order by SA4.A4_CGC, SA4.A4_COD " + CRLF
	ElseIf cOrdem == "3"  // 3=E-mail.
		cQuery += "and upper(SA4.A4_EMAIL) like '" + AllTrim(upper(cChave)) + "%' " + CRLF
		cQuery += "order by SA4.A4_EMAIL, SA4.A4_COD " + CRLF
	ElseIf cOrdem == "4"  // 4=Próprio.
		cQuery += "and SA4.A4_XPROP = '1' " + CRLF
		If !empty(rtrim(cChave))
			cQuery += "and upper(SA4.A4_NOME) like '%" + AllTrim(upper(cChave)) +  "%' " + CRLF
		Endif
		cQuery += "order by SA4.A4_NOME, SA4.A4_COD " + CRLF
	Endif

	cAliasTop := MPSysOpenQuery(cQuery)
	Do While (cAliasTop)->(!eof())
		// Adiciona os resultados na tela.
		aAdd(aTABRecNo, (cAliasTop)->TABRecNo)
		SA4->(dbGoTo((cAliasTop)->TABRecNo))
		SA4->(aAdd(aResult, {A4_COD, A4_NOME, A4_MUN, A4_EST, StrTran(Transform(A4_CGC, PicPes("J")), "%C", ""), A4_EMAIL, A4_NOME}))

		// Próximo item.
		(cAliasTop)->(dbSkip())
	EndDo
	(cAliasTop)->(dbCloseArea())
Endif

// Atualiza a tela.
If empty(aResult)
	aAdd(aResult, {"", "", "", "", "", ""})
	aAdd(aTABRecNo, 0)
Endif
oGdTrp:Cargo := aTABRecNo
oGdTrp:SetArray(aResult)
oGdTrp:bLine := {|| aResult[oGdTrp:nAt]}
oGdTrp:nAt   := 1
oGdTrp:Refresh()

Return lRet


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : ConPDSB1
// Descrição: Consulta padrão avançada de produtos.
// Retorno  : Lógico, indicando se o usuário confirmou a tela.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 11/01/15 | Felipe Raposo     | Desenvolvimento da rotina.
// 22/01/18 | Wilson A Silva Jr | Retorna grade completa quando acessado por Contratos.
// ---------+-------------------+------------------------------------------------
User Function ConPDSB1()
Local lRet       := .F.
Local nLin, nCol

Local oDlgBusca, oGrBusca, oGrRes, oGrPrd, oGrDet, oGrFil, oGrEst, oGrBut
Local oGetBusca, oGdPrd, oGdGrp, oGdMar, oGdEst, oSayTot

Local cChave     := PadR("", 200)
Local bBusca     := {|| msAguarde({|| BuscaSB1(oGdPrd, oGdGrp, oGdMar, oSayTot, cChave, lContratos)}, "Aguarde", "Pesquisando produto...", .F.), If(empty(cChave), oGetBusca:SetFocus(), oGdPrd:SetFocus()), .T.}

Local bOK        := {|| SB1->(dbGoTo(oGdPrd:Cargo[1, oGdPrd:nAt, 4])), lRet := .T., oDlgBusca:End()}
Local bCancel    := {|| lRet := .F., oDlgBusca:End()}

Local lContratos := IsInCallStack("U_PROMA020")

// Verifica se o usuário já digitou algo na busca.
If Type("__ReadVar") == "C" .and. Type(__ReadVar) == "C" .and. !empty(__ReadVar)
	cChave := PadR(&__ReadVar, 200)
Endif

// Se for módulo de compras/estoque e o usuário tiver digitado algo na busca, abre a consulta padrão de produto.
If !empty(LTrim(cChave)) .and. !IsInCallStack("U_PROMA030") .and. !lContratos
	lRet := ConPad1(,,, "SB1",, .T., .F., __ReadVar,, &__ReadVar)
Else
	// Monta tela de entrada.
	DEFINE MSDIALOG oDlgBusca TITLE "Busca de produtos" FROM 0, 0 TO 560, 880 PIXEL Style DS_MODALFRAME

	// Grupo de dados da busca.
	oGrBusca := TScrollArea():New(oDlgBusca, 0, 0, 18, 0, .F., .F.)
	oGrBusca:Align := CONTROL_ALIGN_TOP

	nLin := 002; nCol := 002
	@ nLin, nCol SAY "Chave de busca" of oGrBusca PIXEL
	@ nLin, nCol + 50 MSGET oGetBusca VAR cChave SIZE 389, 010 of oGrBusca VALID (empty(cChave) .or. Eval(bBusca)) PIXEL F3 "SB1"

	// Grupo de dados de produtos.
	oGrRes := TScrollArea():New(oDlgBusca, 0, 0, 0, 0, .F., .F.)
	oGrRes:Align := CONTROL_ALIGN_ALLCLIENT

	// Grupo de dados dos produtos.
	oGrPrd := TScrollArea():New(oGrRes, 0, 0, 140, 0, .F., .F.)
	oGrPrd:Align := CONTROL_ALIGN_TOP

	// Grid de produtos.
	aCabec := {"Produto", "CA", "Descrição", "Est. Disp.", "P1", "Grupo", "Marca"}
	oGdPrd := TWBrowse():New(0, 0, 0, 0,, aCabec, {40, 35, 70, 30, 30, 70, 70}, oGrPrd)
	oGdPrd:Align := CONTROL_ALIGN_ALLCLIENT
	oGdPrd:blDblClick := bOK
	oGdPrd:bChange    := {|| EstoqSB1(oGdPrd, oGdEst, oGdPrd:nAt)}  // Atualiza o grid de estoque.
	oGdPrd:bHeaderClick := {|o, nCol| OrdSB1(oGdPrd, nCol) }

	// Grupo de dados dos filtros
	oGrDet := TFolder():New(0, 0, {"Filtros", "Estoque"}, {}, oGrRes,,,, .T., .F., 300, 0)
	oGrDet:Align := CONTROL_ALIGN_ALLCLIENT
	oGrFil := oGrDet:aDialogs[1]
	oGrEst := oGrDet:aDialogs[2]

	// Grid de grupos.
	aCabec := {"", "Grupo", "Descrição"}
	oGdGrp := TWBrowse():New(0, 0, 210, 0,, aCabec, {5, 20, 70}, oGrFil)
	oGdGrp:Align := CONTROL_ALIGN_LEFT
	oGdGrp:blDblClick   := {|| FiltraSB1(oGdPrd, oGdGrp, oGdMar, oSayTot, cChave, 1, oGdGrp:nAt)}
	oGdGrp:bHeaderClick := {|o, nCol| If(nCol = 1, FiltraSB1(oGdPrd, oGdGrp, oGdMar, oSayTot, cChave, 1, 0), nil)}

	// Grid de marcas.
	aCabec := {"", "Marca"}
	oGdMar := TWBrowse():New(0, 0, 0, 0,, aCabec, {5, 70}, oGrFil)
	oGdMar:Align := CONTROL_ALIGN_ALLCLIENT
	oGdMar:blDblClick   := {|| FiltraSB1(oGdPrd, oGdGrp, oGdMar, oSayTot, cChave, 2, oGdMar:nAt)}
	oGdMar:bHeaderClick := {|o, nCol| If(nCol = 1, FiltraSB1(oGdPrd, oGdGrp, oGdMar, oSayTot, cChave, 2, 0), nil)}

	// Grid de estoque.
	aCabec := {"Opção", "Disponível", "Descrição", "Cód. antigo", "Descrição antiga"}
	oGdEst := TWBrowse():New(0, 0, 0, 0,, aCabec, {5, 40, 100, 40, 100}, oGrEst)
	oGdEst:Align := CONTROL_ALIGN_ALLCLIENT

	// Grupo dos botões.
	oGrBut := TScrollArea():New(oDlgBusca, 0, 0, 14, 0, .F., .F.)
	oGrBut:Align := CONTROL_ALIGN_BOTTOM

	@ 2, 2 SAY oSayTot VAR Space(100) of oGrBut PIXEL
	tButton():New(2, 366, "Confirmar", oGrBut, bOK,     35, 10,,,, .T.)
	tButton():New(2, 403, "Cancelar",  oGrBut, bCancel, 35, 10,,,, .T.)

	// Atualiza lista de produtos.
	Eval(bBusca)

	ACTIVATE MSDIALOG oDlgBusca centered
	dbSelectArea("SB1")
Endif

Return lRet


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : BuscaSB1
// Descrição:
// Retorno  : Lógico, indicando se o usuário confirmou ou não a busca.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 11/01/15 | Felipe Raposo     | Desenvolvimento da rotina.
// 22/01/18 | Wilson A Silva Jr | Retorna grade completa quando acessado por Contratos.
// ---------+-------------------+------------------------------------------------
Static Function BuscaSB1(oGdPrd, oGdGrp, oGdMar, oSayTot, cChave, lContratos)
Local lRet       := .T.
Local aChave     := {}
Local cChaveCod  := ""
Local cChvCodAnt := ""
Local aResult    := {}
Local aGrupo     := {}
Local aMarca     := {}
Local nX, nLinha := 0

Local cCodSU0    := ""

Local oOk        := LoadBitmap(nil, "LBOK")
Local oNo        := LoadBitmap(nil, "LBNO")

Local cQuery     := ""
Local cAliasTop  := ""

DEFAULT lContratos := .F.

// Verifica os portfólios que o usuário tem acesso.
Static cPortf
If ValType(cPortf) <> "C"
	cPortf := ""
	SU7->(dbSetOrder(4))  // U7_FILIAL, U7_CODUSU.
	If SU7->(msSeek(xFilial() + __cUserId, .F.))
		cCodSU0 := TkPosto(SU7->U7_COD, "U0_CODIGO")
		ZBC->(dbSetOrder(1))  // ZBC_FILIAL, ZBC_GRUPO, ZBC_PORTF.
		ZBC->(dbSeek(xFilial() + cCodSU0, .F.))
		Do While ZBC->(!eof() .and. ZBC_FILIAL + ZBC_GRUPO == xFilial() + cCodSU0)
			cPortf += "'" + ZBC->ZBC_PORTF + "', "
			ZBC->(dbSkip())
		EndDo
	Endif
Endif

// Quantidade máxima de registros a serem listadas na busca de produtos.
Static nTopPDSB1 := SuperGetMV("BZ_TPPDSB1",, 100)

cChave := AllTrim(cChave)
If !empty(cChave)

	// Monta queries para pesquisa de produtos.
	aChave := U_CtoA(cChave, " ", .T.)
	cQuery := "select " + If(nTopPDSB1 > 0, "top " + str(nTopPDSB1), "") + " A.*, SZE.R_E_C_N_O_ SZERecNo, SA2.R_E_C_N_O_ SA2RecNo, " + CRLF
	cQuery += "isnull(( " + CRLF
	cQuery += "    select sum(SB2.B2_QATU - SB2.B2_RESERVA - SB2.B2_QPEDVEN) " + CRLF
	cQuery += "    from " + RetSQLName("SB2") + " SB2 with (noLock)  " + CRLF
	cQuery += "    where SB2.D_E_L_E_T_ = ' ' " + CRLF
	cQuery += "    and SB2.B2_FILIAL  = '" + xFilial("SB2") + "' " + CRLF
	If lContratos
		cQuery += "    and SB2.B2_COD = SB1.B1_COD " + CRLF
	Else
		cQuery += "    and SB2.B2_COD like left(SB1.B1_COD, " + str(nTamRef) + ") + '%' " + CRLF
	EndIf
	cQuery += "    and SB2.B2_LOCAL   = '" + ERP_LOC_VENDAS + "' " + CRLF
	cQuery += "), 0) QtdEst, " + CRLF
	cQuery += IIF(lContratos,"SB1.B1_PRV1","isnull(SB4.B4_PRV1, SB1.B1_PRV1)")+" PRV1, " + CRLF
	cQuery += "round(isnull(( " + CRLF
	cQuery += "    select top 1 case when DA1.DA1_PRCMAX > 0 then DA1.DA1_PRCMAX else "+IIF(lContratos,"SB1.B1_PRV1","isnull(SB4.B4_PRV1, SB1.B1_PRV1)")+" * (1 + (DA1.DA1_PERDES / 100)) end " + CRLF
	cQuery += "    from " + RetSQLName("DA1") + " DA1 with (noLock)  " + CRLF
	cQuery += "    where DA1.D_E_L_E_T_ = ' ' " + CRLF
	cQuery += "    and DA1.DA1_FILIAL = '" + xFilial("DA1") + "' " + CRLF
	cQuery += "    and DA1.DA1_CODPRO = "+IIF(lContratos,"SB1.B1_COD","isnull(SB4.B4_COD, SB1.B1_COD)")+" "+ CRLF
	cQuery += "), 0), " + cValToChar(nArredPrc) + ") PRVDES1, " + CRLF
	cQuery += IIF(lContratos,"SB1.B1_COD","isnull(SB4.B4_COD, SB1.B1_COD)")+" PROD, " + CRLF
	cQuery += IIF(lContratos,"SB1.B1_CA","isnull(SB4.B4_CA, SB1.B1_CA)")+" CA, " + CRLF
	cQuery += IIF(lContratos,"SB1.B1_DESC","isnull(SB4.B4_DESC, SB1.B1_DESC)")+" DESCRIC, " + CRLF
	cQuery += IIF(lContratos,"SB1.B1_XGRUPO","isnull(SB4.B4_XGRUPO, SB1.B1_XGRUPO)")+" GRUPO, " + CRLF
	cQuery += "isnull(SZE.ZE_DESC, '') GRP_DESC, " + CRLF
	cQuery += "case " + CRLF
	cQuery += "   when "+IIF(lContratos,"SB1.B1_FABRIC","isnull(SB4.B4_FABRIC, SB1.B1_FABRIC)")+" = '' then case when SA2.A2_NREDUZ = '' then SA2.A2_NOME else SA2.A2_NREDUZ end " + CRLF
	cQuery += "   else "+IIF(lContratos,"SB1.B1_FABRIC","isnull(SB4.B4_FABRIC, SB1.B1_FABRIC)")+" " + CRLF
	cQuery += "end FABRIC " + CRLF
	cQuery += "from ( " + CRLF

	// Busca pela descrição do produto.
	cQuery += "    select 3 PRIORIDADE, max(SB1.R_E_C_N_O_) SB1RecNo " + CRLF
	cQuery += "    from " + RetSQLName("SB1") + " SB1 with (noLock) " + CRLF
	cQuery += "    where SB1.D_E_L_E_T_ = ' ' " + CRLF
	cQuery += "    and SB1.B1_FILIAL  = '" + xFilial("SB1") + "' " + CRLF
	cQuery += "    and SB1.B1_MSBLQL  <> '1' " + CRLF
	For nX := 1 to len(aChave)
		If !empty(aChave[nX])
			cQuery += "    and CONTAINS(SB1.B1_DESC, '" + '"' + aChave[nX] + '"' + "') " + CRLF
			If len(aChave[nX]) > 3
				cChaveCod += StrTran(aChave[nX], "'", "''") + " "
			Endif
			cChvCodAnt += StrTran(aChave[nX], "'", "''") + " "
		Endif
	Next nX
	cQuery += "    group by SB1.B1_FILIAL, "+IIF(lContratos,"SB1.B1_COD","left(SB1.B1_COD, " + str(nTamRef) + ")")+" " + CRLF

	// Busca pela descrição antiga do produto.
	cQuery += "    union " + CRLF
	cQuery += "    select 3 PRIORIDADE, max(SB1.R_E_C_N_O_) SB1RecNo " + CRLF
	cQuery += "    from " + RetSQLName("SB1") + " SB1 with (noLock) " + CRLF
	cQuery += "    where SB1.D_E_L_E_T_ = ' ' " + CRLF
	cQuery += "    and SB1.B1_FILIAL  = '" + xFilial("SB1") + "' " + CRLF
	cQuery += "    and SB1.B1_MSBLQL  <> '1' " + CRLF
	For nX := 1 to len(aChave)
		If !empty(aChave[nX])
			cQuery += "    and CONTAINS(SB1.B1_XDESC, '" + '"' + aChave[nX] + '"' + "') " + CRLF
		Endif
	Next nX
	cQuery += "    group by SB1.B1_FILIAL, "+IIF(lContratos,"SB1.B1_COD","left(SB1.B1_COD, " + str(nTamRef) + ")")+" " + CRLF

	If !empty(cChaveCod)
		// Busca pelo código do produto.
		cQuery += "    union " + CRLF
		cQuery += "    select 1 PRIORIDADE, max(SB1.R_E_C_N_O_) SB1RecNo " + CRLF
		cQuery += "    from " + RetSQLName("SB1") + " SB1 with (noLock) " + CRLF
		cQuery += "    where SB1.D_E_L_E_T_ = ' ' " + CRLF
		cQuery += "    and SB1.B1_FILIAL  = '" + xFilial("SB1") + "' " + CRLF
		cQuery += "    and SB1.B1_MSBLQL  <> '1' " + CRLF
		cQuery += "    and FREETEXT(SB1.B1_COD, '" + cChaveCod + "') " + CRLF
		cQuery += "    group by SB1.B1_FILIAL, "+IIF(lContratos,"SB1.B1_COD","left(SB1.B1_COD, " + str(nTamRef) + ")")+" " + CRLF
	Endif

	// Busca pelo código antigo do produto.
	cQuery += "    union " + CRLF
	cQuery += "    select 2 PRIORIDADE, max(SB1.R_E_C_N_O_) SB1RecNo " + CRLF
	cQuery += "    from " + RetSQLName("SB1") + " SB1 with (noLock) " + CRLF
	cQuery += "    where SB1.D_E_L_E_T_ = ' ' " + CRLF
	cQuery += "    and SB1.B1_FILIAL  = '" + xFilial("SB1") + "' " + CRLF
	cQuery += "    and SB1.B1_MSBLQL  <> '1' " + CRLF
	cQuery += "    and FREETEXT(SB1.B1_XCOD, '" + cChvCodAnt + "') " + CRLF
	cQuery += "    group by SB1.B1_FILIAL, "+IIF(lContratos,"SB1.B1_COD","left(SB1.B1_COD, " + str(nTamRef) + ")")+" " + CRLF

	// Busca pelo CA.
	cQuery += "    union " + CRLF
	cQuery += "    select 2 PRIORIDADE, max(SB1.R_E_C_N_O_) SB1RecNo " + CRLF
	cQuery += "    from " + RetSQLName("SB1") + " SB1 with (noLock) " + CRLF
	cQuery += "    where SB1.D_E_L_E_T_ = ' ' " + CRLF
	cQuery += "    and SB1.B1_FILIAL  = '" + xFilial("SB1") + "' " + CRLF
	cQuery += "    and SB1.B1_MSBLQL  <> '1' " + CRLF
	cQuery += "    and FREETEXT(SB1.B1_CA, '" + StrTran(cChave, "'", "''") + "') " + CRLF
	cQuery += "    group by SB1.B1_FILIAL, "+IIF(lContratos,"SB1.B1_COD","left(SB1.B1_COD, " + str(nTamRef) + ")")+" " + CRLF

	cQuery += ") A " + CRLF

	cQuery += "inner join " + RetSQLName("SB1") + " SB1 with (noLock) on SB1.R_E_C_N_O_ = A.SB1RecNo " + CRLF

	cQuery += "left  join " + RetSQLName("SB4") + " SB4 with (noLock) on SB4.D_E_L_E_T_ = ' ' " + CRLF
	cQuery += "and SB4.B4_FILIAL  = '" + xFilial("SB4") + "' " + CRLF
	cQuery += "and SB4.B4_COD     = left(SB1.B1_COD, " + str(nTamRef) + ") " + CRLF

	cQuery += "left  join " + RetSQLName("SZE") + " SZE with (noLock) on SZE.D_E_L_E_T_ = ' ' " + CRLF
	cQuery += "and SZE.ZE_FILIAL  = '" + xFilial("SZE") + "' " + CRLF
	cQuery += "and SZE.ZE_COD     = SB1.B1_XGRUPO " + CRLF

	cQuery += "left  join " + RetSQLName("SA2") + " SA2 with (noLock) on SA2.D_E_L_E_T_ = ' ' " + CRLF
	cQuery += "and SA2.A2_FILIAL  = '" + xFilial("SA2") + "' " + CRLF
	cQuery += "and SA2.A2_COD     = SB1.B1_PROC " + CRLF
	cQuery += "and SA2.A2_LOJA    = case when SB1.B1_LOJPROC = '' then '0000' else SB1.B1_LOJPROC end " + CRLF

	cQuery += "where SB1.B1_ATIVO <> 'N' " + CRLF
	cQuery += "and isnull(SZE.ZE_ATIVO, '') <> 'N' " + CRLF
	If !empty(cPortf)
		cQuery += "and isnull(SZE.ZE_PORTF, '') in (" + cPortf + " '') " + CRLF
	Endif

	If lContratos
		cQuery += "order by PRIORIDADE, PRVDES1 desc, QtdEst desc, SB1.B1_COD " + CRLF
	Else
		cQuery += "order by PRIORIDADE, QtdEst desc, SB1.B1_COD " + CRLF
	EndIf

	cAliasTop := MPSysOpenQuery(cQuery)

	msProcTxt("Carregando produtos na tela...")
	ProcessMessages()  // Atualiza a pintura da janela. Processa mensagens do windows.
	Do While (cAliasTop)->(!eof())
		// Adiciona os resultados na tela.
		cMarca := (cAliasTop)->FABRIC
		(cAliasTop)->(aAdd(aResult, {{PROD, CA, DESCRIC, Transform(QtdEst, cPictQtd), Transform(If(PRVDES1 > 0, PRVDES1, PRV1), cPictVlr), GRP_DESC, cMarca}, {0, 0}, {}, SB1RecNo}))
		nLinha ++

		// Adiciona matriz de grupos.
		nX := aScan(aGrupo, {|x| x[1] = (cAliasTop)->SZERecNo})
		If nX = 0
			(cAliasTop)->(aAdd(aGrupo, {SZERecNo, .T., {oOk, GRUPO, If(empty(GRP_DESC), "(Vazio)", GRP_DESC)}, len(aGrupo) + 1, GRUPO}))
			nX := len(aGrupo)
		Endif
		aResult[nLinha, 2, 1] := nX

		// Adiciona matriz de marcas.
		nX := aScan(aMarca, {|x| x[1] = cMarca})
		If nX = 0
			aAdd(aMarca, {cMarca, .T., {oOk, If(empty(cMarca), "(Vazio)", cMarca)}, len(aMarca) + 1, cMarca})
			nX := len(aMarca)
		Endif
		aResult[nLinha, 2, 2] := nX

		// Próximo item.
		(cAliasTop)->(dbSkip())
	EndDo
	(cAliasTop)->(dbCloseArea())
Endif

msProcTxt("Atualizando grade de produtos...")
ProcessMessages()  // Atualiza a pintura da janela. Processa mensagens do windows.

// Atualiza o objeto de grupos.
If empty(aGrupo)
	aAdd(aGrupo, {0, .F., {oNo, "", ""}, 1, ""})
Else
	aSort(aGrupo,,, {|x, y| x[5] < y[5]})
Endif
oGdGrp:Cargo := aGrupo
oGdGrp:SetArray(aGrupo)
oGdGrp:bLine := {|| aGrupo[oGdGrp:nAt, 3]}
oGdGrp:nAt   := 1
oGdGrp:Refresh()

// Atualiza o objeto de marcas.
If empty(aMarca)
	aAdd(aMarca, {0, .F., {oNo, ""}, 1, ""})
Else
	aSort(aMarca,,, {|x, y| x[5] < y[5]})
Endif
oGdMar:Cargo := aMarca
oGdMar:SetArray(aMarca)
oGdMar:bLine := {|| aMarca[oGdMar:nAt, 3]}
oGdMar:nAt   := 1
oGdMar:Refresh()

// Atualiza o resultado da busca.
If empty(aResult)
	aAdd(aResult, {{"", "", "", Transform(0, cPictQtd), Transform(0, cPictVlr), "", ""}, {0, 0}, {}, 0})
Endif
oGdPrd:Cargo := {aResult, aResult}
oGdPrd:SetArray(aResult)
oGdPrd:bLine := {|| aResult[oGdPrd:nAt, 1]}
oGdPrd:nAt   := 1
oGdPrd:Refresh()

// Atualiza texto com quantidade de produtos na tela.
oSayTot:SetText(If(nLinha = 0, "Nenhum produto encontrado", If(nLinha = 1, "1 produto encontrado", cValToChar(nLinha) +  " produtos encontrados")))

// Atualiza grid de estoque.
Eval(oGdPrd:bChange)

Return lRet


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : FiltraSB1
// Descrição:
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 11/01/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
Static Function FiltraSB1(oGdPrd, oGdGrp, oGdMar, oSayTot, cChave, nOpcao, nLinha)
Local nContador  := 0
Local nContFil   := 0
Local lFiltrado  := .F.

Local aResultFil := oGdPrd:Cargo[1]
Local aResult    := oGdPrd:Cargo[2]
Local aGrupo     := oGdGrp:Cargo
Local aMarca     := oGdMar:Cargo
Local oOk        := LoadBitmap(nil, "LBOK")
Local oNo        := LoadBitmap(nil, "LBNO")
Local nX

If !empty(aResult[1, 1, 1])
	If nOpcao = 1  // Grupo.
		If nLinha = 0
			For nX := 1 to len(aGrupo)
				aGrupo[nX, 2] := !aGrupo[nX, 2]
				aGrupo[nX, 3, 1] := If(aGrupo[nX, 2], oOk, oNo)
			Next nX
		Else
			aGrupo[nLinha, 2] := !aGrupo[nLinha, 2]
			aGrupo[nLinha, 3, 1] := If(aGrupo[nLinha, 2], oOk, oNo)
		Endif
		oGdGrp:Refresh()
	Else  // Marca.
		If nLinha = 0
			For nX := 1 to len(aMarca)
				aMarca[nX, 2] := !aMarca[nX, 2]
				aMarca[nX, 3, 1] := If(aMarca[nX, 2], oOk, oNo)
			Next nX
		Else
			aMarca[nLinha, 2] := !aMarca[nLinha, 2]
			aMarca[nLinha, 3, 1] := If(aMarca[nLinha, 2], oOk, oNo)
		Endif
		oGdMar:Refresh()
	Endif

	aResultFil := {}
	For nX := 1 to len(aResult)
		If aGrupo[aScan(aGrupo, {|x| x[4] = aResult[nX, 2, 1]}), 2] .and. aMarca[aScan(aMarca, {|x| x[4] = aResult[nX, 2, 2]}), 2]
			aAdd(aResultFil, aClone(aResult[nX]))
			nContFil++
		Else
			lFiltrado := .T.
		Endif
		nContador++
	Next nX

	// Atualiza texto com quantidade de produtos na tela.
	oSayTot:SetText(If(nContFil = 0, "Nenhum produto listado", If(nContFil = 1, "1 produto listado", cValToChar(nContFil) +  " produtos listado")) + If(lFiltrado, " (filtrado de " + If(nContador = 1, "1 produto)", cValToChar(nContador) + " produtos)"), ""))
Endif

// Atualiza o resultado da busca.
If empty(aResultFil)
	aAdd(aResultFil, {{"", "", Transform(0, cPictQtd), "", ""}, {0, 0}, {}, 0})
Endif
oGdPrd:Cargo[1] := aResultFil
oGdPrd:SetArray(aResultFil)
oGdPrd:bLine := {|| aResultFil[oGdPrd:nAt, 1]}
oGdPrd:nAt   := 1
oGdPrd:Refresh()

Return


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : EstoqSB1
// Descrição:
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 12/01/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
Static Function EstoqSB1(oGdPrd, oGdEst, nLinha)
Local aEstoque   := {}
Local aResult    := oGdPrd:Cargo[1]
Local nSB1RecNo  := aResult[nLinha, 4]

Local cProduto   := ""
Local cProdOpc   := ""
Local cDescric   := ""
Local cQuery     := ""
Local cAliasTop  := ""

If nSB1RecNo > 0
	If empty(aResult[nLinha, 3])
		SB1->(dbGoTo(nSB1RecNo))
		cProduto := left(SB1->B1_COD, nTamRef)

		cQuery := "select SB1.R_E_C_N_O_ SB1RecNo, SB2.R_E_C_N_O_ SB2RecNo, " + CRLF
		cQuery += "TAM.BV_CHAVE TAM, RTrim(TAM.BV_DESCRI) TAM_DESC, " + CRLF
		cQuery += "COR.BV_CHAVE COR, RTrim(COR.BV_DESCRI) COR_DESC " + CRLF

		cQuery += "from " + RetSQLName("SB1") + " SB1 with (noLock) " + CRLF

		cQuery += "left  join " + RetSQLName("SB4") + " SB4 with (noLock) on SB4.D_E_L_E_T_ = ' ' " + CRLF
		cQuery += "and SB4.B4_FILIAL  = '" + xFilial("SB4") + "' " + CRLF
		cQuery += "and SB4.B4_COD     = left(SB1.B1_COD, " + str(nTamRef) + ") " + CRLF

		cQuery += "left  join " + RetSQLName("SBV") + " TAM with (noLock) on TAM.D_E_L_E_T_ = ' ' " + CRLF
		cQuery += "and TAM.BV_FILIAL  = '" + xFilial("SBV") + "' " + CRLF
		cQuery += "and TAM.BV_TABELA  = SB4.B4_LINHA " + CRLF
		cQuery += "and TAM.BV_CHAVE   = substring(SB1.B1_COD, " + str(nTamRef + 1) + ", " + str(nTamLin) + ") " + CRLF

		cQuery += "left  join " + RetSQLName("SBV") + " COR with (noLock) on COR.D_E_L_E_T_ = ' ' " + CRLF
		cQuery += "and COR.BV_FILIAL  = '" + xFilial("SBV") + "' " + CRLF
		cQuery += "and COR.BV_TABELA  = SB4.B4_COLUNA " + CRLF
		cQuery += "and COR.BV_CHAVE   = substring(SB1.B1_COD, " + str(nTamRef + nTamLin + 1) + ", " + str(nTamCol) + ") " + CRLF

		cQuery += "left  join " + RetSQLName("SB2") + " SB2 with (noLock) on SB2.D_E_L_E_T_ = ' ' " + CRLF
		cQuery += "and SB2.B2_FILIAL  = '" + xFilial("SB2") + "' " + CRLF
		cQuery += "and SB2.B2_COD     = SB1.B1_COD " + CRLF
		cQuery += "and SB2.B2_LOCAL   = '" + ERP_LOC_VENDAS + "' " + CRLF

		cQuery += "where SB1.D_E_L_E_T_ = ' ' " + CRLF
		cQuery += "and SB1.B1_FILIAL  = '" + xFilial("SB1") + "' " + CRLF
		cQuery += "and SB1.B1_MSBLQL  <> '1' " + CRLF
		cQuery += "and SB1.B1_COD     like '" + cProduto  + "%' " + CRLF

		cQuery += "order by SB1.B1_FILIAL, TAM.BV_XORDEM, COR.BV_XORDEM, SB1.B1_COD " + CRLF

		cAliasTop := MPSysOpenQuery(cQuery)

		Do While (cAliasTop)->(!eof())
			// Posiciona tabelas.
			SB1->(dbGoTo((cAliasTop)->SB1RecNo))
			SB2->(dbGoTo((cAliasTop)->SB2RecNo))

			If len(rtrim(SB1->B1_COD)) > nTamRef
				cProdOpc := SubStr(SB1->B1_COD, nTamRef + 1)
				cDescric := (cAliasTop)->(RTrim(TAM_DESC) + " / " + RTrim(COR_DESC))
			Else
				cProdOpc := SB1->B1_COD
				cDescric := RTrim(SB1->B1_DESC)
			Endif

			// Armazena posição da tabela.
			aAdd(aEstoque, {cProdOpc, Transform(SB2->(B2_QATU - B2_RESERVA - B2_QPEDVEN), cPictQtd), cDescric, SB1->B1_XCOD, SB1->B1_XDESC})

			// Próximo item.
			(cAliasTop)->(dbSkip())
		EndDo
		(cAliasTop)->(dbCloseArea())
		aResult[nLinha, 3] := aEstoque
	Else
		aEstoque := aResult[nLinha, 3]
	Endif
Endif

// Atualiza o resultado da busca.
If empty(aEstoque)
	aAdd(aEstoque, {"", Transform(0, cPictQtd), "", "", ""})
Endif
oGdEst:SetArray(aEstoque)
oGdEst:bLine := {|| aEstoque[oGdEst:nAt]}
oGdEst:nAt   := 1
oGdEst:Refresh()

Return


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : CPCliFil
// Descrição:
// Retorno  : Lógico, indicando se o usuário confirmou a tela.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 17/07/14 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function CPCliFil(lTela)
Local xRet       := .F.
Local aFilTela   := {}
Local nFiliais   := 0
Local nLin

Local lAltera    := .F.
Local oDialog, oGrFil, oGrBut, oGdFil
Local bOK, bCancel

Local oOk        := LoadBitmap(nil, "LBOK")
Local oNo        := LoadBitmap(nil, "LBNO")

// Define se poderá alterar a seleção.
If IsInCallStack("MATA030")
	lAltera := (Type("ALTERA") == "L" .and. ALTERA) .or. (Type("INCLUI") == "L" .and. INCLUI)
ElseIf IsInCallStack("FAltSA1")
	lAltera := .T.
Endif

If Type("aCliFil") == "A" .and. !empty(aCliFil)
	Default lTela := .T.
	If lTela
		// Monta tela de entrada.
		DEFINE MSDIALOG oDialog TITLE "Filiais" FROM 0, 0 TO 400, 840 PIXEL Style DS_MODALFRAME

		// Grupo de dados da transportadora.
		oGrFil := TScrollArea():New(oDialog, 0, 0, 0, 0, .F., .F.)
		oGrFil:Align := CONTROL_ALIGN_ALLCLIENT

		// Grid das transportadoras.
		aFilTela := aClone(aCliFil)
		aEval(aFilTela, {|x| x[2, 1] := If(x[1], oOk, oNo)})
		aCabec := {"", "Fil.", "Nome", "Operador", "Gestor", ""}
		oGdFil := TWBrowse():New(0, 0, 0, 0,, aCabec, {5, 15, 80, 80, 80, 0}, oGrFil)
		oGdFil:Align := CONTROL_ALIGN_ALLCLIENT
		If lAltera
			oGdFil:blDblClick := {|| nLin := oGdFil:nAt, aFilTela[nLin, 1] := !aFilTela[nLin, 1], aFilTela[nLin, 2, 1] := If(aFilTela[nLin, 1], oOk, oNo), aFilTela[nLin, 2]}
		Endif

		// Atualiza a tela.
		oGdFil:Cargo := aFilTela
		oGdFil:SetArray(aFilTela)
		oGdFil:bLine := {|| aFilTela[oGdFil:nAt, 2]}
		oGdFil:nAt   := 1
		oGdFil:Refresh()

		// Grupo dos botões.
		oGrBut := TScrollArea():New(oDialog, 0, 0, 15, 0, .F., .F.)
		oGrBut:Align := CONTROL_ALIGN_BOTTOM

		bOK     := {|| xRet := .T., oDialog:End()}
		bCancel := {|| xRet := .F., oDialog:End()}
		If lAltera
			tButton():New(2, 345, "Confirmar", oGrBut, bOK,     35, 10,,,, .T.)
			tButton():New(2, 382, "Cancelar",  oGrBut, bCancel, 35, 10,,,, .T.)
		Else
			tButton():New(2, 382, "Fechar",    oGrBut, bCancel, 35, 10,,,, .T.)
		Endif

		ACTIVATE MSDIALOG oDialog centered

		// Posiciona registro da filial para retorno da consulta padrão.
		If xRet
			nLin    := 0
			aCliFil := aFilTela
			aEval(aCliFil, {|x, y| If(x[1], (nLin := y, nFiliais++), nil)})

			// Atualiza filial na tela.
			If nFiliais > 1
				M->A1_XEMPRE  := "**"
				M->A1_XFILNAM := "Diversos"
			ElseIf nFiliais == 1
				U_AtuCamp("A1_XEMPRE", aCliFil[nLin, 2, 2])
			ElseIf nFiliais == 0
				M->A1_XEMPRE  := CriaVar("A1_XEMPRE",  .F.)
				M->A1_XFILNAM := CriaVar("A1_XFILNAM", .F.)
			Endif
		Endif
	Else
		xRet := M->A1_XEMPRE
	Endif
Endif

Return xRet


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : CPCtrFil
// Descrição:
// Retorno  : Lógico, indicando se o usuário confirmou a tela.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 21/08/14 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function CPCtrFil()
Local lRet       := .F.
Local cFilSel    := ""
Local aFilSel    := {}
Local nFiliais   := 0
Local cFilDes    := ""
Local nLin
Local cFilPrc	:= ""

Local lAltera    := .F.
Local oDialog, oGrFil, oGrBut
Local oGdFil

Local oOk        := LoadBitmap(nil, "LBOK")
Local oNo        := LoadBitmap(nil, "LBNO")

Local bOK        := {|| lRet := .T., oDialog:End()}
Local bCancel    := {|| lRet := .F., oDialog:End()}

// Guarda em variável estáticas as filiais que farão parte do processamento.
If empty(aFiliais)
	U_Filiais(@cFilPrc, @aFiliais)
Endif

Default aFilCtrt := {}
For nLin := 1 to len(aFiliais)
	cFilSel := aFiliais[nLin]
	If aScan(aFilCtrt, {|x| x[1] .and. x[2] == cFilSel}) > 0 .or. M->ZA1_FILORI == cFilSel .OR. cFilSel $ M->ZA1_FILDES
		aAdd(aFilSel, {.T., {oOk, cFilSel, FWFilialName(nil, cFilSel, 1)}})
	Else
		aAdd(aFilSel, {.F., {oNo, cFilSel, FWFilialName(nil, cFilSel, 1)}})
	Endif
Next nLin

// Define se poderá alterar a seleção.
lAltera := (Type("ALTERA") == "L" .and. ALTERA) .or. (Type("INCLUI") == "L" .and. INCLUI)

// Monta tela de entrada.
DEFINE MSDIALOG oDialog TITLE "Filiais" FROM 0, 0 TO 350, 300 PIXEL Style DS_MODALFRAME

// Grupo de dados da transportadora.
oGrFil := TScrollArea():New(oDialog, 0, 0, 0, 0, .F., .F.)
oGrFil:Align := CONTROL_ALIGN_ALLCLIENT

// Grid das filiais.
aCabec := {"", "Fil.", "Nome"}
oGdFil := TWBrowse():New(0, 0, 0, 0,, aCabec, {5, 10, 80}, oGrFil)
oGdFil:Align := CONTROL_ALIGN_ALLCLIENT
If lAltera
	oGdFil:blDblClick := {|| nLin := oGdFil:nAt, aFilSel[nLin, 1] := !aFilSel[nLin, 1], aFilSel[nLin, 2, 1] := If(aFilSel[nLin, 1], oOk, oNo), aFilSel[nLin, 2]}
Endif

// Atualiza a tela.
oGdFil:Cargo := aFilSel
oGdFil:SetArray(aFilSel)
oGdFil:bLine := {|| aFilSel[oGdFil:nAt, 2]}
oGdFil:nAt   := 1
oGdFil:Refresh()

// Grupo dos botões.
oGrBut := TScrollArea():New(oDialog, 0, 0, 15, 0, .F., .F.)
oGrBut:Align := CONTROL_ALIGN_BOTTOM

If lAltera
	tButton():New(2, 075, "Confirmar", oGrBut, bOK,     35, 10,,,, .T.)
	tButton():New(2, 112, "Cancelar",  oGrBut, bCancel, 35, 10,,,, .T.)
Else
	tButton():New(2, 112, "Fechar",    oGrBut, bCancel, 35, 10,,,, .T.)
Endif

ACTIVATE MSDIALOG oDialog centered

// Guarda as amarrações para gravação.
If lRet
	aFilCtrt := {}
	For nLin := 1 to len(aFilSel)
		cFilSel := aFilSel[nLin, 2, 2]
		If aFilSel[nLin, 1]
			nFiliais ++
			cFilCtrt := cFilSel
			cFilDes += If(!Empty(cFilDes), "/", "") + cFilSel
			aAdd(aFilCtrt, {.T., cFilSel})
		Else
			aAdd(aFilCtrt, {.F., cFilSel})
		Endif
	Next nLin

	// Posiciona registro da filial para retorno da consulta padrão.
	If nFiliais == 0
		cFilCtrt := "  "
	ElseIf nFiliais > 1
		cFilCtrt := M->ZA1_FILORI
	Endif

	// Acerta a descrição das filiais.
	M->ZA1_FILDES := cFilDes
Endif

Return lRet


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : PVDesmem
// Descrição:
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 27/10/14 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function PVDesmem()
Local lRet       := .F.
Local aArea      := GetArea()
Local nLin

Local oOk        := LoadBitmap(nil, "LBOK")
Local oNo        := LoadBitmap(nil, "LBNO")
Local oEstOK     := LoadBitmap(nil, "BR_VERDE")
Local oEstNOK    := LoadBitmap(nil, "BR_VERMELHO")

Local oDialog, oGrPed, oGrTot, oTotPed1, oTotPed2
Local oGdPed, bAtuMrk, bAtuTot, bDesmem, bSelec, bQtde, bQtdDesm

Local aCabec[0], aTam[0]
Local aItens[0], aItem[0], aItensAux[0]
Local nSaldoSB2  := 0
Local lEstoque   := .F.
Local nDesmemb   := 0
Local nTotPed1   := 0
Local nTotPed2   := 0

Local bOK, bCancel, aButtons[0]

Private lMsErroAuto := .F.

If SC5->C5_XBLQCON == "C" //Bloqueio Comercial
	HELP(' ',1,"Não permitido desmembrar" ,,"Pedido com bloqueio comercial, não pode ser desmembrado!",2,0,,,,,, {"Verifique com a gestão comercial ref. o bloqueio do pedido. Somente após liberação é que o pedido poderá ser desmembrado."})
Else
// Não permite selecionar a rotina em filial diferente do pedido posicionado.
If xFilial("SC5", SC5->C5_FILIAL) <> xFilial("SC5", cFilAnt)
	MsgAlert("Selecione a filial correta do pedido de venda.", "Atenção")
ElseIf U_MT410ACE()
	If SoftLock("SC5")
		// Monta matriz com itens a desmembrar.
		lRet := .T.
		SC6->(dbSetOrder(1))  // C6_FILIAL, C6_NUM, C6_ITEM, C6_PRODUTO.
		SC6->(dbSeek(xFilial() + SC5->C5_NUM, .F.))
		Do While SC6->(!eof() .AND. C6_FILIAL + C6_NUM == xFilial() + SC5->C5_NUM)
			If SC6->(C6_QTDVEN - C6_QTDENT) > 0 .and. !SC6->C6_BLQ $ "RS"
				SB1->(dbSetOrder(1))  // B1_FILIAL, B1_COD.
				SB1->(msSeek(xFilial() + SC6->C6_PRODUTO, .F.))
				SB2->(dbSetOrder(1))  // B2_FILIAL, B2_COD, B2_LOCAL.
				SB2->(msSeek(xFilial() + SC6->(C6_PRODUTO + C6_LOCAL), .F.))
				nSaldoSB2 := SaldoSB2()
				lEstoque  := (nSaldoSB2 >= SC6->(C6_QTDVEN - C6_QTDENT))
				nDesmemb  := If(lEstoque, 0, SC6->(C6_QTDVEN - C6_QTDENT))

				aItem := {}
				aAdd(aItem, oNo)
				aAdd(aItem, oEstOK)
				aAdd(aItem, SC6->C6_ITEM)
				aAdd(aItem, SC6->C6_PRODUTO)
				aAdd(aItem, Transform(SC6->(C6_QTDVEN - C6_QTDENT), cPictQtd))
				aAdd(aItem, Transform(nDesmemb, cPictQtd))
				aAdd(aItem, Transform(SB2->B2_QATU, cPictQtd))
				aAdd(aItem, Transform(SC6->C6_QTDEMP, cPictQtd))
				aAdd(aItem, Transform(nSaldoSB2, cPictQtd))
				aAdd(aItem, Transform(SB1->B1_QE, cPictQtd))
				aAdd(aItem, Transform(SC6->C6_PRCVEN, cPictVlr))
				aAdd(aItem, Transform(SC6->C6_VALOR,  cPictVlr))
				aAdd(aItem, SC6->C6_DESCRI)
				aAdd(aItem, Alltrim(X3Combo("C5_TPFRETE", SC5->C5_TPFRETE)))
				aAdd(aItem, SC5->C5_XCDMUNE + " - " + Alltrim(Posicione("CC2", 1, xFilial("CC2") + SC5->C5_XEST + SC5->C5_XCDMUNE, "CC2_MUN"))) // Indice 1 - CC2_FILIAL+CC2_EST+CC2_CODMUN
				aAdd(aItem, Alltrim(SC5->C5_XEST))
				aAdd(aItem, SC5->C5_CONDPAG + " - " + Alltrim(Posicione("SE4", 1, xFilial("SE4") + SC5->C5_CONDPAG, "E4_DESCRI")))
				aAdd(aItens, aItem)
				aAdd(aItensAux, {.F., SC6->(C6_QTDVEN - C6_QTDENT), 0, nSaldoSB2, SC6->(RecNo()), SC6->C6_PRCVEN, SC6->C6_PRODUTO})

				If !lEstoque
					aTail(aItens)[1] := oOk
					aTail(aItens)[2] := oEstNOK
					aTail(aItensAux)[1] := .T.
					aTail(aItensAux)[3] := nDesmemb
				Endif
			Endif

			SC6->(dbSkip())
		EndDo

		If empty(aItens)
			MsgAlert("O pedido de venda selecionado não possui itens com saldo a faturar.", "Atenção")
		Else
			// Monta tela de entrada.
			DEFINE MSDIALOG oDialog TITLE "Pedido " + SC5->C5_NUM FROM 0, 0 TO 450, 840 PIXEL Style DS_MODALFRAME

			// Grupo de dados da transportadora.
			oGrPed := TScrollArea():New(oDialog, 0, 0, 0, 0, .F., .F.)
			oGrPed:Align := CONTROL_ALIGN_ALLCLIENT

			// Grid das transportadoras.
			aCabec := {"", "", "Item", "Produto", "Qtde ven.", "Desmembrar", "Estoque","Reservado","Disponível", "Qtd. Emb.", "Vlr Unit.", "Total",  "Descrição", "Tipo Frete", "Mun. Entrega", "UF Entrega", "Cond.Pag."}
			aTam   := {5, 5, 15, 50, 35, 35, 35, 35,35, 35, 35, 35, 80}
			oGdPed := TWBrowse():New(0, 0, 0, 0,, aCabec, aTam, oGrPed)
			oGdPed:Align := CONTROL_ALIGN_ALLCLIENT
			oGdPed:SetArray(aItens)
			oGdPed:bLine := {|| aItens[oGdPed:nAt]}
			oGdPed:nAt   := 1

			// Grupo de dados da transportadora.
			oGrTot := TScrollArea():New(oGrPed, 0, 0, 15, 0, .F., .F.)
			oGrTot:Align := CONTROL_ALIGN_BOTTOM

			// Mensagem no rodapé, com valores totais.
			@ 01, 002 SAY "Total pedido original"     of oGrTot PIXEL
			@ 01, 070 MSGET oTotPed1 VAR nTotPed1 PICTURE cPictVlr of oGrTot SIZE 045, 010 PIXEL READONLY
			@ 01, 182 SAY "Total pedido desmembrado"  of oGrTot PIXEL
			@ 01, 255 MSGET oTotPed2 VAR nTotPed2 PICTURE cPictVlr of oGrTot SIZE 045, 010 PIXEL READONLY

			tButton():New(2, 383, "Ped. vendas", oGrTot, {|| U_ProdRes(aItens[oGdPed:nAt, 4])}, 035, 010,,,, .T.)

			// Configura double-click do objeto.
			bAtuMrk  := {|nLin| aItens[nLin, 1] := If(aItensAux[nLin, 1], oOk, oNo)}
			bAtuTot  := {|| nTotPed1 := nTotPed2 := 0, aEval(aItensAux, {|aItem| nTotPed1 += (aItem[2] - aItem[3]) * aItem[6], nTotPed2 += aItem[3] * aItem[6]}), oTotPed1:Refresh(), oTotPed2:Refresh()}
			bDesmem  := {|nLin, nQtdDes| If(ValType(nQtdDes) == "N", (aItensAux[nLin, 3] := max(0, min(nQtdDes, aItensAux[nLin, 2])), aItens[nLin, 6] := Transform(aItensAux[nLin, 3], cPictQtd)), nil)}
			bSelec   := {|oGdItens, nLin, nCol| aItensAux[nLin, 1] := !aItensAux[nLin, 1], Eval(bDesmem, nLin, If(aItensAux[nLin, 1], aItensAux[nLin, 2], 0))}
			bQtdDesm := {|| VAL_Q_DESM(aItensAux[oGdPed:nAt, 7], aItensAux[oGdPed:nAt, 3])}
			bQtde    := {|oGdItens, nLin, nCol| aItens[nLin, 6] := aItensAux[nLin, 3], lEditCell(aItens, oGdItens, cPictQtd, nCol), Eval(bDesmem, nLin, aItens[nLin, 6]), aItensAux[nLin, 1] := (aItensAux[nLin, 3] > 0), Eval(bQtdDesm)}
			oGdPed:blDblClick := {|nLin, nCol| nLin := oGdPed:nAt, Eval(If(nCol = 6, bQtde, bSelec), oGdPed, nLin, nCol), Eval(bAtuMrk, nLin), Eval(bAtuTot, nLin)}

			Eval(bAtuTot)
			bOK     := {|| If(TEL_Q_DESM(aItensAux), (msAguarde({|| PVDesmem(aItensAux)}, "Desmembrando pedido", "Aguarde...", .F.), If(lMsErroAuto, nil, oDialog:End())), Nil)}
			bCancel := {|| oDialog:End()}
			ACTIVATE MSDIALOG oDialog ON INIT EnchoiceBar(oDialog, bOK, bCancel,, aButtons) centered
		Endif

		SC5->(MsUnlock())
	Else
		Help(" ", 1, "Help", "PVDesmem", "Pedido em manutenção por outro usuário.", 3, 0)
	EndIf
Endif
EndIf
RestArea(aArea)

Return lRet
//-------------------------------------------------------------------
/*/{Protheus.doc} PVDESWIS
Desmembramento do Pedido no Retorno do WIS

@author  Guilherme Santos
@since   29/10/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
User Function PVDESWIS(cPedido)
	Local nSaldoSB2		:= 0
	Local lRetorno 		:= .T.
	Local aItensAux		:= {}
	Private lMsErroAuto	:= .F.

	DbSelectArea("SC5")
	DbSetOrder(1)	//C5_FILIAL, C5_NUM

	If SC5->(DbSeek(xFilial("SC5") + cPedido))
		DbSelectArea("SC6")
		DbSetOrder(1)		//C6_FILIAL, C6_NUM, C6_ITEM

		If SC6->(DbSeek(xFilial("SC6") + SC5->C5_NUM))
			While !SC6->(Eof()) .AND. xFilial("SC6") + SC5->C5_NUM == SC6->C6_FILIAL + SC6->C6_NUM
				DbSelectArea("SB2")
				DbSetOrder(1)		//B2_FILIAL, B2_COD, B2_LOCAL

				If SB2->(DbSeek(xFilial("SB2") + SC6->C6_PRODUTO + SC6->C6_LOCAL))
					nSaldoSB2 := SaldoSB2()
				Else
					nSaldoSB2 := 0
				EndIf

				aAdd(aItensAux, {.T., SC6->C6_QTDVEN - SC6->C6_QTDENT, If(SC6->C6_XQTEXP <> SC6->C6_QTDVEN, SC6->C6_QTDVEN - SC6->C6_QTDENT - SC6->C6_XQTEXP, 0) , nSaldoSB2, SC6->(RecNo()), SC6->C6_PRCVEN, SC6->C6_PRODUTO})

				SC6->(DbSkip())
			End
		Else
			Help(" ", 1, "Help", "PVDESWIS", "Itens do Pedido não localizados.", 3, 0)
			lRetorno := .F.
		EndIf
	Else
		Help(" ", 1, "Help", "PVDESWIS", "Pedido não localizado.", 3, 0)
		lRetorno := .F.
	EndIf

	If Empty(aItensAux)
		Help(" ", 1, "Help", "PVDESWIS", "Não existem itens com quantidades divergentes para desmembramento.", 3, 0)
		lRetorno := .F.
	Else
		PVDesmem(aItensAux)

		If lMsErroAuto
			lRetorno := .F.
		EndIf
	EndIf

Return lRetorno
// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Ciro Pedreira
// Modulo   : Faturamento / Call Center
// Função   : TEL_Q_DESM
// Descrição: Valida o botão OK da tela de desmembramento do pedido de venda.
// Retorno  : Lógico, informando se permite ou não a alteração da quantidade.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 15/05/17 | Ciro Pedreira     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
Static Function TEL_Q_DESM(aItensAux)
Local lRet     := .T.
Local nX

For nX := 1 to len(aItensAux)
	If !VAL_Q_DESM(aItensAux[nX, 7], aItensAux[nX, 3])
		lRet := .F.
		Exit
	Endif
Next nX

Return lRet


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Ciro Pedreira
// Modulo   : Faturamento / Call Center
// Função   : VAL_Q_DESM
// Descrição: Função executada ao alterar o quantidade do produto.
//          : Função baseada na TGQuant() que fica dentro do arquivo
//          : "PROMA030.PRW".
// Retorno  : Lógico, informando se permite ou não a alteração da quantidade.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 15/05/17 | Ciro Pedreira     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
Static Function VAL_Q_DESM(cProduto, nQtde)
Local lRet     := .T.
Local aArea    := GetArea()
Local aAreaSB1 := SB1->(GetArea())

// Verifica o múltiplo do produto.
SB1->(dbSetOrder(1))  // Indice 1 - B1_FILIAL+B1_COD
SB1->(msSeek(xFilial() + cProduto, .F.))

If SB1->(B1_XMULTGV > 1 .and. (nQtde % B1_XMULTGV) <> 0)
	MsgAlert("Produto " + cProduto + " só pode ser vendido em múltiplo de " + cValToChar(SB1->B1_XMULTGV) + ".", "Atenção")
	lRet := .F.
ElseIf SB1->(B1_QE > 1 .and. (nQtde % B1_QE) <> 0)
	MsgInfo("Produto " + cProduto + " múltiplo de " + cValToChar(SB1->B1_QE) + ".", "Atenção")
EndIf

RestArea(aAreaSB1)
RestArea(aArea)

Return lRet


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : PVDesmem
// Descrição:
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 27/10/14 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
Static Function PVDesmem(aItensAux)
Local aArea      := GetArea()
Local aAreaSC5   := SC5->(GetArea())
Local cMsgErro   := ""
Local nX, nY

Local cFilPed    := SC5->C5_FILIAL
Local cAtend     := SC5->C5_XATEND
Local cPedido    := SC5->C5_NUM
Local cPedPai	 := SC5->C5_XPEDPAI
Local cPedOri    := iIf(!empty(SC5->C5_XPEDORI),SC5->C5_XPEDORI,SC5->C5_NUM)
Local cOperado   := SC5->C5_XOPER
Local cHora      := SC5->C5_XHORA
Local cDepVen    := SC5->C5_XDEPVEN

Local cPedFilho  := ""
Local aLibCred   := {SC5->C5_XCRDLIB, SC5->C5_XDTLIB, SC5->C5_XUSRLIB, SC5->C5_XCONLIB, SC5->C5_XOBSLIB, SC5->C5_XVLRLIB, 0, SC5->C5_XVLDCRD}
Local cOrdCom    := ""
Local cCliente   := ""

Local nOpcAuto   := 0
Local aAutoSC5o  := {}
Local aAutoSC6o  := {}
Local aAutoSC5d  := {}
Local aAutoSC6d  := {}
Local aItem      := {}

Local aStruSC5   := {}
Local aStruSC6   := {}
Local cCampo     := ""
Local aIgnoraCpo := {}

Local cTo        := ""
Local cAssunto   := ""
Local cMsgMail   := ""
Local cItensPai  := ""
Local cItensFil  := ""
Local lBlqCon    := .F.
local lMantem	 := SC5->(C5_XCRDLIB == "L" .and. C5_XCONLIB = C5_CONDPAG .and. C5_XVLRLIB >= C5_XVALFAT .and. C5_XVLDCRD >= date() .and. allTrim(C5_XUSRLIB) != "Sistema")
local lMant      := SC5->C5_XVLDCRD < date()
local cUsrLib	 := allTrim(SC5->C5_XUSRLIB)
Local lRet		 := .F.
Local cMsg		 := ""
Local aMsg       := {}
Local nAviso	 := 0

// Campos que não serão copiados no desmembramento.
aAdd(aIgnoraCpo, "C5_NUM")
aAdd(aIgnoraCpo, "C5_TIPO")
aAdd(aIgnoraCpo, "C5_CLIENT")
aAdd(aIgnoraCpo, "C5_LOJAENT")
aAdd(aIgnoraCpo, "C5_FRETE")
if ! lMantem
	aAdd(aIgnoraCpo, "C5_XCRDLIB")
	aAdd(aIgnoraCpo, "C5_XDTLIB")
	aAdd(aIgnoraCpo, "C5_XVLDCRD")
	aAdd(aIgnoraCpo, "C5_XUSRLIB")
	aAdd(aIgnoraCpo, "C5_XCONLIB")
	aAdd(aIgnoraCpo, "C5_XOBSLIB")
	aAdd(aIgnoraCpo, "C5_XVLRLIB")
endIf

//aAdd(aIgnoraCpo, "C5_XWIS")
aAdd(aIgnoraCpo, "C5_XOPER")
aAdd(aIgnoraCpo, "C5_XHORA")
aAdd(aIgnoraCpo, "C6_PEDCLI")
aAdd(aIgnoraCpo, "C6_QTDEMP")
aAdd(aIgnoraCpo, "C6_QTDENT")
aAdd(aIgnoraCpo, "C6_VALOR")
aAdd(aIgnoraCpo, "C5_XPEDPAI")

// Verifica se o usuário selecionou algum item para desmembrar.
If aScan(aItensAux, {|x| x[3] > 0}) = 0 .or. aScan(aItensAux, {|x| x[2] <> x[3]}) = 0
	MsgAlert("Selecione pelo menos um item para desmembrar.", "Atenção")
	lMsErroAuto := .T.
Else

	If IsInCallStack("U_PVDESWIS")
		lRet := .T.
	Else
		If lMant
			cMsg := "O crédito do pedido está vencido, selecione a ação que deseja efetuar ?"
			aMsg := {"Enviar Crédito", "Desmembrar", "Cancelar"}
		else
			cMsg := "Tem certeza que deseja efetuar o desmembramento ?"
			aMsg := {"Sim","Não"}
		EndIf

		nAviso := Aviso("Desmembramento", cMsg, aMsg)

		If Len(aMsg) == 3
			If nAviso == 1
				U_MA061Crd()
				Return
			ElseIf nAviso == 2
				lRet := .T.
			EndIf
		Else
			If nAviso == 1
				lRet := .T.
			EndIf
		EndIf
	EndIf

	If lRet
		// Monta o cabeçalho do pedido.
		aAdd(aAutoSC5o, {"C5_TIPO", "N", nil})
		aStruSC5 := SC5->(dbStruct())
		For nY := 1 to len(aStruSC5)
			cCampo := aStruSC5[nY, 1]
			If aScan(aIgnoraCpo, rtrim(cCampo)) = 0 .AND. X3Uso(GetSX3Cache(cCampo, "X3_USADO")) .and. cNivel >= GetSX3Cache(cCampo, "X3_NIVEL")
				xValor := SC5->(FieldGet(FieldPos(cCampo)))
				If !empty(xValor)
					aAdd(aAutoSC5o, {cCampo, xValor, nil})
				Endif
			Endif
		Next nY

		// Pega a ordem de compra do pedido original.
		SC6->(dbGoTo(aItensAux[1, 5]))
		cOrdCom := SC6->C6_NUMPCOM

		// Cria tabelas dos pedidos.
		cItensPai := "<table>"
		cItensPai += " <tr>"
		cItensPai += "  <th style='border:1px solid #002a5c; border-collapse:collapse; width:05%; font-weight:bold; text-align:left'>It</th>"
		cItensPai += "  <th style='border:1px solid #002a5c; border-collapse:collapse; width:30%; font-weight:bold; text-align:left'>Produto</th>"
		cItensPai += "  <th style='border:1px solid #002a5c; border-collapse:collapse; width:15%; font-weight:bold; text-align:right'>Qtde anterior</th>"
		cItensPai += "  <th style='border:1px solid #002a5c; border-collapse:collapse; width:15%; font-weight:bold; text-align:right'>Qtde ap&oacute;s</th>"
		cItensPai += "  <th style='border:1px solid #002a5c; border-collapse:collapse; width:35%; font-weight:bold; text-align:left'>Observa&ccedil;&atilde;o</th>"
		cItensPai += " </tr>"

		cItensFil := "<table>"
		cItensFil += " <tr>"
		cItensFil += "  <th style='border:1px solid #002a5c; border-collapse:collapse; width:05%; font-weight:bold; text-align:left'>It</th>"
		cItensFil += "  <th style='border:1px solid #002a5c; border-collapse:collapse; width:80%; font-weight:bold; text-align:left'>Produto</th>"
		cItensFil += "  <th style='border:1px solid #002a5c; border-collapse:collapse; width:15%; font-weight:bold; text-align:right'>Qtde desmembrada</th>"
		cItensFil += " </tr>"

		// Monta matrizes com os itens dos dois pedidos (original e desmembrado).
		aStruSC6 := SC6->(dbStruct())
		For nX := 1 to len(aItensAux)
			SC6->(dbGoTo(aItensAux[nX, 5]))
			aItem := {}
			For nY := 1 to len(aStruSC6)
				cCampo := aStruSC6[nY, 1]
				If aScan(aIgnoraCpo, rtrim(cCampo)) = 0 .and. X3Uso(GetSX3Cache(cCampo, "X3_USADO")) .and. cNivel >= GetSX3Cache(cCampo, "X3_NIVEL")
					xValor := SC6->(FieldGet(FieldPos(cCampo)))
					If !empty(xValor)
						aAdd(aItem, {cCampo, xValor, nil})
					Endif
				Endif
			Next nY

			// Adiciona item na tabela do e-mail.
			cItensPai += " <tr>"
			cItensPai += "  <th style='border:1px solid #002a5c; border-collapse:collapse; font-weight:normal; text-align:left'>" + SC6->C6_ITEM + "</th>"
			cItensPai += "  <th style='border:1px solid #002a5c; border-collapse:collapse; font-weight:normal; text-align:left'>" + U_Txt2HTML(SC6->C6_PRODUTO, .T.) + "</th>"
			cItensPai += "  <th style='border:1px solid #002a5c; border-collapse:collapse; font-weight:normal; text-align:right'>" + Transform(SC6->(C6_QTDVEN - C6_QTDENT), cPictQtd) + "</th>"
			cItensPai += "  <th style='border:1px solid #002a5c; border-collapse:collapse; font-weight:normal; text-align:right'>" + Transform(SC6->(C6_QTDVEN - C6_QTDENT) - aItensAux[nX, 3], cPictVlr) + "</th>"
			cItensPai += "  <th style='border:1px solid #002a5c; border-collapse:collapse; font-weight:normal; text-align:left'>" + If(aItensAux[nX, 3] = 0, "", If(aItensAux[nX, 3] == SC6->(C6_QTDVEN - C6_QTDENT), "Item desmembrado completo.", "Item desmembrado parcialmente.")) + "</th>"
			cItensPai += " </tr>"

			// Adiciona o item para ser alterado no pedido original.
			aAdd(aAutoSC6o, aClone(aItem))
			If aItensAux[nX, 3] > 0
				// Se o cliente selecionou o item, adiciona no pedido
				// novo (desmembrado), e exclui do original.
				aAdd(aAutoSC6d, aClone(aItem))

				// Verifica se é para desmembrar 100% da linha.
				If aItensAux[nX, 3] == SC6->(C6_QTDVEN - C6_QTDENT)
					// Marca item para ser excluído no pedido original.
					aItem := aTail(aAutoSC6o)
					aAdd(aItem, {"AUTDELETA", "S", SC6->C6_ITEM})
				Else
					// Altera a quantidade nos dois pedidos.

					// No pedido original fica o saldo - quantidade desmembrada.
					aItem := aTail(aAutoSC6o)
					aItem[aScan(aItem, {|x| rtrim(x[1]) == "C6_QTDVEN"}), 2] := SC6->(C6_QTDVEN - C6_QTDENT) - aItensAux[nX, 3]

					// No pedido filho fica a quantidade desmembrada.
					aItem := aTail(aAutoSC6d)
					aItem[aScan(aItem, {|x| rtrim(x[1]) == "C6_QTDVEN"}), 2] := aItensAux[nX, 3]
				Endif

				// Adiciona item na tabela do e-mail.
				cItensFil += " <tr>"
				cItensFil += "  <th style='border:1px solid #002a5c; border-collapse:collapse; font-weight:normal; text-align:left'>" + SC6->C6_ITEM + "</th>"
				cItensFil += "  <th style='border:1px solid #002a5c; border-collapse:collapse; font-weight:normal; text-align:left'>" + U_Txt2HTML(SC6->C6_PRODUTO, .T.) + "</th>"
				cItensFil += "  <th style='border:1px solid #002a5c; border-collapse:collapse; font-weight:normal; text-align:right'>" + Transform(aItensAux[nX, 3], cPictQtd) + "</th>"
				cItensFil += " </tr>"
			Endif
		Next nX

		// Finaliza tabelas de itens dos pedidos.
		cItensPai += "</table>"
		cItensFil += "</table>"

		// Verifica se ambos pedidos possuem ordem de compra.
		If !empty(cOrdCom)
			// Verifica a ordem de compra no pedido original.
			nX := aScan(aItensAux, {|x| x[2] <> x[3]})  // Busca o primeiro item que ficará no pedido original.
			If aScan(aAutoSC6o[nX], {|x| rtrim(x[1]) == "C6_NUMPCOM"}) == 0
				aAdd(aAutoSC6o[nX], {"C6_NUMPCOM", cOrdCom, nil})
			Endif

			// Verifica a ordem de compra no pedido desmembrado.
			If aScan(aAutoSC6d[1], {|x| rtrim(x[1]) == "C6_NUMPCOM"}) == 0
				aAdd(aAutoSC6d[1], {"C6_NUMPCOM", cOrdCom, nil})
			Endif
		Endif

		// Efetua a gravação dos pedidos.
		Begin Transaction

		U_DebugMsg("PVDesmem - Início do desmembramento", "I")

		// Limpa as funções fiscais.
		If MaFisFound()
			U_DebugMsg("PVDesmem - Limpando funções fiscais...")
			MaFisEnd()
		Endif

		// Inclui o pedido com os itens desmembrado.
		U_DebugMsg("PVDesmem - Incluindo pedido novo...")
		aAutoSC5d := aClone(aAutoSC5o)
		aAdd(aAutoSC5d, {"C5_XATEND",  cAtend,   nil})
		aAdd(aAutoSC5d, {"C5_XOPER",   cOperado, nil})
		aAdd(aAutoSC5d, {"C5_XHORA",   cHora,    nil})
		aAdd(aAutoSC5d, {"C5_XPEDPAI", cPedido,  nil})
		aAdd(aAutoSC5d, {"C5_XPEDORI", cPedOri,  nil})
		nOpcAuto := 3  // 3-Inclui.
		msExecAuto({|x, y, z| MATA410(x, y, z)}, aAutoSC5d, aAutoSC6d, nOpcAuto)
		U_DebugMsg("PVDesmem - Fim da inclusão do pedido novo...")

		// Verifica se houve erro na gravação do pedido.
		If !lMsErroAuto
			If __lSX8
				U_DebugMsg("PVDesmem - Confirmando SXE/SXF...")
				ConfirmSX8()
			Endif

			cPedFilho   := SC5->C5_NUM
			aLibCred[7] := SC5->C5_XVALFAT
			lBlqCon		:= SC5->C5_XBLQCON $ "|2|3|4|5|6|7|8|"

			// Limpa as funções fiscais.
			If MaFisFound()
				U_DebugMsg("PVDesmem - Limpando funções fiscais...")
				MaFisEnd()
			Endif

			// Altera o pedido original (desmembrado).
			U_DebugMsg("PVDesmem - Alterando pedido anterior...")
			aAdd(aAutoSC5o, nil); aIns(aAutoSC5o, 1)
			aAutoSC5o[1] := {"C5_NUM", cPedido, nil}
			// Preserva o número do pedido pai do pedido original
			aAdd(aAutoSC5o, {"C5_XPEDPAI", cPedPai, nil})

			nOpcAuto := 4  // 4-Altera.
			msExecAuto({|x, y, z| MATA410(x, y, z)}, aAutoSC5o, aAutoSC6o, nOpcAuto)
			U_DebugMsg("PVDesmem - Fim da alteração do pedido anterior...")

			// Verifica se houve erro na gravação do pedido.
			If !lMsErroAuto
				If __lSX8
					U_DebugMsg("PVDesmem - Confirmando SXE/SXF...")
					ConfirmSX8()
				Endif

				// Grava histórico no pedido original.
				U_DebugMsg("PVDesmem - Gravando histórioco do pedido...")
				U_HistPV(SC5->C5_NUM, "2", "Pedido desmembrado -> " + cPedFilho, "Valor de mercadorias: R$ " + AllTrim(Transform(SC5->C5_XVALMER, PesqPict('SC5', 'C5_XVALMER'))))

				// Soma o valor de faturamento dos pedidos
				// resultantes do desmembramento.
				aLibCred[7] += SC5->C5_XVALFAT

				SC5->(dbSetOrder(1))  // C5_FILIAL, C5_NUM.

				// Se o valor resultante for igual, mantém a condição
				// da liberação de crédito anterior.
				lLibera := (aLibCred[6] >= aLibCred[7])

				// Grava status da liberação dos dois pedidos.
				If lLibera
					U_DebugMsg("PVDesmem - Liberando crédito dos pedidos...")

					// Grava status da liberação de crédito do pedido original.
					SC5->(dbSeek(xFilial() + cPedido, .F.))
					RecLock("SC5", .F.)
					SC5->C5_XCRDLIB := aLibCred[1]
					SC5->C5_XDTLIB  := aLibCred[2]
					SC5->C5_XUSRLIB := aLibCred[3]
					SC5->C5_XCONLIB := aLibCred[4]
					SC5->C5_XOBSLIB := aLibCred[5]
					SC5->C5_XVLRLIB := (aLibCred[6] / aLibCred[7]) * SC5->C5_XVALFAT
					SC5->C5_XVLDCRD := aLibCred[8]
					SC5->(msUnLock())

					// Grava status da liberação de crédito do pedido desmembrado.
					SC5->(dbSeek(xFilial() + cPedFilho, .F.))
					RecLock("SC5", .F.)
					SC5->C5_XCRDLIB := aLibCred[1]
					SC5->C5_XDTLIB  := aLibCred[2]
					SC5->C5_XUSRLIB := aLibCred[3]
					SC5->C5_XCONLIB := aLibCred[4]
					SC5->C5_XOBSLIB := aLibCred[5]
					SC5->C5_XVLRLIB := (aLibCred[6] / aLibCred[7]) * SC5->C5_XVALFAT
					SC5->C5_XVLDCRD := aLibCred[8]
					SC5->(msUnLock())

					DBSelectArea('SC9')
					SC9->(DBSetOrder(1))

					For nX := 1 to Len(aAutoSC6o)

						nPos := aScan(aAutoSC6o[nX], {|x| x[1] == 'AUTDELETA'})

						If nPos > 0 .And. aAutoSC6o[nX][nPos][2] == "S"
							If SC9->(DBSeek(xFilial('SC9')+cPedido+aAutoSC6o[nX][1][2]))
								While C9_FILIAL == xFilial('SC9') .And. C9_PEDIDO == cPedido .AND. C9_ITEM == aAutoSC6o[nX][1][2]

									RecLock('SC9', .F.)

										SC9->(DbDelete())

									SC9->(MsUnlock())
									SC9->(DBSkip())
								EndDo
							EndIf
						EndIf
					Next nX

					if ! lMantem
						U_HistPV(SC5->C5_NUM, "3", "Liberação de crédito", "Crédito liberado no pedido " + cPedido, SC5->C5_XUSRLIB)
					else
						U_HistPV(SC5->C5_NUM, "3", "Liberação de crédito", "Crédito liberado no pedido " + cPedido, cUsrLib)
					endIf
					U_DebugMsg("PVDesmem - Crédito dos pedidos OK")
				Endif
			Endif
		Endif

		// Limpa as funções fiscais.
		If MaFisFound()
			U_DebugMsg("PVDesmem - Limpando funções fiscais...")
			MaFisEnd()
		Endif

		If lMsErroAuto .or. !empty(cMsgErro)
			U_DebugMsg("PVDesmem - ** ERRO ** - Executado rollback...")

			If __lSX8
				RollBackSX8()
			Endif
			DisarmTransaction()
		Endif

		U_DebugMsg("PVDesmem - Processo finalizado", "F")

		End Transaction

		// Exibe mensagem de erro, se houver.
		If lMsErroAuto
			MostraErro()
		ElseIf !empty(cMsgErro)
			MsgAlert(cMsgErro, "Atenção")
			lMsErroAuto := .T.
		Else
			// Pega o email do operador do cliente do pedido.

			SA1->(dbSetOrder(1))  // A1_FILIAL, A1_COD, A1_LOJA.
			SA1->(msSeek(xFilial(, cFilPed) + SC5->(C5_CLIENTE + C5_LOJACLI), .F.))
			ZA7->(dbSetOrder(7))  // ZA7_FILIAL, ZA7_DEPVEN, ZA7_CLIENT, ZA7_LOJA, ZA7_OPERAD.
			If ZA7->(msSeek(xFilial(, cFilPed) + cDepVen + SA1->(A1_COD + A1_LOJA), .F.) .and. !empty(ZA7_OPERAD))
				SU7->(dbSetOrder(1))  // U7_FILIAL, U7_COD.
				If SU7->(msSeek(xFilial() + ZA7->ZA7_OPERAD, .F.))
					cTo := U_UserMail(SU7->U7_NREDUZ)[1]
					If !empty(cTo)
						cTo := rtrim(cTo) + "; "
					Endif
				Endif
			Endif

			cCliente := SA1->(A1_COD + "/" + A1_LOJA + " " + RTrim(A1_NOME))
			cAssunto := "Pedido (" + cFilAnt + "-" + rtrim(FWFilialName(nil, cFilAnt, 1)) + ") " + cPedido + " desmembrado -> " + cPedFilho + " (Cliente " + cCliente + ")"
			cMsgMail := U_Txt2HTML("O pedido " + cPedido + " foi desmembrado, gerando o pedido " + cPedFilho + ".")
			cMsgMail += U_Txt2HTML(CRLF + "Cliente " + cCliente)
			cMsgMail += U_Txt2HTML(CRLF + CRLF + "Pedido " + cPedido + CRLF)   + cItensPai
			cMsgMail += U_Txt2HTML(CRLF + CRLF + "Pedido " + cPedFilho + CRLF) + cItensFil
			cMsgMail := cMsgMail
			U_EnvEMail(nil, cTo, cAssunto, cMsgMail,, .T.)

			MsgInfo("Pedido desmembrado gerado: " + cPedFilho + "." + If(lBlqCon, " Pedido gerado com Bloqueio Comercial.", ""), "Atenção")
		Endif
	Else
		lMsErroAuto := .T.
	Endif
Endif

RestArea(aAreaSC5)
RestArea(aArea)

Return


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : HistPV
// Descrição: Grava histórico de interação do pedido de venda.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 28/01/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function HistPV(cPedido, cOcorr, cDescr, cObserv, cPUserName, dData, cHora)
Local aArea      := GetArea()
Local cQuery     := ""
Local cAliasTop  := ""
Local lSistema 	 := cOcorr = "3" .and. allTrim(cPUserName) == "Sistema"

Default cPedido    := SC5->C5_NUM
Default cDescr     := ""
Default cObserv    := ""
Default cPUserName := cUserName
Default dData      := Date()
Default cHora      := Time()

If Empty(cPUserName)
	cPUserName := "Sistema"
	lSistema := .T.
EndIf

cQuery := "select ZC1.R_E_C_N_O_ ZC1RecNo " + CRLF
cQuery += "from " + RetSQLName("ZC1") + " ZC1 with (noLock) " + CRLF
cQuery += "where ZC1.D_E_L_E_T_ = ' ' " + CRLF
cQuery += "and ZC1.ZC1_FILIAL = '" + xFilial("ZC1") + "' " + CRLF
cQuery += "and ZC1.ZC1_PEDIDO = '" + cPedido + "' " + CRLF
cQuery += "and ZC1.ZC1_OCOR   = '" + cOcorr + "' " + CRLF
cQuery += "and ZC1.ZC1_USUARI = '" + rtrim(cPUserName) + "' " + CRLF
cQuery += "and ZC1.ZC1_DESCR  = '" + rtrim(cDescr) + "' " + CRLF
if ! lSistema
	cQuery += "and ZC1.ZC1_DATA   = '" + dtos(dData) + "' " + CRLF
	cQuery += "and ZC1.ZC1_HORA   = '" + cHora + "' " + CRLF
endIf

cAliasTop := MPSysOpenQuery(cQuery)

If (cAliasTop)->(eof())
	// Ocorrências -> 1-Inclusão do pedido / 2-Alteração do pedido / 3-Crédito / 4-Logística / 5-Reserva / 8-Cancelamento do pedido / 9-Faturamento.
	RecLock("ZC1", .T.)
	ZC1->ZC1_FILIAL := xFilial("ZC1")
	ZC1->ZC1_PEDIDO := cPedido
	ZC1->ZC1_OCOR   := cOcorr
	ZC1->ZC1_USUARI := cPUserName
	ZC1->ZC1_DESCR  := cDescr
	ZC1->ZC1_DATA   := dData
	ZC1->ZC1_HORA   := cHora
	If ValType(cObserv) <> "U"
		ZC1->ZC1_OBS := cObserv
	Endif
	ZC1->(msUnLock())
Else
	If (lSistema .Or. cOcorr == '7') .and. UltHstPV(cPedido, cOcorr, cDescr, cObserv)
		// Ocorrências -> 1-Inclusão do pedido / 2-Alteração do pedido / 3-Crédito / 4-Logística / 5-Reserva / 8-Cancelamento do pedido / 9-Faturamento.
		RecLock("ZC1", .T.)
		ZC1->ZC1_FILIAL := xFilial("ZC1")
		ZC1->ZC1_PEDIDO := cPedido
		ZC1->ZC1_OCOR   := cOcorr
		ZC1->ZC1_USUARI := cPUserName
		ZC1->ZC1_DESCR  := cDescr
		ZC1->ZC1_DATA   := dData
		ZC1->ZC1_HORA   := cHora
		If ValType(cObserv) <> "U"
			ZC1->ZC1_OBS := cObserv
		Endif
		ZC1->(msUnLock())
	EndIf
Endif

(cAliasTop)->(dbCloseArea())

RestArea(aArea)

Return

//-------------------------------------------------------------------
/*/{Protheus.doc} UltHstPV
Retorna se pode gravar a nova linha de histórico. Se falso, é porque
o histórico que está vindo já existe e não precisa ser regravado.
@author  João Leão
@since   20/09/2019
@version 12.1.25
/*/
//-------------------------------------------------------------------
Static Function UltHstPV(cPedido, cOcorr, cDescr, cObserv)
Local lRet 		:= .T.
Local aArea		:= GetArea()
Local cQuery	:= ""
Local cAliasTop	:= ""

cQuery := "select top 1 convert(varchar(1024), convert(varbinary(1024), ZC1.ZC1_OBS)) OBS " + CRLF
cQuery += "from " + RetSQLName("ZC1") + " ZC1 with (noLock) " + CRLF
cQuery += "where ZC1.D_E_L_E_T_ = ' ' " + CRLF
cQuery += "and ZC1.ZC1_FILIAL = '" + xFilial("ZC1") + "' " + CRLF
cQuery += "and ZC1.ZC1_PEDIDO = '" + cPedido + "' " + CRLF
cQuery += "and ZC1.ZC1_OCOR   = '" + cOcorr + "' " + CRLF
cQuery += "and ZC1.ZC1_DESCR   = '" + cDescr + "' " + CRLF
cQuery += "order by ZC1.ZC1_DATA DESC, ZC1.ZC1_HORA DESC"
cAliasTop := MPSysOpenQuery(cQuery)

If (cAliasTop)->(!eof())
	If AllTrim(Upper(cObserv)) $ AllTrim(Upper((cAliasTop)->OBS))
		lRet := .F.
	EndIf
Endif

(cAliasTop)->(dbCloseArea())
RestArea(aArea)

Return lRet
// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : FHistAte
// Descrição: Abre tela de histórico do pedido.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 29/01/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function VHistPV(cAteFil, cAtendP, cPedidoP)
Local oDlgHist, oHistAtend, oGrBut
Local aCabec[0], aTam[0]
Local aSUAArea	:= SUA->(GetArea())

Local aHist      := {}
Local bOcor      := {|x| cOcor := x, nAt := aScan(aHPVOcor, {|x| x[1] == cOcor}), If(nAt > 0, aHPVOcor[nAt, 2], "")}

Local cQuery     := ""
Local cAliasTop  := ""
Local lPedido	 := .F.

Static aHPVOcor   := {}
If empty(aHPVOcor)
	aAdd(aHPVOcor, {"1", "Inclusão do pedido"})
	aAdd(aHPVOcor, {"2", "Alteração do pedido"})
	aAdd(aHPVOcor, {"3", "Crédito"})
	aAdd(aHPVOcor, {"4", "Logística"})
	aAdd(aHPVOcor, {"5", "Reserva de estoque"})
	aAdd(aHPVOcor, {"6", "Agendamento de Coleta"})
	aAdd(aHPVOcor, {"7", "Bloqueio por Regra"})
	aAdd(aHPVOcor, {"8", "Cancelamento do pedido"})
	aAdd(aHPVOcor, {"9", "Faturamento"})
Endif

// Histórico de atendimento.
If empty(cAtendP)
	Default cPedidoP := SC5->C5_NUM
Else
	SUA->(dbSetOrder(1))  // UA_FILIAL, UA_NUM.
	If SUA->(dbSeek(xFilial(, cAteFil) + cAtendP, .F.))
		//Tenta buscar o novo histórico do atendimento. João Leão 24/11/2021
		U_vOrcHistor(cAteFil, cAtendP, @aHist, .T.)
		If Empty(aHist)
			SU7->(dbSetOrder(1))  // U7_FILIAL, U7_COD.
			SU7->(msSeek(xFilial() + SUA->UA_OPERADO, .F.))
			aAdd(aHist, {dtoc(SUA->UA_EMISSAO) + "  " + SUA->UA_FIM, SU7->U7_NREDUZ, "Inclusão do orçamento", "", ""})
			If !empty(SUA->UA_CODCANC)
				aAdd(aHist, {Transform(SUA->UA_DATCANC, "@R 99/99/9999") + "  " + Transform(SUA->UA_HORCANC, "@R 99:99:99"), SUA->UA_XUSRCAN, "Orçamento perdido", "", MSMM(SUA->UA_CODCANC, TamSX3("UA_OBSCANC")[1])})
			Endif
		EndIf

		Default cPedidoP := SUA->UA_NUMSC5
	Endif
Endif

// Consuta histórico no WIS e atualiza o Protheus.
If !empty(cPedidoP)
	lPedido := .T.
	SC5->(dbSetOrder(1))  // C5_FILIAL, C5_NUM.
	If SC5->(dbSeek(xFilial(, cAteFil) + cPedidoP, .F.) .and. !empty(C5_XWIS))
		msAguarde({|| U_WISPVHst()}, "Aguarde", "Consultando WIS...", .F.)
	Endif

	// Histórico do pedido.
	ZC1->(dbSetOrder(1))  // ZC1_FILIAL, ZC1_PEDIDO, ZC1_DATA, ZC1_HORA.
	ZC1->(dbSeek(xFilial(, cAteFil) + cPedidoP, .F.))
	Do While ZC1->(!eof() .and. ZC1_FILIAL + ZC1_PEDIDO == xFilial(, cAteFil) + cPedidoP)
		ZC1->(aAdd(aHist, {dtoc(ZC1_DATA) + "  " + ZC1_HORA, ZC1_USUARI, Eval(bOcor, ZC1_OCOR), RTrim(ZC1_DESCR), SubStr(RTrim(ZC1_OBS),1,200)}))
		ZC1->(dbSkip())
	EndDo

	// Histórico de entrega (GFE)
	If !empty(SC5->C5_NOTA) .and. SC5->C5_NOTA != "XXX"
		cQuery := "select GWD.R_E_C_N_O_ GWDRecNo, GU5.R_E_C_N_O_ GU5RecNo " + CRLF

		cQuery += "from " + RetSQLName("GWD") + " GWD with (noLock) " + CRLF

		cQuery += "inner join " + RetSQLName("GWL") + " GWL on GWL.D_E_L_E_T_ = ' ' " + CRLF
		cQuery += "and GWL.GWL_FILIAL = '" + xFilial("GWL") + "' " + CRLF
		cQuery += "and GWL.GWL_NROCO  = GWD.GWD_NROCO " + CRLF

		cQuery += "inner join " + RetSQLName("GU5") + " GU5 on GU5.D_E_L_E_T_ = ' ' " + CRLF
		cQuery += "and GU5.GU5_FILIAL = '" + xFilial("GU5") + "' " + CRLF
		cQuery += "and GU5.GU5_CDTIPO = GWD.GWD_CDTIPO " + CRLF

		cQuery += "where GWD.D_E_L_E_T_ = ' ' " + CRLF
		cQuery += "and GWD.GWD_FILIAL = '" + xFilial("GWD") + "' " + CRLF
		cQuery += "and GWL.GWL_NRDC   = '" + SC5->C5_NOTA + "' " + CRLF
		cQuery += "and GWL.GWL_SERDC  = '" + SC5->C5_SERIE + "'" + CRLF

		cQuery += "order by GWD.GWD_DTOCOR, GWD.GWD_HROCOR "
		cAliasTop := MPSysOpenQuery(cQuery)

		Do While (cAliasTop)->(!eof())
			GWD->(dbGoTo((cAliasTop)->GWDRecNo))
			GU5->(dbGoTo((cAliasTop)->GU5RecNo))

			GWD->(aAdd(aHist, {dtoc(GWD_DTOCOR) + "  " + GWD_HROCOR, "", Eval(bOcor, "4"), Capital(GU5->GU5_DESC), RTrim(GWD_DSOCOR)}))
			(cAliasTop)->(dbSkip())
		EndDo
		(cAliasTop)->(dbCloseArea())
	Endif
Endif

// Se não houver nada, traz uma linha em branco.
If empty(aHist)
	aHist := {{"", "", "", "", ""}}
Endif

If lPedido .Or. !Empty(aHist)
	// Monta tela de entrada.
	DEFINE MSDIALOG oDlgHist TITLE "Histórico do atendimento/pedido" FROM 0, 0 TO 400, 950 PIXEL

	aCabec := {"Data/hora", "Usuário", "Ocorrência", "Descrição", "Observação"}
	aTam   := {60, 50, 70, 100, 200}
	oHistAtend := TWBrowse():New(0, 0, 0, 0,, aCabec, aTam, oDlgHist,,,,,,,,,,,,.F.,,.T.,,.F.)
	oHistAtend:Align := CONTROL_ALIGN_ALLCLIENT
	oHistAtend:SetArray(aHist)
	oHistAtend:bLine := {|| aHist[oHistAtend:nAt]}
	oHistAtend:nAt   := 1

	// Grupo dos botões.
	oGrBut := TScrollArea():New(oDlgHist, 0, 0, 15, 0, .F., .F.)
	oGrBut:Align := CONTROL_ALIGN_BOTTOM

	tButton():New(2, 438, "Sair", oGrBut, {|| oDlgHist:End()}, 35, 10,,,, .T.)

	ACTIVATE MSDIALOG oDlgHist CENTERED
Else
	U_vOrcHistor(cAteFil, cAtendP)
EndIf

RestArea(aSUAArea)

Return


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : UATransp
// Descrição: Retorna a transportadora no TGV (gatilho do UA_TRANSP).
// Retorno  : Código da transportadora.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 08/01/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function UATransp()
Local cRet       := ""

// Verifica se o gatilho foi chamado pelo TGV.
If IsInCallStack("U_PROMA030")
	SA4->(dbSetOrder(1))  // A4_FILIAL, A4_COD.
	SA4->(msSeek(xFilial() + M->A1_TRANSP, .F.))
	If !empty(M->A1_TRANSP) .and. M->A1_TRANSP <> '000999' .and. M->A1_TRANSP == SA4->A4_COD
		cRet := SA4->A4_COD
	ElseIf M->UA_TPFRETE = "C" .and. cFilAnt == MATRIZ
		cRet := Posicione("CC2", 4, xFilial("CC2") + M->(UA_ESTE + UA_MUNE), "CC2_XTRANS")
	Else
		cRet := CriaVar("UA_TRANSP", .T.)
	Endif
ElseIf Type("M->UA_TRANSP") == "C" .and. !empty(M->UA_TRANSP)
	cRet := M->UA_TRANSP
Else
	cRet := CriaVar("UA_TRANSP", .T.)
Endif

Return cRet


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : Cliente
// Descrição: Retorna conteúdo de um campo do cliente, de acordo com parâmetros.
// Retorno  : Conteúdo do campo passado.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 27/11/13 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function Cliente(lProspec, cCliente, cLoja, cGrupo, cCampo)
Local xRet
Local cAlias     := Alias()
Local aArea      := {}

Local cQuery     := ""
Local cAliasTop  := ""

Default cCampo     := ""

// Salva áreas de trabalho.
If empty(cAlias)
	cAlias := "SC5"
	dbSelectArea(cAlias)
Endif
aArea := sGetArea()

If lProspec
	// Acerta o nome do campo.
	If cCampo = "A1_"
		cCampo := "US_" + SubStr(cCampo, 4)
	Endif

	cQuery := "select top 1 SUS.R_E_C_N_O_ SUSRecNo "
	cQuery += "from " + RetSQLName("SUS") + " SUS with (noLock) "
	cQuery += "where SUS.D_E_L_E_T_ = ' ' "
	cQuery += "and SUS.US_FILIAL  = '" + xFilial("SUS") + "' "
	cQuery += "and SUS.US_COD     = '" + cCliente + "' "
	If !empty(cLoja)
		cQuery += "and SUS.US_LOJA    = '" + cLoja + "' "
	ElseIf SUS->(FieldPos("US_MSBLQL")) > 0
		cQuery += "and SUS.US_MSBLQL <> '1' "
	Endif
	cQuery += "order by SUS.US_COD, SUS.US_LOJA "
	cAliasTop := MPSysOpenQuery(cQuery)

	If (cAliasTop)->(eof())
		If empty(cCampo)
			xRet := .F.
		Else
			xRet := CriaVar(cCampo, .F.)
		Endif
	Else
		SUS->(dbGoTo((cAliasTop)->SUSRecNo))
		If empty(cCampo)
			xRet := .T.
		Else
			xRet := SUS->(&cCampo)
		Endif
	Endif
	(cAliasTop)->(dbCloseArea())
Else
	// Acerta o nome do campo.
	If cCampo = "US_"
		cCampo := "A1_" + SubStr(cCampo, 4)
	Endif

	cQuery := "select top 1 SA1.R_E_C_N_O_ SA1RecNo "
	cQuery += "from " + RetSQLName("SA1") + " SA1 with (noLock) "
	cQuery += "where SA1.D_E_L_E_T_ = ' ' "
	cQuery += "and SA1.A1_FILIAL  = '" + xFilial("SA1") + "' "
	cQuery += "and SA1.A1_COD     = '" + cCliente + "' "
	If !empty(cLoja)
		cQuery += "and SA1.A1_LOJA    = '" + cLoja + "' "
	Else
		If !empty(cGrupo)
			cQuery += "and SA1.A1_XGRUPO  = '" + cGrupo + "' "
		Endif
		cQuery += "and SA1.A1_MSBLQL <> '1' "
	Endif
	cQuery += "order by SA1.A1_COD, SA1.A1_LOJA, SA1.A1_XGRUPO "
	cAliasTop := MPSysOpenQuery(cQuery)

	If (cAliasTop)->(eof())
		If empty(cCampo)
			xRet := .F.
		Else
			SA1->(dbGoTo(0))
			xRet := CriaVar(cCampo, .F.)
		Endif
	Else
		SA1->(dbGoTo((cAliasTop)->SA1RecNo))
		If empty(cCampo)
			xRet := .T.
		Else
			xRet := SA1->(&cCampo)
		Endif
	Endif
	(cAliasTop)->(dbCloseArea())
Endif

// Restaura areas de trabalho.
(cAlias)->(sRestArea(aArea))
dbSelectArea(cAlias)

Return xRet

// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : Invent
// Descrição: Cadastra a digitação do inventário, sem lotes.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 11/05/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function Invent(cDocumento, dDtCont, cProduto, cArmazem, nQuant, lVerSB2Sld)
Local nX

Local nSalLote   := 0
Local lPriLote   := .T.
Local aLotes     := {}

Local aAutoSB7   := {}
Local aAutoAux   := {}

Local cQuery     := ""
Local cAliasTop  := ""

Private lMsErroAuto := .F.

cDocumento := PadR(cDocumento, nTamDoc)
cProduto   := PadR(cProduto,   nTamProd)
cArmazem   := PadR(cArmazem,   nTamLoc)
Default lVerSB2Sld := .F.

U_DebugMsg("Importando " + cProduto + "/" + cArmazem + " - qtde " + cValToChar(nQuant))

SB7->(dbSetOrder(3))  // B7_FILIAL, B7_DOC, B7_COD, B7_LOCAL.
If SB7->(msSeek(xFilial() + cDocumento + cProduto + cArmazem, .F.))
	U_DebugMsg("   Skip - SB7 já importado.")
Else
	SB2->(dbSetOrder(1))   // B2_FILIAL, B2_COD, B2_LOCAL.
	SB2->(msSeek(xFilial() + cProduto + cArmazem, .F.))
	If nQuant == 0 .and. SB2->(eof() .or. SB2->B2_QATU = 0)
		U_DebugMsg("   Skip - Quantidade zerada.")
	Else
		// Verifica se existe SB2 criado para esse produto.
		If SB2->(eof())
			CriaSB2(cProduto, cArmazem)
		Endif

		// Acerta o inventário somente se houver diferença de estoque.
		If lVerSB2Sld .and. SB2->B2_QATU == nQuant
			U_DebugMsg("   Skip - SB2 OK.")
		Else
			// Cria o registro de contagem de inventário.
			aLotes   := {}
			aAutoAux := {}
			aAdd(aAutoAux, {"B7_DOC",   cDocumento, nil})
			aAdd(aAutoAux, {"B7_DATA",  dDtCont,    nil})
			aAdd(aAutoAux, {"B7_COD",   cProduto,   ".T."})
			aAdd(aAutoAux, {"B7_LOCAL", cArmazem,   nil})

			// Se o produto controla lote, busca os lotes disponíveis.
			SB1->(dbSetOrder(1))  // B1_FILIAL, B1_COD.
			SB1->(msSeek(xFilial() + cProduto, .F.))
			If SB1->B1_RASTRO $ "L|S"
				cQuery := "select SB8.R_E_C_N_O_ SB8RecNo, SB8.B8_SALDO SALDO " + CRLF
				cQuery += "from " + RetSQLName("SB8") + " SB8 with (noLock) " + CRLF
				cQuery += "where SB8.D_E_L_E_T_ = ' ' " + CRLF
				cQuery += "and SB8.B8_FILIAL  = '" + xFilial("SB8") + "' " + CRLF
				cQuery += "and SB8.B8_PRODUTO = '" + cProduto + "' " + CRLF
				cQuery += "and SB8.B8_LOCAL   = '" + cArmazem + "' " + CRLF
				cQuery += "order by SB8.B8_SALDO desc, SB8.B8_DATA " + CRLF
				cAliasTop := MPSysOpenQuery(cQuery)

				If (cAliasTop)->(eof())
					// Se não existe lote que receberá a quantidade inventariada, cria um novo.
					CriaSB8(cDocumento, cProduto, cArmazem)

					aAutoSB7 := aClone(aAutoAux)
					aAdd(aAutoSB7, {"B7_QUANT",   nQuant, nil})
					aAdd(aAutoSB7, {"B7_LOTECTL", SB8->B8_LOTECTL, nil})
					aAdd(aAutoSB7, {"B7_NUMLOTE", SB8->B8_NUMLOTE, nil})
					aAdd(aAutoSB7, {"B7_DTVALID", SB8->B8_DTVALID, nil})
					aAdd(aLotes, aAutoSB7)
				Else
					nSalLote := 0
					lPriLote := .T.
					Do While (cAliasTop)->(!eof())
						SB8->(dbGoTo((cAliasTop)->SB8RecNo))

						If (lPriLote .or. SALDO <> 0)
							aAutoSB7 := aClone(aAutoAux)
							aAdd(aAutoSB7, {"B7_QUANT",   max(0, SB8->B8_SALDO),   nil})
							aAdd(aAutoSB7, {"B7_LOTECTL", SB8->B8_LOTECTL, nil})
							aAdd(aAutoSB7, {"B7_NUMLOTE", SB8->B8_NUMLOTE, nil})
							aAdd(aAutoSB7, {"B7_DTVALID", SB8->B8_DTVALID, nil})
							aAdd(aLotes, aAutoSB7)
							nSalLote += max(0, SB8->B8_SALDO)
						Endif

						lPriLote := .F.
						(cAliasTop)->(dbSkip())
					EndDo

					nDifInv := (nQuant - nSalLote)
					If nDifInv = 0
						// Se o estoque bateu, apenas informa os lotes como estão.
					ElseIf nDifInv > 0
						// Se for inventário positivo...
						// ... joga a diferença no primeiro.
						aLotes[1, 5, 2] += nDifInv
						aLotes := aSize(aLotes, 1)
					ElseIf nDifInv < 0
						// ... senão vai subtraindo do lote menor até o maior.
						aLotesAux := {}
						For nX := len(aLotes) to 1 step -1
							If aLotes[nX, 5, 2] > -nDifInv
								aLotes[nX, 5, 2] += nDifInv
								aAdd(aLotesAux, aClone(aLotes[nX]))
								Exit
							Else
								nDifInv += aLotes[nX, 5, 2]
								aLotes[nX, 5, 2] := 0
								aAdd(aLotesAux, aClone(aLotes[nX]))
							Endif
						Next nX
						aLotes := aLotesAux
					Endif
				Endif
				(cAliasTop)->(dbCloseArea())
			Else
				aAutoSB7 := aClone(aAutoAux)
				aAdd(aAutoSB7, {"B7_QUANT",   nQuant, nil})
				aAdd(aAutoSB7, {"B7_LOTECTL", CriaVar("B8_LOTECTL"), nil})
				aAdd(aAutoSB7, {"B7_NUMLOTE", CriaVar("B8_NUMLOTE"), nil})
				aAdd(aAutoSB7, {"B7_DTVALID", stod(""), nil})
				aAdd(aLotes, aAutoSB7)
			Endif

			// Gera inventário por lotes.
			For nX := 1 to len(aLotes)
				aAutoSB7 := aLotes[nX]
				aAdd(aAutoSB7, {"B7_LOCALIZ", CriaVar("B7_LOCALIZ"), nil})
				aAdd(aAutoSB7, {"B7_NUMSERI", CriaVar("B7_NUMSERI"), nil})
				aAdd(aAutoSB7, {"B7_TPESTR",  CriaVar("B7_TPESTR"),  nil})

				msExecAuto({|x, y| MATA270(x, y)}, aAutoSB7, .T.)
				U_DebugMsg("   Lote " + aAutoSB7[6, 2] + " - qtde " + cValToChar(aAutoSB7[5, 2]))

				If lMsErroAuto
					Exit
				Endif
			Next nX

			If lMsErroAuto
				MostraErro()
			Endif
		Endif
	Endif
Endif

Return !lMsErroAuto


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Eduardo Nakamatu / Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : CriaSB8
// Descrição:
// Retorno  :
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 10/09/13 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
Static Function CriaSB8(cDoc, cProduto, cLocal)
Local nOpcAuto   := 0
Local aAutoCD5   := {}
Local cPrfLot    := AllTrim(SuperGetMV("PC_PRFLOTN",, "LOTE"))
Local nTamLot    := TamSx3("B8_LOTECTL")[1]
Local cLoteCtl   := cPrfLot + StrZero(1, nTamLot - len(cPrfLot))

aAdd(aAutoCD5, {"D5_DOC",     cDoc, nil})
aAdd(aAutoCD5, {"D5_DATA",    dDataBase, nil})
aAdd(aAutoCD5, {"D5_PRODUTO", cProduto,  ".T."})
aAdd(aAutoCD5, {"D5_LOCAL",   cLocal, nil})
aAdd(aAutoCD5, {"D5_QUANT",   0, nil})
aAdd(aAutoCD5, {"D5_LOTECTL", cLoteCtl, nil})
aAdd(aAutoCD5, {"D5_DTVALID", stod("20451231"), nil})

nOpcAuto := 3  // 3-Inclusão / 4-Alteração.
msExecAuto({|x, y| MATA390(x, y)}, aAutoCD5, 3)

If !lMsErroAuto
	SB8->(dbSetOrder(1))  // B8_FILIAL, B8_PRODUTO, B8_LOCAL, B8_DTVALID, B8_LOTECTL, B8_NUMLOTE.
	If SB8->(!msSeek(xFilial() + cProduto + cLocal, .F.))
		lMsErroAuto := .T.
		UserException("Erro ao criar lote para produto [" + cProduto + "] - local [" + cLocal + "]")
	Endif
Endif

Return


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Materiais
// Função   : CodSA1
// Descrição: Retorna próximo número de cliente.
// Retorno  :
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 17/09/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function CodSA1()
	Local lMVCCustomer	:= SuperGetMv("MV_MVCSA1",,.F.) .AND. !IsInCallStack("U_PROMA030") .AND. IsInCallStack("CRMA980")
	Local cCodigo    := If(lMVCCustomer,FwFldGet(A1_COD),M->A1_COD)

	// Busca o código do cliente, caso o usuário solicite.
	If cCodigo = 'A'
		cCodigo := ""
		SA1->(dbSetOrder(1))  // A1_FILIAL, A1_COD, A1_LOJA.
		Do While empty(cCodigo) .or. SA1->(msSeek(xFilial() + cCodigo, .F.))
			If !empty(cCodigo)
				SA1->(ConfirmSX8())
			Endif
			cCodigo := GetSXENum("SA1", "A1_COD")
		EndDo
	Endif
Return cCodigo
// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Materiais
// Função   : MarcaCli
// Descrição: Retorna a marca do cliente baseado na razão social.
// Retorno  :
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 21/09/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function MarcaCli()
Local cMarca     := ""
Local aNome      := StrToKarr(StrTran(M->A1_NOME, ".", " "), " ")
Local bInitial   := {|cNome| If(len(cNome) > 1 .and. (len(cNome) > 2 .or. !(lower(SubStr(cNome, 2, 1)) $ "aeiou")), left(cNome, 1) + ".", "")}
Local nX

For nX := 1 to len(aNome) - 1
	cMarca += Eval(bInitial, aNome[nX])
Next nX
If M->A1_PESSOA = "F"
	cMarca += Eval(bInitial, aTail(aNome))
Else
	cMarca += aTail(aNome)
Endif

Return cMarca


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : FAltSA1
// Descrição: Função para alterar dados do cliente.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 24/06/13 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function FAltSA1(lProspect)
Local aArea      := GetArea()
Local cFunName   := FunName()
Local cAlias     := ""

Private INCLUI     := .F.
Private ALTERA     := .T.

Default lProspect := .F.

If lProspect
	cAlias := "SUS"
Else
	cAlias := "SA1"
Endif

dbSelectArea(cAlias)
If !eof()
	If lProspect
		// Configura o nome da função para funcionar o mashup.
		SetFunName("TMKA260")

		// Abre tela padrão do sistema de alteração do prospect.
		Tk260Altera(cAlias, RecNo(), 4)
	Else
		// Variáveis usadas pelo A030Altera().
		Private aMemos     := {{"A1_CODMARC", "A1_VM_MARC"}, {"A1_OBS", "A1_VM_OBS"}}
		Private bFiltraBrw := {|| nil}
		Private aRotina, aRotAuto

		// Configura o nome da função para funcionar o mashup.
		SetFunName("MATA030")

		// Abre tela padrão do sistema de alteração de clientes.
		A030Altera("SA1", RecNo(), 4)
	Endif

	// Configura nome da função.
	SetFunName(cFunName)
Else
	MsgAlert("Informe um código válido", If(lProspect, "Prospect", "Cliente") + " não cadastrado.")
Endif

RestArea(aArea)

Return


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : PedMin
// Descrição: Calcula o valor mínimo de pedido para um cliente.
// Retorno  : Matriz:
//          :    1 - Numérico, com o valor mínimo.
//          :    2 - Observação.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 19/04/16 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function PedMin(cCliente, cLoja, lProspect, cTpFrete, nValFrete, cCondPag, cTipoFat)
Local nRet		:= 0
Local cObserv	:= ""

Local nPedMin	:= 0
Local nFrtMin	:= 40
Local cEstE		:= ""
Local cZona		:= ""

Local cContrato	:= ""
Local lVlrMin	:= SuperGetMv("PC_VLRMIN", NIL, .F.)		//Tratamento para Valor Minimo Frete CIF


If lVlrMin .AND. M->UA_TPFRETE == "C" .AND. Empty(M->UA_XIDVTV3) .AND. !Left(M->UA_XPEDLV, 2) == "JC"
	// Verifica se o cliente tem contrato ativo.
	If !lProspect
		U_MA020Ctr(cCliente, cLoja,,, @cContrato,, .T.)
	Endif

	If Empty(cContrato)
		//Tratamento Valor Minimo
		If !Empty(M->UA_XCDMUNE) .AND. !Empty(M->UA_ESTE)
			nPedMin := U_MinFrete(M->UA_ESTE, M->UA_XCDMUNE)
		Else
			nPedMin := 0
		EndIf
	Else
		nPedMin := Posicione("ZA1", 1, xFilial("ZA1") + cContrato, "ZA1_PEDMIN")
	EndIf

	nRet := nPedMin
Else
	// Valida somente matriz.
	If cFilAnt == MATRIZ
		If lProspect
			SUS->(dbSetOrder(1))  // US_FILIAL, US_COD, US_LOJA.
			SUS->(msSeek(xFilial() + cCliente + cLoja, .F.))
			cEstE := SUS->US_EST
			cZona := ""
		Else
			SA1->(dbSetOrder(1))  // A1_FILIAL, A1_COD, A1_LOJA.
			SA1->(msSeek(xFilial() + cCliente + cLoja, .F.))
			cEstE := SA1->A1_ESTE
			cZona := SA1->A1_XTRZONA
		Endif

		// Verifica se foi cobrado frete do cliente.
		If nValFrete >= nFrtMin
			nRet := nBZPedMin
		Else
			// Se for cliente retira.
			If cTipoFat = "3"  // 1=Prometido para / 2=Programado / 3=Cliente retira.
				SE4->(dbSetOrder(1))  // E4_FILIAL, E4_CODIGO.
				SE4->(msSeek(xFilial() + cCondPag, .F.))
				If SE4->E4_DESCRI = "A VISTA"
					nPedMin := 200
				Else
					nPedMin := 300
				Endif
			Else
				// Verifica se o cliente tem contrato ativo.
				If !lProspect
					U_MA020Ctr(cCliente, cLoja,,, @cContrato,, .T.)
				Endif
				If !empty(cContrato)
					nPedMin := Posicione("ZA1", 1, xFilial("ZA1") + cContrato, "ZA1_PEDMIN")
					cObserv := "Contrato " + cContrato + " - Pedido mínimo: R$ " + LTrim(Transform(nPedMin, "@E 999,999,999.99"))
				Else
					If cTpFrete == "F"
						// Se for frete FOB, o pedido mínimo é o geral.
						nPedMin := nBZPedMin
					ElseIf cEstE <> "SP"
						nPedMin := 1200
						cObserv := "Cliente com entrega em " + cEstE + " - Pedido mínimo: R$ " + LTrim(Transform(nPedMin, "@E 999,999,999.99")) + If(nFrtMin = 0, "" , ", ou frete de R$ " + LTrim(Transform(nFrtMin, "@E 999,999,999.99")))
					ElseIf empty(cZona)
						nPedMin := 1200
						cObserv := "Cliente sem zona cadastrada - Pedido mínimo: R$ " + LTrim(Transform(nPedMin, "@E 999,999,999.99")) + If(nFrtMin = 0, "" , ", ou frete de R$ " + LTrim(Transform(nFrtMin, "@E 999,999,999.99")))
					Else
						If cZona == "001"
							nPedMin := nBZPedMin
						ElseIf cZona == "002"
							nPedMin := 800
						ElseIf cZona == "003"
							nPedMin := nBZPedMin
						ElseIf cZona == "004"
							nPedMin := 800
						ElseIf cZona == "005"
							nPedMin := 800
						ElseIf cZona == "006"
							nPedMin := 800
						ElseIf cZona == "007"
							nPedMin := nBZPedMin
						Else
							nPedMin := 1200
						Endif
						cObserv := "Cliente zona " + cZona + " - Pedido mínimo: R$ " + LTrim(Transform(nPedMin, "@E 999,999,999.99")) + If(nFrtMin = 0, "" , ", ou frete de R$ " + LTrim(Transform(nFrtMin, "@E 999,999,999.99")))
					Endif
				Endif
			Endif
			nRet := nPedMin
		Endif
	Else
		nRet := nBZPedMin
	Endif

	If empty(cObserv)
		cObserv := "Pedido mínimo: R$ " + LTrim(Transform(nRet, "@E 999,999,999.99"))
	Endif
EndIf


Return {nRet, cObserv}


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : FatMin
// Descrição: Verifica se o pedido atingiu o valor mínimo de faturamento.
// Retorno  : Lógico, indicando se o pedido está OK.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 08/10/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function FatMin(lPedido, cMsg)
Local lRet       := .T.
Local nPedMin    := 0
Local cObserv    := ""

Local cCliente   := ""
Local cLoja      := ""
Local cTpFrete   := ""
Local nValFrete  := 0

Local cCondPag   := ""
Local cTipoFat   := ""
Local nValFat    := 0
Local lNetSupr   := .F.

Default lPedido    := .T.

// Dados do pedido ou orçamento.
If lPedido
	cCliente  := SC5->C5_CLIENTE
	cLoja     := SC5->C5_LOJACLI
	If empty(SC5->C5_REDESP)
		cTpFrete  := SC5->C5_TPFRETE
	Else
		cTpFrete  := SC5->C5_TFRDP1
	Endif
	nValFrete := SC5->C5_FRETE

	cCondPag  := SC5->C5_CONDPAG
	cTipoFat  := SC5->C5_XTPPREV
	nValFat   := SC5->C5_XVALFAT
	lNetSupr  := !empty(SC5->C5_XIDVTV3)
Else
	cCliente  := M->UA_CLIENTE
	cLoja     := M->UA_LOJA
	cTpFrete  := M->UA_TPFRETE
	nValFrete := M->UA_FRETE

	cCondPag  := M->UA_CONDPG
	cTipoFat  := M->UA_XTPPREV
	nValFat   := MaFisRet(nil, "NF_BASEDUP")
	lNetSupr  := !empty(SUA->UA_XIDVTV3)
Endif

// Pedidos/orçamentos NetSuprimentos não param por valor mínimo.
If lNetSupr
	lRet := .T.
Else
	// Busca o valor mínimo do pedido para faturamento.
	aPedMin := U_PedMin(cCliente, cLoja, .F., cTpFrete, nValFrete, cCondPag, cTipoFat)
	nPedMin := aPedMin[1]
	cObserv := aPedMin[2]

	// Verifica se o valor é maior que o pedido mínimo.
	If nValFat < nPedMin
		lRet := .F.
		cMsg := cObserv
	Endif
Endif

Return lRet


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : EMailPV
// Descrição: Busca e-mail dos usuários envolvidos no pedido de venda posicionado.
// Retorno  : Array ou caractere, com os e-mails relacionados.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 02/12/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function EMailPV(nTipo, lOperador, lFinanceiro, lCarteira, lLogistica)
Local xRet
Local aTo        := {}
Local aEMailAux  := {}
Local nX

Static cUserCart  := SuperGetMV("PC_USERCPV",, "")
Static cEmailSL   := SuperGetMV("PC_MAILSL",, "")

Default nTipo       := 1  // 1-Retorna matriz / 2-Retorna caractere.
Default lOperador   := .F.
Default lFinanceiro := .F.
Default lCarteira   := .F.
Default lLogistica  := .F.

If nTipo == 1  // 1-Retorno matriz.
	xRet := {}
ElseIf nTipo == 2  // 2-Retorna caractere.
	xRet := ""
Endif

// Pega o e-mail do operador do cliente do pedido.
If lOperador
	// Pega o email do operador do cliente do pedido.
	SA1->(dbSetOrder(1))  // A1_FILIAL, A1_COD, A1_LOJA.
	SA1->(msSeek(xFilial() + SC5->(C5_CLIENTE + C5_LOJACLI), .F.))
	ZA7->(dbSetOrder(7))  // ZA7_FILIAL, ZA7_DEPVEN, ZA7_CLIENT, ZA7_LOJA, ZA7_OPERAD.
	ZA7->(dbSeek(xFilial(, SC5->C5_FILIAL) + SC5->(C5_XDEPVEN + C5_CLIENTE + C5_LOJACLI), .F.))
	SU7->(dbSetOrder(1))  // U7_FILIAL, U7_COD.
	If SU7->((msSeek(xFilial() + ZA7->ZA7_OPERAD, .F.) .and. !empty(U7_NREDUZ)) .or. (msSeek(xFilial() + SC5->C5_XOPER, .F.) .and. !empty(U7_NREDUZ)))
		// Tenta pegar o e-mail do operador da ficha do cliente, senão pega do operador que incluiu o pedido.
		aAdd(aTo, U_UserMail(SU7->U7_NREDUZ)[1])
	Endif
Endif

// Pega o e-mail do analista financeiro do pedido.
If lFinanceiro
	If !empty(SC5->C5_XUSRLIB)
		aAdd(aTo, U_UserMail(SC5->C5_XUSRLIB)[1])
	Endif
Endif

// Pega e-mail dos operadores da carteira de pedidos.
If lCarteira
	If !empty(cUserCart)
		aEMailAux := StrToKarr(StrTran(cUserCart, ",", ";"), ";")
	Endif
	For nX := 1 to len(aEMailAux)
		aAdd(aTo, U_UserMail(aEMailAux[nX])[1])
	Next nX
Endif

// Pega e-mail do supervisor de logística.
If lLogistica
	If !empty(cEmailSL)
		aEMailAux := StrToKarr(StrTran(cEmailSL, ",", ";"), ";")
	Endif
	For nX := 1 to len(aEMailAux)
		aAdd(aTo, U_UserMail(aEMailAux[nX])[1])
	Next nX
Endif

// Verifica se todos os e-mails são válidos.
For nX := 1 to len(aTo)
	If "@" $ aTo[nX] .and. "." $ aTo[nX] .and. IsAlpha(lower(left(aTo[nX], 1)))
		If nTipo == 1  // 1-Retorno matriz.
			aAdd(xRet, aTo[nX])
		ElseIf nTipo == 2  // 2-Retorna caractere.
			xRet += aTo[nX] + "; "
		Endif
	Endif
Next nX

Return xRet


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : FEvolCli
// Descrição: Monta a tela de evolução do cliente.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 18/06/13 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function FEvolCli(cCliente, cLoja, cGrupo, lMostra)
Local aFatur     := {0, 0}
Local oDialog, oGrpSup, oGrpBut, oGraphic, oSayTot1, oSayTot2
Local nSerie1, aSerie1[12], nSerie2, aSerie2[12]
Local nX

Local cQuery     := ""
Local cQuerySD1  := ""
Local cQuerySD2  := ""
Local cAliasTop  := ""

Local aParamBox[0], aRetParam[0]
Local aMes       := Array(12)
Local dDtBase    := dDataBase
Local nMesAtu    := 0
Local nAnoIni    := 0
Local cDataDe    := ""
Local cDataAte   := ""
Local nMes       := 0
Local cNomeCli   := ""
Local nPrecis    := 0
Local nTamNumero := 0
Local nRangeY    := 0
Local nValorMax  := 0
Local nDiv       := 0
Local cDiv       := ""
Local cFoot      := ""

Local aRodape    := {"", ""}
Local nValFat    := 0
Local nValDev    := 0
Local nQtdNF     := 0
Local nCusto     := 0
Local nPrcLiq    := 0
Local oPanel     := NIL

Default cGrupo    := CriaVar("A1_XGRUPO", .F.)
Default lMostra   := .T.

aAdd(aParamBox, {1, "Cliente",   cCliente,,, "SA1", ".F.", 45, .F.})
aAdd(aParamBox, {1, "Loja",      cLoja,,,,       If(lMostra, "empty(mv_par03)", ".F."), 20, .F.})
aAdd(aParamBox, {1, "Grupo",     cGrupo, "@!",,, If(lMostra, "empty(mv_par02)", ".F."), 15, .F.})
aAdd(aParamBox, {1, "Data base", dDtBase,,,,, 50, .F.})
If ParamBox(aParamBox, "Informe os parâmetros - evolução do cliente", @aRetParam,,,,,,,, .F., .F.)
	cLoja      := aRetParam[2]
	cGrupo     := aRetParam[3]
	dDtBase    := aRetParam[4]
	nMesAtu    := Month(dDtBase)
	nAnoIni    := Year(dDtBase)
	cDataDe    := dtos(MonthSub(FirstDay(dDtBase), 11))
	cDataAte   := dtos(dDtBase)

	// Busca o faturamento do cliente no período.
	cQuerySD2 := "from " + RetSQLName("SD2") + " SD2 " + CRLF
	If !empty(cGrupo)
		cQuerySD2 += "inner join " + RetSQLName("SA1") + " SA1 on SA1.D_E_L_E_T_ = ' ' " + CRLF
		cQuerySD2 += "and SA1.A1_FILIAL  in ('', SD2.D2_FILIAL) " + CRLF
		cQuerySD2 += "and SA1.A1_COD     = SD2.D2_CLIENTE " + CRLF
		cQuerySD2 += "and SA1.A1_LOJA    = SD2.D2_LOJA " + CRLF
		cQuerySD2 += "and SA1.A1_XGRUPO  = '" + cGrupo + "' " + CRLF
	Endif
	cQuerySD2 += "inner join " + RetSQLName("SF4") + " SF4 on SF4.D_E_L_E_T_ = ' ' " + CRLF
	cQuerySD2 += "and SF4.F4_FILIAL  in ('', SD2.D2_FILIAL) " + CRLF
	cQuerySD2 += "and SF4.F4_CODIGO  = SD2.D2_TES " + CRLF
	cQuerySD2 += "and SF4.F4_DUPLIC  = 'S' " + CRLF
	cQuerySD2 += "where SD2.D_E_L_E_T_ = ' ' " + CRLF
	cQuerySD2 += "and SD2.D2_TIPO    not in ('D', 'B') " + CRLF
	cQuerySD2 += "and SD2.D2_CLIENTE = '" + cCliente + "' " + CRLF
	If !empty(cLoja)
		cQuerySD2 += "and SD2.D2_LOJA    = '" + cLoja + "' " + CRLF
	Endif
	cQuerySD2 += "and SD2.D2_EMISSAO between '" + cDataDe + "' and '" + cDataAte + "' " + CRLF

	cQuery := "select left(SD2.D2_EMISSAO, 6) MES, sum(SD2.D2_VALBRUT) VALOR, count(distinct SD2.D2_FILIAL + SD2.D2_EMISSAO + SD2.D2_DOC + SD2.D2_SERIE + SD2.D2_CLIENTE + SD2.D2_LOJA) NOTAS, " + CRLF
	cQuery += "sum(SD2.D2_CUSTO1) CUSTO, sum(SD2.D2_TOTAL - SD2.D2_VALICM - SD2.D2_VALIMP5 - SD2.D2_VALIMP6 - SD2.D2_ICMSCOM - SD2.D2_DIFAL - SD2.D2_VFCPDIF) VLRLIQ " + CRLF
	cQuery += cQuerySD2
	cQuery += "group by left(SD2.D2_EMISSAO, 6) " + CRLF
	cQuery += "order by 1 " + CRLF
	msAguarde({|| cAliasTop := MPSysOpenQuery(cQuery)}, "Aguarde", "Verificando faturamento...", .F.)

	aFill(aSerie1, 0)
	Do While (cAliasTop)->(!eof())

		nMes := val(SubStr((cAliasTop)->MES, 5, 2))
		aSerie1[nMes] := (cAliasTop)->VALOR
		nValorMax := max(nValorMax, aSerie1[nMes])
		nValFat   += (cAliasTop)->VALOR
		nCusto    += (cAliasTop)->CUSTO
		nPrcLiq   += (cAliasTop)->VLRLIQ
		nQtdNF    += (cAliasTop)->NOTAS
		aFatur[1] += (cAliasTop)->VALOR

		(cAliasTop)->(dbSkip())
	EndDo
	(cAliasTop)->(dbCloseArea())

	// Busca as devoluções do cliente no período.
	cQuerySD1 := "from " + RetSQLName("SD1") + " SD1 " + CRLF
	If !empty(cGrupo)
		cQuerySD1 += "inner join " + RetSQLName("SA1") + " SA1 on SA1.D_E_L_E_T_ = ' ' " + CRLF
		cQuerySD1 += "and SA1.A1_FILIAL  in ('', SD1.D1_FILIAL) " + CRLF
		cQuerySD1 += "and SA1.A1_COD     = SD1.D1_FORNECE " + CRLF
		cQuerySD1 += "and SA1.A1_LOJA    = SD1.D1_LOJA " + CRLF
		cQuerySD1 += "and SA1.A1_XGRUPO  = '" + cGrupo + "' " + CRLF
	Endif
	cQuerySD1 += "inner join " + RetSQLName("SF4") + " SF4 on SF4.D_E_L_E_T_ = ' ' " + CRLF
	cQuerySD1 += "and SF4.F4_FILIAL  in ('', SD1.D1_FILIAL) " + CRLF
	cQuerySD1 += "and SF4.F4_CODIGO  = SD1.D1_TES " + CRLF
	cQuerySD1 += "and SF4.F4_DUPLIC  = 'S' " + CRLF
	cQuerySD1 += "where SD1.D_E_L_E_T_ = ' ' " + CRLF
	cQuerySD1 += "and SD1.D1_TIPO    = 'D' " + CRLF
	cQuerySD1 += "and SD1.D1_FORNECE = '" + cCliente + "' " + CRLF
	If !empty(cLoja)
		cQuerySD1 += "and SD1.D1_LOJA    = '" + cLoja + "' " + CRLF
	Endif
	cQuerySD1 += "and SD1.D1_DTDIGIT between '" + cDataDe + "' and '" + cDataAte + "' " + CRLF

	cQuery := "select left(SD1.D1_DTDIGIT, 6) MES, sum(SD1.D1_TOTAL + SD1.D1_VALIPI + SD1.D1_ICMSRET + SD1.D1_VALFRE) VALOR " + CRLF
	cQuery += cQuerySD1
	cQuery += "group by left(SD1.D1_DTDIGIT, 6) " + CRLF
	cQuery += "order by 1 " + CRLF
	msAguarde({|| cAliasTop := MPSysOpenQuery(cQuery)}, "Aguarde", "Verificando devoluções...", .F.)

	aFill(aSerie2, 0)
	Do While (cAliasTop)->(!eof())

		nMes := val(SubStr((cAliasTop)->MES, 5, 2))
		aSerie2[nMes] := (cAliasTop)->VALOR
		nValorMax := max(nValorMax, aSerie2[nMes])
		nValDev   += (cAliasTop)->VALOR
		aFatur[2] += (cAliasTop)->VALOR

		(cAliasTop)->(dbSkip())
	EndDo
	(cAliasTop)->(dbCloseArea())

	// Exibe o resultado na tela.
	If lMostra
		// Pega a quantidade de decimais do valor máximo.
		nTamNumero := 0
		nX := (nValorMax / 10)
		Do While nX >= 1
			nTamNumero ++
			nX /= 10
		EndDo

		// Fórmula para buscar a melhor faixa de valores para o gráfico.
		nPrecis := (10 ^ nTamNumero)
		nRangeY := Round(nValorMax + (nPrecis / 2) - 1, nTamNumero)
		If (nRangeY - (nPrecis / 2)) > nValorMax
			nRangeY -= (nPrecis / 2)
		Endif

		// Divide o número por mil, caso seja muito grande.
		If nRangeY > 10^9
			nDiv  := 10^6
			cDiv  := " (x 1 Mi)"
			cFoot := "x R$ 1.000.000,00"
		ElseIf nRangeY > 10^6
			nDiv := 10^3
			cDiv := " (x mil)"
			cFoot := "x R$ 1.000,00"
		Else
			nDiv := 1
		Endif
		nRangeY /= nDiv

		// Monta tela de entrada.
		DEFINE MSDIALOG oDialog TITLE "Evoluçao do cliente" FROM 0, 0 TO 500, 950 PIXEL

		// Grupo superior.
		oGrpSup := TScrollArea():New(oDialog, 0, 0, 0, 0, .F., .F.)
		oGrpSup:Align := CONTROL_ALIGN_ALLCLIENT

		// Cria o objeto do gráfico.
		oGraphic := TMSGraphic():New(0, 0, oGrpSup,,,, 0, 0)
		oGraphic:Align := CONTROL_ALIGN_ALLCLIENT
		oGraphic:blDblClick := {|oSelf| FEvolNot(cNomeCli, cQuerySD1, cQuerySD2)}

		cNomeCli := cCliente + "/" + if(empty(cLoja), "****", cLoja) + " - " + RTrim(Posicione("SA1", 1, xFilial("SA1") + cCliente + if(empty(cLoja), "", cLoja), "A1_NOME"))
		oGraphic:SetTitle("Evoluçao do cliente " + cNomeCli, If(dDtBase = date(), dtoc(date()) + " as " + Time(), "Data base: " + dtoc(dDtBase)), CLR_BLACK, A_LEFTJUST, GRP_TITLE)
		If !empty(cFoot)
			oGraphic:SetTitle(, cFoot, CLR_BLACK, A_CENTER, GRP_FOOT)
		Endif
		oGraphic:SetMargins(2, 6, 6, 6)
		oGraphic:SetGradient(GDBOTTOMTOP, CLR_HGRAY, CLR_GREEN)
		oGraphic:SetLegenProp(GRP_SCRRIGHT, CLR_HGRAY, GRP_SERIES, .T.)

		oGraphic:SetRangeY(0, nRangeY)
		oGraphic:l3D        := .T.  // Grafico em 3D
		oGraphic:lAxisVisib := .T.  // Mostra os eixos

		// Cria as séries de dados.
		nSerie1 := oGraphic:CreateSerie(GRP_BAR, 'Faturamento' + cDiv, 2, .T.)
		nSerie2 := oGraphic:CreateSerie(GRP_BAR, 'Devolução' + cDiv,   2, .T.)

		// Insere os dados da série.
		For nX := 1 to 12
			nMes := nX + nMesAtu - If(nX + nMesAtu > 12, 12, 0)
			aMes[nMes] := left(MesExtenso(nMes), 3) + "/" + right(Str(If(nMes > nMesAtu, nAnoIni - 1, nAnoIni), 4), 2)
			oGraphic:Add(nSerie1, round(aSerie1[nMes] / nDiv, 2), aMes[nMes], CLR_BLUE)
			oGraphic:Add(nSerie2, round(aSerie2[nMes] / nDiv, 2), aMes[nMes], CLR_RED)
		Next nX

		oPanel := tPanel():New(0,0,,oGrpSup,,,,,,500,40)
		oPanel:Align := CONTROL_ALIGN_BOTTOM

		// Mensagem no rodapé, com valores totais.
		aRodape[1] := "   Valor faturado R$ " + AllTrim(Transform(nValFat, cPictVlr)) + " - devolvido R$ " + AllTrim(Transform(nValDev, cPictVlr)) + " = total R$ " + AllTrim(Transform(nValFat - nValDev, cPictVlr))
		@ 0, 0 SAY oSayTot1 VAR aRodape[1] of oPanel PIXEL
		oSayTot1:Align := CONTROL_ALIGN_BOTTOM

		aRodape[2] := "   Total de notas: " + AllTrim(Transform(nQtdNF, cPictQtd)) + " - ticket médio: R$ " + If(nQtdNF = 0, "?", AllTrim(Transform(nValFat / nQtdNF, cPictVlr))) + " - rentab. média: " + AllTrim(Transform(If(nPrcLiq = 0, 0, (1 - (nCusto / nPrcLiq)) * 100), "@E 999.9") + "%")
		@ 0, 0 SAY oSayTot2 VAR aRodape[2] of oPanel PIXEL
		oSayTot2:Align := CONTROL_ALIGN_BOTTOM

		// Botões.
		oGrpBut := TScrollArea():New(oDialog, 0, 0, 14, 0, .F., .F.)
		oGrpBut:Align := CONTROL_ALIGN_BOTTOM

		// Botões.
		tButton():New(2, 005, "2&D/3D",          oGrpBut, {|| oGraphic:l3D := !oGraphic:l3D}, 50, 8,,,, .T.)
		tButton():New(2, 060, "&Salva imagem",   oGrpBut, {|| GrafSave(oGraphic)}, 50, 8,,,, .T.)
		tButton():New(2, 115, "&Ver notas",      oGrpBut, {|| FEvolNot(cNomeCli, cQuerySD1, cQuerySD2)}, 50, 8,,,, .T.)
		tButton():New(2, 170, "Por &fornecedor", oGrpBut, {|| FEvolFor(cNomeCli, cQuerySD1, cQuerySD2, nValFat, nValDev)}, 50, 8,,,, .T.)
		tButton():New(0, 430, "Sair (Esc)",      oGrpBut, {|| oDialog:End()}, 42, 10,,,, .T.)

		ACTIVATE MSDIALOG oDialog CENTERED
	Endif
Endif

Return aFatur


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : FEvolNot
// Descrição: Exibe as notas que fazem parte da evolução do cliente.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 12/02/16 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
Static Function FEvolNot(cNomeCli, cQuerySD1, cQuerySD2)
Local oDialog, oGrpSup, oGrpBut
Local oFolder, oGdFat, oGdDev
Local aCabec     := {}
Local aTam       := {}
Local nX

Local cQuery     := ""
Local cAliasTop  := ""
Local aFatur     := {}
Local aDevol     := {}
Local aTotais    := {0, 0}

// Monta tela de entrada.
DEFINE MSDIALOG oDialog TITLE "Notas da evoluçao do cliente" FROM 0, 0 TO 490, 900 PIXEL

// Grupo superior.
oGrpSup := TScrollArea():New(oDialog, 0, 0, 0, 0, .F., .F.)
oGrpSup:Align := CONTROL_ALIGN_ALLCLIENT

// Folder com as notas de entrada e saída.
aCabec := {"Faturamento", "Devoluções"}
oFolder := TFolder():New(0, 0, aCabec, {}, oGrpSup,,,, .T., .F., 0, 0)
oFolder:Align      := CONTROL_ALIGN_ALLCLIENT

aCabec := {"Filial", "Emissão", "Nota", "Série", "Cliente", "Loja", "Valor bruto", "Rentab."}
aTam   := {15, 30, 40, 20, 30, 20, 50, 30}
oGdFat := TWBrowse():New(0, 0, 0, 0,, aCabec, aTam, oFolder:aDialogs[1],,,,,,,,,,,,.F.,,.T.,,.F.)
oGdFat:Align      := CONTROL_ALIGN_ALLCLIENT
oGdFat:blDblClick := {|| FAbreNF(aFatur[oGdFat:nAt], .F.)}

aCabec := {"Filial", "Dt. Digit", "Nota", "Série", "Cliente", "Loja", "Valor bruto", "Rentab."}
aTam   := {15, 30, 40, 20, 30, 20, 50, 30}
oGdDev := TWBrowse():New(0, 0, 0, 0,, aCabec, aTam, oFolder:aDialogs[2],,,,,,,,,,,,.F.,,.T.,,.F.)
oGdDev:Align      := CONTROL_ALIGN_ALLCLIENT
oGdDev:blDblClick := {|| FAbreNF(aDevol[oGdDev:nAt], .T.)}

// Popula a lista das notas de faturamento.
cQuery := "select SD2.D2_FILIAL, SD2.D2_EMISSAO, SD2.D2_DOC, SD2.D2_SERIE, SD2.D2_CLIENTE, SD2.D2_LOJA, " + CRLF
cQuery += "sum(SD2.D2_VALBRUT) VALOR, " + CRLF
cQuery += "sum(SD2.D2_CUSTO1) CUSTO, sum(SD2.D2_TOTAL - SD2.D2_VALICM - SD2.D2_VALIMP5 - SD2.D2_VALIMP6 - SD2.D2_ICMSCOM - SD2.D2_DIFAL - SD2.D2_VFCPDIF) VLRLIQ " + CRLF
cQuery += cQuerySD2
cQuery += "group by SD2.D2_FILIAL, SD2.D2_EMISSAO, SD2.D2_DOC, SD2.D2_SERIE, SD2.D2_CLIENTE, SD2.D2_LOJA " + CRLF
cQuery += "order by SD2.D2_FILIAL, SD2.D2_EMISSAO, SD2.D2_DOC, SD2.D2_SERIE, SD2.D2_CLIENTE, SD2.D2_LOJA " + CRLF
msAguarde({|| cAliasTop := MPSysOpenQuery(cQuery)}, "Aguarde", "Verificando faturamento...", .F.)

Do While (cAliasTop)->(!eof())
	(cAliasTop)->(aAdd(aFatur, {D2_FILIAL, stod(D2_EMISSAO), D2_DOC, D2_SERIE, D2_CLIENTE, D2_LOJA, VALOR, If(VLRLIQ = 0, 0, (1 - (CUSTO / VLRLIQ)) * 100)}))
	(cAliasTop)->(dbSkip())
EndDo
(cAliasTop)->(dbCloseArea())

If empty(aFatur)
	aAdd(aFatur, {"", stod(""), "", "", 0, 0, 0, 0})
Endif
For nX := 1 to len(aFatur)
	aTotais[1] += aFatur[nX, 7]
	aFatur[nX, 7] := Transform(aFatur[nX, 7], cPictVlr)
	aFatur[nX, 8] := Transform(aFatur[nX, 8], cPictPer) + "%"
Next nX
oGdFat:Cargo := aFatur
oGdFat:SetArray(aFatur)
oGdFat:bLine := {|| aFatur[oGdFat:nAt]}
oGdFat:nAt   := 1

@ 0, 10 SAY oSayTot VAR "Total: R$ " + Transform(aTotais[1], cPictVlr) of oFolder:aDialogs[1] PIXEL
oSayTot:Align := CONTROL_ALIGN_BOTTOM

// Popula a lista das notas de devolução.
cQuery := "select SD1.D1_FILIAL, SD1.D1_DTDIGIT, SD1.D1_DOC, SD1.D1_SERIE, SD1.D1_FORNECE, SD1.D1_LOJA, " + CRLF
cQuery += "sum(SD1.D1_TOTAL + SD1.D1_VALIPI + SD1.D1_ICMSRET + SD1.D1_VALFRE) VALOR, " + CRLF
cQuery += "sum(SD1.D1_CUSTO) CUSTO, sum(SD1.D1_TOTAL - SD1.D1_VALICM - SD1.D1_VALIMP5 - SD1.D1_VALIMP6 - SD1.D1_ICMSCOM - SD1.D1_DIFAL - SD1.D1_VFCPDIF) VLRLIQ " + CRLF
cQuery += cQuerySD1
cQuery += "group by SD1.D1_FILIAL, SD1.D1_DTDIGIT, SD1.D1_DOC, SD1.D1_SERIE, SD1.D1_FORNECE, SD1.D1_LOJA " + CRLF
cQuery += "order by SD1.D1_FILIAL, SD1.D1_DTDIGIT, SD1.D1_DOC, SD1.D1_SERIE, SD1.D1_FORNECE, SD1.D1_LOJA " + CRLF
msAguarde({|| cAliasTop := MPSysOpenQuery(cQuery)}, "Aguarde", "Verificando devoluções...", .F.)

Do While (cAliasTop)->(!eof())
	(cAliasTop)->(aAdd(aDevol, {D1_FILIAL, stod(D1_DTDIGIT), D1_DOC, D1_SERIE, D1_FORNECE, D1_LOJA, VALOR, If(VLRLIQ = 0, 0, (1 - (CUSTO / VLRLIQ)) * 100)}))
	(cAliasTop)->(dbSkip())
EndDo
(cAliasTop)->(dbCloseArea())

If empty(aDevol)
	aAdd(aDevol, {"", stod(""), "", "", "", "", 0, 0})
Endif
For nX := 1 to len(aDevol)
	aTotais[2] += aDevol[nX, 7]
	aDevol[nX, 7] := Transform(aDevol[nX, 7], cPictVlr)
	aDevol[nX, 8] := Transform(aDevol[nX, 8], cPictPer) + "%"
Next nX
oGdDev:Cargo := aDevol
oGdDev:SetArray(aDevol)
oGdDev:bLine := {|| aDevol[oGdDev:nAt]}
oGdDev:nAt   := 1

@ 0, 10 SAY oSayTot VAR "Total: R$ " + Transform(aTotais[2], cPictVlr) of oFolder:aDialogs[2] PIXEL
oSayTot:Align := CONTROL_ALIGN_BOTTOM

// Botões.
oGrpBut := TScrollArea():New(oDialog, 0, 0, 14, 0, .F., .F.)
oGrpBut:Align := CONTROL_ALIGN_BOTTOM

// Botões.
tButton():New(2, 405, "Sair (Esc)",    oGrpBut, {|| oDialog:End()}, 42, 10,,,, .T.)

ACTIVATE MSDIALOG oDialog CENTERED

Return


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : FAbreNF
// Descrição: Exibe a nota fiscal para o usuário.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 12/02/16 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
Static Function FAbreNF(aNota, lDev)
Local cNFFil     := aNota[1]
Local cNFiscal   := aNota[3]
Local cSerie     := aNota[4]
Local cCliente   := aNota[5]
Local cLoja      := aNota[6]
Local cFilBak    := cFilAnt
Private cFilAnt := cNFFil

If lDev
	SF1->(dbSetOrder(1))  // F1_FILIAL, F1_DOC, F1_SERIE, F1_FORNECE, F1_LOJA, F1_TIPO.
	If SF1->(dbSeek(xFilial(, cNFFil) + cNFiscal + cSerie + cCliente + cLoja, .F.))
		msAguarde({|| A103NFiscal("SF1", SF1->(RecNo()), 2)}, "Carregando nota...")
	Else
		MsgAlert("Nota fiscal " + cNFFil + "-" + cSerie + "/" + cNFiscal + " não encontrada.", "Atenção")
	Endif
Else
	SF2->(dbSetOrder(1))  // F2_FILIAL, F2_DOC, F2_SERIE, F2_CLIENTE, F2_LOJA, F2_FORMUL, F2_TIPO.
	If SF2->(dbSeek(xFilial(, cNFFil) + cNFiscal + cSerie + cCliente + cLoja, .F.))
		msAguarde({|| Mc090Visual("SF2", SF2->(RecNo()), 2)}, "Carregando nota...")
	Else
		MsgAlert("Nota fiscal " + cNFFil + "-" + cSerie + "/" + cNFiscal + " não encontrada.", "Atenção")
	Endif
Endif
cFilAnt := cFilBak

Return


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : FEvolFor
// Descrição: Exibe os fornecedores que fazem parte da evolução do cliente.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 12/02/16 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
Static Function FEvolFor(cNomeCli, cQuerySD1, cQuerySD2, nValFat, nValDev)
Local oDialog, oGrpSup, oGrpBut
Local oFolder, oGdFat, oGdDev
Local aCabec     := {}
Local aTam       := {}
Local nX

Local cQuery     := ""
Local cAliasTop  := ""
Local aFatur     := {}
Local aDevol     := {}
Local aTotais    := {0, 0}

// Monta tela de entrada.
DEFINE MSDIALOG oDialog TITLE "Evoluçao do cliente - por fornecedor" FROM 0, 0 TO 490, 900 PIXEL

// Grupo superior.
oGrpSup := TScrollArea():New(oDialog, 0, 0, 0, 0, .F., .F.)
oGrpSup:Align := CONTROL_ALIGN_ALLCLIENT

// Folder com as notas de entrada e saída.
aCabec := {"Faturamento", "Devoluções"}
oFolder := TFolder():New(0, 0, aCabec, {}, oGrpSup,,,, .T., .F., 0, 0)
oFolder:Align      := CONTROL_ALIGN_ALLCLIENT

aCabec := {"Filial", "Fornecedor", "Loja", "Nome", "Notas", "Valor bruto", "Rentab.", "Participação"}
aTam   := {15, 40, 20, 80, 30, 50, 30, 30}
oGdFat := TWBrowse():New(0, 0, 0, 0,, aCabec, aTam, oFolder:aDialogs[1],,,,,,,,,,,,.F.,,.T.,,.F.)
oGdFat:Align      := CONTROL_ALIGN_ALLCLIENT
oGdFat:blDblClick := {|| FAbreNF(aFatur[oGdFat:nAt], .F.)}

aCabec := {"Filial", "Fornecedor", "Loja", "Nome", "Notas", "Valor bruto", "Rentab.", "Participação"}
aTam   := {15, 40, 20, 80, 30, 50, 30, 30}
oGdDev := TWBrowse():New(0, 0, 0, 0,, aCabec, aTam, oFolder:aDialogs[2],,,,,,,,,,,,.F.,,.T.,,.F.)
oGdDev:Align      := CONTROL_ALIGN_ALLCLIENT
oGdDev:blDblClick := {|| FAbreNF(aDevol[oGdDev:nAt], .T.)}

// Popula a lista de fornecedores (faturamento).
cQuery := "select A.D2_FILIAL, SA2.A2_COD, SA2.A2_LOJA, SA2.A2_NOME, " + CRLF
cQuery += "count(distinct A.NOTA) NOTAS, sum(A.D2_VALBRUT) VALOR, " + CRLF
cQuery += "sum(A.CUSTO) CUSTO, sum(A.VLRLIQ) VLRLIQ " + CRLF
cQuery += "from ( " + CRLF
cQuery += "  select SD2.D2_FILIAL, SD2.D2_DOC + SD2.D2_SERIE + SD2.D2_CLIENTE + SD2.D2_LOJA NOTA, " + CRLF
cQuery += "  SD2.D2_COD, SD2.D2_VALBRUT, " + CRLF
cQuery += "  SD2.D2_CUSTO1 CUSTO, (SD2.D2_TOTAL - SD2.D2_VALICM - SD2.D2_VALIMP5 - SD2.D2_VALIMP6 - SD2.D2_ICMSCOM - SD2.D2_DIFAL - SD2.D2_VFCPDIF) VLRLIQ " + CRLF
cQuery +=    cQuerySD2
cQuery += ") A " + CRLF

cQuery += "left join " + RetSQLName("SB1") + " SB1 with (noLock) on SB1.D_E_L_E_T_ = ' ' " + CRLF
cQuery += "and SB1.B1_FILIAL  = '" + xFilial("SB1") + "' " + CRLF
cQuery += "and SB1.B1_COD     = A.D2_COD " + CRLF

cQuery += "left  join " + RetSQLName("SA2") + " SA2 with (noLock) on SA2.D_E_L_E_T_ = ' ' " + CRLF
cQuery += "and SA2.A2_FILIAL  = '" + xFilial("SA2") + "' " + CRLF
cQuery += "and SA2.A2_COD     = SB1.B1_PROC " + CRLF
cQuery += "and SA2.A2_LOJA    = case when SB1.B1_LOJPROC = '' then '0000' else SB1.B1_LOJPROC end " + CRLF

cQuery += "group by A.D2_FILIAL, SA2.A2_COD, SA2.A2_LOJA, SA2.A2_NOME " + CRLF
cQuery += "order by VALOR desc, A.D2_FILIAL, SA2.A2_COD, SA2.A2_LOJA, SA2.A2_NOME " + CRLF
msAguarde({|| cAliasTop := MPSysOpenQuery(cQuery)}, "Aguarde", "Verificando faturamento...", .F.)

Do While (cAliasTop)->(!eof())
	(cAliasTop)->(aAdd(aFatur, {D2_FILIAL, A2_COD, A2_LOJA, A2_NOME, NOTAS, VALOR, If(VLRLIQ = 0, 0, (1 - (CUSTO / VLRLIQ)) * 100), (VALOR / nValFat) * 100}))
	(cAliasTop)->(dbSkip())
EndDo
(cAliasTop)->(dbCloseArea())

If empty(aFatur)
	aAdd(aFatur, {"", "", "", "", 0, 0, 0, 0})
Endif
For nX := 1 to len(aFatur)
	aTotais[1] += aFatur[nX, 6]
	aFatur[nX, 5] := Transform(aFatur[nX, 5], cPictQtd)
	aFatur[nX, 6] := Transform(aFatur[nX, 6], cPictVlr)
	aFatur[nX, 7] := Transform(aFatur[nX, 7], cPictPer) + "%"
	aFatur[nX, 8] := Transform(aFatur[nX, 8], cPictPer) + "%"
Next nX
oGdFat:Cargo := aFatur
oGdFat:SetArray(aFatur)
oGdFat:bLine := {|| aFatur[oGdFat:nAt]}
oGdFat:nAt   := 1

@ 0, 10 SAY oSayTot VAR "Total: R$ " + Transform(aTotais[1], cPictVlr) of oFolder:aDialogs[1] PIXEL
oSayTot:Align := CONTROL_ALIGN_BOTTOM

// Popula a lista de fornecedores (devolução).
cQuery := "select A.D1_FILIAL, SA2.A2_COD, SA2.A2_LOJA, SA2.A2_NOME,  " + CRLF
cQuery += "count(distinct A.NOTA) NOTAS, sum(A.VALOR) VALOR, " + CRLF
cQuery += "sum(A.CUSTO) CUSTO, sum(A.VLRLIQ) VLRLIQ " + CRLF
cQuery += "from ( " + CRLF
cQuery += "  select SD1.D1_FILIAL, SD1.D1_DOC + SD1.D1_SERIE + SD1.D1_FORNECE + SD1.D1_LOJA NOTA, " + CRLF
cQuery += "  SD1.D1_COD, SD1.D1_TOTAL + SD1.D1_VALIPI + SD1.D1_ICMSRET + SD1.D1_VALFRE VALOR, " + CRLF
cQuery += "  SD1.D1_CUSTO CUSTO, (SD1.D1_TOTAL - SD1.D1_VALICM - SD1.D1_VALIMP5 - SD1.D1_VALIMP6 - SD1.D1_ICMSCOM - SD1.D1_DIFAL - SD1.D1_VFCPDIF) VLRLIQ " + CRLF
cQuery +=    cQuerySD1
cQuery += ") A " + CRLF

cQuery += "left join " + RetSQLName("SB1") + " SB1 with (noLock) on SB1.D_E_L_E_T_ = ' ' " + CRLF
cQuery += "and SB1.B1_FILIAL  = '" + xFilial("SB1") + "' " + CRLF
cQuery += "and SB1.B1_COD     = A.D1_COD " + CRLF

cQuery += "left  join " + RetSQLName("SA2") + " SA2 with (noLock) on SA2.D_E_L_E_T_ = ' ' " + CRLF
cQuery += "and SA2.A2_FILIAL  = '" + xFilial("SA2") + "' " + CRLF
cQuery += "and SA2.A2_COD     = SB1.B1_PROC " + CRLF
cQuery += "and SA2.A2_LOJA    = case when SB1.B1_LOJPROC = '' then '0000' else SB1.B1_LOJPROC end " + CRLF

cQuery += "group by A.D1_FILIAL, SA2.A2_COD, SA2.A2_LOJA, SA2.A2_NOME " + CRLF
cQuery += "order by VALOR desc, A.D1_FILIAL, SA2.A2_COD, SA2.A2_LOJA, SA2.A2_NOME " + CRLF
msAguarde({|| cAliasTop := MPSysOpenQuery(cQuery)}, "Aguarde", "Verificando devoluções...", .F.)

Do While (cAliasTop)->(!eof())
	(cAliasTop)->(aAdd(aDevol, {D1_FILIAL, A2_COD, A2_LOJA, A2_NOME, NOTAS, VALOR, If(VLRLIQ = 0, 0, (1 - (CUSTO / VLRLIQ)) * 100), (VALOR / nValDev) * 100}))
	(cAliasTop)->(dbSkip())
EndDo
(cAliasTop)->(dbCloseArea())

If empty(aDevol)
	aAdd(aDevol, {"", "", "", "", 0, 0, 0, 0})
Endif
For nX := 1 to len(aDevol)
	aTotais[2] += aDevol[nX, 6]
	aDevol[nX, 5] := Transform(aDevol[nX, 5], cPictQtd)
	aDevol[nX, 6] := Transform(aDevol[nX, 6], cPictVlr)
	aDevol[nX, 7] := Transform(aDevol[nX, 7], cPictPer) + "%"
	aDevol[nX, 8] := Transform(aDevol[nX, 8], cPictPer) + "%"
Next nX
oGdDev:Cargo := aDevol
oGdDev:SetArray(aDevol)
oGdDev:bLine := {|| aDevol[oGdDev:nAt]}
oGdDev:nAt   := 1

@ 0, 10 SAY oSayTot VAR "Total: R$ " + Transform(aTotais[2], cPictVlr) of oFolder:aDialogs[2] PIXEL
oSayTot:Align := CONTROL_ALIGN_BOTTOM

// Botões.
oGrpBut := TScrollArea():New(oDialog, 0, 0, 14, 0, .F., .F.)
oGrpBut:Align := CONTROL_ALIGN_BOTTOM

// Botões.
tButton():New(2, 405, "Sair (Esc)",    oGrpBut, {|| oDialog:End()}, 42, 10,,,, .T.)

ACTIVATE MSDIALOG oDialog CENTERED

Return


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : GrafSave
// Descrição: Salva o gráfico em imagem.
// Retorno  : Lógico, indicando se a função foi executada com sucesso.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 16/01/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
Static Function GrafSave(oGraphic)
Local lRet       := .F.
Local cFileDrv   := ""
Local cFileDir   := ""
Local cFileName  := ""
Local cFileExt   := ""
Local cMascara   := ""

Local cPath      := ""
Local cArqTemp   := ""
Local cRaizSrv   := ""
Local lAbrir     := .F.

// Pede para o usuário selecionar o local onde salvar o arquivo.
cMascara += "Todos os arquivos (*.*)|*.*|"
cMascara += "Arquivo JPEG (*.JPG)|*.JPG|"
cMascara += "Arquivo PNG (*.PNG)|*.PNG|"
cMascara += "Arquivo BMP (*.BMP)|*.BMP|"
cFileName := cGetFile(cMascara, "Selecione local para a foto", 2, cFileName, .T., GETF_LOCALFLOPPY + GETF_LOCALHARD, .F.)

If !Empty(cFileName)
	If at("\", cFileName) > 0
		SplitPath(cFileName, @cFileDrv, @cFileDir, @cFileName, @cFileExt)
		cFileExt  := If(empty(cFileExt), ".JPG", cFileExt)
		cPath 	  := cFileDrv + cFileDir
		cFileName := cFileName + cFileExt
	Endif

	cArqTemp := getNextAlias() + cFileExt
	cRaizSrv := cStartPath + If(right(cStartPath, 1) == "\", "", "\")
	lRet := oGraphic:SaveToImage(cArqTemp, cRaizSrv, SubStr(cFileExt, 2))
	If lRet
		If file(cPath + cFileName)
			fErase(cPath + cFileName)
		Endif
		lRet := __CopyFile(cRaizSrv + cArqTemp, cPath + cFileName)
		fErase(cRaizSrv + cArqTemp)
		lAbrir := .T.
	Endif

	If lRet
		MsgInfo("Arquivo " + cFileName + " gerado com sucesso")
		If lAbrir
			WinExec('Explorer.exe /select, "' + cPath + cFileName + '"')
		Endif
	Else
		ApMsgAlert("Não foi possivel gerar o arquivo " + cFileExt + " gráfico.")
	Endif
Endif

Return lRet


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Materiais
// Função   : TransfArm
// Descrição: Transfere quantidade divergente para o armazém de divergências.
// Retorno  : Lógico, indicando se a função foi executada com sucesso.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 19/03/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function TransfArm(aTransf, dEmissao, cHist, lVerOrig, lProc, cMsgErro, cProcOri, cItOri, cProcDes, cItDes)
Local nOpcAuto   := 3  // 3 - Incluir.
Local aMata261   := {}
Local cDocSD3    := ""

Local cProduto   := ""
Local nQuant     := 0
Local cLocalOri  := ""
Local cLocalDes  := ""
Local cLoteCtl   := ""
Local cNumLote   := ""

Local nSaldo     := 0
Local cQuery     := ""
Local cAliasTop  := ""

Local nX

Private lMsErroAuto := .F.

// Define se o sistema irá transferir somente o que tiver no estoque de origem.
Default lVerOrig 	:= .F.
Default lProc 		:= .F.
Default cMsgErro 	:= ""
Default cProcOri 	:= ""
Default cItOri 		:= ""
Default cProcDes 	:= ""
Default cItDes 		:= ""

Begin Transaction

cDocSD3 := GetSXENum("SD3", "D3_DOC")
aAdd(aMata261, {cDocSD3, dEmissao})  // Cabecalho [documento, emissao].

For nX := 1 to len(aTransf)
	cProduto   := aTransf[nX, 1]
	nQuant     := aTransf[nX, 2]
	cLocalOri  := aTransf[nX, 3]
	cLocalDes  := aTransf[nX, 4]
	cLoteCtl   := If(len(aTransf[nX]) >= 5, aTransf[nX, 5], CriaVar('D3_LOTECTL', .F.))
	cNumLote   := If(len(aTransf[nX]) >= 6, aTransf[nX, 6], CriaVar('D3_NUMLOTE', .F.))

	SB1->(dbSetOrder(1))  // B1_FILIAL, B1_COD.
	SB1->(msSeek(xFilial() + cProduto, .F.))

	If SB1->B1_RASTRO $ "L|S"
		cQuery := "select SB8.R_E_C_N_O_ SB8RecNo, SB8.B8_SALDO SALDO " + CRLF
		cQuery += "from " + RetSQLName("SB8") + " SB8 with (noLock) " + CRLF
		cQuery += "where SB8.D_E_L_E_T_ = ' ' " + CRLF
		cQuery += "and SB8.B8_FILIAL  = '" + xFilial("SB8") + "' " + CRLF
		cQuery += "and SB8.B8_PRODUTO = '" + cProduto + "' " + CRLF
		cQuery += "and SB8.B8_LOCAL   = '" + cLocalOri + "' " + CRLF
		cQuery += "and SB8.B8_SALDO   > 0 " + CRLF
		If !empty(cLoteCtl)
			cQuery += "and SB8.B8_LOTECTL = '" + cLoteCtl + "' " + CRLF
		Endif
		If !empty(cNumLote)
			cQuery += "and SB8.B8_NUMLOTE = '" + cNumLote + "' " + CRLF
		Endif
		cQuery += "order by SB8.B8_SALDO, SB8.B8_DATA " + CRLF
		cAliasTop := MPSysOpenQuery(cQuery)
		nSaldo := nQuant
		Do While (cAliasTop)->(!eof() .and. nSaldo > 0)
			SB8->(dbGoTo((cAliasTop)->SB8RecNo))
			nQtdeSB8 := min(nSaldo, SB8->B8_SALDO)
			nSaldo   -= nQtdeSB8

			//Verifica se existe registro de saldo para o armazem de destino
			U_SLDINI(cProduto, cLocalDes)

			If u_ARMESPECIAL()
				aAdd(aMata261, {;
					cProduto, SB1->B1_DESC, SB1->B1_UM, cLocalOri, CriaVar('D3_LOCALIZ'),;  // Origem.
					cProduto, SB1->B1_DESC, SB1->B1_UM, cLocalDes, CriaVar('D3_LOCALIZ'),;  // Destino (armazém de perdas).
					CriaVar('D3_NUMSERI'), SB8->B8_LOTECTL, SB8->B8_NUMLOTE, CriaVar('D3_DTVALID'),;
					CriaVar('D3_POTENCI'), nQtdeSB8, CriaVar("D3_QTSEGUM"), CriaVar('D3_ESTORNO'),;
					CriaVar('D3_NUMSEQ'), SB8->B8_LOTECTL, CriaVar('D3_DTVALID'), CriaVar('D3_ITEMGRD'), cHist,;
					cProcOri, cItOri, cProcDes, cItDes })
			Else
				aAdd(aMata261, {;
					cProduto, SB1->B1_DESC, SB1->B1_UM, cLocalOri, CriaVar('D3_LOCALIZ'),;  // Origem.
					cProduto, SB1->B1_DESC, SB1->B1_UM, cLocalDes, CriaVar('D3_LOCALIZ'),;  // Destino (armazém de perdas).
					CriaVar('D3_NUMSERI'), SB8->B8_LOTECTL, SB8->B8_NUMLOTE, CriaVar('D3_DTVALID'),;
					CriaVar('D3_POTENCI'), nQtdeSB8, CriaVar("D3_QTSEGUM"), CriaVar('D3_ESTORNO'),;
					CriaVar('D3_NUMSEQ'), SB8->B8_LOTECTL, CriaVar('D3_DTVALID'), CriaVar('D3_ITEMGRD'), cHist })
			EndIf

			(cAliasTop)->(dbSkip())
		EndDo
		(cAliasTop)->(dbCloseArea())

		If nSaldo > 0 .and. !lVerOrig
			MsgAlert("Saldo insuficiente em estoque." + CRLF + "Produto " + cProduto + "/" + cLocalOri + CRLF + "Quantidade: " + cValToChar(nQuant), "Atenção")
			lMsErroAuto := .T.
			Exit
		Endif
	Else
		If lVerOrig
			SB2->(dbSetOrder(1))   // B2_FILIAL, B2_COD, B2_LOCAL.
			SB2->(msSeek(xFilial() + SB1->B1_COD + cLocalOri, .F.))
			nQuant := min(nQuant, SB2->B2_QATU)
		Endif

		If nQuant > 0
			//Verifica se existe registro de saldo para o armazem de destino
			U_SLDINI(cProduto, cLocalDes)

			If u_ARMESPECIAL()
				aAdd(aMata261, {;
					cProduto, SB1->B1_DESC, SB1->B1_UM, cLocalOri, CriaVar('D3_LOCALIZ'),;  // Origem.
					cProduto, SB1->B1_DESC, SB1->B1_UM, cLocalDes, CriaVar('D3_LOCALIZ'),;  // Destino (armazém de perdas).
					CriaVar('D3_NUMSERI'), CriaVar('D3_LOTECTL'), CriaVar('D3_NUMLOTE'), CriaVar('D3_DTVALID'),;
					CriaVar('D3_POTENCI'), nQuant, CriaVar("D3_QTSEGUM"), CriaVar('D3_ESTORNO'),;
					CriaVar('D3_NUMSEQ'), CriaVar('D3_LOTECTL'), CriaVar('D3_DTVALID'), CriaVar('D3_ITEMGRD'), cHist,;
					cProcOri, cItOri, cProcDes, cItDes })
			Else
				aAdd(aMata261, {;
					cProduto, SB1->B1_DESC, SB1->B1_UM, cLocalOri, CriaVar('D3_LOCALIZ'),;  // Origem.
					cProduto, SB1->B1_DESC, SB1->B1_UM, cLocalDes, CriaVar('D3_LOCALIZ'),;  // Destino (armazém de perdas).
					CriaVar('D3_NUMSERI'), CriaVar('D3_LOTECTL'), CriaVar('D3_NUMLOTE'), CriaVar('D3_DTVALID'),;
					CriaVar('D3_POTENCI'), nQuant, CriaVar("D3_QTSEGUM"), CriaVar('D3_ESTORNO'),;
					CriaVar('D3_NUMSEQ'), CriaVar('D3_LOTECTL'), CriaVar('D3_DTVALID'), CriaVar('D3_ITEMGRD'), cHist})
			EndIf
		Endif
	Endif
Next nX

// Transferência para armazém de perda.
If !lMsErroAuto .and. len(aMata261) > 1
	msExecAuto({|x, y| MATA261(x, y)}, aMata261, nOpcAuto)
Endif

// Atualiza histórico na movimentação.
If !lMsErroAuto .and. !empty(cHist)
	SD3->(dbSetOrder(2))  // D3_FILIAL, D3_DOC, D3_COD.
	If SD3->(dbSeek(xFilial() + cDocSD3, .F.))
		Do While SD3->(!eof() .and. D3_FILIAL + D3_DOC == xFilial() + cDocSD3)
			RecLock("SD3", .F.)
				SD3->D3_XOBS 	:= cHist
			SD3->(msUnLock())
			SD3->(dbSkip())
		EndDo
		SD3->(dbSeek(xFilial() + cDocSD3, .F.))
	Endif
Endif

End Transaction

If lMsErroAuto
	If lProc .OR. IsInCallStack("U_PROMM300")
		cMsgErro += MostraErro("\system\", "TransfArm.LOG") + CRLF
	Else
		MostraErro()
	EndIf
Endif

Return !lMsErroAuto


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : SC0Inc
// Descrição: Faz a reserva do produto, atrelado a um pedido.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 19/05/16 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function SC0Inc(cDoc, cProduto, cArmazem, nQuant, cObserv, dValida, nQtdeEst, cMsgErro)
Local lRet       := .T.
Local aLotes     := {}
Local aLoteAux   := {}
Local nSalLote   := 0

Local cQuery     := ""
Local cAliasTop  := ""
Local nX		:= 0

cDoc     := PadR(cDoc,     nTamSC0)
cProduto := PadR(cProduto, nTamProd)
cArmazem := PadR(cArmazem, nTamLoc)
cMsgErro := ""
Default dValida := dDataBase

// Verifica o estoque do produto.
SB2->(dbSetOrder(1))   // B2_FILIAL, B2_COD, B2_LOCAL.
If SB2->(msSeek(xFilial() + cProduto + cArmazem, .F.))
	nQtdeEst := SaldoSB2()
Else
	nQtdeEst := 0
Endif

cQuery := "select sum(SC0.C0_QUANT) SC0Reserv " + CRLF
cQuery += "from " + RetSQLName("SC0") + " SC0 " + CRLF
cQuery += "where SC0.D_E_L_E_T_ = ' ' " + CRLF
cQuery += "and SC0.C0_FILIAL  = '" + xFilial("SB8") + "' " + CRLF
cQuery += "and SC0.C0_NUM     = '" + cDoc + "' " + CRLF
cQuery += "and SC0.C0_PRODUTO = '" + cProduto + "' " + CRLF
cQuery += "and SC0.C0_LOCAL   = '" + cArmazem + "' " + CRLF
cAliasTop := MPSysOpenQuery(cQuery)

If (cAliasTop)->(!eof())
	If (cAliasTop)->SC0Reserv <> nQuant
		// Exclui reservas para refazê-la posteriormente.
		U_SC0Exc(cDoc, cProduto)
	Else
		// Ajusta data e observação.
		SC0->(dbSetOrder(1))  // C0_FILIAL, C0_NUM, C0_PRODUTO, C0_LOCAL.
		SC0->(dbSeek(xFilial() + cDoc + cProduto + cArmazem, .F.))
		Do While SC0->(!eof() .and. C0_FILIAL + C0_NUM + C0_PRODUTO + C0_LOCAL == xFilial() + cDoc + cProduto + cArmazem)
			If SC0->(C0_OBS <> cObserv .or. C0_VALIDA <> dValida)
				RecLock("SC0", .F.)
				SC0->C0_OBS    := cObserv
				SC0->C0_VALIDA := dValida
				SC0->(msUnLock())
			Endif
			SC0->(dbSkip())
		EndDo
	Endif
Endif
(cAliasTop)->(dbCloseArea())

SC0->(dbSetOrder(1))  // C0_FILIAL, C0_NUM, C0_PRODUTO, C0_LOCAL.
If SC0->(!dbSeek(xFilial() + cDoc + cProduto + cArmazem, .F.))
	If nQtdeEst >= nQuant
		// Se o produto controla lote, busca os lotes disponíveis.
		SB1->(dbSetOrder(1))  // B1_FILIAL, B1_COD.
		SB1->(msSeek(xFilial() + cProduto, .F.))
		If SB1->B1_RASTRO $ "L|S"
			cQuery := "select SB8.R_E_C_N_O_ SB8RecNo " + CRLF
			cQuery += "from " + RetSQLName("SB8") + " SB8 " + CRLF
			cQuery += "where SB8.D_E_L_E_T_ = ' ' " + CRLF
			cQuery += "and SB8.B8_FILIAL  = '" + xFilial("SB8") + "' " + CRLF
			cQuery += "and SB8.B8_PRODUTO = '" + cProduto + "' " + CRLF
			cQuery += "and SB8.B8_LOCAL   = '" + cArmazem + "' " + CRLF
			cQuery += "and SB8.B8_SALDO   > 0 " + CRLF
			cQuery += "order by SB8.B8_DTVALID, SB8.B8_DATA, SB8.B8_SALDO " + CRLF
			cAliasTop := MPSysOpenQuery(cQuery)

			If (cAliasTop)->(eof())
				// Se não existe lote disponível, retorna falso.
				lRet := .F.
				cMsgErro := "Produto sem saldo SB8."
			Else
				nSalLote := nQuant
				Do While (cAliasTop)->(!eof() .and. nSalLote > 0)
					SB8->(dbGoTo((cAliasTop)->SB8RecNo))

					aLoteAux := {min(nSalLote, SB8->B8_SALDO), SB8->B8_NUMLOTE, SB8->B8_LOTECTL}
					nSalLote -= aLoteAux[1]
					aAdd(aLotes, aLoteAux)

					(cAliasTop)->(dbSkip())
				EndDo

				If nSalLote <> 0
					// Se não existe lote disponível suficiente, retorna falso.
					lRet := .F.
					cMsgErro := "Produto sem saldo SB8 suficiente (" + cValToChar(nSalLote) + ")."
				Endif
			Endif
			(cAliasTop)->(dbCloseArea())
		Else
			aAdd(aLotes, {nQuant, "", ""})
		Endif
	Else
		// Se não existe saldo físico suficiente, retorna falso.
		lRet := .F.
		cMsgErro := "Produto sem saldo SB2."
	Endif

	// Efetua a reserva.
	If lRet
		For nX := 1 to len(aLotes)
			lRet := a430Reserv({1, "PD", cDoc, "NetSuprimentos", cFilAnt, cObserv}, cDoc, cProduto, cArmazem, aLotes[nX, 1], {aLotes[nX, 2], aLotes[nX, 3], "", ""}, {}, {}, 0)
			If lRet
				SC0->(dbSetOrder(1))  // C0_FILIAL, C0_NUM, C0_PRODUTO, C0_LOCAL.
				If SC0->(dbSeek(xFilial() + cDoc + cProduto + cArmazem, .F.))
					RecLock("SC0", .F.)
					SC0->C0_VALIDA := dValida
					SC0->(msUnLock())
				Endif
			Endif
		Next nX
	Endif
Endif

Return lRet


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : SC0Exc
// Descrição: Faz a reserva do produto, atrelado a um pedido.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 27/05/16 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function SC0Exc(xSC0Num, cProduto)
Local cSC0Num    := cValToChar(xSC0Num)
Local lRet       := .T.

Default cProduto := ""
cProduto := PadR(cProduto, nTamProd)

SC0->(dbSetOrder(1))  // C0_FILIAL, C0_NUM, C0_PRODUTO, C0_LOCAL.
If SC0->(dbSeek(xFilial() + cSC0Num, .F.))
	// Exclui as reservas.
	Do While SC0->(!eof() .and. C0_FILIAL + C0_NUM == xFilial() + cSC0Num) .and. lRet
		If empty(cProduto) .or. SC0->C0_PRODUTO == cProduto
			lRet := SC0->(a430Reserv({3, C0_TIPO, C0_DOCRES, C0_SOLICIT, C0_FILRES}, C0_NUM, C0_PRODUTO, C0_LOCAL, C0_QUANT, {C0_NUMLOTE, C0_LOTECTL, C0_LOCALIZ, C0_NUMSERI}))
		Endif
		SC0->(dbSkip())
	EndDo
Endif

Return lRet


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Joao Leao
// Modulo   : Faturamento
// Função   : SC6GatTes
// Descrição: Executa o gatilho que preenche o TES nos itens do pedido de venda.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 27/05/16 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function SC6GatTes()
Local nI      	:= 0
Local nAnt    	:= 0
Local lBlqEstW	:= SuperGetMV("MT410TOK05", NIL, "VPC") == M->C5_XTIPO		//Tipo de Pedido que não força o envio para o WMS caso movimente estoque

If Type("oGetDad") == "O" .and. !IsInCallStack("msExecAuto") .AND. !lBlqEstW
	nAnt := n
	For nI := 1 To Len(aCols) // Passa por todos os itens
		n := nI
		U_RET_C6_TES()
	Next nI
	n := nAnt

	oGetDad:Refresh()
EndIf

Return .T.


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento
// Função   : GetIdEnt
// Descrição: Retorna o código de entidade de uma filial no TSS.
// Retorno  : Código da entidade.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 14/06/16 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function GetIdEnt(cEmpPar, cFilPar)
	Local cIdEnt     := ""
	Local aArea      := GetArea()
	Local cURL       := PadR(GetNewPar("MV_SPEDURL", "http://"), 250)
	Local cError     := ""
	Local oWs
	Local cEmpBkp	:= cEmpAnt
	Local cFilBkp	:= cFilAnt

	// Posiciona a filial.
	Default cEmpPar := cEmpAnt
	Default cFilPar := cFilAnt

	cEmpAnt := cEmpPar
	cFilAnt := cFilPar
	FWSM0Util():setSM0PositionBycFilAnt()

	// Obtem o código da entidade.
	oWS := WsSPEDAdm():New()
	oWS:cUSERTOKEN := "TOTVS"

	oWS:oWSEMPRESA:cCNPJ      := If(FWSM0Util():GetSM0Data(cEmpAnt, cFilAnt, {"M0_TPINSC"})[1][2] == 2 .or. empty(FWSM0Util():GetSM0Data(cEmpAnt, cFilAnt, {"M0_TPINSC"})[1][2]), FWSM0Util():GetSM0Data(cEmpAnt, cFilAnt, {"M0_CGC"})[1][2], "")
	oWS:oWSEMPRESA:cCPF       := If(FWSM0Util():GetSM0Data(cEmpAnt, cFilAnt, {"M0_TPINSC"})[1][2] == 3, FWSM0Util():GetSM0Data(cEmpAnt, cFilAnt, {"M0_CGC"})[1][2], "")
	oWS:oWSEMPRESA:cIE        := FWSM0Util():GetSM0Data(cEmpAnt, cFilAnt, {"M0_INSC"})[1][2]
	oWS:oWSEMPRESA:cIM        := FWSM0Util():GetSM0Data(cEmpAnt, cFilAnt, {"M0_INSCM"})[1][2]
	oWS:oWSEMPRESA:cNOME      := FWSM0Util():GetSM0Data(cEmpAnt, cFilAnt, {"M0_NOMECOM"})[1][2]
	oWS:oWSEMPRESA:cFANTASIA  := FWSM0Util():GetSM0Data(cEmpAnt, cFilAnt, {"M0_NOME"})[1][2]
	oWS:oWSEMPRESA:cENDERECO  := FisGetEnd(FWSM0Util():GetSM0Data(cEmpAnt, cFilAnt, {"M0_ENDENT"})[1][2])[1]
	oWS:oWSEMPRESA:cNUM       := FisGetEnd(FWSM0Util():GetSM0Data(cEmpAnt, cFilAnt, {"M0_ENDENT"})[1][2])[3]
	oWS:oWSEMPRESA:cCOMPL     := FisGetEnd(FWSM0Util():GetSM0Data(cEmpAnt, cFilAnt, {"M0_ENDENT"})[1][2])[4]
	oWS:oWSEMPRESA:cUF        := FWSM0Util():GetSM0Data(cEmpAnt, cFilAnt, {"M0_ESTENT"})[1][2]
	oWS:oWSEMPRESA:cCEP       := FWSM0Util():GetSM0Data(cEmpAnt, cFilAnt, {"M0_CEPENT"})[1][2]
	oWS:oWSEMPRESA:cCOD_MUN   := FWSM0Util():GetSM0Data(cEmpAnt, cFilAnt, {"M0_CODMUN"})[1][2]
	oWS:oWSEMPRESA:cCOD_PAIS  := "1058"  // Brasil.
	oWS:oWSEMPRESA:cBAIRRO    := FWSM0Util():GetSM0Data(cEmpAnt, cFilAnt, {"M0_BAIRENT"})[1][2]
	oWS:oWSEMPRESA:cMUN       := FWSM0Util():GetSM0Data(cEmpAnt, cFilAnt, {"M0_CIDENT"})[1][2]
	oWS:oWSEMPRESA:cCEP_CP    := nil
	oWS:oWSEMPRESA:cCP        := nil
	oWS:oWSEMPRESA:cDDD       := Str(FisGetTel(FWSM0Util():GetSM0Data(cEmpAnt, cFilAnt, {"M0_TEL"})[1][2])[2], 3)
	oWS:oWSEMPRESA:cFONE      := AllTrim(str(FisGetTel(FWSM0Util():GetSM0Data(cEmpAnt, cFilAnt, {"M0_TEL"})[1][2])[3], 15))
	oWS:oWSEMPRESA:cFAX       := AllTrim(str(FisGetTel(FWSM0Util():GetSM0Data(cEmpAnt, cFilAnt, {"M0_FAX"})[1][2])[3], 15))
	oWS:oWSEMPRESA:cEMAIL     := UsrRetMail(RetCodUsr())
	oWS:oWSEMPRESA:cNIRE      := FWSM0Util():GetSM0Data(cEmpAnt, cFilAnt, {"M0_NIRE"})[1][2]
	oWS:oWSEMPRESA:dDTRE      := FWSM0Util():GetSM0Data(cEmpAnt, cFilAnt, {"M0_DTRE"})[1][2]
	oWS:oWSEMPRESA:cNIT       := If(FWSM0Util():GetSM0Data(cEmpAnt, cFilAnt, {"M0_TPINSC"})[1][2] == 1, FWSM0Util():GetSM0Data(cEmpAnt, cFilAnt, {"M0_CGC"})[1][2], "")
	oWS:oWSEMPRESA:cINDSITESP := ""
	oWS:oWSEMPRESA:cID_MATRIZ := ""

	oWS:oWSOUTRASINSCRICOES:oWSInscricao := SPEDADM_ARRAYOFSPED_GENERICSTRUCT():New()
	oWS:_URL := AllTrim(cURL)+"/SPEDADM.apw"
	If oWs:ADMEMPRESAS()
		cIdEnt := oWs:cADMEMPRESASRESULT
	Else
		cError := GetWscError(3)
		Aviso("SPED", If(empty(cError), GetWscError(1), cError), {"OK"}, 3)
	Endif

	cEmpAnt := cEmpBkp
	cFilAnt := cFilBkp
	FWSM0Util():setSM0PositionBycFilAnt()

	RestArea(aArea)

Return(cIdEnt)


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento
// Função   : ConsCad
// Descrição: Faz a consulta do contribuinte junto ao SEFAZ.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 16/06/16 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function ConsCad(cIE, cUF)
Local oDlgKey
Local oFont
Local oBtnCon

If empty(cIE) .or. empty(cUF)
	DEFINE FONT oFont BOLD

	DEFINE MSDIALOG oDlgKey TITLE "Consulta contribuinte" FROM 0,0 TO 100, 300 PIXEL OF GetWndDefault()

	@ 003, 004 SAY "Informe os dados para a consulta" PIXEL FONT oFont OF oDlgKey

	@ 018, 006 SAY "Estado" PIXEL OF oDlgKey
	@ 016, 026 MSGET oGetUF VAR cUF OF oDlgKey SIZE 020, 009 PICTURE "@!" PIXEL F3 "12" VALID {|| empty(cUF) .or. ExistCpo('SX5', '12' + cUF, 1)}

	@ 018, 083 SAY "IE" PIXEL OF oDlgKey
	@ 016, 090 MSGET cIE OF oDlgKey SIZE 060, 009 PIXEL

	@ 035, 004 BUTTON oBtnCon PROMPT "Consultar" SIZE 38, 11 OF oDlgKey PIXEL ACTION If(empty(cUF) .or. empty(cIE), nil, (ConsCad(cIE, cUF), oDlgKey:End()))
	@ 038, 120 BUTTON oBtnCon PROMPT "Sair"      SIZE 30, 08 OF oDlgKey PIXEL ACTION (oDlgKey:End())

	ACTIVATE DIALOG oDlgKey CENTERED
Else
	ConsCad(cIE, cUF)
Endif

Return


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento
// Função   : ConsCad
// Descrição: Faz a consulta do contribuinte junto ao SEFAZ.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 16/06/16 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
Static Function ConsCad(cIE, cUF)
Local lConsulta  := .F.
Local cURL       := PadR(GetNewPar("MV_SPEDURL", "http://"), 250)
Local cRazSoci   := ""
Local cRegApur   := ""
Local cCnpj      := ""
Local cCpf       := ""
Local cSituacao  := ""
Local cPictCNPJ  := ""
Local dIniAtiv   := ctod("")
Local dAtualiza  := ctod("")
Local cError     := ""
Local nX

Private oWS

oWs := WsNFeSBra():New()
oWs:cUserToken := "TOTVS"
oWs:cID_ENT    := U_GetIdEnt()
oWs:cUF        := cUF
oWs:cCPF       := ""
oWs:cCNPJ      := ""
oWs:cIE        := Alltrim(cIE)
oWs:_URL       := cURL + "/NFeSBRA.apw"

msAguarde({|| lConsulta := oWs:CONSULTACONTRIBUINTE()}, "Consultando SEFAZ...")
If lConsulta
	If Type("oWs:OWSCONSULTACONTRIBUINTERESULT:OWSNFECONSULTACONTRIBUINTE") <> "U"
		If len(oWs:OWSCONSULTACONTRIBUINTERESULT:OWSNFECONSULTACONTRIBUINTE) > 0
			nX := Len(oWs:OWSCONSULTACONTRIBUINTERESULT:OWSNFECONSULTACONTRIBUINTE)

			If ValType(oWs:OWSCONSULTACONTRIBUINTERESULT:OWSNFECONSULTACONTRIBUINTE[nX]:dInicioAtividade) <> "U"
				dIniAtiv  := oWs:OWSCONSULTACONTRIBUINTERESULT:OWSNFECONSULTACONTRIBUINTE[nX]:dInicioAtividade
			Endif
			cRazSoci  := AllTrim(oWs:OWSCONSULTACONTRIBUINTERESULT:OWSNFECONSULTACONTRIBUINTE[nX]:cRazaoSocial)
			cRegApur  := AllTrim(oWs:OWSCONSULTACONTRIBUINTERESULT:OWSNFECONSULTACONTRIBUINTE[nX]:cRegimeApuracao)
			cCnpj     := AllTrim(oWs:OWSCONSULTACONTRIBUINTERESULT:OWSNFECONSULTACONTRIBUINTE[nX]:cCNPJ)
			cCpf      := AllTrim(oWs:OWSCONSULTACONTRIBUINTERESULT:OWSNFECONSULTACONTRIBUINTE[nX]:cCPF)
			cIe       := AllTrim(oWs:OWSCONSULTACONTRIBUINTERESULT:OWSNFECONSULTACONTRIBUINTE[nX]:cIE)
			cUf       := AllTrim(oWs:OWSCONSULTACONTRIBUINTERESULT:OWSNFECONSULTACONTRIBUINTE[nX]:cUF)
			cSituacao := AllTrim(oWs:OWSCONSULTACONTRIBUINTERESULT:OWSNFECONSULTACONTRIBUINTE[nX]:cSituacao)
			If ValType(oWs:OWSCONSULTACONTRIBUINTERESULT:OWSNFECONSULTACONTRIBUINTE[nX]:dUltimaSituacao) <> "U"
				dAtualiza := oWs:OWSCONSULTACONTRIBUINTERESULT:OWSNFECONSULTACONTRIBUINTE[nX]:dUltimaSituacao
			Endif

			If cSituacao == "1"
				cSituacao := "1 - Habilitado"
			ElseIf cSituacao == "0"
				cSituacao := "0 - Não Habilitado"
			Endif

			If Empty(cCnpj)
				cCnpj     := cCPF
				cPictCNPJ := "@R 999.999.999-99"
			Else
				cCnpj     := cCnpj
				cPictCNPJ := "@R 99.999.999/9999-99"
			Endif

			DEFINE MSDIALOG oDlgKey TITLE "Retorno do consulta contribuinte" FROM 0, 0 TO 200, 400 PIXEL
			@ 008, 006 SAY "Início das atividades" PIXEL OF oDlgKey
			@ 008, 068 GET dIniAtiv                PIXEL OF oDlgKey READONLY
			@ 008, 170 SAY "UF"                    PIXEL OF oDlgKey
			@ 008, 180 GET cUf                     PIXEL OF oDlgKey SIZE 018, 10 READONLY
			@ 022, 006 SAY "Razão social"          PIXEL OF oDlgKey
			@ 022, 042 GET cRazSoci                PIXEL OF oDlgKey SIZE 156, 10 READONLY
			@ 036, 006 SAY "CNPJ/CPF"              PIXEL OF oDlgKey
			@ 036, 042 GET cCnpj                   PIXEL OF oDlgKey SIZE 060, 10 READONLY PICTURE cPictCNPJ
			@ 036, 112 SAY "IE"                    PIXEL OF oDlgKey
			@ 036, 119 GET cIe                     PIXEL OF oDlgKey SIZE 079, 10 READONLY
			@ 050, 006 SAY "Regime"                PIXEL OF oDlgKey
			@ 050, 042 GET cRegApur                PIXEL OF oDlgKey SIZE 156, 10 READONLY
			@ 064, 006 SAY "Situação"              PIXEL OF oDlgKey
			@ 064, 042 GET cSituacao               PIXEL OF oDlgKey SIZE 156, 10 READONLY

			@ 090, 006 SAY "Atualizado em " + dtoc(dAtualiza) PIXEL OF oDlgKey
			@ 090, 160 BUTTON oBtnCon PROMPT "Fechar" SIZE 38, 08 PIXEL ACTION oDlgKey:End()
			ACTIVATE DIALOG oDlgKey CENTERED
		Endif
	Endif
Else
	cError := GetWscError(3)
	Aviso("SPED", If(empty(cError), GetWscError(1), cError), {"OK"}, 3)
Endif

Return


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento
// Função   : TMKPreco
// Descrição: Calcula preço de venda no orçamento (gatilho de produto).
// Retorno  : Preço de venda.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 21/09/16 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function TMKPreco(lEdi)
Local lRet       := .T.
Local nPrcUnit   := 0

Local cProduto   := 0
Local nPerICMS   := 0
Local nRedICMS   := 0
Local nBaseDes   := 0
Local nPerICC    := 0
Local nPerFECPC  := 0
Local cContrato  := ""

Static nArredVlr  := 0

Default lEdi := .F.

If !IsInCallStack("U_PROMA030") .and. MaFisFound("IT", n) .and. !IsInCallStack("U_PROMA611")

	// Verifica a precisão de preço que o usuário possui.
	If nArredVlr = 0
		SU7->(dbSetOrder(4))  // U7_FILIAL, U7_CODUSU.
		If SU7->(msSeek(xFilial() + __cUserId, .F.))
			If !empty(SU7->U7_XPRECIS) .and. SU7->U7_XPRECIS <> "D"
				nPrecis := val(SU7->U7_XPRECIS)
			Else
				nPrecis := val(TkPosto(SU7->U7_COD, "U0_XPRECIS"))
			Endif
			If nPrecis > 0
				nArredVlr  := nPrecis
			Else
				nArredVlr  := TamSX3("UB_VRUNIT")[2]
			Endif
		Endif
	Endif

	// Variáveis utilizadas no cálculo de preço.
	cProduto := GdFieldGet("UB_PRODUTO")
	nPerICMS := MaFisRet(n, "IT_ALIQICM")
	nRedICMS := MaFisRet(n, "IT_PREDIC")
	nBaseDes := MaFisRet(n, "IT_BASEDES")
	If nBaseDes > 0
		nPerICC   := MaFisRet(n, "IT_ALIQCMP")
		nPerFECPC := MaFisRet(n, "IT_ALFCCMP")
	Endif

	// Busca o preço de contrato.
	nPrcUnit := U_MA020Ctr(M->UA_CLIENTE, M->UA_LOJA, cProduto, max(nPerICMS, nPerICC + nPerFECPC), @cContrato,,, nRedICMS)
	GdFieldPut("UB_XCONTR", cContrato)

	// Se não há contrato, pega preço de venda padrão.
	If nPrcUnit = 0
		If !lEdi
			nPrcUnit := U_ProdPrv(cProduto, M->UA_CLIENTE, M->UA_LOJA, M->UA_PROSPEC)[1]
		Else
			nPrcUnit := GdFieldGet("UB_VRUNIT")
		EndIf
	Endif

	// Arredonda o preço unitário.
	nPrcUnit := Round(nPrcUnit, nArredVlr)

	// Executa gatilhos do campo.
	U_AtuCamp("UB_VRUNIT", nPrcUnit)
Endif

Return lRet


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Tiago Melo
// Modulo   : Compras
// Função   : SitTribT(cProduto, cTes)
// Descrição: Gatilho que traz a origem da situação tributária de acordo com a
//            saída (transferência).
// Retorno  : Código da situação tributária.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 18/11/16 | Tiago Melo        | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function SitTribT(cProduto, cTes)
Local cRetorno := ""

SD2->(DbSetOrder(3)) //D2_FILIAL, D2_DOC, D2_SERIE, D2_CLIENTE, D2_LOJA, D2_COD, D2_ITEM.
If SD2->(MsSeek(SUBSTR(cloja,3,2) + cNFiscal + cSerie + cA100For + "00" + cFilAnt + cProduto))
	cRetorno = SubStr(SD2->D2_CLASFIS,1,1)+Posicione("SF4", 1, xFilial("SF4") + cTes, "F4_SITTRIB")
Endif

Return cRetorno


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Materiais
// Função   : NNRDesc
// Descrição: Retorna a descrição do armazém.
// Retorno  : Descrição do armazém.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 15/12/16 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function NNRDesc(cLocal)
Local cRet       := ""

NNR->(dbSetOrder(1))  // NNR_FILIAL, NNR_CODIGO.
If NNR->(dbSeek(xFilial() + cLocal, .F.))
	cRet := AllTrim(NNR->NNR_DESCRI)
Endif

Return cRet


// ##############################################################################
// Projeto  : PROT-CAP
// Autor    : Rodrigo Nunes TOAT
// Modulo   : Call Center
// Função   : SIMFRETE
// Descrição: Cálculo do frete.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 24/01/17 | Rodrigo Nunes     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function SimFrete(cCNPJDes, oOrcamento)
Local lRet       := .T.
Local aAreaSB1   := SB1->(GetArea())
Local oGet1
Local oGet2
Local oGet3
Local oGet4
Local oGet5
Local oGet6
Local oGet7
Local oGet8
Local oGet9
Local oGet10
Local oGet11
Local oSay1
Local oSay2
Local oSay3
Local oSay4
Local oSay5
Local oSay6
Local oSay7
Local aCabec     := {}
Local aTam       := {}
Local cDesTpOp
Local cDesClFr
Local oSize
Local aPosEnch
Local nValTot    := 0
Local nTotKg     := 0
Local nTotM3     := 0
Local oOk        := LoadBitmap(nil, "LBOK")
Local oNo        := LoadBitmap(nil, "LBNO")
Local nPsProd    := GdFieldPos("UB_PRODUTO", oOrcamento:aHeader)
Local nPsQtde    := GdFieldPos("UB_QUANT",   oOrcamento:aHeader)
Local nPsVlr     := GdFieldPos("UB_VLRITEM", oOrcamento:aHeader)
Local aListbox   := {}
Local nlX, nLin

Local oDlg, oListbox
Local cCodRem    := ""
Local cDesRem    := ""
Local cCodDes    := ""
Local cDsDest    := ""

Local nAltura    := 0
Local nLargura   := 0
Local nComprim   := 0

Private cTpOp    := ""
Private cCdClFr  := ""
Private nVlCarga := 0
Private nPsReal  := 0
Private nVolume  := 0
Private Inclui   := .T.
Private Altera   := .F.

cTpOp    := PADR(AllTrim(GetMv("MV_TPOPEMB")), TamSX3("GWN_CDTPOP")[1])
cDesTpOp := Posicione("GV4", 1, xFilial("GV4") + cTpOp,   "GV4_DSTPOP")

cCdClFr  := PADR(AllTrim(GetMv("MV_CDCLFR")),  TamSX3("GWN_CDCLFR")[1])
cDesClFr := Posicione("GUB", 1, xFilial("GUB") + cCdClFr, "GUB_DSCLFR")

cCodRem := FWSM0Util():GetSM0Data(cEmpAnt, cFilAnt, {"M0_CGC"})[1][2]
GFEX011Desc(3, cCodRem, @cDesRem)
If !Empty(cCNPJDes)
	cCodDes := cCNPJDes
	GFEX011Desc(3, cCodDes, @cDsDest)
Else
	cCodDes := CriaVar("GW1_CDDEST", .F.)
	cDsDest := CriaVar("GU3_NMEMIT", .F.)
EndIf

// Carrega os dados para o cálculo do frete.
SB1->(dbSetOrder(1))  // B1_FILIAL, B1_COD.
for nlX := 1 to Len(oOrcamento:aCols)
	If !aTail(oOrcamento:aCols[nlX])
		SB1->(msSeek(xFilial() + oOrcamento:aCols[nlX, nPsProd], .F.))
		If Empty(SB1->B1_PESBRU)
			Alert("Peso do produto " +Alltrim(SB1->B1_COD)+ " está zerado. Corrigir o cadastro do produto.")
			lRet := .F.
			Exit
		Endif
		nValTot += oOrcamento:aCols[nlX, nPsVlr]
		nTotKg  += (oOrcamento:aCols[nlX, nPsQtde] * SB1->B1_PESBRU)

		nAltura  := U_ProdCpo(SB1->B1_COD, "B5_ALTURA")
		nLargura := U_ProdCpo(SB1->B1_COD, "B5_LARG")
		nComprim := U_ProdCpo(SB1->B1_COD, "B5_COMPR")

		/*  // removido temporariamente, enquanto cadastro de produtos esta incompleto.
		If Empty(SB1->B1_XQEM) .OR. Empty(nAltura) .OR. Empty(nLargura) .OR. Empty(nComprim)
			Alert("Há campos para medição do volume de carga (m³) em branco. Corrigir o cadastro do produto.")
			lRet := .F.
			Exit
		Else
			nTotCXM3 := (nAltura * nLargura * nComprim)
			nTotUNM3 := (nTotCXM3 / SB1->B1_XQEM)
			nTotM3	 += (oOrcamento:aCols[nlX, nPsQtde] * nTotUNM3)
		Endif
		*/

		// Remover essas 3 linhas apos cadastro de produtos estar completo.
		nTotCXM3  := (nAltura * nLargura * nComprim)
		nTotUNM3  := (nTotCXM3 / SB1->B1_XQEM)
		nTotM3	  += (oOrcamento:Acols[nlX][3] * nTotUNM3)
	EndIf
Next nlX

If lRet
	nVlCarga := nValTot
	nPsReal  := nTotKg
	nVolume  := nTotM3
	If SA4->(FieldPos("A4_XFATCOR") > 0 .and. A4_XFATCOR > 0)
		nPsReal := (nPsReal * SA4->A4_XFATCOR)
	Endif

	oSize := FWDefSize():New(.T.)
	oSize:AddObject("ENCHOICE", 100, 60, .T., .T.)
	oSize:SetWindowSize({000, 000, 665, 700})
	oSize:lLateral := .F.  // Calculo vertical
	oSize:Process()

	aPosEnch := {oSize:GetDimension("ENCHOICE","LININI"),;
                 oSize:GetDimension("ENCHOICE","COLINI"),;
                 oSize:GetDimension("ENCHOICE","LINEND"),;
                 oSize:GetDimension("ENCHOICE","COLEND")}

	DEFINE MSDIALOG oDlg TITLE "Simulação de frete" ;
		FROM oSize:aWindSize[1],oSize:aWindSize[2] ;
		TO oSize:aWindSize[3]-100,oSize:aWindSize[4] ;
		COLORS 0, 16777215 OF oMainWnd PIXEL

	nLin := aPosEnch[1] + 5
   	@ nLin, aPosEnch[2]+020 SAY oSay1 PROMPT "Tipo Operação"           SIZE 050, 010 OF oDlg COLORS 0, 16777215 PIXEL
   	@ nLin + 10, aPosEnch[2]+020 MSGET oGet1  VAR cTpOp 	SIZE 065, 010 OF oDlg WHEN .T. PICTURE "@!" COLORS 0, 16777215 F3 "GV4" Valid GFEX011Desc(1,cTpOp,@cDesTpOp) READONLY PIXEL hasbutton
   	@ nLin + 10, aPosEnch[2]+090 MSGET oGet2  VAR cDesTpOp 	SIZE 156, 010 OF oDlg WHEN .T. PICTURE "@!" COLORS 0, 16777215 READONLY PIXEL
   	nLin += 25

   	@ nLin, aPosEnch[2]+020 SAY oSay2 PROMPT "Classificação Frete"     SIZE 050, 010 OF oDlg COLORS 0, 16777215 PIXEL
   	@ nLin + 10, aPosEnch[2]+020 MSGET oGet3  VAR cCdClFr 	SIZE 065, 010 OF oDlg WHEN .T. PICTURE "@!" COLORS 0, 16777215 F3 "GUB" Valid GFEX011Desc(2,cCdClFr,@cDesClFr) READONLY PIXEL hasbutton
	@ nLin + 10, aPosEnch[2]+090 MSGET oGet4  VAR cDesClFr 	SIZE 156, 010 OF oDlg WHEN .T. PICTURE "@!" COLORS 0, 16777215 READONLY PIXEL
	nLin += 25

   	@ nLin, aPosEnch[2]+020 SAY oSay3 PROMPT "Valor total da carga"    SIZE 057, 010 OF oDlg COLORS 0, 16777215 PIXEL
   	@ nLin + 10, aPosEnch[2]+020 MSGET oGet5  VAR nVlCarga  SIZE 055, 010 OF oDlg WHEN .T. PICTURE PESQPICT("GW8", "GW8_VALOR") COLORS 0, 16777215 READONLY PIXEL hasbutton
   	@ nLin, aPosEnch[2]+087 SAY oSay4 PROMPT "Peso total (Kg)"         SIZE 078, 010 OF oDlg COLORS 0, 16777215 PIXEL
   	@ nLin + 10, aPosEnch[2]+087 MSGET oGet6  VAR nPsReal 	SIZE 055, 010 OF oDlg WHEN .T. PICTURE PESQPICT("GW8", "GW8_PESOR") COLORS 0, 16777215 READONLY PIXEL hasbutton
   	@ nLin, aPosEnch[2]+154 SAY oSay7 PROMPT "Volume total (m³)"       SIZE 078, 010 OF oDlg COLORS 0, 16777215 PIXEL
   	@ nLin + 10, aPosEnch[2]+154 MSGET oGet11 VAR nVolume 	SIZE 055, 010 OF oDlg WHEN .T. PICTURE PESQPICT("GW8", "GW8_VOLUME") COLORS 0, 16777215 READONLY PIXEL hasbutton
   	nLin += 25

   	@ nLin, aPosEnch[2]+020 SAY oSay6 PROMPT "Remetente"               SIZE 057, 010 OF oDlg COLORS 0, 16777215 PIXEL
   	@ nLin + 10, aPosEnch[2]+020 MSGET oGet7  VAR cCodRem 	SIZE 060, 010 OF oDlg WHEN .T. PICTURE "@!" COLORS 0, 16777215 F3 "GU3" Valid GFEX011Desc(3, cCodRem, @cDesRem) READONLY PIXEL hasbutton
   	@ nLin + 10, aPosEnch[2]+087 MSGET oGet8  VAR cDesRem 	SIZE 155, 010 OF oDlg WHEN .T. PICTURE "@!" COLORS 0, 16777215 READONLY PIXEL
   	nLin += 25

   	@ nLin, aPosEnch[2]+020 SAY oSay5 PROMPT "Destinatário"            SIZE 057, 010 OF oDlg COLORS 0, 16777215 PIXEL
   	@ nLin + 10, aPosEnch[2]+020 MSGET oGet9  VAR cCodDes 	SIZE 060, 010 OF oDlg WHEN .T. PICTURE "@!" COLORS 0, 16777215 F3 "GU3" Valid GFEX011Desc(3, cCodDes, @cDsDest)READONLY PIXEL hasbutton
   	@ nLin + 10, aPosEnch[2]+087 MSGET oGet10 VAR cDsDest 	SIZE 155, 010 OF oDlg WHEN .T. PICTURE "@!" COLORS 0, 16777215 READONLY PIXEL
   	nLin += 25

    aListbox := GFEX011SIM(cCodRem, cCodDes)
    aCabec   := {"", "Código", "Val.Frete", "Prev. (dias)", "Nome", "% da Nota"}
    aTam     := {05, 10, 70, 50, 30, 15}
    oListbox := TWBrowse():New(nLin, aPosEnch[2] + 020, 320, 120,, aCabec, aTam, oDlg,,,,,,,,,,,, .F.,, .T.,, .F.)
	oListbox:SetArray(aListbox)
	oListbox:bLine      := {|| aListbox[oListbox:nAT, 3, 1] := If(aListbox[oListbox:nAT, 1], oOk, oNO), aListbox[oListbox:nAT, 3]}
	oListbox:bLDblClick := {|| BSCTRANSP(oListbox, aListbox)}

    ACTIVATE MSDIALOG oDlg ON INIT EnchoiceBar(oDlg, {|| ATUVAL(aListbox), ODlg:End()}, {||ODlg:End()},,) CENTERED

	RestArea(aAreaSB1)
Endif

Return


// ##############################################################################
// Projeto  : PROT-CAP
// Autor    : Rodrigo Nunes TOAT
// Modulo   : Call Center
// Função   : SIMFRETE
// Descrição: Valida e aplica o gatilho da descrição
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 24/01/17 | Rodrigo Nunes     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
Static Function GFEX011Desc(nOp,cCod,cDesc)
	Local lRet := .T.

	If nOp == 1 //Tipo de Operação
		If !Empty(cCod)
			If GFEExistC("GV4",,cCod,"GV4->GV4_SIT=='1'")
		   		cDesc := POSICIONE("GV4",1,xFilial("GV4")+cCod,"GV4_DSTPOP")
		   	Else
	   			lRet := .F.
	   		EndIf
	   	Else
	   		cDesc := ''
	   	EndIf
	ElseIf nOp == 2 // Classificação de Frete
		If !Empty(cCod)
			If GFEExistC("GUB",,cCod,"GUB->GUB_SIT=='1'")
		   		cDesc := POSICIONE("GUB",1,xFilial("GUB")+cCod,"GUB_DSCLFR")
	   		Else
	    		lRet := .F.
	   		EndIf
	   	Else
	   		cDesc := ''
	   	EndIf
	ElseIf nOp == 3  // Destinatario e remetente
		If GFEExistC("GU3",,cCod,"GU3->GU3_SIT=='1'")
	   		cDesc := POSICIONE("GU3",1,xFilial("GU3")+cCod,"GU3_NMEMIT")
	 	Else
	   		lRet := .F.
	 	EndIf
    ElseIf nOp == 4  // Tipo de Veículo
    	If !Empty(cCod)
    		If GFEExistC("GV3",,cCod,"GV3->GV3_SIT=='1'")
		   		cDesc := POSICIONE("GV3",1,xFilial("GV3")+cCod,"GV3_DSTPVC")
		   	Else
		   		lRet := .F.
		 	EndIf
		Else
			cDesc := ""
    	EndIf
	EndIf

Return lRet


// ##############################################################################
// Projeto  : PROT-CAP
// Autor    : Rodrigo Nunes TOAT
// Modulo   : Call Center
// Função   : SIMFRETE
// Descrição: Calcula o frete
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 24/01/17 | Rodrigo Nunes     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
Static Function GFEX011SIM(cCodRem, cCodDes)
	Local aListTrans := {}
	Local oModelSim  := FWLoadModel("GFEX010")
	Local oModelNeg  := oModelSim:GetModel("GFEX010_01")
	Local oModelAgr  := oModelSim:GetModel("DETAIL_01")  // oModel do grid "Agrupadores"
	Local oModelDC   := oModelSim:GetModel("DETAIL_02")  // oModel do grid "Doc Carga"
	Local oModelIt   := oModelSim:GetModel("DETAIL_03")  // oModel do grid "Item Carga"
	Local oModelTr   := oModelSim:GetModel("DETAIL_04")  // oModel do grid "Trechos"
	Local oModelInt  := oModelSim:GetModel("SIMULA")
	Local oModelCal1 := oModelSim:GetModel("DETAIL_05")
	Local oModelCal2 := oModelSim:GetModel("DETAIL_06")
	Local cQuery     := ""
	Local cAliasTop  := ""
	Local lSelec     := .F.
	Local nOrdem     := 0
	Local nDias      := 0
	Local aItem      := {}
	Local nCont

	oModelSim:SetOperation(3)
	oModelSim:Activate()

	dbSelectArea("GV5")
	dbSetOrder(3)
	dbSeek(xFilial("GV5")+"1")

	oModelNeg:LoadValue('CONSNEG',    "1" )

	// Agrupadores
	oModelAgr:LoadValue('GWN_NRROM',  "01" )
	oModelAgr:LoadValue('GWN_CDCLFR', cCdClFr)
	oModelAgr:LoadValue('GWN_CDTPOP', cTpOp)
	oModelAgr:LoadValue('GWN_DOC',    "ROMANEIO")

	// Documento de Carga
	oModelDC:LoadValue('GW1_EMISDC', cCodRem)
	oModelDC:LoadValue('GW1_NRDC',   "00001")
	oModelDC:LoadValue('GW1_CDTPDC', GV5->GV5_CDTPDC)
	oModelDC:LoadValue('GW1_CDREM',  cCodRem)
	oModelDC:LoadValue('GW1_CDDEST', cCodDes)
	oModelDC:LoadValue('GW1_TPFRET', "1")
	oModelDC:LoadValue('GW1_ICMSDC', "2")
	oModelDC:LoadValue('GW1_USO',    "1")
	oModelDC:LoadValue('GW1_NRROM',  "01")
	oModelDC:LoadValue('GW1_QTUNI',  1)

	// Trechos
	oModelTr:LoadValue('GWU_EMISDC', cCodRem)
	oModelTr:LoadValue('GWU_NRDC',   "00001")
	oModelTr:LoadValue('GWU_CDTPDC', GV5->GV5_CDTPDC)
	oModelTr:LoadValue('GWU_SEQ',    "01")
	oModelTr:LoadValue('GWU_NRCIDD', POSICIONE("GU3",1,xFilial("GU3")+cCodDes,"GU3_NRCID"))
	oModelTr:LoadValue('GWU_CDTPVC', CriaVar("GWU_CDTPVC", .F.))

	// Itens
	oModelIt:LoadValue('GW8_EMISDC', cCodRem)
	oModelIt:LoadValue('GW8_NRDC',   "00001")
	oModelIt:LoadValue('GW8_CDTPDC', GV5->GV5_CDTPDC)
	oModelIt:LoadValue('GW8_ITEM',   "ItemA"  )
	oModelIt:LoadValue('GW8_DSITEM', "Item Generico")
	oModelIt:LoadValue('GW8_CDCLFR', cCdClFr)
	oModelIt:LoadValue('GW8_PESOR',  nPsReal)
	oModelIt:LoadValue('GW8_VALOR',  nVlCarga)
	oModelIt:LoadValue('GW8_VOLUME', nVolume)
	oModelIt:LoadValue('GW8_TRIBP',  "1")

   	// Dispara a simulação
	oModelInt:SetValue("INTEGRA",    "A")

	If oModelCal1:GetQtdLine() > 1 .Or. !Empty( oModelCal1:GetValue('C1_NRCALC'  ,1) )
		For nCont := 1 to oModelCal1:GetQtdLine()
			oModelCal1:GoLine(nCont)

			cQuery := "select SA4.A4_COD, SA4.A4_NOME, SA4.A4_CGC " + CRLF
			cQuery += "from " + RetSQLName("SA4") + " SA4 " + CRLF
			cQuery += "where SA4.D_E_L_E_T_ = ' ' " + CRLF
			cQuery += "and SA4.A4_FILIAL  = '" + xFilial("SA4") + "' " + CRLF
			cQuery += "and SA4.A4_CGC     = '" + oModelCal2:GetValue('C2_CDEMIT', 1) + "' " + CRLF
			cQuery += "and SA4.A4_MSBLQL  <> '1' " + CRLF  // Não bloqueado.
			cQuery += "and SA4.A4_XPROP   = '1' " + CRLF  // Transportadora própria.
			cQuery += "and SA4.A4_XVTRANS <> '4' " + CRLF // Via de Transporte Aereo
			cQuery += "order by SA4.R_E_C_N_O_ desc " + CRLF
			cAliasTop := MPSysOpenQuery(cQuery)

			If (cAliasTop)->(!eof())
				lSelec := (cAliasTop)->A4_COD == M->UA_TRANSP
				If lSelec // remover o IF logo apos o cadastro de produtos ser atualizado
					// Novo calculo de leadtime de entrega.
					nDias := RetPrzCalc(M->UA_ESTE, M->UA_XCDMUNE, (cAliasTop)->A4_COD)

					nOrdem  := If(lSelec, -1, oModelCal1:GetValue('C1_VALFRT', nCont))
					nPercNf := Round(((oModelCal1:GetValue('C1_VALFRT', nCont) / nVlCarga) * 100), 2)
					aItem   := {nil, (cAliasTop)->A4_COD, Transform(oModelCal1:GetValue('C1_VALFRT', nCont), cPictVlr), nDias, (cAliasTop)->A4_NOME, nPercNf}
					aAdd(aListTrans, {lSelec, nOrdem, aItem})
				EndIf
			Endif
			(cAliasTop)->(dbCloseArea())
		Next nCont
	Endif

	If empty(aListTrans)
		lSelec := .F.
		aItem  := {nil, , "", Transform(0, cPictVlr), 0, ""}
		aAdd(aListTrans, {lSelec, nOrdem, aItem})
	Else
		aSort(aListTrans,,, {|x, y| x[2] < y[2]})
	Endif

Return aListTrans


// ##############################################################################
// Projeto  : PROT-CAP
// Autor    : Rodrigo Nunes TOAT
// Modulo   : Call Center
// Função   : SIMFRETE
// Descrição: Atualiza tela de vendas com as informaçoes selecionada
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 24/01/17 | Rodrigo Nunes     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
Static Function BSCTRANSP(oListbox, aListbox)
Local nX

For nX := 1 to Len(aListBox)
	If nX = oListbox:nAt .and. !empty(aListbox[oListbox:nAt, 3, 2])
		aListbox[oListbox:nAt, 1] := !aListbox[oListbox:nAt, 1]
	Else
		aListbox[nX][1] := .F.
	EndIf
Next nX
oListbox:refresh()

Return


// ##############################################################################
// Projeto  : PROT-CAP
// Autor    : Rodrigo Nunes TOAT
// Modulo   : Call Center
// Função   : ATUVAL
// Descrição: Atualiza tela de vendas com as informaçoes selecionada
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 24/01/17 | Rodrigo Nunes     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
Static Function ATUVAL(aListbox)
Local nX

For nX := 1 to Len(aListBox)
	If aListbox[nX][1]
		U_AtuCamp("UA_TRANSP",  aListbox[nX, 3, 2])
		// U_AtuCamp("UA_FRETE",   aListbox[nX, 3, 4])
		Exit
	EndIf
Next nX

Return


// ##############################################################################
// Projeto  : PROT-CAP
// Autor    : Joao Leao
// Modulo   : Faturamento
// Função   : PVPComp
// Descrição: Busca o pedido pela Ordem de Compra e posiciona o registro na SC5.
// Retorno  : Nenhum
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 04/12/15 | Joao Leao         | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function PVPComp()
Local cPerg := "PVPCOM"
Local oObjBrow

//Se o grupo de perguntas nao existir, cria.
PutSx1(cPerg,"01", "Ordem de Compra ?" , "Ordem de Compra ?" , "Ordem de Compra ?" ,"mv_ch1" ,"C",TAMSX3("C6_NUMPCOM")[1],00,00,"G","","PVPCOM"   ,"","","mv_par01")
PutSx1(cPerg,"02", "Filial", "Filial", "Filial","mv_ch2" ,"C",TAMSX3("C6_FILIAL")[1],00,00,"S","",""   ,"","","mv_par02")
PutSx1(cPerg,"03", "Pedido" , "Pedido" , "Pedido" ,"mv_ch3" ,"C",TAMSX3("C6_NUM")[1],00,00,"S","",""   ,"","","mv_par03")

If Pergunte(cPerg,.T.)
	SC5->(dbSetOrder(1)) //C5_FILIAL,C5_NUM
	If SC5->(dbSeek(MV_PAR02+MV_PAR03))
		If FunName() == "MATA410"
			oObjBrow := GetObjBrow()
			oObjBrow:GoTo(Recno())
			oObjBrow:Refresh()
		Else
			oBrowse:GoTo(Recno())
			oBrowse:Refresh()
		EndIf
	EndIf
EndIf

Return


// ##############################################################################
// Projeto  : PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento
// Função   : PVWebs
// Descrição: Amarra o pedido website ao pedido Protheus.
// Retorno  : Nenhum
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 03/01/17 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function PVWebs()
Local cVTexID3   := "PTC-" + SubStr(CriaVar("C5_XIDVTV3", .F.), 5)
Local aParam     := {}
Local aRetorno   := {}

If SC5->C5_FILIAL = xFilial("SC5", MATRIZ)
	aAdd(aParam, {1, "Ped. NetSuprimentos", cVTexID3,,,,, 65, .F.})
	ParamBox(aParam, "Informe pedido NetSuprimentos", @aRetorno, {|| PVWebs(aRetorno[1])},,,,,,, .F., .F.)
Else
	MsgAlert("Função válida somente para pedidos da matriz.", "Atenção")
Endif

Return


// ##############################################################################
// Projeto  : PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento
// Função   : PVWebs
// Descrição: Amarra o pedido website ao pedido Protheus.
// Retorno  : Nenhum
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 03/01/17 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
Static Function PVWebs(cVTexID3)
Local lRet       := .F.
Local cRet       := ""
Local cStsCode   := ""
Local dDueDate   := stod("")
Local oPedido

// Busca o pedido NetSuprimentos via webservice.
msAguarde({|| cRet := U_VTEXGet("PROT", "/api/oms/pvt/orders/" + cVTexID3,, "orders\PROT\" + cVTexID3, @cStsCode)}, "Consultando pedido...", "NetSuprimentos", .F.)

// Gera reserva de todos os itens do pedido.
If cStsCode = "200"
	// Pega os dados do pedido.
	FWJsonDeserialize(cRet, @oPedido)
	cSequence  := oPedido:sequence
	cOrderMPO  := oPedido:marketPlaceOrderId
	dDueDate   := stod(StrTran(Left(oPedido:creationDate, 10), "-", ""))

	// Preenche os dados no pedido.
	RecLock("SC5", .F.)
	SC5->C5_XIDVTV3 := cVTexID3
	SC5->C5_XIDVTEX := val(cSequence)
	SC5->C5_XPEDLV  := cOrderMPO
	SC5->C5_XDATA   := dDueDate
	SC5->(msUnLock())

	MsgInfo("Pedido amarrado com sucesso.", "Atenção")
	lRet := .T.
Else
	MsgAlert("Pedido inválido.", "Atenção")
Endif

Return lRet


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : SB1xSB4
// Descrição: Retorna matriz com os campos correlacionados do SB1 e SB4.
// Retorno  : Matriz onde as linhas são:
//            {posição campo SB1, posição campo SB4, campo SB1, campo SB4}
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 01/03/17 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function SB1xSB4()
Local aCamposSB1 := {}
Local aCamposSB4 := {}
Local cCampoSB1  := ""
Local cCampoSB4  := ""
Local aCamposExc := {"B4_COD", "B4_DESC", "B4_XAPWEB", "B4_XPROWEB", "B4_XPROVLD", "B4_XFATOR1", "B4_XFATOR2", "B4_XFATOR3", "B4_PRV1", "B4_PRV2", "B4_PRV3", "B4_XESP", "B4_XQE", "B4_XQEM", "B4_XPE","B4_CONTA"}
Local nX 		 := 0

// Verifica quais campos existem em ambas tabelas.
Static aSB1SB4Cpo := {}
If empty(aSB1SB4Cpo)
	aCamposSB1 := SB1->(dbStruct())
	aCamposSB4 := SB4->(dbStruct())
	For nX := 1 to len(aCamposSB4)
		cCampoSB4 := aCamposSB4[nX, 1]
		If aScan(aCamposExc, RTrim(cCampoSB4)) == 0
			If X3Uso(GetSX3Cache(cCampoSB4, "X3_USADO")) .and. cNivel >= GetSX3Cache(cCampoSB4, "X3_NIVEL")

				If AllTrim(cCampoSB4) == "B4_XQE"
					cCampoSB1 := "B1_QE"
				ElseIf AllTrim(cCampoSB4) == "B4_XPE"
					cCampoSB1 := "B1_PE"
				Else
					cCampoSB1 := "B1_" + SubStr(cCampoSB4, 4)
				EndIf

				If aScan(aCamposSB1, {|x| AllTrim(x[1]) == AllTrim(cCampoSB1)}) > 0
					aAdd(aSB1SB4Cpo, {SB1->(FieldPos(cCampoSB1)), SB4->(FieldPos(cCampoSB4)), cCampoSB1, cCampoSB4})
				Endif
			Endif
		Endif
	Next nX
Endif

Return aSB1SB4Cpo


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : SB5xSB4
// Descrição: Retorna matriz com os campos correlacionados do SB5 e SB4.
// Retorno  : Matriz onde as linhas são:
//            {posição campo SB5, posição campo SB4, campo SB5, campo SB4}
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 01/03/17 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function SB5xSB4()
Local aDePara    := {}
Local cCampoSB5  := ""
Local cCampoSB4  := ""
Local nX 		 := 0

// Verifica quais campos existem em ambas tabelas.
Static aSB5SB4Cpo := {}
If empty(aSB5SB4Cpo)

	aAdd(aDePara, {"B5_XFATOR1", "B4_XFATOR1"})
	aAdd(aDePara, {"B5_XFATOR2", "B4_XFATOR2"})
	aAdd(aDePara, {"B5_XFATOR3", "B4_XFATOR3"})
	aAdd(aDePara, {"B5_PRV1",    "B4_PRV1"})
	aAdd(aDePara, {"B5_PRV2",    "B4_PRV2"})
	aAdd(aDePara, {"B5_PRV3",    "B4_PRV3"})
	aAdd(aDePara, {"B5_XESPEC",  "B4_XESPEC"})
	aAdd(aDePara, {"B5_XAPLIC",  "B4_XAPLIC"})
	aAdd(aDePara, {"B5_ALTURA",  "B4_XALTURA"})
	aAdd(aDePara, {"B5_LARG",    "B4_XLARGUR"})
	aAdd(aDePara, {"B5_COMPR",   "B4_XCOMPR"})
	aAdd(aDePara, {"B5_CEME",    "B4_DESC"})
	aAdd(aDePara, {"B5_XDIVIS",  "B4_XDIVIS"})
	aAdd(aDePara, {"B5_XCATEG",  "B4_XCATEG"})
	aAdd(aDePara, {"B5_XSUBCAT", "B4_XSUBCAT"})
	aAdd(aDePara, {"B5_XFAMIL",  "B4_XFAMIL"})

	For nX := 1 to len(aDePara)
		cCampoSB5 := aDePara[nX, 1]
		cCampoSB4 := aDePara[nX, 2]
		aAdd(aSB5SB4Cpo, {SB5->(FieldPos(cCampoSB5)), SB4->(FieldPos(cCampoSB4)), cCampoSB5, cCampoSB4})
	Next nX
Endif

Return aSB5SB4Cpo


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : ProdRes
// Descrição: Abre tela com detalhes da reserva de estoque do produto.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 20/09/13 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function ProdRes(cProduto)
Local aArea      := GetArea()
Local aLegendas  := U_MA061Leg()  // Legenda dos pedidos.
Local oLegenda, nLeg

Local oDlgEst, oGrpRod
Local oGrPed, oGdPed
Local oGrTrn, oGdTrn
Local oSayPed, oSayTrn, oSayTot
Local nLin, nCol
Local aTotal     := {0, 0}

Local aCampos[0], aItem[0], aCabec[0]
Local aCposPed[0], aTamCpoPed[0], aItensPed[0]
Local aCposTrn[0], aTamCpoTrn[0], aItensTrn[0]
Local nPosPed, nPosQtd, nPosObs

Local cQuery     := ""
Local cAliasTop  := ""

Default cProduto := SB1->B1_COD

// Define os campos da matriz de pedidos.
aCposPed   := {"", "C6_NUM", "C6_ITEM", "C6_QTDVEN", "C6_QTDEMP", "C5_EMISSAO", "C5_XDTPFAT", "C5_CLIENTE", "C5_LOJACLI", "A1_NOME"}
aTamCpoPed := {5, 30, 15, 35, 35, 35, 35, 30, 15, 120}
nPosPed    := aScan(aCposPed, "C6_NUM")
nPosQtd    := aScan(aCposPed, "C6_QTDVEN")
nPosObs    := aScan(aCposPed, "A1_NOME")

// Define os campos da matriz de transferência.
aCposTrn   := {"", "C6_NUM", "C5_LOJACLI", "A1_MUN", "C6_QTDVEN", "C6_QTDEMP", "C5_EMISSAO", "C5_XDTPFAT"}
aTamCpoTrn := {5, 30, 20, 50, 35, ,35 ,35, 35}

// Monta e excuta query.
cQuery := "select SA1.R_E_C_N_O_ SA1RecNo, SC5.R_E_C_N_O_ SC5RecNo, SC6.R_E_C_N_O_ SC6RecNo "

cQuery += "from " + RetSQLName("SC5") + " SC5 with (noLock) "

cQuery += "inner join " + RetSQLName("SC6") + " SC6 with (noLock) on SC6.D_E_L_E_T_ = ' ' "
cQuery += "and SC6.C6_FILIAL  = '" + xFilial("SC6") + "' "
cQuery += "and SC6.C6_NUM     = SC5.C5_NUM "

cQuery += "inner join " + RetSQLName("SF4") + " SF4 with (noLock) on SF4.D_E_L_E_T_ = ' ' "
cQuery += "and SF4.F4_FILIAL  = '" + xFilial("SF4") + "' "
cQuery += "and SF4.F4_CODIGO  = SC6.C6_TES "
cQuery += "and SF4.F4_ESTOQUE = 'S' "

cQuery += "left  join " + RetSQLName("SA1") + " SA1 with (noLock) on SA1.D_E_L_E_T_ = ' ' "
cQuery += "and SA1.A1_FILIAL  = '" + xFilial("SA1") + "' "
cQuery += "and SA1.A1_COD     = SC5.C5_CLIENTE "
cQuery += "and SA1.A1_LOJA    = SC5.C5_LOJACLI "
cQuery += "and SC5.C5_TIPO    not in ('D', 'B') "

cQuery += "where SC5.D_E_L_E_T_ = ' ' "
cQuery += "and SC5.C5_FILIAL  = '" + xFilial("SC5") + "' "
cQuery += "and SC6.C6_PRODUTO = '" + cProduto + "' "
cQuery += "and SC6.C6_QTDVEN  > SC6.C6_QTDENT and SC6.C6_BLQ not in ('R', 'S') "

cQuery += "order by SC5.C5_XDTPFAT, SC6.C6_NUM, SC6.C6_ITEM "
msAguarde({|| cAliasTop := MPSysOpenQuery(cQuery)}, "Aguarde", "Verificando reservas...", .F.)

Do While (cAliasTop)->(!eof())
	// Posiciona tabelas.
	SA1->(dbGoTo((cAliasTop)->SA1RecNo))
	SC5->(dbGoTo((cAliasTop)->SC5RecNo))
	SC6->(dbGoTo((cAliasTop)->SC6RecNo))

	If SA1->A1_COD <> '999999'
		// Pedido.
		aAdd(aItensPed, {})
		aItem   := aTail(aItensPed)
		aCampos := aCposPed
		aTotal[1] += SC6->(C6_QTDVEN - C6_QTDENT)
	Else
		// Transferência.
		aAdd(aItensTrn, {})
		aItem   := aTail(aItensTrn)
		aCampos := aCposTrn
		aTotal[2] += SC6->(C6_QTDVEN - C6_QTDENT)
	Endif

	// Adiciona legenda.
	nLeg := aScan(aLegendas, {|aLeg| SC5->(&(aLeg[4]))})
	If nLeg = 0
		oLegenda := LoadBitmap(nil, "")
	Else
		oLegenda := LoadBitmap(nil, aLegendas[nLeg, 2])
	Endif
	aAdd(aItem, oLegenda)

	// Acerta os campos do pedido.
	aEval(aCampos, {|cCpo| aAdd(aItem, Posicione("SX3", 2, cCpo, "Transform(&(X3_ARQUIVO + '->' + X3_CAMPO), X3_PICTURE)"))}, 2)

	// Próximo item.
	(cAliasTop)->(dbSkip())
EndDo
(cAliasTop)->(dbCloseArea())

// Monta e excuta query.
cQuery := "select SC0.R_E_C_N_O_ SC0RecNo "

cQuery += "from " + RetSQLName("SC0") + " SC0 with (noLock) "

cQuery += "where SC0.D_E_L_E_T_ = ' ' "
cQuery += "and SC0.C0_FILIAL  = '" + xFilial("SC0") + "' "
cQuery += "and SC0.C0_PRODUTO = '" + cProduto + "' "
cQuery += "and SC0.C0_QUANT + SC0.C0_QTDPED <> 0 "

cQuery += "order by SC0.C0_FILIAL, SC0.C0_EMISSAO, SC0.C0_PRODUTO, SC0.C0_LOCAL, SC0.C0_NUM "
msAguarde({|| cAliasTop := MPSysOpenQuery(cQuery)}, "Aguarde", "Verificando reservas...", .F.)

Do While (cAliasTop)->(!eof())
	// Posiciona tabelas.
	SC0->(dbGoTo((cAliasTop)->SC0RecNo))

	// Pedido.
	aAdd(aItensPed, {})
	aItem   := aTail(aItensPed)
	aCampos := aCposPed
	aTotal[1] += SC0->(C0_QUANT + C0_QTDPED)

	// Adiciona legenda.
	nLeg     := aScan(aLegendas, {|aLeg| SC5->(&(aLeg[4]))})
	oLegenda := LoadBitmap(nil, "BR_VERDE")
	aAdd(aItem, oLegenda)

	// Acerta os campos do pedido.
	aEval(aCampos, {|cCpo| aAdd(aItem, CriaVar(cCpo, .F.))}, 2)
	aItem[nPosPed] := SC0->C0_DOCRES
	aItem[nPosQtd] := Transform(SC0->(C0_QUANT + C0_QTDPED), PesqPict("SC6", "C6_QTDVEN"))
	aItem[nPosObs] := RTrim(SC0->C0_OBS)

	// Próximo item.
	(cAliasTop)->(dbSkip())
EndDo
(cAliasTop)->(dbCloseArea())

// Se as matrizes estiverem vazias, cria uma linha em branco.
If empty(aItensPed)
	aAdd(aItensPed, {LoadBitmap(nil, "")})
	aEval(aCposPed, {|cCpo| aAdd(aTail(aItensPed), CriaVar(cCpo, .F.))}, 2)
Endif
If empty(aItensTrn)
	aAdd(aItensTrn, {LoadBitmap(nil, "")})
	aEval(aCposTrn, {|cCpo| aAdd(aTail(aItensTrn), CriaVar(cCpo, .F.))}, 2)
Endif

// Monta tela de entrada.
DEFINE MSDIALOG oDlgEst TITLE "Pedidos de venda - produto " + cProduto FROM 0, 0 TO 500, 900 PIXEL Style DS_MODALFRAME

// Pedidos de venda.
oGrPed := TFolder():New(0, 0, {"Pedidos pendentes"}, {}, oDlgEst,,,, .T., .F., 225, 0)
oGrPed:Align := CONTROL_ALIGN_LEFT
oGrPed := oGrPed:aDialogs[1]

// Grid com os itens de pedido.
aCabec := {""}; aEval(aCposPed, {|cCpo| aAdd(aCabec, Posicione("SX3", 2, cCpo, "RTrim(X3Titulo())"))}, 2)
oGdPed := TWBrowse():New(0, 0, 0, 0,, aCabec, aTamCpoPed, oGrPed)
oGdPed:Align := CONTROL_ALIGN_ALLCLIENT
oGdPed:SetArray(aItensPed)
oGdPed:bLine := {|| aItensPed[oGdPed:nAt]}
oGdPed:nAt   := 1

// Pedidos de transferência.
oGrTrn := TFolder():New(0, 0, {"Pedidos de transferência"}, {}, oDlgEst,,,, .T., .F., 0, 0)
oGrTrn:Align := CONTROL_ALIGN_ALLCLIENT
oGrTrn := oGrTrn:aDialogs[1]

// Grid com os itens de pedido.
aCabec := {""}; aEval(aCposTrn, {|cCpo| aAdd(aCabec, Posicione("SX3", 2, cCpo, "RTrim(X3Titulo())"))}, 2)
oGdTrn := TWBrowse():New(0, 0, 0, 0,, aCabec, aTamCpoTrn, oGrTrn)
oGdTrn:Align := CONTROL_ALIGN_ALLCLIENT
oGdTrn:SetArray(aItensTrn)
oGdTrn:bLine := {|| aItensTrn[oGdTrn:nAt]}
oGdTrn:nAt   := 1

// Rodapé da tela.
oGrpRod := TScrollBox():New(oDlgEst, 0, 0, 17, 0, .F., .F., .F.)
oGrpRod:Align := CONTROL_ALIGN_BOTTOM

// Totais.
nLin := 2; nCol := 2
@ nLin, nCol SAY oSayPed VAR "Pedidos "       + AllTrim(Transform(aTotal[1], cPictQtd))             of oGrpRod PIXEL
nCol += 80
@ nLin, nCol SAY oSayTrn VAR "Transferência " + AllTrim(Transform(aTotal[2], cPictQtd))             of oGrpRod PIXEL
nCol += 80
@ nLin, nCol SAY oSayTot VAR "Total "         + AllTrim(Transform(aTotal[1] + aTotal[2], cPictQtd)) of oGrpRod PIXEL
nCol += 80

// Botões.
tButton():New(nLin + 2, 375, "Legenda", oGrpRod, {|| U_MA061Leg(.T.)}, 33, 08,,,, .T.)
tButton():New(nLin,     413, "Sair",    oGrpRod, {|| oDlgEst:End()},   35, 10,,,, .T.)

ACTIVATE MSDIALOG oDlgEst CENTERED

RestArea(aArea)

Return


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Ciro Pedreira
// Modulo   : Faturamento / Call Center
// Função   : VLDESM01
// Descrição: Valida o campo C6_QTDVEN na tela de pedido de venda.
//          : Inserir esta função na validação de usuário do campo C6_QTDVEN.
// Retorno  : Lógico, informando se permite ou não a alteração da quantidade.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 15/05/17 | Ciro Pedreira     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function VLDESM01()
Local lRet     := .T.
Local aAreaMem := GetArea()

// Somente informativo, não deve travar a continuidade do processo.
VAL_Q_DESM(GdFieldGet("C6_PRODUTO"), M->C6_QTDVEN)

RestArea(aAreaMem)

Return lRet


// ##############################################################################
// Projeto  : PROT-CAP
// Autor    : João Leão
// Modulo   : Compras
// Função   : TabPedVen
// Descrição: Consulta os pedidos de venda em aberto com o mesmo produto e monta.
//            uma tabela HTML.
// Retorno  : Texto
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 19/10/15 | João Leão         | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function TabPedVen(cProduto, aPedidos)
Local cQuery    := ""
Local cAliasPV  := ""
Local nQtdePV   := 0
Local cMsg      := ""

// Inicializa variável de pedidos de venda afetados.
Default aPedidos := {}

cQuery := "select SC6.C6_NUM, SC6.C6_ITEM, SC6.C6_QTDVEN - SC6.C6_QTDENT QTDPEN, SC5.C5_XDESMEM, SC5.C5_CLIENTE, SC5.C5_LOJACLI, SA1.A1_NOME, SU7.U7_COD, SU7.U7_NOME "

cQuery += "from " + RetSqlName("SC6") + " SC6 "

cQuery += "inner join " + RetSqlName("SC5") + " SC5 on SC5.D_E_L_E_T_ = '' "
cQuery += "and SC5.C5_FILIAL  = '" + xFilial("SC5") + "' "
cQuery += "and SC5.C5_NUM     = SC6.C6_NUM "

cQuery += "inner join " + RetSqlName("SA1") + " SA1 on SA1.D_E_L_E_T_ = '' "
cQuery += "and SA1.A1_FILIAL  = '" + xFilial("SA1") + "' "
cQuery += "and SA1.A1_COD     = SC5.C5_CLIENTE "
cQuery += "and SA1.A1_LOJA    = SC5.C5_LOJACLI "

cQuery += "left  join " + RetSqlName("ZA7") + " ZA7 on ZA7.D_E_L_E_T_ = '' "
cQuery += "and ZA7.ZA7_FILIAL = '" + xFilial("ZA7") + "' "
cQuery += "and ZA7.ZA7_DEPVEN = SC5.C5_XDEPVEN "
cQuery += "and ZA7.ZA7_CLIENT = SC5.C5_CLIENTE "
cQuery += "and ZA7.ZA7_LOJA   = SC5.C5_LOJACLI "

cQuery += "left  join " + RetSqlName("SU7") + " SU7 on SU7.D_E_L_E_T_ = '' "
cQuery += "and SU7.U7_FILIAL  = '" + xFilial("SU7") + "' "
cQuery += "and SU7.U7_COD     = ZA7.ZA7_OPERAD "

cQuery += "where SC6.D_E_L_E_T_ = '' "
cQuery += "and SC6.C6_FILIAL  = '" + xFilial("SC6") + "' "
cQuery += "and SC6.C6_PRODUTO = '" + cProduto + "' "
cQuery += "and SC6.C6_QTDVEN  > SC6.C6_QTDENT "
cQuery += "and SC6.C6_BLQ     NOT IN ('R', 'S') "
cQuery += "and SC5.C5_TIPO    NOT IN ('B', 'D') "

cQuery += "order by SC6.C6_NUM, SC6.C6_ITEM "

cAliasPV := MPSysOpenQuery(cQuery)

If (cAliasPV)->(!EOF())
	cMsg := '<table border="1" width="100%" id="table1">'
	cMsg += ' <tr>'
	cMsg += '  <th>Pedido</th>'
	cMsg += '  <th>Qtde</th>'
	cMsg += '  <th>Desmembra</th>'
	cMsg += '  <th>Cliente</th>'
	cMsg += '  <th>Atendente</th>'
	cMsg += ' </tr>'

	Do While (cAliasPV)->(!eof())
		aAdd(aPedidos, {(cAliasPV)->C6_NUM, (cAliasPV)->C6_ITEM})
		cMsg += ' <tr>'
		cMsg += '  <td>' + (cAliasPV)->(C6_NUM + "/" + C6_ITEM) + '</td>'
		cMsg += '  <td style="text-align:right">' + Transform((cAliasPV)->QTDPEN, PesqPict("SC6", "C6_QTDVEN")) + '</td>'
		cMsg += '  <td style="text-align:center">' + X3Combo("C5_XDESMEM", (cAliasPV)->C5_XDESMEM) + '</td>'
		cMsg += '  <td>' + (cAliasPV)->(C5_CLIENTE +  "/" + C5_LOJACLI +  " - " + U_Txt2HTML(RTrim(A1_NOME))) + '</td>'
		cMsg += '  <td>' + (cAliasPV)->(If(empty(U7_COD), "", U7_COD +  " - " + U_Txt2HTML(RTrim(U7_NOME)))) + '</td>'
		cMsg += ' </tr>'
		nQtdePV += (cAliasPV)->QTDPEN
		(cAliasPV)->(dbSkip())
	EndDo
	cMsg += '</table>'
EndIf
(cAliasPV)->(dbCloseArea())

Return cMsg


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Joao Leao
// Modulo   : Faturamento / Call Center
// Função   : DepVen
// Descrição: Preenche o array com os departamentos de venda das carteiras do cliente.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 24/10/17 | João Leão         | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function DepVen(cCliente, cLoja, nTipo)
Local aRet       := {}
Local cFilTodas  := ""
Local cQuery     := ""
Local cAliasDV   := ""
Local cDeparts   := superGetMV("PC_DEPARTS",,"'01','02'")

U_Filiais(@cFilTodas)

cQuery := "SELECT DISTINCT ZA7_DEPVEN, ZA7_OPERAD, ZA7_CART, U7_NOME, ZA7_FILIAL, " + CRLF
cQuery += "ZA6.R_E_C_N_O_ ZA6RecNo, ZA7.R_E_C_N_O_ ZA7RecNo " + CRLF

cQuery += "FROM " + RetSQLName("ZA7") + " ZA7 (NOLOCK) " + CRLF

cQuery += "INNER JOIN " + RetSQLName("ZA6") + " ZA6 (NOLOCK) " + CRLF
cQuery += "ON ZA6.D_E_L_E_T_ = '' " + CRLF
cQuery += "AND ZA6_FILIAL = ZA7_FILIAL " + CRLF
cQuery += "AND ZA6_COD    = ZA7_CART " + CRLF
cQuery += "AND ZA6_OPERAD = ZA7_OPERAD " + CRLF

cQuery += "INNER JOIN " + RetSQLName("SU7") + " U7 (NOLOCK) " + CRLF
cQuery += "ON U7.D_E_L_E_T_ = '' " + CRLF
cQuery += "AND U7_FILIAL  = '" + xFilial("SU7") + "' " + CRLF
cQuery += "AND U7_COD     = ZA7_OPERAD " + CRLF

cQuery += "WHERE ZA7.D_E_L_E_T_ = '' " + CRLF
cQuery += "AND ZA7_FILIAL in (" + cFilTodas + ") " + CRLF
cQuery += "AND ZA7_CLIENT = '" + cCliente + "' " + CRLF
cQuery += "AND ZA7_LOJA   = '" + cLoja + "' " + CRLF
cQuery += "AND ZA7_CART   <> '' " + CRLF
cQuery += "AND ZA7_DEPVEN in (" + cDeparts + ") " + CRLF
cQuery += "AND ZA7_OPERAD <> '' " + CRLF
cQuery += "ORDER BY 1 "
cAliasDV := MPSysOpenQuery(cQuery)

If nTipo = 1
	// Alimenta o array auxiliar somente com os departamentos.
	aAdd(aRet, "")
	While (cAliasDV)->(!eof())
		aAdd(aRet, (cAliasDV)->ZA7_DEPVEN + ' - ' + AllTrim((cAliasDV)->U7_NOME))
		(cAliasDV)->(dbSkip())
	EndDo
Else
	If (cAliasDV)->(!eof())
		aRet := {(cAliasDV)->ZA6RecNo, (cAliasDV)->ZA7RecNo}
	Else
		aRet := {0, 0}
	Endif
Endif
(cAliasDV)->(dbCloseArea())

Return aRet

// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Modulo   : Faturamento / Call Center
// Função   : RetPrzCalc
// Descrição: Rotina para retornar o prazo de entrega de acordo com o cadastro
// Descrição: de leadtime por estado, municipio e transportadora (tabela ZAG).
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 04/07/18 | Wilson A. Silva Jr| Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
Static Function RetPrzCalc(cUF, cCodMun, cTransp)
Local aArea 	:= GetArea()
Local cTMP1 	:= ""
Local cQuery 	:= ""
Local nPrazo 	:= 0

DEFAULT cUF 	:= M->UA_ESTE
DEFAULT cCodMun := M->UA_XCDMUNE
DEFAULT cTransp := M->UA_TRANSP

cQuery := " SELECT "+ CRLF
cQuery += " 	ZAG.ZAG_PRAZO "+ CRLF
cQuery += " FROM "+RetSqlName("ZAG")+" ZAG "+ CRLF
cQuery += " WHERE "+ CRLF
cQuery += " 	ZAG.ZAG_FILIAL = '"+xFilial("ZAG")+"' "+ CRLF
cQuery += " 	AND ZAG.ZAG_EST = '"+cUF+"' "+ CRLF
cQuery += " 	AND ZAG.ZAG_CODMUN = '"+cCodMun+"' "+ CRLF
cQuery += " 	AND ZAG.ZAG_TRANSP = '"+cTransp+"' "+ CRLF
cQuery += " 	AND ZAG.D_E_L_E_T_ = ' ' "+ CRLF

cTMP1 := MPSysOpenQuery(cQuery)

If (cTMP1)->(!EOF())
	nPrazo := (cTMP1)->ZAG_PRAZO
EndIf

(cTMP1)->(DbCloseArea())

RestArea(aArea)

Return nPrazo

//-------------------------------------------------------------------
/*/{Protheus.doc} MinFrete
Retorna o Valor Minimo para Pedidos com Frete CIF

@author  Guilherme Santos
@since   18/10/2018
@version 12.1.17
/*/
//-------------------------------------------------------------------
User Function MinFrete(cEstado, cCodMun)
	Local aArea		:= GetArea()
	Local aAreaZAG	:= ZAG->(GetArea())
	Local nRetorno := 0

	DbSelectArea("ZAG")
	DbSetOrder(1)		//ZAG_FILIAL, ZAG_EST, ZAG_CODMUN, ZAG_TRANSP

	If ZAG->(DbSeek(xFilial("ZAG") + cEstado + cCodMun))
		While !ZAG->(Eof()) .AND. ZAG->ZAG_FILIAL + ZAG->ZAG_EST + ZAG->ZAG_CODMUN == xFilial("ZAG") + cEstado + cCodMun
			If ZAG->ZAG_ATIVO == "S" .AND. ZAG->ZAG_PEDMIN > 0
				nRetorno := ZAG->ZAG_PEDMIN
				Exit
			EndIf

			ZAG->(DbSkip())
		End
	EndIf

	RestArea(aAreaZAG)
	RestArea(aArea)

Return nRetorno

// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Modulo   : Faturamento / Call Center
// Função   : OrdSB1
// Descrição: Ordena resultado de pesquisa de produto pela coluna clicada.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 09/10/18 | Wilson A. Silva Jr| Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
Static Function OrdSB1(oGdPrd, nCol)
Local aResultFil := oGdPrd:Cargo[1]

If nUltCol == nCol
	lCrescente := !lCrescente
Else
	lCrescente := .T.
EndIf
nUltCol := nCol

If lCrescente
	ASort(aResultFil,,, { |x, y| x[1][nCol] < y[1][nCol] } ) // Crescente
Else
	ASort(aResultFil,,, { |x, y| x[1][nCol] > y[1][nCol] } ) // Decrescente
EndIf

oGdPrd:Cargo[1] := aResultFil
oGdPrd:SetArray(aResultFil)
oGdPrd:bLine := {|| aResultFil[oGdPrd:nAt, 1]}
oGdPrd:nAt   := 1
oGdPrd:Refresh()

// Atualiza grid de estoque.
Eval(oGdPrd:bChange)

Return .T.
// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Wilson A. Silva Jr
// Modulo   : Faturamento / Call Center
// Função   : ValidCA
// Descrição: Rotina para emitir alerta de vencimento do CA quando digitado o produto
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 11/09/18 | Wilson A. Silva Jr| Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function ValidCA(cProduto, cNumCA, lAuto, cMsgErro)
	Local aAreaAtu 		:= GetArea()
	Local aAreaZB6 		:= ZB6->(GetArea())
	Local nAlertCA 		:= If(IsInCallStack("U_BIAPRO01"), 0, SuperGetMV("PC_VLDCAPC",,60))
	Local nDiasVenc		:= 0
	Local lRetorno 		:= .T.
	Local cTpMov		:= If(IsInCallStack("U_BIAPRO01"), "transferido", "comprado")

	Default cNumCA		:= Posicione("SB1", 1, xFilial("SB1") + cProduto, "B1_CA")
	Default lAuto		:= .F.
	Default cMsgErro	:= ""

	DbSelectArea("ZB6")
	DbSetOrder(1) // ZB6_FILIAL+ZB6_COD
	If ZB6->(DbSeek(xFilial("ZB6") + cNumCA))

		nDiasVenc := ZB6->ZB6_VENCTO - Date()

		If nDiasVenc <= nAlertCA
			lRetorno := .F.
			cMsgErro += "Atenção! Produto não pode ser " + cTpMov + "." + CRLF
			If nDiasVenc < 0
				cMsgErro += "CA (" + AllTrim(cNumCA) + ") vencido a " + cValToChar(nDiasVenc * -1) + " dias, produto (" + AllTrim(cProduto) + ")." + CRLF
			Else
				cMsgErro += "Restam " + cValToChar(nDiasVenc) + " dias para o vencimento do CA (" + AllTrim(cNumCA) + "), produto (" + AllTrim(cProduto) + ")." + CRLF
			EndIf
			cMsgErro += "Data de Vencimento: " + DToC(ZB6->ZB6_VENCTO) + CRLF

			If ZB6->ZB6_PROTOC == "S" .And. ZB6->ZB6_VENCTO > Date()
				lRetorno := .T.
			ElseIf !lAuto
				If !lRetorno
					Help(" ", 1, "Help", "ValidCA", cMsgErro, 3, 0)
				EndIf
			EndIf
		EndIf
	EndIf

	RestArea(aAreaZB6)
	RestArea(aAreaAtu)

Return lRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} VldCAPed
Valida se o Pedido possui itens com CA vencido antes de enviar ao WMS

@author  Guilherme Santos
@since   16/10/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
User Function VldCAPed(cFilPed, cNumPed, cMsgCA)
	Local lRetorno 	:= .T.
	Local cQuery 	:= ""
	Local cTabQry	:= ""

	cQuery += "SELECT	SC6.C6_PRODUTO" + CRLF
	cQuery += ",		SB1.B1_CA" + CRLF
	cQuery += ",		ZB6.ZB6_VENCTO" + CRLF

	cQuery += "FROM		" + RetSqlName("SC6") + " SC6" + CRLF

	cQuery += "			INNER JOIN" + CRLF
	cQuery += "			" + RetSqlName("SB1") + " SB1" + CRLF

	cQuery += "			ON SB1.B1_FILIAL = '" + xFilial("SB1", cFilPed) + "'" + CRLF
	cQuery += "			AND SB1.B1_COD = SC6.C6_PRODUTO" + CRLF
	cQuery += "			AND SB1.D_E_L_E_T_ = ''" + CRLF

	cQuery += "			INNER JOIN" + CRLF
	cQuery += "			" + RetSqlName("ZB6") + " ZB6" + CRLF

	cQuery += "			ON	ZB6.ZB6_FILIAL = '" + xFilial("ZB6", cFilPed) + "'" + CRLF
	cQuery += "			AND ZB6.ZB6_COD = SB1.B1_CA" + CRLF
	cQuery += "			AND ZB6.ZB6_VENCTO <= '" + DtoS(dDatabase) + "'" + CRLF
	cQuery += "			AND ZB6.D_E_L_E_T_ = ''" + CRLF

	cQuery += "WHERE	SC6.C6_FILIAL = '" + xFilial("SC6", cFilPed) + "'" + CRLF
	cQuery += "AND		SC6.C6_NUM = '" + cNumPed + "'" + CRLF
	cQuery += "AND		SC6.D_E_L_E_T_ = ''" + CRLF

	cTabQry := MPSysOpenQuery(cQuery)

	If !(cTabQry)->(Eof())
		lRetorno := .F.

		While !(cTabQry)->(Eof())
			cMsgCA += "Produto " + AllTrim((cTabQry)->C6_PRODUTO) + " com CA (" + AllTrim((cTabQry)->B1_CA) + ") vencido em " + DtoC(StoD((cTabQry)->ZB6_VENCTO)) + CRLF

			(cTabQry)->(DbSkip())
		End
	EndIf

	If Select(cTabQry) > 0
		(cTabQry)->(DbCloseArea())
	EndIf

Return lRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} fVCAIncP
Validacao do CA na Inclusão do Pedido de Venda
Disparada a partir do X3_VLDUSER do Campo C6_PRODUTO

@author  Guilherme Santos
@since   06/04/2021
@version 12.1.25
/*/
//-------------------------------------------------------------------
User Function fVCAIncP()
	Local cProduto	:= &(ReadVar())								//C6_PRODUTO
	Local cMensagem	:= ""
	Local lBlqCA	:= SuperGetMV("PC_BLTGVCA", NIL, .T.)		//Habilita o bloqueio da venda de produtos com CA vencidos ou a vencer
	Local lRetorno	:= .T.
	Local nDiasCA	:= SuperGetMV("PC_TGVDBCA", NIL, 5)			//Dias ate o vencimento do CA para bloqueio da venda
	Local nAlertCA 	:= SuperGetMV("PC_ALERTCA", NIL, 20)		//Dias para emitir o aviso de CA proximo ao vencimento
	Local nDiasVenc	:= 0
	Local lExecAuto	:= Type("l410Auto") <> "U" .AND. l410Auto

	//Permite a inclusao para Devolucao, Remessa para Conserto e Transferencias
	If lBlqCA .AND. M->C5_TIPO == "N" .AND. M->C5_CLIENTE <> "999999"
		DbSelectArea("SB1")
		DbSetOrder(1)		//B1_FILIAL, B1_COD

		If SB1->(DbSeek(xFilial("SB1") + cProduto))
			If !Empty(SB1->B1_CA)
				DbSelectArea("ZB6")
				DbSetOrder(1)		//ZB6_FILIAL, ZB6_COD

				If ZB6->(DbSeek(xFilial("ZB6") + SB1->B1_CA))
					nDiasVenc := ZB6->ZB6_VENCTO - Date()
					If ZB6->ZB6_VENCTO - nDiasCA <= dDataBase .OR. ZB6->ZB6_STATUS == "V"
						cMensagem += "Atenção! Produto com venda suspensa, não pode ser comercializado, devido ao certificado de aprovação vencido." + CRLF
						cMensagem += "Produto: " + SB1->B1_COD + CRLF
						cMensagem += "CA: " + AllTrim(SB1->B1_CA) + CRLF
						cMensagem += "Vencimento: " + DtoC(ZB6->ZB6_VENCTO) + CRLF
						lRetorno := .F.
					ElseIf nDiasVenc <= nAlertCA
 						If !lExecAuto
							cMensagem += "Atenção! Restam " + cValToChar(nDiasVenc) + " dias para o vencimento do CA (" + AllTrim(SB1->B1_CA) + "), produto (" + AllTrim(cProduto) + "). Data de Vencimento: " + DToC(ZB6->ZB6_VENCTO)
						EndIf
					EndIf

					If ZB6->ZB6_PROTOC == "S" .And. ZB6->ZB6_VENCTO > Date()
						lRetorno := .T.
					Else
						If !lRetorno
							Help(" ", 1, "Help", "fVCAIncP", cMensagem, 3, 0)
						EndIf
					EndIf
				EndIf
			EndIf
		EndIf
	EndIf

Return lRetorno
// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Wilson A. Silva Jr
// Função   : SumDtWork
// Descrição: Função para somar dias uteis a uma data.
// Retorno  : Retorna a data mais os dias uteis.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 19/04/18 | Wilson A. Silva Jr| Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function SumDtWork(dData, nDias)
While nDias > 0
	dData++
	If U_DayWork(dData)
		nDias--
	EndIf
EndDo

Return dData

// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Wilson A. Silva Jr
// Função   : DayWork
// Descrição: Função para verificar se uma data é dia utel.
// Retorno  : Retorna verdadeiro caso seja dia util.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 19/04/18 | Wilson A. Silva Jr| Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function DayWork(dData)
Return DataValida(dData,.T.) == dData

// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Wilson A. Silva Jr
// Função   : DateDiff
// Descrição: Função para verificar a diferença em dias uteis entre duas datas.
// Retorno  : Retorna verdadeiro caso seja dia util.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 19/04/18 | Wilson A. Silva Jr| Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function DateDiff(dData1, dData2)
Local nDias := 0
Local dAux

If Empty(dData1) .Or. Empty(dData2)
	Return nDias
EndIf

If dData1 <= dData2
	dAux := (dData1 + 1)
	While dAux <= dData2
		If U_DayWork(dAux)
			nDias++
		EndIf
		dAux++
	EndDo
Else
	dAux := (dData2 + 1)
	While dAux <= dData1
		If U_DayWork(dAux)
			nDias--
		EndIf
		dAux++
	EndDo
EndIf

Return nDias

// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Wilson A. Silva Jr
// Modulo   : Estoque
// Função   : ConnWIS
// Descrição: Rotina para conexão com o banco de dados Oracle do WMS WIS.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 17/05/19 | Wilson A. Silva Jr| Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function ConnWIS(lRet)
	Local cDBOra     := GetNewPar("PC_WISBDAL",If(U_Producao(), "ORACLE/ORACLE-WIS", "ORACLE/ORACLE-WIS")) // Alias
	Local cSrvOra    := GetNewPar("PC_WISDBIP",If(U_Producao(), "192.168.2.104", "192.168.2.26")) // IP
	Local nPorta     := GetNewPar("PC_WISDBPO",If(U_Producao(), 7892, 7892)) // Porta
	Local cProtect   := ""
    Local nSTHndConn := 0
    Local nHandleERP := AdvConnection() //-- Conexao atual
    Local cIniFile   := GetAdv97()

	Default lRet := .T.

    lRet := .T.

	If !empty(cDBOra) .and. !empty(cSrvOra)
		If nSTHndConn <= 0 .or. !TCIsConnected(nSTHndConn)
			If Empty(cDBOra) .Or. Empty(cSrvOra) .Or. Empty(nPorta)
				Aviso("Atenção", "Um ou mais dos parametros 'PC_WISBDAL', 'PC_WISDBIP', 'PC_WISDBPO' está(ão) vazio(s). Favor verificar", {"Ok"}, 3)
				lRet	:= .F.
			Endif
			If lRet
				cProtect := GetPvProfString("DbAccess", "ProtheusOnly", "0", cIniFile)
				cProtect := GetSrvProfString("TopProtheusOnly", cProtect)
				cProtect := If(cProtect == "1", "@@__@@", "")  // Assinatura para o TOP.

				nSTHndConn := TcLink(cProtect +  "@!!@" + cDbOra, cSrvOra, nPorta)
				TcSetConn(nHandleERP)
			Endif
		Endif
	Endif

Return nSTHndConn
//-------------------------------------------------------------------
/*/{Protheus.doc} WhenD1
Verifica se os campos D1_COD e D1_QUANT da NF de Entrada podem ser alterados

@author  Guilherme Santos
@since   25/07/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
User Function WhenD1()
	Local lRetorno := .T.
	Local lWMSSythex := GetMV("BZ_WMS",,"") == "WIS"

	If lWMSSythex .AND. IsInCallStack("MATA103") .AND. ALTERA .AND. SF1->F1_XWIS == "1"
		lRetorno := .F.
	EndIf

Return lRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} BzGetFil
Retorna o Codigo da Unidade de Negocio do CNPJ informado

@author  Guilherme Santos
@since   05/11/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
User Function BzGetFil(cCnpj)
	Local aFiliais 	:= FWLoadSM0()
	Local cRetorno 	:= ""
	Local nI		:= 0

	For nI := 1 to Len(aFiliais)
		If aFiliais[nI][18] == cCnpj
			cRetorno := aFiliais[nI][02]
			Exit
		EndIf
	Next nI

Return cRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} XPMPgto
Retorna o prazo médio de pagamento
@author  João Leão
@since   13/12/2019
@version 12.1.25
/*/
//-------------------------------------------------------------------
User Function XPMPgto(nValTot, cCondPagto, dDataEmi)

Local nRet			:= 0
Local aCondicao		:= {}
Local nX			:= 0
Local nSomaParc		:= 0
Local nParXDias		:= 0
Local aArea			:= GetArea()

Default nValTot		:= 0
Default cCondPagto	:= ""
Default dDataEmi	:= CToD("  /  /    ")

If nValTot > 0 .And. !Empty(cCondPagto) .And. !Empty(dDataEmi) .And. !U_Ma060VCP(cCondPagto) //Não executa para condpagto à vista, antecipado ou de cartão de crédito
	aCondicao := Condicao(nValTot, cCondPagto, , dDataEmi)
	For nX := 1 To Len(aCondicao)
		nSomaParc += aCondicao[nX][2]
		nParXDias += aCondicao[nX][2] * DateDiffDay(dDataEmi, aCondicao[nX][1])
	Next nX

	nRet := nParXDias / nSomaParc
EndIf

RestArea(aArea)

Return Round(nRet,0)

//-------------------------------------------------------------------
/*/{Protheus.doc} XB5BlqEsp
Retorna array com informações de bloqueio especial do produto, campo
B5_XBLQESP.
[1] - Indica se o produto tem bloqueio
[2] - Tipo do bloqueio
[3] - Descrição do bloqueio
[4] - Cor de fundo
[5] - Cor da fonte
@author  João Leão
@since   05/02/2020
@version 12.1.25
/*/
//-------------------------------------------------------------------
User Function XB5BlqEsp(cProduto)
Local aRet		:= {.F., "", "", CLR_WHITE, CLR_BLACK}

//Verifica se o Produto tem bloqueio especial
SB5->(DBSetOrder(1)) //B5_FILIAL, B5_COD
If SB5->(DBSeek(xFilial("SB5") + cProduto))
	If !Empty(SB5->B5_XBLQESP)
		aRet[1] := .T.
		aRet[2] := SB5->B5_XBLQESP
		aRet[3] := X3Combo("B5_XBLQESP", SB5->B5_XBLQESP)
	EndIf

	If SB5->B5_XBLQESP == "1" //Epidemia
		aRet[4] := RGB(123,104,238) //MediumSlateBlue (Roxo claro)
		aRet[5] := CLR_BLACK
	EndIf
EndIf

Return aRet
//-------------------------------------------------------------------
/*/{Protheus.doc} CorTam
Retorna a Cor e o Tamanho do SKU

@author  Guilherme Santos
@since   06/02/2020
@version 12.1.25
/*/
//-------------------------------------------------------------------
User Function CorTam(cProduto)
	Local aArea		:= GetArea()
	Local cRetorno	:= ""
	Local cTabQry	:= ""
	Local cQuery	:= ""

	cQuery += "SELECT	RTRIM(ISNULL(SBV1.BV_DESCRI, '')) + '/' + RTRIM(ISNULL(SBV2.BV_DESCRI, '')) TAM_COR" + CRLF

	cQuery += "FROM		" + RetSqlName("SB1") + " SB1" + CRLF

	cQuery += "			INNER JOIN" + CRLF
	cQuery += "			" + RetSqlName("SB4") + " SB4" + CRLF
	cQuery += "			ON SB4.B4_FILIAL = '" + xFilial("SB4") + "'" + CRLF
	cQuery += "			AND SB4.B4_COD = SUBSTRING(SB1.B1_COD, 1, 10)" + CRLF
	cQuery += "			AND SB4.D_E_L_E_T_ = ''" + CRLF

	cQuery += "			LEFT JOIN" + CRLF
	cQuery += "			" + RetSqlName("SBV") + " SBV1" + CRLF
	cQuery += "			ON SBV1.BV_FILIAL = '" + xFilial("SBV") + "'" + CRLF
	cQuery += "			AND SBV1.BV_TABELA = SB4.B4_LINHA" + CRLF
	cQuery += "			AND SBV1.BV_CHAVE = SUBSTRING(SB1.B1_COD, 11, 3)" + CRLF
	cQuery += "			AND SBV1.D_E_L_E_T_ = ''" + CRLF

	cQuery += "			LEFT JOIN" + CRLF
	cQuery += "			" + RetSqlName("SBV") + " SBV2" + CRLF
	cQuery += "			ON SBV2.BV_FILIAL = '" + xFilial("SBV") + "'" + CRLF
	cQuery += "			AND SBV2.BV_TABELA = SB4.B4_COLUNA" + CRLF
	cQuery += "			AND SBV2.BV_CHAVE = SUBSTRING(SB1.B1_COD, 14, 2)" + CRLF
	cQuery += "			AND SBV2.D_E_L_E_T_ = ''" + CRLF

	cQuery += "WHERE	SB1.B1_FILIAL = '" + xFilial("SB1") + "'" + CRLF
	cQuery += "AND		SB1.B1_COD = '" + cProduto + "'" + CRLF
	cQuery += "AND		SB1.D_E_L_E_T_ = ''" + CRLF

	cTabQry := MPSysOpenQuery(cQuery)

	While !(cTabQry)->(Eof())

		cRetorno := (cTabQry)->TAM_COR

		(cTabQry)->(DbSkip())
	End

	If Select(cTabQry) > 0
		(cTabQry)->(DbCloseArea())
	EndIf

	RestArea(aArea)
Return cRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} PrevEntr
Retorna os prazos de entrega do Produto / Orcamento / Pedido

@author  	Guilherme Santos
@since   	12/05/2020
@version 	12.1.25
@nOpcao 	1=Orcamento, 2=Pedido
@cParam1	Produto / Pedido
/*/
//-------------------------------------------------------------------
User Function PrevEntr(nOpcao, lItem, cProduto, nQtdVen, aPrevEntr, aLstPrv, aHeader, aCols, cPedido, cTransp, lAtuOrig)
	Local lCalcPFat  	:= SuperGetMv("PC_TGVPFAT",, .T.)
	Local nDiasFat   	:= SuperGetMv("PC_TGVDFAT",, 0)
	Local nSaldoSB2		:= 0
	Local nSaldoEnt  	:= 0
	Local nX			:= 0
	Local dEntrItem		:= CtoD("")
	Local dRet			:= CtoD("")
	Local nDiasProc		:= SuperGetMV("PC_TMPPROC", NIL, 5)
	Local lSldFil		:= .F.
	Local nSldDisp		:= 0
	Local nDiasRetira	:= SuperGetMV("PC_DIASRET", NIL, 10)
	Local lRecalculo	:= IsInCallStack("U_PROMA770")
	Local lPrevTransp	:= SuperGetMV("BZ_PRZENTR", NIL, .T.) // calcula e grava os campos de prazo de entrega de transporte

	Default nOpcao		:= 1
	Default lItem		:= .T.
	Default cProduto	:= ""
	Default aPrevEntr	:= {}
	Default aLstPrv		:= {}
	Default cPedido		:= {}
	Default lAtuOrig	:= .t.
	If lCalcPFat
		If lItem
			If Empty(aPrevEntr) .OR. Empty(aLstPrv)
				U_ListaEnt(cProduto, @aPrevEntr, @aLstPrv)
			EndIf

			DbSelectArea("SB2")
			DbSetOrder(1)		//B2_FILIAL, B2_COD, B2_LOCAL

			If SB2->(DbSeek(xFilial("SB2") + cProduto + ERP_LOC_VENDAS))
				// Verifica se não tem saldo em estoque suficiente.
				// Se a rotina foi chamada a partir do calculo do Pedido de Venda,
				// abate a quantidade do proprio pedido da conta do estoque
				nSaldoSB2 := SB2->(B2_QATU - B2_RESERVA - B2_QPEDVEN + If(nOpcao == 2, nQtdVen, 0))
			EndIf

			If nSaldoSB2 < nQtdVen
				//Verificar se o segmento do produto é encomenda
				//Se for, verificar os saldos das unidades 00 e 02 para transferencia
				If U_GetSegto(cFilAnt, cProduto) == "E" .AND. cFilAnt <> MATRIZ .AND. cFilAnt <> "02"

					lSldFil := U_VerSld(cFilAnt, cProduto, nQtdVen, @dRet, @nSldDisp)

					//Se tem saldo para atender o pedido retorna a data calculada na Funcao VerSld na 00 e 02
					//Se não tem saldo, abate o saldo negativo (Saldo Atual - Pedidos de Venda) dos Pedidos de Compra
					If !lSldFil
						nSaldoEnt := nQtdVen - nSldDisp - nSaldoSB2

						// Verifica qual pedido de compra vai atender.
						aSort(aPrevEntr,,, {|x, y| x[XDTPREV] < y[XDTPREV]})  // Ordena por data.
						For nX := 1 to len(aPrevEntr)
							//Se não for: LeadTime, Pedido de Compra de CDs ou Pedido de Compra atrasado utiliza para calcular a previsão
							If !aPrevEntr[nX][XLEADTM] .AND. !aPrevEntr[nX][XFILCDS] .AND. !aPrevEntr[nX][XATRCOM]
								nSaldoEnt -= aPrevEntr[nX, XSALDO]
								If nSaldoEnt <= 0 .or. aPrevEntr[nX, XSALDO] > 0
									dRet := max(max(aPrevEntr[nX, XDTPREV], dDataBase), dRet)
									Exit
								Endif
							EndIf
						Next nX

						//Se os pedidos de compra incluidos não forem suficientes
						//para atender a demanda, retorna o LeadTime do Produto
						If nSaldoEnt > 0 .or. Empty(dRet)
							dRet := U_SumDate(date(), U_LeadTime(cFilAnt, cProduto))
						EndIf
					EndIf
				Else
					nSaldoEnt := nQtdVen - nSaldoSB2

					// Verifica qual pedido de compra vai atender.
					aSort(aPrevEntr,,, {|x, y| x[XDTPREV] < y[XDTPREV]})  // Ordena por data.
					For nX := 1 to len(aPrevEntr)
						//Se não for: LeadTime, Pedido de Compra de CDs ou Pedido de Compra atrasado utiliza para calcular a previsão
						If !aPrevEntr[nX][XLEADTM] .AND. !aPrevEntr[nX][XFILCDS] .AND. !aPrevEntr[nX][XATRCOM]
							nSaldoEnt -= aPrevEntr[nX, XSALDO]
							If nSaldoEnt <= 0 .or. aPrevEntr[nX, XSALDO] < 0
								dRet := max(max(aPrevEntr[nX, XDTPREV], dDataBase), dRet)
								Exit
							Endif
						EndIf
					Next nX

					//Se os pedidos de compra incluidos não forem suficientes
					//para atender a demanda, retorna o LeadTime do Produto
					If nSaldoEnt > 0
						dRet := U_SumDate(date(), U_LeadTime(cFilAnt, cProduto))
					EndIf
				EndIf
			Else
					//Saldo em Estoque - Database + Dias Processamento
					dRet := U_SumDate(dDataBase, nDiasProc)
			Endif
		Else
			Do Case
			Case nOpcao == 1		//Orcamento
				dRet := dDataBase
				For nX := 1 to len(aCols)
					// Verifica se a linha não está excluída.
					cProduto := GdFieldGet("UB_PRODUTO", nX,, aHeader, aCols)
					If !aTail(aCols[nX]) .and. !empty(cProduto)
						dEntrItem := GdFieldGet("UB_DTENTRE", nX,, aHeader, aCols)
						If empty(dEntrItem)
							dRet := dEntrItem
							Exit
						Else
							dRet := max(dRet, dEntrItem)
						Endif
					Endif
				Next nX

				If !Empty(dRet)
					//Se for cliente retira, adiciona 10 dias ao prazo
					DbSelectArea("SA4")
					SA4->(dbSetOrder(1))  // A4_FILIAL, A4_COD.
					If SA4->(DbSeek(xFilial("SA4") + cTransp)) .AND. 'RETIRA' $ SA4->A4_NOME
						dRet := U_SumDate(dRet, nDiasRetira)
					EndIf
				EndIf

				// Considera o prazo que a filial leva para faturar um pedido.
				If !Empty(dRet)
					dRet := U_SumDate(dRet, nDiasFat)
				Endif

			Case nOpcao == 2		//Pedido
				DbSelectArea("SC5")
				DbSetOrder(1)

				If SC5->(DbSeek(xFilial("SC5") + cPedido))
					DbSelectArea("SC6")
					DbSetOrder(1)		//C6_FILIAL, C6_NUM, C6_ITEM

					If SC6->(DbSeek(xFilial("SC6") + SC5->C5_NUM))
						dRet := dDataBase
						While !SC6->(Eof()) .AND. xFilial("SC6") + SC5->C5_NUM == SC6->C6_FILIAL + SC6->C6_NUM
							aPrevEntr	:= {}
							aLstPrv		:= {}
							dEntrItem	:= U_PrevEntr(2, .T., SC6->C6_PRODUTO, SC6->C6_QTDVEN - SC6->C6_QTDENT, @aPrevEntr, @aLstPrv, NIL, NIL, SC5->C5_NUM, SC5->C5_TRANSP)
							If Empty(dEntrItem)
								dRet := dEntrItem
								Exit
							Else
								RecLock("SC6", .F.)
									If lRecalculo
										SC6->C6_XFATBZL	:= dEntrItem
									Else
										SC6->C6_ENTREG	:= dEntrItem
									EndIf
								MsUnLock()

								dRet := max(dRet, dEntrItem)
							EndIf

							SC6->(DbSkip())
						End
					EndIf

					// Considera o prazo que a filial leva para faturar um pedido.
					If !Empty(dRet)
						dRet := U_SumDate(dRet, nDiasFat)
					Endif
					If !Empty(dRet)
						//Se for cliente retira, adiciona 10 dias ao prazo
						DbSelectArea("SA4")
						SA4->(dbSetOrder(1))  // A4_FILIAL, A4_COD.
						If SA4->(DbSeek(xFilial("SA4") + cTransp)) .AND. 'RETIRA' $ SA4->A4_NOME
							dRet := U_SumDate(dRet, nDiasRetira)
						EndIf
					EndIf

					If !Empty(dRet)
						RecLock("SC5", .F.)
							If lRecalculo
								if SC5->C5_XFATBZL <> dRet
									SC5->C5_XFATBZL := dRet
									if lPrevTransp
										SC5->C5_XDTENTI := U_PrevTransp(SC5->C5_FILIAL,SC5->C5_NUM, 1)
										SC5->C5_XDTENTF := U_PrevTransp(SC5->C5_FILIAL,SC5->C5_NUM, 2)
									endif
									U_HistPV(SC5->C5_NUM, "2", "Entrega reprogramada." + "Nova data de entrega: " + dtoc(dRet) )
								endif
							Else
								if !empty(SC5->C5_XDTPFAT) //se for uma alteração no pedido
									SC5->C5_XFATBZL := dRet
									if lPrevTransp
										SC5->C5_XDTENTF := U_PrevTransp(SC5->C5_FILIAL,SC5->C5_NUM, 2)
									endif
								endif
								if lAtuOrig
									SC5->C5_XDTPFAT := dRet
									if lPrevTransp
										SC5->C5_XDTENTI := U_PrevTransp(SC5->C5_FILIAL,SC5->C5_NUM, 1)
									endif
								endif
							EndIf
						MsUnlock()
					EndIf
				Endif
			EndCase
		EndIf
	EndIf

Return dRet
//-------------------------------------------------------------------
/*/{Protheus.doc} ListEnt
Retorna os Prazos de Entrega
@author  Guilherme Santos
@since   12/05/2020
@version 12.1.25
/*/
//-------------------------------------------------------------------
User Function ListaEnt(cProduto, aPrevEntr, aLstPrv,cNumPed,cNumItem)
	Local cPictQtd   	:= "@E 9,999,999"
	Local nLin			:= 0
	Local cFilComp 		:= StrTran(SuperGetMV("PC_TGVFILP",,"02"),",","','")		// Filiais que aparecem as compras estando logado em outra unidade
	Local cQuery		:= ""
	Local cAliasTop		:= ""
	Local cAliasTop2	:= ""
	Local cNomSts		:= ""
	Local dPrevEnt		:= CtoD("")
	Local dPrazoEnt		:= CtoD("")
	Local nPrazoEnt		:= 0
	Local nDiasProc		:= SuperGetMV("PC_TMPPROC", NIL, 5)
	Local nDiasAg		:= SuperGetMV("PC_DIASAG", NIL, 0)
	Local nSaldoImp		:= 0

	Default cProduto 	:= ""
	Default aPrevEntr 	:= {}
	Default aLstPrv 	:= {}

	Default cNumPed		:= ""
	Default cNumItem	:= ""

	If !Empty(cProduto)
		aPrevEntr	:= {}
		aLstPrv		:= {}

		// Inclui as pré-notas.
		cQuery := " SELECT "+ CRLF
		cQuery += " 	SD1.R_E_C_N_O_ SD1RecNo, "+ CRLF
		cQuery += " 	SC7.R_E_C_N_O_ SC7RecNo "+ CRLF
		cQuery += " FROM "+RetSQLName("SD1")+" SD1 (NOLOCK) "+ CRLF
		cQuery += " INNER JOIN "+RetSQLName("SF1")+" SF1 (NOLOCK) "+ CRLF
		cQuery += " 	ON SF1.F1_FILIAL = SD1.D1_FILIAL "+ CRLF
		cQuery += " 	AND SF1.F1_DOC = SD1.D1_DOC "+ CRLF
		cQuery += " 	AND SF1.F1_SERIE = SD1.D1_SERIE "+ CRLF
		cQuery += " 	AND SF1.F1_FORNECE = SD1.D1_FORNECE "+ CRLF
		cQuery += " 	AND SF1.F1_LOJA = SD1.D1_LOJA "+ CRLF
		cQuery += " 	AND SF1.F1_STATUS IN (' ','B') "+ CRLF
		cQuery += " 	AND SF1.D_E_L_E_T_ = ' ' "+ CRLF
		cQuery += " INNER JOIN "+RetSQLName("SC7")+" SC7 (NOLOCK) "+ CRLF
		cQuery += " 	ON SC7.C7_FILIAL = SD1.D1_FILIAL "+ CRLF
		cQuery += " 	AND SC7.C7_NUM = SD1.D1_PEDIDO "+ CRLF
		cQuery += " 	AND SC7.C7_ITEM = SD1.D1_ITEMPC "+ CRLF
		If !Empty(cNumPed)
			cQuery += " AND SC7.C7_NUM ='"+cNumPed+"' " 	+CRLF
			cQuery += " AND SC7.C7_ITEM='"+cNumItem+"' " 	+CRLF
		EndIf
		cQuery += " 	AND SC7.D_E_L_E_T_ = ' ' "+ CRLF
		cQuery += " WHERE "+ CRLF
		cQuery += " 	SD1.D1_FILIAL IN ('"+xFilial("SD1")+"','"+xFilial("SD1", MATRIZ)+"','"+xFilial("SD1", FIL_SUPR)+"','"+cFilComp+"') "+ CRLF
		cQuery += " 	AND SD1.D1_COD = '"+cProduto+"' "+ CRLF
		cQuery += " 	AND SD1.D1_LOCAL = '"+ERP_LOC_VENDAS+"' "+ CRLF
		cQuery += " 	AND SD1.D_E_L_E_T_ = ' ' "+ CRLF
		cQuery += " ORDER BY "+ CRLF
		cQuery += " 	SD1.D1_DTDIGIT, "+ CRLF
		cQuery += " 	SD1.D1_PEDIDO, "+ CRLF
		cQuery += " 	SD1.D1_ITEMPC "+ CRLF

		cAliasTop := MPSysOpenQuery(cQuery)

		Do While (cAliasTop)->(!eof())
			// Posiciona registros.
			SD1->(dbGoTo((cAliasTop)->SD1RecNo))
			SC7->(dbGoTo((cAliasTop)->SC7RecNo))

			// Verifica se o pedido tem agendamento de entrega.
			cQuery := "select top 1 ZC5.ZC5_DATA DTAGENDA, ZC5.ZC5_STATUS, ZC6.ZC6_DOC " + CRLF
			cQuery += "from " + RetSQLName("ZC6") + " ZC6 with (noLock) " + CRLF
			cQuery += "inner join " + RetSQLName("ZC5") + " ZC5 with (noLock) " + CRLF
			cQuery += "on ZC5.D_E_L_E_T_ = ' ' " + CRLF
			cQuery += "and ZC5.ZC5_FILIAL = '" + xFilial("ZC5", SD1->D1_FILIAL) + "' " + CRLF
			cQuery += "and ZC5.ZC5_CODIGO = ZC6.ZC6_AGENDA " + CRLF
			cQuery += "where ZC6.D_E_L_E_T_ = ' ' " + CRLF
			cQuery += "and ZC6.ZC6_FILIAL = '" + xFilial("ZC6", SD1->D1_FILIAL) + "' " + CRLF
			cQuery += "and ZC6.ZC6_NUMPED = '" + SD1->D1_PEDIDO + "' " + CRLF
			cQuery += "and ZC6.ZC6_ITEM   = '" + SD1->D1_ITEMPC + "' " + CRLF
			cQuery += "and ZC6.ZC6_STATUS IN (' ','01','03') " + CRLF
			cQuery += "and ZC5.ZC5_STATUS IN (' ','01','03') " + CRLF
			cQuery += "order by ZC5.ZC5_DATA desc " + CRLF

			cAliasTop2 := MPSysOpenQuery(cQuery)

			aAdd(aLstPrv, {})
			nLin := len(aLstPrv)

			If !Empty(SD1->D1_TES) //Nota Classificada
				cNomSts  := "Disponibilidade"
				dPrevEnt := U_SumDate(SC7->C7_DATPRF, nDiasProc)
				aAdd(aLstPrv[nLin], cNomSts )
				aAdd(aLstPrv[nLin], "Sim" )
				aAdd(aLstPrv[nLin], DToC(dPrevEnt) )
				aAdd(aLstPrv[nLin], Transform(SD1->D1_QUANT, cPictQtd) )
				aAdd(aLstPrv[nLin], SD1->(D1_PEDIDO + "-" + D1_ITEMPC) )
				aAdd(aLstPrv[nLin], "")

			ElseIf (cAliasTop2)->(eof()) .Or. (!Empty((cAliasTop2)->ZC6_DOC) .And. !((cAliasTop2)->ZC6_DOC == SD1->D1_DOC))

				dPrevEnt := U_SumDate(SC7->C7_DATPRF, nDiasProc)

				aAdd(aLstPrv[nLin], "Pré-nota" )
				aAdd(aLstPrv[nLin], "Não" )
				aAdd(aLstPrv[nLin], DToC(dPrevEnt) )
				aAdd(aLstPrv[nLin], Transform(SD1->D1_QUANT, cPictQtd) )
				aAdd(aLstPrv[nLin], SD1->(D1_PEDIDO + "-" + D1_ITEMPC) )
				aAdd(aLstPrv[nLin], "")
			Else
				If Empty((cAliasTop2)->ZC5_STATUS)
					cNomSts  := "Pré-nota"
				Else
					cNomSts  := "Armazenando"
				EndIf
				dPrevEnt := U_SumUteis(SToD((cAliasTop2)->DTAGENDA), nDiasAg)

				aAdd(aLstPrv[nLin], cNomSts )
				aAdd(aLstPrv[nLin], "Sim")
				aAdd(aLstPrv[nLin], DToC(dPrevEnt) )
				aAdd(aLstPrv[nLin], Transform(SD1->D1_QUANT, cPictQtd) )
				aAdd(aLstPrv[nLin], SD1->(D1_PEDIDO + "-" + D1_ITEMPC) )
				aAdd(aLstPrv[nLin], "")
			Endif

			DO CASE
				CASE SD1->D1_FILIAL == MATRIZ
					aAdd(aLstPrv[nLin], SD1->D1_FILIAL + "-Matriz")
				CASE SD1->D1_FILIAL == FIL_SUPR
					aAdd(aLstPrv[nLin], SD1->D1_FILIAL + "-" + FIL_SUPRN)
				CASE SD1->D1_FILIAL == xFilial("SD1")
					aAdd(aLstPrv[nLin], SD1->D1_FILIAL + "-Filial")
				OTHERWISE
					aAdd(aLstPrv[nLin], SD1->D1_FILIAL + "-" +FWFilialName(cEmpAnt, SD1->D1_FILIAL, 1) ) // Nome Reduzido Filial
			ENDCASE

			aAdd(aLstPrv[nLin], SD1->D1_FORNECE + "/" + SD1->D1_LOJA + " " + Posicione("SA2", 1, xFilial("SA2") + SD1->D1_FORNECE + SD1->D1_LOJA, "rtrim(A2_NOME)"))

			// Variável usada para calcular previsão de entrega do item.
			aAdd(aPrevEntr, {	DataValida(dPrevEnt),;													//01 - Data Previsao
								SD1->D1_QUANT,;															//02 - Quantidade / Saldo
								SD1->D1_FILIAL != xFilial("SD1"),;										//03 - Matriz ou Filial CD
								.F.,;																	//04 - LeadTime
								.F.})																	//05 - Pedido Atrasado

			(cAliasTop2)->(dbCloseArea())

			// Próximo registro.
			(cAliasTop)->(dbSkip())
		EndDo
		(cAliasTop)->(dbCloseArea())

		// Inclui os pedidos de compra em aberto.
		cQuery := "select SC7.R_E_C_N_O_ SC7RecNo " + CRLF
		cQuery += "from " + RetSQLName("SC7") + " SC7 with (noLock) " + CRLF
		cQuery += "where SC7.D_E_L_E_T_ = ' ' " + CRLF
		cQuery += "and SC7.C7_FILIAL  in ('" + xFilial("SC7") + "', '" + xFilial("SC7", MATRIZ) + "', '" + xFilial("SC7", FIL_SUPR) + "', '" + cFilComp + "') " + CRLF
		cQuery += "and SC7.C7_PRODUTO = '" + cProduto + "' " + CRLF
		cQuery += "and SC7.C7_LOCAL   = '" + ERP_LOC_VENDAS + "' " + CRLF
		cQuery += "and (SC7.C7_QUANT-SC7.C7_QTDACLA-SC7.C7_QUJE) > 0 " + CRLF
		cQuery += "and SC7.C7_ENCER <> 'E' " + CRLF
		cQuery += "and SC7.C7_RESIDUO = ' ' " + CRLF
		If !Empty(cNumPed)
			cQuery += " AND SC7.C7_NUM='"+cNumPed+"' " 		+CRLF
			cQuery += " AND SC7.C7_ITEM='"+cNumItem+"' " 	+CRLF
		EndIf
		cQuery += "order by SC7.C7_DATPRF, SC7.C7_NUM, SC7.C7_ITEM " + CRLF

		cAliasTop := MPSysOpenQuery(cQuery)

		Do While (cAliasTop)->(!eof())
			// Posiciona registros.
			SC7->(dbGoTo((cAliasTop)->SC7RecNo))

			// Verifica se o pedido tem agendamento de entrega.
			cQuery := "select top 1 ZC5.ZC5_DATA DTAGENDA " + CRLF
			cQuery += "from " + RetSQLName("ZC6") + " ZC6 with (noLock) " + CRLF
			cQuery += "inner join " + RetSQLName("ZC5") + " ZC5 with (noLock) " + CRLF
			cQuery += "on ZC5.D_E_L_E_T_ = ' ' " + CRLF
			cQuery += "and ZC5.ZC5_FILIAL = '" + xFilial("ZC5", SC7->C7_FILIAL) + "' " + CRLF
			cQuery += "and ZC5.ZC5_CODIGO = ZC6.ZC6_AGENDA " + CRLF
			cQuery += "where ZC6.D_E_L_E_T_ = ' ' " + CRLF
			cQuery += "and ZC6.ZC6_FILIAL = '" + xFilial("ZC6", SC7->C7_FILIAL) + "' " + CRLF
			cQuery += "and ZC6.ZC6_NUMPED = '" + SC7->C7_NUM + "' " + CRLF
			cQuery += "and ZC6.ZC6_ITEM   = '" + SC7->C7_ITEM + "' " + CRLF
			cQuery += "and ZC6.ZC6_STATUS = '' " + CRLF
			cQuery += "and ZC5.ZC5_STATUS = '' " + CRLF
			cQuery += "order by ZC5.ZC5_DATA desc " + CRLF

			cAliasTop2 := MPSysOpenQuery(cQuery)

			aAdd(aLstPrv, {})
			nLin := len(aLstPrv)

			If (cAliasTop2)->(eof())
				dPrevEnt := U_SumDate(SC7->C7_DATPRF, nDiasProc)

				aAdd(aLstPrv[nLin], "Pedido" )
				aAdd(aLstPrv[nLin], "Não" )
				aAdd(aLstPrv[nLin], DToC(dPrevEnt) )
				aAdd(aLstPrv[nLin], Transform(SC7->(C7_QUANT - C7_QUJE - C7_QTDACLA), cPictQtd) )
				aAdd(aLstPrv[nLin], SC7->(C7_NUM + "-" + C7_ITEM) )
				aAdd(aLstPrv[nLin], AllTrim(U_PROM316L(SC7->C7_FILIAL, SC7->C7_NUM, SC7->C7_ITEM)))
			Else
				dPrevEnt := U_SumUteis(SToD((cAliasTop2)->DTAGENDA), nDiasAg)

				aAdd(aLstPrv[nLin], "Pedido" )
				aAdd(aLstPrv[nLin], "Sim" )
				aAdd(aLstPrv[nLin], DToC(dPrevEnt) )
				aAdd(aLstPrv[nLin], Transform(SC7->(C7_QUANT - C7_QUJE - C7_QTDACLA), cPictQtd) )
				aAdd(aLstPrv[nLin], SC7->(C7_NUM + "-" + C7_ITEM) )
				aAdd(aLstPrv[nLin], AllTrim(U_PROM316L(SC7->C7_FILIAL, SC7->C7_NUM, SC7->C7_ITEM)))
			Endif

			DO CASE
				CASE SC7->C7_FILIAL == MATRIZ
					aAdd(aLstPrv[nLin], SC7->C7_FILIAL + "-Matriz")
				CASE SC7->C7_FILIAL == FIL_SUPR
					aAdd(aLstPrv[nLin], SC7->C7_FILIAL + "-" + FIL_SUPRN)
				CASE SC7->C7_FILIAL == xFilial("SC7")
					aAdd(aLstPrv[nLin], SC7->C7_FILIAL + "-Filial")
				OTHERWISE
					aAdd(aLstPrv[nLin], SC7->C7_FILIAL + "-" + FWFilialName(cEmpAnt, SC7->C7_FILIAL, 1) ) // Nome Reduzido Filial
			ENDCASE

			aAdd(aLstPrv[nLin], SC7->(C7_FORNECE + "/" + C7_LOJA + " " + Posicione("SA2", 1, xFilial("SA2") + C7_FORNECE + C7_LOJA, "rtrim(A2_NOME)")))

			// Variável usada para calcular previsão de entrega do item.
			aAdd(aPrevEntr, {	dPrevEnt,;											//01 - Data Previsao
								SC7->C7_QUANT - SC7->C7_QUJE - SC7->C7_QTDACLA,;	//02 - Quantidade / Saldo
								SC7->C7_FILIAL != xFilial("SC7"),;					//03 - Matriz ou Filial CD
								.F.,;												//04 - LeadTime
								dPrevEnt <= dDatabase})								//05 - Pedido Atrasado

			(cAliasTop2)->(dbCloseArea())

			// Próximo registro.
			(cAliasTop)->(dbSkip())
		EndDo
		(cAliasTop)->(dbCloseArea())

		//Inclusão de Saldos do Armazem 90 - Nao considerar no calculo do prazo de entrega
		SB2->(dbSetOrder(1))   // B2_FILIAL, B2_COD, B2_LOCAL.
		If SB2->(DbSeek(xFilial("SB2", MATRIZ) + cProduto + ERP_LOC_IMPORT))
			nSaldoImp := SB2->B2_QATU - SB2->B2_RESERVA - SB2->B2_QPEDVEN
			If nSaldoImp > 0
				aAdd(aLstPrv, {"", "", "", Transform(nSaldoImp, cPictQtd), "Armazém " + U_NNRDesc(ERP_LOC_IMPORT) + " (" + ERP_LOC_IMPORT + ")", "", SB2->B2_FILIAL + "-Matriz", "Produto em trânsito de importação"})
				aAdd(aPrevEntr, {	CtoD(""),;			//01 - Data Previsao
									nSaldoImp,;			//02 - Quantidade / Saldo
									.T.,;				//03 - Matriz ou Filial CD
									.T.,;				//04 - LeadTime
									.F.})				//05 - Pedido Atrasado
			Endif
		Endif

		//Prazo para produtos sem pedido de compra
		nPrazoEnt := U_LeadTime(cFilAnt, cProduto)
		dPrazoEnt := U_SumDate(dDatabase, nPrazoEnt)

		aAdd(aLstPrv, {"", "", "", "", "Lead-time: " + AllTrim(Transform(nPrazoEnt, cPictQtd)) + " dias", "", "", ""})
		aAdd(aPrevEntr, {	dPrazoEnt,;			//01 - Data Previsao
							-1,;				//02 - Quantidade / Saldo
							.F.,;				//03 - Matriz ou Filial CD
							.T.,;				//04 - LeadTime
							.F.})				//05 - Pedido Atrasado

	Endif

	If empty(aLstPrv)
		aLstPrv := {{"", "", "", "", "", "", "", ""}}
	Endif

Return NIL

//-------------------------------------------------------------------
/*/{Protheus.doc} xCusMed
Retorna o custo médio atual ou calculado.
@author  João Leão
@since   15/05/2020
@version 12.1.25
/*/
//-------------------------------------------------------------------
User Function xCusMed(cFilSKU, cSKU, cLocal, lAtual, dData)

Local aArea		:= GetArea()
Local aSB2		:= SB2->(GetArea())
Local aRet		:= {}

Default dData	:= CToD("  /  /    ")

If !lAtual .And. !Empty(dData)
	aRet := CalcEst(cSKU, cLocal, dData)
ElseIf lAtual
	SB2->(DBSetOrder(1)) //B2_FILIAL, B2_COD, B2_LOCAL
	If SB2->(DBSeek(cFilSKU + cSKU + cLocal))
		AAdd(aRet, SB2->B2_CM1)
	Else
		AAdd(aRet, 0)
	EndIf
EndIf

RestArea(aSB2)
RestArea(aArea)

Return aRet
//-------------------------------------------------------------------
/*/{Protheus.doc} SumDate
Soma a Quantidade de dias Uteis a Data informada

@author  Guilherme Santos
@since   03/06/2020
@version 12.1.25
/*/
//-------------------------------------------------------------------
User Function SumDate(dData, nDUteis)
	Local dRet		:= dData
	Local nCount	:= 0
	Local lUteis	:= SuperGetMV("BZ_PRVDUTL", NIL, .F.)

	If lUteis
		While nCount < nDUteis
			dRet := dRet + 1
			If dRet == DataValida(dRet)
				nCount++
			EndIf
		End
	Else
		dRet := DataValida(dData + nDUteis)
	EndIf

Return dRet
//-------------------------------------------------------------------
/*/{Protheus.doc} SumUteis
Soma a quantidade de dias uteis

@author  Guilherme Santos
@since   12/03/21
@version 12.1.25
/*/
//-------------------------------------------------------------------
User Function SumUteis(dData, nDUteis)
	Local dRet		:= dData
	Local nCount	:= 0

	While nCount < nDUteis
		dRet := dRet + 1
		If dRet == DataValida(dRet)
			nCount++
		EndIf
	End

Return dRet
//-------------------------------------------------------------------
/*/{Protheus.doc} PrzTransf
Retorna a quantidade de dias para Transferencia entre unidades

@author  Guilherme Santos
@since   04/06/2020
@version 12.1.25
/*/
//-------------------------------------------------------------------
User Function PrzTransf(cFilOri, cFilDest)
	Local aArea		:= GetArea()
	Local aAreaZBE	:= ZBE->(GetArea())
	Local nDias		:= 999

	DbSelectArea("ZBE")
	DbSetOrder(1)		//ZBE_FILIAL, ZBE_FILDES, ZBE_PE

	If ZBE->(DbSeek(cFilOri + cFilDest))
		nDias := ZBE->ZBE_PE
	EndIf

	RestArea(aAreaZBE)
	RestArea(aArea)
Return nDias

//-------------------------------------------------------------------
/*/{Protheus.doc} RetZBE
Retorna a transportadora de acordo com a ZBE passando a filial origem e a UF destino caso seja Transferencia
Retorna a transportadora de acordo com a ZAG caso não seja transferência

@author  Weslley T. Pereira
@since   27/05/2024
@version 12.1.2210
/*/
//-------------------------------------------------------------------
User Function RetTransp(cFilOri, cFilDes, cTransp)
	Local cQuery 	:= ""
	Local cTMPZBE 	:= GetNextAlias()
	Local cRet		:= ""

	Default cFilOri := cFilAnt
	Default cFilDes := ""
	Default cTransp	:= ""

	If !Empty(cTransp)
		cRet := cTransp
	EndIf

	If IsInCallStack("U_BIAPRO01") .And. ValType(SZO->ZO_TIPO) <> 'U' .And. SZO->ZO_TIPO $ "TRN|TMC|TAI"
		If Empty(cFilOri)
			cFilOri := cFilAnt
		EndIf

		cQuery := " SELECT "+ CRLF
		cQuery += " 	ZBE.ZBE_TRANSP "+ CRLF
		cQuery += " FROM "+RetSqlName("ZBE")+" ZBE "+ CRLF
		cQuery += " INNER JOIN "+RetSqlName("SA4")+" SA4 "+ CRLF
		cQuery += " 	ON SA4.A4_FILIAL = '"+xFilial("SA4")+"' "+ CRLF
		cQuery += " 	AND SA4.A4_COD = ZBE.ZBE_TRANSP "+ CRLF
		cQuery += " 	AND SA4.D_E_L_E_T_ = ' ' "+ CRLF
		cQuery += " WHERE "+ CRLF
		cQuery += " 	ZBE.ZBE_FILIAL = '"+cFilOri+"' "+ CRLF
		cQuery += "		AND ZBE.ZBE_FILDES = '" + cFilDes + "' "+ CRLF
		cQuery += " 	AND ZBE.D_E_L_E_T_ = ' ' "+ CRLF

		DbUseArea(.T.,"TOPCONN",TCGENQRY(,,cQuery),cTMPZBE,.T.,.T.)

		If (cTMPZBE)->(!EOF())
			cRet := (cTMPZBE)->ZBE_TRANSP
		EndIf

		(cTMPZBE)->(DbCloseArea())

	ElseIf !IsInCallStack("U_PVDesmem") .And. M->C5_TPFRETE="C" .And. !M->C5_XTIPO $ "TRN|TMC|TAI"
		If !Empty(M->C5_REDESP)
			M->C5_REDESP := Iif(ZAG->(!EOF()),U_AA080RTR(M->C5_XEST, M->C5_XCDMUNE),M->C5_REDESP)
			cRet := SuperGetMV("PC_TRANRED",, "")
		Else
			cRet := Iif(ZAG->(!EOF()),U_AA080RTR(M->C5_XEST, M->C5_XCDMUNE),M->C5_TRANSP)
		EndIf
	EndIf

Return cRet

//-------------------------------------------------------------------
/*/{Protheus.doc} LeadTime
Retorna o Prazo em Dias que o Fornecedor leva para entregar o Produto

@author  Guilherme Santos
@since   04/06/2020
@version 12.1.25
/*/
//-------------------------------------------------------------------
User Function LeadTime(cFilPed, cProduto)
	Local aArea		:= GetArea()
	Local aAreaSB1	:= SB1->(GetArea())
	Local aAreaSBZ	:= SBZ->(GetArea())
	Local aAreaSA2	:= SA2->(GetArea())
	Local nDias 	:= 999
	Local cTipCom	:= ""
	Local nDiasProc	:= SuperGetMV("PC_PRZNPED", NIL, 15)		//Prazo adicionado ao Lead Time do Pedido de Compra para Produtos sem Pedido

	DbSelectArea("SB1")
	DbSetOrder(1)	//B1_FILIAL, B1_COD

	If SB1->(DbSeek(xFilial("SB1") + cProduto))
		//Lead Time do Fornecedor + Dias de Processamento
		nDias := SB1->B1_PE + nDiasProc

		If !Empty(SB1->B1_PROC) .AND. !Empty(SB1->B1_LOJPROC)
			DbSelectArea("SA2")
			DbSetOrder(1)		//A2_FILIAL, A2_COD, A2_LOJA

			If SA2->(DbSeek(xFilial("SA2") + SB1->B1_PROC + SB1->B1_LOJPROC))
				cTipCom := SA2->A2_XTPCOMP

				Do Case
				Case cTipCom == "C" //Consolidado Guarulhos
					//Somar bz_pe da matriz
					nDias += Posicione("SBZ", 1, MATRIZ + cProduto, "BZ_PE")
					If cFilPed <> MATRIZ
						//Lead Time + Prazo Transferencia da Matriz
						nDias += U_PrzTransf(MATRIZ, cFilPed)
					EndIf
				Case cTipCom == "D"	//Direto
					DbSelectArea("SBZ")
					DbSetOrder(1)		//BZ_FILIAL, BZ_COD

					If SBZ->(DbSeek(cFilPed + cProduto))
						//Lead Time Matriz + Prazo de Processamento + LeadTime da Unidade
						nDias += SBZ->BZ_PE
					EndIf
				Case cTipCom == "E"	//Consolidado CDs
					Do Case
					Case cFilPed $ PROTCAP
						//Somar bz_pe da matriz
						nDias += Posicione("SBZ", 1, MATRIZ + cProduto, "BZ_PE")

						//Unidades que não vendem pela Matriz
						If cFilPed <> MATRIZ .AND. !(cFilPed $ FIL_MATRIZ)
							nDias += U_PrzTransf(MATRIZ, cFilPed)
						EndIf
					Case cFilPed $ CASADOEPI
						//somar bz_pe de contagem
						nDias += Posicione("SBZ", 1, "02" + cProduto, "BZ_PE")
						If cFilPed <> "02"
							nDias += U_PrzTransf("02", cFilPed)
						EndIf
					EndCase

				Case cTipCom == "F"	//Consolidado CDs + BA
					Do Case
					Case cFilPed == '01'

						nDias += Posicione("SBZ", 1, "01" + cProduto, "BZ_PE")

					Case cFilPed $ PROTCAP
						//Somar bz_pe da matriz
						nDias += Posicione("SBZ", 1, MATRIZ + cProduto, "BZ_PE")

						//Unidades que não vendem pela Matriz
						If cFilPed <> MATRIZ .AND. !(cFilPed $ FIL_MATRIZ)
							nDias += U_PrzTransf(MATRIZ, cFilPed)
						EndIf
					Case cFilPed $ CASADOEPI
						//somar bz_pe de contagem
						nDias += Posicione("SBZ", 1, "02" + cProduto, "BZ_PE")
						If cFilPed <> "02"
							nDias += U_PrzTransf("02", cFilPed)
						EndIf
					EndCase

				Case cTipCom == "M"	//Consolidado Minas
					//somar bz_pe de contagem
					nDias += Posicione("SBZ", 1, "02" + cProduto, "BZ_PE")
					If cFilPed <> "02"
						nDias += U_PrzTransf("02", cFilPed)
					EndIf
				EndCase
			EndIf
		EndIf
	EndIf

	RestArea(aAreaSA2)
	RestArea(aAreaSBZ)
	RestArea(aAreaSB1)
	RestArea(aArea)

Return nDias

//-------------------------------------------------------------------
/*/{Protheus.doc} GetSegto
Retorna o Segmento do Produto

@author  Guilherme Santos
@since   04/06/2020
@version 12.1.25
/*/
//-------------------------------------------------------------------
User Function GetSegto(cFilPed, cProduto)
	Local aArea		:= GetArea()
	Local aAreaSB5	:= SB5->(GetArea())
	Local aAreaSBZ	:= SBZ->(GetArea())
	Local cRetorno  := ''
	DbSelectArea("SB5")
	DbSetOrder(1)		//B1_FILIAL, B1_COD

	If SB5->(DbSeek(xFilial("SB5") + cProduto))

		cRetorno := SB5->B5_XSEGMEN
	EndIf

	RestArea(aAreaSBZ)
	RestArea(aAreaSB5)
	RestArea(aArea)

Return cRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} VerSld
Verifica se existe saldo para atender o pedido nas unidades 00 e 02

@author  Guilherme Santos
@since   04/06/2020
@version 12.1.25
/*/
//-------------------------------------------------------------------
User Function VerSld(cFilPed, cProduto, nQuant, dPrevEnt, nSldDisp)
	Local aArea			:= GetArea()
	Local cQuery 		:= ""
	Local cTabQry		:= ""
	Local lRetorno 		:= .F.
	Default dPrevEnt	:= CtoD("")
	Default nSldDisp	:= 0

	cQuery += "SELECT	SUM(SB2.B2_QATU - SB2.B2_RESERVA - SB2.B2_QPEDVEN) SLDDISP" + CRLF
	cQuery += "FROM		" + RetSqlName("SB2") + " SB2" + CRLF
	cQuery += "WHERE	SB2.B2_FILIAL IN ('00', '02')" + CRLF
	cQuery += "AND		SB2.B2_COD = '" + cProduto + "'" + CRLF
	cQuery += "AND		SB2.B2_LOCAL = '" + ERP_LOC_VENDAS + "'" + CRLF
	cQuery += "AND		SB2.D_E_L_E_T_ = ''" + CRLF

	cTabQry := MPSysOpenQuery(cQuery)

	If !(cTabQry)->(Eof())
		//Se tem saldo para atender o Orcamento / Pedido
		If (cTabQry)->SLDDISP >= nQuant
			dPrevEnt := U_SumDate(dDatabase, Max(U_PrzTransf(MATRIZ, cFilPed), U_PrzTransf("02", cFilPed)))
			nSldDisp := (cTabQry)->SLDDISP
			lRetorno := .T.
		Else
			//Se não tem saldo ou o saldo está negativo, abater o saldo negativo dos Pedidos de Compra
			nSldDisp := (cTabQry)->SLDDISP
		EndIf
	Else
		//Saldo zerado
		nSldDisp := 0
	EndIf

	If Select(cTabQry) > 0
		(cTabQry)->(DbCloseArea())
	EndIf

	RestArea(aArea)

Return lRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} RetLocEsp
Retorna armazens de controle especial.

@author  Wilson A. Silva Jr
@since   12/12/2019
@version 12.1.25
/*/
//-------------------------------------------------------------------
User Function RetLocEsp()

Local aArea   := GetArea()
Local cTMP1   := ""
Local cQuery  := ""
Local cLocEsp := ""

If u_ARMESPECIAL()

	cQuery := " SELECT "+ CRLF
	cQuery += " 	NNR_CODIGO "+ CRLF
	cQuery += " 	,NNR_DESCRI "+ CRLF
	cQuery += " FROM "+RetSqlName("NNR")+" NNR (NOLOCK) "+ CRLF
	cQuery += " WHERE "+ CRLF
	cQuery += " 	NNR_FILIAL = '"+xFilial("NNR")+"' "+ CRLF
	cQuery += " 	AND NNR_XCTRLE = 'S' "+ CRLF
	cQuery += " 	AND NNR.D_E_L_E_T_ = ' ' "+ CRLF
	cQuery += " ORDER BY "+ CRLF
	cQuery += " 	NNR_CODIGO "+ CRLF

	cTMP1 := MPSysOpenQuery(cQuery)

	While (cTMP1)->(!EOF())

		cLocEsp += (cTMP1)->NNR_CODIGO + "|"

		(cTMP1)->(DbSkip())
	EndDo

	(cTMP1)->(DbCloseArea())
EndIf

RestArea(aArea)

Return cLocEsp

//-------------------------------------------------------------------
/*/{Protheus.doc} RetCliFor
Retorna campo do cliente ou fornecedor de acordo com o tipo da nota
de entrada.

@author  Wilson A. Silva Jr
@since   12/12/2019
@version 12.1.25
/*/
//-------------------------------------------------------------------
User Function RetCliFor(cTipoNF,cChave,nOrdem,cCpoSA1,cCpoSA2)

Local aAreaAtu := GetArea()
Local aAreaSA1 := SA1->(GetArea())
Local aAreaSA2 := SA2->(GetArea())
Local xRetorno

DEFAULT cTipoNF := "N"
DEFAULT cChave  := &(ReadVar())
DEFAULT nOrdem  := 1
DEFAULT cCpoSA1 := "A1_NOME"
DEFAULT cCpoSA2 := "A2_NOME"

If cTipoNF $ "D,B"
	DbSelectArea("SA1")
	DbSetOrder(nOrdem)
	DbSeek(xFilial("SA1")+cChave)
	xRetorno := &("SA1->"+cCpoSA1)
Else
	DbSelectArea("SA2")
	DbSetOrder(nOrdem)
	DbSeek(xFilial("SA2")+cChave)
	xRetorno := &("SA2->"+cCpoSA2)
EndIf

RestArea(aAreaSA2)
RestArea(aAreaSA1)
RestArea(aAreaAtu)

Return xRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} ChkDesm
Verifica se o Pedido pai já foi desmembrado

@author  Guilherme Santos
@since   14/09/2020
@version 12.1.25
/*/
//-------------------------------------------------------------------
User Function ChkDesm(cPedido)
	Local cQuery 	:= ""
	Local cTabQry	:= ""
	Local lRetorno 	:= .F.

	cQuery += "SELECT	COUNT(SC5.C5_NUM) QTDREG" + CRLF
	cQuery += "FROM		" + RetSqlName("SC5") + " SC5 WITH (NOLOCK)" + CRLF
	cQuery += "WHERE	SC5.C5_FILIAL = '" + xFilial("SC5") + "'" + CRLF
	cQuery += "AND		SC5.C5_NUM != '" + cPedido + "'" + CRLF
	cQuery += "AND		(SC5.C5_XPEDPAI = '" + cPedido + "'" + CRLF
	cQuery += "OR		SC5.C5_XPEDORI = '" + cPedido + "')" + CRLF
	cQuery += "AND		SC5.D_E_L_E_T_ = ''" + CRLF
	cQuery += "GROUP BY SC5.C5_NUM" + CRLF

	cTabQry := MPSysOpenQuery(cQuery)

	While !(cTabQry)->(Eof())
		lRetorno := .T.

		(cTabQry)->(DbSkip())
	End

	If Select(cTabQry) > 0
		(cTabQry)->(DbCloseArea())
	EndIf

Return lRetorno

//-------------------------------------------------------------------
/*/{Protheus.doc} xSaldoSb2
Retorna saldo das filiais

cParam1: Código do produto
aParam2: Matriz com as filiais a serem consideradas
lParam3: .T. = Subtrai o campo B2_QPEDVEN, .F. = Não subtrai
lParam4: .T. = Subtrai o campo B2_RESERVA, .F. = Não subtrai
cParam5: Armazem a ser considerado

@author  Victor Dessunte
@since   06/10/2020
@version 12.1.25
/*/
//-------------------------------------------------------------------

User Function xSaldoSb2(cProd,aFiliais,lPedVen,lReserva,cLocal)

Local aAreaSB2 	:= SB2->(GetArea())
Local nSaldo 	:= 0
Local nQPedVen	:= 0
Local nReserva	:= 0
Local nX		:= 0
Local cFiliais	:= ""

Default aFiliais := {}
Default lPedVen  := .F.
Default cLocal	:= ERP_LOC_VENDAS

If Len(aFiliais) == 0
	U_Filiais(@cFiliais, @aFiliais)
EndIf

SB2->(dbSetOrder(1))
For nX:=1 to Len(aFiliais)
	If SB2->(dbSeek(aFiliais[nX]+PADR(cProd,TAMSX3('B1_COD')[1])+cLocal))
		nSaldo 		+= SB2->B2_QATU
		nQPedVen 	+= SB2->B2_QPEDVEN
		nReserva	+= SB2->B2_RESERVA
	EndIf
End

If lPedVen
	nSaldo := nSaldo - nQPedVen
EndIf

If lReserva
	nSaldo := nSaldo - nReserva
EndIf

RestArea(aAreaSB2)

Return nSaldo

//-------------------------------------------------------------------
/*/{Protheus.doc} XVALMERC
Retorna o total do valor de mercadorias do pedido de venda
@author  João Leão
@since   08/10/20
@version 12.1.25
/*/
//-------------------------------------------------------------------
User Function XVALMERC(cFilPed, cNumPed)
Local cQuery	:= ""
Local cAliasTop	:= ""
Local nRet		:= 0

cQuery := "SELECT  " + CRLF
cQuery += "	SUM(C6_VALOR) VALMERC " + CRLF
cQuery += "FROM  " + CRLF
cQuery += "	" + RetSQLName("SC6") + " SC6(NOLOCK)  " + CRLF
cQuery += "WHERE  " + CRLF
cQuery += "SC6.D_E_L_E_T_ = '' " + CRLF
cQuery += "AND C6_BLQ NOT IN ('R','S') " + CRLF
cQuery += "AND C6_FILIAL = '" + cFilPed +"'  " + CRLF
cQuery += "AND C6_NUM = '" + cNumPed + "' " + CRLF
cAliasTop := MPSysOpenQuery(cQuery)

If (cAliasTop)->(!EOF())
	nRet := (cAliasTop)->VALMERC
EndIf

(cAliasTop)->(DBCloseArea())

Return nRet

//-------------------------------------------------------------------
/*/{Protheus.doc} ARMESPECIAL
Retorna se está ativo o controle especial de Armazem
@author  Victor Dessunte
@since   05/02/2021
@version 12.1.25
/*/
//-------------------------------------------------------------------

User Function ARMESPECIAL()
Return SuperGetMv("PC_ARZESP",,.F.)

//-------------------------------------------------------------------
/*/{Protheus.doc} fVldCA
Bloqueia a venda dos produtos com CA vencido

@author  Guilherme Santos
@since   18/11/2020
@version 12.1.25
/*/
//-------------------------------------------------------------------
User Function fVldCA(cProduto, lMensagem, cMsgCA)
	Local cMensagem	:= ""
	Local lBlqCA	:= SuperGetMV("PC_BLTGVCA", NIL, .T.)		//Habilita o bloqueio da venda de produtos com CA vencidos ou a vencer
	Local lRetorno	:= .T.
	Local nDiasCA	:= SuperGetMV("PC_TGVDBCA", NIL, 5)			//Dias ate o vencimento do CA para bloqueio da venda
	Local nAlertCA 	:= SuperGetMV("PC_ALERTCA", NIL, 20)		//Dias para emitir o aviso de CA proximo ao vencimento
	Local nDiasVenc	:= 0
	Local aArea		:= GetArea()
	Local aSB1		:= SB1->(GetArea())
	Local aZB6		:= ZB6->(GetArea())

	Default cMsgCA	:= ""

	If lBlqCA
		DbSelectArea("SB1")
		DbSetOrder(1)		//B1_FILIAL, B1_COD

		If SB1->(DbSeek(xFilial("SB1") + cProduto))
			If !Empty(SB1->B1_CA)
				DbSelectArea("ZB6")
				DbSetOrder(1)		//ZB6_FILIAL, ZB6_COD

				If ZB6->(DbSeek(xFilial("ZB6") + SB1->B1_CA))
					nDiasVenc := ZB6->ZB6_VENCTO - Date()
					If (ZB6->ZB6_VENCTO - nDiasCA <= Date()) .OR. ZB6->ZB6_STATUS == "V"
						cMensagem += "Atenção! Produto com venda suspensa, não pode ser comercializado, devido ao certificado de aprovação vencido." + CRLF
						cMensagem += "Produto: " + SB1->B1_COD + CRLF
						cMensagem += "CA: " + AllTrim(SB1->B1_CA) + CRLF
						cMensagem += "Vencimento: " + DtoC(ZB6->ZB6_VENCTO) + CRLF
						lRetorno := .F.
					ElseIf nDiasVenc <= nAlertCA
						cMensagem += "Atenção! Restam " + cValToChar(nDiasVenc) + " dias para o vencimento do CA (" + AllTrim(SB1->B1_CA) + "), produto (" + AllTrim(cProduto) + "). Data de Vencimento: " + DToC(ZB6->ZB6_VENCTO)
					EndIf

					If ZB6->ZB6_PROTOC == "S" .And. ZB6->ZB6_VENCTO > Date()
						lRetorno := .T.
					Else
						If !lRetorno
							If lMensagem
								Aviso("fVldCA", cMensagem, {"Fechar"})
							Else
								cMsgCA := cMensagem
							EndIf
						EndIf
					EndIf
				EndIf
			EndIf
		EndIf
	EndIf

RestArea(aZB6)
RestArea(aSB1)
RestArea(aArea)

Return lRetorno

//-------------------------------------------------------------------
/*/{Protheus.doc} fValMul
Retorna se produto deve validar Múltiplo de vendas
@author  Victor Dessunte
@since   26/05/2021
@version 12.1.25
/*/
//-------------------------------------------------------------------

User Function fValMul(cProduto,cCliente,cLoja,lEstBr,lValida,nQtde)

Local aAreaSB1	:= SB1->(GetArea())
Local aAreaSB5	:= SB5->(GetArea())
Local aAreaSA1	:= SA1->(GetArea())
Local aAreaSB2	:= SB2->(GetArea())
Local nRet		:= 0
Local nSaldoSB2	:= 0
Local lRet		:= .T.
Local cSegmenta := GetNewPar("PC_MULSEGM","E") 				// Segmentos que devem validar multiplo e minimo de venda
Local cMulTipos := GetNewPar("PC_MULTIPO","ME,PA") 			// Produtos que devem validar multiplo e minimo de venda
Local dMultCort := GetNewPar("PC_MULCORT",SToD("20200612")) // Data de corte validações de multiplo e minimo em produtos contrato
Local cContrato := ""
Local dVigCrt 	:= StoD("")

Default nQtde := 0

SB1->(dbSetOrder(1))  // B1_FILIAL, B1_COD.
SB1->(msSeek(xFilial() + cProduto, .F.))

SB5->(dbSetOrder(1))  // B5_FILIAL, B5_COD.
SB5->(msSeek(xFilial() + cProduto, .F.))

SA1->(dbSetOrder(1))  // A1_FILIAL, A1_COD, A1_LOJA
SA1->(msSeek(xFilial() + cCliente + cLoja, .F.))

SB2->(dbSetOrder(1))  // B2_FILIAL, B2_COD, B2_LOCAL.
SB2->(msSeek(xFilial() + cProduto + ERP_LOC_VENDAS, .F.))
nSaldoSB2 := (SaldoSB2())

If SB5->B5_XSEGMEN $ cSegmenta .And. SB1->B1_TIPO $ cMulTipos

	U_MA020Ctr(SA1->A1_COD, SA1->A1_LOJA, cProduto,, @cContrato, @dVigCrt, .F.)

	ZA1->(dbSetOrder(1))  // ZA1_FILIAL, ZA1_COD, ZA1_VERSAO.
	ZA1->(dbSeek(xFilial() + cContrato, .F.))

	If (Empty(cContrato) .Or. dVigCrt < dDatabase .Or. ZA1->ZA1_DTATIV >= dMultCort) .And. !(ZA1->ZA1_VLDMM == 'N') .AND. SB5->B5_XMULENC > 0
		nRet := SB5->B5_XMULENC

		If lValida .AND. nRet > 0
			nVenda := nQtde
			nMulti := IIF(SB5->B5_XMULENC>0,SB5->B5_XMULENC,1)
			nSaldo := IIF(lEstBr,u_xSaldoSB2(SB1->B1_COD,,.T.,.T.,ERP_LOC_VENDAS),nSaldoSB2)

			If !lEstBr .AND. nSaldo >= nVenda
				Return {0,.T.}
			EndIf

			lRet   := .F.

			cInMsg	:= "ATENÇÃO!" + CRLF
			cInMsg	+= "ESTE ITEM ESTÁ CLASSIFICADO COMO ENCOMENDA NA BUNZL" + CRLF
			cInMsg  += "ITEM: " + cProduto
			cMsgAux := ""
			nSugMin := 0
			nSugMax := 0

			If (MOD(nVenda,nMulti) == 0) .Or. (nVenda < nSaldo) .Or. (MOD(nVenda,nMulti) <= nSaldo)
				lRet := .T.
			Else
				cMsgAux += "ADEQUAR MULTIPLO" + CRLF + CRLF
				nSugMin := (INT(nVenda / nMulti) * nMulti) + nSaldo
				nSugMax := (INT(nVenda / nMulti) + 1) * nMulti
			EndIf

			If !lRet
				cMsgAux += "  -> Estoque: " + cValToChar(IIF(nSaldo<=0,0,nSaldo)) + CRLF
				cMsgAux += "  -> Múltiplo: " + cValToChar(nMulti) + CRLF
				If nSugMin <= 0
					cMsgAux += "  -> Adequar para: " + cValToChar(nSugMax) + CRLF
				Else
					cMsgAux += "  -> Adequar para: " + IIF(nSugMin>0,cValToChar(nSugMin) + " ou " + cValToChar(nSugMax),cValToChar(nSugMax)) + CRLF
				EndIf

				cMsgAux := cInMsg + CRLF + CRLF + cMsgAux

				Help(,, 'Help',, cMsgAux, 1, 0)
			EndIf
		EndIf
	Else
		nRet := SB5->B5_XMULENC
	EndIf
EndIf

RestArea(aAreaSB2)
RestArea(aAreaSB1)
RestArea(aAreaSB5)
RestArea(aAreaSA1)

Return {nRet,lRet}

//-------------------------------------------------------------------
/*/{Protheus.doc} GetSup
Retorna um array com os superiores de uma determinada carteira
N1 - Supervisor, N2-Gerente, N3-Gerente Geral, N4-Gerente Regional, N5-Diretor, N6-Presidente
@author  Guelder A. Santos
@since   07/06/2021
@version 12.1.25
/*/
//-------------------------------------------------------------------
User Function GetSup(cCart,cDep,cFil)
	Local aArea   := GetArea()
	Local cAliSup := ''
	Local aSup	  := {}
	Local cQuery  := ''

	Default cCart := ''
	Default cDep  := '01'
	Default cFil  := '00'

    cQuery := " SELECT DISTINCT " + CRLF
    cQuery += "		ISNULL(ZAE1.ZAE_CODIGO " + CRLF
    cQuery += "         , CASE  " + CRLF
    cQuery += "             WHEN LEFT(ZAE.ZAE_CODIGO,1) = '1' THEN ZAE.ZAE_CODIGO " + CRLF
    cQuery += "             WHEN LEFT(ZAE.ZAE_CODIGO,1) = '3' THEN '13' + RIGHT(ZAE.ZAE_CODIGO,4) " + CRLF
    cQuery += "             WHEN LEFT(ZAE.ZAE_CODIGO,1) = '5' THEN '15' + RIGHT(ZAE.ZAE_CODIGO,4) " + CRLF
    cQuery += "             WHEN LEFT(ZAE.ZAE_CODIGO,1) = '6' THEN '16' + RIGHT(ZAE.ZAE_CODIGO,4) " + CRLF
    cQuery += "             WHEN LEFT(ZAE.ZAE_CODIGO,1) = '7' THEN '17' + RIGHT(ZAE.ZAE_CODIGO,4) " + CRLF
    cQuery += "             WHEN LEFT(ZAE.ZAE_CODIGO,1) = '8' THEN '18' + RIGHT(ZAE.ZAE_CODIGO,4) " + CRLF
    cQuery += "         END " + CRLF
    cQuery += "     ) 'N1' " + CRLF
    cQuery += "     , ISNULL(ZAE1.ZAE_DESC, ZAE.ZAE_DESC) 'DescN1' " + CRLF
    cQuery += "     , ISNULL(ZAE3.ZAE_CODIGO " + CRLF
    cQuery += "         , CASE " + CRLF
    cQuery += "             WHEN LEFT(ZAE.ZAE_CODIGO,1) = '3' THEN ZAE.ZAE_CODIGO " + CRLF
    cQuery += "             WHEN LEFT(ZAE.ZAE_CODIGO,1) = '1' THEN '31' + RIGHT(ZAE.ZAE_CODIGO,4) " + CRLF
    cQuery += "             WHEN LEFT(ZAE.ZAE_CODIGO,1) = '5' THEN '35' + RIGHT(ZAE.ZAE_CODIGO,4) " + CRLF
    cQuery += "             WHEN LEFT(ZAE.ZAE_CODIGO,1) = '6' THEN '36' + RIGHT(ZAE.ZAE_CODIGO,4) " + CRLF
    cQuery += "             WHEN LEFT(ZAE.ZAE_CODIGO,1) = '7' THEN '37' + RIGHT(ZAE.ZAE_CODIGO,4) " + CRLF
    cQuery += "             WHEN LEFT(ZAE.ZAE_CODIGO,1) = '8' THEN '38' + RIGHT(ZAE.ZAE_CODIGO,4) " + CRLF
    cQuery += "         END " + CRLF
    cQuery += "     ) 'N2' " + CRLF
    cQuery += "     , ISNULL(ZAE3.ZAE_DESC, ZAE.ZAE_DESC) 'DescN2' " + CRLF
    cQuery += "     , ISNULL(ZAE5.ZAE_CODIGO " + CRLF
    cQuery += "         , CASE " + CRLF
    cQuery += "             WHEN LEFT(ZAE.ZAE_CODIGO,1) = '5' THEN ZAE.ZAE_CODIGO " + CRLF
    cQuery += "             WHEN LEFT(ZAE.ZAE_CODIGO,1) = '1' THEN '51' + RIGHT(ZAE.ZAE_CODIGO,4) " + CRLF
    cQuery += "             WHEN LEFT(ZAE.ZAE_CODIGO,1) = '3' THEN '53' + RIGHT(ZAE.ZAE_CODIGO,4) " + CRLF
    cQuery += "             WHEN LEFT(ZAE.ZAE_CODIGO,1) = '6' THEN '56' + RIGHT(ZAE.ZAE_CODIGO,4) " + CRLF
    cQuery += "             WHEN LEFT(ZAE.ZAE_CODIGO,1) = '7' THEN '57' + RIGHT(ZAE.ZAE_CODIGO,4) " + CRLF
    cQuery += "             WHEN LEFT(ZAE.ZAE_CODIGO,1) = '8' THEN '58' + RIGHT(ZAE.ZAE_CODIGO,4) " + CRLF
    cQuery += "         END " + CRLF
    cQuery += "     ) 'N3' " + CRLF
    cQuery += "     , ISNULL(ZAE5.ZAE_DESC, ZAE.ZAE_DESC) 'DescN3' " + CRLF
    cQuery += "     , ISNULL(ZAE6.ZAE_CODIGO " + CRLF
    cQuery += "         , CASE " + CRLF
    cQuery += "             WHEN LEFT(ZAE.ZAE_CODIGO,1) = '6' THEN ZAE.ZAE_CODIGO " + CRLF
    cQuery += "             WHEN LEFT(ZAE.ZAE_CODIGO,1) = '1' THEN '61' + RIGHT(ZAE.ZAE_CODIGO,4) " + CRLF
    cQuery += "             WHEN LEFT(ZAE.ZAE_CODIGO,1) = '3' THEN '63' + RIGHT(ZAE.ZAE_CODIGO,4) " + CRLF
    cQuery += "             WHEN LEFT(ZAE.ZAE_CODIGO,1) = '5' THEN '65' + RIGHT(ZAE.ZAE_CODIGO,4) " + CRLF
    cQuery += "             WHEN LEFT(ZAE.ZAE_CODIGO,1) = '7' THEN '67' + RIGHT(ZAE.ZAE_CODIGO,4) " + CRLF
    cQuery += "             WHEN LEFT(ZAE.ZAE_CODIGO,1) = '8' THEN '68' + RIGHT(ZAE.ZAE_CODIGO,4) " + CRLF
    cQuery += "         END " + CRLF
    cQuery += "     ) 'N4' " + CRLF
    cQuery += "     , ISNULL(ZAE6.ZAE_DESC, ZAE.ZAE_DESC) 'DescN4' " + CRLF
    cQuery += "     , ISNULL(ZAE7.ZAE_CODIGO " + CRLF
    cQuery += "         , CASE " + CRLF
    cQuery += "             WHEN LEFT(ZAE.ZAE_CODIGO,1) = '7' THEN ZAE.ZAE_CODIGO " + CRLF
    cQuery += "             WHEN LEFT(ZAE.ZAE_CODIGO,1) = '1' THEN '71' + RIGHT(ZAE.ZAE_CODIGO,4) " + CRLF
    cQuery += "             WHEN LEFT(ZAE.ZAE_CODIGO,1) = '3' THEN '73' + RIGHT(ZAE.ZAE_CODIGO,4) " + CRLF
    cQuery += "             WHEN LEFT(ZAE.ZAE_CODIGO,1) = '5' THEN '75' + RIGHT(ZAE.ZAE_CODIGO,4) " + CRLF
    cQuery += "             WHEN LEFT(ZAE.ZAE_CODIGO,1) = '6' THEN '76' + RIGHT(ZAE.ZAE_CODIGO,4) " + CRLF
    cQuery += "             WHEN LEFT(ZAE.ZAE_CODIGO,1) = '8' THEN '78' + RIGHT(ZAE.ZAE_CODIGO,4) " + CRLF
    cQuery += "         END " + CRLF
    cQuery += "     ) 'N5' " + CRLF
    cQuery += "     , ISNULL(ZAE7.ZAE_DESC, ZAE.ZAE_DESC) 'DescN5' " + CRLF
    cQuery += "     , ISNULL(ZAE8.ZAE_CODIGO " + CRLF
    cQuery += "         , CASE  " + CRLF
    cQuery += "             WHEN LEFT(ZAE.ZAE_CODIGO,1) = '8' THEN ZAE.ZAE_CODIGO " + CRLF
    cQuery += "             WHEN LEFT(ZAE.ZAE_CODIGO,1) = '1' THEN '81' + RIGHT(ZAE.ZAE_CODIGO,4) " + CRLF
    cQuery += "             WHEN LEFT(ZAE.ZAE_CODIGO,1) = '3' THEN '83' + RIGHT(ZAE.ZAE_CODIGO,4) " + CRLF
    cQuery += "             WHEN LEFT(ZAE.ZAE_CODIGO,1) = '5' THEN '85' + RIGHT(ZAE.ZAE_CODIGO,4) " + CRLF
    cQuery += "             WHEN LEFT(ZAE.ZAE_CODIGO,1) = '6' THEN '86' + RIGHT(ZAE.ZAE_CODIGO,4) " + CRLF
    cQuery += "             WHEN LEFT(ZAE.ZAE_CODIGO,1) = '7' THEN '87' + RIGHT(ZAE.ZAE_CODIGO,4) " + CRLF
    cQuery += "         END " + CRLF
    cQuery += "     ) 'N6' " + CRLF
    cQuery += "     , ISNULL(ZAE8.ZAE_DESC, ZAE.ZAE_DESC) 'DescN6' " + CRLF
    cQuery += " FROM " + CRLF
    cQuery +=      RetSqlName("ZAE") + " ZAE WITH (NOLOCK) " + CRLF
    cQuery += "    INNER JOIN " + RetSqlName("ZA6") + " ZA6 WITH(NOLOCK) " + CRLF
    cQuery += "         ON ZA6.D_E_L_E_T_ = '' " + CRLF
    cQuery += "         AND ZA6.ZA6_GRCART = ZAE.ZAE_CODIGO " + CRLF
    cQuery += "    LEFT JOIN " + RetSqlName("ZAE") + " ZAE1 WITH (nolock) " + CRLF
    cQuery += "         ON ZAE1.D_E_L_E_T_ = '' " + CRLF
    cQuery += "         AND ZAE1.ZAE_FILIAL = '" + xFilial("ZAE") + "' " + CRLF
    cQuery += "         AND ZAE1.ZAE_CODIGO = ZAE.ZAE_GRPSUP " + CRLF
    cQuery += "         AND LEFT(ZAE1.ZAE_CODIGO,1) = '1' " + CRLF
    cQuery += "     LEFT JOIN " + RetSqlName("ZAE") + " ZAE3 WITH (NOLOCK) " + CRLF
    cQuery += "         ON ZAE3.D_E_L_E_T_ = '' " + CRLF
    cQuery += "         AND ZAE3.ZAE_FILIAL = '" + xFilial("ZAE") + "' " + CRLF
    cQuery += "         AND ZAE3.ZAE_CODIGO IN (ZAE.ZAE_GRPSUP, ZAE1.ZAE_GRPSUP) " + CRLF
    cQuery += "         AND LEFT(ZAE3.ZAE_CODIGO,1) = '3' " + CRLF
    cQuery += "    LEFT JOIN " + RetSqlName("ZAE") + " ZAE5 WITH (NOLOCK) " + CRLF
    cQuery += "         ON ZAE5.D_E_L_E_T_ = '' " + CRLF
    cQuery += "         AND ZAE5.ZAE_FILIAL = '" + xFilial("ZAE") + "' " + CRLF
    cQuery += "         AND ZAE5.ZAE_CODIGO IN (ZAE.ZAE_GRPSUP, ZAE1.ZAE_GRPSUP, ZAE3.ZAE_GRPSUP) " + CRLF
    cQuery += "         AND LEFT(ZAE5.ZAE_CODIGO,1) = '5' " + CRLF
    cQuery += "     LEFT JOIN " + RetSqlName("ZAE") + " ZAE6 WITH (NOLOCK) " + CRLF
    cQuery += "         ON ZAE6.D_E_L_E_T_ = '' " + CRLF
    cQuery += "         AND ZAE6.ZAE_FILIAL = '" + xFilial("ZAE") + "' " + CRLF
    cQuery += "         AND ZAE6.ZAE_CODIGO IN (ZAE.ZAE_GRPSUP, ZAE1.ZAE_GRPSUP, ZAE3.ZAE_GRPSUP, ZAE5.ZAE_GRPSUP) " + CRLF
    cQuery += "         AND LEFT(ZAE6.ZAE_CODIGO,1) = '6' " + CRLF
    cQuery += "     LEFT JOIN " + RetSQLName("ZAE") + " ZAE7 WITH (NOLOCK) " + CRLF
    cQuery += "         ON ZAE7.D_E_L_E_T_ = '' " + CRLF
    cQuery += "         AND ZAE7.ZAE_FILIAL = '"+xFilial("ZAE")+ "' " + CRLF
    cQuery += "         AND ZAE7.ZAE_CODIGO IN (ZAE.ZAE_GRPSUP, ZAE1.ZAE_GRPSUP, ZAE3.ZAE_GRPSUP, ZAE5.ZAE_GRPSUP, ZAE6.ZAE_GRPSUP) " + CRLF
    cQuery += "          AND LEFT(ZAE7.ZAE_CODIGO,1) = '7' " + CRLF
    cQuery += "     LEFT JOIN " + RetSqlName("ZAE") + " ZAE8 WITH (NOLOCK) " + CRLF
    cQuery += "          ON ZAE8.D_E_L_E_T_ = '' " + CRLF
    cQuery += "          AND ZAE8.ZAE_FILIAL = '" + xFilial("ZAE") + "' " + CRLF
    cQuery += "          AND ZAE8.ZAE_CODIGO IN (ZAE.ZAE_GRPSUP, ZAE1.ZAE_GRPSUP, ZAE3.ZAE_GRPSUP, ZAE5.ZAE_GRPSUP, ZAE6.ZAE_GRPSUP, ZAE7.ZAE_GRPSUP) " + CRLF
    cQuery += "          AND LEFT(ZAE8.ZAE_CODIGO,1) = '8' " + CRLF
    cQuery += "  WHERE  " + CRLF
    cQuery += "  ZAE.D_E_L_E_T_ = '' " + CRLF
	cQuery += "  AND ZAE.ZAE_CODIGO = '" + cCart + "' " + CRLF
    cQuery += "  AND LEFT(ZAE.ZAE_CODIGO,1) >= '1'" + CRLF
	cQuery += "  AND EXISTS( SELECT TOP 1 ZA6.ZA6_GRCART " + CRLF
	cQuery += " 		FROM " + RetSqlName("ZA6") + " ZA6 WITH(NOLOCK) " + CRLF
	cQuery += " 		WHERE " + CRLF
	cQuery += " 		ZA6.D_E_L_E_T_ = '' " + CRLF
	cQuery += " 		AND ZA6.ZA6_GRCART = ZAE.ZAE_CODIGO " + CRLF
	cQuery += "			AND ZA6.ZA6_FILIAL = '" + cFil + "' " + CRLF
	cQuery += " 		AND ZA6.ZA6_DEPVEN = '" + cDep + "' ) " + CRLF

    cAliSup:= MPSysOpenQuery(cQuery)

	If (cAliSup)->(!eof())
			aadd(aSup,{	(cAliSup)->(N1)		,;//01-CÓDIGO DO SUPERVISOR
						(cAliSup)->(DESCN1)	,;//02-NOME DO SUPERVISOR
						(cAliSup)->(N2)		,;//03-CÓDIGO DO GERENTE
						(cAliSup)->(DESCN2)	,;//04-NOME DO GERENTE
						(cAliSup)->(N3)		,;//05-CÓDIGO DO GERENTE GERAL
						(cAliSup)->(DESCN3)	,;//06-NOME DO GERENTE GERAL
						(cAliSup)->(N4)		,;//07-CÓDIGO DO REGIONAL
						(cAliSup)->(DESCN4)	,;//08-NOME DO REGIONAL
						(cAliSup)->(N5)		,;//09-CÓDIGO DO DIRETOR
						(cAliSup)->(DESCN5)	,;//10-NOME DO DIRETOR
						(cAliSup)->(N6)		,;//11-CÓDIGO DO PRESIDENTE
						(cAliSup)->(DESCN6) })//12-NOME DO PRESIDENTE
	Else
		aadd(aSup,{"","","","","","","","","","","",""})
	EndIf

	RestArea( aArea )
    (cAliSup)->(dbCloseArea())

Return aSup


//-------------------------------------------------------------------
/*/{Protheus.doc} GtLdTime
Retorna o Prazo em Dias que o Fornecedor leva para entregar o Produto

@author  Guilherme Santos
@since   04/06/2020
@version 12.1.25
/*/
//-------------------------------------------------------------------
User Function GtLdTime(cFilPed, cProduto)
	Local aArea		:= GetArea()
	Local aAreaSB1	:= SB1->(GetArea())
	Local aAreaSBZ	:= SBZ->(GetArea())
	Local aAreaSA2	:= SA2->(GetArea())
	Local nDias 	:= 999
	Local cTipCom	:= ""
	Local nDiasProc	:= SuperGetMV("PC_PRZNPED", NIL, 15)		//Prazo adicionado ao Lead Time do Pedido de Compra para Produtos sem Pedido

	DbSelectArea("SB1")
	DbSetOrder(1)	//B1_FILIAL, B1_COD

	If SB1->(DbSeek(xFilial("SB1") + cProduto))
		//Lead Time do Fornecedor + Dias de Processamento
		nDias := SB1->B1_PE + nDiasProc

			Do Case
				Case cTipCom == "E"	//Consolidado CDs
					Do Case
					Case cFilPed $ PROTCAP
						//Somar bz_pe da matriz
						//nDias += Posicione("SBZ", 1, MATRIZ + cProduto, "BZ_PE")

						//Unidades que não vendem pela Matriz
						If cFilPed <> MATRIZ .AND. !(cFilPed $ FIL_MATRIZ)
							nDias += U_PrzTransf(MATRIZ, cFilPed)
						EndIf
					Case cFilPed $ CASADOEPI
						//somar bz_pe de contagem
						//nDias += Posicione("SBZ", 1, "02" + cProduto, "BZ_PE")
						If cFilPed <> "02"
							nDias += U_PrzTransf("02", cFilPed)
						EndIf
					EndCase
			EndCase


	EndIf

	RestArea(aAreaSA2)
	RestArea(aAreaSBZ)
	RestArea(aAreaSB1)
	RestArea(aArea)

Return nDias

/*/{Protheus.doc} zLimpaEsp
Função que limpa os caracteres especiais dentro de um campo
@type function
@author Guelder
@since 28/06/2021
@version 1.0
@param cConteudo
    @example
    u_zLimpaEsp()
/*/

User Function zLimpaEsp(cConteudo)
    Local aArea     := GetArea()
    Local cStrRet   := cConteudo

    //Retirando caracteres
    cStrRet := StrTran(cStrRet, "'", "")
    cStrRet := StrTran(cStrRet, "%", "")
    cStrRet := StrTran(cStrRet, "*", "")
    cStrRet := StrTran(cStrRet, "&", "E")
    cStrRet := StrTran(cStrRet, ">", "")
    cStrRet := StrTran(cStrRet, "<", "")
    cStrRet := StrTran(cStrRet, "!", "")
    cStrRet := StrTran(cStrRet, "@", "")
    cStrRet := StrTran(cStrRet, "$", "")
    cStrRet := StrTran(cStrRet, "=", "")
    cStrRet := StrTran(cStrRet, "+", "")
    cStrRet := StrTran(cStrRet, "{", "")
    cStrRet := StrTran(cStrRet, "}", "")
    cStrRet := StrTran(cStrRet, "[", "")
    cStrRet := StrTran(cStrRet, "]", "")
    cStrRet := StrTran(cStrRet, "?", "")
    cStrRet := StrTran(cStrRet, ".", "")
    cStrRet := StrTran(cStrRet, ":", "")
    cStrRet := StrTran(cStrRet, ";", "")
    cStrRet := StrTran(cStrRet, '"', '')
    cStrRet := StrTran(cStrRet, '°', '')
    cStrRet := StrTran(cStrRet, 'ª', '')
    cStrRet := StrTran(cStrRet, ",", "")

    RestArea(aArea)
Return(cStrRet)

//-------------------------------------------------------------------
/*/{Protheus.doc} SLDINI
Verifica se existe saldo para o Armazem no SB2, se não existir cria o saldo inicial (SB9)

@author  Guilherme Santos
@since   12/08/2021
@version 12.1.25
/*/
//-------------------------------------------------------------------
User Function SLDINI(cProduto, cLocSld)
	Local aArea		:= GetArea()
	Local aAreaSB2	:= SB2->(GetArea())
	Local aAreaSB9	:= SB9->(GetArea())
	Local aItensSB9	:= {}
	Local cMensagem	:= ""
	Local lRetorno 	:= .T.

	Private	lMsErroAuto	:= .F.

	DbSelectArea("SB2")
	SB2->(DbSetOrder(1))		//B2_FILIAL, B2_COD, B2_LOCAL

	If !SB2->(DbSeek(xFilial("SB2") + cProduto + cLocSld))
		Aadd(aItensSB9, {"B9_FILIAL", xFilial("SB9"), NIL})
		Aadd(aItensSB9, {"B9_COD", cProduto, NIL})
		Aadd(aItensSB9, {"B9_LOCAL", cLocSld, NIL})
		Aadd(aItensSB9, {"B9_QINI", 0, NIL})
		Aadd(aItensSB9, {"B9_VINI1", 0, NIL})
		Aadd(aItensSB9, {"B9_DATA", CtoD(""), NIL})

		lMsErroAuto	:= .F.

		MSExecAuto({|x, y| Mata220(x, y)}, aItensSB9, 3)

		If lMsErroAuto
			lRetorno 	:= .F.
			cMensagem	:= MostraErro("\debug\", "MATA220.log")
		EndIf
	EndIf

	RestArea(aAreaSB9)
	RestArea(aAreaSB2)
	RestArea(aArea)

Return lRetorno

//-------------------------------------------------------------------
/*/{Protheus.doc} XCULTCOM
Atualiza a data do último faturamento no campo A1_ULTCOM
@author  João Leão
@since   22/09/2021
@version 12.1.25
/*/
//-------------------------------------------------------------------
User Function XCULTCOM(aParam, cCodCli, cLojaCli)

Local cQuery	:= ""
Local cAliasTop	:= ""
Local lAuto		:= isBlind()
Local cAtuCpo1	:= ""

Default cCodCli		:= ""
Default cLojaCli	:= ""

If lAuto
	RpcSetType(3)
	RpcSetEnv(aParam[1], aParam[2],,, "FAT")
Else
	aParam := {cEmpAnt, cFilAnt}
EndIf

cAtuCpo1 := SuperGetMV("XCULTCOM01",,"N")

If cAtuCpo1 == "S"

	cQuery := "UPDATE SA1 SET A1_ULTCOM = A.D2_EMISSAO " + CRLF
	cQuery += "FROM ( " + CRLF
	cQuery += "	SELECT " + CRLF
	cQuery += "		D2_CLIENTE " + CRLF
	cQuery += "		, D2_LOJA " + CRLF
	cQuery += "		, MAX(D2_EMISSAO) D2_EMISSAO " + CRLF
	cQuery += "	FROM " + RetSQLName("SD2") + " SD2 WITH(NOLOCK) " + CRLF
	cQuery += "		inner join " + RetSQLName("SF4") + " SF4 with (nolock)  " + CRLF
	cQuery += "			on SF4.D_E_L_E_T_ = ' '  " + CRLF
	cQuery += "			and SF4.F4_FILIAL = '" + xFilial("SF4") + "'   " + CRLF
	cQuery += "			and SF4.F4_CODIGO = SD2.D2_TES   " + CRLF
	cQuery += "			and SF4.F4_DUPLIC = 'S'  " + CRLF
	cQuery += "			and SF4.F4_CF NOT IN ('5551','6551','7551') " + CRLF
	cQuery += "	WHERE " + CRLF
	cQuery += "	SD2.D_E_L_E_T_ = ' '   " + CRLF
	cQuery += "	and SD2.D2_FILIAL BETWEEN '  ' AND 'ZZ' " + CRLF
	cQuery += "	and SD2.D2_TIPO not in ('D','B')   " + CRLF
	cQuery += "	and SD2.D2_CLIENTE <> '999999' " + CRLF
	cQuery += "	GROUP BY D2_CLIENTE, D2_LOJA " + CRLF
	cQuery += ") A " + CRLF
	cQuery += "	INNER JOIN " + RetSQLName("SA1") + " SA1 WITH(NOLOCK) " + CRLF
	cQuery += "		ON SA1.D_E_L_E_T_ = '' " + CRLF
	cQuery += "		AND A1_FILIAL = '" + xFilial("SA1") + "' " + CRLF
	cQuery += "		AND A1_COD = A.D2_CLIENTE " + CRLF
	cQuery += "		AND A1_LOJA = A.D2_LOJA   " + CRLF
	cQuery += "WHERE " + CRLF
	cQuery += "A1_ULTCOM <> A.D2_EMISSAO " + CRLF
	If !Empty(cCodCli) .And. !Empty(cLojaCli)
		cQuery += "AND A1_COD = '" + cCodCli + "' " + CRLF
		cQuery += "AND A1_LOJA = '" + cLojaCli + "' " + CRLF
	EndIf
	cAliasTop := MPSysOpenQuery(cQuery)

	If (cAliasTop)->(!EOF())
		TCSqlExec(cQuery)
	EndIf

	(cAliasTop)->(dbCloseArea())

EndIf

Return Nil

//-------------------------------------------------------------------
/*/{Protheus.doc} xRetChvEDI
Mostras as chaves edi do cliente+loja e retorna a escolhida pelo usuário.
@author  João Leão
@since   11/11/2021
@version 12.1.25
/*/
//-------------------------------------------------------------------
User Function xRetChvEDI(cCodCli, cLojaCli, cChvEDI, cCodCont, cCNPJEDI, lExibeDialog)
Local cRet			:= ""
Local aSA1			:= SA1->(GetArea())
Local cTitulo		:= "EDI Endereços - " + Posicione("SA1", 1, xFilial("SA1") + cCodCli + cLojaCli, "A1_NOME")
Local oDlg, oLbx
Local nChvEDI		:= 0
Local nContEDI		:= 0
Local aChvEDI		:= {}
Local cQuery		:= ""
Local cAliasTop		:= ""

Default cChvEDI			:= ""
Default cCodCont		:= ""
Default cCNPJEDI		:= ""
Default lExibeDialog	:= .T.

cQuery := "SELECT  " + CRLF
cQuery += "	ZW_CLIENTE, ZW_LOJA, ZW_CHVEDI, ZW_CODCONT, ZW_CGC " + CRLF
cQuery += "FROM " + RetSQLName("SZW") + " SZW WITH(NOLOCK) " + CRLF
cQuery += "WHERE SZW.D_E_L_E_T_ = '' " + CRLF
cQuery += "AND ZW_FILIAL = '" + xFilial("SZW") + "' " + CRLF
cQuery += "AND ZW_CLIENTE = '" + cCodCli + "' " + CRLF
cQuery += "AND ZW_LOJA = '" + cLojaCli + "' " + CRLF
If !Empty(cChvEDI)
	cQuery += "AND ZW_CHVEDI = '" + cChvEDI + "' " + CRLF
EndIf
If !Empty(cCodCont)
	cQuery += "AND ZW_CODCONT = '" + cCodCont + "' " + CRLF
EndIf
If !Empty(cChvEDI)
	cQuery += "AND ZW_CGC = '" + cCNPJEDI + "' " + CRLF
EndIf
cAliasTop := MPSysOpenQuery(cQuery)

While (cAliasTop)->(!EOF())
	nContEDI++
	AAdd(aChvEDI, {cCodCli, cLojaCli, AllTrim((cAliasTop)->ZW_CHVEDI), (cAliasTop)->ZW_CODCONT, (cAliasTop)->ZW_CGC})
	cRet := AllTrim((cAliasTop)->ZW_CHVEDI)
	(cAliasTop)->(DBSkip())
EndDo

If nContEDI > 1 .And. lExibeDialog
	DEFINE MSDIALOG oDlg FROM  48,171 TO 230,800 TITLE cTitulo PIXEL

		@  3,2 TO  73, 310 LABEL "EDI Endereços:" OF oDlg  PIXEL
			@ 10,5	LISTBOX oLbx FIELDS ;
					HEADER ;
					"Cliente",;
					"Loja",;
					"Chave EDI",;
					"Cód. Contato",;
					"CNPJ";
					SIZE 303,60  NOSCROLL OF oDlg PIXEL ;
					ON DBLCLICK( nOpcao:= 1,nChvEDI := oLbx:nAt,oDlg:End() )

					oLbx:SetArray(aChvEDI)
					oLbx:bLine:={ || {	aChvEDI[oLbx:nAt,1],;	//Cliente
										aChvEDI[oLbx:nAt,2],;	//Loja
										aChvEDI[oLbx:nAt,3],;	//Chave EDI
										aChvEDI[oLbx:nAt,4],;	//Cód. Contato
										aChvEDI[oLbx:nAt,5];	//CNPJ
										}}

		DEFINE SBUTTON FROM 74,252 TYPE 1	ENABLE OF oDlg ACTION (nOpcao:= 1,nChvEDI:= oLbx:nAt,oDlg:End())
		DEFINE SBUTTON FROM 74,282 TYPE 2	ENABLE OF oDlg ACTION (nOpcao:= 0,nChvEDI:= oLbx:nAt,oDlg:End())

	ACTIVATE MSDIALOG oDlg CENTERED

	If (nOpcao == 1 .Or. nOpcao == 0)
		cRet := aChvEDI[nChvEDI,3]
	Endif
EndIf

RestArea(aSA1)
Return cRet

//-------------------------------------------------------------------
/*/{Protheus.doc} UpdGtCli
description Update no campo A1_XGTP para renvio das informações de clientes
@author  Victor Dessunte
@since   28/12/2021
@version version
/*/
//-------------------------------------------------------------------

User Function UpdGtCli(cGrpCli)

Local aArea := GetArea()
Local cQry  := ''

cQry := " UPDATE " + RetSqlName("SA1") + CRLF
cQry += " SET A1_XGTP = '' " + CRLF
cQry += " WHERE D_E_L_E_T_ = '' " + CRLF
cQry += " AND A1_FILIAL = '' " + CRLF
cQry += " AND A1_XGTP <> '' " + CRLF
cQry += " AND A1_GRPVEN = '" + cGrpCli + "' "

TCSqlExec(cQry)

RestArea(aArea)

Return

//-------------------------------------------------------------------
/*/{Protheus.doc} ListaSim
Retorna lista de produtos similares
@author  João Leão
@since   21/02/2022
@version 12.1.27
/*/
//-------------------------------------------------------------------
User Function ListaSim(cProduto, aLstSim, cCodCli, cLojCli, lProspect, lPrdlSim)
Local cQuery		:= ""
Local cAliasTop		:= ""
Local lRet			:= .F.
Local aSB1			:= SB1->(GetArea())
Local aArea			:= GetArea()
Local cTamRef		:= Substr(GetMv("MV_MASCGRD"), 1, 2)
Local oCalcRnt		:= Nil
Local nRentStd		:= 0
Local aPrc			:= {}
Local lBlqCA		:= .F.
Local nMulVen		:= 0

SB1->(DBSetOrder(1))
If !Empty(cProduto) .And. SB1->(DBSeek(xFilial("SB1") + cProduto))
	cQuery += "SELECT  	ZFA.ZFA_ID		IDLISTA" + CRLF
	cQuery += ",		ZFA.ZFA_PROD	SKU" + CRLF
	cQuery += ",		ZFA.ZFA_DESTAQ	DTQ" + CRLF
	cQuery += ",		SB1.B1_DESC		DESCRI" + CRLF
	cQuery += ",		SB1.B1_UM		UMED" + CRLF
	cQuery += ",		SB1.B1_PRV1		PRECO1" + CRLF
	cQuery += ",		SB1.B1_PV2		PRECO2" + CRLF
	cQuery += ",		ISNULL(SB2.B2_QATU - SB2.B2_QPEDVEN - SB2.B2_RESERVA, 0) DISPO" + CRLF
	cQuery += ",		0				RENTA" + CRLF
	cQuery += ",		SB5.B5_MARCA	MARCA" + CRLF
	cQuery += ",		SB1.B1_CA		CA" + CRLF
	cQuery += ",		SBZ.BZ_XSEGMEN	SEGMEN" + CRLF
	cQuery += ",		SB1.B1_XMULTGV	MULTGV" + CRLF
	cQuery += ",		SB1.B1_XSTATUS	XSTATUS" + CRLF

	cQuery += "FROM	(	SELECT   ZFA_ID" + CRLF
	cQuery += "			,		ZFA_PROD" + CRLF
	cQuery += "			,		ZFA_FILIAL" + CRLF
	cQuery += "			FROM	" + RetSqlName("ZFA") + " ZFA (NOLOCK)" + CRLF
	cQuery += "			WHERE   ZFA.D_E_L_E_T_ = ''" + CRLF
	cQuery += "			AND		ZFA.ZFA_FILIAL = ''" + CRLF
	cQuery += "			AND		ZFA.ZFA_PROD = '" + cProduto + "') A" + CRLF

	cQuery += "			INNER JOIN" + CRLF
	cQuery += "			" + RetSqlName("ZFA") + " ZFA (NOLOCK)" + CRLF
	cQuery += "			ON		ZFA.D_E_L_E_T_ = ''" + CRLF
	cQuery += "			AND		ZFA.ZFA_FILIAL = A.ZFA_FILIAL" + CRLF
	cQuery += "			AND		ZFA.ZFA_ID = A.ZFA_ID" + CRLF
	cQuery += "			AND		LEFT(ZFA.ZFA_PROD, " + cTamRef + ") <> LEFT(A.ZFA_PROD, " + cTamRef + ")" + CRLF

	cQuery += "			INNER JOIN " + CRLF
	cQuery += "			" + RetSqlName("SB1") + " SB1 (NOLOCK)" + CRLF
	cQuery += "			ON SB1.D_E_L_E_T_ = ''" + CRLF
	cQuery += "			AND SB1.B1_FILIAL = '" + xFilial("SB1") + "'" + CRLF
	cQuery += "			AND SB1.B1_COD = ZFA.ZFA_PROD" + CRLF

	cQuery += "			INNER JOIN " + CRLF
	cQuery += "			" + RetSqlName("SB5") + " SB5 (NOLOCK)" + CRLF
	cQuery += "			ON SB5.D_E_L_E_T_ = ''" + CRLF
	cQuery += "			AND SB5.B5_FILIAL = SB1.B1_FILIAL" + CRLF
	cQuery += "			AND SB5.B5_COD = SB1.B1_COD" + CRLF

	cQuery += "			LEFT JOIN " + CRLF
	cQuery += "			" + RetSqlName("SB2") + " SB2 (NOLOCK)" + CRLF
	cQuery += "			ON SB2.D_E_L_E_T_ = ''" + CRLF
	cQuery += "			AND SB2.B2_FILIAL = '" + xFilial("SB2") + "'" + CRLF
	cQuery += "			AND SB2.B2_COD = SB1.B1_COD" + CRLF
	cQuery += "			AND SB2.B2_LOCAL = '" + ERP_LOC_VENDAS + "'" + CRLF

	cQuery += "			LEFT JOIN " + CRLF
	cQuery += "			" + RetSqlName("SBZ") + " SBZ (NOLOCK)" + CRLF
	cQuery += "			ON SBZ.D_E_L_E_T_ = ''" + CRLF
	cQuery += "			AND SBZ.BZ_FILIAL = SB2.B2_FILIAL" + CRLF
	cQuery += "			AND SBZ.BZ_COD = SB2.B2_COD" + CRLF
	cQuery += "			AND SBZ.BZ_LOCPAD = SB2.B2_LOCAL" + CRLF

	cQuery += "WHERE   SB1.B1_MSBLQL <> '1'" + CRLF
	cQuery += "AND		((SB5.B5_XSEGMEN IN ('E') AND ('" + Dtos(dDatabase) + "' BETWEEN ZFA.ZFA_DTINIE AND ZFA.ZFA_DTFIME))" + CRLF
	cQuery += "OR		(SB5.B5_XSEGMEN NOT IN ('E')))" + CRLF
	cQuery += "ORDER BY SB1.B1_PRV1 DESC" + CRLF

	cAliasTop := MPSysOpenQuery(cQuery)

	If (cAliasTop)->(!EOF())
		lPrdlSim	:= .T.
		lRet		:= .T.

		While (cAliasTop)->(!EOF())
			lBlqCA := U_fVldCA((cAliasTop)->SKU, .F.)
			//Pega o preço de lista
			aPrc  := U_ProdPrv((cAliasTop)->SKU, cCodCli, cLojCli, lProspect)
			//Calcula rentabilidade real.
			oCalcRnt := RntXCalc():New(cCodCli, cLojCli, lProspect, (cAliasTop)->SKU, "01", aPrc[1], 1, aPrc[1])
			If oCalcRnt:Calcular()
				nRentStd := oCalcRnt:GetRntStd()
			Else
				nRentStd := 0
			EndIf

			nMulVen	   := u_fValMul((cAliasTop)->SKU, cCodCli, cLojCli, .T., .F.)[1]

			If nMulVen == 0
				nMulVen := IIF((cAliasTop)->MULTGV > 0, (cAliasTop)->MULTGV, 0)
			EndIf

			//aCabSim := {"Foco","SKU", "Descrição", "Un.Med.", "P1", "P2", "Renta", "Marca", "CA", "DRE", "Múltiplo", "Sld.Disp.", "Status BZL"}
			AAdd(aLstSim,	{(cAliasTop)->DTQ,;								//01 - Foco
							(cAliasTop)->SKU,;								//02 - SKU
							AllTrim((cAliasTop)->DESCRI),;					//03 - Descricao
							(cAliasTop)->UMED,;								//04 - Un.Med.
							aPrc[1],;										//05 - P1
							aPrc[2],;										//06 - P2
							nRentStd,;										//07 - Renta
							AllTrim((cAliasTop)->MARCA),;					//08 - Marca
							(cAliasTop)->CA,;								//09 - CA
							X3Combo("BZ_XSEGMEN", (cAliasTop)->SEGMEN),;	//10 - DRE
							nMulVen,;										//11 - Multiplo
							(cAliasTop)->DISPO,;							//12 - Sld.Disp.
							X3Combo("B1_XSTATUS", (cAliasTop)->XSTATUS),;	//13 - Status BZL
							lBlqCA})										//14 - Blq. CA

			(cAliasTop)->(DBSkip())
		EndDo
	Else
			AAdd(aLstSim,	{"",;		//01 - Foco
							"",;		//02 - SKU
							"",;		//03 - Descricao
							"",;		//04 - Un.Med.
							0,;			//05 - P1
							0,;			//06 - P2
							0,;			//07 - Renta
							"",;		//08 - Marca
							"",;		//09 - CA
							"",;		//10 - DRE
							0,;			//11 - Multiplo
							0,;			//12 - Sld.Disp.
							"",;		//13 - Status BZL
							.F.})		//14 - Blq. CA

	EndIf
	(cAliasTop)->(DBCloseArea())
Else
	AAdd(aLstSim,	{"",;		//01 - Foco
					"",;		//02 - SKU
					"",;		//03 - Descricao
					"",;		//04 - Un.Med.
					0,;			//05 - P1
					0,;			//06 - P2
					0,;			//07 - Renta
					"",;		//08 - Marca
					"",;		//09 - CA
					"",;		//10 - DRE
					0,;			//11 - Multiplo
					0,;			//12 - Sld.Disp.
					"",;		//13 - Status BZL
					.F.})		//14 - Blq. CA
EndIf

FreeObj(oCalcRnt)
oCalcRnt := Nil
RestArea(aSB1)
RestArea(aArea)

Return lRet

//-------------------------------------------------------------------
/*/{Protheus.doc} xCartDados
Busca dados da carteira do cliente.
@author  João Leão
@since   31/03/2022
@version 12.1.27
/*/
//-------------------------------------------------------------------
User Function xCartDados(cCodCli, cLojCli, lProspect, cCodSU7, cCarteira, cOpeCart, cDepVen)

Local aArea			:= GetArea()
Local aSU7			:= SU7->(GetArea())
Local aZA7			:= ZA7->(GetArea())
Local aDepVen		:= {}
Local nPosDepVen	:= 0

//Preenche o array com os departamentos de venda
If !lProspect
	aDepVen := U_DepVen(cCodCli, cLojCli, 1)
Endif

If Len(aDepVen) > 1
	If !Empty(cDepVen)
		nPosDepVen := aScan(aDepven,{|x| SubStr(x,1,2) = cDepVen})
		if nPosDepVen > 0
			cDepVen := aDepven[nPosDepVen]
		Endif
	Endif

	If empty(cDepVen)
		nPosDepVen := aScan(aDepven,{|x| SubStr(x,1,2) = "01"})
		If nPosDepVen <> 0
			cDepVen := aDepven[nPosDepVen]
		ElseIf Len(aDepVen) == 2
			cDepVen := aDepven[2]
		Else
			cDepVen := "  "
		EndIf
	EndIf
Else //cliente sem carteira. Salva o departamento de quem digita
	cDepVen := "01"
	aAdd(aDepVen, "01" + ' - ' + AllTrim(Posicione("SU7",1,xFilial("SU7") + cCodSU7,"U7_NOME")))
EndIf

cDepVen := Left(cDepVen, 2)
//Localizar a carteira e preencher as variáveis
If ! lProspect
	ZA7->(DBSetOrder(7)) //ZA7_FILIAL, ZA7_DEPVEN, ZA7_CLIENT, ZA7_LOJA
	If !Empty(cDepVen) .And. ZA7->(DBSeek(xFilial("ZA7") + cDepVen + cCodCli + cLojCli))
		cCarteira	:= ZA7->ZA7_CART
		cOpeCart	:= ZA7->ZA7_OPERAD
	Else
		cCarteira	:= ""
		cOpeCart	:= ""
	EndIf
Else
	cCarteira	:= ""
	cOpeCart	:= ""
EndIf

RestArea(aSU7)
RestArea(aZA7)
RestArea(aArea)

Return Nil

//-------------------------------------------------------------------
/*/{Protheus.doc} EncripArq
Retorna um arquivo encriptado numa string em formato JSON
@author  João Leão
@since   16/05/2022
@version 12.1.27
/*/
//-------------------------------------------------------------------
User Function EncriptArq(cArquivo)

Local oFile			:= Nil
Local cFileConteu	:= ""
Local cFileEncode	:= ""
Local cDados		:= ""

If File(cArquivo)
    //Tenta abrir o arquivo
    oFile   := FwFileReader():New(cArquivo)
    If oFile:Open()
        cFileConteu     := oFile:FullRead()
        cFileEncode     := Encode64(cFileConteu,, .F., .F.)

        //Define o retorno do webservice
        cDados := '{' + CRLF
        cDados += '	"Arquivo":"' + cFileEncode + '"' + CRLF
        cDados += '}'
    Else
        cDados := '{' + CRLF
        cDados += '	"Arquivo":"Falha na tentativa de ler arquivo de origem."' + CRLF
        cDados += '}'
    EndIf

	FreeObj(oFile)
EndIf

Return cDados
//-------------------------------------------------------------------
/*/{Protheus.doc} PesqSku
Retorna primeiro SKU para cálculo dos impostos

@author  Victor Dessunte
@since   23/09/2022
@version 12.1.33
/*/
//-------------------------------------------------------------------
User Function PesqSku(cProd)
Local lXGrvLog := U_XGRVLOG("FMATXFUN", "U_PESQSKU")

Local aArea := GetArea()
Local cSku  := ""

Default cProd := ""

SB1->(dbSetOrder(1))  // B1_FILIAL, B1_COD.
If Len(ALLTRIM(cProd)) > 10
	SB1->(msSeek(xFilial() + cProd))
Else
	SB1->(msSeek(xFilial() + left(cProd, nTamRef), .F.))
	Do While SB1->(!eof() .and. B1_FILIAL + B1_COD = xFilial() + left(cProd, nTamRef) .and. B1_MSBLQL == "1")
		SB1->(dbSkip())
	EndDo
EndIf

cSku := SB1->B1_COD

RestArea(aArea)

Return cSku
//-------------------------------------------------------------------
/*/{Protheus.doc} fGetOrig
Retorna a Origem de inclusao do Orcamento ou Pedido

@author  Guilherme Santos
@since   03/05/2023
@version 12.1.33
/*/
//-------------------------------------------------------------------
User Function fGetOrig(lIntEDI)
	Local cTabQry	:= ""
	Local cQuery	:= ""
	Local cPilha	:= ""
	Local cTabela	:= "Z3"
	Local cRetorno	:= ""

	Default lIntEDI := .F.

	If lIntEDI
		cRetorno := ZZ0->ZZ0_XDTA
	Else
		cPilha	:= U_fGetProc()

		cQuery += "SELECT	SX5.X5_TABELA" + CRLF
		cQuery += ",		SX5.X5_CHAVE" + CRLF
		cQuery += ",		SX5.X5_DESCRI" + CRLF
		cQuery += "FROM		" + RetSqlName("SX5") + " SX5" + CRLF
		cQuery += "WHERE	SX5.X5_FILIAL = '00'" + CRLF		//Tabela incluida somente na Filial GRU
		cQuery += "AND		SX5.X5_TABELA = '" + cTabela + "'" + CRLF
		cQuery += "AND		SX5.X5_DESCSPA IN " + cPilha + CRLF
		cQuery += "AND		SX5.D_E_L_E_T_ = ''" + CRLF

		cTabQry := MPSysOpenQuery(cQuery)

		If !(cTabQry)->(Eof())
			cRetorno := AllTrim((cTabQry)->X5_CHAVE)
		EndIf

		If Select(cTabQry) > 0
			(cTabQry)->(DbCloseArea())
		EndIf
	EndIf

	If Empty(cRetorno)
		cRetorno := "999"		//Origem nao cadastrada
	EndIf

Return cRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} fGetProc
Retorna a pilha de chamadas para Verificar a origem

@author  Guilherme Santos
@since   03/05/2023
@version 12.1.33
/*/
//-------------------------------------------------------------------
User Function fGetProc()
	Local cFuncao	:= ""
	Local cRetorno 	:= ""
	Local nI		:= 0
	Local lContinua := .T.

	While lContinua
		cFuncao := ProcName(nI)

		If Empty(cFuncao)
			lContinua := .F.
		Else
			If "U_" $ cFuncao
				If Empty(cRetorno)
					cRetorno += "("
				Else
					cRetorno += ","
				EndIf

				cRetorno += "'" + cFuncao + "'"
			EndIf
		EndIf

		nI++
	End

	If !Empty(cRetorno)
		cRetorno += ")"
	EndIf

Return cRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} fPrevTransp
Retorna a previso de entrega baseado no prazo da transportadora
@author  Aline Vale
@since   02/08/2023
@version 12.1.22.10
/*/
//-------------------------------------------------------------------
User Function PrevTransp(cFilPed,cPedido,nPosicao)
	Local dDataEntr := ctod(space(8))
	Local nSomaLead	:= 2 //calculo Lead Time em dias teis + 2
	Local nPrazo	:= 0
	Default nPosicao := 1 // 1=inicial C5_XDTPFAT, 2=Atualizada C5_XFATBZL

	if SC5->C5_FILIAL+C5_NUM <> cFilPed+cPedido .and. !(SC5->(eof())) //posiciona o pedido
		SC5->(msSeek(cFilPed,cPedido))
	endif
	nPrazo := RetPrzCalc(SC5->C5_XEST, SC5->C5_XCDMUNE, SC5->C5_TRANSP)
	dDataEntr := if(nPosicao == 1, SC5->C5_XDTPFAT, SC5->C5_XFATBZL)
	dDataEntr := U_SumDtWork(dDataEntr, nPrazo + nSomaLead)
Return dDataEntr//-----------------------------------------------------------
/*/{Protheus.doc} fGetProc
Retorna a aliquota dos impotos utilizado para calcular campos da AIB na API desbloqueio e
gatilhos na AIB pelo motor fiscal

@author  Tanimoto
@since   28/11/2023
@version 12.1.2210
/*/
//-----------------------------------------------------------
User Function fMatFisAIB(cFornece,cLoja,cProduto,cTpOper,nPRCCOM,nAICMMat)
	Local lTESInt   := SuperGetMV("PC_TESINT",, .F.)
	Local cUFFor	:= ""
	Local cTipoFor	:= ""
	Local cCodTes	:= ""
	Local nAlqICMS 	:= 0
	Local nAlqIPI	:= 0
	Local nBASEICM 	:= 0
	Local aExcecao	:= {}
	Local nAlqRedu 	:= 0
	Local nPrcGross := 0
	Local aRet		:= {}
	Local nTxPIS	:= SuperGetMV("MV_TXPIS")
	Local nTxCofins	:= SuperGetMV("MV_TXCOFIN")

	Default cTpOper := "50"
	Default nAICMMat:= 0

	cTipoFor    := Posicione("SA2",1,xFilial("SA2")+cFornece+cLoja,"A2_TIPO")

	If lTESInt
  	    cCodTes := MaTesInt(1, cTpOper, cFornece, cLoja, "F", cProduto)
	Else
		cUFFor	:= Posicione("SA2",1,xFilial("SA2")+cFornece+cLoja,"A2_EST")
	    cCodTes := U_RetTes("N", "E", cFornece , cLoja, cProduto, "CPR", cUFFor, cFornece , cLoja)
	Endif

	//---- Inicio a funcao MAFIS para calcular o total dos impostos
	MaFisIni(cFornece,cLoja,"F","N",cTipoFor,MaFisRelImp("MT100",{"SF1","SD1"}),,,"SB1","MATA103")
	MaFisAdd(cProduto,cCodTes,1,100,0,Space(9),Space(3),0,0,0,0,0,100,0,0,0)
	nAlqICMS := MaFisRet(1,"IT_ALIQICM")
	nAlqIPI	 := SB1->B1_XIPIF//(1,"IT_ALIQIPI")
	nBASEICM := MaFisRet(1,"IT_BASEICM")
	aExcecao := MaFisRet(1,"IT_EXCECAO") //Array da EXCECAO Fiscal
	nAlqRedu := Round(nAlqICMS*(nBASEICM/100),2)
	nBaseSol := MaFisRet(,"NF_BASESOL")
	nAliqSol := MaFisRet(1,"IT_ALIQSOL")
	nValSol  := MaFisRet(,"NF_VALSOL")

	If nAlqRedu < nAlqICMS
		nPrcGross:= Round(((nPRCCOM  /((100-(nTxPIS + nTxCofins))/100))/((100-nAlqRedu)/100)),2)
	Else
		nPrcGross:= Round(((nPRCCOM  /((100-(nTxPIS + nTxCofins))/100))/((100-nAlqICMS)/100)),2)
	EndIf

	MaFisEnd()
	//---- Finaliza funcao MAFIS

	aAdd(aRet,{nPrcGross,nAlqICMS,nAlqIPI,nTxPIS,nTxCofins,nBASEICM,nAlqRedu,nBaseSol,nAliqSol,nValSol}) // aAdd(aRet,{nPrcGross,nAlqICMS,nAlqIPI})

Return aRet

//-------------------------------------------------------------------
/*/{Protheus.doc} fMatFisPre
Retorna a aliquota dos impostos para Analise de Pricing
@author  Eduardo Solorzano Rodriguez
@since   01/04/2024
@version 12.1.2210
/*/
//-------------------------------------------------------------------
User Function fMatFisPre(cFornece,cLoja,cProduto,cTpOper,nPRCCOM,nAICMMat)
	Local lTESInt   := SuperGetMV("PC_TESINT",, .F.)
	Local cUFFor	:= ""
	Local cTipoFor	:= ""
	Local cCodTes	:= ""
	Local nAlqICMS 	:= 0
	Local nAlqIPI	:= 0
	Local nBASEICM 	:= 0
	Local aExcecao	:= {}
	Local nAlqRedu 	:= 0
	Local nPrcGross := 0
	Local aRet		:= {}
	Local nTxPIS	:= SuperGetMV("MV_TXPIS")
	Local nTxCofins	:= SuperGetMV("MV_TXCOFIN")
	Local nBaseSol 	:= 0
	Local nAliqSol 	:= 0
	Local nValSol  	:= 0

	Default cTpOper := "50"
	Default nAICMMat:= 0

	cTipoFor    := Posicione("SA2",1,xFilial("SA2")+cFornece+cLoja,"A2_TIPO")

	If lTESInt
  	    cCodTes := MaTesInt(1, cTpOper, cFornece, cLoja, "F", cProduto)
	Else
		cUFFor	:= Posicione("SA2",1,xFilial("SA2")+cFornece+cLoja,"A2_EST")
	    cCodTes := U_RetTes("N", "E", cFornece , cLoja, cProduto, "CPR", cUFFor, cFornece , cLoja)
	Endif

	//---- Inicio a funcao MAFIS para calcular o total dos impostos
	MaFisIni(cFornece,cLoja,"F","N",cTipoFor,MaFisRelImp("MT100",{"SF1","SD1"}),,,"SB1","MATA103")
	MaFisAdd(cProduto,cCodTes,1,100,0,Space(9),Space(3),0,0,0,0,0,100,0,0,0)
	nAlqICMS := MaFisRet(1,"IT_ALIQICM")
	nAlqIPI	 := SB1->B1_XIPIF//(1,"IT_ALIQIPI")
	nBASEICM := MaFisRet(1,"IT_BASEICM")
	aExcecao := MaFisRet(1,"IT_EXCECAO") //Array da EXCECAO Fiscal
	nAlqRedu := Round(nAlqICMS*(nBASEICM/100),2)
	nBaseSol := MaFisRet(,"NF_BASESOL")
	nAliqSol := MaFisRet(1,"IT_ALIQSOL")
	nValSol  := MaFisRet(,"NF_VALSOL")

	//nPrcGross:= Round(((nPRCCOM  /((100-(nTxPIS + nTxCofins))/100))/((100-nAlqICMS)/100)),2)

	If nAlqRedu < nAlqICMS
		nPrcGross:= Round(((nPRCCOM  /((100-(nTxPIS + nTxCofins))/100))/((100-nAlqRedu)/100)),2)
	Else
		nPrcGross:= Round(((nPRCCOM  /((100-(nTxPIS + nTxCofins))/100))/((100-nAlqICMS)/100)),2)
	EndIf

	MaFisEnd()
	//---- Finaliza funcao MAFIS

	aAdd(aRet,{nPrcGross,nAlqICMS,nAlqIPI,nTxPIS,nTxCofins,nBASEICM,nAlqRedu,nBaseSol,nAliqSol,nValSol}) //aAdd(aRet,{nPrcGross,nAlqICMS,nAlqIPI,nTxPIS,nTxCofins,nBASEICM,nAlqRedu})

Return aRet
//-------------------------------------------------------------------
/*/{Protheus.doc} BZLGATTR
Valida a Execucao do Gatilho da Transportadora na SUA

@author  Guilherme Santos
@since   10/09/2024
@version 12.1.2310
/*/
//-------------------------------------------------------------------
User Function BZLGATTR(cParTipo)
	Local cTpFrete	:= M->UA_TPFRETE
	Local lPROMA030	:= IsInCallStack("U_PROMA030")
	Local lRetorno 	:= lPROMA030 .AND. cTpFrete == cParTipo
Return lRetorno



