#INCLUDE "PROTHEUS.CH"
#INCLUDE "MSOBJECT.CH"
//-------------------------------------------------------------------
/*/{Protheus.doc} RNTXFUN
Fonte para Calculo de Rentabilidade do Produto

@author  Guilherme Santos
@since   20/02/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
User Function RNTXFUN()
Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} RntXCalc
Classe para Calculo de Rentabilidade do Produto

@author  Guilherme Santos
@since   20/02/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
Class RntXCalc
	Data cCliente
	Data cLojaCli
	Data lProspect
	Data cProduto
	Data cArmazem
	Data nPreco
	Data nQuant
	Data lCalculado

	Data nCstReal
	Data nCstStd
	Data nICMS
	Data nRedICM
	Data nValICM
	Data nAlqPIS
	Data nValPIS
	Data nAlqCOF
	Data nValCOF
	Data nValICD
	Data nValICC
	Data nValor
	Data nPrcLiq
	Data nRntSTD
	Data nRntReal
	Data cICMSST
	Data nAlqIPI
	Data nValIPI
	Data nRntP1
	Data nPrcP1
	Data nLiqP1
	Data nTotP1
	Data nICMSP1
	Data nCOFP1
	Data nPISP1
	Data nRegSB1
	Data nRegSF4
	Data nTamRef
	Data nAliqPCC
	Data nRndRnt
	Data nRndCst
	Data cTes
	Data nBasST
	Data nAlqST
	Data nVlrST
	Data nTotST

	Method New(cCliente, cLojaCli, lProspect, cProduto, cArmazem, nPreco, nQuant, nPrcP1) Constructor
	Method Init(cCliente, cLojaCli, lProspect, cProduto, cArmazem, nPreco, nQuant, nPrcP1)
	Method CheckCalc(cCliente, cLojaCli, lProspect, cProduto, cArmazem, nPreco, nQuant, nPrcP1)
	Method GetKey()
	Method Calcular()
	Method GetCliente()
	Method GetLojaCli()
	Method SetCstReal()
	Method GetCstReal()
	Method SetCstStd()
	Method GetCstStd()
	Method GetAlqICMS()
	Method GetRedICMS()
	Method GetValICMS()
	Method GetAlqPIS()
	Method GetValPIS()
	Method GetAlqCOF()
	Method GetValCOF()
	Method GetValICD()
	Method GetValICC()
	Method GetPrcLiq()
	Method GetRntSTD()
	Method GetRntReal()
	Method GetICMSST()
	Method GetAlqIPI()
	Method GetValIPI()
	Method GetRntP1()
	Method GetLiqP1()
	Method GetTes()
	Method GetBasST()
	Method GetAlqST()
	Method GetVlrST()
	Method GetTotST()
EndClass
//-------------------------------------------------------------------
/*/{Protheus.doc} New
Inicializa o Objeto

@author  Guilherme Santos
@since   20/02/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
Method New(cCliente, cLojaCli, lProspect, cProduto, cArmazem, nPreco, nQuant, nPrcP1) Class RntXCalc
	::Init(cCliente, cLojaCli, lProspect, cProduto, cArmazem, nPreco, nQuant, nPrcP1)
Return Self
//-------------------------------------------------------------------
/*/{Protheus.doc} Init
Inicializa o Objeto

@author  Guilherme Santos
@since   20/02/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
Method Init(cCliente, cLojaCli, lProspect, cProduto, cArmazem, nPreco, nQuant, nPrcP1) Class RntXCalc
	::nRndRnt		:= SuperGetMV("BZ_RNDRNT", NIL, 2)
	::nRndCst		:= SuperGetMV("BZ_RNDCST", NIL, 8)
	::cCliente 		:= cCliente
	::cLojaCli 		:= cLojaCli
	::lProspect		:= lProspect
	::cProduto 		:= cProduto
	::cArmazem 		:= cArmazem
	::nPreco 		:= nPreco
	::nQuant 		:= If(nQuant == 0, 1, nQuant)
	::lCalculado	:= .F.
	::nCstReal		:= 0										//Custo Real
	::nCstStd		:= 0										//Custo Standard
	::nICMS			:= 0										//Aliq. ICMS
	::nRedICM		:= 0										//Perc. Red. ICMS
	::nValICM		:= 0										//Valor ICMS
	::nAlqPIS		:= 0										//Aliq. PIS
	::nValPIS		:= 0										//Valor PIS
	::nAlqCOF		:= 0										//Aliq. COFINS
	::nValCOF		:= 0										//Valor COFINS
	::nValICD   	:= 0										//Valor Difal + Valor Fundo de Combate a pobreza
	::nValICC   	:= 0										//Valor ICMS Complementar
	::nValor		:= Round(::nQuant * ::nPreco, ::nRndCst)	//Valor Total do Item
	::nPrcLiq		:= 0										//Preco Liquido do Produto
	::nRntSTD		:= 0										//Rentabilidade Standard
	::nRntReal		:= 0										//Rentabilidade Real
	::cICMSST		:= ""										//ICMS ST?
	::nAlqIPI		:= 0										//Aliq. IPI
	::nValIPI		:= 0										//Valor IPI
	::nPrcP1		:= nPrcP1									//Preco de Tabela
	::nLiqP1		:= 0										//Preco Liquido de Tabela
	::nTotP1		:= Round(::nQuant * ::nPrcP1, ::nRndCst)	//Total P1
	::nICMSP1		:= 0										//Valor ICMS P1
	::nCOFP1		:= 0										//Valor COFINS P1
	::nPISP1		:= 0										//Valor PIS P1
	::nRntP1		:= 0										//Rentabilidade Preco de Tabela
	::nRegSB1		:= 0										//Recno do SB1
	::nRegSF4		:= 0										//Recno do SB1
	::nTamRef   	:= Val(Substr(GetMv("MV_MASCGRD"), 1, 2))
	::nAliqPCC		:= SuperGetMV("MV_TXPIS") + SuperGetMV("MV_TXCOFIN")
	::cTes			:= ""
	::nBasST		:= 0
	::nAlqST		:= 0
	::nVlrST		:= 0
	::nTotST		:= 0
Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} CheckCalc
Retorna se o produto e o mesmo e se ja calculou a Rentabilidade

@author  Guilherme Santos
@since   20/02/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
Method CheckCalc(cCliente, cLojaCli, lProspect, cProduto, cArmazem, nPreco, nQuant, nPrcP1) Class RntXCalc
	Local lRetorno := .F.

	If Empty(cLojaCli)
		If lProspect
			DbSelectArea("SUS")
			DbSetOrder(1)		//US_FILIAL, US_COD, US_LOJA

			If SUS->(DbSeek(xFilial("SUS") + cCliente + RTrim(cLojaCli)))
				cLojaCli := SUS->US_LOJA
			EndIf
		Else
			DbSelectArea("SA1")
			DbSetOrder(1)		//A1_FILIAL, A1_COD, A1_LOJA

			If SA1->(DbSeek(xFilial("SA1") + cCliente + RTrim(cLojaCli)))
				cLojaCli := SA1->A1_LOJA
			EndIf
		EndIf
	EndIf

	If ::lCalculado .AND. cCliente == ::cCliente .AND. cLojaCli == ::cLojaCli .AND. lProspect == ::lProspect .AND. cProduto == ::cProduto .AND. cArmazem == ::cArmazem .AND. nPreco == ::nPreco .AND. nQuant == ::nQuant .AND. nPrcP1 == ::nPrcP1
		lRetorno := .T.
	Else
		::Init(cCliente, cLojaCli, lProspect, cProduto, cArmazem, nPreco, nQuant, nPrcP1)
		lRetorno := ::Calcular()
	EndIf

Return lRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} GetKey
Retorna a Chave do Objeto

@author  Guilherme Santos
@since   10/05/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
Method GetKey() Class RntXCalc
	Local cRetorno := ::cCliente + ::cLojaCli + If(::lProspect, "T", "F") + ::cProduto + ::cArmazem
Return cRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} Calcular
Executa o Calculo da Rentabilidade do Produto

@author  Guilherme Santos
@since   20/02/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
Method Calcular() Class RntXCalc
	Local aArea			:= GetArea()
	Local aAreaSB1		:= SB1->(GetArea())
	Local aAreaSB2		:= SB2->(GetArea())
	Local aAreaSUS		:= SUS->(GetArea())
	Local aAreaSA1		:= SA1->(GetArea())
	Local aAreaSF4		:= SF4->(GetArea())
	Local cMunSuf		:= ""
	Local cEstEnt		:= ""
	Local cMV_ESTADO	:= GetMV("MV_ESTADO")
	Local cTipoCli		:= ""
	Local cTpVenda 		:= ""
	Local lRestore		:= .F.
	Local lSuframa		:= .F.
	Local lTESInt		:= SuperGetMV("PC_TESINT",, .F.)
	Local lRetorno 		:= .T.
	Local lContrib		:= .T.

	::cTes := ''

	If ::lProspect
		DbSelectArea("SUS")
		DbSetOrder(1)		//US_FILIAL, US_COD, US_LOJA

		If SUS->(DbSeek(xFilial("SUS") + ::cCliente + RTrim(::cLojaCli)))
			cEstEnt 	:= SUS->US_EST
			cTipoCli	:= SUS->US_TIPO
			lContrib	:= SUS->US_CONTRIB == "1"

			If Empty(RTrim(::cLojaCli))
				::cLojaCli := SUS->US_LOJA
			EndIf
		Else
			lRetorno 	:= .F.
		EndIf
	Else
		DbSelectArea("SA1")
		DbSetOrder(1)		//A1_FILIAL, A1_COD, A1_LOJA

		If SA1->(DbSeek(xFilial("SA1") + ::cCliente + RTrim(::cLojaCli)))
			cEstEnt 	:= If(empty(SA1->A1_ESTE), SA1->A1_EST, SA1->A1_ESTE)
			cMunSuf		:= SA1->A1_CODMUN
			cTipoCli	:= SA1->A1_TIPO
			lSuframa	:= !Empty(SA1->A1_SUFRAMA)
			lContrib	:= SA1->A1_CONTRIB == "1"

			If Empty(RTrim(::cLojaCli))
				::cLojaCli := SA1->A1_LOJA
			EndIf
		Else
			lRetorno 	:= .F.
		EndIf
	EndIf

	If lRetorno
		lRetorno := ::SetCstStd()
	EndIf

	If lRetorno
		lRetorno := ::SetCstReal()
	EndIf

	If lRetorno
		If lTESInt .AND. !::lProspect
			::cTES := MaTesInt(2, "01", ::cCliente, ::cLojaCli, "C", ::cProduto)
		Else
			If !::lProspect .and. cEstEnt <> cMV_ESTADO .and. lSuframa .AND. Empty(FWSM0Util():GetSM0Data(cEmpAnt, cFilAnt, {"M0_INS_SUF"})[1][2])
				cTpVenda := If(Empty(cMunSuf), "VAO", "VZF")
			Else
				cTpVenda := "VNO"
			Endif

			::cTES := U_RetTes("N", "S", cTipoCli, cTipoCli, ::cProduto, cTpVenda, cEstEnt, ::cCliente, ::cLojaCli)
		Endif

		If Empty(::cTES)
			lRetorno := .F.

			Help("", 1, "Help","RNTXFUN" ,"Produto " + AllTrim(::cProduto) + " sem amarração fiscal. Entre em contato com o Departamento Fiscal.",3,0)
		EndIf
	EndIf

	If lRetorno
		DbSelectArea("SF4")
		DbSetOrder(1)		//F4_FILIAL, F4_CODIGO

		If SF4->(DbSeek(xFilial("SF4") + ::cTES))
			::nRegSF4 := SF4->(RecNo())

			If SF4->F4_SITTRIB == "10" .OR. SF4->F4_SITTRIB == "60"
				::cICMSST := "1"	//1=Sim
			Else
				::cICMSST := "2"	//2=Nao
			EndIf
		Else
			lRetorno := .F.
		EndIf
	EndIf

	If lRetorno
		If MaFisFound()
			MaFisSave()
			MaFisEnd()
			lRestore := .T.
		EndIf

		//Inicializa a Funcao Fiscal
		MaFisIni(::cCliente, ::cLojaCli, "C", "N", cTipoCli, NIL, NIL, NIL, NIL, "MATA461", NIL, NIL, If(::lProspect, ::cCliente + ::cLojaCli, ""))

		//Adiciona o Produto para o Calculo
		MaFisAdd(::cProduto, ::cTES, ::nQuant, ::nPreco, 0, "", "", 0, 0, 0, 0, 0, ::nValor, 0, ::nRegSB1, ::nRegSF4)

		::nICMS			:= If(MaFisRet(1, "IT_BASEDES") > 0 .AND. !lContrib, Max(MaFisRet(1, "IT_ALIQICM"), MaFisRet(1, "IT_ALFCCMP") + MaFisRet(1, "IT_ALIQCMP")), MaFisRet(1, "IT_ALIQICM"))
		::nRedICM 		:= MaFisRet(1, "IT_PREDIC")
		::nValICM 		:= MaFisRet(1, "IT_VALICM")
		::nAlqPIS 		:= MaFisRet(1, "IT_ALIQPS2")
		::nValPIS 		:= MaFisRet(1, "IT_VALPS2")
		::nAlqCOF 		:= MaFisRet(1, "IT_ALIQCF2")
		::nValCOF 		:= MaFisRet(1, "IT_VALCF2")
		::nAlqIPI		:= MaFisRet(1, "IT_ALIQIPI")
		::nValIPI		:= MaFisRet(1, "IT_VALIPI")
		::nValICD   	:= MaFisRet(1, "IT_DIFAL") + MaFisRet(1, "IT_VFCPDIF")		//Valor Difal + Valor Fundo de Combate a pobreza
		::nValICC   	:= MaFisRet(1, "IT_VALCMP")		//Valor ICMS Complementar
		::nBasST		:= MaFisRet(1, "IT_BASESOL")
		::nAlqST		:= If(MaFisRet(1, "IT_VALSOL") > 0, MaFisRet(1, "IT_ALIQSOL"), 0)
		::nVlrST		:= NoRound(MaFisRet(1, "IT_VALSOL") / ::nQuant, 2)
		::nTotST		:= MaFisRet(1, "IT_VALSOL")

		MaFisEnd()

		//Preco Liquido sem impostos sobre o Preco da Venda
		::nPrcLiq 		:= ::nValor - ::nValICM - ::nValPIS - ::nValCOF - ::nValICD - ::nValICC

		//Rentabilidade Standard sobre o Preco de Venda
		::nRntSTD		:= Round((1 - (::nCstStd / ::nPrcLiq)) * 100, ::nRndRnt)

		//Rentabilidade Real sobre o Preco da Venda
		::nRntReal		:= If(::nPrcLiq == 0, 0, Round((1 - (::nCstReal / ::nPrcLiq)) * 100, ::nRndRnt))

		If ::nPrcP1 > 0
			//Inicializa a Funcao Fiscal
			MaFisIni(::cCliente, ::cLojaCli, "C", "N", cTipoCli, NIL, NIL, NIL, NIL, "MATA461", NIL, NIL, If(::lProspect, ::cCliente + ::cLojaCli, ""))

			//Adiciona o Produto para o Calculo
			MaFisAdd(::cProduto, ::cTES, ::nQuant, ::nPrcP1, 0, "", "", 0, 0, 0, 0, 0, ::nTotP1, 0, ::nRegSB1, ::nRegSF4)

			//Rentabilidade Standard do Preco de Tabela
			::nICMSP1		:= MaFisRet(1, "IT_VALICM")
			::nCOFP1		:= MaFisRet(1, "IT_VALCF2")
			::nPISP1		:= MaFisRet(1, "IT_VALPS2")
			::nLiqP1		:= ::nTotP1 - ::nICMSP1 - ::nCOFP1 - ::nPISP1 - MaFisRet(1, "IT_DIFAL") - MaFisRet(1, "IT_VFCPDIF") - MaFisRet(1, "IT_VALCMP")
			::nRntP1		:= Round((1 - (::nCstStd / ::nLiqP1)) * 100, ::nRndRnt)

			MaFisEnd()
		EndIf

		If lRestore
			MaFisRestore()
		EndIf

		::lCalculado	:= .T.
	EndIf

	RestArea(aAreaSF4)
	RestArea(aAreaSA1)
	RestArea(aAreaSUS)
	RestArea(aAreaSB2)
	RestArea(aAreaSB1)
	RestArea(aArea)

Return lRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} GetCliente
Retorna o Codigo do Cliente utilizado para o calculo

@author  Guilherme Santos
@since   29/04/2022
@version 12.1.27
/*/
//-------------------------------------------------------------------
Method GetCliente() Class RntXCalc
Return ::cCliente
//-------------------------------------------------------------------
/*/{Protheus.doc} GetLojaCli
Retorna a Loja do Cliente utilizado para o calculo

@author  Guilherme Santos
@since   29/04/2022
@version 12.1.27
/*/
//-------------------------------------------------------------------
Method GetLojaCli() Class RntXCalc
Return ::cLojaCli
//-------------------------------------------------------------------
/*/{Protheus.doc} SetCstReal
Localiza e atribui o Custo Real

@author  Guilherme Santos
@since   20/02/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
Method SetCstReal() Class RntXCalc
	Local lRetorno 	:= .T.

	DbSelectArea("SB2")
	DbSetOrder(1)		//B2_FILIAL, B2_COD, B2_LOCAL

	If SB2->(DbSeek(xFilial("SB2") + ::cProduto + ::cArmazem))
		::nCstReal 	:= Round(If(SB2->B2_QATU == 0, SB2->B2_CM1, SB2->B2_VATU1 / SB2->B2_QATU), ::nRndCst) * ::nQuant
	EndIf

	//Se permaneceu zerado retorna o custo Standard
	If ::nCstReal == 0
		::nCstReal := ::nCstStd
	EndIf

Return lRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} GetCstReal
Retorna o Custo Real

@author  Guilherme Santos
@since   20/02/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
Method GetCstReal() Class RntXCalc
Return ::nCstReal
//-------------------------------------------------------------------
/*/{Protheus.doc} SetCstStd
Localiza e atribui o Custo Standard

@author  Guilherme Santos
@since   20/02/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
Method SetCstStd() Class RntXCalc
	Local lRetorno 	:= .T.

	DbSelectArea("SB4")
	DbSetOrder(1)		//B4_FILIAL, B4_COD

	If Len(AllTrim(::cProduto)) <= ::nTamRef .AND. SB4->(DbSeek(xFilial("SB4") + Left(::cProduto, ::nTamRef)))

		//Tira primeiro o ICMS do Custo Standard
		::nCstStd 	:= Round(SB4->B4_CUSTD * (1 - (SB4->B4_XICMF / 100)), ::nRndCst)
		//Tira os demais impostos do custo sem o ICMS
		::nCstStd 	:= Round(::nCstStd * (1 - ((::nAliqPCC + If(SB4->B4_ORIGEM == "1", SB4->B4_XIPIF, 0)) / 100)), ::nRndCst) * ::nQuant

		DbSelectArea("SB1")
		DbSetOrder(1)		//B1_FILIAL, B1_COD

		If SB1->(DbSeek(xFilial("SB1") + ::cProduto)) .OR. SB1->(DbSeek(xFilial("SB1") + Substr(::cProduto, 1, ::nTamRef)))
			::nRegSB1	:= SB1->(Recno())
		EndIf
	Else
		DbSelectArea("SB1")
		DbSetOrder(1)		//B1_FILIAL, B1_COD

		//Se nao achar o produto procura o primeiro item da grade
		If SB1->(DbSeek(xFilial("SB1") + ::cProduto)) .OR. SB1->(DbSeek(xFilial("SB1") + Substr(::cProduto, 1, ::nTamRef)))
			//Tira primeiro o ICMS do Custo Standard
			::nCstStd 	:= Round(SB1->B1_CUSTD * (1 - (SB1->B1_XICMF / 100)), ::nRndCst)
			//Tira os demais impostos do custo sem o ICMS
			::nCstStd 	:= Round(::nCstStd * (1 - ((::nAliqPCC + If(SB1->B1_ORIGEM == "1", SB1->B1_XIPIF, 0)) / 100)), ::nRndCst) * ::nQuant

			::nRegSB1	:= SB1->(Recno())
		Else
			lRetorno := .F.
		EndIf
	EndIf

Return lRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} GetCstStd
Retorna o Custo Standard

@author  Guilherme Santos
@since   20/02/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
Method GetCstStd() Class RntXCalc
Return ::nCstStd
//-------------------------------------------------------------------
/*/{Protheus.doc} GetAlqICMS
Retorna a Aliq. ICMS

@author  Guilherme Santos
@since   20/02/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
Method GetAlqICMS() Class RntXCalc
Return ::nICMS
//-------------------------------------------------------------------
/*/{Protheus.doc} GetRedICMS
Retorna o Perc. Red. ICMS

@author  Guilherme Santos
@since   20/02/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
Method GetRedICMS() Class RntXCalc
Return ::nRedICM
//-------------------------------------------------------------------
/*/{Protheus.doc} GetValICMS
Retorna o Valor ICMS

@author  Guilherme Santos
@since   20/02/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
Method GetValICMS() Class RntXCalc
Return ::nValICM
//-------------------------------------------------------------------
/*/{Protheus.doc} GetAlqPIS
Retorna a Aliq. PIS

@author  Guilherme Santos
@since   20/02/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
Method GetAlqPIS() Class RntXCalc
Return ::nAlqPIS
//-------------------------------------------------------------------
/*/{Protheus.doc} GetValPIS
Retorna o Valor PIS

@author  Guilherme Santos
@since   20/02/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
Method GetValPIS() Class RntXCalc
Return ::nValPIS
//-------------------------------------------------------------------
/*/{Protheus.doc} GetAlqCOF
Retorna a Aliq. COFINS

@author  Guilherme Santos
@since   20/02/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
Method GetAlqCOF() Class RntXCalc
Return ::nAlqCOF
//-------------------------------------------------------------------
/*/{Protheus.doc} GetValCOF
Retorna o Valor COFINS

@author  Guilherme Santos
@since   20/02/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
Method GetValCOF() Class RntXCalc
Return ::nValCOF
//-------------------------------------------------------------------
/*/{Protheus.doc} GetValCOF
Retorna o Valor ICD

@author  Guilherme Santos
@since   20/02/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
Method GetValICD() Class RntXCalc
Return ::nValICD
//-------------------------------------------------------------------
/*/{Protheus.doc} GetValCOF
Retorna o Valor ICC

@author  Guilherme Santos
@since   20/02/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
Method GetValICC() Class RntXCalc
Return ::nValICC
//-------------------------------------------------------------------
/*/{Protheus.doc} GetPrcLiq
Retorna o Preco Liquido do Produto

@author  Guilherme Santos
@since   20/02/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
Method GetPrcLiq() Class RntXCalc
Return ::nPrcLiq
//-------------------------------------------------------------------
/*/{Protheus.doc} GetRntSTD
Retorna a Rentabilidade Standard

@author  Guilherme Santos
@since   20/02/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
Method GetRntSTD() Class RntXCalc
Return ::nRntSTD
//-------------------------------------------------------------------
/*/{Protheus.doc} GetRntReal
Retorna a Rentabilidade Real

@author  Guilherme Santos
@since   20/02/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
Method GetRntReal() Class RntXCalc
Return ::nRntReal
//-------------------------------------------------------------------
/*/{Protheus.doc} GetICMSST
Retorna se tem ICMS ST

@author  Guilherme Santos
@since   20/02/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
Method GetICMSST() Class RntXCalc
Return ::cICMSST
//-------------------------------------------------------------------
/*/{Protheus.doc} GetAlqIPI
Retorna a Aliq. IPI

@author  Guilherme Santos
@since   20/02/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
Method GetAlqIPI() Class RntXCalc
Return ::nAlqIPI
//-------------------------------------------------------------------
/*/{Protheus.doc} GetValIPI
Retorna o Valor IPI

@author  Guilherme Santos
@since   20/02/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
Method GetValIPI() Class RntXCalc
Return ::nValIPI
//-------------------------------------------------------------------
/*/{Protheus.doc} GetValIPI
Retorna a Rentabilidade sobre o Preco de Tabela

@author  Guilherme Santos
@since   20/02/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
Method GetRntP1() Class RntXCalc
Return ::nRntP1
//-------------------------------------------------------------------
/*/{Protheus.doc} GetValIPI
Retorna o Valor Liquido Sobre o Preco de Tabela

@author  Guilherme Santos
@since   20/02/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
Method GetLiqP1() Class RntXCalc
Return ::nLiqP1

//-------------------------------------------------------------------
/*/{Protheus.doc} GetTes
Retorna Tes

@author  Victor Dessunte
@since   18/02/2020
@version 12.1.25
/*/
//-------------------------------------------------------------------
Method GetTes() Class RntXCalc
Return ::cTes
//-------------------------------------------------------------------
/*/{Protheus.doc} GetBasST
Retorna a Base da Substituicao Tributaria

@author  Guilherme Santos
@since   27/04/2022
@version 12.1.27
/*/
//-------------------------------------------------------------------
Method GetBasST() Class RntXCalc
Return ::nBasST
//-------------------------------------------------------------------
/*/{Protheus.doc} GetAlqST
Retorna a Aliquota da Substituicao Tributaria

@author  Guilherme Santos
@since   27/04/2022
@version 12.1.27
/*/
//-------------------------------------------------------------------
Method GetAlqST() Class RntXCalc
Return ::nAlqST
//-------------------------------------------------------------------
/*/{Protheus.doc} GetVlrST
Retorna o Valor da Substituicao Tributaria

@author  Guilherme Santos
@since   27/04/2022
@version 12.1.27
/*/
//-------------------------------------------------------------------
Method GetVlrST() Class RntXCalc
Return ::nVlrST
//-------------------------------------------------------------------
/*/{Protheus.doc} GetTotST
Retorna o Total da Substituicao Tributaria

@author  Guilherme Santos
@since   27/04/2022
@version 12.1.27
/*/
//-------------------------------------------------------------------
Method GetTotST() Class RntXCalc
Return ::nTotST
