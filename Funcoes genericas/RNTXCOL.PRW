#INCLUDE "PROTHEUS.CH"
#INCLUDE "MSOBJECT.CH"
//-------------------------------------------------------------------
/*/{Protheus.doc} RNTXCOL
Funcao Generica para Compilacao
@author  Guilherme Santos
@since   10/05/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
User Function RNTXCOL()
Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} RNTXCOL
Classe para manipulacao de uma colecao de calculos de Rentabilidades

@author  Guilherme Santos
@since   10/05/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
Class RNTXCOL
	Data aProdutos

	Method New() Constructor
	Method AddItem(cCliente, cLojaCli, lProspect, cProduto, cArmazem, nPreco, nQuant, nPrcP1)
	Method ChkItem(cCliente, cLojaCli, lProspect, cProduto, cArmazem, nPreco, nQuant, nPrcP1)
	Method GetItem(nPosItem)
EndClass
//-------------------------------------------------------------------
/*/{Protheus.doc} New
Instancia o Objeto

@author  Guilherme Santos
@since   10/05/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
Method New() Class RNTXCOL
	::aProdutos := {}
Return Self
//-------------------------------------------------------------------
/*/{Protheus.doc} AddItem
Adiciona um item ao Objeto

@author  Guilherme Santos
@since   10/05/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
Method AddItem(cCliente, cLojaCli, lProspect, cProduto, cArmazem, nPreco, nQuant, nPrcP1) Class RNTXCOL
	Local nPosItem	:= 0
	Local oCalcRnt	:= RntXCalc():New(cCliente, cLojaCli, lProspect, cProduto, cArmazem, nPreco, nQuant, nPrcP1)

	If oCalcRnt:CheckCalc(cCliente, cLojaCli, lProspect, cProduto, cArmazem, nPreco, nQuant, nPrcP1)
		Aadd(::aProdutos, oCalcRnt)
		nPosItem := Len(::aProdutos)
	Else
		nPosItem := 0
	EndIf

Return nPosItem
//-------------------------------------------------------------------
/*/{Protheus.doc} AddItem
Verifica se o Item ja existe no Objeto e se o calculo precisa ser atualizado

@author  Guilherme Santos
@since   10/05/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
Method ChkItem(cCliente, cLojaCli, lProspect, cProduto, cArmazem, nPreco, nQuant, nPrcP1) Class RNTXCOL
	Local nI		:= 0
	Local nPosItem	:= 0
	Local oCalcRnt	:= NIL

	If nQuant == 0
		nQuant := 1
	EndIf

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

	For nI := 1 to Len(::aProdutos)
		If cCliente + cLojaCli + If(lProspect, "T", "F") + cProduto + cArmazem == ::aProdutos[nI]:GetKey()
			nPosItem := nI
			Exit
		EndIf
	Next nI

	If nPosItem > 0
		oCalcRnt := ::GetItem(nPosItem)

		If !oCalcRnt:CheckCalc(cCliente, cLojaCli, lProspect, cProduto, cArmazem, nPreco, nQuant, nPrcP1)
			nPosItem := 0
		EndIf
	Else
		nPosItem := ::AddItem(cCliente, cLojaCli, lProspect, cProduto, cArmazem, nPreco, nQuant, nPrcP1)
	EndIf

Return nPosItem
//-------------------------------------------------------------------
/*/{Protheus.doc} GetItem
Retorna o Objeto com o Calculo da Rentabilidade do Produto

@author  Guilherme Santos
@since   10/05/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
Method GetItem(nPosItem) Class RNTXCOL
	Local oItem	:= NIL

	If nPosItem <= Len(::aProdutos)
		oItem := ::aProdutos[nPosItem]
	EndIf

Return oItem
