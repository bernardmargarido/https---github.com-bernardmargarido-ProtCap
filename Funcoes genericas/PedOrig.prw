#include "protheus.ch"
//-------------------------------------------------------------------
/*/{Protheus.doc} PedOrig
Encontra o número do pedido original em caso de desmembramentos.
Deve receber a filial e número do pedido e o número do pedido pai por
referência.
@author  João
@since   21/11/18
@version 12.1.17
/*/
//-------------------------------------------------------------------
User Function PedOrig(cFilPed,cPedPai,cPedOri)
Local lSearch	:= .T.
Local cPedido	:= cPedPai
Local aSC5		:= SC5->(GetArea())
Local aArea		:= GetArea()

SC5->(dbSetOrder(1)) //FILIAL,NUM

While lSearch
	If SC5->(dbSeek(cFilPed + cPedido))
		If Empty(SC5->C5_XPEDPAI) .Or. SC5->C5_XPEDPAI == cPedido
			cPedOri := SC5->C5_NUM
			lSearch	:= .F.
		Else
			cPedido := SC5->C5_XPEDPAI
		EndIf
	Else
		lSearch := .F.
	EndIf
EndDo

RestArea(aSC5)
RestArea(aArea)

Return
