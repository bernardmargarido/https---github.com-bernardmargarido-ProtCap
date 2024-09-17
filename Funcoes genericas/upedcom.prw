#INCLUDE "PROTHEUS.CH"
#INCLUDE "MSOBJECT.CH"

User Function uPedCom()
Return Nil
//-------------------------------------------------------------------
/*/{Protheus.doc} uPedCompra
Classe Responsavel pela gravacao do Pedido de Compra via MsExecAuto.

@author  Guilherme Santos
@since   03/04/09
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Class uPedCompra From uExecAuto
	Data cPedido

	Method New()
	Method AddCabec(cCampo, xValor, xValid)
	Method SetItem()
	Method Gravacao(nOpcao)
	Method GetNumero()
EndClass
//-------------------------------------------------------------------
/*/{Protheus.doc} New
Inicializa o Objeto

@author  Guilherme Santos
@since   03/04/09
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method New() Class uPedCompra
	_Super:New()		//Herda os Metodos e Propriedades da Classe Pai uExecAuto

	::aTabelas		:= {"SC7","SA2","SB1","SB2","SF4"}
	::cPedido		:= ""
	::cFileLog		:= "MATA120.LOG"
Return Self
//-------------------------------------------------------------------
/*/{Protheus.doc} AddCabec
Armazena os Valores do Cabecalho do Pedido para gravacao

@author  Guilherme Santos
@since   03/04/09
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method AddCabec(cCampo, xValor, xValid) Class uPedCompra
	Default xValid := NIL

	If AllTrim(cCampo) == "C7_NUM"
		::cPedido	:= xValor
	EndIf

	If AllTrim(cCampo) == "C7_EMISSAO"
		::dEmissao := xValor
	EndIf

	_Super:AddCabec(cCampo, xValor, xValid)
Return Nil
//-------------------------------------------------------------------
/*/{Protheus.doc} SetItem
Adiciona o Item Atual ao Array de Itens

@author  Guilherme Santos
@since   28/04/09
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method SetItem() Class uPedCompra
	Local nPosNum	:= Ascan(::aItemTemp, {|x| x[01] == "C7_NUM"})

	If nPosNum == 0
		Aadd(::aItemTemp, {"C7_NUM", ::cPedido, NIL})
	EndIf

	FWVetByDic(::aItemTemp, "SC7")

	_Super:SetItem()
Return Nil
//-------------------------------------------------------------------
/*/{Protheus.doc} Gravacao
Gravacao do Pedido de Compra via MsExecAuto

@author  Guilherme Santos
@since   03/04/09
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method Gravacao(nOpcao) Class uPedCompra
	Local cItem				:= ""
	Local dDataBackup		:= dDataBase
	Local nPosNum			:= 0
	Local nPosItm			:= 0
	Local nRecno			:= 0
	Local nI				:= 0
	Local lReserva			:= .F.
	Local lRetorno			:= .T.

	Private lMsErroAuto		:= .F.

	//Prepara o Ambiente na Empresa e Filial para Gravacao
	::SetEnv(1, "COM")
	
	DbSelectArea("SC7")
	DbSetOrder(1)

	//Altera a Database para Geracao do Pedido na data correta
	If !Empty(::dEmissao)
		dDataBase := ::dEmissao
	EndIf

	//Se for inclusao, reserva um numero para o Pedido
	If nOpcao == 3
		If Empty(::cPedido)
			//Se for Inclusao Pega o Proximo Numero de Pedido
			::cPedido	:= GetSX8Num("SC7", "C7_NUM")
			lReserva	:= .T.
	
			::AddCabec("C7_NUM", ::cPedido)
		EndIf
	Else
		If Empty(::cPedido)
			lRetorno		:= .F.
			::cMensagem		:= "Numero do Pedido não informado."
		Else
			If !SC7->(DbSeek(xFilial("SC7") + ::cPedido))
				lRetorno 	:= .F.
				::cMensagem	:= "Pedido " + ::cPedido + " não cadastrado."
			EndIf
		EndIf
	EndIf

	If lRetorno
		If nOpcao <> 3
			nRecno := SC7->(Recno())
		EndIf
	EndIf

	If lRetorno
		For nI := 1 to Len(::aItens)
			//Guarda o Numero do Pedido no Item
			nPosNum := Ascan(::aItens[nI], {|x| x[01] == "C7_NUM"})
			If nPosNum <> 0
				::aItens[nI][nPosNum][02] := ::cPedido
			EndIf

			If nOpcao <> 3
				nPosItm	:= Ascan(::aItens[nI], {|x| x[01] == "C7_ITEM"})
				If nPosItm <> 0
					cItem := ::aItens[nI][nPosItm][02]

					DbSelectArea("SC7")
					DbSetOrder(1)	//C7_FILIAL, C7_NUM, C7_ITEM

					If SC7->(DbSeek(xFilial("SC7") + ::cPedido + cItem))
						Aadd(::aItens[nI], {"C7_REC_WT", SC7->(Recno()), NIL})
					EndIf
				EndIf
			EndIf
		Next nI

		If nOpcao <> 3
			SC7->(DbGoto(nRecno))
		EndIf

		FWVetByDic(::aCabec, "SC7")

		//Gravacao do Pedido de Compra
		MSExecAuto({|a, b, c, d| MATA120(a, b, c, d)}, 1, ::aCabec, ::aItens, nOpcao)
	
		If lMsErroAuto
			lRetorno := .F.

			If lReserva
				RollBackSX8()
			EndIf
	
			If ::lExibeTela
				MostraErro()
			EndIf
			
			If ::lGravaLog
				::cMensagem := MostraErro(::cPathLog, ::cFileLog)
			EndIf
		Else
			If lReserva
				ConfirmSX8()
			EndIf
		EndIf
	
	EndIf

	::SetEnv(2, "COM")

	dDataBase := dDataBackup

Return lRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} GetNumero
Retorna o Numero do Pedido de Compra

@author  Guilherme Santos
@since   03/04/09
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method GetNumero() Class uPedCompra
Return ::cPedido
