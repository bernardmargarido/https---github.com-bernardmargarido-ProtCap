#INCLUDE "PROTHEUS.CH"
#INCLUDE "MSOBJECT.CH"

User Function uSolCom()
Return Nil
//-------------------------------------------------------------------
/*/{Protheus.doc} uSolCompra
Classe Responsavel pela gravacao da Solicitacao de Compra via MsExecAuto.

@author  Guilherme Santos
@since   21/07/2023
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Class uSolCompra From uExecAuto
	Data cSolCom

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
@since   21/07/2023
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method New() Class uSolCompra
	_Super:New()		//Herda os Metodos e Propriedades da Classe Pai uExecAuto

	::aTabelas		:= {"SC1","SA2","SB1","SB2","SF4"}
	::cSolCom		:= ""
	::cFileLog		:= "MATA110.LOG"
Return Self
//-------------------------------------------------------------------
/*/{Protheus.doc} AddCabec
Armazena os Valores do Cabecalho da Solicitacao para gravacao

@author  Guilherme Santos
@since   21/07/2023
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method AddCabec(cCampo, xValor, xValid) Class uSolCompra
	Default xValid := NIL

	If AllTrim(cCampo) == "C1_NUM"
		::cSolCom	:= xValor
	EndIf

	If AllTrim(cCampo) == "C1_EMISSAO"
		::dEmissao := xValor
	EndIf

	_Super:AddCabec(cCampo, xValor, xValid)
Return Nil
//-------------------------------------------------------------------
/*/{Protheus.doc} SetItem
Adiciona o Item Atual ao Array de Itens

@author  Guilherme Santos
@since   21/07/2023
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method SetItem() Class uSolCompra
	Local nPosNum	:= Ascan(::aItemTemp, {|x| x[01] == "C1_NUM"})

	If nPosNum == 0
		Aadd(::aItemTemp, {"C1_NUM", ::cSolCom, NIL})
	EndIf

	FWVetByDic(::aItemTemp, "SC1")

	_Super:SetItem()
Return Nil
//-------------------------------------------------------------------
/*/{Protheus.doc} Gravacao
Gravacao da Solicitacao de Compra via MsExecAuto

@author  Guilherme Santos
@since   21/07/2023
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method Gravacao(nOpcao) Class uSolCompra
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
	
	DbSelectArea("SC1")
	DbSetOrder(1)

	//Altera a Database para Geracao da Solicitacao na data correta
	If !Empty(::dEmissao)
		dDataBase := ::dEmissao
	EndIf

	//Se for inclusao, reserva um numero para a Solicitacao
	If nOpcao == 3
		If Empty(::cSolCom)
			//Se for Inclusao Pega o Proximo Numero de Pedido
			::cSolCom	:= GetSX8Num("SC1", "C1_NUM")
			lReserva	:= .T.
	
			::AddCabec("C1_NUM", ::cSolCom)
		EndIf
	Else
		If Empty(::cSolCom)
			lRetorno		:= .F.
			::cMensagem		:= "Numero da Solicitação não informado."
		Else
			If !SC1->(DbSeek(xFilial("SC1") + ::cSolCom))
				lRetorno 	:= .F.
				::cMensagem	:= "Solicitação " + ::cSolCom + " não cadastrada."
			EndIf
		EndIf
	EndIf

	If lRetorno
		If nOpcao <> 3
			nRecno := SC1->(Recno())
		EndIf
	EndIf

	If lRetorno
		For nI := 1 to Len(::aItens)
			//Guarda o Numero da Solicitacao no Item
			nPosNum := Ascan(::aItens[nI], {|x| x[01] == "C1_NUM"})
			If nPosNum <> 0
				::aItens[nI][nPosNum][02] := ::cSolCom
			EndIf

			If nOpcao <> 3
				nPosItm	:= Ascan(::aItens[nI], {|x| x[01] == "C1_ITEM"})
				If nPosItm <> 0
					cItem := ::aItens[nI][nPosItm][02]

					DbSelectArea("SC1")
					DbSetOrder(1)	//C1_FILIAL, C1_NUM, C1_ITEM

					If SC1->(DbSeek(xFilial("SC1") + ::cSolCom + cItem))
						Aadd(::aItens[nI], {"C1_REC_WT", SC1->(Recno()), NIL})
					EndIf
				EndIf
			EndIf
		Next nI

		If nOpcao <> 3
			SC1->(DbGoto(nRecno))
		EndIf

		FWVetByDic(::aCabec, "SC1")

		//Gravacao do Pedido de Compra
		MSExecAuto({|a, b, c| MATA110(a, b, c)}, ::aCabec, ::aItens, nOpcao)
	
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
@since   21/07/2023
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method GetNumero() Class uSolCompra
Return ::cSolCom
