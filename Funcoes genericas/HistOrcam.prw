#Include "Protheus.ch"
//-------------------------------------------------------------------
/*/{Protheus.doc} HistOrcam
Histórico de Orçamento
@author  João Leão
@since   16/11/2021
@version 12.1.25
@param   cTpHist 1=Inclusão;2=Alteração;3=Liberação;4=Rejeição;5=Desmembramento;6=Pedido Emitido;9=Cancelamento
/*/
//-------------------------------------------------------------------
User Function HistOrcam(cFilOrc, cNumOrc, cTpHist, cDetalhe, dDtHist, cHrHist, cUsuario)

Default dDtHist     := Date()
Default cHrHist     := Time()
Default cUsuario    := cUserName

If Empty(cUsuario)
	cUsuario := "Sistema"
EndIf

If RecLock("ZCD",.T.)
	ZCD->ZCD_FILIAL := cFilOrc
	ZCD->ZCD_ORCAME := cNumOrc
	ZCD->ZCD_TPHIST := cTpHist
	ZCD->ZCD_DETALH := cDetalhe
	ZCD->ZCD_DATA   := dDtHist
	ZCD->ZCD_HORA   := cHrHist
	ZCD->ZCD_USUARI := cUsuario
	ZCD->(MSUnlock())
EndIf

Return Nil

//-------------------------------------------------------------------
/*/{Protheus.doc} vOrcHistor
Exibe a tela com o histórico do orçamento solicitado.
@author  João Leão
@since   19/11/2021
@version 12.1.25
/*/
//-------------------------------------------------------------------
User Function vOrcHistor(cFilOrc, cNumOrc, aPVHist, lPVHist)

Local oDlgHist, oHistAtend, oGrBut
Local aCabec[0], aTam[0]
Local aHist      := {}

Default aPVHist := {}
Default lPVHist	:= {}

ZCD->(DBSetOrder(2))  //ZCD_FILIAL, ZCD_ORCAME, ZCD_DATA, ZCD_HORA
If ZCD->(DBSeek(cFilOrc + cNumOrc, .F.))
	While ZCD->(!EOF()) .and. ZCD->(ZCD_FILIAL + ZCD_ORCAME) == cFilOrc + cNumOrc
		aAdd(aHist,;
			{dtoc(ZCD->ZCD_DATA) + "  " + ZCD->ZCD_HORA,;
			AllTrim(ZCD->ZCD_USUARI),;
			X3Combo("ZCD_TPHIST", ZCD->ZCD_TPHIST),;
			AllTrim(ZCD->ZCD_DETALH)})
		If lPVHist
			aAdd(aPVHist,;
				{dtoc(ZCD->ZCD_DATA) + "  " + ZCD->ZCD_HORA,;
				AllTrim(ZCD->ZCD_USUARI),;
				X3Combo("ZCD_TPHIST", ZCD->ZCD_TPHIST),;
				AllTrim(ZCD->ZCD_DETALH),;
				"Histórico de Orçamento"})
		EndIf
		ZCD->(dbSkip())
	End
EndIf

If ! lPVHist
	// Se não houver nada, traz uma linha em branco.
	If empty(aHist)
		aHist := {{"", "", "", ""}}
	Endif

	// Monta tela de entrada.
	DEFINE MSDIALOG oDlgHist TITLE "Histórico do orçamento" FROM 0, 0 TO 400, 950 PIXEL

	aCabec := {"Data/hora", "Usuário", "Ocorrência", "Detalhe"}
	aTam   := {60, 50, 70, 100}
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
EndIf

Return Nil
