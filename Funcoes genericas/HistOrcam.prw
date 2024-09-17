#Include "Protheus.ch"
//-------------------------------------------------------------------
/*/{Protheus.doc} HistOrcam
Hist�rico de Or�amento
@author  Jo�o Le�o
@since   16/11/2021
@version 12.1.25
@param   cTpHist 1=Inclus�o;2=Altera��o;3=Libera��o;4=Rejei��o;5=Desmembramento;6=Pedido Emitido;9=Cancelamento
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
Exibe a tela com o hist�rico do or�amento solicitado.
@author  Jo�o Le�o
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
				"Hist�rico de Or�amento"})
		EndIf
		ZCD->(dbSkip())
	End
EndIf

If ! lPVHist
	// Se n�o houver nada, traz uma linha em branco.
	If empty(aHist)
		aHist := {{"", "", "", ""}}
	Endif

	// Monta tela de entrada.
	DEFINE MSDIALOG oDlgHist TITLE "Hist�rico do or�amento" FROM 0, 0 TO 400, 950 PIXEL

	aCabec := {"Data/hora", "Usu�rio", "Ocorr�ncia", "Detalhe"}
	aTam   := {60, 50, 70, 100}
	oHistAtend := TWBrowse():New(0, 0, 0, 0,, aCabec, aTam, oDlgHist,,,,,,,,,,,,.F.,,.T.,,.F.)
	oHistAtend:Align := CONTROL_ALIGN_ALLCLIENT
	oHistAtend:SetArray(aHist)
	oHistAtend:bLine := {|| aHist[oHistAtend:nAt]}
	oHistAtend:nAt   := 1

	// Grupo dos bot�es.
	oGrBut := TScrollArea():New(oDlgHist, 0, 0, 15, 0, .F., .F.)
	oGrBut:Align := CONTROL_ALIGN_BOTTOM

	tButton():New(2, 438, "Sair", oGrBut, {|| oDlgHist:End()}, 35, 10,,,, .T.)

	ACTIVATE MSDIALOG oDlgHist CENTERED
EndIf

Return Nil
