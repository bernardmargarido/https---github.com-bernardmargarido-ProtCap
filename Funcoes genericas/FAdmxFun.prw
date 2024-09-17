#include "protheus.ch"

Static cPictVlr   := "@E 999,999,999.99"
Static cCodBCO    := PadR(SuperGetMV("PC_CODBCO",, ""),    03)
Static cCodAGE    := PadR(SuperGetMV("PC_CODAGE",, ""),    05)
Static cCodCTA    := PadR(SuperGetMV("PC_CODCTA",, ""),    10)
Static cSubCTA    := PadR(SuperGetMv("PC_SUBCTA",, "REM"), 03)

// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Administrativo
// Fun��o   : FBoleto
// Descri��o: Gera c�digo de barras e linha digit�vel de um t�tulo.
// Retorno  : L�gico, indicando se gerou os c�digos com sucesso.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 14/04/13 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function FBoleto(cCart, cNumBco)

Local lRet       := .T.
Local nValor     := SE1->E1_VALOR
Local dVencto    := SE1->E1_VENCREA
Local aArea      := SEE->(GetArea())

Local cSA6Chave  := ""
Local cBanco     := ""
Local cAgencia   := ""
Local cConta     := ""
Local cDigCC     := ""
Local cCodEmp    := ""
Local cCpoEsp    := ""

Default cCart      := ""
Default cCodBCO    := SE1->E1_PORTADO
Default cCodAGE    := SE1->E1_AGEDEP
Default cCodCTA    := SE1->E1_CONTA
Default cSubCTA    := "REM"

// Busca o banco do t�tulo.
If empty(SE1->(E1_PORTADO + E1_AGEDEP + E1_CONTA))
	cSA6Chave := cCodBCO + cCodAGE + cCodCTA
Else
	cSA6Chave := SE1->(E1_PORTADO + E1_AGEDEP + E1_CONTA)
Endif

// Posiciona banco.
SA6->(dbSetOrder(1))  // A6_FILIAL, A6_COD, A6_AGENCIA, A6_NUMCON.
SA6->(dbSeek(xFilial() + cSA6Chave, .F.))

// Posiciona a configura��o da conta.
SEE->(dbSetOrder(1))  // EE_FILIAL, EE_CODIGO, EE_AGENCIA, EE_CONTA, EE_SUBCTA.
SEE->(dbSeek(xFilial() + cSA6Chave + cSubCTA, .F.))

// Pega a configura��o da conta.
cBanco   := SA6->A6_COD
cAgencia := AllTrim(SA6->A6_AGENCIA)
If empty(SA6->A6_DVCTA)
	cConta := StrTran(SA6->A6_NUMCON, "-", "")
	cConta := left(SA6->A6_NUMCON, len(rtrim(SA6->A6_NUMCON)) - 1)
	cDigCC := right(rtrim(SA6->A6_NUMCON), 1)
Else
	cConta := AllTrim(StrTran(SA6->A6_NUMCON, "-", ""))
	cDigCC := AllTrim(SA6->A6_DVCTA)
Endif
cCodEmp := AllTrim(SEE->EE_CODEMP)

If empty(cCart)
	cCart   := SEE->EE_CART
Endif
cCart := AllTrim(cCart)

If empty(cNumBco)
	cNumBco := SE1->E1_NUMBCO
Endif
cNumBco := AllTrim(cNumBco)

// Monta campo espec�fico de cada banco.
If empty(cNumBco)
	// Se n�o tiver n�mero banc�rio, n�o faz nada.

ElseIf SA6->A6_COD == "001"  // Banco do Brasil.
	// Monta a string do boleto.
	cCart    := PadL(If(empty(cCart), "11", cCart), 2, "0")
	cNumBco  := PadL(left(cNumBco, len(rtrim(cNumBco)) - 1), 11, "0")  // Retira o c�digo verificador do n�mero banc�rio.

	cCpoEsp  := cNumBco                  // N�mero banc�rio.
	cCpoEsp  += PadL(cAgencia, 04, "0")  // Agencia (sem DV).
	cCpoEsp  += PadL(cConta,   08, "0")  // Conta corrente (sem DV).
	cCpoEsp  += cCart                    // Tipo de carteira / modalidade de cobran�a.

ElseIf SA6->A6_COD == "033"  // Santander.
	// Monta a string do boleto.
	cCart    := PadL(If(empty(cCart), "104", cCart), 3, "0")
	cCodEmp  := PadL(right(cCodEmp, 07), 07, "0")
	cNumBco  := PadL(right(cNumBco, 13), 13, "0")
	dVencto  := SE1->E1_VENCREA

	cCpoEsp  := "9"       // Fixo "9".
	cCpoEsp  += cCodEmp   // C�digo do cedente padr�o Santander.
	cCpoEsp  += cNumBco   // N�mero banc�rio.
	cCpoEsp  += "0"       // IOS (seguradoras).
	cCpoEsp  += cCart     // Tipo de carteira / modalidade de cobran�a.

ElseIf SA6->A6_COD == "341"  // Ita�.
	// Acerta o tamanho dos campos.
	cAgencia := PadL(cAgencia, 04, "0")
	cConta   := PadL(cConta,   05, "0")
	cCart    := PadL(If(empty(cCart), "112", cCart), 3, "0")
	cNumBco  := PadL(left(cNumBco, len(rtrim(cNumBco)) - 1), 08, "0")  // Retira o c�digo verificador do n�mero banc�rio.

	// Monta a string do boleto.
	cCpoEsp  := cCart     // Tipo de carteira / modalidade de cobran�a.
	cCpoEsp  += cNumBco   // N�mero banc�rio.
	cCpoEsp  += Modulo10(cAgencia + cConta + cDigCC + cCart + cNumBco)  // DAC [ag�ncia/conta/carteira/nosso n�mero].
	cCpoEsp  += cAgencia  // Agencia (sem DV).
	cCpoEsp  += cConta    // Conta corrente (sem DV).
	cCpoEsp  += cDigCC    // DAC [ag�ncia/conta].
	cCpoEsp  += "000"     // Zeros.
Endif

// Gera c�digo de barras e linha digit�vel.
If empty(cCpoEsp)
	lRet := .F.
Else
	cCodBar := FCodBar(cBanco,, nValor, dVencto, cCpoEsp)
	RecLock("SE1", .F.)
	SE1->E1_CODBAR := cCodBar
	SE1->E1_CODDIG := FCodDig(cCodBar)
	SE1->(msUnLock())
Endif

RestArea(aArea)

Return lRet


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Administrativo
// Fun��o   : CodBar
// Descri��o: Retorna o c�digo de barras de um t�tulo.
// Retorno  : O c�digo de barras (44 caracteres).
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 14/04/13 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
Static Function FCodBar(cBanco, cMoeda, nValor, dVencto, cLivre)

Local cCodBar  := ""
Local aCodBar  := array(6)

Default cMoeda := 9  // 9-Real.
Default cLivre := ""

// Montagem do codigo de barras.
aCodBar[1] := padl(cBanco, 3, "0")
aCodBar[2] := padl(cMoeda, 1, "0")
aCodBar[3] := ""     // M�dulo 11 do c�digo de barras.
aCodBar[4] := StrZero((dVencto - stod("19971007")), 4)
aCodBar[5] := StrZero(nValor * 100, 10)
aCodBar[6] := padl(cLivre, 25, "0")  // Campo livre do c�digo de barras para uso interno do banco.

// Calcula digito verificador, com base no modulo 11
cCodBar := ""; aEval(aCodBar, {|x| cCodBar += x})
aCodBar[3] := Modulo11(cCodBar, 2, 9)
aCodBar[3] := If(aCodBar[3] == "0", "1", aCodBar[3])

// Monta o c�digo de barras.
cCodBar := ""; aEval(aCodBar, {|x| cCodBar += x})

Return cCodBar


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Administrativo
// Fun��o   : CodDig
// Descri��o: Retorna a linha digit�vel do c�digo de barras de um t�tulo.
// Retorno  : A linha digit�vel (47 caracteres).
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 14/04/13 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
Static Function FCodDig(cCodBar)

Local cCodDig    := ""
Local cCampo1    := ""
Local cCampo2    := ""
Local cCampo3    := ""
Local cCampo4    := ""
Local cCampo5    := ""

// Montagem da linha digit�vel.
cCampo1 := SubStr(cCodBar, 01, 04) + SubStr(cCodBar, 20, 05)
cCampo1 := cCampo1 + Modulo10(cCampo1)

cCampo2 := SubStr(cCodBar, 25, 10)
cCampo2 := cCampo2 + Modulo10(cCampo2)

cCampo3 := SubStr(cCodBar, 35, 10)
cCampo3 := cCampo3 + Modulo10(cCampo3)

cCampo4 := SubStr(cCodBar, 05, 01)
cCampo5 := SubStr(cCodBar, 06, 04) + SubStr(cCodBar, 10, 10)

cCodDig := cCampo1 + cCampo2 + cCampo3 + cCampo4 + cCampo5

Return cCodDig


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Administrativo
// Fun��o   : SE1Cart
// Descri��o: Retorna a carteira a qual o t�tulo pertence.
// Retorno  : A carteira.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 10/05/17 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function SE1Cart()
Return If(empty(SE1->E1_CODBAR), SEE->EE_CART, SubStr(SE1->E1_CODBAR, 20, 03))


// ##############################################################################
// Projeto  : PROT-CAP
// Autor    : Joao Leao
// Modulo   : Financeiro
// Fun��o   : RETMAILFIN
// Descri��o: Retorna o e-mail financeiro do cliente, caso exista.
// Retorno  : Texto.
// ---------+---------------------------+----------------------------------------
// Data     | Autor                     | Descricao
// ---------+---------------------------+----------------------------------------
// 26/01/16 | Joao Leao                 | Desenvolvimento da rotina
// ---------+---------------------------+----------------------------------------
User Function RetMailFin(cClinte, cLoja)

Local aAreaSA1   := SA1->(GetArea())
Local cEmail     := ""

SA1->(dbSetOrder(1)) //A1_FILIAL,A1_COD,A1_LOJA
SA1->(msSeek(xFilial() + cClinte + cLoja, .F.))
If "@" $ SA1->A1_MAILFIN .And. "." $ SA1->A1_MAILFIN
	cEmail := AllTrim(SA1->A1_MAILFIN)
ElseIf "@" $ SA1->A1_XMAILRE .And. "." $ SA1->A1_XMAILRE
	cEmail := AllTrim(SA1->A1_XMAILRE)
ElseIf "@" $ SA1->A1_XCLICRD .And. "." $ SA1->A1_XCLICRD
	SA1->(msSeek(xFilial() + SA1->A1_XCLICRD, .F.))
	If "@" $ SA1->A1_MAILFIN .And. "." $ SA1->A1_MAILFIN
		cEmail += AllTrim(SA1->A1_MAILFIN)
	ElseIf "@" $ SA1->A1_XMAILRE .And. "." $ SA1->A1_XMAILRE
		cEmail += AllTrim(SA1->A1_XMAILRE)
	EndIf
EndIf

RestArea(aAreaSA1)

Return cEmail


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Administrativo
// Fun��o   : DocsFin
// Descri��o: Fun��o para anexar arquivos financeiros ao cadastro do cliente.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 19/12/16 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function DocsFin(cAlias, nReg, nOpc)

If cAlias = "SA1"
	SA1->(dbGoTo(nReg))
	ZB8->(dbSetOrder(1))  // ZB8_FILIAL, ZB8_COD, ZB8_LOJA.
	If !ZB8->(msSeek(xFilial() + SA1->(A1_COD + A1_LOJA), .F.))
		RecLock("ZB8", .T.)
		ZB8->ZB8_FILIAL := xFilial()
		ZB8->ZB8_COD    := SA1->A1_COD
		ZB8->ZB8_LOJA   := SA1->A1_LOJA
		ZB8->ZB8_NOME   := SA1->A1_NOME
		ZB8->(msUnLock())
	Endif
	MsDocument("ZB8", ZB8->(RecNo()), nOpc)
Endif

Return


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Financeiro
// Fun��o   : VLogCrd
// Descri��o: Abre tela de hist�rico de cr�dito do cliente.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 05/01/17 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function VLogCrd(cAteFil, cAtendP, cPedidoP)

Local oDlgHist, oHistAtend, oGrBut
Local aCabec[0], aTam[0]
Local aHist      := {}
Local cQuery     := ""
Local cAliasTop  := GetNextAlias()

cQuery := "select ZE1.R_E_C_N_O_ ZE1RecNo " + CRLF
cQuery += "from " + RetSQLName("ZE1") + " ZE1 with (noLock) " + CRLF
cQuery += "where ZE1.D_E_L_E_T_ = ' ' " + CRLF
cQuery += "and ZE1.ZE1_FILIAL = '" + xFilial("ZE1") + "' " + CRLF
cQuery += "and ZE1.ZE1_CLIENT = '" + SA1->A1_COD + "' " + CRLF
cQuery += "and ZE1.ZE1_LOJA   = '" + SA1->A1_LOJA + "'" + CRLF
cQuery += "order by ZE1.ZE1_DATA, ZE1.R_E_C_N_O_ "

dbUseArea(.T., "TOPCONN", TCGenQry(,, cQuery), cAliasTop, .F., .T.)

Do While (cAliasTop)->(!eof())
	ZE1->(dbGoTo((cAliasTop)->ZE1RecNo))

	ZE1->(aAdd(aHist, {dtoc(ZE1_DATA), ZE1_USUARI, Transform(ZE1_LC, cPictVlr), ZE1_VENCLC, ZE1_RISCO, ""}))
	(cAliasTop)->(dbSkip())
EndDo
(cAliasTop)->(dbCloseArea())

// Se n�o houver nada, traz uma linha em branco.
If empty(aHist)
	aHist := {{"", "", "0,00", "  /  /    ", "", ""}}
Endif

// Monta tela de entrada.
DEFINE MSDIALOG oDlgHist TITLE "Hist�rico de altera��o de cr�dito" FROM 0, 0 TO 300, 850 PIXEL

aCabec := {"Data", "Usu�rio", "Limite (R$)", "Vencimento", "Risco", ""}
aTam   := {35, 70, 50, 40, 30, 0}
oHistAtend := TWBrowse():New(0, 0, 0, 0,, aCabec, aTam, oDlgHist,,,,,,,,,,,,.F.,,.T.,,.F.)
oHistAtend:Align := CONTROL_ALIGN_ALLCLIENT
oHistAtend:SetArray(aHist)
oHistAtend:bLine := {|| aHist[oHistAtend:nAt]}
oHistAtend:nAt   := 1

// Grupo dos bot�es.
oGrBut := TScrollArea():New(oDlgHist, 0, 0, 15, 0, .F., .F.)
oGrBut:Align := CONTROL_ALIGN_BOTTOM

tButton():New(2, 388, "Sair", oGrBut, {|| oDlgHist:End()}, 35, 10,,,, .T.)

ACTIVATE MSDIALOG oDlgHist CENTERED

Return
// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Bruno Carvalho
// Modulo   : Financeiro
// Fun��o   : VLogblq
// Descri��o: Abre tela de hist�rico de bloqueio do cliente.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 29/01/18 | Bruno Carvalho    | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function VLogblq()

Local oDlgHist, oHistBlq, oGrBut
Local aCabec[0], aTam[0]
Local aHist      := {}
Local cQuery     := ""
Local cAliasTop  := GetNextAlias()

cQuery := "select ZCB.R_E_C_N_O_ ZCBRecNo " + CRLF
cQuery += "from " + RetSQLName("ZCB") + " ZCB with (noLock) " + CRLF
cQuery += "where ZCB.D_E_L_E_T_ = ' ' " + CRLF
cQuery += "and ZCB.ZCB_FILIAL = '" + xFilial("SA1") + "' " + CRLF
cQuery += "and ZCB.ZCB_CLIENT = '" + SA1->A1_COD + "' " + CRLF
cQuery += "and ZCB.ZCB_LOJA   = '" + SA1->A1_LOJA + "'" + CRLF
cQuery += "order by ZCB.ZCB_DTBLQ, ZCB.R_E_C_N_O_ "

dbUseArea(.T., "TOPCONN", TCGenQry(,, cQuery), cAliasTop, .F., .T.)

Do While (cAliasTop)->(!eof())
	ZCB->(dbGoTo((cAliasTop)->ZCBRecNo))

	ZCB->(aAdd(aHist, {ZCB_CLIENT, ZCB_LOJA, If(ZCB_STATUS == "1", "BLOQUEADO", "DESBLOQUEADO"), ZCB_DESMOT, ZCB_AREA, ZCB_USERBL, dtoc(ZCB_DTBLQ), ZCB_HRBLQ}))
	(cAliasTop)->(dbSkip())

EndDo
(cAliasTop)->(dbCloseArea())

// Se n�o houver nada, traz uma linha em branco.
If empty(aHist)
	aHist := {{"", "", "", "", "", "", "  /  /    ", "00:00"}}
Endif

// Monta tela de entrada.
DEFINE MSDIALOG oDlgHist TITLE "Hist�rico de bloqueio do cliente" FROM 0, 0 TO 300, 850 PIXEL

aCabec := {"Cliente", "Loja", "Status", "Motivo", "�rea", "Usu�rio", "Dt. blq.", "Hr. Blq."}
aTam   := {30, 15, 50, 200, 30, 30, 20, 20}
oHistBlq := TWBrowse():New(0, 0, 0, 0,, aCabec, aTam, oDlgHist,,,,,,,,,,,,.F.,,.T.,,.F.)
oHistBlq:Align := CONTROL_ALIGN_ALLCLIENT
oHistBlq:SetArray(aHist)
oHistBlq:bLine := {|| aHist[oHistBlq:nAt]}
oHistBlq:nAt   := 1

// Grupo dos bot�es.
oGrBut := TScrollArea():New(oDlgHist, 0, 0, 15, 0, .F., .F.)
oGrBut:Align := CONTROL_ALIGN_BOTTOM

tButton():New(2, 388, "Sair", oGrBut, {|| oDlgHist:End()}, 35, 10,,,, .T.)

ACTIVATE MSDIALOG oDlgHist CENTERED

Return

//-------------------------------------------------------------------
/*/{Protheus.doc} BZGRPVEN
Inclui grupo de venda para cliente
@author  Victor Dessunte
@since   12/01/2021
@version 12.1.25
/*/
//-------------------------------------------------------------------
User Function BZGRPVEN(cCodCli, cLojCli, cCanal, cClasse, cSClasse)

Local aArea		:= GetArea()
Local cCodGrupo	:= ""
Local cDesc		:= ""
Local lContinua := .T.
Local lCont	    := .T.

SA1->(DBSetOrder(1))
If SA1->(DBSeek(xFilial("SA1") + cCodCli + cLojCli))
	//Cliente pessoa jur�dica � verificado se existe grupo com a mesma raiz de CNPJ
	If Empty(SA1->A1_GRPVEN) .and. SA1->A1_EST <> "EX"
		If SA1->A1_PESSOA == 'J'
			cCodGrupo := U_FNDGRPVE(left(SA1->A1_CGC, 8),.F.)
		Else //Pessoa Fisica
			cCodGrupo := U_FNDGRPVE(SA1->A1_CGC,.T.)		
		EndIf
	EndIf

	If Empty(cCodGrupo)
		While lCont
			cCodGrupo	:= GetSxeNum("ACY", "ACY_GRPVEN")
			ACY->(dbSetOrder(1))
			If ACY->(dbSeek(xFilial("ACY")+cCodGrupo))
				ConfirmSx8()
			Else
				lCont := .F.
			EndIf
		End
	Else
		lContinua := .F.
	EndIf

	If lContinua
		If SA1->A1_EST == 'EX'
			cDesc := LEFT(UPPER(AllTrim(SA1->A1_NOME)), 30)
		Else
			cDesc := LEFT(UPPER(AllTrim(SA1->A1_NOME)), 26) + " " + RIGHT(ALLTRIM(SA1->A1_CGC),3)
		EndIf
		
		RecLock("ACY", .T.)
			ACY->ACY_FILIAL := xFilial("ACY")
			ACY->ACY_GRPVEN := cCodGrupo
			ACY->ACY_DESCRI := cDesc
			ACY->ACY_XCANAL := cCanal
			ACY->ACY_XCLASS := cClasse
			ACY->ACY_XDTALT := Date()
			ACY->ACY_XSCLAS := cSClasse
			ACY->ACY_XTPGRP := "S"

		ACY->(MsUnlock())
		ConfirmSX8()
	EndIf

	If Empty(SA1->A1_GRPVEN)
		RecLock("SA1", .F.)
			SA1->A1_GRPVEN := cCodGrupo
		SA1->(MsUnlock())
	EndIf

	If !Empty(cCodGrupo)
		//Acerta o tipo do grupo
		U_FIXGRPVE(cCodGrupo)
	EndIf
EndIf

RestArea(aArea)

Return
