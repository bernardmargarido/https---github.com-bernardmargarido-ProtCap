#include "protcap.ch"
#include "WISStatus.ch"
#include "fwmvcdef.ch"

// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Materiais
// Fun��o   : MA030TOK
// Descri��o: P.E. executado para validar a altera��o ou inclus�o de cliente.
// Retorno  : L�gico, validando ou n�o o processamento.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 16/09/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
// 04/01/17 | Francisco Oliveira| Manuten��o para chamada de fun��o da fabrica de
//          |                   | software U_F0100101 - integra��o WMS Winthor
// ------------------------------------------------------------------------------
User Function MA030TOK()

Local lRet       := .T.
Local aAreaSA1   := {}

If !IsInCallStack("U_WSVTEX46")
	aAreaSA1 := SA1->(GetArea())

	// Valida campos fiscais do cliente.
	If lRet
		lRet := A1VldFis()
	Endif

	// Valida��o para verificar se h� pedidos de venda em aberto.
	If lRet .And. ALTERA
		lRet := A1VldPV(M->A1_COD, M->A1_LOJA)
	Endif

	// Campo CNAE obrigat�rio para pessoa jur�dica ou produtor rural
	If lRet .and. (Empty(M->A1_CNAE) .Or. Val(M->A1_CNAE) == 0)
		If (M->A1_PESSOA == 'J' .Or. M->A1_TIPO == "L")
		    Help(,,ProcName(),,'Por favor, informe o campo: ' + AllTrim(RetTitle("A1_CNAE")) + '.',1,5)
			lRet := .F.
		EndIf
	Endif

	If lRet
		// Cliente do tipo Revendedor precisa ser contribuinte.
		If M->A1_TIPO == 'R' .And. M->A1_CONTRIB # '1'
			Help(,,ProcName(),,'Para tipo Revendedor � obrigat�rio preenchimento da I.E. e contribuinte SIM.',1,5)
			lRet := .F.
		EndIf
	EndIf

	If lRet
		If !Empty(M->A1_XCLICRD)
			If SubStr(M->A1_XCLICRD, 1, TamSx3('A1_COD')[1]) # M->A1_COD
				Help(,,ProcName(),,'O codigo do cliente utilizado para consumo do cr�dito n�o pode ser diferente do codigo principal.',1,5)
				lRet := .F.
			EndIf
		EndIf
	EndIf

	//Tratamento do campo Grupo de venda na inclus�o manual
	if lRet .and. INCLUI .and. !IsInCallStack("U_BPFATM03") //( !IsInCallStack("U_BZAPI001") .Or. !IsInCallStack("U_BPFATM03") ) //SSA-CAD
		//Se n�o encontrar, deve incluir o grupo.
		if Empty(M->A1_GRPVEN) .and. M->A1_EST <> "EX"
			Help(,,ProcName(),,'O c�digo do grupo de clientes � obrigat�rio.',1,5)
			lRet := .F.
		endIf
	endIf

	If lRet .and. ALTERA .and. !IsInCallStack("U_BPFATM03") //( !IsInCallStack("U_BZAPI001") .Or. !IsInCallStack("U_BPFATM03") ) //SSA-CAD
		If Empty(M->A1_GRPVEN)
			Help(,,ProcName(),,'O c�digo do grupo de clientes deve ser informado. Por favor, providencie o preenchimento do campo o mais breve poss�vel.',1,5)
		EndIf
	EndIf

	If !lRet .and. nModulo = 9 .and. MsgYesNo("Existem inconsist�ncias no cadastro do cliente. Deseja continuar assim mesmo?")
		lRet := .T.
	Endif

	If lRet .and. (u_PCMA144D() .AND. M->A1_MSBLQL <> "1") //Inativo
		msgInfo(u_PCMA144E(M->A1_COD, M->A1_LOJA))
		lRet := .F.
    EndIf

	//envio de hist�rico de bloqueio
	if lRet .and. ALTERA
		u_PCMA144C()
	endIf

	RestArea(aAreaSA1)
Endif

Return lRet


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Ciro Pedreira
// Modulo   : Faturamento / Call Center
// Fun��o   : A1VldPV
// Descri��o: Verifica se houve altera��o em alguns campos do cadastro de
//          : clientes referentes a impostos.
// Retorno  : Logico (.T. = permite alteracao do cliente; .F. = bloqueia a
//          : altera��o do cliente).
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 27/03/17 | Ciro Pedreira     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
Static Function A1VldPV(cCli241, cLoja241)

Local aArea    := GetArea()
Local lRet     := .T.
Local aCpo241  := {}
Local cCpo241  := ''
Local aRet240  := {}
Local nC
Local cMsg     := ''

Default cCli241  := M->A1_COD
Default cLoja241 := M->A1_LOJA

// Lista de campos que ser�o validados.
AAdd(aCpo241, 'A1_TIPO')
AAdd(aCpo241, 'A1_CONTRIB')
AAdd(aCpo241, 'A1_RECCOFI')
AAdd(aCpo241, 'A1_RECPIS')
AAdd(aCpo241, 'A1_RECCSLL')
AAdd(aCpo241, 'A1_RECIRRF')
AAdd(aCpo241, 'A1_ABATIMP')
AAdd(aCpo241, 'A1_NATUREZ')
AAdd(aCpo241, 'A1_GRPTRIB')

For nC := 1 To Len(aCpo241)
	If M->&(aCpo241[nC]) # SA1->&(aCpo241[nC])
		cCpo241 += AllTrim(RetTitle(aCpo241[nC])) + CRLF
		lRet := .F.
	EndIf
Next nC

If !lRet // Caso tenha alterado algum campo.
	aRet240 := A1PedAbe(cCli241, cLoja241)
	If !aRet240[1] // Caso tenha pedidos de venda em aberto.

	cMsg := 'A manuten��o do(s) campo(s) abaixo est� bloqueada, pois h� pedido(s) em aberto para este cliente.'+;
	CRLF+CRLF+;
	cCpo241+;
	CRLF+;
	'Quantidade de pedido(s) em aberto: ' + AllTrim(Str(aRet240[2]))

			Help(,,ProcName(),,cMsg,1,5)
	Else
		lRet := .T. // Se n�o houver pedido em aberto, libera a altera��o do cliente.
	EndIf
EndIf

RestArea(aArea)

Return lRet


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Ciro Pedreira
// Modulo   : Faturamento / Call Center
// Fun��o   : A1PedAbe
// Descri��o: Retorna se ha pedidos de venda em aberto para determinado cliente.
// Retorno  : Logico (.T. = N�o h� pedidos em aberto; .F. = H� pedidos em aberto)
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 24/03/17 | Ciro Pedreira     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
Static Function A1PedAbe(cCli240, cLoja240)

Local aArea     := GetArea()
Local aRet      := {.T., 0}
Local cQuery    := ""
Local cAliasTRB := GetNextAlias()

cQuery := "SELECT COUNT(PEDIDO) PEDIDO " + CRLF
cQuery += "FROM ( " + CRLF
cQuery += "	SELECT DISTINCT SC6.C6_NUM PEDIDO " + CRLF
cQuery += "	FROM " + RetSqlName('SC6') + " SC6 " + CRLF
cQuery += " inner join " + RetSQLName("SC5") + " SC5 with (noLock) " + CRLF
cQuery += " on SC5.D_E_L_E_T_ = ' ' " + CRLF
cQuery += " and SC5.C5_FILIAL  = SC6.C6_FILIAL " + CRLF
cQuery += " and SC5.C5_NUM     = SC6.C6_NUM " + CRLF
cQuery += " and SC5.C5_TIPO    NOT IN ('B', 'D') "
cQuery += "	WHERE SC6.D_E_L_E_T_ = ' ' " + CRLF
cQuery += "	AND SC6.C6_CLI     = '" + cCli240 + "' " + CRLF
cQuery += "	AND SC6.C6_LOJA    = '" + cLoja240 + "' " + CRLF
cQuery += "	AND SC6.C6_QTDVEN  > SC6.C6_QTDENT " + CRLF
cQuery += " AND SC6.C6_BLQ     NOT IN ('R', 'S') "
cQuery += ") A "

DbUseArea(.T., "TOPCONN", TCGenQry(,, cQuery), cAliasTRB, .F., .T.)

If (cAliasTRB)->(!EOF() .and. PEDIDO > 0)
	aRet[1] := .F.
	aRet[2] := (cAliasTRB)->PEDIDO
EndIf
(cAliasTRB)->(DbCloseArea())

RestArea(aArea)

Return aRet


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Materiais
// Fun��o   : A1VldFis
// Descri��o: Valida os campos fiscais do cliente.
// Retorno  : L�gico, validando ou n�o o processamento.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 14/12/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
Static Function A1VldFis()

Local lRet       := .T.

// Se for constru��o civil, verifica se � n�o contribuinte.
// N�o valida caso seja ExecAuto.
If !IsInCallStack("msExecAuto")
	If M->A1_SATIV1 = '12 '  // 12 - Constru��o civil.
		If lRet .and. M->A1_CONTRIB <> "2"  // 2-N�o.
			lRet := MsgYesNo("Esse cliente est� cadastrado como constru��o civil, por�m o campo 'contribuinte' est� como sim. Deseja prosseguir mesmo assim?", "Aten��o")
		Endif
		If lRet .and. M->A1_IENCONT <> "N"
			lRet := MsgYesNo("Esse cliente est� cadastrado como constru��o civil, por�m o campo 'destaca IE' est� como sim. Deseja prosseguir mesmo assim?", "Aten��o")
		Endif
	Endif
Endif

Return lRet
