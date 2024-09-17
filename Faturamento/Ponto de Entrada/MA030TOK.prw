#include "protcap.ch"
#include "WISStatus.ch"
#include "fwmvcdef.ch"

// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Materiais
// Função   : MA030TOK
// Descrição: P.E. executado para validar a alteração ou inclusão de cliente.
// Retorno  : Lógico, validando ou não o processamento.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 16/09/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
// 04/01/17 | Francisco Oliveira| Manutenção para chamada de função da fabrica de
//          |                   | software U_F0100101 - integração WMS Winthor
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

	// Validação para verificar se há pedidos de venda em aberto.
	If lRet .And. ALTERA
		lRet := A1VldPV(M->A1_COD, M->A1_LOJA)
	Endif

	// Campo CNAE obrigatório para pessoa jurídica ou produtor rural
	If lRet .and. (Empty(M->A1_CNAE) .Or. Val(M->A1_CNAE) == 0)
		If (M->A1_PESSOA == 'J' .Or. M->A1_TIPO == "L")
		    Help(,,ProcName(),,'Por favor, informe o campo: ' + AllTrim(RetTitle("A1_CNAE")) + '.',1,5)
			lRet := .F.
		EndIf
	Endif

	If lRet
		// Cliente do tipo Revendedor precisa ser contribuinte.
		If M->A1_TIPO == 'R' .And. M->A1_CONTRIB # '1'
			Help(,,ProcName(),,'Para tipo Revendedor é obrigatório preenchimento da I.E. e contribuinte SIM.',1,5)
			lRet := .F.
		EndIf
	EndIf

	If lRet
		If !Empty(M->A1_XCLICRD)
			If SubStr(M->A1_XCLICRD, 1, TamSx3('A1_COD')[1]) # M->A1_COD
				Help(,,ProcName(),,'O codigo do cliente utilizado para consumo do crédito não pode ser diferente do codigo principal.',1,5)
				lRet := .F.
			EndIf
		EndIf
	EndIf

	//Tratamento do campo Grupo de venda na inclusão manual
	if lRet .and. INCLUI .and. !IsInCallStack("U_BPFATM03") //( !IsInCallStack("U_BZAPI001") .Or. !IsInCallStack("U_BPFATM03") ) //SSA-CAD
		//Se não encontrar, deve incluir o grupo.
		if Empty(M->A1_GRPVEN) .and. M->A1_EST <> "EX"
			Help(,,ProcName(),,'O código do grupo de clientes é obrigatório.',1,5)
			lRet := .F.
		endIf
	endIf

	If lRet .and. ALTERA .and. !IsInCallStack("U_BPFATM03") //( !IsInCallStack("U_BZAPI001") .Or. !IsInCallStack("U_BPFATM03") ) //SSA-CAD
		If Empty(M->A1_GRPVEN)
			Help(,,ProcName(),,'O código do grupo de clientes deve ser informado. Por favor, providencie o preenchimento do campo o mais breve possível.',1,5)
		EndIf
	EndIf

	If !lRet .and. nModulo = 9 .and. MsgYesNo("Existem inconsistências no cadastro do cliente. Deseja continuar assim mesmo?")
		lRet := .T.
	Endif

	If lRet .and. (u_PCMA144D() .AND. M->A1_MSBLQL <> "1") //Inativo
		msgInfo(u_PCMA144E(M->A1_COD, M->A1_LOJA))
		lRet := .F.
    EndIf

	//envio de histórico de bloqueio
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
// Função   : A1VldPV
// Descrição: Verifica se houve alteração em alguns campos do cadastro de
//          : clientes referentes a impostos.
// Retorno  : Logico (.T. = permite alteracao do cliente; .F. = bloqueia a
//          : alteração do cliente).
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

// Lista de campos que serão validados.
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

	cMsg := 'A manutenção do(s) campo(s) abaixo está bloqueada, pois há pedido(s) em aberto para este cliente.'+;
	CRLF+CRLF+;
	cCpo241+;
	CRLF+;
	'Quantidade de pedido(s) em aberto: ' + AllTrim(Str(aRet240[2]))

			Help(,,ProcName(),,cMsg,1,5)
	Else
		lRet := .T. // Se não houver pedido em aberto, libera a alteração do cliente.
	EndIf
EndIf

RestArea(aArea)

Return lRet


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Ciro Pedreira
// Modulo   : Faturamento / Call Center
// Função   : A1PedAbe
// Descrição: Retorna se ha pedidos de venda em aberto para determinado cliente.
// Retorno  : Logico (.T. = Não há pedidos em aberto; .F. = Há pedidos em aberto)
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
// Função   : A1VldFis
// Descrição: Valida os campos fiscais do cliente.
// Retorno  : Lógico, validando ou não o processamento.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 14/12/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
Static Function A1VldFis()

Local lRet       := .T.

// Se for construção civil, verifica se é não contribuinte.
// Não valida caso seja ExecAuto.
If !IsInCallStack("msExecAuto")
	If M->A1_SATIV1 = '12 '  // 12 - Construção civil.
		If lRet .and. M->A1_CONTRIB <> "2"  // 2-Não.
			lRet := MsgYesNo("Esse cliente está cadastrado como construção civil, porém o campo 'contribuinte' está como sim. Deseja prosseguir mesmo assim?", "Atenção")
		Endif
		If lRet .and. M->A1_IENCONT <> "N"
			lRet := MsgYesNo("Esse cliente está cadastrado como construção civil, porém o campo 'destaca IE' está como sim. Deseja prosseguir mesmo assim?", "Atenção")
		Endif
	Endif
Endif

Return lRet
