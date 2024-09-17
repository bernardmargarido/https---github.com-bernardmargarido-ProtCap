#include "Protheus.ch"
#include "FWMVCDEF.CH"
#include "protcap.ch"
#include "WISStatus.ch"
/*-------------------------------------------------------------------
    {Protheus.doc} CRMA980cquery
    Ponto de Entrada do Cadastro de Clientes (MVC)
    @param      NÃ£o hÃ¡
    @return     VÃ¡rios. DependerÃ¡ de qual PE estÃ¡ sendo executado.
-------------------------------------------------------------------*/
User Function CRMA980()
    Local aParam	:= Nil
    Local xRet		:= .T.
    Local nOper		:= 0
    Local lIsGrid	:= .F.
    Local cIDPonto	:= ''
    Local cIDModel	:= ''
    Local oObj		:= NIL
	Local lDbg		:= .F.

	If lDbg //Somente mudar para .T. em ambiente de teste/desenv (forca o teste)

		nSA1Rcn := 166969
		RpcSetEnv("01","00")
		TcSqlExec("UPDATE SA1010 SET A1_GRPTRIB = '', A1_MUN = '', A1_CODSIAF = ''WHERE R_E_C_N_O_ = 166969")
		dbSelectArea("AC8")
		dbSelectArea("SA1")
		SA1->(dbSetOrder(1))
		SA1->(dbGoTo(nSA1Rcn))
		CRMA980INC()

	Else

		aParam := PARAMIXB

	EndIf

    If aParam <> NIL

        oObj        := aParam[1]
        cIDPonto    := aParam[2]
        cIDModel    := aParam[3]
        lIsGrid     := (Len(aParam) > 3)
        nOper       := oObj:GetOperation()

		// Ronald, 04-Jan-2023: quando este P.E. e' executado por MsExecAuto (MVC), nao cria as variaveis INCLUI e ALTERA.
		// O tratamento abaixo forca a criacao destas variaveis para nao dar erro de execucao em MsExecAuto.
		If IsBlind()

			Private INCLUI := .F.
			Private ALTERA := .F.

			If nOper == 3
				INCLUI := .T.
			ElseIf nOper == 4
				ALTERA := .T.
			EndIf

		EndIf
    
		If cIDPonto == 'FORMPOS'
            If nOper = 3 .or. nOper = 4
                xRet := CRMA980TOK()
            EndIf
        ElseIf cIDPonto == 'MODELCOMMITTTS'
            If nOper = 3
                xRet := CRMA980INC()
            ElseIf nOper = 4
                CRMA980ALT()
            EndIf
        ElseIf cIDPonto == 'MODELCOMMITNTTS'
            If nOper = 3
                CRMA980PALT()
            EndIf
        EndIf
    EndIf
 
Return xRet



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
// ---------+-------------------+------------------------------------------------
// 19/07/22 | Tanimoto          | Alterado nome de userfunction para novo padrao MVC do CRMA980
// ------------------------------------------------------------------------------
Static Function CRMA980TOK()
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
		if lRet .and. INCLUI .and. !IsInCallStack("U_BPFATM03") //( !IsInCallStack("U_BZAPI001") .And. !IsInCallStack("U_BPFATM03") )//SSA-CAD
			//Se não encontrar, deve incluir o grupo.
			if Empty(M->A1_GRPVEN) .and. M->A1_EST <> "EX"
				Help(,,ProcName(),,'O código do grupo de clientes é obrigatório.',1,5)
				lRet := .F.
			endIf
		endIf

		If lRet .and. ALTERA .and. !IsInCallStack("U_BPFATM03") //( !IsInCallStack("U_BZAPI001") .And. !IsInCallStack("U_BPFATM03") ) //SSA-CAD
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
	If !IsBlind()
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




// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Materiais
// Função   : NEWM030Inc
// Descrição: P.E. após a inclusão dos dados do cliente (MATA030).
// Retorno  : Lógico, validando ou não o processamento.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 06/12/13 | Felipe Raposo     | Desenvolvimento da rotina.
// 29/03/19 | Wilson A. Silva Jr| Inclusao de cópia dos contatos.
// 19/07/22 | Tanimoto          | Alterado nome de userfunction para novo padrao MVC do CRMA980
// ---------+-------------------+------------------------------------------------
Static Function CRMA980INC()
	Local aAreaAtu	:= GetArea()
	Local aAreaAC8	:= AC8->(GetArea())
	Local lRet		:= .T.
	Local lPedNet	:= IsInCallStack("U_WSVTEX30")  // Se o cadastro do cliente está sendo gerado pelo portal NetSuprimentos.
	Local lCasaEPI	:= (cFilAnt $ CASADOEPI)
	Local lCopia	:= Type("_cCliAnt") == "C" .And. Type("_cLojAnt") == "C" .And. !Empty(_cCliAnt) .And. !Empty(_cLojAnt)
	Local lJobInt	:= SuperGetMV("WIS_JOBINT", NIL, .T.)
	Local cGrupo	:= ""
	Local cGrpTrib	:= ""
	Local aDatMun   := {}
	Local cTrt001	:= SuperGetMV("CRMA980001",,"S") //Regrava Grupo de Tributacao? S/N
	Local cTrt002	:= SuperGetMV("CRMA980002",,"S") //Regrava Nome do Municipio? S/N

	//Envia e-mail de inclusão de cliente PJ da NetSuprimentos para a célula de cadastro
	If SA1->A1_PESSOA == "J" .And. ("WSVTEX46" $UPPER(FUNNAME()))
		u_PCMA144F(SA1->A1_COD, SA1->A1_LOJA)
	EndIf

	//WIS - Job de Integracao
	If lJobInt
		U_WISJINC("/interfacewis/entrada/cliente", "SA1", SA1->(Recno()), "U_WISSA1")
	EndIf

	// Se for conversão de prospect, transfere os contatos, contratos e atendimentos TMK.
	If lRet
		If lCopia
			FwMsgRun( ,{|| ReplicaAC8(_cCliAnt+_cLojAnt) }, , "Aguarde... Replicando Contatos..." )
		Else
			lRet := ConvProsp()
		Endif
	Endif

	// Envia e-mail com os novos cadastros de clientes.
	If !lCasaEPI .and. !lPedNet
		U_PROMM144()
	Endif

	//Preenche o CNAE e Ramo de atividade para pessoa física
	If Empty(SA1->A1_CNAE) .And. SA1->A1_PESSOA == "F" .And. SA1->A1_TIPO <> "L"
		If RecLock("SA1", .F.)
			SA1->A1_CNAE := "0000-0/00"
			SA1->A1_SATIV1 := "4029"
			SA1->(MsUnlock())
		EndIf
	EndIf

	//Tratamento do campo Grupo de venda na inclusão manual
	//Busca grupo de venda existente para a raiz do CNPJ. Somente para pessoa jurídica e não Net
	if Empty(SA1->A1_GRPVEN) .And. SA1->A1_PESSOA == 'J' .and. SA1->A1_EST <> "EX" .and. !lPedNet
		cGrupo := U_FNDGRPVE(left(SA1->A1_CGC, 8))
		if !empty(cGrupo)
			recLock('SA1', .F.)
				SA1->A1_GRPVEN := cGrupo
			SA1->(msUnlock())
		endIf
	endIf

	//Regrava Grupo de Tributação, mesmo se retornar vazio de GrpTrib()
	If cTrt001 == "S"
		cGrpTrib := GrpTrib()
		If RecLock("SA1",.F.)
			SA1->A1_GRPTRIB := cGrpTrib
			SA1->(msUnlock())
		EndIf
	EndIf

	//Regrava Nome do Municipio e Codigo SIAF
	If cTrt002 == "S"
		If ( !Empty(SA1->A1_COD_MUN) .And. !Empty(SA1->A1_EST) )
			aDatMun := RDatMun( SA1->A1_COD_MUN, SA1->A1_EST )
			If ( !Empty(aDatMun[1]) .Or. !Empty(aDatMun[2]) )
				If RecLock("SA1",.F.)
					If !Empty(aDatMun[1])
						SA1->A1_MUN := aDatMun[1]
					EndIf
					If !Empty(aDatMun[2])
						SA1->A1_CODSIAF := aDatMun[2]
					EndIf
					SA1->(msUnlock())
				EndIf
			EndIf
		EndIf
	EndIf

	If !Empty(cGrupo) .and. !lPedNet
		//Acerta o tipo do grupo
		U_FIXGRPVE(cGrupo)
	EndIf

	RestArea(aAreaAC8)
	RestArea(aAreaAtu)

Return lRet



// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Materiais
// Função   : ConvProsp
// Descrição: Converte os atendimentos e propostas de contrato, do prospect para
//            o cliente.
// Retorno  : Lógico, validando ou não o processamento.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 06/12/13 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
Static Function ConvProsp()
	Local lRet       := .T.
	Local aArea      := GetArea()
	Local lProcessa  := .F.
	Local cQuery     := ""
	Local cAliasTop  := GetNextAlias()
	Local nPMPgto	 := 0

	If !empty(SA1->A1_CGC)
		// Verifica se o cliente já estava cadastrado como prospect.
		cQuery := "select top 1 SUS.R_E_C_N_O_ SUSRecNo "
		cQuery += "from " + RetSQLName("SUS") + " SUS "
		cQuery += "where SUS.D_E_L_E_T_ = ' ' "
		cQuery += "and SUS.US_FILIAL  = '" + xFilial("SUS") + "' "
		cQuery += "and SUS.US_CGC     = '" + SA1->A1_CGC + "' "
		cQuery += "and SUS.US_CODCLI  = '' "
		cQuery += "and SUS.US_LOJACLI = '' "
		cQuery += "and SUS.US_STATUS  <> '6' "
		dbUseArea(.T., "TOPCONN", TCGenQry(,, cQuery), cAliasTop, .F., .T.)
		If (cAliasTop)->(!eof())
			SUS->(dbGoTo((cAliasTop)->SUSRecNo))  // Posiciona registro.
			lProcessa := .T.
		Endif
		(cAliasTop)->(dbCloseArea())

		// Faz a conversão do prospect.
		If lProcessa

			// Grava o prospect como cliente.
			Reclock("SUS", .F.)
			SUS->US_STATUS  := "6"  // 6 - Cliente.
			SUS->US_CODCLI  := SA1->A1_COD
			SUS->US_LOJACLI := SA1->A1_LOJA
			SUS->US_DTCONV  := dDataBase
			SUS->(msUnLock())

			// Atualizar os relacionamentos.
			AC8->(dbSetOrder(2))  // AC8_FILIAL, AC8_ENTIDA, AC8_FILENT, AC8_CODENT, AC8_CODCON.
		 	Do While AC8->(dbSeek(xFilial() + "SUS" + xFilial("SUS") + SUS->(US_COD + US_LOJA), .F.))
				Reclock("AC8", .F.)
				AC8->AC8_ENTIDA := "SA1"
				AC8->AC8_FILENT := xFilial("SA1")
				AC8->AC8_CODENT := SA1->(A1_COD + A1_LOJA)
				AC8->(msUnLock())
			EndDo

			// Atualizar o banco de conhecimentos.
			AC9->(dbSetOrder(2))  // AC9_FILIAL, AC9_ENTIDA, AC9_FILENT, AC9_CODENT, AC9_CODOBJ.
			Do While AC9->(dbSeek(xFilial() + "SUS" + xFilial("SUS") + SUS->(US_COD + US_LOJA), .F.))
				Reclock("AC9", .F.)
				AC9->AC9_ENTIDA := "SA1"
				AC9->AC9_FILENT := xFilial("SA1")
				AC9->AC9_CODENT := SA1->(A1_COD + A1_LOJA)
				AC9->(msUnLock())
			EndDo

			// Converter propostas de contrato abertas (ZA1).
			cQuery := "select ZA1.R_E_C_N_O_ ZA1RecNo "
			cQuery += "from " + RetSQLName("ZA1") + " ZA1 "
			cQuery += "where ZA1.D_E_L_E_T_ = ' ' "
			// Não filtrar filial, pois todas as filiais devem ser convertidas.
			cQuery += "and ZA1.ZA1_CLIENT = '" + SUS->US_COD + "' "
			cQuery += "and ZA1.ZA1_LOJA   = '" + SUS->US_LOJA + "' "
			cQuery += "and ZA1.ZA1_PROSPE = 'T' "  // Prospect.
			dbUseArea(.T., "TOPCONN", TCGenQry(,, cQuery), cAliasTop, .F., .T.)
			Do While (cAliasTop)->(!eof())
				// Posiciona registro.
				ZA1->(dbGoTo((cAliasTop)->ZA1RecNo))

				Reclock("ZA1", .F.)
				ZA1->ZA1_CLIENT := SA1->A1_COD
				ZA1->ZA1_LOJA   := SA1->A1_LOJA
				ZA1->ZA1_PROSPE := .F.
				ZA1->(msUnLock())

				(cAliasTop)->(dbSkip())
			EndDo
			(cAliasTop)->(dbCloseArea())

			// Converter atendimentos abertos (SUA).
			cQuery := "select SUA.R_E_C_N_O_ SUARecNo "
			cQuery += "from " + RetSQLName("SUA") + " SUA "
			cQuery += "where SUA.D_E_L_E_T_ = ' ' "
			// Não filtrar filial, pois todas as filiais devem ser convertidas.
			cQuery += "and SUA.UA_CLIENTE = '" + SUS->US_COD + "' "
			cQuery += "and SUA.UA_LOJA    = '" + SUS->US_LOJA + "' "
			cQuery += "and SUA.UA_PROSPEC = 'T' "  // Prospect.
			dbUseArea(.T., "TOPCONN", TCGenQry(,, cQuery), cAliasTop, .F., .T.)
			Do While (cAliasTop)->(!eof())
				// Posiciona registro.
				SUA->(dbGoTo((cAliasTop)->SUARecNo))

				// Atualiza o prazo médio de pagamento da cotação, pois não é feito enquanto prospect
				nPMPgto := 0
				nPMPgto := U_XPMPgto(SUA->UA_VALBRUT, SUA->UA_CONDPG, Date())

				Reclock("SUA", .F.)
				SUA->UA_CLIENTE := SA1->A1_COD
				SUA->UA_LOJA    := SA1->A1_LOJA
				SUA->UA_PROSPEC := .F.
				SUA->UA_XPMPGTO := SA1->A1_XPMPGTO
				SUA->UA_XPMPCAL := nPMPgto
				SUA->UA_XDEPVEN := ""
				SUA->(msUnLock())

				(cAliasTop)->(dbSkip())
			EndDo
			(cAliasTop)->(dbCloseArea())
		Endif
	Endif

	RestArea(aArea)

Return lRet

// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Wilson A. Silva Jr.
// Modulo   : Faturamento / Call Center
// Função   : ReplicaAC8
// Descrição: Efetua a cópia dos contatos de um cliente para o outro.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 29/03/19 | Wilson A. Silva Jr| Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
Static Function ReplicaAC8(cCodEnt)
	Local aArea  := GetArea()
	Local cTMP1  := CriaTrab(,.F.)
	Local cQuery := ""
	
	cQuery := " SELECT "+ CRLF
	cQuery += " 	AC8_FILIAL, "+ CRLF
	cQuery += " 	AC8_ENTIDA, "+ CRLF
	cQuery += " 	AC8_CODENT, "+ CRLF
	cQuery += " 	AC8_CODCON "+ CRLF
	cQuery += " FROM "+RetSqlName("AC8")+" AC8 (NOLOCK) "+ CRLF
	cQuery += " WHERE "+ CRLF
	cQuery += " 	AC8_FILIAL = '"+xFilial("AC8")+"' "+ CRLF
	cQuery += " 	AND AC8_ENTIDA = 'SA1' "+ CRLF
	cQuery += " 	AND AC8_CODENT = '"+cCodEnt+"' "+ CRLF
	cQuery += " 	AND AC8.D_E_L_E_T_ = ' ' "+ CRLF
	cQuery += " ORDER BY "+ CRLF
	cQuery += " 	AC8_FILIAL, "+ CRLF
	cQuery += " 	AC8_ENTIDA, "+ CRLF
	cQuery += " 	AC8_CODENT "+ CRLF
	
	IIF(Select(cTMP1)>0,(cTMP1)->(DbCloseArea()),Nil)
	
	DbUseArea(.T.,"TOPCONN",TCGENQRY(,,cQuery),cTMP1,.F.,.T.)
	
	While (cTMP1)->(!EOF())
	
		DbSelectArea("AC8")
		Reclock("AC8",.T.)
			REPLACE AC8_FILIAL WITH (cTMP1)->AC8_FILIAL
			REPLACE AC8_ENTIDA WITH (cTMP1)->AC8_ENTIDA
			REPLACE AC8_CODENT WITH SA1->(A1_COD + A1_LOJA)
			REPLACE AC8_CODCON WITH (cTMP1)->AC8_CODCON
		MsUnLock()
	
		(cTMP1)->(DbSkip())
	EndDo
	
	(cTMP1)->(DbCloseArea())
	
	RestArea(aArea)

Return .T.


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Materiais
// Função   : MALTCLI
// Descrição: P.E. executado após a alteração de cliente.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 16/09/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
// 04/01/17 | Francisco Oliveira| Manutenção para chamada de função da fabrica de
//          |                   | software U_F0100103 - integração WMS Winthor
// ------------------------------------------------------------------------------
Static Function CRMA980ALT()
	Local aAreaSA1   := SA1->(GetArea())

	// Trata campos financeiros do cliente.
	A1PosFin()

	// Trata limite de crédito das lojas.
	A1PosLim()

	// Trata campos fiscais do cliente.
	A1PosFis()

	// Alteração realizada para a customização do EndPoint ERP_CUSTOMER da GTPLAN.
	If ALTERA
		U_PROMA708()		
	EndIf

	If ALTERA .and. SA1->A1_XWINTH == "1"
		U_F0100103()
	Endif

	RestArea(aAreaSA1)

	//GRAVA LOG CLIENTE
	If ALTERA .And. FunName() == "MATA030"
		recLock("SA1",.F.)
		SA1->A1_XDTALT := date()
		SA1->A1_XUSRALT := cUserName
		SA1->(msUnlock())
	EndIf

Return


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Materiais
// Função   : A1PosFin
// Descrição: Trata campos financeiros do cliente.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 14/12/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
Static Function A1PosFin()
	Local aAreaSA1   := {}
	Local cCliCred   := ""
	Local cCliente   := ""

	// Analisa se é para replicar o cliente de crédito para todas as lojas.
	If SA1->(A1_XCLICRD = A1_COD + A1_LOJA)
		aAreaSA1 := SA1->(GetArea())
		cCliCred := SA1->A1_XCLICRD
		cCliente := SA1->(A1_FILIAL + A1_COD)
		SA1->(dbSetOrder(1))  // A1_FILIAL, A1_COD, A1_LOJA.
		SA1->(msSeek(cCliente, .F.))
		Do While SA1->(!eof() .and. A1_FILIAL + A1_COD == cCliente)
			If SA1->(A1_XCLICRD <> cCliCred .or. A1_RISCO <> " ") .and. SA1->(A1_COD + A1_LOJA <> cCliCred) .and. SA1->A1_MSBLQL <> "1"
				RecLock("SA1", .F.)
				SA1->A1_XCLICRD := cCliCred
				SA1->A1_RISCO   := ""
				SA1->(msUnLock())
			ElseIf SA1->A1_MSBLQL = "1"
				RecLock("SA1", .F.)
				SA1->A1_XCLICRD := ""
				SA1->A1_RISCO   := ""
				SA1->A1_LC      := 0
				SA1->A1_VENCLC  := stod("")
				SA1->(msUnLock())
			Endif

			// Grava log de alteração de dados de crédito do cliente.
			A1LogFin()

			SA1->(dbSkip())
		EndDo
		RestArea(aAreaSA1)
	Endif

	RecLock("SA1", .F.)
	SA1->A1_CARGO1 := CriaVar("A1_CARGO1", .T.)
	If SA1->(A1_XCLICRD = A1_COD + A1_LOJA)
		SA1->A1_XCLICRD := ""
	Endif

	If SA1->A1_MSBLQL = "1"
		SA1->A1_XCLICRD := ""
		SA1->A1_RISCO   := ""
		SA1->A1_LC      := 0
		SA1->A1_VENCLC  := stod("")
	Endif
	SA1->(msUnLock())

	// Grava log de alteração de dados de crédito do cliente.
	A1LogFin()

Return

// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Rodrigo Nunes
// Modulo   : Materiais
// Função   : A1PosLim
// Descrição: Trata campos de limite de crédito.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 06/03/17 | Rodrigo Nunes     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
Static Function A1PosLim()
	Local aAreaSA1   := SA1->(GetArea())
	Local cCliCred   := SA1->A1_XCLICRD
	//Local nTamCod    := TamSX3("A1_COD")[1]
	//Local nTamLoj    := TamSX3("A1_LOJA")[1]
	Local cQuery     := ""
	Local cAliasTop  := ""

	If empty(cCliCred)
		If SA1->(A1_LC == 0 .or. A1_MSBLQL == "1")
			cQuery := "select SA1.R_E_C_N_O_ SA1RecNo " + CRLF
			cQuery += "from " + RetSQLName("SA1") + " SA1 " + CRLF
			cQuery += "where SA1.D_E_L_E_T_ = ' ' " + CRLF
			cQuery += "and SA1.A1_FILIAL  = '" + xFilial("SA1") + "' " + CRLF
			cQuery += "and SA1.A1_XCLICRD = '" + SA1->(A1_COD + A1_LOJA) + "' " + CRLF
		Endif
	Else
		SA1->(dbSetOrder(1))  // A1_FILIAL, A1_COD, A1_LOJA.
		If SA1->(msSeek(xFilial() + cCliCred, .F.) .and. (A1_LC == 0 .OR. A1_MSBLQL == "1"))
			cQuery := "select SA1.R_E_C_N_O_ SA1RecNo " + CRLF
			cQuery += "from " + RetSQLName("SA1") + " SA1 " + CRLF
			cQuery += "where SA1.D_E_L_E_T_ = ' ' " + CRLF
			cQuery += "and SA1.A1_FILIAL  = '" + xFilial("SA1") + "' " + CRLF
			cQuery += "and SA1.A1_XCLICRD = '" + cCliCred + "' " + CRLF
		Endif
	Endif

	If !empty(cQuery)

		cAliasTop := MPSysOpenQuery(cQuery)

		Do While (cAliasTop)->(!eof())
			SA1->(dbGoTo((cAliasTop)->SA1RecNo))
			RecLock("SA1", .F.)
			If SA1->A1_RISCO <> "E"
				SA1->A1_RISCO   := ""
			Endif
			SA1->A1_LC      := 0
			SA1->A1_VENCLC  := stod("")
			SA1->A1_XCLICRD := ""
			SA1->(msUnLock())

			// Grava log de alteração de dados de crédito do cliente.
			A1LogFin()

			(cAliasTop)->(dbSkip())
		EndDo
		(cAliasTop)->(dbCloseArea())
	Endif

	RestArea(aAreaSA1)

Return


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Materiais
// Função   : A1LogFin
// Descrição: Grava log de alteração de dados de crédito do cliente.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 29/12/16 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
Static Function A1LogFin()
	Local cQuery     := ""
	Local cAliasTop  := GetNextAlias()

	cQuery += "select top 1 ZE1.ZE1_LC, ZE1.ZE1_VENCLC, ZE1.ZE1_RISCO " + CRLF
	cQuery += "from " + RetSQLName("ZE1") + " ZE1 with (noLock) " + CRLF
	cQuery += "where ZE1.D_E_L_E_T_ = ' ' " + CRLF
	cQuery += "and ZE1.ZE1_FILIAL = '" + xFilial("ZE1") + "' " + CRLF
	cQuery += "and ZE1.ZE1_CLIENT = '" + SA1->A1_COD + "' " + CRLF
	cQuery += "and ZE1.ZE1_LOJA   = '" + SA1->A1_LOJA + "' " + CRLF
	cQuery += "order by ZE1.ZE1_DATA desc, ZE1.R_E_C_N_O_ desc " + CRLF
	dbUseArea(.T., "TOPCONN", TCGenQry(,, cQuery), cAliasTop, .F., .T.)

	// Se a última alteração for diferente da atual, grava um registro.
	If (cAliasTop)->(eof() .or. ZE1_LC <> SA1->A1_LC .or. ZE1_VENCLC <> dtos(SA1->A1_VENCLC) .or. ZE1_RISCO <> SA1->A1_RISCO)
		RecLock("ZE1", .T.)
		ZE1->ZE1_FILIAL := xFilial("ZE1")
		ZE1->ZE1_CLIENT := SA1->A1_COD
		ZE1->ZE1_LOJA   := SA1->A1_LOJA
		ZE1->ZE1_DATA   := Date()
		ZE1->ZE1_USUARI := IIF(IsInCallStack('BZAPI001'),'SSA-CAD',cUserName)
		ZE1->ZE1_LC     := SA1->A1_LC
		ZE1->ZE1_VENCLC := SA1->A1_VENCLC
		ZE1->ZE1_RISCO  := SA1->A1_RISCO
		ZE1->(msUnLock())
	Endif
	(cAliasTop)->(dbCloseArea())

Return


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Materiais
// Função   : A1PosFis
// Descrição: Trata campos financeiros do cliente.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 14/12/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
Static Function A1PosFis()
	Local aCampo     := {}
	Local aSufMun    := {}
	Local nSufMun    := 0

	// Se tiver SUFRAMA, algumas regras devem ser obedecidas.
	If empty(SA1->A1_SUFRAMA)
		aAdd(aCampo, {|| SA1->A1_CODMUN  := ""})
		aAdd(aCampo, {|| SA1->A1_CALCSUF := "N"})  // Não calcula.
	Else
		// Preenche código de município ZF.
		SX5->(dbSetOrder(1))  // X5_FILIAL, X5_TABELA, X5_CHAVE.
		SX5->(dbSeek(xFilial() + "S1", .F.))
		Do While SX5->(!eof() .and. X5_FILIAL + X5_TABELA == xFilial() + "S1")
			SX5->(aAdd(aSufMun, {X5_CHAVE, rtrim(X5_DESCRI)}))
			SX5->(dbSkip())
		EndDo
		nSufMun := aScan(aSufMun, {|x| x[2] == rtrim(SA1->A1_MUN)})
		If nSufMun = 0
			aAdd(aCampo, {|| SA1->A1_CODMUN := ""})
		Else
			aAdd(aCampo, {|| SA1->A1_CODMUN := aSufMun[nSufMun, 1]})
		Endif

		If nSufMun = 0 .or. SA1->A1_TIPO == "F"  // Consumidor final
			aAdd(aCampo, {|| SA1->A1_CALCSUF := "N"})  // Não calcula.
		ElseIf SA1->A1_TIPO == "R"  // Revendedos.
			aAdd(aCampo, {|| SA1->A1_CALCSUF := "I"})  // ICMS.
		Endif
	Endif

	// Atualiza os campos do cliente.
	If !empty(aCampo)
		RecLock("SA1", .F.)
		aEval(aCampo, {|x| Eval(x)})
		SA1->(msUnLock())
	Endif

Return


//-------------------------------------------------------------------
/*/{Protheus.doc} m030palt
PE após gravação do cliente
@author  joao.leao
@since   11/04/2019
@version 12.1.17
/*/
// ---------+-------------------+------------------------------------------------
// 19/07/22 | Tanimoto          | Alterado nome de userfunction para novo padrao MVC do CRMA980
//-------------------------------------------------------------------
Static Function CRMA980PALT()
	Local cGrupo	:= ""
	Local aCarEsp	:= separa(superGetMV('PC_CARESP',,'A1_COMPLEM'),";")
	Local nI		:= 0
	Local aArea		:= getArea()
	Local lJobInt	:= SuperGetMV("WIS_JOBINT", NIL, .T.)
	//Local nOpcao	:= PARAMIXB[1]

	//Preenche o CNAE e Ramo de atividade para pessoa física
	If Empty(SA1->A1_CNAE) .And. SA1->A1_PESSOA == "F" .And. SA1->A1_TIPO <> "L"
		If RecLock("SA1", .F.)
			SA1->A1_CNAE := "0000-0/00"
			SA1->A1_SATIV1 := "4029"
			SA1->(MsUnlock())
		EndIf
	EndIf

	//Tratamento do campo Grupo de venda na inclusão manual
	//Busca grupo de venda existente para a raiz do CNPJ. Somente para pessoa jurídica e não Net
	if Empty(SA1->A1_GRPVEN) .And. SA1->A1_PESSOA == 'J' .and. SA1->A1_EST <> "EX"
		cGrupo := U_FNDGRPVE(left(SA1->A1_CGC, 8))
		if !empty(cGrupo)
			recLock('SA1', .F.)
				SA1->A1_GRPVEN := cGrupo
			SA1->(msUnlock())
		endIf
	endIf

	If !Empty(cGrupo)
		//Acerta o tipo do grupo
		U_FIXGRPVE(cGrupo)
	EndIf

	//remove caracteres especiais de campos que podem gerar erro no arquivo do Sped Fiscal
	for nI := 1 to len(aCarEsp)
		if !empty(&('SA1->' + aCarEsp[nI]))
			recLock('SA1', .F.)
				&('SA1->' + aCarEsp[nI]) := u_bzNoAcento(&('SA1->' + aCarEsp[nI]))
			SA1->(msUnlock())
		endIf
	next nI

	//WIS - Job de Integracao
	If lJobInt
		U_WISJINC("/interfacewis/entrada/cliente", "SA1", SA1->(Recno()), "U_WISSA1")
	EndIf

	restArea(aArea)

return nil


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// Função   : MA030ROT
// Descrição: P.E. na geração do menu da rotina de cadastro de clientes (MATA030).
// Retorno  : Matriz com as rotinas a serem adicionadas no menu.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 19/12/16 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
// 17/08/22 | Glauce			| Alterado nome de userfunction para novo padrao MVC do CRMA980
//-------------------------------------------------------------------------------

USER FUNCTION CRM980MDef()
Local aMenuUsr   := {}
Local lWMSSythex := SuperGetMV("BZ_WMS",, "") == "WIS"
Local lJobInt	 := SuperGetMV("WIS_JOBINT", NIL, .T.)

If !IsBlind()

	If cModulo $ "FIN|FAT|FIS|CFG"
		aAdd(aMenuUsr,{"Conhec. fin.","U_DocsFin"  ,MODEL_OPERATION_VIEW,0})
		aAdd(aMenuUsr,{"Hist. crédito.","U_VLogCrd",MODEL_OPERATION_VIEW,0})
		aAdd(aMenuUsr,{"Hist. bloqueio","U_VLogBlq",MODEL_OPERATION_VIEW,0})
	Endif

	If !IsInCallStack("U_BZJOB001") .AND. PswAdmin(,, RetCodUsr()) = 0
		aAdd(aMenuUsr,{"WMS Envia","U_F0100103"	 ,MODEL_OPERATION_VIEW,0})
	Endif

	If lWMSSythex .AND. lJobInt
		aAdd(aMenuUsr,{"WMS Monitor Integração","U_PROMA652('2')" ,MODEL_OPERATION_VIEW,0})
	EndIf

	aAdd(aMenuUsr,{"Cadastro EDI","U_PROMA640" ,MODEL_OPERATION_VIEW,0})

EndIf

Return aMenuUsr



Static Function RDatMun( cCodMun, cEstado )
Local cQry := ""
Local aRet := {"",""}

cQry := "SELECT "
cQry += "CC2_MUN AS CC2MUN, "
cQry += "CC2_CDSIAF AS CC2CDSIAF "
cQry += "FROM " +RetSqlName("CC2")+ " WHERE D_E_L_E_T_ = ' ' "
cQry += "AND CC2_FILIAL = '" +xFilial("CC2")+ "' "
cQry += "AND CC2_EST = '" +cEstado+ "' "
cQry += "AND CC2_CODMUN = '" +cCodMun+ "'"

Iif(Select("AWRK")>0,AWRK->(dbCloseArea()),Nil)
dbUseArea(.T.,"TOPCONN",TCGenQry(,,cQry),"AWRK",.T.,.T.)
AWRK->(dbGoTop())

If AWRK->(!EoF())
	If !Empty(AWRK->CC2MUN)
		aRet[1] := Alltrim(SubStr(AWRK->CC2MUN,1,30))
	EndIf
	If !Empty(AWRK->CC2CDSIAF)
		aRet[2] := Alltrim(SubStr(AWRK->CC2CDSIAF,1,4))
	EndIf
EndIf
AWRK->(dbCloseArea())

Return(aRet)



Static Function GrpTrib()
Local cQry := ""
Local cRetorno := ""
Local cA1CODSEG := Alltrim(SA1->A1_CODSEG)
Local cA1EST := Alltrim(SA1->A1_EST)
Local cA1CODMUN := Alltrim(SA1->A1_COD_MUN)
Local cA1TIPO := Alltrim(SA1->A1_TIPO)
Local cA1PESSOA := Alltrim(SA1->A1_PESSOA)
Local cA1CONTRIB := Alltrim(SA1->A1_CONTRIB)
Local cZZ7Tbl := RetSqlName("ZZ7")
Local cZZ7Fil := xFilial("ZZ7")

If cA1CODSEG == "01" //ORGAO PUBLICO FEDERAL

	cRetorno := "004"

ElseIf cA1CODSEG == "02" //ORGAO PUBLICO ESTADUAL

	cRetorno := "005"

ElseIf cA1CODSEG == "03" //ORGAO PUBLICO MUNICIPAL

	cRetorno := "011"

Else

	If ( !Empty(cA1EST) .and. !Empty(cA1CODMUN) .and. !Empty(cA1TIPO) .and. !Empty(cA1PESSOA) .and. !Empty(cA1CONTRIB) )

		cQry := "SELECT ZZ7_GRPTRI AS ZZ7GRPTRI "
		cQry += "FROM " +cZZ7Tbl+ " WHERE D_E_L_E_T_ = ' ' "
		cQry += "AND ZZ7_FILIAL = '" +cZZ7Fil+ "' "
		cQry += "AND ZZ7_EST = '" +cA1EST+ "' "
		cQry += "AND ZZ7_CODMUN = '" +cA1CODMUN+ "' "
		cQry += "AND ZZ7_PESSOA = '" +cA1PESSOA+ "' "
		cQry += "AND ZZ7_CONTRI = '" +cA1CONTRIB+ "' "
		cQry += "AND ZZ7_TIPO = '" +cA1TIPO+ "' "

		Iif(Select("AWRK")>0,AWRK->(dbCloseArea()),Nil)
		dbUseArea(.T.,"TOPCONN",TCGenQry(,,cQry),"AWRK",.T.,.T.)
		AWRK->(dbGoTop())

		If AWRK->(!EoF())
			If !Empty(AWRK->ZZ7GRPTRI)
				cRetorno := AWRK->ZZ7GRPTRI
			EndIf
		EndIf
		AWRK->(dbCloseArea())

	EndIf

	If Empty( cRetorno )

		cQry := "SELECT ZZ7_GRPTRI AS ZZ7GRPTRI "
		cQry += "FROM " +cZZ7Tbl+ " WHERE D_E_L_E_T_ = ' ' "
		cQry += "AND ZZ7_FILIAL = '" +cZZ7Fil+ "' "
		cQry += "AND ZZ7_EST = '**' "
		cQry += "AND ZZ7_CODMUN = '*****' "
		cQry += "AND ZZ7_PESSOA = '*' "
		If !Empty(cA1CONTRIB)
			cQry += "AND ZZ7_CONTRI = '" +cA1CONTRIB+ "' "
		EndIf
		If !Empty(cA1TIPO)
			cQry += "AND ZZ7_TIPO = '" +cA1TIPO+ "' "
		EndIf

		Iif(Select("BWRK")>0,BWRK->(dbCloseArea()),Nil)
		dbUseArea(.T.,"TOPCONN",TCGenQry(,,cQry),"BWRK",.T.,.T.)
		BWRK->(dbGoTop())

		If BWRK->(!EoF())
			If !Empty(BWRK->ZZ7GRPTRI)
				cRetorno := BWRK->ZZ7GRPTRI
			EndIf
		EndIf
		BWRK->(dbCloseArea())

	EndIf

	If Empty( cRetorno )
		cRetorno := Space(3)
	EndIf

EndIf

Return(cRetorno)
