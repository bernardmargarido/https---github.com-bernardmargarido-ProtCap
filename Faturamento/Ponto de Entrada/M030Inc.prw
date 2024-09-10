#include "protcap.ch"
#include "WISStatus.ch"
#include "fwmvcdef.ch"

// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Materiais
// Função   : M030Inc
// Descrição: P.E. após a inclusão dos dados do cliente (MATA030).
// Retorno  : Lógico, validando ou não o processamento.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 06/12/13 | Felipe Raposo     | Desenvolvimento da rotina.
// 29/03/19 | Wilson A. Silva Jr| Inclusao de cópia dos contatos.
// ---------+-------------------+------------------------------------------------
User Function M030Inc()

Local aAreaAtu	:= GetArea()
Local aAreaAC8	:= AC8->(GetArea())
Local lRet		:= .T.
Local lBotOk	:= (ParamIXB == 0)  // Se o usuário clicou no botão Ok do cadastro. Se o usuário cancelar a tela, esta variavel vem com o conteudo 3.
Local lPedNet	:= IsInCallStack("U_WSVTEX30")  // Se o cadastro do cliente está sendo gerado pelo portal NetSuprimentos.
Local lCasaEPI	:= (cFilAnt $ CASADOEPI)
Local lCopia	:= Type("_cCliAnt") == "C" .And. Type("_cLojAnt") == "C" .And. !Empty(_cCliAnt) .And. !Empty(_cLojAnt)
Local lJobInt	:= SuperGetMV("WIS_JOBINT", NIL, .T.)
Local cGrupo	:= ""

//Envia e-mail de inclusão de cliente PJ da NetSuprimentos para a célula de cadastro
If SA1->A1_PESSOA == "J" .And. "VTEX." $ AllTrim(Upper(SA1->A1_EMAIL))
	//u_PCMA144F(SA1->A1_COD, SA1->A1_LOJA)
	U_BPFATM02(SA1->A1_COD, SA1->A1_LOJA)
EndIf

// Verifica se o usuário confirmou a tela de cadastro.
If lBotOk
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

	If !Empty(cGrupo) .and. !lPedNet
		//Acerta o tipo do grupo
		U_FIXGRPVE(cGrupo)
	EndIf
Endif

//Insere data de inclusão do cliente para controle de alteração e integração com a ferramenta SSA-CAD
Reclock("SA1",.F.)
SA1->A1_MSEXP := dtos(dDataBase)
SA1->(MsUnlock())

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
