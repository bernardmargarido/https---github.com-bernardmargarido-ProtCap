#INCLUDE "PROTHEUS.CH"
#INCLUDE "FWMVCDEF.CH"
#INCLUDE "SHELL.CH"

#DEFINE XTITULO 	"Cadastro de Relatorios (Querys) x Grupos de Usuarios"
//-------------------------------------------------------------------
/*/{Protheus.doc} uRepMod
Cadastro de Relatorios x Grupos de Usuarios

@author  Guilherme Santos
@since   04/04/2024
@version 12.1.2210
/*/
//-------------------------------------------------------------------
User Function uRepMod()
	Local oBrowse 	:= BrowseDef()

	oBrowse:Activate()

Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} BrowseDef
Definicoes do Browse

@author  Guilherme Santos
@since   04/04/2024
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Static Function BrowseDef()
	Local oBrowse 	:= FwMBrowse():New()

	oBrowse:SetAlias("PB2")
	oBrowse:SetDescription(XTITULO)
	oBrowse:SetFilterDefault( U_uRepFilt() )

Return oBrowse
//-------------------------------------------------------------------
/*/{Protheus.doc} MenuDef
Definicoes das Rotinas do Menu

@author  Guilherme Santos
@since   04/04/2024
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Static Function MenuDef()
	Local aRotinas	:= {}
	Local lAdmin	:= PswAdmin(NIL, NIL, RetCodUsr()) == 0

	ADD OPTION aRotinas TITLE 'Pesquisar'  	ACTION 'PesqBrw'			OPERATION OP_PESQUISAR ACCESS 0

	If lAdmin
		ADD OPTION aRotinas TITLE 'Visualizar' ACTION 'VIEWDEF.UREPMOD'	OPERATION MODEL_OPERATION_VIEW ACCESS 0
		ADD OPTION aRotinas TITLE 'Incluir'    ACTION 'VIEWDEF.UREPMOD'	OPERATION MODEL_OPERATION_INSERT ACCESS 0
		ADD OPTION aRotinas TITLE 'Alterar'    ACTION 'VIEWDEF.UREPMOD'	OPERATION MODEL_OPERATION_UPDATE ACCESS 0
		ADD OPTION aRotinas TITLE 'Excluir'    ACTION 'VIEWDEF.UREPMOD'	OPERATION MODEL_OPERATION_DELETE ACCESS 0
	EndIf

	ADD OPTION aRotinas TITLE 'Executar'  	ACTION 'U_uRepExec'			OPERATION MODEL_OPERATION_VIEW ACCESS 0

Return aRotinas
//-------------------------------------------------------------------
/*/{Protheus.doc} ModelDef
Definicao do Modelo dos Dados

@author  Guilherme Santos
@since   04/04/2024
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Static Function ModelDef()
	Local oModel	:= MPFormModel():New("X_UREPMOD", /*bPreVld*/, /*bPosVld*/, /*bCommit*/, /*bCancel*/)
	Local oStrPB2	:= FWFormStruct(1, "PB2")
	Local oStrPB3	:= FWFormStruct(1, "PB3")
	Local oStrPB4	:= FWFormStruct(1, "PB4")

	oModel:SetDescription(XTITULO)

	oModel:AddFields("PB2CAB", NIL, oStrPB2, /*bPreLoad*/, /*bPost*/, /*bLoadHead*/)
	oModel:SetPrimaryKey({"PB2_FILIAL", "PB2_ID"})
	oModel:GetModel("PB2CAB"):SetDescription(XTITULO)

	oStrPB3:SetProperty("PB3_ID", MODEL_FIELD_INIT, FwBuildFeature(STRUCT_FEATURE_INIPAD, "FwFldGet('PB2_ID')"))
	oStrPB4:SetProperty("PB4_ID", MODEL_FIELD_INIT, FwBuildFeature(STRUCT_FEATURE_INIPAD, "FwFldGet('PB2_ID')"))
	oStrPB4:SetProperty("PB4_ORDEM", MODEL_FIELD_INIT, FwBuildFeature(STRUCT_FEATURE_INIPAD, "StrZero(U_UREPORD(), TamSX3('PB4_ORDEM')[1])"))

	oStrPB3:AddTrigger("PB3_GRPUSR", "PB3_GRPDES", {|| .T.}, {|| GatGrpName()})

	oModel:AddGrid("PB3GRID", "PB2CAB", oStrPB3, /*bLinePre*/, /*bLinePost*/, /*bPreGrd*/, /*bPosValid*/, /*bLoadGrid*/)
	oModel:SetRelation("PB3GRID", {{"PB3_FILIAL", "xFilial('PB3')"}, {"PB3_ID", "PB2_ID"}} , PB3->(IndexKey(1)))			//PB3_FILIAL+PB3_ID+PB3_GRPUSR
	oModel:GetModel("PB3GRID"):SetDescription("Grupos de Usuarios")
	oModel:GetModel("PB3GRID"):SetOptional(.F.)

	oModel:AddGrid("PB4GRID", "PB2CAB", oStrPB4, /*bLinePre*/, /*bLinePost*/, /*bPreGrd*/, /*bPosValid*/, /*bLoadGrid*/)
	oModel:SetRelation("PB4GRID", {{"PB4_FILIAL", "xFilial('PB4')"}, {"PB4_ID", "PB2_ID"}} , PB4->(IndexKey(1)))			//PB4_FILIAL+PB4_ID+PB4_ORDEM
	oModel:GetModel("PB4GRID"):SetDescription("Perguntas do Relatorio")
	oModel:GetModel("PB4GRID"):SetOptional(.T.)

Return oModel
//-------------------------------------------------------------------
/*/{Protheus.doc} ViewDef
Definicao do Interface dos Dados

@author  Guilherme Santos
@since   04/04/2024
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Static Function ViewDef()
	Local oModel	:= FWLoadModel("UREPMOD")
	Local oView		:= FWFormView():New()
	Local oStrPB2	:= FWFormStruct(2, "PB2")
	Local oStrPB3	:= FWFormStruct(2, "PB3")
	Local oStrPB4	:= FWFormStruct(2, "PB4")
	Local lIncrem	:= oModel:GetOperation() == MODEL_OPERATION_INSERT .OR. oModel:GetOperation() == MODEL_OPERATION_UPDATE

	oView:SetModel(oModel)

	oView:AddField("VIEW_PB2", oStrPB2, "PB2CAB")
	oView:AddGrid("VIEW_PB3", oStrPB3, "PB3GRID")
	oView:AddGrid("VIEW_PB4", oStrPB4, "PB4GRID")

	oView:CreateHorizontalBox('HB_PB2', 45)
	oView:CreateHorizontalBox('HB_GRD', 55)

	oView:CreateVerticalBox('HB_PB3', 35, 'HB_GRD')
	oView:CreateVerticalBox('HB_PB4', 65, 'HB_GRD')

	oView:SetOwnerView('VIEW_PB2', 'HB_PB2')
	oView:SetOwnerView('VIEW_PB3', 'HB_PB3')
	oView:SetOwnerView('VIEW_PB4', 'HB_PB4')

	oView:SetViewProperty("VIEW_PB3", "GRIDSEEK", {.T.})
	oView:SetViewProperty("VIEW_PB4", "GRIDSEEK", {.T.})

	If lIncrem
		oView:AddIncrementField("VIEW_PB4", "PB4_ORDEM")
	EndIf

Return oView
//-------------------------------------------------------------------
/*/{Protheus.doc} GatGrpName
Gatilho para alimentacao da Descricao do Grupo de Usuarios

@author  Guilherme Santos
@since   04/04/2024
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Static Function GatGrpName()
	Local oModel	:= FWModelActive()
	Local oSubPB3	:= oModel:GetModel("PB3GRID")
	Local cGrpID	:= oSubPB3:GetValue("PB3_GRPUSR")
	Local aGroups	:= FWSFAllGrps()
	Local nPosGrp	:= AScan(aGroups, {|x| AllTrim(x[2]) == AllTrim(cGrpID)})
	Local cRetorno	:= If(nPosGrp > 0, aGroups[nPosGrp][04], "")
Return cRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} IniOrdem
Retorna a Linha para o Inic Padrao da Ordem

@author  Guilherme Santos
@since   04/04/2024
@version 12.1.2210
/*/
//-------------------------------------------------------------------
User Function UREPORD()
	Local oModel	:= FWModelActive()
	Local oSubPB4	:= oModel:GetModel("PB4GRID")
	Local nRetorno	:= oSubPB4:GetLine() + 1
Return nRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} uRepFilt
Filtros do Browse

@author  Guilherme Santos
@since   04/04/2024
@version 12.1.2210
/*/
//-------------------------------------------------------------------
User Function uRepFilt()
	Local cRetorno := ""
	Local lAdmin	:= PswAdmin(NIL, NIL, RetCodUsr()) == 0
	Local aGrpUsr	:= UsrRetGrp(UsrRetName(RetCodUsr()), RetCodUsr())
	Local nGrupo	:= 0
	Local cGrupos	:= ""

	If !lAdmin
		If Len(aGrpUsr) > 0
			For nGrupo := 1 to Len(aGrpUsr)
				cGrupos += "'" + aGrpUsr[nGrupo] + "'"
				If nGrupo < Len(aGrpUsr)
					cGrupos += ","
				EndIf
			Next nGrupo
		Else
			cGrupos += "''"
		EndIf

		cRetorno += "@PB2_FILIAL = '" + xFilial("PB2") + "' "
		cRetorno += "AND EXISTS ( SELECT PB3.PB3_ID "
		cRetorno += "FROM " + RetSqlName("PB3") + " PB3 (NOLOCK) "
		cRetorno += "WHERE PB3.PB3_FILIAL = '" + xFilial("PB3") + "' " 
		cRetorno += "AND PB3.PB3_ID = PB2_ID "
		cRetorno += "AND PB3.PB3_GRPUSR IN (" + cGrupos  + ") "
		cRetorno += "AND PB3.D_E_L_E_T_ = '') "
	EndIf

Return cRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} uRepExec
Execucao do Relatorio e envio para o serviço do Python

@author  Guilherme Santos
@since   05/04/2024
@version 12.1.2210
/*/
//-------------------------------------------------------------------
User Function uRepExec()
	Local oJson 	:= uQryJson():New()
	Local oReport	:= XLSPython():New()
	Local cQuery	:= AllTrim(PB2->PB2_QUERY)
	Local cCpoAct	:= AllTrim(PB2->PB2_ACENTO)
	Local aParam	:= GetParam()
	Local aRetPar	:= {}
	Local nParam	:= 0
	Local cStrPar	:= ""
	Local cStrRes	:= ""
	Local lRetorno	:= .T.

	If Len(aParam) > 0
		If ParamBox(aParam, "Parametros", @aRetPar, NIL, NIL, .F., NIL, NIL, NIL, NIL, .F., .F.)

			//Substitui os parametros digitados no ParamBox na Query
			For nParam := 1 to Len(aRetPar)
				cStrPar := "%MV_PAR" + StrZero(nParam, 2) + "%"

				Do Case
				Case ValType(aRetPar[nParam]) == "N"
					cStrRes := AllTrim(Str(aRetPar[nParam]))
				Case ValType(aRetPar[nParam]) == "D"
					cStrRes := DtoS(aRetPar[nParam])
				Otherwise
					cStrRes := aRetPar[nParam]
				EndCase

				cQuery := StrTran(cQuery, cStrPar, cStrRes)
			Next nParam
		Else
			lRetorno := .F.
		EndIf
	Else
		If !MsgYesNo("Confirma a execução do Relatório?")
			lRetorno := .F.
		EndIf
	EndIf

	If lRetorno
		oJson:SetQuery(cQuery)

		MsAguarde({|| lRetorno := oJson:SetJson(cCpoAct)}, "Selecionando dados para geração do Relatorio...")

		If lRetorno
			oReport:SetJson(oJson:GetJson())

			MsAguarde({|| lRetorno := oReport:SendData()}, "Enviando dados para geracao do Relatorio...")

			If lRetorno
				cUrlRet := oReport:GetURLDown()
				ShellExecute("open", cUrlRet, "", "", SW_SHOWMAXIMIZED)
			Else
				Aviso("SendData", "Erro na geracao do Relatorio em XLS:" + CRLF + oReport:GetErro(), {"Fechar"})
			EndIf
		Else
			Aviso("SetJson", "Erro ao Formatar o Resultado da Query.", {"Fechar"})
		EndIf
	EndIf

	FreeObj(oJson)
	FreeObj(oReport)

Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} GetParam
Retorna os parametros cadastrados para a Execucao da Query

@author  Guilherme Santos
@since   05/04/2024
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Static Function GetParam()
	Local aParam	:= {}
	Local cQuery	:= ""
	Local cTabQry	:= ""
	Local cIDRep	:= PB2->PB2_ID
	Local nTpPar	:= 0
	Local xVar		:= NIL
	Local aCombo	:= NIL
	Local nTam		:= NIL
	Local aAux		:= {}
	Local cPict		:= ""

	cQuery += "SELECT	PB4.PB4_ORDEM" + CRLF
	cQuery += ",		PB4.PB4_DESCRI" + CRLF
	cQuery += ",		PB4.PB4_TIPO" + CRLF
	cQuery += ",		PB4.PB4_TAM" + CRLF
	cQuery += ",		PB4.PB4_DEC" + CRLF
	cQuery += ",		PB4.PB4_F3" + CRLF
	cQuery += ",		PB4.PB4_CMBOPC" + CRLF
	cQuery += ",		PB4.PB4_VALID" + CRLF
	cQuery += "FROM		" + RetSqlName("PB4") + " PB4 (NOLOCK)" + CRLF
	cQuery += "WHERE	PB4.PB4_FILIAL = '" + xFilial("PB4") + "'" + CRLF
	cQuery += "AND		PB4.PB4_ID = '" + cIDRep + "'" + CRLF
	cQuery += "AND		PB4.D_E_L_E_T_ = ''" + CRLF
	cQuery += "ORDER BY PB4.PB4_ORDEM" + CRLF

	cTabQry := MPSysOpenQuery(cQuery)

	While !(cTabQry)->(Eof())

		nTpPar 	:= 1
		cPict	:= ""
		xVar	:= NIL
		aCombo	:= NIL
		nTam	:= ((cTabQry)->PB4_TAM + (cTabQry)->PB4_DEC) * 10
		aAux	:= {}

		//C=Caractere;N=Numerico;L=Logico;D=Data;B=ComboBox
		Do Case
		Case (cTabQry)->PB4_TIPO == "C"
			xVar := Space((cTabQry)->PB4_TAM)
		Case (cTabQry)->PB4_TIPO == "N"
			cPict	:= "@E " + Replicate("9", (cTabQry)->PB4_TAM) + If((cTabQry)->PB4_DEC > 0, "." + Replicate("9", (cTabQry)->PB4_DEC), "")
			xVar 	:= Transform(0, cPict)
		Case (cTabQry)->PB4_TIPO == "D"
			xVar := CtoD("")
		Case (cTabQry)->PB4_TIPO == "B"
			nTpPar	:= 2
			nTam	:= 70
			aCombo	:= StrToKArr((cTabQry)->PB4_CMBOPC, ";")

			If Len(aCombo) > 0
				xVar	:= Substr(aCombo[1], 1, 1)		//Passa sempre a primeira opcao como Default
			Else
				xVar	:= Space(1)
			EndIf
		EndCase

		If nTpPar == 2
			aAux := {nTpPar, (cTabQry)->PB4_DESCRI, xVar, aCombo, nTam, (cTabQry)->PB4_VALID, .F.}
		Else
			aAux := {nTpPar, (cTabQry)->PB4_DESCRI, xVar, cPict, (cTabQry)->PB4_VALID, (cTabQry)->PB4_F3, NIL, nTam, .F.}
		EndIf

		Aadd(aParam, aClone(aAux))

		(cTabQry)->(DbSkip())
	End

Return aParam
