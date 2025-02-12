#INCLUDE "PROTHEUS.CH"
#INCLUDE "APWEBSRV.CH"
#INCLUDE "TOPCONN.CH"
#INCLUDE "TBICONN.CH"

#DEFINE CRLF CHR(13) + CHR(10)

Static cCodInt	:= "03A"
Static cDescInt	:= "PRODUTOS"
Static cDirImp	:= "/ecommerce/"
Static cDirSave	:= "produtos/"

/*******************************************************************************************/
/*/{Protheus.doc} AECOI03A
	@description Envia as Especificações 
	@author Bernard M. Margarido
	@since 22/01/2018
	@version 1.0
	@type function
/*/
/*******************************************************************************************/
User Function AECOI03A()

Private cThread		:= Alltrim(Str(ThreadId()))
Private cStaLog		:= "0"
Private cArqLog		:= ""	

Private nQtdInt		:= 0

Private cHrIni		:= Time()
Private dDtaInt		:= Date()

Private aMsgErro	:= {}

//------------------------------+
// Inicializa Log de Integracao |
//------------------------------+
MakeDir(cDirImp)
cArqLog := cDirImp + "ESPECIFICACAO" + cEmpAnt + cFilAnt + ".LOG"
LogExec("")	
LogExec(Replicate("-",80))
LogExec("INICIA INTEGRACAO ESPECIFICACAO DE PRODUTOS COM A VTEX - DATA/HORA: "+DTOC( DATE() )+" AS "+TIME())

//---------------------------------------+
// Inicia processo de envio dos produtos |
//---------------------------------------+
Processa({|| AECOINT3A() },"Aguarde...","Consultando Produtos.")

LogExec("FINALIZA INTEGRACAO ESPECIFICACAO DE PRODUTOS COM A VTEX - DATA/HORA: "+DTOC( DATE() )+" AS "+TIME())
LogExec(Replicate("-",80))
LogExec("")

//----------------------------------+
// Envia e-Mail com o Logs de Erros |
//----------------------------------+
If Len(aMsgErro) > 0
	cStaLog := "1"
	u_AEcoMail(cCodInt,cDescInt,aMsgErro)
EndIf

Return Nil

/**************************************************************************************************/
/*/{Protheus.doc} AECOINT3A
	@description	Rotina consulta e envia Produtos para a pataforma e-Commerce
	@author			Bernard M.Margarido
	@version   		1.00
	@since     		10/02/2016
/*/
/**************************************************************************************************/
Static Function AECOINT3A()
Local aArea			:= GetArea()

Local cCodPai 		:= ""
Local cNomePrd		:= ""
Local cCampo		:= ""

Local cAlias		:= GetNextAlias()

Local nIdCpo		:= 0 
Local nIdProd		:= 0
Local nToReg		:= 0

Local _oJSon 		:= Nil 
//Local _oValue		:= Nil 
//---------------------------------------------+
// Valida se existem produtos a serem enviadas |
//---------------------------------------------+
If !AEcoQry(cAlias,@nToReg)
	aAdd(aMsgErro,{"002","NAO EXISTEM REGISTROS PARA SEREM ENVIADOS."})  
	RestArea(aArea)
	Return .T.
EndIf

//-----------------------------+
// Inicia o envio das produtos |
//-----------------------------+
ProcRegua(nToReg)
While (cAlias)->( !Eof() )
	
	//-----------------------------------+
	// Incrementa regua de processamento |
	//-----------------------------------+
	IncProc("Produtos " + Alltrim((cAlias)->CODIGO) + " - " + Alltrim((cAlias)->NOME) )

					
	//----------------------+
	// Dados da Produto Pai |
	//----------------------+
	cCodPai 	:= (cAlias)->CODIGO
	cNomePrd	:= (cAlias)->NOME
	nIdProd		:= (cAlias)->IDPROD
	nIdCpo		:= (cAlias)->IDESPECIFICO
	cCampo		:= RTrim((cAlias)->NOME_CAMPO)
	cDesCampo	:= RTrim((cAlias)->DESC_ESPE)
	
	LogExec("ENVIANDO CAMPOS ESPECIFICACOS PRODUTO " + Alltrim((cAlias)->CODIGO) + " - " + Alltrim((cAlias)->NOME) )

	_oJSon				:= Nil 
	_oJSon				:= JSonObject():New()
	_oJSon['id']		:= nIdCpo
	_oJSon['name']		:= cCampo
	_oJSon['Value']		:= {}
	aAdd(_oJSon['Value'],cDesCampo)

	//aAdd(_oJSon,_oAdd)

	cRest			:= _oJSon:ToJson()		
	cRest			:= "[" + cRest + "]"
	//-----------------------------------------+
	// Rotina realiza o envio para a ecommerce |
	//-----------------------------------------+
	AEcoEnv(cCodPai,cNomePrd,nIdProd,cCampo,cDesCampo)
	
	(cAlias)->( dbSkip() )
				
EndDo

//----------------------------+
// Encerra arquivo temporario |
//----------------------------+
(cAlias)->( dbCloseArea() )

RestArea(aArea)
Return .T.

/**************************************************************************************************/
/*/{Protheus.doc} AEcoEnv
	@description	Rotina envia dados do produto para a plataforma e-commerce
	@author			Bernard M.Margarido
	@version   		1.00
	@since     		02/02/2016
	@type function
/*/
/**************************************************************************************************/
Static Function AEcoEnv(cCodPai,cNomePrd,nIdProd,cCampo,cDesCampo)
						
Local aArea			:= GetArea()

Local _oVTEX 		:= VTEX():New()
Local _oJSon 		:= Nil 

Private cType		:= ""

LogExec("ENVIANDO PRODUTO " + cCodPai + " - " + Alltrim(cNomePrd) + " ." )

//--------------------------------+
// Cria diretorio caso nao exista |
//--------------------------------+
MakeDir(cDirImp)
MakeDir(cDirImp + cDirSave)
MemoWrite(cDirImp + cDirSave + "\jsonproduto_specification_" + RTrim(cCodPai) + ".json",cRest)

//---------------------+
// Parametros de envio | 
//---------------------+
_oVTEX:cMetodo		:= "POST"
_oVTEX:cJSon		:= cRest
_oVTEX:cID			:= cValToChar(nIdProd)

If _oVTEX:ProductSpecification()
	
	_oJSon := JSonObject():New()
	_oJSon:FromJson(_oVTEX:cJSonRet)
	If ValType(_oJSon) <> "U"
		LogExec("CAMPOS ESPECIFICOS DO PRODUTO " + cCodPai + " - " + Alltrim(cNomePrd) + " . ENVIADA COM SUCESSO." )
	Else
		If ValType(_oVTEX:cError) <> "U"
			aAdd(aMsgErro,{cCodPai,"ERRO AO ENVIAR CAMPOS ESPECIFICOS DO PRODUTO " + Alltrim(cCodPai) + " - " + Upper(Alltrim(cNomePrd)) + ". ERROR: " + RTrim(_oVTEX:cError)})
			LogExec("ERRO AO ENVIAR CAMPOS ESPECIFICOS DO PRODUTO " + Alltrim(cCodPai) + " - " + Upper(Alltrim(cNomePrd)) + ". ERROR: " +  RTrim(_oVTEX:cError))
		Else 
			aAdd(aMsgErro,{cCodPai,"ERRO AO ENVIAR CAMPOS ESPECIFICOS DO PRODUTO " + Alltrim(cCodPai) + " - " + Upper(Alltrim(cNomePrd)) + ". "})
			LogExec("ERRO AO ENVIAR CAMPOS ESPECIFICOS DO PRODUTO " + Alltrim(cCodPai) + " - " + Upper(Alltrim(cNomePrd)) + ". ")
		EndIf 
	EndIf	
Else
	If ValType(_oVTEX:cError) <> "U"
		aAdd(aMsgErro,{cCodPai,"ERRO AO ENVIAR CAMPOS ESPECIFICOS DO PRODUTO " + Alltrim(cCodPai) + " - " + Upper(Alltrim(cNomePrd)) + ". ERROR: " + RTrim(_oVTEX:cError)})
		LogExec("ERRO AO ENVIAR CAMPOS ESPECIFICOS DO PRODUTO " + Alltrim(cCodPai) + " - " + Upper(Alltrim(cNomePrd)) + ". ERROR: " +  RTrim(_oVTEX:cError))
	Else 
		aAdd(aMsgErro,{cCodPai,"ERRO AO ENVIAR CAMPOS ESPECIFICOS DO PRODUTO " + Alltrim(cCodPai) + " - " + Upper(Alltrim(cNomePrd)) + ". "})
		LogExec("ERRO AO ENVIAR CAMPOS ESPECIFICOS DO PRODUTO " + Alltrim(cCodPai) + " - " + Upper(Alltrim(cNomePrd)) + ". ")
	EndIf 
EndIf

FreeObj(_oVTEX)
FreeObj(_oJSon)

RestArea(aArea)
Return .T.

/**************************************************************************************************/
/*/{Protheus.doc} AECOQRY
	@description 	Rotina consulta os produtos a serem enviados para a pataforma e-Commerce
	@author			Bernard M.Margarido
	@version   		1.00
	@since     		10/02/2016
/*/			
/**************************************************************************************************/
Static Function AEcoQry(cAlias,nToReg,_cLojaID)
Local cQuery 		:= ""

Default _cLojaID	:= ""

//-----------------------------+
// Query consulta produtos pai |
//-----------------------------+
cQuery := " SELECT " + CRLF
cQuery += "		CODIGO, " + CRLF
cQuery += "		NOME, " + CRLF
cQuery += "		IDPROD, " + CRLF
cQuery += "		COD_CAMPO, " + CRLF
cQuery += "		NOME_CAMPO, " + CRLF
cQuery += "		IDESPECIFICO, " + CRLF
cQuery += "		DESC_ESPE, " + CRLF
cQuery += "		RECNOZTG " + CRLF
cQuery += " FROM " + CRLF
cQuery += " ( " + CRLF
cQuery += "		SELECT " + CRLF 
cQuery += "			B4.B4_COD CODIGO, " + CRLF 
cQuery += "			B4.B4_DESC NOME, " + CRLF
cQuery += "			B4.B4_XIDPRD IDPROD, " + CRLF
cQuery += "			ZTE.ZTE_COD COD_CAMPO, " + CRLF     
cQuery += "			ZTE.ZTE_NOME NOME_CAMPO, " + CRLF 
cQuery += "			ZTE.ZTE_IDCAMP IDESPECIFICO, " + CRLF
cQuery += "			ZTE.ZTE_DESC DESC_ESPE, " + CRLF 
cQuery += "			ZTE.R_E_C_N_O_ RECNOZTG " + CRLF
cQuery += "		FROM " + CRLF
cQuery += "		" + RetSqlName("ZTG") + " ZTG " + CRLF 
cQuery += "		INNER JOIN " + RetSqlName("SB1") + " B1 ON B1.B1_FILIAL = '" + xFilial("SB1") + "' AND B1.B1_COD = ZTG.ZTG_PRODUT AND B1.D_E_L_E_T_ = '' " + CRLF
cQuery += "		INNER JOIN " + RetSqlName("SB4") + " B4 ON B4.B4_FILIAL = '" + xFilial("SB4") + "' AND B4.B4_COD = SUBSTRING(B1.B1_COD,1,10) AND B4.D_E_L_E_T_ = '' " + CRLF
cQuery += "		INNER JOIN " + RetSqlName("ZTF") + " ZTF ON ZTF.ZTF_FILIAL = '" + xFilial("ZTF") + "' AND ZTF.ZTF_COD = ZTG.ZTG_VALUE AND ZTG.D_E_L_E_T_ = '' " + CRLF
cQuery += "		INNER JOIN " + RetSqlName("ZTE") + " ZTE ON ZTE.ZTE_FILIAL = '" + xFilial("ZTE") + "' AND ZTE.ZTE_COD = ZTG.ZTG_ESPECI AND ZTG.D_E_L_E_T_ = '' " + CRLF 
cQuery += "	WHERE " + CRLF
cQuery += "		ZTG.ZTG_FILIAL = '" + xFilial("ZTG") + "' AND  " + CRLF 
cQuery += "		ZTG.ZTG_PRDSKU = '1' AND " + CRLF
cQuery += "		ZTG.D_E_L_E_T_ = '' " + CRLF
cQuery += " ) CAMPOS_ESPECIFICOS " + CRLF
cQuery += " ORDER BY CODIGO "

dbUseArea(.T.,"TOPCONN",TcGenQry(,,cQuery),cAlias,.T.,.T.)
Count To nToReg  

//------------------------------+
// Quantidade de Itens enviados |
//------------------------------+
nQtdInt := nToReg

dbSelectArea(cAlias)
(cAlias)->( dbGoTop() )

If (cAlias)->( Eof() )
	(cAlias)->( dbCloseArea() )
	Return .F.
EndIf

Return .T.

/*********************************************************************************/
/*/{Protheus.doc} LogExec
	@description Grava Log do processo 
	@author SYMM Consultoria
	@since 26/01/2017
	@version undefined
	@type function
/*/
/*********************************************************************************/
Static Function LogExec(cMsg)
	CONOUT(cMsg)
	LjWriteLog(cArqLog,cMsg)
Return .T.
