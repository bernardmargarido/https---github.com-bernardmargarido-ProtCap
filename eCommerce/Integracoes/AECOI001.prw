#INCLUDE "PROTHEUS.CH"
#INCLUDE "APWEBSRV.CH"
#INCLUDE "TOPCONN.CH"
#INCLUDE "TBICONN.CH"

#DEFINE CRLF CHR(13) + CHR(10)

Static cCodInt	:= "ACU"
Static cDescInt	:= "Categorias"
Static cDirImp	:= "/ecommerce/"
Static cDirSave	:= "categorias/"

/**************************************************************************************************/
/*/{Protheus.doc} AECOI001
	@description	Rotina realiza a integração das categorias para o ecommerce
	@type   		Function 
	@author			Bernard M.Margarido
	@version   		1.00
	@since     		02/02/2016
/*/
/**************************************************************************************************/
User Function AECOI001()
Private cThread			:= Alltrim(Str(ThreadId()))
Private cStaLog			:= "0"
Private cArqLog			:= ""	

Private nQtdInt			:= 0

Private cHrIni			:= Time()
Private dDtaInt			:= Date()

Private aMsgErro		:= {}

//------------------------------+
// Inicializa Log de Integracao |
//------------------------------+
MakeDir(cDirImp)
cArqLog := cDirImp + "CATEGORIAS" + cEmpAnt + cFilAnt + ".LOG"
LogExec("")	
LogExec(Replicate("-",80))
LogExec("INICIA INTEGRACAO DE CATEGORIAS COM A VTEX - DATA/HORA: "+DTOC(DATE())+" AS "+TIME())

//-----------------------------------------+
// Inicia processo de envio das categorias |
//-----------------------------------------+
Processa({|| AECOINT01() },"Aguarde...","Consultando as Categorias.")


LogExec("FINALIZA INTEGRACAO DE CATEGORIAS COM A VTEX - DATA/HORA: "+DTOC(DATE())+" AS "+TIME())
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
/*/{Protheus.doc} AECOINT01
	@description	Rotina consulta e envia categorias para a pataforma e-Commerce
	@author			Bernard M.Margarido
	@version   		1.00
	@since     		02/02/2016
/*/
/**************************************************************************************************/
Static Function AECOINT01()
Local aArea		:= GetArea()

Local _cCodCat 	:= ""
Local _cCodPai 	:= ""	
Local _cName 	:= ""
Local _cKeyWords:= ""
Local _cTitle	:= ""
Local _cDescript:= ""
Local _cMetaTag	:= ""
Local _cStatus	:= ""
Local _cRest	:= ""

Local cAlias	:= GetNextAlias()

Local nToReg	:= 0
Local _nIdVTex	:= 0
Local _nIdPVtex	:= 0
Local _nRecno 	:= 0

Local oJSon 	:= Nil 

//-----------------------------------------------+
// Valida se existem categorias a serem enviadas |
//-----------------------------------------------+
If !AEcoQry(cAlias,@nToReg)
	LogExec("NAO EXISTEM REGISTROS PARA SEREM ENVIADOS.")
	RestArea(aArea)
	Return .T.
EndIf

//-------------------------------+
// Inicia o envio das categorias |
//-------------------------------+
ProcRegua(nToReg)
While (cAlias)->( !Eof() )

	//-----------------------------------+
	// Incrementa regua de processamento |
	//-----------------------------------+
	IncProc("Categoria " + (cAlias)->ACU_COD + " " + (cAlias)->ACU_DESC )

	//--------------------+
	// Dados da Categoria |
	//--------------------+
	_cCodCat 	:= RTrim((cAlias)->ACU_COD)
	_cCodPai 	:= RTrim((cAlias)->CODPAI)
	_cName 		:= RTrim((cAlias)->ACU_DESC)
	_cKeyWords	:= RTrim((cAlias)->ACU_XKWORD)
	_cTitle		:= RTrim((cAlias)->ACU_XTITUL)
	_cDescript	:= RTrim((cAlias)->ACU_XTITUL)
	_cMetaTag 	:= RTrim((cAlias)->ACU_XTITUL)
	_cStatus 	:= (cAlias)->ACU_ECFLAG

	_nIdVTex	:= (cAlias)->ACU_XIDLV
	_nIdPVtex	:= (cAlias)->IDVTPAI
	_nRecno 	:= (cAlias)->RECNOACU

	//------------------+
	// Cria objeto JSON |
	//------------------+
	oJSon 									:= Nil 
	oJSon 									:= JSonObject():New()
	
	oJSon["Name"] 							:= _cName
	oJSon["FatherCategoryId"]				:= IIF(_nIdPVtex > 0 , _nIdPVtex, Nil)
	oJSon["Title"]							:= _cTitle
	oJSon["Description"]					:= _cMetaTag
	oJSon["Keywords"]						:= Lower(_cKeyWords)
	oJSon["IsActive"]						:= IIF(_cStatus == "1", .T., .F.)
	oJSon["LomadeeCampaignCode"]			:= Nil 
	oJSon["AdWordsRemarketingCode"]			:= Nil 
	oJSon["ShowInStoreFront"]				:= .T.
	oJSon["ShowBrandFilter"]				:= .T.
	oJSon["ActiveStoreFrontLink"]			:= .T.
	oJSon["Score"]							:= Nil

	//-------------------------------+
	// Converte objeto em texto JSON |
	//-------------------------------+
	_cRest									:= oJSon:ToJson()		

	LogExec("ENVIANDO CATEGORIA " + _cCodCat + " - " + Upper(_cName) )
		
	//-----------------------------------------+
	// Rotina realiza o envio para o ecommerce |
	//-----------------------------------------+
	AEcoEnv(_cRest,_cCodCat,_cName,_nIdVTex,_nIdPVtex,_nRecno)
			
	(cAlias)->( dbSkip() )
	
EndDo

//----------------------------+
// Encerra arquivo temporario |
//----------------------------+
(cAlias)->( dbCloseArea() )

FreeObj(oJSon)

RestArea(aArea)
Return .T.

/************************************************************************************/
/*/{Protheus.doc} AECOENV
	@description	Rotina envia dados da caegoria para a plataforma e-commerce
	@author			Bernard M.Margarido
	@version   		1.00
	@since     		02/02/2016
/*/								
/*************************************************************************************/
Static Function AEcoEnv(_cRest,_cCodCat,_cName,_nIdVTex,_nIdPVtex,_nRecno)
Local _oVTEX 		:= VTEX():New()
Local _oJSon 		:= Nil 

Local cChave		:= ""
Local cPolitica		:= ""
Local cStatus		:= ""
Local cMsgErro		:= ""

Local nIDVtex		:= 0
Local nRegRep		:= 0
Local nIdLV			:= 0

LogExec("ENVIANDO CATEGORIA " + _cCodCat + " - " + _cName + " ." )

//--------------------------------+
// Cria diretorio caso nao exista |
//--------------------------------+
MakeDir(cDirImp)
MakeDir(cDirImp + cDirSave)
MemoWrite(cDirImp + cDirSave + "\jsoncategoria_" + RTrim(_cCodCat) + ".json",_cRest)

//---------------------+
// Parametros de envio | 
//---------------------+
_oVTEX:cMetodo		:= IIF(nIdCat > 0, "PUT", "POST")
_oVTEX:cJSon		:= _cRest
_oVTEX:cID			:= cValToChar(nIdCat)

If _oVTEX:Categoria()

	//--------------------+
	// Posiciona Registro |
	//--------------------+
	ACU->( dbGoTo(nRecno) )

	_oJSon := JSonObject():New()
	_oJSon:FromJson(_oVTEX:cJSonRet)
	If ValType(_oJSon) <> "U"

		RecLock("ACU",.F.)
			ACU->ACU_XINTLV := "2"
			ACU->ACU_XIDLV	:= _oJSon['Id']
		ACU->( MsUnLock() )

		LogExec("CATEGORIA " + _cCodCat + " - " + _cName + " . ENVIADA COM SUCESSO." )	

		//----------------+
		// Parametros LOG |
		//----------------+
		cStatus		:= "1"
		cMsgErro	:= ""
		nIDVtex		:= _oJSon['Id']

	Else
		
		//----------------+
		// Parametros LOG |
		//----------------+
		cStatus		:= "2"
		cMsgErro	:= RTrim(_oVTEX:cError)
		nIDVtex		:= nIdCat

		LogExec("ERRO AO ENVIAR A CATEGORIA " + _cCodCat + " - " + _cName + " . ERRO: " + RTrim(_oVTEX:cError) )
		aAdd(aMsgErro,{_cCodCat,"ERRO AO ENVIAR CATEGORIA " + _cCodCat + " - " + _cName + " . ERRO: " + RTrim(_oVTEX:cError)}) 
	EndIf
Else 
	If ValType(_oVTEX:cError) <> "U"
		
		//----------------+
		// Parametros LOG |
		//----------------+
		cStatus		:= "2"
		cMsgErro	:= RTrim(_oVTEX:cError)
		nIDVtex		:= nIdCat

		LogExec("ERRO AO ENVIAR A CATEGORIA " + _cCodCat + " - " + _cName + " . ERRO: " + RTrim(_oVTEX:cError) )
		aAdd(aMsgErro,{_cCodCat,"ERRO AO ENVIAR CATEGORIA " + _cCodCat + " - " + _cName + " . ERRO: " + RTrim(_oVTEX:cError) }) 
	Else 
		
		//----------------+
		// Parametros LOG |
		//----------------+
		cStatus		:= "2"
		cMsgErro	:= "Sem comunicação com o integrador"
		nIDVtex		:= nIdCat

		LogExec("ERRO AO ENVIAR A CATEGORIA " + _cCodCat + " - " + _cName + " .")
		aAdd(aMsgErro,{_cCodCat,"ERRO AO ENVIAR CATEGORIA " + _cCodCat + " - " + _cName + " ."}) 
	EndIf 
EndIf 

//---------------+
// Grava LOG ZT0 |
//---------------+
cChave		:= xFilial("ACU") + _cCodCat
cPolitica	:= ""
nRegRep		:= 0
nIdLV		:= 0
U_AEcoGrvLog(cCodInt,cDescInt,cStatus,cMsgErro,cChave,cPolitica,nIDVtex,nTenta,nRegRep,nIdLV)

FreeObj(_oVTEX)
FreeObj(_oJSon)

Return .T.

/**************************************************************************************************/
/*/{Protheus.doc} AECOQRY
	@description 	Rotina consulta e envia categorias para a pataforma e-Commerce
	@author			Bernard M.Margarido
	@version   		1.00
	@since     		02/02/2016
/*/
/**************************************************************************************************/
Static Function AEcoQry(cAlias,nToReg)
Local cQuery 		:= ""

//---------------------------+
// Query consulta categorias |
//---------------------------+
cQuery := "	SELECT " + CRLF
cQuery += "		CATEG_PAI.ACU_COD CODPAI, " + CRLF
cQuery += "		CATEG_PAI.ACU_XIDLV IDVTPAI, " + CRLF
cQuery += "		ACU.ACU_FILIAL, " + CRLF
cQuery += "		ACU.ACU_COD, " + CRLF 
cQuery += "		ACU.ACU_DESC, " + CRLF 
cQuery += "		ACU.ACU_ECFLAG, " + CRLF 
cQuery += "		ACU.ACU_XTITUL, " + CRLF
cQuery += "		ACU.ACU_XKWORD, " + CRLF
cQuery += "		ACU.ACU_XLCMPC, " + CRLF
cQuery += "		ACU.ACU_XREMAR, " + CRLF
cQuery += "		ACU.ACU_XIDLV, " + CRLF
cQuery += "		ACU.R_E_C_N_O_ RECNOACU, " + CRLF
cQuery += " FROM " + CRLF 
cQuery += "		" + RetSqlName("ACU") + " ACU " + CRLF
cQuery += "		OUTER APPLY( " + CRLF
cQuery += "			SELECT " + CRLF 
cQuery += "				ACU_PAI.ACU_COD, " + CRLF
cQuery += "				ACU_PAI.ACU_XIDLV " + CRLF
cQuery += "			FROM " + CRLF
cQuery += "				" + RetSqlName("ACU") + " ACU_PAI " + CRLF
cQuery += "			WHERE " + CRLF
cQuery += "				ACU.ACU_FILIAL = '' AND " + CRLF
cQuery += "				ACU_PAI.ACU_COD = ACU.ACU_CODPAI AND " + CRLF
cQuery += "				ACU_PAI.D_E_L_E_T_ = '' " + CRLF
cQuery += "	) CATEG_PAI " + CRLF
cQuery += " WHERE " + CRLF
cQuery += "		ACU.ACU_FILIAL = '" + xFilial("ACU") + "' AND " + CRLF
cQuery += "		ACU.ACU_XINTLV = '1' AND " + CRLF
cQuery += "		ACU.D_E_L_E_T_ = '' " + CRLF
cQuery += " ORDER BY ACU.ACU_CODPAI,ACU.ACU_COD "

dbUseArea(.T.,"TOPCONN",TcGenQry(,,cQuery),cAlias,.T.,.T.)
count To nToReg  

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
	@param cMsg, characters, descricao
	@type function
/*/
/*********************************************************************************/
Static Function LogExec(cMsg)
	CONOUT(cMsg)
	LjWriteLog(cArqLog,cMsg)
Return .T.
