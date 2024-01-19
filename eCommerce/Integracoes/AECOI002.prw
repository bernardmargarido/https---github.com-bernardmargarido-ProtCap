#INCLUDE "PROTHEUS.CH"
#INCLUDE "APWEBSRV.CH"
#INCLUDE "TOPCONN.CH"
#INCLUDE "TBICONN.CH"

#DEFINE CRLF CHR(13) + CHR(10)

Static cCodInt	:= "ZTD"
Static cDescInt	:= "Marcas"
Static cDirImp	:= "/ecommerce/"
Static cDirSave	:= "marcas/"

/**************************************************************************************************/
/*/{Protheus.doc} AECOI002
	@description	Rotina realiza a integração das Marcas/Fabricantes para o ecommerce
	@type   		Function 
	@author			Bernard M.Margarido
	@version   		1.00
	@since     		02/02/2016
/*/
/**************************************************************************************************/
User Function AECOI002()
		
Private cThread	:= Alltrim(Str(ThreadId()))
Private cStaLog	:= "0"
Private cArqLog	:= ""	

Private nQtdInt	:= 0

Private cHrIni	:= Time()
Private dDtaInt	:= Date()

Private aMsgErro:= {}

//------------------------------+
// Inicializa Log de Integracao |
//------------------------------+
MakeDir(cDirImp)
cArqLog := cDirImp + "MARCAS" + cEmpAnt + cFilAnt + ".LOG"
LogExec("")	
LogExec(Replicate("-",80))
LogExec("INICIA INTEGRACAO DE MARCAS COM A VTEX - DATA/HORA: "+DTOC(DATE())+" AS "+TIME())

//-----------------------------------------+
// Inicia processo de envio das categorias |
//-----------------------------------------+
Processa({|| AECOINT02() },"Aguarde...","Consultando as Categorias.")

LogExec("FINALIZA INTEGRACAO DE MARCAS COM A VTEX - DATA/HORA: "+DTOC(DATE())+" AS "+TIME())
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
/*/{Protheus.doc} AECOINT02
	@description	Rotina consulta e envia Marcas para a pataforma e-Commerce
	@author			Bernard M.Margarido
	@version   		1.00
	@since     		02/02/2016
/*/
/**************************************************************************************************/
Static Function AECOINT02()
Local aArea			:= GetArea()

Local _cCodMarca	:= ""
Local _cName		:= ""
Local _cText	 	:= ""
Local _cSiteTitle	:= ""
Local _cKeyWords	:= ""
Local _cStatus 		:= ""

Local cAlias		:= GetNextAlias()

Local nToReg		:= 0
Local _nIdVTex		:= 0
Local _nRecno 		:= 0

Local oJSon 		:= Nil 

//-------------------------------------------+
// Valida se existem marcas a serem enviadas |
//-------------------------------------------+
If !AEcoQry(cAlias,@nToReg)
	LogExec("NAO EXISTEM REGISTROS PARA SEREM ENVIADOS.")
	RestArea(aArea)
	Return .T.
EndIf

//---------------------------------------+
// Inicia o envio das Marcas/Fabricantes |
//---------------------------------------+
ProcRegua(nToReg)
While (cAlias)->( !Eof() )

	//-----------------------------------+
	// Incrementa regua de processamento |
	//-----------------------------------+
	IncProc("Marcas / Fabricantes " + (cAlias)->ZTD_COD + " " + (cAlias)->ZTD_NOME )

	//-----------------------------+
	// Dados da Marcas/Fabricantes |
	//-----------------------------+

	_cCodMarca 			:= (cAlias)->ZTD_COD
	_cName				:= (cAlias)->ZTD_NOME
	_cText	 			:= (cAlias)->ZTD_TITULO
	_cSiteTitle			:= (cAlias)->ZTD_TITULO
	_cKeyWords			:= (cAlias)->ZTD_KWORD
	_cStatus 			:= "1"

	_nIdVTex			:= (cAlias)->ZTD_IDLV
	_nRecno 			:= (cAlias)->RECNOZTD

	oJSon				:= Nil 
	oJSon				:= JSonObject():New()
	
	oJSon["Name"]		:= _cName
	oJSon["Keywords"]	:= Lower(_cKeyWords)
	oJSon["SiteTitle"]	:= _cSiteTitle
	oJSon["Active"]		:= IIF(_cStatus == "1", .T., .F.)

	_cRest				:= oJSon:ToJson()

	LogExec("ENVIANDO MARCA " + _cCodMarca + " - " + Upper(_cName) )

	//---------------------------------------+
	// Rotina realiza o envio para a Rakuten |
	//---------------------------------------+
	AEcoEnv(_cRest,_cName,_cCodMarca,_nIdVTex,_nRecno)

	(cAlias)->( dbSkip() )

EndDo

//----------------------------+
// Encerra arquivo temporario |
//----------------------------+
(cAlias)->( dbCloseArea() )

RestArea(aArea)
Return .T.

/************************************************************************************/
/*/{Protheus.doc} AECOENV
	@description	Rotina envia dados da caegoria para a plataforma e-commerce
	@author			Bernard M.Margarido
	@version   		1.00
	@since     		02/02/2016
/*/				
/************************************************************************************/
Static Function AEcoEnv(_cRest,_cName,_cCodMarca,_nIdVTex,_nRecno)
Local aArea			:= GetArea()

Local cChave		:= ""
Local cPolitica		:= ""
Local cStatus		:= ""
Local cMsgErro		:= ""

Local nIDVtex		:= 0
Local nRegRep		:= 0
Local nIdLV			:= 0

Local _oVTEX 		:= VTEX():new()
Local _oJSon 		:= Nil 

//--------------------------------+
// Cria diretorio caso nao exista |
//--------------------------------+
MakeDir(cDirImp)
MakeDir(cDirImp + cDirSave)
MemoWrite(cDirImp + cDirSave + "\jsonmarca_" + RTrim(_cCodMarca) + ".json",_cRest)

//---------------------+
// Parametros de envio | 
//---------------------+
_oVTEX:cMetodo		:= IIF(_nIdVTex > 0, "PUT", "POST")
_oVTEX:cJSon		:= _cRest
_oVTEX:cID			:= cValToChar(_nIdVTex)

If _oVTEX:Marca()

	//--------------------+
	// Posiciona Registro |
	//--------------------+
	ZTD->( dbGoTo(_nRecno) )

	LogExec("ENVIANDO MARCA " + _cCodMarca + " - " + _cName + " ." )

	_oJSon := JSonObject():New()
	_oJSon:FromJson(_oVTEX:cJSonRet)
	If ValType(_oJSon) <> "U"

		LogExec("MARCA " + _cCodMarca + " - " + _cName + " . ENVIADA COM SUCESSO." )

		RecLock("ZTD",.F.)
			ZTD->ZTD_IDVTX		:= _oJSon['Id']
			ZTD->ZTD_INTLV		:= '2'
		ZTD->( MsUnLock() )	

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
		nIDVtex		:= _nIdVTex

		LogExec("ERRO AO ENVIAR A MARCA " + _cCodMarca + " - " + _cName + " . ERROR: " + RTrim(_oVTEX:cError) )
		aAdd(aMsgErro,{_cCodMarca,"ERRO AO ENVIAR A MARCA " + _cCodMarca + " - " + _cName + " . ERROR: " + RTrim(_oVTEX:cError)})

	EndIf	
Else
	If ValType(_oVTEX:cError) <> "U"
		
		//----------------+
		// Parametros LOG |
		//----------------+
		cStatus		:= "2"
		cMsgErro	:= RTrim(_oVTEX:cError)
		nIDVtex		:= _nIdVTex

		LogExec("ERRO AO ENVIAR A MARCA " + _cCodMarca + " - " + _cName + " . ERROR: " + RTrim(_oVTEX:cError) )
		aAdd(aMsgErro,{_cCodMarca,"ERRO AO ENVIAR A MARCA " + _cCodMarca + " - " + _cName + " . ERROR: " + RTrim(_oVTEX:cError)})

	Else 

		//----------------+
		// Parametros LOG |
		//----------------+
		cStatus		:= "2"
		cMsgErro	:= "Sem comunicação com o integrador"
		nIDVtex		:= _nIdVTex

		LogExec("ERRO AO ENVIAR A MARCA " + _cCodMarca + " - " + _cName + " . " )
		aAdd(aMsgErro,{_cCodMarca,"ERRO AO ENVIAR A MARCA " + _cCodMarca + " - " + _cName + " . "})

	EndIf 
EndIf

//---------------+
// Grava LOG ZT0 |
//---------------+
cChave		:= xFilial("ZTD") + _cCodMarca
cPolitica	:= ""
nRegRep		:= 0
nIdLV		:= 0
U_AEcoGrvLog(cCodInt,cDescInt,cStatus,cMsgErro,cChave,cPolitica,nIDVtex,nTenta,nRegRep,nIdLV)

FreeObj(_oVTEX)
FreeObj(_oJSon)

RestArea(aArea)
Return .T.

/*************************************************************************************/
/*/{Protheus.doc} AECOQRY
	@description 	Rotina consulta as marcas/fabricantes para a pataforma e-Commerce
	@author			Bernard M.Margarido
	@version   		1.00
	@since     		02/02/2016
/*/			
/*************************************************************************************/
Static Function AEcoQry(cAlias,nToReg,_cLojaID)
Local cQuery 		:= ""

Default _cLojaID	:= ""

//---------------------------------+
// Query consulta Marcas/Fabricane |
//---------------------------------+
cQuery := "	SELECT " + CRLF 
cQuery += "		ZTD_COD, " + CRLF 
cQuery += "		ZTD_DESC, " + CRLF 
cQuery += "		ZTD_IDLV, " + CRLF 
cQuery += "		ZTD_NOME, " + CRLF 
cQuery += "		ZTD_KWORD, " + CRLF  
cQuery += "		ZTD_TITULO, " + CRLF 
cQuery += "		ZTD_LCMPC, " + CRLF 
cQuery += "		ZTD_REMAR, " + CRLF 
cQuery += "		ZTD.R_E_C_N_O_ RECNOZTD " + CRLF 
cQuery += " FROM " + CRLF 
cQuery += "		" + RetSqlName("ZTD") + " ZTD " + CRLF 
cQuery += " WHERE " + CRLF 
cQuery += "		ZTD.ZTD_FILIAL = '" + xFilial("ZTD") + "' AND " + CRLF 
cQuery += "		ZTD.ZTD_INTLV = '1' AND " + CRLF 
cQuery += "		ZTD.D_E_L_E_T_ = '' "

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
	@type function
/*/
/*********************************************************************************/
Static Function LogExec(cMsg)
	CONOUT(cMsg)
	LjWriteLog(cArqLog,cMsg)
Return .T.
