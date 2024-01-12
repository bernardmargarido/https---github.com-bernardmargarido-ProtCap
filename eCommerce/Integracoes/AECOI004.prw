#INCLUDE "PROTHEUS.CH"
#INCLUDE "APWEBSRV.CH"
#INCLUDE "TOPCONN.CH"
#INCLUDE "TBICONN.CH"

#DEFINE CRLF CHR(13) + CHR(10)

Static cCodInt	:= "004"
Static cDescInt	:= "SKU"
Static cDirImp	:= "/ecommerce/"
Static cDirSave	:= "sku/"

/**************************************************************************************************/
/*/{Protheus.doc} AECOI004
	@description	Rotina realiza a integração dos produtos filhos (sku) para o ecommerce
	@type   		Function 
	@author			Bernard M.Margarido
	@version   		1.00
	@since     		10/02/2016
/*/
/**************************************************************************************************/
User Function AECOI004()

Private cThread		:= Alltrim(Str(ThreadId()))
Private cStaLog		:= "0"
Private cArqLog		:= ""	

Private nQtdInt		:= 0

Private cHrIni		:= Time()
Private dDtaInt		:= Date()

Private aMsgErro	:= {}

Private lJob 		:= .F.

Private _lMultLj	:= GetNewPar("EC_MULTLOJ",.T.)

Private _oProcess 	:= Nil

//----------------------------------+
// Grava Log inicio das Integrações | 
//----------------------------------+
u_AEcoGrvLog(cCodInt,cDescInt,dDtaInt,cHrIni,,,,,cThread,1)

//------------------------------+
// Inicializa Log de Integracao |
//------------------------------+
MakeDir(cDirImp)
cArqLog := cDirImp + "SKU" + cEmpAnt + cFilAnt + ".LOG"
ConOut("")	
LogExec(Replicate("-",80))
LogExec("INICIA INTEGRACAO DE SKU COM A VTEX - DATA/HORA: "+DTOC(DATE())+" AS "+TIME())

//----------------------------------+
// Inicia processo de envio dos SKU |
//----------------------------------+
Processa({|| AECOINT04() },"Aguarde...","Consultando Sku.")

//Processa({|| U_AECOI04A()},"Aguarde...","Enviando Especificacoes Sku.")

LogExec("FINALIZA INTEGRACAO DE SKU COM A VTEX - DATA/HORA: "+DTOC(DATE())+" AS "+TIME())
LogExec(Replicate("-",80))
ConOut("")

//----------------------------------+
// Envia e-Mail com o Logs de Erros |
//----------------------------------+
If Len(aMsgErro) > 0
	cStaLog := "1"
	u_AEcoMail(cCodInt,cDescInt,aMsgErro)
EndIf

//----------------------------------+
// Grava Log inicio das Integrações |
//----------------------------------+
u_AEcoGrvLog(cCodInt,cDescInt,dDtaInt,cHrIni,Time(),cStaLog,nQtdInt,aMsgErro,cThread,2)

Return Nil

/**************************************************************************************************/
/*/{Protheus.doc} AECOINT04
	@description	Rotina consulta e envia Produtos Filhos (SKU) para a pataforma e-Commerce
	@author			Bernard M.Margarido
	@version   		1.00
	@since     		10/02/2016
/*/
/**************************************************************************************************/
Static Function AECOINT04()
Local aArea		:= GetArea()

Local cCodPai 	:= ""
Local cNomePrd	:= "" 
Local cCodSku	:= ""
Local cNomeSku	:= ""
Local cCodBar	:= ""  
Local cLocal	:= ""
Local cUnidade	:= ""
Local cStatPrd	:= ""
Local cAlias	:= GetNextAlias()

Local nCodMarca	:= 0
Local nPeso		:= 0
Local nPrcVen	:= 0   
Local nAltPrd	:= 0   
Local nAltEmb	:= 0   
Local nProfPrd	:= 0   
Local nProfEmb	:= 0   
Local nLargPrd	:= 0 
Local nLargEmb	:= 0 
Local nIdProd	:= 0   
Local nIdSku	:= 0 
Local nToReg	:= 0
Local nRecno	:= 0

Local oJSon		:= Nil 

//----------------------------------------+
// Valida se existem SKU a serem enviadas |
//----------------------------------------+
If !AEcoQry(cAlias,@nToReg)
	aAdd(aMsgErro,{"004","NAO EXISTEM REGISTROS PARA SEREM ENVIADOS."})  
	RestArea(aArea)
	Return .T.
EndIf

//------------------------+
// Inicia o envio dos SKU |
//------------------------+
ProcRegua(nToReg)
While (cAlias)->( !Eof() )
	
	//-----------------------------------+
	// Incrementa regua de processamento |
	//-----------------------------------+
	IncProc("Sku " + Alltrim((cAlias)->CODSKU) + " - " + Alltrim((cAlias)->NOMESKU) )
			
	//-----------+
	// Dados SKU |
	//-----------+
	cCodPai 	:= (cAlias)->CODIGO
	cNomePrd	:= RTrim((cAlias)->NOMEPAI)
	cCodSku		:= (cAlias)->CODSKU
	cNomeSku	:= RTrim((cAlias)->NOMEPAI) 
	cCodBar		:= RTrim((cAlias)->CODBARRA)  
	cLocal		:= (cAlias)->ARMAZEM
	cUnidade	:= (cAlias)->UNIDADE
	cStatPrd	:= (cAlias)->STATUSPRD
	cYouTube 	:= IIF(Empty((cAlias)->YOUTUBE),"",'https://www.youtube.com/watch?v=' + RTrim((cAlias)->YOUTUBE))
	
	//-----------------------------+   
	// Peso convertido para Gramas |
	//-----------------------------+
	nCodMarca	:= (cAlias)->CODMARCA
	nPeso		:= (cAlias)->PESOLIQ
	nPesBruto	:= (cAlias)->PESOBRUP
	nPrcVen		:= (cAlias)->PRCVEN
	nAltPrd		:= (cAlias)->ALTPRD   
	nAltEmb		:= (cAlias)->ALTEMB   
	nProfPrd	:= (cAlias)->PROPRD   
	nProfEmb	:= (cAlias)->PROEMB   
	nLargPrd	:= (cAlias)->LARGPRD 
	nLargEmb	:= (cAlias)->LARGEMB
	
	//-----------------------
	// ID Produto eCommerce |
	//-----------------------
	nIdProd		:= (cAlias)->IDPROD   
	nIdSku		:= (cAlias)->IDSKU
	nRecno 		:= (cAlias)->RECNOSB5

	
	//------------------------------+
	// Calcula a Cubagem do Produto |
	//------------------------------+
	If nAltEmb > 0 .And. nProfEmb > 0 .And. nLargEmb > 0 
		nCubic := Round(nAltEmb * nProfEmb * nLargEmb,2) 
	Else
		nCubic := Round(nAltPrd * nProfPrd * nLargPrd,2)
	EndIf

	oJSon							:= Nil 
	oJSon 							:= JSonObject():New()

   	oJSon["ProductId"]				:= nIdProd
   	oJSon["IsActive"]				:= .F. //IIF(cStatPrd == "A",.T.,.F.)
   	oJSon["ActivateIfPossible"]		:= .F.
	oJSon["Name"]					:= cNomeSku
	oJSon["RefId"]					:= cCodSku
	oJSon["Ean"]					:= cCodBar
	oJSon["PackagedHeight"]			:= nAltEmb
	oJSon["PackagedLength"]			:= nProfEmb
	oJSon["PackagedWidth"]			:= nLargEmb
	oJSon["PackagedWeightKg"]		:= nPesBruto
	oJSon["Height"]					:= nAltPrd
	oJSon["Length"]					:= nProfPrd
	oJSon["Width"]					:= nLargPrd
	oJSon["WeightKg"]				:= nPeso
	oJSon["CubicWeight"]			:= nCubic
	oJSon["IsKit"]					:= .F.
	oJSon["CreationDate"]			:= IIF(nIdSku > 0, Nil, FwTimeStamp(3))
	oJSon["RewardValue"]			:= 0
	oJSon["EstimatedDateArrival"]	:= Nil 
	oJSon["ManufacturerCode"]		:= Alltrim(Str(nCodMarca))
	oJSon["CommercialConditionId"]	:= Nil 
	oJSon["MeasurementUnit"]		:= cUnidade
	oJSon["UnitMultiplier"]			:= 1
	oJSon["ModalType"]				:= Nil 
	oJSon["KitItensSellApart"]		:= .F.

	If !Empty(cYouTube)
		oJSon["Videos"]				:= {}
		aAdd(oJSon["Videos"],cYouTube)
	EndIf 

	cRest							:= oJSon:ToJson()		

	LogExec("ENVIANDO SKU " + Alltrim((cAlias)->CODSKU) + " - " + Alltrim((cAlias)->NOMESKU) )
		 
	//---------------------------------------+
	// Rotina realiza o envio para a Rakuten |
	//---------------------------------------+
	AEcoEnv(cRest,cCodSku,cNomeSku,nIdProd,nIdSku,nRecno)
	
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
	@description	Rotina envia dados do SKU para a plataforma e-commerce
	@author			Bernard M.Margarido
	@version   		1.00
	@since     		02/02/2016
	@type function
/*/
/**************************************************************************************************/
Static Function AEcoEnv(cRest,cCodSku,cNomeSku,nIdProd,nIdSku,nRecnoSb5)
Local aArea			:= GetArea()

Local _oVTEX 		:= VTEX():New()
Local _oJSon 		:= Nil 

Private cType		:= ''

LogExec("ENVIANDO PRODUTO " + cCodSku + " - " + Alltrim(cNomeSku) + " ." )

//--------------------------------+
// Cria diretorio caso nao exista |
//--------------------------------+
MakeDir(cDirImp)
MakeDir(cDirImp + cDirSave)
MemoWrite(cDirImp + cDirSave + "\jsonsku_" + RTrim(cCodSku) + ".json",cRest)

//---------------------+
// Parametros de envio | 
//---------------------+
_oVTEX:cMetodo		:= IIF(nIdSku > 0, "PUT", "POST")
_oVTEX:cJSon		:= cRest
_oVTEX:cID			:= cValToChar(nIdSku)

If _oVTEX:Sku()
	_oJSon := JSonObject():New()
	_oJSon:FromJson(_oVTEX:cJSonRet)
	If ValType(_oJSon) <> "U"
		
		LogExec("PRODUTO SKU " + cCodSku + " - " + Alltrim(cNomeSku) + " . ENVIADA COM SUCESSO.")

		//---------------+
		// Posiciona SKU |
		//---------------+
		If nRecnoSb5 > 0
			SB5->( dbGoTo(nRecnoSb5) )
			RecLock("SB5",.F.)
				SB5->B5_XENVSKU := "2"
				SB5->B5_XENVECO := "2"
				SB5->B5_XIDSKU	:= _oJSon['Id']
			SB5->( MsUnLock() )	
		EndIf
		LogExec("PRODUTO SKU " + cCodSku + " - " + Alltrim(cNomeSku) + " . ENVIADA COM SUCESSO.")

	Else
		If ValType(_oVTEX:cError) <> "U"
			aAdd(aMsgErro,{cCodSku,"ERRO AO ENVIARSKU " + Alltrim(cCodSku) + " - " + Upper(Alltrim(cNomeSku)) + ". ERROR: " + RTrim(_oVTEX:cError)})
			LogExec("ERRO AO ENVIAR SKU " + Alltrim(cCodSku) + " - " + Upper(Alltrim(cNomeSku)) + ". ERROR: " +  RTrim(_oVTEX:cError))
		Else 
			aAdd(aMsgErro,{cCodSku,"ERRO AO ENVIAR SKU " + Alltrim(cCodSku) + " - " + Upper(Alltrim(cNomeSku)) + ". "})
			LogExec("ERRO AO ENVIAR SKU " + Alltrim(cCodSku) + " - " + Upper(Alltrim(cNomeSku)) + ". ")
		EndIf 
	EndIf	
Else
	If ValType(_oVTEX:cError) <> "U"
		aAdd(aMsgErro,{cCodSku,"ERRO AO ENVIAR SKU " + Alltrim(cCodSku) + " - " + Upper(Alltrim(cNomeSku)) + ". ERROR: " + RTrim(_oVTEX:cError)})
		LogExec("ERRO AO ENVIAR SKU " + Alltrim(cCodSku) + " - " + Upper(Alltrim(cNomeSku)) + ". ERROR: " +  RTrim(_oVTEX:cError))
	Else 
		aAdd(aMsgErro,{cCodSku,"ERRO AO ENVIAR SKU " + Alltrim(cCodSku) + " - " + Upper(Alltrim(cNomeSku)) + ". "})
		LogExec("ERRO AO ENVIAR SKU " + Alltrim(cCodSku) + " - " + Upper(Alltrim(cNomeSku)) + ". ")
	EndIf 
EndIf

FreeObj(_oVTEX)
FreeObj(_oJSon)

RestArea(aArea)
Return .T.

/*****************************************************************************************************/
/*/{Protheus.doc} AECOQRY
	@description 	Rotina consulta os produtos filhos (SKU) a serem enviados para a pataforma e-Commerce
	@author			Bernard M.Margarido
	@version   		1.00
	@since     		10/02/2016
	@return			lRet - Variavel Logica			
/*/
/******************************************************************************************************/
Static Function AEcoQry(cAlias,nToReg,_cLojaID)
Local cQuery 		:= ""

Local cCodTab		:= GetNewPar("EC_TABECO")

Default _cLojaID	:= ""

//-----------------------------+
// Query consulta produtos pai |
//-----------------------------+
cQuery := "	SELECT " + CRLF
cQuery += "		CODIGO, " + CRLF
cQuery += "		NOMEPAI, " + CRLF
cQuery += "		CODSKU, " + CRLF
cQuery += "		NOMESKU, " + CRLF
cQuery += "		CODBARRA, " + CRLF
cQuery += "		ARMAZEM, " + CRLF
cQuery += "		UNIDADE, " + CRLF
cQuery += "		PESOLIQ, " + CRLF
cQuery += "		PESOBRUP, " + CRLF
cQuery += "		ALTPRD, " + CRLF
cQuery += "		ALTEMB, " + CRLF
cQuery += "		PROPRD, " + CRLF
cQuery += "		PROEMB, " + CRLF
cQuery += "		LARGPRD, " + CRLF
cQuery += "		LARGEMB, " + CRLF  
cQuery += "		CODMARCA, " + CRLF
cQuery += "		IDPROD, " + CRLF
cQuery += "		IDSKU, " + CRLF
cQuery += "		STATUSPRD, " + CRLF
cQuery += "		PRCVEN,  " + CRLF
cQuery += "		YOUTUBE,  " + CRLF
cQuery += "		RECNOSB5 " + CRLF
cQuery += "	FROM " + CRLF
cQuery += "	( " + CRLF
cQuery += "		SELECT " + CRLF
cQuery += "			B5.B5_COD CODIGO, " + CRLF
cQuery += "			B5.B5_XNOMPRD NOMEPAI, " + CRLF
cQuery += "			B5.B5_COD CODSKU, " + CRLF
cQuery += "			B1.B1_DESC NOMESKU, " + CRLF
cQuery += "			B1.B1_CODBAR CODBARRA, " + CRLF
cQuery += "			B1.B1_LOCPAD ARMAZEM, " + CRLF
cQuery += "			B1.B1_UM UNIDADE, " + CRLF
cQuery += "			CASE WHEN B5.B5_PESO > 0 THEN B5.B5_PESO ELSE B1.B1_PESO END PESOLIQ, " + CRLF
cQuery += "			B1.B1_PESBRU PESOBRUP, " + CRLF
cQuery += "			CASE WHEN B5.B5_ALTURA > 0 THEN B5.B5_ALTURA ELSE B5.B5_XALTPRD END ALTPRD, " + CRLF
cQuery += "			CASE WHEN B5.B5_ALTURLC > 0 THEN B5.B5_ALTURLC ELSE B5.B5_XALTEMB END ALTEMB, " + CRLF
cQuery += "			CASE WHEN B5.B5_COMPR > 0 THEN B5.B5_COMPR ELSE B5.B5_XPROPRD END PROPRD, " + CRLF
cQuery += "			CASE WHEN B5.B5_COMPRLC > 0 THEN B5.B5_COMPRLC ELSE B5.B5_XPROEMB END PROEMB, " + CRLF
cQuery += "			CASE WHEN B5.B5_LARG > 0 THEN B5.B5_LARG ELSE B5.B5_XLARPRD END LARGPRD, " + CRLF
cQuery += "			CASE WHEN B5.B5_LARGLC > 0 THEN B5.B5_LARGLC ELSE B5.B5_XLAREMB END LARGEMB, " + CRLF
cQuery += "			AY2.AY2_XIDMAR CODMARCA, " + CRLF
cQuery += "			B5.B5_XIDPROD IDPROD, " + CRLF
cQuery += "			B5.B5_XIDSKU IDSKU, " + CRLF
cQuery += "			B5.B5_xSTATUS STATUSPRD, " + CRLF
cQuery += "			ISNULL(DA1.DA1_PRCVEN,0) PRCVEN, " + CRLF  
cQuery += "			B5.B5_XYOUTUB YOUTUBE, " + CRLF  
cQuery += "			B5.R_E_C_N_O_ RECNOSB5 " + CRLF
cQuery += "		FROM " + CRLF 
cQuery += "			" + RetSqlName("SB5") + " B5 " + CRLF   
cQuery += "			INNER JOIN " + RetSqlName("SB1") + " B1 ON B1.B1_FILIAL = '" + xFilial("SB1") + "' AND B1.B1_COD = B5.B5_COD AND B1.B1_MSBLQL <> '1' AND B1.D_E_L_E_T_ = '' " + CRLF
cQuery += "			INNER JOIN " + RetSqlName("AY2") + " AY2 ON AY2.AY2_FILIAL = '" + xFilial("AY2") + "' AND AY2.AY2_CODIGO = B5.B5_XCODMAR AND AY2.D_E_L_E_T_ = '' " + CRLF
cQuery += "			LEFT OUTER JOIN " + RetSqlName("DA1") + " DA1 ON DA1.DA1_FILIAL = '" + xFilial("DA1") + "' AND DA1.DA1_CODTAB = '" + cCodTab + "' AND DA1.DA1_CODPRO = B1.B1_COD AND DA1.D_E_L_E_T_ = '' " + CRLF
cQuery += "		WHERE " + CRLF
cQuery += "			B5.B5_FILIAL = '" + xFilial("SB5") + "' AND " + CRLF  
cQuery += "			B5.B5_XUSAECO = 'S' AND " + CRLF
cQuery += "			B5.B5_XENVECO = '2' AND " + CRLF
cQuery += "			B5.B5_XENVSKU = '1' AND " + CRLF
cQuery += "			B5.D_E_L_E_T_ = ' ' " + CRLF
cQuery += "	) SKU " + CRLF
cQuery += "	ORDER BY CODIGO "

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
