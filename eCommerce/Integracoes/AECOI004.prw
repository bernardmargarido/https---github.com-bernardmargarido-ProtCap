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
Local aArea			:= GetArea()

Local cAlias		:= GetNextAlias()
Local _cCodSku 		:= ""
Local _cName		:= ""		
Local _cCodBar 		:= ""
Local _cCodFabric	:= ""
Local _cStatus		:= ""
Local _cUM 			:= ""
Local _cRest		:= ""

Local _nAltura 		:= 0 
Local _nComp		:= 0 
Local _nLargura 	:= 0 
Local _nAltEmb		:= 0 
Local _nCompEmb 	:= 0 
Local _nLargEmb 	:= 0 
Local _nPeso 		:= 0 
Local _nCubagem 	:= 0 
Local _nIdVTex		:= 0 
Local _nIdProd		:= 0 
Local _nRecno		:= 0 
Local nToReg		:= 0

Local oJSon			:= Nil 

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
	IncProc("Sku " + RTrim((cAlias)->SKU_COD) + " - " + RTrim((cAlias)->SKU_DESC) )

	LogExec("ENVIANDO SKU " + RTrim((cAlias)->SKU_COD) + " - " + RTrim((cAlias)->SKU_DESC) )

	//-----------+
	// Dados SKU |
	//-----------+
	_cCodSku 	:= RTrim((cAlias)->SKU_COD)
	_cName		:= RTrim((cAlias)->SKU_TITULO)
	_cCodBar 	:= RTrim((cAlias)->SKU_EAN)
	_cCodFabric	:= RTrim((cAlias)->SKU_FRABRICANTE)
	_cUM 		:= (cAlias)->SKU_UNIDADE_MEDIDA

	_nAltura 	:= (cAlias)->SKU_ALTURA 
	_nComp		:= (cAlias)->SKU_PROFUNDIDADE
	_nLargura 	:= (cAlias)->SKU_LARGURA 
	_nAltEmb	:= (cAlias)->SKU_ALTURA_EMB 
	_nCompEmb 	:= (cAlias)->SKU_PROFUNDIDADE_EMB
	_nLargEmb 	:= (cAlias)->SKU_LARGURA_EMB
	_nPeso 		:= (cAlias)->SKU_PESO_LIQUIDO
	_nCubagem 	:= 0
	_nIdVTex	:= (cAlias)->SKU_IDVTEX
	_nIdProd	:= (cAlias)->PR_IDVTEX
	_nRecno		:= (cAlias)->SKU_RECNO

	_lStatus 	:= IIF((cAlias)->SKU_BLOQUEIO == '1', .F., (cAlias)->SKU_STATUS <> '2')
	//------------------------------+
	// Calcula a Cubagem do Produto |
	//------------------------------+
	If _nAltEmb > 0 .And. _nCompEmb > 0 .And. _nLargEmb > 0 
		_nCubagem := Round(_nAltEmb * _nCompEmb * _nLargEmb,2) 
	Else
		_nCubagem := Round(_nAltura * _nComp * _nLargura,2)
	EndIf

	oJSon							:= Nil 
	oJSon 							:= JSonObject():New()

   	oJSon["ProductId"]				:= _nIdProd
   	oJSon["IsActive"]				:= _lStatus
   	oJSon["ActivateIfPossible"]		:= _cStatus
	oJSon["Name"]					:= _cName
	oJSon["RefId"]					:= _cCodSku
	oJSon["Ean"]					:= _cCodBar
	oJSon["PackagedHeight"]			:= _nAltEmb
	oJSon["PackagedLength"]			:= _nCompEmb
	oJSon["PackagedWidth"]			:= _nLargEmb
	oJSon["PackagedWeightKg"]		:= _nPeso
	oJSon["Height"]					:= _nAltura
	oJSon["Length"]					:= _nComp
	oJSon["Width"]					:= _nLargura
	oJSon["WeightKg"]				:= _nPeso
	oJSon["CubicWeight"]			:= _nCubagem
	oJSon["IsKit"]					:= .F.
	oJSon["CreationDate"]			:= IIF(_nIdVTex > 0, Nil, FwTimeStamp(3))
	oJSon["RewardValue"]			:= 0
	oJSon["EstimatedDateArrival"]	:= Nil 
	oJSon["ManufacturerCode"]		:= _cCodFabric
	oJSon["CommercialConditionId"]	:= Nil 
	oJSon["MeasurementUnit"]		:= _cUM
	oJSon["UnitMultiplier"]			:= 1
	oJSon["ModalType"]				:= Nil 
	oJSon["KitItensSellApart"]		:= .F.

	/*
	If !Empty(cYouTube)
		oJSon["Videos"]				:= {}
		aAdd(oJSon["Videos"],cYouTube)
	EndIf 
	*/

	_cRest							:= oJSon:ToJson()		
		 
	//---------------------------------------+
	// Rotina realiza o envio para a Rakuten |
	//---------------------------------------+
	AEcoEnv(_cRest,_cCodSku,_cName,_nIdProd,_nIdVTex,_nRecno)
	
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
Static Function AEcoEnv(_cRest,_cCodSku,_cName,_nIdProd,_nIdVTex,_nRecno)
Local aArea			:= GetArea()

Local cChave		:= ""
Local cPolitica		:= ""
Local cStatus		:= ""
Local cMsgErro		:= ""

Local nIDVtex		:= 0
Local nRegRep		:= 0
Local nIdLV			:= 0

Local _oVTEX 		:= VTEX():New()
Local _oJSon 		:= Nil 

Private cType		:= ''

LogExec("ENVIANDO PRODUTO " + _cCodSku + " - " + Alltrim(_cName) + " ." )

//--------------------------------+
// Cria diretorio caso nao exista |
//--------------------------------+
MakeDir(cDirImp)
MakeDir(cDirImp + cDirSave)
MemoWrite(cDirImp + cDirSave + "\jsonsku_" + RTrim(_cCodSku) + ".json",cRest)

//---------------------+
// Parametros de envio | 
//---------------------+
_oVTEX:cMetodo		:= IIF(_nIdVTex > 0, "PUT", "POST")
_oVTEX:cJSon		:= _cRest
_oVTEX:cID			:= cValToChar(_nIdVTex)

If _oVTEX:Sku()
	_oJSon := JSonObject():New()
	_oJSon:FromJson(_oVTEX:cJSonRet)
	If ValType(_oJSon) <> "U"
		
		LogExec("PRODUTO SKU " + _cCodSku + " - " + Alltrim(_cName) + " . ENVIADA COM SUCESSO.")

		//---------------+
		// Posiciona SKU |
		//---------------+
		If _nRecno > 0
			SB1->( dbGoTo(_nRecno) )
			RecLock("SB1",.F.)
				SB1->B1_XIDSKU	:= _oJSon['Id']
				SB1->B1_XINTLV	:= '2'
			SB1->( MsUnLock() )	
		EndIf

		cChave		:= SB1->B1_FILIAL + SB1->B1_COD

		//--------------------------------------------------+
		// Adiciona Array para envio dos campos especificos |
		//--------------------------------------------------+
		aAdd(aPrdEnv,{SB1->B1_FILIAL,SB1->B1_COD,_nIdVTex,_nIdProd})

		//----------------+
		// Parametros LOG |
		//----------------+
		cStatus		:= "1"
		cMsgErro	:= ""
		nIDVtex		:= _oJSon['Id']

		LogExec("PRODUTO SKU " + _cCodSku + " - " + Alltrim(_cName) + " . ENVIADA COM SUCESSO.")

	Else
		If ValType(_oVTEX:cError) <> "U"

			//----------------+
			// Parametros LOG |
			//----------------+
			cStatus		:= "2"
			cMsgErro	:= RTrim(_oVTEX:cError)
			nIDVtex		:= _nIdVTex

			aAdd(aMsgErro,{_cCodSku,"ERRO AO ENVIARSKU " + Alltrim(_cCodSku) + " - " + Upper(Alltrim(_cName)) + ". ERROR: " + RTrim(_oVTEX:cError)})
			LogExec("ERRO AO ENVIAR SKU " + Alltrim(_cCodSku) + " - " + Upper(Alltrim(_cName)) + ". ERROR: " +  RTrim(_oVTEX:cError))

		Else 

			//----------------+
			// Parametros LOG |
			//----------------+
			cStatus		:= "2"
			cMsgErro	:= "Sem comunicação com o integrador"
			nIDVtex		:= _nIdVTex

			aAdd(aMsgErro,{_cCodSku,"ERRO AO ENVIAR SKU " + Alltrim(_cCodSku) + " - " + Upper(Alltrim(_cName)) + ". "})
			LogExec("ERRO AO ENVIAR SKU " + Alltrim(_cCodSku) + " - " + Upper(Alltrim(_cName)) + ". ")
		EndIf 
	EndIf	
Else
	If ValType(_oVTEX:cError) <> "U"

		//----------------+
		// Parametros LOG |
		//----------------+
		cStatus		:= "2"
		cMsgErro	:= RTrim(_oVTEX:cError)
		nIDVtex		:= _nIdVTex

		aAdd(aMsgErro,{_cCodSku,"ERRO AO ENVIAR SKU " + Alltrim(_cCodSku) + " - " + Upper(Alltrim(cNomeSku)) + ". ERROR: " + RTrim(_oVTEX:cError)})
		LogExec("ERRO AO ENVIAR SKU " + Alltrim(_cCodSku) + " - " + Upper(Alltrim(cNomeSku)) + ". ERROR: " +  RTrim(_oVTEX:cError))
	Else 

		//----------------+
		// Parametros LOG |
		//----------------+
		cStatus		:= "2"
		cMsgErro	:= "Sem comunicação com o integrador"
		nIDVtex		:= _nIdVTex

		aAdd(aMsgErro,{_cCodSku,"ERRO AO ENVIAR SKU " + Alltrim(_cCodSku) + " - " + Upper(Alltrim(cNomeSku)) + ". "})
		LogExec("ERRO AO ENVIAR SKU " + Alltrim(_cCodSku) + " - " + Upper(Alltrim(cNomeSku)) + ". ")
	EndIf 
EndIf

//---------------+
// Grava LOG ZT0 |
//---------------+
cPolitica	:= ""
nRegRep		:= 0
nIdLV		:= 0
U_AEcoGrvLog(cCodInt,cDescInt,cStatus,cMsgErro,cChave,cPolitica,nIDVtex,nTenta,nRegRep,nIdLV)

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
Local _aGrade 		:= Separa(GetMv("MV_MASCGRD"),",")

Local _nTProd 		:= Val(_aGrade[1])
Local _nTLinha 		:= Val(_aGrade[2])
Local _nTColuna		:= Val(_aGrade[3])

Default _cLojaID	:= ""

//-----------------------------+
// Query consulta produtos pai |
//-----------------------------+
cQuery := "	SELECT " + CRLF
cQuery += "		GRADE, " + CRLF
cQuery += "		PR_CODPROD, " + CRLF
cQuery += "		PR_IDVTEX, " + CRLF
cQuery += "		PR_DESC, " + CRLF
cQuery += "		SKU_COD, " + CRLF
cQuery += "		SKU_IDVTEX, " + CRLF
cQuery += "		SKU_DESC, " + CRLF
cQuery += "		SKU_TITULO, " + CRLF
cQuery += "		SKU_EAN, " + CRLF
cQuery += "		SKU_ALTURA, " + CRLF
cQuery += "		SKU_LARGURA, " + CRLF
cQuery += "		SKU_PROFUNDIDADE, " + CRLF
cQuery += "		SKU_ALTURA_EMB, " + CRLF
cQuery += "		SKU_LARGURA_EMB, " + CRLF
cQuery += "		SKU_PROFUNDIDADE_EMB, " + CRLF
cQuery += "		SKU_CUBAGEM, " + CRLF
cQuery += "		SKU_PESO_LIQUIDO, " + CRLF
cQuery += "		SKU_FRABRICANTE, " + CRLF
cQuery += "		SKU_UNIDADE_MEDIDA, " + CRLF
cQuery += "		SKU_NCM, " + CRLF
cQuery += "		SKU_PRECO_CUSTO, " + CRLF
cQuery += "		SKU_PRECO_VENDA, " + CRLF
cQuery += "		SKU_STATUS, " + CRLF
cQuery += "		SKU_BLOQUEIO, " + CRLF
cQuery += "		SKU_RECNO " + CRLF
cQuery += "	FROM ( " + CRLF
cQuery += "		SELECT " + CRLF 
cQuery += "			B1.B1_GRADE GRADE, " + CRLF
cQuery += "			B4.B4_COD PR_CODPROD, " + CRLF
cQuery += "			B4.B4_XIDPRD PR_IDVTEX, " + CRLF
cQuery += "			B4.B4_DESC PR_DESC, " + CRLF
cQuery += "			B1.B1_COD SKU_COD, " + CRLF
cQuery += "			B1.B1_XIDSKU SKU_IDVTEX, " + CRLF
cQuery += "			B1.B1_DESC SKU_DESC, " + CRLF
cQuery += "			LINHA.BV_DESCRI + ' ' + COLUNA.BV_DESCRI SKU_TITULO, " + CRLF
cQuery += "			B1.B1_CODBAR SKU_EAN, " + CRLF
cQuery += "			B5.B5_ALTURA SKU_ALTURA, " + CRLF
cQuery += "			B5.B5_LARG SKU_LARGURA, " + CRLF
cQuery += "			B5.B5_COMPR SKU_PROFUNDIDADE, " + CRLF
cQuery += "			B5.B5_ALTURLC SKU_ALTURA_EMB, " + CRLF
cQuery += "			B5.B5_LARGLC SKU_LARGURA_EMB, " + CRLF
cQuery += "			B5.B5_COMPRLC SKU_PROFUNDIDADE_EMB, " + CRLF
cQuery += "			1 SKU_CUBAGEM, " + CRLF
cQuery += "			B1.B1_PESO SKU_PESO_LIQUIDO, " + CRLF
cQuery += "			B1_FABRIC SKU_FRABRICANTE, " + CRLF
cQuery += "			B1_UM SKU_UNIDADE_MEDIDA, " + CRLF
cQuery += "			B1.B1_POSIPI SKU_NCM, " + CRLF
cQuery += "			B1.B1_PRV1 SKU_PRECO_CUSTO, " + CRLF
cQuery += "			B1.B1_PRV1 SKU_PRECO_VENDA, " + CRLF
cQuery += "			B1.B1_XATIVO SKU_STATUS, " + CRLF
cQuery += "			B1.B1_MSBLQL SKU_BLOQUEIO, " + CRLF
cQuery += "			B1.R_E_C_N_O_ SKU_RECNO " + CRLF
cQuery += "		FROM " + CRLF
cQuery += "			" + RetSqlName("SB1") + " B1 (NOLOCK) " + CRLF
cQuery += "			INNER JOIN " + RetSqlName("SB4") + " B4 (NOLOCK) ON B4.B4_FILIAL = B1.B1_FILIAL AND B4.B4_COD = SUBSTRING(B1.B1_COD,1,10) AND B4.B4_XINTLV = '2' AND B4.B4_XIDPRD > 0 AND AND B4.D_E_L_E_T_ = '' " + CRLF
cQuery += "			INNER JOIN " + RetSqlName("SB5") + " B5 (NOLOCK) ON B5.B5_FILIAL = B1.B1_FILIAL AND B5.B5_COD = B1.B1_COD AND B5.D_E_L_E_T_ = '' " + CRLF
cQuery += "			CROSS APPLY ( " + CRLF
cQuery += "				SELECT " + CRLF
cQuery += "					RTRIM(BV.BV_DESCRI) BV_DESCRI " + CRLF
cQuery += "				FROM " + CRLF
cQuery += "					SBV010 BV (NOLOCK) " + CRLF
cQuery += "				WHERE " + CRLF
cQuery += "					BV.BV_FILIAL = '" + xFilial("SBV") + "' AND " + CRLF
cQuery += "					BV.BV_TABELA = B4.B4_LINHA AND " + CRLF
cQuery += "					BV.BV_TIPO = '1' AND " + CRLF
cQuery += "					BV.BV_CHAVE = SUBSTRING(B1.B1_COD," + cValToChar( _nTProd + 1 ) + "," + cValToChar( _nTLinha ) + ") AND " + CRLF
cQuery += "					BV.D_E_L_E_T_ = '' " + CRLF
cQuery += "			) LINHA " + CRLF
cQuery += "			CROSS APPLY ( " + CRLF
cQuery += "				SELECT " + CRLF
cQuery += "					RTRIM(BV.BV_DESCRI) BV_DESCRI " + CRLF
cQuery += "				FROM " + CRLF
cQuery += "					SBV010 BV (NOLOCK) " + CRLF
cQuery += "				WHERE " + CRLF 
cQuery += "					BV.BV_FILIAL = '" + xFilial("SBV") + "' AND " + CRLF
cQuery += "					BV.BV_TABELA = B4.B4_COLUNA AND " + CRLF
cQuery += "					BV.BV_TIPO = '2' AND " + CRLF
cQuery += "					BV.BV_CHAVE = SUBSTRING(B1.B1_COD," + cValToChar( _nTProd + _nTLinha + 1 ) + "," + cValToChar( _nTColuna ) + ") AND " + CRLF
cQuery += "					BV.D_E_L_E_T_ = '' " + CRLF
cQuery += "		) COLUNA " + CRLF
cQuery += "		WHERE " + CRLF
cQuery += "			B1.B1_FILIAL = '" + xFilial("SB1") + "' AND " + CRLF
cQuery += "			B1.B1_GRADE = 'S' AND " + CRLF
cQuery += "			B1.B1_XINTLV = '1' AND " + CRLF
cQuery += "			B1.D_E_L_E_T_ = '' " + CRLF	
cQuery += "		UNION ALL " + CRLF
cQuery += "		SELECT " + CRLF
cQuery += "			B1.B1_GRADE GRADE, " + CRLF
cQuery += "			B1.B1_COD PR_CODPROD, " + CRLF
cQuery += "			B1.B1_XIDPRD PR_IDVTEX, " + CRLF
cQuery += "			B1.B1_DESC PR_DESC, " + CRLF
cQuery += "			B1.B1_COD SKU_COD, " + CRLF
cQuery += "			B1.B1_XIDSKU SKU_IDVTEX, " + CRLF
cQuery += "			B1.B1_DESC SKU_DESC, " + CRLF
cQuery += "			B5_ECTITU SKU_TITULO, " + CRLF
cQuery += "			B1.B1_CODBAR SKU_EAN, " + CRLF
cQuery += "			B5.B5_ALTURA SKU_ALTURA, " + CRLF
cQuery += "			B5.B5_LARG SKU_LARGURA, " + CRLF
cQuery += "			B5.B5_COMPR SKU_PROFUNDIDADE, " + CRLF
cQuery += "			B5.B5_ALTURLC SKU_ALTURA_EMB, " + CRLF
cQuery += "			B5.B5_LARGLC SKU_LARGURA_EMB, " + CRLF
cQuery += "			B5.B5_COMPRLC SKU_PROFUNDIDADE_EMB, " + CRLF
cQuery += "			1 SKU_CUBAGEM, " + CRLF
cQuery += "			B1.B1_PESO SKU_PESO_LIQUIDO, " + CRLF
cQuery += "			B1_FABRIC SKU_FRABRICANTE, " + CRLF
cQuery += "			B1_UM SKU_UNIDADE_MEDIDA, " + CRLF
cQuery += "			B1.B1_POSIPI SKU_NCM, " + CRLF
cQuery += "			B1.B1_PRV1 SKU_PRECO_CUSTO, " + CRLF
cQuery += "			B1.B1_PRV1 SKU_PRECO_VENDA, " + CRLF
cQuery += "			B1.B1_XATIVO SKU_STATUS, " + CRLF
cQuery += "			B1.B1_MSBLQL SKU_BLOQUEIO, " + CRLF
cQuery += "			B1.R_E_C_N_O_ SKU_RECNO " + CRLF
cQuery += "		FROM " + CRLF
cQuery += "			" + RetSqlName("SB1") + " B1 (NOLOCK) " + CRLF
cQuery += "			INNER JOIN " + RetSqlName("SB5") + " B5 (NOLOCK) ON B5.B5_FILIAL = B1.B1_FILIAL AND B5.B5_COD = B1.B1_COD AND B5.D_E_L_E_T_ = '' " + CRLF
cQuery += "		WHERE " + CRLF
cQuery += "			B1.B1_FILIAL = '" + xFilial("SB1") + "' AND " + CRLF
cQuery += "			B1.B1_GRADE IN('','N') AND " + CRLF
cQuery += "			B1.B1_XINTLV = '1' AND " + CRLF
cQuery += "			B1.B1_XIDPRD > 0 AND " + CRLF
cQuery += "			B1.D_E_L_E_T_ = '' " + CRLF
cQuery += " ) SKU " + CRLF
cQuery += "	ORDER BY SKU_COD "

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
