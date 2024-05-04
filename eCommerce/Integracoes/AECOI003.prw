#INCLUDE "PROTHEUS.CH"
#INCLUDE "APWEBSRV.CH"
#INCLUDE "TOPCONN.CH"
#INCLUDE "TBICONN.CH"

#DEFINE CRLF CHR(13) + CHR(10)

Static cCodInt	:= "SB4"
Static cDescInt	:= "Produto"
Static cDirImp	:= "/ecommerce/"
Static cDirSave	:= "produtos/"

/**************************************************************************************************/
/*/{Protheus.doc} AECOI003
	@description	Rotina realiza a integração dos produtos pai para o ecommerce
	@type   		Function 
	@author			Bernard M.Margarido
	@version   		1.00
	@since     		10/02/2016
/*/
/**************************************************************************************************/
User Function AECOI003()
Local _lEspecificos := GetMv("EC_CPOESP")

Private cThread		:= Alltrim(Str(ThreadId()))
Private cStaLog		:= "0"
Private cArqLog		:= ""	

Private nQtdInt		:= 0

Private cHrIni		:= Time()
Private dDtaInt		:= Date()

Private aMsgErro	:= {}
Private aPrdEnv		:= {}

//------------------------------+
// Inicializa Log de Integracao |
//------------------------------+
MakeDir(cDirImp)
cArqLog := cDirImp + "PRODUTOS" + cEmpAnt + cFilAnt + ".LOG"
LogExec("")	
LogExec(Replicate("-",80))
LogExec("INICIA INTEGRACAO DE PRODUTOS COM A VTEX - DATA/HORA: "+DTOC(DATE())+" AS "+TIME())

//---------------------------------------+
// Inicia processo de envio dos produtos |
//---------------------------------------+
Processa({|| AECOINT03() },"Aguarde...","Consultando Produtos.")

LogExec("FINALIZA INTEGRACAO DE PRODUTOS COM A VTEX - DATA/HORA: "+DTOC(DATE())+" AS "+TIME())
LogExec(Replicate("-",80))
LogExec("")

//-----------------------------------+
// Envia especificações dos produtos |
//-----------------------------------+
If _lEspecificos .And. Len(aPrdEnv) > 0  
	U_AECOI03A()
EndIf 

//----------------------------------+
// Envia e-Mail com o Logs de Erros |
//----------------------------------+
If Len(aMsgErro) > 0
	cStaLog := "1"
	u_AEcoMail(cCodInt,cDescInt,aMsgErro)
EndIf

Return Nil

/**************************************************************************************************/
/*/{Protheus.doc} AECOINT03
	@description	Rotina consulta e envia Produtos para a pataforma e-Commerce
	@author			Bernard M.Margarido
	@version   		1.00
	@since     		10/02/2016
/*/
/**************************************************************************************************/
Static Function AECOINT03()
Local aArea			:= GetArea()

Local _cGrade		:= ""
Local _cCodProd		:= ""
Local _cName		:= ""
Local _cTitle 		:= ""
Local _cKeyWord 	:= ""
Local _cMetaTag 	:= ""
Local _cDescription := ""
Local _cDescriShort := ""
Local _cDescMarca	:= ""
Local _cStatus 		:= ""
Local _cTaxCode		:= ""
Local _cRest 		:= ""

Local _nIdVTex		:= 0
Local _nIdBrand 	:= 0
Local _nCategID 	:= 0
Local _nDepartID 	:= 0 
Local _nRecno		:= 0
Local nToReg		:= 0

Local cAlias		:= GetNextAlias()

Local oJSon			:= Nil 

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
	IncProc("Produtos " + RTrim((cAlias)->PR_CODIGO) + " - " + RTrim((cAlias)->PR_DESCRICAO) )
					
	//----------------------+
	// Dados da Produto Pai |
	//----------------------+
	_cGrade 						:= (cAlias)->GRADE
	_cCodProd 						:= RTrim((cAlias)->PR_CODIGO)
	_cName							:= RTrim((cAlias)->EC_TITULO)
	_cTitle 						:= RTrim((cAlias)->EC_TITULO)
	_cKeyWord 						:= ""
	_cMetaTag 						:= ""
	_cDescription 					:= RTrim((cAlias)->EC_DESCRICAO)
	_cDescriShort 					:= ""
	_cDescMarca						:= RTrim((cAlias)->PR_DESC_MARCA)
	_cStatus 						:= (cAlias)->EC_FLAG
	_cTaxCode 						:= (cAlias)->PR_NCM
	_cCategoria 					:= (cAlias)->PR_CATEGORIA

	_nIdVTex						:= (cAlias)->PR_IDPROD
	_nIdBrand 						:= (cAlias)->PR_MARCA
	_nRecno							:= (cAlias)->RECNOPROD
	_nCategID 						:= 0
	_nDepartID 						:= 0

	If !Empty(_cCategoria)
		AECOI03C(_cCategoria,@_nCategID,@_nDepartID)
	EndIf 

	oJSon							:= Nil 
	oJSon 							:= JSonObject():New()

	oJSon["Name"]					:= _cName
	oJSon["DepartmentId"]			:= _nDepartID
	oJSon["CategoryId"]				:= _nCategID
	oJSon["BrandId"]				:= _nIdBrand
	oJSon["LinkId"]					:= Nil
	oJSon["RefId"]					:= _cCodProd
	oJSon["IsVisible"]				:= IIF(_cStatus == "1",.T.,.F.)
	oJSon["Description"]			:= _cDescription
	oJSon["DescriptionShort"]		:= _cDescriShort
	oJSon["KeyWords"]				:= Nil
	oJSon["Title"]					:= _cTitle
	oJSon["IsActive"]				:= IIF(_cStatus == "1",.T.,.F.)
	oJSon["TaxCode"]				:= _cTaxCode
	oJSon["MetaTagDescription"]		:= Nil
	oJSon["SupplierId"]				:= Nil
	oJSon["ShowWithoutStock"]		:= .T.
	
	_cRest							:= oJSon:ToJson()		

	LogExec("ENVIANDO PRODUTO " + RTrim((cAlias)->PR_CODIGO) + " - " + RTrim((cAlias)->PR_DESCRICAO) )

	//-----------------------------------------+
	// Rotina realiza o envio para o ecommerce |
	//-----------------------------------------+
	AEcoEnv(_cGrade,_cCodProd,_cName,_cRest,_nIdVTex,_nRecno)
	
	(cAlias)->( dbSkip() )
				
EndDo

//----------------------------+
// Encerra arquivo temporario |
//----------------------------+
(cAlias)->( dbCloseArea() )

FreeObj(oJSon)

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
Static Function AEcoEnv(_cGrade,_cCodProd,_cName,_cRest,_nIdVTex,_nRecno)
Local aArea			:= GetArea()

Local _oVTEX 		:= VTEX():New()
Local _oJSon 		:= Nil 

Local cChave		:= ""
Local cPolitica		:= ""
Local cStatus		:= ""
Local cMsgErro		:= ""

Local nIDVtex		:= 0
Local nRegRep		:= 0
Local nIdLV			:= 0

LogExec("ENVIANDO PRODUTO " + _cCodProd + " - " + Alltrim(_cName) + " ." )

//--------------------------------+
// Cria diretorio caso nao exista |
//--------------------------------+
MakeDir(cDirImp)
MakeDir(cDirImp + cDirSave)
MemoWrite(cDirImp + cDirSave + "\jsonproduto_" + RTrim(_cCodProd) + ".json",_cRest)

//---------------------+
// Parametros de envio | 
//---------------------+
_oVTEX:cMetodo		:= IIF(_nIdVTex > 0, "PUT", "POST")
_oVTEX:cJSon		:= _cRest
_oVTEX:cID			:= cValToChar(_nIdVTex)

If _oVTEX:Product()
	
	_oJSon := JSonObject():New()
	_oJSon:FromJson(_oVTEX:cJSonRet)
	If ValType(_oJSon) <> "U"
		
		LogExec("PRODUTO " + _cCodProd + " - " + Alltrim(_cName) + " . ENVIADA COM SUCESSO." )
											
		//------------------+
		// Atualiza produto |
		//------------------+
		If _cGrade == "S"

			SB4->( dbGoTo(_nRecno) )
			RecLock("SB4",.F.)
				SB4->B4_XIDPRD 	:= _oJSon['Id']
				SB4->B4_XINTLV	:= '2'
			SB4->( MsunLock() )	

			cChave		:= SB4->B4_FILIAL + SB4->B4_COD

			//--------------------------------------------------+
			// Adiciona Array para envio dos campos especificos |
			//--------------------------------------------------+
			aAdd(aPrdEnv,{SB4->B4_FILIAL,SB4->B4_COD,SB4->B4_XIDPRD})

		Else 

			SB1->( dbGoTo(_nRecno) )
			RecLock("SB1",.F.)
				SB1->B1_XIDPRD := _oJSon['Id']
				SB1->B1_XINTLV	:= '2'
			SB1->( MsunLock() )	

			cChave		:= SB1->B1_FILIAL + SB1->B1_COD

			//--------------------------------------------------+
			// Adiciona Array para envio dos campos especificos |
			//--------------------------------------------------+
			aAdd(aPrdEnv,{SB1->B1_FILIAL,SB1->B1_COD,SB1->B1_XIDPRD})

		EndIf 

		//----------------+
		// Parametros LOG |
		//----------------+
		cStatus		:= "1"
		cMsgErro	:= ""
		_nIdVTex	:= _oJSon['Id']


	Else
		If ValType(_oVTEX:cError) <> "U"

			//----------------+
			// Parametros LOG |
			//----------------+
			cStatus		:= "2"
			cMsgErro	:= RTrim(_oVTEX:cError)

			aAdd(aMsgErro,{_cCodProd,"ERRO AO ENVIAR PRODUTO PAI " + Alltrim(_cCodProd) + " - " + Upper(Alltrim(_cName)) + ". ERROR: " + RTrim(_oVTEX:cError)})
			LogExec("ERRO AO ENVIAR PRODUTO PAI " + Alltrim(_cCodProd) + " - " + Upper(Alltrim(_cName)) + ". ERROR: " +  RTrim(_oVTEX:cError))
		Else 

			//----------------+
			// Parametros LOG |
			//----------------+
			cStatus		:= "2"
			cMsgErro	:= "Sem comunicação com o integrador"

			aAdd(aMsgErro,{_cCodProd,"ERRO AO ENVIAR PRODUTO PAI " + Alltrim(_cCodProd) + " - " + Upper(Alltrim(_cName)) + ". "})
			LogExec("ERRO AO ENVIAR PRODUTO PAI " + Alltrim(_cCodProd) + " - " + Upper(Alltrim(_cName)) + ". ")
		EndIf 
	EndIf	
Else
	If ValType(_oVTEX:cError) <> "U"

		//----------------+
		// Parametros LOG |
		//----------------+
		cStatus		:= "2"
		cMsgErro	:= RTrim(_oVTEX:cError)

		aAdd(aMsgErro,{_cCodProd,"ERRO AO ENVIAR PRODUTO PAI " + Alltrim(_cCodProd) + " - " + Upper(Alltrim(_cName)) + ". ERROR: " + RTrim(_oVTEX:cError)})
		LogExec("ERRO AO ENVIAR PRODUTO PAI " + Alltrim(_cCodProd) + " - " + Upper(Alltrim(_cName)) + ". ERROR: " +  RTrim(_oVTEX:cError))
	Else 

		//----------------+
		// Parametros LOG |
		//----------------+
		cStatus		:= "2"
		cMsgErro	:= "Sem comunicação com o integrador"

		aAdd(aMsgErro,{_cCodProd,"ERRO AO ENVIAR PRODUTO PAI " + Alltrim(_cCodProd) + " - " + Upper(Alltrim(_cName)) + ". "})
		LogExec("ERRO AO ENVIAR PRODUTO PAI " + Alltrim(_cCodProd) + " - " + Upper(Alltrim(_cName)) + ". ")
	EndIf 
EndIf

//---------------+
// Grava LOG ZT0 |
//---------------+
cPolitica	:= ""
nIDVtex		:= _nIdVTex
nRegRep		:= 0
nIdLV		:= 0
nTenta		:= 1
U_AEcoGrvLog(cCodInt,cDescInt,cStatus,cMsgErro,cChave,cPolitica,nIDVtex,nTenta,nRegRep,nIdLV)


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
cQuery := "	SELECT " + CRLF
cQuery += "		GRADE, " + CRLF
cQuery += "		FILIAL, " + CRLF
cQuery += "		PR_IDPROD, " + CRLF
cQuery += "		PR_CODIGO, " + CRLF
cQuery += "		PR_DESCRICAO,  " + CRLF
cQuery += "		PR_DESC_TECNICA, " + CRLF
cQuery += "		PR_NCM, " + CRLF
cQuery += "		PR_DEPARTAMENTO, " + CRLF
cQuery += "		PR_CATEGORIA, " + CRLF
cQuery += "		PR_MARCA, " + CRLF
cQuery += "		PR_DESC_MARCA, " + CRLF
cQuery += "		PR_STATUS, " + CRLF
cQuery += "		EC_TITULO, " + CRLF
cQuery += "		EC_DESCRICAO, " + CRLF
cQuery += "		EC_CARACTERISTICA, " + CRLF
cQuery += "		EC_FLAG, " + CRLF
cQuery += "		EC_CUBAGEM, " + CRLF
cQuery += "		EC_PROFUNDIDADE, " + CRLF
cQuery += "		EC_COMPRIMENTO, " + CRLF
cQuery += "		EC_LARGURA, " + CRLF
cQuery += "		RECNOPROD " + CRLF
cQuery += " FROM ( " + CRLF
cQuery += "		SELECT " + CRLF 
cQuery += "			'S' GRADE, " + CRLF
cQuery += "			B4.B4_FILIAL FILIAL, " + CRLF
cQuery += "			B4.B4_XIDPRD PR_IDPROD, " + CRLF
cQuery += "			B4.B4_COD PR_CODIGO, " + CRLF
cQuery += "			B4.B4_DESC PR_DESCRICAO, " + CRLF 
cQuery += "			ISNULL(CAST(CAST(B4_XDESCTE AS BINARY(2048)) AS VARCHAR(2048)),'') PR_DESC_TECNICA, " + CRLF
cQuery += "			B4_POSIPI PR_NCM, " + CRLF
cQuery += "			'' PR_DEPARTAMENTO, " + CRLF
cQuery += "			B4.B4_XCATEGO PR_CATEGORIA, " + CRLF
cQuery += "			ZTD.ZTD_IDLV PR_MARCA, " + CRLF
cQuery += "			ZTD.ZTD_DESC PR_DESC_MARCA, " + CRLF
cQuery += "			B4.B4_XATIVO PR_STATUS, " + CRLF
cQuery += "			ISNULL(CAST(CAST(MHH_ECTITU AS BINARY(2048)) AS VARCHAR(2048)),'') EC_TITULO, " + CRLF
cQuery += "			ISNULL(CAST(CAST(MHH_ECDESC AS BINARY(2048)) AS VARCHAR(2048)),'') EC_DESCRICAO, " + CRLF
cQuery += "			ISNULL(CAST(CAST(MHH_ECCARA AS BINARY(2048)) AS VARCHAR(2048)),'') EC_CARACTERISTICA, " + CRLF
cQuery += "			MHH_FLAG EC_FLAG, " + CRLF
cQuery += "			MHH_ECCUBA EC_CUBAGEM, " + CRLF
cQuery += "			MHH_ECPROF EC_PROFUNDIDADE, " + CRLF
cQuery += "			MHH_ECCOMP EC_COMPRIMENTO, " + CRLF
cQuery += "			MHH_ECLARG EC_LARGURA, " + CRLF
cQuery += "			B4.R_E_C_N_O_ RECNOPROD " + CRLF
cQuery += "		FROM " + CRLF 
cQuery += "			" + RetSqlName("SB4") + " B4 (NOLOCK) " + CRLF
cQuery += "			INNER JOIN " + RetSqlName("MHH") + " MHH (NOLOCK) ON MHH.MHH_FILIAL = B4.B4_FILIAL AND MHH.MHH_COD = B4.B4_COD AND MHH.D_E_L_E_T_ = '' " + CRLF
cQuery += "			INNER JOIN " + RetSqlName("ZTD") + " ZTD (NOLOCK) ON ZTD.ZTD_FILIAL = '" + xFilial("ZTD") + "' AND ZTD.ZTD_COD = B4.B4_XMARCA AND ZTD.D_E_L_E_T_ = '' " + CRLF
cQuery += "		WHERE " + CRLF
cQuery += "			B4.B4_FILIAL = '" + xFilial("SB4") + "' AND " + CRLF
cQuery += "			B4.B4_XINTLV  = '1' AND " + CRLF
cQuery += "			B4.D_E_L_E_T_ = '' " + CRLF
cQuery += "		UNION ALL " + CRLF
cQuery += "		SELECT " + CRLF 
cQuery += "			'N' GRADE, " + CRLF
cQuery += "			B1.B1_FILIAL FILIAL, " + CRLF
cQuery += "			B1.B1_XIDPRD PR_IDPROD, " + CRLF
cQuery += "			B1.B1_COD PR_CODIGO, " + CRLF
cQuery += "			B1.B1_DESC PR_DESCRICAO,  " + CRLF
cQuery += "			ISNULL(CAST(CAST(B1_XDESCTE AS BINARY(2048)) AS VARCHAR(2048)),'') PR_DESC_TECNICA, " + CRLF
cQuery += "			B1_POSIPI PR_NCM, " + CRLF
cQuery += "			'' PR_DEPARTAMENTO, " + CRLF 
cQuery += "			B1.B1_XCATEGO PR_CATEGORIA, " + CRLF
cQuery += "			ZTD.ZTD_IDLV PR_MARCA, " + CRLF
cQuery += "			ZTD.ZTD_DESC PR_DESC_MARCA, " + CRLF
cQuery += "			B1.B1_XATIVO PR_STATUS, " + CRLF
cQuery += "			B5_ECTITU EC_TITULO, " + CRLF
cQuery += "			ISNULL(CAST(CAST(B5_ECDESCR AS BINARY(2048)) AS VARCHAR(2048)),'') EC_DESCRICAO, " + CRLF
cQuery += "			ISNULL(CAST(CAST(B5_ECCARAC AS BINARY(2048)) AS VARCHAR(2048)),'') EC_CARACTERISTICA, " + CRLF
cQuery += "			B5_ECFLAG EC_FLAG, " + CRLF
cQuery += "			B5_ECCUBAG EC_CUBAGEM, " + CRLF
cQuery += "			B5_ECPROFU EC_PROFUNDIDADE, " + CRLF
cQuery += "			B5_ECCOMP EC_COMPRIMENTO, " + CRLF
cQuery += "			B5_ECLARGU EC_LARGURA, " + CRLF
cQuery += "			B1.R_E_C_N_O_ RECNOPROD " + CRLF
cQuery += "		FROM  " + CRLF
cQuery += "			" + RetSqlName("SB1") + " B1 (NOLOCK) " + CRLF
cQuery += "			INNER JOIN " + RetSqlName("SB5") + " B5 (NOLOCK) ON B5.B5_FILIAL = B1.B1_FILIAL AND B5.B5_COD = B1.B1_COD AND B5.D_E_L_E_T_ = '' " + CRLF
cQuery += "			INNER JOIN " + RetSqlName("ZTD") + " ZTD (NOLOCK) ON ZTD.ZTD_FILIAL = '" + xFilial("ZTD") + "' AND ZTD.ZTD_COD = B1.B1_XMARCA AND ZTD.D_E_L_E_T_ = '' " + CRLF
cQuery += "		WHERE " + CRLF
cQuery += "			B1.B1_FILIAL = '" + xFilial("SB1") + "' AND " + CRLF
cQuery += "			B1.B1_GRADE IN('','N') AND " + CRLF
cQuery += "			B1.B1_XINTLV = '1' AND " + CRLF
cQuery += "			B1.D_E_L_E_T_ = '' " + CRLF
cQuery += " )PRODUTOS "

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
/*/{Protheus.doc} AECOI03C
	@description Retorna departamento e catgoria 
	@type  Static Function
	@author Bernard M Margarido
	@since 29/04/2024
	@version version
/*/
/*********************************************************************************/
Static Function AECOI03C(_cCategoria,_nCategID,_nDepartID)
Local _aArea 	:= GetArea() 
Local _aCateg	:= {}

Local _lContinua:= .T.	

dbSelectArea("ACU")
ACU->( dbSetOrder(1) )
While ACU->( dbSeek(xFilial("ACU") + _cCategoria) ) .And. _lContinua
	_cCategoria := ACU->ACU_CODPAI
	_nCategID	:= IIF(_nCategID == 0, ACU->ACU_XIDLV, _nCategID)
	_nDepartID  := ACU->ACU_XIDLV
	_lContinua  := IIF(Empty(ACU->ACU_CODPAI), .F., .T.)
	aAdd(_aCateg,{ACU->ACU_COD,ACU->ACU_XIDLV})
EndDo 

RestArea(_aArea)
Return Nil 

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

