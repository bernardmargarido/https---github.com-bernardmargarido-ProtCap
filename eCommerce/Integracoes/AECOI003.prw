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

Local cCodPai 		:= ""
Local cNomePrd		:= ""
Local cTitPrd		:= "" 
Local cSubTitPrd	:= "" 
Local cDescPrd		:= "" 
Local cCarcacPrd	:= "" 
Local cKeyword		:= "" 
Local cStatus		:= ""
Local cTpProd		:= ""
Local cIdLoja		:= ""

Local cAlias		:= GetNextAlias()

Local nCat01		:= 0  
Local nCat02		:= 0   
Local nCat03		:= 0  
Local nCat04		:= 0
Local nCat05		:= 0
Local nFabric		:= 0
Local nIdProd		:= 0
Local nToReg		:= 0

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
	IncProc("Produtos " + Alltrim((cAlias)->CODIGO) + " - " + Alltrim((cAlias)->NOME) )

					
	//----------------------+
	// Dados da Produto Pai |
	//----------------------+
	cCodPai 	:= (cAlias)->CODIGO
	cNomePrd	:= (cAlias)->NOME
	cTitPrd		:= (cAlias)->TIUTLO 
	cSubTitPrd	:= (cAlias)->SUBTITULO 
	cDescPrd	:= (cAlias)->DESCECO 
	cCarcacPrd	:= (cAlias)->DESCCARA 
	cKeyword	:= (cAlias)->KEYWORDS 
	cStatus		:= (cAlias)->STATUSPRD
	cTpProd		:= (cAlias)->TPPROD
	cIdLoja		:= (cAlias)->IDLOJA
	
	nCat01		:= (cAlias)->CATEGO01  
	nCat02		:= (cAlias)->CATEGO02   
	nCat03		:= (cAlias)->CATEGO03  
	nCat04		:= (cAlias)->CATEGO04
	nCat05		:= (cAlias)->CATEGO05
	nFabric		:= (cAlias)->CODMARCA
	nIdProd		:= (cAlias)->IDPROD

	If !Empty(nCat01)
		nCatDepar := nCat01
	EndIf	

	If !Empty(nCat02)
		nCatDepar := nCat02
	EndIf	

	If !Empty(nCat03)
		nCatDepar := nCat03
	EndIf	

	If !Empty(nCat04)
		nCatDepar := nCat04
	EndIf

	If !Empty(nCat05)
		nCatDepar := nCat05
	EndIf

	oJSon							:= Nil 
	oJSon 							:= JSonObject():New()

	oJSon["Name"]					:= RTrim(cNomePrd)
	oJSon["DepartmentId"]			:= nCatDepar
	oJSon["CategoryId"]				:= nCat01
	oJSon["BrandId"]				:= nFabric
	oJSon["LinkId"]					:= Lower(Alltrim(cTitPrd))
	oJSon["RefId"]					:= RTrim(cCodPai)
	oJSon["IsVisible"]				:= IIF(cStatus == "A",.T.,.F.)
	oJSon["Description"]			:= RTrim(cDescPrd)
	oJSon["DescriptionShort"]		:= RTrim(cSubTitPrd)
	oJSon["KeyWords"]				:= RTrim(cKeyword)
	oJSon["Title"]					:= RTrim(cTitPrd)
	oJSon["IsActive"]				:= IIF(cStatus == "A",.T.,.F.)
	oJSon["TaxCode"]				:= Nil
	oJSon["MetaTagDescription"]		:= RTrim(cCarcacPrd)
	oJSon["SupplierId"]				:= Nil
	oJSon["ShowWithoutStock"]		:= .T.
	
	cRest							:= oJSon:ToJson()		

	LogExec("ENVIANDO PRODUTO " + Alltrim((cAlias)->CODIGO) + " - " + Alltrim((cAlias)->NOME) )

	//-----------------------------------------+
	// Rotina realiza o envio para o ecommerce |
	//-----------------------------------------+
	AEcoEnv(cCodPai,cNomePrd,cRest,nIdProd,(cAlias)->RECNOB5)
	
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
Static Function AEcoEnv(cCodPai,cNomePrd,cRest,nIdProd,nRecnoB5)
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
MemoWrite(cDirImp + cDirSave + "\jsonproduto_" + RTrim(cCodPai) + ".json",cRest)

//---------------------+
// Parametros de envio | 
//---------------------+
_oVTEX:cMetodo		:= IIF(nIdProd > 0, "PUT", "POST")
_oVTEX:cJSon		:= cRest
_oVTEX:cID			:= cValToChar(nIdProd)

If _oVTEX:Produto()
	
	_oJSon := JSonObject():New()
	_oJSon:FromJson(_oVTEX:cJSonRet)
	If ValType(_oJSon) <> "U"
		
		LogExec("PRODUTO " + cCodPai + " - " + Alltrim(cNomePrd) + " . ENVIADA COM SUCESSO." )
											
		//------------------+
		// Atualiza produto |
		//------------------+
		SB5->( dbGoTo(nRecnoB5) )
		RecLock("SB5",.F.)
			SB5->B5_XIDPROD := _oJSon['Id']
			SB5->B5_XENVECO := "2"
			SB5->B5_XENVSKU := "1"
			SB5->B5_XENVCAT := "2"
			SB5->B5_XDTEXP	:= Date()
			SB5->B5_XHREXP	:= Time()
		SB5->( MsunLock() )	

		//--------------------------------------------------+
		// Adiciona Array para envio dos campos especificos |
		//--------------------------------------------------+
		aAdd(aPrdEnv,{SB5->B5_FILIAL,SB5->B5_COD,SB5->B5_XIDPROD})

	Else
		If ValType(_oVTEX:cError) <> "U"
			aAdd(aMsgErro,{cCodPai,"ERRO AO ENVIAR PRODUTO PAI " + Alltrim(cCodPai) + " - " + Upper(Alltrim(cNomePrd)) + ". ERROR: " + RTrim(_oVTEX:cError)})
			LogExec("ERRO AO ENVIAR PRODUTO PAI " + Alltrim(cCodPai) + " - " + Upper(Alltrim(cNomePrd)) + ". ERROR: " +  RTrim(_oVTEX:cError))
		Else 
			aAdd(aMsgErro,{cCodPai,"ERRO AO ENVIAR PRODUTO PAI " + Alltrim(cCodPai) + " - " + Upper(Alltrim(cNomePrd)) + ". "})
			LogExec("ERRO AO ENVIAR PRODUTO PAI " + Alltrim(cCodPai) + " - " + Upper(Alltrim(cNomePrd)) + ". ")
		EndIf 
	EndIf	
Else
	If Type(_oVTEX:cError) <> "U"
		aAdd(aMsgErro,{cCodPai,"ERRO AO ENVIAR PRODUTO PAI " + Alltrim(cCodPai) + " - " + Upper(Alltrim(cNomePrd)) + ". ERROR: " + RTrim(_oVTEX:cError)})
		LogExec("ERRO AO ENVIAR PRODUTO PAI " + Alltrim(cCodPai) + " - " + Upper(Alltrim(cNomePrd)) + ". ERROR: " +  RTrim(_oVTEX:cError))
	Else 
		aAdd(aMsgErro,{cCodPai,"ERRO AO ENVIAR PRODUTO PAI " + Alltrim(cCodPai) + " - " + Upper(Alltrim(cNomePrd)) + ". "})
		LogExec("ERRO AO ENVIAR PRODUTO PAI " + Alltrim(cCodPai) + " - " + Upper(Alltrim(cNomePrd)) + ". ")
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
cQuery := "	SELECT " + CRLF
cQuery += "		CODIGO, " + CRLF 
cQuery += "		CODMARCA, " + CRLF 
cQuery += "		NOME, " + CRLF
cQuery += "		TIUTLO, " + CRLF
cQuery += "		SUBTITULO, " + CRLF
cQuery += "		CATEGO01, " + CRLF
cQuery += "		CATEGO02, " + CRLF
cQuery += "		CATEGO03, " + CRLF  
cQuery += "		CATEGO04, " + CRLF  
cQuery += "		CATEGO05, " + CRLF  
cQuery += "		IDPROD, " + CRLF
cQuery += "		IDLOJA, " + CRLF
cQuery += "		DESCECO, " + CRLF 
cQuery += "		DESCCARA, " + CRLF 
cQuery += "		KEYWORDS, " + CRLF
cQuery += "		STATUSPRD, " + CRLF
cQuery += "		TPPROD, " + CRLF
cQuery += "		RECNOB5 " + CRLF
cQuery += "	FROM " + CRLF
cQuery += "	( " + CRLF
cQuery += "		SELECT " + CRLF 
cQuery += "			B5.B5_COD CODIGO, " + CRLF 
cQuery += "			ISNULL(AY2.AY2_XIDMAR,0) CODMARCA, " + CRLF 
cQuery += "			B5.B5_XNOMPRD NOME, " + CRLF
cQuery += "			B5.B5_XTITULO TIUTLO, " + CRLF
cQuery += "			B5.B5_XSUBTIT SUBTITULO, " + CRLF
cQuery += "			ISNULL((SELECT AY0.AY0_XIDCAT FROM " + RetSqlName("AY0") + " AY0 WHERE AY0.AY0_FILIAL = '" + xFilial("AY0") + "' AND AY0.AY0_CODIGO = B5.B5_XCAT01 AND AY0.D_E_L_E_T_ = '' ),0) CATEGO01, " + CRLF   
cQuery += "			ISNULL((SELECT AY0.AY0_XIDCAT FROM " + RetSqlName("AY0") + " AY0 WHERE AY0.AY0_FILIAL = '" + xFilial("AY0") + "' AND AY0.AY0_CODIGO = B5.B5_XCAT02 AND AY0.D_E_L_E_T_ = '' ),0) CATEGO02, " + CRLF  
cQuery += "			ISNULL((SELECT AY0.AY0_XIDCAT FROM " + RetSqlName("AY0") + " AY0 WHERE AY0.AY0_FILIAL = '" + xFilial("AY0") + "' AND AY0.AY0_CODIGO = B5.B5_XCAT03 AND AY0.D_E_L_E_T_ = '' ),0) CATEGO03, " + CRLF 
cQuery += "			ISNULL((SELECT AY0.AY0_XIDCAT FROM " + RetSqlName("AY0") + " AY0 WHERE AY0.AY0_FILIAL = '" + xFilial("AY0") + "' AND AY0.AY0_CODIGO = B5.B5_XCAT04 AND AY0.D_E_L_E_T_ = '' ),0) CATEGO04, " + CRLF 
cQuery += "			ISNULL((SELECT AY0.AY0_XIDCAT FROM " + RetSqlName("AY0") + " AY0 WHERE AY0.AY0_FILIAL = '" + xFilial("AY0") + "' AND AY0.AY0_CODIGO = B5.B5_XCAT05 AND AY0.D_E_L_E_T_ = '' ),0) CATEGO05, " + CRLF 
cQuery += "			B5.B5_XIDPROD IDPROD, " + CRLF
cQuery += "			B5.B5_XIDLOJA IDLOJA, " + CRLF
cQuery += "			ISNULL(CAST(CAST(B5.B5_XDESCRI AS BINARY(2048)) AS VARCHAR(2048)),'') DESCECO, " + CRLF 
cQuery += "			ISNULL(CAST(CAST(B5.B5_XCARACT AS BINARY(2048)) AS VARCHAR(2048)),'') DESCCARA, " + CRLF
cQuery += "			ISNULL(CAST(CAST(B5.B5_XKEYWOR AS BINARY(2048)) AS VARCHAR(2048)),'') KEYWORDS, " + CRLF
cQuery += "			B5.B5_XSTAPRD STATUSPRD, " + CRLF
cQuery += "			B5.B5_XTPPROD TPPROD, " + CRLF
cQuery += "			B5.R_E_C_N_O_ RECNOB5 " + CRLF
cQuery += "		FROM " + CRLF
cQuery += "			" + RetSqlName("SB5") + " B5 " + CRLF 
cQuery += "			LEFT OUTER JOIN " + RetSqlName("AY2") + " AY2 ON AY2.AY2_FILIAL = '" + xFilial("AY2") + "' AND AY2.AY2_CODIGO = B5.B5_XCODMAR AND AY2.D_E_L_E_T_ = '' " + CRLF
cQuery += "		WHERE " + CRLF
cQuery += "			B5.B5_FILIAL = '" + xFilial("SB5") + "' AND " + CRLF  
cQuery += "			B5.B5_XENVECO = '1' AND " + CRLF
cQuery += "			B5.B5_XENVSKU = '1' AND " + CRLF
cQuery += "			B5.B5_XUSAECO = 'S' AND " + CRLF
cQuery += "			B5.D_E_L_E_T_ = '' " + CRLF
cQuery += "	) PRDPAI " + CRLF
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
	@type function
/*/
/*********************************************************************************/
Static Function LogExec(cMsg)
	CONOUT(cMsg)
	LjWriteLog(cArqLog,cMsg)
Return .T.
