#INCLUDE "PROTHEUS.CH"
#INCLUDE "APWEBSRV.CH"
#INCLUDE "TOPCONN.CH"
#INCLUDE "TBICONN.CH"
#INCLUDE "AARRAY.CH"
#INCLUDE "JSON.CH"

#DEFINE CRLF CHR(13) + CHR(10)

Static cCodInt	:= "DA0"
Static cDescInt	:= "Precos"
Static cDirImp	:= "/ecommerce/"
Static cDirSave	:= "precos/"

/**************************************************************************************************/
/*/{Protheus.doc} AECOI009
	@description	Rotina realiza manutenção de preços dos produtos no e-Commerce
	@type   		Function 
	@author			Bernard M.Margarido
	@version   		1.00
	@since     		10/02/2016
/*/
/**************************************************************************************************/
User Function AECOI009()
Private cThread		:= Alltrim(Str(ThreadId()))
Private cStaLog		:= "0"
Private cArqLog		:= ""	

Private nQtdInt		:= 0

Private cHrIni		:= Time()
Private dDtaInt		:= Date()

Private aMsgErro	:= {}

Private _lJob		:= IIF(Isincallstack("U_ECLOJM03"),.T.,.F.)
Private _lMultLj	:= GetNewPar("EC_MULTLOJ",.T.)

Private _oProcess 	:= Nil

//------------------------------+
// Inicializa Log de Integracao |
//------------------------------+
MakeDir(cDirImp)
cArqLog := cDirImp + "ALTERAPRECO" + cEmpAnt + cFilAnt + ".LOG"
ConOut("")	
LogExec(Replicate("-",80))
LogExec("INICIA INTEGRACAO DA ALTERACAO DE PRECO COM O ECOMMERCE - DATA/HORA: "+DTOC(DATE())+" AS "+TIME())

//---------------------------------+
// Inicia processo de envio Preços |
//---------------------------------+
If _lJob
	AECOINT09()
Else 
	Processa({|| AECOINT09() },"Aguarde...","Consultando Precos.")
EndIf 

LogExec("FINALIZA INTEGRACAO DA ALTERACAO DE PRECO COM O ECOMMERCE - DATA/HORA: "+DTOC(DATE())+" AS "+TIME())
LogExec(Replicate("-",80))
ConOut("")

//----------------------------------+
// Envia e-Mail com o Logs de Erros |
//----------------------------------+
If Len(aMsgErro) > 0
	cStaLog := "1"
	u_AEcoMail(cCodInt,cDescInt,aMsgErro)
EndIf

Return Nil

/**************************************************************************************************/
/*/{Protheus.doc} AECOINT09
	@description	Rotina consulta e envia manutenção de preços dos produtos no e-commerce
	@author			Bernard M.Margarido
	@version   		1.00
	@since     		10/02/2016
/*/
/**************************************************************************************************/
Static Function AECOINT09()
Local aArea		:= GetArea()

Local cCodSku	:= ""
Local cDescPrd	:= "" 
Local cDtaDe	:= ""
Local cDtaAte	:= ""
Local cAlias	:= GetNextAlias()

Local nIdSku 	:= 0
Local nPrcCheio	:= 0
Local nPrcPor	:= 0	
Local nToReg	:= 0

Local oJson		:= Nil
Local oPrice	:= Nil

//-------------------------------------------+
// Valida se existem preços a serem enviadas |
//-------------------------------------------+
If !AEcoQry(cAlias,@nToReg)
	LogExec("NAO EXISTEM REGISTROS PARA SEREM ENVIADOS.")  
	RestArea(aArea)
	Return .T.
EndIf

//-----------------------+
// Inicia o envio Preços |
//-----------------------+
If !_lJob
	ProcRegua(nToReg)
EndIf 

While (cAlias)->( !Eof() )
	
	//-----------------------------------+
	// Incrementa regua de processamento |
	//-----------------------------------+
	If !_lJob
		IncProc("Produto" + Alltrim((cAlias)->CODSKU)  + " - " + Alltrim((cAlias)->DESCSKU) )
	EndIf 
		
	//--------------------------------+
	// Dados Preços Produto Pai / Sku |
	//--------------------------------+
	nIdSku 		:= (cAlias)->IDSKU
	nRecno		:= (cAlias)->RECNOSB1
	nPrcCheio	:= (cAlias)->PRCDE
	nPrcPor		:= (cAlias)->PRCDE	
	cCodSku		:= (cAlias)->CODSKU
	cDescPrd	:= (cAlias)->DESCSKU 
	cDtaDe		:= FWTimeStamp(3,Date())
	cDtaAte		:= FWTimeStamp(3,YearSum(Date(),10))
			
	//-----------------------+
	// Monta String API Rest |
	//-----------------------+
	oJson					:= Nil       
	oJson					:= JSonObject():New()
	oJson["listPrice"]		:= nPrcCheio
	oJson["costPrice"]		:= nPrcCheio
	oJson["markup"]			:= 0      
	
	oJson["fixedPrices"]	:= {}

	
	oPrice					:= JSonObject():New()
	oPrice["tradePolicyId"]	:= "1"
	oPrice["value"]			:= nPrcPor
	oPrice["listPrice"]		:= nPrcCheio
	oPrice["minQuantity"]	:= 1

	aAdd(oJson["fixedPrices"],oPrice)

	cRest					:= oJSon:ToJson()

	LogExec("ENVIANDO PRECO PRODUTO " + Alltrim((cAlias)->CODSKU) + " - " + Alltrim((cAlias)->DESCSKU) )
				 
	//-----------------------------------------+
	// Rotina realiza o envio para a eCommerce |
	//-----------------------------------------+
	AEcoEnv(cRest,nIdSku,cCodSku,cDescPrd,nRecno)
				
	(cAlias)->( dbSkip() )
				
EndDo
	
//----------------------------+
// Encerra arquivo temporario |
//----------------------------+
(cAlias)->( dbCloseArea() )

RestArea(aArea)
Return .T.

/**************************************************************************************************/
/*/{Protheus.doc} AECOENV
	@description	Rotina envia manutanção de preços dos produtos para a plataforma e-commerce
	@author			Bernard M.Margarido
	@version   		1.00
	@since     		02/02/2016
/*/							
/**************************************************************************************************/
Static Function AEcoEnv(cRest,nIdSku,cCodSku,cDescPrd,nRecnoSb1)
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

Private cType		:= ""

//--------------------------------+
// Cria diretorio caso nao exista |
//--------------------------------+
MakeDir(cDirImp)
MakeDir(cDirImp + cDirSave)
MemoWrite(cDirImp + cDirSave + "\jsonpreco_" + RTrim(cCodSku) + ".json",cRest)

//--------------------+
// Posiciona registro |
//--------------------+
SB1->( dbGoTo(nRecnoSb1))

//---------------------+
// Parametros de envio | 
//---------------------+
_oVTEX:cMetodo		:= "PUT"
_oVTEX:cJSon		:= cRest
_oVTEX:cID			:= cValToChar(nIdSku)

If _oVTEX:Prices()
	
	_oJSon := JSonObject():New()
	_oJSon:FromJson(_oVTEX:cJSonRet)
	If ValType(_oJSon) <> "U"
		
		cStatus		:= "1"
		cMsgErro	:= "Preço atualizado com sucesso."

		RecLock("SB1",.F.)
			SB1->B1_MSEXP	:= dTos(Date())
		SB1->( MsUnLock() )

		LogExec("PRECO(S) ENVIADO COM SUCESSO. " )
	Else
		If ValType(_oVTEX:cError) <> "U"
			aAdd(aMsgErro,{cCodPai,"ERRO AO ENVIAR PRODUTO PAI " + Alltrim(cCodPai) + " - " + Upper(Alltrim(cNomePrd)) + ". ERROR: " + RTrim(_oVTEX:cError)})
			LogExec("ERRO AO ENVIAR PRODUTO PAI " + Alltrim(cCodPai) + " - " + Upper(Alltrim(cNomePrd)) + ". ERROR: " +  RTrim(_oVTEX:cError))
			cStatus		:= "2"
			cMsgErro	:= _oVTEX:cError
		Else 
			aAdd(aMsgErro,{cCodPai,"ERRO AO ENVIAR PRODUTO PAI " + Alltrim(cCodPai) + " - " + Upper(Alltrim(cNomePrd)) + ". "})
			LogExec("ERRO AO ENVIAR PRODUTO PAI " + Alltrim(cCodPai) + " - " + Upper(Alltrim(cNomePrd)) + ". ")
			cStatus		:= "2"
			cMsgErro	:= "Sem comunicação com o integrador"
		EndIf 
	EndIf	
Else
	If ValType(_oVTEX:cError) <> "U"
		aAdd(aMsgErro,{cCodPai,"ERRO AO ENVIAR PRODUTO PAI " + Alltrim(cCodPai) + " - " + Upper(Alltrim(cNomePrd)) + ". ERROR: " + RTrim(_oVTEX:cError)})
		LogExec("ERRO AO ENVIAR PRODUTO PAI " + Alltrim(cCodPai) + " - " + Upper(Alltrim(cNomePrd)) + ". ERROR: " +  RTrim(_oVTEX:cError))
	Else 
		aAdd(aMsgErro,{cCodPai,"ERRO AO ENVIAR PRODUTO PAI " + Alltrim(cCodPai) + " - " + Upper(Alltrim(cNomePrd)) + ". "})
		LogExec("ERRO AO ENVIAR PRODUTO PAI " + Alltrim(cCodPai) + " - " + Upper(Alltrim(cNomePrd)) + ". ")
	EndIf 
EndIf

//---------------+
// Grava LOG ZT0 |
//---------------+

cPolitica	:= ""
cChave		:= SB1->B1_FILIAL + SB1->B1_COD
nRegRep		:= 0
nIdLV		:= 0
nTenta		:= 1
nIDVtex		:= nIdSku
U_AEcoGrvLog(cCodInt,cDescInt,cStatus,cMsgErro,cChave,cPolitica,nIDVtex,nTenta,nRegRep,nIdLV)

FreeObj(_oVTEX)
FreeObj(_oJSon)

RestArea(aArea)
Return .T.

/**************************************************************************************************/
/*/{Protheus.doc} AECOQRY
	@description 	Rotina consulta preços dos produtos a serem enviados para a plataforma e-commerce
	@author			Bernard M.Margarido
	@version   		1.00
	@since     		10/02/2016
/*/			
/**************************************************************************************************/
Static Function AEcoQry(cAlias,nToReg,_cLojaID)
Local cQuery 	:= ""

Default _cLojaID:= ""

//---------------------------+
// Query consulta preços sku |
//---------------------------+	
cQuery := "	SELECT " + CRLF
cQuery += "		CODSKU , " + CRLF 
cQuery += "		DESCSKU, " + CRLF 
cQuery += "		PRCDE , " + CRLF 
cQuery += "		PRCPOR, " + CRLF 
cQuery += "		IDSKU , " + CRLF  
cQuery += "		RECNOSB1 " + CRLF 
cQuery += "	FROM " + CRLF
cQuery += "	( " + CRLF
cQuery += "		SELECT " + CRLF
cQuery += "			B1.B1_COD CODSKU, " + CRLF
cQuery += "			B1.B1_DESC DESCSKU, " + CRLF
cQuery += "			B1.B1_PRV1 PRCDE, " + CRLF
cQuery += "			B1.B1_PV2 PRCPOR, " + CRLF
cQuery += "			B1.B1_XIDSKU IDSKU, " + CRLF
cQuery += "			B1.R_E_C_N_O_ RECNOSB1 " + CRLF
cQuery += " 	FROM " + CRLF 
cQuery += "			" + RetSqlName("SB1") + " B1 " + CRLF 
cQuery += " 	WHERE " + CRLF 
cQuery += "			B1.B1_FILIAL = '" + xFilial("SB1") + "' AND " + CRLF
cQuery += "			B1.B1_XIDSKU > 0 AND " + CRLF
cQuery += "			B1.B1_XINTLV = '2' AND " + CRLF
cQuery += "			B1.B1_MSEXP = '' AND " + CRLF 
cQuery += "			B1.D_E_L_E_T_ = '' " + CRLF
cQuery += "	) PRECO " + CRLF
cQuery += "	ORDER BY CODSKU "
	
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
