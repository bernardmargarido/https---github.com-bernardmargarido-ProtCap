#INCLUDE "PROTHEUS.CH"
#INCLUDE "APWEBSRV.CH"
#INCLUDE "TOPCONN.CH"
#INCLUDE "TBICONN.CH"
#INCLUDE "AARRAY.CH"
#INCLUDE "JSON.CH"

#DEFINE CRLF CHR(13) + CHR(10)

Static cCodInt	:= "013"
Static cDescInt	:= "Invoice"
Static cDirImp	:= "/ecommerce/"


/************************************************************************************/
/*/{Protheus.doc} AECOI013
	@description Realiza o envio do numero da nota para o e-Commerce
	@author Bernard M. Margarido
	@since 13/02/2017
	@version undefined
	@param cNumOrc		, characters, OrderID eCommerce
	@type function
/*/
/************************************************************************************/
User function AECOI013(cNumOrc)
Local aArea			:= GetArea()
Local aRet			:= {.T.,"",""}

Local _lBloqueio	:= GetNewPar("EC_BLSMSG",.F.)
Local _lRet 		:= .T.

Private cThread		:= Alltrim(Str(ThreadId()))
Private cStaLog		:= "0"
Private cArqLog		:= ""	

Private nQtdInt		:= 0
Private _nTDoc 		:= TamSx3("F2_DOC")[1]
Private _nTSerie	:= TamSx3("F2_SERIE")[1]
Private _nTCodCli	:= TamSx3("F2_CLIENTE")[1]
Private _nTLojCli	:= TamSx3("F2_LOJA")[1]

Private cHrIni		:= Time()
Private dDtaInt		:= Date()

Private aMsgErro	:= {}

Private lJob 		:= IIF(Isincallstack("U_ECLOJM07"),.F.,.T.) 

If _lBloqueio
	RestArea(aArea)
	Return aRet
EndIf

//----------------------------------+
// Grava Log inicio das Integrações | 
//----------------------------------+
u_AEcoGrvLog(cCodInt,cDescInt,dDtaInt,cHrIni,,,,,cThread,1)

//------------------------------+
// Inicializa Log de Integracao |
//------------------------------+
MakeDir(cDirImp)
cArqLog := cDirImp + "INVOICE" + cEmpAnt + cFilAnt + ".LOG"
ConOut("")	
LogExec(Replicate("-",80))
LogExec("<< AECOI013 >> - INICIA ENVIO DE INVOICE COM A VTEX - DATA/HORA: "+DTOC(DATE())+" AS "+TIME())

//-----------------------------------------+
// Inicia processo de envio das categorias |
//-----------------------------------------+
If lJob
	_lRet := AECOINT13(cNumOrc)
Else
	Processa({|| _lRet := AECOINT13(cNumOrc) },"Aguarde...","Enviando invoice.")
EndIf

LogExec("<< AECOI013 >> - FINALIZA ENVIO DE INVOICE COM A VTEX - DATA/HORA: "+DTOC(DATE())+" AS "+TIME())
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

Return _lRet

/**************************************************************************************************/
/*/{Protheus.doc} AECOINT13
	@description	Realiza o envio da Invoice para o e-Commerce
	@author			Bernard M.Margarido
	@version   		1.00
	@since     		02/02/2016
/*/
/**************************************************************************************************/
Static Function AECOINT13(cNumOrc)
Local aArea		:= GetArea()
Local aRet		:= {.F.,"",""}
Local _cCodDLog := GetNewPar("DN_CODDLOG")
Local cOrderID 	:= ""
Local cChaveNfe := ""
Local cTracking := ""
Local cUrlTrack := ""
Local cNumTransp:= ""	
Local cRest		:= ""  
Local cQuant 	:= ""
Local cPrcVen	:= ""
Local cDtaFat	:= ""
Local cVlrFat	:= ""
Local cUrl		:= GetNewPar("EC_URLVTEX")
Local cAppKey	:= GetNewPar("EC_APPVTEX")
Local cAppToken	:= GetNewPar("EC_APTVTEX")
Local cCfop 	:= ""
Local dDtaEmiss	:= ""
Local _cXmlNF	:= ""

Local nIdSku	:= 0
Local _nVlrTotal:= 0
Local _nVolume	:= 0
Local _nTOrc	:= TamSx3("XTA_NUM")[1]

Local _oJson	:= Nil
Local _oItens	:= Nil

//---------------------+
// Posiciona Orçamento |
//---------------------+
dbSelectArea("XTA")
XTA->( dbSetOrder(1) )
If !XTA->( dbSeek(xFilial("XTA") + PadR(cNumOrc,_nTOrc)) )
	LogExec("<< AECOI013 >> - ORCAMENTO " + cNumOrc + " NAO LOCALIZADO." )
	RestArea(aArea)
	Return .F.
EndIf 

//----------------------------------+
// Consulta Data de Emissao da Nota |
//----------------------------------+
aEcoI13DtaE(XTA->XTA_DOC,XTA->XTA_SERIE,XTA->XTA_CLIENT,XTA->XTA_LOJA,@cChaveNfe,@_cXmlNF,@dDtaEmiss,@_nVlrTotal,@cCfop,@_nVolume)

cOrderID	:= RTrim(XTA->XTA_NUMECO)
cTracking 	:= Rtrim(XTA->XTA_TRACKI)
cNumTransp	:= XTA->XTA_TRANSP

//------------------+
// Valida se é DLog |
//------------------+
If Rtrim(cNumTransp) $ RTrim(_cCodDLog)
	aEcoI13Url(XTA->XTA_NUMECO,@cUrlTrack,@cTracking)
EndIf
//-----------------------+
// Monta String API Rest |
//-----------------------+
_oJson					:= {}        
_oJson					:= Array(#)	
_oJson[#"type"]			:= "Output"
_oJson[#"invoiceNumber"]:= RTrim(XTA->XTA_DOC) + "-" + RTrim(XTA->XTA_SERIE)

//------------+
// Chave NF-e |
//------------+
If !Empty(cChaveNfe)
	_oJson[#"invoiceKey"]	:= cChaveNfe
EndIf

If Empty(cTracking) .And. At('Shopee',XTA->XTA_NUMECO) > 0 
	cTracking := RTrim(XTA->XTA_DOC) + RTrim(XTA->XTA_SERIE)
EndIf 

_oJson[#"courier"]			:= cNumTransp
_oJson[#"trackingNumber"]	:= cTracking
_oJson[#"trackingUrl"]		:= cUrlTrack
_oJson[#"embeddedInvoice"]	:= StrTran(_cXmlNF,'"',"'")

//-------------------------+
// Posiciona Itens da Nota |
//-------------------------+
_oJson[#"items"]	:= {}

XTB->( dbSetOrder(1) )
XTB->( dbSeek(xFilial("XTB") + XTA->XTA_NUM ) )
While XTB->( !Eof() .And. xFilial("XTB") + XTA->XTA_NUM == XTB->XTB_FILIAL + XTB->XTB_NUM )

	aAdd(_oJson[#"items"],Array(#))
	_oItens				:= aTail(_oJson[#"items"])   
	
	//------------------------------------------+
	// Posiciona Porduto para pegar codigo Vtex |
	//------------------------------------------+
	aEcoI013Sku(XTA->XTA_IDLOJA,XTB->XTB_PRODUT,@nIdSku)
	
	cQuant := Alltrim(Str(Int(XTB->XTB_QUANT)))
	cPrcVen:= cValToChar(RetPrcUni(XTB->XTB_VRUNIT))

	_oItens[#"id"]			:= 	IIF(Empty(XTB->XTB_KIT),Alltrim(Str(nIdSku)),RTrim(XTB->XTB_KIT))
	_oItens[#"quantity"]	:= 	cQuant
	_oItens[#"price"]		:= 	cPrcVen
		
	XTB->( dbSkip() )   
	
EndDo

//-----------------------------+
// Data e Valor de Faturamento |
//-----------------------------+
cDtaFat := IIF(Empty(dDtaEmiss),dTos(dDataBase),dTos(dDtaEmiss))
cDtaFat := SubStr(cDtaFat,1,4) + "-" + SubStr(cDtaFat,5,2) + "-" + SubStr(cDtaFat,7,2) //+ "T" + SubStr(Time(),1,8)
cVlrFat	:= cValToChar(RetPrcUni(_nVlrTotal))    

//------------------------+
// Data e Valor da Fatura |
//------------------------+
_oJson[#"cfop"]			:= cCfop
_oJson[#"volumes"]		:= _nVolume
_oJson[#"issuanceDate"]	:= cDtaFat
_oJson[#"invoiceValue"]	:= cVlrFat

//---------------------------+
// Transforma Objeto em JSON |
//---------------------------+
cRest := xToJson(_oJson)

//---------------+
// Envia Invoice |
//---------------+ 
aRet := AEcoI13Inv(XTA->XTA_DOC,XTA->XTA_SERIE,cOrderID,cRest,cUrl,cAppKey,cAppToken)

RestArea(aArea)
Return aRet[1]

/************************************************************************************/
/*/{Protheus.doc} aEcoI13DtaE
	@description Retorna dados da nota fiscal de saida 
	@author Bernard M. Margarido
	@since 18/04/2017
	@type function
/*/
/************************************************************************************/
Static Function aEcoI13DtaE(cDoc,cSerie,cCliente,cLoja,cChaveNfe,_cXmlNF,dDtaEmiss,_nVlrTotal,cCfop,_nVolume)
Local aArea		:= GetArea()


dbSelectArea("SF2")
SF2->( dbSetOrder(1) )
If !SF2->( dbSeek(xFilial("SF2") + Padr(cDoc,_nTDoc) + Padr(cSerie,_nTSerie) + PadR(cCliente,_nTCodCli) + PadR(cLoja,_nTLojCli)) )
	RestArea(aArea)
	Return .F.
EndIf	 

dbSelectArea("SD2")
SD2->( dbSetOrder(3))
If !SD2->( dbSeek(xFilial("SF2") + Padr(cDoc,_nTDoc) + Padr(cSerie,_nTSerie) + PadR(cCliente,_nTCodCli) + PadR(cLoja,_nTLojCli)) )
	RestArea(aArea)
	Return .F.
EndIf	 

//---------------+
// Dados da nota |
//---------------+
cChaveNfe := SF2->F2_CHVNFE
dDtaEmiss := SF2->F2_EMISSAO
_nVlrTotal:= SF2->F2_VALBRUT	
_nVolume  := SF2->F2_VOLUME1
cCfop	  := SD2->D2_CF

_cXmlNF   := AEcoI13CC(Padr(cDoc,_nTDoc),Padr(cSerie,_nTSerie))

RestArea(aArea)
Return .T.

/********************************************************************/
/*/{Protheus.doc} aEcoI13Url
	@description Retorna URL de rastreio 
	@author Bernard M. Margarido
	@since 14/02/2017
	@version undefined
	@type function
/*/
/********************************************************************/
Static Function aEcoI13Url(_cNumEco,cUrlTrack,cTracking)
Local _cAlias	:= ""
Local _cQuery   := ""

Local _oJSonUrl	:= JsonObject():New() 

Private _cType	:= ""

_cQuery := " SELECT " + CRLF
_cQuery	+= "	ZZC.R_E_C_N_O_ RECNOZZC, " + CRLF
_cQuery	+= "	ZZC.ZZC_STATUS STATUS, " + CRLF
_cQuery	+= "	CAST(CAST( ZZC.ZZC_JSON AS BINARY(2048)) AS VARCHAR(2048)) JSON_DLOG " + CRLF
_cQuery	+= " FROM " + CRLF
_cQuery	+= "	" + RetSqlName("ZZC") + " ZZC " + CRLF 
_cQuery	+= " WHERE " + CRLF
_cQuery	+= "	ZZC.ZZC_FILIAL = '" + xFilial("ZZC") + "' AND " + CRLF
_cQuery	+= "	ZZC.ZZC_NUMECO = '" + _cNumEco + "' AND " + CRLF
_cQuery	+= "	ZZC.D_E_L_E_T_ = '' " + CRLF

_cAlias := MPSysOpenQuery(_cQuery)

If (_cAlias)->STATUS == "2" .And. !Empty((_cAlias)->JSON_DLOG)
	//_oJSonUrl 	:= xFromJson(RTrim((_cAlias)->JSON_DLOG))
	_oJSonUrl:fromJson(RTrim((_cAlias)->JSON_DLOG))
	If ValType(_oJSonUrl) <> "U"
		_cType := '_oJSonUrl["linkRastreamento"]'
		If ValType(_oJSonUrl["linkRastreamento"]) <> "U"
			cUrlTrack	:= _oJSonUrl["linkRastreamento"]
			cTracking	:= SubStr(cUrlTrack,Rat("/",cUrlTrack) + 1)
		EndIf 
	EndIf 
EndIf

(_cAlias)->( dbCloseArea() )

Return Nil 

/********************************************************************/
/*/{Protheus.doc} AEcoI13Inv
	@description Rotina realiza o envio da Invoice para o e-Commerce
	@author Bernard M. Margarido
	@since 14/02/2017
	@version undefined
	@type function
/*/
/********************************************************************/
Static Function AEcoI13Inv(cDocNum,cSerie,cOrderID,cRest,cUrl,cAppKey,cAppToken)
Local aRet			:= {.T.,"",""}

//Local cUrl		:= GetNewPar("EC_URLREST")
//Local cAppKey		:= GetNewPar("EC_APPKEY")
//Local cAppToken	:= GetNewPar("EC_APPTOKE")

Local nTimeOut		:= 240
Local _nHttpSta		:= 0

Local aHeadOut  	:= {}
Local cXmlHead 	 	:= ""     
Local cRetPost  	:= ""
Local oXmlRet   	:= Nil 

MakeDir("\ecommerce\")
MakeDir("\ecommerce\arquivos\")
MakeDir("\ecommerce\arquivos\invoice")
MemoWrite("ecommerce\arquivos\invoice\jsoninvoice_" + RTrim(cDocNum) + "_" + RTrim(cSerie) + ".json",cRest)

aAdd(aHeadOut,"Content-Type: application/json" )
aAdd(aHeadOut,"X-VTEX-API-AppKey:" + cAppKey )
aAdd(aHeadOut,"X-VTEX-API-AppToken:" + cAppToken ) 
                     
cRetPost	:= HttpPost(cUrl + "/api/oms/pvt/orders/" + Alltrim(cOrderID) + "/invoice","",cRest,nTimeOut,aHeadOut,@cXmlHead) 
_nHttpSta 	:= HTTPGetStatus()

If _nHttpSta == 200 
	If FWJsonDeserialize(cRetPost,@oXmlRet)
		If ValType(oXmlRet) == "O"	
			aRet[1] := .T.
			aRet[2] := cDocNum + cSerie
			aRet[3] := "INVOICE" + cDocNum + cSerie + "ENVIADA COM SUCESSO "
			
			LogExec("<< AECOI013 >> - INVOICE" + cDocNum + cSerie + "ENVIADA COM SUCESSO " )
		EndIf
	Else
		aRet[1] := .T.
		aRet[2] := cDocNum + cSerie
		aRet[3] := "INVOICE" + cDocNum + cSerie + "ENVIADA COM SUCESSO "
		
		LogExec("<< AECOI013 >> - INVOICE" + cDocNum + cSerie + "ENVIADA COM SUCESSO " + CRLF + cRetPost )
	EndIf		
	
Else 
	If FWJsonDeserialize(cRetPost,@oXmlRet)
		If ValType(oXmlRet) == "O"	
			aRet[1] := .F.
			aRet[2] := cDocNum + cSerie 	
			aRet[3] := "ERRO AO ENVIAR INVOICE " + cDocNum + " / " + cSerie 
			
			aAdd(aMsgErro,{aRet[2],aRet[3]})
			
			LogExec("<< AECOI013 >> - ERRO AO ENVIAR INVOICE " + cDocNum + " / " + cSerie) 
		EndIf	
	EndIf	
EndIf

Return aRet

/*******************************************************************************/
/*/{Protheus.doc} aEcoI013Sku
	@description Consulta SKU
	@author Bernard M. Margarido
	@since 23/01/2018
	@version 1.0
	@type function
/*/
/*******************************************************************************/
Static Function aEcoI013Sku(_cIdLoja,cCodProd,nIdSku)
Local aArea		:= GetArea()
Local cQuery 	:= ""
Local cAlias	:= GetNextAlias()

Default _cIdLoja := ""

If Empty(_cIdLoja )

	cQuery := "	SELECT " + CRLF
	cQuery += "		IDSKU " + CRLF
	cQuery += "	FROM " + CRLF
	cQuery += "	( " + CRLF
	cQuery += "		SELECT " + CRLF
	cQuery += "			B5.B5_XIDSKU IDSKU " + CRLF
	cQuery += "		FROM " + CRLF
	cQuery += "			" + RetSqlName("SB5") + " B5 " + CRLF
	cQuery += "		WHERE " + CRLF
	cQuery += "			B5.B5_FILIAL = '" + xFilial("SB5") + "' AND " + CRLF 
	cQuery += "			B5.B5_COD = '" + cCodProd + "' AND  " + CRLF
	cQuery += "			B5.D_E_L_E_T_ = '' " + CRLF 
	cQuery += "	)SKU "

Else 
	cQuery := " SELECT " + CRLF
	cQuery += "		IDSKU " + CRLF
	cQuery += " FROM " + CRLF
	cQuery += " ( " + CRLF
	cQuery += "		SELECT " + CRLF
	cQuery += "			XTD.XTD_IDECOM IDSKU " + CRLF
	cQuery += "		FROM " + CRLF
	cQuery += "			" + RetSqlName("XTD") + " XTD " + CRLF
	cQuery += "		WHERE " + CRLF
	cQuery += "			XTD.XTD_FILIAL = '" + xFilial("XTD") + "' AND  " + CRLF
	cQuery += "			XTD.XTD_CODIGO = '" + _cIdLoja + "' AND " + CRLF
	cQuery += "			XTD.XTD_ALIAS = 'SB5' AND " + CRLF
	cQuery += "			XTD.XTD_CODERP = '" + cCodProd + "' AND " + CRLF
	cQuery += "			XTD.D_E_L_E_T_ = ''  " + CRLF
	cQuery += " )SKU
EndIf 

dbUseArea(.T.,"TOPCONN",TcGenQry(,,cQuery),cAlias,.T.,.T.)

nIdSku := (cAlias)->IDSKU

(cAlias)->( dbCloseArea() )

RestArea(aArea)
Return .T.

/*******************************************************************/
/*/{Protheus.doc} RetPrcUni
	@description Formata valor para envio ao eCommerce
	@author Bernard M. Margarido
	@since 13/02/2017
	@version undefined
	@type function
/*/
/*******************************************************************/
Static Function RetPrcUni(nVlrUnit) 
Local nValor	:= 0
	nValor		:= NoRound(nVlrUnit,2) * 100
Return nValor

/**************************************************************************************************/
/*/{Protheus.doc} AEcoI13CC
	@description Consulta XML da Nota Fiscal 
	@type  Static Function
	@author Bernard M Margarido
	@since 24/08/2022
	@version version
/*/
/**************************************************************************************************/
Static Function AEcoI13CC(_cDoc,_cSerie)
Local _cStatic		:= "S"+"t"+"a"+"t"+"i"+"c"+"C"+"a"+"l"+"l"
Local _cURL     	:= PadR(GetNewPar("MV_SPEDURL","http://"),250)  
Local _cIdEnt 		:= ""
Local _cXML 		:= ""
Local _cCnpjDIni	:= "              "
Local _cCnpjDFim	:= "99999999999999"
Local cVerNfe		:= ""
Local cVerCte		:= ""
Local cNotaIni 		:= ""
Local cNFes 		:= ""
Local cChvNFe  		:= ""
Local cModelo 		:= ""
Local cPrefixo		:= ""
Local cCab1			:= ""
Local cRodap		:= ""
Local _cCNPJDEST	:= Space(14) 

Local _nX			:= 0

Local _dDtaIni		:= DaySub(Date(),15)
Local _dDtaFim		:= Date()

Private oWS 		:= Nil 
Private oRetorno	:= Nil 
Private oXmlExp		:= Nil 
Private oXml		:= Nil 

Private _cType	:= ''

_cIdEnt	:= Eval( {|| &(_cStatic + "(" + "SPEDNFE, GetIdEnt" + ")") }) 
_cDocIni:= _cSerie + _cDoc
_cDocFim:= _cSerie + _cDoc

oWS:= WSNFeSBRA():New()
oWS:cUSERTOKEN        := "TOTVS"
oWS:cID_ENT           := _cIdEnt
oWS:_URL              := RTrim(_cURL)+"/NFeSBRA.apw"   
oWS:cIdInicial        := _cDocIni
oWS:cIdFinal          := _cDocFim
oWS:dDataDe           := _dDtaIni
oWS:dDataAte          := _dDtaFim
oWS:cCNPJDESTInicial  := _cCnpjDIni
oWS:cCNPJDESTFinal    := _cCnpjDFim
oWS:nDiasparaExclusao := 0

If oWS:RetornaFx()
	oRetorno 	:= oWS:oWsRetornaFxResult


	For _nX := 1 To Len(oRetorno:OWSNOTAS:oWsNfeS3)

		oXml     	:= oRetorno:oWsNotas:oWsNfeS3[_nX]
		oXmlExp 	:= XmlParser(oRetorno:oWsNotas:oWsNfeS3[_nX]:oWsNfe:cXml,"","","")
		_cXML		:= ""

		//--------------------------------------+
		// Valida se pessoa e fisica ou juridica|
		//--------------------------------------+
		If ValAtrib('oXmlExp:_NFE:_INFNFE:_DEST:_CNPJ:TEXT') <> "U" 
			_cCNPJDEST := AllTrim(oXmlExp:_Nfe:_InfNfe:_Dest:_Cnpj:Text)
		ElseIF ValAtrib('oXmlExp:_NFE:_INFNFE:_DEST:_CPF:TEXT') <> "U"
			_cCNPJDEST := AllTrim(oXmlExp:_Nfe:_InfNfe:_Dest:_Cpf:Text)				
		Else
			_cCNPJDEST := ""
		EndIf

		cVerNfe := IIF(ValAtrib('oXmlExp:_NFE:_INFNFE:_VERSAO:TEXT') <> "U", oXmlExp:_NFE:_INFNFE:_VERSAO:TEXT, '')                                 
		cVerCte := IIF(ValAtrib('oXmlExp:_CTE:_INFCTE:_VERSAO:TEXT') <> "U", oXmlExp:_CTE:_INFCTE:_VERSAO:TEXT, '')
		cVerMDfe:= IIF(ValAtrib('oXmlExp:_MDFE:_INFMDFE:_VERSAO:TEXT') <> "U", oXmlExp:_MDFE:_INFMDFE:_VERSAO:TEXT, '')

		If !Empty(oXml:oWSNFe:cProtocolo)
					
			cNotaIni 	:= oXml:cID	 		
			cNFes 		:= cNFes+cNotaIni+CRLF
			cChvNFe  	:= NfeIdSPED(oXml:oWSNFe:cXML,"Id")	 			
			cModelo 	:= cChvNFe
			cModelo 	:= StrTran(cModelo,"NFe","")
			cModelo 	:= StrTran(cModelo,"CTe","")
			cModelo	 	:= StrTran(cModelo,"MDFe","")
			cModelo 	:= SubStr(cModelo,21,02)
						
			Do Case
				Case cModelo == "57"
					cPrefixo := "CTe"
				Case cModelo == "65"
					cPrefixo := "NFCe"
				Case cModelo == "58"
					cPrefixo := "MDFe"
				OtherWise
					If '<cStat>302</cStat>' $ oXml:oWSNFe:cxmlPROT								
						cPrefixo := "den"								
					Else
						cPrefixo := "NFe"
					EndIf	
			EndCase	 				
			
			cCab1 := '<?xml version="1.0" encoding="UTF-8"?>'
			If cModelo == "57"
				cCab1  += '<cteProc xmlns="http://www.portalfiscal.inf.br/cte" versao="' + cVerCte + '">'
				cRodap := '</cteProc>'
			Else
				Do Case
					Case cVerNfe <= "1.07"
						cCab1 += '<nfeProc xmlns="http://www.portalfiscal.inf.br/nfe" xmlns:ds="http://www.w3.org/2000/09/xmldsig#" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.portalfiscal.inf.br/nfe procNFe_v1.00.xsd" versao="1.00">'
					Case cVerNfe >= "2.00" .And. "cancNFe" $ oXml:oWSNFe:cXML
						cCab1 += '<procCancNFe xmlns="http://www.portalfiscal.inf.br/nfe" versao="' + cVerNfe + '">'
					OtherWise
						cCab1 += '<nfeProc xmlns="http://www.portalfiscal.inf.br/nfe" versao="' + cVerNfe + '">'
				EndCase
				cRodap := '</nfeProc>'
			EndIf

			_cXML := AllTrim(cCab1) + AllTrim(oXml:oWSNFe:cXML) + AllTrim(oXml:oWSNFe:cXMLPROT) + AllTrim(cRodap)
			//_cXML := AllTrim(oXml:oWSNFe:cXML) + AllTrim(oXml:oWSNFe:cXMLPROT)

		EndIf
	Next _nX
EndIf 

FreeObj(oWS)
FreeObj(oRetorno)
FreeObj(oXmlExp)
FreeObj(oXml)

Return _cXML 

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

static Function ValAtrib(atributo)
Return (type(atributo) )
