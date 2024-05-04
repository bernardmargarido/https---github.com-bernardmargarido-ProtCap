#INCLUDE "PROTHEUS.CH"
#INCLUDE "TBICONN.CH"

/*********************************************************************************/
/*/{Protheus.doc} ECLOJXFUN
	@description Funções utilizadas template e-Commerce
	@author Bernard M. Margarido    
	@since 23/04/2019
	@version 1.0
	@type function
/*/
/*********************************************************************************/

/**************************************************************************************************
	Função:
	AEcoGrvLog
	Autor:
	Bernard M. Margarido
	Data:
	02/02/2016
	Descrição:
	Rotina realiza a gravação dos Logs de Integração eCommerce
	Parâmetros:
	Param01 - Codigo da Interface
	Param02 - Descrição Interface
	Param03 - Data de Integração
	Param04 - Hora de Inicio da Integração
	Param05 - Hora de Termino da Integração
	Param06 - Status da Integração(0 - Sucesso, 1 - Erro)
	Param07 - Quantidade de Itens Integrados
	Param08 - Texto com os Erros (caso haja) 
	Param09 - Tipo de Gravação (1-Inicial,2-Final,3-Inicial/Final)
	Retorno:
	Nenhum
**************************************************************************************************/
User Function AEcoGrvLog(cCodigo,cDescricao,cStatus,cMsgErro,cChave,cPolitica,nIDVtex,nTenta,nRegRep,nIdLV)
Local aArea 		:= GetArea()

Local _nTTab		:= TamSX3("ZT0_TABELA")[1]
Local _nTChv 		:= TamSX3("ZT0_CHAVE")[1]
Local _nProcess		:= TamSX3("ZT0_PROCES")[1]

Local _lGrava 		:= .T.

Default cCodigo		:= "999" 
Default cDescricao	:= "sem dados" 
Default cStatus		:= "0" 
Default cMsgErro	:= "" 
Default cChave		:= "" 
Default cPolitica	:= "" 
Default nIDVtex		:= 0 
Default nTenta		:= 0 
Default nRegRep		:= 0 
Default nIdLV		:= 0 

//--------------------------------------+
// Status                               |
//--------------------------------------+
// 0 - Nao processado                   |
// 1 - Processado com sucesso           |
// 2 - Erro aguardando reprocessamento  |
// 3 - Reprocessado                     | 
// 4 - Ignorado                         |
// 5 - Erro em todas tentativas         |
//--------------------------------------+

dbSelectArea("ZT0")
ZT0->( dbSetOrder(3) )

If ZT0->( dbSeek(xFilial("ZT0") + PadR(cCodigo,_nTTab) + PadR(cChave,_nTChv) + Padr(cDescricao,_nProcess)) )
	If ZT0->ZT0_STATUS $ "2/5"
		nTenta := ZT0->ZT0_TENTAT + 1
		_lGrava:= .F.
	EndIf 
EndIf 

RecLock("ZT0",_lGrava)
	ZT0->ZT0_FILIAL	:= xFilial("ZT0")
	ZT0->ZT0_DATA	:= Date()	  
	ZT0->ZT0_HORA	:= Time()  
	ZT0->ZT0_USER  	:= cUserName
	ZT0->ZT0_TABELA	:= cCodigo
	ZT0->ZT0_CHAVE 	:= cChave
	ZT0->ZT0_PROCES	:= cDescricao
	ZT0->ZT0_IDVTX 	:= cValToChar(nIDVtex)
	ZT0->ZT0_STATUS	:= cStatus
	ZT0->ZT0_MENSAG	:= cMsgErro
	ZT0->ZT0_TENTAT	:= nTenta
	ZT0->ZT0_REGREP	:= nRegRep
	ZT0->ZT0_IDLV  	:= cValToChar(nIdLV)
	ZT0->ZT0_POLCOM	:= cPolitica
ZT0->( MsUnLock() )
	
RestArea(aArea)
Return .T.

/**************************************************************************************************
	Função:
	AEcoMail
	Autor:
	Bernard M. Margarido
	Data:
	02/02/2016
	Descrição:
	Rotina realiza o envio de email com os logs
	Parâmetros:
	Param01 - Codigo da Interface
	Param02 - Descrição Interface
	Param03 - Array com os erros
	Retorno:
	Nenhum
**************************************************************************************************/
User Function AEcoMail(cCodInt,cDescInt,aMsgErro,_cPDF,_cDirEtq,_aETQ)
	Local aArea		:= GetArea()

	Local cServer	:= "sandbox.smtp.mailtrap.io:587" //GetMv("MV_RELSERV")
	Local cUser		:= "efe3aee1d63088"//GetMv("MV_RELAUSR")
	Local cPassword := "e3ad8a124631af" //GetMv("MV_RELAPSW")
	Local cFrom		:= GetMv("MV_RELACNT")

	Local cMail		:= GetNewPar("EC_LOGMAIL")
	Local cBody		:= ""	
	Local cRgbCol	:= ""
	Local cTitulo	:= "BEPI - Integrações e-Commerce"
	Local cEndLogo	:= "https://www.protcap.com.br/themes/protcap/assets/imgs/logo-protcap-iso.jpg" 

	Local nErro		:= 0
	Local _nPort	:= 0
	Local _nPosTmp	:= 0

	Local _xRet		

	Local lEnviado	:= .F.
	Local lZebra	:= .T.
	Local lRelauth  := SuperGetMv("MV_RELAUTH",, .F.)

	Local _oServer	:= Nil
	Local _oMessage	:= Nil

	Default	_cPDF	:= ""
	Default _cDirEtq:= ""
	Default	_aETQ	:= {}
	
	//---------------------------------------------+
	// Montagem do Html que sera enciado com erros |
	//---------------------------------------------+
	cBody := '    <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">'
	cBody += '    <html xmlns="http://www.w3.org/1999/xhtml">'
	cBody += '    <head>'
	cBody += '        <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />' 
	cBody += '        <title>' + cTitulo + '</title>'
	cBody += '    </head>'
	cBody += '    <body>'
	cBody += '        <table width="1000" height="10" border="0" bordercolor="#000000" bgcolor="#FFFFFF">'
	cBody += '            <tr align="center">'
	cBody += '                <td align="center" bgcolor="#0d0000">'
	cBody += '                    <img src="' + Alltrim(cEndLogo)  + '" height="070" width="180" />'
	cBody += '                </td>'    
	cBody += '            </tr>'
	//cBody += '         </table>' + CRLF
	//cBody += '         <table width="818" border="0" bordercolor="#333333" bgcolor="#FFFFFF">' + CRLF   
	cBody += '            <tr>'
	cBody += '                <td width="1000" align= "center">'
	cBody += '                    <font color="#999999" size="+2" face="Arial, Helvetica, sans-serif"><b>' + cCodInt + " - " + Capital(cDescInt) + '</b></font>'
	cBody += '                </td>'
	cBody += '            </tr>'
	cBody += '		   </table>'
	cBody += '         <table width="1000" border="0"  bordercolor="#000000" bgcolor="#FFFFFF">'   
	cBody += '            <tr bordercolor="#FFFFFF" bgcolor="#0d0000">'
	cBody += '                <td width="15%" height="30" align= "left">'
	cBody += '                    <font color="#FFFFFF" size="-1" face="Arial, Helvetica, sans-serif"><b>Código</b></font><br>'
	cBody += '                </td>'
	cBody += '                <td width="85%" height="30" align= "left">'
	cBody += '                    <font color="#FFFFFF" size="-1" face="Arial, Helvetica, sans-serif"><b>Descrição</b></font><br>'
	cBody += '                </td>'
	cBody += '            </tr>'

	For nErro := 1 To Len(aMsgErro)

		If lZebra
			lZebra	:= .F.
			cRgbCol := "#FFFFFF"	
		Else
			lZebra	:= .T.
			cRgbCol := "#A9A9A9"
		EndIf	

		cBody += '            <tr bordercolor="#FFFFFF" bgcolor="' + cRgbCol + '">'
		cBody += '                <td width="15%" height="30" align= "left">'
		cBody += '                    <font color="#000000" size="-1" face="Arial, Helvetica, sans-serif">' + Alltrim(aMsgErro[nErro][1])  + '</font>'
		cBody += '                </td>'
		cBody += '                    <td width="85%" height="30" align= "left">'
		cBody += '                    <font color="#000000" size="-1" face="Arial, Helvetica, sans-serif">' + Alltrim(aMsgErro[nErro][2]) + '</font>'
		cBody += '                </td>'
		cBody += '            </tr>'

	Next nErro
	cBody += '        </table>'
	cBody += '        <br><br><br>'
	cBody += '        <font color="#000000" size="-1" face="Arial, Helvetica, sans-serif">VitreoERP - eCommerce <font face="Times New Roman">&copy;</font> - Enviado em ' + dToc(dDataBase) + ' - ' + Time() + '</font>'
	cBody += '    </body>'
	cBody += '    </html>'

	//-------------------------+	
	// Realiza envio do e-mail | 
	//-------------------------+
	lEnviado := .T.	
	_oServer := TMailManager():New()
	_oServer:SetUseTLS(.T.)

	If ( _nPosTmp := At(":",cServer) ) > 0
		_nPort := Val(SubStr(cServer,_nPosTmp+1,Len(cServer)))
		cServer := SubStr(cServer,1,_nPosTmp-1)
	EndIf

	If  ( _xRet := _oServer:Init( "", cServer, cUser, cPassword,,_nPort) ) == 0
		If ( _xRet := _oServer:SMTPConnect()) == 0
			If lRelauth
				If ( _xRet := _oServer:SMTPAuth( cUser, cPassword ))  <> 0
					_xRet := _oServer:SMTPAuth( SubStr(cUser,1,At("@",cUser)-1), cPassword )
				EndIf
			Endif

			If _xRet == 0
				
				_oMessage := TMailMessage():New()
				_oMessage:Clear()
				
				_oMessage:cDate  	:= cValToChar( Date() )
				_oMessage:cFrom  	:= cFrom
				_oMessage:cTo   	:= cMail
				//_oMessage:cCc   	:= cEmailCc
				_oMessage:cSubject 	:= cTitulo
				_oMessage:cBody   	:= cBody
				
				If (_xRet := _oMessage:Send( _oServer )) <> 0
					Conout("Erro ao enviar e-mail --> " + _oServer:GetErrorString( _xRet ))	
					lEnviado := .F.
				Endif
			Else
				Conout("Erro ao enviar e-mail --> " + _oServer:GetErrorString( _xRet ))	
				lEnviado := .F.
			EndIf
		EndIf
	Else
		Conout("Erro ao Conectar ! ")
	EndIf

	RestArea(aArea)

Return lEnviado

/*****************************************************************************/
/*/{Protheus.doc} SYACENTO
	@description Rotina formata texto para o padrao Protheus
	@author Symm Consultoria
	@since 18/08/2016
	@version undefined
	@param cTexto		, Texto a ser formatado
	@param lUpper		, Se verdadeiro retorna texto em Maiusculo
	@type function
/*/
/****************************************************************************/
User Function ECACENTO(cTexto, lUpper,lMun)

	Local nCount 	:= 0 					
	Local z      	:= 0

	Local aAcentos 	:= {}

	Default lMun	:= .F.

	If ValType(cTexto) <> "C"
		Return(cTexto)
	EndIf

	If lUpper == Nil
		lUpper := .T.
	EndIf

	//----------------------------------------------------------+
	//  Carrega os acentos e respectivos caracteres substitutos |
	//----------------------------------------------------------+
	AADD(aAcentos, {"áàãâ"	, "a"})
	AADD(aAcentos, {"ÁÀÃÂ"	, "A"})
	AADD(aAcentos, {"éèê"	, "e"})
	AADD(aAcentos, {"ÉÈÊ"	, "E"})
	AADD(aAcentos, {"íìî"	, "i"})
	AADD(aAcentos, {"ÍÌÎ"	, "I"})
	AADD(aAcentos, {"óòõô"	, "o"})
	AADD(aAcentos, {"ÓÒÕÔ"	, "O"})
	AADD(aAcentos, {"úùûü"	, "u"})
	AADD(aAcentos, {"ÚÙÛÜ"	, "U"})
	AADD(aAcentos, {"ç"		, "c"})
	AADD(aAcentos, {"Ç"		, "C"})
	AADD(aAcentos, {"Ñ"		, "N"})
	AADD(aAcentos, {"ñ"		, "n"})
	AAdd(aAcentos, {"?!:,./\|@#$%&"," "})

	If !lMun
		AAdd(aAcentos, {"'",""})
		AAdd(aAcentos, {"-",""})	
	EndIf	
	AAdd(aAcentos, {"  "," "})

	//-------------------------------------------+
	// Troca os caracteres caso encontre acentos |
	//-------------------------------------------+
	For nCount := 1 to Len(aAcentos)
		For z := 1 to Len(aAcentos[nCount][1])
			cTexto := StrTran(cTexto, SubStr(aAcentos[nCount][1], z, 1), aAcentos[nCount][2])
		Next z
	Next nCount

Return( If(lUpper, Upper(cTexto), cTexto) )

/**********************************************************************************************/
/*/{Protheus.doc} SYFORMAT
	@description Rotina retira caracteres especiais dos campos 
	@author Symm Consultoria
	@since 18/08/2016
	@version undefined
	@param cTexto		, Texto a ser formatado
	@param cCpo			, Campo Protheus
	@param lFormata		, Se formata campo 
	@param cTipo		, Tipo do campo 
	@type function
/*/
/***********************************************************************************************/
User Function ECFORMAT(cTexto, cCpo, lFormata, cTipo)

	Local cAux     := ""
	Local nI       := 0     
	Local cAcentos := "áàãâÁÀÃÂéèêÉÈÊíìîÍÌÎóòõôÓÒÕÔúùûÚÙÛçÇÑñ"
	Local cOutros  := "?!:,./\|@#$%& "

	Default lFormata := .F.     
	Default cTipo    := "N" 

	Do Case
		Case cTipo == "N"
		For nI := 1 To Len(cTexto)                   
			If (ASC( SubStr(cTexto, nI, 1) ) >= 48) .And. (ASC( SubStr(cTexto, nI, 1) ) <= 57)
				cAux += SubStr(cTexto, nI, 1)
			EndIf
		Next nI                                        

		Case cTipo == "C"
		For nI := 1 To Len(cTexto)                   
			If (ASC( SubStr(cTexto, nI, 1) ) >= 48)  .And. (ASC( SubStr(cTexto, nI, 1) ) <= 57)  .Or.; 
			(ASC( SubStr(cTexto, nI, 1) ) >= 65)  .And. (ASC( SubStr(cTexto, nI, 1) ) <= 90)  .Or.;
			(ASC( SubStr(cTexto, nI, 1) ) >= 97)  .And. (ASC( SubStr(cTexto, nI, 1) ) <= 122) 
				If (SubStr(cTexto, nI, 1) $ Alltrim(cOutros)) .Or. (SubStr(cTexto, nI, 1) $ Alltrim(cAcentos))
					If SubStr(cTexto, nI, 1) == "&"
						cAux += "e"
					Else
						cAux += SubStr(cTexto, nI, 1)
					EndIf
				Else
					cAux += SubStr(cTexto, nI, 1)
				EndIf
			EndIf
		Next nI                                        
	EndCase		

	If lFormata
		cAux := PadR(AllTrim(cAux), TamSx3(cCpo)[01])
	EndIf

Return cAux

/******************************************************************************/
/*/{Protheus.doc} AEcoStaLog
	Rotina grava a status do pedido e-commerce
	@author	Bernard M. Margarido
	@since		18/02/2016
	@version	1.00
/*/
/******************************************************************************/
User Function AEcoStaLog(cCodSta,cOrderId,dDtaEmiss,cHora)
	Local aArea			:= GetArea()

	Default dDtaEmiss 	:= dDataBase
	Default cHora		:= Time() 

	dbSelectArea("XTM")
	XTM->( dbSetOrder(1) )
	If !XTM->( dbSeek(xFilial("XTM") + cOrderId + cCodSta ) )
		RecLock("XTM",.T.)
			XTM->XTM_FILIAL := xFilial("XTM")
			XTM->XTM_IDECOM	:= cOrderId
			XTM->XTM_DATA	:= dDtaEmiss
			XTM->XTM_HORA	:= cHora
			XTM->XTM_CODSTA	:= cCodSta
		XTM->( MsUnLock() )	   	
	EndIf	
	RestArea(aArea)
Return .T.

/***********************************************************************************/
/*/{Protheus.doc} GrvStaEc
    @description Valida se pedido de venda esta liberado ou bloquedo
    @type  Function
    @author Bernard M. Margarido
    @since 06/07/2019
    @version version
/*/
/***********************************************************************************/
User Function GrvStaEc(_cOrderId,_cCodSta)
Local _aArea	:= GetArea()

Local _nTOrderId:= TamSx3("XTA_NUMECO")[1]

//---------------+
// Valida Status |
//---------------+
dbSelectArea("ZTC")
ZTC->( dbSetOrder(1))
If !ZTC->( dbSeek(xFilial("ZTC") + _cCodSta))
	CoNout("STATUS " + _cCodSta + " NAO LOCALIZADO.")
	RestArea(_aArea)
	Return Nil
EndIf

//-----------------------------+
// Posiciona pedido e-Commerce | 
//-----------------------------+
dbSelectArea("XTA")
XTA->( dbSetOrder(2) )
If XTA->( dbSeek(xFilial("XTA") + PadR(_cOrderId,_nTOrderId)) )
	RecLock("XTA",.F.)
		XTA->XTA_CODSTA		:= ZTC->ZTC_ORDEM
		XTA->XTA_DESTAT		:= RTrim(ZTC->ZTC_DESCV3)
	XTA->( MsUnLock() )
EndIf

//------------------+
// Grava status TGV |
//------------------+
If !Empty(XTA->XTA_NUMSUA)
	U_EcStaSUA(XTA->XTA_NUMSUA,ZTC->ZTC_SIGLA)
EndIf 

//------------------------+
// Grava Status do Pedido |
//------------------------+
u_AEcoStaLog(ZTC->ZTC_ORDEM,XTA->XTA_NUMECO,dDataBase,Time())

//--------------------------------+
// Envia Status para o e-Commerce |
//--------------------------------+
If ZTC->ZTC_INTEGR == "2"
	U_AECOI11B(XTA->XTA_NUM,.F.)
EndIf

RestArea(_aArea)
Return Nil

/**************************************************************************************************/
/*/{Protheus.doc} AEcoPerDes
	@description	Calcula percentual de desconto
	@author			Bernard M.Margarido
	@version   		1.00
	@since     		10/02/2016
	@param			nVlr		, Valor Total
	@param			nVlrDesc	, Valor do Desconto
	@return			nPerDes		, Retorna percentual de desconto calculado 
/*/
/**************************************************************************************************/
User Function AEcoPerDes(nVlr,nVlrDesc)
	Local nPerDesc 	:= 0
	Local nVlrDif	:= 0
	
	nVlrDif		:= nVlr - nVlrDesc
	nPerDesc 	:= Round((nVlrDif / nVlr) * 100,2)
	nPerDesc	:= IIF(nPerDesc >= 100,99.99,nPerDesc)	
Return nPerDesc

/*********************************************************************************/
/*/{Protheus.doc} ECLOJXFUN
	@description Funções utilizadas template e-Commerce
	@author Bernard M. Margarido    
	@since 23/04/2019
	@version 1.0
	@type function
/*/
/*********************************************************************************/
User Function EcVldNF(_cDoc,_cSerie,_cOrderId)
Local _aArea	:= GetArea()

Local _cCodSta	:= "010"
Local _cEnvLog 	:= "3"

//-------------------------------+
// Atualiza dados da nota fiscal |
//-------------------------------+
dbSelectArea("SF2")
SF2->( dbSetOrder(1) )
If !SF2->( dbSeek(xFilial("SF2") + _cDoc + _cSerie ) )
	RestArea(_aArea)
	Return .F.	
EndIf

RecLock("SF2",.F.)
	SF2->F2_XIDVTV3 := _cOrderId
SF2->( MsUnLock() )	 

//-------------------------------+
// Atualiza status para faturado | 
//-------------------------------+
dbSelectArea("ZTC")
ZTC->( dbSetOrder(1) )
ZTC->( dbSeek(xFilial("ZTC") + _cCodSta) )

//-------------------------------------+
// Atualiza dados da nota no orçamento |
//-------------------------------------+
dbSelectArea("XTA")
XTA->( dbSetOrder(2) )
If XTA->( dbSeek(xFilial("XTA") + _cOrderId) )
	RecLock("XTA",.F.)
		XTA->XTA_DOC	:= _cDoc
		XTA->XTA_SERIE	:= _cSerie
		XTA->XTA_CODSTA	:= _cCodSta
		XTA->XTA_DESTAT	:= RTrim(ZTC->ZTC_DESCV3)
		XTA->XTA_ENVLOG	:= _cEnvLog
	XTA->( MsUnlock() )
EndIf

//------------------+
// Grava status TGV |
//------------------+
U_EcStaSUA(XTA->XTA_NUMSUA,ZTC->ZTC_SIGLA)

//------------------------+
// Grava Status do Pedido |
//------------------------+
u_AEcoStaLog(_cCodSta,_cOrderId,dDataBase,Time())

//---------------+
// Envia Invoice |
//---------------+
/*
If WS1->WS1_ENVECO == "S" .And. Empty(WSA->WSA_SERPOS)
	U_AECOI013(_cOrderId)
EndIf
*/

RestArea(_aArea)
Return .T.

/*********************************************************************************/
/*/{Protheus.doc} EcStaSUA
	@description Atualiza status TGV
	@type  Function
	@author Bernard M Margarido
	@since 02/04/2024
	@version version
/*/
/*********************************************************************************/
User Function EcStaSUA(_cNumOrc,_cSigla)
Local _aArea	:= GetArea()

//----------------------------+
// SUA - Posiciona orçamentos |
//----------------------------+
dbSelectArea("SUA")
SUA->( dbSetOrder(1) )
If SUA->( dbSeek(xFilial("SUA") + _cNumOrc) )
	RecLock("SUA",.F.)
		SUA->UA_XSTATUS := _cSigla
	SUA->( MsUnLock() )
EndIf 

RestArea(_aArea)
Return Nil 
