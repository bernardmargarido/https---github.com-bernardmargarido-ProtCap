#INCLUDE "PROTHEUS.CH"
#INCLUDE "APWEBSRV.CH"
#INCLUDE "TOPCONN.CH"
#INCLUDE "TBICONN.CH"
#INCLUDE "AARRAY.CH"
#INCLUDE "JSON.CH"

#DEFINE CRLF CHR(13) + CHR(10)

#DEFINE DESTIN 1
#DEFINE ENDERE 2
#DEFINE NUMERO 3
#DEFINE IBGE   4
#DEFINE ESTADO 5
#DEFINE MUNICI 6
#DEFINE BAIRRO 7
#DEFINE CEP    8
#DEFINE TELEF1 9
#DEFINE TELEF2 10
#DEFINE CELULA 11
#DEFINE REFERE 12
#DEFINE COMPLE 13
#DEFINE IDENDE 14
#DEFINE CONTAT 15

Static cCodInt	:= "XTA"
Static cDescInt	:= "orders"
Static cDirImp	:= "/ecommerce/"

Static nTamCnpj	:= TamSx3("A1_CGC")[1]
Static nTCodCli	:= TamSx3("A1_COD")[1]
Static nTamTel	:= TamSx3("A1_TEL")[1] 
Static nTamInsc	:= TamSx3("A1_INSCR")[1]
Static nTamBco	:= TamSx3("A6_COD")[1]
Static nTamAge	:= TamSx3("A6_AGENCIA")[1]
Static nTamCon	:= TamSx3("A6_NUMCON")[1]
Static nTamProd	:= TamSx3("B1_COD")[1]
Static nTamItem	:= TamSx3("C6_ITEM")[1]
Static nTamNuSe1:= TamSx3("E1_NUM")[1]	
Static nTamNatur:= TamSx3("E1_NATUREZ")[1]
Static nTamTipo	:= TamSx3("E1_TIPO")[1]
Static nTamParc	:= TamSx3("E1_PARCELA")[1]
Static nTamTitu	:= TamSx3("E1_NUM")[1]
Static nTamOrder:= TamSx3("XTA_NUMECO")[1]
Static nTPedCli	:= TamSx3("XTA_NUMECL")[1]
Static nTItemL2	:= TamSx3("XTB_ITEM")[1]
Static nDecIt	:= TamSx3("XTB_VLRITE")[2]
Static nTamStat	:= TamSx3("ZTC_CODIGO")[1]
Static nTamOper	:= TamSx3("ZTB_CODADM")[1]
Static _nTEst	:= TamSx3("A1_EST")[1]

/**************************************************************************************************/
/*/{Protheus.doc} AECOI011
	@description	Rotina realiza a integração dos pedidos de vendas do e-Commerce
	@type   		Function 
	@author			Bernard M.Margarido
	@version   		1.00
	@since     		10/02/2016
/*/
/**************************************************************************************************/
User Function AECOI011()
Local _aArea		:= GetArea()

Private cThread		:= Alltrim(Str(ThreadId()))
Private cStaLog		:= "0"
Private cArqLog		:= ""	

Private nQtdInt		:= 0

Private cHrIni		:= Time()
Private dDtaInt		:= Date()

Private aMsgErro	:= {}
Private aOrderId	:= {}	

Private _lJob		:= IIF(Isincallstack("U_ECLOJM03"),.T.,.F.)
Private _lMultLj	:= GetNewPar("EC_MULTLOJ",.T.)

Private _oProcess 	:= Nil

//------------------------------+
// Inicializa Log de Integracao |
//------------------------------+
MakeDir(cDirImp)
cArqLog := cDirImp + "PEDIDOVENDA" + cEmpAnt + cFilAnt + ".LOG"
ConOut("")	
LogExec(Replicate("-",80))
LogExec("INICIA INTEGRACAO CLIENTES / PEDIDOS ECOMMERCE - DATA/HORA: " + DTOC(DATE()) + " AS " + TIME())

//----------------------------+
// Inicia processo de Pedidos |
//----------------------------+

If _lMultLj
	If _lJob
		AECOMULT11()
	Else 
		_oProcess:= MsNewProcess():New( {|| AECOMULT11()},"Aguarde...","Consultando Estoque." )
		_oProcess:Activate()
	EndIf 
Else 
	If _lJob
		AECOINT11()
	Else
		_oProcess:= MsNewProcess():New( {|| AECOINT11()},"Aguarde...","Consultando Pedidos." )
		_oProcess:Activate()
	EndIf	

	//-------------------------------+
	// Inicia gravação / atualização |
	//-------------------------------+
	If Len(aOrderId) > 0 
		If _lJob
			AEcoI11PvC()
		Else
			_oProcess:= MsNewProcess():New( {|| AEcoI11PvC()},"Aguarde...","Gravando/Atualizando Novos Pedidos.")
			_oProcess:Activate()
		EndIf	
	EndIf
EndIf 

LogExec("FINALIZA INTEGRACAO CLIENTES / PEDIDOS ECOMMERCE - DATA/HORA: " + DTOC(DATE()) + " AS " + TIME())
LogExec(Replicate("-",80))
ConOut("")

//----------------------------------+
// Envia e-Mail com o Logs de Erros |
//----------------------------------+
If Len(aMsgErro) > 0
	cStaLog := "1"
	u_AEcoMail(cCodInt,cDescInt,aMsgErro)
EndIf

RestArea(_aArea)	
Return .T.

/*****************************************************************************************/
/*/{Protheus.doc} AECOMULT11
	@description Multi Lojas e-Commerce
	@author Bernard M. Margarido
	@since 17/05/2018
	@version 1.0
	@type function
/*/
/*****************************************************************************************/
Static Function AECOMULT11()
Local _aArea		:= GetArea()

Local _cFilAux 		:= cFilAnt 

Private _cLojaID	:= ""
Private _cUrl		:= ""
Private _cUrl_2		:= ""
Private _cAppKey	:= ""
Private _cAppToken	:= ""

//-----------------+
// Lojas eCommerce |
//-----------------+
dbSelectArea("XTN")
XTN->( dbSetOrder(1) ) 
XTN->( dbGoTop() )

If !_lJob
	_oProcess:SetRegua1( XTN->( RecCount()))
EndIf 

LogExec(" TOTAL REGISTRO " + cValToChar( XTN->( RecCount()) ))

While XTN->( !Eof() )

	If !_lJob	
		_oProcess:IncRegua1("Loja eCommerce " + RTrim(XTN->XTN_IDECOM) )
	EndIf 

	LogExec("Loja eCommerce " + RTrim(XTN->XTN_IDECOM))

	//----------------------+
	// Somente lojas ativas |
	//----------------------+
	If XTN->XTN_STATUS

		//----------------------------+
		// Posiciona a filial correta | 
		//----------------------------+
		If XTN->XTN_FILIAL <> cFilAnt 
			cFilAnt := XTN->XTN_FILIAL
		EndIf  

		//----------------------+
		// Envia as estoque b2b |
		//----------------------+
		_cLojaID	:= RTrim(XTN->XTN_IDECOM)
		_cUrl		:= RTrim(XTN->XTN_URL1)
		_cUrl_2		:= RTrim(XTN->XTN_URL2)
		_cAppKey	:= RTrim(XTN->XTN_APPKEY)
		_cAppToken	:= RTrim(XTN->XTN_APPTOK)
		
		If _lJob
			AECOINT11()
		Else
			_oProcess:= MsNewProcess():New( {|| AECOINT11()},"Aguarde...","Consultando Pedidos." )
			_oProcess:Activate()
		EndIf	

		//-------------------------------+
		// Inicia gravação / atualização |
		//-------------------------------+
		If Len(aOrderId) > 0 
			If _lJob
				AEcoI11PvC()
			Else
				_oProcess:= MsNewProcess():New( {|| AEcoI11PvC()},"Aguarde...","Gravando/Atualizando Novos Pedidos.")
				_oProcess:Activate()
			EndIf	
		EndIf

		//----------------------------+
		// Restaura a filial corrente |
		//----------------------------+
		If _cFilAux <> cFilAnt
			cFilAnt := _cFilAux
		EndIf 

	EndIf
	
	XTN->( dbSkip() )
	
EndDo

RestArea(_aArea)
Return .T.

/**************************************************************************************************/
/*/{Protheus.doc} AECOINT11
	@description	Rotina realiza a integração dos Pedidos de Venda.
	@author			Bernard M.Margarido
	@version   		1.00
	@since     		10/02/2016
/*/
/**************************************************************************************************/
Static Function AECOINT11()
Local _aArea 			:= GetArea()

Local _cError			:= ""

Local _nX				:= 0
Local _nTPages 			:= 0 
Local _nPage 			:= 1

Local _oJSon   			:= Nil 

//---------------------+
// Consulta dados VTEX |
//---------------------+
If AEcoI11X(_nPage,@_cError,@_oJSon)
	//---------------------------+
	// Valida se retornou Objeto |
	//---------------------------+
	If ValType(_oJSon) == "O" 

		_nTPages := _oJSon:paging:pages

		If !_lJob
			_oProcess:SetRegua1(_nTPages)
		EndIf

		While _nTPages >= _nPage 

			LogExec('INTEGRANDDO PEDIDOS PAGINA ' + cValToChar(_nPage) + ' DE ' + cValToChar(_nTPages) + ' .')
			
			If !_lJob
				_oProcess:IncRegua1("Integrando pedidos pagina " + cValToChar(_nPage) + " de " + cValToChar(_nTPages) + " ." )
			EndIf 

			//---------------------------------------------+
			// Valida se existe pedidos a serem integrados |
			//---------------------------------------------+
			If ValType(_oJSon:List) == "A" .And. Len(_oJSon:List) > 0

				_oProcess:SetRegua2( Len(_oJSon:List) )
				For _nX := 1 To Len(_oJSon:List)

					If !_lJob
						_oProcess:IncRegua2("Validando pedido VTEX " + RTrim(_oJSon:List[_nX]:OrderId))
					EndIf	
					
					LogExec('VALIDANDO NOVOS PEDIDOS VTEX ')

					aAdd(aOrderId,_oJSon:List[_nX]:OrderId)
				Next _nX

			Else
				If !_lJob
					Aviso('e-Commerce','Nao existem novos pedidos a serem integrados',{"Ok"})	
				EndIf	
				LogExec('NAO EXISTEM NOVOS PEDIDOS A SEREM INTEGRADOS')
			EndIf

			_nPage++
			FreeObj(_oJSon)

			If !AEcoI11X(_nPage,@_cError,@_oJSon) .And. _nTPages >= _nPage
				If !_lJob
					Aviso('e-Commerce','Não foi possivel realizar a comunicação com o eCommerce.',{"Ok"})	
				EndIf	
				LogExec('ERRO DE COMUNICACAO COM O ECOMMERCE.')
				Exit
			EndIf 	
		EndDo 
	EndIf
Else
	If !_lJob
		Aviso('e-Commerce','Nao existem novos pedidos a serem integrados',{"Ok"})	
	EndIf	
	LogExec('NAO EXISTEM NOVOS PEDIDOS A SEREM INTEGRADOS')
EndIf

RestArea(_aArea)
Return Nil

/********************************************************************************************/
/*/{Protheus.doc} AEcoI11PvC
	@description Atualiza clientes e pedidos
	@author Bernard M. Margarido
	@since 30/01/2017
	@version undefined
	@type function
/*/
/********************************************************************************************/
Static Function AEcoI11PvC()
Local _aArea		:= GetArea()

Local _nX			:= 0

Local aRet			:= {.T.,"",""}
Local aEndRes		:= {}
Local aEndCob		:= {}
Local aEndEnt		:= {}

Local oRestPv   	:= Nil
Local _oVTEX 		:= VTEX():New()


If !_lJob
	_oProcess:SetRegua1( Len(aOrderId) )
EndIf	

For _nX := 1 To Len(aOrderId)
    	
	If !_lJob	
		_oProcess:IncRegua1('Processando OrderId ' + aOrderId[_nX] )
	EndIf	

	LogExec(' PROCESSANDO ORDERID ' + aOrderId[_nX] )

	//--------------------------------------------------+
	// Valida se obteve sucesso no retorno da consulta. |
	//--------------------------------------------------+
	_oVTEX:cID		:= aOrderId[_nX]
	_oVTEX:cMetodo	:= "GET"

	If !Empty(_cLojaID)
		_oVTEX:cAppKey		:= _cAppKey
		_oVTEX:cAppToken	:= _cAppToken
		_oVTEX:cUrl			:= _cUrl
	EndIf 

	If _oVTEX:CompleteOrders()
		
		//------------------------------------+
		// Realiza o Parse para a String Rest |
		//------------------------------------+
		FWJsonDeserialize(_oVTEX:cJSonRet,@oRestPv)
		
		If ValType(oRestPv) == "O"
			
			//---------------------------------+
			// Grava/Atualiza dados do Cliente |
			//---------------------------------+
			aRet 	:= EcGrvCli(oRestPv:ClientProfileData,oRestPv:ShippingData,@aEndRes,@aEndCob,@aEndEnt)
			
			//-----------------------+
			// Grava Pedido de Venda |
			//-----------------------+ 
			If aRet[1]
				aRet := EcGrvPed(oRestPv,aEndRes,aEndCob,aEndEnt,aOrderId[_nX])
			EndIf        

            If !aRet[1]
				aAdd(aMsgErro,{aRet[2],aRet[3]})            
            EndIf
            
			//-------------+
			// Mata Objeto |
			//-------------+
			If ValType(oRestPv) == "O"
				FreeObj(oRestPv)
			EndIf
													
		Else
			If !_lJob	
				Aviso('e-Commerce','Não existem novos pedidos a serem integrados',{"Ok"})	
			EndIf
			LogExec('NAO EXISTEM NOVOS PEDIDOS A SEREM INTEGRADOS')	
		EndIf
	Else
		If !_lJob	
			Aviso('e-Commerce','Erro na requisição de pedidos ' + RTrim(_oVTEX:cError),{"Ok"})	
		EndIf	
		LogExec('ERRO NA REQUISIÇÃO DE PEDIDOS ' + RTrim(_oVTEX:cError))	
	EndIf
		
Next _nX

RestArea(_aArea)
Return aRet[1]

/************************************************************************************/
/*/{Protheus.doc} EcGrvCli
	@description Realiza a gravação / atualização do cliente
	@author Bernard M. Margarido
	@since 30/01/2017
	@version undefined
	@type function
/*/
/************************************************************************************/
Static Function EcGrvCli(oDadosCli,oDadosEnd,aEndRes,aEndCob,aEndEnt)
Local aArea				:= GetArea()
Local aRet				:= {.T.,"",""}
Local _aErro			:= {}

Local cCnpj				:= ""
Local cCodCli			:= ""
Local cLoja				:= ""
Local cNomeCli			:= ""
Local cTpPess			:= ""   
Local cTipoCli			:= ""
Local cContrib			:= ""
Local cContato			:= "" 
Local cInscE			:= ""
Local cEnd				:= ""
Local cNumEnd			:= ""
Local cBairro			:= ""
Local cMun				:= ""
Local cCep				:= ""
Local cEst				:= ""
Local cCodMun			:= ""
Local cEndC				:= ""
Local cNumEndC			:= ""
Local cBairroC			:= ""
Local cMunC				:= ""
Local cCepC				:= ""
Local cEstC				:= ""
Local cEndE				:= ""
Local cNumEndE			:= ""
Local cBairroE			:= ""
Local cMunE				:= ""
Local cCepE				:= ""
Local cEstE				:= "" 
Local _cEndCob			:= ""
Local _cMunCob 			:= ""
Local _cBairroC 		:= ""
Local _cEndEnt			:= ""
Local _cMunEnt 			:= ""
Local _cBairroE 		:= ""
Local _cEMailEc			:= ""
Local _cEndFat			:= ""
Local _cMunFat 			:= ""
Local _cBairroF			:= ""
Local _cLinha			:= ""
Local _cCompEnd 		:= ""
Local _cCompEnt 		:= ""
Local _cProfVtex 		:= ""
Local _cCodVen 			:= GetMV("VT_VTEXVND")
Local _cCondPG  		:= SuperGetMV("VT_COND",,"003")
//Local _cCMunDef			:= GetNewPar("EC_CMUNDE","99999")

Local aCliente  		:= {} 

Local _nTLoja 			:= TamSx3("A1_LOJA")[1]
Local _nTEndC 			:= TamSx3("A1_ENDCOB")[1]
Local _nTMunC 			:= TamSx3("A1_MUNC")[1]
Local _nTBairC			:= TamSx3("A1_BAIRROC")[1] 
Local _nTEndE 			:= TamSx3("A1_ENDENT")[1]
Local _nTMunE 			:= TamSx3("A1_MUNE")[1]
Local _nTBairE			:= TamSx3("A1_BAIRROE")[1]
Local _nTEndF			:= TamSx3("A1_END")[1]
Local _nTMunF			:= TamSx3("A1_MUN")[1]
Local _nTBairF			:= TamSx3("A1_BAIRRO")[1]
Local _nTNome			:= TamSx3("A1_NOME")[1]
Local _nTReduz			:= TamSx3("A1_NREDUZ")[1]
Local _nX 				:= 0
Local nOpcA				:= 0

Local lEcCliCpo			:= ExistBlock("ECADDCPO")

Private lMsErroAuto 	:= .F.
Private lMsHelpAuto 	:= .T.
Private lAutoErrNoFile 	:= .T.
Private l030Auto        := .T.

//---------------------+
// Cnpj/Cpf do cliente |
//---------------------+    
If oDadosCli:IsCorporate   
	cCnpj 	:= PadR(oDadosCli:CorporateDocument,nTamCnpj)
	cTpPess := "J"
	cTipoCli:= "F"
Else
	cCnpj 	:= PadR(oDadosCli:Document,nTamCnpj) 
	cTpPess := "F"
	cTipoCli:= "F"	  
EndIf	

//----------------------------------------------------------+
// Valida se cliente ja existe na base de dados do Protheus |
//----------------------------------------------------------+
dbSelectArea("SA1")
SA1->( dbSetOrder(3) )
If SA1->( dbSeek(xFilial("SA1") + cCnpj ) )
	cCodCli := SA1->A1_COD
	cLoja	:= SA1->A1_LOJA
	nOpcA 	:= 4
	LogExec("INICIA ATUALIZACAO DO CLIENTE " + cCodCli + "-" + cLoja + "-" + Alltrim(SA1->A1_NREDUZ)  )
Else
	cCodCli := GetSxeNum("SA1","A1_COD")
	cLoja	:= StrZero(0,_nTLoja)
	SA1->( dbSetOrder(1) )
	While SA1->( dbSeek(xFilial("SA1") + PadR(cCodCli,nTCodCli) + cLoja ) )
		ConfirmSx8()
		cCodCli	:= GetSxeNum("SA1","A1_COD","",1)
	EndDo	
	nOpcA := 3
	LogExec("INICIA INCLUSAO DO CLIENTE " + cCodCli + "-" + cLoja  )
EndIf

//----------------------+
// Consulta Master Data |
//----------------------+
_cProfVtex := oDadosCli:userProfileId
If !Empty(_cProfVtex)
	aEcoI011MdV(_cProfVtex,cCnpj,@_cEMailEc)
EndIf

//--------------------------+
// Dados passados pela Vtex |
//--------------------------+ 
If oDadosCli:IsCorporate
	cNomeCli	:= IIF(nOpcA == 3,	Alltrim(u_ECACENTO(DecodeUtf8(oDadosCli:CorporateName),.T.))											, SA1->A1_NOME 		) 
	cNReduz		:= IIF(nOpcA == 3,	Alltrim(u_ECACENTO(DecodeUtf8(oDadosCli:TradeName),.T.))												, SA1->A1_NREDUZ	)
	cContato	:= IIF(nOpcA == 3,	u_ECACENTO(DecodeUtf8(oDadosCli:FirstName),.T.) 														, SA1->A1_CONTATO	)	
	cDdd01		:= IIF(nOpcA == 3,	SubStr(oDadosCli:CorporatePhone,5,2)																	, SA1->A1_DDD		)
	cTel01		:= IIF(nOpcA == 3,	StrTran(SubStr(oDadosCli:CorporatePhone,8,nTamTel)," ","")												, SA1->A1_TEL		)
	cInscE		:= IIF(nOpcA == 3,	IIF(ValType(oDadosCli:StateInscription) <> "U",Upper(oDadosCli:StateInscription),"ISENTO")				, SA1->A1_INSCR		)
	cContrib	:= IIF(nOpcA == 3,	IIF(Alltrim(cInscE) == "ISENTO","2","1")																, SA1->A1_CONTRIB	)
	cNomeCli	:= PadR(cNomeCli,_nTNome)
Else	
	cNomeCli	:= IIF(nOpcA == 3,	Alltrim(u_ECACENTO(DecodeUtf8(oDadosCli:FirstName),.T.)) + " " + Alltrim(u_ECACENTO(DecodeUtf8(oDadosCli:LastName),.T.))	, SA1->A1_NOME 		)
	cNReduz		:= IIF(nOpcA == 3,	Alltrim(u_ECACENTO(DecodeUtf8(oDadosCli:FirstName),.T.)) + " " + Alltrim(u_ECACENTO(DecodeUtf8(oDadosCli:LastName),.T.))	, SA1->A1_NREDUZ	)
	cDdd01		:= IIF(nOpcA == 3,	SubStr(oDadosCli:Phone,4,2)										, SA1->A1_DDD		)
	cTel01		:= IIF(nOpcA == 3,	SubStr(oDadosCli:Phone,6,nTamTel) 								, SA1->A1_TEL		)
	cContrib	:= IIF(nOpcA == 3,	"2"																, SA1->A1_CONTRIB	)
	cNomeCli	:= PadR(cNomeCli,_nTNome)
	cNReduz		:= PadR(cNReduz,_nTReduz)
EndIf

cEmail			:= IIF(nOpcA == 3,	Alltrim(oDadosCli:eMail)										, SA1->A1_EMAIL		)

//----------------+
// Dados Endereço |
//----------------+
aEndRes	:= {}
aEndCob	:= {}
aEndEnt	:= {}
EcRetEnd(oDadosEnd:Address,@aEndRes,@aEndCob,@aEndEnt)

//-----------+
// Enderecos |
//-----------+
If Len(aEndRes) > 0 
	cEnd		:= aEndRes[ENDERE]
	cNumEnd		:= aEndRes[NUMERO]
	cBairro		:= aEndRes[BAIRRO]
	cMun		:= aEndRes[MUNICI]	
	cCep		:= aEndRes[CEP]
	cEst		:= aEndRes[ESTADO]
	cCodMun		:= aEndRes[IBGE]
	_cCompEnd	:= aEndRes[COMPLE]
ElseIf Len(aEndEnt) > 0		
	cEnd		:= aEndEnt[ENDERE]
	cNumEnd		:= aEndEnt[NUMERO]
	cBairro		:= aEndEnt[BAIRRO]
	cMun		:= aEndEnt[MUNICI]	
	cCep		:= aEndEnt[CEP]
	cEst		:= aEndEnt[ESTADO]
	cCodMun		:= aEndEnt[IBGE]
	_cCompEnd	:= aEndEnt[COMPLE]
EndIf

//----------------------+
// Endereco de Cobranca | 
//----------------------+
If Len(aEndCob) > 0	
	cEndC		:= aEndCob[ENDERE]
	cNumEndC	:= aEndCob[NUMERO]
	cBairroC	:= aEndCob[BAIRRO]
	cMunC		:= aEndCob[MUNICI]
	cCepC		:= aEndCob[CEP]
	cEstC		:= aEndCob[ESTADO]
ElseIf Len(aEndRes) > 0
	cEndC		:= aEndRes[ENDERE]
	cNumEndC	:= aEndRes[NUMERO]
	cBairroC	:= aEndRes[BAIRRO]
	cMunC		:= aEndRes[MUNICI]
	cCepC		:= aEndRes[CEP]
	cEstC		:= aEndRes[ESTADO]	
EndIf

//---------------------+
// Endereco de Entrega |
//---------------------+
If Len(aEndEnt) > 0		
	cEndE		:= aEndEnt[ENDERE]
	cNumEndE	:= aEndEnt[NUMERO]
	cBairroE	:= aEndEnt[BAIRRO]
	cMunE		:= aEndEnt[MUNICI]
	cCepE		:= aEndEnt[CEP]
	cEstE		:= aEndEnt[ESTADO]
	_cCompEnt	:= aEndEnt[COMPLE]
	_cCodMunE	:= aEndEnt[IBGE]
ElseIf Len(aEndRes) > 0
	cEndE		:= aEndRes[ENDERE]
	cNumEndE	:= aEndRes[NUMERO]
	cBairroE	:= aEndRes[BAIRRO]
	cMunE		:= aEndRes[MUNICI]
	cCepE		:= aEndRes[CEP]
	cEstE		:= aEndRes[ESTADO]
	_cCompEnt	:= aEndRes[COMPLE]
	_cCodMunE	:= aEndRes[IBGE]
EndIf

//--------------------------------------+
// Cria Array para cadastro de clientes |
//--------------------------------------+
aAdd(aCliente ,	{"A1_FILIAL"	,	xFilial("SA1")							,	Nil	})
aAdd(aCliente ,	{"A1_COD"		,	cCodCli									,	Nil	})
aAdd(aCliente ,	{"A1_LOJA"		,	cLoja									,	Nil	})
aAdd(aCliente ,	{"A1_PESSOA"	,	cTpPess									,	Nil	})
aAdd(aCliente ,	{"A1_NOME"		,	cNomeCli								,	Nil	})
aAdd(aCliente ,	{"A1_NREDUZ"	,	cNReduz									,	Nil	})

_cEndFat	:= PadR(cEnd + ", " + cNumEnd,_nTEndF)
_cMunFat 	:= PadR(cMun,_nTMunF)
_cBairroF 	:= PadR(cBairro,_nTBairF)

If nOpcA == 3
	aAdd(aCliente ,	{"A1_END"		,	_cEndFat								,	Nil	})
	aAdd(aCliente ,	{"A1_EST"		,	cEst									,	Nil	})
	aAdd(aCliente ,	{"A1_COD_MUN"	,	cCodMun									,	Nil	})
	aAdd(aCliente ,	{"A1_MUN"		,	_cMunFat								,	Nil	})
	aAdd(aCliente ,	{"A1_BAIRRO"	,	_cBairroF								,	Nil	})
	aAdd(aCliente ,	{"A1_CEP"		,	cCep									,	Nil	})
EndIf	

_cEndCob	:= PadR(cEndC + ", " + cNumEndC,_nTEndC)
_cMunCob 	:= PadR(cMunC,_nTMunC)
_cBairroC 	:= PadR(cBairroC,_nTBairC)

_cEndEnt	:= PadR(cEndE + ", " + cNumEndE,_nTEndE)
_cMunEnt 	:= PadR(cMunE,_nTMunE)
_cBairroE 	:= PadR(cBairroE,_nTBairE)

aAdd(aCliente ,	{"A1_ENDCOB"	,	_cEndCob								,	Nil	})
aAdd(aCliente ,	{"A1_ESTC"		,	cEstC									,	Nil	})
aAdd(aCliente ,	{"A1_MUNC"		,	_cMunCob								,	Nil	})
aAdd(aCliente ,	{"A1_BAIRROC"	,	_cBairroC								,	Nil	})
aAdd(aCliente ,	{"A1_CEPC"		,	cCepC									,	Nil	})  
aAdd(aCliente ,	{"A1_ENDENT"	,	_cEndEnt								,	Nil	})
aAdd(aCliente ,	{"A1_ESTE"		,	cEstE									,	Nil	})
aAdd(aCliente ,	{"A1_MUNE"		,	_cMunEnt								,	Nil	})
aAdd(aCliente ,	{"A1_BAIRROE"	,	_cBairroE								,	Nil	})
aAdd(aCliente ,	{"A1_CEPE"		,	cCepE									,	Nil	})
aAdd(aCliente ,	{"A1_COMPENT"	,	_cCompEnt								,	Nil	})
aAdd(aCliente ,	{"A1_XCDMUNE"	,	_cCodMunE								,	Nil	})
aAdd(aCliente ,	{"A1_XENDCOM"	,	_cEndFat								,	Nil	})
aAdd(aCliente ,	{"A1_XBAIRRC"	,	_cBairroF								,	Nil	})
aAdd(aCliente ,	{"A1_XMUNCOM"	,	_cMunFat								,	Nil	})
aAdd(aCliente ,	{"A1_XESTCOM"	,	cEst									,	Nil	})
aAdd(aCliente ,	{"A1_XCEPCOM"	,	cCep									,	Nil	})

aAdd(aCliente ,	{"A1_VEND"		,	_cCodVen								,	Nil	})
aAdd(aCliente ,	{"A1_COND"		,	_cCondPG								,	Nil	})
aAdd(aCliente ,	{"A1_TIPO"		,	cTipoCli								,	Nil	})
aAdd(aCliente ,	{"A1_DDD"		,	cDdd01									,	Nil	})
aAdd(aCliente ,	{"A1_TEL"		,	cTel01									,	Nil	})
aAdd(aCliente ,	{"A1_PAIS"		,	"105"									,	Nil	})
aAdd(aCliente ,	{"A1_CGC"		,	cCnpj									,	Nil	})
aAdd(aCliente ,	{"A1_EMAIL"		,	_cEMailEc								,	Nil	})
aAdd(aCliente ,	{"A1_DTNASC"	,	dDataBase								,	Nil	})
aAdd(aCliente ,	{"A1_CONTRIB"	,	cContrib								,	Nil	})  
aAdd(aCliente ,	{"A1_CONTATO"	,	cContato								,	Nil	})  

//---------------------------+
// Valida Inscricao estadual |
//---------------------------+
If oDadosCli:IsCorporate
  	If !IE(cInscE,aEndRes[ESTADO],.F.)
		//---------------------+
		// Variavel de retorno |
		//---------------------+
	 	aRet[1] := .F.
	   	aRet[2] := cCnpj
		aRet[3] := "INSCRICAO ESTADUAL " + cInscE + " INVÁLIDA PARA O ESTADO DE " + aEndRes[ESTADO]
		RestArea(aArea)
		Return aRet
	EndIf
EndIf	
	
//-----------------------------------------------+
// Grava Incsrição estadual para pessoa Juridica |
//-----------------------------------------------+
If oDadosCli:IsCorporate
	aAdd(aCliente ,	{"A1_INSCR"	,	Alltrim(cInscE)										,	"AllWaysTrue()"	})
Else
	aAdd(aCliente ,	{"A1_INSCR"	,	Alltrim("ISENTO")									,	"AllWaysTrue()"	})
EndIf

//----------------------------------------------+
// Caso pedido de venda seja outros             |
// o cliente será criado com risco de credito E |
//----------------------------------------------+
aAdd(aCliente ,	{"A1_RISCO"		, "A"													,	Nil	})
aAdd(aCliente ,	{"A1_CODPAIS"	, "01058"												,	Nil	})
aAdd(aCliente ,	{"A1_MAILFIN"	, _cEMailEc												,	Nil	})
aAdd(aCliente ,	{"A1_XEMAILV"	, _cEMailEc												,	Nil	})
aAdd(aCliente ,	{"A1_XMDPROF"	, _cProfVtex											,	Nil	})

//----------------------------------------------------------+
// Ponto de Entrada utilizado para acrescentar novos campos |
//----------------------------------------------------------+
If lEcCliCpo 
	aCliente := ExecBlock("ECADDCPO",.F.,.F.,{oDadosCli,oDadosEnd,aCliente,nOpcA})
EndIf

//--------------------------+
// Ordena pela ordem do SX3 |
//--------------------------+
aCliente := FWVetByDic(aCliente, "SA1")

If Len(aCliente) > 0 
 
	lMsErroAuto := .F.
		
	//--------------------------------------------+
	// Realiza a gravação/atualização de clientes |
	//--------------------------------------------+
	dbSelectArea("SA1")
	SA1->( dbGoTop() )
	
	If MA030IsMVC()
		SetFunName('CRMA980')
		MSExecAuto( { |x, y| CRMA980(x,y) },  aCliente, nOpcA )
	Else
		SetFunName('MATA030')
		MsExecAuto({|x,y| Mata030(x,y)}, aCliente, nOpcA)
	EndIf 
	
	LogExec("PROCESSANDO MANUTENCAO DO CLIENTE " + cCodCli + "-" + cLoja  )
	
	//---------------------+
	// Erro na Atualização |
	//---------------------+
	If lMsErroAuto
	
		RollBackSx8()

		_cLinha	 := ""	
		cMsgErro := ""

		_aErro	 := {}
		_aErro 	 := GetAutoGrLog()

		 For _nX := 1 To Len(_aErro)
            _cLinha := _aErro[_nX]
			_cLinha  := StrTran( _cLinha, Chr(13), " " )
			_cLinha  := StrTran( _cLinha, Chr(10), " " )

			If SubStr( _cLinha, 1, 4 ) == 'HELP'
				cMsgErro += _cLinha + "|"
			EndIf

			If SubStr( _cLinha, 1, 6 ) == 'TABELA'
				cMsgErro += _cLinha + "|"
			EndIf

			If SubStr( _cLinha, 1, 5 ) == 'AJUDA'
				cMsgErro += _cLinha + " | "
			EndIf

			If At("< -- Invalido", _aErro[_nX] ) > 0
				cMsgErro += _aErro[_nX]  + " | "
			EndIf

		Next nX

		/*
		MakeDir("\erros\")
		cSA1Log := "SA1" + cCodCli + cLoja + DToS(dDataBase) + Left(Time(),2) + SubStr(Time(),4,2) + Right(Time(),2) + ".LOG"
		MostraErro("\erros\",cSA1Log)
				
		LogExec("ERRO AO " + Iif(nOpcA == 3,"INCLUIR","ALTERAR") + " O CLIENTE " + cCodCli + "-" + cLoja  )					
		LogExec("FAVOR VERIFICAR O ARQUIVO DE LOG " + cSA1Log + " NA PASTA \erros\" )					
		
		//------------------------------------------------+
		// Adiciona Arquivo de log no Retorno da resposta |
		//------------------------------------------------+
		cMsgErro := ""
		cLiArq	 := ""
		nHndImp	 := 0	
		nHndImp  := FT_FUSE("\erros\" + cSA1Log)
		If nHndImp >= 1
			//-----------------------------+
			// Posiciona Inicio do Arquivo |
			//-----------------------------+
			FT_FGOTOP()
			
			While !FT_FEOF()
				cLiArq := FT_FREADLN()
				If Empty(cLiArq)
					FT_FSKIP(1)
					Loop
				EndIf
				cMsgErro += cLiArq + CRLF
				FT_FSKIP(1)
			EndDo
			FT_FUSE()
		EndIf  
		*/

		//---------------------+
		// Variavel de retorno |
		//---------------------+
		aRet[1] := .F.
		aRet[2] := cCnpj 
		aRet[3] := cMsgErro
																						
	Else
		
		//-----------------------------+
		// Reseta variavel da ExecAuto |
		//-----------------------------+
		ConfirmSx8()

		//----------------------------------------+
		// Grava endereço de entrega nos contatos |
		//----------------------------------------+
		If Len(aEndEnt) > 0 .Or. Len(aEndRes) > 0
			AEc011Cont(cCodCli,cLoja,cNomeCli,IIF(Len(aEndEnt) > 0,aEndEnt,aEndRes))
		Endif
		
		//------------------------------------+
		// Envia e-Mail com erro de municipio |
		//------------------------------------+	
		/*
		If Rtrim(cCodMun) == RTrim(_cCMunDef)
			u_AEcMailC(cCodInt,cDescInt,cCnpj,cNomeCli)
		EndIf
		*/

		//---------------------+
		// Variavel de retorno |
		//---------------------+
		aRet[1] := .T.
		aRet[2] := ""
		aRet[3] := ""

		//------------+
		// Msg CoNout |
		//------------+
		LogExec("CLIENTE " + Iif(nOpcA == 3,"INCLUIDO","ALTERADO") + " COM SUCESSO " + cCodCli + "-" + cLoja  )					
		
	EndIf
	
	//-----------------------------------+
	// Retira Lock de todos os registros |
	//-----------------------------------+
	MsUnLockAll()

EndIf

LogExec("FIM MANUTENCAO DE CLIENTES" )

Restarea(aArea)
Return aRet

/***************************************************************************************/
/*/{Protheus.doc} EcRetEnd
	@description Valida os endereços cadastrados pelo o cliente
	@author Bernard M. Margarido
	@since 30/01/2017
	@version undefined
	@type function
/*/
/***************************************************************************************/
Static Function EcRetEnd(oDadosEnd,aEndRes,aEndCob,aEndEnt)

Local nEnd		:= 0

//-------------------+
// Tipos de Endereco |
// 1 - Residencial   |
// 2 - Entrega       |
// 3 - Cobranca      |
//-------------------+
	
//------------------------+
// Valida tipo  de Objeto |
//------------------------+
If ValType(oDadosEnd) == "O" 
	If SubStr(Upper(oDadosEnd:AddressType),1,3) == "RES"
		aEndRes := EcLoadEnd(oDadosEnd)
	ElseIf SubStr(Upper(oDadosEnd:AddressType),1,3) == "COB"
		aEndEnt := EcLoadEnd(oDadosEnd)
	ElseIf SubStr(Upper(oDadosEnd:AddressType),1,3) == "COM"
		aEndCob := EcLoadEnd(oDadosEnd)
	ElseIf SubStr(Upper(oDadosEnd:AddressType),1,3) == "IND"
		aEndRes := EcLoadEnd(oDadosEnd)
	EndIf	
ElseIf ValType(oDadosEnd) == "A"
	For nEnd := 1 To Len(oDadosEnd)
		If SubStr(Upper(oDadosEnd[nEnd]:AddressType),1,3) == "RES"
			aEndRes := EcLoadEnd(oDadosEnd[nEnd])
		ElseIf SubStr(Upper(oDadosEnd[nEnd]:AddressType),1,3) == "COB"
			aEndEnt := EcLoadEnd(oDadosEnd[nEnd])
		ElseIf SubStr(Upper(oDadosEnd[nEnd]:AddressType),1,3) == "COM"
			aEndCob := EcLoadEnd(oDadosEnd[nEnd])
		ElseIf SubStr(Upper(oDadosEnd[nEnd]:AddressType),1,3) == "IND"
			aEndRes := EcLoadEnd(oDadosEnd[nEnd])
		EndIf
	Next nEnd
EndIf

Return .T.

/****************************************************************************/
/*/{Protheus.doc} EcLoadEnd
	@description Carrega os enderecos cadastrados
	@author Bernard M. Margarido
	@since 30/01/2017
	@version undefined
	@type function
/*/
/****************************************************************************/
Static Function EcLoadEnd(oEndereco)
Local aRet			:= {}
Local cMunicipio	:= ""
Local cEstado		:= ""
Local cComplem		:= ""
Local cPais			:= ""
Local cBairro		:= ""
Local cNumero		:= ""
Local cCep			:= ""
Local cDesti		:= ""
Local cReferen		:= ""
Local cEnd			:= ""
Local cIdEnd		:= ""

//------------------------------------+
// Acerta endereço no padrao protheus |
//------------------------------------+
cMunicipio	:= IIF(ValType(oEndereco:City) <> "U", AllTrim(u_ECACENTO(DecodeUtf8(oEndereco:City),.T.)), "")
If ValType(oEndereco:State) <> "U" .And. Len(Alltrim(oEndereco:State)) > 2
	cEstado 	:= AEcoI11UF(oEndereco:State)
Else 
	cEstado		:= IIF(ValType(oEndereco:State) <> "U", Upper(oEndereco:State), "")
EndIf 
cComplem	:= IIF(ValType(oEndereco:Complement) <> "U", AllTrim(u_ECACENTO(DecodeUtf8(oEndereco:Complement),.T.)), "")
cPais		:= IIF(ValType(oEndereco:Country) <> "U", oEndereco:Country, "")
cBairro		:= IIF(ValType(oEndereco:NeighBorhood) <> "U", AllTrim(u_ECACENTO(DecodeUtf8(oEndereco:NeighBorhood),.T.)), "")
cNumero		:= IIF(ValType(oEndereco:Number) <> "U", oEndereco:Number, "")
cCep		:= IIF(ValType(oEndereco:PostalCode) <> "U", u_ECFORMAT(oEndereco:PostalCode,"A1_CEP",.T.), "")
cDesti		:= IIF(ValType(oEndereco:ReceiverName) <> "U", AllTrim(u_ECACENTO(DecodeUtf8(oEndereco:ReceiverName),.T.)), "")
cReferen	:= IIF(ValType(oEndereco:Reference) <> "U", AllTrim(u_ECACENTO(DecodeUtf8(oEndereco:Reference),.T.)), "")
cEnd		:= IIF(ValType(oEndereco:Street) <> "U", AllTrim(u_ECACENTO(DecodeUtf8(oEndereco:Street),.T.)), "")
cIdEnd		:= IIF(ValType(oEndereco:addressId) <> "U", oEndereco:addressId, "")
cContato	:= IIF(ValType(oEndereco:receiverName) <> "U", AllTrim(u_ECACENTO(DecodeUtf8(oEndereco:receiverName),.T.)), "")

aRet 		:= Array(15) 

cIbge		:= EcCodMun(cEstado,cMunicipio) 
		
aRet[DESTIN]	:= cDesti
aRet[ENDERE]	:= cEnd 
aRet[NUMERO]	:= cNumero
aRet[IBGE]		:= cIbge
aRet[ESTADO]	:= cEstado
aRet[MUNICI]	:= cMunicipio
aRet[BAIRRO]	:= IIF(Empty(cBairro), "S/BAIRRO", cBairro)
aRet[CEP]		:= cCep
aRet[TELEF1]	:= ""
aRet[TELEF2]	:= ""
aRet[CELULA]	:= ""
aRet[REFERE]	:= cReferen
aRet[COMPLE]	:= cComplem
aRet[IDENDE]	:= cIdEnd
aRet[CONTAT]	:= cContato

Return aRet

/***********************************************************************************/
/*/{Protheus.doc} AEcoI11UF
	@description Retorna sigla do estoque de acordo com o nome
	@type  Static Function
	@author Bernard M Margarido
	@since 11/10/2022
	@version version
/*/
/***********************************************************************************/
Static Function AEcoI11UF(_cEstado)
Local _cNome := FWNoAccent(Upper(DecodeUTF8(_cEstado)))
Local _cQuery:= ""
Local _cAlias:= ""
Local _cUF 	 := ""

_cQuery := " SELECT " + CRLF
_cQuery += "	X5_CHAVE " + CRLF
_cQuery += " FROM " + CRLF
_cQuery += "	" + RetSqlName("SX5") + " " + CRLF
_cQuery += " WHERE " + CRLF
_cQuery += "	X5_TABELA = '12' AND " + CRLF
_cQuery += "	X5_DESCRI LIKE '%" + _cNome + "%' AND " + CRLF
_cQuery += "	D_E_L_E_T_ = '' "

_cAlias := MPSysOpenQuery(_cQuery)

_cUF 	:= PadR((_cAlias)->X5_CHAVE,_nTEst)

Return _cUF 

/***********************************************************************************/
/*/{Protheus.doc} EcCodMun
	@description Retorna codigo do municipio
	@author Bernard M. Margarido
	@since 30/01/2017
	@version undefined
	@type function
/*/
/***********************************************************************************/
Static Function EcCodMun(cEstado,cMunicipio)
Local aArea		:= GetARea()

Local cAlias	:= GetNextAlias()
Local cQuery	:= ""
Local cIbge		:= ""
//Local _cMunDef	:= GetNewPar("EC_MUNDEF","INVALIDO")
Local _cCMunDef	:= GetNewPar("EC_CMUNDE","99999")

Local _lAtMunDef:= GetNewPar("EC_ATMUNDE",.T.)

If At("(",cMunicipio) > 0
	cMunicipio := SubStr(cMunicipio,1,At("(",cMunicipio) -1)
EndIf

If At("'",cMunicipio) > 0
	cMunicipio := StrTran(cMunicipio,"'","''")
EndIf

//-----------------------------+
// Cosulta codigo de municipio |
//-----------------------------+
cQuery := "	SELECT " + CRLF 
cQuery += "		CC2_CODMUN " + CRLF 
cQuery += "	FROM " + CRLF 
cQuery += "		" + RetSqlName("CC2") + CRLF   
cQuery += "	WHERE " + CRLF 
cQuery += "		CC2_FILIAL = '" + xFilial("CC2") + "' AND " + CRLF 
cQuery += "		CC2_EST = '" + cEstado + "' AND " + CRLF 
cQuery += "		CC2_MUN = '" + cMunicipio + "' AND " + CRLF 
cQuery += "		D_E_L_E_T_ <> '*' " 

dbUseArea(.T.,"TOPCONN",TcGenQry(,,cQuery),cAlias,.T.,.T.) 

If (cAlias)->( Eof() )
	If _lAtMunDef
		dbSelectArea("CC2")
		CC2->( dbSetOrder(1) )
		If CC2->( dbSeek(xFilial("CC2") + cEstado + _cCMunDef) )
			cIbge	:= CC2->CC2_CODMUN
		Else
			//-------------------------+
			// Grava Municipio Default |
			//-------------------------+
			RecLock("CC2",.T.)
				CC2->CC2_FILIAL := xFilial("CC2")
				CC2->CC2_EST   	:= cEstado
				CC2->CC2_CODMUN	:= _cCMunDef
				CC2->CC2_MUN   	:= cMunicipio
				CC2->CC2_MDEDMA	:= CriaVar("CC2_MDEDMA",.F.)
				CC2->CC2_MDEDSR	:= CriaVar("CC2_MDEDSR",.F.)
				CC2->CC2_PERMAT	:= CriaVar("CC2_PERMAT",.F.)
				CC2->CC2_PERSER	:= CriaVar("CC2_PERSER",.F.)
				CC2->CC2_DTRECO	:= CriaVar("CC2_DTRECO",.F.)
				CC2->CC2_CDSIAF	:= CriaVar("CC2_CDSIAF",.F.)
				CC2->CC2_CPOM  	:= CriaVar("CC2_CPOM  ",.F.)
				CC2->CC2_TPDIA 	:= CriaVar("CC2_TPDIA ",.F.)
				CC2->CC2_CODANP	:= CriaVar("CC2_CODANP",.F.)
			CC2->( MsUnLock() )
			cIbge	:= _cCMunDef
		EndIf
		
	EndIf
Else
	cIbge := (cAlias)->CC2_CODMUN
EndIf


(cAlias)->( dbCloseArea() )	

RestArea(aArea)
Return cIbge 

/**************************************************************************************************/
/*/{Protheus.doc} aEcoI011MdV
	@description Consulta email do cliente no master data 
	@type  Static Function
	@author Bernard M. Margarido
	@since 16/08/2020
/*/
/**************************************************************************************************/
Static Function aEcoI011MdV(_cProfileID,cCnpj,_cEMailEc)
Local _oVTex	:= MasterData():New()
Local _oJSon	:= JSonObject():New() 

_oVTex:cIDCliente 	:= _cProfileID
_cEMailEc			:= ""
If _oVTex:Clientes()
	If ValType(_oVTex:cJson) <> "U"
		_oJSon:FromJson(_oVTex:cJson)
		_cEMailEc := _oJSon["email"]
	EndIf 
Else 
	_cEMailEc := ""
EndIf

FreeObj(_oVTex)
FreeObj(_oJSon)

Return Nil

/**************************************************************************************************/
/*/{Protheus.doc} EcGrvPed
	@description	Rotina valida se pedido ecommerce ja existe na base de dados do protheus
	@autho			Bernard M.Margarido
	@version   		1.00
	@since     		10/02/2016
/*/			
/**************************************************************************************************/
Static Function EcGrvPed(oRestPv,aEndRes,aEndCob,aEndEnt,cOrderId,_cLojaID)
	Local aArea		:= GetArea()
	Local aRet		:= {.T.,"",""}

	Local cNumOrc	:= ""
	Local cNumPv	:= ""

	LogExec("VERIFICANDO ORDERID " + cOrderId)

	//--------------------------------------+
	// Valida se pedido ja esta no Protheus |
	//--------------------------------------+
	dbSelectArea("XTA")
	XTA->( dbSetOrder(2) )
	If XTA->( dbSeek(xFilial("XTA") + PadR(cOrderId,nTamOrder)) )
		//------------------------------------+
		// Atualiza Status do Pedido eCommerce|
		//------------------------------------+
		cNumOrc		:= XTA->XTA_NUM
		cOrdPvCli	:= XTA->XTA_NUMECL
		cNumDoc		:= XTA->XTA_DOC
		cNumSer		:= XTA->XTA_SERIE
		cNumPv		:= XTA->XTA_PEDRES
		
		aRet 		:= AEcoUpdPv(cOrderId,cOrdPvCli,cNumOrc,cNumDoc,cNumSer,cNumPv,oRestPv)

	Else
		//---------------------------------+
		// Realiza a gravação do pedido    |
		// dentro do controle de transação |
		//---------------------------------+
		Begin Transaction 
			aRet := AEcoGrvPv(cOrderId,oRestPv,aEndRes,aEndCob,aEndEnt,_cLojaID)
			If !aRet[1]
				DisarmTransaction()
			Endif
		End Transaction

	EndIf

	RestArea(aArea)
Return aRet

/**************************************************************************************************/
/*/{Protheus.doc} AEcoGrvPv
	@description	Realiza a gravação do pedido de venda e-commerce
	@author			Bernard M.Margarido
	@version   		1.00
	@since     		10/02/2016
/*/			
/**************************************************************************************************/
Static Function AEcoGrvPv(cOrderId,oRestPv,aEndRes,aEndCob,aEndEnt,_cLojaID)
	Local aArea			:= GetArea()
	Local aRet			:= {.T.,"",""}

	Local cVendedor 	:= GetNewPar("EC_VENDECO")
	Local cCnpj			:= ""
	Local cNomeCli		:= ""
	Local cTipoCli		:= ""
	Local cNumOrc		:= ""
	Local cCodCli		:= ""
	Local cLojaCli		:= ""
	Local cEndDest		:= ""
	Local cNumDest 		:= ""
	Local cMunDest 		:= ""
	Local cBaiDest 		:= "" 
	Local cCepDest 		:= ""
	Local cEstDest 		:= ""
	Local cNomDest 		:= ""
	Local cDddCel  		:= ""
	Local cDdd1	 		:= "" 
	Local cTel01 		:= ""
	Local cCelular 		:= ""
	Local cPedCodCli	:= ""
	Local cPedCodInt	:= ""
	Local cCodAfili		:= ""
	Local cIdPost		:= ""
	Local cEndComp		:= "" 
	Local cEndRef		:= ""
	Local cCodTransp	:= ""
	Local _cIdServ		:= ""

	Local nDesconto		:= 0
	Local nQtdParc		:= 0
	Local nVlrFrete		:= 0
	Local nVrSubTot		:= 0
	Local nVlrTotal		:= 0
	Local nQtdItem		:= 0
	Local nVlrTotMkt	:= 0
	Local nJuros 		:= 0

	Local lUsaVend		:= GetNewPar("EC_USAVEND",.F.)
		
	Local dDtaEmiss		:= Nil
	
	Private nPesoBruto	:= 0

	//------------------+
	// Ajusta variaveis |
	//------------------+
	If oRestPv:ClientProfileData:IsCorporate
		cCnpj := PadR(oRestPv:ClientProfileData:CorporateDocument,nTamCnpj)
	Else     
		cCnpj := PadR(oRestPv:ClientProfileData:Document,nTamCnpj)
	EndIf	
	
	cNomeCli:= RTrim(oRestPv:ClientProfileData:FirstName + " " +  oRestPv:ClientProfileData:LastName )	

	//-----------------------------------------------+
	// Valida se cliente esta cadastrado no Protheus |
	//-----------------------------------------------+
	dbSelectArea("SA1")
	SA1->( dbSetOrder(3) )
	If !SA1->( dbSeek(xFilial("SA1") + cCnpj) )
		LogExec("CLIENTE " + cNomeCli + " CNPJ/CPF " + cCnpj + " NAO ENCONTRADO. FAVOR REALIZAR A BAIXA DE CLIENTES DO ECOMMERCE.")
		aRet[1] := .F.
		aRet[2] := cCnpj
		aRet[3] := "CLIENTE " + cNomeCli + " CNPJ/CPF " + cCnpj + " NAO ENCONTRADO. FAVOR REALIZAR A BAIXA DE CLIENTES DO ECOMMERCE."
		RestArea(aArea)
		Return aRet
	EndIf

	//---------------+
	// Dados Cliente |
	//---------------+
	cCodCli := SA1->A1_COD
	cLojaCli:= SA1->A1_LOJA
	cTipoCli:= SA1->A1_TIPO

	//-----------------+
	// Valida vendedor |
	//-----------------+
	If lUsaVend
		dbSelectArea("SA3")
		SA3->( dbSetOrder(1) )
		If !SA3->( dbSeek(xFilial("SA3") + cVendedor) )
			LogExec("VENDEDOR " + cVendedor + " PARA O ECOMMERCE NAO CADASTRADO. FAVOR CADASTRAR O VENDEDOR E INFORMAR O CODIGO DO VENDEDOR NO PARAMETRO EC_VENDECO.")
			aRet[1] := .F.
			aRet[2] := cVendedor
			aRet[3] := "VENDEDOR " + cVendedor + " PARA O ECOMMERCE NAO CADASTRADO. FAVOR CADASTRAR O VENDEDOR E INFORMAR O CODIGO DO VENDEDOR NO PARAMETRO EC_VENDECO."
			RestArea(aArea)
			Return aRet
		EndIf
	EndIf
		
	//--------------+
	// Dados Pedido |
	//--------------+
	cIdEnd		:= IIF(Len(aEndEnt) > 0 ,aEndEnt[IDENDE],aEndRes[IDENDE])
	cEndDest 	:= IIF(Len(aEndEnt) > 0 ,aEndEnt[ENDERE],aEndRes[ENDERE])
	cNumDest 	:= IIF(Len(aEndEnt) > 0 ,aEndEnt[NUMERO],aEndRes[NUMERO])
	cMunDest 	:= IIF(Len(aEndEnt) > 0 ,aEndEnt[MUNICI],aEndRes[MUNICI])
	cBaiDest 	:= IIF(Len(aEndEnt) > 0 ,aEndEnt[BAIRRO],aEndRes[BAIRRO]) 
	cCepDest 	:= IIF(Len(aEndEnt) > 0 ,aEndEnt[CEP],aEndRes[CEP])
	cEstDest 	:= IIF(Len(aEndEnt) > 0 ,aEndEnt[ESTADO],aEndRes[ESTADO])
	cNomDest 	:= IIF(Len(aEndEnt) > 0 ,aEndEnt[DESTIN],aEndRes[DESTIN])
	cEndComp	:= IIF(Len(aEndEnt) > 0 ,aEndEnt[COMPLE],aEndRes[COMPLE])
	cEndRef		:= IIF(Len(aEndEnt) > 0 ,aEndEnt[REFERE],aEndRes[REFERE])
	cDddCel  	:= ""
	cDdd1	 	:= IIF(Empty(oRestPv:ClientProfileData:Phone),"",SubStr(oRestPv:ClientProfileData:Phone,4,2)) 
	cTel01 	 	:= IIF(Empty(oRestPv:ClientProfileData:Phone),"",SubStr(oRestPv:ClientProfileData:Phone,6))
	cCelular 	:= ""
	cMotCancel	:= ""
	cObsPedido	:= ""
	cPedCodCli	:= oRestPv:Sequence
	cPedCodInt	:= oRestPv:OrderGroup
	cHoraEmis	:= SubStr(oRestPv:creationDate,At("T",oRestPv:creationDate) + 1,8)
	cPedStatus	:= Lower(oRestPv:Status)
	cCodAfili	:= oRestPv:Affiliateid
	dDtaEmiss	:= dToc(sTod(StrTran(SubStr(oRestPv:creationDate,1,10),"-","")))
	_cDescLoja  := IIF(Empty(_cLojaID) ,"", Posicione("XTC",1, xFilial("XTC") + _cLojaID,"XTC_DESC"))

	//-------------------------+
	// Valida o Id de Postagem |
	//-------------------------+
	If ValType(oRestPv:ShippingData:LogisticsInfo[1]:DeliveryIds[1]:CourierId) <> "U"
		AEcoI11IP(oRestPv:ShippingData:LogisticsInfo[1]:DeliveryIds[1]:CourierId,cCodAfili,cCodCli,cLojaCli,@cCodTransp,@cIdPost,@_cIdServ)
	ElseIf ValType(oRestPv:ShippingData:LogisticsInfo[1]:DeliveryIds[1]:courierName) ==  "vtex:fob_1"
		cCodTransp 	:= ""
		cIdPost		:= ""
	EndIf	 
		
	nDesconto	:= 0 
	If Len(oRestPv:PayMentData:Transactions) > 1
		aEval(oRestPv:PayMentData:Transactions,{|x| nQtdParc += x:Payments[1]:InstallMents})
	Else
		nQtdParc	:= oRestPv:PayMentData:Transactions[1]:Payments[1]:InstallMents
	EndIf	

	nVlrFrete	:= RetPrcUni(oRestPv:Totals[3]:Value)
	nVrSubTot	:= RetPrcUni(oRestPv:Totals[1]:Value)
	nVlrTotal	:= RetPrcUni(oRestPv:Totals[1]:Value) //RetPrcUni(oRestPv:Value)
	nVlrTotMkt	:= RetPrcUni(oRestPv:Value)
	nQtdItem	:= Len(oRestPv:Items)
		
	//---------------------+
	// Numero do Orcamento |
	//---------------------+
	cNumOrc := GetSxeNum("XTA","XTA_NUM")

	dbSelectArea("XTA")
	XTA->( dbSetOrder(1) )

	While XTA->( dbSeek(xFilial("XTA") + cNumOrc) )
		ConfirmSx8()
		cNumOrc := GetSxeNum("XTA","XTA_NUM","",1)
	EndDo	 

	LogExec("INCLUINDO PEDIDO PROTHEUS " + cNumOrc + " CLIENTE " + cNomeCli )

	//-----------------------+
	// Grava Itens do Pedido |
	//-----------------------+
	aRet := AEcoGrvIt(cOrderId,cNumOrc,cCodCli,cLojaCli,cVendedor,@nDesconto,@nPesoBruto,dDtaEmiss,oRestPv:Items,oRestPv:ShippingData,oRestPv)
	If!aRet[1]
		RestArea(aArea)
		Return aRet
	EndIf

	//-----------------+
	// Grava Cabeçalho |
	//-----------------+
	aRet := AEcoGrvCab(	cNumOrc,cOrderId,cCodCli,cLojaCli,cTipoCli,cVendedor,cEndDest,cNumDest,;
						cMunDest,cBaiDest,cCepDest,cEstDest,cNomDest,cDddCel,cDdd1,cTel01,cCelular,;
						cIdEnd,cMotCancel,cPedCodCli,cPedCodInt,cHoraEmis,dDtaEmiss,cPedStatus,nVlrFrete,;
						nVrSubTot,nVlrTotal,nQtdParc,nDesconto,nPesoBruto,cIdPost,cEndComp,cEndRef,;
						cCodTransp,_cIdServ,cCodAfili,nJuros,_cLojaID,_cDescLoja)

	//------------------------------+					
	// Efetua a gravação da Reserva |
	//------------------------------+
	/*
	If aRet[1]
		aRet := AEcoGrvRes(cOrderId,cPedCodCli,cNumOrc,cCodCli,cLojaCli,cVendedor,nDesconto,dDtaEmiss,oRestPv:Items,oRestPv:ShippingData,oRestPv)
	EndIf
	*/
	
	If!aRet[1]
		RestArea(aArea)
		Return aRet
	EndIf	

	//------------------+
	// Grava Financeiro |
	//------------------+
	aRet := AEcoGrvFin(oRestPv:PaymentData,oRestPv,cNumOrc,cOrderId,cPedCodCli,cHoraEmis,cCodAfili,dDtaEmiss,nVlrTotMkt,nVlrFrete,nJuros,nVlrTotal)
		
	If!aRet[1]
		RestArea(aArea)
		Return aRet
	EndIf

	//------------------+
	// Atualiza Status  |
	//------------------+
	aRet := AEcoGrvXTM(cOrderId,cPedStatus,dDataBase,cHoraEmis)

	If !aRet[1]
		RestArea(aArea)
		Return aRet
	EndIf

	//--------------------------------------+
	// Envia Baixa do Pedido para a Rakuten |
	//--------------------------------------+
	/*
	aRet := u_aEcoI11a(cOrderId,cNumOrc)
	If !aRet[1]
		RestArea(aArea)
		Return aRet
	EndIf
	*/
	//-----------------------------------+
	// Confirma a numeração do orçamento |
	//-----------------------------------+
	ConfirmSx8()
	
	RestArea(aArea)
Return aRet

/**************************************************************************************************/
/*/{Protheus.doc} AEcoGrvIt
	@description	Grava os Itens do Pedido e-Commerce
	@author			Bernard M.Margarido
	@version   		1.00
	@since     		10/02/2016
/*/			
/**************************************************************************************************/
Static Function AEcoGrvIt(cOrderId,cNumOrc,cCliente,cLoja,cVendedor,nDesconto,nPesoBruto,dDtaEmiss,oItems,oTransp,oRestPv)
	Local aArea 	:= GetArea()
	Local aRet		:= {.T.,"",""}

	Local aRefImpos	:= {} 

	Local lTesInt	:= GetNewPar("EC_TESINT")
	Local lGrava	:= .T.
	Local lDescPer	:= .F.
	Local lGift		:= .F.
	Local lBrinde	:= .F.
	Local lGratis	:= .F.

	Local cTpOper	:= GetNewPar("EC_TPOPEREC")
	Local cTesEco	:= GetNewPar("EC_TESECO")
	Local cLocal	:= GetNewPar("EC_ARMVEND")
	Local cProduto	:= ""
	Local cItem		:= "01"
	Local cCodKit	:= "kit"
		
	Local nPrd		:= 0
	Local nPesoPrd	:= 0
	Local nPesoCobr	:= 0
	Local nPesoCuba	:= 0
	Local nPesoCubC	:= 0
	Local nPrzEntr	:= 0
	Local nPerDItem	:= 0
	Local nQtdItem	:= 0
	Local nValor	:= 0
	Local nVlrFinal	:= 0
	Local nVlrBrinde:= 0	
	Local nVlrDesc	:= 0
	Local nVlrTotIt	:= 0
		
	Local dDtaEntr	:= Nil
			
	LogExec("GRAVANDO ITENS DO PEDIDO ORDERID " + cOrderId )

	//--------------+
	// Abre Tabelas |
	//--------------+
	dbSelectArea("SB1")
	dbSelectArea("SF4")
	dbSelectArea("XTB")

	//------------------------+
	// Inicia Funções Fiscais |
	//------------------------+
	MaFisEnd()
	MaFisIni(cCliente,cLoja,"C","N",Nil,aRefImpos,,.T.,,,,,,,)

	For nPrd := 1 To Len(oItems)

		If ValType(oItems[nPrd]:RefId) == "U"
			cProduto	:= ""
			aEcoI011Sb1(oItems[nPrd]:productId,@cProduto)
		Else
			cProduto	:= oItems[nPrd]:RefId
		EndIf

		LogExec("ITEM " + cItem + " PRODUTO " + cProduto )
				
		//---------------+
		// Dados do Item |
		//---------------+
		cProduto	:= PadR(cProduto,nTamProd)
				
		//--------------------+
		// Converte em quilos |
		//--------------------+
		nPesoPrd	:= oItems[nPrd]:AdditionalInfo:Dimension:Weight / 1000
		
		//--------------------+
		// Converte em quilos |
		//--------------------+ 
		nPesoCobr	:= oItems[nPrd]:AdditionalInfo:Dimension:Weight / 1000
		nPesoCuba	:= oItems[nPrd]:AdditionalInfo:Dimension:CubicWeight
		nPesoCubC	:= oItems[nPrd]:AdditionalInfo:Dimension:CubicWeight
		
		//-------------------------+
		// Valida se é produto KIT |
		//-------------------------+
		If AT(cCodKit,cProduto) > 0 .Or. ( ValType(oItems[nPrd]:Components) == "A" .And. Len(oItems[nPrd]:Components) > 0 )
			//----------------------------+
			// Grava itens do produto KIT |
			//----------------------------+
			aRet 	:= AEcoI11Kit(cOrderId,cNumOrc,cCliente,cLoja,cVendedor,nDesconto,nPesoBruto,dDtaEmiss,oItems[nPrd]:Quantity,oItems[nPrd]:ProductId,oItems[nPrd]:Components,oTransp,oRestPv,cProduto,nPrd,@cItem)
			If !aRet[1]
				RestArea(aArea)
				Return aRet
			EndIf
		Else
			//----------------+
			// Valida Produto |
			//----------------+
			SB1->( dbSetOrder(1) )
			If !SB1->( dbSeek(xFilial("SB1") + cProduto) )
				LogExec("PRODUTO " + Alltrim(cProduto) + " NAO ENCONTRADO. PEDIDO ORDERID " + cOrderId )
				aRet[1] := .F.
				aRet[2] := cProduto
				aRet[3] := "PRODUTO " + Alltrim(cProduto) + " NAO ENCONTRADO. PEDIDO ORDERID " + cOrderId 
				RestArea(aArea)
				Return aRet
			EndIf

			//cLocal		:= IIF(Empty(SB1->B1_XLOCPAD), SB1->B1_LOCPAD, SB1->B1_XLOCPAD)
			lGift		:= oItems[nPrd]:IsGift
			lBrinde		:= IIF(!lBrinde,lGift,lBrinde)
			lGratis		:= IIF(oItems[nPrd]:SellingPrice <= 0,.T.,.F.)
			nQtdItem	:= oItems[nPrd]:Quantity
			nValor		:= IIF(lGift .Or. lGratis , 0.01, RetPrcUni(oItems[nPrd]:Price))
			nVlrFinal	:= IIF(lGift .Or. lGratis , 0.01, RetPrcUni(oItems[nPrd]:SellingPrice))
			nVlrBrinde	+= IIF(lBrinde .Or. lGratis , ( 0.01 * nQtdItem) ,0)		

			If Empty(oTransp:LogisticsInfo[nPrd]:ShippingEstimateDate)
				dDtaEntr	:= cTod(dDtaEmiss) + Val(oTransp:LogisticsInfo[nPrd]:ShippingEstimate)
				nPrzEntr	:= Val(oTransp:LogisticsInfo[nPrd]:ShippingEstimate)
			Else
				dDtaEntr	:= StoD(StrTran(SubStr(oTransp:LogisticsInfo[nPrd]:ShippingEstimateDate,1,10),"-",""))
				nPrzEntr	:= StoD(StrTran(SubStr(oTransp:LogisticsInfo[nPrd]:ShippingEstimateDate,1,10),"-","")) - cTod(dDtaEmiss)
			EndIf	 
			
			//--------------------------------------------------------+
			// Valida se desconto foi aplicado em percentual ou valor |
			//--------------------------------------------------------+
			cNameDesc	:= IIF(Len(oItems[nPrd]:PriceTags) > 0 ,oItems[nPrd]:PriceTags[1]:name,"") 
			lDescPer	:= IIF(Len(oItems[nPrd]:PriceTags) > 0 ,oItems[nPrd]:PriceTags[1]:IsPercentual,.F.)
			nVlrDesc	:= IIF(Len(oItems[nPrd]:PriceTags) > 0 ,IIF(oItems[nPrd]:PriceTags[1]:value < 0,oItems[nPrd]:PriceTags[1]:value,0),0)
				
			//------------------------------+
			// Acha percentual de desconto  |
			//------------------------------+ 
			If !lGift .And. nVlrBrinde == 0 .And. At("discount@shipping",Lower(cNameDesc)) <= 0  
				If lDescPer
					nPerDItem := RetPrcUni(oItems[nPrd]:PriceTags[1]:Value * -1)
				ElseIf nVlrDesc < 0
					nVlrDesc := RetPrcUni(oItems[nPrd]:PriceTags[1]:Value * -1)
					nVlrTotIt:= nQtdItem * nValor		
					nPerDItem:= Round(( nVlrDesc / nVlrTotIt ) * 100,2)
				EndIf	
			EndIf
			
			//-----------------+
			// Soma peso Bruto |
			//-----------------+
			nPesoBruto	+= nPesoPrd
			
			//------------------------+
			// Utiliza Tes do Produto |
			//------------------------+
			If !Empty(SB1->B1_TS)
				cTesEco := SB1->B1_TS 
			EndIf
			
			//-----------------+
			// Tes Inteligente |
			//-----------------+
			If lTesInt
				cTesEco :=  MaTesInt(2,cTpOper,cCliente,cLoja,"C",cProduto)
			EndIf
	
			//------------+
			// Valida Tes |
			//------------+
			SF4->( dbSetOrder(1) )
			If !SF4->( dbSeek(xFilial("SF4") + cTesEco) )
				LogExec("CODIGO TES " + Alltrim(cTesEco) + " NAO ENCONTRADO. PEDIDO ORDERID " + cOrderId)
				aRet[1] := .F.
				aRet[2] := cTesEco
				aRet[3] := "CODIGO TES  " + Alltrim(cTesEco) + " NAO ENCONTRADO. PEDIDO ORDERID " + cOrderId
				RestArea(aArea)
				Return aRet
			EndIf
					
			//----------------------------------------+
			// Retira o valor de IPI do produto       |
			// quando pedido for faturado calcula IPI |
			// no padrao do sistema                   |
			//----------------------------------------+
			If SB1->B1_IPI > 0 .And. SF4->F4_IPI == "S" //.And. nVlrBrinde <= 0
				nVlrFinal := NoRound( nVlrFinal / ( 1 + ( SB1->B1_IPI / 100 ) ), 4 )
			EndIf
	
			//-------------------------------+
			// Valida se valor foi informado |
			//-------------------------------+
			If Empty(nVlrFinal) 
				LogExec("VALOR DO PRODUTO " + Alltrim(cProduto) + " NAO INFORMADO. VERIFIQUE VALOR NO ADMINISTRATIVO VTEX PARA PEDIDO ORDERID " + cOrderId)
				aRet[1] := .F.
				aRet[2] := cOrderId
				aRet[3] := "VALOR DO PRODUTO " + Alltrim(cProduto) + " NAO INFORMADO. VERIFIQUE VALOR NO ADMINISTRATIVO VTEX PARA PEDIDO ORDERID " + cOrderId
				RestArea(aArea)
				Return aRet
			EndIf
	
			//----------------------------------------+
			// Adiciona Produto para calculos fiscais |
			//----------------------------------------+
			MaFisAdd(cProduto,cTesEco,nQtdItem,nVlrFinal,nDesconto,"","",,0,0,0,0,Round(nQtdItem * nVlrFinal,nDecIt))
	
			//-----------------+
			// Grava Itens SL2 |
			//-----------------+
			lGrava := .T.
	
			XTB->( dbSetOrder(1) )
			If XTB->( dbSeek(xFilial("XTB") + cNumOrc + cItem + cProduto) )
				lGrava := .F.
			EndIf	
	
			RecLock("XTB",lGrava)
				XTB->XTB_FILIAL 	:= xFilial("XTB")
				XTB->XTB_NUM		:= cNumOrc 
				XTB->XTB_PRODUT		:= cProduto
				XTB->XTB_ITEM		:= cItem
				XTB->XTB_DESCRI		:= SB1->B1_DESC
				XTB->XTB_QUANT		:= nQtdItem
				XTB->XTB_VRUNIT		:= nVlrFinal
				XTB->XTB_VLRITE		:= Round(nQtdItem * nVlrFinal,nDecIt)
				XTB->XTB_LOCAL		:= cLocal
				XTB->XTB_UM			:= SB1->B1_UM
				XTB->XTB_DESC		:= nPerDItem
				XTB->XTB_VALDES		:= IIF(nVlrDesc > 0,nVlrDesc,Round( nValor * (nPerDItem /100 ),2))
				XTB->XTB_TES		:= cTesEco
				XTB->XTB_CF			:= SF4->F4_CF
				XTB->XTB_VALIPI		:= MaFisRet(nPrd,"IT_VALIPI") 
				XTB->XTB_VALICM		:= MaFisRet(nPrd,"IT_VALICM")
				XTB->XTB_VALISS		:= MaFisRet(nPrd,"IT_VALISS")
				XTB->XTB_BASEIC		:= MaFisRet(nPrd,"IT_BASEICM")
				XTB->XTB_STATUS		:= ""
				XTB->XTB_EMISSA		:= cTod(dDtaEmiss)
				XTB->XTB_PRCTAB		:= nValor
				XTB->XTB_GRADE		:= "N"
				XTB->XTB_VEND		:= cVendedor
				XTB->XTB_VALFRE		:= 0
				XTB->XTB_SEGURO		:= 0
				XTB->XTB_DESPES		:= 0
				XTB->XTB_ICMSRE		:= MaFisRet(nPrd,"IT_VALSOL")
				XTB->XTB_BRICMS		:= MaFisRet(nPrd,"IT_BASESOL")
				XTB->XTB_VALPIS		:= MaFisRet(nPrd,"IT_VALPIS")
				XTB->XTB_VALCOF		:= MaFisRet(nPrd,"IT_VALCOF")
				XTB->XTB_VALCSL		:= MaFisRet(nPrd,"IT_VALCSL")
				XTB->XTB_VALPS2		:= MaFisRet(nPrd,"IT_VALPS2")
				XTB->XTB_VALCF2		:= MaFisRet(nPrd,"IT_VALCF2")
				XTB->XTB_BASEPS		:= MaFisRet(nPrd,"IT_BASEPS2")
				XTB->XTB_BASECF		:= MaFisRet(nPrd,"IT_BASECF2")
				XTB->XTB_ALIQPS		:= MaFisRet(nPrd,"IT_ALIQPS2")
				XTB->XTB_ALIQCF		:= MaFisRet(nPrd,"IT_ALIQCF2")
				XTB->XTB_SEGUM		:= SB1->B1_SEGUM
				XTB->XTB_PEDRES		:= ""
				XTB->XTB_FDTENT		:= dDtaEntr
				XTB->XTB_PRODTP		:= ""
				XTB->XTB_PRZENT		:= nPrzEntr
				XTB->XTB_STATIT		:= "" 
				XTB->XTB_DESTAT		:= ""
			XTB->( MsUnLock() )
			
			//------------------------+
			// Codigo do Proximo Item |
			//------------------------+
			cItem := Soma1(cItem) //StrZero(nPrd,nTItemL2)
		EndIf	

	Next nPrd
	
	//-----------------------------------------+
	// Valida se teve brinde                   |
	// aplica desconto de 0.1 no primeiro item | 
	//-----------------------------------------+
	If lBrinde .Or. lGratis
		nDesconto := nVlrBrinde
		//AEcoI11Brinde(cNumOrc,nVlrBrinde)
	EndIf
	
	RestArea(aArea)
Return aRet 

/**************************************************************************************/
/*/{Protheus.doc} AEcoI11Kit
	@description Realiza a gravação dos produtos KIT
	@author Bernard M. Margarido
	@since 02/03/2017
	@version undefined
	@type function
/*/
/**************************************************************************************/
Static Function AEcoI11Kit(cOrderId,cNumOrc,cCliente,cLoja,cVendedor,nDesconto,nPesoBruto,dDtaEmiss,nQtdKit,cRefKit,oItKit,oTransp,oRestPv,cCodKit,nItAtu,cItem)
Local aArea	:= GetArea()

Local aRet		:= {.T.,"",""}

Local lTesInt	:= GetNewPar("EC_TESINT")
Local lGrava	:= .T.
Local lDescPer	:= .F.
//Local lGift		:= .F.
//Local lBrinde	:= .F.
Local lGratis	:= .F.

Local cTpOper	:= GetNewPar("EC_TPOPEREC")
Local cTesEco	:= GetNewPar("EC_TESECO")
Local cLocal	:= GetNewPar("EC_ARMVEND")
Local cProduto	:= ""

Local nPrd		:= 0
Local nPesoPrd	:= 0
//Local nPesoCobr	:= 0
//Local nPesoCuba	:= 0
//Local nPesoCubC	:= 0
Local nPrzEntr	:= 0
Local nPerDItem	:= 0
Local nQtdItem	:= 0
Local nValor	:= 0
Local nVlrFinal	:= 0

Local dDtaEntr	:= Nil
		
LogExec("GRAVANDO ITENS DO PRODUTO KIT " + cCodKit )

//--------------+
// Abre Tabelas |
//--------------+
dbSelectArea("SB1")
dbSelectArea("SF4")
dbSelectArea("XTB")

For nPrd := 1 To Len(oItKit)
	
	//---------------------------+
	// Formata codigo do Produto |
	//---------------------------+
	If ValType(oItKit[nPrd]:RefId) == "U"
		cProduto	:= ""
		aEcoI011Sb1(oItKit[nPrd]:productId,@cProduto)
	Else 
		cProduto	:= PadR(oItKit[nPrd]:RefId,nTamProd) 
	EndIf
	
	LogExec("ITEM " + cItem + " PRODUTO KIT " + cProduto )
	
	//----------------+
	// Valida Produto |
	//----------------+
	SB1->( dbSetOrder(1) )
	If !SB1->( dbSeek(xFilial("SB1") + cProduto) )
		LogExec("PRODUTO " + Alltrim(cProduto) + " NAO ENCONTRADO. PEDIDO ORDERID " + cOrderId )
		aRet[1] := .F.
		aRet[2] := cProduto
		aRet[3] := "PRODUTO " + Alltrim(cProduto) + " NAO ENCONTRADO. PEDIDO ORDERID " + cOrderId 
		RestArea(aArea)
		Return aRet
	EndIf

	//cLocal		:= IIF(Empty(SB1->B1_XLOCPAD), SB1->B1_LOCPAD, SB1->B1_XLOCPAD)
	lGratis		:= ( RetPrcUni(oItKit[nPrd]:SellingPrice) == 0 )
	nQtdItem	:= nQtdKit * oItKit[nPrd]:Quantity
	nValor		:= RetPrcUni(oItKit[nPrd]:Price)
	nVlrFinal	:= IIF(lGratis, 0.01, RetPrcUni(oItKit[nPrd]:SellingPrice))
	nDesconto	+= IIF(lGratis, Round(nQtdItem * nVlrFinal,nDecIt),0)
	If Empty(oTransp:LogisticsInfo[nItAtu]:ShippingEstimateDate)
		dDtaEntr	:= cTod(dDtaEmiss) + Val(oTransp:LogisticsInfo[nItAtu]:ShippingEstimate)
		nPrzEntr	:= Val(oTransp:LogisticsInfo[nItAtu]:ShippingEstimate)
	Else
		dDtaEntr	:= StoD(StrTran(SubStr(oTransp:LogisticsInfo[nItAtu]:ShippingEstimateDate,1,10),"-",""))
		nPrzEntr	:= StoD(StrTran(SubStr(oTransp:LogisticsInfo[nItAtu]:ShippingEstimateDate,1,10),"-","")) - cTod(dDtaEmiss)
	EndIf	 
		
	//--------------------------------------------------------+
	// Valida se desconto foi aplicado em percentual ou valor |
	//--------------------------------------------------------+
	lDescPer	:= IIF(Len(oItKit[nPrd]:PriceTags) > 0 ,oItKit[nPrd]:PriceTags[1]:IsPercentual,.F.)
	nVlrDesc	:= IIF(Len(oItKit[nPrd]:PriceTags) > 0 ,IIF(oItKit[nPrd]:PriceTags[1]:value < 0,oItKit[nPrd]:PriceTags[1]:value,0),0)
		
	//------------------------------+
	// Acha percentual de desconto  |
	//------------------------------+ 
	If lDescPer
		nPerDItem := RetPrcUni(oItKit[nPrd]:PriceTags[1]:Value * -1)
	ElseIf nVlrDesc < 0
		nVlrDesc := RetPrcUni(oItKit[nPrd]:PriceTags[1]:Value * -1)
		nVlrTotIt:= nQtdItem * nValor		
		nPerDItem:= Round(( nVlrDesc / nVlrTotIt ) * 100,2)
	EndIf		
		
	//-----------------+
	// Soma peso Bruto |
	//-----------------+
	nPesoBruto	+= nPesoPrd
	
	//------------------------+
	// Utiliza Tes do Produto |
	//------------------------+
	/*
	If !Empty(SB1->B1_TS)
		cTesEco := SB1->B1_TS 
	EndIf
	*/
	
	//-----------------+
	// Tes Inteligente |
	//-----------------+
	If lTesInt
		cTesEco :=  MaTesInt(2,cTpOper,cCliente,cLoja,"C",cProduto)
	EndIf
	
	//------------+
	// Valida Tes |
	//------------+
	SF4->( dbSetOrder(1) )
	If !SF4->( dbSeek(xFilial("SF4") + cTesEco) )
		LogExec("CODIGO TES " + Alltrim(cTesEco) + " NAO ENCONTRADO. PEDIDO ORDERID " + cOrderId)
		aRet[1] := .F.
		aRet[2] := cTesEco
		aRet[3] := "CODIGO TES  " + Alltrim(cTesEco) + " NAO ENCONTRADO. PEDIDO ORDERID " + cOrderId
		RestArea(aArea)
		Return aRet
	EndIf
			
	//----------------------------------------+
	// Retira o valor de IPI do produto       |
	// quando pedido for faturado calcula IPI |
	// no padrao do sistema                   |
	//----------------------------------------+
	If SB1->B1_IPI > 0 .And. SF4->F4_IPI == "S" 
		nVlrFinal := NoRound( nVlrFinal / ( 1 + ( SB1->B1_IPI / 100 ) ), 4 )
	EndIf
	
	//-------------------------------+
	// Valida se valor foi informado |
	//-------------------------------+
	If Empty(nVlrFinal)
		LogExec("VALOR DO PRODUTO " + Alltrim(cProduto) + " NAO INFORMADO. VERIFIQUE VALOR NO ADMINISTRATIVO ECOMMERCE PARA PEDIDO ORDERID " + cOrderId)
		aRet[1] := .F.
		aRet[2] := cOrderId
		aRet[3] := "VALOR DO PRODUTO " + Alltrim(cProduto) + " NAO INFORMADO. VERIFIQUE VALOR NO ADMINISTRATIVO ECOMMERCE PARA PEDIDO ORDERID " + cOrderId
		RestArea(aArea)
		Return aRet
	EndIf
	
	//----------------------------------------+
	// Adiciona Produto para calculos fiscais |
	//----------------------------------------+
	MaFisAdd(cProduto,cTesEco,nQtdItem,nVlrFinal,nDesconto,"","",,0,0,0,0,Round(nQtdItem * nVlrFinal,nDecIt))
	
	//-----------------+
	// Grava Itens SL2 |
	//-----------------+
	lGrava := .T.
	
	XTB->( dbSetOrder(1) )
	If XTB->( dbSeek(xFilial("XTB") + cNumOrc + cItem + cProduto) )
		lGrava := .F.
	EndIf	
	
	RecLock("XTB",lGrava)
		XTB->XTB_FILIAL 	:= xFilial("XTB")
		XTB->XTB_NUM		:= cNumOrc 
		XTB->XTB_PRODUT		:= cProduto
		XTB->XTB_ITEM		:= cItem
		XTB->XTB_DESCRI		:= SB1->B1_DESC
		XTB->XTB_QUANT		:= nQtdItem
		XTB->XTB_VRUNIT		:= nVlrFinal
		XTB->XTB_VLRITE		:= Round(nQtdItem * nVlrFinal,nDecIt)
		XTB->XTB_LOCAL		:= cLocal
		XTB->XTB_UM			:= SB1->B1_UM
		XTB->XTB_DESC		:= IIF(nPerDItem < 100, nPerDItem, 0)
		XTB->XTB_VALDES		:= IIF(nPerDItem < 100, Round( nValor * (nPerDItem /100 ),2), 0)
		XTB->XTB_TES		:= cTesEco
		XTB->XTB_CF			:= SF4->F4_CF
		XTB->XTB_VALIPI		:= MaFisRet(nPrd,"IT_VALIPI") 
		XTB->XTB_VALICM		:= MaFisRet(nPrd,"IT_VALICM")
		XTB->XTB_VALISS		:= MaFisRet(nPrd,"IT_VALISS")
		XTB->XTB_BASEIC		:= MaFisRet(nPrd,"IT_BASEICM")
		XTB->XTB_STATUS		:= ""
		XTB->XTB_EMISSA		:= cTod(dDtaEmiss)
		XTB->XTB_PRCTAB		:= nValor
		XTB->XTB_GRADE		:= "N"
		XTB->XTB_VEND		:= cVendedor
		XTB->XTB_VALFRE		:= 0
		XTB->XTB_SEGURO		:= 0
		XTB->XTB_DESPES		:= 0
		XTB->XTB_ICMSRE		:= MaFisRet(nPrd,"IT_VALSOL")
		XTB->XTB_BRICMS		:= MaFisRet(nPrd,"IT_BASESOL")
		XTB->XTB_VALPIS		:= MaFisRet(nPrd,"IT_VALPIS")
		XTB->XTB_VALCOF		:= MaFisRet(nPrd,"IT_VALCOF")
		XTB->XTB_VALCSL		:= MaFisRet(nPrd,"IT_VALCSL")
		XTB->XTB_VALPS2		:= MaFisRet(nPrd,"IT_VALPS2")
		XTB->XTB_VALCF2		:= MaFisRet(nPrd,"IT_VALCF2")
		XTB->XTB_BASEPS		:= MaFisRet(nPrd,"IT_BASEPS2")
		XTB->XTB_BASECF		:= MaFisRet(nPrd,"IT_BASECF2")
		XTB->XTB_ALIQPS		:= MaFisRet(nPrd,"IT_ALIQPS2")
		XTB->XTB_ALIQCF		:= MaFisRet(nPrd,"IT_ALIQCF2")
		XTB->XTB_SEGUM		:= SB1->B1_SEGUM
		XTB->XTB_PEDRES		:= ""
		XTB->XTB_FDTENT		:= dDtaEntr
		XTB->XTB_PRODTP		:= ""
		XTB->XTB_PRZENT		:= nPrzEntr
		XTB->XTB_STATIT		:= "" 
		XTB->XTB_DESTAT		:= ""
		XTB->XTB_KIT		:= cRefKit
	XTB->( MsUnLock() )
	
	//------------------------+
	// Codigo do Proximo Item |
	//------------------------+
	cItem := Soma1(cItem) //StrZero(nPrd,nTItemL2)

Next nPrd

RestArea(aArea)
Return aRet

/******************************************************************************/
/*/{Protheus.doc} AEcoI11Brinde
	@description Aplica desconto do brinde no primeiro item do pedido
	@author Bernard M. Margarido
	@since 01/06/2017
	@version undefined
	@type function
/*/
/******************************************************************************/
Static Function AEcoI11Brinde(cNumOrc,nVlrBrinde)
Local aArea		:= GetArea()

Local cItem		:= PadL("01",nTItemL2,"0") 

Local nPrcVen	:= 0
Local nPrcTab	:= 0
Local nVlrTot	:= 0
Local nVlrSDesc	:= 0
Local nQtdVen	:= 0
Local nValDesc	:= 0
Local nPDesc	:= 0	


dbSelectArea("XTB")
XTB->( dbSetOrder(2) )
XTB->( dbSeek(xFilial("XTB") + cNumOrc + cItem) )

//----------------------+
// Grava valores atuais |
//----------------------+
nPrcVen := XTB->XTB_VRUNIT
nPrcTab	:= XTB->XTB_PRCTAB
nQtdVen := XTB->XTB_QUANT
nVlrTot	:= XTB->XTB_VLRITE
nValDesc:= XTB->XTB_VALDES
nPDesc	:= XTB->XTB_DESC

//-------------------------+
// Calula os novos valores |
//-------------------------+
nVlrSDesc	:= Round( nQtdVen * nPrcTab ,2)
nValDesc	:= nValDesc + nVlrBrinde 
nVlrTot		:= nVlrTot - nVlrBrinde  
nPDesc		:= 0
nPrcVen		:= Round(nVlrTot / nQtdVen ,2)

//------------------------------------+
// Atualiza informações no orçamentos |
//------------------------------------+
RecLock("XTB",.F.)
	XTB->XTB_VRUNIT		:= nPrcVen
	XTB->XTB_VLRITE		:= nVlrTot
	XTB->XTB_VALDES		:= nValDesc
	XTB->XTB_DESC		:= nPDesc
XTB->( MsUnLock() )

RestArea(aArea)
Return .T.

/**************************************************************************************/
/*/{Protheus.doc} aEcoI011Sb1
	@description Busca produto pelo ID eCommerce
	@author Bernard M. Margarido
	@since 13/06/2019
	@version undefined
	@type function
/*/
/**************************************************************************************/
Static Function aEcoI011Sb1(_cProductId,cProduto)
Local _aArea	:= GetArea()
Local _cQuery 	:= ""
Local _cAlias 	:= ""

_cQuery := " SELECT " + CRLF
_cQuery += "	B1.B1_COD, " + CRLF
_cQuery += "	B1.B1_XIDSKU " + CRLF
_cQuery += " FROM " + CRLF
_cQuery += "	" + RetSqlName("SB1") + " B1 " + CRLF
_cQuery += " WHERE " + CRLF
_cQuery += "	B1.B1_FILIAL = '" + xFilial("SB1") + "' AND " + CRLF
_cQuery += "	B1.B1_XIDSKU = " + _cProductId  + " AND " + CRLF
_cQuery += "	B1.D_E_L_E_T_ = '' "

_cAlias := MPSysOpenQuery(_cQuery)

cProduto:= (_cAlias)->B1_COD

(_cAlias)->( dbCloseArea() )

RestArea(_aArea)
Return Nil 

/**************************************************************************************/
/*/{Protheus.doc} AEcoGrvRes
	@description Efetua a reserva do item 
	@author Bernard M. Margarido
	@since 13/06/2019
	@version undefined
	@type function
/*/
/**************************************************************************************/
Static Function AEcoGrvRes(cOrderId,cPedCodCli,cNumOrc,cCodCli,cLojaCli,cVendedor,nDesconto,dDtaEmiss,oItems,oTransp,oRestPv)
Local _aArea 		:= GetArea()
Local aRet			:= {.T.,"",""}
Local aOperacao		:= {}
Local aLote			:= {}

Local _cCodRes		:= ""
Local cProduto		:= ""
Local cTipo			:= "LJ"
Local _cClient		:= "ECOMM"
Local cCodKit		:= "kit"
Local cLocal		:= GetNewPar("EC_ARMVEND")

Local _nTCodRes 	:= TamSx3("C0_DOCRES")[1]
Local nQtdItem		:= 0
Local _nSaldoSb2	:= 0
Local nPrd			:= 0

Local dDtVldRserv	:= GetNewPar("EC_DTVLDRE","01/01/2049")

Local _lGerou		:= .F.
Local lGift			:= .F.
Local lBrinde		:= .F.
Local _lInclui		:= .F.

LogExec("EFETUANDO A RESERVA DO PEDIDO " + cOrderId )

//--------------+
// Abre Tabelas |
//--------------+
dbSelectArea("SC0")
dbSelectArea("SB1")
dbSelectArea("XTB")

_cCodRes	:= GetSxeNum("SC0","C0_NUM")
SC0->(dbSetOrder(1) )
While SC0->(dbSeek(xFilial("SC0") + _cCodRes) )
	_cCodRes	:= GetSxeNum("SC0","C0_NUM","",1)
EndDo

For nPrd := 1 To Len(oItems)

	If ValType(oItems[nPrd]:RefId) == "U"
		cProduto	:= ""
		aEcoI011Sb1(oItems[nPrd]:productId,@cProduto)
	Else
		cProduto := oItems[nPrd]:RefId
		If Empty(oItems[nPrd]:RefId)
			cProduto	:= ""
			aEcoI011Sb1(oItems[nPrd]:productId,@cProduto)
		Else 
			cProduto := oItems[nPrd]:RefId
		EndIf 
	EndIf

	LogExec(" GERANDO A RESERVA DO PRODUTO " + cProduto )
				
	//---------------+
	// Dados do Item |
	//---------------+
	cProduto	:= PadR(cProduto,nTamProd)
	nQtdItem	:= oItems[nPrd]:Quantity

	//-------------------------+
	// Valida se é produto KIT |
	//-------------------------+
	If AT(cCodKit,cProduto) > 0 .Or. ( ValType(oItems[nPrd]:Components) == "A" .And. Len(oItems[nPrd]:Components) > 0 )
		//-----------------------------------+
		// Efetua a reserva dos peodutos KIT |
		//-----------------------------------+
		aRet 	:= AEcoGrvRKit(cOrderId,cPedCodCli,cNumOrc,cCodCli,cLojaCli,cVendedor,nDesconto,dDtaEmiss,oItems[nPrd]:Quantity,oItems[nPrd]:Components,oTransp,oRestPv,cProduto,dDtVldRserv)
		If !aRet[1]
			RestArea(_aArea)
			Return aRet
		EndIf
	Else

		//----------------+
		// Valida Produto |
		//----------------+
		SB1->( dbSetOrder(1) )
		If !SB1->( dbSeek(xFilial("SB1") + cProduto) )
			LogExec("PRODUTO " + Alltrim(cProduto) + " NAO ENCONTRADO. PEDIDO ORDERID " + cOrderId )
			aRet[1] := .F.
			aRet[2] := cProduto
			aRet[3] := "PRODUTO " + Alltrim(cProduto) + " NAO ENCONTRADO. PEDIDO ORDERID " + cOrderId 
			RestArea(_aArea)
			Return aRet
		EndIf
		
		//-----------------------------------------+
		// Valida se produto tem desconto / brinde |
		//-----------------------------------------+ 
		//cLocal		:= IIF(Empty(SB1->B1_XLOCPAD), SB1->B1_LOCPAD, SB1->B1_XLOCPAD)
		lGift		:= oItems[nPrd]:IsGift
		lBrinde		:= IIF(!lBrinde,lGift,lBrinde)

		If lBrinde .Or. lGift
			Loop
		Endif

		//--------------------------+
		// Valida se existe reserva |
		//--------------------------+
		_lInclui := .F.
		If !SC0->( dbSeek(xFilial("SC0") + _cCodRes +  cProduto + cLocal) )
			_lInclui := .T.
		ElseIf nQtdItem <> SC0->C0_QUANT
			_lInclui := .T.
		EndIf 

		If _lInclui

			//----------------------------------------+	
			// Valida se produto tem armzem de venda  |
			//----------------------------------------+
			_aAreaSB2 := SB2->( GetArea() )
				dbSelectArea("SB2")
				SB2->( dbSetOrder(1) )
				If !SB2->( dbSeek(xFilial("SB2") + cProduto + cLocal) )
					CriaSb2(cProduto,cLocal)
				EndIf
				//-------------------------+
				// Valida saldo do produto | 
				//-------------------------+
				_nSaldoSb2	:= 0
				_nSaldoSb2 	:= SaldoSB2()
				If _nSaldoSb2 <= 0
					aRet[1]	:= .F.
					aRet[2]	:= cOrderId
					aRet[3]	:= "PRODUTO " + cProduto  + " SEM SALDO EM ESTOQUE NO ARMAZEM " + cLocal + " ."
					RestArea(_aArea)
					Return aRet
				EndIf	
			RestArea(_aAreaSB2)

			//------------------------------+
			// Inicia a gravação da reserva |
			//------------------------------+
			aOperacao 	:= {1, cTipo, cPedCodCli, _cClient, xFilial("SC0"), "Reserva eCommerce:" + cPedCodCli}
			aLote   	:= {"" , "" , "", ""}
			_lGerou		:= a430Reserv(	aOperacao						,;			// Array contendo dados da reserva
										_cCodRes						,;			// Numero da Reserva
										PadR(cProduto, nTamProd)		,;			// Produto 
										cLocal							,;			// Armazem  
										nQtdItem						,;			// Saldo
										aLote,,							)			// Lote 
			If _lGerou
				//--------------------+
				// Confirma numeração |
				//--------------------+
				ConfirmSx8()
				
				//-----------------------+
				// Posiciona Pre Empenho |
				//-----------------------+
				SC0->( dbSetOrder(4) )
				If SC0->( dbSeek(xFilial("SC0") + cTipo + PadR(cPedCodCli,_nTCodRes) ))
					While SC0->( !Eof() .And. xFilial("SC0") + cTipo + PadR(cPedCodCli,_nTCodRes) == SC0->C0_FILIAL + SC0->C0_TIPO + SC0->C0_DOCRES )
						//---------------------------------------------+
						// Validas e Reserva foi realizada com sucesso |
						//---------------------------------------------+
						If ( RTrim(SC0->C0_PRODUTO) == RTrim(cProduto) ) .And. ( cPedCodCli $ SC0->C0_OBS ) .And. ( SC0->C0_NUM == _cCodRes )
							RecLock("SC0",.F.)
								SC0->C0_QUANT  -= nQtdItem
								SC0->C0_QTDPED += nQtdItem
								SC0->C0_VALIDA := IIF(ValType(dDtVldRserv) == "C",cTod(dDtVldRserv),dDtVldRserv)
							SC0->(MsUnLock())

							LogExec("RESERVA " + _cCodRes + " EFETUADA COM SUCESSO.")
						EndIf 
						SC0->( dbSkip() )
					EndDo 
				Else
					//-----------------------------------+
					// Desarma Transacao em caso de erro |
					//-----------------------------------+
					RollBackSx8()
					LogExec("ERRO AO GERAR PRE EMPENHO " + _cCodRes + " .")	

					aRet[1]	:= .F.
					aRet[2]	:= cOrderId
					aRet[3]	:= "ERRO AO GERAR PRE EMPENHO " + _cCodRes + " ."
				EndIf
			Else
				//-----------------------------------+
				// Desarma Transacao em caso de erro |
				//-----------------------------------+
				RollBackSx8()
				LogExec("ERRO AO GERAR PRE EMPENHO " + _cCodRes + " .")	

				aRet[1]	:= .F.
				aRet[2]	:= cOrderId
				aRet[3]	:= "ERRO AO GERAR PRE EMPENHO " + _cCodRes + " ."
				
			EndIf
		EndIf
	EndIf	
Next nPrd			

RestArea(_aArea)
Return aRet

/**************************************************************************************/
/*/{Protheus.doc} AEcoGrvRKit
	@description Realiza a reserva dos produtos KIT
	@author Bernard M. Margarido
	@since 13/06/2019
	@version undefined
	@type function
/*/
/**************************************************************************************/
Static Function AEcoGrvRKit(cOrderId,cPedCodCli,cNumOrc,cCodCli,cLojaCli,cVendedor,nDesconto,dDtaEmiss,nQtdKit,oItKit,oTransp,oRestPv,cCodKit,dDtVldRserv)
Local _aArea	:= GetArea()

Local aRet		:= {.T.,"",""}
Local aOperacao	:= {}
Local aLote		:= {}

Local _cCodRes	:= ""
Local cProduto	:= ""
Local cTipo		:= "LJ"
Local _cClient	:= "ECOMM"
Local cLocal	:= GetNewPar("EC_ARMVEND")

Local _nTCodRes 	:= TamSx3("C0_DOCRES")[1]
Local nQtdItem	:= 0
Local nX		:= 0

Local _lInclui	:= .F.
		
LogExec("EFETUANDO A RESERVA DOS PRODUTOS KIT PEDIDO " + cOrderId)

//--------------+
// Abre Tabelas |
//--------------+
dbSelectArea("SC0")
dbSelectArea("SB1")
dbSelectArea("XTB")

_cCodRes	:= GetSxeNum("SC0","C0_NUM")
SC0->(dbSetOrder(1) )
While SC0->(dbSeek(xFilial("SC0") + _cCodRes) )
	_cCodRes	:= GetSxeNum("SC0","C0_NUM","",1)
EndDo

For nX := 1 To Len(oItKit)
	
	//---------------------------+
	// Formata codigo do Produto |
	//---------------------------+
	If ValType(oItKit[nX]:RefId) == "U" 
		cProduto	:= ""
		aEcoI011Sb1(oItKit[nX]:productId,@cProduto)
	Else
		If Empty(oItKit[nX]:RefId)
			cProduto	:= ""
			aEcoI011Sb1(oItKit[nX]:productId,@cProduto)
		Else 
			cProduto	:= PadR(oItKit[nX]:RefId,nTamProd) 
		EndIf 
	EndIf

	nQtdItem	:= nQtdKit * oItKit[nX]:Quantity

	LogExec(" EFETUAND A RESERVA DO PRODUTO KIT " + cProduto )
	
	//----------------+
	// Valida Produto |
	//----------------+
	SB1->( dbSetOrder(1) )
	If !SB1->( dbSeek(xFilial("SB1") + cProduto) )
		LogExec("PRODUTO " + Alltrim(cProduto) + " NAO ENCONTRADO. PEDIDO ORDERID " + cOrderId )
		aRet[1] := .F.
		aRet[2] := cProduto
		aRet[3] := "PRODUTO " + Alltrim(cProduto) + " NAO ENCONTRADO. PEDIDO ORDERID " + cOrderId 
		RestArea(aArea)
		Return aRet
	EndIf

	//cLocal		:= IIF(Empty(SB1->B1_XLOCPAD), SB1->B1_LOCPAD, SB1->B1_XLOCPAD)

	//--------------------------+
	// Valida se existe reserva |
	//--------------------------+
	dbSelectArea("SC0")
	SC0->( dbSetOrder(1) )
	_lInclui := .F.
	If !SC0->( dbSeek(xFilial("SC0") + _cCodRes +  cProduto + cLocal) )
		_lInclui := .T.
	ElseIf nQtdItem <> SC0->C0_QUANT
		_lInclui := .T.
	EndIf 

	If _lInclui

		//----------------------------------------+	
		// Valida se produto tem armzem de venda  |
		//----------------------------------------+
		_aAreaSB2 := SB2->( GetArea() )
			dbSelectArea("SB2")
			SB2->( dbSetOrder(1) )
			If !SB2->( dbSeek(xFilial("SB2") + cProduto + cLocal) )
				CriaSb2(cProduto,cLocal)
			EndIf

			//-------------------------+
			// Valida saldo do produto | 
			//-------------------------+
			_nSaldoSb2 := 0
			_nSaldoSb2 := SaldoSB2()
			If _nSaldoSb2 <= 0
				aRet[1]	:= .F.
				aRet[2]	:= cOrderId
				aRet[3]	:= "PRODUTO " + cProduto  + " SEM SALDO EM ESTOQUE NO ARMAZEM " + cLocal + " ."
				RestArea(_aArea)
				Return aRet
			EndIf

		RestArea(_aAreaSB2)

		//------------------------------+
		// Inicia a gravação da reserva |
		//------------------------------+
		aOperacao 	:= {1, cTipo, cPedCodCli, _cClient, xFilial("SC0"), "Reserva eCommerce:" + cPedCodCli}
		aLote   	:= {"" , "" , "", ""}
		_lGerou		:= a430Reserv(	aOperacao						,;			// Array contendo dados da reserva
									_cCodRes						,;			// Numero da Reserva
									PadR(cProduto, nTamProd)		,;			// Produto 
									cLocal							,;			// Armazem  
									nQtdItem						,;			// Saldo
									aLote,,							)			// Lote 
		If _lGerou
			//--------------------+
			// Confirma numeração |
			//--------------------+
			ConfirmSx8()
			
			//-----------------------+
			// Posiciona Pre Empenho |
			//-----------------------+
			SC0->( dbSetOrder(4) )
			If SC0->( dbSeek(xFilial("SC0") + cTipo + PadR(cPedCodCli,_nTCodRes) ))
				While SC0->( !Eof() .And. xFilial("SC0") + cTipo + PadR(cPedCodCli,_nTCodRes) == SC0->C0_FILIAL + SC0->C0_TIPO + SC0->C0_DOCRES )
						//---------------------------------------------+
						// Validas e Reserva foi realizada com sucesso |
						//---------------------------------------------+
						If ( RTrim(SC0->C0_PRODUTO) == RTrim(cProduto) ) .And. ( cPedCodCli $ SC0->C0_OBS ) .And. ( SC0->C0_NUM == _cCodRes )
							RecLock("SC0",.F.)
								SC0->C0_QUANT  -= nQtdItem
								SC0->C0_QTDPED += nQtdItem
								SC0->C0_VALIDA := IIF(ValType(dDtVldRserv) == "C",cTod(dDtVldRserv),dDtVldRserv)
							SC0->(MsUnLock())

							LogExec("RESERVA " + _cCodRes + " EFETUADA COM SUCESSO.")
						EndIf 
						SC0->( dbSkip() )
					EndDo 
			Else
				LogExec("ERRO AO GERAR RESERVA " + _cCodRes )
			EndIf
		Else
			//-----------------------------------+
			// Desarma Transacao em caso de erro |
			//-----------------------------------+
			RollBackSx8()
			LogExec("ERRO AO GERAR PRE EMPENHO " + _cCodRes + " .")	

			aRet[1]	:= .F.
			aRet[2]	:= cOrderId
			aRet[3]	:= "ERRO AO GERAR PRE EMPENHO " + _cCodRes + " ."

		EndIf

	EndIf
Next nX
	
RestArea(_aArea)
Return aRet

/***********************************************************************************************************/
/*/{Protheus.doc} AEcoGrvCab
	@description Grava Cabeçalho do Pedido e-Commerce
	@author Bernard M. Margarido
	@since 01/02/2017
	@version undefined
	@type function
/*/
/***********************************************************************************************************/
Static Function AEcoGrvCab(	cNumOrc,cOrderId,cCodCli,cLojaCli,cTipoCli,cVendedor,cEndDest,cNumDest,;
							cMunDest,cBaiDest,cCepDest,cEstDest,cNomDest,cDddCel,cDdd1,cTel01,cCelular,;
							cIdEnd,cMotCancel,cPedCodCli,cPedCodInt,cHoraEmis,dDtaEmiss,cPedStatus,nVlrFrete,;
							nVrSubTot,nVlrTotal,nQtdParc,nDesconto,nPesoBruto,cIdPost,cEndComp,cEndRef,;
							cCodTransp,_cIdServ,cCodAfili,nJuros,_cLojaID,_cDescLoja)

	Local aArea			:= GetArea()
	Local aRet			:= {.T.,"",""}
	
	Local lGrava		:= .T.	

	Local cEspecie		:= GetNewPar("EC_ESPECIE","EMBALAGEM")
	Local cTpFrete		:= ""
	Local cCondPag		:= GetNewPar("EC_CONDPAG","001")
	Local nDiasOrc		:= GetNewPar("EC_DIASORC",15)
		
	//------------------------------+
	// Grava Cabeçalho do Orçamento |
	//------------------------------+
	dbSelectArea("XTA")
	XTA->( dbSetOrder(1) )
	If XTA->( dbSeek(xFilial("XTA") + cNumOrc ) )
		lGrava := .F.
	EndIf							

	//---------------------------+
	// Posiciona Status do Pedido|
	//---------------------------+
	dbSelectArea("ZTC")
	ZTC->( dbSetOrder(3) )
	If !ZTC->( dbSeek(xFilial("ZTC") + Padr(Alltrim(cPedStatus),nTamStat)) )
		LogExec("STATUS " + Capital(cPedStatus) + " NAO LOCALIZADO PARA O PEDIDO ORDERID " + cOrderId + " FAVOR CADASTRAR O STATUS E IMPORTAR O PEDIDO NOVAMENTE.")
		aRet[1] := .F.
		aRet[2] := cOrderId
		aRet[3] := "STATUS " + Capital(cPedStatus) + " NAO LOCALIZADO PARA O PEDIDO ORDERID " + cOrderId + " FAVOR CADASTRAR O STATUS E IMPORTAR O PEDIDO NOVAMENTE."
		RestArea(aArea)
		Return aRet
	EndIf
	
	//----------------------+
	// Valida Tipo de Frete |
	//----------------------+
	If SubStr(Alltrim(cOrderId),1,3) == "MRC"
		cTpFrete := "0"
	ElseIf !Empty(cIdPost)
		If nVlrFrete > 0
			cTpFrete := "F"
		Else
			cTpFrete := "C"
		EndIf
	ElseIf Empty(cIdPost) .And. !Empty(cCodTransp)
		If nVlrFrete > 0
			cTpFrete := "F"
		Else
			cTpFrete := "C"
		EndIf
	ElseIf Empty(cIdPost) .And. Empty(cCodTransp)
		If nVlrFrete > 0
			cTpFrete := "F"
		Else
			cTpFrete := "C"
		EndIf	
	EndIf
	
	//------------------------------+
	// Atualiza endereço de entrega |
	//------------------------------+
	/*
	aEcoI011Entr(	cCodCli,cLojaCli,cNomDest,;
					cEndDest,cNumDest,cBaiDest,;
					cCepDest,cMunDest,cEstDest,cEndComp )
	*/
	RecLock("XTA",lGrava)
		XTA->XTA_FILIAL		:= xFilial("XTA")
		XTA->XTA_NUM		:= cNumOrc
		XTA->XTA_VEND		:= cVendedor
		XTA->XTA_COMIS		:= 0
		XTA->XTA_CLIENT		:= cCodCli
		XTA->XTA_LOJA		:= cLojaCli
		XTA->XTA_TIPOCL		:= cTipoCli	
		XTA->XTA_VLRTOT		:= MaFisRet(,"NF_VALMERC")
		XTA->XTA_DESCON		:= nDesconto
		XTA->XTA_VLRLIQ		:= MaFisRet(,"NF_VALMERC")
		XTA->XTA_DTLIM		:= DaySum(cTod(dDtaEmiss),nDiasOrc)
		XTA->XTA_VALBRU		:= MaFisRet(,"NF_VALMERC") //MaFisRet(,"NF_TOTAL") 
		XTA->XTA_VALMER		:= MaFisRet(,"NF_VALMERC") //MaFisRet(,"NF_VALMERC")
		XTA->XTA_DESCNF		:= 0
		XTA->XTA_DINHEI		:= 0
		XTA->XTA_CHEQUE		:= 0
		XTA->XTA_CARTAO		:= 0
		XTA->XTA_CONVEN		:= 0
		XTA->XTA_VALES		:= 0
		XTA->XTA_FINANC		:= 0
		XTA->XTA_OUTROS		:= 0
		XTA->XTA_PARCEL		:= nQtdParc 	
		XTA->XTA_VALICM		:= MaFisRet(,"NF_VALICM")
		XTA->XTA_VALIPI		:= MaFisRet(,"NF_VALIPI")
		XTA->XTA_VALISS		:= MaFisRet(,"NF_VALISS")
		XTA->XTA_CONDPG		:= cCondPag
		XTA->XTA_FORMPG		:= ""
		XTA->XTA_CREDIT		:= 0
		XTA->XTA_EMISSA		:= cTod(dDtaEmiss)
		XTA->XTA_FATOR		:= 0
		XTA->XTA_AUTORI		:= ""
		XTA->XTA_NSUTEF		:= ""
		XTA->XTA_VLRDEB		:= 0
		XTA->XTA_HORA		:= SubStr(StrTran(cHoraEmis,":",""),1,4)
		XTA->XTA_TXMOED		:= 0
		XTA->XTA_ENDENT		:= Upper(Alltrim(cEndDest)) + " ," + cNumDest
		XTA->XTA_ENDNUM		:= cNumDest
		XTA->XTA_TPFRET		:= cTpFrete
		XTA->XTA_BAIRRE		:= Upper(Alltrim(cBaiDest))
		XTA->XTA_CEPE		:= cCepDest
		XTA->XTA_MUNE		:= Upper(Alltrim(cMunDest))	
		XTA->XTA_ESTE		:= Upper(cEstDest)	
		XTA->XTA_FRETE		:= nVlrFrete
		XTA->XTA_SEGURO		:= 0
		XTA->XTA_DESPES		:= 0
		XTA->XTA_PLIQUI		:= nPesoBruto
		XTA->XTA_PBRUTO		:= nPesoBruto
		XTA->XTA_VOLUME		:= 1 
		XTA->XTA_TRANSP		:= cCodTransp
		XTA->XTA_ESPECI		:= cEspecie
		XTA->XTA_MOEDA		:= 0
		XTA->XTA_BRICMS		:= MaFisRet(,"NF_BASESOL" ) 
		XTA->XTA_ICMSRE		:= MaFisRet(,"NF_VALSOL" )
		XTA->XTA_ABTOPC		:= 0
		XTA->XTA_VALPIS		:= MaFisRet(,'NF_VALPIS')
		XTA->XTA_VALCOF		:= MaFisRet(,'NF_VALCOF')
		XTA->XTA_VALCSL		:= MaFisRet(,'NF_VALCSL')
		XTA->XTA_CGCCLI		:= ""
		XTA->XTA_VALIRR		:= 0
		XTA->XTA_NUMECO		:= cOrderId
		XTA->XTA_NUMECL		:= cPedCodCli
		XTA->XTA_OBSECO		:= cObsPedido
		XTA->XTA_MTCANC		:= cMotCancel
		XTA->XTA_CODSTA		:= ZTC->ZTC_ORDEM
		XTA->XTA_DESTAT		:= RTrim(ZTC->ZTC_DESCV3)
		XTA->XTA_TRACKI		:= ""
		XTA->XTA_VLBXPV		:= "1" 
		XTA->XTA_NOMDES		:= cNomDest
		XTA->XTA_DDDCEL		:= cDddCel
		XTA->XTA_DDD01 		:= cDdd1
		XTA->XTA_CELULA		:= cCelular
		XTA->XTA_TEL01 		:= cTel01
		XTA->XTA_COMPLE		:= cEndComp
		XTA->XTA_REFEN		:= cEndRef 
		XTA->XTA_IDENDE		:= cIdEnd
		XTA->XTA_SERPOS		:= _cIdServ
		XTA->XTA_ENVLOG 	:= "1"
		If XTA->( FieldPos("XTA_IDLOJA") > 0 )
			XTA->XTA_IDLOJA		:= _cLojaID
			XTA->XTA_DESLOJ		:= _cDescLoja
		EndIf 
	XTA->( MsUnLock() )	
			
	RestArea(aArea)
Return aRet

/**************************************************************************************************/
/*/{Protheus.doc} AEcoGrvFin
	@description	Grava os titulos financeiro
	@author			Bernard M.Margarido
	@version   		1.00
	@since     		10/02/2016
/*/			
/**************************************************************************************************/
Static Function AEcoGrvFin(oPayment,oRestPv,cNumOrc,cOrderId,cPedCodCli,cHoraEmis,cCodAfili,dDtaEmiss,nVlrTotMkt,nVlrFrete,nJuros,nTotal)
	Local aArea		:= GetArea()
	Local aRet		:= {.T.,"",""}
	Local aVencTo	:= {}
	
	Local cTipo		:= ""
	Local cAdmCart	:= ""
	Local cParcela 	:= ""
	Local cCondPg	:= ""
	//Local cOperTX	:= GetNewPar("EC_OPERSTX","000")
	Local c1DUP     := SuperGetMv("MV_1DUP")	
	Local cOpera	:= ""	
	Local cCodAuto	:= ""
	Local cNsuId	:= ""
	Local cNumCart	:= ""
	Local cSemNsu	:= ""
	Local cTID		:= ""
	Local cCodAdm	:= ""
	Local cFormPG	:= ""
	Local cPayID	:= ""
	Local cPayName	:= ""
	Local cPrefixo	:= GetNewPar("EC_PREFIXO","ECO")
		
	Local nVlrParc	:= 0
	Local nTxParc	:= 0
	Local nTran		:= 0
	Local nPay		:= 0
	Local nParc		:= 0
	Local nQtdParc	:= 0
	Local nVlrTotal	:= 0
	Local nVlrRefe	:= 0

	Local dDtaVencto:= cTod('  /  /    ')	
	
	Local lTaxaCC	:= GetNewPar("EC_ADMFIN",.F.)
	//Local lUsaSAE	:= GetNewPar("EC_SAEFIN",.F.)
	//Local lGrava 	:= .T.
	
	Local oDadCart	:= Nil
	
	//-----------------------+
	// Operadoras e-Commerce |
	//-----------------------+
	dbSelectArea("XTH")
	XTH->( dbSetOrder(1) )

	For nTran := 1 To Len(oPayMent:Transactions)
		
		If oPayMent:Transactions[nTran]:IsActive
			
			If ValType(oPayMent:Transactions[nTran]:transactionId) == "U"
				Loop
			EndIf 

			For nPay := 1 To Len(oPayMent:Transactions[nTran]:Payments)
				
				//If ValType(oPayMent:Transactions[nTran]:Payments[nPay]:id) <> "U"
					If Len(oPayMent:Transactions[nTran]:Payments) > 1
						cPrefixo := "EC" + Alltrim(Str(nPay))
					EndIf	
					
					//---------------------+
					// Codigo da Operadora |
					//---------------------+
					cTipo 		:= ""
					cFormPG 	:= ""
					cSemNsu 	:= ""
					cCodAuto	:= ""
					cNsuId		:= ""
					cTID		:= ""
					cNumCart	:= ""

					cPayID 		:= oPayMent:Transactions[nTran]:Payments[nPay]:id
					cPayName 	:= oPayMent:Transactions[nTran]:Payments[nPay]:PaymentSystemName
					cPaySys 	:= oPayMent:Transactions[nTran]:Payments[nPay]:PayMentSystem
					cOpera 		:= PadL(oPayMent:Transactions[nTran]:Payments[nPay]:PayMentSystem,nTamOper,"0")
					nQtdParc	:= oPayMent:Transactions[nTran]:Payments[nPay]:InstallMents	
					nVlrTotal	:= RetPrcUni(oPayMent:Transactions[nTran]:Payments[nPay]:Value)
					nVlrRefe	:= RetPrcUni(oPayMent:Transactions[nTran]:Payments[nPay]:referenceValue)
					nJuros		:= nVlrTotal - nVlrRefe

					//-----------------------------------------+
					// Valida se frete é maior que valor total | 
					//-----------------------------------------+
					/*
					If cCodAfili $ "BWW" .And. At("Lojas_Americanas",cOrderId) > 0
						If nVlrFrete > nTotal
							nVlrTotal := nVlrTotal + nVlrFrete	
						EndIf 
					EndIf 
					*/
					
					//-------------------------------------------+
					// Posiciona Operadora de Pagamento eCommerce|
					//-------------------------------------------+
					If !XTH->( dbSeek(xFilial("XTH") + cOpera ) )
						aRet[1] := .F.
						aRet[2] := cOpera
						aRet[3] := "NAO FOI ENCONTRADA A OPERADORA CADASTRADA NO PROTHEUS. FAVOR CADASTRAR A OPERADORA."
						LogExec("NAO FOI ENCONTRADA A OPERADORA CADASTRADA NO PROTHEUS. FAVOR CADASTRAR A OPERADORA.")
						RestArea(aArea)
						Return aRet
					EndIf
					
					//------------+
					// C. Credito |
					//------------+
					If XTH->XTH_FORMA == "1"
						cTipo 		:= "CC"
						cFormPG 	:= "CC"
						oDadCart	:= oPayMent:Transactions[nTran]:Payments[nPay]
						cSemNsu 	:= FWJsonSerialize(oPayMent:Transactions[nTran]:Payments[nPay]:ConnectorResponses)
						cCodAuto	:= IIF(AT("AUTHID",cSemNsu) > 0,oPayMent:Transactions[nTran]:Payments[nPay]:ConnectorResponses:Authid,"")
						cNsuId		:= IIF(AT("NSU",cSemNsu) > 0,oPayMent:Transactions[nTran]:Payments[nPay]:ConnectorResponses:Nsu,"")
						cTID		:= IIF(AT("TID",cSemNsu) > 0,oPayMent:Transactions[nTran]:Payments[nPay]:ConnectorResponses:Tid,"")
						cNumCart	:= Alltrim(oPayMent:Transactions[nTran]:Payments[nPay]:FirstDigits) + "XXXXXX" + Alltrim(oPayMent:Transactions[nTran]:Payments[nPay]:LastDigits) 
					//--------+	
					// Boleto |
					//--------+
					ElseIf XTH->XTH_FORMA == "3"
						cTipo 		:= "BOL"
						cFormPG 	:= "BO"
						cSemNsu 	:= FWJsonSerialize(oPayMent:Transactions[nTran]:Payments[nPay]:ConnectorResponses)
						cCodAuto	:= IIF(AT("AUTHID",cSemNsu) > 0,oPayMent:Transactions[nTran]:Payments[nPay]:ConnectorResponses:Authid,"")
						cNsuId		:= IIF(AT("NSU",cSemNsu) > 0,oPayMent:Transactions[nTran]:Payments[nPay]:ConnectorResponses:Nsu,"")
						cTID		:= IIF(AT("TID",cSemNsu) > 0,oPayMent:Transactions[nTran]:Payments[nPay]:ConnectorResponses:Tid,"")
					//--------+	
					// Debito | 	
					//--------+
					ElseIf XTH->XTH_FORMA == "2"
						cTipo 		:= "CD"
						cFormPG 	:= "CD"
						oDadCart	:= oPayMent:Transactions[nTran]:Payments[nPay]
						cSemNsu 	:= FWJsonSerialize(oPayMent:Transactions[nTran]:Payments[nPay]:ConnectorResponses)
						cCodAuto	:= IIF(AT("AUTHID",cSemNsu) > 0,oPayMent:Transactions[nTran]:Payments[nPay]:ConnectorResponses:Authid,"")
						cNsuId		:= IIF(AT("NSU",cSemNsu) > 0,oPayMent:Transactions[nTran]:Payments[nPay]:ConnectorResponses:Nsu,"")
						cTID		:= IIF(AT("TID",cSemNsu) > 0,oPayMent:Transactions[nTran]:Payments[nPay]:ConnectorResponses:Tid,"")
						cNumCart	:= Alltrim(oPayMent:Transactions[nTran]:Payments[nPay]:FirstDigits) + "XXXXXX" + Alltrim(oPayMent:Transactions[nTran]:Payments[nPay]:LastDigits)
					//--------------+	
					// Market Place |
					//--------------+ 
					ElseIf XTH->XTH_FORMA == "5"
						cTipo 		:= "MKT"
						cFormPG 	:= "BO"
						If nVlrTotal < nVlrTotMkt
							nVlrTotal	:= nVlrTotMkt
						EndIf
					
					//-----+	
					// PIX |
					//-----+ 
					ElseIf XTH->XTH_FORMA == "4"
						cTipo 		:= "PIX"
						cFormPG		:= "PI"
						cSemNsu 	:= FWJsonSerialize(oPayMent:Transactions[nTran]:Payments[nPay]:ConnectorResponses)
						cCodAuto	:= IIF(AT("AUTHID",cSemNsu) > 0,oPayMent:Transactions[nTran]:Payments[nPay]:ConnectorResponses:Authid,"")
						cNsuId		:= IIF(AT("NSU",cSemNsu) > 0,oPayMent:Transactions[nTran]:Payments[nPay]:ConnectorResponses:Nsu,"")
						cTID		:= IIF(AT("TID",cSemNsu) > 0,oPayMent:Transactions[nTran]:Payments[nPay]:ConnectorResponses:Tid,"")

					EndIf
				
					//----------+
					// Grava SE4|
					//----------+
					aRet := AEcoSe4(cOrderId,cTipo,nQtdParc)
				
					If !aRet[1]
						RestArea(aArea)
						Return aRet
					EndIf
					
					//---------------------------------+
					// Codigo da Condição de Pagamento |
					//---------------------------------+
					cCondPg := aRet[2]
									
					//-----------------------------+
					// Descrição da Bandeira usada |
					//-----------------------------+
					cAdmCart := Capital(XTH->XTH_ADQUI)
						
					//----------+
					// Grava SL4|
					//----------+
					aVencTo	:= Condicao(nVlrTotal,cCondPg,,cTod(dDtaEmiss))
					
					//----------------------------------------------+
					// Valida se utiliza Administradora ficnanceira |
					//----------------------------------------------+
					If lTaxaCC .And. XTH->XTH_FORMA $ "1/2/5"
						aRet := aEcoTxAdm(XTH->XTH_COD,Len(aVencTo))
						If aRet[1]
							nTxParc := aRet[2]
							cCodAdm	:= aRet[4]
						Else
							aRet[1] := .F.
							aRet[2] := cOpera
							aRet[3] := "NAO FOI ENCONTRADA TAXA ADMINISTRATIVA PARA A OPERADORA " + cOpera + " - " + cAdmCart + " ."
							LogExec("NAO FOI ENCONTRADA TAXA ADMINISTRATIVA PARA A OPERADORA " + cOpera + " - " + cAdmCart + " .")
							RestArea(aArea)
							Return aRet
						EndIf
					EndIf
					
					dbSelectArea("XTC")
					XTC->( dbSetOrder(1) )
				
					For nParc := 1 To Len(aVencTo)
												
						If XTH->XTH_FORMA == "1"
							cParcela := LJParcela( nParc, c1DUP )
						EndIf
						
						//----------------------------------------------+
						// Valida se utiliza Administradora ficnanceira |
						//----------------------------------------------+
						If lTaxaCC .And. XTH->XTH_FORMA $ "1/2/5"
							nVlrParc 	:= Round( aVencTo[nParc][2] - ( aVencTo[nParc][2] *  nTxParc / 100 ) , 2 ) 
							dDtaVencto	:= aVencTo[nParc][1]
						Else
							nVlrParc 	:= aVencTo[nParc][2]
							dDtaVencto	:= aVencTo[nParc][1]
						EndIf
						
						RecLock("XTC",.T.)				
							XTC->XTC_FILIAL	:= xFilial("XTC")
							XTC->XTC_NUM    	:= cNumOrc
							XTC->XTC_NUMECO		:= cOrderId
							XTC->XTC_NUMECL		:= cPedCodCli
							XTC->XTC_DATA   	:= dDtaVencto
							XTC->XTC_VALOR  	:= nVlrParc		
							XTC->XTC_FORMA  	:= cTipo
							XTC->XTC_FORMPG		:= cFormPG
							XTC->XTC_ADMINI		:= IIF(Empty(cCodAdm),cOpera,cCodAdm)
							XTC->XTC_NUMCAR		:= cNumCart
							XTC->XTC_OBS    	:= cPayName
							XTC->XTC_DATATE		:= dTos(cTod(dDtaEmiss))
							XTC->XTC_HORATE		:= StrTran(cHoraEmis,":","")
							XTC->XTC_DOCTEF 	:= cCodAuto
							XTC->XTC_AUTORI		:= cCodAuto
							XTC->XTC_NSUTEF 	:= cNsuId
							XTC->XTC_MOEDA  	:= 1
							XTC->XTC_PARCTE		:= cPrefixo  
							XTC->XTC_ITEM   	:= cParcela    
							XTC->XTC_TID		:= cTID
							XTC->XTC_PAYID 		:= cPayID
							XTC->XTC_AUTHID		:= cCodAuto
							XTC->XTC_NSU   		:= cNsuId
							XTC->XTC_PAYSYS		:= cPaySys
						XTC->( MsunLock() )
																
					Next nParc
				//EndIf 
			Next nPay
		EndIf	
	Next nTran

	//---------------------+
	// Valida se tem juros |
	//---------------------+
	If nJuros > 0 .And. cCodAfili $ "MGZ"
		dbSelectArea("XTA")
		XTA->( dbSetOrder(2) )
		If XTA->( dbSeek(xFilial("XTA") + cOrderId) )
			RecLock("XTA",.F.)
				XTA->XTA_DESPES := nJuros
			XTA->( MsUnLock() )
		EndIf 
	EndIf 

	//----------+
	// Grava SE1|
	//----------+
	//aRet := AEcoGrvSe1(cNumOrc,cOrderId,cPedCodCli,cOpera,cPrefixo,cNsuId,dDtaEmiss,Len(aVencTo))
	
	
	RestArea(aArea)
Return aRet

/**************************************************************************************************/
/*/{Protheus.doc} AEcoSe4
	@description	Gera condição de pagamento
	@author			Bernard M.Margarido
	@version   		1.00
	@since     		10/02/2016
/*/			
/**************************************************************************************************/
Static Function AEcoSe4(cOrderId,cTipo,nQtdParc)
	Local aArea		:= GetArea()
	Local aRet		:= {.T.,"",""}
	Local aCond		:= {}

	Local cCodigo	:= ""
	Local cDescri	:= ""
	Local cCondPg	:= ""
	Local cTpCond 	:= "1"

	Private lMsErroAuto := .F.

	//--------------------------+
	// Valida tipo de pagamento |
	//--------------------------+
	If cTipo == "CC"

		cCodigo := "C" + PadL(Alltrim(Str(nQtdParc)),2,"0")	
		cDescri	:= PadL(Alltrim(Str(nQtdParc)),2,"0") + "X. C.CREDITO"
		If nQtdParc > 1
			cCondPg	:= "30," + Alltrim(Str(nQtdParc)) + ",30"
			cTpCond := "5"
		Else
			cCondPg	:= "30"
			cTpCond := "1"
		EndIf

	ElseIf cTipo == "BOL"

		cCodigo := "BOL"	
		cDescri	:= "BOLETO"
		cCondPg	:= GetNewPar("EC_BOLVENC","7")
		cTpCond := "1"
	
	ElseIf cTipo == "CD"

		cCodigo := "CD1"	
		cDescri	:= "CARTAO DEBITO"
		cCondPg	:= "01"
		cTpCond := "1"
	
	ElseIf cTipo == "MKT"

		cCodigo := "MKT"	
		cDescri	:= "MARKET PLACE"
		cCondPg	:= GetNewPar("EC_MKTVENC","30")
		cTpCond := "1"
	
	ElseIf cTipo == "PIX"

		cCodigo := "PIX"	
		cDescri	:= "PIX"
		cCondPg	:= GetNewPar("EC_PIXVENC","01")
		cTpCond := "1"

	EndIf	

	//-----------------------------------------------------+
	// Se já existe condição de pagamento retorna o codigo |
	//-----------------------------------------------------+
	dbSelectArea("SE4")
	SE4->( dbSetOrder(1) )
	If SE4->( dbSeek(xFilial("SE4") + PadR(cCodigo,TamSx3("E4_CODIGO")[1]) ) )
		aRet[1] := .T.
		aRet[2] := SE4->E4_CODIGO
		aRet[3] := ""
		RestArea(aArea)
		Return aRet
	EndIf

	aAdd(aCond,{"E4_FILIAL"	,	xFilial("SE4")							,	Nil })
	aAdd(aCond,{"E4_CODIGO"	,	cCodigo									,	Nil })
	aAdd(aCond,{"E4_DESCRI"	,	cDescri									,	Nil })
	aAdd(aCond,{"E4_TIPO"  	,	cTpCond									,	Nil })
	aAdd(aCond,{"E4_COND"	,	cCondPg									,	Nil })

	//-------------------------------------------------------+
	// Insere condição de pagamento especifica do e-Commerce.|
	//-------------------------------------------------------+
	lMsErroAuto := .F.
	MsExecAuto({|x,y,z| Mata360(x,y,z)},aCond,{},3)

	If lMsErroAuto

		cSE4Log	:= "SE4" + cCodigo + DToS(dDataBase)+Left(Time(),2)+SubStr(Time(),4,2)+Right(Time(),2)+".LOG"
		//----------------+
		// Cria diretorio |
		//----------------+ 
		MakeDir("/erros/")
		MostraErro("/erros/",cSE4Log)

		//-------------------------------------------------+
		//³Adiciona Arquivo de log no Retorno da resposta. |
		//-------------------------------------------------+
		cMsgErro := ""
		cLiArq	 := ""
		nHndImp	 := 0	
		nHndImp  := FT_FUSE("/ecommerce/" + cSE4Log)
		If nHndImp >= 1
			//----------------------------+
			// Posiciona Inicio do Arquivo|
			//----------------------------+
			FT_FGOTOP()

			While !FT_FEOF()
				cLiArq := FT_FREADLN()
				If Empty(cLiArq)
					FT_FSKIP(1)
					Loop
				EndIf
				cMsgErro += cLiArq + CRLF
				FT_FSKIP(1)
			EndDo
			FT_FUSE()
		EndIf  

		//--------------------+
		// Variavel de retorno|
		//--------------------+
		aRet[1] := .F.
		aRet[2]	:= cOrderId
		aRet[3] := cMsgErro 

	Else
		LogExec("PAGAMENTO " + cCodigo + "-" + cDescri + " INSERIDO COM SUCESSO" )
		aRet[1] := .T.
		aRet[2] := cCodigo
		aRet[3]	:= ""
	Endif	

	RestArea(aArea)
Return aRet

/*************************************************************************************
{Protheus.doc} aEcoTxAdm

@description  Retorna taxa administrativa 

@author Bernard M. Margarido
@since 23/08/2016
@version undefined

@param cOpera		, Codigo da Operadora e-Comemrce
@param nParcela		, Numero de parcelas

@type function
*************************************************************************************/
Static Function aEcoTxAdm(cOpera,nParcela,nTipo)
Local aArea		:= GetArea()
Local aRet		:= {.T.,"","",""}

Local nTaxa		:= 0

Local cCodCli	:= ""
Local cCodSAE	:= ""
Local cAlias	:= GetNextAlias()

Local cQuery 	:= ""

Default nTipo	:= 1

cQuery := "	SELECT " + CRLF
cQuery += "		AE.AE_COD, " + CRLF
cQuery += "		AE.AE_TAXA, " + CRLF
cQuery += "		AE.AE_PARCDE, " + CRLF
cQuery += "		AE.AE_PARCATE, " + CRLF
cQuery += "		AE.AE_CODCLI " + CRLF
cQuery += "	FROM " + CRLF 
cQuery += "		" + RetSqlName("SAE") + " AE " + CRLF
cQuery += "	WHERE " + CRLF
cQuery += "		AE.AE_FILIAL = '" + xFilial("SAE")  + "' AND " + CRLF 
cQuery += "		AE.AE_COD = '" + cOpera + "' AND " + CRLF 
cQuery += "		AE.D_E_L_E_T_ = '' "

dbUseArea(.T.,"TOPCONN",TcGenQry(,,cQuery),cAlias,.T.,.T.)

If (cAlias)->( Eof() )
	aRet[1] := .F.
	aRet[2] := ""
	aRet[3] := ""
	aRet[4] := ""
	(cAlias)->( dbCloseArea() ) 
	RestArea(aArea)
	Return aRet
EndIf

While (cAlias)->( !Eof() )
	If nTipo == 1
		If nParcela >= (cAlias)->AE_PARCDE .And. nParcela <= (cAlias)->AE_PARCATE  
			nTaxa 	:= (cAlias)->AE_TAXA
			cCodSAE	:= (cAlias)->AE_COD
			Exit
		EndIf
	Else
		If nParcela >= (cAlias)->AE_PARCDE .And. nParcela <= (cAlias)->AE_PARCATE  
			cCodCli := (cAlias)->AE_CODCLI
			cCodSAE	:= (cAlias)->AE_COD
			Exit
		EndIf
	EndIf	
	(cAlias)->( dbSkip() )
EndDo
	

aRet[1] := .T.
aRet[2] := IIF(nTipo == 1,nTaxa,cCodCli)
aRet[3] := IIF(nTipo == 1,"","01")
aRet[4]	:= cCodSAE
(cAlias)->( dbCloseArea() ) 

RestArea(aArea)
Return aRet

/**************************************************************************************************/
/*/{Protheus.doc} AEcoGrvSe1
	@description	Gera contas a receber
	@author			Bernard M.Margarido
	@version   		1.00
	@since     		10/02/2016
/*/			
/**************************************************************************************************/
Static Function AEcoGrvSe1(cNumOrc,cOrderId,cPedCodCli,cOpera,cPrefixo,cNsuId,dDtaEmiss,nParcTx)
	Local aArea 	:= GetArea()
	Local aRet		:= {.T.,"",""}
	Local aSe1 		:= {}

	Local cMsgErro 	:= ""
	Local cLiArq	:= ""
	Local cCliOper	:= ""
	Local cLojaOper	:= ""
			
	Local nHndImp  	:= 0
	Local nOpcA		:= 0
	
	Local lTaxaCC	:= GetNewPar("EC_ADMFIN",.F.)
	
	Private lMsErroAuto	:= .F.

	//----------------------+
	// Posiciona Orçamento  |
	//----------------------+
	dbSelectArea("XTA")
	XTA->( dbSetOrder(1) )
	XTA->( dbSeek(xFilial("XTA") + cNumOrc) )

	//-------------------------------+
	// Posiciona Condição Negociada  |
	//-------------------------------+
	dbSelectArea("XTC")
	XTC->( dbSetOrder(1) )
	If !XTC->( dbSeek(xFilial("XTC") + cNumOrc) )
		aRet[1] := .F.
		aRet[2] := cOrderId
		aRet[3] := "PAGAMENTO NAO ENCONTRADO PARA O PEDIDO ORDERID " + Alltrim(cOrderId) + ". FAVOR INFORMAR O ADMINISTRADOR DO SISTEMA."	
		RestArea(aArea)
		Return aRet
	EndIf
	

	//------------------------+
	// Cria Contas a Receber  |
	//------------------------+
	While XTC->( !Eof() .And. xFilial("XTC") + cNumOrc == XTC->( XTC_FILIAL + XTC_NUM) )

		aSe1 := {}
		
		//----------------------------------------+
		// Caso pagamento tenha mais de um cartão | 
		//----------------------------------------+
		If Alltrim(XTC->XTC_FORMA) == "CC" .And. Alltrim(cNsuId) <> Alltrim(XTC->XTC_NSUTEF)
		 	XTC->( dbSkip() )
			Loop 
		EndIf
		
		cNumE1	:= PadR(XTC->XTC_NUMECL,nTamTitu)
		cParce	:= PadR(XTC->XTC_ITEM,nTamParc)
		cPrefixo:= XTC->XTC_PARCTEF
		nOpcA 	:= 3
		
		//----------------------------+
		// Valida se já existe Titulo |
		//----------------------------+		
		If SE1->( dbSeek(xFilial("SE1") + cPrefixo + cNumE1 + cParce ) )
			nOpcA := 4
			XTC->( dbSkip() )
			Loop 
		EndIf
			
		If Alltrim(XTC->XTC_FORMA) == "CC"
			cNatureza := &(SuperGetMV("MV_NATCART"))
			If lTaxaCC
				aRet := aEcoTxAdm(cOpera,nParcTx,2)
				If aRet[1]
					cCliOper	:= aRet[2]
					cLojaOper	:= aRet[3]
				EndIf	
			EndIf
			cCliente  := XTA->XTA_CLIENTE
			cLoja	  := XTA->XTA_LOJA		
		ElseIf Alltrim(XTC->XTC_FORMA) == "CD"
			cNatureza := &(SuperGetMV("MV_NATTEF"))
			cCliente  := XTA->XTA_CLIENTE
			cLoja	  := XTA->XTA_LOJA
		ElseIf Alltrim(XTC->XTC_FORMA) == "BOL"
			cCliente  := XTA->XTA_CLIENTE
			cLoja	  := XTA->XTA_LOJA	
			cNatureza := &(SuperGetMV("MV_NATOUTR"))
			
		ElseIf Alltrim(XTC->XTC_FORMA) == "MKT"
			cCliente  := XTA->XTA_CLIENTE
			cLoja	  := XTA->XTA_LOJA	
			cNatureza := &(SuperGetMV("MV_NATOUTR"))
		EndIf

		If Alltrim(XTC->XTC_FORMA) $ "BOL/FI"
			cHist := "PED: " + cOrderId + " BOLETO"	
		ElseIf Alltrim(XTC->XTC_FORMA) $ "MKT"	
			cHist := "PED: " + cOrderId + " MARKET PLACE"
		Else
			cHist := "PED: " + cOrderId + " CARTAO:" + Upper(XTC->XTC_ADMINIS)
		EndIf
		
		cNatureza := IIF(ValType(cNatureza) == "N", Alltrim(Str(cNatureza)),cNatureza)
		
		aAdd(aSe1,{"E1_FILIAL"			, xFilial("SE1")						  	    					, Nil })				
		aAdd(aSe1,{"E1_PREFIXO"			, cPrefixo								  	    					, Nil })
		aAdd(aSe1,{"E1_TIPO"			, Alltrim(XTC->XTC_FORMA)					    					, Nil })
		aAdd(aSe1,{"E1_NUM"				, Alltrim(XTC->XTC_NUMECL)											, Nil })
		aAdd(aSe1,{"E1_PARCELA"			, Alltrim(XTC->XTC_ITEM)						   						, Nil })
		aAdd(aSe1,{"E1_NATUREZ"			, cNatureza															, Nil })
		aAdd(aSe1,{"E1_CLIENTE"			, IIF(Empty(cCliOper),cCliente,cCliOper)							, Nil })
		aAdd(aSe1,{"E1_LOJA"			, IIF(Empty(cLojaOper),cLoja,cLojaOper)								, Nil })
		aAdd(aSe1,{"E1_VALOR"			, XTC->XTC_VALOR														, Nil })
		aAdd(aSe1,{"E1_EMISSAO"			, cTod(dDtaEmiss)													, Nil })
		aAdd(aSe1,{"E1_VENCTO"			, XTC->XTC_DATA														, Nil })
		aAdd(aSe1,{"E1_VLCRUZ"			, XTC->XTC_VALOR														, Nil })		
		aAdd(aSe1,{"E1_VENCREA"			, DataValida(XTC->XTC_DATA,.T.)        								, Nil })
		aAdd(aSe1,{"E1_XNUMECO"			, cOrderId															, Nil })  
		aAdd(aSe1,{"E1_XNUMECL"			, cPedCodCli														, Nil })  
		aAdd(aSe1,{"E1_ORIGEM"			, "FINA040"															, Nil })  
		aAdd(aSe1,{"E1_STATUS"			, "A"																, Nil })  
		aAdd(aSe1,{"E1_FLUXO"			, "S"																, Nil })  
		aAdd(aSe1,{"E1_VLRREAL"			, XTC->XTC_VALOR														, Nil })  
		aAdd(aSe1,{"E1_DOCTEF" 			, XTC->XTC_DOCTEF													, Nil })
		aAdd(aSe1,{"E1_NSUTEF"			, XTC->XTC_NSUTEF													, Nil })
		aAdd(aSe1,{"E1_HIST"			, cHist																, Nil })
		
		MsExecAuto({|x,y| FINA040(x,y)},aSe1,nOpcA)

		If lMsErroAuto   

			cSE1Log := "SE1" + DToS(dDataBase) + Left(Time(),2) + SubStr(Time(),4,2) + Right(Time(),2) + ".LOG"

			//----------------+
			// Cria diretorio |
			//----------------+ 
			MakeDir("\erros\")
			MostraErro("\erros\",cSE1Log)

			//------------------------------------------------+
			// Adiciona Arquivo de log no Retorno da resposta |
			//------------------------------------------------+
			cMsgErro := ""
			nHndImp  := FT_FUSE("\erros\" + cSE1Log)

			If nHndImp >= 1
				//-----------------------------+
				// Posiciona Inicio do Arquivo |
				//-----------------------------+
				FT_FGOTOP()
				While !FT_FEOF()
					cLiArq := FT_FREADLN()
					If Empty(cLiArq)
						FT_FSKIP(1)
						Loop
					EndIf
					cMsgErro += cLiArq + CRLF
					FT_FSKIP(1)
				EndDo
				FClose(nHndImp)
			EndIf

			aRet[1] := .F.
			aRet[2] := Alltrim(XTC->XTC_XNUMECL)
			aRet[3] := "ERRO AO GERAR CONTAS A RECEBER " + cMsgErro

		Else
			LogExec("TITULO A RECEBER GERADO COM SUCESSO." )
			aRet[1] := .T.
			aRet[2] := ""
			aRet[3] := ""
		EndIf

		XTC->( dbSkip() )

	EndDo

	RestArea(aArea)
Return aRet

/**************************************************************************************************/
/*/{Protheus.doc} AEcoUpdPv
	@description	Realiza a atualização dos pedidos e-commerce
	@author			Bernard M.Margarido
	@version   		1.00
	@since     		10/02/2016
/*/
/**************************************************************************************************/
Static Function AEcoUpdPv(cOrderId,cOrdPvCli,cNumOrc,cNumDoc,cNumSer,cNumPv,oRestPv)
	Local aArea			:= GetArea()
	Local aRet			:= {.T.,"",""}
	
	Local cVendedor 	:= GetNewPar("EC_VENDECO")
	Local cCnpj			:= ""
	Local cPedStatus	:= ""

	Local lBaixaEco		:= .F.
	Local lEnvStatus	:= .F.
			
	Local dDtaEmiss		:= Nil
		
	//------------------+
	// Ajusta variaveis |
	//------------------+
	If oRestPv:ClientProfileData:IsCorporate
		cCnpj	:= oRestPv:ClientProfileData:CorporateDocument
	Else
		cCnpj	:= oRestPv:ClientProfileData:Document
	EndIF
	cCnpj 	:= PadR(cCnpj,nTamCnpj)
	cNomeCli:= Alltrim(oRestPv:ClientProfileData:FirstName + " " + oRestPv:ClientProfileData:LastName)	
	
	//-----------------------------------------------+
	// Valida se cliente esta cadastrado no Protheus |
	//-----------------------------------------------+
	dbSelectArea("SA1")
	SA1->( dbSetOrder(3) )
	If !SA1->( dbSeek(xFilial("SA1") + cCnpj) )
		LogExec("CLIENTE " + cNomeCli + " CNPJ/CPF " + cCnpj + " NAO ENCONTRADO. FAVOR REALIZAR A BAIXA DE CLIENTES DO ECOMMERCE.")
		aRet[1] := .F.
		aRet[2] := cCnpj
		aRet[3] := "CLIENTE " + cNomeCli + " CNPJ/CPF " + cCnpj + " NAO ENCONTRADO. FAVOR REALIZAR A BAIXA DE CLIENTES DO ECOMMERCE."
		RestArea(aArea)
		Return aRet
	EndIf
		
	//---------------------------+
	// Valida reserva do produto | 
	//---------------------------+
	dDtaEmiss	:= dToc(sTod(StrTran(SubStr(oRestPv:creationDate,1,10),"-","")))
	cCodCli		:= SA1->A1_COD
	cLojaCli	:= sA1->A1_LOJA
	cVendedor	:= ""
	nDesconto	:= 0
		
	//---------------------+
	// Posiciona Orçamento |
	//---------------------+
	dbSelectArea("XTA")	
	XTA->( dbSetOrder(1) )
	XTA->( dbSeek(xFilial("XTA") + cNumOrc) )
	
	//--------------+
	// Dados Pedido |
	//--------------+	
	cPedStatus	:= oRestPv:Status

	//---------------------------+
	// Posiciona Status do Pedido|
	//---------------------------+
	dbSelectArea("ZTC")
	ZTC->( dbSetOrder(3) )
	If !ZTC->( dbSeek(xFilial("ZTC") + Padr(cPedStatus,nTamStat)) )
		LogExec("STATUS CODIGO " + Alltrim(cPedStatus) + " NAO LOCALIZADO PARA O PEDIDO ORDERID " + cOrderId + " FAVOR CADASTRAR O STATUS E IMPORTAR O PEDIDO NOVAMENTE.")
		aRet[1] := .F.
		aRet[2] := cOrderId
		aRet[3] := "STATUS CODIGO " + Alltrim(cPedStatus) + " NAO LOCALIZADO PARA O PEDIDO ORDERID " + cOrderId + " FAVOR CADASTRAR O STATUS E IMPORTAR O PEDIDO NOVAMENTE."
		RestArea(aArea)
		Return aRet
	EndIf

	//------------------+
	// Status Cancelado |
	//------------------+
	If ZTC->ZTC_ORDEM $ "998/999" 

		//--------------------------------+
		// Envia status para o e-Commerce |
		//--------------------------------+
		lEnvStatus	:= IIF(ZTC->ZTC_INTEGR == "1",.T.,.F.)
		lBaixaEco	:= .T.
				
		RecLock("XTA",.F.)
			XTA->XTA_CODSTA	:= ZTC->ZTC_ORDEM
			XTA->XTA_DESTAT	:= RTrim(ZTC->ZTC_DESCV3)
		XTA->( MsUnLock() )	
		
	EndIf

	//------------------------+
	// Grava Status do Pedido |
	//------------------------+
	u_AEcoStaLog(ZTC->ZTC_ORDEM,cOrderId,dDataBase,Time())

	//---------------------------+
	// Atualiza status ecommerce |
	//---------------------------+
	If lEnvStatus
		aRet := u_AEcoStat(XTA->XTA_NUM)
	EndIf	

RestArea(aArea)
Return aRet

/*********************************************************************************************************/
/*/{Protheus.doc} AEcoCancPv

@description	Cancela Pedido e-commerce

@author			Bernard M.Margarido
@version   		1.00
@since     		10/02/2016

@param			cNumOrc		, Numero do Orçamento 
@param			cOrderId	, Numero OrderId e-Commerce
@param			cOrdPvCli	, Numero do pedido e-commerce cliente	
@param			cNumPv		, Numero do Pedido de Venda

@return			aRet		- Array aRet[1] - Logico aRet[2] - Codigo Erro aRet[3] - Descricao do Erro  
/*/
/***********************************************************************************************************/
User Function AEcoCancPv(cOrderId,cOrdPvCli,cNumOrc,cNumPv)
	Local aArea		:= GetArea()
	Local aRet		:= {.T.,"",""}
	
	//Local cAlias	:= GetNextAlias()
		
	//Local lPedido	:= .F.
	//Local lTitulo	:= .F.
	/*
	//----------------------------+
	// Valida se já existe pedido |
	//----------------------------+
	If !aVldCanPv(cAlias,cOrderId)
		lTitulo := .T.
	EndIf
	
	While (cAlias)->( !Eof() )
	
		//---------------------------+
		// Posiciona na filial atual |
		//---------------------------+
		cFilAnt := (cAlias)->C5_FILIAL
		
		//------------------+
		// Posiciona Pedido |
		//------------------+
		SC5->( dbGoTo((cAlias)->RECNOSC5) )
		
		//----------------------------------+
		// Valida se nao existe nota fiscal |
		//----------------------------------+
		If Empty(SC5->C5_NOTA) .And. Empty(SC5->C5_SERIE)
			//-------------------------+
			// Cancela Pedido de Venda |
			//-------------------------+
			aRet := u_aEcoExPv(SC5->C5_NUM)
			
			If aRet[1]	
				//-----------------------------------------+
				// Estorna titulos para o pedido cancelado |
				//-----------------------------------------+
				aRet := u_aEcoExCr(cOrderId,cOrdPvCli,cNumOrc,.F.)
				If !aRet[1]
					RestArea(aArea)
					Return aRet
				EndIf
			EndIf
			
		//-------------------------+	
		// Caso exista nota fiscal |
		//-------------------------+	
		Else
			aRet := u_AEcoTro(SC5->C5_NOTA,SC5->C5_SERIE,SC5->C5_CLIENTE,SC5->C5_LOJACLI,SC5->C5_ORCRES)
		EndIf
		
		(cAlias)->( dbSkip() )
		
	EndDo	
			
	//------------------------------------------+
	// Orçamento somente com o titulo a receber |
	//------------------------------------------+
	If lTitulo
		//-----------------------------------------+
		// Estorna titulos para o pedido cancelado |
		//-----------------------------------------+
		aRet := u_aEcoExCr(cOrderId,cOrdPvCli,cNumOrc,lPedido)
		If !aRet[1]
			RestArea(aArea)
			Return aRet
		EndIf
	EndIf
	
	//--------------------+
	// Encerra temporario |
	//--------------------+
	(cAlias)->( dbCloseArea())
	*/		
	RestArea(aArea)
Return aRet

/***********************************************************************************/
/*/{Protheus.doc} aVldCanPv

@description Consulta pedidos ecommerce

@author Bernard M. Margarido
@since 20/03/2017
@version undefined
@param cAlias	, characters, descricao
@type function
/*/
/***********************************************************************************/
Static Function aVldCanPv(cAlias,cOrderId)
Local cQuery := ""

cQuery := "	SELECT "
cQuery += "		C5.C5_FILIAL, "
cQuery += "		C5.R_E_C_N_O_ RECNOSC5 "
cQuery += "	FROM "	
cQuery += "		" + RetSqlName("SC5") + " C5 "
cQuery += "	WHERE "
cQuery += "		C5.C5_XNUMECO = '" + cOrderId + "' AND "
cQuery += "		C5.D_E_L_E_T_ = '' "
cQuery += "	ORDER BY C5.C5_FILIAL " 

dbUseArea(.T.,"TOPCONN",TcGenQry(,,cQuery),cAlias,.T.,.T.)

If (cAlias)->( Eof() )
	Return .F.
EndIf

Return .T.

/*********************************************************************************************************/
/*/{Protheus.doc} AEcoBxTit

@description	Realiza a baixa dos titulos do cartão 

@author			Bernard M.Margarido
@version   		1.00
@since     		10/02/2016

@return			aRet		- Array aRet[1] - Logico aRet[2] - Codigo Erro aRet[3] - Descricao do Erro  
/*/
/********************************************************************************************************/
Static Function AEcoBxTit(cNumTit)
	Local aArea	 		:= GetARea()
	Local aRet			:= {.T.,"",""}
	Local aBaixa		:= {}

	Local cPrefixo		:= GetNewPar("EC_PREFIXO")
	Local cBcoBx		:= GetNewPar("EC_CODBCO")
	Local cAgBx			:= GetNewPar("EC_AGEBCO")
	Local cContaBx		:= GetNewPar("EC_CONBCO")
	
	Local nTitulo		:= 1

	Private lMsErroAuto	:= .F.

	dbSelectArea("SA6")
	SA6->( dbSetOrder(1) )

	dbSelectArea("SE1")
	SE1->( dbSetOrder(1) )
	If SE1->( dbSeek(xFilial("SE1") + cPrefixo + Padr(cNumTit,nTamTitu)) )
		While SE1->( !Eof() .And. xFilial("SE1") + cPrefixo + Padr(cNumTit,nTamTitu) == SE1->( E1_FILIAL + E1_PREFIXO + E1_NUM) )

			If Empty(SE1->E1_BAIXA) .And. SE1->E1_SALDO > 0

				If SA6->( dbSeek(xFilial("SA6") + PadR(cBcoBx,nTamBco) + PadR(cAgBx,nTamAge) + PadR(cContaBx,nTamCon) ) )

					aBaixa := {}

					aAdd( aBaixa, {"E1_FILIAL"		, xFilial("SE1")									, Nil})
					aAdd( aBaixa, {"E1_PREFIXO"		, SE1->E1_PREFIXO									, Nil})
					aAdd( aBaixa, {"E1_NUM"			, SE1->E1_NUM										, Nil})
					aAdd( aBaixa, {"E1_PARCELA" 	, SE1->E1_PARCELA									, Nil})
					aAdd( aBaixa, {"E1_TIPO"		, SE1->E1_TIPO										, Nil})
					aAdd( aBaixa, {"E1_CLIENTE"		, SE1->E1_CLIENTE									, Nil})
					aAdd( aBaixa, {"E1_LOJA"		, SE1->E1_LOJA										, Nil})
					aAdd( aBaixa, {"AUTMOTBX"  		, "NOR"												, Nil})
					aAdd( aBaixa, {"AUTBANCO"  		, SA6->A6_COD										, Nil})
					aAdd( aBaixa, {"AUTAGENCIA"  	, SA6->A6_AGENCIA									, Nil})
					aAdd( aBaixa, {"AUTCONTA"  		, SA6->A6_NUMCON									, Nil})
					aAdd( aBaixa, {"AUTDTBAIXA"		, dDataBase											, Nil})
					aAdd( aBaixa, {"AUTHIST"   		, "BX. ECOMMERCE ORDERID " + Alltrim(SE1->E1_XNUMECO), Nil})
					aAdd( aBaixa, {"AUTDESCONT" 	, 0													, Nil})
					aAdd( aBaixa, {"AUTMULTA"	 	, 0													, Nil})
					aAdd( aBaixa, {"AUTJUROS"		, 0													, Nil})
					aAdd( aBaixa, {"AUTOUTGAS" 		, 0													, Nil})
					aAdd( aBaixa, {"AUTVLRPG"  		, 0    												, Nil})
					aAdd( aBaixa, {"AUTVLRME"  		, 0													, Nil})
					aAdd( aBaixa, {"AUTCHEQUE"  	, ""												, Nil})

					//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
					//³ Baixa do titulo.                                        ³
					//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
					lMsErroAuto := .F.

					MsExecAuto({|x,y| Fina070(x,y)}, aBaixa, 3)

					If lMsErroAuto

						cSE5Log := "SE5" + DToS(dDataBase) + Left(Time(),2) + SubStr(Time(),4,2) + Right(Time(),2) + ".LOG"

						//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
						//³Cria diretorio³
						//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ 
						MakeDir("/erros/")
						MostraErro("/erros/",cSE5Log)

						//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
						//³Adiciona Arquivo de log no Retorno da resposta.³
						//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
						cMsgErro := ""
						nHndImp  := FT_FUSE("/erros/" + cSE5Log)

						If nHndImp >= 1
							//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
							//³Posiciona Inicio do Arquivo³
							//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
							FT_FGOTOP()
							While !FT_FEOF()
								cLiArq := FT_FREADLN()
								If Empty(cLiArq)
									FT_FSKIP(1)
									Loop
								EndIf
								cMsgErro += cLiArq + CRLF
								FT_FSKIP(1)
							EndDo
							FClose(nHndImp)
						EndIf

						aRet[1] := .F.
						aRet[2] := cNumTit
						aRet[3] := cMsgErro

						If nTitulo <= 1
							Exit
						Endif	

					Else

						aRet[1] := .T.
						aRet[2] := ""
						aRet[3] := ""	

					EndIf

					nTitulo++

				EndIf

			EndIf

			SE1->( dbSkip() )

		EndDo
	EndIf

	RestArea(aArea)
Return aRet 

/**************************************************************************************************/
/*/{Protheus.doc} AEcoGrvXTM
	@description	Grava Status do Pedido eCommerce
	@author			Bernard M.Margarido
	@version   		1.00
	@since     		10/02/2016
/*/
/**************************************************************************************************/
Static Function AEcoGrvXTM(cOrderId,cPedStatus,dDtaEmiss,cHoraEmis)
	Local aArea		:= GetArea()
	Local aRet		:= {.T.,"",""}

	Local cAlias	:= GetNextAlias()
	Local cStatus	:= ""
	Local cQuery	:= ""

	//Local lGrava	:= .T.
	
	//---------------------------+
	// Posiciona Status do Pedido|
	//---------------------------+
	dbSelectArea("ZTC")
	ZTC->( dbSetOrder(3) )
	If !ZTC->( dbSeek(xFilial("ZTC") + Padr(Alltrim(cPedStatus),nTamStat)) )
		LogExec("STATUS " + Capital(cPedStatus) + " NAO LOCALIZADO PARA O PEDIDO ORDERID " + cOrderId + " FAVOR CADASTRAR O STATUS E IMPORTAR O PEDIDO NOVAMENTE.")
		aRet[1] := .F.
		aRet[2] := cOrderId
		aRet[3] := "STATUS " + Capital(cPedStatus) + " NAO LOCALIZADO PARA O PEDIDO ORDERID " + cOrderId + " FAVOR CADASTRAR O STATUS E IMPORTAR O PEDIDO NOVAMENTE."
		RestArea(aArea)
		Return aRet
	EndIf

	
	//------------------+
	// Codigo do Status |
	//------------------+
	cStatus := ZTC->ZTC_ORDEM
	
	cQuery := "	SELECT " + CRLF
	cQuery += "		XTM_CODSTA " + CRLF
	cQuery += "	FROM " + CRLF
	cQuery += "		" + RetSqlName("XTM") + " " + CRLF 
	cQuery += "	WHERE " + CRLF
	cQuery += "		XTM_FILIAL = '" + xFilial("XTM") + "' AND " + CRLF
	cQuery += "		XTM_IDECOM = '" + cOrderId + "' AND " + CRLF
	cQuery += "		D_E_L_E_T_ = '' "

	dbUseArea(.T.,"TOPCONN",TcGenQry(,,cQuery),cAlias,.T.,.T.)

	If (cAlias)->( Eof() )
		U_AEcoStaLog(cStatus,cOrderId,dDtaEmiss,cHoraEmis)
		(cAlias)->( dbCloseArea() )
	Else
		 (cAlias)->( dbCloseArea() )
	EndIf
	
	RestArea(aArea)
Return aRet

/**************************************************************************************************/
/*/{Protheus.doc} RetPrcUni
	@description	Converte para decimal
	@author			Bernard M.Margarido
	@version   		1.00
	@since     		10/02/2016
/*/
/**************************************************************************************************/
Static Function RetPrcUni(nVlrUnit)
Local nValor	:= 0

nValor := nVlrUnit / 100

Return nValor

/*******************************************************************************/
/*/{Protheus.doc} AEcoI11IP
	@description Retorna ID do serviço de Postagem
	@author Bernard M. Margarido
	@since 09/02/2017
	@version undefined
	@param cIdTran	, characters	, ID da Transportadora
	@type function
/*/
/*******************************************************************************/
Static Function AEcoI11IP(cIdTran,cCodAfili,cCodCli,cLojaCli,cCodTransp,cIdPost,_cIdServ)

If Empty(cIdPost)
	AEcoI11TR(cIdTran,@cCodTransp,@_cIdServ)
EndIf	

If Empty(cIdPost) .Or. Empty(cCodTransp)
	AEcoI11TRC(cCodCli,cLojaCli,@cCodTransp)
EndIf 

Return .T. 

/***************************************************************/
/*/{Protheus.doc} AEcoI11TR
	@description Valida se Transportadora Propria
	@author Bernard M. Margarido
	@since 08/03/2017
	@version undefined
	@type function
/*/
/***************************************************************/
Static Function AEcoI11TR(cIdTran,cCodTransp,_cIdServ)
Local cAlias	:= ""
Local cQuery 	:= ""
Local cIdPost	:= ""

cQuery := " SELECT " + CRLF  
cQuery += "		COD_TRANSP, " + CRLF  
cQuery += "		XTG_IDVTEX, " + CRLF  
cQuery += "		XTG_STATUS " + CRLF  
cQuery += " FROM " + CRLF  
cQuery += " ( " + CRLF  
cQuery += "		SELECT " + CRLF  
cQuery += "			A4.A4_COD COD_TRANSP, " + CRLF
cQuery += "			XTG.XTG_IDVTEX, " + CRLF
cQuery += "			XTG.XTG_STATUS " + CRLF
cQuery += "		FROM " + CRLF
cQuery += "			" + RetSqlName("XTG") + " XTG " + CRLF  
cQuery += "			INNER JOIN " + RetSqlName("SA4") + " A4 ON A4.A4_FILIAL = '" + xFilial("SA4") + "' AND A4.A4_COD = XTG.XTG_TRANSP AND A4.D_E_L_E_T_ = '' " + CRLF 
cQuery += "		WHERE " + CRLF 
cQuery += "			XTG.XTG_FILIAL = '" + xFilial("XTG") + "' AND " + CRLF 
cQuery += "			UPPER(XTG.XTG_IDVTEX) = '" + Upper(cIdTran) + "' AND " + CRLF
cQuery += "			XTG.D_E_L_E_T_ = '' "  + CRLF  
cQuery += " ) TRANSP " 

cAlias := MPSysOpenQuery(cQuery)

If (cAlias)->( Eof() )
	cCodTransp	:= ""
	_cIdServ	:= ""
	
	(cAlias)->( dbCloseArea() )

	Return cIdPost
EndIf

cCodTransp	:= (cAlias)->COD_TRANSP
_cIdServ	:= ""

(cAlias)->( dbCloseArea() )

Return .T. 

/*******************************************************************************************/
/*/{Protheus.doc} AEcoI11TRC
	@description Regra para codigo transportadora
	@type  Static Function
	@author Bernard M Margarido
	@since 02/04/2024
	@version version
/*/
/*******************************************************************************************/
Static Function AEcoI11TRC(cCodCli,cLojaCli,cCodTransp)
Local aAreaAtu 	 := GetArea()
Local cUF		 := ""
Local cCodMun 	 := ""

SA1->( dbSetOrder(1) )
If SA1->( dbSeek(xFilial("SA1")+SA1->A1_COD+SA1->A1_LOJA) )

	CC2->( dbSetOrder(1) )
	If CC2->( dbSeek(xFilial("CC2")+ SA1->A1_ESTE + SA1->A1_XCDMUNE) )
		cUF		:= SA1->A1_ESTE
		cCodMun := SA1->A1_XCDMUNE
	Else
		cUF		:= SA1->A1_ESTE
		cCodMun := POSICIONE("CC2", 4, xFilial("CC2") + SA1->A1_ESTE + SA1->A1_MUNE,"CC2_CODMUN")
	EndIf

	cCodTransp := U_AA080RTR(cUF, cCodMun)
EndIf

RestArea(aAreaAtu)	
Return Nil

/*******************************************************************************************/
/*/{Protheus.doc} aEcoI011Entr
	@description Atualiza dados de entrega no cadastro do cliente
	@author Bernard M. Margarido
	@since 22/06/2018
	@version 1.0
	@type function
/*/
/*******************************************************************************************/
Static Function aEcoI011Entr(	cCodCli,cLojaCli,cNomDest,;
								cEndDest,cNumDest,cBaiDest,;
								cCepDest,cMunDest,cEstDest,cEndComp )
Local aArea		:= GetArea()
Local cCodMune	:= ""
Local cEndVld	:= cEndDest + ", " + cNumDest

//-------------------+
// Posiciona Cliente |
//-------------------+
dbSelectArea("SA1")
SA1->( dbSetOrder(1) )
If !SA1->( dbSeek(xFilial("SA1") + cCodCli + cLojaCli) )
	LogExec("CLIENTE NAO LOCALIZADO PARA ATUALIZAÇÃO DE ENDEREÇO DE ENTREGA.")
	RestArea(aArea)
	Return .T.
EndIf

//-------------------------------------------------------------------+
// Mesmo endereço de entrega e cobrança atualiza somente complemento |
//-------------------------------------------------------------------+
If RTrim(SA1->A1_END) == RTrim(cEndVld)
	//---------------------------+
	// Atualiza dados de entrega |
	//---------------------------+
	RecLock("SA1",.F.)
		SA1->A1_COMPLEM	:= cEndComp
	SA1->( MsUnLock() )	
Else
	//----------------------+
	// Municipio de Entrega | 
	//----------------------+
	cCodMune := EcCodMun(cEstDest,cMunDest)
	
	//---------------------------+
	// Atualiza dados de entrega |
	//---------------------------+
	RecLock("SA1",.F.)
		//------------------+
		// Endereço Cliente |
		//------------------+
		SA1->A1_CEP		:= cCepDest
		SA1->A1_END		:= cEndDest + ", " + cNumDest
		SA1->A1_BAIRRO	:= cBaiDest
		SA1->A1_MUN		:= cMunDest
		SA1->A1_EST		:= cEstDest
		SA1->A1_COD_MUN	:= cCodMune
		SA1->A1_COMPLEM	:= cEndComp
		
		//------------------+
		// Endereço Entrega |
		//------------------+
		SA1->A1_CEPE	:= cCepDest
		SA1->A1_ENDENT	:= cEndDest + ", " + cNumDest
		SA1->A1_BAIRROE	:= cBaiDest
		SA1->A1_MUNE	:= cMunDest
		SA1->A1_ESTE	:= cEstDest
		SA1->A1_CODMUNE	:= cCodMune
		
	SA1->( MsUnLock() )
	
EndIf

RestArea(aArea)
Return Nil

/*******************************************************************************************/
/*/{Protheus.doc} AEc011Cont
	@description Grava endereço de entrega nos contatos
	@type  Static Function
	@author Bernard M. Margarido
	@since 22/05/2019
/*/
/*******************************************************************************************/
Static Function AEc011Cont(cCodCli,cLoja,cNomeCli,aEndEnt)
Local _aArea	:= GetArea()

Local _lGrava	:= .T.

Local _cEnd		:= ""
Local _cNumEnd	:= ""
Local _cBairro	:= ""
Local _cMun		:= ""
Local _cCep		:= ""
Local _cEst		:= ""
Local _cCodMun	:= ""
Local _cNumSU5	:= ""
Local _cIdEnd	:= ""
Local _cContato	:= ""

Local _nRecnoSU5:= 0

//-------------------+
// Posiciona Cliente |
//-------------------+
dbSelectArea("SA1")
SA1->( dbSetOrder(1) )
SA1->( dbSeek(xFilial("SA1") + cCodCli + cLoja) )

_cEnd		:= aEndEnt[ENDERE]
_cNumEnd	:= aEndEnt[NUMERO]
_cBairro	:= aEndEnt[BAIRRO]
_cMun		:= aEndEnt[MUNICI]	
_cCep		:= aEndEnt[CEP]
_cEst		:= aEndEnt[ESTADO]
_cCodMun	:= aEndEnt[IBGE]
_cIdEnd		:= aEndEnt[IDENDE]
_cContato	:= aEndEnt[CONTAT]

_nRecnoSU5 	:= QryContato(_cCep)
			
//----------------------------+
// Posiciona dados do contato |
//----------------------------+
If _nRecnoSU5 > 0

	SU5->( dbGoTo(_nRecnoSU5) )
	_cNumSU5:= SU5->U5_CODCONT
	_lGrava	:= .F.

Else

	dbSelectArea("SU5")
	SU5->( dbSetOrder(1) )
	_cNumSU5 := GetSxeNum("SU5","U5_CODCONT")
	While SU5->( dbSeek(xFilial("SU5") + _cNumSU5 ) )
		ConfirmSx8()
		_cNumSU5 := GetSxeNum("SU5","U5_CODCONT")
	EndDo
	_lGrava := .T.

EndIf

//------------------+
// Atualiza Contato |
//------------------+	
RecLock("SU5",_lGrava)
	SU5->U5_FILIAL	:= xFilial("SU5")
	SU5->U5_CODCONT := _cNumSU5
	SU5->U5_CONTAT  := RTrim(_cContato)
	SU5->U5_EMAIL   := RTrim(SA1->A1_EMAIL)
	SU5->U5_CPF		:= RTrim(SA1->A1_CGC)
	SU5->U5_END		:= _cEnd + ", " + _cNumEnd
	SU5->U5_BAIRRO	:= _cBairro
	SU5->U5_MUN		:= _cMun
	SU5->U5_EST		:= _cEst
	SU5->U5_CEP		:= _cCep
	SU5->U5_DDD		:= RTrim(SA1->A1_DDD)
	SU5->U5_FONE	:= RTrim(SA1->A1_TEL)
	SU5->U5_CELULAR := RTrim(SA1->A1_TEL)
	SU5->U5_ATIVO	:= "1"
	SU5->U5_STATUS	:= "2"
	SU5->U5_XIDEND	:= _cIdEnd
	SU5->U5_MSBLQL	:= "2"
SU5->(MsUnLock())

//-----------------------------+
// Amarração Contato X Cliente |
//-----------------------------+
dbSelectArea("AC8")
AC8->( dbSetOrder(1) )
If !AC8->( dbSeek(xFilial("AC8") + _cNumSU5 + "SA1" + xFilial("SA1") + cCodCli + cLoja ) )
	RecLock("AC8",.T.)
		AC8->AC8_FILIAL := xFilial("AC8")
		AC8->AC8_FILENT	:= xFilial("SA1")
		AC8->AC8_ENTIDA := "SA1"
		AC8->AC8_CODENT	:= cCodCli + cLoja
		AC8->AC8_CODCON	:= _cNumSU5
	AC8->( MsUnLock() )	
EndIf 
		
//-------------------+
// Atualiza Endereço |
// Entrega			 |
//-------------------+	
aEcI10AGA(_cNumSU5,aEndEnt)


RestArea(_aArea)
Return Nil

/*******************************************************************************************/
/*/{Protheus.doc} QryContato
	@description Valida se já existe contato salvo
	@type  Static Function
	@author Bernard M. Margarido
	@since 22/05/2019
/*/
/*******************************************************************************************/
Static Function QryContato(_cCep)
Local cQuery := ""
Local cAlias := GetNextAlias()
Local nRecno := 0		

cQuery := "	SELECT " + CRLF
cQuery += "		U5.R_E_C_N_O_ RECNOSU5 " + CRLF
cQuery += "	FROM " + CRLF
cQuery += "		" + RetSqlName("SU5") + " U5 " + CRLF 
cQuery += "	WHERE " + CRLF
cQuery += "		U5.U5_FILIAL = '" + xFilial("SU5") + "' AND " + CRLF
cQuery += "		U5.U5_CEP = '" + _cCep + "' AND " + CRLF
cQuery += "		U5.D_E_L_E_T_ = '' " + CRLF

dbUseArea(.T.,"TOPCONN",TcGenQry(,,cQuery),cAlias,.T.,.T.)

nRecno := (cAlias)->RECNOSU5 

(cAlias)->( dbCloseArea() )

Return nRecno

/*******************************************************************************************/
/*/{Protheus.doc} aEcI10AGA
	@description Grava dados de entrega AGA
	@type  Static Function
	@author Bernard M. Margarido
	@since 22/05/2019
/*/
/*******************************************************************************************/
Static Function aEcI10AGA(_cNumSU5,aEndEnt)
Local _cCodEnd	:= ""
Local _cEnd		:= ""
Local _cNumEnd	:= ""
Local _cBairro	:= ""
Local _cMun		:= ""
Local _cCep		:= ""
Local _cEst		:= ""
Local _cCodMun	:= ""
Local _cIdEnd	:= ""

Local _lGrava	:= .F.

_cEnd		:= aEndEnt[ENDERE]
_cNumEnd	:= aEndEnt[NUMERO]
_cBairro	:= aEndEnt[BAIRRO]
_cMun		:= aEndEnt[MUNICI]	
_cCep		:= aEndEnt[CEP]
_cEst		:= aEndEnt[ESTADO]
_cCodMun	:= aEndEnt[IBGE]
_cIdEnd		:= aEndEnt[IDENDE]

//---------------------------+
// Valida se existe endereço |
//---------------------------+
_lGrava := AEcoVldAga(_cIdEnd)

If _lGrava
	_cCodEnd := GetSxeNum("AGA","AGA_CODIGO")
	dbSelectArea("AGA")
	AGA->( dbSetOrder(2) )
	While AGA->( dbSeek(xFilial("AGA") + _cCodEnd ) )
		ConfirmSx8()
		_cCodEnd := GetSxeNum("AGA","AGA_CODIGO")
	EndDo
	RecLock("AGA", _lGrava)
		AGA->AGA_FILIAL := xFilial("AGA")
		AGA->AGA_CODIGO	:= _cCodEnd
		AGA->AGA_ENTIDA	:= "SU5"					
		AGA->AGA_CODENT	:= _cNumSU5
		AGA->AGA_XIDEND	:= _cIdEnd
		AGA->AGA_TIPO 	:= "2"
		AGA->AGA_PADRAO	:= "1"
		AGA->AGA_END	:= _cEnd + ", " + _cNumEnd
		AGA->AGA_BAIRRO	:= _cBairro
		AGA->AGA_MUNDES	:= _cMun
		AGA->AGA_MUN    := _cCodMun
		AGA->AGA_EST	:= _cEst
		AGA->AGA_CEP	:= _cCep
		AGA->AGA_PAIS	:= "105"  
	AGA->(MsUnLock())
EndIf	
	
Return Nil

/*******************************************************************************************/
/*/{Protheus.doc} AEcoVldAga
	@description Valida se endereço já está cadastrado 
	@type  Static Function
	@author Bernard M. Margarido
	@since 27/05/2019
/*/
/*******************************************************************************************/
Static Function AEcoVldAga(_cIdEnd)
Local _aArea 	:= GetArea()

Local _cAlias	:= ""
Local _cQuery	:= ""

Local _nTID 	:= TamSx3("AGA_XIDEND")[1]

Local _lRet		:= .T.

_cQuery	:= " SELECT " + CRLF 
_cQuery	+= "	AGA.AGA_CODIGO " + CRLF
_cQuery	+= " FROM " + CRLF
_cQuery	+= "	" + RetSqlName("AGA") + " AGA " + CRLF 
_cQuery	+= " WHERE " + CRLF
_cQuery	+= "	AGA.AGA_FILIAL = '" + xFilial("AGA") + "' AND " + CRLF 
_cQuery	+= "	AGA.AGA_XIDEND = '" + PadR(_cIdEnd,_nTID) + "' AND " + CRLF
_cQuery	+= "	AGA.D_E_L_E_T_ = '' "

_cAlias := MPSysOpenQuery(_cQuery)

_lRet := IIF(Empty((_cAlias)->AGA_CODIGO), .T., .F.)

(_cAlias)->( dbCloseArea() )

RestArea(_aArea)
Return _lRet 

/*******************************************************************************************/
/*/{Protheus.doc} AEcoI11X
	@description Realiza a consulta dos pedidos VTEX
	@type  Static Function
	@author Bernard M Margarido
	@since 18/10/2023
	@version version
/*/
/*******************************************************************************************/
Static Function AEcoI11X(_nPage,_cError,_oJson)
Local _cJSon		:= ""
Local _cParams		:= ""     
Local _cOrderBy		:= ""
Local _cPerPage 	:= ""
Local _cPage 		:= ""
Local _cQryParam 	:= ""

Local _oVTEX		:= VTEX():New()

Local _lRet	 		:= .T.

//------------------------+
// Parametros de pesquisa |
//------------------------+
_cQryParam 	:= "f_status=ready-for-handling"
_cOrderBy  	:= "&orderBy=creationDate,asc"
_cPerPage  	:= "&per_page=100"
_cPage	  	:= "&page=" + AllTrim(cValToChar(_nPage)) + "	
_cParams 	:= _cQryParam + _cOrderBy + _cPerPage + _cPage

//---------+
// Timeout |
//---------+
_oVTEX:cParam	:= _cParams
_oVTEX:cMetodo	:= "GET"

If !Empty(_cLojaID)
	_oVTEX:cAppKey		:= _cAppKey
	_oVTEX:cAppToken	:= _cAppToken
	_oVTEX:cUrl			:= _cUrl
EndIf 

If _oVTEX:OrderBatches()
	If ValType(_oVTEX:cJSonRet) <> "U"
		_cJSon := _oVTEX:cJSonRet
		If !FWJsonDeserialize(_cJSon,@_oJson)
			_lRet := .F.
		EndIf 
	Else 
		_cError:= _oVTEX:cError
		_lRet  := .F.
	EndIf 
Else 
	_cError:= _oVTEX:cError
	_lRet := .F.
EndIF 

Return _lRet 

/*******************************************************************************************/
/*/{Protheus.doc} LogExec
	@description Grava log 
	@type  Static Function
	@author Bernard M. Margarido
	@since 22/05/2019
/*/
/*******************************************************************************************/
Static Function LogExec(cMsg)
	CONOUT(cMsg)
	LjWriteLog(cArqLog,cMsg)
Return 
