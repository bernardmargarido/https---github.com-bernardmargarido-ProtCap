#INCLUDE "PROTHEUS.CH"
#INCLUDE "APWEBSRV.CH"
#INCLUDE "TOPCONN.CH"
#INCLUDE "TBICONN.CH"

#DEFINE CRLF CHR(13) + CHR(10)

Static cCodInt	:= "11B"
Static cDescInt	:= "STATUSPEDIDO"
Static cDirImp	:= "/ecommerce/"

/**************************************************************************************************/
/*/{Protheus.doc} AECOI011B
	@description	Rotina realiza a integra็ใo dos status do pedido ecommerce
	@type   		Function 
	@author			Bernard M.Margarido
	@version   		1.00
	@since     		10/02/2016
/*/
/**************************************************************************************************/

User Function AECOI11B(cNumOrc,lCancel)
Local aArea		:= GetArea()
Local aRet		:= {.T.,"",""}

Local _lBloqueio:= GetNewPar("EC_BLSMSG",.F.)	

Private cThread	:= Alltrim(Str(ThreadId()))
Private cStaLog	:= "0"
Private cArqLog	:= ""	

Private nQtdInt	:= 0

Private cHrIni	:= Time()
Private dDtaInt	:= Date()

Private aMsgErro:= {}

Private lJob 	:= .F.

Default lCancel	:= .F.

If _lBloqueio
	RestArea(aArea)
	Return aRet
EndIf

//----------------------------------+
// Grava Log inicio das Integra็๕es | 
//----------------------------------+
u_AEcoGrvLog(cCodInt,cDescInt,dDtaInt,cHrIni,,,,,cThread,1)

//------------------------------+
// Inicializa Log de Integracao |
//------------------------------+
MakeDir(cDirImp)
cArqLog := cDirImp + "BAIXAPEDIDO" + cEmpAnt + cFilAnt + ".LOG"
ConOut("")	
LogExec(Replicate("-",80))
LogExec("INICIA INTEGRACAO DO STATUS PEDIDOS DE VENDA COM O ECOMMERCE - DATA/HORA: "+DTOC(DATE())+" AS "+TIME())

//---------------------------------+
// Inicia processo de envio Pre็os |
//---------------------------------+
Processa({|| aRet := u_AEcoStat(cNumOrc,lCancel) },"Aguarde...","Atualizando status dos pedido no eCommerce.")

LogExec("FINALIZA INTEGRACAO DO STATUS PEDIDOS DE VENDA COM O ECOMMERCE - DATA/HORA: "+DTOC(DATE())+" AS "+TIME())
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
// Grava Log inicio das Integra็๕es |
//----------------------------------+
u_AEcoGrvLog(cCodInt,cDescInt,dDtaInt,cHrIni,Time(),cStaLog,nQtdInt,aMsgErro,cThread,2)

RestArea(aArea)
Return aRet

/*********************************************************************************************************/
/*/{Protheus.doc} AEcoI011B
	@description	Envia status do pedido ecommerce
	@author			Bernard M.Margarido
	@version   		1.00
	@since     		10/02/2016
/*/			
/**********************************************************************************************************/
User Function AEcoStat(cNumOrc,lCancel)
Local aRet			:= {.T.,"",""}
Local aHeadOut		:= {}

Local cUrl			:= GetNewPar("EC_URLVTEX")
Local cAppKey		:= GetNewPar("EC_APPVTEX")
Local cAppToken		:= GetNewPar("EC_APTVTEX")
Local _cVtexName	:= "tfcp19"
Local cXmlHead 	 	:= ""
Local cError    	:= ""

Local nTimeOut		:= 240

Local _lBloqueio	:= GetNewPar("EC_BLSMSG",.F.)

Local oRestRet   	:= Nil 

Default lCancel		:= .F.

If _lBloqueio
	RestArea(aArea)
	Return aRet
EndIf

//-------------------------------+
// Pocisiona Pedido de eCommerce |
//-------------------------------+
dbSelectArea("WSA")
WSA->( dbSetOrder(1) )
If !WSA->( dbSeek(xFilial("WSA") + cNumOrc) )
	aRet[1] := .F.
	aRet[2] :=  cNumOrc
	aRet[3]	:= "NAO FOI POSSIVEL ATUALIZAR STATUS DO PEDIDO " + cNumOrc + ". PEDIDO NAO ENCONTRADO NO PROTHEUS."
	aAdd(aMsgErro,{aRet[2],aRet[3]})
	RestArea(aArea)
	Return aRet
EndIf

aAdd(aHeadOut,"Content-Type: application/json" )
aAdd(aHeadOut,"X-VTEX-API-AppKey:" + cAppKey )
aAdd(aHeadOut,"X-VTEX-API-AppToken:" + cAppToken )

If lCancel
	cHtmlPage :=  HttpPost(cUrl + "/api/oms/pvt/orders/" + RTrim(WSA->WSA_NUMECO) + "/cancel/?an=" + _cVtexName + "","","",nTimeOut,aHeadOut,@cXmlHead)
Else
	cHtmlPage :=  HttpPost(cUrl + "/api/oms/pvt/orders/" + RTrim(WSA->WSA_NUMECO) + "/start-handling/?an=" + _cVtexName + "","","",nTimeOut,aHeadOut,@cXmlHead)
EndIf	 
	
If HTTPGetStatus() == 200 .Or. Empty(cHtmlPage)

	RecLock("WSA",.F.)
		WSA->WSA_VLBXPV := "2"
	WSA->( MsUnLock() )	
	
	aRet[1] := .T.
	aRet[2] := WSA->WSA_NUM
	aRet[3] := "ALTERACAO DE STATUS DO PEDIDO " + WSA->WSA_NUM + " ENVIADO COM SUCESSO."
	LogExec("ALTERACAO DE STATUS DO PEDIDO " + WSA->WSA_NUM + " ENVIADO COM SUCESSO.")
	
Else

	If FWJsonDeserialize(cHtmlPage,@oRestRet)
		cError := oRestRet:ERROR:CODE + ": " + oRestRet:ERROR:MESSAGE
	EndIf
	
	aRet[1] := .F.
	aRet[2] := WSA->WSA_NUM
	aRet[3] := "ERRO AO ENVIAR A ALTERACAI DE STATUS DO PEDIDO " + WSA->WSA_NUM + " " + cError
	LogExec("ALTERACAO DE STATUS DO PEDIDO " + WSA->WSA_NUM + " ENVIADO COM SUCESSO.")
	
	aAdd(aMsgErro,{aRet[2],aRet[3]})
		
EndIf
	
Return aRet

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
ฑฑษออออออออออัออออออออออหอออออออัออออออออออออออออออออหออออออัอออออออออออออปฑฑ
ฑฑบPrograma  ณLogExec   บAutor  ณSYMM Consultoria    บ Data ณ  30/12/14   บฑฑ
ฑฑฬออออออออออุออออออออออสอออออออฯออออออออออออออออออออสออออออฯอออออออออออออนฑฑ
ฑฑบDesc.     ณGrava Log do processo                                       บฑฑ
ฑฑศออออออออออฯออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผฑฑ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿
*/
Static Function LogExec(cMsg)
	CONOUT(cMsg)
	LjWriteLog(cArqLog,cMsg)
Return .T.
