#INCLUDE "PROTHEUS.CH"
#INCLUDE "APWEBSRV.CH"
#INCLUDE "TOPCONN.CH"
#INCLUDE "TBICONN.CH"

#DEFINE CRLF CHR(13) + CHR(10)

Static cCodInt	:= "11A"
Static cDescInt	:= "BAIXAPEDIDO"
Static cDirImp	:= "/ecommerce/"

/**************************************************************************************************/
/*/{Protheus.doc} AECOI011A
	@description	Rotina realiza a integra็ใo da valida็ใo da baixa do pedido
	@type   		Function 
	@author			Bernard M.Margarido
	@version   		1.00
	@since     		10/02/2016
/*/
/**************************************************************************************************/
User Function AECOI11A(cOrderId,cNumOrc)
Local aArea		:= GetArea()
Local aRet		:= {.T.,"",""}

Private cThread	:= Alltrim(Str(ThreadId()))
Private cStaLog	:= "0"
Private cArqLog	:= ""	

Private nQtdInt	:= 0

Private cHrIni	:= Time()
Private dDtaInt	:= Date()

Private aMsgErro:= {}

Private lJob 	:= .F.

//------------------------------+
// Inicializa Log de Integracao |
//------------------------------+
MakeDir(cDirImp)
cArqLog := cDirImp + "BAIXAPEDIDO" + cEmpAnt + cFilAnt + ".LOG"
ConOut("")	
LogExec(Replicate("-",80))
LogExec("INICIA INTEGRACAO DA BAIXA DOS PEDIDOS DE VENDA COM A RAKUTEN - DATA/HORA: "+DTOC(DATE())+" AS "+TIME())

//---------------------------------+
// Inicia processo de envio Pre็os |
//---------------------------------+
Processa({|| aRet := u_AEcoBxPv(cOrderId,cNumOrc) },"Aguarde...","Atualizando baixa dos pedido na Rakuten.")

LogExec("FINALIZA INTEGRACAO DA BAIXA DOS PEDIDOS DE VENDA COM A RAKUTEN - DATA/HORA: "+DTOC(DATE())+" AS "+TIME())
LogExec(Replicate("-",80))
ConOut("")

//----------------------------------+
// Envia e-Mail com o Logs de Erros |
//----------------------------------+
If Len(aMsgErro) > 0
	cStaLog := "1"
	u_AEcoMail(cCodInt,cDescInt,aMsgErro)
EndIf

RestArea(aArea)
Return aRet

/*********************************************************************************************************/
/*/{Protheus.doc} AEcoI011A

@description	Envia a baixa do pedido ecommerce

@author			Bernard M.Margarido
@version   		1.00
@since     		10/02/2016

@param			cOrderId	, Numero OrderId e-Commerce
@param			cNumOrc		, Numero do Or็amento Protheus

@return			aRet 		- Array aRet[1] - Logico aRet[2] - Codigo Erro	aRet[3] - Descricao do Erro  
/*/			
/*********************************************************************************************************/
User Function AEcoBxPv(cOrderId,cNumOrc)
Local aRet		:= {.T.,"",""}

Local oWsBaixaPv:= Nil

oWsBaixaPv :=  WSPedido():New

oWsBaixaPv:_Url						:= AllTrim(GetMV("EC_URLECOM")) + "pedido.asmx?"
oWsBaixaPv:nLojaCOdigo 				:= 0
oWsBaixaPv:nCodigoPedido   			:= Val(cOrderId)
oWsBaixaPv:cCodigoInternoPedido 	:= cNumOrc

If oWsBaixaPv:Validar()
	If oWsBaixaPv:oWsValidarResult:nCodigo == 1
		aRet[1] := .T.
		aRet[2] := "0"
		aRet[3] := oWsBaixaPv:oWsValidarResult:cDescricao
		LogExec("VALIDACAO DA BAIXA DO PEDIDO " + cOrderId + " ENVIADO COM SUCESSO ENVIADA COM SUCESSO." )
	Else
		aRet[1] := .F.
		aRet[2] := "1"
		aRet[3] := oWsBaixaPv:oWsValidarResult:cDescricao
		LogExec("ERRO AO ENVIAR A VALIDACAO DA BAIXA DO PEDIDO " + cOrderId + " " + Upper(oWsBaixaPv:oWsValidarResult:cDescricao) )
		aAdd(aMsgErro,{aRet[2],aRet[3]})	
	EndIf
Else
	aRet[1] := .F.
	aRet[2] := "1"
	aRet[3] := "Erro de Conexใo " + Alltrim(GetWscError()) + CRLF
	LogExec("ERRO DE CONEXAO " + " " + Alltrim(GetWscError()) )
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
