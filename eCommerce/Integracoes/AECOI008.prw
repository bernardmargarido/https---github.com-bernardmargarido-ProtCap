#INCLUDE "PROTHEUS.CH"
#INCLUDE "APWEBSRV.CH"
#INCLUDE "TOPCONN.CH"
#INCLUDE "TBICONN.CH"
#INCLUDE "AARRAY.CH"
#INCLUDE "JSON.CH"

#DEFINE CRLF CHR(13) + CHR(10)

Static cCodInt	:= "008"
Static cDescInt	:= "ESTOQUE"
Static cDirImp	:= "/ecommerce/"
Static cDirSave	:= "estoque/"

/**************************************************************************************************/
/*/{Protheus.doc} AECOI008
	@description	Rotina realiza a integração dos estoques e-commerce
	@type   		Function 
	@author			Bernard M.Margarido
	@version   		1.00
	@since     		10/02/2016
/*/
/**************************************************************************************************/
User Function AECOI008()

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

//----------------------------------+
// Grava Log inicio das Integrações | 
//----------------------------------+
u_AEcoGrvLog(cCodInt,cDescInt,dDtaInt,cHrIni,,,,,cThread,1)

//------------------------------+
// Inicializa Log de Integracao |
//------------------------------+
MakeDir(cDirImp)
cArqLog := cDirImp + "ESTOQUE" + cEmpAnt + cFilAnt + ".LOG"
ConOut("")	
LogExec(Replicate("-",80))
LogExec("INICIA INTEGRACAO DE ESTOQUE COM A VTEX - DATA/HORA: "+DTOC(DATE())+" AS "+TIME())

If _lJob
	AECOINT08()
Else
	Processa({|| AECOINT08() },"Aguarde...","Consultando Estoque.")
EndIf

LogExec("FINALIZA INTEGRACAO DE ESTOQUE COM A VTEX - DATA/HORA: "+DTOC(DATE())+" AS "+TIME())
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
/*/{Protheus.doc} AECOINT08
	@description	Rotina consulta e envia estoque dos produtos para a pataforma e-Commerce
	@author			Bernard M.Margarido
	@version   		1.00
	@since     		10/02/2016
/*/
/**************************************************************************************************/
Static Function AECOINT08()
Local aArea		:= GetArea()

Local cCodSku	:= ""
Local cDescSku	:= ""
Local cAlias	:= GetNextAlias()

Local nToReg	:= 0
Local nIdSku	:= 0

Local oJson		:= Nil
//Local oEstoque	:= Nil

//---------------------------------------------+
// Valida se existem Estoques a serem enviadas |
//---------------------------------------------+
If !AEcoQry(cAlias,@nToReg)
	LogExec("NAO EXISTEM REGISTROS PARA SEREM ENVIADOS.")  
	RestArea(aArea)
	Return .T.
EndIf

//-------------------------+
// Inicia o envio Estoques |
//-------------------------+
If !_lJob
	ProcRegua(nToReg)
EndIf

While (cAlias)->( !Eof() )
	
	//-----------------------------------+
	// Incrementa regua de processamento |
	//-----------------------------------+
	If !_lJob
		IncProc("Estoque " + Alltrim((cAlias)->CODSKU)  + " - " + Alltrim((cAlias)->DESCSKU) )
	Endif	

	LogExec("ESTOQUE " + Alltrim((cAlias)->CODSKU)  + " - " + Alltrim((cAlias)->DESCSKU) )

	//--------------------+
	// Posiciona registro |
	//--------------------+
	SB2->( dbGoTo((cAlias)->RECNOSB2) )
		
	//--------------------------+
	// Dados Filtros X Produtos |
	//--------------------------+
	cCodSku		:= (cAlias)->CODSKU
	cDescSku	:= (cAlias)->DESCSKU
	cWarehouse	:= "1_1"
	nIdSku		:= (cAlias)->IDSKU
	nRecno 		:= (cAlias)->RECNOSB2

	//------------------+
	// Saldo em Estoque |
	//------------------+
	nSaldoB2 := SaldoSb2()
	
	//-----------------+
	// Cria Array JSON |
	//-----------------+
	/*
	oJson								:= Nil 
	oJson								:= JSonObject():New()
	oJson								:= {}
	oEstoque							:= JSonObject():New()
	oEstoque["wareHouseId"]				:= "1_1"
	oEstoque["itemId"]					:= Alltrim(Str(nIdSku))
	oEstoque["unlimitedQuantity"]		:= .F.
	oEstoque["quantity"]				:= nSaldoB2
	oEstoque["dateUtcOnBalanceSystem"]	:= Nil
	aAdd(oJson,oEstoque)
	*/

	oJson								:= Nil 
	oJson								:= JSonObject():New()
	oJson['unlimitedQuantity']			:= .F.
	oJson['dateUtcOnBalanceSystem']		:= Nil 
	oJson['quantity']					:= nSaldoB2

	cRest								:= oJSon:ToJson()

	LogExec("ENVIANDO ESTOQUE " + Alltrim((cAlias)->CODSKU) + " - " + Alltrim((cAlias)->DESCSKU) + " IDSKU " + Alltrim(Str(nIdSku)) + " SALDO " + Alltrim(Str(nSaldoB2)) )
					 
	//-----------------------------------------+
	// Rotina realiza o envio para o ecommerce |
	//-----------------------------------------+
	AEcoEnv(cRest,cCodSku,cDescSku,cWarehouse,nIdSku,nRecno)
					
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
	@description	Rotina envia o estoque dos produtos para a plataforma e-commerce
	@author			Bernard M.Margarido
	@version   		1.00
/*/							
/**************************************************************************************************/

Static Function AEcoEnv(cRest,cCodSku,cDescSku,cWarehouse,nIdSku,nRecnoSb2)
Local aArea			:= GetArea()

Local _oVTEX 		:= VTEX():New()
Local _oJSon 		:= Nil 

Private cType		:= ""

//--------------------------------+
// Cria diretorio caso nao exista |
//--------------------------------+
MakeDir(cDirImp)
MakeDir(cDirImp + cDirSave)
MemoWrite(cDirImp + cDirSave + "\jsonestoque_" + RTrim(cCodSku) + ".json",cRest)

//---------------------+
// Parametros de envio | 
//---------------------+
_oVTEX:cMetodo		:= "PUT"
_oVTEX:cJSon		:= cRest
_oVTEX:cID			:= cValToChar(nIdSku)
_oVTEX:cWarehouse 	:= cWarehouse

If _oVTEX:Estoque()
	
	_oJSon := JSonObject():New()
	_oJSon:FromJson(_oVTEX:cJSonRet)
	
	//--------------------+
	// Posiciona Registro |
	//--------------------+
	SB2->( dbGoTo( nRecnoSb2) )

	RecLock("SB2",.F.)
		SB2->B2_MSEXP		:= dTos(Date())
		SB2->B2_XDTEXP		:= Date()
		SB2->B2_XHREXP		:= Time()	
	SB2->( MsUnLock() )

	LogExec("ESTOQUE PRODUTO " + cCodSku + " ENVIADO COM SUCESSO." )
	
Else
	If ValType(_oVTEX:cError) <> "U"
		aAdd(aMsgErro,{cCodSku,"ERRO AO ENVIAR ESTOQUE PRODUTO " + Alltrim(cCodSku) + " - " + Upper(Alltrim(cDescSku)) + ". ERROR: " + RTrim(_oVTEX:cError)})
		LogExec("ERRO AO ENVIAR ESTOQUE PRODUTO " + Alltrim(cCodSku) + " - " + Upper(Alltrim(cDescSku)) + ". ERROR: " +  RTrim(_oVTEX:cError))
	Else 
		aAdd(aMsgErro,{cCodSku,"ERRO AO ENVIAR ESTOQUE PRODUTO " + Alltrim(cCodSku) + " - " + Upper(Alltrim(cDescSku)) + ". "})
		LogExec("ERRO AO ENVIAR ESTOQUE PRODUTO " + Alltrim(cCodSku) + " - " + Upper(Alltrim(cDescSku)) + ". ")
	EndIf 
EndIf

FreeObj(_oVTEX)
FreeObj(_oJSon)

RestArea(aArea)
Return .T.

/**********************************************************************************************/
/*/{Protheus.doc} AECOQRY
	@description 	Rotina consulta os estoques a serem enviados para a pataforma e-Commerce
	@author			Bernard M.Margarido
	@version   		1.00
	@since     		10/02/2016
/*/			
/************************************************************************************************/
Static Function AEcoQry(cAlias,nToReg,_cLojaID)

Local cQuery 	:= ""
Local cFilEst	:= GetNewPar("EC_FILEST")
Local cLocal	:= FormatIn(GetNewPar("EC_ARMAZEM"),"/")

Default _cLojaID:= ""

//------------------------+
// Query consulta Estoques|
//------------------------+
cQuery := "	SELECT " + CRLF  
cQuery += "		CODSKU, " + CRLF
cQuery += "		DESCSKU, " + CRLF
cQuery += "		IDSKU, " + CRLF
cQuery += "		SALDOB2, " + CRLF
cQuery += "		RECNOSB2 " + CRLF
cQuery += "	FROM " + CRLF
cQuery += "	( " + CRLF
cQuery += "		SELECT " + CRLF
cQuery += "			B2.B2_COD CODSKU, " + CRLF
cQuery += "			B1.B1_DESC DESCSKU, " + CRLF
cQuery += "			B5.B5_XIDSKU IDSKU, " + CRLF
cQuery += "			B2.B2_QATU SALDOB2, " + CRLF
cQuery += "			B2.R_E_C_N_O_ RECNOSB2 " + CRLF
cQuery += "		FROM " + CRLF
cQuery += "			" + RetSqlName("SB2") + " B2 " + CRLF
cQuery += "			INNER JOIN " + RetSqlName("SB1") + " B1 ON B1.B1_FILIAL = '" + xFilial("SB1") + "' AND B1.B1_COD = B2.B2_COD AND B1.B1_MSBLQL <> '1' AND B1.D_E_L_E_T_ = '' " + CRLF 
cQuery += "			INNER JOIN " + RetSqlName("SB5") + " B5 ON B5.B5_FILIAL = '" + xFilial("SB5") + "' AND B5.B5_COD = B2.B2_COD AND B5.B5_XENVECO = '2' AND B5.B5_XENVSKU = '2' AND B5.B5_XUSAECO = 'S' AND B5.D_E_L_E_T_ = '' " + CRLF
cQuery += "		WHERE " + CRLF
cQuery += "			B2.B2_FILIAL = '" + cFilEst  + "' AND " + CRLF 
cQuery += "			B2.B2_LOCAL IN " + cLocal + " AND " + CRLF
cQuery += "			B2.B2_MSEXP = '' AND " + CRLF
cQuery += "			B2.D_E_L_E_T_ = '' " + CRLF
cQuery += "	) ESTOQUE " + CRLF
cQuery += "	ORDER BY CODSKU " 

dbUseArea(.T.,"TOPCONN",TcGenQry(,,cQuery),cAlias,.T.,.T.)
Count To nToReg  

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
	@type function
/*/
/*********************************************************************************/
Static Function LogExec(cMsg)
	CONOUT(cMsg)
	LjWriteLog(cArqLog,cMsg)
Return .T.
