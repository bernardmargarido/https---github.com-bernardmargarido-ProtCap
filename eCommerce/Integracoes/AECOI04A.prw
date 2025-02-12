#INCLUDE "PROTHEUS.CH"
#INCLUDE "APWEBSRV.CH"
#INCLUDE "TOPCONN.CH"
#INCLUDE "TBICONN.CH"

#DEFINE CRLF CHR(13) + CHR(10)

Static cCodInt	:= "04A"
Static cDescInt	:= "SKU"
Static cDirImp	:= "/ecommerce/"

/*******************************************************************************************/
/*/{Protheus.doc} AECOI04A
	@description Realiza envio das especificações de SKU
	@author Bernard M. Margarido
	@since 23/01/2018
	@version 1.0
	@type function
/*/
/*******************************************************************************************/
User Function AECOI04A()

Private cThread	:= Alltrim(Str(ThreadId()))
Private cStaLog	:= "0"
Private cArqLog	:= ""	

Private nQtdInt	:= 0

Private cHrIni	:= Time()
Private dDtaInt	:= Date()

Private aMsgErro:= {}
Private aPrdEnv	:= {}

//------------------------------+
// Inicializa Log de Integracao |
//------------------------------+
MakeDir(cDirImp)
cArqLog := cDirImp + "ESPECIFICACAOSKU" + cEmpAnt + cFilAnt + ".LOG"
ConOut("")	
LogExec(Replicate("-",80))
LogExec("INICIA INTEGRACAO ESPECIFICACAO DE PRODUTOS SKU COM A VTEX - DATA/HORA: " + DTOC( DATE() ) + " AS " + TIME())

//---------------------------------------+
// Inicia processo de envio dos produtos |
//---------------------------------------+
Processa({|| AECOINT4A() },"Aguarde...","Consultando Produtos.")

LogExec("FINALIZA INTEGRACAO ESPECIFICACAO DE PRODUTOS SKU COM A VTEX - DATA/HORA: " + DTOC( DATE() ) + " AS " + TIME())
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
/*/{Protheus.doc} AECOINT4A

@description	Rotina consulta e envia Produtos para a pataforma e-Commerce

@author			Bernard M.Margarido
@version   		1.00
@since     		10/02/2016
/*/
/**************************************************************************************************/
Static Function AECOINT4A()
Local aArea			:= GetArea()

Local cCodSku 		:= ""
Local cNomePrd		:= ""
Local cTamanho		:= ""

Local cAlias		:= GetNextAlias()

Local nIdSku		:= 0
Local nToReg		:= 0

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
	cCodSku 	:= (cAlias)->CODIGO
	cNomePrd	:= (cAlias)->NOME
	cTamanho	:= (cAlias)->TAMANHO
	nIdSku		:= (cAlias)->IDSKU
		
	LogExec("ENVIANDO ESPECIFICACAO PRODUTO SKU " + Alltrim((cAlias)->CODIGO) + " - " + Alltrim((cAlias)->NOME) )
		 
	//---------------------------------------+
	// Rotina realiza o envio para a Rakuten |
	//---------------------------------------+
	AEcoEnv(cCodSku,cNomePrd,nIdSku,cTamanho)
	
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

@description	Rotina envia dados do produto para a plataforma e-commerce

@author			Bernard M.Margarido
@version   		1.00
@since     		02/02/2016

@param cCodPai		, characters, descricao
@param cNomePrd		, characters, descricao
@param cTitPrd		, characters, descricao
@param cSubTitPrd	, characters, descricao
@param cDescPrd		, characters, descricao
@param cCarcacPrd	, characters, descricao
@param cKeyword		, characters, descricao
@param cStatus		, characters, descricao
@param cTpProd		, characters, descricao
@param cIdLoja		, characters, descricao
@param nCat01		, numeric	, descricao
@param nCat02		, numeric	, descricao
@param nCat03		, numeric	, descricao
@param nCat04		, numeric	, descricao
@param nCat05		, numeric	, descricao
@param nFabric		, numeric	, descricao
@param nIdProd		, numeric	, descricao
@param nRecnoB5		, numeric	, descricao
@param nRecnoWs5	, numeric	, descricao

@type function
/*/
/**************************************************************************************************/
Static Function AEcoEnv(cCodSku,cNomePrd,nIdSku,cTamanho)
						
Local aArea			:= GetArea()

Local cUrl			:= GetNewPar("EC_URLECOM")
Local cUsrVTex		:= GetNewPar("EC_USRVTEX")
Local cPswVTex		:= GetNewPar("EC_PSWVTEX")

Local oWsProd		:= Nil

//------------------+
// Instancia Classe |
//------------------+
oWsProd 			:= WSVTex():New() 

//---------------------+
// Parametros de Envio |
//---------------------+
oWsProd:_URL 		:= cUrl
oWsProd:_HEADOUT 	:= {}	

//--------------------------------------------+
// Adiciona Usuario e Senha para autenticação |
//--------------------------------------------+
aAdd(oWsProd:_HEADOUT, "Authorization: Basic " + Encode64(cUsrVTex + ":" + cPswVTex) )

//---------------------------+
// Adiciona dados do Produto |
//---------------------------+
oWsProd:nIdSku		:= nIdSku
oWsProd:cFieldName	:= "Tamanho"

//--------------------------+
// Adiciona Canal de Vendas |
//--------------------------+
oWsProd:oWSfieldValues := Service_ArrayOfstring():New()

//------------------------------------+
// Cria Array para os Canais de Venda |
//------------------------------------+
oWsProd:oWSfieldValues:cString := {}
               

aAdd(oWsProd:oWSfieldValues:cString,cTamanho) 
	

LogExec("ENVIANDO ESPECIFICACAO PRODUTO SKU " + cCodSku + " - " + Alltrim(cNomePrd) + " ." )

WsdlDbgLevel(3)
If oWsProd:StockKeepingUnitEspecificationInsert()  
	LogExec("ESPECIFICACAO PRODUTO SKU " + cCodSku + " - " + Alltrim(cNomePrd) + " . ENVIADA COM SUCESSO." )
Else
	aAdd(aMsgErro,{cCodSku,"ERRO AO ENVIAR ESPECIFICACAO PRODUTO SKU " + Alltrim(cCodSku) + " - " + Upper(Alltrim(cNomePrd)) + " " + Alltrim(GetWscError()) + CRLF})
	LogExec("ERRO AO ENVIAR ESPECIFICACAO PRODUTO SKU " + cCodSku + " - " + cNomePrd + " . " + GetWSCError() )
EndIf

RestArea(aArea)
Return .T.

/**************************************************************************************************/

/*/{Protheus.doc} AECOQRY

@description 	Rotina consulta os produtos a serem enviados para a pataforma e-Commerce

@author			Bernard M.Margarido
@version   		1.00
@since     		10/02/2016

@param			cAlias 		, Nome Arquivo Temporario
@param			nToReg		, Grava total de registros encontrados

@return			lRet - Variavel Logica
/*/			
/**************************************************************************************************/
Static Function AEcoQry(cAlias,nToReg)
Local cQuery := ""

//-----------------------------+
// Query consulta produtos pai |
//-----------------------------+
cQuery := "	SELECT " + CRLF
cQuery += "		CODIGO, " + CRLF 
cQuery += "		NOME, " + CRLF
cQuery += "		IDSKU, " + CRLF
cQuery += "		TAMANHO, " + CRLF
cQuery += "		RECNOZTG " + CRLF
cQuery += "	FROM " + CRLF
cQuery += "	( " + CRLF
cQuery += "		SELECT " + CRLF
cQuery += "			B1.B1_COD CODIGO, " + CRLF 
cQuery += "			B1.B1_DESC NOME, " + CRLF
cQuery += "			B1.B1_XIDSKU IDSKU, " + CRLF
cQuery += "			ZTF.ZTF_VALUE TAMANHO, " + CRLF
cQuery += "			ZTG.R_E_C_N_O_ RECNOZTG " + CRLF 
cQuery += "		FROM " + CRLF
cQuery += "			" + RetSqlName("ZTG") + " ZTG " + CRLF  
cQuery += "			INNER JOIN " + RetSqlName("SB1") + " B1 ON B1.B1_FILIAL = '" + xFilial("SB1") + "' AND B1.B1_COD = ZTG.ZTG_PRODUT AND B1.D_E_L_E_T_ = '' " + CRLF
cQuery += "			INNER JOIN " + RetSqlName("ZTF") + " ZTF ON ZTF.ZTF_FILIAL = '" + xFilial("ZTF") + "' AND ZTF.ZTF_COD = ZTG.ZTG_VALUE AND ZTG.D_E_L_E_T_ = '' " + CRLF
cQuery += "		WHERE " + CRLF
cQuery += "			ZTG.ZTG_FILIAL = '" + xFilial("ZTG") + "' AND " + CRLF  
cQuery += "			ZTG.ZTG_PRDSKU = '2' AND " + CRLF
cQuery += "			ZTG.D_E_L_E_T_ = '' " + CRLF
cQuery += "	) PRDSKU " + CRLF
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
	@type function
/*/
/*********************************************************************************/
Static Function LogExec(cMsg)
	CONOUT(cMsg)
	LjWriteLog(cArqLog,cMsg)
Return .T.
