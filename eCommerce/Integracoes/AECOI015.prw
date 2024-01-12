#INCLUDE "PROTHEUS.CH"
#INCLUDE "APWEBSRV.CH"
#INCLUDE "TOPCONN.CH"
#INCLUDE "TBICONN.CH"
#INCLUDE "AARRAY.CH"
#INCLUDE "JSON.CH"

#DEFINE CRLF CHR(13) + CHR(10)

Static cCodInt	:= "015"
Static cDescInt	:= "Specification"
Static cDirImp	:= "/ecommerce/"
Static cDirSave	:= "especificos/"

/**********************************************************************************************/
/*/{Protheus.doc} AECOI015
    @description Realiza a integracao dos Grupos de Campos Especificos 
    @type  Function
    @author Bernard M Margarido
    @since 13/06/2023
    @version version
/*/
/**********************************************************************************************/
User Function AECOI015()
Private cThread			:= Alltrim(Str(ThreadId()))
Private cStaLog			:= "0"
Private cArqLog			:= ""	

Private nQtdInt			:= 0

Private cHrIni			:= Time()
Private dDtaInt			:= Date()

Private aMsgErro		:= {}
Private _aRecno			:= {}

Private _oProcess 		:= Nil

//----------------------------------+
// Grava Log inicio das Integrações | 
//----------------------------------+
u_AEcoGrvLog(cCodInt,cDescInt,dDtaInt,cHrIni,,,,,cThread,1)

//------------------------------+
// Inicializa Log de Integracao |
//------------------------------+
MakeDir(cDirImp)
cArqLog := cDirImp + "GRUPOSCAMPOSESPECIFICOS" + cEmpAnt + cFilAnt + ".LOG"
LogExec("")	
LogExec(Replicate("-",80))
LogExec("INICIA INTEGRACAO DE GRUPOS DE CAMPOS ESPECIFICOS COM A VTEX - DATA/HORA: "+DTOC(DATE())+" AS "+TIME())

//-----------------------------------------+
// Inicia processo de envio das categorias |
//-----------------------------------------+
Processa({|| AECOINT15() },"Aguarde...","Consultando as Grupo de Campos.")


LogExec("FINALIZA INTEGRACAO DE GRUPOS DE CAMPOS ESPECIFICOS COM A VTEX - DATA/HORA: "+DTOC(DATE())+" AS "+TIME())
LogExec(Replicate("-",80))
LogExec("")

//----------------------------------+
// Grava Log inicio das Integrações |
//----------------------------------+
u_AEcoGrvLog(cCodInt,cDescInt,dDtaInt,cHrIni,Time(),cStaLog,nQtdInt,aMsgErro,cThread,2)    

Return Nil 

/**************************************************************************************************/
/*/{Protheus.doc} AECOINT15
	@description	Rotina consulta e envia categorias para a pataforma e-Commerce
	@author			Bernard M.Margarido
	@version   		1.00
	@since     		13/06/2023
/*/
/**************************************************************************************************/
Static Function AECOINT15()
Local aArea		:= GetArea()

Local cName     := ""
Local cCod      := ""
Local cDesc     := ""
Local cAlias	:= GetNextAlias()

Local nIdGrp    := 0
Local nIdCpo    := 0
Local nIdCat    := 0 
Local nType     := 0
Local nToReg	:= 0
Local nRecno    := 0 

Local oJSon 	:= Nil 

//-----------------------------------------------+
// Valida se existem categorias a serem enviadas |
//-----------------------------------------------+
If !AEcoQry(cAlias,@nToReg)
	LogExec("NAO EXISTEM REGISTROS PARA SEREM ENVIADOS.")
	RestArea(aArea)
	Return .T.
EndIf

//-------------------------------+
// Inicia o envio das categorias |
//-------------------------------+
ProcRegua(nToReg)
While (cAlias)->( !Eof() )
	
    //-----------------------------------+
    // Incrementa regua de processamento |
    //-----------------------------------+
    IncProc("Grupo Especificos " + (cAlias)->WS7_ITEM + " " + (cAlias)->WS7_CAMPO )

    nIdGrp              := (cAlias)->WS5_IDECO
    nIdCpo              := (cAlias)->WS7_IDECO
    nIdCat              := (cAlias)->AY0_XIDCAT
    nType               := Val((cAlias)->WS7_TPCPO)
    nRecno              := (cAlias)->RECNOWS7

    cCod                := (cAlias)->WS7_ITEM
    cDesc               := (cAlias)->WS7_CAMPO
    cName               := (cAlias)->DESCECO

    //--------------------+
    // Dados da Categoria |
    //--------------------+
    oJSon 				    := Nil 
    oJSon 				    := JSonObject():New()
    
    oJSon["FieldTypeId"]    := nType
    oJSon["FieldGroupId"]   := nIdGrp
    oJSon["CategoryId"]     := nIdCat
    oJSon["Name"]           := RTrim(cName)
    oJSon["IsActive"]       := .T.
        
	cRest				:= oJSon:ToJson()		

    //-----------------------------------------+
    // Rotina realiza o envio para o ecommerce |
    //-----------------------------------------+
    AEcoEnv(cRest,cCod,cDesc,nIdCpo,nRecno)
				
	(cAlias)->( dbSkip() )
		
EndDo

//----------------------------+
// Encerra arquivo temporario |
//----------------------------+
(cAlias)->( dbCloseArea() )

FreeObj(oJSon)

RestArea(aArea)
Return .T.

/************************************************************************************/
/*/{Protheus.doc} AECOENV
	@description	Rotina envia dados da caegoria para a plataforma e-commerce
	@author			Bernard M.Margarido
	@version   		1.00
	@since     		13/06/2023
/*/								
/*************************************************************************************/
Static Function AEcoEnv(cRest,cCod,cDesc,nIdCpo,nRecno)
Local _oVTEX 		:= VTEX():New()
Local _oJSon 		:= Nil 

Private cType		:= ""

LogExec("ENVIANDO GRUPO ESPECIFICOS " + cCod + " - " + RTrim(cDesc) + " ." )

//--------------------------------+
// Cria diretorio caso nao exista |
//--------------------------------+
MakeDir(cDirImp)
MakeDir(cDirImp + cDirSave)
MemoWrite(cDirImp + cDirSave + "\jsoncampos_especificos_" + RTrim(cCod) + ".json",cRest)

//---------------------+
// Parametros de envio | 
//---------------------+
_oVTEX:cMetodo		:= IIF(nIdCpo > 0, "PUT", "POST")
_oVTEX:cJSon		:= cRest
_oVTEX:cID			:= cValToChar(nIdCpo)

If _oVTEX:CampoEspecifico()
	//--------------------+
	// Posiciona Registro |
	//--------------------+
	WS7->( dbGoTo(nRecno) )

	_oJSon := JSonObject():New()
	_oJSon:FromJson(_oVTEX:cJSonRet)
	If ValType(_oJSon) <> "U"
		RecLock("WS7",.F.)
			WS7->WS7_ENVECO := "2"
			WS7->WS7_IDECO	:= _oJSon['Id']
			WS7->WS7_DTEXP	:= Date()
			WS7->WS7_HREXP	:= Time()
		WS7->( MsUnLock() )
		LogExec("GRUPO ESPECIFICO " + cCod + " - " + RTrim(cDesc) + " . ENVIADO COM SUCESSO." )	
	Else
		LogExec("ERRO AO ENVIAR GRUPO ESPECIFICO " + cCod + " - " + RTrim(cDesc) + " . ERRO: " + RTrim(_oVTEX:cError) )
		aAdd(aMsgErro,{cCod,"ERRO AO ENVIAR GRUPO ESPECIFICO " + cCod + " - " + RTrim(cDesc) + " . ERRO: " + RTrim(_oVTEX:cError)}) 
	EndIf
Else 
	cType := '_oVTEX:cError'
	If Type(cType) <> "U"
		LogExec("ERRO AO ENVIAR GRUPO ESPECIFICO " + cCod + " - " + RTrim(cDesc) + " . ERRO: " + RTrim(_oVTEX:cError) )
		aAdd(aMsgErro,{cCod,"ERRO AO ENVIAR GRUPO ESPECIFICO " + cCod + " - " + RTrim(cDesc) + " . ERRO: " + RTrim(_oVTEX:cError) }) 
	Else 
		LogExec("ERRO AO ENVIAR A GRUPO ESPECIFICO " + cCod + " - " + RTrim(cDesc) + " .")
		aAdd(aMsgErro,{cCod,"ERRO AO ENVIAR GRUPO ESPECIFICO " + cCod + " - " + RTrim(cDesc) + " ."}) 
	EndIf 
EndIf 

FreeObj(_oVTEX)
FreeObj(_oJSon)

Return .T.

/**************************************************************************************************/
/*/{Protheus.doc} AECOQRY
	@description 	Rotina consulta e envia categorias para a pataforma e-Commerce
	@author			Bernard M.Margarido
	@version   		1.00
	@since     		13/06/2023
/*/
/**************************************************************************************************/
Static Function AEcoQry(cAlias,nToReg,_cLojaID)
Local cQuery 		:= ""

Default _cLojaID	:= ""

//---------------------------+
// Query consulta categorias |
//---------------------------+
cQuery := "	SELECT " + CRLF
cQuery += "		WS7.WS7_COD, " + CRLF
cQuery += "	    WS7.WS7_ITEM, " + CRLF
cQuery += "	    WS7.WS7_CAMPO, " + CRLF 
cQuery += "	    WS7.WS7_TPCPO, " + CRLF 
cQuery += "	    CAST(CAST(WS7.WS7_DESCEC AS BINARY(1024)) AS VARCHAR(1024)) DESCECO, " + CRLF
cQuery += " 	WS7.WS7_IDECO, " + CRLF
cQuery += " 	WS5.WS5_IDECO, " + CRLF 
cQuery += "	    AY0.AY0_XIDCAT, " + CRLF
cQuery += " 	WS7.R_E_C_N_O_ RECNOWS7 " + CRLF 
cQuery += " FROM " + CRLF
cQuery += " 	" + RetSqlName("WS7") + " WS7 (NOLOCK) " + CRLF
cQuery += " 	INNER JOIN " + RetSqlName("WS5") + " WS5 (NOLOCK) ON WS5.WS5_FILIAL = WS7.WS7_FILIAL AND WS5.WS5_COD = WS7.WS7_COD AND WS5.WS5_ENVECO = '2' AND WS5.WS5_IDECO > 0 AND WS5.D_E_L_E_T_ = '' " + CRLF
cQuery += "	    INNER JOIN " + RetSqlName("AY0") + " AY0 (NOLOCK) ON AY0.AY0_FILIAL = '" + xFilial("AY0") + "' AND AY0.AY0_CODIGO = WS5.WS5_CAT01 AND AY0.D_E_L_E_T_ = '' " + CRLF
cQuery += " WHERE " + CRLF
cQuery += " 	WS7.WS7_FILIAL = '" + xFilial("WS7") + "' AND " + CRLF
cQuery += " 	WS7.WS7_ENVECO = '1' AND " + CRLF
cQuery += " 	WS7.D_E_L_E_T_ = '' " + CRLF
cQuery += " ORDER BY WS7.WS7_COD,WS7.WS7_ITEM "

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
	@param cMsg, characters, descricao
	@type function
/*/
/*********************************************************************************/
Static Function LogExec(cMsg)
	CONOUT(cMsg)
	LjWriteLog(cArqLog,cMsg)
Return .T.

