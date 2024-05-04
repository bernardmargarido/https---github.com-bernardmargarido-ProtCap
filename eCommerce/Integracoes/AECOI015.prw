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

Local cAlias	:= GetNextAlias()
Local _cCodigo	:= ""
Local _cName 	:= ""
Local _cDesc 	:= ""
Local _cRest	:= ""

Local _nTpCampo	:= 0
Local _nIdVTex	:= 0
Local _nIdCateg	:= 0
Local _nIdGrupo := 0 
Local _nRecno	:= 0 

Local nToReg	:= 0

Local _lFiltro 	:= .F.
Local _lAtivo 	:= .F.
Local _lObrig	:= .F.

Local oJSon 	:= Nil 

// tabela ZTE 

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
    IncProc("Campos Especificos " + (cAlias)->ZTE_COD + " " + RTrim((cAlias)->ZTE_NOME) )

	_cCodigo	:= (cAlias)->ZTE_COD
	_cName 		:= RTrim((cAlias)->ZTE_NOME)
	_cDesc 		:= RTrim((cAlias)->ZTE_DESC)

	_nTpCampo	:= (cAlias)->ZTE_TPCAMP
	_nIdVTex	:= (cAlias)->ZTE_IDCAMP
	_nIdCateg	:= (cAlias)->ACU_XIDLV
	_nIdGrupo 	:= (cAlias)->ZTE_IDGRUP
	_nRecno		:= (cAlias)->RECNOZTE

	_lFiltro 	:= (cAlias)->ZTE_FILTRO
	_lAtivo 	:= (cAlias)->ZTE_ATIVO
	_lObrig		:= (cAlias)->ZTE_OBRIGA

    //--------------------+
    // Dados da Categoria |
    //--------------------+
    oJSon 				    := Nil 
    oJSon 				    := JSonObject():New()
    
    oJSon["FieldTypeId"]    := _nTpCampo
    oJSon["FieldGroupId"]   := _nIdGrupo
    oJSon["CategoryId"]     := _nIdCateg
    oJSon["Name"]           := _cName
	oJSon["Description"]    := _cDesc
	oJSon["IsFilter"]    	:= _lFiltro
	oJSon["IsRequired"]    	:= _lObrig
    oJSon["IsActive"]       := _lAtivo
        
	_cRest					:= oJSon:ToJson()		

    //-----------------------------------------+
    // Rotina realiza o envio para o ecommerce |
    //-----------------------------------------+
    AEcoEnv(_cRest,_cCodigo,_cName,_nIdVTex,_nRecno)
				
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
Static Function AEcoEnv(_cRest,_cCodigo,_cName,_nIdVTex,_nRecno)
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

LogExec("ENVIANDO GRUPO ESPECIFICOS " + _cCodigo + " - " + RTrim(_cName) + " ." )

//--------------------------------+
// Cria diretorio caso nao exista |
//--------------------------------+
MakeDir(cDirImp)
MakeDir(cDirImp + cDirSave)
MemoWrite(cDirImp + cDirSave + "\jsoncampos_especificos_" + RTrim(_cCodigo) + ".json",cRest)

//---------------------+
// Parametros de envio | 
//---------------------+
_oVTEX:cMetodo		:= IIF(_nIdVTex > 0, "PUT", "POST")
_oVTEX:cJSon		:= _cRest
_oVTEX:cID			:= cValToChar(_nIdVTex)

If _oVTEX:Especification()
	
	_oJSon := JSonObject():New()
	_oJSon:FromJson(_oVTEX:cJSonRet)
	If ValType(_oJSon) <> "U"

		//--------------------+
		// Posiciona Registro |
		//--------------------+
		ZTE->( dbGoTo(nRecno) )

		RecLock("ZTE",.F.)
			ZTE->ZTE_XINTLV := "2"
			ZTE->ZTE_IDCAMP	:= _oJSon['Id']
		ZTE->( MsUnLock() )

		//----------------+
		// Parametros LOG |
		//----------------+
		cStatus		:= "1"
		cMsgErro	:= ""
		nIDVtex		:= _oJSon['Id']

		LogExec("GRUPO ESPECIFICO " + _cCodigo + " - " + RTrim(_cName) + " . ENVIADO COM SUCESSO." )	
	Else
		
		//----------------+
		// Parametros LOG |
		//----------------+
		cStatus		:= "2"
		cMsgErro	:= RTrim(_oVTEX:cError)
		nIDVtex		:= _nIdVTex

		LogExec("ERRO AO ENVIAR GRUPO ESPECIFICO " + _cCodigo + " - " + RTrim(_cName) + " . ERRO: " + RTrim(_oVTEX:cError) )
		aAdd(aMsgErro,{_cCodigo,"ERRO AO ENVIAR GRUPO ESPECIFICO " + _cCodigo + " - " + RTrim(_cName) + " . ERRO: " + RTrim(_oVTEX:cError)}) 
	EndIf
Else 
	cType := '_oVTEX:cError'
	If Type(cType) <> "U"

		//----------------+
		// Parametros LOG |
		//----------------+
		cStatus		:= "2"
		cMsgErro	:= RTrim(_oVTEX:cError)
		nIDVtex		:= _nIdVTex

		LogExec("ERRO AO ENVIAR GRUPO ESPECIFICO " + _cCodigo + " - " + RTrim(_cName) + " . ERRO: " + RTrim(_oVTEX:cError) )
		aAdd(aMsgErro,{_cCodigo,"ERRO AO ENVIAR GRUPO ESPECIFICO " + _cCodigo + " - " + RTrim(_cName) + " . ERRO: " + RTrim(_oVTEX:cError) }) 
	Else

		//----------------+
		// Parametros LOG |
		//----------------+
		cStatus		:= "2"
		cMsgErro	:= "Sem comunicação com o integrador"
		nIDVtex		:= _nIdVTex

		LogExec("ERRO AO ENVIAR A GRUPO ESPECIFICO " + _cCodigo + " - " + RTrim(_cName) + " .")
		aAdd(aMsgErro,{_cCodigo,"ERRO AO ENVIAR GRUPO ESPECIFICO " + _cCodigo + " - " + RTrim(_cName) + " ."}) 
	EndIf 
EndIf 

//---------------+
// Grava LOG ZT0 |
//---------------+
cChave		:= xFilial("ZTE") + _cCodigo
cPolitica	:= ""
nRegRep		:= 0
nIdLV		:= 0
AEcoGrvLog(cCodInt,cDescInt,cStatus,cMsgErro,cChave,cPolitica,nIDVtex,nTenta,nRegRep,nIdLV)

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
cQuery += "		ZTE.ZTE_COD, " + CRLF
cQuery += "		ZTE.ZTE_TPCAMP, " + CRLFC
cQuery += "		ZTE.ZTE_IDGRUP, " + CRLF
cQuery += "		ZTE.ZTE_NOME, " + CRLF 
cQuery += "		ZTE.ZTE_DESC, " + CRLF 
cQuery += "		ZTE.ZTE_FILTRO, " + CRLF
cQuery += "		ZTE.ZTE_ATIVO, " + CRLF
cQuery += "		ZTE.ZTE_OBRIGA, " + CRLF
cQuery += "		ZTE.ZTE_IDCAMP, " + CRLF
cQuery += "		ZTE.ZTE_SKUPRD, " + CRLF
cQuery += "		ZTE.R_E_C_N_O_ RECNOZTE, " + CRLF
cQuery += "		ACU.ACU_XIDLV " + CRLF
cQuery += " FROM " + CRLF
cQuery += "		" + RetSqlName("ZTE") + " ZTE  (NOLOCK) " + CRLF 
cQuery += "		INNER JOIN " + RetSqlName("ACU") + " ACU (NOLOCK) ON ACU.ACU_FILIAL = '" + xFilial("ACU") + "' AND ACU.ACU_COD = ZTE.ZTE_CATEGO AND ACU.D_E_L_E_T_ = '' " + CRLF
cQuery += " WHERE " + CRLF
cQuery += "		ZTE.ZTE_FILIAL = '" + xFilial("ZTE") + "' AND " + CRLF
cQuery += "		ZTE.ZTE_XINTLV = '1' AND " + CRLF
cQuery += "		ZTE.D_E_L_E_T_ = '' "

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

