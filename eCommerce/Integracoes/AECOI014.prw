#INCLUDE "PROTHEUS.CH"
#INCLUDE "APWEBSRV.CH"
#INCLUDE "TOPCONN.CH"
#INCLUDE "TBICONN.CH"
#INCLUDE "AARRAY.CH"
#INCLUDE "JSON.CH"

#DEFINE CRLF CHR(13) + CHR(10)

Static cCodInt	:= "ZTI"
Static cDescInt	:= "GroupSpecification"
Static cDirImp	:= "/ecommerce/"
Static cDirSave	:= "especificos/"

/**********************************************************************************************/
/*/{Protheus.doc} AECOI014
    @description Realiza a integracao dos Grupos de Campos Especificos 
    @type  Function
    @author Bernard M Margarido
    @since 13/06/2023
    @version version
/*/
/**********************************************************************************************/
User Function AECOI014()
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
Processa({|| AECOINT14() },"Aguarde...","Consultando Grupo de Campos.")


LogExec("FINALIZA INTEGRACAO DE GRUPOS DE CAMPOS ESPECIFICOS COM A VTEX - DATA/HORA: "+DTOC(DATE())+" AS "+TIME())
LogExec(Replicate("-",80))
LogExec("")

Return Nil 

/**************************************************************************************************/
/*/{Protheus.doc} AECOINT14
	@description	Rotina consulta e envia categorias para a pataforma e-Commerce
	@author			Bernard M.Margarido
	@version   		1.00
	@since     		13/06/2023
/*/
/**************************************************************************************************/
Static Function AECOINT14()
Local aArea		:= GetArea()

Local _cCodGrp 	:= ""
Local _cNome	:= ""
Local _cRest	:= ""

Local cAlias	:= GetNextAlias()

Local _nPosition:= 0 
Local _nIdVTex	:= 0
Local _nIdCateg	:= 0
Local _nRecno 	:= 0
Local nToReg	:= 0
 
Local oJSon 	:= Nil 

//------------------------------------------+
// Valida se existem grupo a serem enviados |
//------------------------------------------+
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
    IncProc("Grupo Especificos " + (cAlias)->ZTI_CODIGO + " " + Rtrim((cAlias)->ZTI_DESC) )

	_cCodGrp 	:= (cAlias)->ZTI_CODIGO
	_cNome		:= Rtrim((cAlias)->ZTI_DESC)

	_nIdVTex	:= (cAlias)->ZTI_IDVTX
	_nIdCateg	:= (cAlias)->ACU_XIDLV
	_nPosition 	:= (cAlias)->ZTI_POSIT
	_nRecno 	:= (cAlias)->RECNOZTI
   
    //--------------------+
    // Dados da Categoria |
    //--------------------+
    oJSon 				:= Nil 
    oJSon 				:= JSonObject():New()
		
    oJSon["CategoryId"] := _nIdCateg
    oJSon["Name"]		:= RTrim(_cNome)
    
	If _nIdVTex > 0 
		oJSon["Position"] := _nPosition
	EndIf 

	_cRest				:= oJSon:ToJson()		

    //-----------------------------------------+
    // Rotina realiza o envio para o ecommerce |
    //-----------------------------------------+
    AEcoEnv(_cRest,_cCodGrp,_cNome,_nIdVTex,_nIdCateg,_nRecno)
				
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
Static Function AEcoEnv(_cRest,_cCodGrp,_cNome,_nIdVTex,_nIdCateg,_nRecno)
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

LogExec("ENVIANDO GRUPO ESPECIFICOS " + _cCodGrp + " - " + RTrim(_cNome) + " ." )

//--------------------------------+
// Cria diretorio caso nao exista |
//--------------------------------+
MakeDir(cDirImp)
MakeDir(cDirImp + cDirSave)
MemoWrite(cDirImp + cDirSave + "\jsongrupo_especificos_" + RTrim(_cCodGrp) + ".json",_cRest)

//---------------------+
// Parametros de envio | 
//---------------------+
_oVTEX:cMetodo		:= IIF(_nIdVTex > 0, "PUT", "POST")
_oVTEX:cJSon		:= _cRest
_oVTEX:cID			:= cValToChar(_nIdVTex)

If _oVTEX:GroupEspecification()

	_oJSon := JSonObject():New()
	_oJSon:FromJson(_oVTEX:cJSonRet)
	If ValType(_oJSon) <> "U"

		//--------------------+
		// Posiciona Registro |
		//--------------------+
		ZTI->( dbGoTo(_nRecno) )

		RecLock("ZTI",.F.)
			ZTI->ZTI_INTLV := "2"
			ZTI->ZTI_IDVTX	:= _oJSon['Id']
			ZTI->ZTI_POSIT	:= _oJSon['Position']
		ZTI->( MsUnLock() )

		//----------------+
		// Parametros LOG |
		//----------------+
		cStatus		:= "1"
		cMsgErro	:= ""
		nIDVtex		:= _oJSon['Id']

		LogExec("GRUPO ESPECIFICO " + _cCodGrp + " - " + RTrim(_cNome) + " . ENVIADO COM SUCESSO." )	

	Else

		//----------------+
		// Parametros LOG |
		//----------------+
		cStatus		:= "2"
		cMsgErro	:= RTrim(_oVTEX:cError)
		nIDVtex		:= _nIdVTex

		LogExec("ERRO AO ENVIAR GRUPO ESPECIFICO " + _cCodGrp + " - " + RTrim(_cNome) + " . ERRO: " + RTrim(_oVTEX:cError) )
		aAdd(aMsgErro,{_cCodGrp,"ERRO AO ENVIAR GRUPO ESPECIFICO " + _cCodGrp + " - " + RTrim(_cNome) + " . ERRO: " + RTrim(_oVTEX:cError)}) 
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

		LogExec("ERRO AO ENVIAR GRUPO ESPECIFICO " + _cCodGrp + " - " + RTrim(_cNome) + " . ERRO: " + RTrim(_oVTEX:cError) )
		aAdd(aMsgErro,{_cCodGrp,"ERRO AO ENVIAR GRUPO ESPECIFICO " + _cCodGrp + " - " + RTrim(_cNome) + " . ERRO: " + RTrim(_oVTEX:cError) }) 
	Else 

		//----------------+
		// Parametros LOG |
		//----------------+
		cStatus		:= "2"
		cMsgErro	:= "Sem comunicação com o integrador"
		nIDVtex		:= _nIdVTex

		LogExec("ERRO AO ENVIAR A GRUPO ESPECIFICO " + _cCodGrp + " - " + RTrim(_cNome) + " .")
		aAdd(aMsgErro,{_cCodGrp,"ERRO AO ENVIAR GRUPO ESPECIFICO " + _cCodGrp + " - " + RTrim(_cNome) + " ."}) 
	EndIf 
EndIf 

//---------------+
// Grava LOG ZT0 |
//---------------+
cChave		:= xFilial("ZTI") + _cCodGrp
cPolitica	:= ""
nRegRep		:= 0
nIdLV		:= 0
nTenta		:= 1
//U_AEcoGrvLog(cCodInt,cDescInt,cStatus,cMsgErro,cChave,cPolitica,nIDVtex,nTenta,nRegRep,nIdLV)

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
Static Function AEcoQry(cAlias,nToReg)
Local cQuery 		:= ""

//---------------------------+
// Query consulta categorias |
//---------------------------+
cQuery := "	SELECT " + CRLF
cQuery += "		ZTI.ZTI_CODIGO, " + CRLF
cQuery += "		ZTI.ZTI_DESC, " + CRLF
cQuery += "		ZTI.ZTI_IDVTX, " + CRLF
cQuery += "		ZTI.ZTI_POSIT, " + CRLF
cQuery += "		COALESCE(ACU.ACU_XIDLV,0) ACU_XIDLV, " + CRLF
cQuery += "		ZTI.R_E_C_N_O_ RECNOZTI " + CRLF
cQuery += " FROM  " + CRLF
cQuery += "		" + RetSqlName("ZTI") + " ZTI (NOLOCK) " + CRLF 
cQuery += "		LEFT JOIN " + RetSqlName("ACU") + " ACU (NOLOCK) ON ACU.ACU_FILIAL = '" + xFilial("ACU") + "' AND ACU.ACU_COD = ZTI.ZTI_CATEG AND ACU.D_E_L_E_T_ = '' " + CRLF
cQuery += " WHERE " + CRLF
cQuery += "		ZTI_FILIAL = '" + xFilial("ZTI") + "' AND " + CRLF
cQuery += "		ZTI_INTLV = '1' AND " + CRLF
cQuery += "		ZTI.D_E_L_E_T_ = '' "

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

