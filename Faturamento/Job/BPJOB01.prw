#INCLUDE "PROTHEUS.CH"

/******************************************************************************/
/*/{Protheus.doc} BPJOB01
    @descrption JOB - Integração pre cadastro 4MDG 
    @type  Function
    @author Bernard M. Margarido
    @since 10/09/2024
    @version version
/*/
/******************************************************************************/
User Function BPJOB01(_cEmpInt,_cFilInt) 
Local _aArea        := GetArea()

Private _lJob       := IIF(!Empty(_cEmpInt) .And. !Empty(_cFilInt), .T., .F.)

Default _cEmpInt   := "01"
Default _cFilInt   := "00"

If _lJob
    RpcSetType(3)
	RpcSetEnv(_cEmpInt, _cFilInt,,,'FAT')
EndIf

BPJOB01A()

If _lJob
    RpcClearEnv()
EndIf    

RestArea(_aArea)
Return .T.

/******************************************************************************/
/*/{Protheus.doc} BPJOB01A
    @description Realiza envio do pre cadastro 4MDG
    @type  Static Function
    @author Bernard M. Margarido
    @since 10/09/2024
    @version version
/*/
/******************************************************************************/
Static Function BPJOB01A()
Local _cAlias   := ""
Local _cIdProc  := "0048"

Local _nTentat	:= 0

Local _o4MDG    := BS4MDG():New() 

//-----------------------------+
// Z10 - Rotinas de Integracao |
//-----------------------------+
dbSelectArea("Z10")
Z10->( dbSetOrder(1) )
If !Z10->( dbSeek(xFilial("Z10") + PadR(_cIdProc,TamSx3("Z10_ID")[1])))
    Return Nil 
EndIf 

_nTentat := Z10->Z10_TENTAT

//-----------------------------------+
// Consulta dados na fila para envio |
//-----------------------------------+
If !BPJOB01B(_cIdProc,_nTentat,@_cAlias) 
    Return Nil 
EndIf 

//--------------------------+
// Z10 - Monitor de Rotinas |
//--------------------------+
dbSelectArea("Z12")
Z12->( dbSetOrder(1) )

While (_cAlias)->( !Eof() )
    //--------------------+
    // Posiciona registro |
    //--------------------+
    Z12->( dbGoTo((_cAlias)->RECNOZ12))

    //------------------------+
    // Envia dados para 4MDG  |
    //------------------------+
    _o4MDG:cPorta   := "6069"
    _o4MDG:cJson    := RTrim(Z12->Z12_JSON)
    _o4MDG:cBusID   := RTrim(Z12->Z12_CHAVE)
    If _o4MDG:PostCliente()
        U_BzApi01d(Z12->Z12_ID,Z12->Z12_CHAVE,Z12->Z12_JSON,"","2",4)
        BPJOB01C(Z12->Z12_ID,Z12->Z12_CHAVE,Z12->Z12_JSON,"")
    Else
        U_BzApi01d(Z12->Z12_ID,Z12->Z12_CHAVE,Z12->Z12_JSON,_o4MDG:cError,"3",4) 
        BPJOB01C(Z12->Z12_ID,Z12->Z12_CHAVE,Z12->Z12_JSON,_o4MDG:cError)
    EndIf

    (_cAlias)->( dbSkip() )
EndDo 

(_cAlias)->( dbCloseArea() )

FreeObj(_o4MDG)
Return Nil 

/******************************************************************************/
/*/{Protheus.doc} BPJOB01B
    @description Consulta dados na fila de integração 
    @type  Static Function
    @author Bernard M Margarido
    @since 10/09/2024
    @version version
/*/
/******************************************************************************/
Static Function BPJOB01B(_cIdProc,_nTentat,_cAlias) 
Local _cQuery   := ""

_cQuery := " SELECT " + CRLF
_cQuery += "    Z12_ID, " + CRLF
_cQuery += "	Z12_CHAVE, " + CRLF
_cQuery += "	Z12_SEQ, " + CRLF
_cQuery += "    R_E_C_N_O_ RECNOZ12 " + CRLF
_cQuery += " FROM " + CRLF
_cQuery += "	" + RetSqlName("Z12") + " " + CRLF
_cQuery += " WHERE " + CRLF
_cQuery += "	Z12_FILIAL = '" + xFilial("Z12") + "' AND " + CRLF
_cQuery += "	Z12_CHAVE <> '' AND " + CRLF
_cQuery += "	Z12_ID = '" + _cIdProc + "' AND " + CRLF
_cQuery += "	Z12_STPROC = '1' AND " + CRLF
If _nTentat > 0
    _cQuery += "	Z12_TENTAT <= " +Alltrim(Str(_nTentat))+ " AND " + CRLF
EndIf
_cQuery += "	D_E_L_E_T_ = '' "

_cAlias := MPSysOpenQuery(_cQuery)

If (_cAlias)->( Eof() )
    (_cAlias)->( dbCloseArea() )
    Return .F.
EndIf 
    
Return .T.

/***********************************************************************************/
/*/{Protheus.doc} BPJOB01C
    @description Atualiza historico monitor
    @type  Static Function
    @author Bernard M. Margarido
    @since 04/11/2020
/*/
/***********************************************************************************/
Static Function BPJOB01C(_cIdProc,_cChave,_cJson,_cError)
	Local _lRet     := .T.

	Local _oMonitor := ProtMonitor():New()

	_oMonitor:cIdProc   := _cIdProc
	_oMonitor:cChave    := _cChave
	_oMonitor:cError    := _cError
	_oMonitor:cJSon     := _cJson

	If _oMonitor:GrvHistorico()
		_lRet     := .T.
	Else
		_lRet     := .F.
	EndIf

Return _lRet
