#INCLUDE "TOTVS.CH"

/****************************************************************************************************/
/*/{Protheus.doc} BPFATM05
    @description Realiza a integração das transportadoras na 4MDG
    @type  Function
    @author Bernard M Margarido
    @since 18/07/2024
    @version version
/*/
/****************************************************************************************************/
User Function BPFATM05()
Local _aArea    := GetArea() 

Local _cIdProc  := "0053"

Local _nTentat  := 0

Private _aMsg   := {}

//-----------------------------+
// Z10 - Rotinas de Integracao |
//-----------------------------+
dbSelectArea("Z10")
Z10->( dbSetOrder(1) )
If !Z10->( dbSeek(xFilial("Z10") + PadR(_cIdProc,TamSx3("Z10_ID")[1])))
    Return Nil 
EndIf 

_nTentat := Z10->Z10_TENTAT

//-------------------------------+
// Integra novas transportadoras |
//-------------------------------+
BPFATM05A(_cIdProc)

//-------------------------------------+
// Cria / Atualiza transportadoras 4MDG |
//-------------------------------------+
BPFATM05B(_cIdProc,_nTentat)

//-----------------------------------------+
// Envia atualização de clientes para 4MDG |
//-----------------------------------------+
If Len(_aMsg)
    U_BPFATM04("SA4",_aMsg)
EndIf 

RestArea(_aArea)
Return Nil 

/********************************************************************************/
/*/{Protheus.doc} BPFATM05A
    @description Realiza a baixa das transportadoras
    @type  Static Function
    @author Bernard M. Margarido
    @since 11/09/2024
    @version version
/*/
/********************************************************************************/
Static Function BPFATM05A(_cIdProc)
Local _cChave   := ""
Local _cError   := ""
Local _cStatus  := "1"

Local _nX       := 0

Local _o4MDG    := BS4MDG():New()
Local _oJSon    := Nil 
Local _oJSonTrp := Nil 

//---------------------------------------+
// Realiza a consulta dos transportadora |
//---------------------------------------+
_o4MDG:cPorta   := "6069"
If _o4MDG:GetTransportador()
    _oJSon := JSonObject():New()
    _oJSon:FromJson(_o4MDG:cJSonRet)
    If ValType(_oJSon) <> "U"
        If ValType(_oJSon) == "A" .Or. ValType(_oJSon) == "J"
            For _nX := 1 To Len(_oJSon)
                If ValType(_oJSon[_nX]['A4_CGC']) <> "U"

                    //--------------+
                    // Atualiza Z12 |
                    //--------------+
                    _cChave     := FwTimeStamp(3,Date()) //IIF(ValType(_oJSon[_nX]['content_id']) <> "U", _oJSon[_nX]['content_id'], _oJSon[_nX]['A4_CGC'])
                    _oJSonTrp   := JSonObject():New()
                    _oJSonTrp   := _oJSon[_nX]
                    U_BzApi01d(_cIdProc,_cChave,_oJSonTrp:ToJson(),_cError,_cStatus)

                EndIf 
            Next _nX 
        EndIf 
    EndIf 
Else
    _lContinua  := .F.
    aAdd(_aEmail,{"",'ERROR: ' + _o4MDG:cError})
EndIf


FreeObj(_oJSon)
FreeObj(_oJSonTrp)
FreeObj(_o4MDG)

Return Nil 

/********************************************************************************/
/*/{Protheus.doc} BPFATM05B
    @description Realiza a gravação da transportadora
    @type  Static Function
    @author Bernard M Margarido
    @since 26/07/2024
    @version version
/*/
/********************************************************************************/
Static Function BPFATM05B(_cIdProc,_nTentat)
Local _cAlias           := ""
Local _cCgc             := ""
Local _cCodigo          := ""
Local _cUUID            := ""
Local _cLinha	        := ""	
Local _cError           := ""

Local _aStruct          := SA4->( dbStruct() )
Local _aInclui          := {"A4_FILIAL","A4_COD","A4_CGC"}
Local _aTransp          := {}
Local _aErro            := {}

Local _lRet             := .T.
Local _lInclui          := .T.

Local _nX               := 0
Local _nOpcA            := 3
Local _nPMun            := 0
Local _nPEst            := 0 
Local _nTCgc            := TamSx3("A4_CGC")[1]

Private lMsErroAuto     := .F.
Private lMsHelpAuto 	:= .T.
Private lAutoErrNoFile 	:= .T.
Private l050Auto        := .T.

If !BPFATM05C(_cIdProc,_nTentat,@_cAlias)
    Return .T.
EndIf 

//-------------------------+
// Tabela de Monitoramento |
//-------------------------+
dbSelectArea("Z12")
Z12->( dbSetOrder(1) )

While (_cAlias)->( !Eof() )

    //--------------------+
    // Posiciona registro |
    //--------------------+
    Z12->( dbGoTo((_cAlias)->RECNOZ12))

    _oJSon := JSonObject():New()
    _oJSon:FromJson(RTrim(Z12->Z12_JSON))

    //---------------+
    // Valida objeto |
    //---------------+
    If ValType(_oJSon) <> "U"
        //-----------------------------------+
        // Valida se é inclusão ou alteração | 
        //-----------------------------------+
        _cUUID      := _oJSon['content_id']
        _cCgc       := PadR(_oJSon['A4_CGC'] , _nTCgc)

        //-------------------------+
        // SA1 - Posiciona Cliente |
        //-------------------------+
        dbSelectArea("SA4")
        SA4->( dbSetOrder(3) )
        If SA4->( dbSeek(xFilial("SA4") + _cCgc) )
            _lInclui    := .F.
            _cCodigo    := SA4->A4_COD
            _nOpcA      := 4
        EndIf 

        //----------------------+
        // Valida se é inclusão |
        //----------------------+
        For _nX := 1 To Len(_aStruct)
            If ( aScan(_aInclui, {|x| RTrim(x) == RTrim(_aStruct[_nX][1])}) == 0 ) 
                _cCpo   := _aStruct[_nX][1]
                If ValType(_oJSon[_cCpo]) <> "U"
                    If !Empty(_oJSon[_cCpo])
                        _xRet   := ""
                        If _aStruct[_nX][2] == "N"
                            _xRet := IIF(ValType(_oJSon[_cCpo]) == "C", Val(_oJSon[_cCpo]), _oJSon[_cCpo])
                        ElseIf _aStruct[_nX][2] == "D"
                            If AT("-",_oJSon[_cCpo]) > 0 
                                _xRet := sTod(StrTran(_oJSon[_cCpo],"-",""))
                            Else 
                                _xRet := cTod(_oJSon[_cCpo])
                            EndIf 
                            _xRet := _oJSon[_cCpo]
                        EndIf 
                        aAdd(_aTransp, {_cCpo,         _xRet,              Nil})
                    EndIf 
                EndIf 
            EndIf 
        Next _nX 

        //-----------------------------+
        // Adiciona dados obrigatorios |
        //-----------------------------+
        If _lInclui
            dbSelectArea("SA4")
            SA4->( dbSetOrder(1) )

            _cCodigo := GetSxeNum("SA4","A4_COD")

            While SA4->( dbSeek(xFilial("SA4") + _cCodigo))
                ConfirmSx8()
                _cCodigo := GetSxeNum("SA4","A4_COD","","1")
            EndDo 
        EndIf

        _nPMun  := aScan(_aTransp,{|x| RTrim(x[1]) == "A4_MUN"})
        _nPEst  := aScan(_aTransp,{|x| RTrim(x[1]) == "A4_EST"})
        _nPPais := aScan(_aTransp,{|x| RTrim(x[1]) == "A4_CODPAIS"})

        aAdd(_aTransp, { "A4_COD"		   , _cCodigo  	                                                                    , Nil })
        aAdd(_aTransp, { "A4_CGC"          , _cCgc                                                                          , Nil })

        If _nPPais > 0 
            _aTransp[_nPPais][2] := "01058"
        EndIf 

        //-------------------------------+    
        // Realiza a gravação do Cliente |
        //-------------------------------+
        _aTransp := FWVetByDic( _aTransp, "SA4" )   
        lMsErroAuto := .F.
        SetFunName('MATA050')
        MsExecAuto({|x,y| Mata050(x,y)}, _aTransp, _nOpcA)

        //---------------------+
        // Erro na Atualização |
        //---------------------+
        If lMsErroAuto
            
            RollBackSx8()

            _cLinha	    := ""	
            _cError     := ""
            
            _lRet       := .F.

            _aErro	    := {}
            _aErro 	    := GetAutoGrLog()

            For _nX := 1 To Len(_aErro)
                _cLinha := _aErro[_nX]
                _cLinha  := StrTran( _cLinha, Chr(13), " " )
                _cLinha  := StrTran( _cLinha, Chr(10), " " )

                If SubStr( _cLinha, 1, 4 ) == 'HELP'
                    _cError += _cLinha + "|"
                EndIf

                If SubStr( _cLinha, 1, 6 ) == 'TABELA'
                    _cError += _cLinha + "|"
                EndIf

                If SubStr( _cLinha, 1, 5 ) == 'AJUDA'
                    _cError += _cLinha + " | "
                EndIf

                If At("< -- Invalido", _aErro[_nX] ) > 0
                    _cError += _aErro[_nX]  + " | "
                EndIf

            Next _nX

            U_BzApi01d(Z12->Z12_ID,Z12->Z12_CHAVE,Z12->Z12_JSON,_cError,"3",4)
            BPFATM05D(Z12->Z12_ID,Z12->Z12_CHAVE,Z12->Z12_JSON,_cError)

        Else

            ConfirmSX8() 

            _lRet       := .T.
            _cError     := "Transportador incluido com sucesso."

            U_BzApi01d(Z12->Z12_ID,Z12->Z12_CHAVE,Z12->Z12_JSON,"","2",4)
            BPFATM05D(Z12->Z12_ID,Z12->Z12_CHAVE,Z12->Z12_JSON,_cError)

        EndIf 

        //--------------+
        // Array de LOG |
        //--------------+
        aAdd(_aMsg,{_lRet,_cUUID,_cError,_cCgc,.F.})
    EndIf 

    (_cAlias)->( dbSkip() )
EndDo 

(_cAlias)->( dbCloseArea() )

Return Nil  


/************************************************************************************/
/*/{Protheus.doc} BPFATM05C
    @description Consulta transportadoras na fila de integração 
    @type  Static Function
    @author Bernard M Margarido
    @since 11/09/2024
    @version version
/*/
/************************************************************************************/
Static Function BPFATM05C(_cIdProc,_nTentat,_cAlias)
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
/*/{Protheus.doc} BPFATM05C
    @description Atualiza historico monitor
    @type  Static Function
    @author Bernard M. Margarido
    @since 04/11/2020
/*/
/***********************************************************************************/
Static Function BPFATM05D(_cIdProc,_cChave,_cJson,_cError)
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
