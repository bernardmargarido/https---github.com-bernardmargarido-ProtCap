#INCLUDE "TOTVS.CH"

/****************************************************************************************************/
/*/{Protheus.doc} BPCOMM01
    @description Realiza a integração dos fornecedores na 4MDG
    @type  Function
    @author Bernard M Margarido
    @since 18/07/2024
    @version version
/*/
/****************************************************************************************************/
User Function BPCOMM01()
Local _aArea    := GetArea() 

Local _cIdProc  := "0052"

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
BPCOMM01A(_cIdProc)

//-------------------------------------+
// Cria / Atualiza transportadoras 4MDG |
//-------------------------------------+
BPCOMM01B(_cIdProc,_nTentat)

//-----------------------------------------+
// Envia atualização de clientes para 4MDG |
//-----------------------------------------+
If Len(_aMsg)
    U_BPFATM04("SA2",_aMsg)
EndIf 

RestArea(_aArea)
Return Nil 

/********************************************************************************/
/*/{Protheus.doc} BPCOMM01A
    @description Realiza a baixa de fornecedores da 4MDG
    @type  Static Function
    @author Bernard M Margarido
    @since 12/09/2024
    @version version
/*/
/********************************************************************************/
Static Function BPCOMM01A(_cIdProc)
Local _cChave   := ""
Local _cError   := ""
Local _cStatus  := "1"

Local _nX       := 0

Local _o4MDG    := BS4MDG():New()
Local _oJSon    := Nil 
Local _oJSonFor := Nil 

//-------------------------------------+
// Realiza a consulta dos fornecedores |
//-------------------------------------+
_o4MDG:cPorta   := "6069"
If _o4MDG:GetFornecedor()
    _oJSon := JSonObject():New()
    _oJSon:FromJson(_o4MDG:cJSonRet)
    If ValType(_oJSon) <> "U"
        If ValType(_oJSon) == "A" .Or. ValType(_oJSon) == "J"
            For _nX := 1 To Len(_oJSon)
                If ValType(_oJSon[_nX]['A2_CGC']) <> "U" .And. ValType(_oJSon[_nX]['A2_TIPO']) <> "U"  
                    //--------------+
                    // Atualiza Z12 |
                    //--------------+
                    _cChave     := FwTimeStamp(3,Date()) //IIF(ValType(_oJSon[_nX]['content_id']) <> "U", _oJSon[_nX]['content_id'], _oJSon[_nX]['A2_CGC'])
                    _oJSonFor   := JSonObject():New()
                    _oJSonFor   := _oJSon[_nX]
                    U_BzApi01d(_cIdProc,_cChave,_oJSonFor:ToJson(),_cError,_cStatus)
                EndIf 
            Next _nX 
        EndIf 
    EndIf 
EndIf

FreeObj(_oJSon)
FreeObj(_oJSonFor)
FreeObj(_o4MDG)

Return Nil

/********************************************************************************/
/*/{Protheus.doc} BPCOMM01B
    @description Realiza a gravação do fornecedor
    @type  Static Function
    @author Bernard M Margarido
    @since 26/07/2024
    @version version
/*/
/********************************************************************************/
Static Function BPCOMM01B(_cIdProc,_nTentat)
Local _cAlias           := ""
Local _cCgc             := ""
Local _cCodigo          := ""
Local _cLoja            := ""
Local _cUUID            := ""
Local _cLinha	        := ""	
Local _cError           := ""

Local _aStruct          := SA2->( dbStruct() )
Local _aInclui          := {"A2_FILIAL","A2_COD","A2_LOJA","A2_CGC"}
Local _aTPJ             := {"ME","EPP","MEI","Cooperativa"}
Local _aFornecedor      := {}
Local _aErro            := {}

Local _lRet             := .T.
Local _lInclui          := .T.
Local _lEx              := .F.

Local _nX               := 0
Local _nOpcA            := 3
Local _nTCgc            := TamSx3("A2_CGC")[1]

Local _oUrl             := Nil 

Private lMsErroAuto     := .F.
Private lMsHelpAuto 	:= .T.
Private lAutoErrNoFile 	:= .T.
Private l020Auto        := .T.

If !BPCOMM01C(_cIdProc,_nTentat,@_cAlias)
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
        _cCgc       := PadR(_oJSon['A2_CGC'] , _nTCgc)
        _cNifEX     := _oJSon['A2_NIFEX']
        _lEx        := IIF(Upper(_oJSon['A2_TIPO']) == "X", .T., .F.) 

        //----------------------------+
        // SA2 - Posiciona Fornecedor |
        //----------------------------+
        If _lEx
            BPCOMM01D(_cNifEX,@_lInclui,@_cCodigo,@_cLoja,@_nOpcA)
        Else 
            BPCOMM01E(_cCgc,@_cCodigo,@_cLoja,@_nOpcA)
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
                        ElseIf RTrim(_aStruct[_nX][1]) == "A2_TPJ"
                            If ( _nPTPJ := aScan(_aTPJ,{|x| RTrim(x) == RTrim(_oJSon[_cCpo]) }) > 0 )
                                _xRet := cValToChar(_nPTPJ)
                            Else 
                                _xRet := "5"
                            EndIf 
                        ElseIf RTrim(_aStruct[_nX][1]) == "A2_HPAGE"
                            If !Empty(_oJSon[_cCpo])
                                _oUrl := Nil     
                                _oUrl := JSonObject():New()
                                _oUrl:FromJson(_oJSon[_cCpo])
                                _xRet := _oUrl['url']
                            EndIf 
                        Else 
                            _xRet := PadR(_oJSon[_cCpo],TamSx3(_cCpo)[1])
                        EndIf 
                        aAdd(_aFornecedor, {_cCpo,          _xRet                                   , Nil})
                    EndIf 
                EndIf 
            EndIf 
        Next _nX 

        aAdd(_aFornecedor, { "A2_COD"		    , _cCodigo  	                                                                    , Nil })
        aAdd(_aFornecedor, { "A2_LOJA"		    , _cLoja		                                                                    , Nil })
        aAdd(_aFornecedor, { "A2_CGC"           , _cCgc                                                                             , Nil })

        //-------------------------------+    
        // Realiza a gravação do Cliente |
        //-------------------------------+   
        _aFornecedor := FWVetByDic( _aFornecedor, "SA2" )

        lMsErroAuto := .F.
        SetFunName('MATA020')
        MsExecAuto({|x,y| Mata020(x,y)}, _aFornecedor, _nOpcA)

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

            IF Empty(_cError)
                For _nX := 1 To Len(_aErro)
                    _cLinha := _aErro[_nX]
                    _cLinha  := StrTran( _cLinha, Chr(13), " " )
                    _cLinha  := StrTran( _cLinha, Chr(10), " " )

                    _cError += _cLinha + "|"
                Next _nX
            EndIf 

            //--------------------+
            // Atualiza historico |
            //--------------------+
            U_BzApi01d(Z12->Z12_ID,Z12->Z12_CHAVE,Z12->Z12_JSON,_cError,"3",4)
            BPCOMM01G(Z12->Z12_ID,Z12->Z12_CHAVE,Z12->Z12_JSON,_cError)

        Else

            ConfirmSX8() 

            _lRet       := .T.
            _cError     := "Fornecedor incluido com sucesso."

            U_BzApi01d(Z12->Z12_ID,Z12->Z12_CHAVE,Z12->Z12_JSON,"","2",4)
            BPCOMM01G(Z12->Z12_ID,Z12->Z12_CHAVE,Z12->Z12_JSON,_cError)

        EndIf 

        //--------------+
        // Array de LOG |
        //--------------+
        aAdd(_aMsg,{_lRet,_cUUID,_cError,IIF(_lEx,_cNifEX,_cCgc),_lEx})

    EndIf 
    (_cAlias)->( dbSkip() )
EndDo 

Return Nil  

/************************************************************************************/
/*/{Protheus.doc} BPCOMM01D
    @description Consulta cliente estrangeiro
    @type  Static Function
    @author Bernard M Margarido
    @since 07/08/2024
    @version version
/*/
/************************************************************************************/
Static Function BPCOMM01D(_cNIF,_lInclui,_cCodigo,_cLoja,_nOpcA)
Local _cQuery := ""
Local _cAlias := ""

_cQuery := " SELECT " + CRLF
_cQuery += "	A2_COD, " + CRLF
_cQuery += "	A2_LOJA, " + CRLF
_cQuery += "	A2_CGC " + CRLF 
_cQuery += " FROM " + CRLF
_cQuery += "	" + RetSqlName("SA2") + " " + CRLF
_cQuery += " WHERE " + CRLF
_cQuery += "	A2_FILIAL = '" + xFilial("SA1") + "' AND " + CRLF
_cQuery += "	A2_NIFEX = '" + _cNIF + "' AND " + CRLF
_cQuery += "	D_E_L_E_T_ = '' " 

_cAlias := MPSysOpenQuery(_cQuery)

_lInclui    := .T.
_cCodigo    := ""
_cLoja      := ""
_nOpcA      := 3

If !Empty((_cAlias)->A2_COD)
    _lInclui    := .F.
    _cCodigo    := (_cAlias)->A2_COD
    _cLoja      := (_cAlias)->A2_LOJA
    _nOpcA      := 4
EndIf 

(_cAlias)->( dbCloseArea() )	

Return Nil 

/************************************************************************************/
/*/{Protheus.doc} BPCOMM01C
    @description Consulta transportadoras na fila de integração 
    @type  Static Function
    @author Bernard M Margarido
    @since 11/09/2024
    @version version
/*/
/************************************************************************************/
Static Function BPCOMM01C(_cIdProc,_nTentat,_cAlias)
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
/*/{Protheus.doc} BPCOMM01E
@description Consulta Fornecedor pelo CNPJ
@type  Static Function
@author Bernard M. Margarido
@since 06/11/2020
/*/
/***********************************************************************************/
Static Function BPCOMM01E(_cCnpj,_cCodigo,_cLoja,_nOpc)

dbSelectArea("SA2")
SA2->( dbSetOrder(3) )

//---------------+
// Localiza CNPJ |
//---------------+
If SA2->( dbSeek(xFilial("SA2") + _cCnpj))
    BPCOMM01F(_cCnpj,@_cCodigo,@_cLoja,@_nOpc)
//--------------------+
// Localiza Raiz CNPJ |
//--------------------+
ElseIf SA2->( dbSeek(xFilial("SA2") + SubStr(_cCnpj,1,8)))
    BPCOMM01F(SubStr(_cCnpj,1,8),@_cCodigo,@_cLoja,@_nOpc)
//-----------------+
// Novo Fornecedor |
//-----------------+
Else
    _nOpc   := 3
    _cCodigo:= GetSxeNum("SA2","A2_COD")
    _cLoja  := "0000"
    SA2->( dbSetOrder(1) )
    While SA2->( dbSeek(xFilial("SA2") + _cCodigo + _cLoja) )
        ConfirmSx8()
        _cCodigo := GetSxeNum("SA2","A2_COD","",1)
    EndDo
EndIf

Return Nil

/***********************************************************************************/
/*/{Protheus.doc} BPCOMM01F
@description Realiza a consulta dos CNPJ's
@type  Static Function
@author Bernard M. Margarido
@since 06/11/2020
/*/
/***********************************************************************************/
Static Function BPCOMM01F(_cCnpj,_cCodigo,_cLoja,_nOpc)

Local _cQuery   := ""

_cQuery := " SELECT " + CRLF
_cQuery += "	A2_COD CODIGO, " + CRLF
_cQuery += "	MAX(A2_LOJA) LOJA, " + CRLF
_cQuery += "	COUNT(*) TOTAL " + CRLF
_cQuery += " FROM " + CRLF
_cQuery += "	" + RetSqlName("SA2") + " " + CRLF
_cQuery += " WHERE " + CRLF
_cQuery += "	A2_FILIAL = '" + xFilial("SA2") + "' AND " + CRLF

If Len(RTrim(_cCnpj)) < 14
    _cQuery += "	A2_CGC LIKE '%" + _cCnpj + "%' AND " + CRLF
Else
    _cQuery += "	A2_CGC = '" + _cCnpj + "' AND " + CRLF
EndIf
_cQuery += "	D_E_L_E_T_ = '' " + CRLF
_cQuery += " GROUP BY A2_COD "

_cAlias     := MPSysOpenQuery(_cQuery)

_cCodigo    := (_cAlias)->CODIGO
_cLoja      := Soma1((_cAlias)->LOJA)
_nOpc       := 3

(_cAlias)->( dbCloseArea() )

Return Nil

/***********************************************************************************/
/*/{Protheus.doc} BPCOMM01G
    @description Atualiza historico monitor
    @type  Static Function
    @author Bernard M. Margarido
    @since 04/11/2020
/*/
/***********************************************************************************/
Static Function BPCOMM01G(_cIdProc,_cChave,_cJson,_cError)
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
