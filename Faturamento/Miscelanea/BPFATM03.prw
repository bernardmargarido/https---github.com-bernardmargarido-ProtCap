#INCLUDE "TOTVS.CH"

/************************************************************************************/
/*/{Protheus.doc} BPFATM03
    @description Realiza a integração de clientes 4MDG
    @type  Function
    @author Bernard M Margarido
    @since 08/07/2024
    @version version
/*/
/************************************************************************************/
User Function BPFATM03()
Local _aArea    := GetArea() 
Local _aEmail   := {}

Local _nX       := 0
//Local _nCount   := 0
//Local _nLimite  := GetMv("BS_GET4MDG",,10)

Local _lContinua:= .T.

Local _o4MDG    := BS4MDG():New()
Local _oJSon    := Nil 

Private _aMsg   := {}

//---------------------------------+
// Realiza a consulta dos clientes |
//---------------------------------+
_o4MDG:cPorta   := "6069"
If _o4MDG:GetCliente()
    _oJSon := JSonObject():New()
    _oJSon:FromJson(_o4MDG:cJSonRet)
    If ValType(_oJSon) <> "U"
        If ValType(_oJSon) == "A" .Or. ValType(_oJSon) == "J"
            For _nX := 1 To Len(_oJSon)
                If ValType(_oJSon[_nX]['A1_CGC']) == "U" .And. _oJSon[_nX]['A1_TIPO'] <> 'X'
                    _lContinua  := .F.
                    aAdd(_aEmail,{"","Não existem clientes a serem processado."})
                Else 
                    //------------------------+
                    // Cria/Atualiza clientes |
                    //------------------------+
                    BPFATM03A(_oJSon[_nX],@_lContinua)
                EndIf 
            Next _nX 
        EndIf 
    EndIf 
Else
    _lContinua  := .F.
    aAdd(_aEmail,{"",'ERROR: ' + _o4MDG:cError})
EndIf

FreeObj(_oJSon)
FreeObj(_o4MDG)

//-----------------------------------------+
// Envia atualização de clientes para 4MDG |
//-----------------------------------------+
If Len(_aMsg)
    U_BSFATM22("SA1",_aMsg)
EndIf 

RestArea(_aArea)
Return Nil 

/************************************************************************************/
/*/{Protheus.doc} BPFATM03A
    @description Cria/Atualiza cliente 4MDG
    @type  Static Function
    @author Bernard M Margarido
    @since 08/07/2024
    @version version
/*/
/************************************************************************************/
Static Function BPFATM03A(_oJSon,_lContinua)
Local _cCodTab          := GetNewPar("BS_TABPLAB")
Local _cCodNat          := GetNewPar("BS_NATCLI")
Local _cGrpVen          := GetNewPar("BS_GRPVEN")
Local _cCgc             := ""
Local _cStatic          := "S"+"t"+"a"+"t"+"i"+"c"+"C"+"a"+"l"+"l"
Local _cCodigo          := ""
Local _cLoja            := ""
Local _cUUID            := ""
Local _cLinha	        := ""	
Local _cError           := ""
Local _cNif             := ""

Local _xRet             := Nil              

Local _aStruct          := SA1->( dbStruct() )
Local _aAltera          := {"A1_FILIAL","A1_COD","A1_LOJA","A1_CGC","A1_PESSOA"}
//Local _aInlcui          := {"A1_FILIAL","A1_COD","A1_LOJA","A1_CGC"}
Local _aCliente         := {}
Local _aErro            := {}

Local _lRet             := .T.
Local _lInclui          := .T.
Local _lEx              := .F.
Local _nPMun            := 0
Local _nPEst            := 0
Local _nPTipo           := 0
Local _nX               := 0
Local _nOpcA            := 3
Local _nTCgc            := TamSx3("A1_CGC")[1]

Private lMsErroAuto     := .F.
Private lMsHelpAuto 	:= .T.
Private lAutoErrNoFile 	:= .T.
Private l030Auto        := .T.

//-----------------------------------+
// Valida se é inclusão ou alteração | 
//-----------------------------------+
//_lInclui    := IIF(_oJSon['Tipo_cadastro'] == "I", "I", "A")
_cUUID      := _oJSon['content_id']
_cCgc       := PadR(_oJSon['A1_CGC'] , _nTCgc)
_cNif       := _oJSon['A1_NIF']
_lEx        := IIF(_oJSon['A1_TIPO'] == "X", .T., .F.) 
//-------------------------+
// SA1 - Posiciona Cliente |
//-------------------------+
If _lEx
    BPFATM03C(_cNif,@_lInclui,@_cCodigo,@_cLoja,@_nOpcA)
Else 
    dbSelectArea("SA1")
    SA1->( dbSetOrder(3) )
    If SA1->( dbSeek(xFilial("SA1") + _cCgc) )
        _lInclui    := .F.
        _cCodigo    := SA1->A1_COD
        _cLoja      := SA1->A1_LOJA 
        _nOpcA      := 4
    EndIf 
EndIf 
//----------------------+
// Valida se é inclusão |
//----------------------+
For _nX := 1 To Len(_aStruct)
    If ( aScan(_aAltera, {|x| RTrim(x) == RTrim(_aStruct[_nX][1])}) == 0 ) 
        _cCpo   := _aStruct[_nX][1]
        If ValType(_oJSon[_cCpo]) <> "U"
            If !Empty(_oJSon[_cCpo])
                _xRet   := ""
                If _aStruct[_nX][2] == "N"
                    _xRet := IIF(ValType(_oJSon[_cCpo]) == "C", Val(_oJSon[_cCpo]), _oJSon[_cCpo])
                ElseIf _aStruct[_nX][2] == "D"
                    _xRet := sTod(StrTran(_oJSon[_cCpo],"-",""))
                ElseIf RTrim(_aStruct[_nX][1]) == "A1_CNAE"
                    _xRet := IIF(AT("/",_oJSon[_cCpo]) > 0, _oJSon[_cCpo], Transform(_oJSon[_cCpo],"@R ####-#/##"))
                Else 
                    _xRet := _oJSon[_cCpo]
                EndIf 
                aAdd(_aCliente, {_cCpo,         _xRet,              Nil})
            EndIf 
        EndIf 
    EndIf 
Next _nX 

//----------------------------+
// Posiciona dados prenchidos |
//----------------------------+
_nPMun  := aScan(_aCliente,{|x| RTrim(x[1]) == "A1_MUN"})
_nPEst  := aScan(_aCliente,{|x| RTrim(x[1]) == "A1_EST"})
_nPTipo := aScan(_aCliente,{|x| RTrim(x[1]) == "A1_TIPO"})

//-----------------------------+
// Adiciona dados obrigatorios |
//-----------------------------+
If _lInclui
    Eval( {|| &(_cStatic + "(" + "BSFATM19, BSFATM19D, 'SA1',IIF(_lEx,_cNif,_cCgc), @_cCodigo, @_cLoja, .F., _lEx" + ")") })

    aAdd(_aCliente, { "A1_NATUREZ"      , _cCodNat                                                                       , Nil })

    //---------------------+
    // Cliente estrangeiro |
    //---------------------+
    If _lEx
        aAdd(_aCliente, { "A1_CODPAIS"      , "01600"                                                                       , Nil }) 
        aAdd(_aCliente, { "A1_PAIS"         , "160"                                                                         , Nil }) 
        aAdd(_aCliente, { "A1_TABELA"       , _cCodTab                                                                      , Nil })
        aAdd(_aCliente, { "A1_XTABPR2"      , _cCodTab                                                                      , Nil })
        aAdd(_aCliente, { "A1_GRPVEN"       , _cGrpVen                                                                      , Nil })
        aAdd(_aCliente, { "A1_CALCSUF"      , "N"                                                                           , Nil })
        aAdd(_aCliente, { "A1_SIMPLES"      , "2"                                                                           , Nil })
        aAdd(_aCliente, { "A1_SIMPNAC"      , "2"                                                                           , Nil })
        aAdd(_aCliente, { "A1_CONTRIB"      , "2"                                                                           , Nil })
        aAdd(_aCliente, { "A1_TPJ"          , "4"                                                                           , Nil })
    EndIf 

    //------------------------+   
    // Cliente pessoal fisica |
    //------------------------+
    If _aCliente[_nPTipo][2] == "F"
        aAdd(_aCliente, { "A1_TABELA"       , _cCodTab                                                                       , Nil })
        aAdd(_aCliente, { "A1_XTABPR2"      , _cCodTab                                                                       , Nil })
        aAdd(_aCliente, { "A1_GRPVEN"       , _cGrpVen                                                                       , Nil })
        aAdd(_aCliente, { "A1_CALCSUF"      , "N"                                                                            , Nil })
        aAdd(_aCliente, { "A1_SIMPLES"      , "2"                                                                            , Nil })
        aAdd(_aCliente, { "A1_SIMPNAC"      , "2"                                                                            , Nil })
        aAdd(_aCliente, { "A1_CONTRIB"      , "2"                                                                            , Nil })
        aAdd(_aCliente, { "A1_TPJ"          , "4"                                                                            , Nil })
        aAdd(_aCliente, { "A1_CODPAIS"      , "01058"                                                                        , Nil }) 
        aAdd(_aCliente, { "A1_PAIS"         , "105"                                                                          , Nil })
    EndIf  

EndIf

aAdd(_aCliente, { "A1_COD"		    , _cCodigo  	                                                                    , Nil })
aAdd(_aCliente, { "A1_LOJA"		    , _cLoja		                                                                    , Nil })
aAdd(_aCliente, { "A1_CGC"          , _cCgc                                                                             , Nil })
aAdd(_aCliente, { "A1_PESSOA"       , IIF(Len(RTrim(_cCgc)) < 14, "F", "J")                                             , Nil })

_xRet   := BPFATM03B(_aCliente[_nPEst][2],_aCliente[_nPMun][2])
If !Empty(_xRet) .And. !_lEx
    aAdd(_aCliente, { "A1_COD_MUN"      , _xRet                                                                         , Nil })
EndIf 
//-------------------------------+    
// Realiza a gravação do Cliente |
//-------------------------------+
_aCliente := FWVetByDic( _aCliente, "SA1" )

If MA030IsMVC()
    SetFunName('CRMA980')
    MSExecAuto( { |x, y| CRMA980(x,y) },  _aCliente, _nOpcA )
Else
    SetFunName('MATA030')
    MsExecAuto({|x,y| Mata030(x,y)}, _aCliente, _nOpcA)
EndIf 

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
Else

    ConfirmSX8() 

    _lRet       := .T.
    _cError     := "Cliente incluido/atualizado com sucesso."

    //---------------------------+
    // Executa regras tributação |
    //---------------------------+
    dbSelectArea("SA1")
    SA1->( dbSetOrder(3) )
    If SA1->( dbSeek(xFilial("SA1") + _cCgc))
        Eval( {|| &(_cStatic + "(" + "BSFATM19,BSFATM19J,_lInclui" + ")") })
    EndIf 

EndIf 

//--------------+
// Array de LOG |
//--------------+
aAdd(_aMsg,{_lRet,_cUUID,_cError,IIF(_lEx,_cNif,_cCgc), _lEx})

Return _lRet 

/*******************************************************************************/
/*/{Protheus.doc} BPFATM03B
    @description Realiza a consulta do Codigo de Municipio 
    @type  Static Function
    @author Bernard M Margarido
    @since 05/08/2024
    @version version
/*/
/*******************************************************************************/
Static Function BPFATM03B(_cEst,_cMun)
Local _cCodMun  := ""
Local _cStatic  := "S"+"t"+"a"+"t"+"i"+"c"+"C"+"a"+"l"+"l"

_cCodMun  := Eval( {|| &(_cStatic + "(" + "AECOI011, EcCodMun, _cEst, _cMun" + ")") })

Return _cCodMun

/************************************************************************************/
/*/{Protheus.doc} BPFATM03C
    @description Consulta cliente estrangeiro
    @type  Static Function
    @author Bernard M Margarido
    @since 07/08/2024
    @version version
/*/
/************************************************************************************/
Static Function BPFATM03C(_cNIF,_lInclui,_cCodigo,_cLoja,_nOpcA)
Local _cQuery := ""
Local _cAlias := ""

_cQuery := " SELECT " + CRLF
_cQuery += "	A1_COD, " + CRLF
_cQuery += "	A1_LOJA, " + CRLF
_cQuery += "	A1_CGC " + CRLF 
_cQuery += " FROM " + CRLF
_cQuery += "	SA1040 " + CRLF
_cQuery += " WHERE " + CRLF
_cQuery += "	A1_FILIAL = '" + xFilial("SA1") + "' AND " + CRLF
_cQuery += "	A1_NIF = '" + _cNIF + "' AND " + CRLF
_cQuery += "	D_E_L_E_T_ = '' " 

_cAlias := MPSysOpenQuery(_cQuery)

_lInclui    := .T.
_cCodigo    := ""
_cLoja      := ""
_nOpcA      := 3

If !Empty((_cAlias)->A1_COD)
    _lInclui    := .F.
    _cCodigo    := (_cAlias)->A1_COD
    _cLoja      := (_cAlias)->A1_LOJA
    _nOpcA      := 4
EndIf 

(_cAlias)->( dbCloseArea() )	

Return Nil 
