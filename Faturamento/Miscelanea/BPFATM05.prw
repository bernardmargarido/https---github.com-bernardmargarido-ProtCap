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
Local _aEmail   := {}

Local _nX       := 0
//Local _nCount   := 0
//Local _nLimite  := GetMv("BS_GET4MDG",,10)

Local _lContinua:= .T.

Local _o4MDG    := BS4MDG():New()
Local _oJSon    := Nil 

Private _aMsg   := {}

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
                If ValType(_oJSon[_nX]['A4_CGC']) == "U"
                    _lContinua  := .F.
                    aAdd(_aEmail,{"","Não existem transportadoras a serem processado."})
                Else 
                    //---------------------+
                    // Cria transportadora |
                    //---------------------+
                    BPFATM05A(_oJSon[_nX],@_lContinua)
                EndIf 
            Next _nX 
        EndIf 
    EndIf 
Else
    _lContinua  := .F.
    aAdd(_aEmail,{"",'ERROR: ' + _o4MDG:cError})
EndIf

//-----------------------------------------+
// Envia atualização de clientes para 4MDG |
//-----------------------------------------+
If Len(_aMsg)
    U_BSFATM22("SA4",_aMsg)
EndIf 

FreeObj(_oJSon)
FreeObj(_o4MDG)

RestArea(_aArea)
Return Nil 

/********************************************************************************/
/*/{Protheus.doc} BPFATM05A
    @description Realiza a gravação da transportadora
    @type  Static Function
    @author Bernard M Margarido
    @since 26/07/2024
    @version version
/*/
/********************************************************************************/
Static Function BPFATM05A(_oJSon,_lContinua)
Local _cCgc             := ""
Local _cCodigo          := ""
Local _cUUID            := ""
Local _cLinha	        := ""	
Local _cError           := ""

Local _aStruct          := SA4->( dbStruct() )
Local _aInclui          := {"A4_FILIAL","A4_COD","A4_CGC","A4_COD_MUN"}
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

//-----------------------------------+
// Valida se é inclusão ou alteração | 
//-----------------------------------+
//_lInclui    := IIF(_oJSon['Tipo_cadastro'] == "I", "I", "A")
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
            If !Empty(_oJSon[_cCpo]) .And. _oJSon[_cCpo] <> "X"
                _xRet   := ""
                If _aStruct[_nX][2] == "N"
                    _xRet := IIF(ValType(_oJSon[_cCpo]) == "C", Val(_oJSon[_cCpo]), _oJSon[_cCpo])
                ElseIf _aStruct[_nX][2] == "D"
                    If AT("-",_oJSon[_cCpo]) > 0 
                        _xRet := sTod(StrTran(_oJSon[_cCpo],"-",""))
                    Else 
                        _xRet := cTod(_oJSon[_cCpo])
                    EndIf 
                Else 
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

_xRet   := BPFATM05B(_aTransp[_nPEst][2],_aTransp[_nPMun][2])

If !Empty(_xRet)
    aAdd(_aTransp, { "A4_COD_MUN"      , _xRet                                                                          , Nil })
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
Else

    ConfirmSX8() 

    _lRet       := .T.
    _cError     := "Transportador incluido com sucesso."

EndIf 

//--------------+
// Array de LOG |
//--------------+
aAdd(_aMsg,{_lRet,_cUUID,_cError,_cCgc,.F.})

Return _lRet  

/*******************************************************************************/
/*/{Protheus.doc} BPFATM05B
    @description Realiza a consulta do Codigo de Municipio 
    @type  Static Function
    @author Bernard M Margarido
    @since 05/08/2024
    @version version
/*/
/*******************************************************************************/
Static Function BPFATM05B(_cEst,_cMun)
Local _cCodMun  := ""
Local _cStatic  := "S"+"t"+"a"+"t"+"i"+"c"+"C"+"a"+"l"+"l"

_cCodMun  := Eval( {|| &(_cStatic + "(" + "AECOI011, EcCodMun, _cEst, _cMun" + ")") })

Return _cCodMun
