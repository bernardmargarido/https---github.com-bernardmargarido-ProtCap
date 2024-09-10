#INCLUDE "TOTVS.CH"

/*******************************************************************************************/
/*/{Protheus.doc} BPFATM04
    @description Realiza o envio da validação dos cadastros
    @type  Function
    @author Bernard M Margarido
    @since 16/07/2024
    @version version
/*/
/*******************************************************************************************/
User Function BPFATM04(_cAlias,_aArray)
Local _cCodigo  := ""
Local _cLoja    := ""

Local _nX       := 0 

Local _o4MDG    := BS4MDG():New()
Local _oJSon    := Nil 

//-------------------------+
// SA1 - Posiciona cliente |
//-------------------------+
If _cAlias == "SA1"
    dbSelectArea("SA1")
    SA1->( dbSetOrder(3) )
//------------------------------+
// SA2 - Posiciona fornecedores |
//------------------------------+    
ElseIf _cAlias == "SA2"
    dbSelectArea("SA2")
    SA2->( dbSetOrder(3) )
//---------------------------------+
// SA4 - Posiciona transportadoras |
//---------------------------------+
ElseIf _cAlias == "SA4"
    dbSelectArea("SA4")
    SA4->( dbSetOrder(3) )
EndIf 

For _nX := 1 To Len(_aArray)

    _cCodigo := ""
    _cLoja   := ""

    If _cAlias == "SA1"
        If SA1->( dbSeek(xFilial("SA1") + _aArray[_nX][4]))
            _cCodigo := SA1->A1_COD 
            _cLoja   := SA1->A1_LOJA 
        EndIf 
    ElseIf _cAlias == "SA2"
        If SA2->( dbSeek(xFilial("SA2") + _aArray[_nX][4]) )
            _cCodigo := SA2->A2_COD
            _cLoja   := SA2->A2_LOJA
        EndIf
    ElseIf _cAlias == "SA4"
        If SA4->( dbSeek(xFilial("SA4") + _aArray[_nX][4]) )
            _cCodigo := SA4->A4_COD
        EndIf
    EndIf 
    
    _oJSon              := Nil 
    _oJSon              := JSonObject():New()
    _oJSon['status']    := _aArray[_nX][1]
    _oJSon['codigo']    := _cCodigo
    _oJSon['loja']      := _cLoja
    _oJSon['content_id']:= _aArray[_nX][2]
    _oJSon['message']   := _aArray[_nX][3]

    _o4MDG:cPorta       := "6069"
    _o4MDG:cJSon        := _oJSon:ToJSon()
    _o4MDG:RetCadastros()

Next _nX 

FreeObj(_oJSon)
FreeObj(_o4MDG)

Return Nil 
