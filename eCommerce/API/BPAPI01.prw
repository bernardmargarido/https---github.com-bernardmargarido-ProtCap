#INCLUDE "PROTHEUS.CH"
#INCLUDE "TOTVS.CH"
#INCLUDE "RESTFUL.CH"
#INCLUDE "FWMVCDEF.CH"

#DEFINE CRLF CHR(13) + CHR(10)

//----------------+
// Dummy function |
//----------------+
User Function BpApi01()
Return Nil 

/**********************************************************************************************************/
/*/{Protheus.doc} BUNZL - Custumers B2B
    @description API - Informa se existem novos clientes a serem integrados pelo B2B VTEX
    @type  Function
    @author Bernard M. Margarido
    @since 30/06/2023
/*/
/**********************************************************************************************************/
WSRESTFUL CustomerB2B DESCRIPTION "Informa se existem clientes B2B VTEX."

    //WSDATA documentId AS STRING OPTIONAL  
    
    WSMETHOD POST CustomerB2B ;
    DESCRIPTION "Informa se exisyem novos clientes B2B VTEX." ;
    WSSYNTAX "/CustomerB2B" ;
    PATH "/CustomerB2B";
    PRODUCES APPLICATION_JSON
    
    
ENDWSRESTFUL

/**********************************************************************************************************/
/*/{Protheus.doc} BUNZL - Custumers B2B
    @description API - Informa se existem novos clientes a serem integrados pelo B2B VTEX
    @type  Function
    @author Bernard M. Margarido
    @since 30/06/2023
/*/
/**********************************************************************************************************/
WSMETHOD POST CustomerB2B  WSSERVICE CustomerB2B
Local _aArea        := GetArea()

Local _cJSon        := IIF(ValType(Self:GetContent()) <> "U", Self:GetContent() , "")
Local _lRet         := .T.
Local _lGrava       := .T.

Local _oJSon        := Nil 

Self:SetContentType("application/json")

CoNout('<< CustomerB2B - POST >> JSON ' + _cJSon )

If !Empty(_cJSon)

    _oJSon          := JSonObject():New() 
    _oJSon:FromJson(_cJSon)
    If ValType(_oJSon) <> "U"
        _cDocumentID    := _oJSon['corporateDocument']
    EndIf 

    CoNout('<< CustomerB2B - POST >> BODY ' + _cDocumentID )

    If AT(".",_cDocumentID) > 0 
        If Len(_cDocumentID) > 14
            _cDocumentID := StrTran(_cDocumentID,".","")
            _cDocumentID := StrTran(_cDocumentID,"-","")
            _cDocumentID := StrTran(_cDocumentID,"/","")
        Else
            _cDocumentID := StrTran(_cDocumentID,".","")
            _cDocumentID := StrTran(_cDocumentID,"-","")
            _cDocumentID := StrTran(_cDocumentID,"/","")
        EndIf 
    EndIf 

    FreeObj( _oJSon )
    
EndIf 

If !Empty(_cDocumentID)
    //--------------------------------------+
    // XTF - Posiciona fila de clientes B2B |
    //--------------------------------------+
    dbSelectArea("XTF")
    XTF->( dbSetOrder(2) )
    If XTF->( dbSeek(xFilial("XTF") + _cDocumentID) )
        _lGrava := .F.
        _cCodigo:= XTF->XTF_COD
    Else 
        _cCodigo:= GetSxeNum("XTF","XTF_COD")
    EndIf 

    RecLock("XTF",_lGrava)
        XTF->XTF_FILIAL := xFilial("XTF")
        XTF->XTF_COD    := _cCodigo 
        XTF->XTF_DOCID  := _cDocumentID
        XTF->XTF_DATA   := ""
    XTF->( MsUnlock() )

EndIf 

Self:SetResponse( IIF(_lRet, "true", "false") )

RestArea(_aArea)
Return .T.
