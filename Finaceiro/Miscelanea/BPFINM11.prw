#INCLUDE "PROTHEUS.CH"
#INCLUDE "TOTVS.CH"
#INCLUDE "AARRAY.CH"
#INCLUDE "JSON.CH"

/*****************************************************************************************/
/*/{Protheus.doc} BPFINM11
    @description Realiza o envio de uma transferencia
    @type  Function
    @author Bernard M. Margarido
    @since 29/07/2021
/*/
/*****************************************************************************************/
User Function BPFINM11()
Local _aArea    := GetArea()

    FwMsgRun(,{|| BPFINM11A()},"Aguarde...","Enviando transferencia " + XTQ->XTQ_CODIGO)

RestArea(_aArea)
Return Nil 

/*****************************************************************************************/
/*/{Protheus.doc} BPFINM11A
    @description Envia transferencia pagamentos e-Commerce
    @type  Static Function
    @author Bernard M. Margarido
    @since 29/07/2021
/*/
/*****************************************************************************************/
Static Function BPFINM11A()
Local _cRecID   := ""
Local _cRest    := ""
Local _cMsg     := ""

Local _oJSon    := Nil 
Local _oJSonRet := Nil 
Local _oPagarMe := Nil

//-----------------------+
// Localiza ID recebedor |
//-----------------------+
BPFINM11B(@_cRecID)

//--------------------------------------------+
// Cria interface para envio da Transferencia | 
//--------------------------------------------+
_oPagarMe               := PagarMe():New() 
_oJSon                  := JSonObject():New()
_oJSon["amount"]        := XTQ->XTQ_VALOR * 100
_oJSon["recipient_id"]  := _cRecID
_oJSon["metadata"]      := ""

//-------------------+
// Cria arquivo JSon |
//-------------------+
_cRest := _oJSon:ToJson()

//--------------------------------------+
// Envia transferencia para o eCommerce |
//--------------------------------------+
_oPagarMe:cJson := _cRest
If _oPagarMe:Transferencia()

    If FWJsonDeserialize(_oPagarMe:cRetJSon,@_oJSonRet)
        _cID := _oJSonRet:id
        RecLock("XTQ",.F.)
            XTQ->XTQ_IDTRAN := cValToChar(_cID)
            XTQ->XTQ_STATUS := "2"
        XTQ->( MsUnlock() )

        _cMsg := "Transferencia " + XTQ->XTQ_CODIGO + " enviada com sucesso. ID " + cValToChar(_cID)
    EndIf
Else
    _cMsg := "Erro ao enviar transferencia: " + _oPagarMe:cError
EndIf 

MsgAlert(_cMsg,"Bunzl - Avisos")

Return Nil 

/************************************************************************************/
/*/{Protheus.doc} BPFINM11B
    @description Consulta ID conta recebedor
    @type  Static Function
    @author Bernard M Margarido
    @since 04/04/2023
    @version version
/*/
/************************************************************************************/
Static Function BPFINM11B(_cRecID)
Local _lRet     := .F.

Local _cRest    := ""
Local _oJSon    := Nil 
Local _oPagarMe := PagarMe():New() 

_oPagarMe:cMetodo := "GET"
If _oPagarMe:Recebedor()
    _cRest := _oPagarMe:cRetJSon
    _oJSon := JSonObject():New()
    _oJSon:fromJson(_cRest)
    If ValType(_oJSon) <> "U"
       _cRecID := _oJSon[1]['id']
       _lRet   := .T.
    EndIf 
EndIf 

FreeObj(_oPagarMe)
FreeObj(_oJSon)

Return _lRet 
