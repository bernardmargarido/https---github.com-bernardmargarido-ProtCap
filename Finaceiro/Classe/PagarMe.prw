#INCLUDE "PROTHEUS.CH"
#INCLUDE "TOTVS.CH"
#INCLUDE "AARRAY.CH"
#INCLUDE "JSON.CH"

#DEFINE CRLF CHR(13) + CHR(10)

/***************************************************************************************/
/*/{Protheus.doc} PagarMe
    @description Classe - API's Pagar.Me
    @type  Function
    @author Bernard M. Margarido
    @since 15/07/2021
/*/
/***************************************************************************************/
Class PagarMe 

    Data aHeadOut       As Array 
    
    Data cUrl           As String 
    Data cUser          As String 
    Data cPassword      As String 
    Data cStatus        As String 
    Data cTipo          As String 
    Data cTID           As String 
    Data cRecepientID   As String 
    Data cId            As String
    Data cJSon          As String 
    Data cMetodo        As String 
    Data cRetJSon       As String
    Data cError         As String 

    Data _cCACertPath	As String
	Data _cPassword		As String
	Data _cCertPath		As String 
	Data _cKeyPath		As String

    Data dDtaPayment    As Date 
    Data dDTPayIni      As Date 
    Data dDTPayFim      As Date 

    Data nPage          As Integer
    Data _nSSL2		    As Integer 
	Data _nSSL3		    As Integer
	Data _nTLS1		    As Integer 
	Data _nHSM		    As Integer
	Data _nVerbose	    As Integer 
	Data _nBugs		    As Integer 
	Data _nState	    As Integer 

    Data lCancelado     As Boolean 

    Data oFwRest        As Object 
    Data oRetJSon       As Object

    Method New() Constructor 
    Method GetSSLCache()
    Method Saldo() 
    Method Historico()
    Method Recebivel() 
    Method Transferencia()
    Method Chargebacks()
    Method Cliente()
    Method Pedido()
    Method Order()
    Method Transaction()
    Method Recebedor() 

End Class

/***************************************************************************************/
/*/{Protheus.doc} New
    @description Metodo construtor da classe
    @type  Function
    @author Bernard M. Margarido
    @since 15/07/2021
/*/
/***************************************************************************************/
Method New() Class PagarMe 

    ::cUrl          := GetNewPar("BS_URLPAME","https://api.pagar.me")
    ::cUser         := GetNewPar("BS_USRPAME","ak_test_5LGIcVOFfVd22wnkhYjCJcrAN25hYr")
    ::cPassword     := GetNewPar("BS_PASPAME","x")
    ::cStatus       := ""
    ::cTipo         := ""
    ::cTID          := ""
    ::cRecepientID  := ""
    ::cId           := ""
    ::cJSon         := ""
    ::cMetodo       := ""
    ::cRetJSon      := ""
    ::_cPassword	:= ""
	::_cCertPath	:= "" 
	::_cKeyPath		:= "" 
	::_cCACertPath	:= ""
    ::cError        := ""

    ::dDtaPayment   := "" 
    ::dDTPayIni     := "" 
    ::dDTPayFim     := "" 

    ::nPage         := 1

    ::_nSSL2		:= 0
	::_nSSL3		:= 0
	::_nTLS1		:= 3
	::_nHSM			:= 0
	::_nVerbose		:= 1
	::_nBugs		:= 1
	::_nState	    := 1

    ::lCancelado    := .F.

    ::aHeadOut      := {}

    ::oFwRest       := Nil 
    ::oRetJSon      := Nil 

Return Nil 

/****************************************************************************************/
/*/{Protheus.doc} GetSSLCache
    @description Define o uso em memoria da configuração SSL para integrações SIGEP
    @author Bernard M. Margarido
    @since 06/12/2019
    @version 1.0
    @type function
/*/
/****************************************************************************************/
Method GetSSLCache() Class PagarMe
Local _lRet 	:= .F.

//-------------------------------------+
// Utiliza configurações SSL via Cache |
//-------------------------------------+
If HTTPSSLClient( ::_nSSL2, ::_nSSL3, ::_nTLS1, ::_cPassword, ::_cCertPath, ::_cKeyPath, ::_nHSM, .F. , ::_nVerbose, ::_nBugs, ::_nState)
	_lRet := .T.
EndIf

Return _lRet 

/***************************************************************************************/
/*/{Protheus.doc} Saldo
    @description Metodo - Retorna saldo em conta Pagar.Me
    @type  Function
    @author Bernard M. Margarido
    @since 15/07/2021
/*/
/***************************************************************************************/
Method Saldo() Class PagarMe 
Local _lRet := .T.

//---------------+
// Usa cache SSL |
//---------------+
::GetSSLCache()

//----------------------------------------+
// Array contendo parametros de cabeçalho |
//----------------------------------------+
::aHeadOut  := {}
aAdd(::aHeadOut,"Authorization: Basic " + Encode64(RTrim(::cUser) + ":" + RTrim(::cPassword)) )
aAdd(::aHeadOut,"Content-Type: application/json" )

//-------------------------+
// Instancia classe FwRest |
//-------------------------+
::oFwRest   := FWRest():New(::cUrl)

//---------------------+
// TimeOut do processo |
//---------------------+
::oFwRest:nTimeOut := 600

//-------------------------+
// Metodo a ser consultado |
//-------------------------+
::oFwRest:SetPath("/1/balance")

If ::oFwRest:Get(::aHeadOut)
    ::cRetJSon	:= DecodeUtf8(::oFwRest:GetResult())
    _lRet       := .T.
Else
    ::cRetJSon	:= DecodeUtf8(::oFwRest:GetResult())
    ::oRetJSon	:= xFromJson(::cRetJSon)
    ::cError    := ::oRetJSon[#"errors"][1][#"message"] 
    _lRet   := .F.
EndIf

//-------------------+
// Reseta objeto API |
//-------------------+
FreeObj(::oFwRest)

Return _lRet  

/***************************************************************************************/
/*/{Protheus.doc} Historico
    @description Metodo - Retorna historico de operações
    @type  Function
    @author Bernard M. Margarido
    @since 15/07/2021
/*/
/***************************************************************************************/
Method Historico() Class PagarMe 
Local _lRet := .T.
Return _lRet 

/***************************************************************************************/
/*/{Protheus.doc} Recebivel
    @description Metodo Retorna pagamentos disponiveis 
    @type  Function
    @author Bernard M. Margarido
    @since 15/07/2021
/*/
/***************************************************************************************/
Method Recebivel() Class PagarMe 
Local _lRet     := .T.

Local _cParam   := ""

//---------------+
// Usa cache SSL |
//---------------+
::GetSSLCache()

//----------------------------------------+
// Array contendo parametros de cabeçalho |
//----------------------------------------+
::aHeadOut  := {}
aAdd(::aHeadOut,"Authorization: Basic " + Encode64(RTrim(::cUser) + ":" + RTrim(::cPassword)) )
aAdd(::aHeadOut,"Content-Type: application/json" )

//-------------------------+
// Instancia classe FwRest |
//-------------------------+
::oFwRest   := FWRest():New(::cUrl)

//---------------------+
// TimeOut do processo |
//---------------------+
::oFwRest:nTimeOut := 600

//----------------------+
// Metodo a ser enviado | 
//----------------------+
/*
If !Empty(::dDtaPayment)
    _cParam := "payment_date=" + ::dDtaPayment
EndIf
*/

If Empty(Self:cId)
    _cParam := "created_at=>=" + ::dDTPayIni
    _cParam += "&created_at=<=" + ::dDTPayFim
    _cParam += "&page=" + cValToChar(::nPage)
    ::oFwRest:SetPath("/1/payables?" + _cParam)
Else 
    ::oFwRest:SetPath("/1/payables/" + RTrim(::cId))
EndIf 


If ::oFwRest:Get(::aHeadOut)
    ::cRetJSon	:= DecodeUtf8(::oFwRest:GetResult())
    _lRet       := .T.
Else
    ::cRetJSon	:= DecodeUtf8(::oFwRest:GetResult())
    ::oRetJSon	:= xFromJson(::cRetJSon)
    ::cError    := ::oRetJSon[#"errors"][1][#"message"] 
    _lRet   := .F.
EndIf

//-------------------+
// Reseta objeto API |
//-------------------+
FreeObj(::oFwRest)    

Return _lRet 

/***************************************************************************************/
/*/{Protheus.doc} Transferencia
    @description Metodo - Cria/Consulta/Estorna transferencia
    @type  Function
    @author Bernard M. Margarido
    @since 15/07/2021
/*/
/***************************************************************************************/
Method Transferencia() Class PagarMe 
Local _lRet := .T.

//---------------+
// Usa cache SSL |
//---------------+
::GetSSLCache()

//----------------------------------------+
// Array contendo parametros de cabeçalho |
//----------------------------------------+
::aHeadOut  := {}
aAdd(::aHeadOut,"Authorization: Basic " + Encode64(RTrim(::cUser) + ":" + RTrim(::cPassword)) )
aAdd(::aHeadOut,"Content-Type: application/json" )

//-------------------------+
// Instancia classe FwRest |
//-------------------------+
::oFwRest   := FWRest():New(::cUrl)

//---------------------+
// TimeOut do processo |
//---------------------+
::oFwRest:nTimeOut := 600

::oFwRest:SetPath("/1/transfers")
::oFwRest:SetPostParams(EncodeUtf8(::cJSon))
If ::oFwRest:Post(::aHeadOut)
    ::cRetJSon	:= DecodeUtf8(::oFwRest:GetResult())
    _lRet       := .T.
Else
    ::cRetJSon	:= DecodeUtf8(::oFwRest:GetResult())
    ::oRetJSon	:= xFromJson(::cRetJSon)
    ::cError    := ::oRetJSon[#"errors"][1][#"message"] 
    _lRet   := .F.
EndIf

//-------------------+
// Reseta objeto API |
//-------------------+
FreeObj(::oFwRest)

Return _lRet 

/***************************************************************************************/
/*/{Protheus.doc} Chargebacks
    @description Metodo - Retorna estorno/reembolsos realizados pela Pagar.Me 
    @type  Function
    @author Bernard M. Margarido
    @since 15/07/2021
/*/
/***************************************************************************************/
Method Chargebacks() Class PagarMe 
Local _lRet := .T.

Return _lRet 

/***************************************************************************************/
/*/{Protheus.doc} Cliente
    @description Metodo - Realiza a gravação/atualização e consulta de clientes na Pagar.Me
    @type  Function
    @author Bernard M. Margarido
    @since 15/07/2021
/*/
/***************************************************************************************/
Method Cliente() Class PagarMe
Local _lRet     := .T.

//---------------+
// Usa cache SSL |
//---------------+
::GetSSLCache()

//----------------------------------------+
// Array contendo parametros de cabeçalho |
//----------------------------------------+
::aHeadOut  := {}
aAdd(::aHeadOut,"Authorization: Basic " + Encode64(RTrim(::cUser) + ":" + RTrim(::cPassword)) )
aAdd(::aHeadOut,"Content-Type: application/json" )

//-------------------------+
// Instancia classe FwRest |
//-------------------------+
::oFwRest   := FWRest():New(::cUrl)

//---------------------+
// TimeOut do processo |
//---------------------+
::oFwRest:nTimeOut := 600

//--------------------------------------+
// POST - Realiza a gravação do cliente |
//--------------------------------------+
If ::cMetodo == "POST"

    ::oFwRest:SetPath("/1/customers")
    ::oFwRest:SetPostParams(EncodeUtf8(::cJSon))

    If ::oFwRest:Post(::aHeadOut)
        ::cRetJSon	:= DecodeUtf8(::oFwRest:GetResult())
        _lRet       := .T.
    Else
        ::cRetJSon	:= DecodeUtf8(::oFwRest:GetResult())
        ::oRetJSon	:= xFromJson(::cRetJSon)
        If ValType(::oRetJSon) == "O"
            If At("errors",::cRetJSon) > 0
                ::cError    := ::oRetJSon[#"errors"][1][#"message"] 
            Else 
                ::cError    := ::oRetJSon[#"message"] 
            EndIf
        EndIf
        _lRet   := .F.
    EndIf

//---------------------------------------+
// PUT - Realiza a atulização do cliente |
//---------------------------------------+
ElseIf ::cMetodo == "PUT"

    ::oFwRest:SetPath("/1/customers?customer_id=" +  RTrim(::cId))

    If ::oFwRest:Put(::aHeadOut,EncodeUtf8(::cJSon))
        ::cRetJSon	:= DecodeUtf8(::oFwRest:GetResult())
        _lRet       := .T.
    Else
        ::cRetJSon	:= DecodeUtf8(::oFwRest:GetResult())
        ::oRetJSon	:= xFromJson(::cRetJSon)
        ::cError    := ::oRetJSon[#"errors"][1][#"message"] 
        _lRet   := .F.
    EndIf

//-------------------------------------+
// GET - Realiza a consulta do cliente |
//-------------------------------------+
ElseIf ::cMetodo == "GET"

    ::oFwRest:SetPath("/1/customers?customer_id=" +  RTrim(::cId))

    If ::oFwRest:Get(::aHeadOut)
        ::cRetJSon	:= DecodeUtf8(::oFwRest:GetResult())
        _lRet       := .T.
    Else
        ::cRetJSon	:= DecodeUtf8(::oFwRest:GetResult())
        ::oRetJSon	:= xFromJson(::cRetJSon)
        ::cError    := ::oRetJSon[#"errors"][1][#"message"] 
        _lRet   := .F.
    EndIf

EndIf

//-------------------+
// Reseta objeto API |
//-------------------+
FreeObj(::oFwRest)

Return _lRet 

/***************************************************************************************/
/*/{Protheus.doc} Pedido
    @description Metodo - Envia/consulta pedidos Pagar.Me
    @type  Function
    @author Bernard M. Margarido
    @since 15/07/2021
/*/
/***************************************************************************************/
Method Pedido() Class PagarMe
Local _lRet     := .T.

Local _nX       := 0
//---------------+
// Usa cache SSL |
//---------------+
::GetSSLCache()

//----------------------------------------+
// Array contendo parametros de cabeçalho |
//----------------------------------------+
::aHeadOut  := {}
aAdd(::aHeadOut,"Authorization: Basic " + Encode64(RTrim(::cUser) + ":" + RTrim(::cPassword)) )
aAdd(::aHeadOut,"Content-Type: application/json" )

//-------------------------+
// Instancia classe FwRest |
//-------------------------+
::oFwRest   := FWRest():New(::cUrl)

//---------------------+
// TimeOut do processo |
//---------------------+
::oFwRest:nTimeOut := 600

//--------------------------------------+
// POST - Realiza a gravação do cliente |
//--------------------------------------+
If ::cMetodo == "POST"

    If ::lCancelado
        ::oFwRest:SetPath("/1/payment_links/" +  RTrim(::cId) + "/cancel")
    Else 
        ::oFwRest:SetPath("/1/payment_links")
        If ValType(EncodeUtf8(::cJSon)) <> "U"
            ::oFwRest:SetPostParams(EncodeUtf8(::cJSon))
        Else 
            ::oFwRest:SetPostParams(::cJSon)
        EndIf 
    EndIf

    If ::oFwRest:Post(::aHeadOut)
        If ValType(DecodeUtf8(::oFwRest:GetResult())) <> "U"
            ::cRetJSon	:= DecodeUtf8(::oFwRest:GetResult())
        Else 
            ::cRetJSon	:= ::oFwRest:GetResult()
        EndIf 
        _lRet       := .T.
    Else
        If ValType(DecodeUtf8(::oFwRest:GetResult())) <> "U"
            ::cRetJSon	:= DecodeUtf8(::oFwRest:GetResult())
        Else 
            ::cRetJSon	:= ::oFwRest:GetResult()
        EndIf
        ::oRetJSon	:= xFromJson(::cRetJSon)
        If ValType(::oRetJSon) == "O"
            If At("errors",::cRetJSon) > 0
                If ValType(::oRetJSon[#"errors"]) == "A"
                    For _nX := 1 To Len(::oRetJSon[#"errors"])
                        ::cError    += ::oRetJSon[#"errors"][_nX][#"message"] + CRLF
                    Next _nX 
                Else 
                    ::cError    := ::oRetJSon[#"errors"][1][#"message"] 
                EndIf
            Else 
                ::cError    := ::oRetJSon[#"message"] 
            EndIf
        EndIf
        _lRet   := .F.
    EndIf

//-------------------------------------+
// GET - Realiza a consulta do cliente |
//-------------------------------------+
ElseIf ::cMetodo == "GET"

    ::oFwRest:SetPath("/1/payment_links/" +  RTrim(::cId))

    If ::oFwRest:Get(::aHeadOut)
        ::cRetJSon	:= DecodeUtf8(::oFwRest:GetResult())
        _lRet       := .T.
    Else
        ::cRetJSon	:= DecodeUtf8(::oFwRest:GetResult())
        ::oRetJSon	:= xFromJson(::cRetJSon)
        ::cError    := ::oRetJSon[#"errors"][1][#"message"] 
        _lRet   := .F.
    EndIf

EndIf

//-------------------+
// Reseta objeto API |
//-------------------+
FreeObj(::oFwRest)
Return _lRet 

/***************************************************************************************/
/*/{Protheus.doc} Order
    @description Metodo - Retorna order de pagamentos Pagar.Me
    @type  Function
    @author Bernard M. Margarido
    @since 15/07/2021
/*/
/***************************************************************************************/
Method Order() Class PagarMe 
Local _lRet     := .T.

//---------------+
// Usa cache SSL |
//---------------+
::GetSSLCache()

//----------------------------------------+
// Array contendo parametros de cabeçalho |
//----------------------------------------+
::aHeadOut  := {}
aAdd(::aHeadOut,"Authorization: Basic " + Encode64(RTrim(::cUser) + ":" + RTrim(::cPassword)) )
aAdd(::aHeadOut,"Content-Type: application/json" )

//-------------------------+
// Instancia classe FwRest |
//-------------------------+
::oFwRest   := FWRest():New(::cUrl)

//---------------------+
// TimeOut do processo |
//---------------------+
::oFwRest:nTimeOut := 600

If ::cMetodo == "GET"

    ::oFwRest:SetPath("/1/orders?payment_link_id=" + RTrim(::cId))

    If ::oFwRest:Get(::aHeadOut)
        ::cRetJSon	:= DecodeUtf8(::oFwRest:GetResult())
        _lRet       := .T.
    Else
        ::cRetJSon	:= DecodeUtf8(::oFwRest:GetResult())
        ::oRetJSon	:= xFromJson(::cRetJSon)
        ::cError    := ::oRetJSon[#"errors"][1][#"message"] 
        _lRet   := .F.
    EndIf

EndIf

//-------------------+
// Reseta objeto API |
//-------------------+
FreeObj(::oFwRest)

Return _lRet 

/***************************************************************************************/
/*/{Protheus.doc} Transaction
    @description Metodo - Retorna contas bancárias recebedor 
    @type  Function
    @author Bernard M. Margarido
    @since 15/07/2021
/*/
/***************************************************************************************/
Method Transaction() Class PagarMe
Local _lRet     := .T.

//---------------+
// Usa cache SSL |
//---------------+
::GetSSLCache()

//----------------------------------------+
// Array contendo parametros de cabeçalho |
//----------------------------------------+
::aHeadOut  := {}
aAdd(::aHeadOut,"Authorization: Basic " + Encode64(RTrim(::cUser) + ":" + RTrim(::cPassword)) )
aAdd(::aHeadOut,"Content-Type: application/json" )

//-------------------------+
// Instancia classe FwRest |
//-------------------------+
::oFwRest   := FWRest():New(::cUrl)

//---------------------+
// TimeOut do processo |
//---------------------+
::oFwRest:nTimeOut := 600

If ::cMetodo == "GET"

    ::oFwRest:SetPath("/1/transactions?order_id=" +  RTrim(::cId))

    If ::oFwRest:Get(::aHeadOut)
        ::cRetJSon	:= DecodeUtf8(::oFwRest:GetResult())
        _lRet       := .T.
    Else
        ::cRetJSon	:= DecodeUtf8(::oFwRest:GetResult())
        ::oRetJSon	:= xFromJson(::cRetJSon)
        ::cError    := ::oRetJSon[#"errors"][1][#"message"] 
        _lRet   := .F.
    EndIf

EndIf

//-------------------+
// Reseta objeto API |
//-------------------+
FreeObj(::oFwRest)
Return _lRet 

/***************************************************************************************/
/*/{Protheus.doc} Recebedor
    @description Metodo - Retorna contas bancárias recebedor 
    @type  Function
    @author Bernard M. Margarido
    @since 15/07/2021
/*/
/***************************************************************************************/
Method Recebedor() Class PagarMe 
Local _lRet     := .T.

//---------------+
// Usa cache SSL |
//---------------+
::GetSSLCache()

//----------------------------------------+
// Array contendo parametros de cabeçalho |
//----------------------------------------+
::aHeadOut  := {}
aAdd(::aHeadOut,"Authorization: Basic " + Encode64(RTrim(::cUser) + ":" + RTrim(::cPassword)) )
aAdd(::aHeadOut,"Content-Type: application/json" )

//-------------------------+
// Instancia classe FwRest |
//-------------------------+
::oFwRest   := FWRest():New(::cUrl)

//---------------------+
// TimeOut do processo |
//---------------------+
::oFwRest:nTimeOut := 600

If ::cMetodo == "GET"

    ::oFwRest:SetPath("/1/recipients")

    If ::oFwRest:Get(::aHeadOut)
        ::cRetJSon	:= DecodeUtf8(::oFwRest:GetResult())
        _lRet       := .T.
    Else
        ::cRetJSon	:= DecodeUtf8(::oFwRest:GetResult())
        ::oRetJSon	:= xFromJson(::cRetJSon)
        ::cError    := ::oRetJSon[#"errors"][1][#"message"] 
        _lRet   := .F.
    EndIf

EndIf

//-------------------+
// Reseta objeto API |
//-------------------+
FreeObj(::oFwRest)
Return _lRet 
