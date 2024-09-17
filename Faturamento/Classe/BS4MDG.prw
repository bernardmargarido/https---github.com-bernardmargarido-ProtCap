#INCLUDE "TOTVS.CH"

/**********************************************************************************/
/*/{Protheus.doc} BS4MDG
    @description Classe responsavel pelas consultas realizadas a 4mdg
    @author Bernard M Margarido
    @since 19/02/2024
    @version version
/*/
/**********************************************************************************/
Class BS4MDG

    Data cURL       As String 
    Data cPath      As String 
    Data cPorta     As String
    Data cMetodo    As String 
    Data cUser      As String 
    Data cPassWord  As String 
    Data cCompany   As String 
    Data cCompanyN  As String 
    Data cAPIKey    As String
    Data cBusID     As String 
    Data cProcesso  As String 
    Data cBusiness  As String
    Data cJSon      As String 
    Data cJSonRet   As String 
    Data cError     As String 
    Data cMail      As String 
    Data cMailCC    As String 

    Data aMsg       As Array 

    Method New() Constructor
    Method Cliente() 
    Method PostCliente()
    Method GetCliente()
    Method RetCadastros()
    Method GetFornecedor()
    Method GetTransportador()

EndClass

/**********************************************************************************/
/*/{Protheus.doc} New
    @description Metodo contrutor da classe 
    @author Bernard M Margarido
    @since 20/02/2024
    @version version
/*/
/**********************************************************************************/
Method New() Class BS4MDG
    
    Self:cURL       := GetMv("BS_URL4MDG",,"https://nifi.staging.4mdg.com.br")
    Self:cUser      := GetMv("BS_USR4MDG",,"e5c883c1-3617-4cba-a84a-5fdcce5e193d")
    Self:cPassWord  := GetMv("BS_PAS4MDG",,"$2a$11$v1wNrsE/S1J.H9wowzyLe.UohMpe6qCf12CE9eGEuQ5yqCqq4LFSa")
    Self:cCompany   := GetMv("BS_COM4MDG",,"bunzlbrasil")
    Self:cCompanyN  := GetMv("BS_CON4MDG",,"64760614000154")
    Self:cAPIKey    := GetMv("BS_APK4MDG",,"3c41oylDUwDgzOF1z27RCgztaB7UZ9jX4CUaI5CzF0AIX7K7IDg8siJYr1FK1boh")
    Self:cPorta     := "4000"
    Self:cPath      := ""
    Self:cMetodo    := ""
    Self:cJSon      := ""
    Self:cJSonRet   := ""
    Self:cBusID     := ""
    Self:cProcesso  := ""
    Self:cBusiness  := ""
    Self:cError     := ""
    Self:cMail      := ""
    Self:cMailCC    := ""
    
    Self:aMsg       := {}

Return Nil 

/**********************************************************************************/
/*/{Protheus.doc} Cliente
    @description Metodo busca dados dos clientes na MDG
    @author Bernard M Margarido
    @since 20/02/2024
    @version version
/*/
/**********************************************************************************/
Method Cliente() Class BS4MDG
Local _lRet         := .T.

Local _cSetParam    := ""    
Local _cURL         := ""
Local _cXMLHeadRet  := ""
Local _cResponse    := ""
Local _cError       := ""

Local _nStatus      := ""

Local _aHeadOut     := {}

//Local _oFwRest      := Nil 
//Local _oJSon        := Nil 

//---------+
// Headers |
//---------+
aAdd(_aHeadOut, "Content-Type: application/json")
aAdd(_aHeadOut, "user: " + RTrim(Self:cUser))
aAdd(_aHeadOut, "password: " + RTrim(Self:cPassWord))
aAdd(_aHeadOut, "company: " + RTrim(Self:cCompany))

//----------------------------------+
// Instancia classe de consulta API |
//----------------------------------+
//_oFwRest  := FWRest():New(Self:cURL)

//---------------------+
// TimeOut do processo |
//---------------------+
//_oFwRest:nTimeOut := 600

_cSetParam  := "CompanyNumber=" + RTrim(Self:cCompanyN)
_cURL       := Self:cURL + ":" + Self:cPorta + "/api/v1?" + _cSetParam
_cResponse  := HTTPQuote(_cURL, "GET", "", Self:cJSon, 600, _aHeadOut, @_cXMLHeadRet)
_nStatus    := HTTPGetStatus(@_cError)

If ValType(_cResponse) <> "U" 
    If _nStatus >= 200 .And. _nStatus <= 201
        _lRet := .T.
        Self:cJSonRet := DecodeUTF8(_cResponse)
        If Valtype(Self:cJSonRet) == "U"
            Self:cJSonRet := _cResponse
        EndIf 
    ElseIf _nStatus > 300 
        _lRet       := .F.
        Self:cError := "ERROR: " + _cResponse + " " + _cXMLHeadRet
    EndIf 
EndIf 

Return _lRet 

/**********************************************************************************/
/*/{Protheus.doc} PostCliente
    @description Metodo - Responsavel pelo envio do pre cadastro de cliente para a 4MDG
    @author Bernard M Margarido
    @since 03/07/2024
    @version version
/*/
/**********************************************************************************/
Method PostCliente() Class BS4MDG
Local _lRet     := .T.

Local _cSetParam    := ""    
Local _cURL         := ""
Local _cXMLHeadRet  := ""
Local _cResponse    := ""
Local _cError       := ""

Local _nStatus      := ""

Local _aHeadOut     := {}

Local _oJSonRet     := Nil

//---------+
// Headers |
//---------+
aAdd(_aHeadOut, "Content-Type: application/json")
aAdd(_aHeadOut, "X-API-Key: " + RTrim(Self:cAPIKey))

//------------+
// Parametros |
//------------+
_cSetParam  := "business_id=" + RTrim(Self:cBusID) + "&"
_cSetParam  += "PROCESSO=POST_CLIENTES&"
_cSetParam  += "BUSINESS=PROTHEUS"

_cURL       := Self:cURL + ":" + Self:cPorta + "/bunzlbrasil?" + _cSetParam
_cResponse  := HTTPQuote(_cURL, "POST", "", Self:cJSon, 600, _aHeadOut, @_cXMLHeadRet)
_nStatus    := HTTPGetStatus(@_cError)

If ValType(_cResponse) <> "U" 
    If _nStatus >= 200 .And. _nStatus <= 201
        _lRet := .T.
        Self:cJSonRet := DecodeUTF8(_cResponse)
        If Valtype(Self:cJSonRet) == "U"
            Self:cJSonRet := _cResponse
        EndIf 
    ElseIf _nStatus > 300 
        _lRet       := .F.
        _oJSonRet   := JSonObject():New()
        _oJSonRet:FromJSon(_cResponse)
        If ValType(_oJSonRet) <> "U"
            Self:cError := _oJSonRet['mensagem'] + ' - ' + DecodeUTF8(_oJSonRet['mensagem_externa'])
        Else 
            Self:cError := "ERROR: " + DecodeUTF8(_cResponse) + " " + _cXMLHeadRet
        EndIf 
    EndIf 
EndIf 

FreeObj(_oJSonRet)

Return _lRet 

/**********************************************************************************/
/*/{Protheus.doc} GetCliente
    @description Metodo - Realiza a baixa de clientes cadastrados/atualizados 4MDG
    @author Bernard M Margarido
    @since 08/07/2024
    @version version
/*/
/**********************************************************************************/
Method GetCliente() Class BS4MDG
Local _lRet     := .T.

Local _cSetParam    := ""    
Local _cURL         := ""
Local _cXMLHeadRet  := ""
Local _cResponse    := ""
Local _cError       := ""

Local _nStatus      := ""

Local _aHeadOut     := {}

Local _oJSonRet     := Nil 

//---------+
// Headers |
//---------+
aAdd(_aHeadOut, "Content-Type: application/json")
aAdd(_aHeadOut, "X-API-Key: " + RTrim(Self:cAPIKey))

//------------+
// Parametros |
//------------+
_cSetParam  := "PROCESSO=ENVIO_CLIENTES&"
_cSetParam  += "BUSINESS=PROTHEUS&"
_cSetParam  += "COMPANY=BEPI"

_cURL       := Self:cURL + ":" + Self:cPorta + "/bunzlbrasil?" + _cSetParam
_cResponse  := HTTPQuote(_cURL, "GET", "", "", 600, _aHeadOut, @_cXMLHeadRet)
_nStatus    := HTTPGetStatus(@_cError)

If ValType(_cResponse) <> "U" 
    If _nStatus >= 200 .And. _nStatus <= 201
        _lRet := .T.
        Self:cJSonRet := DecodeUTF8(_cResponse)
        If Valtype(Self:cJSonRet) == "U"
            Self:cJSonRet := _cResponse
        EndIf 
    ElseIf _nStatus > 300 
        _lRet       := .F.
        _oJSonRet   := JSonObject():New()
        _oJSonRet:FromJSon(_cResponse)
        If ValType(_oJSonRet) <> "U"
            Self:cError := _oJSonRet['mensagem'] + ' - ' + DecodeUTF8(_oJSonRet['mensagem_externa'])
        Else 
            Self:cError := "ERROR: " + DecodeUTF8(_cResponse) + " " + _cXMLHeadRet
        EndIf 
    EndIf 
EndIf 

FreeObj(_oJSonRet)

Return _lRet 

/**********************************************************************************/
/*/{Protheus.doc} RetCliente
    @description Realiza o envio do status da inclusão/atualização do cliente
    @author Bernard M Margarido
    @since 12/07/2024
    @version version
/*/
/**********************************************************************************/
Method RetCadastros() Class BS4MDG
Local _lRet     := .T.

Local _cSetParam    := ""    
Local _cURL         := ""
Local _cXMLHeadRet  := ""
Local _cResponse    := ""
Local _cError       := ""

Local _nStatus      := ""

Local _aHeadOut     := {}

Local _oJSonRet     := Nil 
//---------+
// Headers |
//---------+
aAdd(_aHeadOut, "Content-Type: application/json")
aAdd(_aHeadOut, "X-API-Key: " + RTrim(Self:cAPIKey))

//------------+
// Parametros |
//------------+
_cSetParam  := "PROCESSO=RESP&"
_cSetParam  += "BUSINESS=PROTHEUS&"
_cSetParam  += "COMPANY=BEPI"

_cURL       := Self:cURL + ":" + Self:cPorta + "/bunzlbrasil?" + _cSetParam
_cResponse  := HTTPQuote(_cURL, "POST", "", EncodeUTF8(Self:cJSon), 600, _aHeadOut, @_cXMLHeadRet)
_nStatus    := HTTPGetStatus(@_cError)

If ValType(_cResponse) <> "U" 
    If _nStatus >= 200 .And. _nStatus <= 201
        _lRet := .T.
    ElseIf _nStatus > 300 
        _lRet       := .F.
        _oJSonRet   := JSonObject():New()
        _oJSonRet:FromJSon(_cResponse)
        If ValType(_oJSonRet) <> "U"
            Self:cError := _oJSonRet['mensagem'] + ' - ' + DecodeUTF8(_oJSonRet['mensagem_externa'])
        Else 
            Self:cError := "ERROR: " + DecodeUTF8(_cResponse) + " " + _cXMLHeadRet
        EndIf 
    EndIf 
EndIf

FreeObj(_oJSonRet)

Return _lRet 

/**********************************************************************************/
/*/{Protheus.doc} GetFornecedor
    @description Metodo - Realiza a baixa de fornecedores cadastrados na 4MDG
    @author Bernard M Margarido
    @since 08/07/2024
    @version version
/*/
/**********************************************************************************/
Method GetFornecedor() Class BS4MDG
Local _lRet     := .T.

Local _cSetParam    := ""    
Local _cURL         := ""
Local _cXMLHeadRet  := ""
Local _cResponse    := ""
Local _cError       := ""

Local _nStatus      := ""

Local _aHeadOut     := {}

Local _oJSonRet     := Nil 

//---------+
// Headers |
//---------+
aAdd(_aHeadOut, "Content-Type: application/json")
aAdd(_aHeadOut, "X-API-Key: " + RTrim(Self:cAPIKey))

//------------+
// Parametros |
//------------+
_cSetParam  := "PROCESSO=ENVIO_FORNECEDORES&"
_cSetParam  += "BUSINESS=PROTHEUS&"
_cSetParam  += "COMPANY=BEPI"

_cURL       := Self:cURL + ":" + Self:cPorta + "/bunzlbrasil?" + _cSetParam
_cResponse  := HTTPQuote(_cURL, "GET", "", "", 600, _aHeadOut, @_cXMLHeadRet)
_nStatus    := HTTPGetStatus(@_cError)

If ValType(_cResponse) <> "U" 
    If _nStatus >= 200 .And. _nStatus <= 201
        _lRet := .T.
        Self:cJSonRet := DecodeUTF8(_cResponse)
        If Valtype(Self:cJSonRet) == "U"
            Self:cJSonRet := _cResponse
        EndIf 
    ElseIf _nStatus > 300 
        _lRet       := .F.
        _oJSonRet   := JSonObject():New()
        _oJSonRet:FromJSon(_cResponse)
        If ValType(_oJSonRet) <> "U"
            Self:cError := _oJSonRet['mensagem'] + ' - ' + DecodeUTF8(_oJSonRet['mensagem_externa'])
        Else 
            Self:cError := "ERROR: " + DecodeUTF8(_cResponse) + " " + _cXMLHeadRet
        EndIf 
    EndIf 
EndIf 

FreeObj(_oJSonRet)

Return _lRet 

/**********************************************************************************/
/*/{Protheus.doc} GetTransportador
    @description Metodo - Realiza a baixa das transportadoras cadastrados na 4MDG
    @author Bernard M Margarido
    @since 08/07/2024
    @version version
/*/
/**********************************************************************************/
Method GetTransportador() Class BS4MDG
Local _lRet     := .T.

Local _cSetParam    := ""    
Local _cURL         := ""
Local _cXMLHeadRet  := ""
Local _cResponse    := ""
Local _cError       := ""

Local _nStatus      := ""

Local _aHeadOut     := {}

Local _oJSonRet     := Nil 

//---------+
// Headers |
//---------+
aAdd(_aHeadOut, "Content-Type: application/json")
aAdd(_aHeadOut, "X-API-Key: " + RTrim(Self:cAPIKey))

//------------+
// Parametros |
//------------+
_cSetParam  := "PROCESSO=ENVIO_TRANSPORTADORAS&"
_cSetParam  += "BUSINESS=PROTHEUS&"
_cSetParam  += "COMPANY=BEPI"

_cURL       := Self:cURL + ":" + Self:cPorta + "/bunzlbrasil?" + _cSetParam
_cResponse  := HTTPQuote(_cURL, "GET", "", "", 600, _aHeadOut, @_cXMLHeadRet)
_nStatus    := HTTPGetStatus(@_cError)

If ValType(_cResponse) <> "U" 
    If _nStatus >= 200 .And. _nStatus <= 201
        _lRet := .T.
        Self:cJSonRet := DecodeUTF8(_cResponse)
        If Valtype(Self:cJSonRet) == "U"
            Self:cJSonRet := _cResponse
        EndIf 
    ElseIf _nStatus > 300 
        _lRet       := .F.
        _oJSonRet   := JSonObject():New()
        _oJSonRet:FromJSon(_cResponse)
        If ValType(_oJSonRet) <> "U"
            Self:cError := _oJSonRet['mensagem'] + ' - ' + DecodeUTF8(_oJSonRet['mensagem_externa'])
        Else 
            Self:cError := "ERROR: " + DecodeUTF8(_cResponse) + " " + _cXMLHeadRet
        EndIf 
    EndIf 
EndIf 

FreeObj(_oJSonRet)

Return _lRet 
