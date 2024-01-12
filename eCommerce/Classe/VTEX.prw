#INCLUDE "TOTVS.CH"

/*************************************************************************************************/
/*/{Protheus.doc} VTEX
    @description Classe responsavel pelas integrações VTEX
    @author Bernard M Margarido
    @since 01/06/2023
    @version version
/*/
/*************************************************************************************************/
Class VTEX
    Data cUrl           As String 
    Data cEndPoint      As String
    Data cAppKey        As String 
    Data cAppToken      As String 
    Data cMetodo        As String 
    Data cJSon          As String 
    Data cJSonRet       As String
    Data cError         As String
    Data cID            As String
    Data cWarehouse     As String 
    Data cPassword	    As String 
	Data cCertPath	    As String 
	Data cKeyPath	    As String 
	Data cCACertPath    As String 
    
    Data nSSL2		    As Integer
	Data nSSL3		    As Integer
	Data nTLS1		    As Integer
	Data nHSM		    As Integer
	Data nVerbose	    As Integer
	Data nBugs		    As Integer
	Data nState	        As Integer

    Method New() Constructor 
    Method GetSSLCache() 
    Method Categoria() 
    Method Marca() 
    Method Produto()
    Method GrupoEspecifico()
    Method CampoEspecifico()
    Method ProdutoSpecification()
    Method Sku()
    Method Precos() 
    Method Estoque()

EndClass

/*************************************************************************************************/
/*/{Protheus.doc} New 
    @description Metodo New - Construtor da Classe
    @author Bernard M Margarido
    @since 01/06/2023
    @version version
/*/
/*************************************************************************************************/
Method New() Class VTEX 

    Self:cUrl       := GetMv("EC_URLVTEX")
    Self:cEndPoint  := GetMv("EC_APIVTEX")
    Self:cAppKey    := GetMv("EC_APPVTEX")
    Self:cAppToken  := GetMv("EC_APTVTEX")
    Self:cMetodo    := ""
    Self:cJSon      := ""
    Self:cJSonRet   := ""
    Self:cID        := ""
    Self:cWarehouse := ""
    Self:cError     := ""
    Self:cPassword	:= ""
	Self:cCertPath	:= "" 
	Self:cKeyPath	:= "" 
	Self:cCACertPath:= ""

    Self:nSSL2		:= 0
	Self:nSSL3		:= 0
	Self:nTLS1		:= 3
	Self:nHSM		:= 0
	Self:nVerbose	:= 1
	Self:nBugs		:= 1
	Self:nState	    := 1

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
Method GetSSLCache() Class VTEX
Local _lRet 	:= .F.

//-------------------------------------+
// Utiliza configurações SSL via Cache |
//-------------------------------------+
If HTTPSSLClient( Self:nSSL2, Self:nSSL3, Self:nTLS1, Self:cPassword, Self:cCertPath, Self:cKeyPath, Self:nHSM, .F. , Self:nVerbose, Self:nBugs, Self:nState)
	_lRet := .T.
EndIf

Return _lRet 

/*************************************************************************************************/
/*/{Protheus.doc} Categoria
    @description Metodo realiza o envio das categorias e-Commerce
    @author Bernard M Margarido
    @since 01/06/2023
    @version version
/*/
/*************************************************************************************************/
Method Categoria() Class VTEX
Local _aHeadOut     := {}

Local _lRet         := .T.    

Local _oJSonRet     := Nil 
Local _oFwRest      := Nil 

//---------------+
// Usa cache SSL |
//---------------+
::GetSSLCache()

//----------------+
// Header conexão |
//----------------+
aAdd(_aHeadOut, "Content-Type: application/json")
aAdd(_aHeadOut, "X-VTEX-API-AppKey: " + Self:cAppKey)
aAdd(_aHeadOut, "X-VTEX-API-AppToken: " + Self:cAppToken)

//----------------------------------+
// Instancia classe de conexao REST |
//----------------------------------+
_oFwRest := FWRest():New(::cUrl)

//--------------------+
// Timeout de conexao |
//--------------------+
_oFwRest:nTimeOut := 600

If Self:cMetodo == "GET"
    //-------------------------+
    // Metodo a ser consultado |
    //-------------------------+
    If Empty(Self:cID)
        _oFwRest:SetPath("/api/catalog/pvt/category")
    Else
        _oFwRest:SetPath("/api/catalog/pvt/category/" + RTrim(Self:cID))
    EndIf 

    If _oFwRest:Get(_aHeadOut)
        Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
        _lRet           := .T.
    Else
        If ValType(_oFwRest:GetResult()) <> "U"
            Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
            _oJSonRet       := JSonObject():New()
            _oJSonRet:FromJson(Self:cJSonRet)

            Self:cError     := _oJSonRet["Message"]
        Else 
            Self:cError    := "Não foi possivel conectar com as API's do eCommerce. Favor tentar mais tarde."
        EndIf
        _lRet   := .F.
    EndIf 

ElseIf Self:cMetodo == "POST"

    //-------------------------+
    // Metodo a ser consultado |
    //-------------------------+
    _oFwRest:SetPath("/api/catalog/pvt/category")
    _oFwRest:SetPostParams(EncodeUtf8(Self:cJSon))

    If _oFwRest:Post(_aHeadOut)
        Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
        _lRet           := .T.
    Else
        If ValType(_oFwRest:GetResult()) <> "U"
            Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
            _oJSonRet       := JSonObject():New()
            _oJSonRet:FromJson(Self:cJSonRet)

            Self:cError     := _oJSonRet["Message"]
        Else 
            Self:cError    := "Não foi possivel conectar com as API's do eCommerce. Favor tentar mais tarde."
        EndIf
        _lRet   := .F.
    EndIf 


ElseIf Self:cMetodo == "PUT"

    _oFwRest:SetPath("/api/catalog/pvt/category/" + RTrim(Self:cID))
    If _oFwRest:Put(_aHeadOut,EncodeUtf8(Self:cJSon))
        Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
        _lRet           := .T.
    Else
        If ValType(_oFwRest:GetResult()) <> "U"
            Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
            _oJSonRet       := JSonObject():New()
            _oJSonRet:FromJson(Self:cJSonRet)

            Self:cError     := _oJSonRet["Message"]
        Else 
            Self:cError    := "Não foi possivel conectar com as API's do eCommerce. Favor tentar mais tarde."
        EndIf
        _lRet   := .F.
    EndIf 

EndIf 

FreeObj(_oFwRest)
FreeObj(_oJSonRet)
Return _lRet 

/***************************************************************************************************/
/*/{Protheus.doc} Marca
    @description Metodo realiza o envio das marcas para o ecommerce
    @author Bernard M Margarido
    @since 02/06/2023
    @version version
/*/
/***************************************************************************************************/
Method Marca() Class VTEX
Local _aHeadOut     := {}

Local _lRet         := .T.    

Local _oJSonRet     := Nil 
Local _oFwRest      := Nil 

//---------------+
// Usa cache SSL |
//---------------+
::GetSSLCache()

//----------------+
// Header conexão |
//----------------+
aAdd(_aHeadOut, "Content-Type: application/json")
aAdd(_aHeadOut, "X-VTEX-API-AppKey: " + Self:cAppKey)
aAdd(_aHeadOut, "X-VTEX-API-AppToken: " + Self:cAppToken)

//----------------------------------+
// Instancia classe de conexao REST |
//----------------------------------+
_oFwRest := FWRest():New(::cUrl)

//--------------------+
// Timeout de conexao |
//--------------------+
_oFwRest:nTimeOut := 600

If Self:cMetodo == "GET"
    //-------------------------+
    // Metodo a ser consultado |
    //-------------------------+
    If Empty(Self:cID)
        _oFwRest:SetPath("/api/catalog_system/pub/brand/list")
    Else
        _oFwRest:SetPath("/api/catalog_system/pub/brand/" + RTrim(Self:cID))
    EndIf 

    If _oFwRest:Get(_aHeadOut)
        Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
        _lRet           := .T.
    Else
        If ValType(_oFwRest:GetResult()) <> "U"
            Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
            _oJSonRet       := JSonObject():New()
            _oJSonRet:FromJson(Self:cJSonRet)

            Self:cError     := _oJSonRet["Message"]
        Else 
            Self:cError    := "Não foi possivel conectar com as API's do eCommerce. Favor tentar mais tarde."
        EndIf
        _lRet   := .F.
    EndIf 

ElseIf Self:cMetodo == "POST"

    //-------------------------+
    // Metodo a ser consultado |
    //-------------------------+
    _oFwRest:SetPath("/api/catalog/pvt/brand")
    _oFwRest:SetPostParams(EncodeUtf8(Self:cJSon))

    If _oFwRest:Post(_aHeadOut)
        Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
        _lRet           := .T.
    Else
        If ValType(_oFwRest:GetResult()) <> "U"
            Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
            _oJSonRet       := JSonObject():New()
            _oJSonRet:FromJson(Self:cJSonRet)

            Self:cError     := _oJSonRet["Message"]
        Else 
            Self:cError    := "Não foi possivel conectar com as API's do eCommerce. Favor tentar mais tarde."
        EndIf
        _lRet   := .F.
    EndIf 


ElseIf Self:cMetodo == "PUT"

    _oFwRest:SetPath("/api/catalog/pvt/brand/" + RTrim(Self:cID))
    If _oFwRest:Put(_aHeadOut,EncodeUtf8(Self:cJSon))
        Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
        _lRet           := .T.
    Else
        If ValType(_oFwRest:GetResult()) <> "U"
            Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
            _oJSonRet       := JSonObject():New()
            _oJSonRet:FromJson(Self:cJSonRet)

            Self:cError     := _oJSonRet["Message"]
        Else 
            Self:cError    := "Não foi possivel conectar com as API's do eCommerce. Favor tentar mais tarde."
        EndIf
        _lRet   := .F.
    EndIf 

EndIf 

FreeObj(_oFwRest)
FreeObj(_oJSonRet)    
Return _lRet 

/***************************************************************************************************/
/*/{Protheus.doc} Produto
    @description Metodo - realiza o envio e atualização dos produtos eCommerce
    @author Bernard M margarido
    @since 07/06/2023
    @version version
/*/
/***************************************************************************************************/
Method Produto() Class VTEX
Local _aHeadOut     := {}

Local _lRet         := .T.    

Local _oJSonRet     := Nil 
Local _oFwRest      := Nil 

//---------------+
// Usa cache SSL |
//---------------+
::GetSSLCache()

//----------------+
// Header conexão |
//----------------+
aAdd(_aHeadOut, "Content-Type: application/json")
aAdd(_aHeadOut, "X-VTEX-API-AppKey: " + Self:cAppKey)
aAdd(_aHeadOut, "X-VTEX-API-AppToken: " + Self:cAppToken)

//----------------------------------+
// Instancia classe de conexao REST |
//----------------------------------+
_oFwRest := FWRest():New(::cUrl)

//--------------------+
// Timeout de conexao |
//--------------------+
_oFwRest:nTimeOut := 600

If Self:cMetodo == "GET"
    //-------------------------+
    // Metodo a ser consultado |
    //-------------------------+
    _oFwRest:SetPath("/api/catalog/pvt/product/" + RTrim(Self:cID))

    If _oFwRest:Get(_aHeadOut)
        Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
        _lRet           := .T.
    Else
        If ValType(_oFwRest:GetResult()) <> "U"
            Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
            _oJSonRet       := JSonObject():New()
            _oJSonRet:FromJson(Self:cJSonRet)

            Self:cError     := _oJSonRet["Message"]
        Else 
            Self:cError    := "Não foi possivel conectar com as API's do eCommerce. Favor tentar mais tarde."
        EndIf
        _lRet   := .F.
    EndIf 

ElseIf Self:cMetodo == "POST"

    //-------------------------+
    // Metodo a ser consultado |
    //-------------------------+
    _oFwRest:SetPath("/api/catalog/pvt/product")
    _oFwRest:SetPostParams(EncodeUtf8(Self:cJSon))

    If _oFwRest:Post(_aHeadOut)
        Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
        _lRet           := .T.
    Else
        If ValType(_oFwRest:GetResult()) <> "U"
            Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
            _oJSonRet       := JSonObject():New()
            _oJSonRet:FromJson(Self:cJSonRet)

            Self:cError     := _oJSonRet["Message"]
        Else 
            Self:cError    := "Não foi possivel conectar com as API's do eCommerce. Favor tentar mais tarde."
        EndIf
        _lRet   := .F.
    EndIf 


ElseIf Self:cMetodo == "PUT"

    _oFwRest:SetPath("/api/catalog/pvt/product/" + RTrim(Self:cID))
    If _oFwRest:Put(_aHeadOut,EncodeUtf8(Self:cJSon))
        Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
        _lRet           := .T.
    Else
        If ValType(_oFwRest:GetResult()) <> "U"
            Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
            _oJSonRet       := JSonObject():New()
            _oJSonRet:FromJson(Self:cJSonRet)

            Self:cError     := _oJSonRet["Message"]
        Else 
            Self:cError    := "Não foi possivel conectar com as API's do eCommerce. Favor tentar mais tarde."
        EndIf
        _lRet   := .F.
    EndIf 

EndIf 

FreeObj(_oFwRest)
FreeObj(_oJSonRet) 
       
Return _lRet 

/***************************************************************************************************/
/*/{Protheus.doc} GrupoEspecifico
    @description Metodo - Realiza o envio dos grupos especificos
    @author Bernard M Margarido
    @since 13/06/2023
    @version version
/*/
/***************************************************************************************************/
Method GrupoEspecifico() Class VTEX
Local _aHeadOut     := {}

Local _lRet         := .T.    

Local _oJSonRet     := Nil 
Local _oFwRest      := Nil 

//---------------+
// Usa cache SSL |
//---------------+
::GetSSLCache()

//----------------+
// Header conexão |
//----------------+
aAdd(_aHeadOut, "Content-Type: application/json")
aAdd(_aHeadOut, "X-VTEX-API-AppKey: " + Self:cAppKey)
aAdd(_aHeadOut, "X-VTEX-API-AppToken: " + Self:cAppToken)

//----------------------------------+
// Instancia classe de conexao REST |
//----------------------------------+
_oFwRest := FWRest():New(::cUrl)

//--------------------+
// Timeout de conexao |
//--------------------+
_oFwRest:nTimeOut := 600

If Self:cMetodo == "GET"
    //-------------------------+
    // Metodo a ser consultado |
    //-------------------------+
    _oFwRest:SetPath("/api/catalog_system/pub/specification/groupGet/" + RTrim(Self:cID))

    If _oFwRest:Get(_aHeadOut)
        Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
        _lRet           := .T.
    Else
        If ValType(_oFwRest:GetResult()) <> "U"
            Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
            _oJSonRet       := JSonObject():New()
            _oJSonRet:FromJson(Self:cJSonRet)

            Self:cError     := _oJSonRet["Message"]
        Else 
            Self:cError    := "Não foi possivel conectar com as API's do eCommerce. Favor tentar mais tarde."
        EndIf
        _lRet   := .F.
    EndIf 

ElseIf Self:cMetodo == "POST"

    //-------------------------+
    // Metodo a ser consultado |
    //-------------------------+
    _oFwRest:SetPath("/api/catalog/pvt/specificationgroup")
    _oFwRest:SetPostParams(EncodeUtf8(Self:cJSon))

    If _oFwRest:Post(_aHeadOut)
        Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
        _lRet           := .T.
    Else
        If ValType(_oFwRest:GetResult()) <> "U"
            Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
            _oJSonRet       := JSonObject():New()
            _oJSonRet:FromJson(Self:cJSonRet)

            Self:cError     := _oJSonRet["Message"]
        Else 
            Self:cError    := "Não foi possivel conectar com as API's do eCommerce. Favor tentar mais tarde."
        EndIf
        _lRet   := .F.
    EndIf 


ElseIf Self:cMetodo == "PUT"

    _oFwRest:SetPath("/api/catalog/pvt/specificationgroup/" + RTrim(Self:cID))
    If _oFwRest:Put(_aHeadOut,EncodeUtf8(Self:cJSon))
        Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
        _lRet           := .T.
    Else
        If ValType(_oFwRest:GetResult()) <> "U"
            Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
            _oJSonRet       := JSonObject():New()
            _oJSonRet:FromJson(Self:cJSonRet)

            Self:cError     := _oJSonRet["Message"]
        Else 
            Self:cError    := "Não foi possivel conectar com as API's do eCommerce. Favor tentar mais tarde."
        EndIf
        _lRet   := .F.
    EndIf 

EndIf 

FreeObj(_oFwRest)
FreeObj(_oJSonRet)

Return _lRet 

/***************************************************************************************************/
/*/{Protheus.doc} CampoEspecifico
    @description Método - realiza o envio dos campos especificos e-Commerce
    @author Bernard M Margarido
    @since 14/06/2023
    @version version
/*/
/***************************************************************************************************/
Method CampoEspecifico() Class VTEX
Local _aHeadOut     := {}

Local _lRet         := .T.    

Local _oJSonRet     := Nil 
Local _oFwRest      := Nil 

//---------------+
// Usa cache SSL |
//---------------+
::GetSSLCache()

//----------------+
// Header conexão |
//----------------+
aAdd(_aHeadOut, "Content-Type: application/json")
aAdd(_aHeadOut, "X-VTEX-API-AppKey: " + Self:cAppKey)
aAdd(_aHeadOut, "X-VTEX-API-AppToken: " + Self:cAppToken)

//----------------------------------+
// Instancia classe de conexao REST |
//----------------------------------+
_oFwRest := FWRest():New(::cUrl)

//--------------------+
// Timeout de conexao |
//--------------------+
_oFwRest:nTimeOut := 600

If Self:cMetodo == "GET"
    //-------------------------+
    // Metodo a ser consultado |
    //-------------------------+
    _oFwRest:SetPath("/api/catalog/pvt/specification/" + RTrim(Self:cID))

    If _oFwRest:Get(_aHeadOut)
        Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
        _lRet           := .T.
    Else
        If ValType(_oFwRest:GetResult()) <> "U"
            Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
            _oJSonRet       := JSonObject():New()
            _oJSonRet:FromJson(Self:cJSonRet)

            Self:cError     := _oJSonRet["Message"]
        Else 
            Self:cError     := "Não foi possivel conectar com as API's do eCommerce. Favor tentar mais tarde."
        EndIf
        _lRet   := .F.
    EndIf 

ElseIf Self:cMetodo == "POST"

    //-------------------------+
    // Metodo a ser consultado |
    //-------------------------+
    _oFwRest:SetPath("/api/catalog/pvt/specification")
    _oFwRest:SetPostParams(EncodeUtf8(Self:cJSon))

    If _oFwRest:Post(_aHeadOut)
        Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
        _lRet           := .T.
    Else
        If ValType(_oFwRest:GetResult()) <> "U"
            Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
            _oJSonRet       := JSonObject():New()
            _oJSonRet:FromJson(Self:cJSonRet)

            Self:cError     := _oJSonRet["Message"]
        Else 
            Self:cError    := "Não foi possivel conectar com as API's do eCommerce. Favor tentar mais tarde."
        EndIf
        _lRet   := .F.
    EndIf 


ElseIf Self:cMetodo == "PUT"

    _oFwRest:SetPath("/api/catalog/pvt/specification/" + RTrim(Self:cID))
    If _oFwRest:Put(_aHeadOut,EncodeUtf8(Self:cJSon))
        Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
        _lRet           := .T.
    Else
        If ValType(_oFwRest:GetResult()) <> "U"
            Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
            _oJSonRet       := JSonObject():New()
            _oJSonRet:FromJson(Self:cJSonRet)

            Self:cError     := _oJSonRet["Message"]
        Else 
            Self:cError    := "Não foi possivel conectar com as API's do eCommerce. Favor tentar mais tarde."
        EndIf
        _lRet   := .F.
    EndIf 

EndIf 

FreeObj(_oFwRest)
FreeObj(_oJSonRet)

Return _lRet 

/***************************************************************************************************/
/*/{Protheus.doc} ProdutoSpecification
    @description Metodo - realiza o envio dos campos especificos dos produtos 
    @author Bernard M margarido
    @since 15/06/2023
    @version version
/*/
/***************************************************************************************************/
Method ProdutoSpecification() Class VTEX
Local _aHeadOut     := {}

Local _lRet         := .T.    

Local _oJSonRet     := Nil 
Local _oFwRest      := Nil 

//---------------+
// Usa cache SSL |
//---------------+
::GetSSLCache()

//----------------+
// Header conexão |
//----------------+
aAdd(_aHeadOut, "Content-Type: application/json")
aAdd(_aHeadOut, "X-VTEX-API-AppKey: " + Self:cAppKey)
aAdd(_aHeadOut, "X-VTEX-API-AppToken: " + Self:cAppToken)

//----------------------------------+
// Instancia classe de conexao REST |
//----------------------------------+
_oFwRest := FWRest():New(::cUrl)

//--------------------+
// Timeout de conexao |
//--------------------+
_oFwRest:nTimeOut := 600

If Self:cMetodo == "GET"
    //-------------------------+
    // Metodo a ser consultado |
    //-------------------------+
    _oFwRest:SetPath("/api/catalog_system/pvt/products/"+ RTrim(Self:cID) +"/specification")

    If _oFwRest:Get(_aHeadOut)
        Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
        _lRet           := .T.
    Else
        If ValType(_oFwRest:GetResult()) <> "U"
            Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
            _oJSonRet       := JSonObject():New()
            _oJSonRet:FromJson(Self:cJSonRet)

            Self:cError     := _oJSonRet["Message"]
        Else 
            Self:cError     := "Não foi possivel conectar com as API's do eCommerce. Favor tentar mais tarde."
        EndIf
        _lRet   := .F.
    EndIf 

ElseIf Self:cMetodo == "POST"

    //-------------------------+
    // Metodo a ser consultado |
    //-------------------------+
    _oFwRest:SetPath("/api/catalog_system/pvt/products/"+ RTrim(Self:cID) +"/specification")
    _oFwRest:SetPostParams(EncodeUtf8(Self:cJSon))

    If _oFwRest:Post(_aHeadOut)
        Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
        _lRet           := .T.
    Else
        If ValType(_oFwRest:GetResult()) <> "U"
            Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
            _oJSonRet       := JSonObject():New()
            _oJSonRet:FromJson(Self:cJSonRet)

            Self:cError     := _oJSonRet["Message"]
        Else 
            Self:cError    := "Não foi possivel conectar com as API's do eCommerce. Favor tentar mais tarde."
        EndIf
        _lRet   := .F.
    EndIf 


ElseIf Self:cMetodo == "PUT"

    _oFwRest:SetPath("/api/catalog_system/pvt/products/"+ RTrim(Self:cID) +"/specification")
    If _oFwRest:Put(_aHeadOut,EncodeUtf8(Self:cJSon))
        Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
        _lRet           := .T.
    Else
        If ValType(_oFwRest:GetResult()) <> "U"
            Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
            _oJSonRet       := JSonObject():New()
            _oJSonRet:FromJson(Self:cJSonRet)

            Self:cError     := _oJSonRet["Message"]
        Else 
            Self:cError    := "Não foi possivel conectar com as API's do eCommerce. Favor tentar mais tarde."
        EndIf
        _lRet   := .F.
    EndIf 

EndIf 

FreeObj(_oFwRest)
FreeObj(_oJSonRet)    
Return _lRet 

/***************************************************************************************************/
/*/{Protheus.doc} Sku
    @description Metodo - realiza o envio dos produtos sku's
    @author Bernard M margarido
    @since 15/06/2023
    @version version
/*/
/***************************************************************************************************/
Method Sku() Class VTEX
Local _aHeadOut     := {}

Local _lRet         := .T.    

Local _oJSonRet     := Nil 
Local _oFwRest      := Nil 

//---------------+
// Usa cache SSL |
//---------------+
::GetSSLCache()

//----------------+
// Header conexão |
//----------------+
aAdd(_aHeadOut, "Content-Type: application/json")
aAdd(_aHeadOut, "X-VTEX-API-AppKey: " + Self:cAppKey)
aAdd(_aHeadOut, "X-VTEX-API-AppToken: " + Self:cAppToken)

//----------------------------------+
// Instancia classe de conexao REST |
//----------------------------------+
_oFwRest := FWRest():New(::cUrl)

//--------------------+
// Timeout de conexao |
//--------------------+
_oFwRest:nTimeOut := 600

If Self:cMetodo == "GET"
    //-------------------------+
    // Metodo a ser consultado |
    //-------------------------+
    _oFwRest:SetPath("/api/catalog_system/pvt/sku/stockkeepingunitByProductId/"+ RTrim(Self:cID) )

    If _oFwRest:Get(_aHeadOut)
        Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
        _lRet           := .T.
    Else
        If ValType(_oFwRest:GetResult()) <> "U"
            Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
            _oJSonRet       := JSonObject():New()
            _oJSonRet:FromJson(Self:cJSonRet)

            Self:cError     := _oJSonRet["Message"]
        Else 
            Self:cError     := "Não foi possivel conectar com as API's do eCommerce. Favor tentar mais tarde."
        EndIf
        _lRet   := .F.
    EndIf 

ElseIf Self:cMetodo == "POST"

    //-------------------------+
    // Metodo a ser consultado |
    //-------------------------+
    _oFwRest:SetPath("/api/catalog/pvt/stockkeepingunit")
    _oFwRest:SetPostParams(EncodeUtf8(Self:cJSon))

    If _oFwRest:Post(_aHeadOut)
        Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
        _lRet           := .T.
    Else
        If ValType(_oFwRest:GetResult()) <> "U"
            Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
            _oJSonRet       := JSonObject():New()
            _oJSonRet:FromJson(Self:cJSonRet)

            Self:cError     := _oJSonRet["Message"]
        Else 
            Self:cError    := "Não foi possivel conectar com as API's do eCommerce. Favor tentar mais tarde."
        EndIf
        _lRet   := .F.
    EndIf 


ElseIf Self:cMetodo == "PUT"

    _oFwRest:SetPath("/api/catalog/pvt/stockkeepingunit/"+ RTrim(Self:cID))
    If _oFwRest:Put(_aHeadOut,EncodeUtf8(Self:cJSon))
        Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
        _lRet           := .T.
    Else
        If ValType(_oFwRest:GetResult()) <> "U"
            Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
            _oJSonRet       := JSonObject():New()
            _oJSonRet:FromJson(Self:cJSonRet)

            Self:cError     := _oJSonRet["Message"]
        Else 
            Self:cError    := "Não foi possivel conectar com as API's do eCommerce. Favor tentar mais tarde."
        EndIf
        _lRet   := .F.
    EndIf 

EndIf 

FreeObj(_oFwRest)
FreeObj(_oJSonRet)    
Return _lRet 

/***************************************************************************************************/
/*/{Protheus.doc} Precos
    @description Metodo - Realiza ao envio da atualização de preços dos produtos 
    @author Bernard M Margarido 
    @since 18/06/2023
    @version version
/*/
/***************************************************************************************************/
Method Precos() Class VTEX 
Local _aHeadOut     := {}

Local _lRet         := .T.    

Local _oJSonRet     := Nil 
Local _oFwRest      := Nil 

//---------------+
// Usa cache SSL |
//---------------+
::GetSSLCache()

//----------------+
// Header conexão |
//----------------+
aAdd(_aHeadOut, "Content-Type: application/json")
aAdd(_aHeadOut, "X-VTEX-API-AppKey: " + Self:cAppKey)
aAdd(_aHeadOut, "X-VTEX-API-AppToken: " + Self:cAppToken)

//----------------------------------+
// Instancia classe de conexao REST |
//----------------------------------+
_oFwRest := FWRest():New(::cEndPoint)

//--------------------+
// Timeout de conexao |
//--------------------+
_oFwRest:nTimeOut := 600

If Self:cMetodo == "GET"
    //-------------------------+
    // Metodo a ser consultado |
    //-------------------------+
    _oFwRest:SetPath("/pricing/prices/"+ RTrim(Self:cID) )

    If _oFwRest:Get(_aHeadOut)
        Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
        _lRet           := .T.
    Else
        If ValType(_oFwRest:GetResult()) <> "U"
            Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
            _oJSonRet       := JSonObject():New()
            _oJSonRet:FromJson(Self:cJSonRet)

            Self:cError     := _oJSonRet["Message"]
        Else 
            Self:cError     := "Não foi possivel conectar com as API's do eCommerce. Favor tentar mais tarde."
        EndIf
        _lRet   := .F.
    EndIf 

ElseIf Self:cMetodo == "POST"

    //-------------------------+
    // Metodo a ser consultado |
    //-------------------------+
    _oFwRest:SetPath("/pricing/prices/"+ RTrim(Self:cID))
    _oFwRest:SetPostParams(EncodeUtf8(Self:cJSon))

    If _oFwRest:Post(_aHeadOut)
        Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
        _lRet           := .T.
    Else
        If ValType(_oFwRest:GetResult()) <> "U"
            Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
            _oJSonRet       := JSonObject():New()
            _oJSonRet:FromJson(Self:cJSonRet)

            Self:cError     := _oJSonRet["Message"]
        Else 
            Self:cError    := "Não foi possivel conectar com as API's do eCommerce. Favor tentar mais tarde."
        EndIf
        _lRet   := .F.
    EndIf 


ElseIf Self:cMetodo == "PUT"

    _oFwRest:SetPath("/pricing/prices/"+ RTrim(Self:cID))
    If _oFwRest:Put(_aHeadOut,EncodeUtf8(Self:cJSon))
        Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
        _lRet           := .T.
    Else
        If ValType(_oFwRest:GetResult()) <> "U"
            Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
            _oJSonRet       := JSonObject():New()
            _oJSonRet:FromJson(Self:cJSonRet)

            Self:cError     := _oJSonRet["Message"]
        Else 
            Self:cError    := "Não foi possivel conectar com as API's do eCommerce. Favor tentar mais tarde."
        EndIf
        _lRet   := .F.
    EndIf 

EndIf 

FreeObj(_oFwRest)
FreeObj(_oJSonRet)    
Return _lRet 

/***************************************************************************************************/
/*/{Protheus.doc} Estoque
    @description Metodo - Realiza a atualização de saldos para o e-Commerce
    @author Bernard M Margarido
    @since 21/06/2023
    @version version
    @param param_name, param_type, param_descr
    @return return_var, return_type, return_description
/*/
/***************************************************************************************************/
Method Estoque() Class VTEX
Local _aHeadOut     := {}

Local _lRet         := .T.    

Local _oJSonRet     := Nil 
Local _oFwRest      := Nil 

//---------------+
// Usa cache SSL |
//---------------+
::GetSSLCache()

//----------------+
// Header conexão |
//----------------+
aAdd(_aHeadOut, "Content-Type: application/json")
aAdd(_aHeadOut, "X-VTEX-API-AppKey: " + Self:cAppKey)
aAdd(_aHeadOut, "X-VTEX-API-AppToken: " + Self:cAppToken)

//----------------------------------+
// Instancia classe de conexao REST |
//----------------------------------+
_oFwRest := FWRest():New(IIF(Self:cMetodo == "POST",Self:cEndPoint,Self:cUrl))

//--------------------+
// Timeout de conexao |
//--------------------+
_oFwRest:nTimeOut := 600

If Self:cMetodo == "GET"
    //-------------------------+
    // Metodo a ser consultado |
    //-------------------------+
    _oFwRest:SetPath("/api/logistics/pvt/inventory/skus/"+ RTrim(Self:cID) )

    If _oFwRest:Get(_aHeadOut)
        Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
        _lRet           := .T.
    Else
        If ValType(_oFwRest:GetResult()) <> "U"
            Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
            _oJSonRet       := JSonObject():New()
            _oJSonRet:FromJson(Self:cJSonRet)

            Self:cError     := _oJSonRet["Message"]
        Else 
            Self:cError     := "Não foi possivel conectar com as API's do eCommerce. Favor tentar mais tarde."
        EndIf
        _lRet   := .F.
    EndIf 

ElseIf Self:cMetodo == "POST"

    //-------------------------+
    // Metodo a ser consultado |
    //-------------------------+
    _oFwRest:SetPath("/api/logistics/pvt/inventory/balance")
    _oFwRest:SetPostParams(EncodeUtf8(Self:cJSon))

    If _oFwRest:Post(_aHeadOut)
        Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
        _lRet           := .T.
    Else
        If ValType(_oFwRest:GetResult()) <> "U"
            Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
            _oJSonRet       := JSonObject():New()
            _oJSonRet:FromJson(Self:cJSonRet)

            Self:cError     := _oJSonRet["Message"]
        Else 
            Self:cError    := "Não foi possivel conectar com as API's do eCommerce. Favor tentar mais tarde."
        EndIf
        _lRet   := .F.
    EndIf 


ElseIf Self:cMetodo == "PUT"

    _oFwRest:SetPath("/api/logistics/pvt/inventory/skus/" + RTrim(Self:cID) + "/warehouses/" + RTrim(Self:cWarehouse))
    If _oFwRest:Put(_aHeadOut,EncodeUtf8(Self:cJSon))
        Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
        _lRet           := .T.
    Else
        If ValType(_oFwRest:GetResult()) <> "U"
            Self:cJSonRet	:= DecodeUtf8(_oFwRest:GetResult())
            _oJSonRet       := JSonObject():New()
            _oJSonRet:FromJson(Self:cJSonRet)

            Self:cError     := _oJSonRet["Message"]
        Else 
            Self:cError    := "Não foi possivel conectar com as API's do eCommerce. Favor tentar mais tarde."
        EndIf
        _lRet   := .F.
    EndIf 

EndIf 

FreeObj(_oFwRest)
FreeObj(_oJSonRet)        
Return _lRet
