#INCLUDE "PROTHEUS.CH"
#INCLUDE "AARRAY.CH"
#INCLUDE "JSON.CH"

#DEFINE CRLF CHR(13) + CHR(10)

/****************************************************************************************/
/*/{Protheus.doc} Astrein
    @description Classe correspondente as integrações Astrein
    @author    Bernard M. Margarido
    @since     31/10/2020
/*/
/****************************************************************************************/
Class Astrein

    Data cURL           As String
    Data cUser          As String
    Data cPassword      As String
    Data cDirToken      As String
    Data cFileToken     As String
    Data cJSonRet       As String
    Data cJSon          As String
    Data cJSonToken     As String
    Data cToken         As String
    Data cError         As String
    Data cTpItem        As String
    Data cPassCert	    As String
	Data cCertPath	    As String
	Data cKeyPath		As String
	Data cCACertPath	As String

    Data nCodProjeto    As Integer
    Data nSSL2		    As Integer
	Data nSSL3		    As Integer
	Data nTLS1		    As Integer
	Data nHSM		    As Integer
	Data nVerbose	    As Integer
	Data nBugs		    As Integer
	Data nState	        As Integer

    Data aHeadOut       As Array

    Data oRest          As Object 
    Data oJson          As Object
    Data oJsonToken     As Object
    Data oJsonRet       As Object

    Method New() Constructor
    Method Token() 
    Method ObterIntegracao()
    Method RetornoIntegracao()
    Method ClearObj()
    Method GetSSLCache()
    Method EnvioAtualizarItemLote()
    Method RetornoAtualizarItemLote()
    Method NovoConteudo()

EndClass

/****************************************************************************************/
/*/{Protheus.doc} New
    @description Metodo construtor da Classe 
    @type  Function
    @author Bernard M. Margarido
    @since 31/10/2020
/*/
/****************************************************************************************/
Method New() Class Astrein
    
    ::cURL          := GetNewPar("BZ_URLASTR","https://api.astrein.com.br/bunzldev/api")
    ::cUser         := GetNewPar("BZ_USRASTR","USER_API")
    ::cPassword     := GetNewPar("BZ_PASASTR","Api@2020")
    ::cDirToken     := GetNewPar("BZ_DIRASTR","\astrein_token\")
    ::cFileToken    := GetNewPar("BZ_ARQASTR","astrein_token")
    ::cJSonRet      := ""
    ::cJSon         := ""
    ::cJSonToken    := ""
    ::cToken        := ""
    ::cError        := ""
    ::cTpItem       := ""
    ::cPassCert	    := ""
	::cCertPath	    := "" 
	::cKeyPath		:= "" 
	::cCACertPath	:= ""

    ::nCodProjeto   := 0
    ::nSSL2		    := 0
	::nSSL3		    := 0
	::nTLS1		    := 3
	::nHSM			:= 0
	::nVerbose		:= 1
	::nBugs		    := 1
	::nState	    := 1

    ::aHeadOut      := {}

    ::oRest         := Nil
    ::oJson         := Nil
    ::oJsonToken    := Nil 
    ::oJsonRet      := Nil 

Return Nil 

/****************************************************************************************/
/*/{Protheus.doc} GetSSLCache
    @description Define o uso em memoria da configuração SSL para integrações SIGEP
    @author Bernard M. Margarido
    @since 31/10/2020
    @version 1.0
    @type function
/*/
/****************************************************************************************/
Method GetSSLCache() Class Astrein
Local _lRet 	:= .F.

//-------------------------------------+
// Utiliza configurações SSL via Cache |
//-------------------------------------+
If HTTPSSLClient( ::nSSL2, ::nSSL3, ::nTLS1, ::cPassCert, ::cCertPath, ::cKeyPath, ::nHSM, .F. , ::nVerbose, ::nBugs, ::nState)
	_lRet := .T.
EndIf

Return _lRet 

/*********************************************************************************/
/*/{Protheus.doc} ClearObj
    @description Método limpa objeto
    @type  Method
    @author Bernard M. Margarido
    @since 31/10/2020
/*/
/*********************************************************************************/
Method ClearObj(_oObj) Class Astrein
Return FreeObj(_oObj)

/****************************************************************************************/
/*/{Protheus.doc} Token
    @description Metodo obtem Token para integraçao Astrein 
    @type  Function
    @author Bernard M. Margarido
    @since 31/10/2020
/*/
/****************************************************************************************/
Method Token() Class Astrein
Local _cLine    := ""

Local _lRet     := .T.
Local _lToken   := .T.

Local _nLine    := 0

//----------------+
// Cria diretorio | 
//----------------+
MakeDir(::cDirToken)

//--------------------+
// Usa SSL temporario | 
//--------------------+
::GetSSLCache()

//-----------------+
// Diretorio Token |
//-----------------+
If File(::cDirToken + ::cFileToken + ".json")
    _cLine := Alltrim(MemoRead( ::cDirToken + ::cFileToken + ".json" ))
    _nLine := MLCount( _cLine )
    ::cJSonToken := ""
    For _nX := 1 To _nLine
        ::cJSonToken += MemoLine( _cLine, , _nX )
    Next _nX
    
    ::oJsonToken := FromJson(::cJSonToken)

    If !Empty(::oJsonToken[#"accessToken"]) .And. ( Left(Time(),5) > SubStr(::oJsonToken[#"expiration"],12,5)  .Or. Date() > sTod(StrTran(SubStr(::oJsonToken[#"expiration"],1,10),"-","")))
        _lToken     := .T.
        ::ClearObj(::oJsonToken)
    Else
        ::cToken    := RTrim(::oJsonToken[#"accessToken"])
        _lToken     := .F.
    EndIf
EndIf

//---------------------+
// Consulta novo Token |
//---------------------+
If _lToken
    ::oJsonToken                := Array(#)    
    ::oJsonToken[#"USU_CODUSU"] := ::cUser
    ::oJsonToken[#"USU_SENHA"]  := ::cPassword

    //---------------------------+
    // Transforma Objeto em JSON |
    //---------------------------+
    ::cJSonToken := ToJson(::oJsonToken)

    //----------------------------------------+
    // Array contendo parametros de cabeçalho |
    //----------------------------------------+
    ::aHeadOut  := {}
    aAdd(::aHeadOut,"Content-Type: application/json" )

    //-------------------------+
    // Instancia classe FwRest |
    //-------------------------+
    ::oRest   := FWRest():New(RTrim(::cURL))

    //---------------------+
    // TimeOut do processo |
    //---------------------+
    ::oRest:nTimeOut := 600

    //----------------------+
    // Metodo a ser enviado | 
    //----------------------+
    ::oRest:SetPath("/login")

    //---------------------+
    // Parametros de Envio |
    //---------------------+
    ::oRest:SetPostParams(::cJSonToken)
 
    //---------------------+
    // Utiliza metodo POST |
    //---------------------+
    If ::oRest:Post(::aHeadOut)

        //---------------------+
        // Desesserializa JSON |
        //---------------------+
        ::cJSonRet	:= RTrim(::oRest:GetResult())
        ::oJsonRet  := FromJson(::cJSonRet)
        ::cToken    := RTrim(::oJsonRet[#"accessToken"])

        //------------------+        
        // Grava JSON Token |
        //------------------+    
        MemoWrite(::cDirToken + ::cFileToken + ".json",::cJSonRet)    
        _lRet       := .T.
    Else

        //---------------------+
        // Desesserializa JSON |
        //---------------------+
        ::cError    := "Erro ao validar token. Error " + ::oRest:GetLastError()
        _lRet       := .F.
        
    EndIf
EndIf

//--------------+
// Limpa Objeto |
//--------------+
::ClearObj(::oJsonToken)
::ClearObj(::oJsonRet)
::ClearObj(::oRest)
Return _lRet 

/****************************************************************************************/
/*/{Protheus.doc} ObterIntegracao
    @description Metodo obtem clientes cadastrados na Astrein 
    @type  Function
    @author Bernard M. Margarido
    @since 31/10/2020
/*/
/****************************************************************************************/
Method ObterIntegracao() Class Astrein 
Local _lRet     := .T.

//-----------------------+
// Retorna token conexão | 
//-----------------------+
::Token()

//----------------------------------------+
// Array contendo parametros de cabeçalho |
//----------------------------------------+
::aHeadOut  := {}
aAdd(::aHeadOut,"Content-Type: application/json" )
aAdd(::aHeadOut,"Authorization: Bearer " + ::cToken)

//-------------------------+
// Instancia classe FwRest |
//-------------------------+
::oRest   := FWRest():New(RTrim(::cURL))

//---------------------+
// TimeOut do processo |
//---------------------+
::oRest:nTimeOut := 600

//----------------------+
// Metodo a ser enviado | 
//----------------------+
::oRest:SetPath("/ObterItemIntegracao")

//---------------------+
// Parametros de Envio |
//---------------------+
::oRest:SetPostParams(::cJSon)

//---------------------+
// Utiliza metodo POST |
//---------------------+
If ::oRest:Post(::aHeadOut)

    //---------------------+
    // Desesserializa JSON |
    //---------------------+
    ::cJSonRet	:= RTrim(::oRest:GetResult())
    _lRet       := .T.
Else

    //---------------------+
    // Desesserializa JSON |
    //---------------------+
    ::cError    := "Erro ao obter integracoes. Error " + ::oRest:GetLastError()
    _lRet       := .F.
    
EndIf

//--------------+
// Limpa Objeto |
//--------------+
::ClearObj(::oRest)
Return _lRet 

/****************************************************************************************/
/*/{Protheus.doc} RetornoIntegracao
    @description Metodo retira clientes da fila de integracção na Astrein 
    @type  Function
    @author Bernard M. Margarido
    @since 31/10/2020
/*/
/****************************************************************************************/
Method RetornoIntegracao() Class Astrein 
Local _lRet     := .T.

//-----------------------+
// Retorna token conexão | 
//-----------------------+
::Token()

//--------------------+
// Usa SSL temporario | 
//--------------------+
::GetSSLCache()

//----------------------------------------+
// Array contendo parametros de cabeçalho |
//----------------------------------------+
::aHeadOut  := {}
aAdd(::aHeadOut,"Content-Type: application/json" )
aAdd(::aHeadOut,"Authorization: Bearer " + ::cToken)

//-------------------------+
// Instancia classe FwRest |
//-------------------------+
::oRest   := FWRest():New(RTrim(::cURL))

//---------------------+
// TimeOut do processo |
//---------------------+
::oRest:nTimeOut := 600

//----------------------+
// Metodo a ser enviado | 
//----------------------+
::oRest:SetPath("/RetornoItemIntegracao")

//---------------------+
// Parametros de Envio |
//---------------------+
::oRest:SetPostParams(::cJSon)

//---------------------+
// Utiliza metodo POST |
//---------------------+
If ::oRest:Post(::aHeadOut)

    //---------------------+
    // Desesserializa JSON |
    //---------------------+
    ::cJSonRet	:= ::oRest:GetResult()
    _lRet       := .T.

Else

    //---------------------+
    // Desesserializa JSON |
    //---------------------+
    ::cError    := "Erro ao enviar retorno integracoes. Error " + ::oRest:GetLastError()
    _lRet       := .F.
    
EndIf

//--------------+
// Limpa Objeto |
//--------------+
::ClearObj(::oRest)
Return _lRet 

/****************************************************************************************/
/*/{Protheus.doc} EnvioAtualizarItemLote
    @description Metodo Envio de alteracao de cadastros em lote (campos) para o endpoint
    EnvioAtualizarItemLote do WS REST Astrein
    @type  Function
    @author Michihiko Tanimoto
    @since 16/12/2020
/*/
/****************************************************************************************/
Method EnvioAtualizarItemLote() Class Astrein 
Local _lRet     := .T.

//-----------------------+
// Retorna token conexão | 
//-----------------------+
::Token()

//----------------------------------------+
// Array contendo parametros de cabeçalho |
//----------------------------------------+
::aHeadOut  := {}
aAdd(::aHeadOut,"Content-Type: application/json" )
aAdd(::aHeadOut,"Authorization: Bearer " + ::cToken)

//-------------------------+
// Instancia classe FwRest |
//-------------------------+
::oRest   := FWRest():New(RTrim(::cURL))

//---------------------+
// TimeOut do processo |
//---------------------+
::oRest:nTimeOut := 600

//----------------------+
// Metodo a ser enviado | 
//----------------------+
::oRest:SetPath("/EnvioAtualizarItemLote")

//---------------------+
// Parametros de Envio |
//---------------------+
::oRest:SetPostParams(::cJSon)

//---------------------+
// Utiliza metodo POST |
//---------------------+
If ::oRest:Post(::aHeadOut)

    //---------------------+
    // Desesserializa JSON |
    //---------------------+
    ::cJSonRet	:= RTrim(::oRest:GetResult())
    _lRet       := .T.
Else

    //---------------------+
    // Desesserializa JSON |
    //---------------------+
    ::cError    := "Erro ao enviar alteracoes. Error " + ::oRest:GetLastError()
    _lRet       := .F.
    
EndIf

//--------------+
// Limpa Objeto |
//--------------+
::ClearObj(::oRest)
Return _lRet 

/****************************************************************************************/
/*/{Protheus.doc} RetornoAtualizarItemLote
    @description Metodo para buscar o restorno do status do ticket criado no metodo 
    AtualizarItemLote que faz a atualizacao de cadastro da Alstrein do ERP para SSACAD 
    @type  Function
    @author Michihiko Tanimoto
    @since 21/12/2020
/*/
/****************************************************************************************/
Method RetornoAtualizarItemLote() Class Astrein 
Local _lRet     := .T.

//-----------------------+
// Retorna token conexão | 
//-----------------------+
::Token()

//--------------------+
// Usa SSL temporario | 
//--------------------+
::GetSSLCache()

//----------------------------------------+
// Array contendo parametros de cabeçalho |
//----------------------------------------+
::aHeadOut  := {}
aAdd(::aHeadOut,"Content-Type: application/json" )
aAdd(::aHeadOut,"Authorization: Bearer " + ::cToken)

//-------------------------+
// Instancia classe FwRest |
//-------------------------+
::oRest   := FWRest():New(RTrim(::cURL))

//---------------------+
// TimeOut do processo |
//---------------------+
::oRest:nTimeOut := 600

//----------------------+
// Metodo a ser enviado | 
//----------------------+
::oRest:SetPath("/RetornoAtualizarItemLote")

//---------------------+
// Parametros de Envio |
//---------------------+
::oRest:SetPostParams(::cJSon)

//---------------------+
// Utiliza metodo POST |
//---------------------+
If ::oRest:Post(::aHeadOut)

    //---------------------+
    // Desesserializa JSON |
    //---------------------+
    ::cJSonRet	:= ::oRest:GetResult()
    _lRet       := .T.

Else

    //---------------------+
    // Desesserializa JSON |
    //---------------------+
    ::cError    := "Erro ao enviar retorno integracoes. Error " + ::oRest:GetLastError()
    _lRet       := .F.
    
EndIf

//--------------+
// Limpa Objeto |
//--------------+
::ClearObj(::oRest)
Return _lRet 

/****************************************************************************************/
/*/{Protheus.doc} NovoConteudo
    @description Metodo para incluir novo conteaudo administrativo na Altrein quando incluído
    novo cadastro nas tabelas auxiliares dos cadastros do ERP para o SSACAD
    @type  Function
    @author Michihiko Tanimoto
    @since 21/12/2020
/*/
/****************************************************************************************/
Method NovoConteudo() Class Astrein 
Local _lRet     := .T.

//-----------------------+
// Retorna token conexão | 
//-----------------------+
::Token()

//--------------------+
// Usa SSL temporario | 
//--------------------+
::GetSSLCache()

//----------------------------------------+
// Array contendo parametros de cabeçalho |
//----------------------------------------+
::aHeadOut  := {}
aAdd(::aHeadOut,"Content-Type: application/json" )
aAdd(::aHeadOut,"Authorization: Bearer " + ::cToken)

//-------------------------+
// Instancia classe FwRest |
//-------------------------+
::oRest   := FWRest():New(RTrim(::cURL))

//---------------------+
// TimeOut do processo |
//---------------------+
::oRest:nTimeOut := 600

//----------------------+
// Metodo a ser enviado | 
//----------------------+
::oRest:SetPath("/NovoConteudo")

//---------------------+
// Parametros de Envio |
//---------------------+
::oRest:SetPostParams(::cJSon)

//---------------------+
// Utiliza metodo POST |
//---------------------+
If ::oRest:Post(::aHeadOut)
    //---------------------+
    // Desesserializa JSON |
    //---------------------+
    ::cJSonRet	:= RTrim(::oRest:GetResult())
    _lRet       := .T.

Else
    ::cError    := "Erro ao enviar retorno integracoes. Error " + ::oRest:GetResult() //::oRest:[#"CRESULT"]//::oRest:GetLastError()
    _lRet       := .F.
EndIf

//--------------+
// Limpa Objeto |
//--------------+
::ClearObj(::oRest)
Return _lRet 
