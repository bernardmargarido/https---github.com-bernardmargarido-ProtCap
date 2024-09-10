#INCLUDE "PROTHEUS.CH"
#INCLUDE "AARRAY.CH"
#INCLUDE "JSON.CH"

#DEFINE CRLF CHR(13) + CHR(10)

/****************************************************************************************/
/*/{Protheus.doc} ProtMonitor
    @description Classe realiza a gravação e atualização do historico de processamento
    @author    Bernard M. Margarido
    @since     31/10/2020
/*/
/****************************************************************************************/
Class ProtMonitor

    Data cIdProc        As String
    Data cJSon          As String
    Data cError         As String
    Data cStatus        As String
    Data cSeq           As String
    Data cSeqZ12        As String
    Data cChave         As String
    Data cFunName       As String
    Data cErrorRet      As String

    Data nOpc           As Integer
    Data nTpGrava       As Integer
    Data nTpRet         As Integer
    Data nQtdReg		As Integer
	Data nTentativa	    As Integer

    Method New() Constructor
    Method ValidZ12() 
    Method ValidZ13() 
    Method GrvMonitor()
    Method GrvHistorico()

EndClass

/****************************************************************************************/
/*/{Protheus.doc} New
    @description Metodo construtor da Classe 
    @type  Function
    @author Bernard M. Margarido
    @since 31/10/2020
/*/
/****************************************************************************************/
Method New() Class ProtMonitor
    
    ::cIdProc       := ""
    ::cJSon         := ""
    ::cError        := ""
    ::cStatus       := ""
    ::cSeq          := ""
    ::cSeqZ12       := ""
    ::cChave        := ""
    ::cFunName      := ""
    ::cErrorRet     := ""

    ::nOpc          := 0
    ::nTpGrava      := 0
    ::nTpRet        := 0
    ::nQtdReg       := 0
	::nTentativa    := 0

Return Nil 

/****************************************************************************************/
/*/{Protheus.doc} ValidZ12
    @description Metodo valida dados da sequencia e tentativa de gravação
    @type  Function
    @author Bernard M. Margarido
    @since 31/10/2020
/*/
/****************************************************************************************/
Method ValidZ12() Class ProtMonitor
Local _cQuery   := ""
Local _cAlias   := ""

//---------+
// Monitor |
//---------+
_cQuery := " SELECT " + CRLF 
_cQuery += "    COALESCE(MAX(Z12_SEQ),'0000') SEQ, " + CRLF 
_cQuery += "    COALESCE(MAX(Z12_TENTAT),0) TENTATIVA " + CRLF 
_cQuery += " FROM " + CRLF 
_cQuery += "    " + RetSqlName("Z12") + " " + CRLF 
_cQuery += " WHERE " + CRLF 
_cQuery += "    Z12_FILIAL = '" + xFilial("Z12") + "' AND " + CRLF  
_cQuery += "    Z12_CHAVE = '" + ::cChave + "' AND " + CRLF 
_cQuery += "    Z12_ID = '" + ::cIdProc + "' AND " + CRLF 
_cQuery += "    D_E_L_E_T_ = '' " + CRLF 
_cQuery += " GROUP BY Z12_ID,Z12_CHAVE "

_cAlias := MPSysOpenQuery(_cQuery)

//------------+
// Incrementa |
//------------+
If ::nTpRet == 1
    ::cSeqZ12   := IIF(Empty((_cAlias)->SEQ), "0001", Soma1((_cAlias)->SEQ))
    ::nTentativa:= IIF(Empty((_cAlias)->TENTATIVA), 1, (_cAlias)->TENTATIVA + 1)
//--------------------------+
// Retorna ultima sequencia |
//--------------------------+
Else
    ::cSeqZ12   := (_cAlias)->SEQ
    ::nTentativa:= (_cAlias)->TENTATIVA
EndIf

//--------------------+
// Encerra temporario |
//--------------------+
(_cAlias)->( dbCloseArea() )

Return Nil 

/****************************************************************************************/
/*/{Protheus.doc} ValidZ13
    @description Metodo valida dados da sequencia do historico
    @type  Function
    @author Bernard M. Margarido
    @since 31/10/2020
/*/
/****************************************************************************************/
Method ValidZ13() Class ProtMonitor
Local _cQuery   := ""
Local _cAlias   := ""

_cQuery := " SELECT " + CRLF
_cQuery += "    COALESCE(MAX(Z13_SEQ),'0000') SEQ " + CRLF
_cQuery += " FROM " + CRLF
_cQuery += "	" + RetSqlName("Z13") + " " + CRLF
_cQuery += " WHERE " + CRLF
_cQuery += "	Z13_FILIAL = '" + xFilial("Z13") + "' AND " + CRLF
_cQuery += "	Z13_CHAVE = '" + ::cChave + "' AND " + CRLF
_cQuery += "	Z13_ID = '" + ::cIdProc + "' AND " + CRLF
_cQuery += "	D_E_L_E_T_ = '' " + CRLF
_cQuery += " GROUP BY Z13_ID,Z13_CHAVE " + CRLF

_cAlias := MPSysOpenQuery(_cQuery)

//------------+
// Incrementa |
//------------+
If ::nTpRet == 1
    ::cSeq      := IIF(Empty((_cAlias)->SEQ), "0001", Soma1((_cAlias)->SEQ))
//--------------------------+
// Retorna ultima sequencia |
//--------------------------+
Else
    ::cSeq      := (_cAlias)->SEQ
EndIf

//--------------------+
// Encerra temporario |
//--------------------+
(_cAlias)->( dbCloseArea() )

Return Nil 

/****************************************************************************************/
/*/{Protheus.doc} GrvMonitor
    @description Realiza a gravação monitor de processamento
    @type  Function
    @author Bernard M. Margarido
    @since 31/10/2020
/*/
/****************************************************************************************/
Method GrvMonitor() Class ProtMonitor
Local _aArea    := GetArea()

Local _lRet     := .T.
Local _lGrava   := .F.

::nTpRet        := 2
::ValidZ12()

//--------------------------+
// Seleciona tabela monitor |
//--------------------------+
dbSelectArea("Z12")
Z12->( dbSetOrder(1) )
If Z12->( dbSeek(xFilial("Z12") + PadR(::cIdProc,TamSx3("Z12_ID")[1]) + PadR(::cChave,TamSx3("Z12_CHAVE")[1]) + PadR(::cSeqZ12,TamSx3("Z12_SEQ")[1])) )
    If Z12->Z12_STPROC == "6" .And. ::cStatus <> "6"
        ::nTpRet    := 1
        _lGrava     := .T.
        ::ValidZ12()
    EndIf
Else
    ::nTpRet        := 1
    _lGrava         := .T.
    ::ValidZ12()
EndIf

If _lGrava
    RecLock("Z12",_lGrava)
        Z12->Z12_FILIAL := xFilial("Z12")
        Z12->Z12_ID     := ::cIdProc
        Z12->Z12_CHAVE  := ::cChave
        Z12->Z12_SEQ    := ::cSeqZ12
        Z12->Z12_DTINC  := Date()
        Z12->Z12_HRINC  := Time()
        Z12->Z12_JSON   := ::cJSon
        Z12->Z12_DTPINI := Nil
        Z12->Z12_HRPINI := ""
        Z12->Z12_DTPFIM := Nil 
        Z12->Z12_HRPFIM := ""
        Z12->Z12_STPROC := ::cStatus
        Z12->Z12_TENTAT := ::nTentativa
        Z12->Z12_QTDREG := ::nQtdReg
        Z12->Z12_ERPFUN := ::cFunName
    Z12->( MsUnLock() )
Else 
    RecLock("Z12",_lGrava)
        Z12->Z12_DTPINI := Date()
        Z12->Z12_HRPINI := Time()
        Z12->Z12_DTPFIM := Date()
        Z12->Z12_HRPFIM := Time()
        Z12->Z12_STPROC := ::cStatus
    Z12->( MsUnLock() )
EndIf

RestArea(_aArea)
Return _lRet 

/****************************************************************************************/
/*/{Protheus.doc} GrvHistorico 
    @description Realiza a gravação historico de processamento
    @type  Function
    @author Bernard M. Margarido
    @since 31/10/2020
/*/
/****************************************************************************************/
Method GrvHistorico() Class ProtMonitor
Local _aArea    := GetArea()

Local _lRet     := .T.

//--------------------------+
// Seleciona tabela monitor |
//--------------------------+
dbSelectArea("Z13")
Z13->( dbSetOrder(1) )

//-----------------------+
// Retorna sequencia Z12 |
//-----------------------+
::nTpRet    := 2
::ValidZ12()

//----------------------+
// Valida sequencia Z13 |
//----------------------+
::nTpRet    := 1
::ValidZ13()

RecLock("Z13",.T.)
    Z13->Z13_FILIAL := xFilial("Z12")
    Z13->Z13_ID     := ::cIdProc
    Z13->Z13_CHAVE  := ::cChave
    Z13->Z13_Z12SEQ := ::cSeqZ12
    Z13->Z13_SEQ    := ::cSeq
    Z13->Z13_DTHIST := Date()
    Z13->Z13_HRHIST := Time()
    Z13->Z13_JSON   := ::cJSon
    Z13->Z13_ERRO   := ::cError
Z13->( MsUnLock() )

RestArea(_aArea)
Return _lRet
