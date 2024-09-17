#INCLUDE "PROTHEUS.CH"
#INCLUDE "AARRAY.CH"
#INCLUDE "JSON.CH"

#DEFINE CRLF CHR(13) + CHR(10)
/***********************************************************************************/
/*/{Protheus.doc} BZAPI002
    @description Rotina realiza a integração das Alteracoes dos cadastros do ERP para 
    o SSACAD utilizando os metodos de EnvioAtualizarItemLote e RetornoAtualizarItemLote 
    localizados na classe Astrein
    @type  Function
    @author Michihiko Tanimoto
    @since 15/12/2020
    Z12_STPROC
    "1"	, "Aguardando processamento"
    "2" , "Processado com sucesso"
    "6" , "Aguardando retorno"
    "3" , "Processado com erro"
    "7" , "Aguardando nova tentativa"
    "4"	, "Registro Ignorado"
    "5"	, "Processado parcialmente"
/*/
/***********************************************************************************/
User Function BZAPI002()
//Local _cDescApi     := "ASTREIN_ALT_MATERIAL_ERP"
Private _lJob       := IIF(Isincallstack("U_BZJOB004"),.T.,.F.)
Private _aRetAstr   := {}

//Default nRecno      := 0

//----------------+
// Processa Integrações API | 
//----------------+
BZAPI002_PROC()

Return Nil 

/***********************************************************************************/
/*/{Protheus.doc} BZAPI002_PROC
    @description Busca integracoes na Z12 e envia o POST para o WS
    @type  Static Function
    @author Michihiko Tanimoto
    @since 16/12/2020
/*/
/***********************************************************************************/
Static Function BZAPI002_PROC()
Local _aArea        := GetArea()
Local _cChave       := ""
Local _cAlias       := ""
Local _cSEQZ12      := ""
Local _oAstrein     := Astrein():New()
Local _oJson        := Nil 
Local _cJson        := ""
Local _cIdProc      := ""
Local _cTicket      := ""

//Busca os registros pendentes envio para alteraao na Alstrein pelo IDPROC e retora no Alias
If !BZAPI002_QRYZ12(_cIdProc,@_cAlias)
    RestArea(_aArea)
    Return .F.
EndIf

// Tabela de fila integracoes | 
dbSelectArea("Z12")
Z12->( dbSetOrder(1) )
While (_cAlias)->( !Eof() )

    Z12->( dbGoTo((_cAlias)->RECNOZ12))         // Posiciona registro |
    _cIdProc:= Z12_ID
    _cChave:= Z12_CHAVE
    _cSEQZ12:= Z12_SEQ
    _cJson  := Z12->Z12_JSON
    _oJson := FromJson(Z12->Z12_JSON)               // Transforma string Json em Objeto |

    _oAstrein:cJson := Z12->Z12_JSON               // Transforma string Json em Objeto |

    //--------------------------------------+
    // Valida se Objeto pode ser processado | 
    //--------------------------------------+
    If ValType(_oJson) <> "U"

        If _oAstrein:EnvioAtualizarItemLote()
            //----------------------+
            // StringJson em Objeto |
            //----------------------+
            _oJson := FromJson(_oAstrein:cJSonRet)
            //-------------------------------------+
            // gravação do monitor                 |
            //-------------------------------------+
            If ValType(_oJson[#"status"]) <> "U" //retornou um JSON valido
                _cError := RTrim(_oJson[#"mensagem"])
                _cTicket:= cValToChar(_oJson[#"numeroTicket"])
                // Atualiza Z12 
                If _oJson[#"status"]==0 //Sucesso
                    BZ_ATUZ12(_cIdProc,_cChave,_cSEQZ12,_cJson,_cError,"2") 
                Else    //Erro
                    BZ_ATUZ12(_cIdProc,_cChave,_cSEQZ12,_cJson,_cError,"3")   //Fila
                    BZ_ATUZ13(_cIdProc,_cChave,_cJson,_cError)            //Historico
                EndIf
            Else    //Nao retornou um JSON valido, tratar pelo conteudo de cErro 
                _cError := _oAstrein:cError   //erro capturado no metodo quando nao tem JSON
                // Atualiza Z12 |
                BZ_ATUZ12(_cIdProc,_cChave,_cSEQZ12,_cJson,_cError,"3") 
                BZ_ATUZ13(_cIdProc,_cChave,_cJson,_cError)          
            EndIf
        Else    //Nao retornou um JSON valido, tratar pelo conteudo de cErro 
            _cError := _oAstrein:cError   //erro capturado no metodo quando nao tem JSON
            // Atualiza Z12 |
            BZ_ATUZ12(_cIdProc,_cChave,_cSEQZ12,_cJson,_cError,"3") 
            BZ_ATUZ13(_cIdProc,_cChave,_cJson,_cError)          
        EndIf
    EndIf
    (_cAlias)->( dbSkip() )
EndDo

(_cAlias)->( dbCloseArea() )

FreeObj(_oAstrein)
FreeObj(_oJSon)
RestArea(_aArea)
Return .T.

/***********************************************************************************/
/*/{Protheus.doc} BZ_ATUZ12
    @description Grava atualização na tabela de LOG
    @type  Static Function
    @author Bernard M. Margarido
    @since 03/11/2020
/*/
/***********************************************************************************/
Static Function BZ_ATUZ12(_cIdProc,_cChave,_cSEQZ12,_cJson,_cError,_cStatus,_nOpc)
Local _aArea    := GetArea()
Local _lRet     := .T.
Local _oMonitor := ProtMonitor():New()

Default _nOpc   := 3

_oMonitor:cIdProc   := _cIdProc
_oMonitor:cChave    := _cChave
_oMonitor:cSeqZ12   := _cSEQZ12
_oMonitor:cStatus   := _cStatus
_oMonitor:cJSon     := _cJson
_oMonitor:nQtdReg   := 1
_oMonitor:nOpc      := _nOpc
_oMonitor:cFunName  := ProcName(2)

If _oMonitor:GrvMonitor()
    _lRet := .T.
Else
    _lRet := .F.
EndIf

RestArea(_aArea)
Return _lRet 

/***********************************************************************************/
/*/{Protheus.doc} BZ_ATUZ13
    @description Atualiza historico monitor quando ocorre erro
    @type  Static Function
    @author Bernard M. Margarido
    @since 16/12/2020
/*/
/***********************************************************************************/
Static Function BZ_ATUZ13(_cIdProc,_cChave,_cJson,_cError)
Local _lRet     := .T.

Local _oMonitor := ProtMonitor():New()

_oMonitor:cIdProc   := _cIdProc
_oMonitor:cChave    := _cChave
_oMonitor:cError    := _cError
_oMonitor:cJSon     := _cJson

If _oMonitor:GrvHistorico()
    _lRet     := .T.
Else
    _lRet     := .F.
EndIf

Return _lRet 

/***********************************************************************************/
/*/{Protheus.doc} BZAPI002_QRYZ12
    @description Realiza a consulta das integracoes pendentes de processamento
    @type  Static Function
    @author Bernard M. Margarido
    @since 03/11/2020
/*/
/***********************************************************************************/
Static Function BZAPI002_QRYZ12(_cIdProc,_cAlias)
Local _cQuery   := ""

_cQuery := " SELECT " + CRLF
_cQuery += "    Z12_ID, " + CRLF
_cQuery += "	Z12_CHAVE, " + CRLF
_cQuery += "	Z12_SEQ, Z12_TENTAT, Z10_QTDEXE, " + CRLF
_cQuery += "    Z12.R_E_C_N_O_ RECNOZ12 " + CRLF
_cQuery += " FROM " + RetSqlName("Z12") + " Z12 " + CRLF 
_cQuery += " JOIN " + RetSqlName("Z10") + " Z10 ON Z10_ID=Z12_ID AND Z10.D_E_L_E_T_='' " + CRLF
_cQuery += " WHERE " + CRLF
_cQuery += "	Z12_FILIAL = '" + xFilial("Z12") + "' AND " + CRLF
_cQuery += "	Z12_CHAVE <> '' AND " + CRLF
_cQuery += "	Z12_ID IN ('0013','0014','0015') AND " + CRLF        //_cQuery += "	Z12_ID = '" + _cIdProc + "' AND " + CRLF
_cQuery += "	(Z12_STPROC='1' OR (Z12_STPROC = '3' AND Z12_TENTAT<Z10_TENTAT) ) AND " + CRLF                 //_cQuery += "	Z12_STPROC IN('1','3') AND " + CRLF
_cQuery += "	Z12.D_E_L_E_T_ = '' "

_cAlias := MPSysOpenQuery(_cQuery)

If (_cAlias)->( Eof() )
    (_cAlias)->( dbCloseArea() )
    Return .F.
Endif

Return .T.
