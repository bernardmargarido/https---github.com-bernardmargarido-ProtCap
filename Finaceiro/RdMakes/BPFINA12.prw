#INCLUDE "PROTHEUS.CH"
#INCLUDE "TOTVS.CH"
#INCLUDE "AARRAY.CH"
#INCLUDE "JSON.CH"

Static _nTID    := TamSx3("XTP_ID")[1]
Static _nTParc  := TamSx3("E1_PARCELA")[1]

/************************************************************************************/
/*/{Protheus.doc} BPFINA12
    @description Importa dados da operadora de pagamento
    @type  Function
    @author Bernard M. Margarido
    @since 15/07/2021
/*/
/************************************************************************************/
User Function BPFINA12()
Local _aArea    := GetArea()
Local _aParam	:= {}
Local _aRet		:= {}

Local _cMsgErro := ""

Local _dDtIni	:= CriaVar("F2_EMISSAO",.F.)
Local _dDtFim	:= CriaVar("F2_EMISSAO",.F.)

Local _bVldParam:= {|| BPFINA12A() }

aAdd(_aParam,{1, "Data De", _dDtIni, "@D", ".T.", , "", 60, .T.})
aAdd(_aParam,{1, "Data Ate", _dDtFim, "@D", ".T.", , "", 60, .T.})
   
If ParamBox(_aParam,"Importa Titulos PagarMe",@_aRet,_bVldParam,,,,,,,.T., .T.)
	//------------+
	// Monta Tela |
	//------------+
	FWMsgRun(, {|_oSay| BPFINA12B(_oSay,@_cMsgErro) }, "Aguarde...", "Consultando registros .... " )
    
    If !Empty(_cMsgErro)
        MsgInfo(_cMsgErro,"Dana - Avisos")
    EndIf

EndIf


RestArea(_aArea)
Return Nil 

/********************************************************************************************/
/*/{Protheus.doc} BPFINA12B
    @description Realiza a consulta dos titulos eCommerce
    @type  Static Function
    @author Bernard M. Margarido
    @since 16/07/2021
/*/
/********************************************************************************************/
Static Function BPFINA12B(_oSay,_cMsgErro)
Local _cCodigo      := ""
Local _cStatus      := ""
Local _cID          := ""
Local _cTID         := ""
Local _cParcela     := ""
Local _cType        := ""
Local _cStaPay      := ""
Local _cRecID       := ""
Local _cJSonPay     := ""
Local _c1DUP        := GetMV("MV_1DUP")

//Local _nDias      := DateDiffDay(mv_par01,mv_par02)
Local _nValor       := 0
Local _nDesc        := 0
Local _nTaxa        := 0
Local _nX           := 0
Local _nPage        := 0

Local _dDtaIni      := mv_par01 //DaySub(mv_par01,1)
Local _dDtaFim      := mv_par02
Local _dDtaEmiss    := ""
Local _dDtaPgto     := ""

Local _lGrava       := .F.
Local _lBaixado     := .F.
Local _lRecebivel   := .T.

Local _oPagarMe     := PagarMe():New()
Local _oJSon        := Nil 

//-----------------------------------+
// XTP- Itens pagamentos disponiveis |
//-----------------------------------+
dbSelectArea("XTP")
XTP->( dbSetOrder(1) )

//-----------------------------------------+
// Processa baixa dos pagamentos eCommerce |
//-----------------------------------------+
While _dDtaIni <= _dDtaFim

    _oSay:cCaption  := "Integrando pagamentos " + dToc(_dDtaIni)
    ProcessMessages()
    _nPage          := 1
    _lGrava         := .F.
    _lRecebivel     := .T.
           
    //--------------------------+
    // Conecta com a adquirente |
    //--------------------------+
    While _lRecebivel

        //_oPagarMe:dDtaPayment   := dToc(_dDtaIni)
        _oPagarMe:dDTPayIni     := dToc(_dDtaIni)
        _oPagarMe:dDTPayFim     := dToc(DaySum(_dDtaIni,1))    
        _oPagarMe:nPage         := _nPage
        If _oPagarMe:Recebivel()

            _oJSon := Nil 
            _oJSon := JSonObject():New() 
            _oJSon:FromJson(_oPagarMe:cRetJSon)

            If ValType(_oJSon) <> "U" .And. Len(_oJSon) > 0

                _cParcela       := _c1DUP

                For _nX := 1 To Len(_oJSon)

                    _cStatus    := "1"
                    _cID        := cValToChar(_oJSon[_nX]["id"])
                    _cTID       := cValToChar(_oJSon[_nX]["transaction_id"])
                    _cType      := _oJSon[_nX]["type"]
                    _cStaPay    := _oJSon[_nX]["status"]
                    _cRecID     := _oJSon[_nX]["recipient_id"]
                    _cJSonPay   := _oJSon:ToJson(_oJSon[_nX])
                    _nValor     := _oJSon[_nX]["amount"] / 100   
                    _nDesc      := (_oJSon[_nX]["amount"] / 100) - ( _oJSon[_nX]["fee"] / 100 )
                    _nTaxa      := _oJSon[_nX]["fee"] / 100
                    _dDtaEmiss  := sTod(SubStr(StrTran(_oJSon[_nX]["date_created"],"-",""),1,10))
                    _dDtaPgto   := sTod(SubStr(StrTran(_oJSon[_nX]["payment_date"],"-",""),1,10))
                    _lGrvPgto   := .T.
                    _lBaixado   := BPFINA12C(_cTID,_cParcela)

                    If XTP->( dbSeek(xFilial("XTP") + PadR(_cTID,_nTID) + PadR(_cParcela,_nTParc)) )
                        _cCodigo := XTP->XTP_CODIGO
                        _lGrvPgto:= .F.
                    Else
                        _cCodigo := GetSxeNum("XTP","XTP_CODIGO")
                        _lGrvPgto:= .T.
                    EndIf 

                    //----------------------------------------+
                    // Gravação itens do pagamento e-Commerce |
                    //----------------------------------------+
                    RecLock("XTP",_lGrvPgto)
                        XTP->XTP_FILIAL     := xFilial("XTP")
                        XTP->XTP_CODIGO     := _cCodigo
                        XTP->XTP_ID         := _cID
                        XTP->XTP_IDPAY      := _cTID
                        XTP->XTP_DTEMIS     := _dDtaEmiss
                        XTP->XTP_DTPGTO     := _dDtaPgto
                        XTP->XTP_PARC       := _cParcela
                        XTP->XTP_VALOR      := IIF(_nValor < 0, 0, _nValor)
                        XTP->XTP_VLRLIQ     := IIF(_nDesc < 0, 0, _nDesc )
                        XTP->XTP_TAXA       := IIF(_nTaxa < 0, _nTaxa * -1, _nTaxa)
                        XTP->XTP_VLRREB     := IIF(_nValor < 0, _nValor * -1, 0)
                        XTP->XTP_STATUS     := IIf(_lBaixado, "2", "1")
                        XTP->XTP_TYPE       := _cType
                        XTP->XTP_STAPAY     := _cStaPay
                        XTP->XTP_RECID      := _cRecID 
                        XTP->XTP_JSON       := _cJSonPay  
                    XTP->( MsUnLock() )

                    //----------------------+
                    // Calcula nova parcela |
                    //----------------------+
                    _cParcela   := BPFINA12P(_cParcela,_nTParc) 

                Next _nX
                //----------------+     
                // Proxima pagina |
                //----------------+ 
                _nPage++
            Else 
                _lRecebivel := .F.
            EndIf
        Else 
            _cMsgErro   := _oPagarMe:cError + " " + CRLF
            _lRecebivel := .F.
        EndIf

    EndDo 
   
    //-------------------------------------------+
    // Valida numeração no controle de numeração | 
    //-------------------------------------------+
    If _lGrava
        ConfirmSX8()
    EndIf

    _dDtaIni := DaySum(_dDtaIni,1)
EndDo

FreeObj(_oJSon)

Return Nil 

/****************************************************************************************************/
/*/{Protheus.doc} BPFINA12C
    @description Valida se titulo ja está baixado 
    @type  Static Function
    @author Bernard M. Margarido
    @since 30/07/2021
/*/
/****************************************************************************************************/
Static Function BPFINA12C(_cTID,_cParcela)
Local _cQuery   := ""
Local _cAlias   := ""

Local _lRet     := .T.

_cQuery := " SELECT " + CRLF
_cQuery += "	CASE " + CRLF
_cQuery += "		WHEN E1.E1_SALDO > 0 THEN " + CRLF
_cQuery += "			'ABERTO' " + CRLF
_cQuery += "		ELSE " + CRLF
_cQuery += "			'BAIXADO' " + CRLF
_cQuery += "	END STATUS " + CRLF
_cQuery += " FROM " + CRLF
_cQuery += "	" + RetSqlName("SE1") + " E1 " + CRLF
_cQuery += " WHERE " + CRLF
_cQuery += "	E1.E1_FILIAL = '" + xFilial("SE1") + "' AND " + CRLF
_cQuery += "	E1.E1_XNSUCAR = '" + _cTID + "' AND " + CRLF
_cQuery += "	( E1.E1_PARCELA = '" + _cParcela + "' OR E1.E1_PARCELA = '' ) AND " + CRLF
_cQuery += "	E1.D_E_L_E_T_ = '' "

_cAlias := MPSysOpenQuery(_cQuery)

If Empty((_cAlias)->STATUS) .Or. RTrim((_cAlias)->STATUS) == "ABERTO"
    _lRet := .F.
ElseIf RTrim((_cAlias)->STATUS) == "BAIXADO"
    _lRet := .T.
EndIf

(_cAlias)->( dbCloseArea() )

Return _lRet  

/****************************************************************************************************/
/*/{Protheus.doc} BPFINA12A
    @description Valida parametros digitados 
    @type  Static Function
    @author Bernard M. Margarido
    @since 16/07/2021
/*/
/****************************************************************************************************/
Static Function BPFINA12A()
Local _lRet := .T.
Return _lRet 

/****************************************************************************************************/
/*/{Protheus.doc} BPFINA12P
    @description Retornar parcela
    @type  Static Function
    @author Bernard M Margarido
    @since 02/04/2025
    @version version
/*/
/****************************************************************************************************/
Static Function BPFINA12P(_cParcela,_nTParc) 
Local _cParFim      := ""
Local _cTipoPar     := ""

_cTipoPar := IIf(_cParcela $ "0123456789" .Or. (!Upper(AllTrim(SubStr(_cParcela,1,1))) $ "ABCDEFGHIJKLMNOPQRSTUVXWYZ"),"N","C")

If _cTipoPar == "N"
    _cParFim := StrZero( Val(_cParcela), _nTParc )
EndIf 

_cParFim := Soma1( _cParcela, _nTParc, .T. )

Return _cParFim
