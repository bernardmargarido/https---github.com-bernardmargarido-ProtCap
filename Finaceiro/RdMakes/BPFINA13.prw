#INCLUDE "TOTVS.CH"
#INCLUDE "PROTHEUS.CH"
#INCLUDE "FWMVCDEF.CH"
#INCLUDE "AARRAY.CH"
#INCLUDE "JSON.CH"

#DEFINE CLR_CINZA RGB(230,230,230)
#DEFINE CLR_RED RGB(200,047,053)
#DEFINE CRLF CHR(13) + CHR(10)

#DEFINE COL_MARK    01
#DEFINE COL_STATUS  02
#DEFINE COL_IDPAY   03
#DEFINE COL_PARCELA 04
#DEFINE COL_DTEMIS  05
#DEFINE COL_DTPGTO  06
#DEFINE COL_VLRTOT  07
#DEFINE COL_VLRLIQ  08
#DEFINE COL_VLRTAX  09
#DEFINE COL_REFUND  10

#DEFINE COL_TMARK    01
#DEFINE COL_TSTATUS  02
#DEFINE COL_TITULO   03
#DEFINE COL_TPREFI   04
#DEFINE COL_TIDPAY   05
#DEFINE COL_TPARCELA 06
#DEFINE COL_TDTEMIS  07
#DEFINE COL_TDTPGTO  08
#DEFINE COL_TVLRTOT  09
#DEFINE COL_TVLRLIQ  10

/********************************************************************************************************************/
/*/{Protheus.doc} BPFINA13
    @description Tela de conciliação 
    @type  Function
    @author Bernard M. Margarido
    @since 20/07/2021
/*/
/********************************************************************************************************************/
User Function BPFINA13()
Local _aArea        := GetArea()

Private _aEcomm     := {}
Private _aTitulo    := {}

Private _oBrowseA  := Nil 
Private _oBrowseB  := Nil 

//----------------------------+
// Grava dados de conciliação |
//----------------------------+
FwMsgRun(,{|| BPFINA13A()},"Aguarde...","Buscando concilações em aberto.")

//------------------+
// Tela conciliação | 
//------------------+
If Len(_aEcomm) > 0 .And. Len(_aTitulo) > 0 
    FwMsgRun(,{|| BaFina13B()},"Aguarde...","Montando tela conciliação.")
EndIf

RestArea(_aArea)
Return Nil 

/********************************************************************************************************************/
/*/{Protheus.doc} BPFINA13A
    @description Consulta dados conciliação 
    @type  Static Function
    @author Bernard M. Margarido
    @since 20/07/2021
/*/
/********************************************************************************************************************/
Static Function BPFINA13A(_nTotal,_nCount,_nTotLiq,_nTotTx,_oSay_02,_oSay_04)
Local _cAlias   := ""
Local _cQuery   := ""

Local _nPosEc   := 0
Local _nPosTit  := 0

Default _nTotal := 0
Default _nTotLiq:= 0 
Default _nTotTx := 0
Default _nCount := 0

Default _oSay_02:= Nil 
Default _oSay_04:= Nil 

_cQuery := " SELECT " + CRLF
_cQuery += "	ID_PAY, " + CRLF
_cQuery += "	DT_EMISS, " + CRLF
_cQuery += "	DT_PGTO, " + CRLF
_cQuery += "	PARCELA, " + CRLF
_cQuery += "	VALOR_TOTAL, " + CRLF
_cQuery += "	VALOR_LIQUIDO, " + CRLF
_cQuery += "	VALOR_TAXA, " + CRLF
_cQuery += "	VALOR_REEMBOLSO, " + CRLF
_cQuery += "	TITULO, " + CRLF
_cQuery += "	PREFIXO, " + CRLF
_cQuery += "	ID_PAY_TITULO, " + CRLF
_cQuery += "	DT_EMISS_TITULO, " + CRLF
_cQuery += "	DT_PGTO_TITULO, " + CRLF
_cQuery += "	PARCELA_TITULO, " + CRLF
_cQuery += "	VALOR_TOTAL_TITULO, " + CRLF
_cQuery += "    CASE " + CRLF
_cQuery += "		WHEN ( ( VALOR_TOTAL - VALOR_TOTAL_TITULO ) < -10  AND ( VALOR_TOTAL - VALOR_TOTAL_TITULO ) < 0 )   THEN " + CRLF
_cQuery += "			'2' " + CRLF
_cQuery += "		WHEN ( ( VALOR_TOTAL - VALOR_TOTAL_TITULO ) > 10 ) THEN " + CRLF
_cQuery += "			'2' " + CRLF
_cQuery += "		ELSE " + CRLF
_cQuery += "			'1' " + CRLF
_cQuery += "	END STATUS " + CRLF
_cQuery += " FROM " + CRLF
_cQuery += " ( " + CRLF
_cQuery += "	SELECT " + CRLF
_cQuery += "		XTP.XTP_IDPAY ID_PAY, " + CRLF
_cQuery += "		XTP.XTP_DTEMIS DT_EMISS, " + CRLF
_cQuery += "		XTP.XTP_DTPGTO DT_PGTO, " + CRLF
_cQuery += "		XTP.XTP_PARC PARCELA, " + CRLF
_cQuery += "		XTP.XTP_VALOR VALOR_TOTAL, " + CRLF
_cQuery += "		XTP.XTP_VLRLIQ VALOR_LIQUIDO, " + CRLF
_cQuery += "		XTP.XTP_TAXA VALOR_TAXA, " + CRLF
_cQuery += "		XTP.XTP_VLRREB VALOR_REEMBOLSO, " + CRLF
_cQuery += "		COALESCE(E1.E1_NUM,'') TITULO, " + CRLF
_cQuery += "		COALESCE(E1.E1_PREFIXO,'') PREFIXO, " + CRLF
_cQuery += "		COALESCE(E1.E1_XNSUCAR,'') ID_PAY_TITULO, " + CRLF
_cQuery += "		COALESCE(E1.E1_EMISSAO,'') DT_EMISS_TITULO, " + CRLF
_cQuery += "		COALESCE(E1.E1_VENCREA,'') DT_PGTO_TITULO, " + CRLF
_cQuery += "		COALESCE(E1.E1_PARCELA,'') PARCELA_TITULO, " + CRLF
_cQuery += "		COALESCE(E1.E1_VALOR,0) VALOR_TOTAL_TITULO " + CRLF
_cQuery += "	FROM " + CRLF
_cQuery += "		" + RetSqlName("XTP") + " XTP " + CRLF 
_cQuery += "		INNER JOIN " + RetSqlName("SE1") + " E1 ON E1.E1_FILORIG = XTP.XTP_FILIAL AND E1.E1_PARCELA = IIF(E1.E1_PARCELA = '','',XTP.XTP_PARC) AND E1.E1_XNSUCAR = XTP.XTP_IDPAY AND E1.E1_BAIXA = '' AND E1.E1_SALDO > 0 AND E1.D_E_L_E_T_ = '' " + CRLF
_cQuery += "	WHERE " + CRLF
_cQuery += "		XTP.XTP_FILIAL = '" + xFilial("XTP") + "' AND " + CRLF
_cQuery += "		XTP.XTP_STATUS = '1' AND " + CRLF
_cQuery += "        XTP.XTP_STAPAY = 'paid' AND " + CRLF
_cQuery += "		XTP.XTP_VALOR > 0 AND " + CRLF
_cQuery += "		XTP.D_E_L_E_T_ = ''	" + CRLF
_cQuery += " )ID_PAGAMENTO " + CRLF
_cQuery += " GROUP BY ID_PAY,DT_EMISS,DT_PGTO,PARCELA,VALOR_TOTAL,VALOR_LIQUIDO,VALOR_TAXA,VALOR_REEMBOLSO,TITULO,PREFIXO,ID_PAY_TITULO,DT_EMISS_TITULO,DT_PGTO_TITULO,PARCELA_TITULO,VALOR_TOTAL_TITULO " + CRLF
_cQuery += " ORDER BY DT_EMISS, ID_PAY "

_cAlias := MPSysOpenQuery(_cQuery)

_aEcomm := {}
_aTitulo:= {}

While (_cAlias)->( !Eof() )

    //------------------------+
    // Array titulo ecommerce |
    //------------------------+
    If (_nPosEc := aScan(_aEcomm,{|x| RTrim(x[COL_IDPAY]) + RTrim(x[COL_PARCELA]) == (_cAlias)->ID_PAY + (_cAlias)->PARCELA}) ) > 0
        If (_cAlias)->VALOR_TOTAL > 0 
            _aEcomm[_nPosEc][COL_VLRTOT] := (_cAlias)->VALOR_TOTAL
        ElseIf (_cAlias)->VALOR_REEMBOLSO > 0 
            _aEcomm[_nPosEc][COL_REFUND] := VALOR_REEMBOLSO
        EndIf
    Else
        aAdd(_aEcomm,Array(10))
        _aEcomm[Len(_aEcomm)][COL_MARK]     := "LBNO"
        _aEcomm[Len(_aEcomm)][COL_STATUS]   := (_cAlias)->STATUS
        _aEcomm[Len(_aEcomm)][COL_IDPAY]    := (_cAlias)->ID_PAY
        _aEcomm[Len(_aEcomm)][COL_PARCELA]  := (_cAlias)->PARCELA
        _aEcomm[Len(_aEcomm)][COL_DTEMIS]   := dToc(sTod((_cAlias)->DT_EMISS))
        _aEcomm[Len(_aEcomm)][COL_DTPGTO]   := dToc(sTod((_cAlias)->DT_PGTO))
        _aEcomm[Len(_aEcomm)][COL_VLRTOT]   := (_cAlias)->VALOR_TOTAL
        _aEcomm[Len(_aEcomm)][COL_VLRLIQ]   := (_cAlias)->VALOR_LIQUIDO
        _aEcomm[Len(_aEcomm)][COL_VLRTAX]   := (_cAlias)->VALOR_TAXA
        _aEcomm[Len(_aEcomm)][COL_REFUND]   := (_cAlias)->VALOR_REEMBOLSO
    EndIf

    //-----------------------+
    // Array Titulo Protheus |
    //-----------------------+
    If (_nPosTit := aScan(_aTitulo,{|x| RTrim(x[COL_TIDPAY]) + RTrim(x[COL_TPARCELA]) == (_cAlias)->ID_PAY_TITULO + (_cAlias)->PARCELA_TITULO}) ) == 0
        aAdd(_aTitulo,Array(9))
        _aTitulo[Len(_aTitulo)][COL_TMARK]      := "LBNO"
        _aTitulo[Len(_aTitulo)][COL_TSTATUS]    := (_cAlias)->STATUS
        _aTitulo[Len(_aTitulo)][COL_TITULO]     := (_cAlias)->TITULO
        _aTitulo[Len(_aTitulo)][COL_TPREFI]     := (_cAlias)->PREFIXO
        _aTitulo[Len(_aTitulo)][COL_TIDPAY]     := (_cAlias)->ID_PAY_TITULO
        _aTitulo[Len(_aTitulo)][COL_TPARCELA]   := (_cAlias)->PARCELA_TITULO    
        _aTitulo[Len(_aTitulo)][COL_TDTEMIS]    := dToc(sTod((_cAlias)->DT_EMISS_TITULO))
        _aTitulo[Len(_aTitulo)][COL_TDTPGTO]    := dToc(sTod((_cAlias)->DT_PGTO_TITULO))
        _aTitulo[Len(_aTitulo)][COL_TVLRTOT]    := (_cAlias)->VALOR_TOTAL_TITULO
    EndIf

    (_cAlias)->( dbSkip() ) 
EndDo

If Len(_aEcomm) == 0
    aAdd(_aEcomm,Array(10))
    _aEcomm[Len(_aEcomm)][COL_MARK]     := "LBNO"
    _aEcomm[Len(_aEcomm)][COL_STATUS]   := "3"
    _aEcomm[Len(_aEcomm)][COL_IDPAY]    := ""
    _aEcomm[Len(_aEcomm)][COL_PARCELA]  := ""
    _aEcomm[Len(_aEcomm)][COL_DTEMIS]   := ""
    _aEcomm[Len(_aEcomm)][COL_DTPGTO]   := ""
    _aEcomm[Len(_aEcomm)][COL_VLRTOT]   := 0
    _aEcomm[Len(_aEcomm)][COL_VLRLIQ]   := 0
    _aEcomm[Len(_aEcomm)][COL_VLRTAX]   := 0
    _aEcomm[Len(_aEcomm)][COL_REFUND]   := 0
EndIf 

If Len(_aTitulo) == 0
    aAdd(_aTitulo,Array(9))
    _aTitulo[Len(_aTitulo)][COL_TMARK]      := "LBNO"
    _aTitulo[Len(_aTitulo)][COL_TSTATUS]    := "3"
    _aTitulo[Len(_aTitulo)][COL_TITULO]     := ""
    _aTitulo[Len(_aTitulo)][COL_TPREFI]     := ""
    _aTitulo[Len(_aTitulo)][COL_TIDPAY]     := ""
    _aTitulo[Len(_aTitulo)][COL_TPARCELA]   := ""
    _aTitulo[Len(_aTitulo)][COL_TDTEMIS]    := ""
    _aTitulo[Len(_aTitulo)][COL_TDTPGTO]    := ""
    _aTitulo[Len(_aTitulo)][COL_TVLRTOT]    := 0
EndIf 

//---------------------------+
// Atualiza dados da Browser |
//---------------------------+
If ValType(_oBrowseA) == "O"

    _oBrowseA:Refresh()
    _oBrowseB:Refresh()

    _nTotal := 0 
    _nCount := 0
    _nTotLiq:= 0 
    _nTotTx := 0

    _oSay_02:SetText(cValToChar(_nCount))
    _oSay_04:SetText(Transform(_nTotal,PesqPict("SE1","E1_VALOR")))
    _oSay_02:Refresh()
    _oSay_04:Refresh()

EndIf

(_cAlias)->( dbCloseArea() ) 

Return Nil 

/********************************************************************************************************************/
/*/{Protheus.doc} BaFina13B
    @description Monta Tela conciliação bancária 
    @type  Static Function
    @author Bernard M. Margarido
    @since 20/07/2021
/*/
/********************************************************************************************************************/
Static Function BaFina13B()
Local _cTitulo      := "Conciliação pagamentos e-Commerce"
Local _cMsgChk      := "Marca ou desmarca todos os titulos de concliação."
Local _nLinIni      := 0
Local _nColIni      := 0
Local _nLinFin      := 0
Local _nColFin      := 0
Local _nTotal       := 0
Local _nTotLiq      := 0
Local _nTotTx       := 0
Local _nCount       := 0

Local _lTodos       := .F.

Local _aCoors   	:= FWGetDialogSize( oMainWnd )

Local _bChkMark		:= {|| IIF(_lTodos,(BPFINA13F(.T.,@_nTotLiq,@_nTotTx,@_nTotal,@_nCount,@_oSay_02,@_oSay_04,@_oSay_06,@_oSay_08),_oChkMark:Refresh()),(BPFINA13F(.F.,@_nTotLiq,@_nTotTx,@_nTotal,@_nCount,@_oSay_02,@_oSay_04,@_oSay_06,@_oSay_08),_oChkMark:Refresh())) }

Local _oSize    	:= FWDefSize():New(.T.)
Local _oLayer       := FWLayer():New()
Local _oFont12      := TFont():New('Arial Black',,-12,.T.)
Local _oDlg     	:= Nil
Local _oChkMark     := Nil 
Local _oSay_01      := Nil 
Local _oSay_02      := Nil 
Local _oSay_03      := Nil 
Local _oSay_04      := Nil 
Local _oSay_05      := Nil 
Local _oSay_06      := Nil 
Local _oSay_07      := Nil 
Local _oSay_08      := Nil 
Local _oPanel_01    := Nil 
Local _oPanel_02    := Nil 
Local _oBtnOk       := Nil 
Local _oBtnSair     := Nil 

//-------------------------------------------------------+
// Inicializa as coordenadas de tela conforme resolução  |
//-------------------------------------------------------+
_oSize:AddObject( "DLG", 100, 100, .T., .T.)
_oSize:SetWindowSize(_aCoors)
_oSize:lProp         := .T.
_oSize:lLateral 	 := .T.
_oSize:Process()

//--------------------------------------------------------+
// 0 - Nenhum alinhamento (default) - CONTROL_ALIGN_NONE  |
// 1 - À esquerda - CONTROL_ALIGN_LEFT                    |
// 2 - À direita - CONTROL_ALIGN_RIGHT                    |
// 3 - No topo - CONTROL_ALIGN_TOP                        |
// 4 - No rodapé - CONTROL_ALIGN_BOTTOM                   |
// 5 - Em todo o parent - CONTROL_ALIGN_ALLCLIENT         |
//--------------------------------------------------------+

//------------------------+
// Monta Dialog principal |
//------------------------+
_oDlg := MsDialog():New(_oSize:aWindSize[1], _oSize:aWindSize[2], _oSize:aWindSize[3], _oSize:aWindSize[4], _cTitulo,,,,DS_MODALFRAME,,,,,.T.)

	_nLinIni := _oSize:GetDimension("DLG","LININI")
	_nColIni := _oSize:GetDimension("DLG","COLINI")
	_nLinFin := _oSize:GetDimension("DLG","LINEND")
	_nColFin := _oSize:GetDimension("DLG","COLEND")
	
	//-------------------------+
	// Painel para informações | 
	//-------------------------+
	_oPanel_01 := TPanel():New(000,000,"",_oDlg,Nil,.T.,.F.,Nil,CLR_CINZA,000,_nLinFin - 035,.T.,.F.)
	_oPanel_01:Align := 3

    _oLayer:Init( _oPanel_01, .F. )
    _oLayer:AddLine( "LINE01", 100 )

    _oLayer:AddCollumn( "COLLL01"  , 050,, "LINE01" )
    _oLayer:AddCollumn( "COLLL02"  , 050,, "LINE01" )

    _oLayer:AddWindow( "COLLL01" , "WNDECOM"  , "Titulos PagarMe"     , 100 ,.F. ,,,"LINE01" )
    _oLayer:AddWindow( "COLLL02" , "WNDTITU"  , "Titulos Protheus"     , 100 ,.F. ,,,"LINE01" )

    _oTEcom  := _oLayer:GetWinPanel( "COLLL01"   , "WNDECOM"  , "LINE01" )
    _oTTitul := _oLayer:GetWinPanel( "COLLL02"   , "WNDTITU"  , "LINE01" )

    //---------------------+
    // Cria Grid eCommerce |
    //---------------------+
    BPFINA13C(@_oBrowseA,@_nTotTx,@_nTotLiq,@_nTotal,@_nCount,_oTEcom,@_oSay_02,@_oSay_04,@_oSay_06,@_oSay_08,@_oChkMark,@_lTodos)

    //--------------------+
    // Cria Grid Protheus |
    //--------------------+
    BPFINA13D(@_oBrowseB,@_nTotTx,@_nTotLiq,@_nTotal,@_nCount,_oTTitul,@_oSay_02,@_oSay_04,@_oSay_06,@_oSay_08,@_oChkMark,@_lTodos)

    //-----------------------+
	// Painel para os Botoes | 
	//-----------------------+
    _oPanel_02 := TPanel():New(000,000,"",_oDlg,Nil,.T.,.F.,Nil,CLR_CINZA,000,030,.T.,.F.)
	_oPanel_02:Align := 4

    //------------------+    
    // Marcar/Desmarcar |
    //------------------+
    _oChkMark	:= TCheckBox():New(_nLinIni - 22,_nColIni ,"Marcar/Desmarcar"	,{|l| IIF( PCount() > 0, _lTodos := l, _lTodos) },_oPanel_02,080,040,,_bChkMark,_oFont12,,,,,.T.,_cMsgChk,,)

    //---------------+
    // Total marcado |
    //---------------+
    _oSay_01 := TSay():New(_nLinIni - 28, _nColFin - 470 , {|| "Total Marcado" }, _oPanel_02,, _oFont12,,,, .T. ,,CLR_CINZA, 080, 010,,,,,,.T.)
    _oSay_01:SetTextAlign(0,0)

    _oSay_02 := TSay():New(_nLinIni - 16, _nColFin - 470 , {|| cValToChar(_nCount) }, _oPanel_02,, _oFont12,,,, .T. ,CLR_RED,CLR_CINZA, 080, 010,,,,,,.T.)
    _oSay_02:SetTextAlign(0,0)

    _oSay_03 := TSay():New(_nLinIni - 28, _nColFin - 340 , {|| "Valor Total" }, _oPanel_02,, _oFont12,,,, .T. ,,CLR_CINZA, 080, 010,,,,,,.T.)
    _oSay_03:SetTextAlign(0,0)

    _oSay_04 := TSay():New(_nLinIni - 16, _nColFin - 360 , {|| Transform(_nTotal,PesqPict("SE1","E1_VALOR")) }, _oPanel_02,, _oFont12,,,, .T. ,CLR_RED,CLR_CINZA, 080, 010,,,,,,.T.)
    _oSay_04:SetTextAlign(0,0)

    _oSay_05 := TSay():New(_nLinIni - 28, _nColFin - 250 , {|| "Valor Liquido" }, _oPanel_02,, _oFont12,,,, .T. ,,CLR_CINZA, 080, 010,,,,,,.T.)
    _oSay_05:SetTextAlign(0,0)

    _oSay_06 := TSay():New(_nLinIni - 16, _nColFin - 270 , {|| Transform(_nTotLiq,PesqPict("SE1","E1_VALOR")) }, _oPanel_02,, _oFont12,,,, .T. ,CLR_RED,CLR_CINZA, 080, 010,,,,,,.T.)
    _oSay_06:SetTextAlign(0,0)

    _oSay_07 := TSay():New(_nLinIni - 28, _nColFin - 160 , {|| "Total Taxa" }, _oPanel_02,, _oFont12,,,, .T. ,,CLR_CINZA, 080, 010,,,,,,.T.)
    _oSay_07:SetTextAlign(0,0)

    _oSay_08 := TSay():New(_nLinIni - 16, _nColFin - 180 , {|| Transform(_nTotTx,PesqPict("SE1","E1_VALOR")) }, _oPanel_02,, _oFont12,,,, .T. ,CLR_RED,CLR_CINZA, 080, 010,,,,,,.T.)
    _oSay_08:SetTextAlign(0,0)

    //--------+
    // Botoes |
    //--------+
    _oBtnOk     := TButton():New( _nLinIni - 26, _nColFin - 100, "Confirmar", _oPanel_02,{|| IIF( BPFINA13J(),( FwMsgRun(,{|_oSay| BPFINA13G(_oSay) },"Aguarde...","Processando conciliação"), FwMsgRun(,{|| BPFINA13A(_nTotal,_nCount,_nTotLiq,_nTotTx,_oSay_02,_oSay_04)},"Aguarde...","Buscando concilações em aberto.") ),Nil)}	, 045,015,,,.F.,.T.,.F.,,.F.,,,.F. )
	_oBtnSair   := TButton():New( _nLinIni - 26, _nColFin - 050, "Sair", _oPanel_02,{|| _oDlg:End() }	, 045,015,,,.F.,.T.,.F.,,.F.,,,.F. )

    _oDlg:lEscClose := .F.    
    _oDlg:lCentered := .T.

_oDlg:Activate(,,,.T.,,,)
Return Nil 

/********************************************************************************************************************/
/*/{Protheus.doc} BPFINA13C
    @description Cria GRID com os dados dos titulos e-Commerce
    @type  Static Function
    @author Bernard M. Margarido
    @since 21/07/2021
/*/
/********************************************************************************************************************/
Static Function BPFINA13C(_oBrowseA,_nTotTx,_nTotLiq,_nTotal,_nCount,_oTEcom,_oSay_02,_oSay_04,_oSay_06,_oSay_08,_oChkMark,_lTodos)
Local _aSeek 	:= {}

//------------------+
// Busca no browser |
//------------------+
aAdd( 	_aSeek, 	{ AllTrim("ID Pay")				,{{"","C",TamSx3("XTP_IDPAY")[1],0                      ,"ID Pay"		, PesqPict("XTP","XTP_IDPAY")}}})
aAdd( 	_aSeek, 	{ AllTrim("Dt. Emissao")		,{{"","D",TamSx3("XTP_DTEMIS")[1],0     	            ,"Dt. Emissao"	, PesqPict("XTP","XTP_DTEMIS")}}})
aAdd( 	_aSeek, 	{ AllTrim("Valor")		        ,{{"","N",TamSx3("XTP_VALOR")[1],TamSx3("XTP_VALOR")[2] ,"Valor"		, PesqPict("XTP","XTP_VALOR")}}})

//--------------+
// Cria browser |
//--------------+
_oBrowseA := FWBrowse():New(_oTEcom)
_oBrowseA:AddMarkColumns( {|| IIF( Len(_aEcomm) > 0 .And. _aEcomm[_oBrowseA:At()][COL_MARK] == "LBOK" , "LBOK", "LBNO")}, {|| BPFINA13E(_oBrowseA,@_oSay_02,@_oSay_04,@_oSay_06,@_oSay_08,@_oChkMark,@_lTodos,@_nTotTx,@_nTotLiq,@_nTotal,@_nCount,_aEcomm,1,1) }, {|| BPFINA13E(_oBrowseA,@_oSay_02,@_oSay_04,@_oSay_06,@_oSay_08,@_oChkMark,@_lTodos,@_nTotTx,@_nTotLiq,@_nTotal,@_nCount,_aEcomm,2,1) })
_oBrowseA:AddLegend({|| _aEcomm[_oBrowseA:At()][COL_STATUS] == '1'}, "GREEN"	, "Título encontrado")
_oBrowseA:AddLegend({|| _aEcomm[_oBrowseA:At()][COL_STATUS] == '2'}, "YELLOW"	, "Título encontrado com divergencia")
_oBrowseA:AddLegend({|| _aEcomm[_oBrowseA:At()][COL_STATUS] == '3'}, "RED"	    , "Título não encontrado")
_oBrowseA:DisableConfig()
_oBrowseA:DisableReport()
_oBrowseA:SetDataArray()
_oBrowseA:SetSeek(,_aSeek)
_oBrowseA:SetArray(_aEcomm)
//_oBrowseA:SetUseFilter()
//_oBrowseA:SetDoubleClick({|| LiFatC01J(_oBrowseA) })

//--------------+
// Cria colunas |
//--------------+
_oColumns := FWBrwColumn():New()
_oColumns:SetData({||_aEcomm[_oBrowseA:At()][COL_IDPAY]})
_oColumns:SetTitle(RetTitle("XTP_IDPAY"))
_oColumns:SetSize(TamSx3("XTP_IDPAY")[1])
_oColumns:SetDecimal(TamSx3("XTP_IDPAY")[2])
_oColumns:SetPicture(PesqPict("XTP","XTP_IDPAY"))
_oBrowseA:SetColumns({_oColumns})

_oColumns := FWBrwColumn():New()
_oColumns:SetData({||_aEcomm[_oBrowseA:At()][COL_PARCELA]})
_oColumns:SetTitle(RetTitle("XTP_PARC"))
_oColumns:SetSize(TamSx3("XTP_PARC")[1])
_oColumns:SetDecimal(TamSx3("XTP_PARC")[2])
_oColumns:SetPicture(PesqPict("XTP","XTP_PARC"))
_oBrowseA:SetColumns({_oColumns})

_oColumns := FWBrwColumn():New()
_oColumns:SetData({||_aEcomm[_oBrowseA:At()][COL_DTEMIS]})
_oColumns:SetTitle(RetTitle("XTP_DTEMIS"))
_oColumns:SetSize(TamSx3("XTP_DTEMIS")[1])
_oColumns:SetDecimal(TamSx3("XTP_DTEMIS")[2])
_oColumns:SetPicture(PesqPict("XTP","XTP_DTEMIS"))
_oBrowseA:SetColumns({_oColumns})

_oColumns := FWBrwColumn():New()
_oColumns:SetData({||_aEcomm[_oBrowseA:At()][COL_DTPGTO]})
_oColumns:SetTitle(RetTitle("XTP_DTPGTO"))
_oColumns:SetSize(TamSx3("XTP_DTPGTO")[1])
_oColumns:SetDecimal(TamSx3("XTP_DTPGTO")[2])
_oColumns:SetPicture(PesqPict("XTP","XTP_DTPGTO"))
_oBrowseA:SetColumns({_oColumns})

_oColumns := FWBrwColumn():New()
_oColumns:SetData({||_aEcomm[_oBrowseA:At()][COL_VLRTOT]})
_oColumns:SetTitle(RetTitle("XTP_VALOR"))
_oColumns:SetSize(TamSx3("XTP_VALOR")[1])
_oColumns:SetDecimal(TamSx3("XTP_VALOR")[2])
_oColumns:SetPicture(PesqPict("XTP","XTP_VALOR"))
_oBrowseA:SetColumns({_oColumns})

_oColumns := FWBrwColumn():New()
_oColumns:SetData({||_aEcomm[_oBrowseA:At()][COL_VLRLIQ]})
_oColumns:SetTitle(RetTitle("XTP_VLRLIQ"))
_oColumns:SetSize(TamSx3("XTP_VLRLIQ")[1])
_oColumns:SetDecimal(TamSx3("XTP_VLRLIQ")[2])
_oColumns:SetPicture(PesqPict("XTP","XTP_VLRLIQ"))
_oBrowseA:SetColumns({_oColumns})

_oColumns := FWBrwColumn():New()
_oColumns:SetData({||_aEcomm[_oBrowseA:At()][COL_VLRTAX]})
_oColumns:SetTitle(RetTitle("XTP_TAXA"))
_oColumns:SetSize(TamSx3("XTP_TAXA")[1])
_oColumns:SetDecimal(TamSx3("XTP_TAXA")[2])
_oColumns:SetPicture(PesqPict("XTP","XTP_TAXA"))
_oBrowseA:SetColumns({_oColumns})

_oColumns := FWBrwColumn():New()
_oColumns:SetData({||_aEcomm[_oBrowseA:At()][COL_REFUND]})
_oColumns:SetTitle(RetTitle("XTP_VLRREB"))
_oColumns:SetSize(TamSx3("XTP_VLRREB")[1])
_oColumns:SetDecimal(TamSx3("XTP_VLRREB")[2])
_oColumns:SetPicture(PesqPict("XTP","XTP_VLRREB"))
_oBrowseA:SetColumns({_oColumns})

//_oBrowseA:SetEditCell(.T., {|| LiFatC01E(_oBrowseA,_oBrowseA:At()) })
_oBrowseA:SetLineHeight( 20 )
_oBrowseA:DeActivate()
_oBrowseA:Activate()
_oBrowseA:Refresh()

Return Nil 

/********************************************************************************************************************/
/*/{Protheus.doc} BPFINA13D
    @description Cria GRID com os dados dos titulos e-Commerce
    @type  Static Function
    @author Bernard M. Margarido
    @since 21/07/2021
/*/
/********************************************************************************************************************/
Static Function BPFINA13D(_oBrowseB,_nTotTx,_nTotLiq,_nTotal,_nCount,_oTTitul,_oSay_02,_oSay_04,_oSay_06,_oSay_08,_oChkMark,_lTodos)
Local _aSeek 	:= {}

//------------------+
// Busca no browser |
//------------------+
aAdd( 	_aSeek, 	{ AllTrim("Titulo")				,{{"","C",TamSx3("E1_NUM")[1],0                         ,"Titulo"		, PesqPict("SE1","E1_NUM")}}})
aAdd( 	_aSeek, 	{ AllTrim("ID Pay")				,{{"","C",TamSx3("XTP_IDPAY")[1],0                      ,"ID Pay"		, PesqPict("XTP","XTP_IDPAY")}}})
aAdd( 	_aSeek, 	{ AllTrim("Dt. Emissao")		,{{"","D",TamSx3("XTP_DTEMIS")[1],0     	            ,"Dt. Emissao"	, PesqPict("XTP","XTP_DTEMIS")}}})
aAdd( 	_aSeek, 	{ AllTrim("Valor")		        ,{{"","N",TamSx3("XTP_VALOR")[1],TamSx3("XTP_VALOR")[2] ,"Valor"		, PesqPict("XTP","XTP_VALOR")}}})

//--------------+
// Cria browser |
//--------------+
_oBrowseB := FWBrowse():New(_oTTitul)                                                                              
_oBrowseB:AddMarkColumns( {|| IIF( Len(_aTitulo) > 0 .And. _aTitulo[_oBrowseB:At()][COL_TMARK] == "LBOK" , "LBOK", "LBNO")}, {|| BPFINA13E(_oBrowseB,@_oSay_02,@_oSay_04,@_oSay_06,@_oSay_08,@_oChkMark,@_lTodos,@_nTotTx,@_nTotLiq,@_nTotal,@_nCount,_aTitulo,1,2) }, {|| BPFINA13E(_oBrowseB,@_oSay_02,@_oSay_04,@_oSay_06,@_oSay_08,@_oChkMark,@_lTodos,@_nTotTx,@_nTotLiq,@_nTotal,@_nCount,_aTitulo,2,2) })
_oBrowseB:AddLegend({|| _aTitulo[_oBrowseB:At()][COL_TSTATUS] == '1'}, "GREEN"	, "Título encontrado")
_oBrowseB:AddLegend({|| _aTitulo[_oBrowseB:At()][COL_TSTATUS] == '2'}, "YELLOW"	, "Título encontrado com divergencia")
_oBrowseB:AddLegend({|| _aTitulo[_oBrowseB:At()][COL_TSTATUS] == '3'}, "RED"	, "Título não encontrado")
_oBrowseB:DisableConfig()
_oBrowseB:DisableReport()
_oBrowseB:SetDataArray()
_oBrowseB:SetSeek(,_aSeek)
_oBrowseB:SetArray(_aTitulo)
//_oBrowseB:SetUseFilter()
//_oBrowseA:SetDoubleClick({|| LiFatC01J(_oBrowseA) })

//--------------+
// Cria colunas |
//--------------+
_oColumns := FWBrwColumn():New()
_oColumns:SetData({||_aTitulo[_oBrowseB:At()][COL_TITULO]})
_oColumns:SetTitle(RetTitle("E1_NUM"))
_oColumns:SetSize(TamSx3("E1_NUM")[1])
_oColumns:SetDecimal(TamSx3("E1_NUM")[2])
_oColumns:SetPicture(PesqPict("SE1","E1_NUM"))
_oBrowseB:SetColumns({_oColumns})

_oColumns := FWBrwColumn():New()
_oColumns:SetData({||_aTitulo[_oBrowseB:At()][COL_TPREFI]})
_oColumns:SetTitle(RetTitle("E1_PREFIXO"))
_oColumns:SetSize(TamSx3("E1_PREFIXO")[1])
_oColumns:SetDecimal(TamSx3("E1_PREFIXO")[2])
_oColumns:SetPicture(PesqPict("SE1","E1_PREFIXO"))
_oBrowseB:SetColumns({_oColumns})

_oColumns := FWBrwColumn():New()
_oColumns:SetData({||_aTitulo[_oBrowseB:At()][COL_TIDPAY]})
_oColumns:SetTitle(RetTitle("XTP_IDPAY"))
_oColumns:SetSize(TamSx3("XTP_IDPAY")[1])
_oColumns:SetDecimal(TamSx3("XTP_IDPAY")[2])
_oColumns:SetPicture(PesqPict("XTP","XTP_IDPAY"))
_oBrowseB:SetColumns({_oColumns})

_oColumns := FWBrwColumn():New()
_oColumns:SetData({||_aTitulo[_oBrowseB:At()][COL_TPARCELA]})
_oColumns:SetTitle(RetTitle("E1_PARCELA"))
_oColumns:SetSize(TamSx3("E1_PARCELA")[1])
_oColumns:SetDecimal(TamSx3("E1_PARCELA")[2])
_oColumns:SetPicture(PesqPict("SE1","E1_PARCELA"))
_oBrowseB:SetColumns({_oColumns})

_oColumns := FWBrwColumn():New()
_oColumns:SetData({||_aTitulo[_oBrowseB:At()][COL_TDTEMIS]})
_oColumns:SetTitle(RetTitle("E1_EMISSAO"))
_oColumns:SetSize(TamSx3("E1_EMISSAO")[1])
_oColumns:SetDecimal(TamSx3("E1_EMISSAO")[2])
_oColumns:SetPicture(PesqPict("SE1","E1_EMISSAO"))
_oBrowseB:SetColumns({_oColumns})

_oColumns := FWBrwColumn():New()
_oColumns:SetData({||_aTitulo[_oBrowseB:At()][COL_TDTPGTO]})
_oColumns:SetTitle(RetTitle("E1_VENCREA"))
_oColumns:SetSize(TamSx3("E1_VENCREA")[1])
_oColumns:SetDecimal(TamSx3("E1_VENCREA")[2])
_oColumns:SetPicture(PesqPict("SE1","E1_VENCREA"))
_oBrowseB:SetColumns({_oColumns})

_oColumns := FWBrwColumn():New()
_oColumns:SetData({||_aTitulo[_oBrowseB:At()][COL_TVLRTOT]})
_oColumns:SetTitle(RetTitle("E1_VALOR"))
_oColumns:SetSize(TamSx3("E1_VALOR")[1])
_oColumns:SetDecimal(TamSx3("E1_VALOR")[2])
_oColumns:SetPicture(PesqPict("SE1","E1_VALOR"))
_oBrowseB:SetColumns({_oColumns})

//_oBrowseA:SetEditCell(.T., {|| LiFatC01E(_oBrowseA,_oBrowseA:At()) })
_oBrowseB:SetLineHeight( 20 )
_oBrowseB:DeActivate()
_oBrowseB:Activate()
_oBrowseB:Refresh()

Return Nil 

/*********************************************************************************/
/*/{Protheus.doc} BPFINA13E
    @description Marca titulos aprovados
    @type  Static Function
    @author Bernard M. Margarido
    @since 25/03/2021
/*/
/*********************************************************************************/
Static Function BPFINA13E(_oBrowse,_oSay_02,_oSay_04,_oSay_06,_oSay_08,_oChkMark,_lTodos,_nTotTx,_nTotLiq,_nTotal,_nCount,_aArray,_nMark,_nGrid)
Local _nPos     := 0
Local _nLin     := _oBrowse:nAt
Local _nIDPay   := IIF(_nGrid == 1,COL_TIDPAY,COL_IDPAY)
Local _nParcela := IIF(_nGrid == 1,COL_TPARCELA,COL_PARCELA)
//Local _nValor   := IIF(_nGrid == 1,COL_VLRTOT,COL_TVLRTOT)
Local _nColMark := IIF(_nGrid == 1,COL_MARK,COL_TMARK)

If _nMark == 1
    //------------------------------------+
    // Valida se tem linha marcada        |
    // permite marcar somente um registro | 
    //------------------------------------+
    If _aArray[_nLin][_nColMark] == "LBNO"
        _aArray[_nLin][_nColMark] := "LBOK"
    ElseIf _aArray[_nLin][_nColMark] == "LBOK"
        _aArray[_nLin][_nColMark] := "LBNO"
    EndIf

    If _nGrid == 1
        //----------------------+
        // Marca o mesmo titulo |
        //----------------------+    
        If (_nPos := aScan(_aTitulo,{|x| RTrim(x[_nIDPay]) + RTrim(x[_nParcela]) == RTrim(_aArray[_nLin][COL_IDPAY]) + RTrim(_aArray[_nLin][COL_PARCELA]) }) ) > 0
            If _aTitulo[_nPos][_nColMark] == "LBNO"
                _aTitulo[_nPos][_nColMark] := "LBOK"
                _nTotal += _aArray[_nLin][COL_VLRTOT]
                _nTotTx += _aArray[_nLin][COL_VLRTAX]
                _nTotLiq+= _aArray[_nLin][COL_VLRLIQ]
                _nCount++
            ElseIf _aTitulo[_nLin][_nColMark] == "LBOK"
                _aTitulo[_nLin][_nColMark] := "LBNO"  
                _lTodos := .F.
                _nTotal -= _aArray[_nLin][COL_VLRTOT]
                _nTotTx -= _aArray[_nLin][COL_VLRTAX]
                _nTotLiq-= _aArray[_nLin][COL_VLRLIQ]
                _nCount--  
            EndIf
        ElseIf (_nPos := aScan(_aTitulo,{|x| RTrim(x[_nIDPay]) + RTrim(x[_nParcela]) == RTrim(_aArray[_nLin][COL_IDPAY]) + "" }) ) > 0
            If _aTitulo[_nPos][_nColMark] == "LBNO"
                _aTitulo[_nPos][_nColMark] := "LBOK"
                _nTotal += _aArray[_nLin][COL_VLRTOT]
                _nTotTx += _aArray[_nLin][COL_VLRTAX]
                _nTotLiq+= _aArray[_nLin][COL_VLRLIQ]
                _nCount++
            ElseIf _aTitulo[_nLin][_nColMark] == "LBOK"
                _aTitulo[_nLin][_nColMark] := "LBNO" 
                _lTodos := .F.
                _nTotal -= _aArray[_nLin][COL_VLRTOT]
                _nTotTx -= _aArray[_nLin][COL_VLRTAX]
                _nTotLiq-= _aArray[_nLin][COL_VLRLIQ]
                _nCount--   
            EndIf  
        EndIf

        //------------------+
        // Atualiza Browser |
        //------------------+
        _oBrowseB:GoTo(_nPos)
        _oBrowseB:Refresh()

    Else 

        //----------------------+
        // Marca o mesmo titulo |
        //----------------------+    
        If (_nPos := aScan(_aEcomm,{|x| RTrim(x[_nIDPay]) + RTrim(x[_nParcela]) == RTrim(_aArray[_nLin][COL_TIDPAY]) + RTrim(_aArray[_nLin][COL_TPARCELA]) }) ) > 0
            If _aEcomm[_nPos][_nColMark] == "LBNO"
                _aEcomm[_nPos][_nColMark] := "LBOK"
                _nTotal += _aEcomm[_nLin][COL_VLRTOT]
                _nTotTx += _aEcomm[_nLin][COL_VLRTAX]
                _nTotLiq+= _aEcomm[_nLin][COL_VLRLIQ]
                _nCount++
            ElseIf _aEcomm[_nLin][_nColMark] == "LBOK"
                _aEcomm[_nLin][_nColMark] := "LBNO"  
                _lTodos := .F.
                _nTotal -= _aEcomm[_nLin][COL_VLRTOT]
                _nTotTx -= _aEcomm[_nLin][COL_VLRTAX]
                _nTotLiq-= _aEcomm[_nLin][COL_VLRLIQ]
                _nCount--  
            EndIf
        ElseIf (_nPos := aScan(_aEcomm,{|x| RTrim(x[_nIDPay]) + RTrim(x[_nParcela]) == RTrim(_aArray[_nLin][COL_TIDPAY]) + "01" }) ) > 0
            If _aEcomm[_nPos][_nColMark] == "LBNO"
                _aEcomm[_nPos][_nColMark] := "LBOK"
                _nTotal += _aEcomm[_nLin][COL_VLRTOT]
                _nTotTx += _aEcomm[_nLin][COL_VLRTAX]
                _nTotLiq+= _aEcomm[_nLin][COL_VLRLIQ]
                _nCount++
            ElseIf _aEcomm[_nLin][_nColMark] == "LBOK"
                _aEcomm[_nLin][_nColMark] := "LBNO"  
                _lTodos := .F.
                _nTotal -= _aEcomm[_nLin][COL_VLRTOT]
                _nTotTx -= _aEcomm[_nLin][COL_VLRTAX]
                _nTotLiq-= _aEcomm[_nLin][COL_VLRLIQ]
                _nCount--  
            EndIf 
        ElseIf (_nPos := aScan(_aEcomm,{|x| RTrim(x[_nIDPay]) + "01" == RTrim(_aArray[_nLin][COL_TIDPAY]) + RTrim(_aArray[_nLin][COL_TPARCELA]) }) ) > 0
            If _aEcomm[_nPos][_nColMark] == "LBNO"
                _aEcomm[_nPos][_nColMark] := "LBOK"
                _nTotal += _aEcomm[_nLin][COL_VLRTOT]
                _nTotTx += _aEcomm[_nLin][COL_VLRTAX]
                _nTotLiq+= _aEcomm[_nLin][COL_VLRLIQ]
                _nCount++
            ElseIf _aEcomm[_nLin][_nColMark] == "LBOK"
                _aEcomm[_nLin][_nColMark] := "LBNO"  
                _lTodos := .F.
                _nTotal -= _aEcomm[_nLin][COL_VLRTOT]
                _nTotTx -= _aEcomm[_nLin][COL_VLRTAX]
                _nTotLiq-= _aEcomm[_nLin][COL_VLRLIQ]
                _nCount--  
            EndIf 
        EndIf

        //------------------+
        // Atualiza Browser |
        //------------------+
        _oBrowseA:GoTo(_nPos)
        _oBrowseA:Refresh()

    EndIf
EndIf

//------------------+
// Atualiza Browser |
//------------------+
_oBrowse:Refresh()

If ValType(_oSay_02) == "O" 
    _oSay_02:Refresh()
    _oSay_04:Refresh()
    _oSay_06:Refresh()
    _oSay_08:Refresh()
EndIf

If !_lTodos .And. ValType(_oChkMark) == "O"
    _oChkMark:Refresh()
EndIf 

Return .T.

/*********************************************************************************/
/*/{Protheus.doc} BPFINA13F
    @description Marca ou desmarca todos os titulos 
    @type  Static Function
    @author Bernard M. Margarido
    @since 25/03/2021
/*/
/*********************************************************************************/
Static Function BPFINA13F(_lMark,_nTotLiq,_nTotTx,_nTotal,_nCount,_oSay_02,_oSay_04,_oSay_06,_oSay_08)
Local _nX   := 1

_nTotLiq    := 0
_nTotTx     := 0
_nTotal     := 0 
_nCount     := 0

For _nX := 1 To Len(_aEcomm)
    If _lMark
        If _aEcomm[_nX][COL_STATUS] == "1"
            _aEcomm[_nX][COL_MARK]  := IIF(_lMark,"LBOK","LBNO")
            _nTotal += _aEcomm[_nX][COL_VLRTOT]
            _nTotLiq+= _aEcomm[_nX][COL_VLRLIQ]
            _nTotTx += _aEcomm[_nX][COL_VLRTAX]
            _nCount++
        EndIf 
    Else 
        _aEcomm[_nX][COL_MARK]  := IIF(_lMark,"LBOK","LBNO")
    EndIf
Next _nX 

For _nX := 1 To Len(_aTitulo)
    If _lMark
        If _aEcomm[_nX][COL_STATUS] == "1"
            _aTitulo[_nX][COL_TMARK]  := IIF(_lMark,"LBOK","LBNO")
        EndIf 
    Else 
        _aTitulo[_nX][COL_TMARK]  := IIF(_lMark,"LBOK","LBNO")
    EndIf 
Next _nX 

_oBrowseA:Refresh()
_oBrowseB:Refresh()

If ValType(_oSay_02) == "O" 
    _oSay_02:Refresh()
    _oSay_04:Refresh()
    _oSay_06:Refresh()
    _oSay_08:Refresh()
EndIf

Return Nil 

/*********************************************************************************/
/*/{Protheus.doc} BPFINA13G
    @description Realiza a baixa dos titulos eCommerce 
    @type  Static Function
    @author Bernard M. Margarido
    @since 22/07/2021
/*/
/*********************************************************************************/
Static Function BPFINA13G(_oSay)
Local _aArea    := GetArea()
Local _aBaixa   := {}

Local _cFilFat  := FWUUIDV4()
Local _cNumFat  := ""
Local _cCodTran := ""

Local _nX       := 0
Local _nValFat  := 0
Local _nValLiq  := 0
Local _nVlrTit  := 0

If !MsgYesNo("Confirma a conciliação dos pagametos marcados?","BEPI - Avisos!")
    RestArea(_aArea)
    Return .F.
EndIf 

//-------------------------------------+
// Orderna somente os titulos marcados |
//-------------------------------------+
_aTitulo := aSort(_aTitulo,,,{|x,y| x[1] > y[1]})
_oSay:cCaption := "Validando titulos marcados"
For _nX := 1 To Len(_aTitulo)
    If _aTitulo[_nX][COL_TMARK] == "LBOK"
        //-----------------------+
        // Cria Array para baixa | 
        //-----------------------+
        _nVlrTaxa := 0
        If ( _nPos := aScan(_aEcomm,{|x| RTrim(x[COL_IDPAY]) == RTrim(_aTitulo[_nX][COL_TIDPAY]) }) ) > 0
            _nVlrTaxa   := _aEcomm[_nPos][COL_VLRTAX]
            _nVlrLiq    := _aEcomm[_nPos][COL_VLRLIQ]
            _nVlrTit    := _aEcomm[_nPos][COL_VLRTOT]
        EndIf
        aAdd(_aBaixa,{_aTitulo[_nX][COL_TITULO],_aTitulo[_nX][COL_TPREFI],_aTitulo[_nX][COL_TPARCELA],_aTitulo[_nX][COL_TVLRTOT],_nVlrTaxa,_aTitulo[_nX][COL_TIDPAY],_nVlrLiq,_nVlrTit})
    EndIf
Next _nX 

//-----------------------------+
// Realiza a baixa dos titulos |
//-----------------------------+
If Len(_aBaixa) > 0

    //----------------------------+
    // Atualiza dados dos titulos |
    //----------------------------+
    BPFINA13I(_aBaixa,_cFilFat,@_nValFat,@_nValLiq,@_oSay)

    //------------------------------+
    // Cria fatura de transferencia |
    //------------------------------+
    If BPFINA13H(_cFilFat,_nValFat,_nValLiq,@_cNumFat,@_cCodTran,@_oSay)

        //-----------------------------+
        // Atualiza status conciliação |
        //-----------------------------+
        BPFINA13K(_aBaixa,@_oSay)

        //----------------------------------+
        // Envia transferencia para PagarMe |
        //----------------------------------+
        BPFINA13L(_cFilFat,_cNumFat,_cCodTran,@_oSay)
    EndIf

EndIf


RestArea(_aArea)
Return Nil

/*********************************************************************************/
/*/{Protheus.doc} BPFINA13H
    @description Realiza a baixa dos titulos eCommerce 
    @type  Static Function
    @author Bernard M. Margarido
    @since 22/07/2021
/*/
/*********************************************************************************/
Static Function BPFINA13H(_cFilFat,_nValFat,_nValLiq,_cNumFat,_cCodTran,_oSay)
Local _aArea            := GetArea()
Local _aCabec		    := {}
Local _aItens		    := {}
Local _aItem		    := {}
Local _aParcelas        := {}

Local _cErro            := ""
Local _cFiltro		    := ""
Local _cParcela         := ""
Local _c1DUP            := SuperGetMv("MV_1DUP")
Local _cCliAdm		    := GetNewPar("PG_CODADM","C20944")
Local _cLojaAdm		    := GetNewPar("PG_LOJADM","0000")
Local _cNumBco		    := PadR( Rtrim( GetMv( "PG_BCONUM" ,,"341") ), TamSX3("E1_BCOCHQ")[1] )
Local _cNumAg		    := PadR( Rtrim( GetMv( "PG_AGNUM" ,,"0002") ), TamSX3("E1_AGECHQ")[1] )
Local _cAccount		    := PadR( Rtrim( GetMv( "PG_NCONTA" ,,"358802") ), TamSX3("E1_CTACHQ")[1] )
Local _cCond		    := PadR( Rtrim( GetMv( "PG_CONDPG" ,,"001") ), TamSX3("E4_CODIGO")[1] )
Local _cNaturez		    := PadR( Rtrim( GetMv( "PG_NATUREZ" ,,"010212") ), TamSX3("E1_NATUREZ")[1] )
Local _cTipo		    := PadR( Rtrim( GetMv( "PG_TPFATEC" ,,"PGM") ), TamSX3("E1_TIPO")[1] )
Local _nMoeda		    := 1 
Local _cPrefixo		    := PadR( Rtrim( GetMv( "PG_PREFIX",,"PGM" ) ), TamSX3("E1_PREFIXO")[1] )

Local _nX               := 0
Local _nValXTB          := 0

Local _lRet             := .T.

Private lAutoErrNoFile  := .T.
Private lMsErroAuto     := .F.

//---------------+
// Numero Fatura |
//---------------+
dbSelectArea("SE1")
SE1->( dbSetOrder(1) )

_cNumFat := GetSxeNum("SE1","E1_NUM")

While SE1->( dbSeek(xFilial("SE1") + _cPrefixo + _cNumFat) )
	ConfirmSx8()
	_cNumFat := GetSxeNum("SE1","E1_NUM","",1)	
EndDo

//------------------+
// Cabeçalho Fatura |
//------------------+
aAdd(_aCabec, {"cCondicao"	, _cCond 	})
aAdd(_aCabec, {"cNatureza"	, _cNaturez })
aAdd(_aCabec, {"E1_TIPO" 	, _cTipo 	})
aAdd(_aCabec, {"cCliente"	, _cCliAdm 	})
aAdd(_aCabec, {"nMoeda"		, _nMoeda 	}) 
aAdd(_aCabec, {"cLoja"		, _cLojaAdm })

//_aParcelas  := Condicao( _nValFat, _cCond,, dDataBase)
_aParcelas  := Condicao( _nValLiq, _cCond,, dDataBase)
//_nValLiq    := IIF(Len(_aParcelas) > 1, _nValLiq / Len(_aParcelas), _nValLiq )

For _nX := 1 To Len(_aParcelas)

    _cParcela := IIF(Len(_aParcelas) > 1,LJParcela( _nX, _c1DUP ), "")
    _aItens   := {}
    _nValXTB  += _aParcelas[_nX][2]

    aAdd(_aItens, {"E1_PREFIXO"	, _cPrefixo				})		//Prefixo
    aAdd(_aItens, {"E1_BCOCHQ"	, _cNumBco 				})		//Banco
    aAdd(_aItens, {"E1_AGECHQ"	, _cNumAg 				})		//Agencia
    aAdd(_aItens, {"E1_CTACHQ"	, _cAccount 			})		//Conta
    aAdd(_aItens, {"E1_NUM"		, _cNumFat 				})		//Nro. cheque (dará origem ao numero do titulo)
    aAdd(_aItens, {"E1_PARCELA"	, _cParcela		        })		//Numero da Parcela
    aAdd(_aItens, {"E1_EMITCHQ"	, "PAGARME"				}) 		//Emitente do cheque
    aAdd(_aItens, {"E1_VENCTO"	, _aParcelas[_nX][1]    })		//Data boa 
    aAdd(_aItens, {"E1_VLCRUZ"	, _aParcelas[_nX][2]    })		//Valor do cheque/titulo
    //aAdd(_aItens, {"E1_XECOMME"	, "S"					})		//Titulo eCommerce
    //aAdd(_aItens, {"E1_DECRESC"	, _nValLiq		        })		//Valor do cheque/titulo

    aAdd(_aItem,_aItens)

Next _nX 

//--------+
// Filtro |
//--------+
_cFiltro := " E1_XFILFAT == '" + _cFilFat + "' "

//-------------+
// Gera Fatura |
//-------------+	
_oSay:cCaption := "Gerando fatura " + _cNumFat

If Len(_aItem) > 0 .And. Len(_aCabec) > 0

	//---------------+
	// Numero Fatura |
	//---------------+
	//dbSelectArea("SE1")
	//SE1->( dbSetOrder(1) )
	//SE1->( dbGoTop() )
    dbSelectArea("SA1")
    SA1->( dbSetOrder(1) )
    SA1->( dbSeek(xFilial("SA1") + PadR(_cCliAdm,TamSx3("A1_COD")[1]) + PadR(_cLojaAdm,TamSx3("A1_LOJA")[1])) )

	lMsErroAuto := .F.

    Pergunte("AFI460",.F.)
    SetMVValue("AFI460","mv_par01", 2)
    SetMVValue("AFI460","mv_par02", 2)
    SetMVValue("AFI460","mv_par03", 2)
    SetMVValue("AFI460","mv_par04", 1)
    SetMVValue("AFI460","mv_par05", 2)
    SetMVValue("AFI460","mv_par06", 2)
    SetMVValue("AFI460","mv_par07", 2)
    SetMVValue("AFI460","mv_par08", "")
    SetMVValue("AFI460","mv_par09", 1)

    SetFunName("FINA460")

    nModulo  := 6 
	FINA460(,_aCabec, _aItem, 3, _cFiltro)
		
	//------+
	// Erro | 
	//------+
	If lMsErroAuto
        
        RollBackSX8()

        _lRet  := .F.
		_cErro := "" 
    	
        AEval(GetAutoGRLog(), {|x| _cErro += x + CRLF})

        MsgInfo(_cErro, "BEPI - Avisos")
	Else
		ConfirmSX8()

        dbSelectArea("XTQ")
        XTQ->( dbSetOrder(1) )

        _cCodTrf    := GetSxeNum("XTQ","XTQ_CODIGO")

        While XTQ->( dbSeek(xFilial("XTQ") + _cCodTrf) )
            ConfirmSx8()
            _cCodTrf := GetSxeNum("XTQ","XTQ_CODIGO","",1)	
        EndDo

        RecLock("XTQ",.T.)
            XTQ->XTQ_FILIAL := xFilial("XTQ")
            XTQ->XTQ_CODIGO := _cCodTrf
            XTQ->XTQ_IDTRAN := ""
            XTQ->XTQ_DTEMIS := dDataBase
            XTQ->XTQ_DTBAIX := Nil 
            XTQ->XTQ_VALOR  := _nValXTB
            XTQ->XTQ_BANCO  := _cNumBco
            XTQ->XTQ_AGEN   := _cNumAg
            XTQ->XTQ_CONTA  := _cAccount
            XTQ->XTQ_NUMSE1 := _cNumFat
            XTQ->XTQ_PRESE1 := _cPrefixo
            XTQ->XTQ_STATUS := "1"
        XTQ->( MsUnLock() )  

        MsgInfo("Fatura " + _cNumFat + " " + _cPrefixo + " gerada com sucesso no valor de " + Alltrim(Transform(_nValXTB,PesqPict("SE1","E1_VALOR"))) + ".", "BEPI - Avisos")
	EndIf
EndIf	


RestArea(_aArea)
Return _lRet 

/*********************************************************************************/
/*/{Protheus.doc} BPFINA13I
    @description Realiza a atualização dos titulos ecommerce
    @type  Static Function
    @author Bernard M. Margarido
    @since 22/07/2021
/*/
/*********************************************************************************/
Static Function BPFINA13I(_aBaixa,_cFilFat,_nValFat,_nValLiq,_oSay)
Local _aArea    := GetArea()
Local _nVlrTit  := 0
Local _nVlrPGM  := 0 
Local _nTaxaPGM := 0
Local _nDesAcr  := 0
Local _nMoeda   := 1
Local _nTxMoeda := 0
Local _nX       := 0

//-----------------------------------+
// SE1 - Posiciona Titulos a Receber |
//-----------------------------------+
dbSelectArea("SE1")
SE1->( dbSetOrder(1) )

For _nX := 1 To Len(_aBaixa)
    _oSay:cCaption := "Atualizando titulos marcados " + RTrim(_aBaixa[_nX][2]) + RTrim(_aBaixa[_nX][1])
    If SE1->( dbSeek(xFilial("SE1") + _aBaixa[_nX][2] + _aBaixa[_nX][1] + _aBaixa[_nX][3] ) )
        _nTxMoeda:= RecMoeda(dDataBase, SE1->E1_MOEDA)
        _nVlrTit := _aBaixa[_nX][4]
        _nVlrPGM := _aBaixa[_nX][8]
        _nTaxaPGM:= _aBaixa[_nX][5]
        _nDesAcr := _aBaixa[_nX][4] - _aBaixa[_nX][7]
        _nValFat += _aBaixa[_nX][8]
        _nValLiq += _aBaixa[_nX][7]
        RecLock("SE1",.F.)
            If _nDesAcr > 0
                SE1->E1_SDDECRE := Round(NoRound(xMoeda( _nDesAcr,SE1->E1_MOEDA,_nMoeda, dDataBase,3,_nTxMoeda),3),2)
            ElseIf _nDesAcr < 0
                SE1->E1_SDACRES := Round(NoRound(xMoeda( ( _nDesAcr * -1) ,SE1->E1_MOEDA,_nMoeda, dDataBase,3,_nTxMoeda),3),2)
            EndIf 
            SE1->E1_XFILFAT := _cFilFat
        SE1->( MsUnLock() )
    EndIf
Next _nX 

RestArea(_aArea)
Return Nil 

/*********************************************************************************/
/*/{Protheus.doc} BPFINA13K
    @description Atualiza status conciliação eCommerce
    @type  Static Function
    @author Bernard M. Margarido
    @since 29/07/2021
/*/
/*********************************************************************************/
Static Function BPFINA13K(_aBaixa,_oSay)
Local _aArea    := GetArea()
Local _aCodConc := {}

Local _nX       := 0
Local _nTIdPay  := TamSx3("XTP_IDPAY")[1]
Local _nTParc   := TamSx3("XTP_PARC")[1]
//-----------------------------------+
// XTP - Posiciona titulos eCommerce |K
//-----------------------------------+
dbSelectArea("XTP")
XTP->( dbSetOrder(1) )

For _nX := 1 To Len(_aBaixa)
    _oSay:cCaption := "Atualizando status conciliação" + _aBaixa[_nX][6] + " " + _aBaixa[_nX][3]
    If XTP->( dbSeek(xFilial("XTP") + PadR(_aBaixa[_nX][6],_nTIdPay) + PadR(_aBaixa[_nX][3],_nTParc) ))
        
        If aScan(_aCodConc,{|x| RTrim(x) == RTrim(XTP->XTP_CODIGO)}) == 0
            aAdd(_aCodConc,XTP->XTP_CODIGO)
        EndIf
        
        RecLock("XTP",.F.)
            XTP->XTP_STATUS := "2"
        XTP->( MsUnLock() )
    EndIf 
Next _nX 

RestArea(_aArea)
Return Nil 

/*********************************************************************************/
/*/{Protheus.doc} BPFINA13J
    @description Valida se existem titulos marcados após confirmar
    @type  Static Function
    @author Bernard M. Margarido
    @since 22/07/2021
/*/
/*********************************************************************************/
Static Function BPFINA13J()
Local _lRet := .T. 

If aScan(_aEcomm,{|x| RTrim(x[COL_MARK]) == "LBOK"}) == 0
    MsgInfo("Não é possivel realizar a conciliação. Não existem titulos marcados.","BEPI - Avisos")
    _lRet := .F.
EndIf 

//------------------------+
// Valida dados bancários |
//------------------------+
If Empty(GetMv("PG_BCONUM"))
    MsgInfo("Não é possivel realizar a conciliação. Não existem as informações bancárias. " + CRLF + " Favor cadastrar os dados bancários para realizar a conciliação","BEPI - Avisos")
    _lRet := .F.
EndIf

Return _lRet

/********************************************************************************/
/*/{Protheus.doc} BPFINA13L
    @description Realiza o envio da transferencia para PagarMe
    @type  Static Function
    @author Bernard M Margarido
    @since 04/04/2023
    @version version
/*/
/********************************************************************************/
Static Function BPFINA13L(_cFilFat,_cNumFat,_cCodTran,_oSay)
Local _aArea    := GetArea() 

Local _cPrefixo	:= PadR( Rtrim( GetMv( "PG_PREFIX",,"ECO" ) ), TamSX3("E1_PREFIXO")[1] )
Local _cRecID   := ""
Local _cRest    := ""
Local _cMsg     := ""

Local _oJSon    := Nil 
Local _oJSonRet := Nil 
Local _oPagarMe := Nil 

Local _lGrava   := .F.

//--------------------------------+
// Localiza ID conta do recebedor |
//--------------------------------+
If !BPFINA13M(@_cRecID)
    MsgStop("ID conta recebedor não localizado!","BEPI - Avisos")
    RestArea(_aArea)
    Return .F.
EndIf 

//------------------------+
// SE1 - Posiciona Fatura |
//------------------------+
dbSelectArea("SE1")
SE1->( dbSetOrder(1) )
If !SE1->( dbSeek(xFilial("SE1") + _cPrefixo + _cNumFat))
    MsgStop("Fatura " + _cNumFat + " não localizada!","BEPI - Avisos")
    RestArea(_aArea)
    Return .F.
EndIf 

//--------------------------------------------+
// Cria interface para envio da Transferencia | 
//--------------------------------------------+
_oJSon  := JSonObject():New()
_oJSon["amount"]       := SE1->E1_SALDO * 100
_oJSon["recipient_id"] := _cRecID
_oJSon["metadata"]     := ""

//-------------------+
// Cria arquivo JSon |
//-------------------+
_cRest := _oJSon:ToJson(_oJSon)

//--------------------------------------+
// Envia transferencia para o eCommerce |
//--------------------------------------+
_oPagarMe       := PagarMe():New() 
_oPagarMe:cJson := _cRest
If _oPagarMe:Transferencia()

    If FWJsonDeserialize(_oPagarMe:cRetJSon,@_oJSonRet)
        _cID    := _oJSonRet:id
        _lGrava := .T.
        _cMsg   := "Transferencia enviada com sucesso. ID " + cValToChar(_cID)
    EndIf
Else
    _cMsg := "Erro ao enviar transferencia: " + _oPagarMe:cError
EndIf 

//-------------------------+
// Atualiza transferencia  |
//-------------------------+
If _lGrava 
    dbSelectArea("XTQ")
    XTQ->( dbSetOrder(1) )
    If XTQ->( dbSeek(xFilial("XTQ") + _cCodTran ) )
        RecLock("XTQ",.F.)
            XTQ->XTQ_IDTRAN := cValToChar(_cID)
            XTQ->XTQ_STATUS := "2"
        XTQ->( MsUnLock() )
    EndIf 
EndIF 

MsgAlert(_cMsg,"BEPI - Avisos")

RestArea(_aArea)
Return Nil 

/************************************************************************************/
/*/{Protheus.doc} BPFINA13M
    @description Consulta ID conta recebedor
    @type  Static Function
    @author Bernard M Margarido
    @since 04/04/2023
    @version version
/*/
/************************************************************************************/
Static Function BPFINA13M(_cRecID)
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
