#INCLUDE "PROTHEUS.CH"
#INCLUDE "TBICONN.CH"
#INCLUDE "COLORS.CH"
#INCLUDE "TBICODE.CH"
#INCLUDE "TOPCONN.CH"
#INCLUDE "RWMAKE.CH"

#DEFINE CRLF CHR(13) + CHR(10)

/************************************************************************************/
/*/{Protheus.doc} ECLOJ011
    @description Monitor de Integrações 
    @author Bernard M. Margarido
    @since 29/04/2019
    @version undefined
    @type function
/*/
/************************************************************************************/
User Function ECLOJ011()
Local _aArea    := GetArea()

Local _oDlg     := Nil
Local _oLayer   := Nil 
Local _oPanel   := Nil
Local _oGroup01 := Nil
Local _oGroup02 := Nil 

Local _bFlagEco := { || Processa({|| EcLoj011F() }, "Verificando tabelas para envio p/ e-Commerce ...") }

Private _cCodTab:= GetNewPar("EC_TABECO")    
Private _cFilEst:= GetNewPar("EC_FILEST")
Private _cLocal := FormatIn(GetNewPar("EC_ARMAZEM"),"/")

Private _lAY1   := .F.
Private _lAY2   := .F.
Private _lWS5   := .F.
Private _lSB4   := .F.
Private _lSB1   := .F.
Private _lSB2   := .F.
Private _lDA1   := .F.
Private _lATV   := .F.

//---------------+
// Inicia rotina |
//---------------+
Processa({|| EcLoj011F() }, "Verificando tabelas para envio p/ e-Commerce ...")

//------------------------+
// Monta Dialog principal |
//------------------------+
_oDlg := MsDialog():New(000, 000, 375, 615,"Monitor Integrações",,,,,,,,,.T.)

    //--------------+
	// Painel Layer |
	//--------------+
	_oLayer := FwLayer():New()
	_oLayer:Init(_oDlg,.F.)  
	  
   	//------------+
	// 1o. Painel |
	//------------+
	_oLayer:addLine("LINE01",100, .F.)
	_oLayer:addCollumn( "COLMONIT",100, .F. , "LINE01")
	_oLayer:addWindow("COLMONIT", "WNDMONIT", "WebServices e-Commerce", 100, .F., .F., , "LINE01") 
	_oPanel := _oLayer:GetWinPanel("COLMONIT","WNDMONIT","LINE01")

    //----------------------------------------+
    // Grupo integrações Protheus X eCommerce |
    //----------------------------------------+
    _oGroup01 := TGroup():New(002,001,080,300,"Integração - Protheus X e-Commerce",_oPanel,,,.T.)

    @ 030,010 Button "01 ) Categoria"				Size 92,12 When _lAY1  Action ( U_AECOI001() 	, Eval(_bFlagEco) ) Pixel
    @ 030,110 Button "02 ) Marcas"					Size 92,12 When _lAY2  Action ( U_AECOI002()	, Eval(_bFlagEco) ) Pixel
    @ 030,210 Button "02 ) Grupos Especiais"    	Size 92,12 When _lWS5  Action ( U_AECOI002()	, Eval(_bFlagEco) ) Pixel

    @ 045,010 Button "03 ) Produto"			        Size 92,12 When _lSB4  Action ( U_AECOI003() 	, Eval(_bFlagEco) ) Pixel
	@ 045,110 Button "04 ) Produto Sku"		        Size 92,12 When _lSB1  Action ( U_AECOI004() 	, Eval(_bFlagEco) ) Pixel
    @ 045,210 Button "05 ) Atualiza Preço" 			Size 92,12 When _lDA1  Action ( U_AECOI009()	, Eval(_bFlagEco) ) Pixel

	@ 060,010 Button "06 ) Atualiza Estoque" 		Size 92,12 When _lSB2  Action ( U_AECOI008() 	, Eval(_bFlagEco) ) Pixel
	//@ 060,110 Button "07 ) Ativa Prod. X Sku"		Size 92,12 When _lATV  Action ( U_AECOI012()	, Eval(_bFlagEco) ) Pixel

    //----------------------------------------+
    // Grupo integrações eCommerce X Protheus |
    //----------------------------------------+
    _oGroup02 := TGroup():New(082,001,142,300,"Integração - e-Commerce X Protheus",_oPanel,,,.T.)

    @ 112,010 Button "01 ) Ws Pedidos Novos" 	Size 92,12 When .T.		Action U_AECOI011()								Pixel

_oDlg:Activate(,,,.T.,,,)

RestArea(_aArea)
Return Nil

/************************************************************************************/
/*/{Protheus.doc} EcLoj011F
    @description Monitor de Integrações 
    @author Bernard M. Margarido
    @since 29/04/2019
    @version undefined
    @type function
/*/
/************************************************************************************/
Static Function EcLoj011F()
Local _cQuery  := ""

//--------------------+  
// Restaura variaveis |
//--------------------+
_lAY1   := .F.
_lAY2   := .F.
_lSB4   := .F.
_lSB1   := .F.
_lWS5   := .F.

//------------------------+
// regua de processamento |
//------------------------+
ProcRegua(11)

//------------+
// Categorias |
//------------+
IncProc("Categorias... ")

_cQuery := "	SELECT " + CRLF
_cQuery += "		COUNT(*) TOTAL " + CRLF
_cQuery += "	FROM " + CRLF 
_cQuery += "		 " + RetSqlName("AY1") + " AY1 " + CRLF
_cQuery += "		INNER JOIN " + RetSqlName("AY0") + " AY0 ON AY0.AY0_CODIGO = AY1.AY1_SUBCAT AND AY0.AY0_ENVECO = '1' AND AY0.D_E_L_E_T_ = '' " + CRLF
_cQuery += "	WHERE " + CRLF
_cQuery += "		AY1.D_E_L_E_T_ = ''  

_lAY1   := EcLoj011Qry(_cQuery)

//--------+
// Marcas |
//--------+
IncProc("Marcas...")

_cQuery := "	SELECT " + CRLF
_cQuery += "		COUNT(*) TOTAL " + CRLF
_cQuery += "	FROM " + CRLF
_cQuery += "		" + RetSqlName("AY2") + " AY2 " + CRLF
_cQuery += "	WHERE " + CRLF
_cQuery += "		AY2.AY2_FILIAL = '" + xFilial("AY2") + "' AND " + CRLF
_cQuery += "		AY2.AY2_ENVECO = '1' AND " + CRLF 
_cQuery += "		AY2.D_E_L_E_T_ = '' " 

_lAY2   := EcLoj011Qry(_cQuery)

//-----------------------------+
// Grupo de Campos especificos |
//-----------------------------+
IncProc("Grupo de campos especificos...")

_cQuery := " SELECT " + CRLF
_cQuery += "    COUNT(*) TOTAL " + CRLF
_cQuery += " FROM " + CRLF
_cQuery += "    " + RetSqlName("WS5") + " WS5 (NOLOCK) " + CRLF
_cQuery += "	INNER JOIN " + RetSqlName("AY0") + " AY0 (NOLOCK) ON AY0.AY0_FILIAL = '" + xFilial("AY0") + "' AND AY0.AY0_CODIGO = WS5.WS5_CAT01 AND AY0.D_E_L_E_T_ = '' " + CRLF
_cQuery += " WHERE " + CRLF
_cQuery += "    WS5.WS5_FILIAL = '" + xFilial("WS5") + "' AND " + CRLF
_cQuery += "	WS5.WS5_ENVECO = '1' AND " + CRLF
_cQuery += "	WS5.D_E_L_E_T_ = '' "

_lWS5   := EcLoj011Qry(_cQuery)

//--------------+
// Produtos Pai |
//--------------+
IncProc("Produto Pai... ")

_cQuery := " SELECT " + CRLF
_cQuery += "	COUNT(*) TOTAL " + CRLF
_cQuery += " FROM " + CRLF
_cQuery += "	" + RetSqlName("SB5") + " B5 " + CRLF
_cQuery += " WHERE " + CRLF
_cQuery += "	B5.B5_FILIAL = '" + xFilial("SB5") + "' AND " + CRLF  
_cQuery += "	B5.B5_XENVECO = '1' AND " + CRLF
_cQuery += "	B5.B5_XENVSKU = '1' AND " + CRLF
_cQuery += "	B5.B5_XUSAECO = 'S' AND " + CRLF
_cQuery += "	B5.D_E_L_E_T_ = ''"

_lSB4   := EcLoj011Qry(_cQuery)

//-----+
// SKU |
//-----+

IncProc("SKU ...")

_cQuery := " SELECT " + CRLF
_cQuery += "    COUNT(*) TOTAL " + CRLF
_cQuery += " FROM " + CRLF 
_cQuery += "    " + RetSqlName("SB5") + " B5 " + CRLF   
_cQuery += "    INNER JOIN " + RetSqlName("SB1") + " B1 ON B1.B1_FILIAL = '" + xFilial("SB1") + "' AND B1.B1_COD = B5.B5_COD AND B1.B1_MSBLQL <> '1' AND B1.D_E_L_E_T_ = '' " + CRLF
_cQuery += "	INNER JOIN " + RetSqlName("AY2") + " AY2 ON AY2.AY2_FILIAL = '" + xFilial("AY2") + "' AND AY2.AY2_CODIGO = B5.B5_XCODMAR AND AY2.D_E_L_E_T_ = '' " + CRLF
_cQuery += "	LEFT OUTER JOIN " + RetSqlName("DA1") + " DA1 ON DA1.DA1_FILIAL = '" + xFilial("DA1") + "' AND DA1.DA1_CODTAB = '" + _cCodTab + "' AND DA1.DA1_CODPRO = B1.B1_COD AND DA1.D_E_L_E_T_ = '' " + CRLF
_cQuery += " WHERE " + CRLF
_cQuery += "	B5.B5_FILIAL = '" + xFilial("SB5") + "' AND " + CRLF  
_cQuery += "	B5.B5_XUSAECO = 'S' AND " + CRLF
_cQuery += "	B5.B5_XENVECO = '2' AND " + CRLF
_cQuery += "	B5.B5_XENVSKU = '1' AND " + CRLF
_cQuery += "	B5.D_E_L_E_T_ = ' ' "

_lSB1   := EcLoj011Qry(_cQuery)

//---------+
// Estoque |
//---------+
IncProc("Estoques...")

_cQuery := " SELECT " + CRLF
_cQuery += "    COUNT(*) TOTAL " + CRLF
_cQuery += " FROM " + CRLF
_cQuery += "	" + RetSqlName("SB2") + " B2 " + CRLF
_cQuery += "	INNER JOIN " + RetSqlName("SB1") + " B1 ON B1.B1_FILIAL = '" + xFilial("SB1") + "' AND B1.B1_COD = B2.B2_COD AND B1.B1_MSBLQL <> '1' AND B1.D_E_L_E_T_ = '' " + CRLF 
_cQuery += "	INNER JOIN " + RetSqlName("SB5") + " B5 ON B5.B5_FILIAL = '" + xFilial("SB5") + "' AND B5.B5_COD = B2.B2_COD AND B5.B5_XENVECO = '2' AND B5.B5_XENVSKU = '2' AND B5.B5_XUSAECO = 'S' AND B5.D_E_L_E_T_ = '' " + CRLF
_cQuery += " WHERE " + CRLF
_cQuery += "	B2.B2_FILIAL = '" + _cFilEst  + "' AND " + CRLF 
_cQuery += "	B2.B2_LOCAL IN " + _cLocal + " AND " + CRLF
_cQuery += "	B2.B2_MSEXP = '' AND " + CRLF
_cQuery += "	B2.D_E_L_E_T_ = '' "

_lSB2   := EcLoj011Qry(_cQuery)

//------------+
// Preços Pai |
//------------+
IncProc("Verificando Preços Produtos ")

_cQuery := "	SELECT " + CRLF
_cQuery += "		COUNT(*) TOTAL " + CRLF
_cQuery += "	FROM " + CRLF
_cQuery += "		" + RetSqlName("DA1") + " DA1 " + CRLF 
_cQuery += "		INNER JOIN " + RetSqlName("DA0") + " DA0 ON DA0.DA0_FILIAL = '" + xFilial("DA0") + "' AND DA0.DA0_CODTAB = DA1.DA1_CODTAB AND DA0.D_E_L_E_T_ = '' " + CRLF 
_cQuery += "		INNER JOIN " + RetSqlName("SB5") + " B5 ON B5.B5_FILIAL = '" + xFilial("SB5") + "' AND B5.B5_COD = DA1.DA1_CODPRO AND B5.B5_XENVECO = '2' AND B5.B5_XENVSKU = '2' AND B5.B5_XUSAECO = 'S' AND B5.D_E_L_E_T_ = '' " + CRLF 
_cQuery += "		INNER JOIN " + RetSqlName("SB1") + " B1 ON B1.B1_FILIAL = '" + xFilial("SB1") + "' AND B1.B1_COD = DA1.DA1_CODPRO AND B1.B1_MSBLQL <> '1' AND B1.D_E_L_E_T_ = '' " + CRLF 
_cQuery += "	WHERE " + CRLF 
_cQuery += "		DA1.DA1_FILIAL = '" + xFilial("DA1") + "' AND " + CRLF 
_cQuery += "		DA1.DA1_CODTAB = '" + _cCodTab + "' AND " + CRLF
_cQuery += "		DA1.DA1_XENVEC = '1' AND " + CRLF  
_cQuery += "		DA1.D_E_L_E_T_ = '' "

_lDA1   := EcLoj011Qry(_cQuery)

Return Nil

/************************************************************************************/
/*/{Protheus.doc} EcLoj011F
    @description Consulta fila de integração
    @author Bernard M. Margarido
    @since 29/04/2019
    @version undefined
    @type function
/*/
/************************************************************************************/
Static Function EcLoj011Qry(_cQuery)
Local _cAlias   := ""
Local _lRet     := .T.

_cAlias := MPSysOpenQuery(_cQuery)

If (_cAlias)->( Eof() )
    Return .F.
EndIf

_lRet := IIF((_cAlias)->TOTAL > 0 , .T., .F.)

(_cAlias)->( dbCloseArea() )

Return _lRet
