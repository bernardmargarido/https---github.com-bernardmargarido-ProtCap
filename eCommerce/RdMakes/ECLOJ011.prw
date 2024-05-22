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

Private _lACU   := .F.
Private _lZTD   := .F.
Private _lZTI   := .F.
Private _lZTE   := .F.
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

    @ 030,010 Button "01 ) Categoria"				Size 92,12 When _lACU  Action ( U_AECOI001() 	, Eval(_bFlagEco) ) Pixel
    @ 030,110 Button "02 ) Marcas"					Size 92,12 When _lZTD  Action ( U_AECOI002()	, Eval(_bFlagEco) ) Pixel
    @ 030,210 Button "03 ) Grupos Especificos"    	Size 92,12 When _lZTI  Action ( U_AECOI014()	, Eval(_bFlagEco) ) Pixel

    @ 045,010 Button "04 ) Campos Especificos"      Size 92,12 When _lZTE  Action ( U_AECOI015() 	, Eval(_bFlagEco) ) Pixel
    @ 045,110 Button "05 ) Produtos "		        Size 92,12 When _lSB4  Action ( U_AECOI003() 	, Eval(_bFlagEco) ) Pixel
	@ 045,210 Button "06 ) SKU's"   		        Size 92,12 When _lSB1  Action ( U_AECOI004() 	, Eval(_bFlagEco) ) Pixel
    
	@ 060,010 Button "07 ) Atualiza Estoque" 		Size 92,12 When _lSB2  Action ( U_AECOI008() 	, Eval(_bFlagEco) ) Pixel
    @ 060,110 Button "08 ) Atualiza Preço" 			Size 92,12 When _lDA1  Action ( U_AECOI009()	, Eval(_bFlagEco) ) Pixel
	//@ 060,110 Button "09 ) Ativa Prod. X Sku"		Size 92,12 When _lATV  Action ( U_AECOI012()	, Eval(_bFlagEco) ) Pixel

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
_lACU   := .F.
_lZTD   := .F.
_lZTI   := .F.
_lZTE   := .F.
_lSB4   := .F.
_lSB1   := .F.
_lSB2   := .F.
_lDA1   := .F.
_lATV   := .F.

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
_cQuery += "		 " + RetSqlName("ACU") + " ACU (NOLOCK)  " + CRLF
_cQuery += " WHERE " + CRLF
_cQuery += "		ACU.ACU_FILIAL = '" + xFilial("ACU") + "' AND " + CRLF
_cQuery += "		ACU.ACU_XINTLV = '1' AND " + CRLF
_cQuery += "		ACU.D_E_L_E_T_ = '' " + CRLF

_lACU   := EcLoj011Qry(_cQuery)

//--------+
// Marcas |
//--------+
IncProc("Marcas...")

_cQuery := "	SELECT " + CRLF
_cQuery += "		COUNT(*) TOTAL " + CRLF
_cQuery += "	FROM " + CRLF
_cQuery += "		" + RetSqlName("ZTD") + " ZTD (NOLOCK) " + CRLF
_cQuery += "	WHERE " + CRLF
_cQuery += "		ZTD.ZTD_FILIAL = '" + xFilial("ZTD") + "' AND " + CRLF 
_cQuery += "		ZTD.ZTD_INTLV = '1' AND " + CRLF 
_cQuery += "		ZTD.D_E_L_E_T_ = '' "

_lZTD   := EcLoj011Qry(_cQuery)

//-----------------------------+
// Grupo de Campos especificos |
//-----------------------------+
IncProc("Grupo especificos...")

_cQuery := " SELECT " + CRLF
_cQuery += "    COUNT(*) TOTAL " + CRLF
_cQuery += " FROM " + CRLF
_cQuery += "    " + RetSqlName("ZTI") + " ZTI (NOLOCK) " + CRLF
_cQuery += " WHERE " + CRLF
_cQuery += "	ZTI_FILIAL = '" + xFilial("ZTI") + "' AND " + CRLF
_cQuery += "	ZTI_INTLV = '1' AND " + CRLF
_cQuery += "	ZTI.D_E_L_E_T_ = '' "

_lZTI   := EcLoj011Qry(_cQuery)

//--------------------+
// Campos especificos |
//--------------------+
IncProc("Campos especificos...")

_cQuery := " SELECT " + CRLF
_cQuery += "    COUNT(*) TOTAL " + CRLF
_cQuery += " FROM " + CRLF
_cQuery += "    " + RetSqlName("ZTE") + " ZTE (NOLOCK) " + CRLF
_cQuery += " WHERE " + CRLF
_cQuery += "	ZTE.ZTE_FILIAL = '" + xFilial("ZTE") + "' AND " + CRLF
_cQuery += "	ZTE.ZTE_XINTLV = '1' AND " + CRLF
_cQuery += "	ZTE.D_E_L_E_T_ = '' "

_lZTE   := EcLoj011Qry(_cQuery)

//--------------+
// Produtos Pai |
//--------------+
IncProc("Produto Pai... ")

_cQuery := " SELECT " + CRLF
_cQuery += "    TOTAL " + CRLF
_cQuery += " FROM " + CRLF
_cQuery += " ( " + CRLF
_cQuery += "    SELECT " + CRLF
_cQuery += "    	COUNT(*) TOTAL " + CRLF
_cQuery += "    FROM " + CRLF
_cQuery += "    	" + RetSqlName("SB4") + " B4 (NOLOCK) " + CRLF
_cQuery += "    WHERE " + CRLF
_cQuery += "    	B4.B4_FILIAL = '" + xFilial("SB4") + "' AND " + CRLF
_cQuery += "    	B4.B4_XINTLV  = '1' AND " + CRLF
_cQuery += "    	B4.D_E_L_E_T_ = '' " + CRLF
_cQuery += "	UNION ALL " + CRLF
_cQuery += "    SELECT " + CRLF
_cQuery += "    	COUNT(*) TOTAL " + CRLF
_cQuery += "    FROM " + CRLF
_cQuery += "    	" + RetSqlName("SB1") + " B1 (NOLOCK) " + CRLF
_cQuery += "    WHERE " + CRLF
_cQuery += "		B1.B1_FILIAL = '" + xFilial("SB1") + "' AND " + CRLF
_cQuery += "		B1.B1_GRADE IN('','N') AND " + CRLF
_cQuery += "		B1.B1_XINTLV = '1' AND " + CRLF
_cQuery += "		B1.D_E_L_E_T_ = '' " + CRLF
_cQuery += ") PRODUTOS_PAI "

_lSB4   := EcLoj011Qry(_cQuery)

//-----+
// SKU |
//-----+

IncProc("SKU ...")

_cQuery := " SELECT " + CRLF
_cQuery += "    COUNT(*) TOTAL " + CRLF
_cQuery += " FROM " + CRLF 
_cQuery += "    " + RetSqlName("SB1") + " B1 " + CRLF   
_cQuery += " WHERE " + CRLF
_cQuery += "	B1.B1_FILIAL = '" + xFilial("SB1") + "' AND " + CRLF
_cQuery += "	B1.B1_XINTLV = '1' AND " + CRLF
_cQuery += "	B1.D_E_L_E_T_ = '' " + CRLF	

_lSB1   := EcLoj011Qry(_cQuery)

//---------+
// Estoque |
//---------+
IncProc("Estoques...")

_cQuery := " SELECT " + CRLF
_cQuery += "    COUNT(*) TOTAL " + CRLF
_cQuery += " FROM " + CRLF
_cQuery += "	" + RetSqlName("SB2") + " B2 " + CRLF
_cQuery += "	INNER JOIN " + RetSqlName("SB1") + " B1 ON B1.B1_FILIAL = '" + xFilial("SB1") + "' AND B1.B1_COD = B2.B2_COD AND B1.B1_MSBLQL <> '1' AND B1.B1_XINTLV = '2' AND B1.B1_XIDSKU > 0 AND B1.D_E_L_E_T_ = '' " + CRLF 
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

_cQuery := " SELECT " + CRLF
_cQuery += "	COUNT(*) TOTAL " + CRLF
_cQuery += " FROM " + CRLF
_cQuery += "	" + RetSqlName("SB1") + " B1 " + CRLF 
_cQuery += " WHERE " + CRLF 
_cQuery += "	B1.B1_FILIAL = '" + xFilial("SB1") + "' AND " + CRLF
_cQuery += "	B1.B1_XIDSKU > 0 AND " + CRLF
_cQuery += "	B1.B1_XINTLV = '2' AND " + CRLF
_cQuery += "	B1.B1_MSEXP = '' AND " + CRLF 
_cQuery += "	B1.D_E_L_E_T_ = '' " 

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
