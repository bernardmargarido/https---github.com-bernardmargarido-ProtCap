#INCLUDE "TOTVS.CH"
#INCLUDE "PROTHEUS.CH"
#INCLUDE "FWMVCDEF.CH"

/*************************************************************************************************************/
/*/{Protheus.doc} BPFINA11
    @description Browser Inicial com os pagamentos por data 
    @type  Function
    @author Bernard M. Margarido
    @since 15/07/2021
/*/
/*************************************************************************************************************/
User Function BPFINA11()
Private _nOldLen := SetVarNameLen(255) 
Private _oBrowse := Nil 

//------------------------------------+
// Instanciamento da Classe FWMBrowse |
//------------------------------------+
_oBrowse := FWMBrowse():New()
//-----------------+
// Alias utilizado |
//-----------------+
_oBrowse:SetAlias("XTP")
//-------------------+
// Adiciona Legendas |
//-------------------+
_oBrowse:AddLegend( "XTP_STATUS == '1'", "GREEN"    , "Em Aberto" )
_oBrowse:AddLegend( "XTP_STATUS == '2'", "RED"      , "Conciliado" )

//------------------+
// Titulo do Browse |
//------------------+
_oBrowse:SetDescription('Conciliação Pagar.Me')

//--------------------+
// Ativação do Browse |
//--------------------+
_oBrowse:Activate()
SetVarNameLen(_nOldLen)

Return Nil 

/************************************************************************************/
/*/{Protheus.doc} ModelDef
@description  Modelo de dados, estrutura dos dados e modelo de negocio
@author Bernard M. Margarido
@since 10/08/2017
@version undefined
@type function
/*/
/************************************************************************************/
Static Function ModelDef()
Local _oModel		:= Nil
Local _oStruXTP     := Nil

//Local _bCommit      := {|_oModel| DFinaA01A(_oModel)}
//-----------------+
// Monta Estrutura |
//-----------------+
_oStruXTP   := FWFormStruct(1,"XTP")

//-------+
// Model |
//-------+
_oModel 	:= MPFormModel():New('BPFINA_01', /*bPreValid*/ , /*_bPosValid*/ , /*_bCommit*/ , /*_bCancel*/ )
_oModel:SetDescription('Conciliação Pagar.Me')

//-----------------+
// Adiciona campos | 
//-----------------+
_oModel:addFields('XTP_01',,_oStruXTP)

//----------------+
// Chave primaria | 
//----------------+
_oModel:SetPrimaryKey({"XTP_FILIAL","XTP_CODIGO"})

Return _oModel

/************************************************************************************/
/*/{Protheus.doc} ViewDef
    @description Cria interface com o usuario
    @author Bernard M. Margarido
    @since 10/08/2017
    @version undefined
    @type function
/*/
/************************************************************************************/
Static Function ViewDef() 
Local _oView        
Local _oModel
Local _oStrViewXTP	:= Nil

//-------------------------+
// Carrega Modelo de Dados | 
//-------------------------+
_oModel := FWLoadModel("BPFINA11")

//--------------------------------------+
// Cria a estrutura a ser usada na View |
//--------------------------------------+
_oStrViewXTP	:= FWFormStruct( 2,'XTP') 

//---------------------+
// Instancia Interface |
//---------------------+
_oView	:= FWFormView():New()
_oView:SetModel(_oModel)
_oView:SetDescription('Conciliação Pagar.Me')

//---------------------+
// View das estruturas |
//---------------------+
_oView:AddField('XTP_FORM' 	, _oStrViewXTP , 'XTP_01' )

//------------------------------------------------------------+
// Criar "box" horizontal para receber algum elemento da view |
//------------------------------------------------------------+
_oView:CreateHorizontalBox( 'SUP_01' , 100 ,,, /*'PASTAS'*/, /*'ABA01'*/ )

_oView:SetOwnerView('XTP_FORM'	    ,'SUP_01')

//------------------------+
// Titulo componente GRID |
//------------------------+
_oView:EnableTitleView('XTP_FORM','Dados Recebiveis')

//-------------------+
// Adicionando botão | 
//-------------------+
//_oView:AddUserButton( 'Visualiza NF-e', 'CLIPS', {|_oView| U_BSFATA07() } )

Return _oView 

/************************************************************************************/
/*/{Protheus.doc} BPFIN11A
    @descriptiom Estorna conciliação 
    @type user function
    @author Bernard M Margarido
    @since 07/04/2025
    @version version
/*/
/************************************************************************************/
User Function BPFIN11A(_cFilial,_cNumLiq)
Local _aArea    := GetArea()

Local _cQuery   := ""
Local _cAlias   := ""

_cQuery := " SELECT " + CRLF
_cQuery += "	FO0_FILIAL, " + CRLF 
_cQuery += "	FO0_CLIENT, " + CRLF
_cQuery += "	FO0_LOJA, " + CRLF
_cQuery += "	FK7_PREFIX, " + CRLF
_cQuery += "	FK7_NUM, " + CRLF
_cQuery += "	FK7_PARCEL, " + CRLF
_cQuery += "	E1_XNSUCAR, " + CRLF
_cQuery += "	XTP.XTP_IDPAY, " + CRLF
_cQuery += "	XTP.XTP_PARC, " + CRLF
_cQuery += "	XTP.R_E_C_N_O_ RECNOXTP " + CRLF
_cQuery += " FROM " + CRLF
_cQuery += "	" + RetSqlName("FO0") + " FO0 " + CRLF
_cQuery += "	INNER JOIN " + RetSqlName("FO1") + " FO1 ON FO1.FO1_FILIAL = FO0.FO0_FILIAL AND FO1.FO1_PROCES = FO0.FO0_PROCES AND FO1.D_E_L_E_T_ = '' " + CRLF
_cQuery += "	INNER JOIN " + RetSqlName("FK7") + " FK7 ON FK7.FK7_FILTIT = FO1.FO1_FILIAL AND FK7.FK7_IDDOC = FO1.FO1_IDDOC AND FK7.D_E_L_E_T_ = '' " + CRLF
_cQuery += "	INNER JOIN " + RetSqlName("SE1") + " SE1 ON SE1.E1_FILIAL = FK7.FK7_FILTIT AND SE1.E1_PREFIXO = FK7.FK7_PREFIX	AND SE1.E1_NUM = FK7.FK7_NUM AND SE1.E1_PARCELA = FK7.FK7_PARCEL AND SE1.E1_TIPO = FK7.FK7_TIPO	AND SE1.E1_CLIENTE = FK7.FK7_CLIFOR	AND SE1.E1_LOJA = FK7.FK7_LOJA AND SE1.D_E_L_E_T_ = '' " + CRLF
_cQuery += "	INNER JOIN " + RetSqlName("XTP") + " XTP ON XTP.XTP_FILIAL = SE1.E1_FILORIG AND XTP.XTP_IDPAY = SE1.E1_XNSUCAR AND XTP.XTP_PARC = IIF(SE1.E1_PARCELA = '','AA',SE1.E1_PARCELA) AND XTP.D_E_L_E_T_ = '' " + CRLF
_cQuery += " WHERE " + CRLF
_cQuery += "	FO0_FILIAL = '" + _cFilial + "' AND " + CRLF
_cQuery += "	FO0_NUMLIQ = '" + _cNumLiq + "' AND " + CRLF
_cQuery += "	FO0_STATUS = '4' AND " + CRLF
_cQuery += "	FO0.D_E_L_E_T_ = '' "

_cAlias := MPSysOpenQuery(_cQuery)

//-----------------------------+
// XTP - Posiciona conciliacao |
//-----------------------------+
dbSelectArea("XTP")
XTP->( dbSetOrder(1) )

While (_cAlias)->( !Eof() )
    XTP->( dbGoTo((_cAlias)->RECNOXTP))

    RecLock("XTP",.F.)
        XTP->XTP_STATUS := "1"
    XTP->( MsUnLock() )

    (_cAlias)->( dbSkip() )
EndDo 

(_cAlias)->( dbCloseArea() ) 

RestArea(_aArea)
Return Nil 

/************************************************************************************/
/*/{Protheus.doc} MenuDef
	@description Menu padrao para manutencao do cadastro
	@author Bernard M. Margarido
	@since 10/08/2017
	@version undefined
/*/
/************************************************************************************/
Static Function MenuDef()
Local _aRotina := {}

ADD OPTION _aRotina TITLE "Pesquisar"            	ACTION "PesqBrw"            		OPERATION 1 ACCESS 0  
ADD OPTION _aRotina TITLE "Visualizar"           	ACTION "VIEWDEF.BPFINA11" 			OPERATION 2 ACCESS 0 
ADD OPTION _aRotina TITLE "Incluir"              	ACTION "U_BPFINA12" 			    OPERATION 3 ACCESS 0 
ADD OPTION _aRotina TITLE "Excluir"              	ACTION "VIEWDEF.BPFINA11" 			OPERATION 5 ACCESS 0 
ADD OPTION _aRotina TITLE "Conciliar"           	ACTION "U_BPFINA13"		 			OPERATION 6 ACCESS 0 
ADD OPTION _aRotina TITLE "Saldo"           	    ACTION "U_BPFINA14"		 			OPERATION 6 ACCESS 0 

Return _aRotina
