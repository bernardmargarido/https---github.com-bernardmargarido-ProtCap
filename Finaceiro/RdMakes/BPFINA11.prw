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
