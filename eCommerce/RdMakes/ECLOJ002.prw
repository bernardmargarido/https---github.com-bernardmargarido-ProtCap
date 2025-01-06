#INCLUDE "PROTHEUS.CH"
#INCLUDE "FWMVCDEF.CH"

#DEFINE CRLF CHR(13) + CHR(10)

/**************************************************************************************/
/*/{Protheus.doc} ECLOJ002
    @description Cadastro de Marcas 
    @type  Function
    @author Bernard M Margarido
    @since 16/02/2024
    @version version
/*/
/**************************************************************************************/
User Function ECLOJ002()
Private _nOldLen := SetVarNameLen(255) 
Private _oBrowse := Nil 

_oBrowse := FWMBrowse():New()
_oBrowse:SetAlias("ZTD")

_oBrowse:SetDescription('Marcas')

_oBrowse:AddLegend("ZTD_INTLV == '0' "	, "GREEN"				, "Nao Integrar")
_oBrowse:AddLegend("ZTD_INTLV == '1' "	, "YELLOW"	    	    , "Integrar")
_oBrowse:AddLegend("ZTD_INTLV == '2' "	, "RED"				    , "Integrado")

_oBrowse:Activate()
SetVarNameLen(_nOldLen)    
Return Nil

/************************************************************************************/
/*/{Protheus.doc} ModelDef
    @description  Modelo de dados, estrutura dos dados e modelo de negocio
    @author Bernard M. Margarido
    @since 21/11/2023
    @version undefined
    @type function
/*/
/************************************************************************************/
Static Function ModelDef()
Local _oStruZTD := Nil 
Local _oModel   := Nil 

//-----------------------------------------------+
// Cria Estrutura a ser usada no Modelo de Dados |
//-----------------------------------------------+
_oStruZTD := FWFormStruct(1,"ZTD")

//----------------------------------+
// Cria o Objeto do Modelo de Dados |
//----------------------------------+
_oModel	:= MPFormModel():New("ZTD_00")

//-----------------------------------------------+
// Adiciona ao modelo o componente de formulário |
//-----------------------------------------------+
_oModel:AddFields("ZTDMASTER",/*cOwner*/,_oStruZTD)

//---------------------+
// Cria Chave Primaria |
//---------------------+
_oModel:SetPrimaryKey( {"ZTD_FILIAL","ZTD_COD"} )

//-----------------------------------------+
// Adiciona a descrição do Modelo de Dados |
//-----------------------------------------+
_oModel:SetDescription('Marcas')

//-------------------------------------------------------+
// Adiciona a descrição do componente do Modelo de Dados |
//-------------------------------------------------------+
_oModel:GetModel('ZTDMASTER'):SetDescription('Marcas')

Return _oModel

/************************************************************************************/
/*/{Protheus.doc} ViewDef
    @description Cria interface com o usuario
    @author Bernard M. Margarido
    @since 21/11/2023
    @version undefined
    @type function
/*/
/************************************************************************************/
Static Function ViewDef() 
Local _oView        
Local _oModel
Local _oStrViewZTD	:= Nil

//-------------------------+
// Carrega Modelo de Dados | 
//-------------------------+
_oModel := FWLoadModel("ECLOJ002")

//--------------------------------------+
// Cria a estrutura a ser usada na View |
//--------------------------------------+
_oStrViewZTD	:= FWFormStruct( 2,'ZTD') 

//---------------------+
// Instancia Interface |
//---------------------+
_oView	:= FWFormView():New()
_oView:SetModel(_oModel)
_oView:SetDescription('Marcas')

//---------------------+
// View das estruturas |
//---------------------+
_oView:AddField('ZTD_FORM' 	, _oStrViewZTD , 'ZTDMASTER' )

//------------------------------------------------------------+
// Criar "box" horizontal para receber algum elemento da view |
//------------------------------------------------------------+
_oView:CreateHorizontalBox( 'SUP_01' , 100 ,,, /*'PASTAS'*/, /*'ABA01'*/ )
_oView:SetOwnerView('ZTD_FORM'	    ,'SUP_01')

//------------------------+
// Titulo componente GRID |
//------------------------+
_oView:EnableTitleView('ZTD_FORM','Marcas')

Return _oView 

/************************************************************************************/
/*/{Protheus.doc} MenuDef
    @description Menu padrao para manutencao do cadastro
    @author Bernard M. Margarido
    @since 21/11/2023
    @version undefined
/*/
/************************************************************************************/
Static Function MenuDef()
Return FWMVCMenu( "ECLOJ002" )


