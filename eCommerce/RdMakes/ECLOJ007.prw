#INCLUDE "PROTHEUS.CH"
#INCLUDE "FWMVCDEF.CH"

#DEFINE CRLF CHR(13) + CHR(10)

/************************************************************************************/
/*/{Protheus.doc} ECLOJ007
    @description Status eCommerce
    @author Bernard M. Margarido
    @since 29/04/2019
    @version undefined
    @type function
/*/
/************************************************************************************/
User Function ECLOJ007()
Private oBrowse		:= Nil

Private aRotina     := MenuDef()

//---------------------------------+
// Instanciamento da Classe Browse |
//---------------------------------+
oBrowse := FWMBrowse():New()

//------------------+
// Tabela utilizado |
//------------------+
oBrowse:SetAlias("ZTC")

//------------------+
// Titulo do Browse |
//------------------+
oBrowse:SetDescription('Status e-Commerce')

//--------------------+
// Ativação do Browse |
//--------------------+
oBrowse:Activate()

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
Local oStruct 
Local oModel

//-----------------------------------------------+
// Cria Estrutura a ser usada no Modelo de Dados |
//-----------------------------------------------+
oStruct := FWFormStruct(1,"ZTC")

//----------------------------------+
// Cria o Objeto do Modelo de Dados |
//----------------------------------+
oModel	:= MPFormModel():New("ZTC_00",,{|oModel| EcLoj07TOk(oModel) })

//-----------------------------------------------+
// Adiciona ao modelo o componente de formulário |
//-----------------------------------------------+
oModel:AddFields("ZTCMASTER",/*cOwner*/,oStruct)

//---------------------+
// Cria Chave Primaria |
//---------------------+
oModel:SetPrimaryKey( {"ZTC_FILIAL","ZTC_ORDEM"} )

//-----------------------------------------+
// Adiciona a descrição do Modelo de Dados |
//-----------------------------------------+
oModel:SetDescription('Cadastro - Status eCommerce')

//-------------------------------------------------------+
// Adiciona a descrição do componente do Modelo de Dados |
//-------------------------------------------------------+
oModel:GetModel('ZTCMASTER'):SetDescription('Status eCommerce')

Return(oModel)

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
Local oDefModel
Local oDefStruct
Local oDefView  

//----------------------------------------------------------------------------+
//³Cria um objeto de Modelo de dados baseado no ModelDef() do fonte informado |
//----------------------------------------------------------------------------+
oDefModel := FWLoadModel("ECLOJ007") 

//--------------------------------------+
// Cria a estrutura a ser usada na View |
//--------------------------------------+
oDefStruct := FWFormStruct( 2,'ZTC') 

//-----------------------+
// Cria o objeto de View |
//-----------------------+
oDefView := FWFormView():New() 

//------------------------------------------------------+
// Define qual o Modelo de dados será utilizado na View |
//------------------------------------------------------+
oDefView:SetModel(oDefModel) 

//-------------------------------------------------------+
// Adiciona no nosso View um controle do tipo formulário |
// (antiga Enchoice)                                     |
//-------------------------------------------------------+
oDefView:AddField( 'VIEW_ZTC', oDefStruct, 'ZTCMASTER' ) 

//---------------------------------------------------------------+
// Criar um "box" horizontal para receber algum elemento da view |
//---------------------------------------------------------------+
oDefView:CreateHorizontalBox( 'TELA' , 100 ) 

//------------------------------------------------------------------+
// Relaciona o identificador (ID) da View com o "box" para exibição |
//------------------------------------------------------------------+
oDefView:SetOwnerView( 'VIEW_ZTC', 'TELA' ) 

Return( oDefView )

/************************************************************************************/
/*/{Protheus.doc} SyVa07TdOk
    @description Valida registro
    @author Bernard M. Margarido
    @since 22/01/2018
    @version 1.0
    @param oModel, object, descricao
    @type function
/*/
/************************************************************************************/
Static Function EcLoj07TOk(oModel)
Local lRet	:= .T.
	
Return lRet

/************************************************************************************/
/*/{Protheus.doc} MenuDef
    @description Menu padrao para manutencao do cadastro
    @author Bernard M. Margarido
    @since 10/08/2017
    @version undefined
    @type function
/*/
/************************************************************************************/
Static Function MenuDef()
Return FWMVCMenu( "ECLOJ007" )
