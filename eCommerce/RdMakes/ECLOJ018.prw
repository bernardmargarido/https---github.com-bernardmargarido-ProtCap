#INCLUDE "TOTVS.CH"
#INCLUDE "PROTHEUS.CH"
#INCLUDE "FWMVCDEF.CH"

/******************************************************************************/
/*/{Protheus.doc} ECLOJ018
    @description Amarração Transportadoras ERP X eCommerce
    @type  Function
    @author Bernard M. Margarido
    @since 18/11/2020
/*/
/******************************************************************************/
User Function ECLOJ018()
Private _oBrowse := Nil 

//------------------------------------+
// Instanciamento da Classe FWMBrowse |
//------------------------------------+
_oBrowse := FWMBrowse():New()
//-----------------+
// Alias utilizado |
//-----------------+
_oBrowse:SetAlias("XTG")

_oBrowse:AddLegend( "XTG_STATUS == .T.", "GREEN" , "Ativo" )
_oBrowse:AddLegend( "XTG_STATUS == .F.", "RED" , "Inativo" )

//------------------+
// Titulo do Browse |
//------------------+
_oBrowse:SetDescription('Transportadoras ERP x eCommerce')
_oBrowse:SetMenuDef("ECLOJ018")

//--------------------+
// Ativação do Browse |
//--------------------+
_oBrowse:Activate()

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
Local _oStruXTG     := Nil

//-----------------+
// Monta Estrutura |
//-----------------+
_oStruXTG   := FWFormStruct(1,"XTG")

//--------------------+
// Gatillho campo CGC |
//--------------------+
_oStruXTG:AddTrigger( 	'XTG_TRANSP' 	/*cIdField*/ ,;
                        'XTG_NOME'	/*cTargetIdField*/ ,;  
                        { || .T. } /*bPre*/ ,;
                        { || ECLOJ018A("XTG_TRANSP","XTG_NOME") } /*bSetValue*/ )

//-------+
// Model |
//-------+
_oModel 	:= MPFormModel():New('ECLOJ18_01', /*bPreValid*/ , /*_bPosValid*/ , /*_bCommit*/ , /*_bCancel*/ )
//-----------------+
// Adiciona campos | 
//-----------------+
_oModel:addFields('MASTER',,_oStruXTG)

_oModel:SetDescription('Transportadoras ERP X eCommerce')
_oModel:GetModel( 'MASTER' ):SetDescription(  "Transportadoras ERP X eCommerce"  )

//------------------------+
// Chave primaria produto | 
//------------------------+
_oModel:SetPrimaryKey({"XTG_FILIAL","XTG_TRANSP","XTG_IDVTEX"})

_oModel:SetActivate()

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
Local _oStrViewXTG	:= Nil

//-------------------------+
// Carrega Modelo de Dados | 
//-------------------------+
_oModel := FWLoadModel("ECLOJ018")

//--------------------------------------+
// Cria a estrutura a ser usada na View |
//--------------------------------------+
_oStrViewXTG	:= FWFormStruct( 2,'XTG') 

//---------------------+
// Instancia Interface |
//---------------------+
_oView	:= FWFormView():New()
_oView:SetModel(_oModel)
_oView:SetDescription('Transportadoras ERP X eCommerce')

//---------------------+
// View das estruturas |
//---------------------+
_oView:AddField('XTG_FORM' 	, _oStrViewXTG , 'MASTER' )

//------------------------------------------------------------+
// Criar "box" horizontal para receber algum elemento da view |
//------------------------------------------------------------+
_oView:CreateHorizontalBox( 'SUP_01' , 100 ,,, /*'PASTAS'*/, /*'ABA01'*/ )

_oView:SetOwnerView('XTG_FORM'	    ,'SUP_01')

Return _oView 

/*************************************************************************************/
/*/{Protheus.doc} ECLOJ018A
    @description Gatilho - Função para realizar gatilho no campo MVC
    @type  Static Function
    @author Bernard M Margarido
    @since 07/02/2024
    @version version
/*/
/*************************************************************************************/
Static Function ECLOJ018A(_cCpoOri,_cCpoAtu)
Local _oModel   := FWModelActive()
Local _oModelXTG:= _oModel:GetModel("MASTER")

Local _cOrigem  := ""    
Local _cResult  := ""

dbSelectArea("SA4")
SA4->( dbSetOrder(1) )

//---------------------------+
// Realiza pesquisa do campo |
//---------------------------+
_cOrigem        := _oModelXTG:GetValue(_cCpoOri)

If SA4->( MsSeek(xFilial("SA4") + _cOrigem) )
    _cResult := SA4->A4_NOME
EndIf 

_oModelXTG:LoadValue( _cCpoAtu , _cResult )

Return _cResult

/************************************************************************************/
/*/{Protheus.doc} MenuDef
	@description Menu padrao para manutencao do cadastro
	@author Bernard M. Margarido
	@since 10/08/2017
	@version undefined
/*/
/************************************************************************************/
Static Function MenuDef()
Return FwMVCMenu('ECLOJ018')
