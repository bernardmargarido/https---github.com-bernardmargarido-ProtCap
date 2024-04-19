#INCLUDE "TOTVS.CH"
#INCLUDE "PROTHEUS.CH"
#INCLUDE "FWMVCDEF.CH"

/**************************************************************************/
/*/{Protheus.doc} ECLOJ019
    @description Filiais Protheus X VTEX
    @type  Function
    @author Bernard M Margarido
    @since 16/02/2024
    @version version
/*/
/**************************************************************************/
User Function ECLOJ019()
Private _oBrowse := Nil 

//------------------------------------+
// Instanciamento da Classe FWMBrowse |
//------------------------------------+
_oBrowse := FWMBrowse():New()
//-----------------+
// Alias utilizado |
//-----------------+
_oBrowse:SetAlias("XTN")

_oBrowse:AddLegend( "XTN_STATUS == .T.", "GREEN" , "Ativo" )
_oBrowse:AddLegend( "XTN_STATUS == .F.", "RED" , "Inativo" )

//------------------+
// Titulo do Browse |
//------------------+
_oBrowse:SetDescription('Filiais Protheus X VTEX')
_oBrowse:SetMenuDef("ECLOJ019")

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
Local _oStruXTN     := Nil

Local _bWhen        := {|_oModel| ECLOJ019B(_oModel)}

//-----------------+
// Monta Estrutura |
//-----------------+
_oStruXTN   := FWFormStruct(1,"XTN")

//--------------------+
// Gatillho campo CGC |
//--------------------+
_oStruXTN:AddTrigger( 	'XTN_IDECOM' 	/*cIdField*/ ,;
                        'XTN_DESC'	/*cTargetIdField*/ ,;  
                        { || .T. } /*bPre*/ ,;
                        { || ECLOJ019A("XTN_FILIAL","XTN_DESC") } /*bSetValue*/ )


_oStruXTN:SetProperty( 'XTN_DESC'   , MODEL_FIELD_WHEN, {|| .F.} )
_oStruXTN:SetProperty( 'XTN_URL1'   , MODEL_FIELD_WHEN, _bWhen)
_oStruXTN:SetProperty( 'XTN_URL2'   , MODEL_FIELD_WHEN, _bWhen)
_oStruXTN:SetProperty( 'XTN_APPKEY' , MODEL_FIELD_WHEN, _bWhen)
_oStruXTN:SetProperty( 'XTN_APPTOK' , MODEL_FIELD_WHEN, _bWhen)

//-------+
// Model |
//-------+
_oModel 	:= MPFormModel():New('ECLOJ19_01', /*bPreValid*/ , /*_bPosValid*/ , /*_bCommit*/ , /*_bCancel*/ )
//-----------------+
// Adiciona campos | 
//-----------------+
_oModel:addFields('MASTER',,_oStruXTN)

_oModel:SetDescription('Filiais ERP X Loja eCommerce')
_oModel:GetModel( 'MASTER' ):SetDescription(  "Filiais ERP X Loja eCommerce"  )

//------------------------+
// Chave primaria produto | 
//------------------------+
_oModel:SetPrimaryKey({"XTN_FILIAL","XTN_ID"})

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
Local _oStrViewXTN	:= Nil

//-------------------------+
// Carrega Modelo de Dados | 
//-------------------------+
_oModel := FWLoadModel("ECLOJ019")

//--------------------------------------+
// Cria a estrutura a ser usada na View |
//--------------------------------------+
_oStrViewXTN	:= FWFormStruct( 2,'XTN') 

//---------------------+
// Instancia Interface |
//---------------------+
_oView	:= FWFormView():New()
_oView:SetModel(_oModel)
_oView:SetDescription('Filiais ERP X Loja eCommerce')

//---------------------+
// View das estruturas |
//---------------------+
_oView:AddField('XTN_FORM' 	, _oStrViewXTN , 'MASTER' )

//------------------------------------------------------------+
// Criar "box" horizontal para receber algum elemento da view |
//------------------------------------------------------------+
_oView:CreateHorizontalBox( 'SUP_01' , 100 ,,, /*'PASTAS'*/, /*'ABA01'*/ )

_oView:SetOwnerView('XTN_FORM'	    ,'SUP_01')

Return _oView 

/*************************************************************************************/
/*/{Protheus.doc} ECLOJ019A
    @description Gatilho - Função para realizar gatilho no campo MVC
    @type  Static Function
    @author Bernard M Margarido
    @since 07/02/2024
    @version version
/*/
/*************************************************************************************/
Static Function ECLOJ019A(_cCpoOri,_cCpoAtu)
Local _oModel   := FWModelActive()
Local _oModelXTG:= _oModel:GetModel("MASTER")

Local _cOrigem  := ""    
Local _cResult  := ""

Local _nTFil    := TamSx3("XTN_DESC")[1]
//---------------------------+
// Realiza pesquisa do campo |
//---------------------------+
_cOrigem := cFilAnt
_cResult := PadR(FwFilialName(cEmpAnt,cFilAnt),_nTFil)

_oModelXTG:LoadValue( _cCpoAtu , _cResult )

Return _cResult

/************************************************************************************/
/*/{Protheus.doc} ECLOJ019B
    @description Validas e campo pode ser editado 
    @type  Static Function
    @author Bernard M Margarido
    @since 13/04/2024
    @version version
/*/
/************************************************************************************/
Static Function ECLOJ019B(_oModel)
Local _nOper    := _oModel:GetOperation()

Local _cUserAut := GetMv("EC_LOJAID",,"000000/003104")

Local _lRet     := .F.

If _nOper == 3 .Or. __cUserId $ _cUserAut 
    _lRet := .T.
EndIf

Return _lRet 

/************************************************************************************/
/*/{Protheus.doc} MenuDef
	@description Menu padrao para manutencao do cadastro
	@author Bernard M. Margarido
	@since 10/08/2017
	@version undefined
/*/
/************************************************************************************/
Static Function MenuDef()
Return FwMVCMenu('ECLOJ019')
