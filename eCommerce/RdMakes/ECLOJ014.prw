#INCLUDE "TOTVS.CH"
#INCLUDE "POSCSS.CH"
#INCLUDE "PROTHEUS.CH"
#INCLUDE "FWMVCDEF.CH"

#DEFINE FF_LAYOUT_VERT_DESCR_TOP 			001 // Vertical com descrição acima do get
#DEFINE FF_LAYOUT_VERT_DESCR_LEFT			002 // Vertical com descrição a esquerda
#DEFINE FF_LAYOUT_HORZ_DESCR_TOP 			003 // Horizontal com descrição acima do get
#DEFINE FF_LAYOUT_HORZ_DESCR_LEFT			004 // Horizontal com descrição a esquerda

#DEFINE CRLF CHR(13) + CHR(10)

/************************************************************************************/
/*/{Protheus.doc} ECLOJ014
    @description Campos Especificos
    @author Bernard M. Margarido
    @since 21/01/2024
    @version undefined
    @type function
/*/
/************************************************************************************/
User Function ECLOJ014()
Private _oBrowse	:= Nil

Private _nOldLen	 	:= SetVarNameLen(255) 

_oBrowse := FWMBrowse():New()
_oBrowse:SetAlias("ZTE")
_oBrowse:SetDescription('Campos Especificos.')
_oBrowse:Activate()

SetVarNameLen(_nOldLen)

Return Nil

/************************************************************************************/
/*/{Protheus.doc} ModelDef
    @description  Modelo de dados, estrutura dos dados e modelo de negocio
    @author Bernard M. Margarido
    @since 21/01/2024
    @version undefined
    @type function
/*/
/************************************************************************************/
Static Function ModelDef()
Local _oStrZTE  := FWFormStruct(1,"ZTE")
Local _oModel   := Nil 

//-----------------+
// Gatilho Produto | 
//-----------------+
_oStrZTE:AddTrigger( 	'ZTE_GRUPO' 	/*cIdField*/ ,;
					 	'ZTE_IDGRUP'	/*cTargetIdField*/ ,;  
					 	{ || .T. } /*bPre*/ ,;
					 	{ || ECLOJ014G("ZTE_GRUPO","ZTE_IDGRUP") } /*bSetValue*/ )

//----------------------------------+
// Cria o Objeto do Modelo de Dados |
//----------------------------------+
_oModel	:= MPFormModel():New("ZTE_00")

//-----------------------------------------------+
// Adiciona ao modelo o componente de formulário |
//-----------------------------------------------+
_oModel:AddFields("ZTE_MASTER",,_oStrZTE)

//---------------------+
// Cria Chave Primaria |
//---------------------+
_oModel:SetPrimaryKey( {"ZTE_FILIAL","ZTE_COD"} )


Return _oModel

/************************************************************************************/
/*/{Protheus.doc} ViewDef
    @description Cria interface com o usuario
    @author Bernard M. Margarido
    @since 21/01/2024
    @version undefined
    @type function
/*/
/************************************************************************************/
Static Function ViewDef() 
Local _oView
Local _oModel
Local _oStrViewZTE	:= FWFormStruct(2 ,"ZTE" )

//----------------------------------------------------------------------------+
//³Cria um objeto de Modelo de dados baseado no ModelDef() do fonte informado |
//----------------------------------------------------------------------------+
_oModel := FWLoadModel("ECLOJ014") 
_oView	:= FWFormView():New()

_oView:SetModel(_oModel)
_oView:SetDescription('Campos Especificos.')

//---------------------+
// View das estruturas |
//---------------------+
_oView:AddField('ZTE_FORM' 	, _oStrViewZTE , 'ZTE_MASTER' )


_oView:CreateHorizontalBox( 'SUPERIOR_A1'    		, 100 ,,, /*'PASTAS'*/, /*'ABA01'*/ )
_oView:CreateVerticalBox( 'ESQ_S1'      ,100 , 'SUPERIOR_A1' )

_oView:SetOwnerView('ZTE_FORM'	,'ESQ_S1')

Return _oView

/************************************************************************************/
/*/{Protheus.doc} ECLOJ014G
    @description Realiza gatilho dos campos em tela
    @type  Static Function
    @author Bernard M Margarido
    @since 23/01/2024
    @version version
/*/
/************************************************************************************/
Static Function ECLOJ014G(_cCpoOri,_cCpoAtu)
Local _oModel   := FWModelActive()
Local _oModelZTE:= _oModel:GetModel("ZTE_MASTER")

Local _cOrigem  := ""    
Local _cResult  := ""

//Padr( Posicione("ZTI",1,xFilial("ZTI") + FwFldGet('ZTE_GRUPO'),'ZTE_IDGRUP'), TamSx3("ZTE_IDGRUP")[1] ) 

dbSelectArea("ZTI")
ZTI->( dbSetOrder(1) )

//---------------------------+
// Realiza pesquisa do campo |
//---------------------------+
_cOrigem        := _oModelZTE:GetValue(_cCpoOri)

If ZTI->(MsSeek(xFilial("ZTI") + _cOrigem) )
    _cResult := ZTI->ZTI_IDVTX
EndIf 

_oModelZTE:LoadValue( _cCpoAtu , _cResult )

Return _cResult

/************************************************************************************/
/*/{Protheus.doc} MenuDef
    @description Menu padrao para manutencao do cadastro
    @author Bernard M. Margarido
    @since 21/01/2024
    @version undefined
/*/
/************************************************************************************/
Static Function MenuDef()
Return FWMVCMenu( "ECLOJ014" )
