#INCLUDE "TOTVS.CH"
#INCLUDE "POSCSS.CH"
#INCLUDE "PROTHEUS.CH"
#INCLUDE "FWMVCDEF.CH"

#DEFINE FF_LAYOUT_VERT_DESCR_TOP 			001 // Vertical com descri��o acima do get
#DEFINE FF_LAYOUT_VERT_DESCR_LEFT			002 // Vertical com descri��o a esquerda
#DEFINE FF_LAYOUT_HORZ_DESCR_TOP 			003 // Horizontal com descri��o acima do get
#DEFINE FF_LAYOUT_HORZ_DESCR_LEFT			004 // Horizontal com descri��o a esquerda

#DEFINE CRLF CHR(13) + CHR(10)

/************************************************************************************/
/*/{Protheus.doc} ECLOJ013
    @description Grupos Especificos
    @author Bernard M. Margarido
    @since 21/01/2024
    @version undefined
    @type function
/*/
/************************************************************************************/
User Function ECLOJ013()
Private _oBrowse	:= Nil

Private _nOldLen	 	:= SetVarNameLen(255) 

_oBrowse := FWMBrowse():New()
_oBrowse:SetAlias("ZTI")
_oBrowse:SetDescription('Grupos Especificos.')
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
Local _oStrZTI  := FWFormStruct(1,"ZTI")
Local _oModel   := Nil 

//-----------------+
// Gatilho Produto | 
//-----------------+
_oStrZTI:AddTrigger( 	'ZTI_CATEG' 	/*cIdField*/ ,;
					 	'ZTI_DESCAT'	/*cTargetIdField*/ ,;  
					 	{ || .T. }      /*bPre*/ ,;
					 	{ ||  ECLOJ013G("ZTI_CATEG","ZTI_DESCAT") })


//----------------------------------+
// Cria o Objeto do Modelo de Dados |
//----------------------------------+
_oModel	:= MPFormModel():New("ZTI_00")

//-----------------------------------------------+
// Adiciona ao modelo o componente de formul�rio |
//-----------------------------------------------+
_oModel:AddFields("ZTI_MASTER",,_oStrZTI)

//---------------------+
// Cria Chave Primaria |
//---------------------+
_oModel:SetPrimaryKey( {"ZTI_FILIAL","ZTI_CODIGO"} )


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
Local _oStrViewZTI	:= FWFormStruct(2 ,"ZTI" )

//----------------------------------------------------------------------------+
//�Cria um objeto de Modelo de dados baseado no ModelDef() do fonte informado |
//----------------------------------------------------------------------------+
_oModel := FWLoadModel("ECLOJ013") 
_oView	:= FWFormView():New()

_oView:SetModel(_oModel)
_oView:SetDescription('Grupos Especificos.')

//---------------------+
// View das estruturas |
//---------------------+
_oView:AddField('ZTI_FORM' 	, _oStrViewZTI , 'ZTI_MASTER' )


_oView:CreateHorizontalBox( 'SUPERIOR_A1'    		, 100 ,,, /*'PASTAS'*/, /*'ABA01'*/ )
_oView:CreateVerticalBox( 'ESQ_S1'      ,100 , 'SUPERIOR_A1' )

_oView:SetOwnerView('ZTI_FORM'	,'ESQ_S1')

Return _oView

/************************************************************************************/
/*/{Protheus.doc} ECLOJ013G
    @description Realiza gatilho dos campos em tela
    @type  Static Function
    @author Bernard M Margarido
    @since 23/01/2024
    @version version
/*/
/************************************************************************************/
Static Function ECLOJ013G(_cCpoOri,_cCpoAtu)
Local _oModel   := FWModelActive()
Local _oModelZTI:= _oModel:GetModel("ZTI_MASTER")

Local _cOrigem  := ""    
Local _cResult  := ""

Local _nTDesc   := TamSx3("ZTI_DESCAT")[1]

dbSelectArea("ACU")
ACU->( dbSetOrder(1) )

//---------------------------+
// Realiza pesquisa do campo |
//---------------------------+
_cOrigem        := _oModelZTI:GetValue(_cCpoOri)

If ACU->(MsSeek(xFilial("ACU") + _cOrigem) )
    _cResult := SubStr(ACU->ACU_DESC,1,_nTDesc)
EndIf 

_oModelZTI:LoadValue( _cCpoAtu , _cResult )

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
Return FWMVCMenu( "ECLOJ013" )
