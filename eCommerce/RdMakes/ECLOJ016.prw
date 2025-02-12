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
/*/{Protheus.doc} ECLOJ016
    @description Produtos X Especificações
    @author Bernard M. Margarido
    @since 21/01/2024
    @version undefined
    @type function
/*/
/************************************************************************************/
User Function ECLOJ016()
Private _oBrowse	:= Nil

Private _nOldLen	 	:= SetVarNameLen(255) 

_oBrowse := FWMBrowse():New()
_oBrowse:SetAlias("ZTG")
_oBrowse:SetDescription('Produtos X Especificações')
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
Local _oStrZTG  := FWFormStruct(1,"ZTG")

Local _oModel   := Nil 

//-----------------+
// Gatilho Produto | 
//-----------------+
_oStrZTG:AddTrigger( 	'ZTG_ESPECI' 	/*cIdField*/ ,;
					 	'ZTG_ESPNOM'	/*cTargetIdField*/ ,;  
					 	{ || .T. } /*bPre*/ ,;
					 	{ || ECLOJ016G("ZTG_ESPECI","ZTG_ESPNOM") } /*bSetValue*/ )

_oStrZTG:AddTrigger( 	'ZTG_VALUE' 	/*cIdField*/ ,;
					 	'ZTG_DESC'	    /*cTargetIdField*/ ,;  
					 	{ || .T. } /*bPre*/ ,;
					 	{ || ECLOJ016H("ZTG_VALUE","ZTG_DESC") } /*bSetValue*/ )

//----------------------------------+
// Cria o Objeto do Modelo de Dados |
//----------------------------------+
_oModel	:= MPFormModel():New("ZTG_00")

//-----------------------------------------------+
// Adiciona ao modelo o componente de formulário |
//-----------------------------------------------+
_oModel:AddFields("ZTG_MASTER",,_oStrZTG)

//---------------------+
// Cria Chave Primaria |
//---------------------+
_oModel:SetPrimaryKey( {"ZTG_FILIAL","ZTG_PRODUT"} )

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
Local _oStrViewZTG	:= FWFormStruct(2 ,"ZTG" )

//----------------------------------------------------------------------------+
//³Cria um objeto de Modelo de dados baseado no ModelDef() do fonte informado |
//----------------------------------------------------------------------------+
_oModel := FWLoadModel("ECLOJ016") 
_oView	:= FWFormView():New()

_oView:SetModel(_oModel)
_oView:SetDescription('Produtos X Especificações')

//---------------------+
// View das estruturas |
//---------------------+
_oView:AddField('ZTG_FORM' 	, _oStrViewZTG , 'ZTG_MASTER' )

_oView:CreateHorizontalBox( 'SUPERIOR'    		, 100 ,,, /*'PASTAS'*/, /*'ABA01'*/ )

_oView:SetOwnerView('ZTG_FORM'	,'SUPERIOR')

Return _oView

/************************************************************************************/
/*/{Protheus.doc} ECLOJ016G
    @description Realiza gatilho dos campos em tela
    @type  Static Function
    @author Bernard M Margarido
    @since 23/01/2024
    @version version
/*/
/************************************************************************************/
Static Function ECLOJ016G(_cCpoOri,_cCpoAtu)
Local _oModel   := FWModelActive()
Local _oModelZTG:= _oModel:GetModel("ZTG_MASTER")

Local _cOrigem  := ""    
Local _cResult  := ""

//Padr( Posicione("ZTI",1,xFilial("ZTI") + FwFldGet('ZTE_GRUPO'),'ZTE_IDGRUP'), TamSx3("ZTE_IDGRUP")[1] ) 

dbSelectArea("ZTE")
ZTE->( dbSetOrder(1) )

//---------------------------+
// Realiza pesquisa do campo |
//---------------------------+
_cOrigem        := _oModelZTG:GetValue(_cCpoOri)

If ZTE->(MsSeek(xFilial("ZTE") + _cOrigem) )
    _cResult := ZTE->ZTE_NOME
EndIf 

_oModelZTG:LoadValue( _cCpoAtu , _cResult )

Return _cResult

/************************************************************************************/
/*/{Protheus.doc} ECLOJ016G
    @description Realiza gatilho dos campos em tela
    @type  Static Function
    @author Bernard M Margarido
    @since 23/01/2024
    @version version
/*/
/************************************************************************************/
Static Function ECLOJ016H(_cCpoOri,_cCpoAtu)
Local _oModel   := FWModelActive()
Local _oModelZTG:= _oModel:GetModel("ZTG_MASTER")

Local _cOrigem  := ""    
Local _cResult  := ""

dbSelectArea("ZTF")
ZTF->( dbSetOrder(1) )

//---------------------------+
// Realiza pesquisa do campo |
//---------------------------+
_cOrigem        := _oModelZTG:GetValue(_cCpoOri)

If ZTF->(MsSeek(xFilial("ZTF") + _cOrigem) )
    _cResult := ZTF->ZTF_VALUE
EndIf 

_oModelZTG:LoadValue( _cCpoAtu , _cResult )

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
Return FWMVCMenu( "ECLOJ016" )
