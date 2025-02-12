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
/*/{Protheus.doc} ECLOJ015
    @description Campos Especificos
    @author Bernard M. Margarido
    @since 21/01/2024
    @version undefined
    @type function
/*/
/************************************************************************************/
User Function ECLOJ015()
Private _oBrowse	:= Nil

Private _nOldLen	 	:= SetVarNameLen(255) 

_oBrowse := FWMBrowse():New()
_oBrowse:SetAlias("ZTF")
_oBrowse:SetDescription('Valores X Especificações')
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
Local _oStrZTF  := FWFormStruct(1,"ZTF")

Local _oModel   := Nil 

//-----------------+
// Gatilho Produto | 
//-----------------+
_oStrZTF:AddTrigger( 	'ZTF_ESPCID' 	/*cIdField*/ ,;
					 	'ZTF_CATEGO'	/*cTargetIdField*/ ,;  
					 	{ || .T. } /*bPre*/ ,;
					 	{ || ECLOJ015G("ZTF_ESPCID","ZTF_CATEGO") } /*bSetValue*/ )

//----------------------------------+
// Cria o Objeto do Modelo de Dados |
//----------------------------------+
_oModel	:= MPFormModel():New("ZTF_00")

//-----------------------------------------------+
// Adiciona ao modelo o componente de formulário |
//-----------------------------------------------+
_oModel:AddFields("ZTF_MASTER",,_oStrZTF)

//---------------------+
// Cria Chave Primaria |
//---------------------+
_oModel:SetPrimaryKey( {"ZTF_FILIAL","ZTF_COD"} )

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
Local _oStrViewZTF	:= FWFormStruct(2 ,"ZTF" )

//----------------------------------------------------------------------------+
//³Cria um objeto de Modelo de dados baseado no ModelDef() do fonte informado |
//----------------------------------------------------------------------------+
_oModel := FWLoadModel("ECLOJ015") 
_oView	:= FWFormView():New()

_oView:SetModel(_oModel)
_oView:SetDescription('Valores X Especificações')

//---------------------+
// View das estruturas |
//---------------------+
_oView:AddField('ZTF_FORM' 	, _oStrViewZTF , 'ZTF_MASTER' )

_oView:CreateHorizontalBox( 'SUPERIOR'    		, 100 ,,, /*'PASTAS'*/, /*'ABA01'*/ )

_oView:SetOwnerView('ZTF_FORM'	,'SUPERIOR')

Return _oView

/************************************************************************************/
/*/{Protheus.doc} ECLOJ015G
    @description Realiza gatilho dos campos em tela
    @type  Static Function
    @author Bernard M Margarido
    @since 23/01/2024
    @version version
/*/
/************************************************************************************/
Static Function ECLOJ015G(_cCpoOri,_cCpoAtu)
Local _oModel   := FWModelActive()
Local _oModelZTF:= _oModel:GetModel("ZTF_MASTER")

Local _cOrigem  := ""    
Local _cResult  := ""

//Padr( Posicione("ZTI",1,xFilial("ZTI") + FwFldGet('ZTE_GRUPO'),'ZTE_IDGRUP'), TamSx3("ZTE_IDGRUP")[1] ) 

dbSelectArea("ZTE")
ZTE->( dbSetOrder(1) )

//---------------------------+
// Realiza pesquisa do campo |
//---------------------------+
_cOrigem        := _oModelZTF:GetValue(_cCpoOri)

If ZTE->(MsSeek(xFilial("ZTE") + _cOrigem) )
    _cResult := ZTE->ZTE_CATEGO
EndIf 

_oModelZTF:LoadValue( _cCpoAtu , _cResult )

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
Return FWMVCMenu( "ECLOJ015" )
