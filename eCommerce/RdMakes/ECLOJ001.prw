#INCLUDE "PROTHEUS.CH"
#INCLUDE "FWMVCDEF.CH"

#DEFINE CRLF CHR(13) + CHR(10)

Static _nTCodCat    := TamSx3("ACU_COD")[1]

/**************************************************************************************/
/*/{Protheus.doc} ECLOJ001
    @description Cadastro de categorias 
    @type  Function
    @author Bernard M Margarido
    @since 16/02/2024
    @version version
/*/
/**************************************************************************************/
User Function ECLOJ001()
Private _nOldLen := SetVarNameLen(255) 
Private _oBrowse := Nil 

_oBrowse := FWMBrowse():New()
_oBrowse:SetAlias("ACU")

_oBrowse:SetDescription('Categorias')

_oBrowse:AddLegend("ACU_MSBLQL <> '1' "	, "GREEN"				, "Ativo")
_oBrowse:AddLegend("ACU_MSBLQL == '1' "	, "RED"				    , "Inativo")

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
Local _oStruACU := Nil 
Local _oModel   := Nil 

//-----------------------------------------------+
// Cria Estrutura a ser usada no Modelo de Dados |
//-----------------------------------------------+
_oStruACU := FWFormStruct(1,"ACU")

//----------------------------------+
// Cria o Objeto do Modelo de Dados |
//----------------------------------+
_oModel	:= MPFormModel():New("ACU_00")

//-----------------------------------------------+
// Adiciona ao modelo o componente de formulário |
//-----------------------------------------------+
_oModel:AddFields("ACUMASTER",/*cOwner*/,_oStruACU)

//---------------------+
// Cria Chave Primaria |
//---------------------+
_oModel:SetPrimaryKey( {"ACU_FILIAL","ACU_COD"} )

//-----------------------------------------+
// Adiciona a descrição do Modelo de Dados |
//-----------------------------------------+
_oModel:SetDescription('Categorias')

//-------------------------------------------------------+
// Adiciona a descrição do componente do Modelo de Dados |
//-------------------------------------------------------+
_oModel:GetModel('ACUMASTER'):SetDescription('Categorias')

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
Local _oStrViewACU	:= Nil

//-------------------------+
// Carrega Modelo de Dados | 
//-------------------------+
_oModel := FWLoadModel("ECLOJ001")

//--------------------------------------+
// Cria a estrutura a ser usada na View |
//--------------------------------------+
_oStrViewACU	:= FWFormStruct( 2,'ACU') 

//---------------------+
// Instancia Interface |
//---------------------+
_oView	:= FWFormView():New()
_oView:SetModel(_oModel)
_oView:SetDescription('Categorias')

//---------------------+
// View das estruturas |
//---------------------+
_oView:AddField('ACU_FORM' 	, _oStrViewACU , 'ACUMASTER' )

//------------------------------------------------------------+
// Criar "box" horizontal para receber algum elemento da view |
//------------------------------------------------------------+
_oView:CreateHorizontalBox( 'SUP_01' , 100 ,,, /*'PASTAS'*/, /*'ABA01'*/ )
_oView:SetOwnerView('ACU_FORM'	    ,'SUP_01')

_oView:AddUserButton( 'Estrutura', 'FORM', {|_oView| U_ECLOJ01A()} )

//------------------------+
// Titulo componente GRID |
//------------------------+
_oView:EnableTitleView('ACU_FORM','Categorias')

Return _oView 

/************************************************************************************/
/*/{Protheus.doc} ECLOJ001A
    @description Monta estrutura categorias 
    @type  Static Function
    @author Bernard M Margarido
    @since 16/02/2024
/*/
/************************************************************************************/
User Function ECLOJ01A()
Local _aCoors       := FWGetDialogSize( oMainWnd )

Local _cTitulo      := "Estrutura das Categorias"

Local _oSize        := FWDefSize():New( .T. )
Local _oLayer       := FWLayer():New()
Local _oDlg         := Nil
Local _oPTree       := Nil

Private _oTree      := Nil
Private _nRecNo     := ACU->( RecNo() )
Private _cCodPai    := CriaVar("ACU_COD",.F.) 

//-------------------------------------------------------+
// Inicializa as coordenadas de tela conforme resolução  |
//-------------------------------------------------------+
_oSize:AddObject( "DLG", 100, 100, .T., .T.)
_oSize:SetWindowSize(_aCoors)
_oSize:lProp         := .T.
_oSize:lLateral 	:= .T.
_oSize:Process()

//------------------------+
// Monta Dialog principal |
//------------------------+
_oDlg := MsDialog():New(_oSize:aWindSize[1], _oSize:aWindSize[2],_oSize:aWindSize[3], _oSize:aWindSize[4],_cTitulo,,,,,,,,,.T.)
    //--------------------+
    // Layer da estrutura |
    //--------------------+
    _oLayer:Init( _oDlg, .F. )
    _oLayer:AddLine( "LINE01", 100 )
    _oLayer:AddCollumn( "TREE"   , 100,, "LINE01" )
    _oLayer:AddWindow( "TREE"   , "WNDTREE"      , "Estrutura"   , 100 ,.F. ,,,"LINE01" )
    
    _oPTree := _oLayer:GetWinPanel( "TREE"      , "WNDTREE"     , "LINE01" )

    _oTree := DbTree():New(0,0,0,0,_oPTree,,,.T.)
	_oTree:ALIGN := CONTROL_ALIGN_ALLCLIENT  
	
	//+------------------------------------------------------------------------+
	//| Monta o Tree View da Estrutura                                         |
	//+------------------------------------------------------------------------+
	_oTree:BeginUpdate()
	    ECLOJ001T(_cCodPai,Nil,Nil,Nil,Nil,Nil)
	_oTree:EndUpdate()

    //---------------+
    // Enchoice Tela |
    //---------------+
    _oDlg:bInit := {|| EnchoiceBar(_oDlg,{|| _oDlg:End() },{|| _oDlg:End() },.F.)}

_oDlg:Activate(,,,.T.,,,)


Return Nil 

/************************************************************************************/
/*/{Protheus.doc} ECLOJ001T
    @description Realiza a montagem da arvore de categorias 
    @type  Static Function
    @author Bernard M Margarido
    @since 16/02/2024
/*/
/************************************************************************************/
Static Function ECLOJ001T(_cCodPai,_lSeek1,_cTexto,_cCodCargo,_nCargo,_nCargoPai)
Local _cFilACU  := xFilial("ACU")

Local _nRecno    := 0

Local _lCpBloq	 := ( ACU->(FieldPos("ACU_MSBLQL")) > 0 )


Default _cTexto    := Space(130)
Default _cCodCargo := ""

Default _lSeek1    := .F.

Default _cCategoria := StrZero(0, _nTCodCat)
Default _cCatPai    := StrZero(0, _nTCodCat)
Default _nCargo     := 0
Default _nCargoPai  := 0

//----------------------------+
// ACU - Posiciona Categorias | 
//----------------------------+
dbSelectArea("ACU")
ACU->( dbSetOrder(2) )

//--------------------------------------------------------+
// Procura por uma categoria nao bloqueada (campo MSBLQL) |
//--------------------------------------------------------+
If !_lSeek1

	_lSeek1:= MsSeek(_cFilACU + _cCodPai) .And. (!_lCpBloq .OR. (_lCpBloq  .And. ACU->ACU_MSBLQL <> '1'))

	If !_lSeek1 .And. Found()
		While !_lSeek1 .And. ACU->( !Eof() .And. ACU->ACU_FILIAL == _cFilACU .And. ACU->ACU_CODPAI == _cCodPai )
			ACU->( dbSkip() )
			_lSeek1:= (!_lCpBloq .Or. (_lCpBloq  .And. ACU->ACU_MSBLQL <> '1'))			
		End
	EndIf

EndIf


If _lSeek1
    _nCargo++
    _nCargoPai := _nCargo

    _cCargo    := StrZero(_nCargo, _nTCodCat)
    _cCargoPai := StrZero(_nCargoPai, _nTCodCat)

	If !Empty(_cCodPai) .And. !Empty(_cTexto) .And. !Empty(_cCodCargo)
		_oTree:AddTree(_cTexto,.T.,,,"BPMSEDT3","BPMSEDT3","1" + _cCodCargo + _cCargo + _cCargoPai)			
	Else
		_oTree:AddTree(Space(Len(ACU->ACU_DESC) + 130 ),.T.,,,"BPMSEDT3","BPMSEDT3","1" + _cCodCargo + _cCargo + _cCargoPai)
	EndIf

    //---------------------------------------+
	// Enquanto esta regiao for a regiao pai |
    //---------------------------------------+
	While ACU->( !Eof() .And. ACU->ACU_FILIAL + ACU->ACU_CODPAI == _cFilACU + _cCodPai ) 

        //-----------------------------+
        // Salta categorias bloqueadas |
        //-----------------------------+
		If (_lCpBloq  .And. ACU->ACU_MSBLQL == '1')
			ACU->( dbSkip() )
			Loop
		End

		_cCodCargo  :=  ACU->ACU_COD
		_nRecno     :=  ACU->( Recno() )
		_cTexto     :=  RTrim(ACU->ACU_DESC)
		
		//--------------------------------------------------------+
		// Procura por uma categoria nao bloqueada (campo MSBLQL) |
		//--------------------------------------------------------+
		_lSeek1 := MSSeek(_cFilACU + _cCodCargo) .And. (!_lCpBloq .Or. (_lCpBloq  .And. ACU->ACU_MSBLQL <> '1'))
		
		If !_lSeek1 .And. Found()
			While !_lSeek1 .And. ACU->( !Eof() .And. ACU->ACU_FILIAL == _cFilACU .And. ACU->ACU_CODPAI == _cCodCargo )
				ACU->( dbSkip() )
				_lSeek1:= (!_lCpBloq .Or. (_lCpBloq  .And. ACU->ACU_MSBLQL <> '1'))			
			End
		EndIf
		
		If !_lSeek1
			_oTree:AddTreeItem(_cTexto,"BPMSEDT3","BPMSEDT3","1" + _cCodCargo + _cCargo + _cCargoPai)
		Else
			ECLOJ001T(ACU_CODPAI,_lSeek1,_cTexto,_cCodCargo,_nCargo,_nCargoPai)
		EndIf
		ACU->( dbGoto(_nRecno) ) 
		ACU->( dbSkip() ) 
	End
	
	_oTree:EndTree()	
EndIf

Return Nil 

/************************************************************************************/
/*/{Protheus.doc} MenuDef
    @description Menu padrao para manutencao do cadastro
    @author Bernard M. Margarido
    @since 21/11/2023
    @version undefined
/*/
/************************************************************************************/
Static Function MenuDef()
Local _aRotina  := {}

_aRotina := FWMVCMenu( "ECLOJ001" )

aAdd(_aRotina,{"Estrutura","U_ECLOJ01A",0 , 6})

Return _aRotina


