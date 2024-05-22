#INCLUDE "PROTHEUS.CH"
#INCLUDE "FWMVCDEF.CH"

#DEFINE CRLF CHR(13) + CHR(10)

Static _nLenGRD := 0 

/************************************************************************************/
/*/{Protheus.doc} ECLOJ005
    @description Produtos com Grade
    @author Bernard M. Margarido
    @since 29/04/2019
    @version undefined
    @type function
/*/
/************************************************************************************/
User Function ECLOJ005()
Private _oBrowse	:= Nil

Private _cAliasGRD  := "GRD"
Private _cALiasSKU  := "SKU"

Private  _nOldLen   := SetVarNameLen(255) 

//---------------------------------+
// Instanciamento da Classe Browse |
//---------------------------------+
_oBrowse := FWMBrowse():New()

//------------------+
// Tabela utilizado |
//------------------+
_oBrowse:SetAlias("SB4")

//-------------------+
// Adiciona Legendas |
//-------------------+
_oBrowse:AddLegend("B4_XSTATUS == '1' "	, "GREEN"				, "Normal"          , '1')
_oBrowse:AddLegend("B4_XSTATUS == '9' "	, "BLACK"				, "Descontinuado"   , '1')

_oBrowse:AddLegend("B4_XATIVO == '1' "	, "GREEN"				, "Ativo"           , '2')
_oBrowse:AddLegend("B4_XATIVO == '2' "	, "RED"				    , "Inativo"         , '2')

//------------------+
// Titulo do Browse |
//------------------+
_oBrowse:SetDescription('Produtos - Grade')

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
    @since 21/01/2024
    @version undefined
    @type function
/*/
/************************************************************************************/
Static Function ModelDef()
Local _oStrSB4  := Nil 
Local _oStrMHH  := Nil 
Local _oStrGRD  := Nil 
Local _oStrSB1  := Nil 
Local _oModel   := Nil 
Local _oEvent   := Nil 

Local _bLoadGRD := {|_oModel| ECLOJ05C(_oModel)}
Local _bLoadSB1 := {|_oModel| ECLOJ05E(_oModel)}

//----------------------------------------+
// Estrutura dos campos Grade de Produtos |
//----------------------------------------+
_oStrSB4    := FWFormStruct(1,"SB4")
_oStrMHH    := FWFormStruct(1,"MHH")
_oStrGRD    := ECLOJ05STR(_cAliasGRD,1)
_oStrSB1    := ECLOJ05STR(_cALiasSKU,1) 

//-----------------------------+
// Altera propriedade do campo |
//-----------------------------+
_oStrSB4:SetProperty('B4_COLUNA', MODEL_FIELD_VALID, FwBuildFeature(STRUCT_FEATURE_VALID, 'VAZIO() .OR. U_ECLOJ05A(B4_LINHA,B4_COLUNA)'))	

//----------------------------------+
// Cria o Objeto do Modelo de Dados |
//----------------------------------+
_oModel	:= MPFormModel():New("SB4_00", /*_bPreValid*/ , /*_bPosValid*/ , /*_bCommit*/ , /*_bCancel*/ )

//-----------------------------------+
// Classe para validação dos eventos |
//-----------------------------------+
_oEvent := GradeProduto():New()

//-----------------------------------------------+
// Adiciona ao modelo o componente de formulário |
//-----------------------------------------------+
_oModel:AddFields("SB4_MASTER",,_oStrSB4)
_oModel:AddFields("MHH_MASTER","SB4_MASTER",_oStrMHH)

_oModel:AddGrid("GRD_GRADE","SB4_MASTER",_oStrGRD, /*_bLinePre*/ , /*_bLinePost*/, /*_bPre */, /*_bLinePost*/ ,_bLoadGRD)
_oModel:AddGrid("SKU_MASTER","SB4_MASTER",_oStrSB1, /*_bLinePre*/ ,  /*_bLinePost*/ ,  /*_bPre*/ , /*_bLinePost*/ ,_bLoadSB1)

_oModel:SetRelation("MHH_MASTER",{{"MHH_FILIAL",'xFilial("MHH")'},{"MHH_COD","B4_COD"}},MHH->(IndexKey(1)))

_oModel:GetModel('MHH_MASTER'):SetOptional(.T.)
_oModel:GetModel('GRD_GRADE'):SetOptional(.T.)
_oModel:GetModel('SKU_MASTER'):SetOptional(.T.)

//----------+
// Inclusao |
//----------+
If _oModel:GetOperation() == 3

    _oModel:GetModel('GRD_GRADE'):SetNoInsertLine(.F.)
    _oModel:GetModel('GRD_GRADE'):SetNoUpdateLine(.F.)
    _oModel:GetModel('GRD_GRADE'):SetNoDeleteLine(.F.)

    _oModel:GetModel('SKU_MASTER'):SetNoInsertLine(.F.)
	_oModel:GetModel('SKU_MASTER'):SetNoUpdateLine(.F.)
	_oModel:GetModel('SKU_MASTER'):SetNoDeleteLine(.F.)

ElseIf _oModel:GetOperation() == 4

    _oModel:GetModel('GRD_GRADE'):SetNoInsertLine(.F.)
    _oModel:GetModel('GRD_GRADE'):SetNoUpdateLine(.F.)
    _oModel:GetModel('GRD_GRADE'):SetNoDeleteLine(.T.)

    _oModel:GetModel('SKU_MASTER'):SetNoInsertLine(.F.)
	_oModel:GetModel('SKU_MASTER'):SetNoUpdateLine(.F.)
	_oModel:GetModel('SKU_MASTER'):SetNoDeleteLine(.T.)

ElseIf _oModel:GetOperation() == 5 .Or. _oModel:GetOperation() == 2

    _oModel:GetModel('GRD_GRADE'):SetNoInsertLine(.T.)
    _oModel:GetModel('GRD_GRADE'):SetNoUpdateLine(.T.)
    _oModel:GetModel('GRD_GRADE'):SetNoDeleteLine(.T.)

    _oModel:GetModel('SKU_MASTER'):SetNoInsertLine(.T.)
	_oModel:GetModel('SKU_MASTER'):SetNoUpdateLine(.T.)
	_oModel:GetModel('SKU_MASTER'):SetNoDeleteLine(.T.)

EndIf 

//------------------------+
// Chave primaria produto | 
//------------------------+
//_oModel:SetPrimaryKey({})
_oModel:SetPrimaryKey( {"B4_FILIAL","B4_COD"} )

_oModel:SetDescription('Bepi - Grade Produto')
_oModel:GetModel('SB4_MASTER'):SetDescription('Dados Produto')
_oModel:GetModel('MHH_MASTER'):SetDescription('Dados e-Commerce')
_oModel:GetModel('GRD_GRADE'):SetDescription( "Dados Grade" )
_oModel:GetModel('SKU_MASTER'):SetDescription( "Dados Sku" )

//_oModel:SetActivate( {|_oModel| ECLOJ05D(_oModel) } )
_oModel:InstallEvent("GRADEPROD",, _oEvent)

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
Local _oStrViewSB4	:= Nil
Local _oStrViewMHH	:= Nil
Local _oStrViewGRD	:= Nil 
Local _oStrViewSB1	:= Nil 

_oStrViewSB4	:= FWFormStruct(2 ,"SB4" )
_oStrViewMHH	:= FWFormStruct(2 ,"MHH" )
_oStrViewGRD	:= ECLOJ05STR(_cAliasGRD,2)
_oStrViewSB1	:= ECLOJ05STR(_cALiasSKU,2)

//----------------------------------------------------------------------------+
//³Cria um objeto de Modelo de dados baseado no ModelDef() do fonte informado |
//----------------------------------------------------------------------------+
_oModel := FWLoadModel("ECLOJ005") 
_oView	:= FWFormView():New()

_oView:SetModel(_oModel)
_oView:SetDescription('Grade Produtos')

//---------------------+
// View das estruturas |
//---------------------+
_oView:AddField('SB4_FORM' 	, _oStrViewSB4 , 'SB4_MASTER' )
_oView:AddField('MHH_FORM' 	, _oStrViewMHH , 'MHH_MASTER' )

_oView:AddGrid('GRD_FORM'	, _oStrViewGRD , 'GRD_GRADE' )
_oView:AddGrid('SKU_FORM'	, _oStrViewSB1 , 'SKU_MASTER' )

//----------------------+
// Remove campo da View |
//----------------------+
_oStrViewSB1:RemoveField("SKU_DELET")
_oStrViewSB1:RemoveField("GRD_DELET")

//------------------------------------------------------------+
// Criar "box" horizontal para receber algum elemento da view |
//------------------------------------------------------------+
_oView:CreateHorizontalBox( 'SUPERIOR'    		, 060 ,,, /*'PASTAS'*/, /*'ABA01'*/ )
_oView:CreateHorizontalBox( 'INFERIOR'    		, 040 ,,, /*'PASTAS'*/, /*'ABA01'*/ )

//-------------+
// Cria pastas |
//-------------+
_oView:CreateFolder('DADOS',"SUPERIOR")
_oView:CreateFolder('GRADE',"INFERIOR")

_oView:AddSheet( 'DADOS', 'ABA01', "Dados Produto" )
_oView:AddSheet( 'DADOS', 'ABA02', "e-Commerce" )
_oView:AddSheet( 'GRADE', 'ABA01', "SKU - Grade" )
_oView:AddSheet( 'GRADE', 'ABA02', "SKU - Dados" )

_oView:CreateHorizontalBox( 'SUPERIOR_E1'       , 100,,, 'DADOS', 'ABA01' )
_oView:CreateHorizontalBox( 'SUPERIOR_D1'       , 100,,, 'DADOS', 'ABA02' )
_oView:CreateHorizontalBox( 'INFERIOR_E1' 	    , 100,,, 'GRADE', 'ABA01' )
_oView:CreateHorizontalBox( 'INFERIOR_D1' 	    , 100,,, 'GRADE', 'ABA02' )

_oView:SetOwnerView('SB4_FORM'	    ,'SUPERIOR_E1')
_oView:SetOwnerView('MHH_FORM'	    ,'SUPERIOR_D1')
_oView:SetOwnerView('GRD_FORM'      ,'INFERIOR_E1')
_oView:SetOwnerView('SKU_FORM'      ,'INFERIOR_D1')

Return _oView

/**************************************************************************************/
/*/{Protheus.doc} ECLOJ05A
    @description Valida linha e coluna para montar grade de produtos
    @type  Function
    @author Bernard M. Margarido
    @since 01/02/2024
    @version version
/*/
/**************************************************************************************/
User Function ECLOJ05A(_cLinha,_cColuna)
Local _oView 	    := FWViewActive()
Local _oViewGRD     := _oView:GetViewStruct("GRD_GRADE")

Local _aFields      := _oViewGRD:GetFields()

Local _nX           := 0

//Local _oModel	    := FWModelActive()

//Local _oStrMdl      := Nil 

Local _lRet         := .T.    

If Empty(_cLinha)
    _cMsg := "Campo " + RetTitle("B4_LINHA") + " não preenchido."
    _lRet := .F.
EndIf 

If Empty(_cColuna)
    _cMsg := "Campo " + RetTitle("B4_COLUNA") + " não preenchido."
    _lRet := .F.
EndIf 

//----------------------------------------------+
// Cria grade com as novas informações da Grade |
//----------------------------------------------+
If _lRet 

    For _nX := 1 To Len(_aFields)
        _oViewGRD:RemoveField(_aFields[_nX][1])
    Next _nX 
   
    _oViewGRD	:= ECLOJ05STR(_cAliasGRD,2)

    If _oView <> Nil 
        _oView:Refresh()
    EndIf 

EndIf 

If !_lRet .And. !Empty(_cMsg)
    Help( ,, 'ECLOJ005',, _cMsg, 1, 0 )
EndIf 

Return _lRet 

/************************************************************************************/
/*/{Protheus.doc} ECLOJ05C
    @description Carrega dados das tabelas 
    @type  Static Function
    @author Bernard M Margarido
    @since 12/08/2022
/*/
/************************************************************************************/
Static Function ECLOJ05C(_oModel)
Local _cModelID := _oModel:GetId()

Local _nOpcA    := _oModel:GetOperation()

Local _aLoad    := {}

If _cModelID == "GRD_GRADE"
    If _nOpcA <> 3
        ECLOJ05F(_oModel,_aLoad, .T.)
    Else 
        ECLOJ05F(_oModel,_aLoad, .F.)
    EndIf 
EndIf 

Return _aLoad

/************************************************************************************/
/*/{Protheus.doc} ECLOJ05E
    @description Carrega dados das tabelas 
    @type  Static Function
    @author Bernard M Margarido
    @since 12/08/2022
/*/
/************************************************************************************/
Static Function ECLOJ05E(_oModel)
Local _cModelID := _oModel:GetId()

Local _nOpcA    := _oModel:GetOperation()

Local _aLoad    := {}

If _cModelID == "SKU_MASTER"
    If _nOpcA <> 3
        ECLOJ05G(_oModel, _aLoad, .T.) 
    Else 
        aAdd(_aLoad,{0, {" ", " ", " ",.F.} })
    EndIf 
EndIf

Return _aLoad

/************************************************************************************/
/*/{Protheus.doc} ECLOJ05D
    @description Carrega dados em tela 
    @type  Static Function
    @author Bernard M Margarido
    @since 23/01/2024
    @version version
/*/
/************************************************************************************/
Static Function ECLOJ05D(_oModel)
Local _lRet         := .T.

Local _nOpc         := _oModel:GetOperation()

If _nOpc == 3
    //----------------------+
    // Cria grade variações |
    //----------------------+
    ECLOJ05F(_oModel)    
EndIf 

Return _lRet 

/************************************************************************************/
/*/{Protheus.doc} ECLOJ05F
    @description Cria grade de variações 
    @type  Static Function
    @author Bernard M Margarido 
    @since 25/01/2024
    @version version
/*/
/************************************************************************************/
Static Function ECLOJ05F(_oModel, _aLoad, _lLoad)    
Local _oView 	    := FWViewActive()
Local _oViewGRD     := _oView:GetViewObj("GRD_GRADE")

Local _oModelSB4    := _oModel:GetModel( "SB4_MASTER" )

Local _cCodPai      := ""
Local _cLinha       := ""
Local _cColuna      := ""
Local _cLinGrd      := ""
Local _cColGrd      := ""

Local _aMascara 	:= Separa(GetMv('MV_MASCGRD'),',')
Local _aViewGRD     := _oViewGRD[3]:oStructView:aFields 

Local _nPMarca      := 0
Local _nX           := 0
Local _nTPai        := Val(_aMascara[1])
Local _nTLinha      := Val(_aMascara[2])
Local _nTColuna     := Val(_aMascara[3])

Default _aLoad      := {}
Default _lLoad      := .F.

//-----------------------------+
// Carrega dados da Grid Grade |
//-----------------------------+
_cCodPai    := IIF(_lLoad, M->B4_COD, _oModelSB4:GetValue("B4_COD"))
_cLinha     := IIF(_lLoad, M->B4_LINHA, _oModelSB4:GetValue("B4_LINHA"))
_cColuna    := IIF(_lLoad, M->B4_COLUNA, _oModelSB4:GetValue("B4_COLUNA"))

If _lLoad 
    dbSelectArea("SB1")
    SB1->( dbSetOrder(1) )
    If SB1->( dbSeek(xFilial("SB1") + PadR(SubStr(_cCodPai, 1, _nTPai ), _nTPai)))
        While SB1->( !Eof() .And. xFilial("SB1") + PadR(SubStr(_cCodPai, 1, _nTPai ), _nTPai) == SB1->B1_FILIAL + SubStr(SB1->B1_COD, 1, _nTPai) )

            _cLinGrd    := SubStr(SB1->B1_COD,_nTPai + 1, _nTLinha)
            _cDescLin   := Posicione("SBV", 1, xFilial("SBV") + _cLinha + _cLinGrd, "BV_DESCRI")
            _cDescLin   := RTrim(_cLinGrd) + " - [ " + RTrim(_cDescLin) + " ]"
            _cColGrd    := SubStr(SB1->B1_COD,_nTPai + _nTLinha + 1, _nTColuna)   
            _nPCol      := aScan(_aViewGRD,{|x| SubStr(RTrim(x[1]),5) == _cColGrd} )

            If Len(_aLoad) > 0 
                _nPMarca    := aScan(_aLoad,{|x| RTrim(x[2][1]) == RTrim(_cDescLin)} )
            EndIf 

            If _nPMarca == 0
                aAdd(_aLoad,{SBV->( Recno() ),Array(_nLenGRD + 1)})
            EndIf 

            For _nX := 1 To _nLenGRD
                If _nX == 1 
                    _aLoad[Len(_aLoad)][2][_nX] := _cDescLin
                ElseIf _nPCol == _nX
                    _aLoad[Len(_aLoad)][2][_nX] := 'X'
                Else 
                    _aLoad[Len(_aLoad)][2][_nX] := IIF(Empty(_aLoad[Len(_aLoad)][2][_nX]), ' ', _aLoad[Len(_aLoad)][2][_nX])
                EndIf 
            Next _nX 

             _aLoad[Len(_aLoad)][2][_nLenGRD + 1] := .F.

            SB1->( dbSkip() )
        EndDo 
    EndIf 
Else 

    dbSelectArea("SBV")
    SBV->( dbSetOrder(1) )
    If SBV->( dbSeek(xFilial("SBV") + _cColuna ))
        aAdd(_aLoad,{0 , {} })
        
        aAdd(_aLoad[Len(_aLoad)][2], " ")

        While SBV->( !Eof() .And. xFilial("SBV") + _cColuna == SBV->BV_FILIAL + SBV->BV_TABELA)
            aAdd(_aLoad[Len(_aLoad)][2], " ")
            SBV->( dbSkip() )
        EndDo 
        
        aAdd(_aLoad[Len(_aLoad)][2], .F.)

    Else 
        aAdd(_aLoad,{0 , Array(3)})
        _aLoad[Len(_aLoad)][2][1]   := ""
        _aLoad[Len(_aLoad)][2][2]   := ""
        _aLoad[Len(_aLoad)][2][3]   := .F.
    EndIf 
EndIf 

Return _oModel

/************************************************************************************/
/*/{Protheus.doc} ECLOJ05G
    @description Cria grade de SKU's
    @type  Static Function
    @author Bernard M Margarido 
    @since 25/01/2024
    @version version
/*/
/************************************************************************************/
Static Function ECLOJ05G(_oModel, _aLoad, _lLoad)
Local _oModelSB4    := _oModel:GetModel( "SB4_MASTER" )
Local _oModelSB1    := _oModel:GetModel( "SKU_MASTER" )

Local _cCodPai      := ""
Local _cLinha       := ""
Local _cColuna      := ""

Local _aMascara 	:= Separa(GetMv('MV_MASCGRD'),',')

Local _nTPai        := Val(_aMascara[1])

Default _aLoad      := {}
Default _lLoad      := .T.

//-----------------------------+
// Carrega dados da Grid Grade |
//-----------------------------+
_cCodPai    := IIF(_lLoad, M->B4_COD, _oModelSB4:GetValue("B4_COD"))
_cLinha     := IIF(_lLoad, M->B4_LINHA, _oModelSB4:GetValue("B4_LINHA"))
_cColuna    := IIF(_lLoad, M->B4_COLUNA, _oModelSB4:GetValue("B4_COLUNA"))

dbSelectArea("SB1")
SB1->( dbSetOrder(1) )
If SB1->( dbSeek(xFilial("SB1") + PadR(SubStr(_cCodPai, 1, _nTPai ), _nTPai)))
    While SB1->( !Eof() .And. xFilial("SB1") + PadR(SubStr(_cCodPai, 1, _nTPai ), _nTPai) == SB1->B1_FILIAL + SubStr(SB1->B1_COD, 1, _nTPai) )
        
        If _lLoad

            aAdd(_aLoad,{SB1->( Recno() ),Array(4)})
            _aLoad[Len(_aLoad)][2][1] := SB1->B1_COD
            _aLoad[Len(_aLoad)][2][2] := SB1->B1_DESC
            _aLoad[Len(_aLoad)][2][3] := SB1->B1_CODBAR
            _aLoad[Len(_aLoad)][2][4] := .F.

        Else 

            _oModelSB1:LoadValue("SKU_COD", SB1->B1_COD ,.T.)
            _oModelSB1:LoadValue("SKU_DESC", SB1->B1_DESC ,.T.)
            _oModelSB1:LoadValue("SKU_CODBAR", SB1->B1_CODBAR ,.T.)
            _oModelSB1:LoadValue("SKU_DELET", .F. ,.T.)

        EndIf 

        If SB1->( !Eof() .And. xFilial("SB1") + PadR(SubStr(_cCodPai, 1, _nTPai ), _nTPai) == SB1->B1_FILIAL + SubStr(SB1->B1_COD, 1, _nTPai) ) .And. !_lLoad
            _oModelSB1:AddLine() 
        EndIf 

        SB1->( dbSkip() )
    EndDo 
EndIf 

Return _oModel

/************************************************************************************/
/*/{Protheus.doc} ECLOJ05X
    @description Valida dado digitado na Grade 
    @type  Function
    @author Bernard M Margarido
    @since 26/01/2024
    @version version
/*/
/************************************************************************************/
User Function ECLOJ05X()
Local _oModel	:= FWModelActive()
Local _oView    := FWViewActive()
Local _oModelSB4:= _oModel:GetModel( "SB4_MASTER" )
Local _oModelGRD:= _oModel:GetModel( "GRD_GRADE" )
Local _oModelSKU:= _oModel:GetModel( "SKU_MASTER" )

Local _aMascara := Separa(GetMv("MV_MASCGRD"),",")

Local _xRead    := ReadVar()
Local _cCpo     := SubStr(_xRead,4)
Local _cMsg     := ""
Local _cCodPai  := ""
Local _cTabLin  := ""
Local _cTabCol  := ""
Local _cGrdLin  := ""
Local _cGrdCol  := ""
Local _cCodSku  := ""
Local _cDescPai := ""
Local _cDescLin := ""
Local _cDescCol := ""

Local _nTPai    := Val(_aMascara[1])
Local _nTLinha  := Val(_aMascara[2])
Local _nTColuna := Val(_aMascara[3])

Local _lRet     := .T.
Local _lAdd     := .T.

If !Empty(&(_xRead))
    If RTrim(_xRead) <> "M->GRD_LINHA"
        If &(_xRead) <> "X"
            _oModelGRD:LoadValue(_cCpo,"X")
            _oModelGRD:LoadValue("GRD_DELET",.T.)
            &(_xRead) := "X"
        EndIf 
    EndIf 
EndIf 

//-----------------+
// Valida GRID SKU |
//-----------------+
dbSelectArea("SB1")
SB1->( dbSetOrder(1) )

_cCodPai := SubStr(_oModelSB4:GetValue("B4_COD"), 1, _nTPai)
_cDescPai:= _oModelSB4:GetValue("B4_DESC")
_cTabLin := _oModelSB4:GetValue("B4_LINHA")
_cTabCol := _oModelSB4:GetValue("B4_COLUNA")
_cGrdLin := SubStr(_oModelGRD:GetValue("GRD_LINHA"), 1, _nTLinha)
_cGrdCol := SubStr(StrTran(_xRead,"M->GRD_",""), 1, _nTColuna)
_cCodSku := _cCodPai + _cGrdLin + _cGrdCol

_lSKU    := _oModelSKU:SeekLine({{"SKU_COD",_cCodSku}},.T.)

If _lSKU .And. Empty(&(_xRead))  
    _lAdd := .F.
    If _oModelSKU:GetValue("SKU_DELET")
        If !_oModelSKU:IsDeleted()
            _oModelSKU:DeleteLine()
        EndIf 
    Else 
        _cMsg := "Não é possivel alterar grade de produtos já inseridos."
        _lRet := .F.
    EndIf 
ElseIf _lSKU .And. !Empty(&(_xRead)) 
    _lAdd := .F.
    If _oModelSKU:GetValue("SKU_DELET")
        If _oModelSKU:IsDeleted()
            _oModelSKU:UnDeleteLine()
        EndIf 
    EndIf 
EndIf 

If _lAdd
    If !_lSKU .And. !Empty(&(_xRead))  
        
        _cDescLin := RTrim(Posicione("SBV", 1, xFilial("SBV") + _cTabLin + _cGrdLin, "BV_DESCRI"))
        _cDescCol := RTrim(Posicione("SBV", 1, xFilial("SBV") + _cTabCol + _cGrdCol, "BV_DESCRI"))
        _cDescSKU:= RTrim(_cDescPai) + " " + _cDescLin + "/" + _cDescCol

        _oModelSKU:AddLine() 
        _oModelSKU:LoadValue("SKU_COD", _cCodSku ,.T.)
        _oModelSKU:LoadValue("SKU_DESC", _cDescSKU ,.T.)
        _oModelSKU:LoadValue("SKU_CODBAR", "" ,.T.)
        _oModelSKU:LoadValue("SKU_DELET", .T. ,.T.)

    EndIf 
EndIf 
//-------------------+
// Mensagem de aviso | 
//-------------------+
If !_lRet
    Help( ,, 'ECLOJ005',, _cMsg, 1, 0 )

    _oModelGRD:LoadValue(_cCpo,"X")
    &(_xRead) := "X"

EndIf 

//------------------------+
// Atualiza dados na tela |
//------------------------+
If _oView <> Nil
    _oView:Refresh()
EndIf

Return _lRet

/************************************************************************************/
/*/{Protheus.doc} ECLOJ05Z
    @description Realiza a validação do codigo de barras digitado
    @type  Function
    @author Bernard M Margarido
    @since 29/01/2024
    @version version
/*/
/************************************************************************************/
User Function ECLOJ05Z()
Local _oModel	:= FWModelActive()
Local _oView    := FwViewActive()
Local _oModelSKU:= _oModel:GetModel( "SKU_MASTER" )

Local _aSaveRows:= {}

Local _xRead    := ReadVar()
Local _cCpo     := SubStr(_xRead,4)    
Local _cCodBar  := &(_xRead)
Local _cDigBar  := ""
Local _cCBarra  := ""

Local _lRet     := .T.


//---------------------------+
// Atualiza codigo de barras |
//---------------------------+
_aSaveRows          := FWSaveRows()

If !Empty(_cCodBar)
    If Len(RTrim(_cCodBar)) == 12
        _cDigBar    := EanDigito(_cCodBar)
        _cCBarra    := RTrim(_cCodBar) + RTrim(_cDigBar)
    Else 
        _cDigBar    := EanDigito(Left(_cCodBar,12))
        _cCBarra    := RTrim(_cCodBar) + RTrim(_cDigBar)
    EndIf

    &(_xRead)   := _cCBarra
    _oModelSKU:LoadValue(_cCpo,_cCBarra) 

EndIf 

//------------------------+
// Atualiza dados na tela |
//------------------------+
If _oView <> Nil
    _oView:Refresh()
EndIf

FWRestRows( _aSaveRows )
Return _lRet

/************************************************************************************/
/*/{Protheus.doc} ECLOJ05STR
    @description Cria estrutura de campos customzados
    @type  Static Function
    @author Bernard M Margarido
    @since 29/01/2024
    @version version
/*/
/************************************************************************************/
Static Function ECLOJ05STR(_cAlias,_nType,_lAddCol)
Local _oStruct  := Nil 

Local _cColuna  := IIF(ValType(M->B4_COLUNA) <> "U", M->B4_COLUNA, SB4->B4_COLUNA)
Local _cChave   := ""
Local _cDescChv := ""
Local _cTitulo  := ""
Local _cOrder   := "01"
Local _cTCod    := TamSx3("B1_COD")[3]
Local _cTDesc   := TamSx3("B1_DESC")[3]
Local _cTCodbar := TamSx3("B1_CODBAR")[3]

Local _nTCod    := TamSx3("B1_COD")[1]
Local _nTDesc   := TamSx3("B1_DESC")[1]
Local _nTCodbar := TamSx3("B1_CODBAR")[1]

Default _lAddCol:= .F.

//--------------------------------+
// Cria campos no modelo de dados |
//--------------------------------+
If _nType == 1
    _oStruct    := FWFormModelStruct():New()

    If _cAlias == "GRD"
        
        _oStruct:AddField(  "Linha"    		        ,;	// [01] Titulo do campo
                            "Linha" 			    ,;	// [02] ToolTip do campo
                            "GRD_LINHA"		        ,;	// [03] Id do Field
                            "C"					    ,;	// [04] Tipo do campo
                            40                      ,;	// [05] Tamanho do campo
                            0					    ,;	// [06] Decimal do campo
                            { || .T. }			    ,;	// [07] Code-block de validação do campo
                                                    ,;	// [08] Code-block de validação When do campo
                                                    ,;	// [09] Lista de valores permitido do campo
                            .F.                     )   // [10] Indica se o campo tem preenchimento obrigatório

        //-------------------------+
        // SBV - Grade de Produtos |
        //-------------------------+
        dbSelectArea("SBV")
        SBV->( dbSetOrder(1) )
        If SBV->( dbSeek(xFilial("SBV") + _cColuna ))
        
            _nLenGRD  := _nLenGRD + 1

            While SBV->( !Eof() .And. xFilial("SBV") + _cColuna == SBV->BV_FILIAL + SBV->BV_TABELA)

                _cChave   := RTrim(SBV->BV_CHAVE)
                _cDescChv := RTrim(SBV->BV_DESCRI)
                _cTitulo  := _cChave + " [" + _cDescChv + "]"
                _nLenGRD  := _nLenGRD + 1 
                _oStruct:AddField(  _cTitulo    		    ,;	// [01] Titulo do campo
                                    _cTitulo 			    ,;	// [02] ToolTip do campo
                                    "GRD_" + _cChave	    ,;	// [03] Id do Field
                                    "C"					    ,;	// [04] Tipo do campo
                                    1                       ,;	// [05] Tamanho do campo
                                    0					    ,;	// [06] Decimal do campo
                                    FwBuildFeature( STRUCT_FEATURE_VALID,"U_ECLOJ05X()" )	    ,;	// [07] Code-block de validação do campo
                                                            ,;	// [08] Code-block de validação When do campo
                                                            ,;	// [09] Lista de valores permitido do campo
                                    .F.                     )   // [10] Indica se o campo tem preenchimento obrigatório
                SBV->( dbSkip() )
            EndDo

            _oStruct:AddField(  ""    		            ,;	// [01] Titulo do campo
                                "" 			            ,;	// [02] ToolTip do campo
                                "GRD_DELET"		        ,;	// [03] Id do Field
                                "L"					    ,;	// [04] Tipo do campo
                                1                       ,;	// [05] Tamanho do campo
                                0					    ,;	// [06] Decimal do campo
                                { || .T. }			    ,;	// [07] Code-block de validação do campo
                                                        ,;	// [08] Code-block de validação When do campo
                                                        ,;	// [09] Lista de valores permitido do campo
                                .F.                     )   // [10] Indica se o campo tem preenchimento obrigatório
        EndIf 
    ElseIf _cAlias == "SKU"

        _oStruct:AddField(  RTrim(RetTitle("B1_COD"))	,;	// [01] Titulo do campo
                            RTrim(RetTitle("B1_COD"))	,;	// [02] ToolTip do campo
                            "SKU_COD"		            ,;	// [03] Id do Field
                            _cTCod				        ,;	// [04] Tipo do campo
                            _nTCod                      ,;	// [05] Tamanho do campo
                            0					        ,;	// [06] Decimal do campo
                            { || .T. }			        ,;	// [07] Code-block de validação do campo
                            Nil                         ,;	// [08] Code-block de validação When do campo
                            Nil                         ,;	// [09] Lista de valores permitido do campo
                            .F.                         )   // [10] Indica se o campo tem preenchimento obrigatório

        _oStruct:AddField(  RTrim(RetTitle("B1_DESC"))  ,;	// [01] Titulo do campo
                            RTrim(RetTitle("B1_DESC"))  ,;	// [02] ToolTip do campo
                            "SKU_DESC"		            ,;	// [03] Id do Field
                            _cTDesc				        ,;	// [04] Tipo do campo
                            _nTDesc                     ,;	// [05] Tamanho do campo
                            0					        ,;	// [06] Decimal do campo
                            { || .T. }			        ,;	// [07] Code-block de validação do campo
                            Nil                         ,;	// [08] Code-block de validação When do campo
                            Nil                         ,;	// [09] Lista de valores permitido do campo
                            .F.                         )   // [10] Indica se o campo tem preenchimento obrigatório

        _oStruct:AddField(  RTrim(RetTitle("B1_CODBAR"))    ,;	// [01] Titulo do campo
                            RTrim(RetTitle("B1_CODBAR"))    ,;	// [02] ToolTip do campo
                            "SKU_CODBAR"	                ,;	// [03] Id do Field
                            _cTCodbar			            ,;	// [04] Tipo do campo
                            _nTCodbar                       ,;	// [05] Tamanho do campo
                            0					            ,;	// [06] Decimal do campo
                            FwBuildFeature( STRUCT_FEATURE_VALID,"U_ECLOJ05Z()" )			            ,;	// [07] Code-block de validação do campo
                            Nil                             ,;	// [08] Code-block de validação When do campo
                            Nil                             ,;	// [09] Lista de valores permitido do campo
                            .F.                             )   // [10] Indica se o campo tem preenchimento obrigatório
        
        _oStruct:AddField(  ""                              ,;	// [01] Titulo do campo
                            ""                              ,;	// [02] ToolTip do campo
                            "SKU_DELET"	                    ,;	// [03] Id do Field
                            "L"			                    ,;	// [04] Tipo do campo
                            1                               ,;	// [05] Tamanho do campo
                            0					            ,;	// [06] Decimal do campo
                            {|| .T. } 			            ,;	// [07] Code-block de validação do campo
                            Nil                             ,;	// [08] Code-block de validação When do campo
                            Nil                             ,;	// [09] Lista de valores permitido do campo
                            .F.                             )   // [10] Indica se o campo tem preenchimento obrigatório
    EndIf 

//---------------------+
// Cria campos na View |
//---------------------+
ElseIf _nType == 2
    
    _oStruct    := FWFormViewStruct():New()
    
    If _cAlias == "GRD"

        _oStruct:AddField(  "GRD_LINHA"		            ,;	// [01] Id do Field
                            "01"				        ,;	// [02] Ordem
                            "Linha"	                   ,;	// [03] Titulo do campo
                            "Linha" 	                ,;	// [04] ToolTip do campo
                                                        ,;	// [05] Help
                            "G"					        ,;	// [06] Tipo do campo
                            "@!"                        ,;	// [07] Picture
                            Nil                         ,;	// [08] PictVar
                            "SBVLIN"			        ,;  // [09] F3
                            .T.                         ,;  // [10]
                            Nil                         ,;  // [11]
                            ""                          )	// [12]

        dbSelectArea("SBV")
        SBV->( dbSetOrder(1) )
        If SBV->( dbSeek(xFilial("SBV") + _cColuna ))

            _oStruct:AddField(  "GRD_LINHA"		            ,;	// [01] Id do Field
                                "01"				        ,;	// [02] Ordem
                                "Linha"	                   ,;	// [03] Titulo do campo
                                "Linha" 	                ,;	// [04] ToolTip do campo
                                                            ,;	// [05] Help
                                "G"					        ,;	// [06] Tipo do campo
                                "@!"                        ,;	// [07] Picture
                                Nil                         ,;	// [08] PictVar
                                "SBVLIN"			        ,;  // [09] F3
                                .T.                         ,;  // [10]
                                Nil                         ,;  // [11]
                                ""                          )	// [12]

            While SBV->( !Eof() .And. xFilial("SBV") + _cColuna == SBV->BV_FILIAL + SBV->BV_TABELA)
                _cOrder   := Soma1(_cOrder,1)
                _cChave   := RTrim(SBV->BV_CHAVE)
                _cDescChv := RTrim(SBV->BV_DESCRI)
                _cTitulo  := _cChave + " [" + _cDescChv + "]"

                _oStruct:AddField(  "GRD_" + _cChave            ,;	// [01] Id do Field
                                    _cOrder	                    ,;	// [02] Ordem
                                    _cTitulo	                ,;	// [03] Titulo do campo
                                    _cTitulo 	                ,;	// [04] ToolTip do campo
                                                                ,;	// [05] Help
                                    "G"					        ,;	// [06] Tipo do campo
                                    "@!"                        ,;	// [07] Picture
                                    Nil                         ,;	// [08] PictVar
                                    Nil					        ,;  // [09] F3
                                    .T.                         ,;  // [10]
                                    Nil                         ,;  // [11]
                                    ""                          )	// [12]
                SBV->( dbSkip() )
            EndDo 
            
            _cOrder := Soma1(_cOrder,1)
            _oStruct:AddField(  "GRD_DELET"		            ,;	// [01] Id do Field
                                _cOrder				        ,;	// [02] Ordem
                                ""  	                    ,;	// [03] Titulo do campo
                                "" 	                        ,;	// [04] ToolTip do campo
                                                            ,;	// [05] Help
                                "G"					        ,;	// [06] Tipo do campo
                                "@!"                        ,;	// [07] Picture
                                Nil                         ,;	// [08] PictVar
                                ""			                ,;  // [09] F3
                                .T.                         ,;  // [10]
                                Nil                         ,;  // [11]
                                ""                          )	// [12]

        EndIf 
    ElseIf _cAlias == "SKU"
        _oStruct:AddField(  "SKU_COD"		            ,;	// [01] Id do Field
                            "01"				        ,;	// [02] Ordem
                            RTrim(RetTitle("B1_COD"))	,;	// [03] Titulo do campo
                            RTrim(RetTitle("B1_COD")) 	,;	// [04] ToolTip do campo
                            Nil                         ,;	// [05] Help
                            "G"					        ,;	// [06] Tipo do campo
                            "@!"                        ,;	// [07] Picture
                            Nil                         ,;	// [08] PictVar
                            ""					        ,;  // [09] F3
                            .F.                         ,;  // [10]
                            Nil                         ,;  // [11]
                            ""                          )	// [12]
        
        _oStruct:AddField(  "SKU_DESC"	                ,;	// [01] Id do Field
                            "02"				        ,;	// [02] Ordem
                            RTrim(RetTitle("B1_DESC"))  ,;	// [03] Titulo do campo
                            RTrim(RetTitle("B1_DESC"))  ,;	// [04] ToolTip do campo
                            Nil                         ,;	// [05] Help
                            "G"					        ,;	// [06] Tipo do campo
                            "@!"                        ,;	// [07] Picture
                            Nil                         ,;	// [08] PictVar
                            Nil					        ,;  // [09] F3
                            .F.                         ,;  // [10]
                            Nil                         ,;  // [11]
                            ""                          )	// [12]
        
        _oStruct:AddField(  "SKU_CODBAR"	            ,;	// [01] Id do Field
                            "03"				        ,;	// [02] Ordem
                            RTrim(RetTitle("B1_CODBAR")),;	// [03] Titulo do campo
                            RTrim(RetTitle("B1_CODBAR")),;	// [04] ToolTip do campo
                            Nil                         ,;	// [05] Help
                            "G"					        ,;	// [06] Tipo do campo
                            "@!"                        ,;	// [07] Picture
                            Nil                         ,;	// [08] PictVar
                            Nil					        ,;  // [09] F3
                            .T.                         ,;  // [10]
                            Nil                         ,;  // [11]
                            ""                          )	// [12]
        
        _oStruct:AddField(  "SKU_DELET"	                ,;	// [01] Id do Field
                            "04"				        ,;	// [02] Ordem
                            ""                          ,;	// [03] Titulo do campo
                            ""                          ,;	// [04] ToolTip do campo
                            Nil                         ,;	// [05] Help
                            "G"					        ,;	// [06] Tipo do campo
                            "@!"                        ,;	// [07] Picture
                            Nil                         ,;	// [08] PictVar
                            Nil					        ,;  // [09] F3
                            .F.                         ,;  // [10]
                            Nil                         ,;  // [11]
                            ""                          )	// [12]

    EndIf 

EndIf 

Return _oStruct 

/************************************************************************************/
/*/{Protheus.doc} MenuDef
    @description Menu padrao para manutencao do cadastro
    @author Bernard M. Margarido
    @since 21/01/2024
    @version undefined
/*/
/************************************************************************************/
Static Function MenuDef()
Return FWMVCMenu( "ECLOJ005" )
