#INCLUDE "TOTVS.CH"
#INCLUDE "PROTHEUS.CH"
#INCLUDE "FWMVCDEF.CH"

/*************************************************************************************************************/
/*/{Protheus.doc} BSFINA15
    @description Transferencias PagarMe
    @type  Function
    @author Bernard M. Margarido
    @since 15/07/2021
/*/
/*************************************************************************************************************/
User Function BSFINA15()
Private _nOldLen := SetVarNameLen(255) 
Private _oBrowse := Nil 

//------------------------------------+
// Instanciamento da Classe FWMBrowse |
//------------------------------------+
_oBrowse := FWMBrowse():New()
//-----------------+
// Alias utilizado |
//-----------------+
_oBrowse:SetAlias("XTQ")
//-------------------+
// Adiciona Legendas |
//-------------------+
_oBrowse:AddLegend( "XTQ_STATUS == '1'", "GREEN"    , "Criada"      )
_oBrowse:AddLegend( "XTQ_STATUS == '2'", "YELLOW"   , "Solicitado"  )
_oBrowse:AddLegend( "XTQ_STATUS == '3'", "RED"      , "Recebido"    )

//------------------+
// Titulo do Browse |
//------------------+
_oBrowse:SetDescription('Transferencias PagarMe')

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
@since 10/08/2017
@version undefined
@type function
/*/
/************************************************************************************/
Static Function ModelDef()
Local _oModel		:= Nil
Local _oStruXTQ     := Nil

//-----------------+
// Monta Estrutura |
//-----------------+
_oStruXTQ   := FWFormStruct(1,"XTQ")

//-------+
// Model |
//-------+
_oModel 	:= MPFormModel():New('BFINA_15', /*bPreValid*/ , /*_bPosValid*/ , /*_bCommit*/ , /*_bCancel*/ )
_oModel:SetDescription('Transferencias PagarMe')

//-----------------+
// Adiciona campos | 
//-----------------+
_oModel:addFields('XTQ_01',,_oStruXTQ)

//----------------+
// Chave primaria | 
//----------------+
_oModel:SetPrimaryKey({"XTQ_FILIAL","XTQ_CODIGO"})

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
Local _oStrViewXTQ	:= Nil

//-------------------------+
// Carrega Modelo de Dados | 
//-------------------------+
_oModel := FWLoadModel("BSFINA15")

//--------------------------------------+
// Cria a estrutura a ser usada na View |
//--------------------------------------+
_oStrViewXTQ	:= FWFormStruct( 2,'XTQ') 

//---------------------+
// Instancia Interface |
//---------------------+
_oView	:= FWFormView():New()
_oView:SetModel(_oModel)
_oView:SetDescription('Transferencias PagarMe')

//---------------------+
// View das estruturas |
//---------------------+
_oView:AddField('XTQ_FORM' 	, _oStrViewXTQ , 'XTQ_01' )

//------------------------------------------------------------+
// Criar "box" horizontal para receber algum elemento da view |
//------------------------------------------------------------+
_oView:CreateHorizontalBox( 'SUP_01' , 100 ,,, /*'PASTAS'*/, /*'ABA01'*/ )
_oView:SetOwnerView('XTQ_FORM'	    ,'SUP_01')

//------------------------+
// Titulo componente GRID |
//------------------------+
_oView:EnableTitleView('XTQ_FORM','Dados Transferencia')

Return _oView 

/************************************************************************************/
/*/{Protheus.doc} BSFIN15A
    @description Configura dados bancários 
    @type  Function
    @author Bernard M Margarido
    @since 31/10/2023
    @version version
/*/
/************************************************************************************/
User Function BSFIN15A()
Local _aParam	:= {}
Local _aRet		:= {}

Local _cCodBco	:= GetNewPar("PG_BCONUM",CriaVar("A6_COD",.F.))
Local _cAgencia	:= GetNewPar("PG_AGNUM",CriaVar("A6_AGENCIA",.F.))
Local _cConta   := GetNewPar("PG_NCONTA",CriaVar("A6_NUMCON",.F.))
Local _cCondPG  := GetNewPar("PG_CONDPG",CriaVar("E4_CODIGO",.F.))
Local _cNaturez := GetNewPar("PG_NATUREZ",CriaVar("ED_CODIGO",.F.))
Local _cTipo    := GetNewPar("PG_TPFATEC",CriaVar("E1_TIPO",.F.))

//Local _bVldParam:= {|| DnFinA07A() }

aAdd(_aParam,{1, "Banco"        , _cCodBco  , PesqPict("SA6","A6_COD")      , ".T.", "SA6"  , "", TamSx3("A6_COD")[1]       , .T.})
aAdd(_aParam,{1, "Agencia"      , _cAgencia , PesqPict("SA6","A6_AGENCIA")  , ".T.",        , "", TamSx3("A6_AGENCIA")[1]   , .T.})
aAdd(_aParam,{1, "Conta"        , _cConta   , PesqPict("SA6","A6_NUMCON")   , ".T.",        , "", 050                       , .T.})
aAdd(_aParam,{1, "Cond. Pgto"   , _cCondPG  , PesqPict("SE4","E4_CODIGO")   , ".T.", "SE4"  , "", TamSx3("E4_CODIGO")[1]    , .T.})
aAdd(_aParam,{1, "Natureza"     , _cNaturez , PesqPict("SED","ED_CODIGO")   , ".T.", "SED"  , "", 050                       , .T.})
aAdd(_aParam,{1, "Tipo"         , _cTipo    , PesqPict("se1","E1_TIPO")     , ".T.", "05"   , "", TamSx3("E1_TIPO")[1]      , .T.})
   
If ParamBox(_aParam,"Parametros Transferencia PagarMe",@_aRet,/*_bVldParam*/,,,,,,,.T., .T.)

    //--------------------------+
    // SA6 - Cadastro de Bancos |
    //--------------------------+
    dbSelectArea("SA6")
    SA6->( dbSetOrder(1) )
    If !SA6->( dbSeek(xFilial("SA6") + mv_par01 + mv_par02 + mv_par03) )
        _lRet := .F. 
        _cMsg += "Banco/Agencia/Conta não localizada. Favor verificar dados digitados." + CRLF
    EndIf

    //---------------------------+
    // SED - Natureza financeira |
    //---------------------------+
    dbSelectArea("SED")
    SED->( dbSetOrder(1) )
    If !SED->( dbSeek(xFilial(xFilial("SED") + mv_par05)))
        _lRet := .F. 
        _cMsg += "Natureza financeira não localizada. Favor verificar dados digitados." + CRLF
    EndIf

    //-----------------------------+
    // SE4 - Condição de Pagamento | 
    //-----------------------------+
    dbSelectArea("SE4")
    SE4->( dbSetOrder(1) )
    If !SE4->( dbSeek(xFilial("SE4") + mv_par04))
        _lRet := .F. 
        _cMsg += "Condição de Pagamento não localizada. Favor verificar dados digitados." + CRLF
    EndIf

    //----------------------+
    // SX5 - Tipo de Titulo |
    //----------------------+
    dbSelectArea("SX5")
    SX5->( dbSetOrder(1) )
    If !SX5->( dbSeek(xFilial("SX5") + "05" + mv_par06))
        _lRet := .F. 
        _cMsg += "Tipo de Pagamento não localizada. Favor verificar dados digitados." + CRLF
    EndIf

    If Empty(_cMsg)
        //---------------------------+
        // Parametro codigo do banco |
        //---------------------------+
        If !PutMV("PG_BCONUM",mv_par01)
            SX6->( RecLock("SX6",.T.) )
                SX6->X6_FIL		:= xFilial("SX6")
                SX6->X6_VAR		:= "PG_BCONUM"
                SX6->X6_TIPO	:= "C"
                SX6->X6_DESCRIC	:= "Codigo do Banco"
                SX6->X6_CONTEUD	:= mv_par01
                SX6->X6_CONTSPA	:= mv_par01
                SX6->X6_CONTENG	:= mv_par01	
                SX6->X6_PROPRI	:= "U"
                SX6->X6_PYME	:= "S"
            SX6->( MsUnLock() )
        EndIf

        //--------------------------+
        // Parametro codigo agencia |
        //--------------------------+
        If !PutMV("PG_AGNUM",mv_par02)
            SX6->( RecLock("SX6",.T.) )
                SX6->X6_FIL		:= xFilial("SX6")
                SX6->X6_VAR		:= "PG_AGNUM"
                SX6->X6_TIPO	:= "C"
                SX6->X6_DESCRIC	:= "Codigo da Agencia"
                SX6->X6_CONTEUD	:= mv_par02
                SX6->X6_CONTSPA	:= mv_par02
                SX6->X6_CONTENG	:= mv_par02	
                SX6->X6_PROPRI	:= "U"
                SX6->X6_PYME	:= "S"
            SX6->( MsUnLock() )
        EndIf

        //------------------------+
        // Parametro codigo conta |
        //------------------------+
        If !PutMV("PG_NCONTA",mv_par03)
            SX6->( RecLock("SX6",.T.) )
                SX6->X6_FIL		:= xFilial("SX6")
                SX6->X6_VAR		:= "PG_NCONTA"
                SX6->X6_TIPO	:= "C"
                SX6->X6_DESCRIC	:= "Codigo da Conta"
                SX6->X6_CONTEUD	:= mv_par03
                SX6->X6_CONTSPA	:= mv_par03
                SX6->X6_CONTENG	:= mv_par03	
                SX6->X6_PROPRI	:= "U"
                SX6->X6_PYME	:= "S"
            SX6->( MsUnLock() )
        EndIf

        //------------------------------+
        // Parametro condicao pagamento |
        //------------------------------+
        If !PutMV("PG_CONDPG",mv_par04)
            SX6->( RecLock("SX6",.T.) )
                SX6->X6_FIL		:= xFilial("SX6")
                SX6->X6_VAR		:= "PG_CONDPG"
                SX6->X6_TIPO	:= "C"
                SX6->X6_DESCRIC	:= "Codigo da condição de pagamento"
                SX6->X6_CONTEUD	:= mv_par04
                SX6->X6_CONTSPA	:= mv_par04
                SX6->X6_CONTENG	:= mv_par04	
                SX6->X6_PROPRI	:= "U"
                SX6->X6_PYME	:= "S"
            SX6->( MsUnLock() )
        EndIf

        //---------------------------+
        // Parametro codigo natureza |
        //---------------------------+
        If !PutMV("PG_NATUREZ",mv_par05)
            SX6->( RecLock("SX6",.T.) )
                SX6->X6_FIL		:= xFilial("SX6")
                SX6->X6_VAR		:= "PG_NATUREZ"
                SX6->X6_TIPO	:= "C"
                SX6->X6_DESCRIC	:= "Codigo da natureza financeira"
                SX6->X6_CONTEUD	:= mv_par05
                SX6->X6_CONTSPA	:= mv_par05
                SX6->X6_CONTENG	:= mv_par05	
                SX6->X6_PROPRI	:= "U"
                SX6->X6_PYME	:= "S"
            SX6->( MsUnLock() )
        EndIf

        //--------------------------+
        // Parametro tipo do titulo |
        //--------------------------+
        If !PutMV("PG_TPFATEC",mv_par06)
            SX6->( RecLock("SX6",.T.) )
                SX6->X6_FIL		:= xFilial("SX6")
                SX6->X6_VAR		:= "PG_TPFATEC"
                SX6->X6_TIPO	:= "C"
                SX6->X6_DESCRIC	:= "Tipo do titulo financeiro"
                SX6->X6_CONTEUD	:= mv_par06
                SX6->X6_CONTSPA	:= mv_par06
                SX6->X6_CONTENG	:= mv_par06	
                SX6->X6_PROPRI	:= "S"
                SX6->X6_PYME	:= "S"
            SX6->( MsUnLock() )
        EndIf
    Else 
        MsgInfo(_cMsg,"Bunzl - Avisos")
    EndIf

EndIf    
Return Nil 

/************************************************************************************/
/*/{Protheus.doc} MenuDef
	@description Menu padrao para manutencao do cadastro
	@author Bernard M. Margarido
	@since 10/08/2017
	@version undefined
/*/
/************************************************************************************/
Static Function MenuDef()
Local _aRotina := {}

ADD OPTION _aRotina TITLE "Pesquisar"            	ACTION "PesqBrw"            		OPERATION 1 ACCESS 0  
ADD OPTION _aRotina TITLE "Visualizar"           	ACTION "VIEWDEF.BSFINA15" 			OPERATION 2 ACCESS 0 
ADD OPTION _aRotina TITLE "Excluir"              	ACTION "VIEWDEF.BSFINA15" 			OPERATION 5 ACCESS 0 
ADD OPTION _aRotina TITLE "Enviar Transf."         	ACTION "U_BSFINM11"		 			OPERATION 6 ACCESS 0 
ADD OPTION _aRotina TITLE "Banco Transf."         	ACTION "U_BSFIN15A"		 			OPERATION 3 ACCESS 0 

Return _aRotina
