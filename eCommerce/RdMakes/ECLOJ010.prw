#INCLUDE "PROTHEUS.CH"
#INCLUDE "FWMVCDEF.CH"

#DEFINE CRLF CHR(13) + CHR(10)

/************************************************************************************/
/*/{Protheus.doc} ECLOJ010
	@description Gestão de Pedidos e-Commerce
	@author Bernard M. Margarido
	@since 29/04/2019
	@version undefined
	@type function
/*/
/************************************************************************************/
User Function ECLOJ010()

Private oBrowse	:= Nil

//---------------------------------+
// Instanciamento da Classe Browse |
//---------------------------------+
oBrowse := FWMBrowse():New()

//------------------+
// Tabela utilizado |
//------------------+
oBrowse:SetAlias("XTA")

//-------------------+
// Adiciona Legendas |
//-------------------+
	dbSelectArea("ZTC")
	ZTC->( dbGoTop() )
	While ZTC->( !Eof() ) 
		oBrowse:AddLegend( "XTA_CODSTA == '" + ZTC->ZTC_ORDEM + "'", ZTC->ZTC_CORSTA , ZTC->ZTC_DESCV3 )
		ZTC->( dbSkip() )
	EndDo	  

//------------------+
// Titulo do Browse |
//------------------+
oBrowse:SetDescription('Gestão Pedidos eCommerce')

//--------------------+
// Ativação do Browse |
//--------------------+
oBrowse:Activate()

Return Nil

/************************************************************************************/
/*/{Protheus.doc} ECLOJ10A
	@description Visualiza dados do pedido e-Commerce
	@author Bernard M. Margarido
	@since 10/08/2017
	@version undefined
	@type function
/*/
/************************************************************************************/
User Function ECLOJ10A(cAlias,nReg,nOpc)
Local _aArea		:= GetArea()
Local _aCoors       := FWGetDialogSize( oMainWnd )

Local _cTitulo      := "Pedidos - eCommerce"

Local _nOpcA        := 0

Local _oSize        := FWDefSize():New( .T. )
Local _oLayer       := FWLayer():New()
Local _oDlg         := Nil
Local _oPCab        := Nil
Local _oPItem       := Nil
Local _oMsMGet 		:= Nil
Local _oMsMGetEnd	:= Nil
Local _oMsGetDIt   	:= Nil
Local _oMsGetDSt	:= Nil
Local _oMsGetPgt	:= Nil 

Private _oFolder    := Nil

Private _aHeadIt    := {}
Private _aColsIt    := {}
Private _aCab		:= {}
Private _aEnd		:= {}
Private _aHeadSta	:= {}
Private _aColsSta	:= {}
Private _aHeadPgt	:= {}
Private _aColsPgt	:= {}
Private aField 		:= {}

Private aTela[0][0]
Private aGets[0]

//----------------------------------+
// Campos tela de gestão de pedidos |
//----------------------------------+
EcLoj010Cpo()

//-------------------------------------------------------+
// Inicializa as coordenadas de tela conforme resolução  |
//-------------------------------------------------------+
_oSize:AddObject( "DLG", 100, 100, .T., .T.)
_oSize:SetWindowSize(_aCoors)
_oSize:lProp         := .T.
_oSize:lLateral 	:= .T.
_oSize:Process()

//------------------------+
// Cria campos na memória |
//------------------------+
RegToMemory( "XTA", IIF(nOpc == 3,.T.,.F.) )

//------------------------+
// Monta Dialog principal |
//------------------------+
_oDlg := MsDialog():New(_oSize:aWindSize[1], _oSize:aWindSize[2],_oSize:aWindSize[3], _oSize:aWindSize[4],_cTitulo,,,,,,,,,.T.)
  

    //--------------------+
    // Layer da estrutura |
    //--------------------+
    _oLayer:Init( _oDlg, .F. )
    _oLayer:AddLine( "LINE01", 040 )
    _oLayer:AddLine( "LINE02", 055 )

    _oLayer:AddCollumn( "COLLL01"  , 100,, "LINE01" )
    _oLayer:AddCollumn( "COLLL02"  , 100,, "LINE02" )

    _oLayer:AddWindow( "COLLL01" , "WNDCABEC"  , ""     , 100 ,.F. ,,,"LINE01" )
    _oLayer:AddWindow( "COLLL02" , "WNDITEMS"  , ""     , 095 ,.F. ,,,"LINE02" )

    _oPCab  := _oLayer:GetWinPanel( "COLLL01"   , "WNDCABEC"  , "LINE01" )
    _oPItem := _oLayer:GetWinPanel( "COLLL02"   , "WNDITEMS"  , "LINE02" )

	//--------------------+
    // Enchoice Cabeçalho |
    //--------------------+
    _oMsMGet := MsMGet():New("XTA",,2,,,,_aCab,{000,000,000,000},/*aCposAlt*/,,,,,_oPCab,,.F.,.T.)
	_oMsMGet:oBox:Align := CONTROL_ALIGN_ALLCLIENT

	//----------------------------------+
	// Folder Itens/Destinatario/Status |
	//----------------------------------+
	_oFolder := TFolder():New(001,001,{ OemToAnsi("Itens eCommerce"), OemToAnsi("Destinatario"), OemToAnsi("Financeiro"), OemToAnsi("Status Pedido")},{"HEADER"},_oPItem,,,, .T., .F.,000,000)
	_oFolder:Align := CONTROL_ALIGN_ALLCLIENT

	//--------------+
	// Itens Pedido |
	//--------------+
	_oMsGetDIt 	:= MsNewGetDados():New(000,000,000,000,2,/*cLinOk*/,/*cTudoOk1*/,/*cIniCpos*/,/*aAlterGda*/,/*nFreeze*/,/*nMax*/,/*cFieldOk*/,/*cSuperDel*/,/*cDelOk*/,_oFolder:aDialogs[1],_aHeadIt,_aColsIt)
	_oMsGetDIt:oBrowse:Align := CONTROL_ALIGN_ALLCLIENT

	//--------------+
	// Destinatario |
	//--------------+	 
	_oMsMGetEnd := MsMGet():New("XTA",XTA->( Recno() ),2,,,,,{000,000,000,000},,,,,,_oFolder:aDialogs[2],,,,,,.T.,aField)
	_oMsMGetEnd:oBox:Align := CONTROL_ALIGN_ALLCLIENT

	//-----------+
	// Pagamento |
	//-----------+
	_oMsGetPgt 	:= MsNewGetDados():New(000,000,000,000,2,/*cLinOk*/,/*cTudoOk1*/,/*cIniCpos*/,/*aAlterGda*/,/*nFreeze*/,/*nMax*/,/*cFieldOk*/,/*cSuperDel*/,/*cDelOk*/,_oFolder:aDialogs[3],_aHeadPgt,_aColsPgt)
	_oMsGetPgt:oBrowse:Align := CONTROL_ALIGN_ALLCLIENT

	//-----------+
	// Historico |
	//-----------+
	_oMsGetDSt 	:= MsNewGetDados():New(000,000,000,000,2,/*cLinOk*/,/*cTudoOk1*/,/*cIniCpos*/,/*aAlterGda*/,/*nFreeze*/,/*nMax*/,/*cFieldOk*/,/*cSuperDel*/,/*cDelOk*/,_oFolder:aDialogs[4],_aHeadSta,_aColsSta)
	_oMsGetDSt:oBrowse:Align := CONTROL_ALIGN_ALLCLIENT

	//-----------------+
    // Enchoice Botoes |
	//-----------------+
    _oDlg:bInit := {|| EnchoiceBar(_oDlg,{||Iif(Obrigatorio(aGets,aTela), (_nOpcA := 1 ,_oDlg:End()) ,_nOpcA := 0) },{|| _oDlg:End() },.F.)}

_oDlg:Activate(,,,.T.,,,)

RestArea(_aArea)
Return Nil

/***************************************************************************************/
/*/{Protheus.doc} EcLoj010Cpo
	@description Cria campos exibição tela de gestão de pedidos
	@type  Static Function
	@author user
	@since date
	@version version
/*/
/***************************************************************************************/
Static Function EcLoj010Cpo()
Local _aArea	:= GetArea()

Local _nX		:= 0

//------------------+
// Campos cabeçalho |
//------------------+
_aCab	:= {"NOUSER","XTA_NUM","XTA_CLIENT","XTA_LOJA","XTA_NOMCLI",;
			"XTA_EMISSA","XTA_VLRTOT","XTA_NUMECO","XTA_NUMECL",;
			"XTA_DOC","XTA_SERIE","XTA_OBSECO","XTA_MTCANC","XTA_CODSTA",;
			"XTA_DESTAT","XTA_VLBXPV","XTA_IDENDE","XTA_NUMSL1","XTA_NUMSC5","XTA_ENVLOG"}


//---------------------------------+			
// Cria campos folder destinatario |
//---------------------------------+
_aEnd	:= {"XTA_NOMDES","XTA_ENDENT","XTA_ENDNUM","XTA_BAIRRE","XTA_MUNE",;
			"XTA_CEPE","XTA_ESTE","XTA_TPFRET","XTA_FRETE","XTA_SEGURO",;
			"XTA_DESPES","XTA_PLIQUI","XTA_PBRUTO","XTA_VOLUME","XTA_ESPECI",;
			"XTA_TRANSP"}

//---------------------------+
// Array campos destinatario |
//---------------------------+
dbSelectArea("SX3")
SX3->( dbSetOrder(2) )
For _nX := 1 To Len(_aEnd)
	If SX3->( dbSeek(PadR(_aEnd[_nX],10)) )
		aAdd(aField,{SX3->X3_TITULO,SX3->X3_CAMPO,SX3->X3_TIPO,SX3->X3_TAMANHO,SX3->X3_DECIMAL,SX3->X3_PICTURE,,.F.,1,,,,.F.,.F.,Iif(__Language=="SPANISH",SX3->X3_CBOXSPA,Iif(__Language=="ENGLISH",SX3->X3_CBOXENG,SX3->X3_CBOX)),,.F.,,})
	Endif	
Next _nX	

//-----------------+
// Itens do Pedido |
//-----------------+
_aHeadIt    := {}
_aColsIt    := {}

dbSelectArea("SX3")
SX3->( dbSetOrder(1) )
If SX3->( dbSeek("XTB") )
	While SX3->( !Eof() .And. SX3->X3_ARQUIVO == "XTB" )
		If X3Uso(SX3->X3_USADO) //.And. aScan(aCpoGDa, {|x| Upper(AllTrim(x)) == Upper(Alltrim(SX3->X3_CAMPO))}) > 0
				aAdd(_aHeadIt,{	AllTrim(X3Titulo())	,;
								SX3->X3_CAMPO		,;
								SX3->X3_PICTURE		,;
								SX3->X3_TAMANHO		,;
								SX3->X3_DECIMAL		,;
								SX3->X3_VALID		,;
								SX3->X3_USADO		,;
								SX3->X3_TIPO		,;
								SX3->X3_F3			,;
								SX3->X3_CONTEXT		})
			EndIf
		SX3->( dbSkip() )
	EndDo
EndIf

dbSelectArea("XTB")
XTB->( dbSetOrder(1) )
If XTB->(dbSeek(xFilial("XTB") + XTA->XTA_NUM) )
	While XTB->( !Eof() .And. xFilial("XTB") + XTA->XTA_NUM == XTB->XTB_FILIAL + XTB->XTB_NUM )
		aAdd(_aColsIt,Array(Len(_aHeadIt)+1)) 
		For _nX:= 1 To Len(_aHeadIt)
			_aColsIt[Len(_aColsIt)][_nX] := FieldGet(FieldPos(_aHeadIt[_nX][2]))
		Next _nX
		_aColsIt[Len(_aColsIt)][Len(_aHeadIt)+1]:= .F.
		XTB->( dbSkip() )
	EndDo
EndIf

If Len(_aColsIt) <= 0
	aAdd(_aColsIt,Array(Len(_aHeadIt)+1)) 
	For _nX:= 1 To Len(_aHeadIt)
		_aColsIt[1][_nX]:= CriaVar(_aHeadIt[_nX][2],.T.)
	Next _nX
	_aColsIt[1][Len(_aHeadIt)+1]:= .F.
EndIf

//-----------+
// Pagamento |
//-----------+
_aHeadPgt	:= {}
_aColsPgt	:= {}

aAdd(_aHeadPgt,{"Forma"			,"PGTFORMA"		,"@!"							,TamSx3("XTC_FORMA")[1]		,0,".F.","û","C",""," ","" } )
aAdd(_aHeadPgt,{"DT Emissao"	,"PGTEMISS"		,"@D"							,TamSx3("XTC_DATA")[1]		,0,".F.","û","D",""," ","" } )
aAdd(_aHeadPgt,{"DT Vencto"		,"PGTDATA"		,"@D"							,TamSx3("XTC_DATA")[1]		,0,".F.","û","D",""," ","" } )
aAdd(_aHeadPgt,{"Valor "		,"PGTVALOR"		,PesqPict("XTC","XTC_VALOR")	,TamSx3("XTC_VALOR")[1]		,TamSx3("XTC_VALOR")[2],".F.","û","N",""," ","" } )

dbSelectArea("XTC")
XTC->( dbSetOrder(1) )
If XTC->( dbSeek(xFilial("XTC") + XTA->XTA_NUM) )
	While XTC->( !Eof() .And. xFilial("XTC") + XTA->XTA_NUM == XTC->XTC_FILIAL + XTC->XTC_NUM)
		aAdd(_aColsPgt, Array(Len(_aHeadPgt) + 1))
		
		_aColsPgt[Len(_aColsPgt)][1] := XTC->XTC_FORMA
		_aColsPgt[Len(_aColsPgt)][2] := XTA->XTA_EMISSA
		_aColsPgt[Len(_aColsPgt)][3] := XTC->XTC_DATA
		_aColsPgt[Len(_aColsPgt)][4] := XTC->XTC_VALOR

		_aColsPgt[Len(_aColsPgt)][Len(_aHeadPgt) + 1]:= .F.
		
		XTC->( dbSkip() )
	EndDo 
EndIf 

If Len(_aColsPgt) == 0
	aAdd(_aColsPgt, Array(Len(_aHeadPgt) + 1))
		
	_aColsPgt[Len(_aColsPgt)][1] := CriaVar("XTC_FORMA",.F.)
	_aColsPgt[Len(_aColsPgt)][2] := CriaVar("XTA_EMISSA",.F.)
	_aColsPgt[Len(_aColsPgt)][3] := CriaVar("XTC_DATA",.F.)
	_aColsPgt[Len(_aColsPgt)][4] := CriaVar("XTC_VALOR",.F.)

	_aColsPgt[Len(_aColsPgt)][Len(_aHeadPgt) + 1]:= .F.
EndIf 

//------------------+
// Status do Pedido | 
//------------------+
_aHeadSta	:= {}
_aColsSta	:= {}

aAdd(_aHeadSta,{" "				,"XTMLEGEND"	,"@BMP"					,10							,0,""   ,"" ,"C",""," ","" } )
aAdd(_aHeadSta,{"Status"		,"XTMSTATUS"	,"@!"					,TamSx3("XTM_CODSTA")[1]	,0,".F.","û","C",""," ","" } )
aAdd(_aHeadSta,{"Descricao"		,"XTMDESCRI"	,"@!"					,TamSx3("ZTC_DESCV3")[1]	,0,".F.","û","C",""," ","" } )
aAdd(_aHeadSta,{"Data"			,"XTMDATA"		,"@D"					,TamSx3("XTM_DATA")[1]		,0,".F.","û","D",""," ","" } )
aAdd(_aHeadSta,{"Hora "			,"XTMHORA"		,""						,TamSx3("XTM_HORA")[1]		,0,".F.","û","C",""," ","" } )
//aAdd(_aHeadSta,{"Observacao"	,"XTMOBS"		,""						,TamSx3("XTM_OBS")[1]		,0,".F.","û","M",""," ","" } )

dbSelectArea("ZTC")
ZTC->( dbSetOrder(1) )

dbSelectArea("XTM")
XTM->( dbSetOrder(1) )
If XTM->( dbSeek(xFilial("XTM") + XTA->XTA_NUMECO ) )
	While XTM->( !Eof() .And. xFilial("XTM") + XTA->XTA_NUMECO == XTM->XTM_FILIAL + XTM->XTM_IDECOM)

		ZTC->( dbSeek(xFilial("ZTC") + XTM->XTM_CODSTA ) )

		aAdd(_aColsSta, Array(Len(_aHeadSta) + 1))
		_aColsSta[Len(_aColsSta)][1] := ZTC->ZTC_CORSTA
		_aColsSta[Len(_aColsSta)][2] := XTM->XTM_CODSTA
		_aColsSta[Len(_aColsSta)][3] := ZTC->ZTC_DESCV3
		_aColsSta[Len(_aColsSta)][4] := XTM->XTM_DATA
		_aColsSta[Len(_aColsSta)][5] := XTM->XTM_HORA
		//_aColsSta[Len(_aColsSta)][6] := XTM->XTM_OBS

		_aColsSta[Len(_aColsSta)][Len(_aHeadSta) + 1]:= .F.

		XTM->( dbSkip() )
	EndDo
EndIf

If Len(_aColsSta) == 0
	aAdd(_aColsSta, Array(Len(_aHeadSta) + 1))

	_aColsSta[Len(_aColsSta)][1] := CriaVar("ZTC_CORSTA",.F.)
	_aColsSta[Len(_aColsSta)][2] := CriaVar("XTM_CODSTA",.F.)
	_aColsSta[Len(_aColsSta)][3] := CriaVar("ZTC_DESCV3",.F.)
	_aColsSta[Len(_aColsSta)][4] := CriaVar("XTM_DATA",.F.)
	_aColsSta[Len(_aColsSta)][5] := CriaVar("XTM_HORA",.F.)
	_aColsSta[Len(_aColsSta)][6] := CriaVar("XTM_OBS",.F.)
	
	_aColsSta[Len(_aColsSta)][Len(_aHeadSta) + 1]:= .F.

EndIf

RestArea(_aArea)
Return Nil

/************************************************************************************/
/*/{Protheus.doc} ECLOJ101
	@description Realiza a liberação de pedido 
	@author Bernard M. Margarido
	@since 10/08/2017
	@version undefined
	@type function
/*/
/************************************************************************************/
User Function ECLOJ101()
Local _aArea	:= GetArea()

	FWMsgRun(, {|| U_ECLOJ012() }, "Aguarde....", "Processando pedidos e-Commerce." )

RestArea(_aArea)
Return .T.

/************************************************************************************/
/*/{Protheus.doc} ECLOJ102
	@description Realiza faturamento de pedido 
	@author Bernard M. Margarido
	@since 10/08/2017
	@version undefined
	@type function
/*/
/************************************************************************************/
User Function ECLOJ102()
Local _aArea	:= GetArea()

	U_EcLojM05()

RestArea(_aArea)
Return .T.

/************************************************************************************/
/*/{Protheus.doc} ECLOJ103
	@description Realiza transmissão do sefaz
	@author Bernard M. Margarido
	@since 10/08/2017
	@version undefined
	@type function
/*/
/************************************************************************************/
User Function ECLOJ103()
Local _aArea	:= GetArea()

	SPEDNFe()

RestArea(_aArea)
Return .T.

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
Local aRotina 		:= {}
Local aRotFat 		:= {}
//Local aRotTro 		:= {}
//Local aRotIbx 		:= {}

//Local _bIbxPvEnv	:= {|| (U_IBFATM01(),U_IBFATM02())}	

//-----------------------+
// Rotina de Faturamento | 
//-----------------------+
aAdd(aRotFat, {"Libera Pedido"	,"U_ECLOJ101", 0, 4} )	// Libera Pedido
aAdd(aRotFat, {"Prep. Documento","U_ECLOJ102", 0, 4} )	// Prepara Documento
aAdd(aRotFat, {"Trans. Sefaz"	,"U_ECLOJ103", 0, 4} )	// Transmissão Sefaz

//----------------------+
// Troca / Cancelamento |
//----------------------+
//aAdd(aRotTro, {"Cancela Pedido" ,"U_ECLOJ104", 0, 4 } )	// Cancela Pedido
//aAdd(aRotTro, {"Troca/Devolucao","U_ECLOJ105", 0, 4 } )	// Troca / Devolução

//-------------------+
// Pedidos para IBEX |
//-------------------+
//aAdd(aRotIbx, {"Envia PV. IBEX"		,_bIbxPvEnv		, 0, 4 } )	// Envia Pedidos Ibex Logistica
//aAdd(aRotIbx, {"Processa Separacao"	,"U_IBFATM03"	, 0, 4 } )	// Processa separação dos pedidos
//aAdd(aRotIbx, {"Envia NF. IBEX"		,"U_ECLOJM06"	, 0, 4 } )	// Envia Notas Ibex Logistica

aAdd(aRotina, {"Pesquisa"   	, "AxPesqui"    , 0, 1 })  // Pesquisa
aAdd(aRotina, {"Visualizar" 	, "U_ECLOJ10A"  , 0, 2 })  // Visualizar
aAdd(aRotina, {"Faturamento"	, aRotFat		, 0, 4 })  // Faturamento
//aAdd(aRotina, {"Rastreio DLog"	, "U_DLOGA02"	, 0, 4 })  // Faturamento
//aAdd(aRotina, {"Canc / Troca"   , aRotTro  		, 0, 4 })  // Cancelamento / Troca Devolução
//aAdd(aRotina, {"Env. IBEX"   	, aRotIbx  		, 0, 4 })  // Envia Pedidos Ibex Logistica
Return aRotina
