#INCLUDE "PROTHEUS.CH"
#INCLUDE "TOPCONN.CH"

#DEFINE CRLF CHR(13) + CHR(10)

Static cDirImp	:= "/ecommerce/"

/**************************************************************************/
/*/{Protheus.doc} ECLOJ012
    @description Processa as vendas ecommerce 
    @type  Function
    @author Bernard M. Margarido
    @since 28/05/2019
/*/
/**************************************************************************/
User Function ECLOJ012()
Local _aArea    := GetArea()

Local _cAlias   := ""
Local _cCodSta  := GetNewPAr("EC_STAPROC","020")
Local _cFilAux  := cFilAnt 

Local _lContinua:= .T.

Private cArqLog	:= ""	

Private _lJob   := IIF(Isincallstack("U_ECLOJM04"),.T.,.F.)

//------------------------------+
// Inicializa Log de Integracao |
//------------------------------+
MakeDir(cDirImp)
cArqLog := cDirImp + "ECLOJ012" + cEmpAnt + cFilAnt + ".LOG"
ConOut("")	
LogExec(Replicate("-",80))
LogExec("INICIA PROCESSAMENTO DE PEDIDO ECOMMERCE - DATA/HORA: " + DTOC(DATE()) + " AS " + TIME())


//--------------------------+
// Consulta pedidos na fila |
//--------------------------+
If !EcLoj012Qry(@_cAlias)
    LogExec("NAO EXISTEM DADOS PARA SEREM PROCESSADOS.")
    (_cAlias)->( dbCloseArea() )
    _lContinua := .F.
EndIf

//------------------------------+
// Processa as vendas eCommerce |
//------------------------------+
If _lContinua

    //---------------------------------------+
    // Seleciona tabela de pedidos eCommerce |
    //---------------------------------------+
    dbSelectArea("XTA")
    XTA->( dbSetOrder(1) )

    //--------------------------------------+
    // Inicia processo de pedidos eCommerce |
    //--------------------------------------+
    dbSelectArea(_cAlias)
    (_cAlias)->( dbGoTop() )

    While (_cAlias)->( !Eof() )
        //--------------------+
        // Posiciona registro |
        //--------------------+
        XTA->( dbGoTo((_cAlias)->RECNOXTA) )
        
        //--------------------------+
        // Posiciona filial correta |
        //--------------------------+
        If cFilAnt <> XTA->XTA_FILIAL
            cFilAnt := XTA->XTA_FILIAL
        EndIf 

        //-----------------------------+
        // Pagamento pendente/aprovado |
        //-----------------------------+
        If XTA->XTA_CODSTA $ _cCodSta .And. Empty(XTA->XTA_NUMSUA)
            LogExec("==> INICIO ORCAMENTO ECOMMERCE " + XTA->XTA_NUM + "DATA/HORA: " + dToc( Date() ) + " AS " + Time() )
                Begin Transaction 
                    EcLoj012Orc()
                End Transaction    
            LogExec("==> FIM ORCAMENTO ECOMMERCE " + XTA->XTA_NUM + "DATA/HORA: " + dToc( Date() ) + " AS " + Time() )
        //-----------+
        // Cancelado |
        //-----------+
        ElseIf XTA->XTA_CODSTA $ "008"
            LogExec("==> INICIO CANCELAMENTO ORCAMENTO ECOMMERCE " + XTA->XTA_NUM + "DATA/HORA: " + dToc( Date() ) + " AS " + Time() )
                EcLoj012Can()
            LogExec("==> FIM CANCELAMENTO ORCAMENTO ECOMMERCE " + XTA->XTA_NUM + "DATA/HORA: " + dToc( Date() ) + " AS " + Time() )
        EndIf

        //-------------------------+
        // Restaura filial correta |
        //-------------------------+
        If cFilAnt <> _cFilAux
            cFilAnt := _cFilAux
        EndIf 

        (_cAlias)->( dbSkip() )
    EndDo

EndIf

LogExec("FINALIZA PROCESSAMENTO DE PEDIDO ECOMMERCE - DATA/HORA: "+ DTOC(DATE()) + " AS " + TIME())
LogExec(Replicate("-",80))
ConOut("")


RestArea(_aArea)
Return Nil

/**************************************************************************/
/*/{Protheus.doc} EcLoj012Orc
    @description Realiza a gravação / atualização do pedido e-Commerce
    @type  Static Function
    @author Bernard M. Margarido
    @since 28/05/2019
    @version version
/*/
/**************************************************************************/
Static Function EcLoj012Orc()
Local _aArea        := GetArea()

Local _aCabec       := {}
Local _aItem        := {}
Local _aItems       := {}
Local _aRefImpos	:= {} 

Local _cNumLJ       := ""
Local _cOperador    := "000499"
Local _cOper        := "1"
Local _cTMK         := "2"
Local _cTpCarga     := "2"
Local _cTipo        := "VNO"

Local _cCodCont     := ""
Local _cDescCont    := ""
Local _cFormPG      := ""
Local _cFirstCC     := ""
Local _cLastCC      := ""
Local _cNSUTef      := ""
Local _cTID         := ""
Local _cBandeira    := ""
Local  _cPayID      := ""
Local _cAuthID      := ""

Local _nValDesc     := 0
Local _nItem        := 0
Local _nDecIt	    := TamSx3("UB_VLRITE")[2]

Private lMsErroAuto := .F.
Private lAutomatoX  := .T.

//----------------+
// Valida cliente |
//----------------+
dbSelectArea("SA1")
SA1->( dbSetOrder(1) )
If !SA1->( dbSeek(xFilial("SA1") + XTA->XTA_CLIENT + XTA->XTA_LOJA) )
    If !_lJob
        Help("  ",1,"ORC:" + XTA->XTA_NUM,,"CLIENTE " + XTA->XTA_CLIENT + "/" + XTA->XTA_LOJA  + " NÃO LOCALIZADO",3,1)
    EndIf
    LogExec("CLIENTE " + XTA->XTA_CLIENT + "/" + XTA->XTA_LOJA  + " NÃO LOCALIZADO")
    RestArea(_aArea)
    Return Nil
Endif

//----------------------------------+
// Valida se cliente está bloqueado |
//----------------------------------+
If SA1->A1_MSBLQL == "1"
    If !_lJob
        Help("  ",1,"ORC:" + XTA->XTA_NUM,,"CLIENTE " + RTrim(SA1->A1_NOME)  + " BLOQUEADO PARA USO.",3,1)
    EndIf
    LogExec("CLIENTE " + RTrim(SA1->A1_NOME)  + " BLOQUEADO PARA USO.")
    RestArea(_aArea)
    Return Nil
EndIf

//-----------------------------------+
// Valida itens do pedido e-Commerce |
//-----------------------------------+
dbSelectArea("XTB")
XTB->( dbSetOrder(1) )
If !XTB->( dbSeek(xFilial("XTB") + XTA->XTA_NUM) )
    If !_lJob
        Help("  ",1,"ORC:" + XTA->XTA_NUM,,"ITENS DO PEDIDO ECOMMERCE " + XTA->XTA_NUMECO + " NAO LOCALIZADO.",3,1)
    EndIf
    LogExec("ITENS DO PEDIDO ECOMMERCE " + XTA->XTA_NUMECO + " NAO LOCALIZADO.")
    RestArea(_aArea)
    Return Nil
EndIf

//----------------------------+
// Consulta codigo do contato |
//----------------------------+
EcLoj012A(XTA->XTA_IDENDE,@_cCodCont,@_cDescCont)

//------------------------+
// Inicia Funções Fiscais |
//------------------------+
MaFisEnd()
MaFisIni(SA1->A1_COD,SA1->A1_LOJA,"C","N",SA1->A1_TIPO,_aRefImpos,Nil,Nil,Nil,"MATA461",Nil,Nil,Nil,Nil,Nil,Nil)

//----------------------------------+
// Numero do Orçamento no Siga Loja |
//----------------------------------+
_cNumLJ := GetSxENum("SUA","UA_NUM")

dbSelectArea("SUA")
SUA->( dbSetOrder(1) )
While SUA->( dbSeek(xFilial("SUA") + _cNumLJ) )
    ConfirmSx8()
    _cNumLJ := GetSxeNum("SUA","UA_NUM","",1)
EndDo

//--------------------------------+
// Posiciona tabela de preço loja |
//--------------------------------+
dbSelectArea("SB0")
SB0->( dbSetOrder(1) )

While XTB->( !Eof() .And. xFilial("XTB") + XTA->XTA_NUM == XTB->XTB_FILIAL + XTB->XTB_NUM )
        
    //------------------+
    // Cria array itens | 
    //------------------+
    _aItem      := {}
    _nValDesc   := 0 //Max((XTB->XTB_PRCTAB -  XTB->XTB_VRUNIT  ) * XTB->XTB_QUANT ,0)
    _nItem++
    //----------------------------------------+
    // Adiciona Produto para calculos fiscais |
    //----------------------------------------+
    MaFisAdd(   XTB->XTB_PRODUT,;
                XTB->XTB_TES,;
                XTB->XTB_QUANT,;
                XTB->XTB_VRUNIT,;
                XTB->XTB_VALDES,;
                "",;
                "",;
                0,;
                0,;
                0,;
                0,;
                0,;
                Round(XTB->XTB_QUANT * XTB->XTB_VRUNIT,_nDecIt),;
                0,;				   					
                SB1->( Recno() ),;                  
                SF4->( Recno() ),;                  
                XTB->XTB_ITEM)

    aAdd( _aItem, {"UB_FILIAL"	, XTB->XTB_FILIAL										, Nil })
    aAdd( _aItem, {"UB_NUM"	    , _cNumLJ       										, Nil })
    aAdd( _aItem, {"UB_PRODUTO"	, XTB->XTB_PRODUT										, Nil })
	aAdd( _aItem, {"UB_ITEM"	, XTB->XTB_ITEM  									    , Nil })
	aAdd( _aItem, {"UB_QUANT"	, XTB->XTB_QUANT 									    , Nil })
	aAdd( _aItem, {"UB_VRUNIT"	, XTB->XTB_VRUNIT										, Nil })
	aAdd( _aItem, {"UB_UM"		, XTB->XTB_UM    										, Nil })
    aAdd( _aItem, {"UB_DESC"	, XTB->XTB_DESC     								    , Nil })
	aAdd( _aItem, {"UB_VALDESC"	, XTB->XTB_VALDES      									, Nil })
    aAdd( _aItem, {"UB_TES"	    , XTB->XTB_TES	    									, Nil })
	aAdd( _aItem, {"UB_LOCAL"   , XTB->XTB_LOCAL       			    				    , Nil })
    aAdd( _aItem, {"UB_PRCTAB"	, XTB->XTB_VRUNIT										, Nil })
    aAdd( _aItem, {"UB_DTENTRE"	, XTB->XTB_FDTENT										, Nil })
    aAdd( _aItem, {"UB_XBASIPI"	, MaFisRet(_nItem,"IT_BASEIPI")   						, Nil })
    aAdd( _aItem, {"UB_XIPI"	, MaFisRet(_nItem,"IT_VALIPI")	    					, Nil })   
    aAdd( _aItem, {"UB_XVALIPI"	, MaFisRet(_nItem,"IT_VALIPI")  						, Nil })
    aAdd( _aItem, {"UB_BASEICM"	, MaFisRet(_nItem,"IT_BASEICM")							, Nil })
    aAdd( _aItem, {"UB_XALQICM"	, MaFisRet(_nItem,"IT_ALIQICM")		    				, Nil })
    aAdd( _aItem, {"UB_XVALICM"	, MaFisRet(_nItem,"IT_VALICM")  						, Nil })
    aAdd( _aItem, {"UB_XVALPIS"	, MaFisRet(_nItem,"IT_VALPS2")	    					, Nil })
    aAdd( _aItem, {"UB_XVALCOF"	, MaFisRet(_nItem,"IT_VALCF2")  	    				, Nil })
    aAdd( _aItem, {"UB_XBASPCC"	, MaFisRet(_nItem,"IT_BASECF2")   						, Nil })
    aAdd( _aItem, {"UB_XALQPS2"	, MaFisRet(_nItem,"IT_ALIQPS2")   						, Nil })
    aAdd( _aItem, {"UB_XALQCF2"	, MaFisRet(_nItem,"IT_ALIQCF2")							, Nil })
    aAdd( _aItem, {"UB_XVALCMP"	, MaFisRet(_nItem,"IT_VALCMP")	    					, Nil })
    aAdd( _aItem, {"UB_XDIFAL"	, MaFisRet(_nItem,"IT_DIFAL")		    				, Nil }) 
    aAdd( _aItem, {"UB_XVFCPDI"	, MaFisRet(_nItem,'IT_VFCPDIF')		    				, Nil })
    aAdd( _aItem, {"UB_XBASDES"	, MaFisRet(_nItem,'IT_BASEDES')							, Nil })
    aAdd( _aItem, {"UB_XALQCMP"	, MaFisRet(_nItem,"IT_ALIQCMP")						    , Nil })
    aAdd( _aItem, {"UB_XALFCMP"	, MaFisRet(_nItem,"IT_ALFCCMP")							, Nil })
    aAdd( _aItem, {"UB_XREDICM"	, MaFisRet(_nItem,"IT_PREDST")							, Nil })
    aAdd( _aItem, {"UB_XBASSOL"	, MaFisRet(_nItem,"IT_BASESOL")						    , Nil })
    aAdd( _aItem, {"UB_XVALSOL"	, MaFisRet(_nItem,"IT_VALSOL")							, Nil })
    
    aAdd(_aItems,_aItem)

    XTB->( dbSkip() )
EndDo

//------------+
// Pagamentos | 
//------------+
dbSelectArea("XTC")
XTC->( dbSetOrder(1) )
If !XTC->( dbSeeK(xFilial("XTC") + XTA->XTA_NUM) )
    If !_lJob
        Help("  ",1,"ORC:" + XTA->XTA_NUM,,"PAGAMENTOS DO PEDIDO ECOMMERCE " + XTA->XTA_NUMECO + " NAO LOCALIZADO.",3,1)
    EndIf
    LogExec("PAGAMENTOS DO PEDIDO ECOMMERCE " + XTA->XTA_NUMECO + " NAO LOCALIZADO.")
    RestArea(_aArea)
    Return Nil
Endif

//-----------------------+
// Parametros pagamentos |
//-----------------------+
_cFormPG    := XTC->XTC_FORMA
_cFirstCC   := Left(XTC->XTC_NUMCAR,6)
_cLastCC    := Right(XTC->XTC_NUMCAR,4)
_cTID       := XTC->XTC_TID
_cBandeira  := RTrim(XTC->XTC_OBS)
_cPayID     := RTrim(XTC->XTC_PAYID)
_cAuthID    := RTrim(XTC->XTC_AUTHID)
_cNSUTef    := RTrim(XTC->XTC_NSU)

//-----------+
// Cabeçalho |
//-----------+
aAdd( _aCabec,	{"UA_FILIAL"    , xFilial("SUA")                                        , Nil }) 
aAdd( _aCabec,	{"UA_NUM"       , _cNumLJ                                               , Nil })
aAdd( _aCabec,	{"UA_CLIENTE"	, SA1->A1_COD									        , Nil })
aAdd( _aCabec,	{"UA_LOJA"	    , SA1->A1_LOJA									        , Nil })
aAdd( _aCabec,	{"UA_CLIENT"	, SA1->A1_COD									        , Nil }) 
aAdd( _aCabec,	{"UA_LOJAENT"	, SA1->A1_LOJA									        , Nil })
aAdd( _aCabec,	{"UA_CODCONT"   , _cCodCont 									        , Nil })
aAdd( _aCabec,	{"UA_DESCNT"    , _cDescCont 									        , Nil }) 
aAdd( _aCabec,	{"UA_OPERADO"   , _cOperador 									        , Nil })
aAdd( _aCabec,	{"UA_CONDPG"	, XTA->XTA_CONDPG								        , "AllwaysTrue()" })
aAdd( _aCabec,	{"UA_TIPOCLI"	, SA1->A1_TIPO  								        , Nil })
aAdd( _aCabec,	{"UA_OPER"		, _cOper										        , Nil })
aAdd( _aCabec,	{"UA_EMISSAO"	, XTA->XTA_EMISSA 								        , Nil })
aAdd( _aCabec,	{"UA_TMK"	    , _cTMK            								        , Nil })
aAdd( _aCabec,	{"UA_FORMPG"	, _cFormPG      								        , Nil })
aAdd( _aCabec,	{"UA_TPFRETE"	, IIF(XTA->XTA_FRETE > 0 ,"C","F")  			        , Nil })
aAdd( _aCabec,	{"UA_FRETE"		, XTA->XTA_FRETE    							        , Nil })
aAdd( _aCabec,	{"UA_ENDCOB"	, SA1->A1_ENDCOB   								        , Nil })
aAdd( _aCabec,	{"UA_BAIRROC"	, SA1->A1_BAIRROC  								        , Nil })
aAdd( _aCabec,	{"UA_MUNC"	    , SA1->A1_MUNC     								        , Nil })
aAdd( _aCabec,	{"UA_ESTC"	    , SA1->A1_ESTC    								        , Nil })
aAdd( _aCabec,	{"UA_CEPC"	    , SA1->A1_CEPC  								        , Nil })
aAdd( _aCabec,	{"UA_ENDENT"	, XTA->XTA_ENDENT  								        , Nil })
aAdd( _aCabec,	{"UA_BAIRROE"	, XTA->XTA_BAIRRE  								        , Nil })
aAdd( _aCabec,	{"UA_MUNE"  	, XTA->XTA_MUNE 								        , Nil })
aAdd( _aCabec,	{"UA_ESTE"	    , XTA->XTA_ESTE   								        , Nil })
aAdd( _aCabec,	{"UA_CEPE"	    , XTA->XTA_CEPE 								        , Nil })
aAdd( _aCabec,	{"UA_TRANSP"	, XTA->XTA_TRANSP								        , Nil })
aAdd( _aCabec,	{"UA_TPCARGA"	, _cTpCarga        								        , Nil })
aAdd( _aCabec,	{"UA_XOBSCOM"	, XTA->XTA_OBSECO							            , Nil })
aAdd( _aCabec,	{"UA_XOBSCOM"	, XTA->XTA_OBSECO							            , Nil })
aAdd( _aCabec,	{"UA_XSTATUS"	, "AGINT"      								            , Nil })
aAdd( _aCabec,	{"UA_XHORA"	    , Time()       								            , Nil })
aAdd( _aCabec,	{"UA_XDATA" 	, Date()       								            , Nil })
aAdd( _aCabec,	{"UA_XIDVTV3"	, XTA->XTA_NUMECO								        , Nil })
aAdd( _aCabec,	{"UA_XIDVTEX"	, Val(XTA->XTA_NUMECL)							        , Nil })
aAdd( _aCabec,	{"UA_XFIRSTC"	, _cFirstCC    								            , Nil })
aAdd( _aCabec,	{"UA_XLASTC "	, _cLastCC   								            , Nil })
aAdd( _aCabec,	{"UA_XTRSCT1"	, _cPayID    								            , Nil })
aAdd( _aCabec,	{"UA_XTRSCT3"	, _cTID      								            , Nil })
aAdd( _aCabec,	{"UA_XAUTHID"	, _cAuthID     								            , Nil })
aAdd( _aCabec,	{"UA_XNSU"	    , _cNSUTef     								            , Nil })
aAdd( _aCabec,	{"UA_XPAYNAM"	, _cBandeira   								            , Nil })
aAdd( _aCabec,	{"UA_XPAYSYS"	, _cBandeira    							            , Nil })
aAdd( _aCabec,	{"UA_XPEDLV"	, XTA->XTA_NUMECO   						            , Nil })
aAdd( _aCabec,	{"UA_XSEQLV"	, Val(XTA->XTA_NUMECL)						            , Nil })
aAdd( _aCabec,	{"UA_XDTA"	    , "200"         							            , Nil })
aAdd( _aCabec,	{"UA_XDTAM"	    , "A"            							            , Nil })
aAdd( _aCabec,  {"UA_DESPESA"	, XTA->XTA_DESPES					                    , Nil })
aAdd( _aCabec,  {"UA_XTIPO"	    , _cTipo					                            , Nil })
aAdd( _aCabec,  {"UA_VALICM"	, MaFisRet(,"NF_VALICM")	                            , Nil }) 
aAdd( _aCabec,  {"UA_VALISS"	, MaFisRet(,"NF_VALISS")	                            , Nil }) 
aAdd( _aCabec,  {"UA_VALIPI"	, MaFisRet(,"NF_VALIPI")	                            , Nil }) 
aAdd( _aCabec,  {"UA_XVALCMP"	, MaFisRet(,"NF_VALCMP")	                            , Nil })
aAdd( _aCabec,  {"UA_XDIFAL"	, MaFisRet(,"NF_DIFAL")		                            , Nil }) 
aAdd( _aCabec,  {"UA_XVFCPDI"	, MaFisRet(,"NF_VFCPDIF")	                            , Nil })
aAdd( _aCabec,  {"UA_XBASSOL"	, MaFisRet(,"NF_BASESOL" )	                            , Nil })
aAdd( _aCabec,  {"UA_XVALSOL"	, MaFisRet(,"NF_VALSOL" )	                            , Nil })
aAdd( _aCabec,  {"UA_VALMERC"	, MaFisRet(,"NF_VALMERC")	                            , Nil })
aAdd( _aCabec,  {"UA_VLRLIQ"	, MaFisRet(,"NF_VALMERC") + XTA->XTA_FRETE              , Nil })
aAdd( _aCabec,	{"UA_VEND"		, XTA->XTA_VEND				                            , Nil })
aAdd( _aCabec,	{"UA_INICIO"	, Time()    				                            , Nil })
aAdd( _aCabec,	{"UA_FIM"		, Time()    				                            , Nil })
aAdd( _aCabec,	{"UA_STATUS"	, "   "    				                                , "AllwaysTrue()" })

//UA_DIASDAT 
//UA_HORADAT
//UA_VALBRUT
//UA_DTLIM 
//UA_PARCELA
//UA_MOEDA
//UA_XPEDMIN
//UA_XTIPOPV 
//UA_XCUSTO
//UA_XVLRLIQ
//UA_XDEPVEN


//------------------------+
// Processa ExecAuto Loja |
//------------------------+
If Len(_aCabec) > 0 .And. Len(_aItems) > 0 
    LogExec("INICIO EXECAUTO DATA " + dToc( Date()) + " HORA " + Time() )

    nModulo     := 13
    lMsErroAuto := .F.
    
    //-----------------------------+
    // Ajusta Array com dicionario |
    //-----------------------------+    
    _aCabec     := FWVetByDic( _aCabec, "SUA" )
    _aItems     := FWVetByDic( _aItems, "SUB", .T. )

    MSExecAuto({|x,y,z,w| Tmka271(x,y,z,w)},_aCabec,_aItems,3,"2")

    If lMsErroAuto
        If _lJob
            cSL1Log	:= "SUA" + XTA->XTA_NUM + DToS(dDataBase) + Left(Time(),2) + SubStr(Time(),4,2) + Right(Time(),2)+".LOG"

            //----------------+
            // Cria diretorio |
            //----------------+ 
            MakeDir("/erros/")
            MostraErro("/erros/",cSL1Log)
            
        Else
            MostraErro()
        EndIf  

        //----------------------+
        // Restaura a numeração |
        //----------------------+  
        RollBackSX8()
    Else

        //----------------------+
        // Confirma a numeração |
        //----------------------+
        ConfirmSx8()

        //----------------------------------+
        // Atualiza dados Orçamento inicial |
        //----------------------------------+
        RecLock("XTA",.F.)
            XTA->XTA_NUMSUA := SUA->UA_NUM
        XTA->( MsUnLock() )

        //------------------------+
        // Libera pedido de Venda | 
        //------------------------+
        /*
        If EcLoj012Lib(SC5->C5_NUM)
            U_GrvStaEc(XTA->XTA_NUMECO,"003")
        Else
            U_GrvStaEc(XTA->XTA_NUMECO,"004")
        EndIf
        */
    EndIf

    LogExec("FIM EXECAUTO DATA " + dToc( Date()) + " HORA " + Time() )

EndIf

/*
Begin Transaction 
    //-----------------+
    // Grava cabeçalho |
    //-----------------+
    RecLock("SUA",.T.)
        For _nX := 1 To Len(_aStrSUA)
            _cCpo01 := 'SUA->' + RTrim(_aStrSUA[_nX][1])
            If ( _nPCpo := aScan(_aCabec,{|x| RTrim(x[1]) == RTrim(_aStrSUA[_nX][1])}) ) > 0 
                &(_cCpo01) := _aCabec[_nPCpo][2]
            Else 
                &(_cCpo01) := CriaVar(_cCpo01,.F.)
            EndIf 
        Next _nX 
    SUA->( MsUnLock() )

    //-------------+
    // Grava Itens | 
    //-------------+
    For _nX := 1 To Len(_aItems)
        RecLock("SUB",.T.)
            For _nY := 1 To Len(_aStrSUB)
                _cCpo01 := 'SUB->' + RTrim(_aStrSUB[_nY][1])
                If ( _nPCpo := aScan(_aItems[_nX],{|x| RTrim(x[1]) == RTrim(_aStrSUB[_nY][1])}) ) > 0 
                    &(_cCpo01) := _aItems[_nX][_nPCpo][2]
                Else 
                    &(_cCpo01) := CriaVar(_cCpo01,.F.)
                EndIf 
            Next _nY 
        SUB->( MsUnLock() )
    Next _nX 
    
    //----------------------+
    // Confirma a numeração |
    //----------------------+
    ConfirmSx8()

    //----------------------------------+
    // Atualiza dados Orçamento inicial |
    //----------------------------------+
    RecLock("XTA",.F.)
        XTA->XTA_NUMSUA := SUA->UA_NUM
    XTA->( MsUnLock() )

End Transaction 
*/

RestArea(_aArea)
Return Nil

/**************************************************************************/
/*/{Protheus.doc} EcLoj012Lib
    @description Libera pedido de venda
    @type  Static Function
    @author Bernard M. Margarido
    @since 27/04/2020
/*/
/**************************************************************************/
Static Function EcLoj012Lib(_cNumPv)
Local _aArea        := GetArea()

Local _aCabec       := {}
Local _aRegSC6      := {}

Local _nVlrLiber    := 0
Local _nValTot      := 0
Local _nQtdLib      := 0

Local _lRet         := .T.

Private lMsErroAuto := .F.

aAdd(_aCabec, { "C5_FILIAL", xFilial("SC5") , Nil   })
aAdd(_aCabec, { "C5_NUM"   , _cNumPv        , Nil   })

//------------------+
// Seleciona pedido | 
//------------------+
dbSelectArea("SC5")
SC5->( dbSetOrder(1) )

//---------------+
// Seleciona TES | 
//---------------+
dbSelectArea("SF4")
SF4->( dbSetOrder(1) )

//---------------------------+
// Seleciona itens liberados | 
//---------------------------+
dbSelectArea("SC9")
SC9->( dbSetOrder(1) )

//--------------------------------------+
// Posiciona itens do pedido e-Commerce |
//--------------------------------------+
dbSelectArea("SC6")
SC6->( dbSetOrder(1) )
SC6->( dbSeek(xFilial("SC6") + _cNumPv))
While SC6->( !Eof() .And. xFilial("SC6") + _cNumPv == SC6->C6_FILIAL + SC6->C6_NUM )
    //--------------------------+
    // Atualiza Total do pedido | 
    //--------------------------+
    _nValTot += SC6->C6_VALOR

    //---------------+
    // Posiciona TES | 
    //---------------+
	SF4->( MsSeek( xFilial("SF4") + SC6->C6_TES ) )
    
    //------------------+
    // Reserva registro |
    //------------------+
    If SC5->( RecLock("SC5",.F.) )

		_nQtdLib := IIF(SC6->C6_QTDLIB == 0,SC6->C6_QTDVEN,SC6->C6_QTDLIB)
		
		//---------------------------------+
		// Recalcula a Quantidade Liberada |
		//---------------------------------+
		SC6->( RecLock("SC6",.F.) )
		
		//---------------------------+
		// Libera por Item de Pedido |
		//---------------------------+
		Begin Transaction
			SC6->C6_QTDLIB := IIF(SC6->C6_QTDLIB == 0,SC6->C6_QTDVEN,SC6->C6_QTDLIB) 

            //--------------------------------+    
            // Valida se item já foi liberado | 
            //--------------------------------+    
			If SC9->( dbSeek(xFilial("SC9") + SC6->C6_NUM + SC6->C6_ITEM))

                //----------------------+        
                // Salva valor liberado | 
                //----------------------+    
				_nVlrLiber := SC6->C6_VALOR

                //-------------+
                // Salva Recno | 
                //-------------+
                _aRegSC6    := {}
				aAdd(_aRegSC6, SC6->(RecNo()))

                //------------------+
				// Posiciona pedido |
                //------------------+
				SC5->( dbSeek(xFilial("SC5") + SC6->C6_NUM ) )

                //----------------------------------------------------------------------------+    
				// Caso possuir o chamo a função de validação de cabecario do pedido de venda |
                //----------------------------------------------------------------------------+
				MaAvalSC5("SC5",3,.F.,.F.,,,,,,SC9->C9_PEDIDO,_aRegSC6,.T.,.F.,@_nVlrLiber)		
				
                //--------------------------------------------------------------------+
                // A liberação do credito é forçada pois o pagamento já foi realizado |
                //--------------------------------------------------------------------+
				RecLock("SC9",.F.)
				    SC9->C9_BLCRED := " "
				SC9->( MsUnlock() )										
			Else						
                //------------------------------------------------------------------------------+ 
				// Liberação do Credito/estoque do pedido de venda mais informações ver fatxfun |
                //------------------------------------------------------------------------------+ 
				MaLibDoFat( SC6->(RecNo()),_nQtdLib,.T.,.T.,.F.,.F.,.F.,.F.)	
			EndIf 											
			
		End Transaction
	EndIf
	
	SC5->( MsUnLock() )
	SC6->( MsUnLock() )

    SC6->( dbSkip() )
EndDo

//--------------------------------------------------------------------------+
// Verifica se existe bloqueio de crédito ou estoque, se existir desbloqueia|
//--------------------------------------------------------------------------+
MaLiberOk( { SC6->C6_NUM } )
SC5->(dbSeek(xFilial("SC5") + _cNumPv) )
If !lMsErroAuto
	_lRet   := .T.
Else
	MostraErro("/erros/" + "SC5_LIB" + _cNumPv )
	_lRet   := .F.
EndIf

RestArea(_aArea)
Return .T.

/**************************************************************************/
/*/{Protheus.doc} EcLoj012Qry
    @description Consulta pedidos eCommerce
    @type  Static Function
    @author Bernard M. Margarido
    @since 28/05/2019
    @version version
/*/
/**************************************************************************/
Static Function EcLoj012Qry(_cAlias)
Local _cQuery   := ""
Local _cCodSta  := GetNewPAr("EC_STAPROC","020")
Local _lRet     := .T.

//---------------------------+
// Formata para condição SQL |
//---------------------------+
_cCodSta:= FormatIn(_cCodSta,"/") 

_cQuery := " SELECT " + CRLF
_cQuery += "    XTA.R_E_C_N_O_ RECNOXTA " + CRLF
_cQuery += " FROM " + CRLF
_cQuery += "    " + RetSqlName("XTA") + " XTA " + CRLF 
_cQuery += " WHERE " + CRLF
_cQuery += "    XTA.XTA_FILIAL = '" + xFilial("XTA") + "' AND  " + CRLF
_cQuery += "    XTA.XTA_CODSTA IN " + _cCodSta + " AND " + CRLF
_cQuery += "    XTA.XTA_NUMSUA = '' AND " + CRLF 
_cQuery += "    XTA.D_E_L_E_T_ = '' "

_cAlias := MPSysOpenQuery(_cQuery)

If (_cAlias)->( Eof() )
    _lRet := .F.
EndIf

Return _lRet

/*********************************************************************************/
/*/{Protheus.doc} EcLoj012Can
    @description Realiza a gravação dos orçamentos na LOJA
    @type  Static Function
    @author Bernard M. Margarido
    @since 13/06/2019
    @version version
/*/
/*********************************************************************************/
Static Function EcLoj012Can()

Return Nil

/*********************************************************************************/
/*/{Protheus.doc} EcLoj012A
    @description Contulta contato
    @type  Static Function
    @author Bernard M Margarido
    @since 14/02/2024
    @version version
/*/
/*********************************************************************************/
Static Function EcLoj012A(_cIdEnt,_cCodCont,_cDescCont)
Local _cAlias   := ""
Local _cQuery   := ""

_cQuery := " SELECT " + CRLF 
_cQuery += "    U5_CODCONT, " + CRLF 
_cQuery += "    U5_CONTAT " + CRLF 
_cQuery += " FROM " + CRLF 
_cQuery += "    SU5010 " + CRLF 
_cQuery += " WHERE " + CRLF 
_cQuery += "    U5_FILIAL = '" + xFilial("SU5") + "' AND " + CRLF 
_cQuery += "    U5_XIDEND = '" + _cIdEnt + "' AND " + CRLF 
_cQuery += "    D_E_L_E_T_ = '' "

_cAlias := MPSysOpenQuery(_cQuery)

_cCodCont   := IIF(Empty((_cAlias)->U5_CODCONT),"000001",(_cAlias)->U5_CODCONT)
_cDescCont  := IIF(Empty((_cAlias)->U5_CONTAT),"PADRAO",(_cAlias)->U5_CONTAT)

(_cAlias)->( dbCloseArea() )

Return Nil 

/*********************************************************************************/
/*/{Protheus.doc} LogExec
    @description Grava Log do processo 
    @author SYMM Consultoria
    @since 26/01/2017
    @version undefined
    @param cMsg, characters, descricao
    @type function
/*/
/*********************************************************************************/
Static Function LogExec(cMsg)
	CONOUT(cMsg)
	LjWriteLog(cArqLog,cMsg)
Return .T.
