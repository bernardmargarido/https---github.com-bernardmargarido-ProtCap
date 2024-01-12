#INCLUDE "PROTHEUS.CH"
#INCLUDE "AARRAY.CH"
#INCLUDE "JSON.CH"

#DEFINE CRLF CHR(13) + CHR(10)

/******************************************************************************/
/*/{Protheus.doc} BZJOB002
    @descrption JOB - Integração Astrein 
    @type  Function
    @author Bernard M. Margarido
    @since 05/11/2020
    @version version
/*/
/******************************************************************************/
User Function BZJOB002(_cEmpInt,_cFilInt) 
Local _aArea        := GetArea()

Local _cIdProc      := ""
Local _cDescApi     := "ASTREIN_INTEGRACAO"

Private _lJob       := IIF(!Empty(_cEmpInt) .And. !Empty(_cFilInt), .T., .F.)

Private _aRetAstr   := {}

Default _cEmpInt   := "01"
Default _cFilInt   := "00"

//------------------+
// Mensagem console |
//------------------+
CoNout("<< BZJOB002 >> - INICIO " + dTos( Date() ) + " - " + Time() )

//-----------------------+
// Abre empresa / filial | 
//-----------------------+
If _lJob
    RpcSetType(3)
	RpcSetEnv(_cEmpInt, _cFilInt,,,'FAT')
EndIf

//--------------------------+
// Posiciona ID do processo | 
//--------------------------+
dbSelectArea("Z10")
Z10->( dbSetOrder(2) )
Z10->( dbSeek(xFilial("Z10") + PadR(_cDescApi,TamSx3("Z10_DESCR")[1])))
_cIdProc := Z10->Z10_ID

//-----------------------------+
// Integração de dados Astrein |
//-----------------------------+
//CoNout("<< BZJOB002 >> - INICIO COLETA DADOS PARA RETORNO ASTREIN " + dTos( Date() ) + " - " + Time() )
    BZJOB02A(_cIdProc)
//CoNout("<< BZJOB002 >> - FIM COLETA DADOS PARA RETORNO ASTREIN " + dTos( Date() ) + " - " + Time() )

//CoNout("<< BZJOB002 >> - INICIO INTEGRACAO RETORNO ASTREIN " + dTos( Date() ) + " - " + Time() )
    If Len(_aRetAstr) > 0
        BZJOB02D(_cIdProc)
    EndIf
//CoNout("<< BZJOB002 >> - FIM INTEGRACAO RETORNO ASTREIN " + dTos( Date() ) + " - " + Time() )
//------------------------+
// Fecha empresa / filial |
//------------------------+
If _lJob
    RpcClearEnv()
EndIf    

CoNout("<< BZJOB002 >> - FIM " + dTos( Date() ) + " - " + Time() )

RestArea(_aArea)
Return .T.

/*****************************************************************************************/
/*/{Protheus.doc} BZJOB02A
    @description Consulta dados para envio de retorno Astrein
    @type  Static Function
    @author Bernard M. Margarido
    @since 09/12/2020
/*/
/*****************************************************************************************/
Static Function BZJOB02A(_cIdProc)
Local _cAlias   := ""
Local _cChave   := ""
Local _cSeqZ12  := ""
Local _cCodigo  := ""
Local _cStatus  := ""
Local _cMsgLog  := ""
Local _cCnpj    := ""
Local _cProduto := ""

Local _nTCGC    := TamSx3("A1_CGC")[1]
Local _nTProd   := TamSx3("B1_COD")[1]

Local _oJson    := Nil 

//---------------------------+
// Consulta dados para envio |
//---------------------------+
If !BZJOB02B(@_cAlias,_cIdProc)
    CoNout("<< BZJOB002 >> - BZJOB002A NAO EXISTEM DADOS PARA SEREM ENVIADOS")
    Return Nil 
EndIf

//-------------------------+
// Tabela de Monitoramento | 
//-------------------------+
dbSelectArea("Z12")
Z12->( dbSetOrder(1) )

While (_cAlias)->( !Eof() )

     //--------------------+
    // Posiciona registro |
    //--------------------+
    Z12->( dbGoTo((_cAlias)->RECNOZ12))

    //----------------------------------+
    // Transforma string Json em Objeto |
    //----------------------------------+
    _oJson := FromJson(RTrim(Z12->Z12_JSON))

    //--------------------------------------+
    // Valida se Objeto pode ser processado | 
    //--------------------------------------+
    If ValType(_oJson[#"NewDataSet"][#"MATERIAL"]) <> "U"

        _oMaterial  := _oJson[#"NewDataSet"][#"MATERIAL"]
        _oIntItem   := _oJson[#"NewDataSet"][#"INTITEM"]
        _cIdAstrein := _oIntItem[#"ITE_CODIGO"]
        _cChave     := Z12->Z12_CHAVE
        _cSeqZ12    := Z12->Z12_SEQ
        _cStatus    := IIF(Z12->Z12_STPROC == "2","0","1")
        _cJSon      := RTrim(Z12->Z12_JSON)
        _cCodigo    := ""
        _cMsgLog    := ""

        If At("PRODUTO",_oJson[#"NewDataSet"][#"MATERIAL"][#"TIPO_ITEM"]) > 0
            _cProduto   := PadR(_oMaterial[#"B1_COD"],_nTProd)
            dbSelectArea("SB1")
            SB1->( dbSetOrder(1) )
            If SB1->( dbSeek(xFilial("SB1") + _cProduto) )
                _cCodigo    := SB1->B1_COD
            EndIf
        ElseIf At("FORNECEDOR",_oJson[#"NewDataSet"][#"MATERIAL"][#"TIPO_ITEM"]) > 0
            _cCnpj  := PadR(_oMaterial[#"A2_CGC"],_nTCGC)
            dbSelectArea("SA2")
            SA2->( dbSetOrder(3) )
            If SA2->( dbSeek(xFilial("SA2") + _cCnpj) )
                _cCodigo    := SA2->A2_COD + SA2->A2_LOJA
            Endif
        ElseIf At("CLIENTE",_oJson[#"NewDataSet"][#"MATERIAL"][#"TIPO_ITEM"]) > 0
            _cCnpj  := PadR(_oMaterial[#"A1_CGC"],_nTCGC)
            dbSelectArea("SA1")
            SA1->( dbSetOrder(3) )
            If SA1->( dbSeek(xFilial("SA1") + _cCnpj) )
                _cCodigo    := SA1->A1_COD + SA1->A1_LOJA
            EndIf
        EndIf

        //-------------------+
        // Consulta erro Z13 |
        //-------------------+
        BZJOB02C(_cIdProc,_cChave,_cSeqZ12,@_cMsgLog)

        //-----------------------------------------------------------+
        // Adiciona dados para realizar a baixa dos dados na Astrein |
        //-----------------------------------------------------------+
        aAdd(_aRetAstr,{_cIdAstrein, _cStatus, _cMsgLog,_cCodigo,_cChave,_cJSon,(_cAlias)->RECNOZ12})
        
    EndIf
    (_cAlias)->( dbSkip() )
EndDo

(_cAlias)->(dbCloseArea() )

Return Nil 

/*****************************************************************************************/
/*/{Protheus.doc} BZJOB02B
    @description Consulta dados para serem enviados 
    @type  Static Function
    @author Bernard M. Margarido
    @since 09/12/2020
/*/
/*****************************************************************************************/
Static Function BZJOB02B(_cAlias,_cIdProc)
Local _cQuery   := ""

_cQuery := " SELECT " + CRLF
_cQuery += "    Z12_ID, " + CRLF
_cQuery += "	Z12_CHAVE, " + CRLF
_cQuery += "	Z12_SEQ, " + CRLF
_cQuery += "    R_E_C_N_O_ RECNOZ12 " + CRLF
_cQuery += " FROM " + CRLF
_cQuery += "	" + RetSqlName("Z12") + " " + CRLF 
_cQuery += " WHERE " + CRLF
_cQuery += "	Z12_FILIAL = '" + xFilial("Z12") + "' AND " + CRLF
_cQuery += "	Z12_CHAVE <> '' AND " + CRLF
_cQuery += "	Z12_ID = '" + _cIdProc + "' AND " + CRLF
_cQuery += "	Z12_STPROC IN('2','3') AND " + CRLF
_cQuery += "	D_E_L_E_T_ = '' "

_cAlias := MPSysOpenQuery(_cQuery)

If (_cAlias)->( Eof() )
    (_cAlias)->( dbCloseArea() )
    Return .F.
EndIf

Return .T. 

/*****************************************************************************************/
/*/{Protheus.doc} BZJOB02C
    @description Consulta dados log gravado 
    @type  Static Function
    @author Bernard M. Margarido
    @since 09/12/2020
/*/
/*****************************************************************************************/
Static Function BZJOB02C(_cIdProc,_cChave,_cSeqZ12,_cMsgLog)
Local _cQuery := ""
Local _cAlias := ""

_cQuery := " SELECT " + CRLF
_cQuery += "    Z13.Z13_ID, " + CRLF	
_cQuery += "    Z13.Z13_CHAVE, " + CRLF
_cQuery += "    Z13.Z13_Z12SEQ, " + CRLF
_cQuery += "	Z13.R_E_C_N_O_ RECNOZ13 " + CRLF
_cQuery += " FROM " + CRLF
_cQuery += "	" + RetSqlName("Z13") + " Z13 " + CRLF
_cQuery += " WHERE
_cQuery += "	Z13.Z13_FILIAL = '" + xFilial("Z13") + "' AND " + CRLF
_cQuery += "	Z13.Z13_ID = '" + _cIdProc + "' AND " + CRLF
_cQuery += "	Z13.Z13_CHAVE = '" + _cChave + "' AND " + CRLF
_cQuery += "	Z13.Z13_Z12SEQ = '" + _cSeqZ12 + "' AND " + CRLF
_cQuery += "	Z13.D_E_L_E_T_ = '' "

_cAlias := MPSysOpenQuery(_cQuery)

If (_cAlias)->( Eof() )
    (_cAlias)->( dbCloseArea() )
    Return .F.
EndIf

//--------------------+
// Posiciona registro |
//--------------------+
dbSelectArea("Z13")
Z13->( dbSetOrder(1) )
Z13->( dbGoTo((_cAlias)->RECNOZ13) )
_cMsgLog := Alltrim(Z13->Z13_ERRO)

(_cAlias)->( dbCloseArea() )

Return .T.

/*****************************************************************************************/
/*/{Protheus.doc} BZJOB02D
    @description Envia retorno para Astrein 
    @type  Static Function
    @author Bernard M. Margarido
    @since 09/12/2020
/*/
/*****************************************************************************************/
Static Function BZJOB02D(_cIdProc)
Local _aArea    := GetArea()

Local _nTpItem  := GetNewPar("BZ_TPIASTR",1)
Local _nX       := 0

Local _oAstrein := Astrein():New()
Local _oJson    := Nil 

//----------------------+
// Z12  - Tabela de LOG |
//----------------------+
dbSelectArea("Z12")
Z12->( dbSetOrder(1) )

//----------------------------+
// Envia a baixa para Astrein | 
//----------------------------+
For _nX := 1 To Len(_aRetAstr)

    //--------------------+
    // Posiciona registro | 
    //--------------------+
    Z12->( dbGoTo(_aRetAstr[_nX][7]))

    //-------------------+
    // Monta objeto Json |
    //-------------------+
    _oJson                           := Array(#)
    _oJson[#"divisao"]               := _nTpItem
    _oJson[#"idIntegracao"]          := _aRetAstr[_nX][1]
    _oJson[#"statusIntegracao"]      := _aRetAstr[_nX][2]
    _oJson[#"mensagemErroIntegracao"]:= _aRetAstr[_nX][3]
    _oJson[#"codigoERP"]             := _aRetAstr[_nX][4]
    _oJson[#"idiomaIntegracaoErro"]  := "" 

    _oAstrein:cJSon := ToJson(_oJson)

    If _oAstrein:RetornoIntegracao()
        CoNout("<< BZJOB002 >> - BZJOB02D RETORNO INTEGRACAO ASTREIN REALIZADO COM SUCESSO.")
        //--------------+
        // Atualiza Z12 |
        //--------------+
        StaticCall(BZAPI001, BzApi001d, _cIdProc,_aRetAstr[_nX][5],_aRetAstr[_nX][6],_aRetAstr[_nX][3],"6")

    Else
       CoNout("<< BZJOB002 >> - BZJOB02D ERRO NO ENVIO DO RETORNO INTEGRACAO ASTREIN " + RTrim(_oAstrein:cError))
    EndIf

Next _nX 

RestArea(_aArea)
Return Nil 