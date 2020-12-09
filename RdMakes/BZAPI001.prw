#INCLUDE "PROTHEUS.CH"
#INCLUDE "AARRAY.CH"
#INCLUDE "JSON.CH"

#DEFINE CRLF CHR(13) + CHR(10)

Static _nTCGC := TamSx3("A1_CGC")[1]
Static _nTProd:= TamSx3("B1_COD")[1]

/***********************************************************************************/
/*/{Protheus.doc} BZAPI001
    @description Rotina realiza a integração de clientes Astrein 
    @type  Function
    @author Bernard M. Margarido 
    @since 31/10/2020
/*/
/***********************************************************************************/
User Function BZAPI001(nRecno)
Local _cIdProc      := ""
Local _cChave       := ""
Local _cDescApi     := "ASTREIN_INTEGRACAO"

Private _lJob       := IIF(Isincallstack("U_BZJOB001"),.T.,.F.)
Private _aRetAstr   := {}

Default nRecno      := 0

//------------------+
// Mensagem console |
//------------------+
// CoNout("<< BZAPI001 >> - INICIO " + dTos( Date() ) + " - " + Time() )

//-------------------------------------+
// Posiciona registro de monitoramento |
//-------------------------------------+
dbSelectArea("Z10")
Z10->( dbSetOrder(2) )

If nRecno > 0 
    Z10->( dbGoto(nRecno) )
    _cIdProc := Z10->Z10_ID
Else
    Z10->( dbSeek(xFilial("Z10") + PadR(_cDescApi,TamSx3("Z10_DESCR")[1])))
    _cIdProc := Z10->Z10_ID
EndIf

//-----------------------------+
// Integração de Dados Astrein |
//-----------------------------+
// CoNout("<< BZAPI001 >> - INICIO OBTER INTEGRACOES ASTREIN " + dTos( Date() ) + " - " + Time() )
    BZAPI001A(_cIdProc,@_cChave)
// CoNout("<< BZAPI001 >> - FIM OBTER INTEGRACOES ASTREIN " + dTos( Date() ) + " - " + Time() )

//----------------+
// Processa Dados | 
//----------------+
// CoNout("<< BZAPI001 >> - INICIO RETORNO INTEGRACOES ASTREIN " + dTos( Date() ) + " - " + Time() )
    BZAPI001C(_cIdProc,_cChave)
// CoNout("<< BZAPI001 >> - FIM RETORNO INTEGRACOES ASTREIN " + dTos( Date() ) + " - " + Time() )

// CoNout("<< BZAPI001 >> - FIM " + dTos( Date() ) + " - " + Time() )
Return Nil 

/***********************************************************************************/
/*/{Protheus.doc} BZAPI001A
    @description Realiza a integração de obter dados da Astrein 
    @type  Static Function
    @author Bernard M. Margarido
    @since 01/11/2020
/*/
/***********************************************************************************/
Static Function BZAPI001A(_cIdProc,_cChave)
Local _aArea    := GetArea()

Local _cError   := ""
Local _cStatus  := ""
Local _cCodEmp  := GetNewPar("BZ_EMPASTR","BEPI")

Local _nTpItem  := GetNewPar("BZ_TPIASTR",1)

Local _oAstrein := Astrein():New()
Local _oJSon    := Nil 

//-------------------+
// Monta objeto Json |
//-------------------+
_oJSon                   := Array(#)
_oJSon[#"codigoProjeto"] := _nTpItem
_oJSon[#"tipoItem"]      := _cCodEmp

_oAstrein:cJSon          := ToJson(_oJSon)

_cChave                  := ""

CoNout("<< BZAPI001 >> - BZAPI001A INICIO API OBTER DADOS ASTREIN " + dTos( Date() ) + " - " + Time() )

If _oAstrein:ObterIntegracao()
    CoNout("<< BZAPI001 >> - BZAPI001A CONSULTA REALIZADA COM SUCESSO.")
   
    //----------------------+
    // StringJson em Objeto |
    //----------------------+
    _oJson := FromJson(_oAstrein:cJSonRet)

    //-------------------------------------+
    // Cria chave para gravação do monitor |
    //-------------------------------------+
    If ValType(_oJson[#"NewDataSet"][#"INTITEM"]) <> "U"

        _cError := ""
        _cStatus:= "1"
        _cChave := RTrim(_oJson[#"NewDataSet"][#"INTITEM"][#"ITE_CODIGO"]) + RTrim(_oJson[#"NewDataSet"][#"INTITEM"][#"MAT_CODIGO"])

        //--------------+
        // Atualiza Z12 |
        //--------------+
        BzApi001d(_cIdProc,_cChave,_oAstrein:cJSonRet,_cError,_cStatus)

    EndIf

EndIf

CoNout("<< BZAPI001 >> - BZAPI001A FIM API OBTER DADOS ASTREIN " + dTos( Date() ) + " - " + Time() )

FreeObj(_oAstrein)
FreeObj(_oJSon)
RestArea(_aArea)
Return Nil

/***********************************************************************************/
/*/{Protheus.doc} BZAPI001C
    @description Realiza a criação/atualização dos clientes
    @type  Static Function
    @author Bernard M. Margarido
    @since 03/11/2020
/*/
/***********************************************************************************/
Static Function BZAPI001C(_cIdProc,_cChave)
Local _aArea        := GetArea()
Local _aArray       := {}
Local _aStruct      := {}
Local _aErroAuto    := {}

Local _cCpoDel      := "A1_TABELA"
Local _cArqLog      := ""
Local _cMsgLog      := ""
Local _cAlias       := ""
Local _cCpj         := ""
Local _cCodigo      := ""
Local _cLoja        := ""
Local _xRet         := ""

Local _nOpc         := 0
Local _nX           := 0 

Local _oJson        := Nil 
Local _oMaterial    := Nil 

Private lMsErroAuto := .F.

If !BzApi01CQry(_cIdProc,@_cAlias)
    CoNout("<< BZAPI001 >> - BZAPI001C NAO EXISTEM DADOS PARA SEREM PROCESSADOS")
    RestArea(_aArea)
    Return .F.
EndIf

//-------------------------+
// Tabela de Monitoramento | 
//-------------------------+
dbSelectArea("Z12")
Z12->( dbSetOrder(1) )

//------------------------+
// Processa clientes BEPI |
//------------------------+
While (_cAlias)->( !Eof() )
    //--------------------+
    // Posiciona registro |
    //--------------------+
    Z12->( dbGoTo((_cAlias)->RECNOZ12))

    //----------------------------------+
    // Transforma string Json em Objeto |
    //----------------------------------+
    _oJson := FromJson(Z12->Z12_JSON)

    //--------------------------------------+
    // Valida se Objeto pode ser processado | 
    //--------------------------------------+
    If ValType(_oJson[#"NewDataSet"][#"MATERIAL"]) <> "U"

        //-------------------+
        // Processa Clientes | 
        //-------------------+
        If At("CLIENTE",_oJson[#"NewDataSet"][#"MATERIAL"][#"TIPO_ITEM"]) > 0

            _aArray     := {}
            _aErroAuto  := {}
            _aStruct    := SA1->( dbStruct() )
            _oMaterial  := _oJson[#"NewDataSet"][#"MATERIAL"]
            _oIntItem   := _oJson[#"NewDataSet"][#"INTITEM"]
            _nOpc       := 0    
            _cCpj       := PadR(_oMaterial[#"A1_CGC"],_nTCGC)
			_cIdAstrein := _oIntItem[#"MAT_CODIGO"]

            //---------------------------------+    
            // Valida codigo e loja do cliente |
            //---------------------------------+
            BzApi01Cli(_cCpj,_cIdAstrein,@_cCodigo,@_cLoja,@_nOpc)

            //-----------------------+    
            // Cria array de cliente |
            //-----------------------+
            For _nX := 1 To Len(_aStruct)

				If _aStruct[_nX][1] == "A1_COD" 
					_xRet := _cCodigo
					aAdd(_aArray,{_aStruct[_nX][1], _xRet, Nil })
				ElseIf _aStruct[_nX][1] == "A1_LOJA"
					_xRet := _cLoja
					aAdd(_aArray,{_aStruct[_nX][1], _xRet, Nil })
				Endif

                If !_aStruct[_nX][1] $ _cCpoDel
                    If ValType(_oMaterial[#_aStruct[_nX][1]]) <> "U"
                        If RTrim(_oMaterial[#_aStruct[_nX][1]]) <> "NAO APLICAVEL"
                            Do Case 
                                Case _aStruct[_nX][2] == "D"
                                    _xRet := cTod(_oMaterial[#_aStruct[_nX][1]])
                                Case _aStruct[_nX][2] == "N"
                                    _xRet := Val(_oMaterial[#_aStruct[_nX][1]])
                                Case _aStruct[_nX][2] == "L"
                                    _xRet := IIF(SubStr(_oMaterial[#_aStruct[_nX][1]],1,1) $ "T/t", .T., .F.)
                                OtherWise
                                    If _aStruct[_nX][1] == "A1_TIPO"
                                        _xRet := SubStr(_oMaterial[#_aStruct[_nX][1]],1,1)
                                    ElseIf _aStruct[_nX][1] == "A1_SIMPLES"
                                        _xRet := IIF(SubStr(_oMaterial[#_aStruct[_nX][1]],1,1) == "N",2,1)
                                    ElseIf _aStruct[_nX][1] == "A1_COD_MUN" 
                                        _xRet := IIF(Len(RTrim(_oMaterial[#_aStruct[_nX][1]])) > 5, SubStr(_oMaterial[#_aStruct[_nX][1]],3),_oMaterial[#_aStruct[_nX][1]])
                                    //ElseIf _aStruct[_nX][1] == "A1_LOJA" .And. _nOpc == 3
                                    //    _xRet := _cLoja
                                    //ElseIf _aStruct[_nX][1] == "A1_COD" .And. _nOpc == 3    
                                    //    _xRet := _cCodigo
                                    Else
                                        _xRet := _oMaterial[#_aStruct[_nX][1]]
                                    EndIf
                            EndCase
                            aAdd(_aArray,{_aStruct[_nX][1], _xRet, Nil })
                        EndIf
                    //----------------------------------------------+
                    // Campos que não fazem parte do Layput Astrein | 
                    //----------------------------------------------+
                    Else
                        If _aStruct[_nX][1] == "A1_XIDASTR"
                            _xRet := Val(_cIdAstrein)
                            aAdd(_aArray,{_aStruct[_nX][1], _xRet, Nil })
                        EndIf
                    EndIf 
                EndIf
            Next _nX

            //-----------------------+
            // Roda ExecAuto Cliente |
            //-----------------------+
            lMsErroAuto     := .F.
            lAutoErrNoFile  := .T.
            MsExecAuto({|x,y| Mata030(x,y)}, _aArray, _nOpc)

            If lMsErroAuto
                RollBackSx8()
                
                //MakeDir("/erros/")
                //_cArqLog    := "SA1_" + RTrim(Z12->Z12_CHAVE) + "_" + DToS(Date()) + Left(Time(),2) + SubStr(Time(),4,2) + Right(Time(),2) + ".LOG"
                //_cMsgLog    := MostraErro("/erros/",_cArqLog)

                //-------------------+
                // Log erro ExecAuto |
                //-------------------+
                _aErroAuto := GetAutoGRLog()

                //------------------------------------+
                // Retorna somente a linha com o erro | 
                //------------------------------------+
                ErroAuto(_aErroAuto,@_cMsgLog)

                //--------------------+
                // Atualiza historico | 
                //--------------------+
                BzApi001d(Z12->Z12_ID,Z12->Z12_CHAVE,Z12->Z12_JSON,_cMsgLog,"3",4)

                BzApi001e(Z12->Z12_ID,Z12->Z12_CHAVE,Z12->Z12_JSON,_cMsgLog)

                //-----------------------------------------------------------+
                // Adiciona dados para realizar a baixa dos dados na Astrein |
                //-----------------------------------------------------------+
                aAdd(_aRetAstr,{_cIdAstrein,"1",_cMsgLog,""})

            Else
                ConfirmSx8()

                //-------------------------------------+    
                // Atualiza historico do processamento |
                //-------------------------------------+
                BzApi001d(Z12->Z12_ID,Z12->Z12_CHAVE,Z12->Z12_JSON,"","2",4)

                //-----------------------------------------------------------+
                // Adiciona dados para realizar a baixa dos dados na Astrein |
                //-----------------------------------------------------------+
                aAdd(_aRetAstr,{_cIdAstrein,"0","CLIENTE BAIXADO COM SUCESSO",RTrim(SA1->A1_COD) + RTrim(SA1->A1_LOJA)})

            Endif
        
        //---------------------+
        // Processa fornecedor |
        //---------------------+
        ElseIf At("FORNECEDOR",_oJson[#"NewDataSet"][#"MATERIAL"][#"TIPO_ITEM"]) > 0

            _aArray     := {}
            _aErroAuto  := {}
            _aStruct    := SA2->( dbStruct() )
            _oMaterial  := _oJson[#"NewDataSet"][#"MATERIAL"]
            _oIntItem   := _oJson[#"NewDataSet"][#"INTITEM"]
            _nOpc       := 0    
            _cCpj       := PadR(_oMaterial[#"A2_CGC"],_nTCGC)
            _cIdAstrein := _oIntItem[#"ITE_CODIGO"]

            //------------------------------------+    
            // Valida codigo e loja do fornecedor |
            //------------------------------------+
            BzApi01For(_cCpj,_cIdAstrein,@_cCodigo,@_cLoja,@_nOpc)

            //-----------------------+    
            // Cria array de cliente |
            //-----------------------+
            For _nX := 1 To Len(_aStruct)
                If ValType(_oMaterial[#_aStruct[_nX][1]]) <> "U"
                    If RTrim(_oMaterial[#_aStruct[_nX][1]]) <> "NAO APLICAVEL"
                        Do Case 
                            Case _aStruct[_nX][2] == "D"
                                _xRet := cTod(_oMaterial[#_aStruct[_nX][1]])
                            Case _aStruct[_nX][2] == "N"
                                _xRet := Val(_oMaterial[#_aStruct[_nX][1]])
                            Case _aStruct[_nX][2] == "L"
                                _xRet := IIF(SubStr(_oMaterial[#_aStruct[_nX][1]],1,1) $ "T/t", .T., .F.)
                            OtherWise
                                If _aStruct[_nX][1] == "A2_TIPO"
                                    _xRet := SubStr(_oMaterial[#_aStruct[_nX][1]],1,1)
                                ElseIf _aStruct[_nX][1] == "A2_SIMPLES"
                                    _xRet := IIF(SubStr(_oMaterial[#_aStruct[_nX][1]],1,1) == "N",2,1)
                                ElseIf _aStruct[_nX][1] == "A2_COD_MUN" 
                                    _xRet := IIF(Len(RTrim(_oMaterial[#_aStruct[_nX][1]])) > 5, SubStr(_oMaterial[#_aStruct[_nX][1]],3),_oMaterial[#_aStruct[_nX][1]])
                                ElseIf _aStruct[_nX][1] == "A2_LOJA" .And. _nOpc == 3
                                    _xRet := _cLoja
                                ElseIf _aStruct[_nX][1] == "A2_COD" .And. _nOpc == 3    
                                    _xRet := _cCodigo
                                Else
                                    _xRet := _oMaterial[#_aStruct[_nX][1]]
                                EndIf
                        EndCase
                        aAdd(_aArray,{_aStruct[_nX][1], _xRet, Nil })
                    EndIf
                //----------------------------------------------+
                // Campos que não fazem parte do Layput Astrein | 
                //----------------------------------------------+
                Else
                    If _aStruct[_nX][1] == "A2_XIDASTR"
                        _xRet := Val(_cIdAstrein)
                        aAdd(_aArray,{_aStruct[_nX][1], _xRet, Nil })
                    EndIf
                EndIf 
            Next _nX

            //-----------------------+
            // Roda ExecAuto Cliente |
            //-----------------------+
            lMsErroAuto     := .F.
            lAutoErrNoFile  := .T.
            MsExecAuto({|x,y| Mata020(x,y)}, _aArray, _nOpc)

            If lMsErroAuto
                RollBackSx8()
                
                //MakeDir("/erros/")
                //_cArqLog    := "SA2_" + RTrim(Z12->Z12_CHAVE) + "_" + DToS(Date()) + Left(Time(),2) + SubStr(Time(),4,2) + Right(Time(),2) + ".LOG"
                //_cMsgLog    := MostraErro("/erros/",_cArqLog)

                //-------------------+
                // Log erro ExecAuto |
                //-------------------+
                _aErroAuto := GetAutoGRLog()

                //------------------------------------+
                // Retorna somente a linha com o erro | 
                //------------------------------------+
                ErroAuto(_aErroAuto,@_cMsgLog)
                            
                //--------------------+
                // Atualiza historico | 
                //--------------------+
                BzApi001d(Z12->Z12_ID,Z12->Z12_CHAVE,Z12->Z12_JSON,_cMsgLog,"3",4)

                BzApi001e(Z12->Z12_ID,Z12->Z12_CHAVE,Z12->Z12_JSON,_cMsgLog)

                //-----------------------------------------------------------+
                // Adiciona dados para realizar a baixa dos dados na Astrein |
                //-----------------------------------------------------------+
                aAdd(_aRetAstr,{_cIdAstrein,"1",_cMsgLog,""})
            Else

                ConfirmSx8()
                BzApi001d(Z12->Z12_ID,Z12->Z12_CHAVE,Z12->Z12_JSON,"","2",4)
                //-----------------------------------------------------------+
                // Adiciona dados para realizar a baixa dos dados na Astrein |
                //-----------------------------------------------------------+
                aAdd(_aRetAstr,{_cIdAstrein,"0","FORNECEDOR PROCESSADO COM SUCESSO",RTrim(SA2->A2_COD) + RTrim(SA2->A2_LOJA)})

            EndIf
        //------------------+
        // Processa produto |
        //------------------+
        ElseIf At("PRODUTO",_oJson[#"NewDataSet"][#"MATERIAL"][#"TIPO_ITEM"]) > 0

            //--------------------------+    
            // SB1 - Tabela de Produtos |
            //--------------------------+
            dbSelectArea("SB1")
            SB1->( dbSetOrder(1) )
                
            _aArray     := {}
            _aErroAuto  := {}
            _aStruct    := SB1->( dbStruct() )
            _oMaterial  := _oJson[#"NewDataSet"][#"MATERIAL"]
            _oIntItem   := _oJson[#"NewDataSet"][#"INTITEM"]
            _nOpc       := 3    
            _cCodPrd    := PadR(_oMaterial[#"B1_COD"],_nTProd)
            _cIdAstrein := _oIntItem[#"ITE_CODIGO"]

            //-------------------+    
            // Posiciona produto | 
            //-------------------+    
            If SB1->( dbSeek(xFilial("SB1") + _cCodPrd) )
                _nOpc := 4
            EndIf

            //-----------------------+    
            // Cria array de cliente |
            //-----------------------+
            For _nX := 1 To Len(_aStruct)
                If ValType(_oMaterial[#_aStruct[_nX][1]]) <> "U"
                    If RTrim(_oMaterial[#_aStruct[_nX][1]]) <> "NAO APLICAVEL"
                        Do Case 
                            Case _aStruct[_nX][2] == "D"
                                _xRet := cTod(_oMaterial[#_aStruct[_nX][1]])
                            Case _aStruct[_nX][2] == "N"
                                _xRet := Val(_oMaterial[#_aStruct[_nX][1]])
                            Case _aStruct[_nX][2] == "L"
                                _xRet := IIF(SubStr(_oMaterial[#_aStruct[_nX][1]],1,1) $ "T/t", .T., .F.)
                            OtherWise
                                _xRet := _oMaterial[#_aStruct[_nX][1]]
                        EndCase
                        aAdd(_aArray,{_aStruct[_nX][1], _xRet, Nil })
                    EndIf
                //----------------------------------------------+
                // Campos que não fazem parte do Layput Astrein | 
                //----------------------------------------------+
                Else
                    If _aStruct[_nX][1] == "B1_XIDASTR"
                        _xRet := Val(_cIdAstrein)
                        aAdd(_aArray,{_aStruct[_nX][1], _xRet, Nil })
                    EndIf
                EndIf 
            Next _nX

            //-----------------------+
            // Roda ExecAuto Cliente |
            //-----------------------+
            lMsErroAuto     := .F.
            lAutoErrNoFile  := .T.
            MsExecAuto({|x,y| Mata020(x,y)}, _aArray, _nOpc)

            If lMsErroAuto
                RollBackSx8()
                
                //MakeDir("/erros/")
                //_cArqLog    := "SA2_" + RTrim(Z12->Z12_CHAVE) + "_" + DToS(Date()) + Left(Time(),2) + SubStr(Time(),4,2) + Right(Time(),2) + ".LOG"
                //_cMsgLog    := MostraErro("/erros/",_cArqLog)

                //-------------------+
                // Log erro ExecAuto |
                //-------------------+
                _aErroAuto := GetAutoGRLog()

                //------------------------------------+
                // Retorna somente a linha com o erro | 
                //------------------------------------+
                ErroAuto(_aErroAuto,@_cMsgLog)

                //--------------------+
                // Atualiza historico | 
                //--------------------+
                BzApi001d(Z12->Z12_ID,Z12->Z12_CHAVE,Z12->Z12_JSON,_cMsgLog,"3",4)

                BzApi001e(Z12->Z12_ID,Z12->Z12_CHAVE,Z12->Z12_JSON,_cMsgLog)

                //-----------------------------------------------------------+
                // Adiciona dados para realizar a baixa dos dados na Astrein |
                //-----------------------------------------------------------+
                aAdd(_aRetAstr,{_cIdAstrein,"1",_cMsgLog,""})
            Else

                ConfirmSx8()
                BzApi001d(Z12->Z12_ID,Z12->Z12_CHAVE,Z12->Z12_JSON,"","2",4)
                //-----------------------------------------------------------+
                // Adiciona dados para realizar a baixa dos dados na Astrein |
                //-----------------------------------------------------------+
                aAdd(_aRetAstr,{_cIdAstrein,"0","FORNECEDOR PROCESSADO COM SUCESSO",RTrim(SA2->A2_COD) + RTrim(SA2->A2_LOJA)})

            Endif
        EndIf
    EndIf
    (_cAlias)->( dbSkip() )
EndDo

(_cAlias)->( dbCloseArea() )

RestArea(_aArea)
Return .T.

/***********************************************************************************/
/*/{Protheus.doc} BzApi001d
    @description Grava atualização na tabela de LOG
    @type  Static Function
    @author Bernard M. Margarido
    @since 03/11/2020
/*/
/***********************************************************************************/
Static Function BzApi001d(_cIdProc,_cChave,_cJson,_cError,_cStatus,_nOpc)
Local _aArea    := GetArea()

Local _lRet     := .T.

Local _oMonitor := ProtMonitor():New()

Default _nOpc   := 3

CoNout("<< BZAPI001 >> - BZAPI001D INICIA A GRAVACAO DO MONITOR Z12 " + dTos( Date() ) + " - " + Time())

_oMonitor:cIdProc   := _cIdProc
_oMonitor:cChave    := _cChave
_oMonitor:cStatus   := _cStatus
_oMonitor:cJSon     := _cJson
_oMonitor:nQtdReg   := 1
_oMonitor:nOpc      := _nOpc
_oMonitor:cFunName  := ProcName(2)

If _oMonitor:GrvMonitor()
    _lRet := .T.
    CoNout("<< BZAPI001 >> - BZAPI001D GRAVACAO DO MONITOR Z12 REALIZADA COM SUCESSO .")
Else
    _lRet := .F.
    CoNout("<< BZAPI001 >> - BZAPI001D ERRO AO REALIZAR A GRAVACAO DO MONITOR Z12.")
EndIf

CoNout("<< BZAPI001 >> - BZAPI001D FIM A GRAVACAO DO MONITOR Z12 " + dTos( Date() ) + " - " + Time())

RestArea(_aArea)
Return _lRet 

/***********************************************************************************/
/*/{Protheus.doc} BzApi001e
    @description Atualiza historico monitor
    @type  Static Function
    @author Bernard M. Margarido
    @since 04/11/2020
/*/
/***********************************************************************************/
Static Function BzApi001e(_cIdProc,_cChave,_cJson,_cError)
Local _lRet     := .T.

Local _oMonitor := ProtMonitor():New()

CoNout("<< BZAPI001 >> - BZAPI001E INICIO A GRAVACAO DO HISTORICO Z13 " + dTos( Date() ) + " - " + Time())

_oMonitor:cIdProc   := _cIdProc
_oMonitor:cChave    := _cChave
_oMonitor:cError    := _cError
_oMonitor:cJSon     := _cJson

If _oMonitor:GrvHistorico()
    _lRet     := .T.
    CoNout("<< BZAPI001 >> - BZAPI001E GRAVACAO DO HISTORICO Z13 REALIZADA COM SUCESSO .")
Else
    _lRet     := .F.
    CoNout("<< BZAPI001 >> - BZAPI001E ERRO AO REALIZAR A GRAVACAO DO HISTORICO Z13.")
EndIf
CoNout("<< BZAPI001 >> - BZAPI001E FIM A GRAVACAO DO HISTORICO Z13 " + dTos( Date() ) + " - " + Time())

Return _lRet 

/***********************************************************************************/
/*/{Protheus.doc} BzApi01Cli
    @description Valida codigo e Loja do Cliente
    @type  Static Function
    @author Bernard M. Margarido
    @since 03/11/2020
/*/
/***********************************************************************************/
Static Function BzApi01Cli(_cCpj,_cIdAstrein,_cCodigo,_cLoja,_nOpc)
Local _aArea    := GetArea()

If !Empty(_cIdAstrein)
    If !BzApiCliA(_cIdAstrein,@_cCodigo,@_cLoja,@_nOpc)
        BzApiCliB(_cCpj,@_cCodigo,@_cLoja,@_nOpc)    
    EndIf
Else
    BzApiCliB(_cCpj,@_cCodigo,@_cLoja,@_nOpc)
EndIf

RestArea(_aArea)
Return Nil

/***********************************************************************************/
/*/{Protheus.doc} BzApiCliA
    @description Consulta cliente pelo ID Astrein 
    @type  Static Function
    @author Bernard M. Margarido
    @since 06/11/2020
/*/
/***********************************************************************************/
Static Function BzApiCliA(_cIdAstrein,_cCodigo,_cLoja,_nOpc)
Local _lRet     := .T.
Local _cAlias   := ""
Local _cQuery   := ""

_cQuery := " SELECT " + CRLF
_cQuery += "	A1_COD, " + CRLF 
_cQuery += "	A1_LOJA " + CRLF
_cQuery += " FROM " + CRLF
_cQuery += "	" + RetSqlName("SA1") + " " + CRLF 
_cQuery += " WHERE " + CRLF
_cQuery += "	A1_FILIAL = '" + xFilial("SA1") + "' AND " + CRLF
_cQuery += "	A1_XIDASTR = '" + _cIdAstrein + "' AND " + CRLF
_cQuery += "	D_E_L_E_T_ = '' " + CRLF

_cAlias := MPSysOpenQuery(_cQuery)

_lRet       := IIF(EmptY((_cAlias)->A1_COD),.F.,.T.)
_nOpc       := IIF(EmptY((_cAlias)->A1_COD), 3, 4)
_cCodigo    := IIF(EmptY((_cAlias)->A1_COD), "", (_cAlias)->A1_COD)
_cLoja      := IIF(EmptY((_cAlias)->A1_LOJA), "", (_cAlias)->A1_LOJA)

(_cAlias)->( dbCloseArea() )
Return _lRet 

/***********************************************************************************/
/*/{Protheus.doc} BzApiCliB
    @description Consulta Cliente pelo CNPJ
    @type  Static Function
    @author Bernard M. Margarido
    @since 06/11/2020
/*/
/***********************************************************************************/
Static Function BzApiCliB(_cCnpj,_cCodigo,_cLoja,_nOpc)

dbSelectArea("SA1")
SA1->( dbSetOrder(3) )
//---------------+
// Localiza CNPJ |
//---------------+
If SA1->( dbSeek(xFilial("SA1") + _cCnpj))
    BzApiCliC(_cCnpj,@_cCodigo,@_cLoja,@_nOpc)
//--------------------+
// Localiza Raiz CNPJ |
//--------------------+    
ElseIf SA1->( dbSeek(xFilial("SA1") + SubStr(_cCnpj,1,8)))
    BzApiCliC(SubStr(_cCnpj,1,8),@_cCodigo,@_cLoja,@_nOpc)
//--------------+
// Novo Cliente |
//--------------+
Else
    _nOpc   := 3
    _cCodigo:= GetSxeNum("SA1","A1_COD")
    _cLoja  := "0000"
    SA1->( dbSetOrder(1) )
    While SA1->( dbSeek(xFilial("SA1") + _cCodigo + _cLoja) )
        ConfirmSx8()
        _cCodigo := GetSxeNum("SA1","A1_COD","",1)
    EndDo	
EndIf

Return Nil

/***********************************************************************************/
/*/{Protheus.doc} BzApiCliC
    @description Realiza a consulta dos CNPJ's 
    @type  Static Function
    @author Bernard M. Margarido
    @since 06/11/2020
/*/
/***********************************************************************************/
Static Function BzApiCliC(_cCnpj,_cCodigo,_cLoja,_nOpc)
Local _cQuery   := ""

_cQuery := " SELECT " + CRLF
_cQuery += "	A1_COD CODIGO, " + CRLF
_cQuery += "	MAX(A1_LOJA) LOJA, " + CRLF
_cQuery += "	COUNT(*) TOTAL " + CRLF	
_cQuery += " FROM " + CRLF
_cQuery += "	" + RetSqlName("SA1") + " " + CRLF
_cQuery += " WHERE " + CRLF
_cQuery += "	A1_FILIAL = '" + xFilial("SA1") + "' AND " + CRLF

If Len(RTrim(_cCnpj)) < 14
    _cQuery += "	A1_CGC LIKE '%" + _cCnpj + "%' AND " + CRLF
Else
    _cQuery += "	A1_CGC = '" + _cCnpj + "' AND " + CRLF
EndIf
_cQuery += "	D_E_L_E_T_ = '' " + CRLF
_cQuery += " GROUP BY A1_COD "

_cAlias     := MPSysOpenQuery(_cQuery)

_cCodigo    := (_cAlias)->CODIGO

(_cAlias)->( dbCloseArea() )

_cQuery := " SELECT " + CRLF
_cQuery += "	MAX(A1_LOJA) LOJA " + CRLF
_cQuery += " FROM " + CRLF
_cQuery += "	" + RetSqlName("SA1") + " " + CRLF
_cQuery += " WHERE " + CRLF
_cQuery += "	A1_FILIAL = '" + xFilial("SA1") + "' AND " + CRLF
_cQuery += "	A1_COD = '" + _cCodigo + "' AND " + CRLF
_cQuery += "	D_E_L_E_T_ = '' " + CRLF

_cAlias     := MPSysOpenQuery(_cQuery)

_cLoja      := Soma1((_cAlias)->LOJA)
_nOpc       := 3

(_cAlias)->( dbCloseArea() )

Return Nil 

/***********************************************************************************/
/*/{Protheus.doc} BzApi01For
    @description Valida codigo e loja do fornecedor 
    @type  Static Function
    @author Bernard M. Margarido
    @since 22/11/2020
/*/
/***********************************************************************************/
Static Function BzApi01For(_cCpj,_cIdAstrein,_cCodigo,_cLoja,_nOpc)
Local _aArea    := GetArea()

If !Empty(_cIdAstrein)
    If !BzApiForA(_cIdAstrein,@_cCodigo,@_cLoja,@_nOpc)
        BzApiForB(_cCpj,@_cCodigo,@_cLoja,@_nOpc)    
    EndIf
Else
    BzApiForB(_cCpj,@_cCodigo,@_cLoja,@_nOpc)
EndIf

RestArea(_aArea)
Return Nil 

/***********************************************************************************/
/*/{Protheus.doc} BzApiForA
    @description Consulta Fornecedor pelo ID Astrein 
    @type  Static Function
    @author Bernard M. Margarido
    @since 06/11/2020
/*/
/***********************************************************************************/
Static Function BzApiForA(_cIdAstrein,_cCodigo,_cLoja,_nOpc)
Local _lRet     := .T.
Local _cAlias   := ""
Local _cQuery   := ""

_cQuery := " SELECT " + CRLF
_cQuery += "	A2_COD, " + CRLF 
_cQuery += "	A2_LOJA " + CRLF
_cQuery += " FROM " + CRLF
_cQuery += "	" + RetSqlName("SA2") + " " + CRLF 
_cQuery += " WHERE " + CRLF
_cQuery += "	A2_FILIAL = '" + xFilial("SA2") + "' AND " + CRLF
_cQuery += "	A2_XIDASTR = '" + _cIdAstrein + "' AND " + CRLF
_cQuery += "	D_E_L_E_T_ = '' " + CRLF

_cAlias := MPSysOpenQuery(_cQuery)

_lRet       := IIF(EmptY((_cAlias)->A2_COD),.F.,.T.)
_nOpc       := IIF(EmptY((_cAlias)->A2_COD), 3, 4)
_cCodigo    := IIF(EmptY((_cAlias)->A2_COD), "", (_cAlias)->A2_COD)
_cLoja      := IIF(EmptY((_cAlias)->A2_LOJA), "", (_cAlias)->A2_LOJA)

(_cAlias)->( dbCloseArea() )
Return _lRet 

/***********************************************************************************/
/*/{Protheus.doc} BzApiForB
    @description Consulta Fornecedor pelo CNPJ
    @type  Static Function
    @author Bernard M. Margarido
    @since 06/11/2020
/*/
/***********************************************************************************/
Static Function BzApiForB(_cCnpj,_cCodigo,_cLoja,_nOpc)

dbSelectArea("SA2")
SA2->( dbSetOrder(3) )
//---------------+
// Localiza CNPJ |
//---------------+
If SA2->( dbSeek(xFilial("SA2") + _cCnpj))
    BzApiForC(_cCnpj,@_cCodigo,@_cLoja,@_nOpc)
//--------------------+
// Localiza Raiz CNPJ |
//--------------------+    
ElseIf SA2->( dbSeek(xFilial("SA2") + SubStr(_cCnpj,1,8)))
    BzApiForC(SubStr(_cCnpj,1,8),@_cCodigo,@_cLoja,@_nOpc)
//--------------+
// Novo Cliente |
//--------------+
Else
    _nOpc   := 3
    _cCodigo:= GetSxeNum("SA2","A2_COD")
    _cLoja  := "0000"
    SA2->( dbSetOrder(1) )
    While SA2->( dbSeek(xFilial("SA2") + _cCodigo + _cLoja) )
        ConfirmSx8()
        _cCodigo := GetSxeNum("SA2","A2_COD","",1)
    EndDo	
EndIf

Return Nil

/***********************************************************************************/
/*/{Protheus.doc} BzApiForC
    @description Realiza a consulta dos CNPJ's 
    @type  Static Function
    @author Bernard M. Margarido
    @since 06/11/2020
/*/
/***********************************************************************************/
Static Function BzApiForC(_cCnpj,_cCodigo,_cLoja,_nOpc)
Local _cQuery   := ""

_cQuery := " SELECT " + CRLF
_cQuery += "	A2_COD CODIGO, " + CRLF
_cQuery += "	MAX(A2_LOJA) LOJA, " + CRLF
_cQuery += "	COUNT(*) TOTAL " + CRLF	
_cQuery += " FROM " + CRLF
_cQuery += "	" + RetSqlName("SA2") + " " + CRLF
_cQuery += " WHERE " + CRLF
_cQuery += "	A2_FILIAL = '" + xFilial("SA2") + "' AND " + CRLF

If Len(RTrim(_cCnpj)) < 14
    _cQuery += "	A2_CGC LIKE '%" + _cCnpj + "%' AND " + CRLF
Else
    _cQuery += "	A2_CGC = '" + _cCnpj + "' AND " + CRLF
EndIf
_cQuery += "	D_E_L_E_T_ = '' " + CRLF
_cQuery += " GROUP BY A2_COD "

_cAlias     := MPSysOpenQuery(_cQuery)

_cCodigo    := (_cAlias)->CODIGO
_cLoja      := Soma1((_cAlias)->LOJA)
_nOpc       := 3

(_cAlias)->( dbCloseArea() )

Return Nil 

/***********************************************************************************/
/*/{Protheus.doc} BzApi01CQry
    @description Realiza a consulta dos dados pendentes de processamento
    @type  Static Function
    @author Bernard M. Margarido
    @since 03/11/2020
/*/
/***********************************************************************************/
Static Function BzApi01CQry(_cIdProc,_cAlias)
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
_cQuery += "	Z12_STPROC IN('1','3') AND " + CRLF
_cQuery += "	D_E_L_E_T_ = '' "

_cAlias := MPSysOpenQuery(_cQuery)

If (_cAlias)->( Eof() )
    (_cAlias)->( dbCloseArea() )
    Return .F.
Endif

Return .T.

/*************************************************************************************/
/*/{Protheus.doc} ErroAuto
    @description Tratamento da mensagem de erro ExecAuto 
    @type  Static Function
    @author Bernard M. Margarido
    @since 24/11/2020
/*/
/*************************************************************************************/
Static Function ErroAuto(_aErroAuto,_cMsgLog)
Local _lHelp    := .F.
Local _lTabela  := .F.
Local _lAjuda   := .F.
Local _lHelpMvc := .F.

Local _cLinha   := ""
Local _nX       := 0

For _nX := 1 To Len(_aErroAuto)

	_cLinha  := Upper(_aErroAuto[_nX])
	_cLinha  := StrTran( _cLinha, Chr(13), " " )
	_cLinha  := StrTran( _cLinha, Chr(10), " " )
	
	If SubStr( _cLinha, 1, 4 ) == 'HELP'
		_lHelp := .T.
	EndIf
	
	If SubStr( _cLinha, 1, 6 ) == 'TABELA'
		_lHelp   := .F.
		_lTabela := .T.
	EndIf

    If SubStr( _cLinha, 1, 5 ) == 'AJUDA'
		_lHelp   := .F.
		_lTabela := .F.
        _lAjuda  := .T.
	EndIf

    /*
    If  SubStr(_cLinha,1,82 ) == "  --------------------------------------------------------------------------------"
        _lHelp   := .F.
		_lTabela := .F.
        _lAjuda  := .F.
        _lHelpMvc:= .T.
    EndIf
    */
	If (_lHelp .Or. _lTabela .Or. _lAjuda) 
        If ( '< -- INVALIDO' $ _cLinha )
		    _cMsgLog += _cLinha + CRLF
        EndIf
	EndIf

    If _lHelpMvc 
        _cMsgLog += SubStr(_cLinha,83) + CRLF
    EndIf
	
Next _nX

Return Nil 