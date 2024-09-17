#INCLUDE "TOTVS.CH"

/************************************************************************************/
/*/{Protheus.doc} BPFATM03
    @description Realiza a integração de clientes 4MDG
    @type  Function
    @author Bernard M Margarido
    @since 08/07/2024
    @version version
/*/
/************************************************************************************/
User Function BPFATM03()
Local _aArea    := GetArea() 

Local _cIdProc  := "0049"

Local _nTentat  := 0

Private _aMsg   := {}

//-----------------------------+
// Z10 - Rotinas de Integracao |
//-----------------------------+
dbSelectArea("Z10")
Z10->( dbSetOrder(1) )
If !Z10->( dbSeek(xFilial("Z10") + PadR(_cIdProc,TamSx3("Z10_ID")[1])))
    Return Nil 
EndIf 

_nTentat := Z10->Z10_TENTAT

//------------------------+
// Integra novos clientes |
//------------------------+
BPFATM03A(_cIdProc)

//-------------------------------------+
// Cria / Atualiza novos clientes 4MDG |
//-------------------------------------+
BPFATM03B(_cIdProc,_nTentat)

//-----------------------------------------+
// Envia atualização de clientes para 4MDG |
//-----------------------------------------+
If Len(_aMsg)
    U_BPFATM04("SA1",_aMsg)
EndIf 

RestArea(_aArea)
Return Nil 

/************************************************************************************/
/*/{Protheus.doc} BPFATM03A
    @description Integra novos clientes 4MDG
    @type  Static Function
    @author Bernard M Margarido
    @since 08/07/2024
    @version version
/*/
/************************************************************************************/
Static Function BPFATM03A(_cIdProc)
Local _nX       := 0

Local _cChave   := ""
Local _cError   := ""
Local _cStatus  := "1"

Local _o4MDG    := BS4MDG():New()
Local _oJSon    := Nil 
Local _oJSonCli := Nil 

//---------------------------------+
// Realiza a consulta dos clientes |
//---------------------------------+
_o4MDG:cPorta   := "6069"
If _o4MDG:GetCliente()
    _oJSon := JSonObject():New()
    _oJSon:FromJson(_o4MDG:cJSonRet)
    If ValType(_oJSon) <> "U"
        If ValType(_oJSon) == "A" .Or. ValType(_oJSon) == "J"
            For _nX := 1 To Len(_oJSon)
                If ValType(_oJSon[_nX]['A1_CGC']) <> "U" .Or. (  ValType(_oJSon[_nX]['A1_CGC']) == "U" .And. _oJSon[_nX]['A1_TIPO'] <> 'X' )

                    //--------------+
                    // Atualiza Z12 |
                    //--------------+
                    _cChave     := FwTimeStamp(3,Date())   //IIF(ValType(_oJSon[_nX]['content_id']) <> "U", _oJSon[_nX]['content_id'], _oJSon[_nX]['A1_CGC'])
                    _oJSonCli   := JSonObject():New()
                    _oJSonCli   := _oJSon[_nX]
                    U_BzApi01d(_cIdProc,_cChave,_oJSonCli:ToJson(),_cError,_cStatus,Nil,.T.)

                EndIf 
            Next _nX 
        EndIf 
    EndIf 
Else
    _lContinua  := .F.
    aAdd(_aEmail,{"",'ERROR: ' + _o4MDG:cError})
EndIf

FreeObj(_oJSon)
FreeObj(_oJSonCli)
FreeObj(_o4MDG)

Return .T.

/************************************************************************************/
/*/{Protheus.doc} BPFATM03B
    @description Cria/Atualiza cliente 4MDG
    @type  Static Function
    @author Bernard M Margarido
    @since 08/07/2024
    @version version
/*/
/************************************************************************************/
Static Function BPFATM03B(_cIdProc,_nTentat)
Local _cNotCli		    := SuperGetMv("BZ_ASTNCL",,"")
Local _cCgc             := ""
Local _cCodigo          := ""
Local _cLoja            := ""
Local _cUUID            := ""
Local _cLinha	        := ""	
Local _cError           := ""
Local _cNif             := ""
Local _cAlias           := ""
Local _cPessoa          := ""
Local _cCarteira        := ""
Local _cCartExec        := ""
Local _cCanal           := ""
Local _cClasse          := ""
Local _cSClasse         := ""
Local _cGrpVen          := ""
Local _cCodMunE	        := ""
Local _cCodMunEn        := ""
Local _cEst             := ""
Local _cEste		    := ""
Local _cCodMun	        := ""
Local _cCodMunC	        := ""
Local _cCodMunCo	    := ""
Local _cBco1		    := ""
Local _cCond            := ""
Local _cRisco		    := ""

Local _xRet             := Nil              

Local _dVencLC		    := cToD("  /  /  ")

Local _aStruct          := SA1->( dbStruct() )
Local _aAltera          := {"A1_FILIAL","A1_COD","A1_LOJA","A1_CGC","A1_PESSOA"}
Local _aCliente         := {}
Local _aErro            := {}
Local _aCartExec        := {}

Local _lRet             := .T.
Local _lInclui          := .T.
Local _lEx              := .F.
Local _lLoja            := .F.
Local _lContinua        := .T.

Local _nX               := 0
Local _nOpcA            := 3
Local _nTCgc            := TamSx3("A1_CGC")[1]
Local _nTRazao          := TamSx3("A1_NOME")[1]
Local _nTNReduz         := TamSx3("A1_NREDUZ")[1]

Local _oJSon            := Nil 

Private lMsErroAuto     := .F.
Private lMsHelpAuto 	:= .T.
Private lAutoErrNoFile 	:= .T.
Private l030Auto        := .T.

//------------------------------------------+
// Consulta novos clientes a serem baixados |
//------------------------------------------+
If !BPFATM03C(_cIdProc,_nTentat,@_cAlias)
    Return .T.
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

    _oJSon := JSonObject():New()
    _oJSon:FromJson(RTrim(Z12->Z12_JSON))

    //---------------+
    // Valida objeto |
    //---------------+
    If ValType(_oJSon) <> "U"

        //-----------------------------------+
        // Valida se é inclusão ou alteração | 
        //-----------------------------------+
        _cCodigo	:= ""
        _cLoja      := ""
        _cError     := ""
        _cUUID      := _oJSon['content_id']
        _cCarteira  := IIF(ValType(_oJSon['ZA6_COD_V']) <> "U", _oJSon['ZA6_COD_V'], "")
        _cCartExec  := IIF(ValType(_oJSon['ZA6_COD_E']) <> "U", _oJSon['ZA6_COD_E'], "")
        _cCanal     := IIF(ValType(_oJSon['ACY_XCANAL']) <> "U", _oJSon['ACY_XCANAL'], "")
        _cClasse    := IIF(ValType(_oJSon['ACY_XCLASS']) <> "U", _oJSon['ACY_XCLASS'], "")
        _cSClasse   := IIF(ValType(_oJSon['ACY_XSCLAS']) <> "U", _oJSon['ACY_XSCLAS'], "")
        _cPessoa    := IIF(ValType(_oJSon['A1_PESSOA']) <> "U", _oJSon['A1_PESSOA'], "")
        _cGrpVen    := IIF(ValType(_oJSon['A1_GRPVEN']) <> "U", _oJSon['A1_GRPVEN'], "")
        _cEst       := IIF(ValType(_oJSon['A1_EST']) <> "U", _oJSon['A1_EST'], "")
        _cEste		:= IIF(ValType(_oJSon['A1_ESTE']) <> "U", _oJSon['A1_ESTE'], "")
        _cCodMun	:= IIF(ValType(_oJSon['A1_COD_MUN']) <> "U", IIF(Len(_oJSon['A1_COD_MUN']) > 5, SubStr(_oJSon['A1_COD_MUN'],3) , _oJSon['A1_COD_MUN']) , "")
        _cCodMunE	:= IIF(ValType(_oJSon['A1_XCDMUNE']) <> "U", IIF(Len(_oJSon['A1_XCDMUNE']) > 5, SubStr(_oJSon['A1_XCDMUNE'],3) , _oJSon['A1_XCDMUNE']) , "")
        _cCodMunC	:= IIF(ValType(_oJSon['A1_MUNC']) <> "U", IIF(Len(_oJSon['A1_MUNC']) > 5, SubStr(_oJSon['A1_MUNC'],3) , _oJSon['A1_MUNC']) , "")
        _cCodMunEn	:= IIF(ValType(_oJSon['A1_CODMUNE']) <> "U", IIF(Len(_oJSon['A1_CODMUNE']) > 5, SubStr(_oJSon['A1_CODMUNE'],3) , _oJSon['A1_CODMUNE']) , "")
        _cCodMunCo	:= IIF(ValType(_oJSon['A1_XMUNCOM']) <> "U", _oJSon['A1_XMUNCOM'], "")
        _cBco1		:= IIF(ValType(_oJSon['A1_FPAGTO']) <> "U", _oJSon['A1_FPAGTO'], "")
        _cCond      := IIF(ValType(_oJSon['A1_COND']) <> "U", _oJSon['A1_COND'], "")
        _cRisco		:= IIF(ValType(_oJSon['A1_RISCO']) <> "U", _oJSon['A1_RISCO'], "")
        _cCgc       := PadR(_oJSon['A1_CGC'] , _nTCgc)
        _cNif       := IIF(ValType(_oJSon['A1_NIF']) <> "U", _oJSon['A1_NIF'], "")

        _nLC        := IIF(ValType(_oJSon['A1_LC']) <> "U", Val(_oJSon['A1_LC']), 0)

        _dVencLC    := DaySum(Date(),90)

        _lLoja      := .F.
        _lContinua  := .T.
        _lEx        := IIF(_oJSon['A1_TIPO'] == "X", .T., .F.) 

        _aCliente   := {}

        //-------------------------+
        // SA1 - Posiciona Cliente |
        //-------------------------+
        If _lEx
            BPFATM03D(_cNif,@_lInclui,@_cCodigo,@_cLoja,@_nOpcA)
        Else 
            BPFATM03E(_cCgc,@_cCodigo,@_cLoja,@_lInclui,_cCodigo,@_lLoja)
        EndIf 

        If _cPessoa == 'J' .And. !Empty(_cEste) .And. _cEst <> _cEste
            _cError     := "Estado de faturamento e entrega diferentes para pessoa Juridica não permitido."
            _lContinua  := .F.
		EndIf

        If _lInclui
            
            //----------------------------+
            // Valida carteira do cliente |
            //----------------------------+
            If Empty(_cCarteira) .And. _lContinua
                _cError     := "Codigo da carteira nao preenchido."
                _lContinua  := .F.
            Else
                _aCarteira := BPFATM03I(_cCarteira)
                If Len(_aCarteira) > 0 .And. _lContinua
                    ZA6->(dbGoTo(_aCarteira[1]))
                    
                    If ZA6->ZA6_COD <> _cCarteira
                        _cError     := "Codigo da carteira nao localizado."
                        _lContinua  := .F.
                    EndIf

                    If !(_aCarteira[2] $ '01*02') .And. _lContinua
                        _cError     := "Carteira não pertence ao departamento de vendas de clientes."
                        _lContinua  := .F.
                    EndIf

                Else
                    If _lContinua
                        _cError     := "Codigo da carteira nao localizado."
                        _lContinua  := .F.
                    EndIf 
                EndIf
            EndIf 

            //---------------------------------+
            // Validação dos dados da carteira |
            //---------------------------------+
            If !Empty(_cCartExec) .And. _lContinua
                _aCartExec := BPFATM03I(_cCartExec)
                If Len(_aCartExec) > 0
                    ZA6->(dbGoTo(_aCartExec[1]))
                    If ZA6->ZA6_COD <> _cCartExec .And. _lContinua
                        _cError     := "Codigo da carteira executiva nao localizado."
                        _lContinua  := .F.
                    EndIf
                    If _aCartExec[2] <> '04' .And. _lContinua
                        _cError     := "Carteira não pertence ao departamento de vendas de carteira executiva."
                        _lContinua  := .F.
                    EndIf
                Else
                    If _lContinua
                        _cError     := "Codigo da carteira executiva nao localizado."
                        _lContinua  := .F.
                    EndIf 
                EndIf
            EndIf

            If Empty(_cCanal) .And. _lContinua
                _cError     := "Canal não informado."
                _lContinua  := .F.
            EndIf

            If Empty(_cClasse) .And. _lContinua
                _cError     := "Classificação não informada."
                _lContinua  := .F.
            EndIf

            If _cCodigo $ _cNotCli .And. _lContinua
                _cError     := "Raiz de CNPJ pertencente a GR ou Telefônica"
                _lContinua  := .F.
            EndIf 

            If ValType(_oJSon['A1_NOME']) <> 'U' .And. _lContinua
            	If Len(RTrim(_oJSon['A1_NOME'])) > _nTRazao
                    _cError     := "Razão social maior que o disponível no Protheus (" + RTRIM(STR(_nTRazao)) + ")"
                    _lContinua  := .F.
                EndIf
            EndIf

            If _lContinua
                u_PROMM190(_cPessoa, _cEst, _cCgc, _cCodigo, _cLoja, .T., @_cError)
                _lContinua := IIF(Empty(_cError), .T., .F. )
            EndIf

        EndIf 
        
        If !_lContinua

            U_BzApi01d(Z12->Z12_ID,Z12->Z12_CHAVE,Z12->Z12_JSON,_cError,"3",4)
            BPFATM03H(Z12->Z12_ID,Z12->Z12_CHAVE,Z12->Z12_JSON,_cError)

        EndIf 
        

        //----------------------+
        // Valida se é inclusão |
        //----------------------+
        If _lContinua
           //-----------------------+
            // Cria array de cliente |
            //-----------------------+
            For _nX := 1 To Len(_aStruct)
                If ( aScan(_aAltera, {|x| RTrim(x) == RTrim(_aStruct[_nX][1])}) == 0 ) 
                    _cCpo   := _aStruct[_nX][1]
                    If ValType(_oJSon[_cCpo]) <> "U"
                        If !Empty(_oJSon[_cCpo])
                            Do Case
                                Case _aStruct[_nX][2] == "D"
                                    _xRet := cTod(StrTran(_oJSon[_cCpo],"-",""))
                                Case _aStruct[_nX][2] == "N"
                                    If _aStruct[_nX][1] $ "A1_LC"
                                        _xRet := _nLC
                                    Else
                                        _xRet := Val(_oJSon[_cCpo])
                                    EndIf
                                Case _aStruct[_nX][2] == "L"
                                    _xRet := IIF(SubStr(_oJSon[_cCpo],1,1) $ "T/t", .T., .F.)
                            OtherWise
                                If _aStruct[_nX][1] == "A1_TIPO"
                                    _xRet := SubStr(_oJSon[_cCpo],1,1)
                                ElseIf _aStruct[_nX][1] == "A1_NOME"
                                    _xRet := AllTrim(SubStr(u_BzNoAcento(_oJSon[_cCpo]),1,_nTRazao))
                                ElseIf _aStruct[_nX][1] == "A1_NREDUZ"
                                    _xRet := Alltrim(SubStr(u_BzNoAcento(_oJSon[_cCpo]),1,_nTNReduz))
                                    If '*' $ _xRet .Or. Empty(_xRet)
                                        _xRet := AllTrim(u_BzNoAcento(Subst(_oJSon["A1_NOME"],1,20)))
                                    EndIf
                                ElseIf _aStruct[_nX][1] == "A1_SIMPLES"
                                    _xRet := IIF(SubStr(_oJSon[_cCpo],1,1) == "N","2","1")
                                ElseIf _aStruct[_nX][1] == "A1_SIMPNAC"
                                    _xRet := IIF(SubStr(_oJSon[_cCpo],1,1) == "0","1","2")
                                ElseIf _aStruct[_nX][1] == "A1_COD_MUN"
                                    _xRet := _cCodMun
                                ElseIf _aStruct[_nX][1] == 'A1_XCDMUNE'
                                    _xRet := _cCodMunE
                                ElseIf _aStruct[_nX][1] == 'A1_CODMUNE'
                                    _xRet := _cCodMunEn
                                ElseIf _aStruct[_nX][1] == 'A1_MUNC'
                                    _xRet := _cCodMunC
                                ElseIf _aStruct[_nX][1] == "A1_CNAE"
                                    _xRet := StrTran(StrTran(StrTran( _oJSon[_cCpo], "-","" ) , ".","") , "/","")
                                    _xRet  := Transform( _xRet, "@R ####-#/##" )
                                ElseIf _aStruct[_nX][1] == "A1_INSCR"
                                    If UPPER(_oJSon[_cCpo]) == 'ISENTO'
                                        _xRet := 'ISENTO'
                                    Else
                                        _xRet := StrTran(StrTran(StrTran( _oJSon[_cCpo], "-","" ) , ".","") , "/","")
                                    EndIf
                                ElseIf _aStruct[_nX][1] == "A1_END"
                                    _xRet := AllTrim(u_BzNoAcento(_oJSon[_cCpo]))
                                    _xRet := IIF(SubStr(_xRet,Len(_xRet)-3,Len(_xRet)) == "S/N",SubStr(_xRet,1,Len(_xRet)-6),_xRet)
                                ElseIf _aStruct[_nX][1] == "A1_BAIRRO"
                                    _xRet := AllTrim(u_BzNoAcento(_oJSon[_cCpo]))
                                ElseIf _aStruct[_nX][1] == "A1_COMPLEM"
                                    _xRet := AllTrim(u_BzNoAcento(_oJSon[_cCpo]))
                                    If _xRet == "********"
                                        _xRet := ""
                                    EndIf
                                ElseIf _aStruct[_nX][1] == "A1_TEL"
                                    _xRet := u_Numeros(Alltrim(_oJSon[_cCpo]))
                                    If Len(_xRet) >= 10
                                        _xRet := SubStr(_xRet,3,Len(_xRet))
                                    EndIf
                                ElseIf _cPessoa == "F" .AND. _aStruct[_nX][1] == "A1_CNAE"
                                    _xRet := "000000"
                                ElseIf _cPessoa == "F" .AND. _aStruct[_nX][1] == "A1_SATIV1"
                                    _xRet := "4029"
                                ElseIf _aStruct[_nX][1] $ "A1_CEP*A1_CEPC*A1_XCEPCOM*A1_CEPE"
                                    _xRet := u_Numeros(Alltrim(_oJSon[_cCpo]))

                                Else
                                    _xRet := U_BZNoAcento(_oJSon[_cCpo])
                                EndIf
                            EndCase
                            aAdd(_aCliente, { _aStruct[_nX][1], _xRet, Nil })
                        EndIf
                    EndIf
                EndIf
            Next _nX

            aAdd(_aCliente, { "A1_COD"		    , _cCodigo  	                                                                    , Nil })
            aAdd(_aCliente, { "A1_LOJA"		    , _cLoja		                                                                    , Nil })
            aAdd(_aCliente, { "A1_CGC"          , _cCgc                                                                             , Nil })
            aAdd(_aCliente, { "A1_PESSOA"       , IIF(Len(RTrim(_cCgc)) < 14, "F", "J")                                             , Nil })

            If _nOpcA == 3
                If aScan(_aCliente, {|x| Rtrim(x[1]) == "A1_TIPO"}) == 0 
                    aAdd(_aCliente, { "A1_TIPO" , "F"   , Nil }) 
                EndIf 
                aAdd(_aCliente, { "A1_XDTINC" , Date()   , Nil }) //Data de Inclusão do cadastro
                aAdd(_aCliente, { "A1_XUSRINC", "SSA-CAD", Nil }) //Nome de Inclusão do cadastro
            Else
                aAdd(_aCliente, { "A1_XDTALT" , Date()   , Nil }) //Data de Alteração do cadastro
                aAdd(_aCliente, { "A1_XUSRALT", "SSA-CAD", Nil }) //Nome de Alteração do cadastro
            EndIf

            //-------------------------------+    
            // Realiza a gravação do Cliente |
            //-------------------------------+
            _aCliente   := FWVetByDic( _aCliente, "SA1" )
            
            lMsErroAuto := .F.

            If MA030IsMVC()
                SetFunName('CRMA980')
                MSExecAuto( { |x, y| CRMA980(x,y) },  _aCliente, _nOpcA )
            Else
                SetFunName('MATA030')
                MsExecAuto({|x,y| Mata030(x,y)}, _aCliente, _nOpcA)
            EndIf 

            //---------------------+
            // Erro na Atualização |
            //---------------------+
            If lMsErroAuto
                
                RollBackSx8()

                _cLinha	    := ""	
                _cError     := ""
                
                _lRet       := .F.

                _aErro	    := {}
                _aErro 	    := GetAutoGrLog()

                For _nX := 1 To Len(_aErro)
                    _cLinha := _aErro[_nX]
                    _cLinha  := StrTran( _cLinha, Chr(13), " " )
                    _cLinha  := StrTran( _cLinha, Chr(10), " " )

                    If SubStr( _cLinha, 1, 4 ) == 'HELP'
                        _cError += _cLinha + "|"
                    EndIf

                    If SubStr( _cLinha, 1, 6 ) == 'TABELA'
                        _cError += _cLinha + "|"
                    EndIf

                    If SubStr( _cLinha, 1, 5 ) == 'AJUDA'
                        _cError += _cLinha + " | "
                    EndIf

                    If At("< -- Invalido", _aErro[_nX] ) > 0
                        _cError += _aErro[_nX]  + " | "
                    EndIf

                Next _nX

                If Empty(_cError)
                    For _nX := 1 To Len(_aErro)
                        _cLinha := _aErro[_nX]
                        _cLinha  := StrTran( _cLinha, Chr(13), " " )
                        _cLinha  := StrTran( _cLinha, Chr(10), " " )
                         _cError += _cLinha + "|"
                    Next _nX
                EndIf 

                U_BzApi01d(Z12->Z12_ID,Z12->Z12_CHAVE,Z12->Z12_JSON,_cError,"3",4)
                BPFATM03H(Z12->Z12_ID,Z12->Z12_CHAVE,Z12->Z12_JSON,_cError)

            Else

                ConfirmSX8() 

                _lRet       := .T.
                _cError     := "Cliente incluido/atualizado com sucesso."

                U_BzApi01d(Z12->Z12_ID,Z12->Z12_CHAVE,Z12->Z12_JSON,"","2",4)
                BPFATM03H(Z12->Z12_ID,Z12->Z12_CHAVE,Z12->Z12_JSON,_cError)
                
                If _nOpcA == 3
                    BPFATM03G(_cCodigo,_cLoja,_nLC,_dVencLC,_cRisco)
                EndIf 

            EndIf 

            //--------------+
            // Array de LOG |
            //--------------+
            aAdd(_aMsg,{_lRet,_cUUID,_cError,IIF(_lEx,_cNif,_cCgc), _lEx})
        EndIf 
    EndIf 

    (_cAlias)->( dbSkip() )

EndDo 

(_cAlias)->( dbCloseArea() )

Return Nil 

/************************************************************************************/
/*/{Protheus.doc} BPFATM03C
    @description Consulta clientes na fila de integração 
    @type  Static Function
    @author Bernard M Margarido
    @since 11/09/2024
    @version version
/*/
/************************************************************************************/
Static Function BPFATM03C(_cIdProc,_nTentat,_cAlias)
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
_cQuery += "	Z12_STPROC = '1' AND " + CRLF

If _nTentat > 0
    _cQuery += "	Z12_TENTAT <= " +Alltrim(Str(_nTentat))+ " AND " + CRLF
EndIf

_cQuery += "	D_E_L_E_T_ = '' "

_cAlias := MPSysOpenQuery(_cQuery)

If (_cAlias)->( Eof() )
    (_cAlias)->( dbCloseArea() )
    Return .F.
EndIf 
        
Return .T.
/************************************************************************************/
/*/{Protheus.doc} BPFATM03D
    @description Consulta cliente estrangeiro
    @type  Static Function
    @author Bernard M Margarido
    @since 07/08/2024
    @version version
/*/
/************************************************************************************/
Static Function BPFATM03D(_cNIF,_lInclui,_cCodigo,_cLoja,_nOpcA)
Local _cQuery := ""
Local _cAlias := ""

_cQuery := " SELECT " + CRLF
_cQuery += "	A1_COD, " + CRLF
_cQuery += "	A1_LOJA, " + CRLF
_cQuery += "	A1_CGC " + CRLF 
_cQuery += " FROM " + CRLF
_cQuery += "	" + RetSqlName("SA1") + " " + CRLF
_cQuery += " WHERE " + CRLF
_cQuery += "	A1_FILIAL = '" + xFilial("SA1") + "' AND " + CRLF
_cQuery += "	A1_NIF = '" + _cNIF + "' AND " + CRLF
_cQuery += "	D_E_L_E_T_ = '' " 

_cAlias := MPSysOpenQuery(_cQuery)

_lInclui    := .T.
_cCodigo    := ""
_cLoja      := ""
_nOpcA      := 3

If !Empty((_cAlias)->A1_COD)
    _lInclui    := .F.
    _cCodigo    := (_cAlias)->A1_COD
    _cLoja      := (_cAlias)->A1_LOJA
    _nOpcA      := 4
EndIf 

(_cAlias)->( dbCloseArea() )	

Return Nil 

/***********************************************************************************/
/*/{Protheus.doc} BPFATM03E
@description Valida codigo e Loja do Cliente
@type  Static Function
@author Bernard M. Margarido
@since 03/11/2020
/*/
/***********************************************************************************/
Static Function BPFATM03E(_cCnpj,_cCodigo,_cLoja,_lInclui,_cCodCli,_lLoja)
Local _aArea    := GetArea()

dbSelectArea("SA1")
SA1->( dbSetOrder(3) )

//---------------+
// Localiza CNPJ |
//---------------+
If SA1->( dbSeek(xFilial("SA1") + _cCnpj))
    BPFATM03F(_cCnpj,@_cCodigo,@_cLoja,@_lInclui,_cCodCli,@_lLoja)
//--------------------+
// Localiza Raiz CNPJ |
//--------------------+
ElseIf SA1->( dbSeek(xFilial("SA1") + SubStr(_cCnpj,1,8)))
    BPFATM03F(SubStr(_cCnpj,1,8),@_cCodigo,@_cLoja,@_lInclui,_cCodCli,@_lLoja)
//--------------+
// Novo Cliente |
//--------------+
Else
    
    _lInclui    := .T.
    _cCodigo    := GetSxeNum("SA1","A1_COD")
    _cLoja      := "0000"

    SA1->( dbSetOrder(1) )
    While SA1->( dbSeek(xFilial("SA1") + _cCodigo + _cLoja) )
        ConfirmSx8()
        _cCodigo := GetSxeNum("SA1","A1_COD","",1)
    EndDo
EndIf


RestArea(_aArea)
Return Nil

/***********************************************************************************/
/*/{Protheus.doc} BPFATM03F
@description Realiza a consulta dos CNPJ's
@type  Static Function
@author Bernard M. Margarido
@since 06/11/2020
/*/
/***********************************************************************************/
Static Function BPFATM03F(_cCnpj,_cCodigo,_cLoja,_lInclui,_cCodCli,_lLoja)

Local _cQuery   := ""

//--------------------------------------+
// Valida cliente pelo CNPJ / CNPJ RAIZ |
//--------------------------------------+
_cQuery := " SELECT " + CRLF
_cQuery += "	A1_COD CODIGO, " + CRLF
_cQuery += "	MAX(A1_LOJA) LOJA, " + CRLF
_cQuery += "	COUNT(*) TOTAL " + CRLF
_cQuery += " FROM " + CRLF
_cQuery += "	" + RetSqlName("SA1") + " " + CRLF
_cQuery += " WHERE " + CRLF
_cQuery += " A1_FILIAL = '" + xFilial("SA1") + "' AND " + CRLF
_cQuery += " A1_LOJA   <> 'RENT' AND " + CRLF

If Len(RTrim(_cCnpj)) < 14
    _cQuery += "	A1_CGC LIKE '%" + _cCnpj + "%' AND " + CRLF
Else
    _cQuery += "	A1_CGC = '" + _cCnpj + "' AND " + CRLF
EndIf

If !Empty(_cCodCli)
    _cQuery += " A1_COD = '" + _cCodCli + "' AND " + CRLF
EndIf

_cQuery += "	D_E_L_E_T_ = '' " + CRLF
_cQuery += " GROUP BY A1_COD "

_cAlias     := MPSysOpenQuery(_cQuery)

_cCodigo    := (_cAlias)->CODIGO

(_cAlias)->( dbCloseArea() )

//-------------------------------------------------+
// Caso seja CNPJ RAIZ cria proximo codigo de loja |
//-------------------------------------------------+
_cQuery := " SELECT " + CRLF
_cQuery += "	MAX(A1_LOJA) LOJA " + CRLF
_cQuery += " FROM " + CRLF
_cQuery += "	" + RetSqlName("SA1") + " " + CRLF
_cQuery += " WHERE " + CRLF
_cQuery += "	A1_FILIAL = '" + xFilial("SA1") + "' AND " + CRLF
_cQuery += "	A1_COD = '" + _cCodigo + "' AND " + CRLF
_cQuery += "	A1_LOJA <> 'RENT' AND " + CRLF
_cQuery += "	D_E_L_E_T_ = '' " + CRLF

_cAlias     := MPSysOpenQuery(_cQuery)

_cLoja      := Soma1((_cAlias)->LOJA)
_lInclui    := .T.
_lLoja      := .T.

(_cAlias)->( dbCloseArea() )

Return Nil

/*************************************************************************************************************************************/
/*/{Protheus.doc} BPFATM03G
    @description Quando incluir um cliente novo (codogo e loja), grava o historico do limite de credito na tabela de historico SZ6
    @type  Static Function
    @author Tanimoto
    @since 30/08/2021
    @version version
/*/
/*************************************************************************************************************************************/
Static Function BPFATM03G(_cCodigo,_cLoja,_nLC,_dVencLC,_cRisco)
    
    RecLock("ZE1", .T.)
        ZE1->ZE1_FILIAL := xFilial("ZE1")
        ZE1->ZE1_CLIENT := _cCodigo
        ZE1->ZE1_LOJA   := _cLoja
        ZE1->ZE1_DATA   := Date()
        ZE1->ZE1_USUARI := 'SSA-CAD'
        ZE1->ZE1_LC     := _nLC
        ZE1->ZE1_VENCLC := _dVencLC
        ZE1->ZE1_RISCO  := _cRisco
	ZE1->( MsUnLock() )

Return Nil 

/***********************************************************************************/
/*/{Protheus.doc} BPJOB01C
    @description Atualiza historico monitor
    @type  Static Function
    @author Bernard M. Margarido
    @since 04/11/2020
/*/
/***********************************************************************************/
Static Function BPFATM03H(_cIdProc,_cChave,_cJson,_cError)
	Local _lRet     := .T.

	Local _oMonitor := ProtMonitor():New()

	_oMonitor:cIdProc   := _cIdProc
	_oMonitor:cChave    := _cChave
	_oMonitor:cError    := _cError
	_oMonitor:cJSon     := _cJson

	If _oMonitor:GrvHistorico()
		_lRet     := .T.
	Else
		_lRet     := .F.
	EndIf

Return _lRet

/*************************************************************************************/
/*/{Protheus.doc} BPFATM03I
@description Verifica se existe carteira informada
@type  Static Function
@author Victor Dessunte
@since 24/11/2020/
/*/
/*************************************************************************************/

Static Function BPFATM03I(_cCart)

	Local _cQuery   := ""
	Local _nRec		:= 0
	Local _cDepVen  := ""

	_cQuery := " SELECT " + CRLF
	_cQuery += "    R_E_C_N_O_ RECNOZA6, " + CRLF
	_cQuery += "	ZA6_DEPVEN " + CRLF
	_cQuery += " FROM " + CRLF
	_cQuery += "	" + RetSqlName("ZA6") + " " + CRLF
	_cQuery += " WHERE " + CRLF
	_cQuery += "	ZA6_COD = '" + _cCart + "' AND " + CRLF
	_cQuery += "	D_E_L_E_T_ = '' "

	_cAlias := MPSysOpenQuery(_cQuery)

	If (_cAlias)->(!Eof())
		_nRec 		:= (_cAlias)->RECNOZA6
		_cDepVen 	:= (_cAlias)->ZA6_DEPVEN
	Endif

	(_cAlias)->( dbCloseArea() )

Return {_nRec,_cDepVen}
