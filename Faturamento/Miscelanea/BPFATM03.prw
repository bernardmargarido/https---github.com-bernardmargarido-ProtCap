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

Local _cIdProc  := "0051"

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
                    _cParte1    := dTos(Date()) + StrZero(_nX,3)//Alltrim(StrTran(StrTran(FwTimeStamp(3,Date(),Time()),"-",""),":",""))
                    _cParte2    := Alltrim(IIF(ValType(_oJSon[_nX]['A1_CGC']) <> "U", _oJSon[_nX]['A1_CGC'], IIF(ValType(_oJSon[_nX]['A1_NIF']) <> "U", _oJSon[_nX]['A1_NIF'], "999999")))
                    _cChave     := _cParte1 + _cParte2 //IIF(ValType(_oJSon[_nX]['content_id']) <> "U", _oJSon[_nX]['content_id'], _oJSon[_nX]['A1_CGC'])
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
//Local _cCodMunC	        := ""
Local _cCodMunCo	    := ""
Local _cBco1		    := ""
Local _cCond            := ""
Local _cRisco		    := ""
Local _cRazao           := ""
Local _cInscR           := ""

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
Local _lContinua        := .T.
Local _lValid           := .T.

Local _nX               := 0
Local _nOpcA            := 3
Local _nPDDD            := 0
Local _nPTel            := 0
Local _nPCNae           := 0
Local _nPCep            := 0
Local _nPMailF          := 0
Local _nPMail           := 0
Local _nPCgc            := 0
Local _nPNif            := 0
Local _nPMun            := 0
Local _nPEst            := 0
Local _nPMunE           := 0
Local _nPEstE           := 0
Local _nPCodMun         := 0
Local _nPCodMunE        := 0
Local _nTMun            := TamSx3("A1_MUN")[1]
Local _nTMunE           := TamSx3("A1_MUNE")[1]
Local _nTCgc            := TamSx3("A1_CGC")[1]
Local _nTRazao          := TamSx3("A1_NOME")[1]
Local _nTNReduz         := TamSx3("A1_NREDUZ")[1]
Local _nTCodMun		    := TamSx3("A1_COD_MUN")[1]
Local _nTXCodMun	    := TamSx3("A1_XCDMUNE")[1]
//Local _nTMunC		    := TamSx3("A1_MUNC")[1]	
Local _nTCodMunE       := TamSx3("A1_CODMUNE")[1]

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
        _cCodMun	:= IIF(ValType(_oJSon['A1_COD_MUN']) <> "U", IIF(Len(_oJSon['A1_COD_MUN']) > 5, PadR(SubStr(_oJSon['A1_COD_MUN'],3),_nTCodMun) , PadR(_oJSon['A1_COD_MUN'],_nTCodMun)) , "")
        _cCodMunE	:= IIF(ValType(_oJSon['A1_XCDMUNE']) <> "U", IIF(Len(_oJSon['A1_XCDMUNE']) > 5, PadR(SubStr(_oJSon['A1_XCDMUNE'],3),_nTXCodMun) , PadR(_oJSon['A1_XCDMUNE'],_nTXCodMun)) , "")
        //_cCodMunC	:= IIF(ValType(_oJSon['A1_MUNC']) <> "U", IIF(Len(_oJSon['A1_MUNC']) > 5, PadR(SubStr(_oJSon['A1_MUNC'],3),_nTMunC) , PadR(_oJSon['A1_MUNC'],_nTMunC)) , "")
        _cCodMunEn	:= IIF(ValType(_oJSon['A1_CODMUNE']) <> "U", IIF(Len(_oJSon['A1_CODMUNE']) > 5, PadR(SubStr(_oJSon['A1_CODMUNE'],3),_nTCodMunE) , PadR(_oJSon['A1_CODMUNE'],_nTCodMunE)) , "")
        _cCodMunCo	:= IIF(ValType(_oJSon['A1_XMUNCOM']) <> "U", _oJSon['A1_XMUNCOM'], "")
        _cBco1		:= IIF(ValType(_oJSon['A1_FPAGTO']) <> "U", _oJSon['A1_FPAGTO'], "")
        _cCond      := IIF(ValType(_oJSon['A1_COND']) <> "U", _oJSon['A1_COND'], "")
        _cRisco		:= IIF(ValType(_oJSon['A1_RISCO']) <> "U", _oJSon['A1_RISCO'], "")
        _cInscR     := IIF(ValType(_oJSon['A1_INSCR']) <> "U", _oJSon['A1_INSCR'], "")
        _cCgc       := PadR(_oJSon['A1_CGC'] , _nTCgc)
        _cNif       := IIF(ValType(_oJSon['A1_NIF']) <> "U", _oJSon['A1_NIF'], _cCgc)
        _cRazao     := IIF(ValType(_oJSon['A1_NOME']) <> "U", _oJSon['A1_NOME'], "")
        
        _nOpcA      := 3
        _nLC        := IIF(ValType(_oJSon['A1_LC']) <> "U", Val(_oJSon['A1_LC']), 0)

        _dVencLC    := DaySum(Date(),90)

        _lContinua  := .T.
        _lEx        := IIF(_oJSon['A1_TIPO'] == "X", .T., .F.) 

        _aCliente   := {}

        //-------------------------+
        // SA1 - Posiciona Cliente |
        //-------------------------+
        If !BPFATM03D(_cCgc,_cRazao,_cInscR,_lEx,@_cCodigo,@_cLoja,@_lInclui,@_nOpcA)
            BPFATM03E(_cCgc,_cRazao,_cInscR,_lEx,@_cCodigo,@_cLoja,@_lInclui,@_nOpcA)
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
        
        //----------------------------------------+
        // Valida se ocorreu erro em alguma regra |
        //----------------------------------------+
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

                    If _lEx .And. RTrim(_aStruct[_nX][1]) == "A1_COD_MUN"
                        Loop 
                    EndIf 

                    _cCpo   := _aStruct[_nX][1]
                    _lValid := .F.
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
                                ElseIf _aStruct[_nX][1] == "A1_EST" 
                                    _xRet   := Upper(_oJSon[_cCpo])
                                    _lValid := IIF(_lEx, .T., .F.)
                                ElseIf _aStruct[_nX][1] == "A1_COD_MUN" 
                                    _xRet   := _cCodMun
                                    _lValid := IIF(_lEx, .T., .F.)
                                ElseIf _aStruct[_nX][1] == 'A1_XCDMUNE'
                                    _xRet := _cCodMunE
                                ElseIf _aStruct[_nX][1] == 'A1_CODMUNE'
                                    _xRet := _cCodMunEn
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
                            aAdd(_aCliente, { _aStruct[_nX][1], _xRet, IIF(_lValid, ".T.", Nil) })
                        EndIf
                    EndIf
                EndIf
            Next _nX

            //-------------------------+
            // Adiciona dados iniciais |
            //-------------------------+
            aAdd(_aCliente, { "A1_COD"		    , _cCodigo  	                                                                    , Nil })
            aAdd(_aCliente, { "A1_LOJA"		    , _cLoja		                                                                    , Nil })
            aAdd(_aCliente, { "A1_CGC"          , _cCgc                                                                             , Nil })
            aAdd(_aCliente, { "A1_PESSOA"       , IIF(Len(RTrim(_cCgc)) < 14, "F", "J")                                             , Nil })

            //----------------------------+
            // Valida campos de Municipio |
            //----------------------------+
            _nPMun      := aScan(_aCliente,{|x| RTrim(x[1]) == "A1_MUN"})
            _nPEst      := aScan(_aCliente,{|x| RTrim(x[1]) == "A1_EST"})
            _nPMunE     := aScan(_aCliente,{|x| RTrim(x[1]) == "A1_MUNE"})
            _nPEstE     := aScan(_aCliente,{|x| RTrim(x[1]) == "A1_ESTE"})
            _nPCodMun   := aScan(_aCliente,{|x| RTrim(x[1]) == "A1_COD_MUN"})
            _nPCodMunE  := aScan(_aCliente,{|x| RTrim(x[1]) == "A1_XCDMUNE"})

            If _nPCodMun > 0 .And. _nPEst > 0
                If _nPMun > 0
                    CC2->(dbSetOrder(1))
                    If CC2->(dbSeek(xFilial("CC2") + _aCliente[_nPEst][2] + _aCliente[_nPCodMun][2]))
                        _aCliente[_nPMun][2] := PadR(CC2->CC2_MUN,_nTMun)
                    Endif 
                Else     
                    CC2->(dbSetOrder(1))
                    If CC2->(dbSeek(xFilial("CC2") + _aCliente[_nPEst][2] + _aCliente[_nPCodMun][2]))
                        aAdd(_aCliente, { "A1_MUN"  , PadR(CC2->CC2_MUN,_nTMun)                                                    , Nil })
                    EndIf 
                EndIf 
            EndIf 

            If _nPCodMunE > 0 .And. _nPEstE > 0 
                If _nPMunE > 0 
                    CC2->(dbSetOrder(1))
                    If CC2->(dbSeek(xFilial("CC2") + _aCliente[_nPEstE][2] + _aCliente[_nPCodMunE][2]))
                        _aCliente[_nPMunE][2] := PadR(CC2->CC2_MUN,_nTMunE)
                    Endif 
                Else     
                    CC2->(dbSetOrder(1))
                    If CC2->(dbSeek(xFilial("CC2") + _aCliente[_nPEstE][2] + _aCliente[_nPCodMunE][2]))
                        aAdd(_aCliente, { "A1_MUNE"  , PadR(CC2->CC2_MUN,_nTMunE)                                                    , Nil })
                    EndIf 
                EndIf 
            EndIf 

            //--------------------------------+
            // Valida campos para Estrangeiro |
            //--------------------------------+
            If _lInclui .And. _lEx

                _nPCgc  := aScan(_aCliente,{|x| RTrim(x[1]) == "A1_CGC"})
                _nPNif  := aScan(_aCliente,{|x| RTrim(x[1]) == "A1_NIF"})
                _nPDDD  := aScan(_aCliente,{|x| RTrim(x[1]) == "A1_DDD"})
                _nPTel  := aScan(_aCliente,{|x| RTrim(x[1]) == "A1_TEL"})
                _nPCNae := aScan(_aCliente,{|x| RTrim(x[1]) == "A1_CNAE"})
                _nPCep  := aScan(_aCliente,{|x| RTrim(x[1]) == "A1_CEP"})
                _nPMailF:= aScan(_aCliente,{|x| RTrim(x[1]) == "A1_MAILFIN"})
                _nPMail := aScan(_aCliente,{|x| RTrim(x[1]) == "A1_EMAIL"})
                _nPCodM := aScan(_aCliente,{|x| RTrim(x[1]) == "A1_COD_MUN"})

                If _nPNif > 0 .And. Empty(_aCliente[_nPNif][2])
                    _aCliente[_nPNif][2]   := _aCliente[_nPCgc][2]
                Else 
                    aAdd(_aCliente, { "A1_NIF" , _aCliente[_nPCgc][2]   , Nil })
                EndIf 

                If _nPCgc > 0 
                    _aCliente[_nPCgc][2]   := ""
                Else 
                    aAdd(_aCliente, { "A1_CGC" , ""     , Nil })
                EndIf 

                If _nPDDD > 0 .And. Empty(_aCliente[_nPDDD][2])
                    _aCliente[_nPDDD][2]   := "000"
                Else 
                    aAdd(_aCliente, { "A1_DDD" , "000"  , Nil })
                EndIf 

                 If _nPTel > 0 .And. Empty(_aCliente[_nPTel][2])
                    _aCliente[_nPTel][2]   := "00000000"
                Else 
                    aAdd(_aCliente, { "A1_TEL" , "00000000" , Nil })
                EndIf 

                 If _nPCNae > 0 .And. Empty(_aCliente[_nPCNae][2])
                    _aCliente[_nPCNae][2]   := "9609-2/04"
                Else 
                    aAdd(_aCliente, { "A1_CNAE" , "9609-2/04"   , Nil })
                EndIf 

                 If _nPCep > 0 .And. Empty(_aCliente[_nPCep][2])
                    _aCliente[_nPCep][2]   := "00000000"
                Else 
                    aAdd(_aCliente, { "A1_CEP"  , "00000000"    , Nil })
                EndIf 

                If _nPMailF > 0 .And. Empty(_aCliente[_nPMailF][2])
                    _aCliente[_nPMailF][2]   := _aCliente[_nPMail][2]
                Else 
                    aAdd(_aCliente, { "A1_MAILFIN" , _aCliente[_nPMail][2]  , Nil })
                EndIf 

                If _nPCodM > 0 .And. Empty(_aCliente[_nPCodM][2])
                    _aCliente[_nPCodM][2]   := "99999"
                Else 
                    aAdd(_aCliente, { "A1_COD_MUN" , "99999"  , Nil })
                EndIf 

            EndIf 

            If _nOpcA == 3
                aAdd(_aCliente, { "A1_XDTINC" , Date()   , Nil }) //Data de Inclusão do cadastro
                aAdd(_aCliente, { "A1_XUSRINC", "SSA-CAD", Nil }) //Nome de Inclusão do cadastro
            Else
                aAdd(_aCliente, { "A1_XDTALT" , Date()   , Nil }) //Data de Alteração do cadastro
                aAdd(_aCliente, { "A1_XUSRALT", "SSA-CAD", Nil }) //Nome de Alteração do cadastro
            EndIf

            //-------------------------------+    
            // Realiza a gravação do Cliente |
            //-------------------------------+
            If _lInclui .And. _lEx
                _aCliente   := FWVetByDic( _aCliente, "SA1" )
            EndIf
            
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

                    If Len(_aCarteira) > 0
                    
                        ZA6->(dbGoTo(_aCarteira[1]))
                        _cMsgLog := BPFATM03J(_cCodigo,_cLoja)

                        If !Empty(_cCartExec)
                            ZA6->(dbGoTo(_aCartExec[1]))
                            _cMsgLog := BPFATM03J(_cCodigo,_cLoja)
                        EndIf
                    EndIf

                    If Empty(_cGrpVen)
                        u_BZGRPVEN(_cCodigo,_cLoja,_cCanal,_cClasse,_cSClasse)
                    EndIf

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
Static Function BPFATM03D(_cCgc,_cRazao,_cInscR,_lEx,_cCodigo,_cLoja,_lInclui,_nOpcA)
Local _lRet     := .T.
Local _cAlias   := ""
Local _cQuery   := ""

_cQuery := " SELECT " + CRLF
_cQuery += "	A1_COD, " + CRLF
_cQuery += "	A1_LOJA, " + CRLF
_cQuery += "	A1_INSCR " + CRLF
_cQuery += " FROM " + CRLF
_cQuery += "	" + RetSqlName("SA1") + " " + CRLF
_cQuery += " WHERE " + CRLF
_cQuery += "	A1_FILIAL = '" + xFilial("SA1") + "' AND " + CRLF
If _lEx
    _cQuery += "	A1_NOME LIKE '%" + _cRazao + "%' AND " + CRLF
Else 
    _cQuery += "	A1_CGC = '" + _cCgc + "' AND " + CRLF
    _cQuery += "	A1_INSCR = '" + _cInscR + "' AND " + CRLF
EndIf 

_cQuery += "	D_E_L_E_T_ = '' " + CRLF

_cAlias := MPSysOpenQuery(_cQuery)

_lRet       := IIF(Empty((_cAlias)->A1_COD),.F.,.T.)
_nOpcA      := IIF(Empty((_cAlias)->A1_COD), 3, 4)
_lInclui    := IIF(Empty((_cAlias)->A1_COD),.T.,.F.)
_cCodigo    := IIF(Empty((_cAlias)->A1_COD), "", (_cAlias)->A1_COD)
_cLoja      := IIF(Empty((_cAlias)->A1_LOJA), "", (_cAlias)->A1_LOJA)


(_cAlias)->( dbCloseArea() )

Return _lRet 

/***********************************************************************************/
/*/{Protheus.doc} BPFATM03E
@description Valida codigo e Loja do Cliente
@type  Static Function
@author Bernard M. Margarido
@since 03/11/2020
/*/
/***********************************************************************************/
Static Function BPFATM03E(_cCgc,_cRazao,_cInscR,_lEx,_cCodigo,_cLoja,_lInclui,_nOpcA)
Local _aArea    := GetArea()

dbSelectArea("SA1")
SA1->( dbSetOrder(3) )

//--------------------+
// Localiza Raiz CNPJ |
//--------------------+
If SA1->( dbSeek(xFilial("SA1") + _cCgc) )
    BPFATM03F(_cCgc,@_cCodigo,@_cLoja,@_lInclui,@_nOpcA)
ElseIf SA1->( dbSeek(xFilial("SA1") + SubStr(_cCgc,1,8))) .And. !_lEx
    BPFATM03F(SubStr(_cCgc,1,8),@_cCodigo,@_cLoja,@_lInclui,@_nOpcA)
//--------------+
// Novo Cliente |
//--------------+
Else
    _lInclui    := .T.
    _nOpcA      := 3
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
Static Function BPFATM03F(_cCgc,_cCodigo,_cLoja,_lInclui,_nOpcA)

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

If Len(RTrim(_cCgc)) < 14
    _cQuery += "	A1_CGC LIKE '%" + _cCgc + "%' AND " + CRLF
Else
    _cQuery += "	A1_CGC = '" + _cCgc + "' AND " + CRLF
EndIf

If !Empty(_cCodigo)
    _cQuery += " A1_COD = '" + _cCodigo + "' AND " + CRLF
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
_nOpcA      := 3

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

/*************************************************************************************/
/*/{Protheus.doc} ErroAuto
@description Grava cliente na carteira
@type  Static Function
@author Victor Dessunte
@since 24/11/2020/
/*/
/*************************************************************************************/

Static Function BPFATM03J(_cCodigo,_cLoja)

	Local lRet		:= .F.
	Local oModel	:= Nil
	Local cMsgErro

	Local cFilAux	:= cFilAnt

	cFilant := ZA6->ZA6_FILIAL

	oModel := FwLoadModel("PROMA148")
	oModel:SetOperation(MODEL_OPERATION_INSERT)
	oModel:Activate()

	oModel:SetValue("ZA7MASTER"	, "ZA7_CLIENT"	, _cCodigo	)
	oModel:SetValue("ZA7MASTER"	, "ZA7_LOJA"	, _cLoja	)

	If oModel:VldData()
		lRet := oModel:CommitData()
	EndIf

	If !lRet
		cMsgErro := MODXERR(oModel)
	EndIf

	oModel := FwModelActive()

	If ValType(oModel) <> "U"
		oModel:Destroy()
		FreeObj(oModel)
	EndIf

	cFilAnt := cFilAux

Return cMsgErro
