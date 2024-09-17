#INCLUDE "TOTVS.CH"
#INCLUDE "FWMVCDEF.CH"

#DEFINE CRLF CHR(13) + CHR(10)

/***********************************************************************************************************/
/*/{Protheus.doc} BPFATM01
    @description Rotina  - Inclui clientes/prospects 4MDG
    @type  Function
    @author Bernard M Margarido
    @since 27/06/2024
    @version version
/*/
/***********************************************************************************************************/
User Function BPFATM01(_cCgc,_lInclui,_lProspect,_lReceita,_lSintegra,_lSimples,_lSuframa)
Local _aArea    := GetArea() 

Local _cError   := ""

Local _lRet     := .F.

Local _oReceita := Nil 
Local _oSintegra:= Nil 
Local _oSimples := Nil 
Local _oSuframa := Nil 

//-------------------------+
// Realiza a consulta 4MDG |
//-------------------------+
_lRet := BPFATM01A(_cCgc,_lInclui,@_lReceita,@_lSintegra,@_lSimples,@_lSuframa,@_oReceita,@_oSintegra,@_oSimples,@_oSuframa,@_cError)

//----------------------------+
// Preenche dados em Prospect |
//----------------------------+
If _lRet
    If _lProspect
        _lRet := BPFATM01B(_cCgc,_lInclui,_lReceita,_lSintegra,_lSimples,_lSuframa,_oReceita,_oSintegra,_oSimples,_oSuframa,@_cError)
    //----------------------------+
    // Preenche dados em Clientes |
    //----------------------------+
    Else 
        _lRet := BPFATM01C(_cCgc,_lInclui,_lReceita,_lSintegra,_lSimples,_lSuframa,_oReceita,_oSintegra,_oSimples,_oSuframa,@_cError)
    EndIf 
EndIf 

If !_lRet .Or. !Empty(_cError)
    /*
    If IsBlind() 
        CoNout('ERROR CLIENTE/PROSPECT ' + _cError)
    Else
        MsgStop("Error: Cliente/Prospect " + CRLF + _cError,"LaborHS - Avisos")
    EndIf 
    */

EndIf 

If Type("_lBsTgv01") <> "U" .And. ValType(_lBsTgv01) == "L"
    _lBsTgv01 := Nil 
EndIf 

RestArea(_aArea)
Return _lRet 

/************************************************************************************************************/
/*/{Protheus.doc} BPFATM01A
    @description Realiza consulta 4MDG
    @type  Static Function
    @author Bernard M Margarido
    @since 27/06/2024
    @version version
/*/
/************************************************************************************************************/
Static Function BPFATM01A(_cCgc,_lInclui,_lReceita,_lSintegra,_lSimples,_lSuframa,_oReceita,_oSintegra,_oSimples,_oSuframa,_cError)
Local _o4MDG        := BS4MDG():New()

Local _cUFSimples   := GetNewPar("BS_UFSIMP","SC")
Local _cUFSuframa   := GetNewPar("BS_UFSUFR","AM/AC/AP/RO/RR")

Local _lConRec      := .T.
Local _lRet         := .F.

Local _oJSon        := Nil 
Local _oJSonRet     := Nil 

//--------------------------------------+
// Cria JSON consulta Receita e Sitegra |
//--------------------------------------+
_oJSon  := Nil 
_oJSon  := JSonObject():New()
_oJSon['services']  := {}

//-------------------------------------+
// Valida servicos a serem consultados |
//-------------------------------------+
If _lReceita .Or. _lInclui
    aAdd(_oJSon['services'],"receita_federal")
EndIf 

If _lSintegra .Or. _lInclui
    aAdd(_oJSon['services'],"sintegra")
EndIf 

If _lSimples 
    aAdd(_oJSon['services'],"simples_nacional")
EndIf 

If _lSuframa
    aAdd(_oJSon['services'],"suframa")
EndIf 

_oJSon['outputs']           := JSonObject():New()
_oJSon['outputs']['cnpj']   := _cCgc

//----------------------------+
// Realiza a consulta na 4MDG |
//----------------------------+
_o4MDG:cJSon    := _oJSon:ToJson()
If _o4MDG:Cliente()
    _cJSon      := _o4MDG:cJSonRet
    _oJSonRet   := Nil 
    _oJSonRet   := JSonObject():New()
    _oJSonRet:FromJson(_cJSon)
    _lRet       := .T.

    //---------+
    // Receita |
    //---------+
    If ( _lReceita .Or. _lInclui ) .And. _lRet
        _oReceita   := _oJSonRet['receita_federal']
        //---------------+
        // Valida receita| 
        //---------------+
        If _oReceita['codigo'] <> '200' 
            _lConRec:= .F.
            _lRet   := .F.
            _cError:= "ERROR: " + _oReceita['mensagem']
        EndIf 
    EndIf 

    //----------+
    // Sintegra |
    //----------+
    If ( _lSintegra .Or. _lInclui ) .And. _lRet
        _oSintegra  := _oJSonRet['sintegra']
        //----------------+
        // Valida sintegra| 
        //----------------+
        If _oSintegra['codigo'] <> '200'
            _lRet   := .F.
            _cError:= "ERROR: " + _oSintegra['mensagem']
        EndIf 
    EndIf 

    //---------------------------------------+
    // Valida se realiza as demais consultas |
    //---------------------------------------+
    If _lRet 
        _cUF := _oReceita['dados']['uf']

        //---------+
        // Simples |
        //---------+
        If _cUF $ _cUFSimples .Or. _lSimples
            _lSimples := .T.
        EndIf 

        //---------+
        // Suframa |
        //---------+
        If _cUF $ _cUFSuframa .Or. _lSuframa
            _lSuframa := .T.
        EndIf  

        //----------------------------+
        // Consulta simples / suframa |
        //----------------------------+
        If _lSimples .Or. _lSuframa
            _lRet  := BPFATM01AB(_cCgc,_lSimples,_lSuframa,@_oSimples,@_oSuframa,@_cError)
        EndIf 
    EndIf 
Else 
    _cError:= _o4MDG:cError
    _lRet  := .F.
EndIf 

FreeObj(_o4MDG)
FreeObj(_oJSon)
FreeObj(_oJSonRet)

Return IIF(_lConRec, .T., _lRet)

/**********************************************************************************************************/
/*/{Protheus.doc} BPFATM01AB
    @description Realiza consulta 4MDG simples nacional ou suframa
    @type  Static Function
    @author Bernard M Margarido
    @since 27/06/2024
    @version version
/*/
/**********************************************************************************************************/
Static Function BPFATM01AB(_cCgc,_lSimples,_lSuframa,_oSimples,_oSuframa,_cError)
Local _lRet         := .T.

Local _cJSon        := ""

Local _o4MDG        := BS4MDG():New()
Local _oJSon        := Nil 
Local _oJSonRet     := Nil 

_oJSon  := Nil 
_oJSon  := JSonObject():New()
_oJSon['services']  := {}
If _lSimples
    aAdd(_oJSon['services'],"simples_nacional")
EndIf 
If _lSuframa
    aAdd(_oJSon['services'],"suframa")
EndIf 
_oJSon['outputs']   := JSonObject():New()
_oJSon['outputs']['cnpj']   := _cCgc

//----------------------------+
// Realiza a consulta na 4MDG |
//----------------------------+
_o4MDG:cJSon    := _oJSon:ToJson()
If _o4MDG:Cliente()
    _cJSon      := _o4MDG:cJSonRet
    _oJSonRet   := Nil 
    _oJSonRet   := JSonObject():New()
    _oJSonRet:FromJson(_cJSon)
    
    If _lSimples
        _oSimples   := _oJSonRet['simples_nacional']

        //---------------+
        // Valida receita| 
        //---------------+
        If _oSimples['codigo'] <> '200' 
            _lRet  := .F.
            _cError:= "ERROR: " + _oSimples['mensagem']
        EndIf 

    EndIf 

    If _lSuframa
        _oSuframa  := _oJSonRet['suframa']
        _lRet       := .T.

         //----------------+
        // Valida sintegra| 
        //----------------+
        If _oSuframa['codigo'] <> '200'
            _lRet  := .F.
            _cError:= "ERROR: " + _oSuframa['mensagem']
        EndIf 
    EndIf 

Else 
    _cError:= _o4MDG:cError
    _lRet  := .F.
EndIf 

FreeObj(_o4MDG)
FreeObj(_oJSon)
FreeObj(_oJSonRet)

Return _lRet  

/***************************************************************************************************/
/*/{Protheus.doc} BPFATM01B
    @description Realiza a inserção/atuaização de prospects 
    @type  Static Function
    @author Bernard M Margarido
    @since 27/06/2024
    @version version
/*/
/***************************************************************************************************/
Static Function BPFATM01B(_cCgc,_lInclui,_lReceita,_lSintegra,_lSimples,_lSuframa,_oReceita,_oSintegra,_oSimples,_oSuframa,_cError)
Local _aArea        := GetArea()

Local _aQSA         := {}
Local _aCnae        := {}
Local _aNatJuri     := {}
Local _aTelefone    := {}

Local _cCodigo      := ""
Local _cLoja        := ""
Local _cRazao       := ""
Local _cFantasia    := ""
Local _cDDD         := ""
Local _cTelefone    := ""
Local _cCNae        := ""
Local _cBairro      := ""
Local _cCapital     := ""
Local _cCep         := ""
Local _cComplem     := ""
Local _dDTInicio    := ""
Local _cEMail       := ""
Local _cEndMaps     := ""
Local _cLogradouro  := ""
Local _cMunicipio   := ""
Local _cNatJuridica := ""
Local _cNumero      := ""
Local _cPorte       := ""
Local _cSitReceita  := ""
Local _cStaReceita  := ""
Local _cTpCNPJ      := ""
Local _cUF          := ""
Local _cURLComp     := ""
Local _cInscR       := ""
Local _cSitSint     := ""
Local _cCodMun      := ""
Local _cStaSimp     := ""
Local _cStaSufr     := ""
Local _cSuframa     := ""
Local _cGrpTrib     := CriaVar("US_GRPTRIB",.F.)
Local _cCodTab      := GetNewPar("BS_TABPLAB")
Local _cCodNat      := GetNewPar("BS_NATCLI")

Local _nTRazao      := TamSx3("US_NOME")[1]
Local _nTReduz      := TamSx3("US_NREDUZ")[1]
Local _nTPorte      := TamSx3("US_XPORTE")[1]
Local _nTDescNj     := TamSx3("US_XDESCNJ")[1]
Local _nTChave      := TamSx3("X5_CHAVE")[1]
Local _nOpcA        := IIF(_lInclui, 3, 4)

Local _lRet         := .F.

Local _oDReceita    := IIF(ValType(_oReceita) <> "U", _oReceita['dados'], Nil)
Local _oDSintegra   := IIF(ValType(_oSintegra) <> "U", _oSintegra['dados'], Nil)
Local _oDSimples    := IIF(ValType(_oSimples) <> "U", _oSimples['dados'], Nil)
Local _oDSuframa    := IIF(ValType(_oSuframa) <> "U", _oSuframa['dados'], Nil)

Private _oModel     := Nil 
Private _oModelSUS  := Nil

//-------------------------------------------------+
// Valida se já existe prospect/cliente cadastrado |
//-------------------------------------------------+
If !BPFATM01L("SUS",_cCgc,@_cError)
    RestArea(_aArea)
    Return .F.
EndIf 

//-----------------------+
// Retorna codigo e loja |
//-----------------------+
BPFATM01D("SUS",_cCgc,@_cCodigo,@_cLoja)

//--------------------+
// Posiciona Prospect |
//--------------------+
If !_lInclui
    dbSelectArea("SUS")
    SUS->( dbSetOrder(1) )
    If !SUS->( dbSeek(xFilial("SUS") + _cCodigo + _cLoja) )
        _cError := "PROSPECT " + _cCgc + " NAO LOCALIZADO."
        RestArea(_aArea)
        Return .F.
    EndIf 
EndIf 

//------------------+
// Dados da receita |
//------------------+
If _lInclui .Or. _lReceita 
    _cRazao         := IIF(ValType(_oDReceita['razao_social']) <> "U", u_SyAcento(_oDReceita['razao_social'],.T.), "")
    _cFantasia      := IIF(ValType(_oDReceita['nome_fantasia']) <> "U", u_SyAcento(_oDReceita['nome_fantasia'],.T.), "")

    If ValType(_oDReceita['telefone']) <> "U" .And. ( AT(";",_oDReceita['telefone']) > 0 .Or. AT("/",_oDReceita['telefone']) > 0 )
        If AT(";",_oDReceita['telefone']) > 0
            _aTelefone := Separa(_oDReceita['telefone'],";")
            _cDDD      := IIF(AT("(",_aTelefone[1]) > 0, SubStr(_aTelefone[1], AT("(",_aTelefone[1]) + 1, AT(")",_aTelefone[1]) -2),"")
            _cTelefone := u_SyFormat(Alltrim(IIF(AT(")",_aTelefone[1]) > 0, SubStr(_aTelefone[1], AT(")",_aTelefone[1]) + 1),_aTelefone[1])),"A1_TEL",.T.)
        ElseIf AT("/",_oDReceita['telefone']) > 0 
            _aTelefone := Separa(_oDReceita['telefone'],"/")
            _cDDD      := IIF(AT("(",_aTelefone[1]) > 0, SubStr(_aTelefone[1], AT("(",_aTelefone[1]) + 1, AT(")",_aTelefone[1]) -2),"")
            _cTelefone := u_SyFormat(Alltrim(IIF(AT(")",_aTelefone[1]) > 0, SubStr(_aTelefone[1], AT(")",_aTelefone[1]) + 1),_aTelefone[1])),"A1_TEL",.T.)
        EndIf 
    Else 
        _cDDD           := IIF(ValType(_oDReceita['telefone']) <> "U", IIF(AT("(",_oDReceita['telefone']) > 0, SubStr(_oDReceita['telefone'], AT("(",_oDReceita['telefone']) + 1, AT(")",_oDReceita['telefone']) -2),""),"")
        _cTelefone      := IIF(ValType(_oDReceita['telefone']) <> "U", u_SyFormat(Alltrim(IIF(AT(")",_oDReceita['telefone']) > 0, SubStr(_oDReceita['telefone'], AT(")",_oDReceita['telefone']) + 1),_oDReceita['telefone'])),"A1_TEL",.T.),"")
    EndIf  
    
    _cCNae          := IIF(ValType(_oDReceita['atividade_economica_codigo']) <> "U", _oDReceita['atividade_economica_codigo'], "")
    _cDescCNae      := IIF(ValType(_oDReceita['atividade_economica_descricao']) <> "U", _oDReceita['atividade_economica_descricao'], "")
    _cBairro        := IIF(ValType(_oDReceita['bairro']) <> "U", u_SyAcento(_oDReceita['bairro'],.T.), "" )
    _cCapital       := IIF(ValType(_oDReceita['capital_social']) <> "U", _oDReceita['capital_social'], "")
    _cCep           := IIF(ValType(_oDReceita['cep']) <> "U", u_SyFormat(_oDReceita['cep'],"A1_CEP",.T.), "") 
    _cCgc           := IIF(ValType(_oDReceita['cnpj']) <> "U", _oDReceita['cnpj'], "") 
    _cComplem       := IIF(ValType(_oDReceita['complemento']) <> "U", _oDReceita['complemento'], "")
    _dDTInicio      := IIF(ValType(_oDReceita['data_inicio_atividade']) <> "U", cToD(_oDReceita['data_inicio_atividade']) , "")
    _cEMail         := IIF(ValType(_oDReceita['email']) <> "U", Lower(_oDReceita['email']), "")
    _cEndMaps       := IIF(ValType(_oDReceita['endereco_maps']) <> "U", _oDReceita['endereco_maps'], "")
    _cLogradouro    := IIF(ValType(_oDReceita['logradouro']) <> "U", u_SyAcento(_oDReceita['logradouro'],.T.), "")
    _cMunicipio     := IIF(ValType(_oDReceita['municipio']) <> "U", u_SyAcento(_oDReceita['municipio'],.T.), "")
    _cNatJuridica   := IIF(ValType(_oDReceita['natureza_juridica_codigo']) <> "U", u_SyFormat(_oDReceita['natureza_juridica_codigo'],"US_XCODNJ",.T.) , "")
    _cDescJuridica  := IIF(ValType(_oDReceita['natureza_juridica_descricao']) <> "U", u_SyAcento(_oDReceita['natureza_juridica_descricao'],.T.), "")
    _cNumero        := IIF(ValType(_oDReceita['numero']) <> "U", _oDReceita['numero'], "")
    _cPorte         := IIF(ValType(_oDReceita['porte']) <> "U", u_SyAcento(_oDReceita['porte'],.T.), "")
    _cSitReceita    := IIF(ValType(_oDReceita['situacao_cadastral']) <> "U", FwNoAccent(DecodeUTF8(_oDReceita['situacao_cadastral'])), "" )
    _cTpCNPJ        := IIF(ValType(_oDReceita['tipo_cnpj']) <> "U", _oDReceita['tipo_cnpj'], "")
    _cUF            := IIF(ValType(_oDReceita['uf']) <> "U", _oDReceita['uf'], "")
    _cURLComp       := IIF(ValType(_oDReceita['url_comprovante']) <> "U", _oDReceita['url_comprovante'], "")
    _cCodMun        := BPFATM01M(_cMunicipio,_cUF)
    _aQSA           := IIF(ValType(_oDReceita['qsa']) <> "U", BPFATM01F(_oDReceita['qsa']), {})
    _aCnae          := IIF(ValType(_oDReceita['atividades_economicas_secundarias']) <> "U", BPFATM01N(DecodeUTF8(_oDReceita['atividades_economicas_secundarias'])), {})
Else
    _cRazao         := SUS->US_NOME
    _cFantasia      := SUS->US_NREDUZ
EndIf 
//-------------------------+
// Adiciona CNAE principal |
//-------------------------+
aAdd(_aCnae,{_cCNae,_cDescCNae})

//-------------------+    
// Natureza Juridica |
//-------------------+    
aAdd(_aNatJuri,{_cNatJuridica, _cDescJuridica})

//----------------+
// Dados Sintegra |
//----------------+
_cInscR         := IIF(ValType(_oDSintegra['inscricao_estadual']) <> "U", _oDSintegra['inscricao_estadual'], "")
_cSitSint       := IIF(ValType(_oDSintegra['situacao_cadastral']) <> "U", FwNoAccent(DecodeUTF8(_oDSintegra['situacao_cadastral'])), "")

//---------------+
// Dados Simples |
//---------------+
_cStaSimp   := "2"
If ValType(_oSimples) <> "U"
    If AT("NÃO OPTANTE PELO SIMPLES NACIONAL",FwNoAccent(DecodeUTF8(_oDSimples['situacao_simples_nacional']))) > 0 
        _cStaSimp   := "2"
    Else 
        _cStaSimp   := "1"
    EndIf 
EndIf 

//---------------+
// Dados Suframa |
//---------------+
_cStaSufr   := "2"
If ValType(_oSuframa) <> "U"
    _cStaSufr := FwNoAccent(DecodeUTF8(_oDSuframa['situacao_cadastral']))
    _cSuframa := _oDSuframa['inscricao_estadual']
EndIf 

//---------------+
// MVC Prospects |
//---------------+
_oModel    	:= FwLoadModel('TMKA260')	
_oModel:SetOperation(IIF(_nOpcA == 3,MODEL_OPERATION_INSERT,MODEL_OPERATION_UPDATE)) 
_oModel:Activate()
_oMdlSUS 	:= _oModel:GetModel("SUSMASTER")

_oMdlSUS:SetValue("US_FILIAL"       , xFilial("SUS")                                                                    )
_oMdlSUS:SetValue("US_COD"		    , _cCodigo  	                                                                    )
_oMdlSUS:SetValue("US_LOJA"		    , _cLoja		                                                                    )
_oMdlSUS:SetValue("US_NOME"		    , PadR(_cRazao,_nTRazao)                                                            )
_oMdlSUS:SetValue("US_NREDUZ"		, PadR(_cFantasia,_nTReduz)                                                         )
_oMdlSUS:SetValue("US_CGC"          , _cCgc                                                                             )


If _lInclui .Or. _lReceita 
    _oMdlSUS:SetValue("US_ORIGEM"       , "B"            		                                                            )
    _oMdlSUS:SetValue("US_PESSOA"       , "J"           		                                                            )
    _oMdlSUS:SetValue("US_DDD"          , _cDDD                                                                             )
    _oMdlSUS:SetValue("US_TEL"          , _cTelefone                                                                        )
    _oMdlSUS:SetValue("US_EMAIL"        , _cEMail                                                                           )
    _oMdlSUS:SetValue("US_END"          , _cLogradouro + ", " + _cNumero                                                    )
    _oMdlSUS:SetValue("US_MUN"          , _cMunicipio                                                                       )
    _oMdlSUS:SetValue("US_BAIRRO"       , _cBairro             		                                                        )
    _oMdlSUS:SetValue("US_CEP"          , _cCep        		                                                                )
    _oMdlSUS:SetValue("US_EST"          , _cUF                                       		                                )
    _oMdlSUS:SetValue("US_COD_MUN"      , _cCodMun             		                                                        )
    _oMdlSUS:SetValue("US_TIPO"         , "R" 		                                                                        )
    _oMdlSUS:SetValue("US_CNAE"         , Transform(_cCNae,"@R ####-#/##")	                                                )
    _oMdlSUS:SetValue("US_STATUS"       , "1"		                                                                        )
    _oMdlSUS:SetValue("US_RECCOFI"	    , "N"												                                )
    _oMdlSUS:SetValue("US_RECCSLL"	    , "N"												                                )
    _oMdlSUS:SetValue("US_RECPIS"	    , "N"												                                )
    _oMdlSUS:SetValue("US_XDTRECF"      , Date()		                                                                    )
    _oMdlSUS:SetValue("US_VEND"         , IIF(_lInclui, u_BsTgvVnd("01",.T.), SUS->US_VEND)                                 )
    _oMdlSUS:SetValue("US_XVEND2"       , IIF(_lInclui, u_BsTgvVnd("01",.T.), SUS->US_VEND)                                 )
    _oMdlSUS:SetValue("US_MSBLQL"       , "2"                                                                               )
    _oMdlSUS:SetValue("US_TABPRE"       , _cCodTab	                                                                        )
    _oMdlSUS:SetValue("US_XTABPR2"      , _cCodTab	                                                                        )
    _oMdlSUS:SetValue("US_GRPTRIB"      , _cGrpTrib	                                                                        )
    _oMdlSUS:SetValue("US_XSUFRAM"      , "2"	                                                                            )
    _oMdlSUS:SetValue("US_XENTFED"      , "4"	                                                                            )
    _oMdlSUS:SetValue("US_REGIAO"       , RTrim(Posicione("SX5",1,xFilial("SX5") + "X9" + PadR(_cUF,_nTChave),"X5_DESCRI")) )
    _oMdlSUS:SetValue("US_XDTNASC"	    , _dDTInicio											                            )
    //-------------------------+    
    // Valida situação receita | 
    //-------------------------+
    If Upper(RTrim(_cSitReceita)) $ "ATIVO/ATIVA"
        _cStaReceita  := "1"
    ElseIf Upper(RTrim(_cSitReceita)) $ "HABILITADO/HABILITADA"
        _cStaReceita  := "2"
    ElseIf Upper(RTrim(_cSitReceita)) $ "NAO HABILITADO/NAO HABILITADA"
        _cStaReceita  := "3"    
    ElseIf Upper(RTrim(_cSitReceita)) $ "NAO LOCALIZADO/NAO LOCALIZADA"
        _cStaReceita  := "4"
    ElseIf Upper(RTrim(_cSitReceita)) $ "SUSPENSO/SUSPENSA"
        _cStaReceita  := "5"
    ElseIf Upper(RTrim(_cSitReceita)) $ "INAPTO/INAPTA"
        _cStaReceita  := "6"
    ElseIf Upper(RTrim(_cSitReceita)) $ "BAIXADO/BAIXADA"
        _cStaReceita  := "7"
    ElseIf Upper(RTrim(_cSitReceita)) $ "IMPEDIDO/IMPEDIDA"
        _cStaReceita  := "8"
    ElseIf Upper(RTrim(_cSitReceita)) $ "INATIVO/INATIVA"
        _cStaReceita  := "9"    
    ElseIf Upper(RTrim(_cSitReceita)) $ "CANCELADO/CANCELADA"
        _cStaReceita  := "A"
    ElseIf Upper(RTrim(_cSitReceita)) $ "NULO/NULA"
        _cStaReceita  := "B"
    ElseIf Upper(RTrim(_cSitReceita)) $ "BLOQUEADO/BLOQUEADA"
        _cStaReceita  := "C"
    EndIf

    _oMdlSUS:SetValue("US_XSITREC"     , _cStaReceita                                                                       )
    _oMdlSUS:SetValue("US_XMSITRE"     , ""                                                                                 )
    _oMdlSUS:SetValue("US_XPORTE"      , PadR(_cPorte,_nTPorte)                                                             )
    _oMdlSUS:SetValue("US_XCODNJ"      , _cNatJuridica                                                                      )
    _oMdlSUS:SetValue("US_XDESCNJ"     , SubStr(_cDescJuridica,1,_nTDescNj)                                                 )
    _oMdlSUS:SetValue("US_NATUREZ"     , _cCodNat                                                                           )
    _oMdlSUS:SetValue("US_XCODPAI"     , "01058"                                                                            ) 
    _oMdlSUS:SetValue("US_PAIS"        , "105"                                                                              ) 
    _oMdlSUS:SetValue("US_XSIMPNA"     , "2"                                                                                )
EndIf 

//--------------------------+    
// Valida situação sintegra | 
//--------------------------+
If _lInclui .Or. _lSintegra

    If Upper(RTrim(_cSitSint)) $ "ATIVO/ATIVA" .Or. ( AT("ATIVO",Upper(RTrim(_cSitSint))) ) > 0
        _cStaSint   := "1" 
    ElseIf Upper(RTrim(_cSitSint)) $ "HABILITADO/HABILITADA" .Or. ( AT("HABILITADO",Upper(RTrim(_cSitSint))) ) > 0
        _cStaSint  := "2"
    ElseIf Upper(RTrim(_cSitSint)) $ "NAO HABILITADO/NAO HABILITADA"
        _cStaSint  := "3"    
    ElseIf Upper(RTrim(_cSitSint)) $ "NAO LOCALIZADO/NAO LOCALIZADA"
        _cStaSint  := "4"
    ElseIf Upper(RTrim(_cSitSint)) $ "SUSPENSO/SUSPENSA"
        _cStaSint  := "5"
    ElseIf Upper(RTrim(_cSitSint)) $ "INAPTO/INAPTA"
        _cStaSint  := "6"
    ElseIf Upper(RTrim(_cSitSint)) $ "BAIXADO/BAIXADA"
        _cStaSint  := "7"
    ElseIf Upper(RTrim(_cSitSint)) $ "IMPEDIDO/IMPEDIDA"
        _cStaSint  := "8"
    ElseIf Upper(RTrim(_cSitSint)) $ "INATIVO/INATIVA"
        _cStaSint  := "9"    
    ElseIf Upper(RTrim(_cSitSint)) $ "CANCELADO/CANCELADA"
        _cStaSint  := "A"
    ElseIf Upper(RTrim(_cSitSint)) $ "NULO/NULA"
        _cStaSint  := "B"
    ElseIf Upper(RTrim(_cSitSint)) $ "BLOQUEADO/BLOQUEADA"
        _cStaSint  := "C"
    Else
        _cStaSint  := "7"
    EndIf

    //-------------------------+
    // Regra Incrição Estadual |
    //-------------------------+
    If _cStaReceita == "1" .And. _cStaSint $ "3/4/5/6/7/8/9/A/B"
        _cInscR := "ISENTO"
    ElseIf _cStaReceita == "1" .And. _cStaSint $ "1/2"
        _cInscR     := IIF(_cUF == "MG", PadL(_cInscR,13,"0"), _cInscR)
    EndIf

    _oMdlSUS:SetValue("US_XSITSIN"     , _cStaSint                                                                          )
    _oMdlSUS:SetValue("US_INSCR"       , _cInscR                                                                            )
    _oMdlSUS:SetValue("US_XDTSINT"     , Date()		                                                                        )
    _oMdlSUS:SetValue("US_CONTRIB"     , "2"                                                                                )
    _oMdlSUS:SetValue("US_XNEOWAY"	   , "S"												                                )
    _oMdlSUS:SetValue("US_XSTANEO"	   , "1"												                                )
    _oMdlSUS:SetValue("US_XNEOSIT" 	   , "1"												                                )
EndIf 

//-------------------------+
// Valida situação Simples |
//-------------------------+
If _lSintegra
    _oMdlSUS:SetValue("US_XSITSIM"      , _cStaSimp												                            )
    _oMdlSUS:SetValue("US_XDTSIMP"      , Date()												                            )
EndIf 

//---------------+
// Regra Suframa |
//---------------+
If _lSuframa
    //-------------------------+
    // Valida situação Suframa |
    //-------------------------+
    If Upper(RTrim(_cStaSufr)) $ "ATIVO/ATIVA"
        _cStaSufr  := "1"
    ElseIf Upper(RTrim(_cStaSufr)) $ "HABILITADO/HABILITADA"
        _cStaSufr  := "2"
    ElseIf Upper(RTrim(_cStaSufr)) $ "NAO HABILITADO/NAO HABILITADA"
        _cStaSufr  := "3"    
    ElseIf Upper(RTrim(_cStaSufr)) $ "NAO LOCALIZADO/NAO LOCALIZADA"
        _cStaSufr  := "4"
    ElseIf Upper(RTrim(_cStaSufr)) $ "SUSPENSO/SUSPENSA"
        _cStaSufr  := "5"
    ElseIf Upper(RTrim(_cStaSufr)) $ "INAPTO/INAPTA"
        _cStaSufr  := "6"
    ElseIf Upper(RTrim(_cStaSufr)) $ "BAIXADO/BAIXADA"
        _cStaSufr  := "7"
    ElseIf Upper(RTrim(_cStaSufr)) $ "IMPEDIDO/IMPEDIDA"
        _cStaSufr  := "8"
    ElseIf Upper(RTrim(_cStaSufr)) $ "INATIVO/INATIVA"
        _cStaSufr  := "9"    
    ElseIf Upper(RTrim(_cStaSufr)) $ "CANCELADO/CANCELADA"
        _cStaSufr  := "A"
    ElseIf Upper(RTrim(_cStaSufr)) $ "NULO/NULA"
        _cStaSufr  := "B"
    ElseIf Upper(RTrim(_cStaSufr)) $ "BLOQUEADO/BLOQUEADA"
        _cStaSufr  := "C"
    EndIf

    If _oMdlSUS:GetValue("US_XSITREC") == "1" .And. _oMdlSUS:GetValue("US_PESSOA") == "J" .And. _oMdlSUS:GetValue("US_EST") $ "AC/AM/AP/RO/RR" .And. _cStaSufr $ "3/4/5/6/7/8/9/A/B" 
        _cSuframa   := ""
    EndIf

    _oMdlSUS:SetValue("US_SUFRAMA"   , _cSuframa												                            )
    _oMdlSUS:SetValue("US_XSITSUF"   , _cStaSufr												                            )
    _oMdlSUS:SetValue("US_XDTSUFR"   , Date()												                                )
    If _cStaSufr $ "1/2" .Or. !Empty(_cSuframa) 
        _oMdlSUS:SetValue("US_XSUFRAM"   , "1"												                                )
    Else
        _oMdlSUS:SetValue("US_XSUFRAM"   , "2"												                                )    
    EndIf
EndIf 

//-------------------------+
// Grava CNAES X Prospects |
//-------------------------+
If Len(_aCnae) > 0 
    BPFATM01G(_cCgc,_aCnae)
EndIf 

//-----------------------+
// Grava QSA X Prospects |
//-----------------------+
If Len(_aQSA) > 0 
    BPFATM01H(_cCgc,_aQSA)
EndIf 

//-------------------------------+
// Natureza juridica X Prospects |
//-------------------------------+
If Len(_aNatJuri) > 0 
    BPFATM01I(_aNatJuri)
EndIf 

//---------------------------+
// Executa regras tributação |
//---------------------------+
BPFATM01J("SUS",_lInclui,_oMdlSUS)

//--------------------------------+    
// Realiza a gravação do Prospect |
//--------------------------------+    
If _oModel:VldData()
    _oModel:CommitData()	
    _cError     := "Prospect atualizado com sucesso." + CRLF
    _lRet       := .T.
Else		
    _aError     := _oModel:GetErrorMessage()
    _cError     := "Erro ao realizar consulta " + Alltrim(_aError[4]) + " " + Alltrim(_aError[5])  + " " + Alltrim(_aError[6])
    _lRet       := .F.
EndIf	

//-------------------------+
// Desctroi a classe Model |
//-------------------------+
_oModel:DeActivate()	
_oModel:Destroy()	
_oModel := Nil

FreeObj(_oDReceita)
FreeObj(_oDSintegra)
FreeObj(_oDSimples)
FreeObj(_oDSuframa)

RestArea(_aArea)
Return _lRet 

/*************************************************************************************/
/*/{Protheus.doc} BPFATM01C
    @description Realiza a inserção/atuaização de clientes 
    @type  Static Function
    @author Bernard M Margarido
    @since 28/06/2024
    @version version
/*/
/*************************************************************************************/
Static Function BPFATM01C(_cCgc,_lInclui,_lReceita,_lSintegra,_lSimples,_lSuframa,_oReceita,_oSintegra,_oSimples,_oSuframa,_cError)
Local _aArea            := GetArea()

Local _aQSA             := {}
Local _aCnae            := {}
Local _aNatJuri         := {}
Local _aErro	        := {}
Local _aCliente         := {}

Local _cCodigo          := ""
Local _cLoja            := ""
Local _cRazao           := ""
Local _cFantasia        := ""
Local _cDDD             := ""
Local _cTelefone        := ""
Local _cCNae            := ""
Local _cBairro          := ""
Local _cCapital         := ""
Local _cCep             := ""
Local _cComplem         := ""
Local _dDTInicio        := ""
Local _cEMail           := ""
Local _cEndMaps         := ""
Local _cLogradouro      := ""
Local _cMunicipio       := ""
Local _cNatJuridica     := ""
Local _cNumero          := ""
Local _cPorte           := ""
Local _cSitReceita      := ""
Local _cStaReceita      := ""
Local _cTpCNPJ          := ""
Local _cUF              := ""
Local _cURLComp         := ""
Local _cInscR           := ""
Local _cSitSint         := ""
Local _cCodMun          := ""
Local _cStaSimp         := ""
Local _cStaSufr         := ""
Local _cSuframa         := ""
Local _cLinha	        := ""	
       
Local _cGrpTrib         := CriaVar("A1_GRPTRIB",.F.)
Local _cCodTab          := GetNewPar("BS_TABPLAB")
Local _cCodNat          := GetNewPar("BS_NATCLI")
Local _cGrpVen          := GetNewPar("BS_GRPVEN")

Local _nX               := 0
Local _nOpcA            := IIF(_lInclui, 3, 4)
Local _nTPorte          := TamSx3("A1_XPORTE")[1]
Local _nTDescNj         := TamSx3("A1_XDESCNJ")[1]
Local _nTRazao          := TamSx3("A1_NOME")[1]
Local _nTReduz          := TamSx3("A1_NREDUZ")[1]
Local _nTChave          := TamSx3("X5_CHAVE")[1]

Local _lRet             := .T.
Local _lMVCCustomer     := IIF( FunName() == "CRMA980", .T., .F.)

Local _oDReceita        := IIF(ValType(_oReceita) <> "U", _oReceita['dados'], Nil)
Local _oDSintegra       := IIF(ValType(_oSintegra) <> "U", _oSintegra['dados'], Nil)
Local _oDSimples        := IIF(ValType(_oSimples) <> "U", _oSimples['dados'], Nil)
Local _oDSuframa        := IIF(ValType(_oSuframa) <> "U", _oSuframa['dados'], Nil)

Private lMsErroAuto     := .F.
Private lMsHelpAuto 	:= .T.
Private lAutoErrNoFile 	:= .T.
Private l030Auto        := .T.

Private _oModel         := Nil
Private _oModelSA1      := Nil 

//-------------------------------------------------+
// Valida se já existe prospect/cliente cadastrado |
//-------------------------------------------------+
If !BPFATM01L("SA1",_cCgc,@_cError)
    RestArea(_aArea)
    Return .F.
EndIf 

//-----------------------+
// Retorna codigo e loja |
//-----------------------+
BPFATM01D("SA1",_cCgc,@_cCodigo,@_cLoja)

//-------------------------+
// SA1 - Posisiona Cliente |
//-------------------------+
If !_lInclui
    dbSelectArea("SA1")
    SA1->( dbSetOrder(1) )
    If !SA1->( dbSeek(xFilial("SA1") + _cCodigo + _cLoja) )
        _cError := "CLIENTE " + _cCgc + " NAO LOCALIZADO."
        RestArea(_aArea)
        Return .F.
    EndIf 
EndIf 

//------------------+
// Dados da Receita |
//------------------+
If _lInclui .Or. _lReceita
    _cRazao         := IIF(ValType(_oDReceita['razao_social']) <> "U", u_SyAcento(_oDReceita['razao_social'],.T.), "")
    _cFantasia      := IIF(_lInclui, IIF(ValType(_oDReceita['nome_fantasia']) <> "U", u_SyAcento(_oDReceita['nome_fantasia'],.T.), ""), SA1->A1_NREDUZ)

    If ValType(_oDReceita['telefone']) <> "U" .And. ( AT(";",_oDReceita['telefone']) > 0 .Or. AT("/",_oDReceita['telefone']) > 0 )
        If AT(";",_oDReceita['telefone']) > 0
            _aTelefone := Separa(_oDReceita['telefone'],";")
            _cDDD      := IIF(_lInclui, IIF(AT("(",_aTelefone[1]) > 0, SubStr(_aTelefone[1], AT("(",_aTelefone[1]) + 1, AT(")",_aTelefone[1]) -2),""), SA1->A1_DDD)
            _cTelefone := IIF(_lInclui, u_SyFormat(Alltrim(IIF(AT(")",_aTelefone[1]) > 0, SubStr(_aTelefone[1], AT(")",_aTelefone[1]) + 1),_aTelefone[1])),"A1_TEL",.T.), SA1->A1_TEL)
        ElseIf AT("/",_oDReceita['telefone']) > 0 
            _aTelefone := Separa(_oDReceita['telefone'],"/")
            _cDDD      := IIF(_lInclui, IIF(AT("(",_aTelefone[1]) > 0, SubStr(_aTelefone[1], AT("(",_aTelefone[1]) + 1, AT(")",_aTelefone[1]) -2),""), SA1->A1_DDD)
            _cTelefone := IIF(_lInclui, u_SyFormat(Alltrim(IIF(AT(")",_aTelefone[1]) > 0, SubStr(_aTelefone[1], AT(")",_aTelefone[1]) + 1),_aTelefone[1])),"A1_TEL",.T.), SA1->A1_TEL)
        EndIf 
    Else 
        _cDDD           := IIF(_lInclui, IIF(ValType(_oDReceita['telefone']) <> "U", IIF(AT("(",_oDReceita['telefone']) > 0, SubStr(_oDReceita['telefone'], AT("(",_oDReceita['telefone']) + 1, AT(")",_oDReceita['telefone']) -2),""),""), SA1->A1_DDD)
        _cTelefone      := IIF(_lInclui, IIF(ValType(_oDReceita['telefone']) <> "U", u_SyFormat(Alltrim(IIF(AT(")",_oDReceita['telefone']) > 0, SubStr(_oDReceita['telefone'], AT(")",_oDReceita['telefone']) + 1),_oDReceita['telefone'])),"A1_TEL",.T.),""), SA1->A1_TEL)
    EndIf  
    
    _cCNae          := IIF(ValType(_oDReceita['atividade_economica_codigo']) <> "U", _oDReceita['atividade_economica_codigo'], "")
    _cDescCNae      := IIF(ValType(_oDReceita['atividade_economica_descricao']) <> "U", _oDReceita['atividade_economica_descricao'], "")
    _cBairro        := IIF(ValType(_oDReceita['bairro']) <> "U", u_SyAcento(_oDReceita['bairro'],.T.), "" )
    _cCapital       := IIF(ValType(_oDReceita['capital_social']) <> "U", _oDReceita['capital_social'], "")
    _cCep           := IIF(ValType(_oDReceita['cep']) <> "U", u_SyFormat(_oDReceita['cep'],"A1_CEP",.T.), "") 
    _cCgc           := IIF(ValType(_oDReceita['cnpj']) <> "U", _oDReceita['cnpj'], "") 
    _cComplem       := IIF(ValType(_oDReceita['complemento']) <> "U", _oDReceita['complemento'], "")
    _dDTInicio      := IIF(ValType(_oDReceita['data_inicio_atividade']) <> "U", cToD(_oDReceita['data_inicio_atividade']) , "")
    _cEMail         := IIF(_lInclui, IIF(ValType(_oDReceita['email']) <> "U", Lower(_oDReceita['email']), ""), SA1->A1_EMAIL )
    _cEndMaps       := IIF(ValType(_oDReceita['endereco_maps']) <> "U", _oDReceita['endereco_maps'], "")
    _cLogradouro    := IIF(ValType(_oDReceita['logradouro']) <> "U", u_SyAcento(_oDReceita['logradouro'],.T.), "")
    _cMunicipio     := IIF(ValType(_oDReceita['municipio']) <> "U", u_SyAcento(_oDReceita['municipio'],.T.), "")
    _cNatJuridica   := IIF(ValType(_oDReceita['natureza_juridica_codigo']) <> "U", u_SyFormat(_oDReceita['natureza_juridica_codigo'],"US_XCODNJ",.T.) , "")
    _cDescJuridica  := IIF(ValType(_oDReceita['natureza_juridica_descricao']) <> "U", u_SyAcento(_oDReceita['natureza_juridica_descricao'],.T.), "")
    _cNumero        := IIF(ValType(_oDReceita['numero']) <> "U", _oDReceita['numero'], "")
    _cPorte         := IIF(ValType(_oDReceita['porte']) <> "U", u_SyAcento(_oDReceita['porte'],.T.), "")
    _cSitReceita    := IIF(ValType(_oDReceita['situacao_cadastral']) <> "U", FwNoAccent(DecodeUTF8(_oDReceita['situacao_cadastral'])), "" )
    _cTpCNPJ        := IIF(ValType(_oDReceita['tipo_cnpj']) <> "U", _oDReceita['tipo_cnpj'], "")
    _cUF            := IIF(ValType(_oDReceita['uf']) <> "U", _oDReceita['uf'], "")
    _cURLComp       := IIF(ValType(_oDReceita['url_comprovante']) <> "U", _oDReceita['url_comprovante'], "")
    _cCodMun        := BPFATM01M(_cMunicipio,_cUF)
    _aQSA           := IIF(ValType(_oDReceita['qsa']) <> "U", BPFATM01F(_oDReceita['qsa']), {})
    _aCnae          := IIF(ValType(_oDReceita['atividades_economicas_secundarias']) <> "U", BPFATM01N(DecodeUTF8(_oDReceita['atividades_economicas_secundarias'])), {})

    //-------------------------+
    // Adiciona CNAE principal |
    //-------------------------+
    aAdd(_aCnae,{_cCNae,_cDescCNae})

    //-------------------+    
    // Natureza Juridica |
    //-------------------+    
    aAdd(_aNatJuri,{_cNatJuridica, _cDescJuridica})

Else 
    _cRazao         := SA1->A1_NOME
    _cFantasia      := SA1->A1_NREDUZ
EndIf 

//----------------+
// Dados Sintegra |
//----------------+
If _lInclui .Or. _lSintegra
    _cInscR         := IIF(ValType(_oDSintegra['inscricao_estadual']) <> "U", _oDSintegra['inscricao_estadual'], "")
    _cSitSint       := IIF(ValType(_oDSintegra['situacao_cadastral']) <> "U", FwNoAccent(DecodeUTF8(_oDSintegra['situacao_cadastral'])), "")

    //---------------+
    // Dados Simples |
    //---------------+
    _cStaSimp   := "2"
    If ValType(_oSimples) <> "U"
        If AT("NÃO OPTANTE PELO SIMPLES NACIONAL",FwNoAccent(DecodeUTF8(_oDSimples['situacao_simples_nacional']))) > 0 
            _cStaSimp   := "2"
        Else 
            _cStaSimp   := "1"
        EndIf 
    EndIf 

EndIf 

//---------------+
// Dados Suframa |
//---------------+
If _lSuframa
    _cStaSufr   := "2"
    If ValType(_oSuframa) <> "U"
        _cStaSufr := FwNoAccent(DecodeUTF8(_oDSuframa['situacao_cadastral']))
        _cSuframa := _oDSuframa['inscricao_estadual']
    EndIf 
EndIf 

aAdd(_aCliente, { "A1_COD"		    , _cCodigo  	                                                                    , Nil })
aAdd(_aCliente, { "A1_LOJA"		    , _cLoja		                                                                    , Nil })
aAdd(_aCliente, { "A1_NOME"		    , PadR(_cRazao,_nTRazao)                                                            , Nil })
aAdd(_aCliente, { "A1_NREDUZ"		, PadR(_cFantasia,_nTReduz)                                                         , Nil })

If _lInclui .Or. _lReceita 
    aAdd(_aCliente, { "A1_TIPO"         , "R" 		                                                                        , Nil })
    aAdd(_aCliente, { "A1_CGC"          , _cCgc                                                                             , Nil })
    aAdd(_aCliente, { "A1_PESSOA"       , "J"           		                                                            , Nil })
    aAdd(_aCliente, { "A1_DDD"          , _cDDD                                                                             , Nil })
    aAdd(_aCliente, { "A1_TEL"          , _cTelefone                                                                        , Nil })
    aAdd(_aCliente, { "A1_EMAIL"        , _cEMail                                                                           , Nil })
    aAdd(_aCliente, { "A1_END"          , _cLogradouro + ", " + _cNumero                                                    , Nil })
    aAdd(_aCliente, { "A1_MUN"          , _cMunicipio                                                                       , Nil })
    aAdd(_aCliente, { "A1_BAIRRO"       , _cBairro             		                                                        , Nil })
    aAdd(_aCliente, { "A1_CEP"          , _cCep        		                                                                , Nil })
    aAdd(_aCliente, { "A1_EST"          , _cUF                                       		                                , Nil })
    aAdd(_aCliente, { "A1_COD_MUN"      , _cCodMun             		                                                        , Nil })
    aAdd(_aCliente, { "A1_CODMUNE"      , _cCodMun             		                                                        , Nil })  
    aAdd(_aCliente, { "A1_ENDENT"       , _cLogradouro + ", " + _cNumero             		                                , Nil })
    aAdd(_aCliente, { "A1_MUNE"         , _cMunicipio             		                                                    , Nil })
    aAdd(_aCliente, { "A1_BAIRROE"      , _cBairro             		                                                        , Nil })
    aAdd(_aCliente, { "A1_CEPE"         , _cCep             		                                                        , Nil })
    aAdd(_aCliente, { "A1_ESTE"         , _cUF             		                                                            , Nil })
    aAdd(_aCliente, { "A1_CNAE"         , Transform(_cCNae,"@R ####-#/##")	                                                , Nil })
    aAdd(_aCliente, { "A1_XDTRECF"      , Date()		                                                                    , Nil })
    aAdd(_aCliente, { "A1_VEND"         , u_BsTgvVnd("01",.T.)	                                                            , Nil })
    aAdd(_aCliente, { "A1_XVEND2"       , u_BsTgvVnd("01",.T.)	                                                            , Nil })
    aAdd(_aCliente, { "A1_MSBLQL"       , "2"                                                                               , Nil })
    aAdd(_aCliente, { "A1_NATUREZ"      , _cCodNat                                                                          , Nil })
    aAdd(_aCliente, { "A1_GRPVEN"       , _cGrpVen                                                                          , Nil })
    aAdd(_aCliente, { "A1_TABELA"       , _cCodTab	                                                                        , Nil })
    aAdd(_aCliente, { "A1_XTABPR2"      , _cCodTab	                                                                        , Nil })
    aAdd(_aCliente, { "A1_GRPTRIB"      , _cGrpTrib	                                                                        , Nil })
    aAdd(_aCliente, { "A1_XSUFRAM"      , "2"	                                                                            , Nil })
    aAdd(_aCliente, { "A1_XENTFED"      , "4"	                                                                            , Nil })
    aAdd(_aCliente, { "A1_REGIAO"       , RTrim(Posicione("SX5",1,xFilial("SX5") + "X9" + PadR(_cUF,_nTChave),"X5_DESCRI")) , Nil })
    aAdd(_aCliente, { "A1_DTNASC"	    , _dDTInicio											                            , Nil })
    //-------------------------+    
    // Valida situação receita | 
    //-------------------------+
    If Upper(RTrim(_cSitReceita)) $ "ATIVO/ATIVA"
        _cStaReceita  := "1"
    ElseIf Upper(RTrim(_cSitReceita)) $ "HABILITADO/HABILITADA"
        _cStaReceita  := "2"
    ElseIf Upper(RTrim(_cSitReceita)) $ "NAO HABILITADO/NAO HABILITADA"
        _cStaReceita  := "3"    
    ElseIf Upper(RTrim(_cSitReceita)) $ "NAO LOCALIZADO/NAO LOCALIZADA"
        _cStaReceita  := "4"
    ElseIf Upper(RTrim(_cSitReceita)) $ "SUSPENSO/SUSPENSA"
        _cStaReceita  := "5"
    ElseIf Upper(RTrim(_cSitReceita)) $ "INAPTO/INAPTA"
        _cStaReceita  := "6"
    ElseIf Upper(RTrim(_cSitReceita)) $ "BAIXADO/BAIXADA"
        _cStaReceita  := "7"
    ElseIf Upper(RTrim(_cSitReceita)) $ "IMPEDIDO/IMPEDIDA"
        _cStaReceita  := "8"
    ElseIf Upper(RTrim(_cSitReceita)) $ "INATIVO/INATIVA"
        _cStaReceita  := "9"    
    ElseIf Upper(RTrim(_cSitReceita)) $ "CANCELADO/CANCELADA"
        _cStaReceita  := "A"
    ElseIf Upper(RTrim(_cSitReceita)) $ "NULO/NULA"
        _cStaReceita  := "B"
    ElseIf Upper(RTrim(_cSitReceita)) $ "BLOQUEADO/BLOQUEADA"
        _cStaReceita  := "C"
    EndIf

    aAdd(_aCliente, { "A1_XSITREC"     , _cStaReceita                                                                       , Nil })
    aAdd(_aCliente, { "A1_XMSITRE"     , ""                                                                                 , Nil })
    aAdd(_aCliente, { "A1_XPORTE"      , PadR(_cPorte,_nTPorte)                                                             , Nil })
    aAdd(_aCliente, { "A1_XCODNJ"      , _cNatJuridica                                                                      , Nil })
    aAdd(_aCliente, { "A1_XDESCNJ"     , SubStr(_cDescJuridica,1,_nTDescNj)                                                 , Nil })
    aAdd(_aCliente, { "A1_CODPAIS"     , "01058"                                                                            , Nil }) 
    aAdd(_aCliente, { "A1_PAIS"        , "105"                                                                              , Nil }) 

EndIf 

//--------------------------+    
// Valida situação sintegra | 
//--------------------------+
If _lInclui .Or. _lSintegra 
    If Upper(RTrim(_cSitSint)) $ "ATIVO/ATIVA" .Or. ( AT("ATIVO",Upper(RTrim(_cSitSint))) ) > 0
        _cStaSint   := "1" 
    ElseIf Upper(RTrim(_cSitSint)) $ "HABILITADO/HABILITADA" .Or. ( AT("HABILITADO",Upper(RTrim(_cSitSint))) ) > 0
        _cStaSint  := "2"
    ElseIf Upper(RTrim(_cSitSint)) $ "NAO HABILITADO/NAO HABILITADA"
        _cStaSint  := "3"    
    ElseIf Upper(RTrim(_cSitSint)) $ "NAO LOCALIZADO/NAO LOCALIZADA"
        _cStaSint  := "4"
    ElseIf Upper(RTrim(_cSitSint)) $ "SUSPENSO/SUSPENSA"
        _cStaSint  := "5"
    ElseIf Upper(RTrim(_cSitSint)) $ "INAPTO/INAPTA"
        _cStaSint  := "6"
    ElseIf Upper(RTrim(_cSitSint)) $ "BAIXADO/BAIXADA"
        _cStaSint  := "7"
    ElseIf Upper(RTrim(_cSitSint)) $ "IMPEDIDO/IMPEDIDA"
        _cStaSint  := "8"
    ElseIf Upper(RTrim(_cSitSint)) $ "INATIVO/INATIVA"
        _cStaSint  := "9"    
    ElseIf Upper(RTrim(_cSitSint)) $ "CANCELADO/CANCELADA"
        _cStaSint  := "A"
    ElseIf Upper(RTrim(_cSitSint)) $ "NULO/NULA"
        _cStaSint  := "B"
    ElseIf Upper(RTrim(_cSitSint)) $ "BLOQUEADO/BLOQUEADA"
        _cStaSint  := "C"
    Else
        _cStaSint  := "7"
    EndIf

    //-------------------------+
    // Regra Incrição Estadual |
    //-------------------------+
    If _cStaReceita == "1" .And. _cStaSint $ "3/4/5/6/7/8/9/A/B"
        _cInscR := "ISENTO"
    ElseIf _cStaReceita == "1" .And. _cStaSint $ "1/2"
        _cInscR     := IIF(_cUF == "MG", PadL(_cInscR,13,"0"), _cInscR)
    EndIf

    aAdd(_aCliente, { "A1_XSITSIN"     , _cStaSint                                                                          , Nil })
    aAdd(_aCliente, { "A1_INSCR"       , _cInscR                                                                            , Nil })
    aAdd(_aCliente, { "A1_XDTSINT"     , Date()		                                                                        , Nil })
    aAdd(_aCliente, { "A1_CONTRIB"     , "2"                                                                                , Nil })
EndIf 

//-------------------------+
// Valida situação Simples |
//-------------------------+
If _lSimples 
    aAdd(_aCliente, { "A1_XDTSIMP"      , Date()												                            , Nil })
    aAdd(_aCliente, { "A1_SIMPLES"     , _cStaSimp                                                                          , Nil })
    aAdd(_aCliente, { "A1_SIMPNAC"     , _cStaSimp                                                                          , Nil })
EndIf 


//-------------------------+
// Valida situação Suframa |
//-------------------------+
If _lSuframa
    If Upper(RTrim(_cStaSufr)) $ "ATIVO/ATIVA"
        _cStaSufr  := "1"
    ElseIf Upper(RTrim(_cStaSufr)) $ "HABILITADO/HABILITADA"
        _cStaSufr  := "2"
    ElseIf Upper(RTrim(_cStaSufr)) $ "NAO HABILITADO/NAO HABILITADA"
        _cStaSufr  := "3"    
    ElseIf Upper(RTrim(_cStaSufr)) $ "NAO LOCALIZADO/NAO LOCALIZADA"
        _cStaSufr  := "4"
    ElseIf Upper(RTrim(_cStaSufr)) $ "SUSPENSO/SUSPENSA"
        _cStaSufr  := "5"
    ElseIf Upper(RTrim(_cStaSufr)) $ "INAPTO/INAPTA"
        _cStaSufr  := "6"
    ElseIf Upper(RTrim(_cStaSufr)) $ "BAIXADO/BAIXADA"
        _cStaSufr  := "7"
    ElseIf Upper(RTrim(_cStaSufr)) $ "IMPEDIDO/IMPEDIDA"
        _cStaSufr  := "8"
    ElseIf Upper(RTrim(_cStaSufr)) $ "INATIVO/INATIVA"
        _cStaSufr  := "9"    
    ElseIf Upper(RTrim(_cStaSufr)) $ "CANCELADO/CANCELADA"
        _cStaSufr  := "A"
    ElseIf Upper(RTrim(_cStaSufr)) $ "NULO/NULA"
        _cStaSufr  := "B"
    ElseIf Upper(RTrim(_cStaSufr)) $ "BLOQUEADO/BLOQUEADA"
        _cStaSufr  := "C"
    EndIf

    //---------------+
    // Regra Suframa |
    //---------------+
    If _cStaSint == "1" .And. _cUF $ "AC/AM/AP/RO/RR" .And. _cStaSufr $ "3/4/5/6/7/8/9/A/B" 
        _cSuframa   := ""
    EndIf

    aAdd(_aCliente, { "A1_SUFRAMA"   , _cSuframa												                            , Nil })
    aAdd(_aCliente, { "A1_XSITSUF"   , _cStaSufr												                            , Nil })
    aAdd(_aCliente, { "A1_XDTSUFR"   , Date()												                                , Nil })
    If _cStaSufr $ "1/2" .Or. !Empty(_cSuframa) 
        aAdd(_aCliente, { "A1_XSUFRAM"   , "1"												                                , Nil })
    Else
        aAdd(_aCliente, { "A1_XSUFRAM"   , "2"												                                , Nil })
    EndIf
EndIf 

//-------------------------+
// Grava CNAES X Prospects |
//-------------------------+
If Len(_aCnae) > 0 
    BPFATM01G(_cCgc,_aCnae)
EndIf 

//-----------------------+
// Grava QSA X Prospects |
//-----------------------+
If Len(_aQSA) > 0 
    BPFATM01H(_cCgc,_aQSA)
EndIf 

//-------------------------------+
// Natureza juridica X Prospects |
//-------------------------------+
If Len(_aNatJuri) > 0 
    BPFATM01I(_aNatJuri)
EndIf 

//-------------------------------+    
// Realiza a gravação do Cliente |
//-------------------------------+
If _lBsTgv01
    If _lMVCCustomer
        _oModel     := FwModelActive()
        _oModelSA1  := _oModel:GetModel( 'SA1MASTER' )

        BPFATM01P(_oModel,_lInclui,_aCliente)
    Else 
        BPFATM01O(_lInclui,_aCliente)
    EndIf 

    //---------------------------+
    // Executa regras tributação |
    //---------------------------+
    BPFATM01J("SA1",_lInclui, IIF(_lMVCCustomer,_oModel,Nil), _lBsTgv01,_lMVCCustomer)

Else 
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
    Else
        ConfirmSX8() 

        _lRet       := .T.

        //---------------------------+
        // Executa regras tributação |
        //---------------------------+
        dbSelectArea("SA1")
        SA1->( dbSetOrder(3) )
        If SA1->( dbSeek(xFilial("SA1") + _cCgc))
            BPFATM01J("SA1",_lInclui)
        EndIf 

    EndIf 
EndIf 

FreeObj(_oDReceita)
FreeObj(_oDSintegra)
FreeObj(_oDSimples)
FreeObj(_oDSuframa)
FreeObj(_oModel)
FreeObj(_oModelSA1)

RestArea(_aArea)
Return _lRet 

/*************************************************************************************/
/*/{Protheus.doc} BPFATM01D
    @description Regra preenchimento codigo e loja
    @type  Static Function
    @author Bernard M. Margarido
    @since 22/06/2020
/*/
/*************************************************************************************/
Static Function BPFATM01D(_cAlias,_cCgc,_cCodigo,_cLoja,_lMatriz,_lEx)
Local _oCodLoja := BsFatC01():New()

Default _lMatriz:= .F.
Default _lEx    := .F.

_oCodLoja:_cCGC     := _cCgc
_oCodLoja:_cAlias   := _cAlias
_oCodLoja:_lEx      := _lEx

If _oCodLoja:SetCodLoja()
    _cCodigo := _oCodLoja:_cCodigo
    _cLoja   := _oCodLoja:_cLoja
    _lMatriz := _oCodLoja:_lMatriz
EndIf

Return .T.

/***********************************************************************************/
/*/{Protheus.doc} BPFATM01G
    @description Amarra CNAE X Prospects
    @type  Static Function
    @author Bernard M Margarido
    @since 27/03/2024
    @version version
/*/
/***********************************************************************************/
Static Function BPFATM01G(_cCgc,_aCnae)
Local _aArea    := GetArea()

Local _cCnae    := ""

Local _nX       := 0

//---------------------------------+
// ZX2 - Tabela Cnae's secundários |
//---------------------------------+
dbSelectArea("ZX2")
ZX2->( dbSetOrder(1) )

For _nX := 1 To Len(_aCnae)
    If !Empty(_aCnae[_nX][1])

        _cCnae := Transform(_aCnae[_nX][1],"@R ####-#/##")

        If !ZX2->( dbSeek(xFilial("ZX2") + _cCgc + _cCnae))
            RecLock("ZX2",.T.)  
                ZX2->ZX2_FILIAL := xFilial("ZX2")
                ZX2->ZX2_CGC    := _cCgc
                ZX2->ZX2_CNAE   := _cCnae
                ZX2->ZX2_DESC   := _aCnae[_nX][2]
            ZX2->( MsUnLock() )
        EndIf
    EndIf
Next _nX

RestArea(_aArea)    
Return Nil 

/***********************************************************************************/
/*/{Protheus.doc} BPFATM01H
    @description Grava QSA X Prospects
    @type  Static Function
    @author Bernard M Margarido
    @since 27/03/2024
    @version version
/*/
/***********************************************************************************/
Static Function BPFATM01H(_cCgc,_aQSA)
Local _aArea    := GetArea()

Local _nX       := 0

//------------------------------------+
// Posiciona tabela natureza juridica | 
//------------------------------------+
dbSelectArea("ZX4")
ZX4->( dbSetOrder(1) )

For _nX := 1 To Len(_aQSA)
    If !Empty(_aQSA[_nX][1])
        If !ZX4->( dbSeek(xFilial("ZX4") + _cCgc) )
            RecLock("ZX4",.T.)
                ZX4_FILIAL  := xFilial("ZX4")
                ZX4_CGC     := _cCgc
                ZX4_NOME    := _aQSA[_nX][1]
                ZX4_QUALIF  := _aQSA[_nX][2]
                ZX4_DESC    := _aQSA[_nX][3]
            ZX4->( MsUnLock() )
        EndIf
    Endif
Next _nX

RestArea(_aArea)    
Return Nil 

/***********************************************************************************/
/*/{Protheus.doc} BPFATM01I
    @description Grava natureza juridica 
    @type  Static Function
    @author Bernard M Margarido
    @since 27/03/2024
    @version version
/*/
/***********************************************************************************/
Static Function BPFATM01I(_aNatJuri)
Local _aArea    := GetArea()

Local _nX       := 0
Local _nTCodJur := TamSx3("ZX6_CODIGO")[1]

//------------------------------------+
// Posiciona tabela natureza juridica | 
//------------------------------------+
dbSelectArea("ZX6")
ZX6->( dbSetOrder(1) )

For _nX := 1 To Len(_aNatJuri)
    If !Empty(_aNatJuri[_nX][1])
        If !ZX6->( dbSeek(xFilial("ZX6") + PadR(_aNatJuri[_nX][1],_nTCodJur)) )
            RecLock("ZX6",.T.)
                ZX6->ZX6_FILIAL := xFilial("ZX6")
                ZX6->ZX6_CODIGO := _aNatJuri[_nX][1]
                ZX6->ZX6_DESC   := _aNatJuri[_nX][2]
            ZX6->( MsUnLock() )
        EndIf
    EndIf
Next _nX

RestArea(_aArea)    
Return Nil 

/***********************************************************************************/
/*/{Protheus.doc} BPFATM01F
    @description Converte string QSA em array
    @type  Static Function
    @author Bernard M Margarido
    @since 27/03/2024
    @version version
/*/
/***********************************************************************************/
Static Function BPFATM01F(_cQSA)
Local _aLines    := {}
Local _aRecord   := {}
Local _aSepara   := {"NOME:", "QUALIFICACAO:", "NOME_REPRESENTANTE_LEGAL:", "QUALIFICACAO_REPRESENTANTE_LEGAL:", "PAIS_ORIGEM:"}

Local _cNome     := ""
Local _cQuali    := ""
Local _cCodQua   := ""
Local _cDesQua   := ""

Local _nX       := 1

// Divide a string em substrings usando o separador "NOME:"
_aLines := StrTokArr2(DecodeUTF8(_cQSA), _aSepara[1])

// Percorre as substrings para extrair as informações
For _nX := 1 To Len(_aLines)

    //------+
    // Nome |
    //------+
    If AT(_aSepara[2],_aLines[_nX]) > 0
        _cNome := SubStr(_aLines[_nX],1,AT(_aSepara[2],_aLines[_nX]) - 2)
    EndIf 

    //--------------+
    // Qualificação |
    //--------------+
    If AT(_aSepara[2],_aLines[_nX]) > 0
        _cQuali := SubStr(_aLines[_nX],AT(_aSepara[2],_aLines[_nX]) + 13, AT(_aSepara[3],_aLines[_nX]) - (AT(_aSepara[2],_aLines[_nX]) + 13))
        If AT("-",_cQuali) > 0 
            _cCodQua := SubStr(_cQuali,1,  AT("-",_cQuali) - 1)
            _cDesQua := SubStr(_cQuali,AT("-",_cQuali) + 1)
        EndIf 
    EndIf 
    
    aAdd(_aRecord,{_cNome,_cCodQua,_cDesQua})

Next _nX
   
Return _aRecord 

/***********************************************************************************/
/*/{Protheus.doc} BPFATM01J
    @description Executa as regras para clientes/prospects
    @type  Static Function
    @author Bernard M Margarido
    @since 27/06/2024
    @version version
/*/
/***********************************************************************************/
Static Function BPFATM01J(_cAlias,_lInclui,_oModel,_lBsTgv01,_lMVCCustomer)
Local _xRet             := ""

Local _lGatilho         := .T.

Default _oModel         := Nil 
Default _lInclui        := .F.
Default _lBsTgv01       := .F.
Default _lMVCCustomer   := .F.

//----------------------+
// Regras para prospect |
//----------------------+
If _cAlias == "SUS"
    //---------------------------+
    // Regra - CNAE contribuinte |
    //---------------------------+
    _xRet := U_BSFATM02("012","SUS",_lGatilho, Nil, @_oModel)
    _oModel:SetValue("US_CNAE"  , _xRet )

    //-------------------------+
    // Regra - Tipo de Cliente |
    //-------------------------+
    _xRet := U_BSFATM02("008","SUS",_lGatilho, Nil, @_oModel)
    _oModel:SetValue("US_TIPO"  , _xRet)

    //----------------------------+
    // Regra - Inscrição estadual | 
    //----------------------------+
    _xRet := U_BSFATM02("001","SUS",_lGatilho, Nil, @_oModel)
    _oModel:SetValue("US_INSCR" , _xRet )

    //-----------------+    
    // Regra - Suframa | 
    //-----------------+
    _xRet := U_BSFATM02("002","SUS",_lGatilho, Nil, @_oModel)
    _oModel:SetValue("US_SUFRAMA" , _xRet )

    //--------------------------+    
    // Regra - Simples Nacional | 
    //--------------------------+
    _xRet := U_BSFATM02("003","SUS",_lGatilho, Nil, @_oModel)
    _oModel:SetValue("US_XSIMPNA"  , _xRet)

    //-------------+    
    // Regra - MEI | 
    //-------------+
    _xRet := U_BSFATM02("004","SUS",_lGatilho, Nil, @_oModel)
    _oModel:SetValue("US_XTPJ" , _xRet)

    //-------------------------+
    // Regra - Ente federativo |
    //-------------------------+
    _xRet := U_BSFATM02("005","SUS",_lGatilho, Nil, @_oModel)
    _oModel:SetValue("US_XENTFED"  , _xRet)

    //---------------------+
    // Regra - Tipo Pessoa |
    //---------------------+
    _xRet := U_BSFATM02("006","SUS",_lGatilho, Nil, @_oModel)
    _oModel:SetValue("US_TPESSOA"  , _xRet)

    //--------------------+
    // Regra - Atacadista |
    //--------------------+
    _xRet := U_BSFATM02("007","SUS",_lGatilho, Nil, @_oModel)
    _oModel:SetValue("US_XATACAD"  , _xRet)

    //--------------------------+
    // Regra - Desconto Suframa |
    //--------------------------+
    _xRet := U_BSFATM02("010","SUS",_lGatilho, Nil, @_oModel)
    _oModel:SetValue("US_XCALCSU"  , _xRet)

    //-------------------------+
    // Regra - Regime Especial |
    //-------------------------+
    _xRet := U_BSFATM02("009","SUS",_lGatilho, Nil, @_oModel)
    _oModel:SetValue("US_XREGESP" , _xRet)

    //----------------------------+
    // Regra - Situação de Compra |
    //----------------------------+
    _xRet := U_BSFATM02("011","SUS",_lGatilho, Nil, @_oModel)
    _oModel:SetValue("US_XCOMPRA"  , _xRet)
    If _xRet == "2"
        _oModel:SetValue("US_MSBLQL" , "1")
        _oModel:SetValue("US_VEND", _cVndBlq)
        _oModel:SetValue("US_XVEND2", _cVndBlq)
    EndIf

    //----------------------+
    // Regra - Contribuinte |
    //----------------------+
    _xRet := U_BSFATM02("013","SUS",_lGatilho, Nil, @_oModel)
    _oModel:SetValue("US_CONTRIB"  , _xRet)

    //---------------------+
    // Regra - Veterinario |
    //---------------------+
    _xRet := U_BSFATM02("014","SUS",_lGatilho, Nil, @_oModel)
    _oModel:SetValue("US_XVETERI"  , _xRet)

    //---------------------------+
    // Regra - Credito Ourtogado |
    //---------------------------+
    _xRet := U_BSFATM02("021","SUS", _lGatilho, Nil, @_oModel)
    _oModel:SetValue("US_XCROURT", _xRet)

    //-----------------------------+
    // Regra - Grupo de Tributacao | 
    //-----------------------------+
    _xRet := U_BSFATM02("017","SUS",_lGatilho, Nil, @_oModel)
    _oModel:SetValue("US_GRPTRIB"  , _xRet)
    _lEnvMail := IIF(Empty(_xRet),.T.,.F.)

    //-------------------------+
    // Regra - Tabela de Preço |
    //-------------------------+
    _xRet := U_BSFATM02("022","SUS", _lGatilho, Nil, @_oModel)
    If !Empty(_xRet[1][1])
        _oModel:SetValue("US_TABPRE", _xRet[1][1])
    EndIf
    If !Empty(_xRet[1][2])
        _oModel:SetValue("US_XTABPR2", _xRet[1][2])
    EndIf

//----------------------+
// Regras para Clientes |
//----------------------+
ElseIf _cAlias == "SA1"
    If _lBsTgv01
        _lGatilho     := .T.

        If _lMVCCustomer
            //---------------------------+
            // Regra - CNAE contribuinte |
            //---------------------------+
            _xRet := U_BSFATM02("012","SA1",_lGatilho)
            _oModelSA1:SetValue("A1_CNAE", _xRet)
            
            //-------------------------+
            // Regra - Tipo de Cliente |
            //-------------------------+
            _xRet := U_BSFATM02("008","SA1",_lGatilho)
            _oModelSA1:SetValue("A1_TIPO", IIF(_lInclui,_xRet, SA1->A1_TIPO))

            //----------------------------+
            // Regra - Inscrição estadual | 
            //----------------------------+
            _xRet := U_BSFATM02("001","SA1",_lGatilho)
            _oModelSA1:SetValue("A1_INSCR", _xRet)

            //-----------------+    
            // Regra - Suframa | 
            //-----------------+
            _xRet := U_BSFATM02("002","SA1",_lGatilho)
            _oModelSA1:SetValue("A1_SUFRAMA", _xRet)

            //--------------------------+    
            // Regra - Simples Nacional | 
            //--------------------------+
            //_xRet := U_BSFATM02("003","SA1",_lGatilho)
            //SA1->A1_SIMPNAC  := _xRet
            //SA1->A1_SIMPLES  := _xRet

            //-------------+    
            // Regra - MEI | 
            //-------------+
            _xRet := U_BSFATM02("004","SA1",_lGatilho)
            _oModelSA1:SetValue("A1_TPJ", _xRet)

            //-------------------------+
            // Regra - Ente federativo |
            //-------------------------+
            _xRet := U_BSFATM02("005","SA1",_lGatilho)
            _oModelSA1:SetValue("A1_XENTFED", _xRet)

            //---------------------+
            // Regra - Tipo Pessoa |
            //---------------------+
            _xRet := U_BSFATM02("006","SA1",_lGatilho)
            _oModelSA1:SetValue("A1_TPESSOA", _xRet)

            //--------------------+
            // Regra - Atacadista |
            //--------------------+
            _xRet := U_BSFATM02("007","SA1",_lGatilho)
            If _lInclui
                _oModelSA1:SetValue("A1_XATACAD", _xRet)
            Else
                If _xRet == "1" .And. SA1->A1_XATACAD == "2"
                    _oModelSA1:SetValue("A1_XATACAD", _xRet)
                    _oModelSA1:SetValue("A1_XREGESP", "" )
                ElseIf _xRet == "1" .And. SA1->A1_XATACAD == "1"
                    _oModelSA1:SetValue("A1_XATACAD", _xRet)
                EndIf
            Endif

            //--------------------------+
            // Regra - Desconto Suframa |
            //--------------------------+
            _xRet := U_BSFATM02("010","SA1",_lGatilho)
            _oModelSA1:SetValue("A1_CALCSUF", _xRet)

            //-------------------------+
            // Regra - Regime Especial |
            //-------------------------+
            _xRet := U_BSFATM02("009","SA1",_lGatilho)
            _oModelSA1:SetValue("A1_XREGESP", IIF(_lInclui,_xRet,IIF(Empty(_xRet),SA1->A1_XREGESP,_xRet)))
                
            //----------------------------+
            // Regra - Situação de Compra |
            //----------------------------+
            _xRet := U_BSFATM02("011","SA1",_lGatilho)
            _oModelSA1:SetValue("A1_XCOMPRA", _xRet)
            If _xRet == "2"
                _oModelSA1:SetValue("A1_MSBLQL", "1")
                _oModelSA1:SetValue("A1_VEND", _cVndBlq)
                _oModelSA1:SetValue("A1_XVEND2", _cVndBlq)
            EndIf

            //----------------------+
            // Regra - Contribuinte |
            //----------------------+
            _xRet := U_BSFATM02("013","SA1",_lGatilho)
            _oModelSA1:SetValue("A1_CONTRIB", _xRet)

            //---------------------+
            // Regra - Veterinario |
            //---------------------+
            _xRet := U_BSFATM02("014","SA1",_lGatilho)
            _oModelSA1:SetValue("A1_XVETERI", IIF(_lInclui,_xRet,IIF(Empty(_xRet),SA1->A1_XVETERI,_xRet)))

            //---------------------------+
            // Regra - Municipio Suframa |
            //---------------------------+
            _xRet := U_BSFATM02("016","SA1",_lGatilho)
            _oModelSA1:SetValue("A1_CODMUN", _xRet)

            //--------------------+
            // Regra - Destaca IE |
            //--------------------+
            _xRet := U_BSFATM02("020","SA1",_lGatilho)
            _oModelSA1:SetValue("A1_IENCONT", _xRet )

            //---------------------------+
            // Regra - Credito Ourtogado |
            //---------------------------+
            If SA1->( FieldPos("A1_XCROURT") ) > 0
                _xRet := U_BSFATM02("021","SA1",_lGatilho)
                _oModelSA1:SetValue("A1_XCROURT", _xRet)
            EndIf
            
            //-----------------------------+
            // Regra - Grupo de Tributacao | 
            //-----------------------------+
            _xRet := U_BSFATM02("017","SA1",_lGatilho)
            _oModelSA1:SetValue("A1_GRPTRIB", _xRet )
            _lEnvMail := IIF(Empty(_xRet),.T.,.F.)
            
            //-------------------------+
            // Regra - Tabela de Preço |
            //-------------------------+
            _xRet := U_BSFATM02("022","SA1",_lGatilho)
            _oModelSA1:SetValue("A1_TABELA", IIF(ValType(_xRet) == "U" .Or. Empty(_xRet[1][1]), SA1->A1_TABELA, _xRet[1][1]))
            If !Empty(_xRet[1][2])
                _oModelSA1:SetValue("A1_XTABPR2", _xRet[1][2])
            EndIf  
        Else 
            //---------------------------+
            // Regra - CNAE contribuinte |
            //---------------------------+
            _xRet := U_BSFATM02("012","SA1",_lGatilho)
            M->A1_CNAE  := _xRet
            
            //-------------------------+
            // Regra - Tipo de Cliente |
            //-------------------------+
            _xRet := U_BSFATM02("008","SA1",_lGatilho)
            M->A1_TIPO  := IIF(_lInclui,_xRet, SA1->A1_TIPO)

            //----------------------------+
            // Regra - Inscrição estadual | 
            //----------------------------+
            _xRet := U_BSFATM02("001","SA1",_lGatilho)
            M->A1_INSCR  := _xRet

            //-----------------+    
            // Regra - Suframa | 
            //-----------------+
            _xRet := U_BSFATM02("002","SA1",_lGatilho)
            M->A1_SUFRAMA  := _xRet

            //--------------------------+    
            // Regra - Simples Nacional | 
            //--------------------------+
            //_xRet := U_BSFATM02("003","SA1",_lGatilho)
            //SA1->A1_SIMPNAC  := _xRet
            //SA1->A1_SIMPLES  := _xRet

            //-------------+    
            // Regra - MEI | 
            //-------------+
            _xRet := U_BSFATM02("004","SA1",_lGatilho)
            M->A1_TPJ  := _xRet

            //-------------------------+
            // Regra - Ente federativo |
            //-------------------------+
            _xRet := U_BSFATM02("005","SA1",_lGatilho)
            M->A1_XENTFED  := _xRet

            //---------------------+
            // Regra - Tipo Pessoa |
            //---------------------+
            _xRet := U_BSFATM02("006","SA1",_lGatilho)
            M->A1_TPESSOA  := _xRet

            //--------------------+
            // Regra - Atacadista |
            //--------------------+
            _xRet := U_BSFATM02("007","SA1",_lGatilho)
            If _lInclui
                M->A1_XATACAD  := _xRet
            Else
                If _xRet == "1" .And. SA1->A1_XATACAD == "2"
                    M->A1_XATACAD   := _xRet
                    M->A1_XREGESP   := ""
                ElseIf _xRet == "1" .And. SA1->A1_XATACAD == "1"
                    M->A1_XATACAD   := _xRet
                EndIf
            Endif

            //--------------------------+
            // Regra - Desconto Suframa |
            //--------------------------+
            _xRet := U_BSFATM02("010","SA1",_lGatilho)
            M->A1_CALCSUF  := _xRet

            //-------------------------+
            // Regra - Regime Especial |
            //-------------------------+
            _xRet := U_BSFATM02("009","SA1",_lGatilho)
            M->A1_XREGESP  := IIF(_lInclui,_xRet,IIF(Empty(_xRet),SA1->A1_XREGESP,_xRet))
                
            //----------------------------+
            // Regra - Situação de Compra |
            //----------------------------+
            _xRet := U_BSFATM02("011","SA1",_lGatilho)
            M->A1_XCOMPRA  := _xRet
            If _xRet == "2"
                M->A1_MSBLQL := "1"
                M->A1_VEND   := _cVndBlq
                M->A1_XVEND2 := _cVndBlq    
            EndIf

            //----------------------+
            // Regra - Contribuinte |
            //----------------------+
            _xRet := U_BSFATM02("013","SA1",_lGatilho)
            M->A1_CONTRIB  := _xRet

            //---------------------+
            // Regra - Veterinario |
            //---------------------+
            _xRet := U_BSFATM02("014","SA1",_lGatilho)
            M->A1_XVETERI  := IIF(_lInclui,_xRet,IIF(Empty(_xRet),SA1->A1_XVETERI,_xRet))

            //---------------------------+
            // Regra - Municipio Suframa |
            //---------------------------+
            _xRet := U_BSFATM02("016","SA1",_lGatilho)
            M->A1_CODMUN  := _xRet

            //--------------------+
            // Regra - Destaca IE |
            //--------------------+
            _xRet := U_BSFATM02("020","SA1",_lGatilho)
            M->A1_IENCONT := _xRet 

            //---------------------------+
            // Regra - Credito Ourtogado |
            //---------------------------+
            If SA1->( FieldPos("A1_XCROURT") ) > 0
                _xRet := U_BSFATM02("021","SA1",_lGatilho)
            M->A1_XCROURT  := _xRet
            EndIf
            
            //-----------------------------+
            // Regra - Grupo de Tributacao | 
            //-----------------------------+
            _xRet := U_BSFATM02("017","SA1",_lGatilho)
            M->A1_GRPTRIB := _xRet 
            _lEnvMail := IIF(Empty(_xRet),.T.,.F.)
            
            //-------------------------+
            // Regra - Tabela de Preço |
            //-------------------------+
            _xRet := U_BSFATM02("022","SA1",_lGatilho)
            M->A1_TABELA := IIF(ValType(_xRet) == "U" .Or. Empty(_xRet[1][1]), SA1->A1_TABELA, _xRet[1][1])
            If !Empty(_xRet[1][2])
                M->A1_XTABPR2 := _xRet[1][2]
            EndIf  
        EndIf 
    Else 
        _lGatilho   := .F.
        RecLock("SA1",.F.)
            //---------------------------+
            // Regra - CNAE contribuinte |
            //---------------------------+
            _xRet := U_BSFATM02("012","SA1",_lGatilho)
            SA1->A1_CNAE  := _xRet
            
            //-------------------------+
            // Regra - Tipo de Cliente |
            //-------------------------+
            _xRet := U_BSFATM02("008","SA1",_lGatilho)
            SA1->A1_TIPO  := IIF(_lInclui,_xRet, SA1->A1_TIPO)

            //----------------------------+
            // Regra - Inscrição estadual | 
            //----------------------------+
            _xRet := U_BSFATM02("001","SA1",_lGatilho)
            SA1->A1_INSCR  := _xRet

            //-----------------+    
            // Regra - Suframa | 
            //-----------------+
            _xRet := U_BSFATM02("002","SA1",_lGatilho)
            SA1->A1_SUFRAMA  := _xRet

            //--------------------------+    
            // Regra - Simples Nacional | 
            //--------------------------+
            //_xRet := U_BSFATM02("003","SA1",_lGatilho)
            //SA1->A1_SIMPNAC  := _xRet
            //SA1->A1_SIMPLES  := _xRet

            //-------------+    
            // Regra - MEI | 
            //-------------+
            _xRet := U_BSFATM02("004","SA1",_lGatilho)
            SA1->A1_TPJ  := _xRet

            //-------------------------+
            // Regra - Ente federativo |
            //-------------------------+
            _xRet := U_BSFATM02("005","SA1",_lGatilho)
            SA1->A1_XENTFED  := _xRet

            //---------------------+
            // Regra - Tipo Pessoa |
            //---------------------+
            _xRet := U_BSFATM02("006","SA1",_lGatilho)
            SA1->A1_TPESSOA  := _xRet

            //--------------------+
            // Regra - Atacadista |
            //--------------------+
            _xRet := U_BSFATM02("007","SA1",_lGatilho)
            If _lInclui
                SA1->A1_XATACAD  := _xRet
            Else
                If _xRet == "1" .And. SA1->A1_XATACAD == "2"
                    SA1->A1_XATACAD   := _xRet
                    SA1->A1_XREGESP   := ""
                ElseIf _xRet == "1" .And. SA1->A1_XATACAD == "1"
                    SA1->A1_XATACAD   := _xRet
                EndIf
            Endif

            //--------------------------+
            // Regra - Desconto Suframa |
            //--------------------------+
            _xRet := U_BSFATM02("010","SA1",_lGatilho)
            SA1->A1_CALCSUF  := _xRet

            //-------------------------+
            // Regra - Regime Especial |
            //-------------------------+
            _xRet := U_BSFATM02("009","SA1",_lGatilho)
            SA1->A1_XREGESP  := IIF(_lInclui,_xRet,IIF(Empty(_xRet),SA1->A1_XREGESP,_xRet))
                
            //----------------------------+
            // Regra - Situação de Compra |
            //----------------------------+
            _xRet := U_BSFATM02("011","SA1",_lGatilho)
            SA1->A1_XCOMPRA  := _xRet
            If _xRet == "2"
                SA1->A1_MSBLQL := "1"
                SA1->A1_VEND   := _cVndBlq
                SA1->A1_XVEND2 := _cVndBlq    
            EndIf

            //----------------------+
            // Regra - Contribuinte |
            //----------------------+
            _xRet := U_BSFATM02("013","SA1",_lGatilho)
            SA1->A1_CONTRIB  := _xRet

            //---------------------+
            // Regra - Veterinario |
            //---------------------+
            _xRet := U_BSFATM02("014","SA1",_lGatilho)
            SA1->A1_XVETERI  := IIF(_lInclui,_xRet,IIF(Empty(_xRet),SA1->A1_XVETERI,_xRet))

            //---------------------------+
            // Regra - Municipio Suframa |
            //---------------------------+
            _xRet := U_BSFATM02("016","SA1",_lGatilho)
            SA1->A1_CODMUN  := _xRet

            //--------------------+
            // Regra - Destaca IE |
            //--------------------+
            _xRet := U_BSFATM02("020","SA1",_lGatilho)
            SA1->A1_IENCONT := _xRet 

            //---------------------------+
            // Regra - Credito Ourtogado |
            //---------------------------+
            If SA1->( FieldPos("A1_XCROURT") ) > 0
                _xRet := U_BSFATM02("021","SA1",_lGatilho)
                SA1->A1_XCROURT  := _xRet
            EndIf
            
            //-----------------------------+
            // Regra - Grupo de Tributacao | 
            //-----------------------------+
            _xRet := U_BSFATM02("017","SA1",_lGatilho)
            SA1->A1_GRPTRIB := _xRet 
            _lEnvMail := IIF(Empty(_xRet),.T.,.F.)
            
            //-------------------------+
            // Regra - Tabela de Preço |
            //-------------------------+
            _xRet := U_BSFATM02("022","SA1",_lGatilho)
            SA1->A1_TABELA := IIF(ValType(_xRet) == "U" .Or. Empty(_xRet[1][1]), SA1->A1_TABELA, _xRet[1][1])
            If !Empty(_xRet[1][2])
                SA1->A1_XTABPR2 := _xRet[1][2]
            EndIf  
        SA1->( MsUnLock() )
    EndIf 
EndIf 

Return _oModel

/*******************************************************************************/
/*/{Protheus.doc} BPFATM01L
    @description Valida cliente e prospects
    @type  Static Function
    @author Bernard M Margarido
    @since 22/03/2024
    @version version
/*/
/*******************************************************************************/
Static Function BPFATM01L(_cAlias,_cCNPJ,_cError)
Local _aArea    := GetArea()

Local _lRet     := .T.

//----------------+
// Novo Prospects |
//----------------+
If _cAlias == "SUS"
    //----------------+
    // Valida cliente |
    //----------------+
    dbSelectArea("SA1")
    SA1->( dbSetOrder(3) )
    If SA1->( dbSeek(xFilial("SA1") + _cCNPJ))
        _cError := "Cliente já cadastrado Codigo: " + SA1->A1_COD + " Loja: " + SA1->A1_LOJA 
        _lRet := .F.
    EndIf
EndIf 

//--------------+
// Novo Cliente |
//--------------+
If _cAlias == "SA1"
    //-----------------+
    // Valida prospect |
    //-----------------+
    dbSelectArea("SUS")
    SUS->( dbSetOrder(3) )
    If SUS->( dbSeek(xFilial("SUS") + _cCNPJ)) .And. _lRet 
        _cError := "Prospect já cadastrado Codigo: " + SUS->US_COD + " Loja: " + SUS->US_LOJA 
        _lRet := .F.
    EndIf
EndIf

RestArea(_aArea)
Return _lRet 

/***********************************************************************************/
/*/{Protheus.doc} BPFATM01M
    @description Consulta codigo de municipio 
    @type  Static Function
    @author Bernard M Margarido
    @since 25/03/2024
    @version version
/*/
/***********************************************************************************/
Static Function BPFATM01M(_cMunicipio,_cUF)
Local _aArea	:= GetArea()
Local _cAlias	:= ""
Local _cQuery	:= ""
Local _cIbge	:= ""

If At("'",_cMunicipio) > 0
	_cMunicipio := StrTran(_cMunicipio,"'","''")
EndIf	

//-----------------------------+
// Cosulta codigo de municipio |
//-----------------------------+
_cQuery := " SELECT " + CRLF 
_cQuery += "	CC2_CODMUN " + CRLF 
_cQuery += " FROM " + CRLF 
_cQuery += "	" + RetSqlName("CC2") + CRLF   
_cQuery += " WHERE " + CRLF 
_cQuery += "	CC2_FILIAL = '" + xFilial("CC2") + "' AND " + CRLF 
_cQuery += "	CC2_EST = '" + _cUF + "' AND " + CRLF 
_cQuery += "	CC2_MUN = '" + _cMunicipio + "' AND " + CRLF 
_cQuery += "	D_E_L_E_T_ = '' " 

_cAlias := MPSysOpenQuery(_cQuery)

If Empty( (_cAlias)->CC2_CODMUN )
	(_cAlias)->( dbCloseArea() )	
	RestArea(_aArea)
	Return _cIbge
EndIf

_cIbge := (_cAlias)->CC2_CODMUN
(_cAlias)->( dbCloseArea() )	

RestArea(_aArea)    
Return _cIbge 

/***********************************************************************************/
/*/{Protheus.doc} BPFATM01N
    @descripton Converte string dos CNAES secundários para Array
    @type  Static Function
    @author Bernard M Margarido
    @since 27/03/2024
    @version version
/*/
/***********************************************************************************/
Static Function BPFATM01N(_cCNaeSec)
Local _cDesc    := ""
Local _cCodCNae := ""
Local _cStrCnae := ""

Local _nI       := 0 
Local _nLength  := 0 
Local _nStart   := 0

Local _lArray   := .F.

Local _aArray   := {}

//------------+
// Parametros |
//------------+
_cStrCnae:= DecodeUTF8(_cCNaeSec)
_cStrCnae:= u_SyAcento(_cCNaeSec,.T.)
_cDesc   := ""
_cCodCNae:= ""

_nI      := 1
_nStart  := 0
_nLength := Len(Alltrim(_cStrCnae))

_lArray  := .F.

While _nI <= _nLength 

    If SubStr(_cStrCnae, _nI, 1) $ "0/1/2/3/4/5/6/7/8/9"
        If _lArray
            aAdd(_aArray,{_cCodCNae,_cDesc})
            _cCodCNae := ""
            _cDesc    := ""
            _lArray   := .F.
        EndIf     
        _cCodCNae += SubStr(_cStrCnae, _nI, 1)
    ElseIf SubStr(_cStrCnae, _nI, 1) != " "
        _cDesc  += SubStr(_cStrCnae,_nI,1)
        _lArray := .T.
    EndIf 

    If _nI == _nLength
        If _lArray
            aAdd(_aArray,{_cCodCNae,_cDesc})
        EndIf 
    Endif 

    _nI++
EndDo 
    
Return _aArray

/***********************************************************************************/
/*/{Protheus.doc} BPFATM01O
    @description Realiza o preenchimento dos dados do cliennte
    @type  Static Function
    @author Bernard M Margarido
    @since 15/07/2024
    @version version
/*/
/***********************************************************************************/
Static Function BPFATM01O(_lInclui,_aCliente)
Local _nX := 0 

For _nX := 1 To Len(_aCliente)

    _cCpoSA1 := 'M->' + _aCliente[_nX][1]

    &(_cCpoSA1) := _aCliente[_nX][2]

Next _nX 

Return Nil 

/***********************************************************************************/
/*/{Protheus.doc} BPFATM01P
    @description Realiza a atualização dos dados para MVC
    @type  Static Function
    @author Bernard M Margarido
    @since 15/07/2024
    @version version
/*/
/***********************************************************************************/
Static Function BPFATM01P(_oModel,_lInclui,_aCliente)
Local _nX := 0 

For _nX := 1 To Len(_aCliente)
    _oModelSA1:SetValue(_aCliente[_nX][1], _aCliente[_nX][2])
Next _nX     

Return _oModel 
