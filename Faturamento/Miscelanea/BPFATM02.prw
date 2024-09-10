#INCLUDE "TOTVS.CH"

/***********************************************************************************************/
/*/{Protheus.doc} BPFATM02
    @description Realiza o envio dos dados do pre cadastro para a 4MDG
    @type  Function
    @author Bernard M Margarido
    @since 03/07/2024
    @version version
/*/
/***********************************************************************************************/
User Function BPFATM02(_cCodCli,_cLojaCli)
Local _lRet     := .T.

Local _cBussID  := ""
Local _cEmail   := ""
Local _cError   := ""
Local _cStatus  := "1"
Local _cChave   := ""
Local _cIdProc  := "0048"

Local _oJSon    := JSonObject():New() 
Local _oContato := Nil 

//--------------------------+
// SA1 - Posiciona clientes |
//--------------------------+
dbSelectArea("SA1")
SA1->( dbSetOrder(1) )
If !SA1->( dbSeek(xFilial("SA1") + _cCodCli + _cLojaCli) )
    Return .F.
EndIf 

//----------------------+
// Valida tipo de envio |
//----------------------+

//---------------------+
// Cliente estrangeiro |
//---------------------+
If SA1->A1_EST == "EX"
    _cBussID                        := "499664c4-0d57-479c-bde2-1b27a85b4c54"
    _cChave                         := "499664c4-0d57-479c-bde2-1b27a85b4c54"
    _cEmail                         := IIF(Empty(SA1->A1_MAILFIN),SA1->A1_EMAIL,SA1->A1_MAILFIN)
    _oJSon['A1_EMPRESA']            := '003'
    _oJSon['A1_COD']                := RTrim(SA1->A1_COD)
    _oJSon['A1_LOJA']               := RTrim(SA1->A1_LOJA)
    _oJSon['A1_BAIRRO']             := RTrim(SA1->A1_BAIRRO)
    _oJSon['A1_CEP']                := RTrim(SA1->A1_CEP)
    _oJSon['A1_NIF']                := RTrim(SA1->A1_NIF)
    _oJSon['A1_COMPLEM']            := RTrim(SA1->A1_COMPLEM)
    _oJSon['A1_EMLFIN']             := IIF(AT(";",_cEmail) > 0, SubStr(_cEmail, 1, AT(";",_cEmail) - 1), RTrim(_cEmail))
    _oJSon['A1_END']                := IIF(AT(",",SA1->A1_END ) > 0, RTrim(SubStr(SA1->A1_END, 1, AT(",",SA1->A1_END ) )), RTrim(SA1->A1_END))
    _oJSon['A1_NUM']                := IIF(AT(",",SA1->A1_END ) > 0, SubStr(SA1->A1_END, AT(",",SA1->A1_END ) + 1), "S/N")
    _oJSon['A1_EST']                := SA1->A1_EST
    _oJSon['A1_MUN']                := RTrim(SA1->A1_MUN)
    _oJSon['A1_NOME']               := RTrim(SA1->A1_NOME)
    _oJSon['A1_PAIS']               := IIF(Empty(SA1->A1_PAIS), "", Posicione("SYA", 1, xFilial("SYA") + SA1->A1_PAIS, "YA_DESCR"))
    _oJSon['A1_GRPVEN']             := RTrim(SA1->A1_GRPVEN)
    _oJSon['A1_SATIV1']             := RTrim(SA1->A1_SATIV1)
    _oJSon['cadastro_ex_contato']   := {}
    _oContato                       := JSonObject():New()
    _oContato['A1_EMLCOMP']         := IIF(At(";",_cEmail) > 0, SubStr(_cEmail, 1, At(";",_cEmail) - 1), RTrim(_cEmail))
    _oContato['A1_NREDUZ']          := RTrim(SA1->A1_NREDUZ)
    //_oContato['A1_DDD']             := IIF(Empty(SA1->A1_DDD), "", SA1->A1_DDD)
    _oContato['A1_TEL']             := RTrim(u_EcFormat(SA1->A1_TEL,"A1_TEL")) //IIF(Empty(SA1->A1_DDD), TransForm(SA1->A1_TEL, "@R ####-####"), TransForm(RTrim(SA1->A1_DDD) + RTrim(SA1->A1_TEL), "@R ##-####-####"))
    aAdd(_oJSon['cadastro_ex_contato'],_oContato)

Else 

    //-------------------------+
    // Cliente pessoa juridica |
    //-------------------------+
    If SA1->A1_PESSOA == "J" 
        _cBussID                                    := "21162401-ef50-4726-a7e4-ac96b7161215"
        _cChave                                     := "21162401-ef50-4726-a7e4-ac96b7161215"
        _cEmail                                     := IIF(Empty(SA1->A1_MAILFIN),SA1->A1_EMAIL,SA1->A1_MAILFIN)
        _oJSon['A1_EMPRESA']                        := '001'
        _oJSon['A1_COD']                            := RTrim(SA1->A1_COD)
        _oJSon['A1_LOJA']                           := RTrim(SA1->A1_LOJA)
        _oJSon['A1_CGC']                            := RTrim(SA1->A1_CGC)
        _oJSon['A1_HPAGE']                          := "" //IIF(Empty(SA1->A1_HPAGE), "", '{ \"url\": \"http:\/\/' + RTrim(SA1->A1_HPAGE) + '\", \"label\": \"Acesse nosso site\"}')
        _oJSon['A1_EMAIL']                          := IIF(AT(";",SA1->A1_EMAIL) > 0, SubStr(SA1->A1_EMAIL, 1, AT(";",SA1->A1_EMAIL) - 1), RTrim(SA1->A1_EMAIL))
        _oJSon['A1_INSCR']                          := IIF(Empty(SA1->A1_INSCR), "ISENTO", RTrim(SA1->A1_INSCR))
        _oJSon['A1_SUFRAMA']                        := RTrim(SA1->A1_SUFRAMA)
        _oJSon['A1_GRPVEN']                         := RTrim(SA1->A1_GRPVEN)
        _oJSon['A1_SATIV1']                         := RTrim(SA1->A1_SATIV1)
        _oJSon['cadastro_pessoa_juridica_contato']  := {}
        _oContato                                   := JSonObject():New()
        _oContato['A1_EMLCOMP']                     := IIF(At(";",_cEmail) > 0, SubStr(_cEmail, 1, At(";",_cEmail) - 1), RTrim(_cEmail))
        _oContato['A1_NOME']                        := RTrim(SA1->A1_NOME)
        _oContato['A1_DDD']                         := IIF(Empty(SA1->A1_DDD), "", SA1->A1_DDD)
        _oContato['A1_TEL']                         := RTrim(u_EcFormat(SA1->A1_TEL,"A1_TEL")) //IIF(Empty(SA1->A1_DDD), TransForm(SA1->A1_TEL, "@R ####-####"), TransForm(RTrim(SA1->A1_DDD) + RTrim(SA1->A1_TEL), "@R ##-####-####"))
        aAdd(_oJSon['cadastro_pessoa_juridica_contato'],_oContato)

    //-----------------------+
    // Cliente pessoa fisica | 
    //-----------------------+
    ElseIf SA1->A1_PESSOA == "F"

        //--------------------------------------+
        // Cliente pessoa fisica produtor rural | 
        //--------------------------------------+
        If SA1->A1_TIPO == "L"
            _cBussID                                    := "eebfc166-1214-4f93-93a8-3f95c8bc2e80"
            _cChave                                     := "eebfc166-1214-4f93-93a8-3f95c8bc2e80"
            _cEmail                                     := IIF(Empty(SA1->A1_MAILFIN),SA1->A1_EMAIL,SA1->A1_MAILFIN)
            _oJSon['A1_EMPRESA']                        := '003'
            _oJSon['A1_COD']                            := RTrim(SA1->A1_COD)
            _oJSon['A1_LOJA']                           := RTrim(SA1->A1_LOJA)
            _oJSon['A1_CGC']                            := RTrim(SA1->A1_CGC)
            _oJSon['A1_EMAIL']                          := IIF(AT(";",SA1->A1_EMAIL) > 0, SubStr(SA1->A1_EMAIL, 1, AT(";",SA1->A1_EMAIL) - 1), RTrim(SA1->A1_EMAIL))
            _oJSon['A1_INSCR']                          := IIF(Empty(SA1->A1_INSCR), "ISENTO", RTrim(SA1->A1_INSCR))
            _oJSon['A1_EST']                            := SA1->A1_EST
            _oJSon['A1_GRPVEN']                         := RTrim(SA1->A1_GRPVEN)
            _oJSon['A1_SATIV1']                         := RTrim(SA1->A1_SATIV1)
            _oJSon['cadastro_pr_contato']               := {}
            _oContato                                   := JSonObject():New()
            _oContato['A1_EMLCOMP']                     := IIF(AT(";",_cEmail) > 0, SubStr(_cEmail, 1, AT(";",_cEmail) - 1), RTrim(_cEmail))
            _oContato['A1_NOME']                        := RTrim(SA1->A1_NOME)
            _oContato['A1_DDD']                         := IIF(Empty(SA1->A1_DDD), "", SA1->A1_DDD)
            _oContato['A1_TEL']                         := RTrim(u_EcFormat(SA1->A1_TEL,"A1_TEL")) //IIF(Empty(SA1->A1_DDD), TransForm(SA1->A1_TEL, "@R ####-####"), TransForm(RTrim(SA1->A1_DDD) + RTrim(SA1->A1_TEL), "@R ##-####-####"))
            aAdd(_oJSon['cadastro_pr_contato'],_oContato)
        //-----------------------+
        // Cliente pessoa fisica |
        //-----------------------+
        Else 
            _cBussID                                    := "a7bc176c-b32b-4870-ac8c-397b2b6f0478"
            _cChave                                     := "a7bc176c-b32b-4870-ac8c-397b2b6f0478"
            _cEmail                                     := IIF(Empty(SA1->A1_MAILFIN),SA1->A1_EMAIL,SA1->A1_MAILFIN)
            _oJSon['A1_EMPRESA']                        := '003'
            _oJSon['A1_COD']                            := RTrim(SA1->A1_COD)
            _oJSon['A1_LOJA']                           := RTrim(SA1->A1_LOJA)
            _oJSon['A1_CEP']                            := SA1->A1_CEP
            _oJSon['A1_CGC']                            := RTrim(SA1->A1_CGC)
            _oJSon['A1_COMPLEM']                        := RTrim(SA1->A1_COMPLEM)
            _oJSon['A1_EMAIL']                          := IIF(AT(";",SA1->A1_EMAIL) > 0, SubStr(SA1->A1_EMAIL, 1, AT(";",SA1->A1_EMAIL) - 1), RTrim(SA1->A1_EMAIL))
            _oJSon['A1_GRPVEN']                         := RTrim(SA1->A1_GRPVEN)
            _oJSon['A1_SATIV1']                         := RTrim(SA1->A1_SATIV1)
            _oJSon['cadastro_pessoa_fisica_contato']    := {}
            _oContato                                   := JSonObject():New()
            _oContato['A1_EMLCOMP']                     := IIF(At(";",_cEmail) > 0, SubStr(_cEmail, 1, At(";",_cEmail) - 1), RTrim(_cEmail))
            _oContato['A1_NOME']                        := RTrim(SA1->A1_NOME)
            _oContato['A1_DDD']                         := IIF(Empty(SA1->A1_DDD), "", SA1->A1_DDD)
            _oContato['A1_TEL']                         := RTrim(u_EcFormat(SA1->A1_TEL,"A1_TEL")) //IIF(Empty(SA1->A1_DDD), TransForm(SA1->A1_TEL, "@R ####-####"), TransForm(RTrim(SA1->A1_DDD) + RTrim(SA1->A1_TEL), "@R ##-####-####"))
            aAdd(_oJSon['cadastro_pessoa_fisica_contato'],_oContato)
        EndIf 
    EndIf 
EndIf 

//--------------+
// Atualiza Z12 |
//--------------+
U_BzApi01d(_cIdProc,_cChave,_oJSon:ToJson(),_cError,_cStatus)

FreeObj(_oJSon)

Return _lRet 
