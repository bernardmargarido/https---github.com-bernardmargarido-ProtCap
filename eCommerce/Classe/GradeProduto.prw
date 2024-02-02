#INCLUDE "PROTHEUS.CH"  
#INCLUDE "FWMVCDEF.CH" 

/********************************************************************************************/
/*/{Protheus.doc} GradeProduto
    @description Classe - Realiza a validação dos eventos MVC
    @author Bernard M Margarido
    @since 25/01/2024
    @version version
/*/
/********************************************************************************************/
Class GradeProduto From FwModelEvent

    Method New() Constructor 
    Method After() 
    Method BeforeTTS()
    Method InTTS()
    Method GridLinePreVld()

    /*
        Method After(oSubModel, cModelId, cAlias, lNewRecord)                                   //Método que é chamado pelo MVC quando ocorrer as ações do commit depois da gravação de cada submodelo (field ou cada linha de uma grid)
        Method Before(oSubModel, cModelId, cAlias, lNewRecord)                                  //Método que é chamado pelo MVC quando ocorrer as ações do commit antes da gravação de cada submodelo (field ou cada linha de uma grid)
        Method AfterTTS(oModel, cModelId)                                                       //Método que é chamado pelo MVC quando ocorrer as ações do  após a transação. Esse evento ocorre uma vez no contexto do modelo principal.
        Method BeforeTTS(oModel, cModelId)                                                      //Método que é chamado pelo MVC quando ocorrer as ações do commit antes da transação. Esse evento ocorre uma vez no contexto do modelo principal.
        Method InTTS(oModel, cModelId)                                                          //Método que é chamado pelo MVC quando ocorrer as ações do commit Após as gravações porém antes do final da transação. Esse evento ocorre uma vez no contexto do modelo principal.
        Method Activate(oModel, lCopy)                                                          //Método que é chamado pelo MVC quando ocorrer a ativação do Model. Esse evento ocorre uma vez no contexto do modelo principal.
        Method DeActivate(oModel)                                                               //Método que é chamado pelo MVC quando ocorrer a desativação do Model. Esse evento ocorre uma vez no contexto do modelo principal.
        Method ModelPreVld(oModel, cModelId)                                                    //Método que é chamado pelo MVC quando ocorrer as ações de pre validação do Model. Esse evento ocorre uma vez no contexto do modelo principal.
        Method ModelPosVld(oModel, cModelId)                                                    //Método que é chamado pelo MVC quando ocorrer as ações de pos validação do Model. Esse evento ocorre uma vez no contexto do modelo principal.
        Method GridPosVld(oSubModel, cModelID)                                                  //Método que é chamado pelo MVC quando ocorrer as ações de pós validação do Grid.
        Method GridLinePreVld(oSubModel, cModelID, nLine, cAction, cId, xValue, xCurrentValue)  //Método que é chamado pelo MVC quando ocorrer as ações de pre validação da linha do Grid.
        Method GridLinePosVld(oSubModel, cModelID, nLine)                                       //Método que é chamado pelo MVC quando ocorrer as ações de pos validação da linha do Grid.
        Method FieldPreVld(oSubModel, cModelID, cAction, cId, xValue)                           //Método que é chamado pelo MVC quando ocorrer a ação de pré validação do Field.
        Method FieldPosVld(oSubModel, cModelID)                                                 //Método que é chamado pelo MVC quando ocorrer a ação de pós validação do Field.
        Method GetEvent(cIdEvent)                                                               //Método que retorna um evento superior da cadeia de eventos. Através do método InstallEvent, é possível encadear dois eventos que estão relacionados, como por exemplo um evento de negócio padrão e um evento localizado que complementa essa regra de negócio. Caso o evento localizado, necessite de atributos da classe superior, ele irá utilizar esse método para recuperá-lo.
    */

EndClass

/****************************************************************************************************************************/
/*/{Protheus.doc} New
    @description Metodo - Metodo construtor da classe 
    @author Bernard M Margarido
    @since 29/01/2024
    @version version
    @param param_name, param_type, param_descr
/*/
/****************************************************************************************************************************/
Method New() Class GradeProduto
Return Nil 

/****************************************************************************************************************************/
/*/{Protheus.doc} After
    @description Metodo - Chamado pelo MVC quando ocorrer as ações do commit depois da gravação de cada submodelo
    @author Bernard M Margarido
    @since 29/01/2024
    @version version
    @param param_name, param_type, param_descr
/*/
/****************************************************************************************************************************/
Method After(_oModel, _cModelId) Class GradeProduto
    
Return Nil 

/****************************************************************************************************************************/
/*/{Protheus.doc} BeforeTTS
    @description Metodo - Chamado pelo MVC quando ocorrer as ações do commit antes da transação
    @author Bernard M Margarido
    @since 29/01/2024
    @version version
    @param param_name, param_type, param_descr
/*/
/****************************************************************************************************************************/
Method BeforeTTS(_oModel, _cModelId) Class GradeProduto
Local _aArea        := GetArea() 
Local _aDelAux      := {}
Local _aRetInt      := {}

Local _lRet         := .T.
Local _lDeleta      := .F.
Local _lIntegDef	:= FWHasEAI("MATA550",.T.,,.T.)

Local _nOpcA        := _oModel:GetOperation()
Local _nX           := 0
Local _nLinhas      := 0

Local _oModelSKU    := _oModel:GetModel("SKU_MASTER")

//-------------------------------------+
// Valida se dados podem ser excluidos |
//-------------------------------------+
If _nOpcA == 5
    _nLinhas := _oModelSKU:Length()

    For _nX := 1 To _nLinhas
        _oModelSKU:GoLine(_nX)
        _lDeleta    := .F. 
        _cCodSku    := _oModelSKU:GetValue("SKU_COD")
        _lDeleta    := a550Prod(_cCodSku)

        If _lDeleta
            aAdd(_aDelAux,_cCodSku)
        Else 
            _lRet := .F. 
            Exit
        EndIf 
    Next _nX
    
    If _lRet .And. _lIntegDef
        _aRetInt := FwIntegDef("MATA550",,,,"MATA550")
        If Valtype(_aRetInt) == "A" .And. Len(_aRetInt) == 2 .And. !_aRetInt[1]
            If Empty(AllTrim(_aRetInt[2]))
                _cMsg := "Existem produtos que não podem ser deletados."
            Else
                _cMsg := AllTrim(_aRetInt[2])
            EndIf
            _lRet := .F.
        EndIf 
    EndIf 

    //-----------------+
    // Deleta produtos |
    //-----------------+
    If _lRet
        For _nX := 1 To Len(_aDelAux)
            a550Del(_aDelAux[_nX])
        Next _nX 
    EndIf 

    //------------------+
    // Mensagem retorno |
    //------------------+
    If !_lRet .And. !Empty(_cMsg)
        Help( ,, 'ECLOJ005',, _cMsg, 1, 0 )
    EndIf 

EndIf 

RestArea(_aArea)
Return _lRet 

/****************************************************************************************************************************/
/*/{Protheus.doc} InTTS
    @description Metodo - Chamado pelo MVC quando ocorrer as ações do commit Após as gravações porém antes do final da transação
    @author Bernard M Margarido
    @since 29/01/2024
    @version version
    @param param_name, param_type, param_descr
/*/
/****************************************************************************************************************************/
Method InTTS(_oModel, _cModelId) Class GradeProduto
Local _aArea        := GetArea() 

Local _lRet         := .T.

//-------------------+
// Inclui / Atualiza |
//-------------------+
If _oModel:GetOperation() == 3 .Or. _oModel:GetOperation() == 4
    
    Begin Transaction 
        
        FwMsgRun(,{|_oSay| _lRet := GrvGrade(_oModel, _cModelId,_oSay)},"Grade Produtos","Aguarde, atualizando grade de produtos.")
        If !_lRet
            DisarmTransaction()
        EndIf 

    End Transaction
    
EndIf 

RestArea(_aArea)
Return _lRet 

/****************************************************************************************************************************/
/*/{Protheus.doc} GridLinePreVld
    @description Metodo - Chamado pelo MVC quando ocorrer as ações de pos validação da linha do Grid.
    @author Bernard M Margarido
    @since 29/01/2024
    @version version
    @param param_name, param_type, param_descr
/*/
/****************************************************************************************************************************/
Method GridLinePreVld(_oSubModel, _cModelID, _nLine, _cAction, _cId, _xValue, _xCurrentValue) Class GradeProduto
Local _oView 	    := FWViewActive()
Local _oModel	    := FWModelActive()

Local _aMascara     := Separa(GetMv("MV_MASCGRD"),",")

Local _cMsg         := ""
Local _cCodSku      := ""
Local _cCodPai      := ""
Local _cCodLin      := ""
Local _cCodCol      := ""
Local _cTabLin      := ""

Local _nTPai        := Val(_aMascara[1])
Local _nTLinha      := Val(_aMascara[2])
Local _nTColuna     := Val(_aMascara[3])

Local _lRet         := .T.

//----------+
// GRID SKU |
//----------+
If _cModelID == "SKU_MASTER"
    If _cAction == "DELETE"
        If !_oSubModel:GetValue("SKU_DELET")
            _cMsg := "Não é possível deletar SKU de produtos alterados."
            _lRet := .F.
        Else 
            _oModelSB4  := _oModel:GetModel( "SB4_MASTER" )
            _oModelGRD  := _oModel:GetModel( "GRD_GRADE" )
            _cCodSku    := _oSubModel:GetValue("SKU_COD")
            _cTabLin    := _oModelSB4:GetValue("B4_LINHA")
            _cCodPai    := SubStr(_cCodSku, 1, _nTPai)
            _cCodLin    := SubStr(_cCodSku, _nTPai + 1, _nTLinha) 
            _cDescLin   := Posicione("SBV", 1, xFilial("SBV") + _cTabLin + _cCodLin,"BV_DESCRI") //RTrim(_cLinGrd) + " - [ " + RTrim(_cDescLin) + " ]"
            _cCodLin    := RTrim(_cCodLin) + " - [ " + RTrim(_cDescLin) + " ]"
            _cCodCol    := SubStr(_cCodSku, _nTPai + _nTLinha + 1, _nTColuna)

            If _oModelGRD:SeekLine({{"GRD_LINHA",_cCodLin},{"GRD_" + _cCodCol, "X"}})
                _oModelGRD:LoadValue("GRD_" + _cCodCol, " ")
            EndIf

        EndIf 
    EndIf 
ElseIf _cModelID == "GRD_GRADE"
    
EndIf 

//------------------------+
// Atualiza dados na tela |
//------------------------+
If _oView <> Nil
    _oView:Refresh()
EndIf

//------------------+
// Mensagem retorno |
//------------------+
If !_lRet
    Help( ,, 'ECLOJ005',, _cMsg, 1, 0 )
EndIf 

Return _lRet 

/****************************************************************************************************************************/
/*/{Protheus.doc} GrvGrade
    @description Realiza a atualização da grade de produtos
    @type  Static Function
    @author Bernard M. Margarido
    @since 01/02/2024
    @version version
/*/
/****************************************************************************************************************************/
Static Function GrvGrade(_oModel, _cModelId,_oSay)
Local _aStruB4      := SB4->( dbStruct() )

Local _cCposNGrv    := "B4_FILIAL\B4_COD\B4_LINHA\B4_COLUNA\B4_STATUS\B4_XQE\B4_XCOMPR\B4_XFATOR1\B4_XFATOR2\B4_XFATOR3\B4_PRV2\B4_PRV3"
Local _cCodSku      := ""
Local _cCodPai      := ""

Local _nX           := 0
Local _nY           := 0
Local _nPos         := 0
Local _nLinhas      := 0 

Local _lRet         := .T.
Local _lGrvSB0      := .T.
Local _lGrvSB5      := .T.
Local _lGrava       := .T.
Local _lECommerce   := SuperGetMV("MV_LJECOMM",.F.,.F.) 
Local _lComplECom   := SuperGetMV("MV_LJGRMHH",.F.,.F.) .And. TableInDic("MHH") 
Local _lGeraSB0     := SuperGetMV("PC_GERSB0",.F.,.T.)
Local _lAtuECFld    := SB0->(FieldPos("B0_ECDTEX")) > 0 .AND. SB0->(FieldPos("B0_ECSEQ")) > 0 
Local _lAltComp     := _lComplECom .And. MHH->(ColumnPos( "MHH_ECFLAG" )) > 0 

Local _oModelSB4    := _oModel:GetModel("SB4_MASTER")
Local _oModelMHH    := _oModel:GetModel("MHH_MASTER")
Local _oModelSKU    := _oModel:GetModel("SKU_MASTER")

//-------------------------+
// SB0 - Preços de produto |
//-------------------------+
dbSelectArea("SB0")
SB0->( dbSetOrder(1) )

//--------------------------+
// SB1 - Tabela de Produtos |
//--------------------------+
dbSelectArea("SB1")
SB1->( dbSetOrder(1) )

//--------------------------------+
// SB5 - Complemeento de produtos |
//--------------------------------+
dbSelectArea("SB5")
SB5->( dbSetOrder(1) )

_nLinhas := _oModelSKU:Length()
_cCodPai := _oModelSB4:GetValue("B4_COD")

For _nX := 1 To _nLinhas
    _oModelSKU:GoLine(_nX)

    _lGrava     := .T.
    _cCodSku    := _oModelSKU:GetValue("SKU_COD")
    _cDescSKU   := _oModelSKU:GetValue("SKU_DESC")
    _cCodBar    := _oModelSKU:GetValue("SKU_CODBAR")

    If !_oModelSKU:IsDeleted()

        //------------------------+
        // Regra de processamento |
        //------------------------+
        _oSay:cCaption := "Produto " + RTrim(_cCodSku) + " - " + RTrim(_cDescSKU)
        ProcessMessages()

        //-------------------------+
        // SB1 - Posiciona produto |
        //-------------------------+
        If SB1->( dbSeek(xFilial("SB1") + _cCodSku ))
            _lGrava := .F.
        EndIf 

        //--------------+
        // Atualiza SKU |
        //--------------+
        RecLock("SB1", _lGrava)

            If _lGrava 
                SB1->B1_FILIAL 	:= xFilial('SB1')
                SB1->B1_COD 	:= _cCodSku
                SB1->B1_GRADE   := 'S'
            EndIf 

            For _nY := 1 To Len(_aStruB4)
                If RTrim(_aStruB4[_nY][1]) $ _cCposNGrv
                    Loop
                ElseIf RTrim(_aStruB4[_nY][1]) == "B4_MSBLQL"
                    SB1->B1_MSBLQL := IIF(Empty(_oModelSKU:GetValue("B4_MSBLQL")), "2", _oModelSKU:GetValue("B4_MSBLQL") )
                ElseIf RTrim(_aStruB4[_nY][1]) == "B4_DESC"
                    SB1->B1_DESC := _cDescSKU
                Else
                    _cCpoSB1 := 'SB1->B1' + SubStr(_aStruB4[_nY][1],3)
                    _cCpoSB4 := 'M->B4' + SubStr(_aStruB4[_nY][1],3)
                    If  (_nPos := SB1->(FieldPos('B1'+SubStr(_aStruB4[_nY][1],3)))) > 0
                        &(_cCpoSB1) := IIF(Empty(&(_cCpoSB4)), CriaVar(_cCpoSB1,.T.), &(_cCpoSB4))
                    EndIf 
                EndIf 
            Next _nY 

            SB1->B1_CODBAR := _cCodBar
            SB1->B1_PRODPAI:= _cCodPai

        SB1->( MsUnLock() )

        //---------------------+
        // Valida atualiza SB0 |
        //---------------------+
        If _lGeraSB0

            //-----------------------------------+
            // SB0 - Atualiza preço dos produtos |
            //-----------------------------------+
            _lGrvSB0 := .T.
            If SB0->( dbSeek(xFilial("SB0") + _cCodSku) )
                _lGrvSB0    := .F.
            EndIf 

            RecLock("SB0",_lGrvSB0)

            If _lGrvSB0
                SB0->B0_FILIAL  := xFilial("SB0")
                SB0->B0_COD     := _cCodSku
            EndIf 

            SB0->B0_PRV1 := _oModelSB4:GetValue("B4_PRV1")
            SB0->B0_PRV2 := _oModelSB4:GetValue("B4_PRV2")
            SB0->B0_PRV3 := _oModelSB4:GetValue("B4_PRV3")
            If _lECommerce .And. _lAtuECFld 
                SB0->B0_ECDTEX := ""
                SB0->B0_ECSEQ  := ""
            EndIf
            SB0->(MsUnlock())
        EndIf 

        //--------------------------------------------+
        // Valida se atualiza complemento de produtos |
        //--------------------------------------------+
        If _lAltComp
            //----------------------------------------+
            // SB5 - Atualiza complemento de produtos |
            //----------------------------------------+
            _lGrvSB5    := .T.
            If SB5->(dbSeek(xFilial("SB5") + _cCodSku))
                _lGrvSB5 := .F.
            EndIf

            RecLock("SB5",_lGrvSB5)

            If _lGrvSB5
                SB5->B5_FILIAL  := xFilial("SB5")
                SB5->B5_COD     := _cCodSku	
                SB5->B5_CEME    := _cDescSKU
            EndIf 

            If _lComplECom
                SB5->B5_ECTITU  := _oModelMHH:GetValue("MHH_ECTITU")
                SB5->B5_ECDESCR := _oModelMHH:GetValue("MHH_ECDESC")
                SB5->B5_ECCARAC := _oModelMHH:GetValue("MHH_ECCARA")
                SB5->B5_ECFLAG  := _oModelMHH:GetValue("MHH_ECFLAG")
                SB5->B5_ECCUBAG := _oModelMHH:GetValue("MHH_ECCUBA")
                SB5->B5_ECPROFU := _oModelMHH:GetValue("MHH_ECPROF")
                SB5->B5_ECCOMP  := _oModelMHH:GetValue("MHH_ECCOMP")
                SB5->B5_ECLARGU := _oModelMHH:GetValue("MHH_ECLARG")
            EndIf
            SB5->( MsUnlock() )
        EndIf 

    //------------------------------------+
    // Linha deletada ou grade desmarcada |
    //------------------------------------+
    Else
        //-------------------------+
        // SB1 - Posiciona produto |
        //-------------------------+
        If SB1->( dbSeek(xFilial("SB1") + _cCodSku ))
            //-------------------------------------+
            // Valida se produto pode ser deletado |
            //-------------------------------------+
            _lRet := a550Prod(_cCodSku)
            If _lRet 
                a550Del(_cCodSku)
                
                If SB0->( dbSeek(xFilial("SB0") + _cCodSku) )
                    RecLock("SB0",.F.)
                        SB0->( dbDelete() )
                    SB0->( MsUnLock() )
                EndIf 
            EndIf 
        EndIf 
    EndIf 
Next _nX 

Return _lRet 
