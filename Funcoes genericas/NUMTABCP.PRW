#Include "Protheus.Ch"

// Funcao:     NUMTABCP
// Finalidade: Retorna nro. da tabela de preco de compra por fornecedor e loja
// Observacao: Deve ser declarado como X3_VLDUSER para os campos AIA_CODFOR e AIA_LOJFOR
// Autor:      Ronald Piscioneri
// Data:       18-Julho-2023

User Function NUMTABCP()
Local cRet := "   "
Local cQry := ""
Local cTblAIA := RetSqlName("AIA")
Local cFilAIA := xFilial("AIA")

If INCLUI

    If !Empty(M->AIA_CODFOR)
        If !Empty(M->AIA_LOJFOR)

            cQry := "SELECT MAX(AIA_CODTAB) AS MAXTAB "
            cQry += "FROM " +cTblAIA+ " (NOLOCK) WHERE D_E_L_E_T_ = ' ' "
            cQry += "AND AIA_FILIAL = '" +cFilAIA+ "' "
            cQry += "AND AIA_CODFOR = '" +M->AIA_CODFOR+ "' "   
            cQry += "AND AIA_LOJFOR = '" +M->AIA_LOJFOR+ "' "

            Iif(Select("AWRK")>0,AWRK->(dbCloseArea()),Nil)
            dbUseArea(.T.,"TOPCONN",TCGenQry(,,cQry),"AWRK",.T.,.T.)
            AWRK->(dbGoTop())

            If AWRK->(!EoF())
                If !Empty(AWRK->MAXTAB)
                    cRet := Soma1(AWRK->MAXTAB)
                EndIf
            EndIf
            AWRK->(dbCloseArea())

            If Empty(cRet)
                cRet := "001"
            EndIf

        EndIf
    EndIf

    M->AIA_CODTAB := cRet

EndIf    

Return(.T.)
