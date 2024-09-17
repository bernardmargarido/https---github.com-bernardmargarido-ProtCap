#Include "PROTHEUS.CH"

// Programa:   BZMNTINT
// Finalidade: Monitor de Integracoes
// Autor:      Ronald Piscioneri
// Data:       25-Ago-2023

User Function BZMNTINT( cFilInt, cIDInt, cDescInt, nQtdReg )
Local lDbg := .F.
Local oFntBlk := TFont():New("Arial",,020,,.T.,,,,,.F.,.F.)
Local aLbx1 := {{"","","","",{}}}
Local oLbx1 := Nil
Local oBtAtl := Nil
Local oBtSair := Nil
Local oSyMsg1 := Nil
Local oSyDesc := Nil
Local oSyStt1 := Nil
Local oSyStt2 := Nil
Local oDlgMnt := Nil
Default cFilInt := "00"
Default cIdInt := "0012"
Default cDescInt := "Astrein - SSA-CAD"
Default nQtdReg := 25

If lDbg
    RpcSetEnv("01","00")
EndIf

Private cZ13Tbl := RetSqlName("Z13")

//Variavel cStMntInt e' atualizada no programa BZAPI001
Private cStMntInt := "indeterminado"

FwMsgRun(,{||AtDatMnt(cFilInt,cIdInt,nQtdReg,@aLbx1,cDescInt)},,"Atualizando Dados")

DEFINE MSDIALOG oDlgMnt TITLE "Monitor de Integração Protheus" FROM 000, 000  TO 600, 800 COLORS 0, 16777215 PIXEL

    @ 006, 005 SAY oSyDesc PROMPT cDescInt SIZE 390, 010 OF oDlgMnt FONT oFntBlk COLORS 0, 16777215 PIXEL
    @ 022, 005 SAY oSyStt1 PROMPT "Status:" SIZE 032, 011 OF oDlgMnt FONT oFntBlk COLORS 0, 16777215 PIXEL
    @ 022, 041 SAY oSyStt2 PROMPT cStMntInt SIZE 354, 011 OF oDlgMnt FONT oFntBlk COLORS 0, 16777215 PIXEL

    @ 039, 005 SAY oSyMsg1 PROMPT "Ultimas " +Alltrim(Str(nQtdReg))+ " integrações, ordenadas da mais recente para a mais antiga. Dê um duplo-clique na linha selecionada para exibir detalhes" SIZE 390, 007 OF oDlgMnt COLORS 0, 16777215 PIXEL
    @ 051, 005 LISTBOX oLbx1 Fields HEADER "Data","Hora","Requisicao","Erro" SIZE 392, 221 OF oDlgMnt PIXEL ColSizes 50,50
      oLbx1:SetArray(aLbx1)
      oLbx1:bLine := {|| {aLbx1[oLbx1:nAt,1],aLbx1[oLbx1:nAt,2],aLbx1[oLbx1:nAt,3],aLbx1[oLbx1:nAt,4]}}
      oLbx1:bLDblClick := {||ShowDet(aLbx1[oLbx1:nAt])}
    @ 280, 311 BUTTON oBtAtl PROMPT "Atualizar" Action(FwMsgRun(,{||AtDatMnt(cFilInt,cIdInt,nQtdReg,@aLbx1,cDescInt),oLbx1:Refresh(),oSyStt2:Refresh(),oDlgMnt:Refresh()},,"Atualizando Dados")) SIZE 037, 012 OF oDlgMnt PIXEL
    @ 280, 357 BUTTON oBtSair PROMPT "Sair" Action(oDlgMnt:End()) SIZE 037, 012 OF oDlgMnt PIXEL

ACTIVATE MSDIALOG oDlgMnt CENTERED

Return(Nil)



// Programa:   ATDATMNT
// Finalidade: Atualiza Dados do Monitor de Integracoes Bunzl
// Autor:      Ronald Piscioneri
// Data:       01-Set-2023

Static Function AtDatMnt( cFilInt, cIdInt, nQtdReg, aLbx1, cDescInt )
Local cQry := ""

//Atualiza "status" (online, offline ou indeterminado) para Astrein / SSA-CAD
If ( ("ASTREIN" $ Upper(Alltrim(cDescInt))) .Or. ("SSA-CAD" $ Upper(Alltrim(cDescInt))) .Or. ("SSACAD" $ Upper(Alltrim(cDescInt))) )
    U_BZAPI001()
EndIf

aLbx1 := {}

cQry := "SELECT TOP " +Alltrim(Str(nQtdReg))+ " "
cQry += "Z13_DTHIST AS DTINT, "
cQry += "Z13_HRHIST AS HRINT, "
cQry += "JSON = RTRIM(LTRIM(CAST(ISNULL(CONVERT(VARCHAR(MAX),CONVERT(VARBINARY(MAX),Z13_JSON)),'') AS CHAR(3000) ))), "
cQry += "ERRO = RTRIM(LTRIM(CAST(ISNULL(CONVERT(VARCHAR(MAX),CONVERT(VARBINARY(MAX),Z13_ERRO)),'') AS CHAR(3000) ))), "
cQry += "Z13_CHAVE AS CHV, "
cQry += "Z13_SEQ AS SEQ "
cQry += "FROM " +cZ13Tbl+ " WHERE D_E_L_E_T_ = ' ' "
cQry += "AND Z13_FILIAL = '" +cFilInt+ "' "
cQry += "AND Z13_ID = '" +cIdInt+ "' "
cQry += "ORDER BY R_E_C_N_O_ DESC"

Iif(Select("AWRK")>0,AWRK->(dbCloseArea()),Nil)
dbUseArea(.T.,"TOPCONN",TCGenQry(,,cQry),"AWRK",.T.,.T.)
TcSetField("AWRK","DTINT","D", 8, 0 )
AWRK->(dbGoTop())

While AWRK->(!EoF())
    aAdd( aLbx1, {  DtoC(AWRK->DTINT) ,;
                    AWRK->HRINT ,;
                    Alltrim(SubStr(Alltrim(AWRK->JSON),1,50)) ,;
                    Alltrim(SubStr(Alltrim(AWRK->ERRO),1,50)) ,;
                    { Alltrim(AWRK->JSON) , Alltrim(AWRK->ERRO), AWRK->CHV, AWRK->SEQ } ;
        })
    AWRK->(dbSkip())
EndDo
AWRK->(dbCloseArea())

If Len(aLbx1) == 0
    aLbx1 := {{"","","","",{}}}
EndIf

Return(Nil)



// Programa:   SHOWDET
// Finalidade: Exibe detalhes da linha
// Autor:      Ronald Piscioneri
// Data:       01-Set-2023

Static Function ShowDet(aDetLin)
Local cGData := Alltrim(aDetLin[1])
Local cGHora := Alltrim(aDetLin[2])
Local cGChave := Alltrim(aDetLin[5,3])
Local cMGReq := Alltrim(aDetLin[5,1])
Local cMGErr := Alltrim(aDetLin[5,2])
Local cGSeq := Alltrim(aDetLin[5,4])
Local oGData := Nil
Local oGHora := Nil
Local oGSeq := Nil
Local oMGErr := Nil
Local oMGReq := Nil
Local oBSair := Nil
Local oGChave := Nil
Local oSay1 := Nil
Local oSay2 := Nil
Local oSay3 := Nil
Local oSay4 := Nil
Local oSay5 := Nil
Local oSay6 := Nil
Local oDgDet := Nil

DEFINE MSDIALOG oDgDet TITLE "Detalhes" FROM 000, 000  TO 575, 748 COLORS 0, 16777215 PIXEL

    @ 003, 005 SAY oSay1 PROMPT "Data" SIZE 025, 007 OF oDgDet COLORS 0, 16777215 PIXEL
    @ 012, 005 MSGET oGData VAR cGData SIZE 052, 010 OF oDgDet COLORS 0, 16777215 READONLY PIXEL
    @ 003, 064 SAY oSay2 PROMPT "Hora" SIZE 025, 007 OF oDgDet COLORS 0, 16777215 PIXEL
    @ 012, 064 MSGET oGHora VAR cGHora SIZE 038, 010 OF oDgDet COLORS 0, 16777215 READONLY PIXEL
    @ 003, 109 SAY oSay3 PROMPT "Chave Doc" SIZE 085, 007 OF oDgDet COLORS 0, 16777215 PIXEL
    @ 012, 108 MSGET oGChave VAR cGChave SIZE 219, 010 OF oDgDet COLORS 0, 16777215 READONLY PIXEL
    @ 003, 334 SAY oSay4 PROMPT "Sequencia" SIZE 035, 007 OF oDgDet COLORS 0, 16777215 PIXEL
    @ 012, 333 MSGET oGSeq VAR cGSeq SIZE 036, 010 OF oDgDet COLORS 0, 16777215 READONLY PIXEL
    @ 028, 005 SAY oSay5 PROMPT "Requisição" SIZE 052, 007 OF oDgDet COLORS 0, 16777215 PIXEL
    @ 037, 005 GET oMGReq VAR cMGReq OF oDgDet MULTILINE SIZE 365, 105 COLORS 0, 16777215 READONLY HSCROLL PIXEL
    @ 150, 005 SAY oSay6 PROMPT "Erro" SIZE 025, 007 OF oDgDet COLORS 0, 16777215 PIXEL
    @ 159, 005 GET oMGErr VAR cMGErr OF oDgDet MULTILINE SIZE 365, 105 COLORS 0, 16777215 READONLY HSCROLL PIXEL
    @ 269, 332 BUTTON oBSair PROMPT "Sair" Action(oDgDet:End()) SIZE 037, 012 OF oDgDet PIXEL

ACTIVATE MSDIALOG oDgDet CENTERED

Return(Nil)
