#INCLUDE 'RWMAKE.CH'
#INCLUDE 'PROTHEUS.CH'
#INCLUDE 'TBICONN.CH'
#INCLUDE 'TOTVS.CH'

/*
===============================================================================================
===============================================================================================
||   Arquivo:	    GTPLAN.prw
===============================================================================================
||   Funcao: 	    
===============================================================================================
||   Descrição: 	Biblioteca de funções GTPLAN.
===============================================================================================
||   Autor:	Guelder A. Santos
||   Data:	24/05/2021
===============================================================================================
===============================================================================================
*/



/*
===============================================================================================
===============================================================================================
||   Arquivo:	    GTPLAN.prw
===============================================================================================
||   Funcao: 	    CONNECT
===============================================================================================
||   Descrição: 	Realiza a conexão com a API GTPLAN.
===============================================================================================
||   Autor:	Guelder A. Santos
||   Data:	24/05/2021
===============================================================================================
===============================================================================================
*/
User Function CONNECT(cMethod,cBody)

    Local cURL       := GETMV("BZ_GTPURL" ,.F.,'https://restbus.gtplanqa.net/')
    Local cUser      := GETMV("BZ_GTPUSR" ,.F.,'9E51D07A56FFB6E53E70AC6685607196') 
    Local cPass      := GETMV("BZ_GTPPAS"  ,.F.,'2E2079D63348233D91CAD1FA9B1361E9')
    Local cToken	 := Encode64( Alltrim( cUser ) + ":" + Alltrim( cPass ) )
    Local cParam     := cMethod+'/?action=INSERT'
    Local aRet       := {.T.,""}    // Posição 1 -> .T. / .F. - Posição 2 -> Se .F., Grava o erro retornado
    Local aHeader    := {}
    Local cError     := ""
    Local nStatus    := 0
    Local oRest 

    default cBody   := ''      

 	Aadd( aHeader, "Authorization: Basic " + cToken )
    Aadd( aHeader, "Accept: application/json"       )
    Aadd( aHeader, "Content-Type: application/json" )

    oRest := FWRest():New(cURL)

    oRest:setPath(cParam)
    oRest:SetPostParams(cBody)
	
    if oRest:Post(aHeader)
        nStatus := HTTPGetStatus(@cError)
        if nStatus >= 200 .And. nStatus <= 299
            aRet[1] := .T. 
            aRet[2] := ""
        Else 
            aRet[1] := .F. 
            aRet[2] := cError
        endif
    else
        aRet[1] := .F. 
        aRet[2] := Alltrim(oRest:getLastError())+" - "+Alltrim(oRest:getResult())
    endif

Return aRet


/*
===============================================================================================
===============================================================================================
||   Arquivo:	    GTPLAN.prw
===============================================================================================
||   Funcao: 	    Z12GrLog
===============================================================================================
||   Descrição: 	Gravação de Log na tabela Z12.
===============================================================================================
||   Autor:	Guelder A. Santos
||   Data:	24/05/2021
===============================================================================================
===============================================================================================
*/
User Function Z12GrLog(lProcess,lErro,cLog,cJson,cQuery,cJob,cProgram,nRegTot,aChvZ12,nRecno)
    
      

    If lProcess

        cProgram := strtran(cProgram,"U_","")  

        dbSelectArea("Z10")
        Z10->(DbSetOrder(2))
        Z10->(DbSeek("00"+cJob))

        dbSelectArea("Z12")

        RecLock("Z12",.T.)
            Z12->Z12_FILIAL  :=  "00" 
            Z12->Z12_ID      := Z10->Z10_ID
            
            IF Alltrim(UPPER(cProgram)) == "PROMA702"
                Z12->Z12_CHAVE   := "SA2"+'-'+DtoS(dDatabase)+SubStr(strtran(time(),":",""),1,4)
            ElseIf Alltrim(UPPER(cProgram)) == "PROMA707"
                Z12->Z12_CHAVE   := "SA1"+'-'+DtoS(dDatabase)+SubStr(strtran(time(),":",""),1,4)    
            Else
                Z12->Z12_CHAVE   := cFilAnt+'-'+DtoS(dDatabase)+SubStr(strtran(time(),":",""),1,4)                                  
            EndIf

            Z12->Z12_SEQ     := "0001"
            Z12->Z12_DTINC   := dDatabase
            Z12->Z12_HRINC   := Time()
            Z12->Z12_DTPINI  := dDatabase
            Z12->Z12_HRPINI  := Time()
            Z12->Z12_JSON    := cJson
            Z12->Z12_STPROC  := "1"
            Z12->Z12_TENTAT  := 0
            Z12->Z12_QTDREG  := nRegTot
            Z12->Z12_ERPFUN  := cProgram
        Z12->(MsUnlock())
    Else
        Z12->(DbSetOrder(1))
        iF Z12->(DbSeek("00"+aChvZ12[1]+aChvZ12[2]))
        
            If lErro

                RecLock("Z12",.F.)
                            
                Z12->Z12_DTPFIM  := dDatabase    
                Z12->Z12_HRPFIM  := Time()
                Z12->Z12_STPROC  := "2"

                Z12->(MsUnlock())
            Else
                RecLock("Z12",.F.)
                            
                Z12->Z12_DTPFIM  := dDatabase    
                Z12->Z12_HRPFIM  := Time()
                Z12->Z12_STPROC  := "3"

                Z12->(MsUnlock())
            EndIf
        EndIf
    EndIf
Return {Z12->Z12_ID,Z12->Z12_CHAVE}


/*
===============================================================================================
===============================================================================================
||   Arquivo:	    GTPLAN.prw
===============================================================================================
||   Funcao: 	    Z13GrLog
===============================================================================================
||   Descrição: 	Gravação de Log na tabela Z13.
===============================================================================================
||   Autor:	Guelder A. Santos
||   Data:	24/05/2021
===============================================================================================
||   Hist 14/07/2021 - Wanderley M Silva - Correção erro no DBSetOrder da tabela Z13
===============================================================================================
===============================================================================================
*/
User Function Z13GrLog(lErro,cLog,cJson,aChvZ12)
    Local nSeq  := 0

    dbSelectArea("Z13")
    Z13->(DbSetOrder(1))
    


    If !Z13->(DbSeek("00"+aChvZ12[1]+aChvZ12[2]+"0001"))
        
        RecLock("Z13",.T.)
            Z13_FILIAL  :=  "00" 
            Z13_ID      :=  aChvZ12[1]
            Z13_CHAVE   :=  aChvZ12[2]
            Z13_Z12SEQ  := "0001"
            Z13_SEQ     := "0001"
            Z13_DTHIST  := dDatabase
            Z13_HRHIST  := Time()
            Z13_JSON    := cJson
            Z13_ERRO    := cLog
            
        Z13->(MsUnlock())
    Else
        nSeq := Val(Z13->Z13_SEQ)
        While Z13->(DbSeek("00"+aChvZ12[1]+aChvZ12[2]+"0001"+Strzero(nSeq,4)))
            nSeq :=  nSeq + 1  
        EndDO

         RecLock("Z13",.T.)
            Z13_FILIAL  :=  "00" 
            Z13_ID      :=  aChvZ12[1]
            Z13_CHAVE   :=  aChvZ12[2]
            Z13_Z12SEQ  := "0001"
            Z13_SEQ     := Strzero(nSeq,4)
            Z13_DTHIST  := dDatabase
            Z13_HRHIST  := Time()
            Z13_JSON    := cJson
            Z13_ERRO    := cLog
            
        Z13->(MsUnlock())

    EndIf
Return

/*
===============================================================================================
===============================================================================================
||   Arquivo:	    GTPLAN.prw
===============================================================================================
||   Funcao: 	    NxtNumGT
===============================================================================================
||   Descrição: 	Retorna o Proximo Numero de Pedido Numérico a ser gerado para GTPLAN
===============================================================================================
||   Autor:	Wanderley M Silva
||   Data:	14/07/2021
===============================================================================================
===============================================================================================
*/
User Function NxtNumGT(_cAlias)
    
Local nRet       := ''
Local aArea      := {}
Local cQuery     := ""
Local cAliasTop  := ""

aArea := GetArea()

If _cAlias == 'SC5'
    nRet := GETMV("BZ_GTNUMC5",.F.,0)
    
    nRet++ 
    
    PUTMV("BZ_GTNUMC5",nRet)

ElseIf _cAlias == 'SC7'
    nRet := GETMV("BZ_GTNUMC7",.F.,0)
    
    nRet++ 
    
    PUTMV("BZ_GTNUMC7",nRet)
EndIf
// Restaura areas de trabalho.
RestArea(aArea)

Return(nRet)


/*
===============================================================================================
===============================================================================================
||   Arquivo:	    GTPLAN.prw
===============================================================================================
||   Funcao: 	    GrvNumGT
===============================================================================================
||   Descrição: 	Grava o Numero de Pedido Numérico gerado para GTPLAN
===============================================================================================
||   Autor:	Wanderley M Silva
||   Data:	14/07/2021
===============================================================================================
===============================================================================================
*/
User Function GrvNumGT(_cAlias,_cFilPed,_cGTNum)
    
Local cRet       := ''
Local aArea      := {}
Local cQuery     := ""
Local cAliasTop  := ""

aArea := GetArea()



If _cAlias == 'SC5'
    SC5->(DbSetOrder(1))	//_FILIAL, _NUM

    If SC5->(DbSeek(_cFilPed))

        While SC5->C5_FILIAL+SC5->C5_NUM == _cFilPed

            RecLock("SC5",.F.)
            SC5->C5_XGTNUM := STR(_cGTNum,6)

            SC5->(MsUnlock())

            SC5->(DbSkip())
        EndDO

    EndIf
ElseIf _cAlias == 'SC7'
    SC7->(DbSetOrder(1))	//_FILIAL, _NUM

    If SC7->(DbSeek( _cFilPed))

        While SC7->C7_FILIAL+SC7->C7_NUM == _cFilPed

            RecLock("SC7",.F.)
            SC7->C7_XGTNUM := STR(_cGTNum,6)

            SC7->(MsUnlock())

            SC7->(DbSkip())
        EndDO

    EndIf
EndIf


// Restaura areas de trabalho.
RestArea(aArea)

Return(NIL)


//-------------------------------------------------------------------
/*/{Protheus.doc} lChkZ12
Verifica se o Job já foi executado no dia
@author  Victor Dessunte
@since   19/10/2021
@version 12.1.25
/*/
//-------------------------------------------------------------------
User Function lChkZ12(cJob,cData)

Local lRet 		:= .F.
Local cQuery 	:= ''
Local cAliZ12	:= ''

Default cJob  := ""
Default cData := DTOS(dDataBase)

cQuery := " SELECT " + CRLF
cQuery += "		ISNULL(COUNT(*),0) TOTAL " + CRLF
cQuery += "	FROM " + CRLF
cQuery += 		RetSqlName("Z12") + " Z12 INNER JOIN " + RetSqlName("Z10") + " Z10 " + CRLF
cQuery += "		ON	Z10.D_E_L_E_T_	= ''
cQuery += " 	AND Z10_FILIAL		= '" + xFilial("Z10") + "' " + CRLF
cQuery += " 	AND Z10_DESCR		= '" + cJob + "' " + CRLF	
cQuery += " WHERE " + CRLF	
cQuery += " 	Z12.D_E_L_E_T_	= '' " + CRLF	
cQuery += " AND Z12_FILIAL		= Z10_FILIAL " + CRLF	
cQuery += " AND Z12_ID			= Z10_ID " + CRLF	
cQuery += " AND Z12_DTINC		= '" + cData + "' "

cAliZ12 := MPSysOpenQuery(cQuery)

(cAliZ12)->(dbGoTop())

If (cAliZ12)->(!EOF())
	If (cAliZ12)->TOTAL > 0
		lRet := .T.
	EndIf
EndIf

Return lRet
