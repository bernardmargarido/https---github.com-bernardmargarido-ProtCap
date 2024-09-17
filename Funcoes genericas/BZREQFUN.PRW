#INCLUDE "PROTHEUS.CH"

//-------------------------------------------------------------------
/*/{Protheus.doc} BzIsNull
Caso variaveis nulas, retorno conteudo default.

@author  Wilson A. Silva Jr
@since   18/10/2021
@version 1.0
/*/
//-------------------------------------------------------------------
User Function BzIsNull(xVariavel, xConteudo)

Local xRetorno

DEFAULT xConteudo := ""

xRetorno := IIF(ValType(xVariavel)<>"U", xVariavel, xConteudo)

Return xRetorno

//-------------------------------------------------------------------
/*/{Protheus.doc} BzConvDt
Converte datetime em array de duas posições, data e hora.

@author  Wilson A. Silva Jr
@since   20/09/2021
@version 1.0
/*/
//-------------------------------------------------------------------
User Function BzConvDt(cDateTime)

Local aDate := Array(2)

aDate[1] := SToD(StrTran(StrTran(SubStr(cDateTime,1,10),"-",""),"/","")) // Date
aDate[2] := SubStr(cDateTime,12,8) // Time

Return aDate

//-------------------------------------------------------------------
/*/{Protheus.doc} BzConvId
Converte ID em numerico.

@author  Wilson A. Silva Jr
@since   18/10/2021
@version 1.0
/*/
//-------------------------------------------------------------------
User Function BzConvId(xID)

Local nRetID := U_BzIsNull(xID, 0)

nRetID := IIF(ValType(nRetID)=="C", Val(nRetID), nRetID)

Return nRetID

//-------------------------------------------------------------------
/*/{Protheus.doc} BzConvId
Converte ID em numerico.

@author  Wilson A. Silva Jr
@since   18/10/2021
@version 1.0
/*/
//-------------------------------------------------------------------
User Function BzRetParc(xParcela)

Local cRetParc := GetNewPar("MV_1DUP")
Local nVezes   := U_BzIsNull(IIF(ValType(xParcela)=="C", Val(xParcela), xParcela), 1)
Local nX

If nVezes == 0
    cRetParc := ''
Endif

For nX := 2 To nVezes
    cRetParc := SOMA1(cRetParc)
Next nX

Return cRetParc

//-------------------------------------------------------------------
/*/{Protheus.doc} BzRetNat
Retorna a natureza de acordo com o tipo de título.

@author  Wilson A. Silva Jr
@since   28/04/2022
@version 1.0
/*/
//-------------------------------------------------------------------
User Function BzRetNat(cTipo)

Local aNaturezas := Separa(GetNewPar("MV_XREQNAT",""),",")
Local cNatRet    := ""
Local cTipTit    := ""
Local cCodNat    := ""
Local nX

If !Empty(cTipo) .And. Len(aNaturezas) > 0

    cTipo := AllTrim(cTipo)

    For nX := 1 To Len(aNaturezas)

        aAux := Separa(aNaturezas[nX],"=")

        If Len(aAux) == 2
            cTipTit := AllTrim(aAux[1])
            cCodNat := AllTrim(aAux[2])

            If cTipTit == cTipo
                cNatRet := cCodNat
                EXIT
            EndIf        
        EndIf

    Next nX
EndIf

Return cNatRet

//-------------------------------------------------------------------
/*/{Protheus.doc} U_BzLogJob
Grava log de execução de JOB.

@author  Wilson A. Silva Jr
@since   29/03/2022
@version 1.0
/*/
//-------------------------------------------------------------------
User Function BzLogJob(cStatus, cRotina, nRecLog, cNome, cMsgJob, dData, cHora)

DEFAULT dData   := Date()
DEFAULT cHora   := Time()
DEFAULT cMsgJob := ""
DEFAULT nRecLog := 0

If cStatus == "1" // 1=Iniciado

    RecLock("XTL",.T.)
        REPLACE XTL_FILIAL  WITH xFilial("XTL")
        REPLACE XTL_ROTINA  WITH cRotina
        REPLACE XTL_NOME    WITH cNome
        REPLACE XTL_DTINI   WITH dData
        REPLACE XTL_HRINI   WITH cHora
        REPLACE XTL_STATUS  WITH "1" // 1=Iniciado
        REPLACE XTL_MSGJOB  WITH cMsgJob
    MsUnlock()

    nRecLog := XTL->(Recno())

ElseIf cStatus == "2" // 2=Encerrado

    DbSelectArea("XTL")
    DbGoTo(nRecLog)
    If XTL->(Recno()) == nRecLog
        RecLock("XTL",.F.)
            REPLACE XTL_DTFIM   WITH dData
            REPLACE XTL_HRFIM   WITH cHora
            REPLACE XTL_STATUS  WITH "2" // 2=Encerrado
            REPLACE XTL_MSGJOB  WITH cMsgJob
        MsUnlock()
    EndIf
EndIf

Return nRecLog

//-------------------------------------------------------------------
/*/{Protheus.doc} EnvFTP
Rotina para envio de arquivos ao FTP do FINE.

@author  Wilson A. Silva Jr
@since   06/04/2022
@version 12.1.25
/*/
//-------------------------------------------------------------------
User Function BZEnvFTP(cDirFTP, cDirArq, cNomArq, cMsgErro)

Local cFTPServ  := GetNewPar("MV_XFINSRV","192.168.50.246") // FTP do FINE
Local nFTPPort  := GetNewPar("MV_XFINPOR",21035)
Local cFTPUser  := GetNewPar("MV_XFINUSR","Fine")
Local cFTPPass  := GetNewPar("MV_XFINPAS","F1n&@2o22")
Local lRetorno  := .T.
Local nFTPRet	:= 0
Local oFTP		:= Nil

cFTPServ := StrTran(cFTPServ,"ftp://","")

// Se conecta ao servidor FTP.
oFTP := TFTPClient():New()
oFTP:bFireWallMode     := .F. // Configurado para Modo FTP Ativo devido alteração na plataforma de hospedagem do Site.
oFTP:bUsesIPConnection := .T.

nFTPRet := oFTP:FTPConnect(cFTPServ, nFTPPort, cFTPUser, cFTPPass)

If nFTPRet <> 0
	cMsgErro := oFTP:GetLastResponse()
	lRetorno := .F.
EndIf

If lRetorno .And. !Empty(cDirFTP)
	nFTPRet := oFTP:ChDir(cDirFTP)

	If nFTPRet <> 0
		cMsgErro := oFTP:GetLastResponse()
		lRetorno := .F.
	EndIf
EndIf

If lRetorno
	nFTPRet := oFTP:SendFile(cDirArq + cNomArq, cNomArq)

	If nFTPRet <> 0
		cMsgErro := oFTP:GetLastResponse()
		lRetorno := .F.
	EndIf
EndIf

oFTP:Close()

Return lRetorno

//-------------------------------------------------------------------
/*/{Protheus.doc} BZDownFTP
Rotina para download de arquivos do FTP do FINE.

@author  Wilson A. Silva Jr
@since   06/04/2022
@version 12.1.25
/*/
//-------------------------------------------------------------------
User Function BZDownFTP(cDirFTP, cDirDes, cArqExt, lDelArq, cMsgErro)

Local cFTPServ  := GetNewPar("MV_XFINSRV","192.168.50.246") // FTP do FINE
Local nFTPPort  := GetNewPar("MV_XFINPOR",21035)
Local cFTPUser  := GetNewPar("MV_XFINUSR","Fine")
Local cFTPPass  := GetNewPar("MV_XFINPAS","F1n&@2o22")
Local lRetorno  := .T.
Local nFTPRet	:= 0
Local oFTP		:= Nil
Local nX

Default cArqExt := "RET"

cFTPServ := StrTran(cFTPServ,"ftp://","")

// Se conecta ao servidor FTP.
oFTP := TFTPClient():New()
oFTP:bFireWallMode     := .F. // Configurado para Modo FTP Ativo devido alteração na plataforma de hospedagem do Site.
oFTP:bUsesIPConnection := .T.

nFTPRet := oFTP:FTPConnect(cFTPServ, nFTPPort, cFTPUser, cFTPPass)

If nFTPRet <> 0
	cMsgErro := oFTP:GetLastResponse()
	lRetorno := .F.
EndIf

If lRetorno .And. !Empty(cDirFTP)
	nFTPRet := oFTP:ChDir(cDirFTP)

	If nFTPRet <> 0
		cMsgErro := oFTP:GetLastResponse()
		lRetorno := .F.
	EndIf
EndIf

If lRetorno
    aRetDir := oFTP:Directory("*."+cArqExt)
	
    For nX := 1 To Len(aRetDir)

        cNomArq := aRetDir[nX][1]
        dUltAlt := aRetDir[nX][3]
    
        nFTPRet := oFTP:ReceiveFile(cNomArq, cDirDes + cNomArq)

        If nFTPRet == 0 .And. File(cDirDes + cNomArq)
            If lDelArq
                oFTP:DeleteFile(cNomArq)
            Else
                oFTP:RenameFile(cNomArq, SubStr(cNomArq,-3) + "LID")
            EndIf
        EndIf
    Next nX
EndIf

oFTP:Close()

Return lRetorno

//-------------------------------------------------------------------
/*/{Protheus.doc} BZVldProc
Rotina para validar execução dos Jobs [Substitui LockByName]

@author  Daniel Salese
@since   30/03/2023
@version 1.0
/*/
//-------------------------------------------------------------------

User Function BZVldProc(cJob)

Local lRet      := .T.
Local cTMP1     := GetNextAlias()
Local cQuery    := ""
Local nTimeExp  := SuperGetMV("BZ_MINUTOS",.F.,10)

cQuery := " SELECT TOP 1 "+ CRLF
cQuery += " XTL.R_E_C_N_O_ AS RECXTL, "+ CRLF
cQuery += " XTL_HRINI "+ CRLF
cQuery += " FROM "+RetSqlName("XTL")+" XTL (NOLOCK) "+ CRLF
cQuery += " WHERE "+ CRLF
cQuery += " 	XTL.D_E_L_E_T_ = ' ' "+ CRLF
cQuery += " 	AND XTL.XTL_ROTINA IN ('"+cJob+"') "+ CRLF
cQuery += " 	AND XTL.XTL_STATUS IN ('1') "+ CRLF // 1=Pendente / 2=Processado / 3=Erro
cQuery += " 	AND XTL.XTL_DTINI = '"+DTOS(DATE())+"' "+ CRLF
cQuery += " ORDER BY "+ CRLF
cQuery += " 	XTL_DTINI DESC "+ CRLF

cTMP1 := MPSysOpenQuery(cQuery)

While (cTMP1)->(!EOF())
    
    If VAL(SUBSTR(ELAPTIME((cTMP1)->XTL_HRINI,TIME()),4,2)) > nTimeExp

        DbSelectArea("XTL")
        DbSetOrder(1) // XTD_FILIAL+XTD_FILCOR+XTD_CHVINT
        DbGoTo((cTMP1)->RECXTL)
        If XTL->(Recno()) == (cTMP1)->RECXTL
            RecLock("XTL",.F.)
            REPLACE XTL_DTFIM   WITH DATE()
            REPLACE XTL_HRFIM   WITH TIME()
            REPLACE XTL_STATUS  WITH "3" // 3=Erro
            MsUnlock()
        EndIf    

        lRet := .T.
    Else
        lRet := .F.
    EndIf    

    (cTMP1)->(DbSkip())
EndDo

(cTMP1)->(DbCloseArea())

Return(lRet)
