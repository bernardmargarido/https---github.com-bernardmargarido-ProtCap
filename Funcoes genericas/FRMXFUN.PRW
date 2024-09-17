#INCLUDE "PROTHEUS.CH"

#DEFINE F_NAME	1
#DEFINE F_ATT	5
//-------------------------------------------------------------------
/*/{Protheus.doc} FRMXFUN
Funcoes Genericas para Gravacao de Log de Utilizacao dos Fontes do Projeto

@author  Guilherme Santos
@since   07/10/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
User Function FRMXFUN()
Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} FRMXINC
Inclusao da chamada da Rotina de Log nos Fontes da Pasta Selecionada

@author  Guilherme Santos
@since   07/10/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
User Function FRMXINC()
	Local nAviso	:= Aviso("FRMXINC", "Esta rotina incluirá a chamada para a rotina de gravacao de log ('XGRVLOG') em todos os fontes da pasta selecionada. Confirma a execução?", {"Confirmar", "Fechar"})
	Local cPath		:= ""

	If nAviso == 1
		cPath := Upper(cGetFile("" , "Selecione o Diretorio do Projeto", 0, "D:\_TEMP\", .F., GETF_LOCALHARD + GETF_RETDIRECTORY, .F.))

		If !Empty(cPath)
			Processa({|| ProcDir(cPath)}, "Aguarde, processando diretorio " + cPath)
		EndIf
	EndIf

Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} FRMXEXC
Exclusao da chamada de gravacao do Log dos fontes do Projeto
@author  Guilherme Santos
@since   14/02/2020
@version 12.1.25
/*/
//-------------------------------------------------------------------
User Function FRMXEXC()
	Local nAviso	:= Aviso("FRMXEXC", "Esta rotina excluirá a chamada para a rotina de gravacao de log ('XGRVLOG') em todos os fontes da pasta selecionada. Confirma a execução?", {"Confirmar", "Fechar"})
	Local cPath		:= ""

	If nAviso == 1
		cPath := Upper(cGetFile("" , "Selecione o Diretorio do Projeto", 0, "D:\_TEMP\", .F., GETF_LOCALHARD + GETF_RETDIRECTORY, .F.))

		If !Empty(cPath)
			Processa({|| ProcDir(cPath)}, "Aguarde, processando diretorio " + cPath)
		EndIf
	EndIf

Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} ProcDir
Processamento do Diretorio

@author  Guilherme Santos
@since   07/10/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
Static Function ProcDir(cPath)
	Local aListDir	:= Directory(cPath + "*.*", "D")
	Local nLin		:= 0
	Local cTipo		:= ""
	Local cTexto	:= ""
	Local lRetorno	:= .T.

	For nLin := 1 to Len(aListDir)
		cTipo 	:= aListDir[nLin][F_ATT]
		cTexto	:= Upper(AllTrim(aListDir[nLin][F_NAME]))

		If !Ignore(cTipo, cTexto)
			Do Case
			Case cTipo == "D"
				Processa({|| ProcDir(cPath + cTexto + "\")}, "Aguarde, processando diretorio " + cPath + cTexto + "\")
			Case cTipo == "A"
				Processa({|| ProcArq(cPath, cTexto)}, "Aguarde, processando arquivo " + cPath + cTexto)
			EndCase
		EndIf
	Next nLin

Return lRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} ProcArq
Processamento do Arquivo

@author  Guilherme Santos
@since   07/10/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
Static Function ProcArq(cPath, cFile)
	Local lRetorno 	:= .T.
	Local oArqOld	:= uArqTxt():New(cPath, cFile)
	Local oArqNew	:= uArqTxt():New(cPath, cFile + ".new")
	Local cLinha	:= ""
	Local cPrograma	:= ""
	Local cFuncao	:= ""
	Local nIniStr	:= 0
	Local nTamStr	:= 0
	Local lInclusao	:= IsInCallStack("U_FRMXINC")
	Local lExclusao	:= IsInCallStack("U_FRMXEXC")

	IncProc("Processando arquivo " + cPath + "\" + cFile)

	If oArqOld:Exists()
		If oArqNew:Create()
			If oArqOld:Open() .AND. oArqOld:Use()
				oArqOld:GoTop()

				Do Case
				Case At(".PRW", Upper(cFile)) > 0
					cPrograma := StrTran(Upper(cFile), ".PRW", "")
				Case At(".PRX", Upper(cFile)) > 0
					cPrograma := StrTran(Upper(cFile), ".PRX", "")
				Otherwise
					cPrograma := Upper(cFile)
				EndCase

				While !oArqOld:Eof()
					cLinha := oArqOld:ReadLn()

					If lInclusao
						If "USER FUNCTION" $ Upper(cLinha) .OR. "STATIC FUNCTION" $ Upper(cLinha)
							Do Case
							Case At("USER FUNCTION", Upper(cLinha)) > 0
								If At("(", cLinha) > 0
									nIniStr := Len("USER FUNCTION") + 2
									nTamStr := At("(", cLinha) - nIniStr
								Else
									nIniStr := Len("USER FUNCTION") + 2
									nTamStr := Len(cLinha)
								EndIf

								cFuncao := "U_" + Upper(Substr(cLinha, nIniStr, nTamStr))

							Case At("STATIC FUNCTION", Upper(cLinha)) > 0

								If At("(", cLinha) > 0
									nIniStr := Len("STATIC FUNCTION") + 2
									nTamStr := At("(", cLinha) - nIniStr
								Else
									nIniStr := Len("STATIC FUNCTION") + 2
									nTamStr := Len(cLinha)
								EndIf

								cFuncao := Upper(Substr(cLinha, nIniStr, nTamStr))
							EndCase

							U_XGRVPRG(cPrograma, cFuncao)

							cLinha += CRLF + 'Local lXGrvLog := U_XGRVLOG("' + cPrograma + '", "' + cFuncao  + '")'
						EndIf

						oArqNew:Write(cLinha)
					EndIf

					If lExclusao
						If !("U_XGRVLOG(" $ Upper(cLinha))
							oArqNew:Write(cLinha)
						EndIf
					EndIf

					oArqOld:Skip()
				End

				oArqOld:Close()
				oArqOld:Free()
				oArqNew:Close()
				oArqNew:Free()

				If oArqOld:Erase()
					oArqNew:Rename(cFile)
				EndIf

				FreeObj(oArqOld)
				FreeObj(oArqNew)
			Else
				lRetorno := .F.
			EndIf
		Else
			lRetorno := .F.
		EndIf
	Else
		lRetorno := .F.
	EndIf

Return lRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} Ignore
Retorna se deve ignorar o Arquivo ou Diretorio

@author  Guilherme Santos
@since   07/10/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
Static Function Ignore(cTipo, cTexto)
	Local lRetorno 	:= .F.
	Local aExclude	:= {}
	Local aInclude	:= {}

	Aadd(aExclude, {"D", "."})
	Aadd(aExclude, {"D", ".."})
	Aadd(aExclude, {"D", ".HG"})
	Aadd(aExclude, {"D", ".VSCODE"})
	Aadd(aExclude, {"D", "UPDATE"})
	Aadd(aExclude, {"D", "INCLUDE"})
	Aadd(aExclude, {"A", "NFESEFAZ.PRW"})
	Aadd(aExclude, {"A", "DANFEII.PRW"})
	Aadd(aExclude, {"A", "DANFEIII.PRW"})
	Aadd(aExclude, {"A", "FRMXFUN.PRW"})
	Aadd(aExclude, {"A", ".BAK"})
	Aadd(aExclude, {"A", ".PTM"})
	Aadd(aExclude, {"A", ".PNG"})
	Aadd(aExclude, {"A", ".CH"})
	Aadd(aExclude, {"A", ".HGIGNORE"})
	Aadd(aExclude, {"A", ".HGTAGS"})

	Aadd(aInclude, {"A", ".PRW"})
	Aadd(aInclude, {"A", ".PRX"})

	If AScan(aExclude, {|x| x[01] == cTipo .AND. x[02] $ cTexto}) > 0
		lRetorno := .T.
	Else
		If cTipo == "A" .AND. AScan(aInclude, {|x| x[01] == cTipo .AND. x[02] $ cTexto}) == 0
			lRetorno := .T.
		EndIf
	EndIf

Return lRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} XGRVLOG
Gravacao do Log de Execucao das Rotinas do Protheus

@author  Guilherme Santos
@since   08/10/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
User Function XGRVLOG(cFonte, cFuncao)
	Local lRetorno 	:= .T.
	Local lXGrvLog	:= If(IsBlind() .OR. Type("cFilAnt") == "U", .F., SuperGetMV("BZ_XGRVLOG", NIL, .F.))
	Local cExecSQL	:= ""

	If lXGrvLog
		cExecSQL	:= "exec bzl_grv_log '" + cFonte + "', '" + cFuncao + "', '" + DtoS(Date()) + "', '" + RetCodUsr() + "', '" + UsrRetName(RetCodUsr()) + "'"

		If TcSqlExec(cExecSQL) < 0
			lRetorno := .F.
		EndIf
	EndIf

Return lRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} XGRVPRG
Gravacao de log de Alteracao dos Fontes do Projeto

@author  Guilherme Santos
@since   09/10/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
User Function XGRVPRG(cFonte, cFuncao)
	Local lRetorno 	:= .T.
	Local cExecSQL	:= ""

	cExecSQL	:= "exec bzl_grv_prg '" + cFonte + "', '" + cFuncao + "'"

	If TcSqlExec(cExecSQL) < 0
		lRetorno := .F.
	EndIf

Return lRetorno
