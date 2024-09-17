#INCLUDE "PROTHEUS.CH"
//-------------------------------------------------------------------
/*/{Protheus.doc} FESTXFUN
Funcoes Genericas Liberacao de Estoque

@author  Guilherme Santos
@since   28/10/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
User Function FESTXFUN()
Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} FESTX001
Habilita o Controle de Semaforo de Liberacao de Estoque

@author  Guilherme Santos
@since   28/10/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
User Function FESTX001()
	Local nCount	:= 0
	Local nQtdTent	:= SuperGetMV("BZ_PVLIBQT", NIL, 20)		//Quantidade de Tentativas de Bloqueio
	Local lRetorno 	:= .T.
	Local lSemaforo	:= SuperGetMV("BZ_PVLIBLK", NIL, .T.)		//Habilita ou Desabilita o Controle de Semaforo na Liberacao de Estoque
	Local lExibeMsg := IsInCallStack("msAguarde")

	While !MayIUseCode(FWCodEmp() + FWCodFil() + "PVLIBEST") .AND. nCount < nQtdTent
		nCount++

		If lExibeMsg
			msProcTxt("FESTX001 - Tentativa: " + AllTrim(Str(nCount)) + "/" + AllTrim(Str(nQtdTent)))
			ProcessMessages()
		Endif

		If nCount == nQtdTent
			lRetorno := .F.
		Else
			Sleep(1000)			//Aguarda um segundo antes de tentar habilitar o Semaforo novamente
		EndIf
	End

Return lRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} FESTX002
Desabilita o Controle de Semaforo de Liberacao de Estoque

@author  Guilherme Santos
@since   28/10/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
User Function FESTX002()
	Leave1Code(FWCodEmp() + FWCodFil() + "PVLIBEST")
Return NIL
