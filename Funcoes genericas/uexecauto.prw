#INCLUDE "PROTHEUS.CH"
#INCLUDE "MSOBJECT.CH"

User Function UEXECAUTO()
Return Nil
//-------------------------------------------------------------------
/*/{Protheus.doc} 	uExecAuto
Classe para gravacao via Rotina Automatica: deve ser utilizada por Heranca.

@author  Guilherme Santos
@since   17/04/09
@version 12.1.25
/*/
//-------------------------------------------------------------------
Class uExecAuto
	Data aCabec										//Dados do Cabecalho
	Data aItens										//Dados dos Itens
	Data aItemTemp									//Array temporario para o Item
	Data aTabelas									//Array com as Tabelas que devem ser abertas na Preparacao do Ambiente
	Data aValues									//Dados para Gravacao
	
	Data cEmpBkp									//Backup da Empresa Original
	Data cFilBkp									//Backup da Filial Original
	Data cEmpGrv									//Empresa para Gravacao
	Data cFileLog									//Nome do Arquivo para Gravacao de Log de Erro da Rotina Automatica
	Data cFilGrv									//Filial para Gravacao
	Data cMensagem									//Mensagem de Erro
	Data cPathLog									//Caminho para Gravacao do Arquivo de Log

	Data dEmissao									//Data da Inclusao ou Alteracao do Registro

	Data lExibeTela									//Define se deve exibir Tela com a Mensagem de Erro
	Data lGravaLog									//Define se deve gravar arquivo de log com a Mensagem de Erro

	Method New()									//Inicializacao do Objeto
	Method AddValues(cCampo, xValor, xValid)		//Adiciona dados para Gravacao
	Method AddCabec(cCampo, xValor, xValid)			//Adicona dados ao Cabecalho
	Method AddItem(cCampo, xValor, xValid)			//Adiciona dados ao Item
	Method SetItem()								//Insere os dados do Item no Array dos Itens
	Method Gravacao(nOpcao)							//Gravacao via Rotina Automatica
	Method GetMensagem()							//Retorno das Mensagens de Erro
	Method SetEnv(nOpcao, cModulo)					//Prepara o Ambiente para Execucao da Rotina Automatica
EndClass
//-------------------------------------------------------------------
/*/{Protheus.doc} New
Inicializa o Objeto

@author  Guilherme Santos
@since   17/04/09
@version 12.1.25
/*/
//-------------------------------------------------------------------
Method New() Class uExecAuto
	::aCabec		:= {}
	::aItens		:= {}
	::aItemTemp		:= {}
	::aTabelas		:= {}
	::aValues		:= {}
	
	::cEmpBkp		:= ""
	::cFilBkp		:= ""
	::cEmpGrv		:= ""
	::cFilGrv		:= ""
	::cMensagem		:= ""

	::cFileLog		:= "MATAXXX.LOG"
	::cPathLog		:= "\LOGS\"

	::dEmissao		:= CtoD("")

	::lExibeTela	:= .F.
	::lGravaLog	:= .T.
Return Self
//-------------------------------------------------------------------
/*/{Protheus.doc} AddValues
Armazena os valores para gravacao

@author  Guilherme Santos
@since   17/04/09
@version 12.1.25
/*/
//-------------------------------------------------------------------
Method AddValues(cCampo, xValor, xValid) Class uExecAuto
	Local nPosCpo		:= Ascan(::aValues, {|x| AllTrim(x[01]) == AllTrim(cCampo)})
	Default xValid	:= NIL

	If AllTrim(cCampo) == "EMPRESA"
		::cEmpGrv := xValor
	Else
		If "_FILIAL" $ AllTrim(cCampo)
			::cFilGrv := xValor
		EndIf

		If nPosCpo == 0
			Aadd(::aValues, {cCampo		,xValor		,xValid})
		Else
			::aValues[nPosCpo][02] := xValor
		EndIf
	EndIf

Return Nil
//-------------------------------------------------------------------
/*/{Protheus.doc} AddCabec
Armazena os Valores do Cabecalho do para gravacao.

@author  Guilherme Santos
@since   17/04/09
@version 12.1.25
/*/
//-------------------------------------------------------------------
Method AddCabec(cCampo, xValor, xValid) Class uExecAuto
	Local nPosCpo		:= Ascan(::aCabec, {|x| x[01] == cCampo})
	Default xValid	:= NIL

	If AllTrim(cCampo) == "EMPRESA"
		::cEmpGrv := xValor
	Else
		If "_FILIAL" $ AllTrim(cCampo)
			::cFilGrv	:= xValor
		EndIf

		If nPosCpo == 0
			Aadd(::aCabec, {cCampo, xValor, xValid})
		Else
			::aCabec[nPosCpo][02] := xValor
		EndIf
	EndIf

Return Nil
//-------------------------------------------------------------------
/*/{Protheus.doc} AddItem
Armazena os Valores do Item para gravacao.

@author  Guilherme Santos
@since   17/04/09
@version 12.1.25
/*/
//-------------------------------------------------------------------
Method AddItem(cCampo, xValor, xValid) Class uExecAuto
	Local nPosCpo		:= Ascan(::aItemTemp, {|x| x[01] == cCampo})
	Default xValid	:= NIL

	If !AllTrim(cCampo) == "EMPRESA"
		If nPosCpo == 0
			Aadd(::aItemTemp, {cCampo, xValor, xValid})
		Else
			::aItemTemp[nPosCpo][02] := xValor
		EndIf
	EndIf

Return Nil
//-------------------------------------------------------------------
/*/{Protheus.doc} SetItem
Armazena os Valores do Item e Reinicializa o Array Temporario.

@author  Guilherme Santos
@since   17/04/09
@version 12.1.25
/*/
//-------------------------------------------------------------------
Method SetItem() Class uExecAuto
	Aadd(::aItens, ::aItemTemp)
	::aItemTemp := {}
Return Nil
//-------------------------------------------------------------------
/*/{Protheus.doc} Gravacao
Gravacao via MsExecAuto.

@author  Guilherme Santos
@since   17/04/09
@version 12.1.25
/*/
//-------------------------------------------------------------------
Method Gravacao(nOpcao) Class uExecAuto
	Local dDataBackup		:= dDataBase		//Backup da Data Base do Sistema
	Local lRetorno		:= .T.				//Retorno da Rotina de Gravacao

	Private lMsErroAuto	:= .F.				//Determina se houve algum erro durante a Execucao da Rotina Automatica
	
	//Prepara o Ambiente para Execucao na Empresa e na Filial Informada
	::SetEnv(1, "EST")

	//Altera a Data da Gravacao
	If !Empty(::dEmissao)
		dDataBase := ::dEmissao
	EndIf

	//Exemplos de Execucao via Rotina Automatica

	//Gravacao via Rotina Automatica com Apenas uma Tabela
	//MSExecAuto({|a, b| MATAXXX(a, b)},	::aValues, nOpcao)

	//Gravacao via Rotina Automatica com Cabecalho e Itens
	//MSExecAuto({|a, b, c| MATAXXX(a, b, c)}, ::aCabec, ::aItens, nOpcao)

	If lMsErroAuto
		lRetorno := .F.

		If ::lExibeTela
			MostraErro()
		EndIf
		
		If ::lGravaLog
			::cMensagem := MostraErro(::cPathLog, ::cFileLog)
		EndIf
	EndIf

	//Restaura a Data Base Original
	dDataBase := dDataBackup

	//Restaura o Ambiente Original
	::SetEnv(2, "EST")

Return lRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} GetMensagem
Retorna a Mensagem de Erro do ExecAuto

@author  Guilherme Santos
@since   17/04/09
@version 12.1.25
/*/
//-------------------------------------------------------------------
Method GetMensagem() Class uExecAuto
Return ::cMensagem
//-------------------------------------------------------------------
/*/{Protheus.doc} SetEnv
Prepara o Ambiente para Gravacao na Empresa correta

@author  Guilherme Santos
@since   17/04/09
@version 12.1.25
@param	nOpcao  => 1 = Prepara / 2 = Restaura
@param	cModulo => "MOD" = Modulo com 3 Letras. Ex. "FAT" - Faturamento / "EST" - Estoque 
/*/
//-------------------------------------------------------------------
Method SetEnv(nOpcao, cModulo) Class uExecAuto
	Local	nTamEmp	:= Len(::cEmpGrv)
	Default cModulo	:= "FAT"

	If nTamEmp > 2
		::cEmpGrv := Substr(::cEmpGrv, 1, 2)
	EndIf

	If nOpcao == 1
		If !Empty(::cEmpGrv) .AND. !Empty(::cFilGrv)
			::cEmpBkp := cEmpAnt
			::cFilBkp := cFilAnt
			
			If ::cEmpGrv <> ::cEmpBkp .OR. ::cFilGrv <> ::cFilBkp
				RPCSetType(3)
				RpcSetEnv(::cEmpGrv, ::cFilGrv, NIL, NIL, cModulo, NIL, ::aTabelas)
			EndIf
		EndIf
	Else
		If !Empty(::cEmpBkp) .AND. !Empty(::cFilBkp)
			If ::cEmpBkp <> cEmpAnt .OR. ::cFilBkp <> cFilAnt
				RPCSetType(3)
				RpcSetEnv(::cEmpBkp, ::cFilBkp, NIL, NIL, cModulo, NIL, ::aTabelas)
			EndIf
		EndIf
	EndIf

	::lExibeTela	:= SuperGetMV("CL_SHOWERR", NIL, .F.)
	::lGravaLog		:= SuperGetMV("CL_GRVLOG", NIL, .T.)

Return Nil
