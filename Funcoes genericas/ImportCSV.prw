#include 'protheus.ch'

#DEFINE CAMPO     1  // Nome do campo. Deve seguir o SX3.
#DEFINE TIPO      2  // Tipo do campo, usado para trasformar texto em conteúdo.
#DEFINE TAMANHO   3  // Tamanho do campo.
#DEFINE DECIMAIS  4  // Decimais do campo.
#DEFINE CONCATENA 5  // Identifica se concatena conteudo atual mais o novo. (valido apenas para campo tipo caractere)

//-------------------------------------------------------------------
/*/{Protheus.doc} ImportCSV
Classe para importação de arquivo CSV via MSExecAuto.

@author Juliane Venteu
@since 07/12/2016
@sample

Static Function Confirmar(cFile)
Local oImport := ImportCSV():New(cFile)
Local nOK

	oImport:SetBValid({|aFields, aDados| ValidDados(aFields, aDados)})
	oImport:SetOperation(4)
	oImport:SetBAuto({|x, y| MATA010(x, y)})

	If oImport:Read()
		_aDados := oImport:GetData()

		nOK := FWExecView("Conferencia de Dados","PROMA230",3,,{|| .T.})

		If nOK == 0
			oImport:Import()
		EndIf
	EndIf
Return

/*/
//-------------------------------------------------------------------
CLASS ImportCSV

	DATA cFile
	DATA aData
	DATA aFields
	DATA bValid
	DATA nOpc
	DATA bAuto
	DATA lValidData

	// @public
	METHOD New() CONSTRUCTOR
	METHOD SetFields()
	METHOD SetBValid()
	METHOD SetBAuto()
	METHOD SetOperation()
	METHOD Read()
	METHOD GetData()
	METHOD IsValid()
	METHOD Execute()
	METHOD Import()

	// @private
	METHOD ReadData()
	METHOD TreatHeader()
	METHOD TreatData()
	METHOD ValidData()

ENDCLASS

//-------------------------------------------------------------------
/*/{Protheus.doc} New
Metodo para criação do objeto.

@param cFile Arquivo CSV que será lido e importado

@author Juliane Venteu
@since 07/12/2016
/*/
//-------------------------------------------------------------------
METHOD New(cFile) CLASS ImportCSV
	::cFile := cFile
	::lValidData := .F.
	::bValid     := {|aFields, aData| .T.}
	::aData      := {}
	::aFields    := {}
	::nOpc       := 0
	::bAuto      := nil
Return

//-------------------------------------------------------------------
/*/{Protheus.doc} Read
Executa a abertura, leitura e validação do arquivo CSV.
Se a planilha não possui cabeçalho aborta a operação.

@return Retorna VERDADEIRO caso consiga efetuar a operação com sucesso

@author Juliane Venteu
@since 07/12/2016
/*/
//-------------------------------------------------------------------
METHOD Read() CLASS ImportCSV
Local lOk := .T.

	If ::nOpc == 0
		Aviso("ImportCSV", "Informe uma operação para a importação")
		lOk := .F.
	ElseIf ::bAuto == NIL
		Aviso("ImportCSV", "Informe o bloco de msExecAuto")
		lOk := .F.
	EndIf

	If lOk
		Processa( {|| ::ReadData(), lOk := (::TreatHeader() .and. ::TreatData() .and. ::ValidData())}, "Aguarde...", "Lendo dados...", .F.)
	EndIf

Return lOk

//-------------------------------------------------------------------
/*/{Protheus.doc} IsValid
Informa se a leitura do arquivo foi efetuada com sucesso.

@return Retorna VERDADEIRO caso consiga efetuar a operação com sucesso

@author Juliane Venteu
@since 07/12/2016
/*/
//-------------------------------------------------------------------
METHOD IsValid() CLASS ImportCSV
Return ::lValidData

//-------------------------------------------------------------------
/*/{Protheus.doc} ReadData
Efetua somente a abertura do arquivo e preenche o array de dados

@author Juliane Venteu
@since 07/12/2016
/*/
//-------------------------------------------------------------------
METHOD ReadData() CLASS ImportCSV
Local oFileIn := FWFileReader():New(::cFile)
Local aAux
Local nX

	If oFileIn:Open()
		aAux := oFileIn:getAllLines()

		For	nX:=1 to Len(aAux)
			aAdd(::aData, Separa(aAux[nX], ";",.T.))
		Next nX

		aSize(aAux, 0)

		oFileIn:Close()
	EndIf
Return

//-------------------------------------------------------------------
/*/{Protheus.doc} SetFields
Define quais campos farão parte do cabeçalho do arquivo.
Sem essa definição, não é possível efetuar a leitura.

@param aFields Array contendo os campos de header, com a estrutura:
	[n][1] ID do campo (X3_CAMPO)
	[n][2] Tipo (C-Caracter, D-Data e N-Numerico)
	[n][3] Verdadeiro para importar dado para o sistema

@author Juliane Venteu
@since 07/12/2016
/*/
//-------------------------------------------------------------------
METHOD SetFields(aFields) CLASS ImportCSV
	::aFields := aFields
Return

//-------------------------------------------------------------------
/*/{Protheus.doc} SetBValid
Define o bloco de codigo que será executado para validar os dados
lidos do arquivo CSV

@param bValid Bloco de codigo recebe como parametro o header
de campos e os dados lidos, o bloco de retornar VERDADEIRO ou FALSO
para indicar se os dados estão validos

@author Juliane Venteu
@since 07/12/2016
/*/
//-------------------------------------------------------------------
METHOD SetBValid(bValid) CLASS ImportCSV
	::bValid := bValid
Return

//-------------------------------------------------------------------
/*/{Protheus.doc} HaveHeader
Valida se o arquivo possui a primeira linha de cabeçalho com os nomes
dos campos a serem importados. Caso não encontre o cabeçalho gera um
erro na tela.

@author Juliane Venteu
@since 07/12/2016
/*/
//-------------------------------------------------------------------
METHOD TreatHeader() CLASS ImportCSV

Local lRet    := .T.
Local nX
Local cCampo
Local cType
Local nTamSX3
Local nDecSX3
Local lConcat

	If Len(::aData) > 0
		// Usa a primeira linha do CSV como cabeçalho.
		For nX:=1 to Len(::aData[1])
			cCampo  := upper(AllTrim(::aData[1][nX]))
			lConcat := Right(cCampo,1) == "+" // Identifica se concatena conteudo atual mais o novo. (valido apenas para campo tipo caractere)
			cCampo  := IIF(lConcat,LEFT(cCampo,Len(cCampo)-1),cCampo)
			cType   := GetSX3Cache(cCampo, "X3_TIPO")
			nTamSX3 := GetSX3Cache(cCampo, "X3_TAMANHO")
			nDecSX3 := GetSX3Cache(cCampo, "X3_DECIMAL")

			If cType <> "M"
				lConcat := .F.
			EndIf

			If cType == NIL
				Aviso("Import CSV", "ERRO: campo " + cCampo + " não encontrado no cadastro.")
				lRet := .F.
				Exit
			Else
				aAdd(::aFields, {cCampo, cType, nTamSX3, nDecSX3, lConcat})
			Endif
		Next nX

		// Remove a linha de cabeçalho dos dados.
		aDel(::aData, 1)
		aSize(::aData, Len(::aData) - 1)
	Else
		Aviso("ImportCSV", "O arquivo CSV não possui linhas. Importação cancelada!")
	EndIf

Return lRet

//-------------------------------------------------------------------
/*/{Protheus.doc} TreatData
Trata os dados lidos. Os dados de um CSV são lidos todos como texto,
o metodo transforma os dados de acordo com o tipo do campo definido
no header.

@author Juliane Venteu
@since 07/12/2016
/*/
//-------------------------------------------------------------------
METHOD TreatData() CLASS ImportCSV

Local lRet    := .T.
Local cType
Local nTamSX3
Local nDecSX3
Local nLenCab := len(::aFields)
Local nX, nY

	For nX := 1 to len(::aData)
		aSize(::aData[nX], nLenCab)
		For nY := 1 to nLenCab
			cType   := ::aFields[nY][TIPO]
			nTamSX3 := ::aFields[nY][TAMANHO]
			nDecSX3 := ::aFields[nY][DECIMAIS]
			Default ::aData[nX][nY] := ""
			If cType == "D"
				// Transforma o texto em data.
				::aData[nX][nY] := CTOD(::aData[nX][nY])
			ElseIf cType == "N"
				// Troca virgula por ponto para indicar decimal.
				::aData[nX][nY] := StrTran(::aData[nX][nY], ",", ".")
				// Transforma o texto em numero, e arredonda o valor.
				::aData[nX][nY] := round(val(::aData[nX][nY]), nDecSX3)
			ElseIf cType == "C"
				::aData[nX][nY] := left(::aData[nX][nY], nTamSX3)
			Endif
		Next nY
	Next nX

Return lRet

//-------------------------------------------------------------------
/*/{Protheus.doc} ValidData
Executa o bloco de validação.

@author Juliane Venteu
@since 07/12/2016
/*/
//-------------------------------------------------------------------
METHOD ValidData() CLASS ImportCSV
	::lValidData := Eval(::bValid, ::aFields, ::aData)
Return ::lValidData

//-------------------------------------------------------------------
/*/{Protheus.doc} GetData
Retorna o array de dados lidos, excluindo o cabeçalho.

@author Juliane Venteu
@since 07/12/2016
/*/
//-------------------------------------------------------------------
METHOD GetData() CLASS ImportCSV
Return ::aData

//-------------------------------------------------------------------
/*/{Protheus.doc} Import
Se a planilha está validada, importa os dados para o sistema chamando
o bloco de execauto.

@author Juliane Venteu
@since 07/12/2016
/*/
//-------------------------------------------------------------------
METHOD Import() CLASS ImportCSV

	If ::lValidData
		Processa( {|| ::Execute() }, "Aguarde...", "Importando dados...", .F.)
	Else
		Aviso("ImportCSV", "Corrija os erros encontrados na planilha e execute o programa de atualização novamente.")
	EndIf
Return

//-------------------------------------------------------------------
/*/{Protheus.doc} Execute
Monta o array de execauto para importação.

OBS: Implementado até o momento, apenas importação de dados de uma tabela,
se for um caso de Tabela pai e filho, tem que implementar (Exemplo: Pedido de Venda)

@author Juliane Venteu
@since 07/12/2016
/*/
//-------------------------------------------------------------------
METHOD Execute() CLASS ImportCSV

Local aAuto      := {}
Local nRegs      := 0
Local cPerc      := ""
Local cErrorFile := ""
Local cErrorPara := ""
Local cErrorLog  := ""
Local cErrorPath := "c:\LOGS\ImportCSV\"
Local nX, nY
Local cFilBkp    := cFilAnt

Private cFilAnt := cFilBkp

Private lMsErroAuto := .F.
Private nOpc        := 0  // Declaração para não gerar error log (Chamado Interno: 1512127852 || Chamado TOTVS: 3398068)

	// Cria pasta de logs de erro.
	FwMakeDir("c:\LOGS\ImportCSV\")

	nRegs := Len(::aData)
	ProcRegua(nRegs)
	For nX := 1 to nRegs

        // Import. TES Inteligente
		If IsInCallStack('U_PROAA100')
		    If ::aFields[12][1] == "FM_TE" .AND. !Empty(::aData[nX][12])
				cRegIdent := "TIPO DE ENTRADA: " + AllTrim(cValToChar(::aData[nX][12]))
			Else
				cRegIdent := "TIPO DE SAÍDA: " + AllTrim(cValToChar(::aData[nX][13]))
			EndIf
		Else
			cRegIdent := AllTrim(cValToChar(::aData[nX][01]))
			cPerc     := Str(nX / nRegs * 100, 3) + "%"
			IncProc("Atualizando " + cRegIdent + " (" + cPerc + ")")
		EndIf

		aAuto := {}
		For nY := 1 to Len(::aFields)
			If '_FILIAL' $ ::aFields[nY][CAMPO] .AND. !IsInCallStack('U_PROAA100') .AND. !IsInCallStack("U_PROMA460")// Bloco de alteração
				cFilAnt := ::aData[nX][nY]
			Else
				aAdd(aAuto, {::aFields[nY][CAMPO], ::aData[nX][nY], nil})
			EndIf
		Next nY

		lMsErroAuto := .F.
        nopc := 4 // 4-Alterar. Atribuição para evitar error log (Chamado Interno: 1512127852 || Chamado TOTVS: 3398068)

		// Import. TES Inteligente
		If IsInCallStack('U_PROAA100')
		   msExecAuto(::bAuto, aAuto, ::nOpc) // Inclusão
		   aAuto := {}
		Else
		   msExecAuto(::bAuto, aAuto, 4, ::aFields)
		EndIf


		If cFilAnt <> cFilBkp
			cFilAnt := cFilBkp
		EndIf
		If lMsErroAuto
			cErrorFile := NomeAutoLog()
			If !empty(cErrorFile)
				// Lê arquivo de erro.
				cErrorLog := "Erro ao importar registro " + cRegIdent + CRLF
				cErrorLog += MemoRead(cErrorFile)

				// Monta arquivo de erro.
				cErrorPara := cErrorPath + "ERR_" + CriaTrab(nil, .F.) + ".LOG"
				If file(cErrorPara)
					fErase(cErrorPara)
				Endif
				MemoWrite(cErrorPara, cErrorLog)
				fErase(cErrorFile)

			Endif
		Endif
	Next nX

	If !empty(cErrorLog)
		MsgAlert("Ocorreram erros em alguns registros. Favor consultar logs na pasta '" + cErrorPath + "'.")
	Else
		MsgInfo("Registros atualizados com sucesso.")
	Endif
Return

//-------------------------------------------------------------------
/*/{Protheus.doc} SetOperation
Define qual será a operação executada pela exec auto.

@author Juliane Venteu
@since 07/12/2016
/*/
//-------------------------------------------------------------------
METHOD SetOperation(nOpc) CLASS ImportCSV
	::nOpc := nOpc
Return

//-------------------------------------------------------------------
/*/{Protheus.doc} SetBAuto
Define o bloco de codigo executado na execAuto,

@author Juliane Venteu
@since 07/12/2016
@sample oImport:SetBAuto({|x, y| MATA010(x, y)})

/*/
//-------------------------------------------------------------------
METHOD SetBAuto(bAuto) CLASS ImportCSV
	::bAuto := bAuto
Return
