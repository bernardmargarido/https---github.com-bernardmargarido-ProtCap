#include "protheus.ch"
#include "aarray.ch"
#include "json.ch"
#include "WISStatus.ch"
#include "fileio.ch"

// ##############################################################################
// Projeto  : DVS - WMS
// Autor    : André Lanzieri
// Modulo   : Estoque
// Função   : WISInvE
// Descrição: Consulta inventário estático no WIS.
// Retorno  : Lógico, indicando se a função foi executada com sucesso.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 23/04/18 | André Lanzieri     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function FWISXINV()

	Local oProcess
	Local aParamBox  := {}
	Local aRetParam  := {}

    aAdd(aParamBox, {2, "Documento",      "1-Usa do WIS", {"1-Usa do WIS", "2-Doc. informado"}, 70,, .T.})
    aAdd(aParamBox, {1, "Doc. informado", CriaVar("B7_DOC",  .T.),,,, "mv_par01 = '2'",, .F.})
    aAdd(aParamBox, {1, "Data invent.",   CriaVar("B7_DATA", .T.),,,,, 50, .T.})
    If ParamBox(aParamBox, "Informe os parâmetros", @aRetParam,,,,,,,, .F., .F.)
        If aRetParam[1] = '2' .and. empty(aRetParam[2])
            MsgAlert("Informe um número de documento", "Atenção")
        Else
            oProcess  := MsNewProcess():New({|lEnd| WISInvE(oProcess, aRetParam)}, "Inventário estático")
            oProcess:Activate()
        Endif
    Endif

Return

Static Function WISInvE(oProcess, aRetParam)

	Local aRet       := {}
	Local cAlias     := Alias()
	Local aArea      := {}
	Local lErro      := .F.
	Local nX

	Local oJson
	Local cStsCode   := ""
	Local aRetJson   := {}
	Local cJson      := ""
	Local nArqs      := 0

	Local lDocWIS    := (aRetParam[1] = '1')
	Local cDocumento := aRetParam[2]
	Local dInvent    := aRetParam[3]

	Local nHdl       := 0
	Local nTamArq    := 0

	Local aInvent    := {}
	Local aReg       := {}
	Local nReg       := 0
	Local nQtdeRegs  := 0
	Local cPorcent   := ""
	Local cMsgProc   := ""

	Local nTamSB1    := TamSX3("B8_PRODUTO")[1]
	Local nTamSB7    := TamSX3("B7_DOC")[1]
	Local cProduto   := ""
	Local nQtdeInv   := 0
	Local lRecLock
	Local cLocal  := ''
	Local cWisDep := ''
	Local aFilWIS		:= U_WISDPFIL()
	Private lMsErroAuto := .F.

	// Acerta a barra de progressão superior.
	oProcess:SetRegua1(1)
	oProcess:IncRegua1("Consultando WIS...")

	oJson := Array(#)
	oJson[#"usuario"] := "%cUser%"
	oJson[#"senha"]   := "%cPass%"

	// Envia o Json para o webservice.
	aRet := U_WISAPost("/saida/inventario/estatico", oJson, "SB7", nil, {|nCount, cRetPost, cStsCode, cHeadRet| !(cRetPost = "NENHUMA ")})

	// Acerta a barra de progressão superior.
	nArqs := len(aRet)
	oProcess:SetRegua1(nArqs + 1)

	// Se deu certo, trata o Json retornado.
	For nX := 1 to nArqs
		lErro    := .F.
		aRetJson := aRet[nX]
		oProcess:IncRegua1("Arquivo " + cValToChar(nX) + " de " + cValToChar(nArqs) + "...")

		oProcess:SetRegua2(2)
		oProcess:IncRegua2("Abrindo arquivo...")
		nHdl := fOpen(aRetJson[3], FO_EXCLUSIVE)
		If nHdl > 0

			BEGIN TRANSACTION
			
				fSeek(nHdl, 0, 0)
				nTamArq := fSeek(nHdl, 0, 2)
				fSeek(nHdl, 0, 0)
				cJson := Space(nTamArq)
				fRead(nHdl, @cJson, nTamArq)
				fClose(nHdl)

				cStsCode := aRetJson[4]

				// Processa os registros do arquivo.
				If (cJson = "NENHUMA ")
					lErro := .T.
				Else
					aInvent := FromJson(cJson)
					If ValType(aInvent) == "A"
						
						nQtdeRegs := len(aInvent)
						oProcess:SetRegua2(nQtdeRegs)
						
						DbSelectArea('SB7')
						DbSetOrder(3) //B7_FILIAL+B7_DOC+B7_COD+B7_LOCAL                                                                                                                                

						For nReg := 1 to nQtdeRegs
							
							aReg       := aInvent[nReg]
							cPorcent   := Transform((nReg / nQtdeRegs) * 100, "@E 999.9") + "%"
							
							If lDocWIS
								cDocumento := "WIS" + StrZero(aReg[#'nu_inventario'], nTamSB7 - 3)
							Endif
							
							cProduto   	:= PadR(aReg[#'cd_produto'], nTamSB1)
							nQtdeInv   	:= aReg[#'qt_produto']
							cLocal		:= aReg[#'ds_area_erp']
							cWisDep 	:= aReg[#'cd_deposito']

							SB1->(dbSetOrder(1))  // B1_FILIAL, B1_COD.
							SB1->(msSeek(xFilial() + cProduto, .F.))
							cMsgProc := "[" + cDocumento + "] - [" + cProduto + "] - qtde [" + cValToChar(nQtdeInv) + "] " + cPorcent
							oProcess:IncRegua2(cMsgProc)

					 		lRecLock := SB7->( DbSeek( xFilial('SB7') + ;
							 Padr( cDocumento,TAMSX3('B7_DOC')[1]) + ;
							 Padr( cProduto,TAMSX3('B7_COD')[1]) + ;
							 Padr( cLocal,TAMSX3('B7_LOCAL')[1])  ))
					
							RecLock('SB7', !lRecLock) // .F. Alteração - .T. Inclusão
							cFilAnt		:= aFilWis[AScan(aFilWIS, {|x| x[02] == cWisDep})][01]
							SB7->B7_FILIAL 	:= xFilial("SB7")
							SB7->B7_COD    	:= cProduto
							SB7->B7_TIPO    := SB1->B1_TIPO
							SB7->B7_LOCAL 	:= cLocal
							SB7->B7_DOC 	:= cDocumento
							SB7->B7_QUANT 	:= SB7->B7_QUANT+nQtdeInv
							SB7->B7_DATA 	:= dDataBase
							SB7->B7_LOTECTL	:= ""
							SB7->B7_DTVALID	:= dDataBase
							SB7->B7_STATUS  := "1"
							//SB7->B7_CONTAGE	:= "001"
							SB7->B7_ORIGEM  := "MATA270"

							SB7->(MsUnlock())

						Next nReg

						oProcess:IncRegua2("Encerrando arquivo...")
					
					Else

						lErro := .T.
					
					Endif
				Endif

				// Move arquivos para pasta de processados.
				U_WISProc(aRetJson, lErro)

			END TRANSACTION

		Else
			lErro := .T.
		Endif
	Next nX

	oProcess:IncRegua2("Fim")
	oProcess:IncRegua1("Fim")

	If nArqs == 0
		MsgInfo("Nenhum arquivo processado")
	Else
		MsgInfo("Total de arquivos processados: " + cValToChar(nArqs))
	Endif

Return
