#INCLUDE "PROTHEUS.CH"
#INCLUDE "MSOBJECT.CH"
//-------------------------------------------------------------------
/*/{Protheus.doc} BRWXARR
Funcao Generica para Compilacao

@author  Guilherme Santos
@since   25/02/2022
@version 12.1.2210
/*/
//-------------------------------------------------------------------
User Function BRWXARR()
Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} uBrwXArr
Classe para criacao de Interface com 3 Browses relacionados

@author  Guilherme Santos
@since   25/02/2022
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Class uBrwXArr
	Data cRotina		//Rotina Principal
	Data cTitulo		//Titulo da Dialog Principal
	Data aButtons		//Botoes da Dialog Principal
	Data oFWLayer		//Layer onde os Browses serão incluidos
	Data nSizeFld		//Proporcao do Tamanho dos campos do Browse

	Data oPnlPar		//Painel da GetDados de Parametros
	Data aFldPar		//Campos da GetDados de Parametros
	Data aButPar		//Botoes da GetDados de Parametros
	Data lGetPar		//Define a GetDados de Parametros existe
	Data aParam			//Array com os Parametros das Consultas
	Data aParObj		//Array com os Objetos da Tela de Parametros
	Data aObjBut		//Array com os Botoes da Tela de Parametros
	Data oGrpChk		//Grupo CheckBox
	Data aChkItens		//Itens CheckBox
	Data cLoadPar		//Bloco de Codigo para carregamento dos parametros

	Data oPnlSup		//Painel do Browse Superior
	Data oBrwSup		//Browse Superior
	Data aFldSup		//Campos do Browse Superior
	Data aColSup		//Colunas para o Browse Superior - Objeto do Tipo FWBrwColumn
	Data aDataSup		//Dados do Browse Superior
	Data cQrySup		//Codigo que retorna a Query para seleção dos dados para o Browse Superior
	Data lBrwSup		//Define se existe o Browse Superior
	Data lEditSup		//Permite a Edicao de Campos no Browse
	Data bDblCSup		//Bloco de Codigo do Duplo Clique do Browse Superior

	Data oPnlInf		//Painel do Browse Inferior
	Data oBrwInf		//Browse Inferior
	Data aFldInf		//Campos do Browse Inferior
	Data aColInf		//Colunas para o Browse Inferior - Objeto do Tipo FWBrwColumn
	Data aDataInf		//Dados do Browse Inferior
	Data cQryInf		//Bloco de Codigo que retorna a Query para seleção dos dados para o Browse Inferior
	Data lEditInf		//Permite a Edicao de Campos no Browse
	Data bDblCInf		//Bloco de Codigo do Duplo Clique do Browse Inferior
	Data lLoadPed		//Carregamento do Array de Pedidos (PROMA725)

	Data aRelation		//Campos do Array Superior que serão enviados como parametros para a Query do Browse Inferior
	Data lBrwInf		//Define se existe o Browse Inferior

	Data oPnlRod		//Painel do Browse do Rodape
	Data oBrwRod		//Browse Rodape
	Data aFldRod		//Campos Rodape
	Data aColRod		//Colunas Rodape
	Data aDataRod		//Dados Browse Rodape
	Data cQryRod		//Codigo que retorna a Query Rodape para seleção dos dados para o Browse do Rodape
	Data aRelRod		//Campos do Array Inferior que serao enviados como parametros para a Query do Browse do Rodape
	Data lBrwRod		//Define se existe o Browse de Rodape
	Data lEditRod		//Permite a Edicao de Campos no Browse
	Data bDblCRod		//Bloco de Codigo do Duplo Clique do Browse Inferior

	Data cVldExit		//Validacao para Fechar a tela

	Data nTamPar		//Tamanho da Tela de Parametros
	Data nTamSup		//Tamanho do Browse Superior
	Data nTamInf		//Tamanho do Browse Inferior
	Data nTamRod		//Tamanho do Browse Rodape

	Data lPrtSup
	Data lPrtInf
	Data lPrtRod

	Data lCfgSup
	Data lCfgInf
	Data lCfgRod

	Data lWinSup
	Data lWinInf
	Data lWinRod

	Method New(cRotina, cTitulo, aButtons, aFldSup, aFldInf, cQrySup, cQryInf, aRelation, aFldRod, cQryRod, aRelRod, cVldExit, lLoadPed, aFldPar, aButPar)
	Method Show(lEditSup, lEditInf, lEditRod, bDblCSup, bDblCInf, bDblCRod, lEmpSup, lEmpInf, lEmpRod)
	Method SetGetPar()
	Method SetBrwSup()
	Method SetBrwInf()
	Method SetBrwRod()
	Method GetFields(nBrowse, nOpcao)
	Method GetDataSup()
	Method GetDataInf(nPosSup)
	Method GetDataRod(nPosInf)
	Method GetQrySup()
	Method GetQryInf(aParInf)
	Method GetQryRod(aParRod)
	Method RefrSup()
	Method RefrInf()
	Method RefrRod()
	Method GetPosSup()
	Method GetPosInf()
	Method GetPosRod()
	Method GetColPar(cCampo)
	Method GetColSup(cCampo)
	Method GetColInf(cCampo)
	Method GetColRod(cCampo)
	Method GetCpoSup(nLinha, nColuna)
	Method GetCpoInf(nLinha, nColuna)
	Method GetCpoRod(nLinha, nColuna)
	Method GetCampo(nBrowse, cField)
	Method SetCampo(nBrowse, cField, xValor)
	Method LoadPed()
	Method SetPrintBrw(lBrwSup, lBrwInf, lBrwRod)
	Method SetConfigBrw(lBrwSup, lBrwInf, lBrwRod)
	Method SetWinPnl(lWinSup, lWinInf, lWinRod)
	Method GetParam(cCampo)
	Method SetParam(cCampo, xConteudo)
EndClass
//-------------------------------------------------------------------
/*/{Protheus.doc} New
Metodo Construtor

@author  Guilherme Santos
@since   25/02/2022
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method New(cRotina, cTitulo, aButtons, aFldSup, aFldInf, cQrySup, cQryInf, aRelation, aFldRod, cQryRod, aRelRod, cVldExit, lLoadPed, aFldPar, aButPar, cLoadPar) Class uBrwXArr
	Local nI			:= 0
	Local nTelas		:= 0
	Local nTamTot		:= 100

	Default cRotina		:= ""
	Default aButtons	:= {}
	Default aFldSup		:= {}
	Default cQrySup		:= ""
	Default aRelation	:= {}
	Default aFldInf		:= {}
	Default cQryInf		:= ""
	Default aRelRod		:= {}
	Default aFldRod 	:= {}
	Default cQryRod		:= ""
	Default cVldExit	:= ""
	Default lLoadPed	:= .F.
	Default aFldPar		:= {}
	Default aButPar		:= {}
	Default cLoadPar	:= ""

	::cRotina	:= cRotina
	::cTitulo	:= cTitulo
	::aButtons	:= aClone(aButtons)
	::oFWLayer	:= NIL
	::nSizeFld	:= 0.75

	::oPnlPar	:= NIL
	::aFldPar	:= aClone(aFldPar)
	::aButPar	:= aClone(aButPar)
	::lGetPar	:= !Empty(::aFldPar)
	::aParam	:= {}
	::aParObj	:= {}
	::aObjBut	:= {}
	::oGrpChk 	:= NIL
	::aChkItens	:= NIL
	::cLoadPar	:= cLoadPar

	::oPnlSup	:= NIL
	::oBrwSup	:= NIL
	::aFldSup	:= aClone(aFldSup)		//Formato: 01-Nome Campo, 02-Tipo, 03-Tamanho, 04-Decimal, 05-Picture, 06-Codeblock, 07-Descricao
	::aDataSup	:= {}
	::cQrySup	:= cQrySup
	::lEditSup	:= .F.
	::bDblCSup	:= NIL

	::lBrwSup	:= !Empty(::aFldSup) .AND. !Empty(::cQrySup)

	::oPnlInf	:= NIL
	::oBrwInf	:= NIL
	::aFldInf	:= aClone(aFldInf)		//Formato: 01-Nome Campo, 02-Tipo, 03-Tamanho, 04-Decimal, 05-Picture, 06-Codeblock, 07-Descricao
	::aDataInf	:= {}
	::cQryInf	:= cQryInf
	::lEditInf	:= .F.
	::bDblCInf	:= NIL

	::aRelation	:= {}

	//Busca as Colunas do Relacionamento por Nome
	For nI := 1 to Len(aRelation)
		Aadd(::aRelation, ::GetColSup(aRelation[nI]))
	Next nI

	::lBrwInf	:= !Empty(::aFldInf) .AND. !Empty(::cQryInf) .AND. !Empty(::aRelation)

	::oPnlRod	:= NIL
	::oBrwRod	:= NIL
	::aFldRod	:= aClone(aFldRod)
	::aDataRod	:= {}
	::cQryRod	:= cQryRod
	::aRelRod	:= {}

	//Busca as Colunas do Relacionamento por Nome
	For nI := 1 to Len(aRelRod)
		Aadd(::aRelRod, ::GetColInf(aRelRod[nI]))
	Next nI

	::lEditRod	:= .F.
	::bDblCRod	:= NIL

	::lBrwRod	:= !Empty(::aFldRod) .AND. !Empty(::cQryRod) .AND. !Empty(::aRelRod)

	::cVldExit	:= cVldExit

	::lLoadPed	:= lLoadPed

	nTelas		+= If(::lBrwSup, 1, 0) + If(::lBrwInf, 1, 0) + If(::lBrwRod, 1, 0)

	If ::lGetPar
		::nTamPar := 36
		nTamTot -= ::nTamPar
	Else
		::nTamPar := 0
	EndIf

	::nTamSup	:= Int(nTamTot / nTelas)
	::nTamInf	:= Int(nTamTot / nTelas)
	::nTamRod	:= Int(nTamTot / nTelas)

	Do Case
	Case nTelas == 2
		::nTamSup := Int(::nTamSup * 1.10)
		::nTamInf := Int(::nTamInf * 0.90)
	Case nTelas == 3
		::nTamSup := Int(::nTamSup * 1.10)
		::nTamInf := Int(::nTamInf * 0.95)
		::nTamRod := Int(::nTamRod * 0.95)
	EndCase

	If ::nTamPar + ::nTamSup + ::nTamInf + ::nTamRod < 100
		::nTamSup := 100 - ::nTamPar - ::nTamInf - ::nTamRod
	EndIf

	//Reduz o Tamanho em 7% para exibicao das Barras de Rolagem
	::nTamSup	-= 07
	::nTamInf	-= 07
	::nTamRod	-= 07

	::lPrtSup := .F.
	::lPrtInf := .F.
	::lPrtRod := .F.

	::lCfgSup := .F.
	::lCfgInf := .F.
	::lCfgRod := .F.

	::lWinSup := .F.
	::lWinInf := .F.
	::lWinRod := .F.

Return Self
//-------------------------------------------------------------------
/*/{Protheus.doc} Show
Exibicao da Interface

@author  Guilherme Santos
@since   25/02/2022
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method Show(lEditSup, lEditInf, lEditRod, bDblCSup, bDblCInf, bDblCRod, lEmpSup, lEmpInf, lEmpRod) Class uBrwXArr
	Local aCoors 		:= FWGetDialogSize(GetWndDefault())		//Coordenadas da Dialog Principal
	Local oDialog		:= NIL
	Local aButtons		:= aClone(::aButtons)
	Local lVldExit		:= !Empty(::cVldExit)

	Default lEditSup	:= .F.
	Default lEditInf	:= .F.
	Default lEditRod	:= .F.
	Default lEmpSup		:= .F.		//Inicializa o Browse zerado
	Default lEmpInf		:= .F.		//Inicializa o Browse zerado
	Default lEmpRod		:= .F.		//Inicializa o Browse zerado

	::lEditSup	:= lEditSup
	::lEditInf	:= lEditInf
	::lEditRod	:= lEditRod

	If !Empty(bDblCSup)
		::bDblCSup	:= bDblCSup
	EndIf

	If !Empty(bDblCInf)
		::bDblCInf	:= bDblCInf
	EndIf

	If !Empty(bDblCRod)
		::bDblCRod	:= bDblCRod
	EndIf

	DEFINE MSDIALOG oDialog Title ::cTitulo From aCoors[1], aCoors[2] To aCoors[3], aCoors[4] PIXEL OF GetWndDefault()
		//Cria o container onde serao colocados os browses
		::oFWLayer	:= FWLayer():New()
		::oFWLayer:Init(oDialog, .F.)

		If ::lGetPar
			::SetGetPar()
		EndIf

		If ::lBrwSup
			//Definicoes do Browse Superior
			::SetBrwSup(lEmpSup)
		EndIf

		If ::lBrwInf
			//Definicoes do Browse Inferior
			::SetBrwInf(lEmpInf)
		EndIf

		If ::lBrwRod
			::SetBrwRod(lEmpRod)
			::oBrwRod:Activate()
		EndIf

		If ::lLoadPed
			::LoadPed()
		EndIf

		If ::lBrwInf
			::oBrwInf:Activate()
		EndIf

		If ::lBrwSup
			::oBrwSup:Activate()
		EndIf

	ACTIVATE MSDIALOG oDialog CENTERED ON INIT EnchoiceBar(oDialog, NIL, {|| If(If(lVldExit , &(::cVldExit), .T.), oDialog:End(), NIL)}, .F., aButtons, NIL, NIL, .F., .F., .F., .F., .F., "0")

Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} SetGetPar
Definicoes da GetDados de Parametros

@author  Guilherme Santos
@since   27/10/2023
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method SetGetPar() Class uBrwXArr
	Local oFont 	:= TFont():New("Arial", NIL, 14, NIL, .F.)
	Local nX		:= 0
	Local nY		:= 0
	Local nLin		:= 00
	Local nAlt		:= 15
	Local nQtdCol	:= 05
	Local nMargem	:= 10
	Local nTamSay	:= 70
	Local nTamCol	:= 100
	Local nTamLin	:= 22
	Local nPosSay	:= 00
	Local nPosCol	:= 00
	Local nColuna	:= 01
	Local lPixel	:= .T.
	Local bBlqVld	:= NIL
	Local bWhen		:= NIL
	Local bBlqVar	:= NIL
	Local cBlqVar	:= ""
	Local cPictInt	:= "@E 999999"
	Local cPictVlr	:= "@E 999,999,999.99"
	Local cPicture	:= ""
	Local cTpObj	:= ""
	Local lQuebra	:= .F.

	::aParam		:= Array(Len(::aFldPar))
	::aParObj		:= Array(Len(::aFldPar))
	::aObjBut		:= Array(Len(::aButPar))

	//Define Painel Superior
	::oFWLayer:AddLine("PAR", ::nTamPar, .T.)
	::oFWLayer:AddCollumn("ALL", 100, .T., "PAR")
	::oFWLayer:AddWindow("ALL", "WIN1", "Informe os Filtros", 100, .F., .F., NIL, "PAR")

	::oPnlPar := ::oFWLayer:GetWinPanel("ALL", "WIN1", "PAR")

	//--------------------
	//Formato Array Campos
	//--------------------
	//01 - Descrição
	//02 - Tipo
	//03 - Tamanho
	//04 - Decimal
	//05 - Picture
	//06 - Validação
	//07 - F3
	//08 - When
	//09 - Opções CheckBox ou ComboBox
	//10 - Campo Modelo (CriaVar)
	//11 - ID Campo
	//12 - Tamanho GET
	//--------------------
	For nX := 1 to Len(::aFldPar)

		//Se passou um tamanho diferente para o GET quebra a linha
		//Se for a primeira linha ignora e quebra mais uma linha depois de incluir o GET
		If Len(::aFldPar[nX]) == 12
			lQuebra := .T.
		EndIf

		//Se for CheckList quebra a linha
		If ::aFldPar[nX][02] == "CHECK"
			lQuebra := .T.
		EndIf

		If nX == 1
			nLin 	:= 10
			nPosSay := 10
			nColuna	:= 01
			nPosCol := nPosSay + nTamSay + nMargem
		Else
			If lQuebra .OR. nColuna >= nQtdCol
				nColuna := 01
				nLin	+= nTamLin
				nPosSay := 10
				nPosCol := nPosSay + nTamSay + nMargem
			Else
				nColuna += 1
				nPosSay := nPosCol + nTamCol + nMargem
				nPosCol := nPosSay + nTamSay + nMargem
			EndIf
		EndIf

		lQuebra 	:= .F.
		cPicture	:= NIL

		If ::aFldPar[nX][02] == "CHECK"
			cTpObj := "CHECK"
		ElseIf ::aFldPar[nX][02] == "COMBO"
			cTpObj	:= "COMBO"
		Else
			cTpObj	:= "GET"
		EndIf

		If cTpObj != "CHECK"
			TSay():New(C(nLin), C(nPosSay), &("{|| '" + ::aFldPar[nX][01] + "'}") , ::oPnlPar, NIL, oFont, NIL, NIL, NIL, .T., NIL, NIL, C(nTamSay), C(nAlt))
		EndIf

		Do Case
		Case ::aFldPar[nX][02] == "C" .OR. ::aFldPar[nX][02] == "CHECK" .OR. ::aFldPar[nX][02] == "COMBO"
			If Empty(::aFldPar[nX][10])
				::aParam[nX] := Space(::aFldPar[nX][03])
			Else
				::aParam[nX] := CriaVar(::aFldPar[nX][10], .F.)
			EndIf
		Case ::aFldPar[nX][02] == "D"
			If Empty(::aFldPar[nX][10])
				::aParam[nX] := CtoD("")
			Else
				::aParam[nX] := CriaVar(::aFldPar[nX][10], .F.)
			EndIf
			cPicture := "@D"
		Case ::aFldPar[nX][02] == "N"
			If ::aFldPar[nX][04] > 0
				If Empty(::aFldPar[nX][10])
					::aParam[nX] := Transform(0, cPictVlr)
				Else
					::aParam[nX] := Transform(CriaVar(::aFldPar[nX][10], .F.), cPictVlr)
				EndIf

				If Empty(::aFldPar[nX][05])
					cPicture		:= cPictVlr
				Else
					cPicture := ::aFldPar[nX][05]
				EndIf
			Else
				If Empty(::aFldPar[nX][10])
					::aParam[nX] := Transform(0, cPictInt)
				Else
					::aParam[nX] := Transform(CriaVar(::aFldPar[nX][10], .F.), cPictInt)
				EndIf

				If Empty(::aFldPar[nX][05])
					cPicture := cPictInt
				Else
					cPicture := ::aFldPar[nX][05]
				EndIf
			EndIf
		EndCase

		If Empty(::aFldPar[nX][06])
			bBlqVld := {|| .T.}
		Else
			bBlqVld := &("{||" + ::aFldPar[nX][06] + "}")
		EndIf

		If Empty(::aFldPar[nX][08])
			bWhen := {|| .T.}
		Else
			bWhen := &("{||" + ::aFldPar[nX][08] + "}")
		EndIf
		
		cBlqVar := "Self:aParam[" + AllTrim(Str(nX)) + "]"
		bBlqVar := &("{|U| If(PCount() == 0, " + cBlqVar + ", " + cBlqVar + " := U)}")

		Do Case
		Case cTpObj == "GET"
			If Len(::aFldPar[nX]) == 12
				::aParObj[nX] := TGet():New(C(nLin), C(nPosCol), bBlqVar, ::oPnlPar, C(::aFldPar[nX][12]), C(nAlt), cPicture, bBlqVld, NIL, NIL, oFont, NIL, NIL, lPixel, NIL, NIL, bWhen, NIL, NIL, NIL, NIL, NIL, NIL, cBlqVar, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL)
				//Quebra a linha para que o GET com tamanho diferente utilize a linha toda
				lQuebra := .T.
			Else
				::aParObj[nX] := TGet():New(C(nLin), C(nPosCol), bBlqVar, ::oPnlPar, C(nTamCol), C(nAlt), cPicture, bBlqVld, NIL, NIL, oFont, NIL, NIL, lPixel, NIL, NIL, bWhen, NIL, NIL, NIL, NIL, NIL, NIL, cBlqVar, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL)
			EndIf

			If !Empty(::aFldPar[nX][07])
				::aParObj[nX]:cF3 := ::aFldPar[nX][07]
			EndIf
		Case cTpObj == "COMBO"
			::aParObj[nX] := TComboBox():New(C(nLin), C(nPosCol), bBlqVar, ::aFldPar[nX][09], C(nTamCol), C(nAlt), ::oPnlPar, NIL, NIL, bBlqVld, NIL, NIL, lPixel, oFont, NIL, NIL, bWhen, NIL, NIL, NIL, NIL, ::aParam[nX], NIL, NIL, NIL, NIL)
		Case cTpObj == "CHECK"
			::aChkItens		:= Array(Len(::aFldPar[nX][09]))
			::aParam[nX]	:= Array(Len(::aFldPar[nX][09]))
   			::oGrpChk		:= TGroup():New(C(nLin), C(nPosSay), C(nLin + nTamLin), C((nTamCol + nMargem) * Len(::aFldPar[nX][09])), ::aFldPar[nX][01], ::oPnlPar, NIL, NIL, .T.)
			nLin 			+= 010
			nPosSay			+= nMargem

			For nY := 1 to Len(::aFldPar[nX][09])
				bBlqVld := &("{||" + ::aFldPar[nX][06] + "(" + AllTrim(Str(nY)) + ")" + "}")
				cBlqVar := "Self:aParam[" + AllTrim(Str(nX)) + "][" + AllTrim(Str(nY)) + "]"
				bBlqVar := &("{|U| If(PCount() == 0, " + cBlqVar + ", " + cBlqVar + " := U)}")
				::aParam[nX][nY] := .T.
				::aChkItens[nY]	:= TCheckBox():New( C(nLin), C(nPosSay), ::aFldPar[nX][09][nY], bBlqVar, ::oGrpChk, C(nTamCol), C(nAlt), NIL, bBlqVld/*bLClicked*/, oFont, NIL /*bBlqVld*/, NIL, NIL, NIL, lPixel, NIL, NIL, bWhen)
				nPosSay += nTamCol + nMargem
			Next nY

			//Quebra a linha para que o Checklist utilize a linha toda
			lQuebra := .T.
		EndCase
	Next nX

	If !Empty(::aButPar)
		nLin 	+= nTamLin
		nPosCol := 10
		bWhen 	:= {|| .T.}

		For nX := 1 to Len(::aButPar)
			::aObjBut := TButton():New(C(nLin), C(nPosCol), ::aButPar[nX][01], ::oPnlPar, ::aButPar[nX][02], C(70), C(20), NIL, oFont, NIL, lPixel, NIL, NIL, NIL, bWhen, NIL, NIL)
			nPosCol += 80
		Next nX
	EndIf

	If !Empty(::cLoadPar)
		&(::cLoadPar)
	EndIf

Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} SetBrwSup
Definicoes do Browse Superior

@author  Guilherme Santos
@since   25/02/2022
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method SetBrwSup(lEmpSup) Class uBrwXArr
	Local aFilter	:= ::GetFields(1, 2)
	Local aSeek		:= ::GetFields(1, 3)

	Default lEmpSup	:= .F.

	::aColSup	:= ::GetFields(1, 1)
	::aDataSup	:= ::GetDataSup(lEmpSup)

	//Define Painel Superior
	::oFWLayer:AddLine("UP", ::nTamSup, .T.)
	::oFWLayer:AddCollumn("ALL", 100, .T., "UP")

	If ::lWinSup
		::oFWLayer:AddWindow("ALL", "WIN2", "", 100, .F., .F., NIL, "UP")
		::oPnlSup := ::oFWLayer:GetWinPanel("ALL", "WIN2", "UP")
	Else
		::oPnlSup := ::oFWLayer:GetColPanel("ALL", "UP")
	EndIf

	::oBrwSup	:= FWBrowse():New()

	//Browse Superior
	::oBrwSup:SetDescription(::cTitulo)
	::oBrwSup:SetDataArray()
	::oBrwSup:SetColumns(::aColSup)
	::oBrwSup:SetArray(::aDataSup)
	::oBrwSup:SetOwner(::oPnlSup)

	If !::lPrtSup
		::oBrwSup:DisableReport()
	EndIf

	If !::lCfgSup
		::oBrwSup:DisableConfig()
	EndIf

	If ::lBrwInf
		::oBrwSup:SetChange({|| MsAguarde({|| ::RefrInf()}, "Atualizando Browse inferior...")})
	EndIf

	::oBrwSup:SetProfileID(::cRotina + "SUP")
	::oBrwSup:SetSeek(NIL, aSeek)
	::oBrwSup:SetFieldFilter(aFilter)
	::oBrwSup:SetUseFilter(0)
	::oBrwSup:SetUseCaseFilter(0)

	If !Empty(::bDblCSup)
		::oBrwSup:SetDoubleClick(::bDblCSup)
	EndIf

	If ::lEditSup
		::oBrwSup:SetEditCell(.T.)
	EndIf

Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} SetBrwInf
Definicoes do Browse Inferior

@author  Guilherme Santos
@since   25/02/2022
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method SetBrwInf(lEmpInf) Class uBrwXArr
	Default lEmpInf := .F.

	::aColInf	:= ::GetFields(2, 1)
	::aDataInf	:= ::GetDataInf(::oBrwSup:nAt, lEmpInf)

	//Define o Painel Inferior
	::oFWLayer:AddLine("DOWN", ::nTamInf, .T.)
	::oFWLayer:AddCollumn("ALL" , 100, .T., "DOWN")

	If ::lWinInf
		::oFWLayer:AddWindow("ALL", "WIN3", "", 100, .F., .F., NIL, "DOWN")
		::oPnlInf := ::oFWLayer:GetWinPanel("ALL", "WIN3", "DOWN")
	Else
		::oPnlInf	:= ::oFWLayer:GetColPanel("ALL", "DOWN")
	EndIf

	::oBrwInf 	:= FWBrowse():New()

	::oBrwInf:SetOwner(::oPnlInf)
	::oBrwInf:SetDescription(::cTitulo)
	::oBrwInf:SetDataArray()
	::oBrwInf:SetColumns(::aColInf)
	::oBrwInf:SetArray(::aDataInf)
	::oBrwInf:DisableFilter()

	If !::lPrtInf
		::oBrwInf:DisableReport()
	EndIf

	If !::lCfgInf
		::oBrwInf:DisableConfig()
	EndIf

	::oBrwInf:SetProfileID(::cRotina + "INF")

	If ::lBrwRod
		::oBrwInf:SetChange({|| MsAguarde({|| ::RefrRod()}, "Atualizando Browse rodape...")})
	EndIf

	If !Empty(::bDblCInf)
		::oBrwInf:SetDoubleClick(::bDblCInf)
	EndIf

	If ::lEditInf
		::oBrwInf:SetEditCell(.T.)
	EndIf

Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} SetBrwRod
Definicoes do Browse Rodape

@author  Guilherme Santos
@since   23/06/2022
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method SetBrwRod(lEmpRod) Class uBrwXArr
	Default lEmpRod := .F.

	::aColRod	:= ::GetFields(3, 1)
	::aDataRod	:= ::GetDataRod(::oBrwInf:nAt, lEmpRod)

	//Define o Painel Inferior
	::oFWLayer:AddLine("ROD", ::nTamRod, .T.)
	::oFWLayer:AddCollumn("ALL" , 100, .T., "ROD")

	If ::lWinRod
		::oFWLayer:AddWindow("ALL", "WIN4", "", 100, .F., .F., NIL, "ROD")
		::oPnlRod := ::oFWLayer:GetWinPanel("ALL", "WIN4", "ROD")
	Else
		::oPnlRod	:= ::oFWLayer:GetColPanel("ALL", "ROD")
	EndIf

	::oBrwRod 	:= FWBrowse():New()

	::oBrwRod:SetOwner(::oPnlRod)
	::oBrwRod:SetDescription(::cTitulo)
	::oBrwRod:SetDataArray()
	::oBrwRod:SetColumns(::aColRod)
	::oBrwRod:SetArray(::aDataRod)
	::oBrwRod:DisableFilter()

	If !::lPrtRod
		::oBrwRod:DisableReport()
	EndIf

	If !::lCfgRod
		::oBrwRod:DisableConfig()
	EndIf

	::oBrwRod:SetProfileID(::cRotina + "ROD")

	If !Empty(::bDblCRod)
		::oBrwRod:SetDoubleClick(::bDblCRod)
	EndIf

	If ::lEditRod
		::oBrwRod:SetEditCell(.T.)
	EndIf

Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} GetFields
Retorna os Campos para o Browse informado

@author  Guilherme Santos
@since   10/02/2022
@version 12.1.2210
@param	nBrowse 1=Browse Superior;2=Browse Inferior
@param	nOpcao 1=Estrutura Browse;2=Campos Filtros;3=Campos Seek
/*/
//-------------------------------------------------------------------
Method GetFields(nBrowse, nOpcao) Class uBrwXArr
	Local aRetorno	:= {}
	Local nCampo	:= 0
	Local aLegenda	:= {}

	Do Case
	Case nBrowse == 1		//Browse Superior
		Do Case
		Case nOpcao == 1
			For nCampo := 1 to Len(::aFldSup)
				aLegenda := {}

				Aadd(aRetorno, FWBrwColumn():New())
				aRetorno[Len(aRetorno)]:SetType(::aFldSup[nCampo][2])
				aRetorno[Len(aRetorno)]:SetData(::aFldSup[nCampo][6])
				aRetorno[Len(aRetorno)]:SetTitle(::aFldSup[nCampo][7])
				aRetorno[Len(aRetorno)]:SetSize(Round(::aFldSup[nCampo][3] * ::nSizeFld, 0))
				aRetorno[Len(aRetorno)]:SetDecimal(::aFldSup[nCampo][4])
				aRetorno[Len(aRetorno)]:SetPicture(::aFldSup[nCampo][5])

				Do Case
				Case ::aFldSup[nCampo][2] == "C"
					aRetorno[Len(aRetorno)]:SetAlign(CONTROL_ALIGN_LEFT)
				Case ::aFldSup[nCampo][2] == "D"
					aRetorno[Len(aRetorno)]:SetAlign(CONTROL_ALIGN_LEFT)
				Case ::aFldSup[nCampo][2] == "N"
					aRetorno[Len(aRetorno)]:SetAlign(CONTROL_ALIGN_RIGHT)
				Case ::aFldSup[nCampo][2] == "BT"
					aRetorno[Len(aRetorno)]:SetAlign(CONTROL_ALIGN_LEFT)
					aRetorno[Len(aRetorno)]:SetImage(.T.)
				EndCase

				If ::lEditSup .AND. Len(::aFldSup[nCampo]) >= 10 .AND. ::aFldSup[nCampo][8]		//Campo Editavel
					aRetorno[Len(aRetorno)]:SetEdit(.T.)
					aRetorno[Len(aRetorno)]:SetReadVar(::aFldSup[nCampo][9])
					aRetorno[Len(aRetorno)]:SetValid(::aFldSup[nCampo][10])
					If Len(::aFldSup[nCampo]) >= 11 .AND. !Empty(::aFldSup[nCampo][11])
						aRetorno[Len(aRetorno)]:SetF3(::aFldSup[nCampo][11])
					EndIf
					If Len(::aFldSup[nCampo]) == 12
						aRetorno[Len(aRetorno)]:SetOptions(::aFldSup[nCampo][12])
					EndIf
				EndIf
			Next nCampo
		Case nOpcao == 2
			For nCampo := 1 to Len(::aFldSup)
				If ::aFldSup[nCampo][2] <> "BT"
					Aadd(aRetorno,	{	::aFldSup[nCampo][7],;				//01 - Nome
										::aFldSup[nCampo][7],;				//02 - Descricao
										::aFldSup[nCampo][2],;				//03 - Tipo
										::aFldSup[nCampo][3],;				//04 - Tamanho
										::aFldSup[nCampo][4],;				//05 - Decimal
										::aFldSup[nCampo][5]})				//06 - Picture
				EndIf
			Next nCampo
		Case nOpcao == 3
			For nCampo := 1 to Len(::aFldSup)
				If ::aFldSup[nCampo][2] == "C"
					Aadd(aRetorno, {::aFldSup[nCampo][7]})
				EndIf
			Next nCampo
		EndCase

	Case nBrowse == 2		//Browse Inferior
		Do Case
		Case nOpcao == 1
			For nCampo := 1 to Len(::aFldInf)
				Aadd(aRetorno, FWBrwColumn():New())
				aRetorno[Len(aRetorno)]:SetType(::aFldInf[nCampo][2])
				aRetorno[Len(aRetorno)]:SetData(::aFldInf[nCampo][6])
				aRetorno[Len(aRetorno)]:SetTitle(::aFldInf[nCampo][7])
				aRetorno[Len(aRetorno)]:SetSize(Round(::aFldInf[nCampo][3] * ::nSizeFld, 0))
				aRetorno[Len(aRetorno)]:SetDecimal(::aFldInf[nCampo][4])
				aRetorno[Len(aRetorno)]:SetPicture(::aFldInf[nCampo][5])

				Do Case
				Case ::aFldInf[nCampo][2] == "C"
					aRetorno[Len(aRetorno)]:SetAlign(CONTROL_ALIGN_LEFT)
				Case ::aFldInf[nCampo][2] == "D"
					aRetorno[Len(aRetorno)]:SetAlign(CONTROL_ALIGN_LEFT)
				Case ::aFldInf[nCampo][2] == "N"
					aRetorno[Len(aRetorno)]:SetAlign(CONTROL_ALIGN_RIGHT)
				Case ::aFldInf[nCampo][2] == "BT"
					aRetorno[Len(aRetorno)]:SetAlign(CONTROL_ALIGN_LEFT)
					aRetorno[Len(aRetorno)]:SetImage(.T.)
				EndCase

				If ::lEditInf .AND. Len(::aFldInf[nCampo]) >= 10 .AND. ::aFldInf[nCampo][8]		//Campo Editavel
					aRetorno[Len(aRetorno)]:SetEdit(.T.)
					aRetorno[Len(aRetorno)]:SetReadVar(::aFldInf[nCampo][9])
					aRetorno[Len(aRetorno)]:SetValid(::aFldInf[nCampo][10])

					If Len(::aFldInf[nCampo]) >= 11 .AND. !Empty(::aFldInf[nCampo][11])
						aRetorno[Len(aRetorno)]:SetF3(::aFldInf[nCampo][11])
					EndIf
					If Len(::aFldInf[nCampo]) == 12
						aRetorno[Len(aRetorno)]:SetOptions(::aFldInf[nCampo][12])
					EndIf
				EndIf
			Next nCampo
		EndCase
	Case nBrowse == 3
		Do Case
		Case nOpcao == 1
			For nCampo := 1 to Len(::aFldRod)
				Aadd(aRetorno, FWBrwColumn():New())
				aRetorno[Len(aRetorno)]:SetType(::aFldRod[nCampo][2])
				aRetorno[Len(aRetorno)]:SetData(::aFldRod[nCampo][6])
				aRetorno[Len(aRetorno)]:SetTitle(::aFldRod[nCampo][7])
				aRetorno[Len(aRetorno)]:SetSize(Round(::aFldRod[nCampo][3] * ::nSizeFld, 0))
				aRetorno[Len(aRetorno)]:SetDecimal(::aFldRod[nCampo][4])
				aRetorno[Len(aRetorno)]:SetPicture(::aFldRod[nCampo][5])

				Do Case
				Case ::aFldRod[nCampo][2] == "C"
					aRetorno[Len(aRetorno)]:SetAlign(CONTROL_ALIGN_LEFT)
				Case ::aFldRod[nCampo][2] == "D"
					aRetorno[Len(aRetorno)]:SetAlign(CONTROL_ALIGN_LEFT)
				Case ::aFldRod[nCampo][2] == "N"
					aRetorno[Len(aRetorno)]:SetAlign(CONTROL_ALIGN_RIGHT)
				Case ::aFldRod[nCampo][2] == "BT"
					aRetorno[Len(aRetorno)]:SetAlign(CONTROL_ALIGN_LEFT)
					aRetorno[Len(aRetorno)]:SetImage(.T.)
				EndCase

				If ::lEditRod .AND. Len(::aFldRod[nCampo]) >= 10 .AND. ::aFldRod[nCampo][8]		//Campo Editavel
					aRetorno[Len(aRetorno)]:SetEdit(.T.)
					aRetorno[Len(aRetorno)]:SetReadVar(::aFldRod[nCampo][9])
					aRetorno[Len(aRetorno)]:SetValid(::aFldRod[nCampo][10])

					If Len(::aFldRod[nCampo]) >= 11 .AND. !Empty(::aFldRod[nCampo][11])
						aRetorno[Len(aRetorno)]:SetF3(::aFldRod[nCampo][11])
					EndIf
					If Len(::aFldRod[nCampo]) == 12
						aRetorno[Len(aRetorno)]:SetOptions(::aFldRod[nCampo][12])
					EndIf
				EndIf
			Next nCampo
		EndCase
	EndCase

Return aRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} GetDataSup
Retorna os dados para o Browse Superior

@author  Guilherme Santos
@since   25/02/2022
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method GetDataSup(lEmpSup) Class uBrwXArr
	Local aAux		:= {}
	Local aRetorno 	:= {}
	Local cQuery	:= ""
	Local cTabHead	:= ""
	Local nAux		:= 0
	Local nCampo	:= 0
	Local lEmpty	:= .F.

	Default lEmpSup := .F.

	If lEmpSup
		lEmpty := .T.
	Else
		cQuery		:= ::GetQrySup()
		cTabHead	:= MpSysOpenQuery(cQuery)

		If (cTabHead)->(Eof())
			lEmpty := .T.
		Else
			While !(cTabHead)->(Eof())
				For nAux := 1 to (cTabHead)->(FCount())
					If ::aFldSup[nAux][02] == "D"
						Aadd(aAux, StoD((cTabHead)->(FieldGet(nAux))))
					Else
						Aadd(aAux, (cTabHead)->(FieldGet(nAux)))
					EndIf
				Next nAux

				Aadd(aRetorno, aAux)
				aAux := {}

				(cTabHead)->(DbSkip())
			End
		EndIf

		If Select(cTabHead) > 0
			(cTabHead)->(DbCloseArea())
		EndIf
	EndIf

	If lEmpty
		//Inicializa um Array Vazio para o Browse
		aAux	:= Array(Len(::aFldSup))

		For nCampo := 1 to Len(::aFldSup)
			Do Case
			Case ::aFldSup[nCampo][02] == "C"
				aAux[nCampo] := Space(::aFldSup[nCampo][3])
			Case ::aFldSup[nCampo][02] == "N"
				aAux[nCampo] := 0
			Case ::aFldSup[nCampo][02] == "D"
				aAux[nCampo] := CtoD("")
			OtherWise
				aAux[nCampo] := Space(::aFldSup[nCampo][3])
			EndCase
		Next nCampo

		Aadd(aRetorno, aAux)
		aAux := {}
	EndIf

Return aRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} GetDataInf
Retorna os dados para o Browse Inferior

@author  Guilherme Santos
@since   25/02/2022
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method GetDataInf(nPosSup, lEmpInf) Class uBrwXArr
	Local aAux		:= {}
	Local aRetorno 	:= {}
	Local aParInf	:= {}
	Local cQuery	:= ""
	Local cTabItem	:= ""
	Local nAux		:= 0
	Local nCampo	:= 0
	Local nFldRel	:= 0
	Local lVazio	:= .F.

	Default nPosSup	:= 1
	Default lEmpInf := .F.

	If lEmpInf
		lVazio := .T.
	Else
		//Valida os dados do Browse Superior
		For nCampo := 1 to Len(::aRelation)
			nFldRel := ::aRelation[nCampo]

			If Empty(::aDataSup[nPosSup][nFldRel])
				lVazio := .T.
				Exit
			Else
				Aadd(aParInf, ::aDataSup[nPosSup][nFldRel])
			EndIf
		Next nCampo
	EndIf

	//Executa a Query e retorna os dados do Browse Inferior
	If !lVazio
		cQuery		:= ::GetQryInf(aParInf)
		cTabItem	:= MpSysOpenQuery(cQuery)

		If (cTabItem)->(Eof())
			lVazio := .T.
		Else
			While !(cTabItem)->(Eof())
				For nAux := 1 to (cTabItem)->(FCount())
					If ::aFldInf[nAux][02] == "D"
						Aadd(aAux, StoD((cTabItem)->(FieldGet(nAux))))
					Else
						Aadd(aAux, (cTabItem)->(FieldGet(nAux)))
					EndIf
				Next nAux

				Aadd(aRetorno, aAux)
				aAux := {}

				(cTabItem)->(DbSkip())
			End
		EndIf

		If Select(cTabItem) > 0
			(cTabItem)->(DbCloseArea())
		EndIf
	EndIf

	If lVazio
		//Inicializa um Array Vazio para o Browse
		aAux	:= Array(Len(::aFldInf))

		For nCampo := 1 to Len(::aFldInf)
			Do Case
			Case ::aFldInf[nCampo][02] == "C"
				aAux[nCampo] := Space(::aFldInf[nCampo][3])
			Case ::aFldInf[nCampo][02] == "N"
				aAux[nCampo] := 0
			Case ::aFldInf[nCampo][02] == "D"
				aAux[nCampo] := CtoD("")
			OtherWise
				aAux[nCampo] := Space(::aFldInf[nCampo][3])
			EndCase
		Next nCampo

		Aadd(aRetorno, aAux)
		aAux := {}
	EndIf

Return aRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} GetDataRod
Retorna os dados para o Browse Rodape

@author  Guilherme Santos
@since   23/06/2022
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method GetDataRod(nPosInf, lEmpRod) Class uBrwXArr
	Local aAux		:= {}
	Local aRetorno 	:= {}
	Local aParRod	:= {}
	Local cQuery	:= ""
	Local cTabItem	:= ""
	Local nAux		:= 0
	Local nCampo	:= 0
	Local nFldRel	:= 0
	Local lVazio	:= .F.

	Default nPosInf	:= 1
	Default lEmpRod := .F.

	If lEmpRod
		lVazio := .T.
	Else
		//Valida os dados do Browse Superior
		For nCampo := 1 to Len(::aRelRod)
			nFldRel := ::aRelRod[nCampo]

			If Empty(::aDataInf[nPosInf][nFldRel])
				lVazio := .T.
				Exit
			Else
				Aadd(aParRod, ::aDataInf[nPosInf][nFldRel])
			EndIf
		Next nCampo
	EndIf

	//Executa a Query e retorna os dados do Browse Inferior
	If !lVazio
		cQuery		:= ::GetQryRod(aParRod)
		cTabItem	:= MpSysOpenQuery(cQuery)

		If (cTabItem)->(Eof())
			lVazio := .T.
		Else
			While !(cTabItem)->(Eof())
				For nAux := 1 to (cTabItem)->(FCount())
					If ::aFldRod[nAux][02] == "D"
						Aadd(aAux, StoD((cTabItem)->(FieldGet(nAux))))
					Else
						Aadd(aAux, (cTabItem)->(FieldGet(nAux)))
					EndIf
				Next nAux

				Aadd(aRetorno, aAux)
				aAux := {}

				(cTabItem)->(DbSkip())
			End
		EndIf

		If Select(cTabItem) > 0
			(cTabItem)->(DbCloseArea())
		EndIf
	EndIf

	If lVazio
		//Inicializa um Array Vazio para o Browse
		aAux	:= Array(Len(::aFldRod))

		For nCampo := 1 to Len(::aFldRod)
			Do Case
			Case ::aFldRod[nCampo][02] == "C"
				aAux[nCampo] := Space(::aFldRod[nCampo][3])
			Case ::aFldRod[nCampo][02] == "N"
				aAux[nCampo] := 0
			Case ::aFldRod[nCampo][02] == "D"
				aAux[nCampo] := CtoD("")
			OtherWise
				aAux[nCampo] := Space(::aFldRod[nCampo][3])
			EndCase
		Next nCampo

		Aadd(aRetorno, aAux)
		aAux := {}
	EndIf

Return aRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} GetQrySup
Retorna a Query para Selecao dos dados do Browse Superior

@author  Guilherme Santos
@since   25/02/2022
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method GetQrySup() Class uBrwXArr
	Local cQuery := &(::cQrySup)

	If !U_Producao()
		MemoWrite("C:\temp\" + ::cRotina + "_brwsup.sql", cQuery)
	EndIf

Return cQuery
//-------------------------------------------------------------------
/*/{Protheus.doc} GetQryInf
Retorna a Query para Selecao dos dados do Browse Inferior

@author  Guilherme Santos
@since   25/02/2022
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method GetQryInf(aParInf) Class uBrwXArr
	Local cQuery 	:= ""
	Default aParInf	:= {}

	cQuery := &(::cQryInf)

	If !U_Producao()
		MemoWrite("C:\temp\" + ::cRotina + "_brwinf.sql", cQuery)
	EndIf

Return cQuery
//-------------------------------------------------------------------
/*/{Protheus.doc} GetQryRod
Retorna a Query para Selecao dos dados do Browse Rodape

@author  Guilherme Santos
@since   23/06/2022
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method GetQryRod(aParRod) Class uBrwXArr
	Local cQuery 	:= ""
	Default aParRod	:= {}

	cQuery := &(::cQryRod)

	If !U_Producao()
		MemoWrite("C:\temp\" + ::cRotina + "_brwrod.sql", cQuery)
	EndIf

Return cQuery
//-------------------------------------------------------------------
/*/{Protheus.doc} RefrSup
Atualizacao do Browse dos Orcamentos e Pedidos

@author  Guilherme Santos
@since   06/12/2018
@version 12.1.17
/*/
//-------------------------------------------------------------------
Method RefrSup(lGetData) Class uBrwXArr
	Default lGetData := .F.

	If lGetData
		::aDataSup	:= ::GetDataSup()
	EndIf

	::oBrwSup:SetArray(::aDataSup)
	::oBrwSup:Refresh(.T.)
	SysRefresh()
	::oBrwSup:SetFocus()
Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} RefrInf
Atualizacao do Browse Inferior

@author  Guilherme Santos
@since   10/02/2022
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method RefrInf(lRefrSup) Class uBrwXArr
	Default lRefrSup := .T.

	::aDataInf	:= ::GetDataInf(::oBrwSup:nAt)
	::oBrwInf:SetArray(::aDataInf)
	::oBrwInf:Refresh(.T.)

	If ::lBrwRod .AND. lRefrSup
		::RefrRod()
	EndIf

	If lRefrSup
		::oBrwSup:SetFocus()
		::oBrwSup:GoColumn(1)
	EndIf

Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} RefrRod
Atualizacao do Browse Rodape

@author  Guilherme Santos
@since   23/06/2022
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method RefrRod() Class uBrwXArr
	::aDataRod	:= ::GetDataRod(If(Empty(::oBrwInf:nAt), 1, ::oBrwInf:nAt))
	::oBrwRod:SetArray(::aDataRod)
	::oBrwRod:Refresh(.T.)
Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} GetPosSup
Retorna a Linha atual do Browse Superior

@author  Guilherme Santos
@since   21/06/2022
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method GetPosSup() Class uBrwXArr
Return ::oBrwSup:nAt
//-------------------------------------------------------------------
/*/{Protheus.doc} GetPosInf
Retorna a Linha atual do Browse Inferior

@author  Guilherme Santos
@since   21/06/2022
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method GetPosInf() Class uBrwXArr
Return ::oBrwInf:nAt
//-------------------------------------------------------------------
/*/{Protheus.doc} GetPosRod
Retorna a Linha atual do Browse Rodape

@author  Guilherme Santos
@since   21/06/2022
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method GetPosRod() Class uBrwXArr
Return ::oBrwRod:nAt
//-------------------------------------------------------------------
/*/{Protheus.doc} GetColPar
Retorna a posicao da Coluna informada

@author  Guilherme Santos
@since   10/11/2023
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method GetColPar(cCampo) Class uBrwXArr
	Local nPosCpo := AScan(::aFldPar, {|x| AllTrim(x[11]) == cCampo})
Return nPosCpo
//-------------------------------------------------------------------
/*/{Protheus.doc} GetColSup
Retorna a posicao da Coluna informada

@author  Guilherme Santos
@since   21/06/2022
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method GetColSup(cCampo) Class uBrwXArr
	Local nPosCpo := AScan(::aFldSup, {|x| AllTrim(x[1]) == cCampo})
Return nPosCpo
//-------------------------------------------------------------------
/*/{Protheus.doc} GetColInf
Retorna a posicao da Coluna informada

@author  Guilherme Santos
@since   21/06/2022
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method GetColInf(cCampo) Class uBrwXArr
	Local nPosCpo := AScan(::aFldInf, {|x| AllTrim(x[1]) == cCampo})
Return nPosCpo
//-------------------------------------------------------------------
/*/{Protheus.doc} GetColRod
Retorna a posicao da Coluna informada

@author  Guilherme Santos
@since   21/06/2022
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method GetColRod(cCampo) Class uBrwXArr
	Local nPosCpo := AScan(::aFldRod, {|x| AllTrim(x[1]) == cCampo})
Return nPosCpo
//-------------------------------------------------------------------
/*/{Protheus.doc} GetCpoSup
Retorna o conteudo de um campo do Browse Superior

@author  Guilherme Santos
@since   21/06/2022
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method GetCpoSup(nLinha, nColuna) Class uBrwXArr
	Local xRetorno := ""

	If nLinha <= Len(::aDataSup) .AND. nColuna <= Len(::aFldSup)
		xRetorno := ::aDataSup[nLinha][nColuna]
	EndIf

Return xRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} GetCpoInf
Retorna o conteudo de um campo do Browse Inferior

@author  Guilherme Santos
@since   21/06/2022
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method GetCpoInf(nLinha, nColuna) Class uBrwXArr
	Local xRetorno := ""

	If nLinha <= Len(::aDataInf) .AND. nColuna <= Len(::aFldInf)
		xRetorno := ::aDataInf[nLinha][nColuna]
	EndIf

Return xRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} GetCpoRod
Retorna o conteudo de um campo do Browse Rodape

@author  Guilherme Santos
@since   21/06/2022
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method GetCpoRod(nLinha, nColuna) Class uBrwXArr
	Local xRetorno := ""

	If nLinha <= Len(::aDataRod) .AND. nColuna <= Len(::aFldRod)
		xRetorno := ::aDataRod[nLinha][nColuna]
	EndIf

Return xRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} GetCampo
Retorna o Conteudo de um Campo do Browse

@author  Guilherme Santos
@since   02/09/2022
@version 12.1.33
/*/
//-------------------------------------------------------------------
Method GetCampo(nBrowse, cField) Class uBrwXArr
	Local xRetorno := NIL

	Do Case
	Case nBrowse == 1
		xRetorno := ::GetCpoSup(::GetPosSup(), ::GetColSup(cField))
	Case nBrowse == 2
		xRetorno := ::GetCpoInf(::GetPosInf(), ::GetColInf(cField))
	Case nBrowse == 3
		xRetorno := ::GetCpoRod(::GetPosRod(), ::GetColRod(cField))
	EndCase

Return xRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} SetCampo
Altera o Conteudo de um Campo do Browse

@author  Guilherme Santos
@since   02/09/2022
@version 12.1.33
/*/
//-------------------------------------------------------------------
Method SetCampo(nBrowse, cField, xValor) Class uBrwXArr

	Do Case
	Case nBrowse == 1
		::aDataSup[::GetPosSup()][::GetColSup(cField)] := xValor
	Case nBrowse == 2
		::aDataInf[::GetPosInf()][::GetColInf(cField)] := xValor
		::RefrRod()
	Case nBrowse == 3
		::aDataRod[::GetPosRod()][::GetColRod(cField)] := xValor
	EndCase

Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} LoadPed
Carrega os Dados para Geracao dos Pedidos de Compras

@author  Guilherme Santos
@since   28/02/2023
@version 12.1.33
/*/
//-------------------------------------------------------------------
Method LoadPed() Class uBrwXArr
	Local nSup		:= 0
	Local nInf		:= 0

	If ::lLoadPed
		For nSup := 1 to Len(::aDataSup)
			::oBrwSup:GoTo(nSup, .T.)		//Forca o Refresh para atualizacao
			::RefrInf(.F.)

			For nInf := 1 to Len(::aDataInf)
				::oBrwInf:GoTo(nInf, .T.)		//Forca o Refresh para atualizacao

				//Chama a Rotina para Atualizacao do Array de Pedidos
				U_A725Load("QTDE")

			Next nLine
		Next nSup

		::oBrwSup:GoTo(1, .T.)
		::oBrwInf:GoTo(1, .T.)
		::RefrInf(.F.)
	EndIf

Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} SetPrintBrw
Habilita a Impressao para os Browses

@author  Guilherme Santos
@since   20/10/2023
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method SetPrintBrw(lBrwSup, lBrwInf, lBrwRod) Class uBrwXArr
	::lPrtSup := lBrwSup
	::lPrtInf := lBrwInf
	::lPrtRod := lBrwRod
Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} SetConfigBrw
Habilita a Configuracao das Colunas dos Browses

@author  Guilherme Santos
@since   07/11/2023
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method SetConfigBrw(lBrwSup, lBrwInf, lBrwRod) Class uBrwXArr
	::lCfgSup := lBrwSup
	::lCfgInf := lBrwInf
	::lCfgRod := lBrwRod
Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} SetWinPnl
Habilita a exibição em Janelas

@author  Guilherme Santos
@since   11/12/2023
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method SetWinPnl(lWinSup, lWinInf, lWinRod) Class uBrwXArr
	::lWinSup := lWinSup
	::lWinInf := lWinInf
	::lWinRod := lWinRod
Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} GetParam
Retorna os dados preenchidos nos parametros

@author  Guilherme Santos
@since   31/10/2023
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method GetParam(cCampo) Class uBrwXArr
	Local xRetorno	:= NIL
	Local nPosCpo	:= 0
	Local nCheck	:= 0

	Default cCampo	:= ""

	If Empty(cCampo)
		xRetorno := aClone(::aParam)
	Else
		nPosCpo	:= ::GetColPar(cCampo)

		If nPosCpo > 0
			If ::aFldPar[nPosCpo][02] == "CHECK"
				xRetorno := ""
				For nCheck := 1 to Len(::aParam[nPosCpo])
					If ::aParam[nPosCpo][nCheck]
						xRetorno += Substr(::aFldPar[nPosCpo][09][nCheck], 1, 1)
					Else
						xRetorno += Space(1)
					EndIf
				Next nCheck
			Else
				xRetorno := ::aParam[nPosCpo]
			EndIf
		EndIf
	EndIf

Return xRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} SetParam
Atribui os dados preenchidos nos parametros

@author  Guilherme Santos
@since   31/10/2023
@version 12.1.2210
/*/
//-------------------------------------------------------------------
Method SetParam(cCampo, xConteudo) Class uBrwXArr
	Local nPosCpo 	:= ::GetColPar(cCampo)
	Local nCheck	:= 0

	If nPosCpo > 0
		If ::aFldPar[nPosCpo][02] == "CHECK"
			For nCheck := 1 to Len(xConteudo)
				::aParam[nPosCpo][nCheck] := !Empty(Substr(xConteudo, nCheck, 1))
				::aChkItens[nCheck]:Refresh()
			Next nCheck
		Else
			::aParam[nPosCpo] := xConteudo
		EndIf
	EndIf

Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} C
Funcao responsavel por manter o Layout independente da
resolucao horizontal do Monitor do Usuario.

@author  Norbert/Ernani/Mansano
@since   10/05/2005
@version 12.1.17
/*/
//-------------------------------------------------------------------
Static Function C(nTam)                                                         
	Local nHRes	:=	oMainWnd:nClientWidth	// Resolucao horizontal do monitor
	Local nRet	:= nTam

	//1300 - Notebook 14
	//1532 - Notebook 15
	//1916 - Monitor 19

	If nHRes >= 1800
		nRet *= 0.90
	ElseIf nHRes <= 1799 .AND. nHRes >= 1500
		nRet *= 0.75
	ElseIf nHRes <= 1499 .AND. nHRes >= 1300
		nRet *= 0.65
	Else
		nRet *= 1
	EndIf

Return Int(nRet)
