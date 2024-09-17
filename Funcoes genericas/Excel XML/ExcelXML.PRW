#INCLUDE "PROTHEUS.CH"

#DEFINE  TAMANHO_LIMITE 50000 // Define o tamanho máximo de array para objetos

//+------------------------------------------------------------------+
//| Rotina implementada para permitir a geração de patch da classe.  |
//+------------------------------------------------------------------+
User Function ExcelXML()
Return .T.

/**************************************************************************************************
BEGINDOC

Classe:
XMLExcel

Autor:
Tiago Bandeira Brasiliano

Data:
21/02/2009

Descrição:
Classe utilizada para criação de um arquivo que pode ser aberto diretamente pelo excel.
Este arquivo é salvo como xml, e pode conter formatações de células que normalmente não são 
aceitas nas classes padrões do Protheus, fornecendo assim uma maior possibilidade para criação
de relatórios customizados para os clientes.

ENDDOC
**************************************************************************************************/
CLASS ExcelXML
	DATA   aRowCount
	DATA   aColCount
	DATA   aCells
	DATA   cStyles          // String contendo o código XML de todos os estilos utilizados pela planilha.
	DATA   cStylesAux       // String auxiliar que contém o ID de todos os estilos utilizados (separado por | ). Utilizado para otimização de performance.
	DATA   aColSizes
	DATA   aRowSizes
	DATA   cRowHeight
	DATA   aMerge
	DATA   aShowGridLine
	DATA   aZoom
	DATA   aLayout
	DATA   aHCenter
	DATA   aVCenter
	DATA   aTopMar
	DATA   aBottomMar
	DATA   aLeftMar
	DATA   aRightMar
	DATA   aHeaderMar
	DATA   aFooterMar
	DATA   aPaperSize
	DATA   aZoomScale
	DATA   aFitWidth
	DATA   aFitHeight
	DATA   aRowBreak
	DATA   aColBreak
	DATA   nFolderCount
	DATA   nFolderIndex
	DATA   aFolderName
	
	METHOD New() CONSTRUCTOR
	METHOD AddRow(aRowSize, aItens, xStyles, nPos)
	METHOD SetColSize(aColSize)
	METHOD SkipLine(cRowSize, xStyle, nQtdLin)
	METHOD SetMerge(nRow, nCol, nRowSize, nColSize)
	METHOD ShowGridLine(lShow)
	METHOD SetZoom(nZoom)
	METHOD SetPageSetup(nLayout, lHCenter, lVCenter, nTopMar, nBottomMar, nLeftMar, nRightMar, nHeaderMar, nFooterMar)
	METHOD SetPrintSetup(nPaperSize, nZoomScale, nFitWidth, nFitHeight)
	METHOD SetRowBreak(nRow)
	METHOD SetColBreak(nCol)
	METHOD SetFolder(nFolder)
	METHOD SetFolderName(cName)
	METHOD GetXML(cArquivo)
	METHOD GetCSV()
ENDCLASS


/**************************************************************************************************
BEGINDOC

Método:
New

Autor:
Tiago Bandeira Brasiliano

Data:
21/02/2009

Descrição:
Construtor da classe ExcelXML

Parâmetros:
Nenhum

Retorno:
Nenhum

ENDDOC
**************************************************************************************************/
METHOD New() CLASS ExcelXML

::aCells        := {{}}
::aRowSizes     := {{}}
::aColSizes     := {{}}
::cRowHeight    := "15"
::cStyles       := ""
::cStylesAux    := "|"
::aMerge        := {{}}
::aRowCount     := {0}
::aColCount     := {0}
::aShowGridLine := {.T.}
::aZoom         := {100}
::aLayout       := {1}
::aHCenter      := {.F.}
::aVCenter      := {.F.}
::aTopMar       := {2}
::aBottomMar    := {2}
::aLeftMar      := {1.3}
::aRightMar     := {1.3} 
::aHeaderMar    := {0.8}
::aFooterMar    := {0.8}
::aPaperSize    := {9}
::aZoomScale    := {100}
::aFitWidth     := {0}
::aFitHeight    := {0}
::aRowBreak     := {{}}
::aColBreak     := {{}}
::nFolderIndex  := 1
::nFolderCount  := 1
::aFolderName   := {"Plan1"}

Return Nil


/**************************************************************************************************
BEGINDOC

Método:
AddRow

Autor:
Tiago Bandeira Brasiliano

Data:
21/02/2009

Descrição:
Adiciona uma nova linha de informação no objeto da planilha excel xml.

Parâmetros:
cRowSize    => Tamanho da linha que será inserida. Esse valor representa a medida da altura em 
               pontos (1 ponto é igual a aproximadamente 1/72 de polegada ou 0,035 cm). A altura 
               padrão da linha é de 12,75 pontos (cerca de 1/6 de polegada ou 0,4 cm). Se uma 
               linha tiver altura igual a 0, ela ficará oculta. O valor é do tipo string. Ex.:
               "8.25"
aItens      => Array contendo o conteúdo das celulas da linha. Este poderá ser um string, um valor
               numérico, um valor do tipo data, uma hora, ou uma string contendo uma fórmula para 
               o excel.
               Formato Hora:
               Para os valores do tipo hora, é necessário passar o valor com o tamanho 9 no seguinte 
               formato: T23:59:59 (utilizar o T maiúsculo antes da hora).
               Além do formato, é necessário acrescentar para a célula um estilo de formatação de hora,
               como "h:mm;@" por exemplo.
               Formato Data:
               Para campos do tipo data, a classe já verifica o ValType do campo e já atribui de forma
               automática um estilo data pré-definido (é possível atribuir outro estilo de data caso 
               necessário.)
               Formato Fórmula:
               Este tipo de dado é identificado através do "=" no início do campo.
               
xStyles     => Objeto do tipo Style ou array de Objetos.
               Se for passado apenas um objeto, este estilo será definido para todas as celulas
               informadas no parâmetro aItens. Caso seja definido um array, cada elemento de 
               aItens terá o estilo definido pelo estilo de mesmo índice de xStyles.
               Se nenhum estilo for passado, o estilo padrão será assumido.
nPos        => Não utilizado - Reservado para o futuro

Retorno:
Nenhum

ENDDOC
**************************************************************************************************/
METHOD AddRow(cRowSize, aItens, xStyles, nPos) CLASS ExcelXML

Local nI       := 0

//+------------------------------------------------------+
//|  Adiciona os conteúdos no array e trata os estilos.  |
//| O tratamento dos estilos é feito com strings         |
//| (cStyles e cStylesAux), pois estavam ocorrendo       |
//| erros quando eram utilizadas variáveis do tipo array |
//| para o controle dos dados.                           |
//| Após a inclusão de várias linhas, algumas            |
//| variáveis de referencia para objetos do tipo         |
//| CellStyle simplesmente ficavam com a referencia nula |
//+------------------------------------------------------+
aAdd(::aCells[::nFolderIndex], {}) // Adiciona a nova linha a planilha

For nI := 1 To Len(aItens)
	If ValType(xStyles) == "A" // Passado um array, onde você tem um estilo para cada célula
		If nI > Len(xStyles)
			aAdd(::aCells[::nFolderIndex, Len(::aCells[::nFolderIndex])], {aItens[nI], Nil})
		Else
			If ValType(xStyles[nI]) == "O"
				aAdd(::aCells[::nFolderIndex, Len(::aCells[::nFolderIndex])], {aItens[nI], xStyles[nI]:cID})
				If !"|"+xStyles[nI]:cID+"|" $ ::cStylesAux
					::cStyles += xStyles[nI]:GetXML()
					::cStylesAux += xStyles[nI]:cID+"|"
				EndIf
			Else
				aAdd(::aCells[::nFolderIndex, Len(::aCells[::nFolderIndex])], {aItens[nI], Nil})
			EndIf				
		EndIf
	Else
		If ValType(xStyles) == "O" // Passado um objeto, onde você tem um único estilo para todas as células.
			aAdd(::aCells[::nFolderIndex, Len(::aCells[::nFolderIndex])], {aItens[nI], xStyles:cID})
			If !"|"+xStyles:cID+"|" $ ::cStylesAux
				::cStyles += xStyles:GetXML()
				::cStylesAux += xStyles:cID+"|"
			EndIF
		Else
			aAdd(::aCells[::nFolderIndex, Len(::aCells[::nFolderIndex])], {aItens[nI], Nil})
		EndIf	
	EndIf	
Next nI
                                                     
//+-------------------------------------------------+
//| Define o tamanho da linha                       |
//+-------------------------------------------------+
aAdd(::aRowSizes[::nFolderIndex], cRowSize)

//+-------------------------------------------------+
//| Incrementa a contagem de linhas                 |
//+-------------------------------------------------+
::aRowCount[::nFolderIndex]++

//+-------------------------------------------------+
//| Incrementa a contagem de colunas                |
//| (atualiza com o maior valor)                    |
//+-------------------------------------------------+
If Len(aItens) > ::aColCount[::nFolderIndex]
	::aColCount[::nFolderIndex] := Len(aItens)
EndIf

Return Nil


/**************************************************************************************************
BEGINDOC

Método:
SetColSize

Autor:
Tiago Bandeira Brasiliano

Data:
25/02/2009

Descrição:
Define a largura das colunas da planilha

Parâmetros:
aRowSize    => Array contendo a medida das colunas da planilha em caracteres.Em uma planilha, você 
               pode especificar uma largura de coluna de 0 (zero) a 255. Esse valor representa o 
               número de caracteres que podem ser exibidos em uma célula formatada com a fonte 
               padrão (fonte padrão: a fonte de texto padrão para planilhas. A fonte padrão 
               determina a fonte padrão para o estilo de célula Normal.). A largura padrão da coluna 
               é de 8,43 caracteres. Se uma coluna tiver largura igual a 0, ela ficará oculta. Ex.:
               {"61.5", "140.25", "70", "82"}

Retorno:
Nenhum

ENDDOC
**************************************************************************************************/
METHOD SetColSize(aColSize) CLASS ExcelXML
                        
::aColSizes[::nFolderIndex] := aColSize

Return Nil


/**************************************************************************************************
BEGINDOC

Método:
SkipLine

Autor:
Tiago Bandeira Brasiliano

Data:
25/02/2009

Descrição:
Pula Linhas na planilha deixando o conteúdo das células em branco e com a altura previamente definida

Parâmetros:
cRowSize    => Tamanho (altura) da célula que será pulada. Esse valor representa a medida da altura em 
               pontos (1 ponto é igual a aproximadamente 1/72 de polegada ou 0,035 cm). A altura 
               padrão da linha é de 12,75 pontos (cerca de 1/6 de polegada ou 0,4 cm). Se uma 
               linha tiver altura igual a 0, ela ficará oculta. O valor é do tipo string. Ex.:
               "8.25"
xStyle      => Estilo que será atribuido para as células. Este parâmetro pode ser um único objeto do
               tipo Style, ou um array de objetos.
nQtdLin     => Determina a quantidade de linhas que serão puladas. Default 1

Retorno:
Nenhum

ENDDOC
**************************************************************************************************/
METHOD SkipLine(cRowSize, xStyle, nQtdLin) CLASS ExcelXML

Local aCols := {}
Local nA := 1

Default nQtdLin := 1

If ::aColCount[::nFolderIndex] > 0
	For nA := 1 To ::aColCount[::nFolderIndex]
		aAdd(aCols, "")
	Next nA
EndIf

For nA := 1 To nQtdLin
	Self:AddRow(cRowSize, aCols, xStyle)
Next nA

Return Nil


/**************************************************************************************************
BEGINDOC

Método:
SetMerge

Autor:
Tiago Bandeira Brasiliano

Data:
28/03/2009

Descrição:
Efetua uma mescla entre células da planilha.
Atenção: Deve ser tomado um cuidado especial para que o merge entre as células nunca ultrapasse
o tamanho total da planilha, tanto das colunas quanto das linhas.
Outro cuidado que deve ser tomado, é na sobreposição de merges, por exemplo, um merge da linha
1 até a 10, e na mesma coluna outro merge da linha 5 até 15.
As duas situações acima podem causar um erro no arquivo XML.

Parâmetros:

nRow        => Linha onde a mescla irá iniciar. Default = última linha adicionada.
nCol        => Coluna onde a mescla irá inicial. Default = primeira coluna da planilha.
nRowSize    => Quatidade de linhas que serão mescladas (Merge Vertical).
nColSize    => Quatidade de colunas que serão mescladas (Merge Horizontal).

Retorno:
Nenhum

ENDDOC
**************************************************************************************************/
METHOD SetMerge(nRow, nCol, nRowSize, nColSize) CLASS ExcelXML

Default nRow     := ::aRowCount[::nFolderIndex]
Default nCol     := 1
Default nRowSize := 0
Default nColSize := 0

If nRowSize <> 0 .Or. nColSize <> 0
	aAdd(::aMerge[::nFolderIndex], {nRow, nCol, nRowSize, nColSize})
EndIf

Return .T.


/**************************************************************************************************
BEGINDOC

Método:
ShowGridLine

Autor:
Tiago Bandeira Brasiliano

Data:
28/03/2009

Descrição:
Exibe ou oculta a exibição das linhas de grade da planilha.

Parâmetros:
lShow       => Indica se as linhas de grades devem ser exibidas (.T.) ou não (.F.)

Retorno:
Nenhum

ENDDOC
**************************************************************************************************/
METHOD ShowGridLine(lShow) CLASS ExcelXML

::aShowGridLine[::nFolderIndex] := lShow

Return Nil


/**************************************************************************************************
BEGINDOC

Método:
SetZoom

Autor:
Tiago Bandeira Brasiliano

Data:
02/04/2009

Descrição:
Define o percentual de zoom para a planilha.

Parâmetros:
nZoom      => Valor numérico e inteiro que define o percentual de zoom para a planilha. O valor
              default para o objeto é 100 (100% de zoom).
              Caso seja passado um valor com decimais, o mesmo será truncado.

Retorno:
Nenhum

ENDDOC
**************************************************************************************************/
METHOD SetZoom(nZoom) CLASS ExcelXML

::aZoom[::nFolderIndex] := NoRound(nZoom,0)

Return Nil


/**************************************************************************************************
BEGINDOC

Método:
SetPageSetup

Autor:
Tiago Bandeira Brasiliano

Data:
07/04/2009

Descrição:
Define opções de impressão para a planilha Excel.

Parâmetros:
nLayout    => Valor numérico e inteiro que define o layout de impressão da planilha:
              1 = Retrato (Portrait) = Valor default para a classe
              2 = Paisagem (Landscape)
lHCenter   => Habilita (.T.) ou desabilita (.F.) a centralização horizontal de impressão.
              O default da classe é desabilitado (.F.).
lVCenter   => Habilita (.T.) ou desabilita (.F.) a centralização vertical de impressão.
              O default da classe é desabilitado (.F.).
nTopMar    => Define o tamanho da margem superior (em centimetros).
              Valor Default = 2. Ex.: 2.6 
nBottomMar => Define o tamanho da margem inferior (em centimetros).
              Valor Default = 2.
nLeftMar   => Define o tamanho da margem esquerda (em centimetros).
              Valor Default = 1.3.
nRightMar  => Define o tamanho da margem direita (em centimetros).
              Valor Default = 1.3.
nHeaderMar => Define o tamanho da margem do cabeçalho (em centimetros).
              Valor Default = 0.8.
nFooterMar => Define o tamanho da margem do rodapé (em centimetros).
              Valor Default = 0.8.

Retorno:
Nenhum

ENDDOC
**************************************************************************************************/
METHOD SetPageSetup(nLayout, lHCenter, lVCenter, nTopMar, nBottomMar, nLeftMar, nRightMar, nHeaderMar, nFooterMar) CLASS ExcelXML

Default nLayout       := 1
Default lHCenter      := .F.
Default lVCenter      := .F.
Default nTopMar       := 2
Default nBottomMar    := 2
Default nLeftMar      := 1.3
Default nRightMar     := 1.3
Default nHeaderMar    := 0.8
Default nFooterMar    := 0.8
      
::aLayout[::nFolderIndex]     := nLayout
::aHCenter[::nFolderIndex]    := lHCenter
::aVCenter[::nFolderIndex]    := lVCenter
::aTopMar[::nFolderIndex]     := nTopMar * 0.3937 // 0.3937 = Constante do tamanho para 1 centímetro
::aBottomMar[::nFolderIndex]  := nBottomMar * 0.3937
::aLeftMar[::nFolderIndex]    := nLeftMar * 0.3937
::aRightMar[::nFolderIndex]   := nRightMar * 0.3937
::aHeaderMar[::nFolderIndex]  := nHeaderMar * 0.3937
::aFooterMar[::nFolderIndex]  := nFooterMar * 0.3937

Return .T.


/**************************************************************************************************
BEGINDOC

Método:
SetPrintSetup

Autor:
Tiago Bandeira Brasiliano

Data:
07/04/2009

Descrição:
Define opções de impressão para a planilha Excel.

Parâmetros:
nPaperSize => Valor numérico e inteiro que define o tipo de papel:
              1  = Carta
              2  = Carta Pequeno
              3  = Tablóide
              4  = Ledger
              5  = Ofício
              6  = Extrato
              7  = Executivo
              8  = A3
              9  = A4 (Default da classe)
              10 = A4 Pequeno
              11 = A5
              12 = B4 (JIS)
              13 = B5 (JIS)
              14 = Fólio
              15 = Quarto
nZoomScale => Valor numérico e inteiro que define o percentual de scala de zoom para a impressão do
              documento. O valor padrão é 100 (100%)
nFitWidth  => Reduz a largura da saída de impressão de modo que ela se ajuste ao número de páginas
              definido por este parâmetro. O default da classe é 0 (Ajuste automático).
nFitHeight => Reduz a altura da saída de impressão de modo que ela se ajuste ao número de páginas
              definido por este parâmetro. O default da classe é 0 (Ajuste automático).

Retorno:
Nenhum

ENDDOC
**************************************************************************************************/
METHOD SetPrintSetup(nPaperSize, nZoomScale, nFitWidth, nFitHeight) CLASS ExcelXML
                                                                  
Default nPaperSize := 9
Default nZoomScale := 100
Default nFitWidth  := 0
Default nFitHeight := 0

::aPaperSize[::nFolderIndex] := nPaperSize
::aZoomScale[::nFolderIndex] := nZoomScale
::aFitWidth[::nFolderIndex]  := nFitWidth
::aFitHeight[::nFolderIndex] := nFitHeight

Return .T.


/**************************************************************************************************
BEGINDOC

Método:
SetRowBreak

Autor:
Tiago Bandeira Brasiliano

Data:
07/04/2009

Descrição:
Define uma quebra de impressão para uma linha.

Parâmetros:
nRow       => Valor numérico e inteiro que define a linha onde será inserida a quebra de impressão.
              Caso nenhuma linha seja informada, a quebra será definida para a última linha
              inserida.

Retorno:
Nenhum

ENDDOC
**************************************************************************************************/
METHOD SetRowBreak(nRow) CLASS ExcelXML
                      
Default nRow := ::aRowCount[::nFolderIndex]

If aScan(::aRowBreak[::nFolderIndex], {|x| x == nRow}) == 0
	aAdd(::aRowBreak[::nFolderIndex], nRow)
EndIf


/**************************************************************************************************
BEGINDOC

Método:
SetColBreak

Autor:
Tiago Bandeira Brasiliano

Data:
07/04/2009

Descrição:
Define uma quebra de impressão para uma coluna.

Parâmetros:
nCol       => Valor numérico e inteiro que define a coluna onde será inserida a quebra de impressão.
              Caso nenhuma coluna seja informada, a quebra será definida para a última coluna.

Retorno:
Nenhum

ENDDOC
**************************************************************************************************/
METHOD SetColBreak(nCol) CLASS ExcelXML

Default nCol := ::aColCount[::nFolderIndex]

If aScan(::aColBreak[::nFolderIndex], {|x| x == nCol}) == 0
	aAdd(::aColBreak[::nFolderIndex], nCol)
EndIf

Return Nil
	

/**************************************************************************************************
BEGINDOC

Método:
SetFolder

Autor:
Tiago Bandeira Brasiliano

Data:
07/07/2009

Descrição:
Define o folder (aba) da planilha que está sendo editado.

Parâmetros:
nFolder    => Valor numérico e inteiro que define o folder editado. Valor default: 1

Retorno:
Nenhum

ENDDOC
**************************************************************************************************/
METHOD SetFolder(nFolder) CLASS ExcelXML

Local nI
Default nFolder := 1

For nI := (::nFolderCount+1) To nFolder
	aAdd(::aCells, {})
	aAdd(::aRowSizes, {})
	aAdd(::aColSizes, {})
	aAdd(::aMerge, {})
	aAdd(::aRowCount, 0)
	aAdd(::aColCount, 0)
	aAdd(::aShowGridLine, .T.)
	aAdd(::aZoom, 100)
	aAdd(::aLayout, 1)
	aAdd(::aHCenter, .F.)
	aAdd(::aVCenter, .F.)
	aAdd(::aTopMar, 2)
	aAdd(::aBottomMar, 2)
	aAdd(::aLeftMar, 1.3)
	aAdd(::aRightMar, 1.3) 
	aAdd(::aHeaderMar, 0.8)
	aAdd(::aFooterMar, 0.8)
	aAdd(::aPaperSize, 9)
	aAdd(::aZoomScale, 100)
	aAdd(::aFitWidth, 0)
	aAdd(::aFitHeight, 0)
	aAdd(::aRowBreak, {})
	aAdd(::aColBreak, {})
	aAdd(::aFolderName, "Plan"+AllTrim(Str(nI)))
Next nI
If nFolder > ::nFolderCount
	::nFolderCount := nFolder
EndIf
::nFolderIndex := nFolder

Return Nil


/**************************************************************************************************
BEGINDOC

Método:
SetFolderName

Autor:
Tiago Bandeira Brasiliano

Data:
07/07/2009

Descrição:
Define o nome da planilha (aba).

Parâmetros:
cName     => String contendo o nome.

Retorno:
Nenhum

ENDDOC
**************************************************************************************************/
METHOD SetFolderName(cName) CLASS ExcelXML

Default cName := "Plan"+AllTrim(Str(::nFolderIndex))

::aFolderName[::nFolderIndex] := cName

Return Nil


/**************************************************************************************************
BEGINDOC

Método:
GetXML

Autor:
Tiago Bandeira Brasiliano

Data:
21/02/2009

Descrição:
Retorna a string contendo o código XML Excel do objeto.

Parâmetros:
cArquivo    => Caminho + nome do arquivo que será gerado. Este parâmetro é opcional.

Retorno:
cRet        => Caso o parâmetro cArquivo não seja passado, será retornado o string contendo o 
               código XML da planilha criada até o momento.
               Caso o parâmetro cArquivo seja passado o retorno será o nome do arquivo caso o 
               arquivo seja gerado com sucesso. Caso ocorra algum problema com a geração do arquivo
               uma string em branco ('') será retornada.

ENDDOC
**************************************************************************************************/             
METHOD GetXML(cArquivo) CLASS ExcelXML
Local cRet         := ""
Local nA, nB
Local cStyle       := ""
Local nPosMerge    := 0
Local lSkipCell    := .F. // Indica se deve omitir a impressão de uma célula
Local nMerRowIni   := 0
Local nMerColIni   := 0
Local nMerRowLim   := 0
Local nMerColLim   := 0
Local lLastPos     := .F.
Local cColCount    := ""
Local cRowCount    := ""
Local cLayout      := ""
Local cHorCenter   := ""
Local cVerCenter   := ""
Local bGravaLinha  := {|cLinha| Iif(lGravaDisco, FWrite(nHandle, cLinha), cRet += cLinha)}
Local nHandle      := 0
Local cArqTemp     := "\EXCELXML"
Local aArqTemp     := {}
Local aMergeMap    := {}
Local lGravaDisco  := .F.
Local cDirDestino  := ""
Local nFldIdx      := 1
Local nI           := 0
Local oMerge       := Nil

Default cArquivo   := ""
lGravaDisco        := !Empty(cArquivo)

//+------------------------------------------------------+
//| Abre o arquivo onde será salvo o XML caso seja usado |
//+------------------------------------------------------+
If lGravaDisco
	makeDir(cArqTemp)
	cArqTemp +="\"+DtoS(Date())+StrTran(Time(),":","")+__cUserID // Gera um diretório temporário no servidor com a data + o código do usuário
	makeDir(cArqTemp)
	cArqtemp += SubStr(cArquivo, Rat("\", cArquivo))
	nHandle := FCreate(cArqTemp, 1) // 1 = FC_NORMAL (leitura + gravação)
EndIf

Eval(bGravaLinha, '<?xml version="1.0" encoding="ISO-8859-1" ?>'+CRLF)
Eval(bGravaLinha, '<?mso-application progid="Excel.Sheet"?>'+CRLF)
Eval(bGravaLinha, '<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"'+CRLF)
Eval(bGravaLinha, ' xmlns:o="urn:schemas-microsoft-com:office:office"'+CRLF)
Eval(bGravaLinha, ' xmlns:x="urn:schemas-microsoft-com:office:excel"'+CRLF)
Eval(bGravaLinha, ' xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"'+CRLF)
Eval(bGravaLinha, ' xmlns:html="http://www.w3.org/TR/REC-html40">'+CRLF)
Eval(bGravaLinha, ' <DocumentProperties xmlns="urn:schemas-microsoft-com:office:office">'+CRLF)
Eval(bGravaLinha, '  <Created></Created>'+CRLF)
Eval(bGravaLinha, '  <Version>12.00</Version>'+CRLF)
Eval(bGravaLinha, ' </DocumentProperties>'+CRLF)
Eval(bGravaLinha, ' <ExcelWorkbook xmlns="urn:schemas-microsoft-com:office:excel">'+CRLF)
Eval(bGravaLinha, '  <WindowHeight>8415</WindowHeight>'+CRLF)
Eval(bGravaLinha, '  <WindowWidth>19095</WindowWidth>'+CRLF)
Eval(bGravaLinha, '  <WindowTopX>0</WindowTopX>'+CRLF)
Eval(bGravaLinha, '  <WindowTopY>150</WindowTopY>'+CRLF)
Eval(bGravaLinha, '  <ProtectStructure>False</ProtectStructure>'+CRLF)
Eval(bGravaLinha, '  <ProtectWindows>False</ProtectWindows>'+CRLF)
Eval(bGravaLinha, ' </ExcelWorkbook>'+CRLF)
Eval(bGravaLinha, ' <Styles>'+CRLF)
Eval(bGravaLinha, '  <Style ss:ID="Default" ss:Name="Normal">'+CRLF)
Eval(bGravaLinha, '   <Alignment ss:Vertical="Bottom"/>'+CRLF)
Eval(bGravaLinha, '   <Borders/>'+CRLF)
Eval(bGravaLinha, '   <Font ss:FontName="Calibri" x:Family="Swiss" ss:Size="11" ss:Color="#000000"/>'+CRLF)
Eval(bGravaLinha, '   <Interior/>'+CRLF)
Eval(bGravaLinha, '   <NumberFormat/>'+CRLF)
Eval(bGravaLinha, '   <Protection/>'+CRLF)
Eval(bGravaLinha, '  </Style>'+CRLF)
Eval(bGravaLinha, '  <Style ss:ID="sData">'+CRLF)
Eval(bGravaLinha, '     <NumberFormat ss:Format="Short Date"/>'+CRLF)
Eval(bGravaLinha, '    </Style>'+CRLF)
//+-------------------------------+
//| Insere os estilos criados     |
//+-------------------------------+
Eval(bGravaLinha, ::cStyles)
Eval(bGravaLinha, ' </Styles>'+CRLF)

For nFldIdx := 1 To ::nFolderCount
	cColCount := Iif(::aColCount[nFldIdx] > 1, AllTrim(Str(::aColCount[nFldIdx])), "1")
	cRowCount := Iif(::aRowCount[nFldIdx] > 1, AllTrim(Str(::aRowCount[nFldIdx])), "1")

	Eval(bGravaLinha, ' <Worksheet ss:Name="'+::aFolderName[nFldIdx]+'">'+CRLF)
	Eval(bGravaLinha, '  <Table ss:ExpandedColumnCount="'+cColCount+'" ss:ExpandedRowCount="'+cRowCount+'" x:FullColumns="1"'+CRLF)
	Eval(bGravaLinha, '   x:FullRows="1" ss:DefaultRowHeight="'+::cRowHeight+'">'+CRLF)
	
	//+----------------------------------------------------+
	//| Obtem o mapa com as células onde ocorrerão o merge |
	//+----------------------------------------------------+
	aMergeMap := MergeMap(::aMerge[nFldIdx])
	                                   
	//+-------------------------------+
	//| Insere a largura das colunas  |
	//+-------------------------------+
	If ValType(::aColSizes[nFldIdx]) == "A" .And. Len(::aColSizes[nFldIdx]) > 0
		For nA := 1 To Min(Len(::aColSizes[nFldIdx]),::aColCount[nFldIdx])
			Eval(bGravaLinha, '   <Column ss:AutoFitWidth="0" ss:Width="'+AllTrim(::aColSizes[nFldIdx, nA])+'"/>'+CRLF)
		Next nA
	EndIf
	
	//+-------------------------------+
	//| Insere as células na planilha |
	//+-------------------------------+
	If Len(::aCells[nFldIdx]) > 0
		ProcRegua(Len(::aCells[nFldIdx]))	
		For nA := 1 To Len(::aCells[nFldIdx])
			IncProc("Gravando dados no arquivo ("+AllTrim(Str(nFldIdx))+"/"+AllTrim(Str(::nFolderCount))+")...")
			If ValType(::aCells[nFldIdx, nA]) == "A"
				
				// Define a altura da linha caso tenha sido informado
				If ValType(::aRowSizes[nFldIdx]) == "A" .And. Len(::aRowSizes[nFldIdx]) >= nA .And. ValType(::aRowSizes[nFldIdx, nA]) == "C" 
					Eval(bGravaLinha, '    <Row ss:AutoFitHeight="0" ss:Height="'+AllTrim(::aRowSizes[nFldIdx, nA])+'">'+CRLF)
				Else
					Eval(bGravaLinha, '    <Row>'+CRLF)
				EndIf
				
				For nB := 1 To Len(::aCells[nFldIdx, nA])
				    
				    lSkipCell := .F.
				    cMerge    := "" 
				    
				    //+-------------------------------------------+
				    //| Tratamento para o merge entre as células. |
				    //+-------------------------------------------+
				    //If Len(aMergeMap) >= nA .And. ValType(aMergeMap[nA]) == "A" .And.;
				    //   Len(aMergeMap[nA]) >= nB .And. ValType(aMergeMap[nA,nB]) == "O"
				    oMerge := GetObjArray(aMergeMap, nA, nB)
				    
				    If ValType(oMerge) == "O"
				    	If oMerge:lSkip
				    		lSkipCell := .T.
				    	Else
					    	If oMerge:nMergeAcross > 0
					    		cMerge := ' ss:MergeAcross="'+AllTrim(Str(oMerge:nMergeAcross))+'"'
					    	EndIf
					    	If oMerge:nMergeDown > 0
					    		cMerge += ' ss:MergeDown="'+AllTrim(Str(oMerge:nMergeDown))+'"'
					    	EndIf
					    	If oMerge:nIndex > 0 
				    			cMerge += ' ss:Index="'+AllTrim(Str(oMerge:nIndex))+'"'
					    	EndIf
					    EndIf		    
				    EndIf
				    
				    /*
				    cMerge    := ""
				    lSkipCell := .F. // Indica se deve omitir a impressão de uma célula
				    nPosMerge := aScan(::aMerge[nFldIdx], {|x| x[1] == nA .And. x[2] == nB})
				    If nPosMerge > 0 .And. nMerRowLim == 0 .And. nMerColLim == 0
				    	nMerRowIni   := ::aMerge[nFldIdx, nPosMerge][1]
				    	nMerColIni   := ::aMerge[nFldIdx, nPosMerge][2]
				    	nMerRowLim   := nA + ::aMerge[nFldIdx, nPosMerge][3] 
				    	nMerColLim   := nB + ::aMerge[nFldIdx, nPosMerge][4] + 1
				    	
				    	If ::aMerge[nFldIdx, nPosMerge][4] > 0
				    		cMerge := ' ss:MergeAcross="'+AllTrim(Str(::aMerge[nFldIdx, nPosMerge][4]))+'"'
				    	EndIf
				    	If ::aMerge[nFldIdx, nPosMerge][3] > 0
				    		cMerge := cMerge+' ss:MergeDown="'+AllTrim(Str(::aMerge[nFldIdx, nPosMerge][3]))+'"'
				    	EndIf
				    	
				    	If nB == Len(::aCells[nFldIdx, nA]) .And. nA == nMerRowLim
				    		lLastPos := .F.
				    		nMerRowIni   := 0
					    	nMerColIni   := 0
					    	nMerRowLim   := 0
					    	nMerColLim   := 0
				    	EndIf
				    ElseIf nB >= nMerColIni .And. nB < nMerColLim
				    	lSkipCell := .T.
				    	If nB + 1 == nMerColLim .And. nB <> Len(::aCells[nFldIdx, nA])
				    		lLastPos := .T.
				    	ElseIf (nB == Len(::aCells[nFldIdx, nA])) .And. nA == nMerRowLim
				    		lLastPos := .F.
				    		nMerRowIni   := 0
					    	nMerColIni   := 0
					    	nMerRowLim   := 0
					    	nMerColLim   := 0
				    	EndIf
				    ElseIf lLastPos 
				    	cMerge := ' ss:Index="'+AllTrim(Str(nMerColLim))+'"'
				    	lLastPos := .F.
				    	If nA == nMerRowLim			    	
					    	If nPosMerge > 0
						    	nMerRowIni   := ::aMerge[nFldIdx, nPosMerge][1]
						    	nMerColIni   := ::aMerge[nFldIdx, nPosMerge][2]
						    	nMerRowLim   := nA + ::aMerge[nFldIdx, nPosMerge][3] 
						    	nMerColLim   := nB + ::aMerge[nFldIdx, nPosMerge][4] + 1
						    	
						    	If ::aMerge[nFldIdx, nPosMerge][4] > 0
						    		cMerge := ' ss:MergeAcross="'+AllTrim(Str(::aMerge[nFldIdx, nPosMerge][4]))+'"'
						    	EndIf
						    	If ::aMerge[nFldIdx, nPosMerge][3] > 0
						    		cMerge := cMerge+' ss:MergeDown="'+AllTrim(Str(::aMerge[nFldIdx, nPosMerge][3]))+'"'
						    	EndIf
						    	
						    	If nB == Len(::aCells[nFldIdx, nA])
						    		nMerRowIni   := 0
							    	nMerColIni   := 0
							    	nMerRowLim   := 0
							    	nMerColLim   := 0
						    	EndIf
						    Else			    	
						    	nMerRowIni   := 0
						    	nMerColIni   := 0
						    	nMerRowLim   := 0
						    	nMerColLim   := 0
						    EndIf
					    EndIf		    	
				    EndIf
				    */
				    
				    If !lSkipCell
					    cStyle := ""
					    If ValType(::aCells[nFldIdx, nA, nB, 2]) == "C"
					    	cStyle := ' ss:StyleID="'+::aCells[nFldIdx, nA, nB, 2]+'"'
					    EndIf
					
						// Define o tipo de dado da célula
						If ValType(::aCells[nFldIdx, nA, nB, 1]) == "C"
							If  Len(::aCells[nFldIdx, nA, nB, 1]) == 9 .And. SubStr(::aCells[nFldIdx, nA, nB, 1],1,1) == "T" .And. SubStr(::aCells[nFldIdx, nA, nB, 1],4,1) == ":" .And. SubStr(::aCells[nFldIdx, nA, nB, 1],7,1) == ":" // Formato Hora => T99:99:99
								cStyle   := Iif(Empty(cStyle), ' ss:StyleID="sData"', cStyle)
								Eval(bGravaLinha, '      <Cell'+cMerge+cStyle+'><Data ss:Type="DateTime">1899-12-31'+::aCells[nFldIdx, nA, nB, 1]+'.000</Data></Cell>'+CRLF)	
							ElseIf SubStr(::aCells[nFldIdx, nA, nB, 1],1,1) == "=" // Fórmula
								Eval(bGravaLinha, '      <Cell'+cMerge+cStyle+' ss:Formula="'+::aCells[nFldIdx, nA, nB, 1]+'"><Data ss:Type="Number"></Data></Cell>'+CRLF)	
							ElseIf Upper(SubStr(::aCells[nFldIdx, nA, nB, 1],1,5)) == "HREF=" // Link Web
								Eval(bGravaLinha, '      <Cell'+cMerge+cStyle+' ss:HRef="'+SubStr(::aCells[nFldIdx, nA, nB, 1],6,At("|", ::aCells[nFldIdx, nA, nB, 1])-6)+'"><Data ss:Type="String">'+SubStr(::aCells[nFldIdx, nA, nB, 1],At("|", ::aCells[nFldIdx, nA, nB, 1])+1)+'</Data></Cell>'+CRLF)
					  		Else
					  			Eval(bGravaLinha, '      <Cell'+cMerge+cStyle+'><Data ss:Type="String">'+::aCells[nFldIdx, nA, nB, 1]+'</Data></Cell>'+CRLF)
							EndIf
						ElseIf ValType(::aCells[nFldIdx, nA, nB, 1]) == "N"
							Eval(bGravaLinha, '      <Cell'+cMerge+cStyle+'><Data ss:Type="Number">'+AllTrim(Str(::aCells[nFldIdx, nA, nB, 1]))+'</Data></Cell>'+CRLF)
						ElseIf ValType(::aCells[nFldIdx, nA, nB, 1]) == "D"
							If !Empty(DtoS(::aCells[nFldIdx, nA, nB, 1]))
								cStyle := Iif(Empty(cStyle), ' ss:StyleID="sData"', cStyle)
								Eval(bGravaLinha, '      <Cell'+cMerge+cStyle+'><Data ss:Type="DateTime">'+SubStr(DtoS(::aCells[nFldIdx, nA, nB, 1]),1,4)+'-'+SubStr(DtoS(::aCells[nFldIdx, nA, nB, 1]),5,2)+'-'+SubStr(DtoS(::aCells[nFldIdx, nA, nB, 1]),7,2)+'T00:00:00.000</Data></Cell>'+CRLF)
							Else // Tratamento para data em branco
								Eval(bGravaLinha, '      <Cell'+cMerge+cStyle+'><Data ss:Type="String"></Data></Cell>'+CRLF)
							EndIf
						Else
							Eval(bGravaLinha, '      <Cell'+cMerge+cStyle+'><Data ss:Type="String"></Data></Cell>'+CRLF)
						EndIf
				    EndIf
				Next nB
				Eval(bGravaLinha, '    </Row>'+CRLF)
			EndIf
		Next nA
	EndIf
	
	Eval(bGravaLinha, '  </Table>'+CRLF)
	Eval(bGravaLinha, '  <WorksheetOptions xmlns="urn:schemas-microsoft-com:office:excel">'+CRLF)
		
	//+----------------------------------------------+
	//| Tratamento para a configuração da página     |
	//+----------------------------------------------+
	cLayout    := Iif(::aLayout[nFldIdx] <> 1, ' x:Orientation="Landscape"', '') // Retrato = 1 / Paisagem = 2
	cHorCenter := Iif(::aHCenter[nFldIdx], ' x:CenterHorizontal="1"', '')
	cVerCenter := Iif(::aVCenter[nFldIdx], ' x:CenterVertical="1"', '')	
	
	Eval(bGravaLinha, '   <PageSetup>'+CRLF)
	If !Empty(cLayout) .Or. !Empty(cHorCenter) .Or. !Empty(cVerCenter) 
		Eval(bGravaLinha, '    <Layout'+cLayout+cHorCenter+cVerCenter+'/>'+CRLF)
	EndIf
	Eval(bGravaLinha, '    <Header x:Margin="'+AllTrim(Str(::aHeaderMar[nFldIdx]))+'"/>'+CRLF)
	Eval(bGravaLinha, '    <Footer x:Margin="'+AllTrim(Str(::aFooterMar[nFldIdx]))+'"/>'+CRLF)
	Eval(bGravaLinha, '    <PageMargins x:Bottom="'+AllTrim(Str(::aBottomMar[nFldIdx]))+'" x:Left="'+AllTrim(Str(::aLeftMar[nFldIdx]))+'"'+CRLF)
	Eval(bGravaLinha, '     x:Right="'+AllTrim(Str(::aRightMar[nFldIdx]))+'" x:Top="'+AllTrim(Str(::aTopMar[nFldIdx]))+'"/>'+CRLF)
	Eval(bGravaLinha, '   </PageSetup>'+CRLF)
	Eval(bGravaLinha, '   <Unsynced/>'+CRLF)
	
	//+------------------------------------------------+
	//| Tratamento para as configurações de impressão  |
	//+------------------------------------------------+
	Eval(bGravaLinha, '   <Print>'+CRLF)
	Eval(bGravaLinha, '    <FitWidth>'+AllTrim(Str(::aFitWidth[nFldIdx]))+'</FitWidth>'+CRLF)
	Eval(bGravaLinha, '    <FitHeight>'+AllTrim(Str(::aFitHeight[nFldIdx]))+'</FitHeight>'+CRLF)
	Eval(bGravaLinha, '    <PaperSizeIndex>'+AllTrim(Str(::aPaperSize[nFldIdx]))+'</PaperSizeIndex>'+CRLF)
	Eval(bGravaLinha, '    <Scale>'+AllTrim(Str(::aZoomScale[nFldIdx]))+'</Scale>'+CRLF)
	Eval(bGravaLinha, '   </Print>'+CRLF)
	   
	//+----------------------------------------------+
	//| Tratamento para o zoom da planilha.          |
	//+----------------------------------------------+
	If ::aZoom[nFldIdx] <> 100
		Eval(bGravaLinha, '   <Zoom>'+AllTrim(Str(::aZoom[nFldIdx]))+'</Zoom>'+CRLF)
	EndIf
	
	Eval(bGravaLinha, '   <Selected/>'+CRLF)
	//+----------------------------------------------+
	//| Tratamento para exibição das linhas de grade |
	//+----------------------------------------------+
	If !::aShowGridLine[nFldIdx]
		Eval(bGravaLinha, '   <DoNotDisplayGridlines/>'+CRLF)
	EndIf
	Eval(bGravaLinha, '   <ProtectObjects>False</ProtectObjects>'+CRLF)
	Eval(bGravaLinha, '   <ProtectScenarios>False</ProtectScenarios>'+CRLF)
	Eval(bGravaLinha, '  </WorksheetOptions>'+CRLF)
	//+----------------------------------------------+
	//| Tratamento para a quebra de impressão        |
	//+----------------------------------------------+
	If Len(::aRowBreak[nFldIdx]) > 0 .Or. Len(::aColBreak[nFldIdx]) > 0
		Eval(bGravaLinha, '    <PageBreaks xmlns="urn:schemas-microsoft-com:office:excel">'+CRLF)
		If Len(::aColBreak[nFldIdx]) > 0
			Eval(bGravaLinha, '     <ColBreaks>'+CRLF)
			For nI := 1 To Len(::aColBreak[nFldIdx])
				Eval(bGravaLinha, '      <ColBreak>'+CRLF)
				Eval(bGravaLinha, '       <Column>'+AllTrim(Str(::aColBreak[nFldIdx, nI]))+'</Column>'+CRLF)
				Eval(bGravaLinha, '      </ColBreak>'+CRLF)
			Next nI
			Eval(bGravaLinha, '     </ColBreaks>'+CRLF)
		EndIf
		If Len(::aRowBreak[nFldIdx]) > 0
			Eval(bGravaLinha, '     <RowBreaks>'+CRLF)
			For nI := 1 To Len(::aRowBreak[nFldIdx])
				Eval(bGravaLinha, '      <RowBreak>'+CRLF)
				Eval(bGravaLinha, '       <Row>'+AllTrim(Str(::aRowBreak[nFldIdx, nI]))+'</Row>'+CRLF)
				Eval(bGravaLinha, '      </RowBreak>'+CRLF)
			Next nI
			Eval(bGravaLinha, '     </RowBreaks>'+CRLF)
		EndIf	
		Eval(bGravaLinha, '    </PageBreaks>'+CRLF)
	EndIf
	
	Eval(bGravaLinha, ' </Worksheet>'+CRLF)
	
	aMergeMap := Nil
Next nFldIdx

Eval(bGravaLinha, '</Workbook>')

//+-------------------------------------------------------+
//| Fecha o arquivo onde será salvo o XML caso seja usado |
//+-------------------------------------------------------+
If lGravaDisco
	cDirDestino := SubStr(cArquivo, 1, RAt("\", cArquivo))
	If FClose(nHandle)
		FT_FUSE()
		cRet := cArquivo // Retorna o nome do arquivo caso tudo esteja ok.
	EndIf
	CpyS2T(cArqTemp, cDirDestino, .T.) // Copia do Servidor para a estação
	FErase(cArqTemp)                                               // Apaga o arquivo temporário do server
	DirRemove(SubStr(cArqTemp, 1, RAt("\", cArqTemp)))             // Apaga o diretório temporário
	//+-----------------------------------------------------------------+
	//| Tratamento efetuado para apagar o arquivo temporário (ap*.tmp)  |
	//| que era deixado na pasta do client. Isto estava gerando erros   |
	//| durante a geração de alguns arquivos excel.                     |
	//+-----------------------------------------------------------------+
	aArqTemp := Directory(cDirDestino+"ap*.tmp")
	aEval(aArqTemp, {|x| FErase(cDirDestino+x[1])})	
EndIf

Return cRet


/**************************************************************************************************
BEGINDOC

Método:
GetCSV

Autor:
Tiago Bandeira Brasiliano

Data:
18/03/2009

Descrição:
Retorna a string contendo as células do objeto no formato separado por vírgula para ser salvo em 
arquivos do tipo CSV.

Parâmetros:
cArquivo    => Caminho + nome do arquivo que será gerado. Este parâmetro é opcional.

Retorno:
cRet        => Caso o parâmetro cArquivo não seja passado, será retornado o string contendo os 
               dados separados por vírgula.
               Caso o parâmetro cArquivo seja passado o retorno será o nome do arquivo caso o 
               arquivo seja gerado com sucesso. Caso ocorra algum problema com a geração do arquivo
               uma string em branco ('') será retornada.

ENDDOC
**************************************************************************************************/             
METHOD GetCSV(cArquivo) CLASS ExcelXML
Local cRet         := ""
Local nA, nB
Local nPosID       := 0
Local cID          := ""
Local aIDs         := {}
Local cStyle       := ""
Local bGravaLinha  := {|cLinha| Iif(lGravaDisco, FWrite(nHandle, cLinha), cRet += cLinha)}
Local nHandle      := 0
Local cArqTemp     := "\EXCELXML"
Local lGravaDisco  := .F.

Default cArquivo   := ""
lGravaDisco        := !Empty(cArquivo)

//+------------------------------------------------------+
//| Abre o arquivo onde será salvo o XML caso seja usado |
//+------------------------------------------------------+
If lGravaDisco
	makeDir(cArqTemp)
	cArqTemp +="\"+DtoS(Date())+StrTran(Time(),":","")+__cUserID
	makeDir(cArqTemp)
	cArqtemp += SubStr(cArquivo, Rat("\", cArquivo))
	nHandle := FCreate(cArqTemp, 1) // 1 = FC_NORMAL (leitura + gravação)
EndIf


//+-------------------------------+
//| Insere as células na planilha |
//+-------------------------------+
If Len(::aCells[1]) > 0
	For nA := 1 To Len(::aCells[1])	
		
		For nB := 1 To Len(::aCells[1, nA])		    
		    
		    If nB <> 1
		    	Eval(bGravaLinha, ";")
		    EndIf
		    
			// Define o tipo de dado da célula e concatena a string de retorno
			If ValType(::aCells[1, nA, nB, 1]) == "C"
				If SubStr(::aCells[1, nA, nB, 1],1,1) <> "="
					Eval(bGravaLinha, ::aCells[1, nA, nB, 1])
				EndIf
			ElseIf ValType(::aCells[1, nA, nB, 1]) == "N"
				Eval(bGravaLinha, AllTrim(Str(::aCells[1, nA, nB, 1])))
			ElseIf ValType(::aCells[1, nA, nB, 1]) == "D"
				Eval(bGravaLinha, DtoS(::aCells[1, nA, nB, 1]))
			EndIf
		  
		Next nB
		Eval(bGravaLinha, CRLF)
	Next nA
EndIf

//+-------------------------------------------------------+
//| Fecha o arquivo onde será salvo o XML caso seja usado |
//+-------------------------------------------------------+
If lGravaDisco
	If FClose(nHandle)
		FT_FUSE()
		cRet := cArquivo // Retorna o nome do arquivo caso tudo esteja ok.
	EndIf
	CpyS2T(cArqTemp, SubStr(cArquivo, 1, RAt("\", cArquivo)), .T.) // Copia do Servidor para a estação
	FErase(cArqTemp)                                               // Apaga o arquivo temporário
	DirRemove(SubStr(cArqTemp, 1, RAt("\", cArqTemp)))             // Apaga o diretório temporário
EndIf

Return cRet


/**************************************************************************************************
Função:
MergeMap

Autor:
Tiago Bandeira Brasiliano

Data:
30/06/2010

Descrição:
Cria o mapeamento de campos para o Merge

Parâmetros:
nFolderIdx  => Indice do folder que está sendo processado.

Retorno:
aMergeMap   => Array contendo o mapeamento de merge, onde:
               aMergeMap[]
**************************************************************************************************/
Static Function MergeMap(aMerge)

Local nMerge    := 0
Local nLin      := 0
Local nCol      := 0
Local nLinIni   := 0
Local nColIni   := 0
Local nLinFim   := 0
Local nColFim   := 0
Local nLinQtd   := 0
Local nColQtd   := 0
Local aMergeMap := {}

For nMerge := 1 To Len(aMerge)
	
	nLinIni  := aMerge[nMerge,1]
	nColIni  := aMerge[nMerge,2]
	nLinQtd  := aMerge[nMerge,3]
	nColQtd  := aMerge[nMerge,4]
	nLinFim  := nLinIni + nLinQtd
	nColFim  := nColIni + nColQtd

	For nLin := nLinIni To nLinFim
	                              
	
		For nCol := nColIni To nColFim + 1 //Adiciona 1, pois a próxima coluna pode ter a informação do indice
			If Len(aMergeMap) >= nLin .And. ValType(aMergeMap[nLin]) == "A" .And.;
			   Len(aMergeMap[nLin]) >= nCol .And. ValType(aMergeMap[nLin,nCol]) == "O"
			   oMrgSts := aMergeMap[nLin,nCol]
			Else
				oMrgSts := MergeCellInfo():New()
			EndIf
		
			If nLin == nLinIni .And. nCol == nColIni
				If nColQtd > 0
					//+---------------+
					//|  Merge Across |
					//+---------------+
		    		oMrgSts:nMergeAcross := nColQtd
				EndIf
				If nLinQtd > 0
					//+---------------+
					//| Merge Down    |
					//+---------------+
		    		oMrgSts:nMergeDown := nLinQtd
				EndIf			
			ElseIf nCol <= nColFim
				//+---------------+
				//| Skip          |
				//+---------------+
		  		oMrgSts:lSkip := .T.
			ElseIf aScan(aMerge, {|x| nLin > x[1] .And. nCol > x[2] }) > 0
				//+---------------+
				//| Index         |
				//+---------------+
		    	oMrgSts:nIndex := nColFim + 1
			EndIf		
		    
		    //+-----------------------------------------+
		    //| Adiciona o status de merge ao mapa      |
		    //+-----------------------------------------+
			AddObjArray(@aMergeMap, nLin, nCol, oMrgSts)
		Next nCol
	
	Next nLin

Next nMerge

Return aMergeMap                    


/**************************************************************************************************
Função:
AddObjArray

Autor:
Tiago Bandeira Brasiliano

Data:
27/07/2010

Descrição:
Adiciona dados em um array, já efetuando o tratamento de paginação.
Esta paginação é necessária, pois durante testes foi verificado que o tamanho máximo para um
array contendo objetos é de 50000 itens.

Parâmetros:
aArray      => Array que será manipulado
nLin        => Linha que será trabalhada
nCol        => Coluna que será trabalhada
xConteudo   => Conteúdo que será atribuido ao array

Retorno:
Nenhum
**************************************************************************************************/
Static Function AddObjArray(aArray, nLin, nCol, xConteudo)

Local nPagina := Int(nLin/(TAMANHO_LIMITE+0.1))+1
Local nLinPag := Iif(Int(nLin % TAMANHO_LIMITE)==0,TAMANHO_LIMITE,Int(nLin % TAMANHO_LIMITE))

// Adiciona nova pagina de array caso necessário
If nPagina > Len(aArray)
	aAdd(aArray, {})
EndIf

// Redimensiona o tamanho da linha caso necessário
If nLinPag > Len(aArray[nPagina])
	aSize(aArray[nPagina], nLinPag)
	aTail(aArray[nPagina]) := {}
EndIf

// Redimensiona o tamanho da coluna caso necessário
If nCol > Len(aArray[nPagina,nLinPag])
	aSize(aArray[nPagina,nLinPag], nCol)
EndIf

aArray[nPagina, nLinPag, nCol] := xConteudo

Return .T.


/**************************************************************************************************
Função:
AddObjArray

Autor:
Tiago Bandeira Brasiliano

Data:
27/07/2010

Descrição:
Obtem dados em um array, já considerando a questão da paginação.
Esta paginação é necessária, pois durante testes foi verificado que o tamanho máximo para um
array contendo objetos é de 50000 itens.

Parâmetros:
aArray      => Array que será manipulado
nLin        => Linha que será trabalhada
nCol        => Coluna que será trabalhada

Retorno:
xDado       => Dado que será retornado
**************************************************************************************************/
Static Function GetObjArray(aArray, nLin, nCol)

Local nPagina := Int(nLin/(TAMANHO_LIMITE+0.1))+1
Local xDado   := Nil

If Len(aArray) >= nPagina .And. Len(aArray[nPagina]) >= nLin .And. ValType(aArray[nPagina, nLin]) == "A" .And.;
   Len(aArray[nPagina, nLin]) >= nCol .And. ValType(aArray[nPagina, nLin, nCol]) == "O"
	xDado   := aArray[nPagina, nLin, nCol] 
EndIf
				       
Return xDado


/**************************************************************************************************
BEGINDOC

Classe:
MergeCellInfo

Autor:
Tiago Bandeira Brasiliano

Data:
02/07/2010 (Holanda 2 X 1 Brasil - Que deprê!)

Descrição:
Classe que contém as informações de merge de uma determinada célula.
Será utilizada nas células mapeadas para o merge.

ENDDOC
**************************************************************************************************/
CLASS MergeCellInfo
	DATA   lSkip
	DATA   nMergeAcross
	DATA   nMergeDown
	DATA   nIndex
	
	METHOD New() CONSTRUCTOR
ENDCLASS


/**************************************************************************************************
BEGINDOC

Método:
New

Autor:
Tiago Bandeira Brasiliano

Data:
02/07/2010

Descrição:
Construtor da classe MergeCellInfo

Parâmetros:
Nenhum

Retorno:
Nenhum

ENDDOC
**************************************************************************************************/
METHOD New() CLASS MergeCellInfo

::lSkip         := .F.
::nMergeAcross  := 0
::nMergeDown    := 0
::nIndex        := 0

Return Nil
