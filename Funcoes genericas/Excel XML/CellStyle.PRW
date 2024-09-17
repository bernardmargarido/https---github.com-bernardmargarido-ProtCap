#INCLUDE "PROTHEUS.CH"

//+------------------------------------------------------------------+
//| Rotina implementada para permitir a geração de patch da classe.  |
//+------------------------------------------------------------------+
User Function CellStyle()
Return .T.

/**************************************************************************************************
BEGINDOC

Classe:
CellStyle

Autor:
Tiago Bandeira Brasiliano

Data:
25/02/2009

Descrição:
Classe utilizada para fornecer diferentes estilos de formatação para as células da classe XMLExcel.


ENDDOC
**************************************************************************************************/
CLASS CellStyle
	DATA   cID
	DATA   cVAlign
	DATA   cHAlign
	DATA   lTopBorder
	DATA   nTopBorderWeight
	DATA   cTopBorderStyle
	DATA   lBottomBorder
	DATA   nBottomBorderWeight
	DATA   cBottomBorderStyle
	DATA   lRightBorder
	DATA   nRightBorderWeight
	DATA   cRightBorderStyle
	DATA   lLeftBorder
	DATA   nLeftBorderWeight
	DATA   cLeftBorderStyle
	DATA   cFontName
	DATA   nFontSize
	DATA   cFontColor
	DATA   lFontBold
	DATA   lFontItalic
	DATA   cFontVAlign
	DATA   cFontUnderline
	DATA   lStrikeThrough
	DATA   cInteriorColor
	DATA   cInteriorPattern
	DATA   cNumberFormat
	DATA   lWrapText
	DATA   nRotate
	DATA   cTopBorderColor
	DATA   cBottomBorderColor
	DATA   cRightBorderColor
	DATA   cLeftBorderColor
	
	METHOD New(cID) CONSTRUCTOR
	METHOD SetVAlign(cVAlign)
	METHOD SetHAlign(cHAlign)
	METHOD SetWrapText(lWrapText)
	METHOD SetRotate(nRotate)
	METHOD SetBorder(cBorderPos, cStyle, nWeight, lEnable, cColor)
	METHOD RemoveBorder(cBorderPos)
	METHOD SetFont(cName, nSize, cColor, lBold, lItalic, lUnderline, cVAlign, lStrikeThrough)
	METHOD SetInterior(cColor, cPattern)
	METHOD SetNumberFormat(cFormat)
	METHOD GetXML()
	METHOD GetID()
ENDCLASS


/**************************************************************************************************
BEGINDOC

Método:
New

Autor:
Tiago Bandeira Brasiliano

Data:
25/02/2009

Descrição:
Construtor da classe CellStyle

Parâmetros:
cName     => Nome do Estilo que está sendo criado

Retorno:
Nenhum

ENDDOC
**************************************************************************************************/
METHOD New(cID) CLASS CellStyle  

::cID                   := cID
::cVAlign               := "Center"
::cHAlign               := "Center"
::lTopBorder            := .F.
::nTopBorderWeight      := 1
::cTopBorderStyle       := "Continuous"
::lBottomBorder         := .F.
::nBottomBorderWeight   := 1
::cBottomBorderStyle    := "Continuous"
::lRightBorder          := .F.
::nRightBorderWeight    := 1
::cRightBorderStyle     := "Continuous"
::lLeftBorder           := .F.
::nLeftBorderWeight     := 1
::cLeftBorderStyle      := "Continuous"
::cFontName             := "Calibri"
::nFontSize             := 11
::cFontColor            := "#000000"
::lFontBold             := .F.
::lFontItalic           := .F.
::cFontVAlign           := ""
::cFontUnderline        := ""
::lStrikeThrough        := .F.
::cInteriorColor        := ""
::cInteriorPattern      := ""
::cNumberFormat         := ""
::lWrapText             := .F. 
::nRotate               := 0
::cTopBorderColor		:= "#000000"
::cBottomBorderColor	:= "#000000"
::cRightBorderColor		:= "#000000"
::cLeftBorderColor		:= "#000000"
	
Return Nil


/**************************************************************************************************
BEGINDOC

Método:
SetVAlign

Autor:
Tiago Bandeira Brasiliano

Data:
25/02/2009

Descrição:
Define a posição vertical do texto dentro da célula.

Parâmetros:
cVAlign     => Tipo do alinhamento:
               "Upper"   => Alinhado em cima
               "Center"  => Alinhado centralizado
               "Bottom"   => Alinhado em baixo

Retorno:
Nenhum

ENDDOC
**************************************************************************************************/
METHOD SetVAlign(cVAlign) CLASS CellStyle

::cVAlign := Capital(cVAlign)

Return Nil


/**************************************************************************************************
BEGINDOC

Método:
SetHAlign

Autor:
Tiago Bandeira Brasiliano

Data:
25/02/2009

Descrição:
Define a posição horizontal do texto dentro da célula.

Parâmetros:
cHAlign     => Tipo do alinhamento:
               "Left"    => Alinhado a esquerda
               "Center"  => Alinhado centralizado
               "Right"   => Alinhado a direita

Retorno:
Nenhum

ENDDOC
**************************************************************************************************/
METHOD SetHAlign(cHAlign) CLASS CellStyle

::cHAlign := Capital(cHAlign)

Return Nil


/**************************************************************************************************
BEGINDOC

Método:
SetWrapText

Autor:
Tiago Bandeira Brasiliano

Data:
06/04/2009

Descrição:
Habilita ou desabilita a quebra automática do texto dentro da célula.

Parâmetros:
lWrapText   => Boleano que indica se o texto será quebrado automaticamente (.T.) ou não (.F).
               O default para o objeto é falso (não quebra automaticamente).
               Atenção: A altura da linha deve ser ajustada manualmente.

Retorno:
Nenhum

ENDDOC
**************************************************************************************************/
METHOD SetWrapText(lWrapText) CLASS CellStyle

::lWrapText := lWrapText

Return Nil


/**************************************************************************************************
BEGINDOC

Método:
SetRotate

Autor:
Tiago Bandeira Brasiliano

Data:
06/04/2009

Descrição:
Define rotação para o texto da célula.

Parâmetros:
nRotate     => Valor em graus para a rotação da célula.
               Este valor deve ser um inteiro entre -90 até 90.
               Valores com decimais serão truncados.

Retorno:
Nenhum

ENDDOC
**************************************************************************************************/
METHOD SetRotate(nRotate) CLASS CellStyle

::nRotate := NoRound(nRotate,0)

Return Nil


/**************************************************************************************************
BEGINDOC

Método:
SetBorder

Autor:
Tiago Bandeira Brasiliano

Data:
10/03/2009

Descrição:
Define a posição, o estilo e a largura de uma borda na célula.

Parâmetros:
cBorderPos  => Posição da Borda que está sendo adicionada:
               "Botton"  => Borda Inferior
               "Left"    => Borda Esquerda
               "Right"   => Borda Direita
               "Top"     => Borda Superior
               "All"     => Todas as bordas
cStyle      => Estilo da borda que está sendo adicionada:
               "Continuous" => Borda contínua (default)
               "Dot"        => Pontilhada
               "DashDotDot" => Pontilhada com traço
nWeight     => Largura da Borda
lEnable     => Habilita ou Desabilita a borda.

Retorno:
Nenhum

ENDDOC
**************************************************************************************************/
METHOD SetBorder(cBorderPos, cStyle, nWeight, lEnable, cColor) CLASS CellStyle

Default cStyle  := "Continuous"
Default nWeight := 1
Default lEnable := .T.
Default cColor  := "#000000"

Do Case
Case Upper(cBorderPos) == "BOTTOM"
	::lBottomBorder          := lEnable
	::nBottomBorderWeight    := nWeight
	::cBottomBorderStyle     := cStyle
	::cBottomBorderColor     := cColor
Case Upper(cBorderPos) == "LEFT"
	::lLeftBorder            := lEnable
	::nLeftBorderWeight      := nWeight
	::cLeftBorderStyle       := cStyle
	::cLeftBorderColor       := cColor
Case Upper(cBorderPos) == "RIGHT"
	::lRightBorder           := lEnable
	::nRightBorderWeight     := nWeight
	::cRightBorderStyle      := cStyle
	::cRightBorderColor      := cColor
Case Upper(cBorderPos) == "TOP"
	::lTopBorder             := lEnable
	::nTopBorderWeight       := nWeight
	::cTopBorderStyle        := cStyle
	::cTopBorderColor        := cColor
Case Upper(cBorderPos) == "ALL"
	::lTopBorder             := lEnable
	::nTopBorderWeight       := nWeight
	::cTopBorderStyle        := cStyle
	::lBottomBorder          := lEnable
	::nBottomBorderWeight    := nWeight
	::cBottomBorderStyle     := cStyle
	::lLeftBorder            := lEnable
	::nLeftBorderWeight      := nWeight
	::cLeftBorderStyle       := cStyle
	::lRightBorder           := lEnable
	::nRightBorderWeight     := nWeight
	::cRightBorderStyle      := cStyle
	::cTopBorderColor        := cColor
	::cBottomBorderColor     := cColor
	::cRightBorderColor      := cColor
	::cLeftBorderColor       := cColor
EndCase

Return Nil


/**************************************************************************************************
BEGINDOC

Método:
RemoveBorder

Autor:
Tiago Bandeira Brasiliano

Data:
10/03/2009

Descrição:
Remove uma borda de uma célula

Parâmetros:
cBorderPos  => Posição da Borda que está sendo Removida:
               "Botton"  => Borda Inferior
               "Left"    => Borda Esquerda
               "Right"   => Borda Direita
               "Upper"   => Borda Superior

Retorno:
Nenhum

ENDDOC
**************************************************************************************************/
METHOD RemoveBorder(cBorderPos) CLASS CellStyle

Self:SetBorder(cBorderPos,,.F.)

Return Nil


/**************************************************************************************************
BEGINDOC

Método:
SetFont

Autor:
Tiago Bandeira Brasiliano

Data:
10/03/2009

Descrição:
Define a fonte de uma célula.

Parâmetros:
cName          => Nome da Fonte que será atribuída para a célula. Ex:
                  "Botton"  => Borda Inferior
                  "Left"    => Borda Esquerda
                  "Right"   => Borda Direita
                  "Upper"   => Borda Superior
nSize          => Tamanho da Fonte
cColor         => Código RGB Hexidecimal da cor da fonte. Ex.:
                  "#000000" => Preto (Default)
                  "#FFFFFF" => Branco
                  "#FF0000" => Vermelho
                  "#00FF00" => Verde
                  "#0000FF" => Azul
lBold          => Negrito (.T. / .F.)
lItalic        => Itálico (.T. / .F.)
lUnderline     => Sublinhado (se omitido, não será utilizado):
                  "Single" => Simples
                  "Double" => Duplo
cVAlign        => Alinhamento vertical da fonte (se omitido, será normal):
                  "Subscript"   => Subscrito
                  "Superscript" => Sobrescrito
lStrike        => Tachado (.T. / .F.)

Retorno:
Nenhum

ENDDOC
**************************************************************************************************/
METHOD SetFont(cName, nSize, cColor, lBold, lItalic, lUnderline, cVAlign, lStrike) CLASS CellStyle

Default cName             := "Calibri"
Default nSize             := 11
Default cColor            := "#000000"
Default lBold             := .F.
Default lItalic           := .F.
Default cVAlign           := ""
Default LUnderline        := ""
Default lStrike           := .F.

::cFontName             := cName
::nFontSize             := nSize
::cFontColor            := cColor
::lFontBold             := lBold
::lFontItalic           := lItalic
::cFontUnderline        := lUnderline
::cFontVAlign           := cVAlign
::lStrikeThrough        := lStrike

Return Nil


/**************************************************************************************************
BEGINDOC

Método:
SetInterior

Autor:
Tiago Bandeira Brasiliano

Data:
10/03/2009

Descrição:
Define o padrão do interior de uma célula.

Parâmetros:
cColor         => Código RGB Hexidecimal da cor da fonte. Ex.:
                  "#000000" => Preto (Default)
                  "#FFFFFF" => Branco
                  "#FF0000" => Vermelho
                  "#00FF00" => Verde
                  "#0000FF" => Azul
cPattern       => Estilo Padrão do Preenchimento. Ex:
                  "Solid"          => Preenchimento sólido.
                  "Gray75"         => Cinza 75%
                  "ThinHorzStripe" => Horizontal Listrado Fina
                  
Retorno:
Nenhum

ENDDOC
**************************************************************************************************/
METHOD SetInterior(cColor, cPattern) CLASS CellStyle

Default cColor    := "#FFFFFF" // Branco
Default cPattern  := "Solid"

::cInteriorColor        := cColor
::cInteriorPattern      := cPattern

Return Nil

/**************************************************************************************************
BEGINDOC

Método:
SetNumberFormat

Autor:
Tiago Bandeira Brasiliano

Data:
10/03/2009

Descrição:
Define o formato numérico para a célula.

Parâmetros:
cFormat        => Define o formato da célula. Exemplos:
               "h:mm;@"     => Formato para hora.
               "Fixed"      => Numérico com duas casas decimais.
               "0.000"      => Numérico com três casas decimais.
               "Percent"    => Percentual
               "Short Date" => Data em formato curto.
               Observação: Quando um formato do tipo data for utilizado, o conteúdo da célula
                           deve ser do tipo data.
                  
Retorno:
Nenhum

ENDDOC
**************************************************************************************************/
METHOD SetNumberFormat(cFormat) CLASS CellStyle

Default cFormat := ""

::cNumberFormat   := cFormat

Return Nil


/**************************************************************************************************
BEGINDOC

Método:
GetXML

Autor:
Tiago Bandeira Brasiliano

Data:
10/03/2009

Descrição:
Retorna a string contendo o código XML do Estilo.

Parâmetros:
Nenhum

Retorno:
cRet        => String contendo o código XML do estilo da célula criada.

ENDDOC
**************************************************************************************************/             
METHOD GetXML() CLASS CellStyle

Local cRet       := ""
Local cBold      := Iif(::lFontBold, ' ss:Bold="1"', '')
Local cItalic    := Iif(::lFontItalic, ' ss:Italic="1"', '')
Local cStrike    := Iif(::lStrikeThrough, ' ss:StrikeThrough="1"', '')
Local cVAlign    := Iif(!Empty(::cFontVAlign), ' ss:VerticalAlign="'+::cFontVAlign+'"', "")
Local cUnderline := Iif(!Empty(::cFontUnderline), ' ss:Underline="="'+::cFontUnderline+'"', "")
Local cRotate    := ""
Local cWrapText  := ""

//+--------------------------------------------+
//| Tratamento para rotação do texto           |
//+--------------------------------------------+
If ::nRotate <> 0
	cRotate := ' ss:Rotate="'+AllTrim(Str(::nRotate))+'"' 
EndIf

//+--------------------------------------------+
//| Tratamento para quebra automática do texto |
//+--------------------------------------------+
If ::lWrapText
	cWrapText := ' ss:WrapText="1"' 
EndIf

cRet  +=  '<Style ss:ID="'+::cID+'">'+CRLF
cRet  +=  '   <Alignment ss:Horizontal="'+::cHAlign+'" ss:Vertical="'+::cVAlign+'"'+cRotate+cWrapText+'/>'+CRLF
//+--------------------------------------------+
//| Tratamento para as bordas                  |
//+--------------------------------------------+
If ::lTopBorder .Or. ::lBottomBorder .Or. ::lRightBorder .Or. ::lLeftBorder	
	cRet  +=  '   <Borders>'+CRLF
	If ::lTopBorder
		cRet  +=  '      <Border ss:Position="Top" ss:LineStyle="'+::cTopBorderStyle+'" ss:Weight="'+AllTrim(Str(::nTopBorderWeight))+'" ss:Color="'+::cTopBorderColor+'"/>'+CRLF
	EndIf
	If ::lBottomBorder
		cRet  +=  '      <Border ss:Position="Bottom" ss:LineStyle="'+::cBottomBorderStyle+'" ss:Weight="'+AllTrim(Str(::nBottomBorderWeight))+'" ss:Color="'+::cBottomBorderColor+'"/>'+CRLF
	EndIf
	If ::lRightBorder
		cRet  +=  '      <Border ss:Position="Right" ss:LineStyle="'+::cRightBorderStyle+'" ss:Weight="'+AllTrim(Str(::nRightBorderWeight))+'" ss:Color="'+::cRightBorderColor+'"/>'+CRLF
	EndIf
	If ::lLeftBorder
		cRet  +=  '      <Border ss:Position="Left" ss:LineStyle="'+::cLeftBorderStyle+'" ss:Weight="'+AllTrim(Str(::nLeftBorderWeight))+'" ss:Color="'+::cLeftBorderColor+'"/>'+CRLF
	EndIf
    cRet  +=  '   </Borders>'+CRLF
EndIf	
//+---------------------------+
//| Tratamento para a fonte   |
//+---------------------------+
cRet  +=  '   <Font ss:FontName="'+::cFontName+'" ss:Size="'+AllTrim(Str(::nFontSize))+'"'+CRLF
cRet  +=  '         ss:Color="'+::cFontColor+'"'+cBold+cItalic+cStrike+cVAlign+cUnderline+'/>'+CRLF
//+----------------------------------------+
//| Tratamento para o interior da célula   |
//+----------------------------------------+
If !Empty(::cInteriorColor)
	cRet  +=  '   <Interior ss:Color="'+::cInteriorColor+'" ss:Pattern="'+::cInteriorPattern+'"/>'+CRLF
Else
	cRet  +=  '   <Interior/>'+CRLF
EndIf
//+----------------------------------------+
//| Tratamento para o formato da célula    |
//+----------------------------------------+
If !Empty(::cNumberFormat)
	cRet  +=  '   <NumberFormat ss:Format="'+::cNumberFormat+'"/>'+CRLF
EndIf
cRet  +=  '</Style>'+CRLF

Return cRet


/**************************************************************************************************
BEGINDOC

Método:
GetID

Autor:
Tiago Bandeira Brasiliano

Data:
10/03/2009

Descrição:
Retorna o ID (Nome) do Estilo

Parâmetros:
Nenhum

Retorno:
cName      => Nome (ID) do estilo

ENDDOC
**************************************************************************************************/             
METHOD GetID() CLASS CellStyle

Local _ID := ::cID
 
Return _ID
