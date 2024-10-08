#INCLUDE "PROTHEUS.CH"

//+------------------------------------------------------------------+
//| Rotina implementada para permitir a gera��o de patch da classe.  |
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

Descri��o:
Classe utilizada para fornecer diferentes estilos de formata��o para as c�lulas da classe XMLExcel.


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

M�todo:
New

Autor:
Tiago Bandeira Brasiliano

Data:
25/02/2009

Descri��o:
Construtor da classe CellStyle

Par�metros:
cName     => Nome do Estilo que est� sendo criado

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

M�todo:
SetVAlign

Autor:
Tiago Bandeira Brasiliano

Data:
25/02/2009

Descri��o:
Define a posi��o vertical do texto dentro da c�lula.

Par�metros:
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

M�todo:
SetHAlign

Autor:
Tiago Bandeira Brasiliano

Data:
25/02/2009

Descri��o:
Define a posi��o horizontal do texto dentro da c�lula.

Par�metros:
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

M�todo:
SetWrapText

Autor:
Tiago Bandeira Brasiliano

Data:
06/04/2009

Descri��o:
Habilita ou desabilita a quebra autom�tica do texto dentro da c�lula.

Par�metros:
lWrapText   => Boleano que indica se o texto ser� quebrado automaticamente (.T.) ou n�o (.F).
               O default para o objeto � falso (n�o quebra automaticamente).
               Aten��o: A altura da linha deve ser ajustada manualmente.

Retorno:
Nenhum

ENDDOC
**************************************************************************************************/
METHOD SetWrapText(lWrapText) CLASS CellStyle

::lWrapText := lWrapText

Return Nil


/**************************************************************************************************
BEGINDOC

M�todo:
SetRotate

Autor:
Tiago Bandeira Brasiliano

Data:
06/04/2009

Descri��o:
Define rota��o para o texto da c�lula.

Par�metros:
nRotate     => Valor em graus para a rota��o da c�lula.
               Este valor deve ser um inteiro entre -90 at� 90.
               Valores com decimais ser�o truncados.

Retorno:
Nenhum

ENDDOC
**************************************************************************************************/
METHOD SetRotate(nRotate) CLASS CellStyle

::nRotate := NoRound(nRotate,0)

Return Nil


/**************************************************************************************************
BEGINDOC

M�todo:
SetBorder

Autor:
Tiago Bandeira Brasiliano

Data:
10/03/2009

Descri��o:
Define a posi��o, o estilo e a largura de uma borda na c�lula.

Par�metros:
cBorderPos  => Posi��o da Borda que est� sendo adicionada:
               "Botton"  => Borda Inferior
               "Left"    => Borda Esquerda
               "Right"   => Borda Direita
               "Top"     => Borda Superior
               "All"     => Todas as bordas
cStyle      => Estilo da borda que est� sendo adicionada:
               "Continuous" => Borda cont�nua (default)
               "Dot"        => Pontilhada
               "DashDotDot" => Pontilhada com tra�o
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

M�todo:
RemoveBorder

Autor:
Tiago Bandeira Brasiliano

Data:
10/03/2009

Descri��o:
Remove uma borda de uma c�lula

Par�metros:
cBorderPos  => Posi��o da Borda que est� sendo Removida:
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

M�todo:
SetFont

Autor:
Tiago Bandeira Brasiliano

Data:
10/03/2009

Descri��o:
Define a fonte de uma c�lula.

Par�metros:
cName          => Nome da Fonte que ser� atribu�da para a c�lula. Ex:
                  "Botton"  => Borda Inferior
                  "Left"    => Borda Esquerda
                  "Right"   => Borda Direita
                  "Upper"   => Borda Superior
nSize          => Tamanho da Fonte
cColor         => C�digo RGB Hexidecimal da cor da fonte. Ex.:
                  "#000000" => Preto (Default)
                  "#FFFFFF" => Branco
                  "#FF0000" => Vermelho
                  "#00FF00" => Verde
                  "#0000FF" => Azul
lBold          => Negrito (.T. / .F.)
lItalic        => It�lico (.T. / .F.)
lUnderline     => Sublinhado (se omitido, n�o ser� utilizado):
                  "Single" => Simples
                  "Double" => Duplo
cVAlign        => Alinhamento vertical da fonte (se omitido, ser� normal):
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

M�todo:
SetInterior

Autor:
Tiago Bandeira Brasiliano

Data:
10/03/2009

Descri��o:
Define o padr�o do interior de uma c�lula.

Par�metros:
cColor         => C�digo RGB Hexidecimal da cor da fonte. Ex.:
                  "#000000" => Preto (Default)
                  "#FFFFFF" => Branco
                  "#FF0000" => Vermelho
                  "#00FF00" => Verde
                  "#0000FF" => Azul
cPattern       => Estilo Padr�o do Preenchimento. Ex:
                  "Solid"          => Preenchimento s�lido.
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

M�todo:
SetNumberFormat

Autor:
Tiago Bandeira Brasiliano

Data:
10/03/2009

Descri��o:
Define o formato num�rico para a c�lula.

Par�metros:
cFormat        => Define o formato da c�lula. Exemplos:
               "h:mm;@"     => Formato para hora.
               "Fixed"      => Num�rico com duas casas decimais.
               "0.000"      => Num�rico com tr�s casas decimais.
               "Percent"    => Percentual
               "Short Date" => Data em formato curto.
               Observa��o: Quando um formato do tipo data for utilizado, o conte�do da c�lula
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

M�todo:
GetXML

Autor:
Tiago Bandeira Brasiliano

Data:
10/03/2009

Descri��o:
Retorna a string contendo o c�digo XML do Estilo.

Par�metros:
Nenhum

Retorno:
cRet        => String contendo o c�digo XML do estilo da c�lula criada.

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
//| Tratamento para rota��o do texto           |
//+--------------------------------------------+
If ::nRotate <> 0
	cRotate := ' ss:Rotate="'+AllTrim(Str(::nRotate))+'"' 
EndIf

//+--------------------------------------------+
//| Tratamento para quebra autom�tica do texto |
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
//| Tratamento para o interior da c�lula   |
//+----------------------------------------+
If !Empty(::cInteriorColor)
	cRet  +=  '   <Interior ss:Color="'+::cInteriorColor+'" ss:Pattern="'+::cInteriorPattern+'"/>'+CRLF
Else
	cRet  +=  '   <Interior/>'+CRLF
EndIf
//+----------------------------------------+
//| Tratamento para o formato da c�lula    |
//+----------------------------------------+
If !Empty(::cNumberFormat)
	cRet  +=  '   <NumberFormat ss:Format="'+::cNumberFormat+'"/>'+CRLF
EndIf
cRet  +=  '</Style>'+CRLF

Return cRet


/**************************************************************************************************
BEGINDOC

M�todo:
GetID

Autor:
Tiago Bandeira Brasiliano

Data:
10/03/2009

Descri��o:
Retorna o ID (Nome) do Estilo

Par�metros:
Nenhum

Retorno:
cName      => Nome (ID) do estilo

ENDDOC
**************************************************************************************************/             
METHOD GetID() CLASS CellStyle

Local _ID := ::cID
 
Return _ID
