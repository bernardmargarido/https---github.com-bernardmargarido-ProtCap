#INCLUDE "PROTHEUS.CH"
#INCLUDE "MSOBJECT.CH"
//-------------------------------------------------------------------
/*/{Protheus.doc} uArqTxt
Funcao Generica para Compilacao

@author		Guilherme Santos
@since		07/06/2018
@version	12.1.17
/*/
//-------------------------------------------------------------------
User Function uArqTxt()
Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} uArqTxt
Classe para Manipulacao de Arquivos Texto

@author		Guilherme Santos
@since		07/06/2018
@version	12.1.17
/*/
//-------------------------------------------------------------------
Class uArqTxt
	Data aDados										//Array com os Dados do Arquivo Texto
	Data cArquivo									//cPath + cNomeArq
	Data cNomeArq									//Nome do arquivo
	Data cPath										//Caminho do arquivo
	Data nHandle									//Handle do arquivo
	
	Method New(cPath, cNomeArq)
	Method Open(nModo)
	Method Close()
	Method Write(cTexto,lAddEnter)
	Method Use()
	Method Free()
	Method GoTop()
	Method GoBottom()
	Method Skip()
	Method Bof()
	Method Eof()
	Method ReadLn()
	Method Read()
	Method ReadStr()
	Method Create()
	Method Exists()
	Method Erase()
	Method Rename(cNewName)
	Method GetName()
	Method Move(cDestino)
	Method SplitName()
EndClass
//-------------------------------------------------------------------
/*/{Protheus.doc} New
Construtor da Classe uArqTxt

@author		Guilherme Santos
@since		07/06/2018
@version	12.1.17
/*/
//-------------------------------------------------------------------
Method New(cPath, cNomeArq, cFullName) Class uArqTxt
	Default cNomeArq 	:= ""
	Default cPath 		:= ""
	Default cFullName	:= ""
	
	::aDados	:= {}

	If !Empty(cFullName)
		::cArquivo	:= cFullName
		::cNomeArq	:= ""
		::cPath		:= ""

		::SplitName()
	Else
		::cNomeArq 	:= cNomeArq
		::cPath		:= cPath

		//Verifica se o path esta vindo com barra no final
		If Substr(::cPath, Len(::cPath), 1) == "\"
			::cArquivo	:= ::cPath + ::cNomeArq	
		Else
			::cArquivo	:= ::cPath + "\" + ::cNomeArq
		EndIf
	EndIf
	
	::nHandle  	:= 0

Return Self
//-------------------------------------------------------------------
/*/{Protheus.doc} Open
Abre o Arquivo Texto

@author		Guilherme Santos
@since		07/06/2018
@version	12.1.17
/*/
//-------------------------------------------------------------------
Method Open(nModo) Class uArqTxt
	Local lRetorno	:= .T.					//Variavel de retorno do metodo
	Default nModo	:= 0

	//Abre o arquivo
	::nHandle := FOPEN(::cArquivo, nModo)

	If ::nHandle == -1
		lRetorno := .F.	
	EndIf 
Return lRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} Close
Fecha o Arquivo Texto

@author		Guilherme Santos
@since		07/06/2018
@version	12.1.17
/*/
//-------------------------------------------------------------------
Method Close() Class uArqTxt
	Local lRetorno := .F.					//Variavel de retorno do metodo
	
	//Fecha o arquivo
	lRetorno := FCLOSE(::nHandle)
Return lRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} Write
Insere uma Linha no Arquivo com o Conteudo do Parametro

@author		Guilherme Santos
@since		07/06/2018
@version	12.1.17
/*/
//-------------------------------------------------------------------
Method Write(cTexto,lAddEnter) Class uArqTxt
	Local lRetorno 	:= .T.						//Variavel de retorno do metodo
	Local nTamanho 	:= 0						//Tamanho do texto a ser escrito
	Local nRetorno 	:= 0						//Variavel de retorno do metodo FWRITE
	Local nSize		:= 0						//Tamanho de bytes do arquivo
	
	Default lAddEnter := .T.
	
	If lAddEnter
		cTexto += CRLF
	EndIf
	
	//Posiciona no final do arquivo
	nSize := FSEEK(::nHandle, 0, 2)
	
	//Escreve no Arquivo
	nRetorno := FWRITE(::nHandle, cTexto)
	
	//Verifica se o comando foi executado
	nTamanho := Len(cTexto)
	
	If nRetorno != nTamanho
		lRetorno := .F.
	EndIf

Return lRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} Create
Cria o Arquivo Texto

@author		Guilherme Santos
@since		07/06/2018
@version	12.1.17
/*/
//-------------------------------------------------------------------
Method Create() Class uArqTxt
	Local lRetorno 	:= .T.						//Variavel de retorno do metodo

	//Cria o arquivo
	::nHandle := FCREATE(::cArquivo)

	If ::nHandle == -1
		lRetorno := .F.	
	EndIf 
Return lRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} Exists
Verifica se o Arquivo Texto Existe

@author		Guilherme Santos
@since		07/06/2018
@version	12.1.17
/*/
//-------------------------------------------------------------------
Method Exists() Class uArqTxt
	Local lRetorno 	:= .F.						//Variavel de retorno do metodo
	
	//Verifica se o arquivo existe
	lRetorno := File(::cArquivo)
			
Return lRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} Erase
Apaga o Arquivo Texto

@author		Guilherme Santos
@since		07/06/2018
@version	12.1.17
/*/
//-------------------------------------------------------------------
Method Erase() Class uArqTxt
	Local lRetorno 	:= .T.						//Variavel de retorno do metodo
	Local nRetorno	:= 0						//Retorno da funcao FERASE
	
	//Apaga o arquivo
	nRetorno := FERASE(::cArquivo)
	
	If nRetorno == -1
		lRetorno := .F.	
	EndIf 
Return lRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} Use
Informa ao Sistema Operacional que o Arquivo esta em Uso

@author		Guilherme Santos
@since		07/06/2018
@version	12.1.17
/*/
//-------------------------------------------------------------------
Method Use() Class uArqTxt
	Local lRetorno := FT_FUSE(::cArquivo) <> -1
Return lRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} Free
Libera o arquivo para o Sistema Operacional

@author		Guilherme Santos
@since		07/06/2018
@version	12.1.17
/*/
//-------------------------------------------------------------------
Method Free() Class uArqTxt
	FT_FUSE()
Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} GoTop
Posiciona no Inicio do Arquivo
Move o Ponteiro para o Inicio do Arquivo

@author		Guilherme Santos
@since		07/06/2018
@version	12.1.17
/*/
//-------------------------------------------------------------------
Method GoTop() Class uArqTxt
	FT_FGOTOP()
Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} GoBottom
Posiciona n
Move o Ponteiro para o Final do Arquivo

@author		Guilherme Santos
@since		07/06/2018
@version	12.1.17
/*/
//-------------------------------------------------------------------
Method GoBottom() Class uArqTxt
	FSEEK(::nHandle, 0, 2)
Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} Bof
Retorna se esta no Inicio do Arquivo

@author		Guilherme Santos
@since		07/06/2018
@version	12.1.17
/*/
//-------------------------------------------------------------------
Method Bof() Class uArqTxt
	Local lRetorno := FT_FBOF()
Return lRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} Eof
Retorna se esta no Final do Arquivo

@author		Guilherme Santos
@since		07/06/2018
@version	12.1.17
/*/
//-------------------------------------------------------------------
Method Eof() Class uArqTxt
	Local lRetorno := FT_FEOF()
Return lRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} Skip
Move o Ponteiro para a Proxima Linha do Arquivo

@author		Guilherme Santos
@since		07/06/2018
@version	12.1.17
/*/
//-------------------------------------------------------------------
Method Skip() Class uArqTxt
   FT_FSKIP()
Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} ReadLn
Retorna a Linha Atual do Arquivo

@author		Guilherme Santos
@since		07/06/2018
@version	12.1.17
/*/
//-------------------------------------------------------------------
Method ReadLn() Class uArqTxt
	Local cLinha	:= FT_FREADLN()
Return cLinha
//-------------------------------------------------------------------
/*/{Protheus.doc} Read
Le o Conteudo do Arquivo Texto e Retorna em um Array

@author		Guilherme Santos
@since		07/06/2018
@version	12.1.17
/*/
//-------------------------------------------------------------------
Method Read() Class uArqTxt
	Local cLinha	:= ""				//Retorno da leitura do arquivo
	
	::aDados := {}

	//Coloca o arquivo em uso
	::Use()

	//Posiciona na primeira linha
	::GoTop()
 
	//Varre o arquivo
	While !::Eof()
	   
		//Adiciona a Linha ao Array de Retorno
		Aadd(::aDados, {::ReadLn()})

		//Pula a linha
		::Skip()
	End 
    
	//Libera o arquivo
	::Free()
			
Return ::aDados
//-------------------------------------------------------------------
/*/{Protheus.doc} ReadStr
Le o Conteudo do Arquivo Texto e Retorna em uma String

@author		Wilson A. Silva Jr.
@since		03/10/2019
@version	12.1.17
/*/
//-------------------------------------------------------------------
Method ReadStr() Class uArqTxt
	Local cArqStr	:= ""				//Retorno da leitura do arquivo
	
	//Coloca o arquivo em uso
	::Use()

	//Posiciona na primeira linha
	::GoTop()
 
	//Varre o arquivo
	While !::Eof()
	   
		//Adiciona a Linha ao Array de Retorno
		cArqStr += ::ReadLn() + CRLF

		//Pula a linha
		::Skip()
	End 
    
	//Libera o arquivo
	::Free()
			
Return cArqStr
//-------------------------------------------------------------------
/*/{Protheus.doc} Rename
Altera o Nome do Arquivo Texto

@author		Guilherme Santos
@since		07/06/2018
@version	12.1.17
/*/
//-------------------------------------------------------------------
Method Rename(cNewName) Class uArqTxt

	Local lRetorno := .F. 				//Retorno do metodo
	
	//Renomeia o arquivo
	lRetorno := (FRENAME(::cPath + ::cNomeArq , ::cPath + cNewName) == 0)

	If lRetorno
		::cNomeArq := cNewName
	EndIf

Return lRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} GetName
Retorna o Nome do Arquivo Texto

@author		Guilherme Santos
@since		07/06/2018
@version	12.1.17
/*/
//-------------------------------------------------------------------
Method GetName() Class uArqTxt
Return ::cNomeArq
//-------------------------------------------------------------------
/*/{Protheus.doc} GetName
Move o Arquivo Texto para o Caminho informado

@author		Guilherme Santos
@since		07/06/2018
@version	12.1.17
/*/
//-------------------------------------------------------------------
Method Move(cDestino) Class uArqTxt
	Local lRetorno := .F.

	If __CopyFile(::cArquivo, cDestino + "\" + ::cNomeArq)
		::Free()
		If ::Erase()
			lRetorno := .T.
			::cArquivo 	:= cDestino + "\" + ::cNomeArq
			::cPath		:= cDestino + "\"
		EndIf
	EndIf

Return lRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} SplitName
Separa o Caminho do Nome do Arquivo Texto

@author  Guilherme Santos
@since   13/05/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
Method SplitName() Class uArqTxt
	Local lName		:= .T.
	Local nI		:= 0

	For nI := Len(::cArquivo) to 1 Step -1

		If lName .AND. Substr(::cArquivo, nI, 1) == "\"
			lName := .F.
		EndIf

		If lName
			::cNomeArq := Substr(::cArquivo, nI, 1) + ::cNomeArq
		Else
			::cPath := Substr(::cArquivo, nI, 1) + ::cPath
		EndIf
	Next nI

Return NIL
