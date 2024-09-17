#include "protheus.ch"
#include "shell.ch"
#include "fileio.ch"

Static cTmpLocal  := ""
Static lSmartHTML := (GetRemoteType() == 5)
Static lJob       := (GetRemoteType() == -1)

// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Geral
// FunÁ„o   : Rateio
// DescriÁ„o: Efetua o rateio de um certo valor em uma matriz.
// Param.   : nRateio - quantidade a ser rateada.
//            aPesos  - matriz base a ser considerada para o rateio.
//                      Essa matriz devera conter os fatores para receber o rateio.
//            bPeso   - Execblock para tratamento dos pesos, caso necess·rio.
//            nPrecis - precis„o do valor rateado (n˙mero de casas decimais).
// Retorno  : aRet    - matriz, com o mesmo tamanho que a matriz base, com a
//                      quantidade rateada de cada item, respectivamente.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 19/10/04 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function Rateio(nRateio, aPesos, bPeso, nPrecis)
// Declaracao de variaveis.
Local aRet       := {}
Local nSaldo     := 0
Local nBase      := 0
Local nFator     := 0
Local nRateado   := 0
Local aResto     := {}
Local nX

// Acerta os valores padroes dos parametros.
Default aPesos  := {}
Default bPeso   := {|x| x}
Default nPrecis := 0

// Matriz que sera retornada.
aRet := array(len(aPesos))

// Verifica se ha valor a ser rateado.
If nRateio <> 0
	// Parametros para calculo da matriz de retorno.
	aResto := array(len(aPesos))
	nBase  := 0; aEval(aPesos, {|x| nBase += eval(bPeso, x)})
	nFator := 10 ^ nPrecis

	// Monta matriz de pesos.
	If nBase == 0
		aFill(aPesos, 1)
		nBase := len(aPesos)
	Endif

	// Efetua o rateio.
	nSaldo := nRateio
	For nX := 1 to len(aPesos)
		nPeso      := eval(bPeso, aPesos[nX])
		nRateado   := (nRateio / nBase) * nPeso
		aRet[nX]   := int(nRateado * nFator) / nFator  // Arredonda para baixo a quantidade de casas decimais.
		aResto[nX] := {nRateado - aRet[nX], nX}
		nSaldo -= aRet[nX]
	Next nX

	// Rateia o saldo restante, se houver.
	If nSaldo <> 0
		// Define a ordem dos que receberao o rateio primeiro.
		aSort(aResto,,, {|x, y| x[1] > y[1] .or. (x[1] = y[1] .and. x[2] < y[2])})

		// Faz o rateio do saldo restante.
		For nX := 1 to (nSaldo * nFator)
			aRet[aResto[nX, 2]] += (1 / nFator)
			nSaldo -= (1 / nFator)
		Next nX
	Endif
Else
	aFill(aRet, 0)
Endif

// Retorna os valores rateados.
Return(aRet)


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Geral
// FunÁ„o   : CtoA
// DescriÁ„o: Transforma texto, formado por itens separados por algum delimitador
//            qualquer, em uma matriz.
//            O limitador de palavras podera ser definido pelo usu·rio e passado
//            por par‚metro. O padr„o È uma vÌrgula ou ponto-vÌrgula.
// Param.   : cTexto    - texto que sera convertido em matriz.
//            cDelim    - caracter delimitador de campos. Se n„o passado, o
//                        sistema assumira vÌrgula ou ponto-vÌrgula.
//            lRetAspas - informe ao sistema se dever· retirar as aspas do
//                        caractere. Se n„o for para retirar, o sistema
//                        retornar· os item com as aspas. Padr„o: .T.
//            cAspas    - caracter usado como qualificador de textos.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 17/09/03 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function CtoA(cTexto, cDelim, lRetAspas, cAspas)
// Declaracao de variaveis.
Local aRet       := {}
Local cChar      := ""
Local cLinha     := ""
Local cAbreCpo   := ""
Local lDelim     := .F.
Local nTamTxt    := 0
Local nChar      := 0

// Verifica os parametros passados.
Default cTexto    := ""
Default cDelim    := ";,"
Default lRetAspas := .F.
Default cAspas    := "'" + '"'

// Varre todo o texto passado por parametro, um caracter por vez.
nTamTxt := len(cTexto)
For nChar := 1 to nTamTxt

	// Pega o caracter da posicao.
	cChar := SubStr(cTexto, nChar, 1)

	// Testa se eh um caracter delimitador.
	// Nao considera delimitadores dentro do texto (entre aspas).
	lDelim := (cChar $ cDelim) .and. (empty(cAbreCpo) .or. cAbreCpo == right(RTrim(cLinha), 1))

	// Se nao for um caracter delimitador, inclui no campo.
	If !lDelim
		// Se uma aspa for o primeiro caracter do campo...
		If empty(cLinha) .and. cChar $ cAspas
			// ... armazena essa aspa em uma variavel auxiliar.
			cAbreCpo := cChar
		Endif

		// Adiciona o caractere da posicao.
		cLinha += cChar
	Endif

	// Testa se eh fim da linha ou fim do texto.
	If lDelim .or. (nChar == nTamTxt)
		// Verifica se abriu e fechou com aspas.
		If lRetAspas .and. !empty(cAbreCpo) .and. right(RTrim(cLinha), 1) == cAbreCpo
			cLinha := AllTrim(cLinha)
			cLinha := right(cLinha, len(cLinha) - 1)  // Retira a aspa do inicio...
			cLinha := left(cLinha, len(cLinha) - 1)   // ... e do final.
		Endif

		// Adiciona o campo corrente na matriz que sera retornada.
		aAdd(aRet, cLinha)

		// Zera as variaveis auxiliares.
		cLinha   := ""
		cAbreCpo := ""
	Endif
Next nChar

Return(aRet)


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Materiais
// FunÁ„o   : Numero
// DescriÁ„o: Converte um texto de n˙meros com m·scara em apenas n˙meros.
// Retorno  : Texto com os n˙meros sem a m·scara.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 28/04/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function Numeros(cNumero)
Local cRet       := ""
Local cChar      := ""
Local nX

Default cNumero := ""
For nX := 1 to len(cNumero)
	cChar := SubStr(cNumero, nX, 1)
	If IsDigit(cChar)
		cRet += cChar
	Endif
Next nX

Return cRet
// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Leo Kume
// Modulo   : Faturamento / Call Center
// FunÁ„o   : AtuCamp
// DescriÁ„o: Executa validaÁıes e os gatilhos do campo.
// Retorno  :
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 19/07/11 | Leo Kume          | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function AtuCamp(cCampo, xValor)
Local lRet     := .T.
Local aArea    := GetArea()
Local aAreaSX3 := SX3->(GetArea())
Local aAreaSX7 := SX7->(GetArea())
Local cValid   := ""
Local cVldUser := ""
Local nLinha   := If(Type("N") == "N", n, 0)

//TODO: Verificar se o SX3 pode ser posicionado
cCampo := PadR(cCampo, 10)
SX3->(dbSetOrder(2))  // X3_CAMPO.
If SX3->(dbSeek(cCampo, .F.))

	// Cria campo de memoria, simulando digitacao diretamente na aCols.
	Private __ReadVar  := "M->" + cCampo
	Private &__ReadVar := xValor

	// Executa validacoes do campo.
	cValid   := GetSx3Cache(cCampo, "X3_VALID")
	cVldUser := GetSx3Cache(cCampo, "X3_VLDUSER")

	If (empty(cVldUser) .or. &(cVldUser)) .and. (empty(cValid) .or. &(cValid))

		// Verifica se o campo esta na GetDados, e atualiza a aCols.
		If (Type("aCols") == "A" .and. nLinha > 0 .and. GdFieldPos(cCampo) > 0)
			n := nLinha  // Corrige vari·vel, caso as validaÁıes tenham desposicionado a linha.
			GdFieldPut(cCampo, xValor)
			//TODO: Verificar se a Funcao RunTrigger continuara funcionando
			RunTrigger(2, n)  // Executa gatilhos.
			n := nLinha  // Corrige vari·vel, caso os gatilhos tenham desposicionado a linha.
		Else
			//TODO: Verificar se a Funcao RunTrigger continuara funcionando
			RunTrigger(1)  // Executa gatilhos.
		Endif
	Else
		lRet := .F.
	Endif
Endif

// Restaura areas de trabalho.
RestArea(aAreaSX7)
RestArea(aAreaSX3)
RestArea(aArea)

Return lRet


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// FunÁ„o   : Trigger
// DescriÁ„o: Executa os gatilhos do campo.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 19/07/11 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function Trigger(cCampo)
Local aArea    := GetArea()
Default cCampo := StrTran(ReadVar(), "M->", "")

// Verifica se o campo esta na GetDados, e atualiza a aCols.
If ExistTrigger(cCampo)
	If (Type("aCols") == "A" .and. Type("N") == "N" .and. GdFieldPos(cCampo) > 0)
		GdFieldPut(cCampo, xValor)
		RunTrigger(2, n)
	Else
		RunTrigger(1,,,,cCampo)
	Endif
EndIf

RestArea(aArea)

Return


/*
‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±…ÕÕÕÕÕÕÕÕÕÕ—ÕÕÕÕÕÕÕÕÕÕÀÕÕÕÕÕÕÕ—ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÀÕÕÕÕÕÕ—ÕÕÕÕÕÕÕÕÕÕÕÕÕª±±
±±∫Programa  ≥ VerLog   ∫Autor  ≥ Felipe Raposo      ∫ Data ≥  19/07/11   ∫±±
±±ÃÕÕÕÕÕÕÕÕÕÕÿÕÕÕÕÕÕÕÕÕÕ ÕÕÕÕÕÕÕœÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ ÕÕÕÕÕÕœÕÕÕÕÕÕÕÕÕÕÕÕÕπ±±
±±∫Desc.     ≥ Funcao para exibir o arquivo de log ao usuario.            ∫±±
±±∫          ≥                                                            ∫±±
±±ÃÕÕÕÕÕÕÕÕÕÕÿÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕπ±±
±±∫Uso       ≥ Generico.                                                  ∫±±
±±»ÕÕÕÕÕÕÕÕÕÕœÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ
*/
User Function VerLog(cFileLog, cTitle)
Local oDlg, oFont, oMemo
Local cMsgLog := ""
Default cTitle := cFileLog

// Exibe log na tela.
cMsgLog := MemoRead(cFileLog)
If !empty(cMsgLog)
	DEFINE FONT oFont NAME "Courier New" SIZE 5, 0
	DEFINE MSDIALOG oDlg TITLE cTitle From 3, 0 to 340, 417 PIXEL

	@ 5,5 GET oMemo VAR cMsgLog MEMO SIZE 200,145 OF oDlg PIXEL
	oMemo:bRClicked := {|| AllwaysTrue()}
	oMemo:oFont := oFont

	DEFINE SBUTTON  FROM 153,175 TYPE 01 ACTION oDlg:End() ENABLE OF oDlg PIXEL
	ACTIVATE MSDIALOG oDlg CENTER
Endif

Return


// ##############################################################################
// Projeto  : PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento
// FunÁ„o   : UserMail
// DescriÁ„o: Busca o email do usuario logado no sistema.
// Retorno  : aRet - matriz com os dados do usuario:
//                1 - string: nome do usuario <email>
//                2 - matriz: {nome do usuario; email}
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 19/07/11 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function UserMail(cUsrName)
// Declaracao de variaveis.
Local lRet   := .F.
Local aRet   := {"", {"", ""}}
Local cNome  := ""
Local cEmail := ""

// Pesquisa o usu·rio.
If ValType(cUsrName) == "C"
	PswOrder(2)  // Nome do usu·rio/grupo.
	lRet := PswSeek(rtrim(cUsrName))
Else
	PswOrder(1)  // ID do usu·rio/grupo.
	lRet := PswSeek(__CUSERID)
Endif

// Se encontrou o usu·rio, retornar seu nome e email.
If lRet
	aRetUser := PswRet(1)
	If !empty(aRetUser)
		cNome  := Capital(AllTrim(aRetUser[1, 4]))
		cEmail := lower(AllTrim(aRetUser[1, 14]))
		aRet[1] := cNome + " <" + cEmail + ">"
		aRet[2] := {cNome, cEmail}
	Endif
Endif

Return aRet


// ##############################################################################
// Projeto  : PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento
// FunÁ„o   : EnvEMail
// DescriÁ„o: Rotina de envio de e-mail.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 04/09/13 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function EnvEMail(cFrom, cTo, cSubject, cBody, aFiles, lSelfBCC, cCC, cBCC, lForceTest, lThread, cMsgErro)
Local nRet       := 0
Local aEnvir     := {}
Local aParMail   := {}

Default lThread := .F.
Default cMsgErro := ""
//cSubject := EncodeUtf8(cSubject,'cp1252')

If lThread
	aEnvir   := {cEmpAnt, cFilAnt, __cUserID, "FIS"}
	aParMail := {cFrom, cTo, cSubject, cBody, aFiles, lSelfBCC, cCC, cBCC, lForceTest}
	StartJob('U_EnvMailJ', GetEnvServer(), .F., {aEnvir, aParMail})
Else
	nRet := EnvEMail(cFrom, cTo, cSubject, cBody, aFiles, lSelfBCC, cCC, cBCC, lForceTest, cMsgErro)
Endif

Return nRet

User Function EnvMailJ(aParam)
Local aEnvir     := aParam[1]
Local aParMail   := aParam[2]

// Vari·veis de ambiente.
Local cRPCEmp    := aEnvir[1]
Local cRPCFil    := aEnvir[2]
Local cRPCUser   := aEnvir[3]
Local cRPCMod    := aEnvir[4]

// Prepara o ambiente.
RPCSetType(3)  // N„o consome licenÁa.
RPCSetEnv(cRPCEmp, cRPCFil, nil, nil, cRPCMod)

// Abre como se fosse o mesmo usu·rio.
__cUserId := cRPCUser
PswOrder(1)  // Por ID.
PswSeek(__cUserID)

// Envia o e-mail.
EnvEMail(aParMail[1], aParMail[2], aParMail[3], aParMail[4], aParMail[5], aParMail[6], aParMail[7], aParMail[8], aParMail[9])

Return

Static Function EnvEMail(cFrom, cTo, cSubject, cBody, aFiles, lSelfBCC, cCC, cBCC, lForceTest, cMsgErro)
Local nRet        := -1
Local aUsrMail    := {}
Local cUsrMail    := ""
Local lMsProcTxt  := (Type("oText") == "O")
Local cBCCAux     := ""
Local aBCC        := {}
Local cToAlt      := ""

Local aFilesSrv   := {}
Local cFileName   := ""
Local cFileExt    := ""
Local oServer
Local cMailServer := ""
Local nMailPort   := 0
Local lSmtpAuth   := .F.
Local cMailConta  := ""
Local cMailSenha  := ""
Local nTimeOut    := 0
Local cHeaderAlt  := ""
Local nX

Static oMessage
Static cTmpServ   := "\mailspool\"

// ConfiguraÁ„o do servidor SMTP.
aUsrMail    := U_UserMail()  // Busca o e-mail do usu·rio logado.
cUsrMail    := If(empty(aUsrMail[2, 2]), "", aUsrMail[1])
DEFAULT cFrom := If(__cUserId = "000000", IIF(cFilAnt $ "02*15*16*19","erp@casadoepi.com.br","erp@protcap.com.br"), cUsrMail)
cFrom := AllTrim(cFrom)

//Tratamento para JOB OnStart sem usu·rio
If Empty(cFrom) .and. IsBlind()
	If cFilAnt $ "02*15*16*19"
		cFrom := "erp@casadoepi.com.br"
	Else
		cFrom := "erp@protcap.com.br"
	EndIf
EndIf
cMailServer := SuperGetMV("MV_RELSERV",, "")
nMailPort   := SuperGetMV("MV_GCPPORT",, 25)
lSmtpAuth   := SuperGetMV("MV_RELAUTH",, .T.)

If lSmtpAuth
	cMailConta := cFrom
	If cMailConta == 'financeiro.info@protcap.com.br'
		cMailSenha  := SuperGetMV("PC_PSWFINI",,"")
		cMailServer := SuperGetMV("PC_XRELCSR",,"")
	ElseIf cMailConta == 'sp.cobranca@protcap.com.br'
		cMailSenha  := SuperGetMV("PC_PSWCOSP",,"")
		cMailServer := SuperGetMV("PC_XRELCSR",,"")
	ElseIf cMailConta == 'mg.cobranca@casadoepi.com.br'
		cMailSenha  := SuperGetMV("PC_PSWCOMG",,"")
		cMailServer := SuperGetMV("PC_XRELCSR",,"")
	ElseIf cMailConta == 'nf@bepimais.com.br'
		nMailPort   := SuperGetMV("BM_PORTAEM",, 587)
		cMailSenha  := SuperGetMV("BM_PSWEMAI",,"")
		cMailServer := SuperGetMV("BM_SRVEMAI",,"")
		cMailConta  := SuperGetMV("BM_USREMAI ",, "")
	Else
		if ("@protcap.com.br" $ cFrom)
			cMailServer := SuperGetMV("MV_RELSERV",, "","00")
			nMailPort   := SuperGetMV("MV_GCPPORT",, 25,"00")
			cMailConta  := SuperGetMV("MV_RELACNT",, "","00")
			cMailSenha  := SuperGetMV("MV_RELPSW" ,, "","00")
		elseif ("@casadoepi.com.br" $ cFrom)
			cMailServer := SuperGetMV("MV_RELSERV",, "","02")
			nMailPort   := SuperGetMV("MV_GCPPORT",, 25,"02")
			cMailConta  := SuperGetMV("MV_RELACNT",, "","02")
			cMailSenha  := SuperGetMV("MV_RELPSW" ,, "","02")
		else
			cMailConta  := SuperGetMV("MV_RELACNT",, "")
			cMailSenha  := SuperGetMV("MV_RELPSW" ,, "")
		endif
	EndIf
Endif

Default cFrom      := If(__cUserId = "000000", IIF(cFilAnt $ "02*15*16*19","erp@casadoepi.com.br","erp@protcap.com.br"), cUsrMail)
Default aFiles     := {}
Default lSelfBCC   := .F.
Default cCC        := ""
Default cBCC       := ""
Default lForceTest := .F.
Default cMsgErro   := ""
cFrom := AllTrim(cFrom)

//Tratamento para JOB OnStart sem usu·rio
If Empty(cFrom) .and. IsBlind()
	If cFilAnt $ "02*15*16*19"
		cFrom := "erp@casadoepi.com.br"
	Else
		cFrom := "erp@protcap.com.br"
	EndIf
EndIf

If empty(cFrom)
	cMsgErro := "Usu·rio " + cUserName + " sem e-mail configurado. Falar com o administrador do sistema."
	Help(,, 'Help',, cMsgErro, 1, 0)
Else

	// Limpa vari·veis antes de enviar o e-mail.
	cTo      := AllTrim(cTo)
	cSubject := AllTrim(cSubject)
	cBody    := AllTrim(cBody)
	cCC      := AllTrim(cCC)
	cBCC     := AllTrim(cBCC)

	// Se for base de testes, enviar e-mail para o prÛprio operador.
	If !U_Producao()
		If lForceTest .or. empty(cTo)
			cSubject += " (base teste)"
		Else
			If cFrom == aUsrMail[1]
				cToAlt := cFrom
			Else
				cToAlt := "sistemas@protcap.com.br"
			Endif

			cHeaderAlt := "Para: " + cTo + CRLF
			If !empty(cCC)
				cHeaderAlt += "Cc:   " + cCC + CRLF
			Endif
			If !empty(cBCC)
				cHeaderAlt += "Cco:  " + cBCC + CRLF
			Endif

			cSubject += " (base teste - e-mail n„o enviado para destinat·rios)"
			cBody    := U_Txt2HTML(cHeaderAlt + CRLF) + cBody

			cTo  := cToAlt
			cCC  := ""
			cBCC := ""
		Endif
	Endif

	// Conecta ao servidor SMTP.
	If lMsProcTxt
		msProcTxt("Conectando ao servidor " + cMailServer + "..."); ProcessMessages()
	Endif
	oServer := TMailManager():New()
	oServer:SetUseTLS(.T.)
	oServer:Init("", cMailServer, cMailConta, cMailSenha,0, nMailPort)
	oServer:SetSmtpTimeOut(nTimeOut)
	nRet := oServer:SmtpConnect()

	If nRet <> 0
		cMsgErro := "Erro " + ltrim(str(nRet)) + " - falha ao conectar ao servidor [" + rtrim(cMailServer) + "]."
		Help(,, 'Help',, cMsgErro, 1, 0)
	Else
		oServer:SMTPAuth( cMailConta, cMailSenha )
		If lMsProcTxt
			msProcTxt("Enviando e-mail para " + AllTrim(cTo) + "..."); ProcessMessages()
		Endif

		// Cria objeto para envio de e-mails, e o mantÈm em memÛria para futuras chamadas.
		If ValType(oMessage) <> "O"
			oMessage   := TMailMessage():New()
		Endif

		// E-mails de cÛpia oculta.
		If lSelfBCC
			If !empty(cUsrMail)
				aAdd(aBCC, cUsrMail)
			Endif
			If !empty(cFrom) .and. aScan(aBCC, cFrom) == 0
				aAdd(aBCC, cFrom)
			Endif
		Endif
		If !empty(cBCC) .and. aScan(aBCC, cBCC) == 0
			aAdd(aBCC, cBCC)
		Endif

		// Monta e-mail.
		oMessage:Clear()
		oMessage:cFrom    := cFrom
		oMessage:cTo      := cTo
		If !empty(cCC)
			oMessage:cCC  := cCC
		Endif
		If !empty(aBCC)
			cBCCAux := ""
			For nX := 1 to len(aBCC)
				cBCCAux += AllTrim(aBCC[nX]) + "; "
			Next nX
			oMessage:cBCC := left(cBCCAux, len(cBCCAux) - 2)
		Endif
		oMessage:cSubject := cSubject
		oMessage:cBody    := cBody
		oMessage:MsgBodyType("text/html")

		// Anexa arquivos.
		For nX := 1 to len(aFiles)
			SplitPath(aFiles[nX], nil, nil, @cFileName, @cFileExt)
			cFileExt := rtrim(cFileExt)
			__CopyFile(aFiles[nX], cTmpServ + cFileName + cFileExt)
			If file(cTmpServ + cFileName + cFileExt)
				// Guarda o nome do arquivo a ser excluÌdo posteriormente.
				aAdd(aFilesSrv, cTmpServ + cFileName + cFileExt)

				// Anexa arquivo ao e-mail.
				nRet := oMessage:AttachFilePath(cTmpServ + cFileName + cFileExt)
				If nRet < 0
					cMsgErro := "Erro " + ltrim(str(nRet)) + " - erro ao anexar arquivo " + cFileName + cFileExt + "."
					Help(,, 'Help',, cMsgErro, 1, 0)
				Else
					nRet := 0

					// Adiciona uma tag informando a ID do arquivo.
					oMessage:AddAttHTag('Content-ID: <ID_' + cFileName + cFileExt + '>')
					oMessage:cBody := StrTran(oMessage:cBody, 'src="' + cFileName + cFileExt + '"', 'src="cid:ID_' + cFileName + cFileExt + '"')
				Endif
			Else
				cMsgErro := "Erro ao copiar arquivo " + cFileName + cFileExt + "."
				Help(,, 'Help',, cMsgErro, 1, 0)
				nRet := -1
			Endif
		Next nX

		// Envia o e-mail.
		If nRet == 0
			nRet := oMessage:Send(oServer)
		Endif

		// Se deu falha, exibe mensagem na tela.
		If nRet <> 0
			cMsgErro := "Falha ao enviar e-mail." + CRLF
			cMsgErro += "De: " + RTrim(oMessage:cFrom) + CRLF
			cMsgErro += "Para: " + RTrim(oMessage:cTo) + CRLF
			If !empty(oMessage:cCC)
				cMsgErro += "CÛpia: " + RTrim(oMessage:cCC) + CRLF
			Endif
			If !empty(oMessage:cBCC)
				cMsgErro += "CÛpia oculta: " + RTrim(oMessage:cBCC) + CRLF
			Endif
			cMsgErro += "Erro: " + oServer:GetErrorString(nRet)
			Help(,, 'Help',, cMsgErro, 1, 0)
		Endif

		// Desconecta do servidor.
		If lMsProcTxt
			msProcTxt("Desconectando..."); ProcessMessages()
		Endif
		oServer:SmtpDisconnect()

		// Apaga arquivos depois de enviado o e-mail.
		aEval(aFilesSrv, {|x| fErase(x)})
	Endif
Endif

Return nRet


// ##############################################################################
// Projeto  : PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento
// FunÁ„o   : Texto
// DescriÁ„o: Retira caracteres especiais de um texto.
// Retorno  : Texto limpo.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 28/01/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function Texto(cTexto, lAlpha, lDigit, lSpace, nTamRet)
Local cRet       := ""
Local cChar      := ""
Local bAlpha     := {|| .F.}
Local bDigit     := {|| .F.}
Local bSpace     := {|| .F.}
Local nX

Default lAlpha := .T.
If lAlpha
	bAlpha := {|cChar| IsAlpha(cChar)}
Endif

Default lDigit := .T.
If lDigit
	bDigit := {|cChar| IsDigit(cChar)}
Endif

Default lSpace := .F.
If lSpace
	bSpace := {|cChar| cChar = " "}
Endif

For nX := 1 to len(cTexto)
	cChar := SubStr(cTexto, nX, 1)
	If Eval(bAlpha, cChar) .or. Eval(bDigit, cChar) .or. Eval(bSpace, cChar)
		cRet += cChar
	Endif
Next nX

Default nTamRet := len(cTexto)
cRet := PadR(cRet, nTamRet)

Return cRet


// ##############################################################################
// Projeto  : PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento
// FunÁ„o   : Txt2HTML
// DescriÁ„o: Converte um texto acentuado para o formato HTML.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 04/09/13 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function Txt2HTML(cTexto, lConEspaco)
Local cRet       := ""
Local cChar      := ""
Local nX, nY

Default lConEspaco := .F.

Static aPort      := {}
If empty(aPort)
	aAdd(aPort, {char(034), '&quot;'})
	aAdd(aPort, {char(038), '&amp;'})
	aAdd(aPort, {char(060), '&lt;'})
	aAdd(aPort, {char(062), '&gt;'})
	aAdd(aPort, {char(145), '&#8217;'})
	aAdd(aPort, {char(146), '&#8217;'})
	aAdd(aPort, {char(147), '&quot;'})
	aAdd(aPort, {char(149), '&bull;'})
	aAdd(aPort, {char(148), '&quot;'})
	aAdd(aPort, {char(150), '&#9472;'})
	aAdd(aPort, {char(160), ' '})
	aAdd(aPort, {char(161), '&iexcl;'})
	aAdd(aPort, {char(162), '&cent;'})
	aAdd(aPort, {char(163), '&pound;'})
	aAdd(aPort, {char(164), '&curren;'})
	aAdd(aPort, {char(165), '&yen;'})
	aAdd(aPort, {char(166), '&brvbar;'})
	aAdd(aPort, {char(167), '&sect;'})
	aAdd(aPort, {char(168), '&uml;'})
	aAdd(aPort, {char(169), '&copy;'})
	aAdd(aPort, {char(170), '&ordf;'})
	aAdd(aPort, {char(171), '&laquo;'})
	aAdd(aPort, {char(172), '&not;'})
	aAdd(aPort, {char(173), '-'})
	aAdd(aPort, {char(174), '&reg;'})
	aAdd(aPort, {char(175), '&macr;'})
	aAdd(aPort, {char(176), '&deg;'})
	aAdd(aPort, {char(177), '&plusmn;'})
	aAdd(aPort, {char(178), '&sup2;'})
	aAdd(aPort, {char(179), '&sup3;'})
	aAdd(aPort, {char(180), '&acute;'})
	aAdd(aPort, {char(181), '&micro;'})
	aAdd(aPort, {char(182), '&para;'})
	aAdd(aPort, {char(183), '&middot;'})
	aAdd(aPort, {char(184), '&cedil;'})
	aAdd(aPort, {char(185), '&sup1;'})
	aAdd(aPort, {char(186), '&ordm;'})
	aAdd(aPort, {char(187), '&raquo;'})
	aAdd(aPort, {char(188), '&frac14;'})
	aAdd(aPort, {char(189), '&frac12;'})
	aAdd(aPort, {char(190), '&frac34;'})
	aAdd(aPort, {char(191), '&iquest;'})
	aAdd(aPort, {char(192), '&Agrave;'})
	aAdd(aPort, {char(193), '&Aacute;'})
	aAdd(aPort, {char(194), '&Acirc;'})
	aAdd(aPort, {char(195), '&Atilde;'})
	aAdd(aPort, {char(196), '&Auml;'})
	aAdd(aPort, {char(197), '&Aring;'})
	aAdd(aPort, {char(198), '&AElig;'})
	aAdd(aPort, {char(199), '&Ccedil;'})
	aAdd(aPort, {char(200), '&Egrave;'})
	aAdd(aPort, {char(201), '&Eacute;'})
	aAdd(aPort, {char(202), '&Ecirc;'})
	aAdd(aPort, {char(203), '&Euml;'})
	aAdd(aPort, {char(204), '&Igrave;'})
	aAdd(aPort, {char(205), '&Iacute;'})
	aAdd(aPort, {char(206), '&Icirc;'})
	aAdd(aPort, {char(207), '&Iuml;'})
	aAdd(aPort, {char(208), '&ETH;'})
	aAdd(aPort, {char(209), '&Ntilde;'})
	aAdd(aPort, {char(210), '&Ograve;'})
	aAdd(aPort, {char(211), '&Oacute;'})
	aAdd(aPort, {char(212), '&Ocirc;'})
	aAdd(aPort, {char(213), '&Otilde;'})
	aAdd(aPort, {char(214), '&Ouml;'})
	aAdd(aPort, {char(215), '&times;'})
	aAdd(aPort, {char(216), '&Oslash;'})
	aAdd(aPort, {char(217), '&Ugrave;'})
	aAdd(aPort, {char(218), '&Uacute;'})
	aAdd(aPort, {char(219), '&Ucirc;'})
	aAdd(aPort, {char(220), '&Uuml;'})
	aAdd(aPort, {char(221), '&Yacute;'})
	aAdd(aPort, {char(222), '&THORN;'})
	aAdd(aPort, {char(223), '&szlig;'})
	aAdd(aPort, {char(224), '&agrave;'})
	aAdd(aPort, {char(225), '&aacute;'})
	aAdd(aPort, {char(226), '&acirc;'})
	aAdd(aPort, {char(227), '&atilde;'})
	aAdd(aPort, {char(228), '&auml;'})
	aAdd(aPort, {char(229), '&aring;'})
	aAdd(aPort, {char(230), '&aelig;'})
	aAdd(aPort, {char(231), '&ccedil;'})
	aAdd(aPort, {char(232), '&egrave;'})
	aAdd(aPort, {char(233), '&eacute;'})
	aAdd(aPort, {char(234), '&ecirc;'})
	aAdd(aPort, {char(235), '&euml;'})
	aAdd(aPort, {char(236), '&igrave;'})
	aAdd(aPort, {char(237), '&iacute;'})
	aAdd(aPort, {char(238), '&icirc;'})
	aAdd(aPort, {char(239), '&iuml;'})
	aAdd(aPort, {char(240), '&eth;'})
	aAdd(aPort, {char(241), '&ntilde;'})
	aAdd(aPort, {char(242), '&ograve;'})
	aAdd(aPort, {char(243), '&oacute;'})
	aAdd(aPort, {char(244), '&ocirc;'})
	aAdd(aPort, {char(245), '&otilde;'})
	aAdd(aPort, {char(246), '&ouml;'})
	aAdd(aPort, {char(247), '&divide;'})
	aAdd(aPort, {char(248), '&oslash;'})
	aAdd(aPort, {char(249), '&ugrave;'})
	aAdd(aPort, {char(250), '&uacute;'})
	aAdd(aPort, {char(251), '&ucirc;'})
	aAdd(aPort, {char(252), '&uuml;'})
	aAdd(aPort, {char(253), '&yacute;'})
	aAdd(aPort, {char(254), '&thorn;'})
	aAdd(aPort, {char(255), '&yuml;'})
Endif

// Converte os acentos da lÌngua portuguesa.
For nX := 1 to len(cTexto)
	cChar := SubStr(cTexto, nX, 1)
	If asc(cChar) > 0  // Retira caracter CHAR(0) do texto.
		nY := aScan(aPort, {|x| x[1] == cChar})
		If nY > 0
			cChar := aPort[nY, 2]
		ElseIf lConEspaco .and. cChar == " "
			// Converte espaÁos em branco.
			cChar := "&nbsp;"
		Endif
		cRet += cChar
	Endif
Next nX

// Converte as quebras de linha.
cRet := StrTran(cRet, CRLF,     '<BR/>')
cRet := StrTran(cRet, char(10), '<BR/>')
cRet := StrTran(cRet, char(13), '<BR/>')

Return cRet


// ##############################################################################
// Projeto  : PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento
// FunÁ„o   : Browser
// DescriÁ„o: ExibiÁ„o de HTML na tela.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 18/11/14 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function Browser(cHtml, cTitulo, lAbreWin, aButtons, lImprimir, lExcel, bAcao, cURL, cFileOpen)
Local lRet       := .F.
Local lWinHTML   := SuperGetMv("BZ_WINHTML", .F., .T.)
Local oBrowser, oGetURL, oNavigate

Local cFileName  := ""
Local nHdlHTML   := 0
Local cNomArq	 := "Arq" + DtoS(Date()) + StrTran(Time(), ":", "")

Default cTitulo   := "Navegador"
Default lAbreWin  := .F.
Default aButtons  := {}
Default lImprimir := .T.
Default lExcel    := .F.
Default bAcao     := {|| nil}
Default cFileOpen := ""

If empty(cURL)
	// Grava o arquivo no computador cliente.
	If empty(cTmpLocal)
		cTmpLocal := AllTrim(GetTempPath())
	Endif
	cFileName := cTmpLocal + cNomArq + ".htm"
	nHdlHTML  := fCreate(cFileName)
	FWrite(nHdlHTML, cHtml)
	FClose(nHdlHTML)
	cUrl := cFileName
Else
	cFileName := cURL
Endif

cFileOpen := cFileName

// Define se o HTML ser· aberto pelo Protheus ou pelo browser padr„o do usu·rio.
If lSmartHTML
	// Abre o HTML em um popup.
	ShellExecute("open", cFileName, "", "", SW_SHOWMAXIMIZED)
ElseIf lAbreWin .and. lWinHTML
	// Abre o HTML no browser padr„o do sistema operacional.
	ShellExecute("open", cFileName, "", "", SW_SHOWMAXIMIZED)
Else
// Desenha a DIALOG para visualizar o HTML.
Define msDialog oBrowser from 0, 0 to 0, 0 title cTitulo pixel
oBrowser:lMaximized := .T.

oGetURL := TGet():new(0, 0, {|| cFileName}, oBrowser, 0, 10,,,,,, .F.,, .T., "", .F.,, .F., .F.,, .T.)
oGetURL:Align := CONTROL_ALIGN_TOP

	oNavigate := TWebEngine():New(oBrowser, 0, 0, 0, 0)
oNavigate:Align := CONTROL_ALIGN_ALLCLIENT
oNavigate:Navigate(cFileName)

// Inclui a opÁ„o de imprimir.
If lExcel
	aAdd(aButtons, nil); aIns(aButtons, 1)
	aButtons[1] := {"EXCEL", {|| oExcelApp := MsExcel():New(), oExcelApp:WorkBooks:Open(cFileName), oExcelApp:SetVisible(.T.)}, "Excel"}
Endif

// Inclui a opÁ„o de imprimir.
If lImprimir
	aAdd(aButtons, nil); aIns(aButtons, 1)
		aButtons[1] := {"PRINT", {|| oNavigate:PrintPDF()}, "Imprimir PDF"}

		aAdd(aButtons, nil); aIns(aButtons, 1)
		aButtons[1] := {"PRINT", {|| ShellExecute("open", cFileName, "", "", SW_SHOWMAXIMIZED)}, "Imprimir"}
Endif

Activate msDialog oBrowser centered on init (EnchoiceBar(oBrowser, {|| lRet := .T., oBrowser:End()}, {|| lRet := .F., oBrowser:End()},, aButtons), Sleep(2000), Eval(bAcao, oNavigate))
Endif

// Apaga arquivos tempor·rios.
If empty(cURL)
	fErase(cFileName)
Endif

Return lRet
// ##############################################################################
// Projeto  : PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento
// FunÁ„o   : USetKey
// DescriÁ„o: Armazena ou restaura as configuraÁıes de teclas Fn.
// Retorno  : Array com as configuraÁıes salvas.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 01/06/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function USetKey(aKeySet, lClear)
Default aKeySet := {}
Default lClear  := .F.

Static aTeclasFn  := {}

If empty(aKeySet)  // Armazena as configuracoes das teclas.
	If empty(aTeclasFn)
		aAdd(aTeclasFn, VK_F2)
		aAdd(aTeclasFn, VK_F3)
		aAdd(aTeclasFn, VK_F4)
		aAdd(aTeclasFn, VK_F5)
		aAdd(aTeclasFn, VK_F6)
		aAdd(aTeclasFn, VK_F7)
		aAdd(aTeclasFn, VK_F8)
		aAdd(aTeclasFn, VK_F9)
		aAdd(aTeclasFn, VK_F10)
		aAdd(aTeclasFn, VK_F11)
		aAdd(aTeclasFn, VK_F12)
		aAdd(aTeclasFn, VK_F13)
		aAdd(aTeclasFn, VK_F14)
		aAdd(aTeclasFn, VK_F15)
		aAdd(aTeclasFn, VK_F16)
		aAdd(aTeclasFn, VK_F17)
		aAdd(aTeclasFn, VK_F18)
		aAdd(aTeclasFn, VK_F19)
		aAdd(aTeclasFn, VK_F20)
		aAdd(aTeclasFn, VK_F21)
		aAdd(aTeclasFn, VK_F22)
		aAdd(aTeclasFn, VK_F23)
		aAdd(aTeclasFn, VK_F24)
	Endif

	// Armazena a configuraÁ„o de cada tecla.
	aEval(aTeclasFn, {|nKey| aAdd(aKeySet, {nKey, SetKey(nKey, nil)})})

	// Se n„o for para limpar, restaura as configuraÁıes das teclas.
	If !lClear
		U_USetKey(aKeySet, .F.)
	Endif
Else  // Restaura as teclas de atalho.
	aEval(aKeySet, {|aKey| SetKey(aKey[1], aKey[2])})
Endif

// Limpa as teclas de atalho.
If lClear
	aEval(aKeySet, {|aKey| SetKey(aKey[1], nil)})
Endif

Return aKeySet


// ##############################################################################
// Projeto  : PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento
// FunÁ„o   : Producao
// DescriÁ„o: Retorna se a funÁ„o est· sendo executada em ambiente de produÁ„o.
// Retorno  : LÛgico, indicando est· sendo executada em ambiente de produÁ„o.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 11/03/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function Producao()
	Static lServerPrd
	If ValType(lServerPrd) == "U"
		lServerPrd := .F.
		lServerPrd := lServerPrd .or. (AllTrim(GetSrvProfString("SpecialKey", "")) == "BEPI_PRD")
	Endif

Return lServerPrd


// ##############################################################################
// Projeto  : PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento
// FunÁ„o   : DebugMsg
// DescriÁ„o: Exibe mensagem no console se executada em servidor de teste.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 07/01/16 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function DebugMsg(cMsg, cTipo, lForce)
Default lForce := .F.
If lForce .or. lJob .or. "DESENV" $ upper(GetEnvServer()) .or. !U_Producao() .or. (!empty(RetCodUsr(cUserName)) .and. PswAdmin(,, RetCodUsr(cUserName)) == 0)
	DebugMsg(cMsg, cTipo)
Endif

Return


// ##############################################################################
// Projeto  : PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento
// FunÁ„o   : DebugMsg
// DescriÁ„o: Exibe mensagem no console se executada em servidor de teste.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 07/01/16 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
Static Function DebugMsg(cMsg, cTipo)
Local nDebugTime := 0
Local nElapsed   := 0

Static aDebugTime := {}

If cTipo = "I"
	aAdd(aDebugTime, {Date(), Seconds()})
Endif

nDebugTime := len(aDebugTime)
If nDebugTime > 0
	nElapsed := (Seconds() - aDebugTime[nDebugTime, 2]) + ((Date() - aDebugTime[nDebugTime, 1]) * 86400)
Endif

If cTipo = "F" .and. nDebugTime > 0
	aDebugTime[nDebugTime] := nil
	aDebugTime := aSize(aDebugTime, nDebugTime - 1)
Endif

Return


// ##############################################################################
// Projeto  : PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento
// FunÁ„o   : MSAguarde
// DescriÁ„o: FunÁ„o para corrigir falha de escopo de vari·vel da funÁ„o
//            padr„o MSAguarde().
// Retorno  : O mesmo que a funÁ„o padr„o.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 05/09/16 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function MSAguarde(bAction, cTitle, cMsg, lAbort)
Private oText
Return MSAguarde(bAction, cTitle, cMsg, lAbort)


// ###########################################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Fabio Luiz Gesser
// Modulo   : Faturamento / Call Center
// FunÁ„o   : LogZX1
// DescriÁ„o: Grava log de processamento na tabela ZX1.
// Retorno  : Nenhum
// ---------+-------------------+-------------------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+-------------------------------------------------------------
// 01/11/16 | Fabio Gesser      | Desenvolvimento da rotina.
// ---------+-------------------+-------------------------------------------------------------
User Function LogZX1(cSeqZX1, cUsrLog, cChamada, cBotLog, cTipLog, cHoraIni, cHoraFim, cObserv)
Local aArea		:= {}
Local cUsrGrv	:= SuperGetMV("PC_LGUSRGR", NIL, "")

Static lLogTgv
If ValType(lLogTgv) <> "L"
	lLogTgv := SuperGetMV("PC_LOGTGV",, .F.) .OR. cUsrLog $ cUsrgrv 	// Determina se ira gerar log dos acessos no TGV.
Endif

If lLogTgv
	Default cHoraIni := Time()
	Default cHoraFim := Time()
	Default cObserv  := ""
	aArea := GetArea()
	RecLock("ZX1", .T.)
	ZX1->ZX1_FILIAL := xFilial("ZX1")
	ZX1->ZX1_SEQPRC := cSeqZX1
	ZX1->ZX1_USER   := cUsrLog
	ZX1->ZX1_PROCES := cChamada
	ZX1->ZX1_FUNCAO := cBotLog
	ZX1->ZX1_TIPO   := cTipLog
	ZX1->ZX1_DATA   := dDataBase
	ZX1->ZX1_HORINI := cHoraIni
	ZX1->ZX1_HORFIM := cHoraFim
	ZX1->ZX1_OBS    := cObserv
	ZX1->(msUnLock())
	RestArea(aArea)
Endif

Return


// ###########################################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Ciro Pedreira
// Modulo   : Faturamento / Call Center
// FunÁ„o   : CONDGATX7
// DescriÁ„o: CondiÁ„o para disparar alguns gatilhos customizados.
// Retorno  : LÛgico - .T. dispara ou .F. n„o dispara
// ---------+-------------------+-------------------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+-------------------------------------------------------------
// 21/07/17 | Ciro Pedreira     | Desenvolvimento da rotina.
// ---------+-------------------+-------------------------------------------------------------
User Function CONDGATX7(cCampX7, cSeqX7)
Local aAreaMem := GetArea()
Local lRet     := .T.

/*
A ideia dessa rotina È substituir a condiÁ„o digitada no gatilho, pois o campo tem um tamanho pequeno.
Utilize essa rotina para informar a condiÁ„o do gatilho e faÁa a chamada da mesma no campo
de CondiÁ„o do gatilho, passando como parametro o nome do campo e a sequencia para diferenciar qual
condiÁ„o executar.
OBS.: Tente unificar todas as possiveis condiÁıes de gatilho nessa funÁ„o, passando o nome do campo e a
sequencia para facilitar a identificaÁ„o do gatilho.
*/

If cCampX7 == 'A1_MUN' .And. cSeqX7 == '001'
	lRet := M->(A1_EST <> "DF" .And. A1_EST <> "EX") .And. !IsInCallStack("U_WSVTEX30")
ElseIf cCampX7 == 'A1_EST' .And. cSeqX7 == '003'
	lRet := (M->(A1_EST == "DF" .Or. A1_EST == "EX")) .And. !IsInCallStack("U_WSVTEX30")
EndIf

RestArea(aAreaMem)

Return lRet


// ###########################################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Faturamento / Call Center
// FunÁ„o   : Filiais
// DescriÁ„o: Busca todas as filiais da empresa posicionada.
// Retorno  : Nenhum.
// ---------+-------------------+-------------------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+-------------------------------------------------------------
// 21/07/17 | Ciro Pedreira     | Desenvolvimento da rotina.
// ---------+-------------------+-------------------------------------------------------------
User Function Filiais(cFiliais, aFiliais)
	Local aFilPad	:= FWAllFilial()
	Local nFilPad	:= 0

	// Guarda em vari·veis est·ticas as filiais que far„o parte do processamento.
	Static __cFiliais	:= ""
	Static __aFiliais	:= {}

	If Empty(__cFiliais)
		__cFiliais += "''"

		For nFilPad := 1 to len(aFilPad)
			__cFiliais += ", '" + aFilPad[nFilPad] + "'"
			aAdd(__aFiliais, aFilPad[nFilPad])
		Next nFilPad
	Endif

	cFiliais := __cFiliais
	aFiliais := aClone(__aFiliais)

Return

/**************************************************************************************************
FunÁ„o:
XMLPerg

Autor:
Tiago Bandeira Brasiliano

Data:
24/03/2009

DescriÁ„o:
Rotina utilizada para criar um dialogo contendo as perguntas e os par‚metros necess·rios para a
criaÁ„o de relatÛrios XML Excel

Par‚metros:
cPerg     => Pergunta (SX1) utilizada pelo relatÛrio
cDir      => DiretÛrio default onde ser„o salvos os arquivos criados (Default: C:\)
bOk       => Rotina executada pelo bot„o Ok (confirma). Esta rotina dever· voltar um string com
             o conte˙do que ser· salvo no arquivo.
             O Primeiro par‚metro desta rotina deve ser uma vari·vel lÛgica que ir· permitir o
             controle de cancelamento do processamento.
             O segundo par‚metro desta rotina ser· o caminho + nome do arquivo que ser· gerado.
cTitulo   => Titulo do relatÛrio que ser· exibido na janela
cNome     => Nome do relatÛrio. Este nome tambÈm ser· o nome padr„o do arquivo gerado
cDesc     => DescriÁ„o do relatÛrio
cExtensao => Extens„o do arquivo que ser· gerado apÛs o processamento da rotina em bOK
             (valor default: XML)
nOpc      => OpÁ„o de geraÁ„o:
             1 = Gera arquivo no disco e depois abre (default)
             2 = Some gera o arquivo em disco
cMsgProc  => Mensagem para ser exibida durante o processamento. AlÈm desta opÁ„o, È possÌvel
             utilizar dentro da rotina executada pela opÁ„o bOk as rotinas ProcRegua() e
             IncProc().

Retorno:
Nenhum
**************************************************************************************************/
User Function XMLPerg(cPerg, cDir, bOk, cTitulo, cNome, cDesc, cExtensao, nOpc, cMsgProc)
Local oFont1
Local oDialog
Local oImagem
Local cTipoArq 	:= "Todos os Arquivos (*.*)     | *.* |"
Local cArquivo  := ""
Local cXML      := ""
Local lCancela  := .F.
Local bGravaArq := {|| Iif(lCancela, Alert("Processamento cancelado pelo usu·rio."), Iif(!Empty(cXML), Eval({|| Iif(nOpc == 1, ShellExecute("open", AllTrim(cArquivo)+"."+AllTrim(cExtensao), "", "", 1), NIL), Aviso("Resultado de processamento", "Arquivo gerado com sucesso!", {"Ok"})}), Alert("Nenhum arquivo foi gerado!")))}

DEFAULT cPerg	  := ""
DEFAULT cDir      := "C:\"
DEFAULT cExtensao := "XML"
DEFAULT cMsgProc  := "Aguarde. Gerando relatÛrio..."

If !Empty(cPerg)
	Pergunte(cPerg, .F.)
EndIf

cArquivo := cDir + cNome
cArquivo := PadR(cArquivo, 250)

oFont1 := TFont():New(,,16,,.T.)

oDialog := MSDialog():New(0, 0, 270, 400, OemToAnsi(cTitulo),,,,,,CLR_WHITE,,,.T.,,,)

TSay():New(005,063,{|| "GeraÁ„o de relatÛrio Excel"},,,oFont1,,,,.T.)

TGroup():New(015,004,043,197," Destino: "  ,oDialog,,,.T.)
TGet():New(024,008,bSetGet(cArquivo),,171,010,,,,,,,,.T.)
TBtnBmp2():New(047,359,026,026,"SDUOPEN",,,,{|| cPath := cGetFile(cTipoArq,"Selecione o diretÛrio de destino",0,cDir,.T.,GETF_LOCALFLOPPY+GETF_NETWORKDRIVE+GETF_LOCALHARD+GETF_RETDIRECTORY, .F.), cDir := Iif(Empty(cPath), cDir, cPath), cArquivo := PadR(cDir + cNome, 250)},oDialog,"Pesquisa local de destino")

TGroup():New(045,004,100,197," DescriÁ„o: ",oDialog,,,.T.)
TMultiGet():New(054,008,bSetGet(cDesc),,185,040,,,,,,.T.,,,,,,.T.,,,)

TGroup():New(102,004,133,095," OpÁıes: ",oDialog,,,.T.)
oImagem := TBitmap():New(113,009, 32, 32, "MDIEXCEL",,.T.,oDialog,,,,,,,,,.T.,,,.T.)
TRadMenu():New(110,025,{"Gera arquivo + Abre","Somente gera arquivo"},bSetGet(nOpc), oDialog,,{|| oImagem:Load(Iif(nOpc == 1, "MDIEXCEL", "SALVAR"))},,,,,,065,011,,,,.T.)

If !Empty(cPerg)
	SButton():New(120, 110, 5, {|| Pergunte(cPerg, .T.)}) // Parametros
EndIf
SButton():New(120, 140, 1, {|| Iif(VldNomeArq(@cArquivo, cExtensao), Eval({|| Processa({|lEnd| cXML := Eval(bOk, @lEnd, AllTrim(cArquivo)+"."+AllTrim(cExtensao)), lCancela := lEnd},cMsgProc,,.T.), Eval(bGravaArq), oDialog:End()}),Nil)} ) // Ok
SButton():New(120, 170, 2, {|| oDialog:End()})         // Cancela

Activate Dialog oDialog CENTERED

Return .T.


/**************************************************************************************************
FunÁ„o:
VldNomeArq

Autor:
Tiago Bandeira Brasiliano

Data:
08/05/2009

DescriÁ„o:
Esta funÁ„o verifica se j· existe um arquivo com o mesmo nome do arquivo que est· sendo salvo.
Caso exista È exibida uma mensagem informando que j· existe um arquivo com o mesmo nome, e pergunta
ao usu·rio se ele deseja substituir, ou se deseja manter o arquivo atual.

Par‚metros:
cArq     => Path e nome do arquivo
cExt     => Extens„o do arquivo

Retorno:
lRet     => Booleano indicando se deve prosseguir com o processamento do arquivo ou n„o.
            .T. - Continua com o processamento.
            .F. - Cancela o processamento.

**************************************************************************************************/
Static Function VldNomeArq(cArq, cExt)
Local lRet     := .T.
Local lAchou   := .F.
Local cNomeRel := SubStr(cArq, RAt("\", cArq) + 1)
Local cPath    := SubStr(cArq, 1, RAt("\", cArq))

// Procura pelo arquivo no diretÛrio
aEval(Directory(cPath+"*."+cExt), {|aArqTXT| Iif(AllTrim(Upper(SubStr(aArqTXT[1],1,RAt(".",aArqTXT[1]) - 1))) == AllTrim(Upper(cNomeRel)), lAchou := .T., Nil)})

If lAchou
	If MsgNoYes("J· existe um arquivo no diretÛrio com este nome. Deseja continuar e substituir este arquivo?")
		If FErase(AllTrim(cArq)+"."+AllTrim(cExt)) < 0// Apaga o arquivo
			lRet := .F.
			Alert("AtenÁ„o! N„o foi possÌvel excluir o arquivo "+AllTrim(cArq)+"."+AllTrim(cExt)+". Verifique se o mesmo est· aberto ou sendo utilizado por outro programa.")
		EndIf
	Else
		lRet := .F.
	EndIf
EndIf

Return lRet

//-------------------------------------------------------------------
/*/{Protheus.doc} BZCHKFUN
Verifica se a Rotina cadastrada est· compilada no RPO

@author  Guilherme Santos
@since   29/09/2020
@version 12.1.25
/*/
//-------------------------------------------------------------------
User Function BZCHKFUN()
	Local cRotina	:= &(ReadVar())
	Local lRetorno	:= FindFunction(cRotina)

	If !lRetorno
		Help(" ", 1, "Help", "BZCHKFUN", "Rotina " + cRotina + " n„o disponÌvel nesse ambiente.", 3, 0)
	EndIf

Return lRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} BZF3Fil
Consulta Especifica para Selecao da Filial

@author  Guilherme Santos
@since   03/05/2022
@version 12.1.27
/*/
//-------------------------------------------------------------------
User Function BZF3Fil(cFilSel)
	Local aParBox	:= {}
	Local aRetPar	:= {}
	Local cFilNew	:= Space(2)
	Local lRetorno	:= .T.

	Default cFilSel := cFilAnt

	Aadd(aParBox, {1, "Filial", cFilNew,,"ExistCpo('SM0', cEmpAnt + MV_PAR01)", "SM0",,, .T.})

	If ParamBox(aParBox, "Informe os par‚metros", @aRetPar,,,,,,,, .F.)
		cFilSel	:= aRetPar[1]
	Else
		lRetorno := .F.
	EndIf

Return lRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} GRVStamp
Habilitacao da Gravacao dos campos de TimeStamp

@author  Guilherme Santos
@since   30/11/2022
@version 12.1.33
/*/
//-------------------------------------------------------------------
User Function GRVStamp()
	Local cRetorno	:= ""
	Local aBoxParam := {}
	Local aRetParam := {}

	AADD( aBoxParam, {1,"Tabela",Space(6),"","","","",50,.F.} )

	If ParamBox(aBoxParam,"Informe os par‚metros.",@aRetParam,,,,,,,,.F.)
		TCLink()
		cRetorno := TCConfig("SETAUTOSTAMP=ON")
		TCRefresh(MV_PAR01)
		TCCONfig("SETAUTOSTAMP=OFF")
		TCUnlink()

		Aviso("GRVStamp", "Retorno TCConfig: " + cRetorno, {"Fechar"})
	EndIf

Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} BZGetPar
Retorna o parametro da Filial informada

@author  Guilherme Santos
@since   03/02/2023
@version 12.1.33
/*/
//-------------------------------------------------------------------
User Function BZGetPar(cParametro, xDefault, cFilPar)
	Local xRetorno := SuperGetMV(cParametro, NIL, xDefault, cFilPar)
Return xRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} Tipo
Compara o Tipo da variavel

@author  Guilherme Santos
@since   10/05/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
/*/{Protheus.doc} EdiRetSku
Retira caracteres do SKU recebido via EDI

@author  Victor Dessunte
@since   29/03/2023
@version 12.1.33
/*/
//-------------------------------------------------------------------
User Function EdiRetSku(cSkuCli,cCnpjFat)

	Local aArea		:= GetArea()
	Local nX		:= 0
	Local cSkuRet	:= cSkuCli
    Local aContent	:= {}

	If !Empty(cSkuCli)
		SZW->(dbSetOrder(2)) //Filial+CNPJ
		If SZW->(dbSeek(xFilial("SZW")+cCnpjFat))
			If Len(FWGetSX5("Z1",SZW->ZW_CLIENTE,"pt-br")) > 0
				aContent := StrTokArr(FWGetSX5("Z1",SZW->ZW_CLIENTE,"pt-br")[1][4],",")
			EndIf
		EndIf

		If Len(aContent) > 0
			For nX := 1 to Len(aContent)
				If (At(aContent[nX],cSkuCli)) > 0
					cSkuRet := StrTran(cSkuCli,aContent[nX])
				EndIf
			Next
		EndIf
	EndIf

	RestArea(aArea)

Return cSkuRet

//-------------------------------------------------------------------
/*/{Protheus.doc} EdiRetPrf
Retira prefixo do numpedido para clientes informados na SX5

@author  Victor Dessunte
@since   29/03/2023
@version 12.1.33
/*/
//-------------------------------------------------------------------
User Function EdiRetPrf(cNumPed,cCnpjFat)

	Local aArea		:= GetArea()
	Local nX		:= 0
	Local cNumRet	:= cNumPed
    Local aContent	:= {}

	If !Empty(cNumPed)
		SZW->(dbSetOrder(2)) //Filial+CNPJ
		If SZW->(dbSeek(xFilial("SZW")+cCnpjFat))
			If Len(FWGetSX5("Z1",SZW->ZW_CLIENTE,"pt-br")) > 0
				aContent := StrTokArr(FWGetSX5("Z1",SZW->ZW_CLIENTE,"pt-br")[1][4],",")
			EndIf
		EndIf

		If Len(aContent) > 0
			For nX := 1 to Len(aContent)
				If (At(aContent[nX],cNumPed)) > 0
					cNumRet := StrTran(cNumPed,aContent[nX])
				EndIf
			Next
		EndIf
	EndIf

	RestArea(aArea)

Return cNumRet

//-------------------------------------------------------------------
/*/{Protheus.doc} Tipo
Compara o Tipo da variavel

@author  Guilherme Santos
@since   10/05/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
User Function Tipo(oXML, cVariavel, cTipo)
	Local lRetorno := Type("oXML" + cVariavel) == cTipo
Return lRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} GetX5Cpo
Retorna o Campo Descricao do SX5 para a chave informada

@author  Guilherme Santos
@since   10/05/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
User Function GetX5Cpo(cTable, cKey)
	Local aTable	:= FWGetSX5(cTable, cKey)
	Local nPosKey	:= 0
	Local cRetorno  := ""

	If !Empty(aTable)
		nPosKey := AScan(aTable, {|x| AllTrim(x[4]) == AllTrim(cKey)})

		If nPosKey > 0
			cRetorno := aTable[nPosKey][4]
		EndIf
	EndIf

Return cRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} LocDate
Criacao da Function GetLocalDate no Banco de Dados do Protheus

@author  Guilherme Santos
@since   14/02/2023
@version 12.1.33
/*/
//-------------------------------------------------------------------
User Function LocDate()
	Local cQuery := ""

	//Exemplo de Utilizacao
	//select dbo.GetLocalDate(GETUTCDATE())

	cQuery += "create function GetLocalDate(@UTCDate datetime)" + CRLF
	cQuery += "returns datetime" + CRLF
	cQuery += "as " + CRLF
	cQuery += "begin" + CRLF
	cQuery += "	declare @LocalDate as datetime" + CRLF
	cQuery += "	set @LocalDate = CONVERT(datetime, SWITCHOFFSET(CONVERT(datetimeoffset, @UTCDate), DATENAME(TzOffset, SYSDATETIMEOFFSET())))" + CRLF
	cQuery += "	return @LocalDate" + CRLF
	cQuery += "end;" + CRLF

	If TcSqlExec(cQuery) < 0
		Aviso("LocDate", "Erro ao criar a function GetLocalDate no Banco de Dados", {"Fechar"})
	EndIf

Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} BaseFIX
Ajuste de Parametros nas bases de HomologaÁ„o

@author  Guilherme Santos
@since   23/06/2023
@version 12.1.2210
/*/
//-------------------------------------------------------------------
User Function BaseFIX()

	If !U_Producao()
		TcSQLExec("exec parametros_hml_dev")
	EndIf

Return NIL

//-------------------------------------------------------------------
/*/{Protheus.doc} GRVCont
Gera amarraÁ„o do contato com o cÛdigo do cliente para todas as lojas

@author  Victor Dessunte
@since   25/08/2023
@version 12.1.2210
/*/
//-------------------------------------------------------------------
User Function GRVCont()

	Local aBoxParam := {}
	Local aRetParam := {}

	AADD( aBoxParam, {1,"CÛdigo do Cliente",Space(6),"@!","EXISTCPO('SA1',MV_PAR01)","SA1","",50,.T.} )
	AADD( aBoxParam, {1,"CÛdigo do Contato",Space(6),"@!","EXISTCPO('SU5',MV_PAR02)","SU5","",50,.T.} )

	If ParamBox(aBoxParam,"Informe os par‚metros.",@aRetParam,,,,,,,,.F.)
		msAguarde({|| GeraCont(MV_PAR01,MV_PAR02)}, "Aguarde", "Gerando amarraÁ„o...", .T.)
		Aviso("GRVCONT", "AmarraÁ„o efetuada com sucesso.", {"Fechar"})
	EndIf

Return

Static Function GeraCont(cCliente,cContato)

Local cTmp := ""
Local cQry := ""

cQry := " SELECT "
cQry += " 	A1_COD, "
cQry += " 	A1_LOJA "
cQry += " FROM "
cQry +=	  	RetSqlName("SA1") + " WITH(NOLOCK) "
cQry += " WHERE "
cQry += " 		D_E_L_E_T_	= '' "
cQry += " AND 	A1_FILIAL	= '' "
cQry += " AND 	A1_COD		= '" + cCliente + "' "

cTmp := MPSysOpenQuery(cQry)
(cTmp)->(DBGOTOP())

While (cTmp)->(!EOF())
	RecLock("AC8",.T.)
	AC8->AC8_FILIAL	:= xFilial("AC8")
	AC8->AC8_FILENT	:= ""
	AC8->AC8_ENTIDA := "SA1"
	AC8->AC8_CODENT := (cTmp)->A1_COD + (cTmp)->A1_LOJA
	AC8->AC8_CODCON := cContato
	AC8->(MsUnlock())

	(cTmp)->(dbSkip())
End

Return
