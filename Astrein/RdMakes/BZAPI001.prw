#INCLUDE "PROTHEUS.CH"
#INCLUDE "AARRAY.CH"
#INCLUDE "JSON.CH"
#INCLUDE "FWMVCDEF.CH"

#DEFINE CRLF CHR(13) + CHR(10)

//Static _nTCGC := TamSx3("A1_CGC")[1]
//Static _nTProd:= TamSx3("B1_COD")[1]

/***********************************************************************************/
/*/{Protheus.doc} BZAPI001
@description Rotina realiza a integração de clientes Astrein
@type  Function
@author Bernard M. Margarido
@since 31/10/2020
/*/
/***********************************************************************************/
User Function BZAPI001(nRecno)

	Local _cIdProc      := ""
	Local _cChave       := ""
	Local _cDescApi     := "ASTREIN_INTEGRACAO"
	Local nTentat		:= 0
	Local lDbg			:= .F.

	Private _aRetAstr   := {}

	Default nRecno      := 0

	If lDbg
		RpcSetEnv("01","00")
	EndIf

	Private _nTCGC := TamSx3("A1_CGC")[1]
	Private _nTProd:= TamSx3("B1_COD")[1]
	Private _nTA1MUN := TamSx3("A1_MUN")[1]
	Private _nTA1MUNE := TamSx3("A1_MUNE")[1]
	Private _nTA1MUNC := TamSx3("A1_MUNC")[1]

	dbSelectArea("Z10")
	Z10->( dbSetOrder(2) )

	If nRecno > 0
		Z10->( dbGoto(nRecno) )
		_cIdProc := Z10->Z10_ID
	Else
		Z10->( dbSeek(xFilial("Z10") + PadR(_cDescApi,TamSx3("Z10_DESCR")[1])))
		_cIdProc := Z10->Z10_ID
	EndIf

	nTentat := Z10->Z10_TENTAT

	//-----------------------------+
	// Integração de Dados Astrein |
	//-----------------------------+
	BZAPI001A(_cIdProc,@_cChave,nTentat)

	//----------------+
	// Processa Dados |
	//----------------+
	BZAPI001C(_cIdProc,_cChave,nTentat)

Return Nil

/***********************************************************************************/
/*/{Protheus.doc} BZAPI001A
@description Realiza a integração de obter dados da Astrein
@type  Static Function
@author Bernard M. Margarido
@since 01/11/2020
/*/
/***********************************************************************************/
Static Function BZAPI001A(_cIdProc,_cChave,nTentat)

	Local _aArea    := GetArea()
	Local _cError   := ""
	Local _cStatus  := ""
	Local _cCodEmp  := GetNewPar("BZ_EMPASTR","BEPI")
	Local _nTpItem  := GetNewPar("BZ_TPIASTR",1)
	Local _oAstrein := Nil
	Local _oJSon    := Nil
	Local _lContinua:= .T.

	While _lContinua
		_aArea    := GetArea()
		_oAstrein := Astrein():New()

		//-------------------+
		// Monta objeto Json |
		//-------------------+
		_oJSon                   := Array(#)
		_oJSon[#"codigoProjeto"] := _nTpItem
		_oJSon[#"tipoItem"]      := _cCodEmp

		_oAstrein:cJSon          := ToJson(_oJSon)

		_cChave                  := ""

		If _oAstrein:ObterIntegracao()

			If IsInCallStack("U_BZMNTINT")
				cStMntInt := "online"
			EndIf

			//----------------------+
			// StringJson em Objeto |
			//----------------------+
			_oJson := FromJson(_oAstrein:cJSonRet)

			//-------------------------------------+
			// Cria chave para gravação do monitor |
			//-------------------------------------+
			If ValType(_oJson[#"NewDataSet"][#"INTITEM"]) <> "U"

				_cError := ""
				_cStatus:= "1"
				_cChave := "/"+RTrim(_oJson[#"NewDataSet"][#"INTITEM"][#"ITE_CODIGO"]) +"/"+ RTrim(_oJson[#"NewDataSet"][#"INTITEM"][#"SCM_CODIGO"]) +"/"+ RTrim(_oJson[#"NewDataSet"][#"INTITEM"][#"MAT_CODIGO"])

				//--------------+
				// Atualiza Z12 |
				//--------------+
				U_BzApi01d(_cIdProc,_cChave,_oAstrein:cJSonRet,_cError,_cStatus)
			Else
				_lContinua := .F.
			EndIf

		Else

			If IsInCallStack("U_BZMNTINT")
				cStMntInt := "offline"
			EndIf

		EndIf

		FreeObj(_oAstrein)
		FreeObj(_oJSon)
		RestArea(_aArea)
	End

Return Nil

/***********************************************************************************/
/*/{Protheus.doc} BZAPI001C
@description Realiza a criação atualização dos clientes
@type  Static Function
@author Bernard M. Margarido
@since 03/11/2020
/*/
/***********************************************************************************/
Static Function BZAPI001C(_cIdProc,_cChave,nTentat)

	Local _aArea        := GetArea()
	Local _aAreaCart	:= {}
	Local _aArray       := {}
	Local _aStruct      := {}
	Local _aErroAuto    := {}
	Local _cCpoDel      := "A1_TABELA"
	Local _cMsgLog      := ""
	Local _cAlias       := ""
	Local _cCpj         := ""
	Local _cCodigo      := ""
	Local _cLoja        := ""
	Local _xRet         := ""
	Local _cCarteira	:= ""
	Local _cCartExec	:= ""
	Local _cCanal		:= ""
	Local _cClasse		:= ""
	Local _cSClasse		:= ""
	Local _cGrpVen		:= ""
	Local _cPessoa		:= ""
	Local _cCodCli		:= ""
	Local _cCdMun		:= ""
	Local _cEste		:= ""
	Local _cCodMunE		:= ""
	Local _cCodMunC		:= ""
	Local _cCodMunCo	:= ""
	Local _cBco1		:= ""
    Local _cCond        := ""
	Local _cRisco		:= ""
	Local _cCliCrd		:= ""
	Local _cStGer		:= ""
	Local _lLoja        := .F.
	Local _nOpc         := 0
	Local _nX           := 0
	Local _nA1LC        := 0
	Local _dVenc		:= CTOD("  /  /  ")
	Local _aCarteira	:= {}
	Local _aCartExec	:= {}
	Local _oJson        := Nil
	Local _oMaterial    := Nil
	Local nRecnoZ12     := 0
	Local _cNotCli		:= SuperGetMv("BZ_ASTNCL",,"")
	Local _aGetCred		:= {}	//Retorno da funcao GETCRD. Tanimoto 30/08/2021
	Local nTA1NREDUZ	:= TamSX3("A1_NREDUZ")[1]
	Local nTA1COD_MUN	:= TamSX3("A1_COD_MUN")[1]
	Local nGrv001		:= SuperGetMV("CRMA980P01",,1) //Grava flag de integração com a ferramenta SSA-CAD ? 1=Sim

	//produto
	Local aAutoSB1   := {}
	Local aAutoSB5   := {}
	Local oModel
	Local lCpo       := .f.
	Local _aStructSB5:={}
	Local _cEst := ""

	If !BzApi01CQr(_cIdProc,@_cAlias,nTentat)
		RestArea(_aArea)
		Return .F.
	EndIf

	//-------------------------+
	// Tabela de Monitoramento |
	//-------------------------+
	dbSelectArea("Z12")
	Z12->( dbSetOrder(1) )

	//------------------------+
	// Processa clientes BEPI |
	//------------------------+
	While (_cAlias)->( !Eof() )
		//--------------------+
		// Posiciona registro |
		//--------------------+
		Z12->( dbGoTo((_cAlias)->RECNOZ12))
		nRecnoZ12 := (_cAlias)->RECNOZ12

		//----------------------------------+
		// Transforma string Json em Objeto |
		//----------------------------------+
		_oJson := FromJson(Z12->Z12_JSON)

		_cMsgLog := ""
		//--------------------------------------+
		// Valida se Objeto pode ser processado |
		//--------------------------------------+
		If ValType(_oJson[#"NewDataSet"][#"MATERIAL"]) <> "U"

			//-------------------+
			// Processa Clientes |
			//-------------------+
			If At("CLIENTE",_oJson[#"NewDataSet"][#"MATERIAL"][#"TIPO_ITEM"]) > 0

				_cPessoa    := ""
				_cCarteira  := ""
				_cCartExec  := ""
				_cCanal     := ""
				_cClasse    := ""
				_cSClasse   := ""
				_cGrpVen    := ""
				_cCodCli	:= ""
				_cCodMunE	:= ""
				_cEste		:= ""
				_cCodMun	:= ""
				_cCodMunC	:= ""
				_cCodMunCo	:= ""
				_cBco1		:= ""
				_cCond      := ""
				_cRisco		:= ""
				_cStGer		:= ""
				_dVenc		:= dDataBase+90
				_lLoja		:= .F.
				_aArray     := {}
				_aErroAuto  := {}
				_aGetCred	:= {}
				_aStruct    := SA1->( dbStruct() )
				_oMaterial  := _oJson[#"NewDataSet"][#"MATERIAL"]
				_oIntItem   := _oJson[#"NewDataSet"][#"INTITEM"]
				_nOpc       := 0
				_nA1LC    	:= 0
				_cCpj       := PadR(_oMaterial[#"A1_CGC"],_nTCGC)
				_cIdAstrein := _oIntItem[#"MAT_CODIGO"]

				If ValType(_oMaterial[#"A1_COD"]) <> 'U'
					_cCodCli := SUBSTR(_oMaterial[#"A1_COD"],1,6)
				EndIf
				If ValType(_oMaterial[#"ZA6_COD_V"]) <> 'U'
					_cCarteira := _oMaterial[#"ZA6_COD_V"]
				EndIf

				If ValType(_oMaterial[#"ZA6_COD_E"]) <> 'U'
					_cCartExec := _oMaterial[#"ZA6_COD_E"]
				EndIf

				If ValType(_oMaterial[#"ACY_XCANAL"]) <> 'U'
					_cCanal := _oMaterial[#"ACY_XCANAL"]
				EndIf

				If ValType(_oMaterial[#"ACY_XCLASS"]) <> 'U'
					_cClasse := _oMaterial[#"ACY_XCLASS"]
				EndIf

				If ValType(_oMaterial[#"ACY_XSCLAS"]) <> 'U'
					_cSClasse := _oMaterial[#"ACY_XSCLAS"]
				EndIf

				If ValType(_oMaterial[#"A1_GRPVEN"]) <> 'U'
					_cGrpVen := _oMaterial[#"A1_GRPVEN"]
				EndIf

				If ValType(_oMaterial[#"A1_PESSOA"]) <> 'U'
					_cPessoa := _oMaterial[#"A1_PESSOA"]
				EndIf

				If ValType(_oMaterial[#"A1_EST"]) == 'U'
					_cMsgLog := "Estado não informado na interface"
				EndIf

				If ValType(_oMaterial[#"A1_COD_MUN"]) == 'U'
					_cMsgLog := "Código do municipio não informado na interface"
				EndIf

				If ValType(_oMaterial[#"A1_EST"]) <> 'U'
					_cEst := _oMaterial[#"A1_EST"]
				EndIf

				If ValType(_oMaterial[#"A1_ESTE"]) <> 'U'
					_cEste := _oMaterial[#"A1_ESTE"]
				EndIf

				If ValType(_oMaterial[#"A1_XCDMUNE"]) <> 'U'
					_cCodMunE := Right(AllTrim(_oMaterial[#"A1_XCDMUNE"]),5)
				EndIf

				If ValType(_oMaterial[#"A1_MUNC"]) <> 'U'
					_cCodMunC := PadR(Alltrim(_oMaterial[#"A1_MUNC"]),_nTA1MUNC)
				EndIf

				If ValType(_oMaterial[#"A1_XMUNCOM"]) <> 'U'
					_cCodMunCo := _oMaterial[#"A1_XMUNCOM"]
				EndIf

				If ValType(_oMaterial[#"A1_FPAGTO"]) <> 'U'
					_cBco1 := _oMaterial[#"A1_FPAGTO"]
				EndIf

				If _cPessoa == 'J' .AND. !Empty(_cEste) .AND. _cEst <> _cEste
					_cMsgLog := "Estado de faturamento e entrega diferentes para pessoa Juridica não permitido."
				EndIf

				If ValType(_oMaterial[#"A1_COND"]) <> 'U'
					_cCond := _oMaterial[#"A1_COND"]
				EndIf

				If ValType(_oMaterial[#"A1_LC"]) <> 'U'
					_nA1LC := VAL(_oMaterial[#"A1_LC"])
				EndIf

				If ValType(_oMaterial[#"STATUS_GESTOR"]) <> 'U'
					_cStGer := _oMaterial[#"STATUS_GESTOR"]
				EndIf
				//---------------------------------+
				// Valida codigo e loja do cliente |
				//---------------------------------+
				BzApi01Cli(_cCpj,_cIdAstrein,@_cCodigo,@_cLoja,@_nOpc,_cCodCli,@_lLoja)

				//---------------------------------+
				// Validação dos dados da carteira |
				//---------------------------------+
				If _nOpc == 3 //Inclusão

					If _cCodigo $ _cNotCli .OR. _cCodCli $ _cNotCli
						_cMsgLog := "Raiz de CNPJ pertencente a GR ou Telefônica"
					Else
						_aAreaCart := GetArea()

						//Valida carteira do cliente
						If Empty(_cCarteira)
							_cMsgLog := "Codigo da carteira nao preenchido."
						Else
							_aCarteira := VerCart(_cCarteira)
							If Len(_aCarteira) > 0
								ZA6->(dbGoTo(_aCarteira[1]))
								If ZA6->ZA6_COD <> _cCarteira
									_cMsgLog := "Codigo da carteira nao localizado."
								EndIf
								If !(_aCarteira[2] $ '01*02')
									_cMsgLog := "Carteira não pertence ao departamento de vendas de clientes."
								EndIf

							Else
								_cMsgLog := "Codigo da carteira nao localizado."
							EndIf
						EndIf

						//---------------------------------+
						// Validação dos dados da carteira |
						//---------------------------------+
						If !Empty(_cCartExec)
							_aCartExec := VerCart(_cCartExec)
							If Len(_aCartExec) > 0
								ZA6->(dbGoTo(_aCartExec[1]))
								If ZA6->ZA6_COD <> _cCartExec
									_cMsgLog := "Codigo da carteira executiva nao localizado."
								EndIf
								If _aCartExec[2] <> '04'
									_cMsgLog := "Carteira não pertence ao departamento de vendas de carteira executiva."
								EndIf
							Else
								_cMsgLog := "Codigo da carteira executiva nao localizado."
							EndIf
						EndIf

						RestArea(_aAreaCart)

						If Empty(_cCanal)
							_cMsgLog := "Canal não informado."
						EndIf

						If Empty(_cClasse)
							_cMsgLog := "Classificação não informada."
						EndIf
					EndIf
				EndIf

                If !Empty(_cCodCli) .AND. Len(_cCodCli) <> 6
				    _cMsgLog := "Código do cliente informado incorretamente"
				Else
				    //-------------------------------+
				    // Validação do Risco do Cliente |
				    //-------------------------------+
					If _cStGer == "APROVADA"
						_cRisco := "D" //Definir regra com Marcelo Abreu
					ELse //Não aprovado ou Null (não passou pelo gestor de crédito)
						_cRisco := "E"
					EndIf

				    //-----------------------------+
				    // Validação Limite de crédito |
				    //-----------------------------+
				    If _nOpc == 3
				    	If !_lLoja //Novo A1_COD e A1_LOJA
							_cCliCrd := ""
				    	Else //A1_CODIGO antigo e A1_LOJA novo
				    		_aGetCred := GetCred(IIF(Empty(_cCodigo),_cCodCli,_cCodigo)) //{ A1_XCLICRD , A1_RISCO , A1_LC }
							If Len(_aGetCred) > 0
				    		    _cCliCrd  := _aGetCred[1]
				    		    _nA1LC	  := 0
				    		    _dVenc	  := CTOD('  /  /  ')
							EndIf
				    	EndIf
				    EndIf
                EndIf

				//----------------------------------+
				// Validação de duplicidade de CNPJ |
				//----------------------------------+
				If Empty(_cMsgLog)
					u_PROMM190(_oMaterial[#"A1_PESSOA"],_oMaterial[#"A1_EST"],_oMaterial[#"A1_CGC"],_cCodigo,_cLoja,.T.,@_cMsgLog)
				EndIf

				If ValType(_oMaterial[#"A1_NOME"]) <> 'U'
					If Len(_oMaterial[#"A1_NOME"]) > TamSX3("A1_NOME")[1]
						_cMsgLog := "Razão social maior que o disponível no Protheus (" + ALLTRIM(STR(TamSX3("A1_NOME")[1])) + ")"
					EndIf
				EndIf

				If !Empty(_cMsgLog)
					//--------------------+
					// Atualiza historico |
					//--------------------+
					Z12->( dbGoTo(nRecnoZ12))
					U_BzApi01d(Z12->Z12_ID,Z12->Z12_CHAVE,Z12->Z12_JSON,_cMsgLog,"3",4)

					BzApi001e(Z12->Z12_ID,Z12->Z12_CHAVE,Z12->Z12_JSON,_cMsgLog)

					//-----------------------------------------------------------+
					// Adiciona dados para realizar a baixa dos dados na Astrein |
					//-----------------------------------------------------------+
					aAdd(_aRetAstr,{_cIdAstrein,"1",_cMsgLog,""})
				Else
					//-----------------------+
					// Cria array de cliente |
					//-----------------------+
					For _nX := 1 To Len(_aStruct)

						If _aStruct[_nX][1] == "A1_COD"
							_xRet := _cCodigo
							aAdd(_aArray,{_aStruct[_nX][1], _xRet, Nil })
						ElseIf _aStruct[_nX][1] == "A1_LOJA"
							_xRet := _cLoja
							aAdd(_aArray,{_aStruct[_nX][1], _xRet, Nil })
						Endif

						If !_aStruct[_nX][1] $ _cCpoDel
							If ValType(_oMaterial[#_aStruct[_nX][1]]) <> "U"
								If RTrim(_oMaterial[#_aStruct[_nX][1]]) <> "NAO APLICAVEL"
									Do Case
										Case _aStruct[_nX][2] == "D"
											_xRet := cTod(_oMaterial[#_aStruct[_nX][1]])
										Case _aStruct[_nX][2] == "N"
											If _aStruct[_nX][1] $ "A1_LC"
												_xRet := _nA1LC
											Else
												_xRet := Val(_oMaterial[#_aStruct[_nX][1]])
											EndIf
										Case _aStruct[_nX][2] == "L"
											_xRet := IIF(SubStr(_oMaterial[#_aStruct[_nX][1]],1,1) $ "T/t", .T., .F.)
									OtherWise
										If _aStruct[_nX][1] == "A1_TIPO"
											_xRet := SubStr(_oMaterial[#_aStruct[_nX][1]],1,1)
										ElseIf _aStruct[_nX][1] == "A1_NOME"
											_xRet := AllTrim(u_BzNoAcento(_oMaterial[#_aStruct[_nX][1]]))
										ElseIf _aStruct[_nX][1] == "A1_NREDUZ"
											_xRet := Alltrim(SubStr(u_BzNoAcento(_oMaterial[#_aStruct[_nX][1]]),1,nTA1NREDUZ))
											If '*' $ _xRet .Or. Empty(_xRet)
												_xRet := AllTrim(u_BzNoAcento(Subst(_oMaterial[#"A1_NOME"],1,20)))
											EndIf
										ElseIf _aStruct[_nX][1] == "A1_SIMPLES"
											_xRet := IIF(SubStr(_oMaterial[#_aStruct[_nX][1]],1,1) == "N","2","1")
										ElseIf _aStruct[_nX][1] == "A1_SIMPNAC"
											_xRet := IIF(SubStr(_oMaterial[#_aStruct[_nX][1]],1,1) == "0","1","2")
										ElseIf _aStruct[_nX][1] == "A1_COD_MUN"
											_xRet := IIF(Len(RTrim(_oMaterial[#_aStruct[_nX][1]])) > 5, SubStr(_oMaterial[#_aStruct[_nX][1]],3,Len(RTrim(_oMaterial[#_aStruct[_nX][1]]))),_oMaterial[#_aStruct[_nX][1]])
											_cCdMun := Alltrim(SubStr(_xRet,1,nTA1COD_MUN))
										ElseIf _aStruct[_nX][1] == 'A1_XCDMUNE'
											_xRet := Right(AllTrim(_oMaterial[#_aStruct[_nX][1]]),5)
											_cCodMunE := _xRet
										ElseIf _aStruct[_nX][1] == "A1_CNAE"
											_xRet := Transform( _oMaterial[#_aStruct[_nX][1]], "@R 9999-9/99" )
										ElseIf _aStruct[_nX][1] == "A1_INSCR"
										 	If UPPER(_oMaterial[#_aStruct[_nX][1]]) == 'ISENTO'
										 		_xRet := 'ISENTO'
										 	Else
										 		_xRet := StrTran(StrTran(StrTran( _oMaterial[#_aStruct[_nX][1]], "-","" ) , ".","") , "/","")
										 	EndIf
										ElseIf _aStruct[_nX][1] == "A1_END"
											_xRet := AllTrim(u_BzNoAcento(_oMaterial[#_aStruct[_nX][1]]))
											_xRet := IIF(SubStr(_xRet,Len(_xRet)-3,Len(_xRet)) == "S/N",SubStr(_xRet,1,Len(_xRet)-6),_xRet)
										ElseIf _aStruct[_nX][1] == "A1_BAIRRO"
											_xRet := AllTrim(u_BzNoAcento(_oMaterial[#_aStruct[_nX][1]]))
										ElseIf _aStruct[_nX][1] == "A1_COMPLEM"
											_xRet := AllTrim(u_BzNoAcento(_oMaterial[#_aStruct[_nX][1]]))
											If _xRet == "********"
												_xRet := ""
											EndIf
										ElseIf _aStruct[_nX][1] == "A1_TEL"
											_xRet := u_Numeros(Alltrim(_oMaterial[#_aStruct[_nX][1]]))
											If Len(_xRet) >= 10
												_xRet := SubStr(_xRet,3,Len(_xRet))
											EndIf
										ElseIf _cPessoa == "F" .AND. _aStruct[_nX][1] == "A1_CNAE"
											_xRet := "000000"
										ElseIf _cPessoa == "F" .AND. _aStruct[_nX][1] == "A1_SATIV1"
											_xRet := "4029"
										ElseIf _aStruct[_nX][1] $ "A1_CEP*A1_CEPC*A1_XCEPCOM*A1_CEPE"
											_xRet := u_Numeros(Alltrim(_oMaterial[#_aStruct[_nX][1]]))

										Else
											_xRet := U_BZNoAcento(_oMaterial[#_aStruct[_nX][1]])
										EndIf
									EndCase
									If _aStruct[_nX][1] <> 'A1_COD' //Não considera código vindo na interface
										aAdd(_aArray,{_aStruct[_nX][1], _xRet, Nil })
									EndIf
								EndIf
								//----------------------------------------------+
								// Campos que não fazem parte do Layput Astrein |
								//----------------------------------------------+
							Else
								If _aStruct[_nX][1] == "A1_XIDASTR"
									_xRet := Val(_cIdAstrein)
									aAdd(_aArray,{_aStruct[_nX][1], _xRet, Nil })
								EndIf
							EndIf
						EndIf
					Next _nX

					If _nOpc == 3
						aAdd(_aArray,{"A1_XDTINC" , Date()   , Nil }) //Data de Inclusão do cadastro
						aAdd(_aArray,{"A1_XUSRINC", "SSA-CAD", Nil }) //Nome de Inclusão do cadastro
					Else
						aAdd(_aArray,{"A1_XDTALT" , Date()   , Nil }) //Data de Alteração do cadastro
						aAdd(_aArray,{"A1_XUSRALT", "SSA-CAD", Nil }) //Nome de Alteração do cadastro
					EndIf

					aAdd(_aArray,{"A1_VENCLC", _dVenc, Nil }) //Vencimento do limite de crédito

					If !Empty(_cRisco)
						aAdd(_aArray,{"A1_RISCO" , _cRisco, Nil })
					EndIf

					If !Empty(_cCliCrd)
						aAdd(_aArray,{"A1_XCLICRD" , _cCliCrd, Nil })
					EndIf

					If !Empty(_cBco1)
						aAdd(_aArray,{'A1_BCO1', _cBco1, Nil })
					EndIf

					//Atualização dos municipios
					CC2->(dbSetOrder(1))
					If CC2->(dbSeek(xFilial("CC2")+_cEst+_cCdMun))
						/*If Empty(_cCodMunC)
							aAdd(_aArray,{"A1_MUNC"		, CC2->CC2_MUN	, Nil })//Municipio de Cobrança
						EndIf

						If Empty(_cCodMunE)
							aAdd(_aArray,{"A1_MUNE"		, CC2->CC2_MUN	, Nil })//Municipio de Entrega
							aAdd(_aArray,{"A1_XCDMUNE"	, _cCdMun		, Nil })//Municipio de Entrega
						EndIf

						If Empty(_cCodMunCo)
							aAdd(_aArray,{"A1_XMUNCOM"	, CC2->CC2_MUN	, Nil })//Municipio de Compras
						EndIf*/
						aAdd(_aArray,{"A1_MUN"	, PadR(Alltrim(CC2->CC2_MUN),_nTA1MUN), Nil })//Municipio de Compras
					EndIf

					If !Empty(_cCodMunE)
						CC2->(dbSetOrder(1))
						If CC2->(dbSeek(xFilial("CC2")+_cEstE+_cCodMunE))
							aAdd(_aArray,{"A1_MUNE"		, PadR(Alltrim(CC2->CC2_MUN),_nTA1MUNE)	, Nil })//Municipio de Entrega
						EndIf
					EndIf


					//-----------------------+
					// Roda ExecAuto Cliente |
					//-----------------------+
					Begin Transaction
						lMsErroAuto     := .F. // .f. = sem erro, sucesso!

						MsExecAuto({|x,y| Mata030(x,y)}, _aArray, _nOpc)


						// forçar o testes pois ao passar pelo PE MA030TOK e existindo regra de validaçã específica
						// a variavel lMsErroAuto fica .T. porem ao voltar para esse programa o conteúdo já está .F.
						// novamente, passando como sucesso de execução.
						_cMsgLog := MostraErro("\system\", "BZAPI001.LOG")

						if !lMsErroAuto
							if !empty(alltrim(_cMsgLog))
								lMsErroAuto:=.T.
							endif
						EndIf

						If lMsErroAuto
							RollBackSx8()

							//------------------------------------+
							// Retorna somente a linha com o erro |
							//------------------------------------+
							_cMsgLog := _cMsgLog

							//--------------------+
							// Atualiza historico |
							//--------------------+
							Z12->( dbGoTo(nRecnoZ12))
							U_BzApi01d(Z12->Z12_ID,Z12->Z12_CHAVE,Z12->Z12_JSON,_cMsgLog,"3",4)

							BzApi001e(Z12->Z12_ID,Z12->Z12_CHAVE,Z12->Z12_JSON,_cMsgLog)

							//-----------------------------------------------------------+
							// Adiciona dados para realizar a baixa dos dados na Astrein |
							//-----------------------------------------------------------+
							aAdd(_aRetAstr,{_cIdAstrein,"1",_cMsgLog,""})

						Else

							ConfirmSx8()

							If nGrv001 == 1
								Reclock("SA1",.F.)
								SA1->A1_MSEXP := dtos(dDataBase)
								SA1->(MsUnlock())
							EndIf

							//-------------------------------------+
							// Atualiza historico do processamento |
							//-------------------------------------+
							Z12->( dbGoTo(nRecnoZ12))

							If _nOpc == 3
								//----------------------------------------+
								// Atualiza Dados da Carteira de Clientes |
								//----------------------------------------+
								If Len(_aCarteira) > 0
									ZA6->(dbGoTo(_aCarteira[1]))
									_cMsgLog := AtuCart(_cCodigo,_cLoja)

									If !Empty(_cCartExec)
										ZA6->(dbGoTo(_aCartExec[1]))
										_cMsgLog := AtuCart(_cCodigo,_cLoja)
									EndIf
								EndIf

								//-----------------------------------+
								// Atualiza Dados do Grupo de Vendas |
								//-----------------------------------+
								If Empty(_cGrpVen)
									u_BZGRPVEN(_cCodigo,_cLoja,_cCanal,_cClasse,_cSClasse)
								EndIf
								//Novo cliente, grava o histórico do limite de credito quando incluir o cliente
								NewCliSZ6(_cCodigo,_cLoja,_nA1LC,_dVenc,_cRisco)
							EndIf

							U_BzApi01d(Z12->Z12_ID,Z12->Z12_CHAVE,Z12->Z12_JSON,"","2",4)
							BzApi001e(Z12->Z12_ID,Z12->Z12_CHAVE,Z12->Z12_JSON,_cMsgLog)

							//-----------------------------------------------------------+
							// Adiciona dados para realizar a baixa dos dados na Astrein |
							//-----------------------------------------------------------+
							aAdd(_aRetAstr,{_cIdAstrein,"0","CLIENTE BAIXADO COM SUCESSO",RTrim(SA1->A1_COD) + RTrim(SA1->A1_LOJA)})
						Endif
					End Transaction
				EndIf
				//---------------------+
				// Processa fornecedor |
				//---------------------+
			ElseIf At("FORNECEDOR",_oJson[#"NewDataSet"][#"MATERIAL"][#"TIPO_ITEM"]) > 0

				_aArray     := {}
				_aErroAuto  := {}
				_aStruct    := SA2->( dbStruct() )
				_oMaterial  := _oJson[#"NewDataSet"][#"MATERIAL"]
				_oIntItem   := _oJson[#"NewDataSet"][#"INTITEM"]
				_nOpc       := 0
				_cCpj       := PadR(_oMaterial[#"A2_CGC"],_nTCGC)
				_cIdAstrein := _oIntItem[#"ITE_CODIGO"]

				//------------------------------------+
				// Valida codigo e loja do fornecedor |
				//------------------------------------+
				BzApi01For(_cCpj,_cIdAstrein,@_cCodigo,@_cLoja,@_nOpc)

				//-----------------------+
				// Cria array de cliente |
				//-----------------------+
				For _nX := 1 To Len(_aStruct)
					If ValType(_oMaterial[#_aStruct[_nX][1]]) <> "U"
						If RTrim(_oMaterial[#_aStruct[_nX][1]]) <> "NAO APLICAVEL"
							Do Case
								Case _aStruct[_nX][2] == "D"
								_xRet := cTod(_oMaterial[#_aStruct[_nX][1]])
								Case _aStruct[_nX][2] == "N"
								_xRet := Val(_oMaterial[#_aStruct[_nX][1]])
								Case _aStruct[_nX][2] == "L"
								_xRet := IIF(SubStr(_oMaterial[#_aStruct[_nX][1]],1,1) $ "T/t", .T., .F.)
								OtherWise
								If _aStruct[_nX][1] == "A2_TIPO"
									_xRet := SubStr(_oMaterial[#_aStruct[_nX][1]],1,1)
								ElseIf _aStruct[_nX][1] == "A2_SIMPLES"
									_xRet := IIF(SubStr(_oMaterial[#_aStruct[_nX][1]],1,1) == "N",2,1)
								ElseIf _aStruct[_nX][1] == "A2_COD_MUN"
									_xRet := IIF(Len(RTrim(_oMaterial[#_aStruct[_nX][1]])) > 5, SubStr(_oMaterial[#_aStruct[_nX][1]],3),_oMaterial[#_aStruct[_nX][1]])
								ElseIf _aStruct[_nX][1] == "A2_LOJA" .And. _nOpc == 3
									_xRet := _cLoja
								ElseIf _aStruct[_nX][1] == "A2_COD" .And. _nOpc == 3
									_xRet := _cCodigo
								Else
									_xRet := _oMaterial[#_aStruct[_nX][1]]
								EndIf
							EndCase
							aAdd(_aArray,{_aStruct[_nX][1], _xRet, Nil })
						EndIf
						//----------------------------------------------+
						// Campos que nï¿½o fazem parte do Layput Astrein |
						//----------------------------------------------+
					Else
						If _aStruct[_nX][1] == "A2_XIDASTR"
							_xRet := Val(_cIdAstrein)
							aAdd(_aArray,{_aStruct[_nX][1], _xRet, Nil })
						EndIf
					EndIf
				Next _nX

				//-----------------------+
				// Roda ExecAuto Cliente |
				//-----------------------+
				lMsErroAuto     := .F.
				MsExecAuto({|x,y| Mata020(x,y)}, _aArray, _nOpc)

				// forçar o testes pois ao passar pelo PE MA030TOK e existindo regra de validação específica
				// a variavel lMsErroAuto fica .T. porem ao voltar para esse programa o conteúdo já está .F.
				// novamente, passando como sucesso de execução.
				_cMsgLog := MostraErro("\system\", "BZAPI001.LOG")

				if !lMsErroAuto
					if !empty(alltrim(_cMsgLog))
						lMsErroAuto:=.T.
					endif
				EndIf

				If lMsErroAuto
					RollBackSx8()

					_cMsgLog := _cMsgLog

					//------------------------------------+
					// Retorna somente a linha com o erro |
					//------------------------------------+
					ErroAuto(_aErroAuto,@_cMsgLog)

					//--------------------+
					// Atualiza historico |
					//--------------------+
					Z12->( dbGoTo(nRecnoZ12))
					U_BzApi01d(Z12->Z12_ID,Z12->Z12_CHAVE,Z12->Z12_JSON,_cMsgLog,"3",4)

					BzApi001e(Z12->Z12_ID,Z12->Z12_CHAVE,Z12->Z12_JSON,_cMsgLog)

					//-----------------------------------------------------------+
					// Adiciona dados para realizar a baixa dos dados na Astrein |
					//-----------------------------------------------------------+
					aAdd(_aRetAstr,{_cIdAstrein,"1",_cMsgLog,""})
				Else
					Z12->( dbGoTo(nRecnoZ12))
					ConfirmSx8()
					U_BzApi01d(Z12->Z12_ID,Z12->Z12_CHAVE,Z12->Z12_JSON,"","2",4)
					//-----------------------------------------------------------+
					// Adiciona dados para realizar a baixa dos dados na Astrein |
					//-----------------------------------------------------------+
					aAdd(_aRetAstr,{_cIdAstrein,"0","FORNECEDOR PROCESSADO COM SUCESSO",RTrim(SA2->A2_COD) + RTrim(SA2->A2_LOJA)})

				EndIf
				//------------------+
				// Processa produto |
				//------------------+
			ElseIf At("PRODUTO",_oJson[#"NewDataSet"][#"MATERIAL"][#"TIPO_ITEM"]) > 0

				//--------------------------+
				// SB1 - Tabela de Produtos |
				//--------------------------+
				dbSelectArea("SB1")
				SB1->( dbSetOrder(1) )

				_aArray     := {}
				_aErroAuto  := {}
				_aStruct    := SB1->( dbStruct() )
				_aStructSB5 := SB5->( dbStruct() )
				_oMaterial  := _oJson[#"NewDataSet"][#"MATERIAL"]
				_oIntItem   := _oJson[#"NewDataSet"][#"INTITEM"]
				_nOpc       := 3
				_cCodPrd    := PadR(_oMaterial[#"B1_COD"],_nTProd)
				_cIdAstrein := _oIntItem[#"MAT_CODIGO"]
				_lSB5       := .F.

				//-------------------+
				// Posiciona produto |
				//-------------------+
				If SB1->( dbSeek(xFilial("SB1") + _cCodPrd) )
					_nOpc := 4
				EndIf

				//código
				aadd(aAutoSB1,{"B1_COD",_cCodPrd,NIL})

				//---------------------------+
				// Cria array de produto SB1 |
				//---------------------------+
				For _nX := 1 To Len(_aStruct)
					lCpo :=.f.
					If ValType(_oMaterial[#_aStruct[_nX][1]]) <> "U"
						If RTrim(_oMaterial[#_aStruct[_nX][1]]) <> "NAO APLICAVEL"
							Do Case
								Case _aStruct[_nX][2] == "D"
								_xRet := cTod(_oMaterial[#_aStruct[_nX][1]])
								Case _aStruct[_nX][2] == "N"
								_xRet := Val(_oMaterial[#_aStruct[_nX][1]])
								Case _aStruct[_nX][2] == "L"
								_xRet := IIF(SubStr(_oMaterial[#_aStruct[_nX][1]],1,1) $ "T/t", .T., .F.)
								OtherWise
								_xRet := _oMaterial[#_aStruct[_nX][1]]
							EndCase

							If _aStruct[_nX][1] == "B1_MSBLQL"
								If _nOpc <> 3
									aAdd(_aArray,{_aStruct[_nX][1], _xRet, Nil })
								EndIf
							Else
								aAdd(_aArray,{_aStruct[_nX][1], _xRet, Nil })
							EndIf

							lCpo := .t.
						EndIf
						//----------------------------------------------+
						// Campos que não fazem parte do Layout Astrein |
						//----------------------------------------------+
					Else
						If _aStruct[_nX][1] == "B1_XIDASTR"
							_xRet := Val(_cIdAstrein)
							aAdd(_aArray,{_aStruct[_nX][1], _xRet, Nil })
							lCpo :=.t.
						EndIf
					EndIf
					if alltrim(_aStruct[_nX][1]) <> "B1_COD" .AND. lCpo
						if SubStr(_aStruct[_nX][1],1,3) == "B1_"
							If _aStruct[_nX][1] == "B1_MSBLQL"
								If _nOpc <> 3
									aadd(aAutoSB1,{alltrim(_aStruct[_nX][1]), _xRet, NIL})
								EndIf
							Else
								aadd(aAutoSB1,{alltrim(_aStruct[_nX][1]), _xRet, NIL})
							EndIf
						endif
						If SubStr(_aStruct[_nX][1],1,8) == "B1_CUSTD"
							aAdd(aAutoSB5, {"B5_XCUSTD", _xRet, "U_ValidPrc()"})
						Endif
					endif
				Next _nX

				//---------------------------+
				// Cria array de produto SB5 |
				//---------------------------+
				For _nX := 1 To Len(_aStructSB5)
					lCpo :=.f.
					If ValType(_oMaterial[#_aStructSB5[_nX][1]]) <> "U"
						If RTrim(_oMaterial[#_aStructSB5[_nX][1]]) <> "NAO APLICAVEL"
							Do Case
								Case _aStructSB5[_nX][2] == "D"
								_xRet := cTod(_oMaterial[#_aStructSB5[_nX][1]])
								Case _aStructSB5[_nX][2] == "N"
								_xRet := Val(_oMaterial[#_aStructSB5[_nX][1]])
								Case _aStructSB5[_nX][2] == "L"
								_xRet := IIF(SubStr(_oMaterial[#_aStructSB5[_nX][1]],1,1) $ "T/t", .T., .F.)
								OtherWise
								_xRet := _oMaterial[#_aStructSB5[_nX][1]]
							EndCase
							aAdd(_aArray,{_aStructSB5[_nX][1], _xRet, Nil })
							lCpo := .t.
						EndIf
						//------------------------------------------------+
						// Campos que nï¿½o fazem parte do Layout Astrein |
						//------------------------------------------------+
					Else

					EndIf
					if alltrim(_aStructSB5[_nX][1]) <> "B5_COD" .AND. lCpo
						if SubStr(_aStructSB5[_nX][1],1,3) == "B5_"
							aadd(aAutoSB5,{alltrim(_aStructSB5[_nX][1]), _xRet,   Iif(SubStr(_aStructSB5[_nX][1],1,6) = "B5_PRV", "U_ValidPrc()", NIL ) })
						endif
					endif
				Next _nX

				lMsErroAuto := .F.
				dbSelectArea("SB5")
				ChkFile("SB5")
				dbSelectArea("SB1")
				ChkFile("SB1")

				oModel := FwLoadModel("MATA010")
				oModel:SetOperation(_nOpc) // (MODEL_OPERATION_INSERT)
				FWMVCRotAuto(oModel,"SB1",_nOpc,{{"SB1MASTER",aAutoSB1},{"SB5DETAIL",aAutoSB5}},,.T.)

				//oModel := FwModelActive()
				// forçar o testes pois ao passar pelo PE MA030TOK e existindo regra de validação específica
				// a variavel lMsErroAuto fica .T. porem ao voltar para esse programa o conteúdo já está .F.
				// novamente, passando como sucesso de execução.
				_cMsgLog := MostraErro("\system\", "BZAPI001.LOG")

				if !lMsErroAuto
					if !empty(alltrim(_cMsgLog))
						lMsErroAuto:=.T.
					endif
				EndIf

				If lMsErroAuto
					RollBackSx8()

					//--------------------+
					// Atualiza historico |
					//--------------------+
					Z12->( dbGoTo(nRecnoZ12))
					U_BzApi01d(Z12->Z12_ID,Z12->Z12_CHAVE,Z12->Z12_JSON,_cMsgLog,"3",4)

					BzApi001e(Z12->Z12_ID,Z12->Z12_CHAVE,Z12->Z12_JSON,_cMsgLog)

					//-----------------------------------------------------------+
					// Adiciona dados para realizar a baixa dos dados na Astrein |
					//-----------------------------------------------------------+
					aAdd(_aRetAstr,{_cIdAstrein,"1",_cMsgLog,""})
				Else
					ConfirmSx8()

					Z12->( dbGoTo(nRecnoZ12))
					U_BzApi01d(Z12->Z12_ID,Z12->Z12_CHAVE,Z12->Z12_JSON,"","2",4)
					//-----------------------------------------------------------+
					// Adiciona dados para realizar a baixa dos dados na Astrein |
					//-----------------------------------------------------------+
					aAdd(_aRetAstr,{_cIdAstrein,"0","Cad.Produto - Sucesso",RTrim(SB1->B1_COD)})

				Endif

				oModel := FwModelActive()

				If ValType(oModel) <> "U"
					oModel:Destroy()
					FreeObj(oModel)
				EndIf

			EndIf
		EndIf
		(_cAlias)->( dbSkip() )
	EndDo

	(_cAlias)->( dbCloseArea() )

	RestArea(_aArea)
Return .T.

/***********************************************************************************/
/*/{Protheus.doc} BzApi01d
@description Grava atualização na tabela de LOG
@type  User Function
@author Bernard M. Margarido
@since 03/11/2020
/*/
/***********************************************************************************/
User Function BzApi01d(_cIdProc,_cChave,_cJson,_cError,_cStatus,_nOpc,_lAtualiza)

	Local _aArea    	:= GetArea()

	Local _lRet     	:= .T.

	Local _oMonitor 	:= ProtMonitor():New()

	Default _nOpc   	:= 3
	Default _lAtualiza	:= .F.

	_oMonitor:cIdProc   := _cIdProc
	_oMonitor:cChave    := _cChave
	_oMonitor:cStatus   := _cStatus
	_oMonitor:cJSon     := _cJson
	_oMonitor:nQtdReg   := 1
	_oMonitor:nOpc      := _nOpc
	_oMonitor:lAtualiza := _lAtualiza
	_oMonitor:cFunName  := ProcName(2)


	If _oMonitor:GrvMonitor()
		_lRet := .T.
	Else
		_lRet := .F.
	EndIf

	RestArea(_aArea)
Return _lRet

/***********************************************************************************/
/*/{Protheus.doc} BzApi001e
@description Atualiza historico monitor
@type  Static Function
@author Bernard M. Margarido
@since 04/11/2020
/*/
/***********************************************************************************/
Static Function BzApi001e(_cIdProc,_cChave,_cJson,_cError)
	Local _lRet     := .T.

	Local _oMonitor := ProtMonitor():New()

	_oMonitor:cIdProc   := _cIdProc
	_oMonitor:cChave    := _cChave
	_oMonitor:cError    := _cError
	_oMonitor:cJSon     := _cJson

	If _oMonitor:GrvHistorico()
		_lRet     := .T.
	Else
		_lRet     := .F.
	EndIf

Return _lRet

/***********************************************************************************/
/*/{Protheus.doc} BzApi01Cli
@description Valida codigo e Loja do Cliente
@type  Static Function
@author Bernard M. Margarido
@since 03/11/2020
/*/
/***********************************************************************************/
Static Function BzApi01Cli(_cCpj,_cIdAstrein,_cCodigo,_cLoja,_nOpc,_cCodCli,_lLoja)

	Local _aArea    := GetArea()

	If !Empty(_cIdAstrein)
		If !BzApiCliA(_cIdAstrein,@_cCodigo,@_cLoja,@_nOpc,@_lLoja)
			BzApiCliB(_cCpj,@_cCodigo,@_cLoja,@_nOpc,_cCodCli,@_lLoja)
		EndIf
	Else
		BzApiCliB(_cCpj,@_cCodigo,@_cLoja,@_nOpc,_cCodCli,@_lLoja)
	EndIf

	RestArea(_aArea)
Return Nil

/***********************************************************************************/
/*/{Protheus.doc} BzApiCliA
@description Consulta cliente pelo ID Astrein
@type  Static Function
@author Bernard M. Margarido
@since 06/11/2020
/*/
/***********************************************************************************/
Static Function BzApiCliA(_cIdAstrein,_cCodigo,_cLoja,_nOpc,_lLoja)

	Local _lRet     := .T.
	Local _cAlias   := ""
	Local _cQuery   := ""

	_cQuery := " SELECT " + CRLF
	_cQuery += "	A1_COD, " + CRLF
	_cQuery += "	A1_LOJA " + CRLF
	_cQuery += " FROM " + CRLF
	_cQuery += "	" + RetSqlName("SA1") + " " + CRLF
	_cQuery += " WHERE " + CRLF
	_cQuery += "	A1_FILIAL = '" + xFilial("SA1") + "' AND " + CRLF
	_cQuery += "	A1_XIDASTR = '" + _cIdAstrein + "' AND " + CRLF
	_cQuery += "	D_E_L_E_T_ = '' " + CRLF

	_cAlias := MPSysOpenQuery(_cQuery)

	_lRet       := IIF(EmptY((_cAlias)->A1_COD),.F.,.T.)
	_nOpc       := IIF(EmptY((_cAlias)->A1_COD), 3, 4)
	_cCodigo    := IIF(EmptY((_cAlias)->A1_COD), "", (_cAlias)->A1_COD)
	_cLoja      := IIF(EmptY((_cAlias)->A1_LOJA), "", (_cAlias)->A1_LOJA)

	(_cAlias)->( dbCloseArea() )
Return _lRet

/***********************************************************************************/
/*/{Protheus.doc} BzApiCliB
@description Consulta Cliente pelo CNPJ
@type  Static Function
@author Bernard M. Margarido
@since 06/11/2020
/*/
/***********************************************************************************/
Static Function BzApiCliB(_cCnpj,_cCodigo,_cLoja,_nOpc,_cCodCli,_lLoja)

	dbSelectArea("SA1")
	SA1->( dbSetOrder(3) )
	//---------------+
	// Localiza CNPJ |
	//---------------+
	If SA1->( dbSeek(xFilial("SA1") + _cCnpj))
		BzApiCliC(_cCnpj,@_cCodigo,@_cLoja,@_nOpc,_cCodCli,@_lLoja)
		//--------------------+
		// Localiza Raiz CNPJ |
		//--------------------+
	ElseIf SA1->( dbSeek(xFilial("SA1") + SubStr(_cCnpj,1,8)))
		BzApiCliC(SubStr(_cCnpj,1,8),@_cCodigo,@_cLoja,@_nOpc,_cCodCli,@_lLoja)
		//--------------+
		// Novo Cliente |
		//--------------+
	Else
		_nOpc   := 3
		_cCodigo:= GetSxeNum("SA1","A1_COD")
		_cLoja  := "0000"
		SA1->( dbSetOrder(1) )
		While SA1->( dbSeek(xFilial("SA1") + _cCodigo + _cLoja) )
			ConfirmSx8()
			_cCodigo := GetSxeNum("SA1","A1_COD","",1)
		EndDo
	EndIf

Return Nil

/***********************************************************************************/
/*/{Protheus.doc} BzApiCliC
@description Realiza a consulta dos CNPJ's
@type  Static Function
@author Bernard M. Margarido
@since 06/11/2020
/*/
/***********************************************************************************/
Static Function BzApiCliC(_cCnpj,_cCodigo,_cLoja,_nOpc,_cCodCli,_lLoja)

	Local _cQuery   := ""

	_cQuery := " SELECT " + CRLF
	_cQuery += "	A1_COD CODIGO, " + CRLF
	_cQuery += "	MAX(A1_LOJA) LOJA, " + CRLF
	_cQuery += "	COUNT(*) TOTAL " + CRLF
	_cQuery += " FROM " + CRLF
	_cQuery += "	" + RetSqlName("SA1") + " " + CRLF
	_cQuery += " WHERE " + CRLF
	_cQuery += " A1_FILIAL = '" + xFilial("SA1") + "' AND " + CRLF
	_cQuery += " A1_LOJA   <> 'RENT' AND " + CRLF

	If Len(RTrim(_cCnpj)) < 14
		_cQuery += "	A1_CGC LIKE '%" + _cCnpj + "%' AND " + CRLF
	Else
		_cQuery += "	A1_CGC = '" + _cCnpj + "' AND " + CRLF
	EndIf

	If !Empty(_cCodCli)
		_cQuery += " A1_COD = '" + _cCodCli + "' AND " + CRLF
	EndIf

	_cQuery += "	D_E_L_E_T_ = '' " + CRLF
	_cQuery += " GROUP BY A1_COD "

	_cAlias     := MPSysOpenQuery(_cQuery)

	_cCodigo    := (_cAlias)->CODIGO

	(_cAlias)->( dbCloseArea() )

	_cQuery := " SELECT " + CRLF
	_cQuery += "	MAX(A1_LOJA) LOJA " + CRLF
	_cQuery += " FROM " + CRLF
	_cQuery += "	" + RetSqlName("SA1") + " " + CRLF
	_cQuery += " WHERE " + CRLF
	_cQuery += "	A1_FILIAL = '" + xFilial("SA1") + "' AND " + CRLF
	_cQuery += "	A1_COD = '" + _cCodigo + "' AND " + CRLF
	_cQuery += "	A1_LOJA <> 'RENT' AND " + CRLF
	_cQuery += "	D_E_L_E_T_ = '' " + CRLF

	_cAlias     := MPSysOpenQuery(_cQuery)

	_cLoja      := Soma1((_cAlias)->LOJA)
	_nOpc       := 3

	(_cAlias)->( dbCloseArea() )
	_lLoja := .T.

Return Nil

/***********************************************************************************/
/*/{Protheus.doc} BzApi01For
@description Valida codigo e loja do fornecedor
@type  Static Function
@author Bernard M. Margarido
@since 22/11/2020
/*/
/***********************************************************************************/
Static Function BzApi01For(_cCpj,_cIdAstrein,_cCodigo,_cLoja,_nOpc)

	Local _aArea    := GetArea()

	If !Empty(_cIdAstrein)
		If !BzApiForA(_cIdAstrein,@_cCodigo,@_cLoja,@_nOpc)
			BzApiForB(_cCpj,@_cCodigo,@_cLoja,@_nOpc)
		EndIf
	Else
		BzApiForB(_cCpj,@_cCodigo,@_cLoja,@_nOpc)
	EndIf

	RestArea(_aArea)
Return Nil

/***********************************************************************************/
/*/{Protheus.doc} BzApiForA
@description Consulta Fornecedor pelo ID Astrein
@type  Static Function
@author Bernard M. Margarido
@since 06/11/2020
/*/
/***********************************************************************************/
Static Function BzApiForA(_cIdAstrein,_cCodigo,_cLoja,_nOpc)

	Local _lRet     := .T.
	Local _cAlias   := ""
	Local _cQuery   := ""

	_cQuery := " SELECT " + CRLF
	_cQuery += "	A2_COD, " + CRLF
	_cQuery += "	A2_LOJA " + CRLF
	_cQuery += " FROM " + CRLF
	_cQuery += "	" + RetSqlName("SA2") + " " + CRLF
	_cQuery += " WHERE " + CRLF
	_cQuery += "	A2_FILIAL = '" + xFilial("SA2") + "' AND " + CRLF
	_cQuery += "	A2_XIDASTR = '" + _cIdAstrein + "' AND " + CRLF
	_cQuery += "	D_E_L_E_T_ = '' " + CRLF

	_cAlias := MPSysOpenQuery(_cQuery)

	_lRet       := IIF(EmptY((_cAlias)->A2_COD),.F.,.T.)
	_nOpc       := IIF(EmptY((_cAlias)->A2_COD), 3, 4)
	_cCodigo    := IIF(EmptY((_cAlias)->A2_COD), "", (_cAlias)->A2_COD)
	_cLoja      := IIF(EmptY((_cAlias)->A2_LOJA), "", (_cAlias)->A2_LOJA)

	(_cAlias)->( dbCloseArea() )
Return _lRet

/***********************************************************************************/
/*/{Protheus.doc} BzApiForB
@description Consulta Fornecedor pelo CNPJ
@type  Static Function
@author Bernard M. Margarido
@since 06/11/2020
/*/
/***********************************************************************************/
Static Function BzApiForB(_cCnpj,_cCodigo,_cLoja,_nOpc)

	dbSelectArea("SA2")
	SA2->( dbSetOrder(3) )
	//---------------+
	// Localiza CNPJ |
	//---------------+
	If SA2->( dbSeek(xFilial("SA2") + _cCnpj))
		BzApiForC(_cCnpj,@_cCodigo,@_cLoja,@_nOpc)
		//--------------------+
		// Localiza Raiz CNPJ |
		//--------------------+
	ElseIf SA2->( dbSeek(xFilial("SA2") + SubStr(_cCnpj,1,8)))
		BzApiForC(SubStr(_cCnpj,1,8),@_cCodigo,@_cLoja,@_nOpc)
		//--------------+
		// Novo Cliente |
		//--------------+
	Else
		_nOpc   := 3
		_cCodigo:= GetSxeNum("SA2","A2_COD")
		_cLoja  := "0000"
		SA2->( dbSetOrder(1) )
		While SA2->( dbSeek(xFilial("SA2") + _cCodigo + _cLoja) )
			ConfirmSx8()
			_cCodigo := GetSxeNum("SA2","A2_COD","",1)
		EndDo
	EndIf

Return Nil

/***********************************************************************************/
/*/{Protheus.doc} BzApiForC
@description Realiza a consulta dos CNPJ's
@type  Static Function
@author Bernard M. Margarido
@since 06/11/2020
/*/
/***********************************************************************************/
Static Function BzApiForC(_cCnpj,_cCodigo,_cLoja,_nOpc)

	Local _cQuery   := ""

	_cQuery := " SELECT " + CRLF
	_cQuery += "	A2_COD CODIGO, " + CRLF
	_cQuery += "	MAX(A2_LOJA) LOJA, " + CRLF
	_cQuery += "	COUNT(*) TOTAL " + CRLF
	_cQuery += " FROM " + CRLF
	_cQuery += "	" + RetSqlName("SA2") + " " + CRLF
	_cQuery += " WHERE " + CRLF
	_cQuery += "	A2_FILIAL = '" + xFilial("SA2") + "' AND " + CRLF

	If Len(RTrim(_cCnpj)) < 14
		_cQuery += "	A2_CGC LIKE '%" + _cCnpj + "%' AND " + CRLF
	Else
		_cQuery += "	A2_CGC = '" + _cCnpj + "' AND " + CRLF
	EndIf
	_cQuery += "	D_E_L_E_T_ = '' " + CRLF
	_cQuery += " GROUP BY A2_COD "

	_cAlias     := MPSysOpenQuery(_cQuery)

	_cCodigo    := (_cAlias)->CODIGO
	_cLoja      := Soma1((_cAlias)->LOJA)
	_nOpc       := 3

	(_cAlias)->( dbCloseArea() )

Return Nil

/***********************************************************************************/
/*/{Protheus.doc} BzApi01CQr
@description Realiza a consulta dos dados pendentes de processamento
@type  Static Function
@author Bernard M. Margarido
@since 03/11/2020
/*/
/***********************************************************************************/
Static Function BzApi01CQr(_cIdProc,_cAlias,nTentat)

	Local _cQuery   := ""

	_cQuery := " SELECT " + CRLF
	_cQuery += "    Z12_ID, " + CRLF
	_cQuery += "	Z12_CHAVE, " + CRLF
	_cQuery += "	Z12_SEQ, " + CRLF
	_cQuery += "    R_E_C_N_O_ RECNOZ12 " + CRLF
	_cQuery += " FROM " + CRLF
	_cQuery += "	" + RetSqlName("Z12") + " " + CRLF
	_cQuery += " WHERE " + CRLF
	_cQuery += "	Z12_FILIAL = '" + xFilial("Z12") + "' AND " + CRLF
	_cQuery += "	Z12_CHAVE <> '' AND " + CRLF
	_cQuery += "	Z12_ID = '" + _cIdProc + "' AND " + CRLF
	_cQuery += "	Z12_STPROC = '1' AND " + CRLF
	If nTentat > 0
		_cQuery += "	Z12_TENTAT <= " +Alltrim(Str(nTentat))+ " AND " + CRLF
	EndIf
	_cQuery += "	D_E_L_E_T_ = '' "

	_cAlias := MPSysOpenQuery(_cQuery)

	If (_cAlias)->( Eof() )
		(_cAlias)->( dbCloseArea() )
		Return .F.
	Endif

Return .T.

/*************************************************************************************/
/*/{Protheus.doc} ErroAuto
@description Tratamento da mensagem de erro ExecAuto
@type  Static Function
@author Bernard M. Margarido
@since 24/11/2020/
/*/
/*************************************************************************************/
Static Function ErroAuto(_aErroAuto,_cMsgLog)

	Local _lHelp    := .F.
	Local _lTabela  := .F.
	Local _lAjuda   := .F.
	Local _lHelpMvc := .F.

	Local _cLinha   := ""
	Local _nX       := 0

	_cMsgLog :=""
	For _nX := 1 To Len(_aErroAuto)

		_cLinha  := Upper(_aErroAuto[_nX])
		_cLinha  := StrTran( _cLinha, Chr(13), " " )
		_cLinha  := StrTran( _cLinha, Chr(10), " " )

		If SubStr( _cLinha, 1, 4 ) == 'HELP'
			_lHelp := .T.
		EndIf

		If SubStr( _cLinha, 1, 6 ) == 'TABELA'
			_lHelp   := .F.
			_lTabela := .T.
		EndIf

		If SubStr( _cLinha, 1, 5 ) == 'AJUDA'
			_lHelp   := .F.
			_lTabela := .F.
			_lAjuda  := .T.
		EndIf

		If (_lHelp .Or. _lTabela .Or. _lAjuda)
			If ( '< -- INVALIDO' $ _cLinha )
				_cMsgLog += _cLinha + CRLF
			EndIf
		EndIf

		If _lHelpMvc
			_cMsgLog += SubStr(_cLinha,83) + CRLF
		EndIf

	Next _nX

Return Nil

/*************************************************************************************/
/*/{Protheus.doc} ErroAuto
@description Verifica se existe carteira informada
@type  Static Function
@author Victor Dessunte
@since 24/11/2020/
/*/
/*************************************************************************************/

Static Function VerCart(_cCart)

	Local _cQuery   := ""
	Local _nRec		:= 0
	Local _cDepVen  := ""

	_cQuery := " SELECT " + CRLF
	_cQuery += "    R_E_C_N_O_ RECNOZA6, " + CRLF
	_cQuery += "	ZA6_DEPVEN " + CRLF
	_cQuery += " FROM " + CRLF
	_cQuery += "	" + RetSqlName("ZA6") + " " + CRLF
	_cQuery += " WHERE " + CRLF
	_cQuery += "	ZA6_COD = '" + _cCart + "' AND " + CRLF
	_cQuery += "	D_E_L_E_T_ = '' "

	_cAlias := MPSysOpenQuery(_cQuery)

	If (_cAlias)->(!Eof())
		_nRec 		:= (_cAlias)->RECNOZA6
		_cDepVen 	:= (_cAlias)->ZA6_DEPVEN
	Endif

	(_cAlias)->( dbCloseArea() )

Return {_nRec,_cDepVen}

/*************************************************************************************/
/*/{Protheus.doc} ErroAuto
@description Grava cliente na carteira
@type  Static Function
@author Victor Dessunte
@since 24/11/2020/
/*/
/*************************************************************************************/

Static Function AtuCart(_cCodigo,_cLoja)

	Local lRet		:= .F.
	Local oModel	:= Nil
	Local cMsgErro

	Local cFilAux	:= cFilAnt

	cFilant := ZA6->ZA6_FILIAL

	oModel := FwLoadModel("PROMA148")
	oModel:SetOperation(MODEL_OPERATION_INSERT)
	oModel:Activate()

	oModel:SetValue("ZA7MASTER"	, "ZA7_CLIENT"	, _cCodigo	)
	oModel:SetValue("ZA7MASTER"	, "ZA7_LOJA"	, _cLoja	)

	If oModel:VldData()
		lRet := oModel:CommitData()
	EndIf

	If !lRet
		cMsgErro := MODXERR(oModel)
	EndIf

	oModel := FwModelActive()

	If ValType(oModel) <> "U"
		oModel:Destroy()
		FreeObj(oModel)
	EndIf

	cFilAnt := cFilAux

Return cMsgErro

//-------------------------------------------------------------------
/*{Protheus.doc} MODXERR
Retorna a Mensagem de Erro do Model

@author Guilherme.Santos
@since 27/06/2018
@version 12.1.17
@param oModMsg, Objeto, Objeto de onde sera Retornada a Mensagem de Erro
@return cRetorno, Caracter, Mensagem de Erro do Objeto
*/
//-------------------------------------------------------------------
Static Function MODXERR(oModMsg)

	Local aMsgErro 	:= oModMsg:GetErrorMessage()
	Local cRetorno 	:= ""
	Local nI			:= 0

	For nI := 1 to Len(aMsgErro)
		Do Case
			Case ValType(aMsgErro[nI]) == "C"
			cRetorno += aMsgErro[nI] + Space(1)
			Case ValType(aMsgErro[nI]) == "N"
			cRetorno += AllTrim(Str(aMsgErro[nI])) + Space(1)
		EndCase
	Next nI

Return cRetorno

/*/{Protheus.doc} GetCred
	(long_description)
	@type  Static Function
	@author Victor Dessunte
	@since 17/06/2021
	@version version
	@param param_name, param_type, param_descr
	@return return_var, return_type, return_description
	@example
	(examples)
	@see (links_or_references)
	/*/
Static Function GetCred(_cCod)

	Local _cQuery 	:= ""
	Local _aRet		:= {}	//Alterei de caracter para array para retornar o LC e o RISCO. Solicitado Victor. Tanimoto 30/08/2021
	_cQuery := " SELECT "
	_cQuery += " 	TOP 1 A1_RISCO, A1_LC, CONCAT(A1_COD,A1_LOJA) XCLICRD"
	_cQuery += " FROM "
	_cQuery +=  	RetSqlName("SA1")
	_cQuery += " WHERE "
	_cQuery += " 	D_E_L_E_T_	= '' "
	_cQuery += " AND A1_COD		= '" + _cCod + "' AND A1_MSBLQL<>'1' "	//Condição dos nao bloqueados. Tanimoto 30/08/2021
	_cQuery += " ORDER BY A1_LC DESC " //Do maior limite para o menor. Alteração solicitada pelo Victor. Tanimoto 30/08/2021

	_cAlias     := MPSysOpenQuery(_cQuery)

	If (_cAlias)->(!EOF())
		_aRet := {(_cAlias)->XCLICRD,(_cAlias)->A1_RISCO,(_cAlias)->A1_LC}
	EndIf

	(_cAlias)->(dbCloseArea())

Return _aRet

/*/{Protheus.doc} GetCred
	Quando incluir um cliente novo (codogo e loja), grava o historico do limite de credito na tabela de historico SZ6
	@type  Static Function
	@author Tanimoto
	@since 30/08/2021
/*/
Static Function NewCliSZ6(_cCodigo,_cLoja,_nA1LC,_dVenc,_cRisco)

	RecLock("ZE1", .T.)
	ZE1->ZE1_FILIAL := xFilial("ZE1")
	ZE1->ZE1_CLIENT := _cCodigo
	ZE1->ZE1_LOJA   := _cLoja
	ZE1->ZE1_DATA   := Date()
	ZE1->ZE1_USUARI := 'SSA-CAD'
	ZE1->ZE1_LC     := _nA1LC
	ZE1->ZE1_VENCLC := _dVenc
	ZE1->ZE1_RISCO  := _cRisco
	ZE1->(msUnLock())

Return(Nil)
