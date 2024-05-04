#INCLUDE "PROTHEUS.CH"
#INCLUDE "APWEBSRV.CH"
#INCLUDE "TOPCONN.CH"
#INCLUDE "TBICONN.CH"

#DEFINE CRLF CHR(13) + CHR(10)

Static cCodInt	:= "010"
Static cDescInt	:= "CLIENTES"
Static cDirImp	:= "/ecommerce/"

/**************************************************************************************************
{Protheus.doc} AECOI010
	@description	Rotina realiza a integração de clientes.
	@type   		Function 
	@author			Robson Oliveira
	@version   		1.00
	@since     		17/02/2016
**************************************************************************************************/
User Function AECOI010()
	
	Private cThread	:= Alltrim(Str(ThreadId()))
	Private cStaLog	:= "0"
	Private cArqLog	:= ""	

	Private nQtdInt	:= 0

	Private cHrIni	:= Time()
	Private dDtaInt	:= Date()

	Private aMsgErro:= {}
	
	//------------------------------+
	// Inicializa Log de Integracao |
	//------------------------------+
	MakeDir(cDirImp)
	cArqLog := cDirImp + "CLIENTES" + cEmpAnt + cFilAnt + ".LOG"
	ConOut("")	
	LogExec(Replicate("-",80))
	LogExec("INICIA INTEGRACAO DE CLIENTES COM A RAKUTEN - DATA/HORA: "+DTOC(DATE())+" AS "+TIME())

	//-----------------------------------------+
	// Inicia processo de envio das categorias |
	//-----------------------------------------+
	Processa({|| AECOINT10() },"Aguarde...","Consultando clientes a serem importados.")

	LogExec("FINALIZA INTEGRACAO DE CLIENTES COM A RAKUTEN - DATA/HORA: " + DTOC(DATE()) + " AS " + TIME())
	LogExec(Replicate("-",80))
	ConOut("")

	//----------------------------------+
	// Envia e-Mail com o Logs de Erros |
	//----------------------------------+
	If Len(aMsgErro) > 0
		cStaLog := "1"
		u_AEcoMail(cCodInt,cDescInt,aMsgErro)
	EndIf
	
Return Nil


Static Function AECOINT10()

	Local aArea			:= GetArea()
	Local cCnpj			:= ""
	Local cCpf			:= ""
	Local cCodCli		:= ""
	Local cTipo			:= ""
	Local cLoja			:= ""
	Local cStatus		:= ""
	Local cDtaNasc		:= ""
	Local cNomeCli		:= ""
	Local cContrib		:= "2"
	Local cInscrEst		:= ""
	Local cPais			:= GetNewPar("MV_WSPAIS","105")
	Local cCodPais		:= GetNewPar("MV_WSCPAIS","01058")
		
	Local nOpcA			:= 0
	Local nValorFixo	:= 0
	Local nX1			:= 0
	
	Local aEndSa1		:= {}
	Local aRetMun		:= {}
	Local aCliente		:= {}
	Local lCliente		:= .F.
	Local lContato		:= .F.  
	Local lEcCliCpo		:= ExistBlock("ECADDCPO")
	Local aRet			:= {,,}

	Private oWsCliente
	Private lMsErroAuto := .F.

	//-------------------------------------------+
	// Instancia objeto de Integração E-Commerce |
	//-------------------------------------------+
	oWsCliente :=  WSCliente():New

	//-------------------+
	// Codigo Loja Ikeda |
	//-------------------+      
	oWsCliente:_Url			:= AllTrim(GetMV("EC_URLECOM"))+"cliente.asmx?"
	oWsCliente:nLojaCodigo 	:= nValorFixo

	//-------------------------------------+
	// Aciona Metodo ListarNovas Clientes. | 
	//-------------------------------------+
	If oWsCliente:ListarNovos()

		If oWsCliente:oWsListarNovosResult:nCodigo == 1

			ProcRegua(Len(oWsCliente:oWsListarNovosResult:oWsLista:oWsClsUsuario))

			For nX1 := 1 To Len(oWsCliente:oWsListarNovosResult:oWsLista:oWsClsUsuario)

				IncProc("Processando ..... " + Alltrim(Str(nX1)) + " / " + Alltrim(Str(Len(oWsCliente:oWsListarNovosResult:oWsLista:oWsClsUsuario))) )

				If ValType(oWsCliente:oWsListarNovosResult:oWsLista:oWsClsUsuario) <> "A"
				
					//------------+
					// Msg CoNout |
					//------------+
					LogExec("NAO EXISTEM NOVOS CLIENTES CADASTRADOS.")
					MsgStop("NAO EXISTEM NOVOS CLIENTES CADASTRADOS.","Atenção!")
					RestArea(aArea)
					Return(.T.)
				Endif	 

				//--------------+
				// Reseta Array |
				//--------------+
				aCliente 	:= {}
				lMsErroAuto := .F.

				//-----------------------------+
				// Inicia atualização clientes |
				//-----------------------------+
				cNomeCli	:= u_SyAcento(	Alltrim(oWsCliente:oWsListarNovosResult:oWsLista:oWsClsUsuario[nX1]:cNome) + " " + ;
											Alltrim(oWsCliente:oWsListarNovosResult:oWsLista:oWsClsUsuario[nX1]:cSobrenome), .T.)

				cCnpj 		:= 	oWsCliente:oWsListarNovosResult:oWsLista:oWsClsUsuario[nX1]:oWsConta:cCnpj
				cCpf  		:= 	oWsCliente:oWsListarNovosResult:oWsLista:oWsClsUsuario[nX1]:oWsConta:cCpf
				cCodCli 	:= 	oWsCliente:oWsListarNovosResult:oWsLista:oWsClsUsuario[nX1]:oWsConta:cContaCodigoInterno
				cTipo		:= 	SubStr(oWsCliente:oWsListarNovosResult:oWsLista:oWsClsUsuario[nX1]:oWsConta:oWsTipo:Value,7,1)
				cStatus		:= 	SubStr(oWsCliente:oWsListarNovosResult:oWsLista:oWsClsUsuario[nX1]:oWsConta:oWsContaStatus:Value,1,1)
				cDtaNasc	:= 	SubStr(Alltrim(oWsCliente:oWsListarNovosResult:oWsLista:oWsClsUsuario[nX1]:oWsConta:cDataNascimento),9,2)	+ "/" + ;
								SubStr(Alltrim(oWsCliente:oWsListarNovosResult:oWsLista:oWsClsUsuario[nX1]:oWsConta:cDataNascimento),6,2) 	+ "/" + ;
								SubStr(Alltrim(oWsCliente:oWsListarNovosResult:oWsLista:oWsClsUsuario[nX1]:oWsConta:cDataNascimento),1,4)	         


				//----------------------------------------------------------+
				// Valida se Cliente já existe na base de dados do Protheus |
				//----------------------------------------------------------+
				SA1->( dbSetOrder(3) )
				If SA1->( dbSeek(xFilial("SA1") + PadR(Iif(!Empty(cCnpj),cCnpj,cCpf),TamSx3("A1_CGC")[1],"")) )
					nOpcA := 4
					cCodCli := SA1->A1_COD
					cLoja	:= SA1->A1_LOJA
				Else
					nOpcA := 3
					cCodCli := ""
					cLoja	:= ""  
					u_SyVldCodCli(@cCodCli, @cLoja, Iif(!Empty(cCnpj),cCnpj,cCpf))
				EndIf

				//--------------------------------+
				// Adiciona ao Array de Clientes. |
				//--------------------------------+
				aAdd(aCliente , {"A1_FILIAL"	, xFilial("SA1")																										, Nil	})
				aAdd(aCliente , {"A1_COD" 		, cCodCli																												, Nil	})
				aAdd(aCliente , {"A1_LOJA" 		, cLoja																													, Nil	})
				aAdd(aCliente , {"A1_CONTAEC"	, Alltrim(oWsCliente:oWsListarNovosResult:oWsLista:oWsClsUsuario[nX1]:oWsConta:cContaCodigoInterno)						, Nil	})
				aAdd(aCliente , {"A1_TIPO"		, IIF(Upper(cTipo) == "F","F","R")																						, Nil	})
				aAdd(aCliente , {"A1_PESSOA"	, Upper(cTipo)																											, Nil	})

				//------------------+
				// Insere endereços |
				//------------------+
				aEndSa1 := GrvZa0(.F.,nX1,cCodCli,cLoja)
				
				//-----------------------------+
				// Retorna Codigo de Municipio |
				//-----------------------------+	
				aRetMun := u_RetCodMun(Alltrim(aEndSa1[1,3]),Alltrim(aEndSa1[1,4]))	

				//---------------------------+
				// Valida Inscrição Estadual |
				//---------------------------+
				cIe := Alltrim(UPPER(oWsCliente:oWsListarNovosResult:oWsLista:oWsClsUsuario[nX1]:oWsConta:cIE))
				cEst:= Upper(Alltrim(aEndSa1[1,3]))

				If !IE(cIe,cEst,.F.)
					//----------------+
					// Array de Error |
					//----------------+
					aAdd(aMsgErro,{IIF(Empty(cCnpj),cCpf,cCnpj),;
					"Inscrição Estadual " + cIe  + " inválida para o estado " + cEst + ". Não foi possivel inserir o cliente no Protheus."})
					Loop			
				EndIf

				//------------------------------------+
				// Codigo de Municipio nao encontrado |
				//------------------------------------+
				If Empty(aRetMun[1])
					//----------------+
					//³Array de Error |
					//----------------+
					aAdd(aMsgErro,{IIF(Empty(cCnpj),cCpf,cCnpj),;
					"Não existe codigo do municipio para " + Alltrim(aEndSa1[1,4]) + ". Não foi possivel encontrar o codigo de municipio na tabela CC2"})
					Loop				
				EndIf

				//-------------------+
				// Tabela Municipios |
				//-------------------+
				CC2->( dbGoTop() )
				CC2->( dbSetOrder(1) ) 

				aAdd(aCliente , {"A1_END"		, u_SyAcento(Alltrim(aEndSa1[1,22]) + " " + Alltrim(aEndSa1[1,1]),.T.) +", " + Alltrim(aEndSa1[1,2])	, Nil })
				aAdd(aCliente , {"A1_EST"		, Upper(Alltrim(aEndSa1[1,3]))																			, Nil })
				aAdd(aCliente , {"A1_COD_MUN"	, PadR(aRetMun[1],TamSx3("A1_COD_MUN")[1],"")															, Nil })
				aAdd(aCliente , {"A1_MUN"		, u_SyAcento(Alltrim(aRetMun[2]),.T.)																	, Nil })
				aAdd(aCliente , {"A1_BAIRRO"	, u_SyAcento(Alltrim(aEndSa1[1,5]),.T.)																	, Nil })
				aAdd(aCliente , {"A1_CEP"		, aEndSa1[1,6]												 											, Nil })
				aAdd(aCliente , {"A1_DDD"		, aEndSa1[1,7]																							, Nil })
				aAdd(aCliente , {"A1_TEL"		, aEndSa1[1,8] 																							, Nil })  	
				aAdd(aCliente , {"A1_DDD1"		, aEndSa1[1,7]			 																				, Nil })
				aAdd(aCliente , {"A1_TEL1"		, aEndSa1[1,8]																							, Nil })
				aAdd(aCliente , {"A1_RAMAL1"	, aEndSa1[1,9]																							, Nil })
				aAdd(aCliente , {"A1_DDD2"		, aEndSa1[1,10]																							, Nil })
				aAdd(aCliente , {"A1_TEL2"		, aEndSa1[1,11]																							, Nil })
				aAdd(aCliente , {"A1_RAMAL2"	, aEndSa1[1,12]																							, Nil })
				aAdd(aCliente , {"A1_DDD3"		, aEndSa1[1,13]																							, Nil })
				aAdd(aCliente , {"A1_TEL3"		, aEndSa1[1,14]																							, Nil })
				aAdd(aCliente , {"A1_RAMAL3"	, aEndSa1[1,15]																							, Nil })
				aAdd(aCliente , {"A1_DDDCEL"	, aEndSa1[1,16]																							, Nil })
				aAdd(aCliente , {"A1_CEL"		, aEndSa1[1,17]																							, Nil })
				aAdd(aCliente , {"A1_DDDFX"		, aEndSa1[1,18]																							, Nil })
				aAdd(aCliente , {"A1_FAX"		, aEndSa1[1,19]																							, Nil })
				aAdd(aCliente , {"A1_VLDBXCL"	, "2"																									, Nil })
				aAdd(aCliente , {"A1_EMAIL"		, oWsCliente:oWsListarNovosResult:oWsLista:oWsClsUsuario[nX1]:cEmail									, Nil })
				aAdd(aCliente , {"A1_COMPLEM"	, aEndSa1[1,24]																							, Nil })
				aAdd(aCliente , {"A1_REFEREN"	, aEndSa1[1,21]																							, Nil })
				aAdd(aCliente , {"A1_RISCO"		, "A"																									, Nil })
				aAdd(aCliente , {"A1_PAIS"		, cPais	  																								, Nil })
				aAdd(aCliente , {"A1_CODPAIS"	, cCodPais																								, Nil })				

				//-----------------------+
				// Tipo de Pessoa Fisica |
				//-----------------------+
				If cTipo == "F"
					aAdd(aCliente , {"A1_NOME"		, Alltrim(cNomeCli)																								, Nil })
					aAdd(aCliente , {"A1_NREDUZ"	, u_SyAcento(Alltrim(oWsCliente:oWsListarNovosResult:oWsLista:oWsClsUsuario[nX1]:cNome),.T.)					, Nil })
					aAdd(aCliente , {"A1_RG"		, Alltrim(oWsCliente:oWsListarNovosResult:oWsLista:oWsClsUsuario[nX1]:oWsConta:cRg)						   		, Nil })
					aAdd(aCliente , {"A1_PFISICA"	, oWsCliente:oWsListarNovosResult:oWsLista:oWsClsUsuario[nX1]:oWsConta:CRG										, Nil })
				
				Else
					//-------------------------+
					// Tipo de Pessoa Juridica |
					//-------------------------+
					cInscrEst := Alltrim(UPPER(oWsCliente:oWsListarNovosResult:oWsLista:oWsClsUsuario[nX1]:oWsConta:cIE))

					If !Empty(cInscrEst) .And. cInscrEst <> "ISENTO"  
						cContrib := "1" 
					EndIf

					aAdd(aCliente , {"A1_NOME"		, u_SyAcento(Alltrim(oWsCliente:oWsListarNovosResult:oWsLista:oWsClsUsuario[nX1]:oWsConta:cRazaoSocial),.T.)	, Nil })
					aAdd(aCliente , {"A1_NREDUZ"	, u_SyAcento(Alltrim(oWsCliente:oWsListarNovosResult:oWsLista:oWsClsUsuario[nX1]:oWsConta:cNomeFantasia),.T.)	, Nil })
					aAdd(aCliente , {"A1_INSCR"		, Alltrim(UPPER(oWsCliente:oWsListarNovosResult:oWsLista:oWsClsUsuario[nX1]:oWsConta:cIE))						, "AllWaysTrue()" })
					aAdd(aCliente , {"A1_INSCRM"	, Alltrim(oWsCliente:oWsListarNovosResult:oWsLista:oWsClsUsuario[nX1]:oWsConta:cIM)								, Nil })
					
				EndIf

				aAdd(aCliente , {"A1_CGC"		, Iif(!Empty(cCnpj),cCnpj,cCpf) 																		, Nil })
				aAdd(aCliente , {"A1_DTNASC"	, cToD(cDtaNasc)																						, Nil })
				aAdd(aCliente , {"A1_MSBLQL"	, Iif(cStatus == "A" ,"2","1")																			, Nil })
				aAdd(aCliente , {"A1_DTINCL"	, dTos(dDataBase)																						, Nil })
				aAdd(aCliente , {"A1_CONTRIB"	, cContrib																								, Nil })

				//-------------------------------------+
				// Ponto de Entrada para adcionar      |
				// novos campos na gravação do cliente |
				//-------------------------------------+
				If lEcCliCpo
					aCliente := ExecBlock("ECADDCPO",.F.,.F.,{oWsCliente:oWsListarNovosResult:oWsLista:oWsClsUsuario[nX1],aCliente})
				EndIf
				
				//-------------------+
				// ExecAuto Cliente. |
				//-------------------+
				If Len(aCliente) > 0 
					
					MsExecAuto({|x,y| MATA030(x,y)}, aCliente,nOpcA)

					//---------------------+
					// Erro na Atualização |
					//---------------------+
					If lMsErroAuto
					
						RollBackSx8()
						
						MakeDir("/erros/")
						cArqLog := "SA1" + cCodCli + " " + cLoja + " " + DToS(dDataBase)+Left(Time(),2)+SubStr(Time(),4,2)+Right(Time(),2)+".LOG"
						MostraErro("/erros/",cArqLog)
						DisarmTransaction()

						//------------------------------------------------+
						// Adiciona Arquivo de log no Retorno da resposta |
						//------------------------------------------------+
						cMsgErro := ""
						nHndImp  := FT_FUSE("/erros/"+cArqLog)

						If nHndImp >= 1
						
							//-----------------------------+
							// Posiciona Inicio do Arquivo |
							//-----------------------------+
							FT_FGOTOP()

							While !FT_FEOF()
								cLiArq := FT_FREADLN()
								If Empty(cLiArq)
									FT_FSKIP(1)
									Loop
								EndIf
								cMsgErro += cLiArq + CRLF
								FT_FSKIP(1)
							EndDo
							FT_FUSE()
						EndIf                                   

						//------------+
						// Msg CoNout |
						//------------+
						LogExec("ERRO AO " + Iif(nOpcA == 3,"INCLUIR","ALTERAR") + " CLIENTE. VERIFICAR O ARQUIVO " + cArqLog + " .")

						aRet[1] := .F.
						aRet[2] := "1"
						aRet[3] := "Erro ao " + Iif(nOpcA == 3,"incluir","alterar") + " cliente."

						aAdd(aMsgErro,{	IIF(Empty(cCnpj),cCpf,cCnpj),;
										"Erro ao " + Iif(nOpcA == 3,"incluir","alterar") + " cliente." + CRLF + cMsgErro})

					Else
						//-----------------------------+
						// Reseta variavel da ExecAuto |
						//-----------------------------+
						lMsErroAuto	:= .F.
						lCliente	:= .T.
						
						ConfirmSx8()

						//------------+
						// Msg CoNout |
						//------------+
						LogExec("CLIENTE INCLUIDO COM SUCESSO.") 

						//---------------+
						// Grava Contato |
						//---------------+
						GrvContat(cCodCli,cLoja,oWsCliente:oWsListarNovosResult:oWsLista:oWsClsUsuario[nX1])
						
						//-----------------+
						// Baixa o cliente | 
						//-----------------+
						EnvBxCli(cCodCli, oWsCliente:OWSLISTARNOVOSRESULT:OWSLISTA:OWSCLSUSUARIO[nX1]:CEMAIL)

						aRet[1] := .T.
						aRet[2] := "0"
						aRet[3] := "CLIENTE INCLUIDO COM SUCESSO.  " + Iif(Empty(cCnpj),cCpf,cCnpj)   

					EndIf

				EndIf

				//------------------------+
				//³Reseta Flags eCommerce |
				//------------------------+
				lCliente := .F.
				lContato := .F.	

			Next nX

		Else
			//------------+
			// Msg CoNout |
			//------------+
			LogExec("NAO EXISTEM NOVOS CLIENTES A SEREM IMPORTADOS.")
			MsgStop("NAO EXISTEM NOVOS CLIENTES A SEREM IMPORTADOS.","Atenção!")
		Endif
	Else
	
		//------------+
		// Msg CoNout |
		//------------+
		LogExec("ERRO AO COMUNICAR COM O SITE " + Alltrim(GetWSCError()))
		Aviso("Problema de Conexão com a Plataforma.",Alltrim(GetWSCError()),{"Ok"},3)
			
	EndIf

	RestArea(aArea)
Return(aRet)

/***************************************************************************************
{Protheus.doc} GrvContat

@description Rotina realiza a gravação do contato 

@author Robson A. Oliveira
@since 18/08/2016

@version undefined

@param cCodCli		, Codigo do Cliente
@param cLoja		, Loja do Cliente
@param oCliente		, Objeto contendo dados do Cliente
@type function
****************************************************************************************/
Static Function GrvContat(cCodCli,cLoja,oCliente)
	Local aArea		:= GetArea()

	Local nRecnoSU5	:= 0
	Local nEnd		:= 0
	
	Local aEndCont	:= {}
	Local aEndEnt	:= {}
	
	//-----------------------------------+
	// Validas se Objeto é do tipo Array |
	//-----------------------------------+
	If ValType(oCliente:oWsEnderecos:oWsClsEndereco) == "A"
		
		For nEnd := 1 To Len(oCliente:oWsEnderecos:oWsClsEndereco)

			//------------------+
			// Reseta variaveis |
			//------------------+
			aEndCont	:= {}
			aEndEnt		:= {}

			//----------------------------------------+
			// Valida se já existe contato cadastrado |
			//----------------------------------------+
			cCep		:= u_SyAcento(oCliente:oWsEnderecos:oWsClsEndereco[nEnd]:cCep,.T.)
			cNome		:= oCliente:oWsEnderecos:oWsClsEndereco[nEnd]:cNome
			cEnd		:= u_SyAcento(oCliente:oWsEnderecos:oWsClsEndereco[nEnd]:cLogradouro,.T.)
			cNumero		:= oCliente:oWsEnderecos:oWsClsEndereco[nEnd]:cNumero

			nRecnoSU5 	:= QryContato(cCep,cNome,cEnd,cNumero)
			
			//----------------------------+
			// Posiciona dados do contato |
			//----------------------------+
			If nRecnoSU5 > 0

				SU5->( dbGoTo(nRecnoSU5) )
				cNumSU5 := SU5->U5_CODCONT
				RecLock("SU5",.F.)

			Else
				dbSelectArea("SU5")
				SU5->( dbSetOrder(1) )

				cNumSU5 := GetSxeNum("SU5","U5_CODCONT")
				While SU5->( dbSeek(xFilial("SU5") + cNumSU5 ) )
					ConfirmSx8()
					cNumSU5 := GetSxeNum("SU5","U5_CODCONT")
				EndDo
				RecLock("SU5",.T.)

			EndIf
			
			//--------------------+
			// Adiciona endereços | 
			//--------------------+	
			AddEndEco(oCliente:oWsEnderecos:oWsClsEndereco[nEnd],@aEndCont,@aEndEnt)
			
			//------------------+
			// Atualiza Contato |
			//------------------+	
			SU5->U5_FILIAL	:= xFilial("SU5")
			SU5->U5_CODCONT := cNumSU5
			SU5->U5_CONTAT  := oCliente:oWsEnderecos:oWsClsEndereco[nEnd]:cNome
			SU5->U5_EMAIL   := oCliente:cEmail 
			SU5->U5_CPF		:= oCliente:oWsConta:cCpf
			SU5->U5_END		:= IIF(Len(aEndCont) > 0 ,aEndCont[1][3] + ", " + aEndCont[1][4] ,aEndEnt[1][3] + ", " + aEndEnt[1][4])
			SU5->U5_BAIRRO	:= IIF(Len(aEndCont) > 0 ,aEndCont[1][5],aEndEnt[1][5])
			SU5->U5_MUN		:= IIF(Len(aEndCont) > 0 ,aEndCont[1][6],aEndEnt[1][6])
			SU5->U5_EST		:= IIF(Len(aEndCont) > 0 ,aEndCont[1][8],aEndEnt[1][8])
			SU5->U5_CEP		:= IIF(Len(aEndCont) > 0 ,aEndCont[1][7],aEndEnt[1][7])
			SU5->U5_DDD		:= IIF(Len(aEndCont) > 0 ,aEndCont[1][11],aEndEnt[1][11])
			SU5->U5_FONE	:= IIF(Len(aEndCont) > 0 ,aEndCont[1][13],aEndEnt[1][13])
			SU5->U5_CELULAR := IIF(Len(aEndCont) > 0 ,aEndCont[1][14],aEndEnt[1][14])
			SU5->U5_ATIVO	:= IIF(oCliente:oWsConta:oWsContaStatus:Value == "Ativo","1","2")
			
			SU5->(MsUnLock())

			//-----------------------------+
			// Amarração Contato X Cliente |
			//-----------------------------+
			dbSelectArea("AC8")
			AC8->( dbSetOrder(1) )
			If !AC8->( dbSeek(xFilial("AC8") + cNumSU5 + "SA1" + xFilial("SA1") + cCodCli + cLoja ) )
				RecLock("AC8",.T.)
					AC8->AC8_FILIAL := xFilial("AC8")
					AC8->AC8_FILENT	:= xFilial("SA1")
					AC8->AC8_ENTIDA := "SA1"
					AC8->AC8_CODENT	:= cCodCli + cLoja
					AC8->AC8_CODCON	:= SU5->U5_CODCONT
				AC8->( MsUnLock() )	
			EndIf 
			
			//-------------------+
			// Atualiza Endereço |
			// Contato 			 |
			//-------------------+	
			If Len(aEndCont) > 0
				aEcI10Aga(cNumSU5,aEndCont)
			EndIf
			
			//-------------------+
			// Atualiza Endereço |
			// Entrega			 |
			//-------------------+	
			If Len(aEndEnt) > 0
				aEcI10Aga(cNumSU5,aEndEnt)
			EndIf	
			
		Next nEnd	
	EndIf

	RestArea(aArea)
Return .T.

/*****************************************************************************
{Protheus.doc} AddEndEco

@description Adicona endereços e-Commerce para os Contatos

@author Bernard M. Margarido
@since 18/08/2016
@version undefined

@param oEndereco		, Objeto contendo o endereço
@param aEndCont			, Array para endereço de contato
@param aEndEnt			, Array para endereço de entrega

@type function
*****************************************************************************/
Static Function AddEndEco(oEndereco,aEndCont,aEndEnt)

	If oEndereco:oWsFinalidade:Value == "Contato"
		aEndCont := LoadEnd(oEndereco) 	
	ElseIf oEndereco:oWsFinalidade:Value == "Entrega"
		aEndEnt :=	LoadEnd(oEndereco)
	EndIf

Return Nil

/********************************************************************************
{Protheus.doc} LoadEnd

@description Rotina realzia a gravação do endereço no Array

@author Bernard M. Margarido
@since 18/08/2016
@version undefined

@param oEnderecos		, Objeto contendo o endereço

@type function
*********************************************************************************/
Static Function LoadEnd(oEnderecos)
	Local aEnd := {}

	aAdd(aEnd,{	oEnderecos:nEnderecoCodigo				,;	// 01. Endereco Codigo
	u_SyAcento(oEnderecos:oWsTipoLogradouro:Value,.T.)	,;	// 02. Tipo de Logradouro
	u_SyAcento(oEnderecos:cLogradouro,.T.)				,;	// 03. Endereço
	oEnderecos:cNumero									,;	// 04. Numero
	u_SyAcento(oEnderecos:cBairro,.T.)					,;	// 05. Bairro
	u_SyAcento(oEnderecos:cCidade,.T.)					,;	// 06. Cidade
	oEnderecos:cCep										,;	// 07. Cep
	u_SyAcento(oEnderecos:cEstado,.T.)					,;	// 08. Estado
	u_SyAcento(oEnderecos:cComplemento,.T.)				,;	// 09. Complemento
	u_SyAcento(oEnderecos:cReferencia,.T.)				,;	// 10. Referencia
	oEnderecos:cDdd1									,;	// 11. Ddd
	oEnderecos:cDddCelular								,; 	// 12. Ddd Celular
	u_SyAcento(oEnderecos:cTelefone1,.T.)				,; 	// 13. Telefone 1
	u_SyAcento(oEnderecos:cCelular,.T.)					,;	// 14. Celular
	u_SyAcento(oEnderecos:cNome,.T.)					,;	// 15. cNome
	oEnderecos:oWsTipo:Value							;	// 16. Tipo de Endereço			
	})
Return aEnd

/***********************************************************************************
{Protheus.doc} aEcI10Aga

@description Rotina realzia a gravação dos endereços na tabela AGA

@author Bernard M. Margarido
@since 18/08/2016
@version undefined

@param cNumSU5			, Codigo do Contato
@param aEndereco		, Array contendo o endereço a ser gravado

@type function
***********************************************************************************/
Static Function aEcI10Aga(cNumSU5,aEndereco)
Local aArea	:= GetARea()
Local nCont	:= 0

	For nCont := 1 To Len(aEndereco)
		lGrava := QryEnd(cNumSU5,aEndereco[nCont][1])
		If lGrava
			cCodEnd := GetSxeNum("AGA","AGA_CODIGO")
			While AGA->( dbSeek(xFilial("AGA") + cCodEnd ) )
				ConfirmSx8()
				cCodEnd := GetSxeNum("AGA","AGA_CODIGO")
			EndDo

			aRetMun := U_RetCodMun(aEndereco[nCont][8],aEndereco[nCont][6])

			RecLock("AGA", lGrava)
				AGA->AGA_FILIAL := xFilial("AGA")
				AGA->AGA_CODIGO	:= cCodEnd
				AGA->AGA_ENTIDA	:= "SU5"					
				AGA->AGA_CODENT	:= cNumSU5
				AGA->AGA_XCODEN	:= Alltrim(Str(aEndereco[nCont][1]))				
				AGA->AGA_TIPO 	:= IIF(aEndereco[nCont][16] == "Comercial","1","2")
				AGA->AGA_PADRAO	:= "1"
				AGA->AGA_XTIPO	:= "1"
				AGA->AGA_END	:= aEndereco[nCont][3] + ", " + aEndereco[nCont][4] 
				AGA->AGA_BAIRRO	:= aEndereco[nCont][5]
				AGA->AGA_MUNDES	:= aEndereco[nCont][6]
				AGA->AGA_MUN    := PadR(aRetMun[1],TamSx3("A1_COD_MUN")[1],"")
				AGA->AGA_EST	:= aEndereco[nCont][8]
				AGA->AGA_CEP	:= aEndereco[nCont][7]
				AGA->AGA_PAIS	:= "105"  
			AGA->(MsUnLock())
		EndIf	
	Next nCont
	
RestArea(aArea)
Return .T.

/*****************************************************************************
{Protheus.doc} QryEnd

@description Rotina valida se já existe endereço cadastrado

@author Bernard M. Margarido
@since 18/08/2016
@version undefined

@param cCodCli		, Codigo do Contato	
@param nCodEnd		, Id do endereço no e-Commerce

@type function
*****************************************************************************/
Static Function QryEnd(cCodCli,nCodEnd)
	Local cQuery := ""
	Local cAlias := GetNextAlias()
	Local lRet	 := .F.	

	cQuery := "	SELECT " 
	cQuery += "		AGA_CODIGO " 
	cQuery += "FROM "
	cQuery += "		" + RetSqlName("AGA") + " " 
	cQuery += "	WHERE "
	cQuery += "		AGA_FILIAL = '" + xFilial("AGA") + "' AND 
	cQuery += "		AGA_ENTIDA = 'SU5' AND 
	cQuery += "		AGA_CODENT = '" + cCodCli + "' AND 
	cQuery += "		AGA_XCODEN = '" + Alltrim(Str(nCodEnd)) + "' AND
	cQuery += "		D_E_L_E_T_ = ''

	dbUseArea(.T.,"TOPCONN",TcGenQry(,,cQuery),cAlias,.T.,.T.)

	If Empty((cAlias)->AGA_CODIGO)
		lRet := .T.
	EndIf

	(cAlias)->( dbCloseArea() )

Return lRet

/*/
{Protheus.doc} QryContato

@description Rotina valida se já existe contato cadastrado 

@author Bernard M. Margarido
@since 18/08/2016
@version undefined

@param cCep			, CEP do Contato
@param cNome		, Nome do Contato
@param cEnd			, Endereço do Contato
@param cNumero		, Numero do Contato

@type function
/*/
Static Function QryContato(cCep,cNome,cEnd,cNumero)
	Local cQuery := ""
	Local cAlias := GetNextAlias()
	Local nRecno := 0		

	cQuery := "	SELECT " + CRLF
	cQuery += "		U5.R_E_C_N_O_ RECNOSU5 " + CRLF
	cQuery += "	FROM " + CRLF
	cQuery += "		" + RetSqlName("SU5") + " U5 " + CRLF 
	cQuery += "	WHERE " + CRLF
	cQuery += "		U5.U5_FILIAL = '" + xFilial("SU5") + "' AND " + CRLF
	cQuery += "		U5.U5_CEP = '" + cCep + "' AND " + CRLF
	cQuery += "		U5.U5_CONTAT = '" + cNome + "' AND " + CRLF
	cQuery += "		U5.U5_END = '" + cEnd + ", " + cNumero + "' AND " + CRLF
	cQuery += "		U5.D_E_L_E_T_ = '' " + CRLF

	dbUseArea(.T.,"TOPCONN",TcGenQry(,,cQuery),cAlias,.T.,.T.)

	nRecno := (cAlias)->RECNOSU5 

	(cAlias)->( dbCloseArea() )
Return nRecno

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³GrvZa0    ºAutor  ³Bernard M. Margaridoº Data ³  23/08/10   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDesc.     ³Altualiza dados do endereços de clientes e-commerce.        º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
Static Function GrvZa0(lGrava,nLinha,cCliente,cLoja)
	Local aArea		:= GetArea()
	
	Local nY		:= 0

	Local aRetEnd	:= {}

	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³Valida o tipo de objeto retornado.³
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ   

	If ValType(oWsCliente:OWSLISTARNOVOSRESULT:OWSLISTA:OWSCLSUSUARIO[nLinha]:OWSENDERECOS:OWSCLSENDERECO) == "A"

		For nY := 1 To Len(oWsCliente:OWSLISTARNOVOSRESULT:OWSLISTA:OWSCLSUSUARIO[nLinha]:OWSENDERECOS:OWSCLSENDERECO)

			aAdd( aRetEnd ,{; 
			Alltrim(oWsCliente:OWSLISTARNOVOSRESULT:OWSLISTA:OWSCLSUSUARIO[nLinha]:OWSENDERECOS:OWSCLSENDERECO[nY]:CLOGRADOURO),;						// 1.
			Alltrim(oWsCliente:OWSLISTARNOVOSRESULT:OWSLISTA:OWSCLSUSUARIO[nLinha]:OWSENDERECOS:OWSCLSENDERECO[nY]:CNUMERO),;							// 2.
			Alltrim(oWsCliente:OWSLISTARNOVOSRESULT:OWSLISTA:OWSCLSUSUARIO[nLinha]:OWSENDERECOS:OWSCLSENDERECO[nY]:CESTADO),;                        	// 3.
			Alltrim(oWsCliente:OWSLISTARNOVOSRESULT:OWSLISTA:OWSCLSUSUARIO[nLinha]:OWSENDERECOS:OWSCLSENDERECO[nY]:CCIDADE),;                       	// 4.
			Alltrim(oWsCliente:OWSLISTARNOVOSRESULT:OWSLISTA:OWSCLSUSUARIO[nLinha]:OWSENDERECOS:OWSCLSENDERECO[nY]:CBAIRRO),;                        	// 5.
			Alltrim(oWsCliente:OWSLISTARNOVOSRESULT:OWSLISTA:OWSCLSUSUARIO[nLinha]:OWSENDERECOS:OWSCLSENDERECO[nY]:CCEP),;                           	// 6. 
			Alltrim(oWsCliente:OWSLISTARNOVOSRESULT:OWSLISTA:OWSCLSUSUARIO[nLinha]:OWSENDERECOS:OWSCLSENDERECO[nY]:CDDD1),;                          	// 7.
			u_SyFormat(Alltrim(oWsCliente:OWSLISTARNOVOSRESULT:OWSLISTA:OWSCLSUSUARIO[nLinha]:OWSENDERECOS:OWSCLSENDERECO[nY]:CTELEFONE1)),;         	// 8.
			Alltrim(oWsCliente:OWSLISTARNOVOSRESULT:OWSLISTA:OWSCLSUSUARIO[nLinha]:OWSENDERECOS:OWSCLSENDERECO[nY]:CRAMAL1),;							// 9.
			Alltrim(oWsCliente:OWSLISTARNOVOSRESULT:OWSLISTA:OWSCLSUSUARIO[nLinha]:OWSENDERECOS:OWSCLSENDERECO[nY]:CDDD2),;								//10.
			u_SyFormat(Alltrim(oWsCliente:OWSLISTARNOVOSRESULT:OWSLISTA:OWSCLSUSUARIO[nLinha]:OWSENDERECOS:OWSCLSENDERECO[nY]:CTELEFONE2)),;         	//11. 
			Alltrim(oWsCliente:OWSLISTARNOVOSRESULT:OWSLISTA:OWSCLSUSUARIO[nLinha]:OWSENDERECOS:OWSCLSENDERECO[nY]:CRAMAL2),;							//12.
			Alltrim(oWsCliente:OWSLISTARNOVOSRESULT:OWSLISTA:OWSCLSUSUARIO[nLinha]:OWSENDERECOS:OWSCLSENDERECO[nY]:CDDD3),;								//13.
			u_SyFormat(Alltrim(oWsCliente:OWSLISTARNOVOSRESULT:OWSLISTA:OWSCLSUSUARIO[nLinha]:OWSENDERECOS:OWSCLSENDERECO[nY]:CTELEFONE3)),;         	//14.
			Alltrim(oWsCliente:OWSLISTARNOVOSRESULT:OWSLISTA:OWSCLSUSUARIO[nLinha]:OWSENDERECOS:OWSCLSENDERECO[nY]:CRAMAL3),;							//15.  
			Alltrim(oWsCliente:OWSLISTARNOVOSRESULT:OWSLISTA:OWSCLSUSUARIO[nLinha]:OWSENDERECOS:OWSCLSENDERECO[nY]:CDDDCELULAR),;                    	//16.
			u_SyFormat(Alltrim(oWsCliente:OWSLISTARNOVOSRESULT:OWSLISTA:OWSCLSUSUARIO[nLinha]:OWSENDERECOS:OWSCLSENDERECO[nY]:CCELULAR)),;           	//17.
			Alltrim(oWsCliente:OWSLISTARNOVOSRESULT:OWSLISTA:OWSCLSUSUARIO[nLinha]:OWSENDERECOS:OWSCLSENDERECO[nY]:CDDDFAX),;                        	//18.
			u_SyFormat(Alltrim(oWsCliente:OWSLISTARNOVOSRESULT:OWSLISTA:OWSCLSUSUARIO[nLinha]:OWSENDERECOS:OWSCLSENDERECO[nY]:CFAX)),;               	//19.
			oWsCliente:OWSLISTARNOVOSRESULT:OWSLISTA:OWSCLSUSUARIO[nLinha]:OWSENDERECOS:OWSCLSENDERECO[nY]:OWSTIPO:VALUE	,;							//20.					
			Alltrim(oWsCliente:OWSLISTARNOVOSRESULT:OWSLISTA:OWSCLSUSUARIO[nLinha]:OWSENDERECOS:OWSCLSENDERECO[nY]:CREFERENCIA) ,;						//21. 
			oWsCliente:OWSLISTARNOVOSRESULT:OWSLISTA:OWSCLSUSUARIO[nLinha]:OWSENDERECOS:OWSCLSENDERECO[nY]:OWSTIPOLOGRADOURO:VALUE,;					//22.
			oWsCliente:OWSLISTARNOVOSRESULT:OWSLISTA:OWSCLSUSUARIO[nLinha]:OWSENDERECOS:OWSCLSENDERECO[nY]:nEnderecoCodigo,;							//23.
			oWsCliente:OWSLISTARNOVOSRESULT:OWSLISTA:OWSCLSUSUARIO[nLinha]:OWSENDERECOS:OWSCLSENDERECO[nY]:cComplemento,;								//24.		  
			})

		Next nY

	EndIf

	RestArea(aArea)
Return(IIf(lGrava,.T.,aRetEnd))          

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³RetCodMun ºAutor  ³Bernard M. Margaridoº Data ³  13/08/10   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDesc.     ³Rotina retorna codigo do municipio					      º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
User Function RetCodMun(cEstado,cMunicipio)
	Local aRet		:= {"",""}

	cEstado 	:= Upper(cEstado)
	cMunicipio  := u_SyAcento(cMunicipio,.T.)

	aRet := RetMunCC2(cEstado,cMunicipio)
	
Return aRet

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³SYINTCLIENTºAutor  ³Microsiga           º Data ³  08/27/12  º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDesc.     ³                                                            º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
Static Function RetMunCC2(cEstado,cMunicipio)
	Local cQuery := ""
	Local aRet	 := {"",""}

	If Select("MUN") > 0
		MUN->( dbCloseArea() )
	EndIf

	cQuery := "SELECT CC2_CODMUN,CC2_MUN  "
	cQuery += "FROM "
	cQuery += RetSqlName("CC2") + " " 
	cQuery += "WHERE "
	cQuery += "CC2_EST = '" + cEstado + "' AND "
	cQuery += "CC2_MUN = '" + cMunicipio + "' AND "
	cQuery += "D_E_L_E_T_ = '' "

	cQuery := ChangeQuery(cQuery)
	dbUseArea(.T.,"TOPCONN",TcGenQry(,,cQuery),"MUN",.T.,.T.) 

	MUN->( dbGoTop() )
	If MUN->(Eof())
		MUN->( dbCloseArea() )
		Return(aRet)
	EndIf

	If MUN->(!Eof())
		aRet[1] := MUN->CC2_CODMUN
		aRet[2] := MUN->CC2_MUN
	EndIf

	MUN->( dbCloseArea() )
Return(aRet)

/**********************************************************************************************************
{Protheus.doc} EnvBxCli

@description Envia a baixa de cliente para o e-Commerce informando que o cadastros foi efetuado no ERP 

@author Bernard M. Margarido
@since 18/08/2016
@version undefined

@param cCodigo		, Codigo do Cliente
@param cEmail		, Email do Cliente
@type function
***********************************************************************************************************/
Static Function EnvBxCli(cCodigo, cEmail)
	Local aArea				:= GetArea()
	Local lRet				:= .F.
	Local cMsgErro			:= ""
	Local oWsBxCliente	
	
	//-------------------------------------------+
	// Instancia objeto de Integração E-Commerce |
	//-------------------------------------------+
	oWsBxCliente :=  WSCliente():New

	//---------------------+
	// Parametros de Envio |
	//---------------------+ 
	oWsBxCliente:_Url					:= AllTrim(GetMV("EC_URLECOM"))+"cliente.asmx?"
	oWsBxCliente:nLojaCodigo			:= 0
	oWsBxCliente:cEmail					:= Alltrim(cEmail)
	oWsBxCliente:cContaCodigoInterno  	:= cCodigo

	//-----------------------------------------------+
	// Metodo Valida Baixa de Clientes do e-Commerce |
	//-----------------------------------------------+
	If oWsBxCliente:Validar()
		If oWsBxCliente:oWsValidarResult:nCodigo == 1
			LogExec("BAIXA DE CLIENTE ENVIADA COM SUCESSO PARA RAKUTEN")
			lRet := .T.
		Else
			LogExec(" ERRO AO ENVIAR BAIXA DE CLIENTE PARA RAKUTEN " + Alltrim(oWsBxCliente:oWsValidarResult:cDescricao) )
			aAdd(aErroMsg,{cCodigo,Alltrim(oWsBxCliente:oWsValidarResult:cDescricao)})
		Endif
	Else
		LogExec("ERRO DE CONEXAO AO ENVIAR BAIXA DE CLIENTES : " + Alltrim(GetWscError()))
		aAdd(aErroMsg,{cCodigo," ERRO DE CONEXAO AO ENVIAR BAIXA DE CLIENTES : " + Alltrim(GetWscError())})	 
	EndIf

	RestArea(aArea)
Return({lRet, cMsgErro})

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³LogExec   ºAutor  ³SYMM Consultoria    º Data ³  30/12/14   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDesc.     ³Grava Log do processo                                       º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
Static Function LogExec(cMsg)
	CONOUT(cMsg)
	LjWriteLog(cArqLog,cMsg)
Return .T.
