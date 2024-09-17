#INCLUDE "PROTHEUS.CH"

/*/{Protheus.doc} PCGATSA1
Gatilho para alimentacao do Grupo Tributario
@author  Guilherme Santos
@since   11/01/2019
@version 12.1.17
/*/

User Function PCGATSA1( cCampo, cOrig )
	Local cRetorno 	:= ""
	Local cQry		:= ""
	Local cZZ7Tbl	:= ""
	Local cZZ7Fil	:= ""
	Local cA1CODSEG := ""
	Local cA1EST	:= ""
	Local cA1CODMUN := ""
	Local cA1TIPO	:= ""
	Local cA1PESSOA	:= ""
	Local cA1CONTRIB:= ""
	Local lDbg := .F.
	Default cOrig := ""

	If lDbg
		RpcSetEnv("01","00")
		dbSelectArea("SA1")
		SA1->(dbSetOrder(1))
		SA1->(dbGoTo(166943))
		cCampo := "A1_CONTRIB"
		cOrig := "PE"
	EndIf

	If cOrig == ""

		cA1CODSEG := AllTrim(GetCpo("A1_CODSEG"))
		cA1EST := AllTrim(GetCpo("A1_EST"))
		cA1CODMUN := AllTrim(GetCpo("A1_COD_MUN"))
		cA1TIPO := Alltrim(GetCpo("A1_TIPO"))
		cA1PESSOA := Alltrim(GetCpo("A1_PESSOA"))
		cA1CONTRIB := Alltrim(GetCpo("A1_CONTRIB"))

	Else

		cA1CODSEG := Alltrim(SA1->A1_CODSEG)
		cA1EST := Alltrim(SA1->A1_EST)
		cA1CODMUN := Alltrim(SA1->A1_COD_MUN)
		cA1TIPO := Alltrim(SA1->A1_TIPO)
		cA1PESSOA := Alltrim(SA1->A1_PESSOA)
		cA1CONTRIB := Alltrim(SA1->A1_CONTRIB)

	EndIf	
	
	Do Case

	Case cCampo == "A1_CONTRIB"

		Do Case

		Case cA1CODSEG == "01" //ORGAO PUBLICO FEDERAL
			cRetorno := "004"

		Case cA1CODSEG == "02" //ORGAO PUBLICO ESTADUAL
			cRetorno := "005"

		Case cA1CODSEG == "03" //ORGAO PUBLICO MUNICIPAL
			cRetorno := "011"

		Otherwise

			cZZ7Tbl := RetSqlName("ZZ7")
			cZZ7Fil := xFilial("ZZ7")

			If ( !Empty(cA1EST) .and. !Empty(cA1CODMUN) .and. !Empty(cA1TIPO) .and. !Empty(cA1PESSOA) .and. !Empty(cA1CONTRIB) )

				cQry := "SELECT ZZ7_GRPTRI AS ZZ7GRPTRI "
				cQry += "FROM " +cZZ7Tbl+ " WHERE D_E_L_E_T_ = ' ' "
				cQry += "AND ZZ7_FILIAL = '" +cZZ7Fil+ "' "
				cQry += "AND ZZ7_EST = '" +cA1EST+ "' "
				cQry += "AND ZZ7_CODMUN = '" +cA1CODMUN+ "' "
				cQry += "AND ZZ7_PESSOA = '" +cA1PESSOA+ "' "
				cQry += "AND ZZ7_CONTRI = '" +cA1CONTRIB+ "' "
				cQry += "AND ZZ7_TIPO = '" +cA1TIPO+ "' "

				Iif(Select("AWRK")>0,AWRK->(dbCloseArea()),Nil)
				dbUseArea(.T.,"TOPCONN",TCGenQry(,,cQry),"AWRK",.T.,.T.)
				AWRK->(dbGoTop())

				If AWRK->(!EoF())
					If !Empty(AWRK->ZZ7GRPTRI)
						cRetorno := AWRK->ZZ7GRPTRI
					EndIf
				EndIf
				AWRK->(dbCloseArea())

			EndIf

			If Empty( cRetorno )

				cQry := "SELECT ZZ7_GRPTRI AS ZZ7GRPTRI "
				cQry += "FROM " +cZZ7Tbl+ " WHERE D_E_L_E_T_ = ' ' "
				cQry += "AND ZZ7_FILIAL = '" +cZZ7Fil+ "' "
				cQry += "AND ZZ7_EST = '**' "
				cQry += "AND ZZ7_CODMUN = '*****' "
				cQry += "AND ZZ7_PESSOA = '*' "
				If !Empty(cA1CONTRIB)
					cQry += "AND ZZ7_CONTRI = '" +cA1CONTRIB+ "' "
				EndIf
				If !Empty(cA1TIPO)
					cQry += "AND ZZ7_TIPO = '" +cA1TIPO+ "' "
				EndIf

				Iif(Select("BWRK")>0,BWRK->(dbCloseArea()),Nil)
				dbUseArea(.T.,"TOPCONN",TCGenQry(,,cQry),"BWRK",.T.,.T.)
				BWRK->(dbGoTop())

				If BWRK->(!EoF())
					If !Empty(BWRK->ZZ7GRPTRI)
						cRetorno := BWRK->ZZ7GRPTRI
					EndIf
				EndIf
				BWRK->(dbCloseArea())

			EndIf

			If Empty( cRetorno )
				cRetorno := Space(3)
			EndIf

		EndCase

	Case cCampo == "A1_COD_MUN"

		If cA1CODMUN $ "|04062|00303|00600|00106|00100|00159|00104|00252|00203|02603|03536|03569|"

			Do Case

			Case cA1EST == "AM" .AND. cA1CODMUN == "04062"
				cRetorno := "09847"
			Case cA1EST == "AP" .AND. cA1CODMUN == "00303"
				cRetorno := "00605"
			Case cA1EST == "AP" .AND. cA1CODMUN == "00600"
				cRetorno := "00615"
			Case cA1EST == "RO" .AND. cA1CODMUN == "00106"
				cRetorno := "00001"
			Case cA1EST == "RR" .AND. cA1CODMUN == "00100"
				cRetorno := "00100"
			Case cA1EST == "RR" .AND. cA1CODMUN == "00159"
				cRetorno := "00307"
			Case cA1EST == "AC" .AND. cA1CODMUN == "00104"
				cRetorno := "00105"
			Case cA1EST == "AC" .AND. cA1CODMUN == "00252"
				cRetorno := "99998"
			Case cA1EST == "AC" .AND. cA1CODMUN == "00203"
				cRetorno := "00107"
			Case cA1EST == "AM" .AND. cA1CODMUN == "02603"
				cRetorno := "00255"
			Case cA1EST == "AM" .AND. cA1CODMUN == "03536"
				cRetorno := "09841"
			Case cA1EST == "AM" .AND. cA1CODMUN == "03569"
				cRetorno := "09843"
			EndCase

		EndIf

		If Empty(cRetorno)
			cRetorno := Space(5)
		EndIf

	EndCase

Return cRetorno



//-------------------------------------------------------------------
/*/{Protheus.doc} A1GET
Retorna o Conteudo do Campo do SA1 para o Gatilho

@author  Guilherme Santos
@since   25/08/2022
@version 12.1.33
/*/
//-------------------------------------------------------------------
User Function A1GET(cCampo)
	cRetorno := GetCpo(cCampo)
Return cRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} GetCpo
Retorna o Campo de acordo com o Parametro da Rotina

@author  Guilherme Santos
@since   24/01/2019
@version 12.1.17
/*/
//-------------------------------------------------------------------
Static Function GetCpo(cCampo)
	Local cRetorno := ""
	Local lMVCCustomer	:= SuperGetMv("MV_MVCSA1",,.F.) .AND. !IsInCallStack("U_PROMA030") .AND. IsInCallStack("CRMA980")

	If IsInCallStack("U_BPFATM03") //IsInCallStackI("U_BZAPI001")
		lMVCCustomer := .F.
	EndIf

	If lMVCCustomer
		cRetorno := FwFldGet(cCampo)
	Else
		cRetorno := &("M->" + cCampo)
	EndIf

Return cRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} PcGatVld
Valida se é chamado do cadastro de clientes MATA030

@author  Victor Dessunte
@since   14/05/2021
@version 12.1.25
/*/
//-------------------------------------------------------------------
User Function PcGatVld()
	Local lRet := .F.
	If (Isincallstack("MATA030") .OR. IsInCallStack("CRMA980")) .AND. !Isincallstack("PROMA061")
		lRet := .T.
	EndIf
Return lRet
//-------------------------------------------------------------------
/*/{Protheus.doc} A1GATPES
Valida se e pessoa Fisica e Consumidor Final

@author  Guilherme Santos
@since   26/08/2022
@version 12.1.33
/*/
//-------------------------------------------------------------------
User Function A1GATPES()
	Local lRetorno := U_A1GET("A1_PESSOA") = "F" .AND. U_A1GET("A1_TIPO") = "F"
Return lRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} A1SATIV1
Valida se o codigo da atividade esta preenchido

@author  Guilherme Santos
@since   26/08/2022
@version 12.1.33
/*/
//-------------------------------------------------------------------
User Function A1SATIV1()
	Local lRetorno := !EMPTY(U_A1GET("A1_SATIV1")) .AND. U_PCGATVLD()
Return lRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} A1GAT002
Retorna se e pessoa fisica e Produtor Rural

@author  Guilherme Santos
@since   26/08/2022
@version 12.1.33
/*/
//-------------------------------------------------------------------
User Function A1GAT002()
	Local lRetorno := U_A1GET("A1_TIPO")=="L".AND.U_A1GET("A1_PESSOA")=="F"
Return lRetorno
