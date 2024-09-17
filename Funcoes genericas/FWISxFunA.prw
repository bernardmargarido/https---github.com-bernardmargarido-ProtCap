/*----------------------------------------------------------------------*\
 * FWISxFunA for AdvPL
 * Copyright (C) 2015  Felipe Raposo <feliperaposo@gmail.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
\*----------------------------------------------------------------------*/

#include "aarray.ch"
#include "json.ch"
#include "WISStatus.ch"
#include "protheus.ch"

// Definições do status de estoque do pedido de venda.
#define AGUARDANDO    "1"
#define DISPONIVEL    "2"
#define RESERVADO     "3"
#define RES_PARCIAL   "4"
#define NAO_MOVIMENTA "9"
// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Materiais
// Função   : WISSB1
// Descrição: Envia produto para o webservice WIS.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 09/03/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function WISSB1(cJson, cErrInt)

Local aArea			:= GetArea()
Local aAreaSB1		:= SB1->(GetArea())
Local aAreaSAH		:= SAH->(GetArea())
Local aAreaSZE		:= SZE->(GetArea())
Local cRet       	:= ""

Local oJson			:= NIL
Local cStsCode		:= ""
Local lEnvCodB		:= SuperGetMV("WIS_CODBAR", NIL, .T.)
Local lRetorno		:= .T.

Local cDescUM		:= ""
Local cGrupo		:= "NC"
Local cDescGrp		:= "NC"
Local cSubGrp		:= "NC"
Local cDesSub		:= "NC"
Local cWisEmp		:= SuperGetMV("WIS_EMP",, "1")
Local cWisDep		:= SuperGetMV("WIS_DEP",, "001")

Default cJson	:= ""
Default cErrInt	:= ""

If SB1->(Eof())
	cErrInt		:= "WIS - SB1 em EOF."
	lRetorno	:= .F.
Else
	DbSelectArea("SAH")
	SAH->(dbSetOrder(1))  // AH_FILIAL, AH_UNIMED.
	If SAH->(dbSeek(xFilial("SAH") + SB1->B1_UM))
		cDescUM	:= left(SAH->AH_DESCPO, 30)
	EndIf

	DbSelectArea("SZE")
	SZE->(dbSetOrder(1))  // ZE_FILIAL, ZE_COD.
	If !Empty(SB1->B1_XGRUPO) .AND. SZE->(dbSeek(xFilial("SZE") + SB1->B1_XGRUPO))
		cGrupo		:= SB1->B1_XGRUPO
		cDescGrp	:= SZE->ZE_DESC
		cSubGrp		:= SB1->B1_XGRUPO
		cDesSub		:= SZE->ZE_DESC
	EndIf

	oJson := Array(#)
	oJson[#"usuario"] := "%cUser%"
	oJson[#"senha"]   := "%cPass%"
	oJson[#"produto"] := Array(#)
	oJson[#"produto"][#"cd_empresa"]             := cWisEmp
	oJson[#"produto"][#"cd_deposito"]            := cWisDep
	oJson[#"produto"][#"cd_produto"]             := SB1->B1_COD
	oJson[#"produto"][#"ds_produto"]             := SB1->(B1_UM + "-" + B1_DESC)
	oJson[#"produto"][#"ds_reduzida"]            := left(SB1->B1_DESC, 38)
	oJson[#"produto"][#"cd_unidade_medida"]      := SB1->B1_UM
	oJson[#"produto"][#"ds_unidade_medida"]      := cDescUM
	oJson[#"produto"][#"id_aceita_decimal"]      := "S"
	oJson[#"produto"][#"cd_embalagem"]           := "NC"
	oJson[#"produto"][#"ds_embalagem"]           := "NC"
	oJson[#"produto"][#"qt_unidade_embalagem"]   := "1"
	oJson[#"produto"][#"cd_produto_master"]      := SB1->B1_COD
	oJson[#"produto"][#"qt_itens"]               := "1"
	oJson[#"produto"][#"cd_familia"]             := "NC"
	oJson[#"produto"][#"ds_familia"]             := "NC"
	oJson[#"produto"][#"vl_altura"]              := "1"
	oJson[#"produto"][#"vl_largura"]             := "1"
	oJson[#"produto"][#"vl_profundidade"]        := "1"
	oJson[#"produto"][#"ps_liquido"]             := LTrim(Transform(SB1->B1_PESO, "99999999999.999"))
	oJson[#"produto"][#"ps_bruto"]               := LTrim(Transform(SB1->B1_PESBRU, "99999999999.999"))
	oJson[#"produto"][#"qt_max_palete"]          := "1"
	oJson[#"produto"][#"cd_situacao"]            := If(SB1->B1_MSBLQL = "1", "16", "15")
	oJson[#"produto"][#"cd_rotatividade"]        := "NC"
	oJson[#"produto"][#"cd_classe"]              := "NC"
	oJson[#"produto"][#"ds_classe"]              := "NC"
	oJson[#"produto"][#"qt_dias_validade"]       := "1"
	oJson[#"produto"][#"qt_dias_remonte"]        := "1"
	oJson[#"produto"][#"id_controle_lote"]       := "N"
	oJson[#"produto"][#"id_controle_serie"]      := "N"
	oJson[#"produto"][#"id_controle_validade"]   := "N"
	oJson[#"produto"][#"qt_caixa_fechada"]       := "1"
	oJson[#"produto"][#"cd_fornecedor"]          := nil
	oJson[#"produto"][#"cd_cnpj_fornecedor"]     := nil
	oJson[#"produto"][#"cd_produto_fornecedor"]  := nil
	oJson[#"produto"][#"cd_linha"]               := "NC"
	oJson[#"produto"][#"ds_linha"]               := "NC"
	oJson[#"produto"][#"cd_grupo"]               := cGrupo
	oJson[#"produto"][#"ds_grupo"]               := cDescGrp
	oJson[#"produto"][#"cd_subgrupo"]            := cSubGrp
	oJson[#"produto"][#"ds_subgrupo"]            := cDesSub
	oJson[#"produto"][#"cd_modelo"]              := "NC"
	oJson[#"produto"][#"ds_modelo"]              := "NC"
	oJson[#"produto"][#"tp_armazenagem_produto"] := nil

	// Envia o Json para o webservice.
	cJson	:= ToJson(oJson, .T.)
	cRet 	:= U_WISPost("/entrada/produto", oJson, "SB1", @cStsCode)

	// B1_XWIS -> 0=Nao enviado / 1=Enviado / 2=Erro.
	RecLock("SB1", .F.)
	If cRet = "SUCCESS"
		SB1->B1_XWIS	:= "1"
	Else
		SB1->B1_XWIS	:= "2"
		lRetorno		:= .F.
	Endif
	SB1->(msUnLock())

	If !lRetorno
		cErrInt := cRet
	EndIf

	// Envia os códigos de barras do produto para o WIS.
	If lRetorno
		If lEnvCodB
			U_WISCdBar()
		EndIf
	Endif
Endif

// Restaura areas de trabalho.
RestArea(aAreaSZE)
RestArea(aAreaSAH)
RestArea(aAreaSB1)
RestArea(aArea)

Return lRetorno
// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Materiais
// Função   : WISCdBar
// Descrição: Envia códigos de barras do produto para o webservice WIS.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 10/03/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function WISCdBar(lExclui)

Local oJson			:= NIL
Local cStsCode   	:= ""
Local cQuery		:= ""
Local cTabQry		:= ""
Local cWisEmp		:= SuperGetMV("WIS_EMP",, "1")

Default lExclui 	:= .F.

If !Empty(SB1->B1_CODBAR)
	//Verifica se deve enviar o Codigo de Barras para o Produto
	If U_fBarForn(SB1->B1_COD)
		oJson := Array(#)
		oJson[#"usuario"] := "%cUser%"
		oJson[#"senha"]   := "%cPass%"
		oJson[#"codigobarras"] := Array(#)
		oJson[#"codigobarras"][#"cd_empresa"]          := cWisEmp
		oJson[#"codigobarras"][#"cd_produto"]          := SB1->B1_COD
		oJson[#"codigobarras"][#"cd_barras"]           := SB1->B1_CODBAR
		oJson[#"codigobarras"][#"cd_situacao"]         := If(lExclui, "2", "1")
		oJson[#"codigobarras"][#"tp_codigo_barras"]    := "13"  // EAN13.
		oJson[#"codigobarras"][#"qt_embalagem"]        := "1"
		oJson[#"codigobarras"][#"id_codigo_principal"] := "S"
		oJson[#"codigobarras"][#"nu_prioridade_uso"]   := nil

		// Envia o Json para o webservice.
		U_WISPost("/entrada/codigobarras", oJson, "SB1cb", @cStsCode)

		FreeObj(oJson)

		If !lExclui
			cQuery += "SELECT	SLK.LK_CODIGO" + CRLF
			cQuery += ",		SLK.LK_CODBAR" + CRLF
			cQuery += ",		SLK.LK_QUANT " + CRLF
			cQuery += "FROM		" + RetSqlName("SLK") + " SLK" + CRLF
			cQuery += "WHERE	SLK.LK_FILIAL = '" + xFilial("SLK") + "'" + CRLF
			cQuery += "AND		SLK.LK_CODIGO = '" + SB1->B1_COD + "'" + CRLF
			cQuery += "AND		SLK.LK_CODBAR <> '" + SB1->B1_CODBAR + "'" + CRLF
			cQuery += "AND		SLK.D_E_L_E_T_ = ''" + CRLF

			cTabQry := MpSysOpenQuery(cQuery)

			While !(cTabQry)->(Eof())
				cStsCode	:= ""
				oJson 		:= Array(#)

				oJson[#"usuario"] := "%cUser%"
				oJson[#"senha"]   := "%cPass%"
				oJson[#"codigobarras"] := Array(#)
				oJson[#"codigobarras"][#"cd_empresa"]          := cWisEmp
				oJson[#"codigobarras"][#"cd_produto"]          := SB1->B1_COD
				oJson[#"codigobarras"][#"cd_barras"]           := (cTabQry)->LK_CODBAR
				oJson[#"codigobarras"][#"cd_situacao"]         := "1"
				oJson[#"codigobarras"][#"tp_codigo_barras"]    := "13"  // EAN13.
				oJson[#"codigobarras"][#"qt_embalagem"]        := AllTrim(Str((cTabQry)->LK_QUANT))
				oJson[#"codigobarras"][#"id_codigo_principal"] := "N"
				oJson[#"codigobarras"][#"nu_prioridade_uso"]   := nil

				// Envia o Json para o webservice.
				U_WISPost("/entrada/codigobarras", oJson, "SB1cb", @cStsCode)

				FreeObj(oJson)

				(cTabQry)->(DbSkip())
			End

			If Select(cTabQry) > 0
				(cTabQry)->(DbCloseArea())
			EndIf
		EndIf
	Endif
EndIf

Return


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Materiais
// Função   : WISSA1
// Descrição: Envia cliente para o webservice WIS.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 10/03/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function WISSA1(lDevol, cJson, cErrInt)

Local lRet		:= .T.
Local cRet		:= ""
Local cAlias	:= Alias()
Local aArea		:= {}

Local oJson
Local cStsCode	:= ""
Local cWisEmp		:= SuperGetMV("WIS_EMP",, "1")
Local cWisDep		:= SuperGetMV("WIS_DEP",, "001")

Default lDevol 	:= .F.
Default cJson	:= ""
Default cErrInt	:= ""

// Guarda posição das tabelas.
If empty(cAlias)
	cAlias := "SX3"
	dbSelectArea(cAlias)
Endif
aArea := sGetArea()

If lDevol .and. SA2->(eof())
	cErrInt := "WIS - SA2 em EOF."
	lRet 	:= .F.
ElseIf !lDevol .and. SA1->(eof())
	cErrInt := "WIS - SA1 em EOF."
	lRet := .F.
Else
	oJson := Array(#)
	oJson[#"usuario"] := "%cUser%"
	oJson[#"senha"]   := "%cPass%"
	oJson[#"cliente"] := Array(#)
	oJson[#"cliente"][#"cd_empresa"]        := cWisEmp
	oJson[#"cliente"][#"cd_deposito"]       := cWisDep
	oJson[#"cliente"][#"id_cliente_filial"] := "N"
	oJson[#"cliente"][#"cd_rota"]           := nil

	// Se for devolução, pega o fornecedor e cadastra como cliente.
	If lDevol
		oJson[#"cliente"][#"cd_cliente"]        := SA2->(A2_COD + A2_LOJA)
		oJson[#"cliente"][#"ds_cliente"]        := SA2->A2_NOME
		oJson[#"cliente"][#"nu_inscricao"]      := U_Numeros(SA2->A2_INSCR)
		oJson[#"cliente"][#"cd_cnpj_cliente"]   := If(Empty(SA2->A2_CGC), StrZero(0, Len(SA2->A2_CGC)), SA2->A2_CGC)
		oJson[#"cliente"][#"cd_cep"]            := SA2->A2_CEP
		oJson[#"cliente"][#"ds_endereco"]       := SA2->A2_END
		oJson[#"cliente"][#"nu_endereco"]       := AllTrim(If(at(',', SA2->A2_END) = 0, nil, SubStr(SA2->A2_END, at(',', SA2->A2_END) + 1, 8)))
		oJson[#"cliente"][#"ds_complemento"]    := nil
		oJson[#"cliente"][#"ds_bairro"]         := SA2->A2_BAIRRO
		oJson[#"cliente"][#"ds_municipio"]      := SA2->A2_MUN
		oJson[#"cliente"][#"cd_uf"]             := SA2->A2_EST
		oJson[#"cliente"][#"nu_telefone"]       := left(U_Numeros(SA2->A2_TEL), 12)
		oJson[#"cliente"][#"nu_fax"]            := left(U_Numeros(SA2->A2_FAX), 12)
		oJson[#"cliente"][#"cd_situacao"]       := If(SA2->A2_MSBLQL = "1", "16", "15")
	Else
		oJson[#"cliente"][#"cd_cliente"]        := SA1->(A1_COD + A1_LOJA)
		oJson[#"cliente"][#"ds_cliente"]        := SA1->A1_NOME
		oJson[#"cliente"][#"nu_inscricao"]      := U_Numeros(SA1->A1_INSCR)
		oJson[#"cliente"][#"cd_cnpj_cliente"]   := If(Empty(SA1->A1_CGC), StrZero(0, Len(SA1->A1_CGC)), SA1->A1_CGC)
		oJson[#"cliente"][#"cd_cep"]            := SA1->A1_CEPE
		oJson[#"cliente"][#"ds_endereco"]       := SA1->A1_ENDENT
		oJson[#"cliente"][#"nu_endereco"]       := AllTrim(If(at(',', SA1->A1_ENDENT) = 0, nil, SubStr(SA1->A1_ENDENT, at(',', SA1->A1_ENDENT) + 1, 8)))
		oJson[#"cliente"][#"ds_complemento"]    := SA1->A1_COMPLEM
		oJson[#"cliente"][#"ds_bairro"]         := SA1->A1_BAIRROE
		oJson[#"cliente"][#"ds_municipio"]      := SA1->A1_MUNE
		oJson[#"cliente"][#"cd_uf"]             := SA1->A1_ESTE
		oJson[#"cliente"][#"nu_telefone"]       := left(U_Numeros(SA1->A1_TEL), 12)
		oJson[#"cliente"][#"nu_fax"]            := left(U_Numeros(SA1->A1_FAX), 12)
		oJson[#"cliente"][#"cd_situacao"]       := If(SA1->A1_MSBLQL = "1", "16", "15")
	Endif

	// Envia o Json para o webservice.
	cJson := ToJson(oJson, .T.)
	cRet := U_WISPost("/entrada/cliente", oJson, "EntCli", @cStsCode)
	FreeObj(oJson)

	If cStsCode <> "200"
		cErrInt := "Erro ao enviar " + If(lDevol, "fornecedor " + SA2->(A2_COD + "/" + A2_LOJA), "cliente " + SA1->(A1_COD + "/" + A1_LOJA)) + CRLF + cRet
		lRet 	:= .F.
	Endif
Endif

// Restaura areas de trabalho.
(cAlias)->(sRestArea(aArea))
dbSelectArea(cAlias)

Return lRet


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Materiais
// Função   : WISSA2
// Descrição: Envia fornecedor para o webservice WIS.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 10/03/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function WISSA2(lDevol, cJson, cErrInt)

Local lRet       := .T.
Local cRet       := ""
Local cAlias     := Alias()
Local aArea      := {}

Local oJson
Local cStsCode   := ""
Local cWisEmp		:= SuperGetMV("WIS_EMP",, "1")
Local cWisDep		:= SuperGetMV("WIS_DEP",, "001")

Default lDevol 	:= .F.
Default cJson	:= ""
Default cErrInt	:= ""

// Guarda posição das tabelas.
If empty(cAlias)
	cAlias := "SX3"
	dbSelectArea(cAlias)
Endif
aArea := sGetArea()

If lDevol .and. SA1->(eof())
	cErrInt := "WIS - SA1 em EOF."
	lRet 	:= .F.
ElseIf !lDevol .and. SA2->(eof())
	cErrInt := "WIS - SA2 em EOF."
	lRet 	:= .F.
Else
	oJson := Array(#)
	oJson[#"usuario"] := "%cUser%"
	oJson[#"senha"]   := "%cPass%"
	oJson[#"fornecedor"] := Array(#)
	oJson[#"fornecedor"][#"cd_empresa"]         := cWisEmp
	oJson[#"fornecedor"][#"cd_deposito"]        := cWisDep

	// Se for devolução, pega o cliente e cadastra como fornecedor.
	If lDevol
		oJson[#"fornecedor"][#"cd_fornecedor"]      := SA1->(A1_COD + A1_LOJA) + "C"
		oJson[#"fornecedor"][#"ds_razao_social"]    := SA1->A1_NOME
		oJson[#"fornecedor"][#"nm_fantasia"]        := SA1->A1_NREDUZ
		oJson[#"fornecedor"][#"cd_cnpj_fornecedor"] := If(Empty(SA1->A1_CGC), StrZero(0, Len(SA1->A1_CGC)), SA1->A1_CGC)
		oJson[#"fornecedor"][#"nu_inscricao"]       := U_Numeros(SA1->A1_INSCR)
		oJson[#"fornecedor"][#"ds_endereco"]        := SA1->A1_ENDENT
		oJson[#"fornecedor"][#"ds_bairro"]          := SA1->A1_BAIRROE
		oJson[#"fornecedor"][#"ds_municipio"]       := SA1->A1_MUNE
		oJson[#"fornecedor"][#"cd_uf"]              := SA1->A1_ESTE
		oJson[#"fornecedor"][#"cd_cep"]             := SA1->A1_CEPE
		oJson[#"fornecedor"][#"nu_telefone"]        := left(U_Numeros(SA1->A1_TEL), 12)
		oJson[#"fornecedor"][#"nu_fax"]             := left(U_Numeros(SA1->A1_FAX), 12)
		oJson[#"fornecedor"][#"cd_situacao"]        := If(SA1->A1_MSBLQL = "1", "16", "15")
	Else
		oJson[#"fornecedor"][#"cd_fornecedor"]      := SA2->(A2_COD + A2_LOJA)
		oJson[#"fornecedor"][#"ds_razao_social"]    := SA2->A2_NOME
		oJson[#"fornecedor"][#"nm_fantasia"]        := SA2->A2_NREDUZ
		oJson[#"fornecedor"][#"cd_cnpj_fornecedor"] := If(Empty(SA2->A2_CGC), StrZero(0, Len(SA2->A2_CGC)), SA2->A2_CGC)
		oJson[#"fornecedor"][#"nu_inscricao"]       := U_Numeros(SA2->A2_INSCR)
		oJson[#"fornecedor"][#"ds_endereco"]        := SA2->A2_END
		oJson[#"fornecedor"][#"ds_bairro"]          := SA2->A2_BAIRRO
		oJson[#"fornecedor"][#"ds_municipio"]       := SA2->A2_MUN
		oJson[#"fornecedor"][#"cd_uf"]              := SA2->A2_EST
		oJson[#"fornecedor"][#"cd_cep"]             := SA2->A2_CEP
		oJson[#"fornecedor"][#"nu_telefone"]        := left(U_Numeros(SA2->A2_TEL), 12)
		oJson[#"fornecedor"][#"nu_fax"]             := left(U_Numeros(SA2->A2_FAX), 12)
		oJson[#"fornecedor"][#"cd_situacao"]        := If(SA2->A2_MSBLQL = "1", "16", "15")
	Endif

	// Envia o Json para o webservice.
	cJson := ToJson(oJson, .T.)
	cRet := U_WISPost("/entrada/fornecedor", oJson, "SA2", @cStsCode)

	If cStsCode <> "200"
		cErrInt := "Erro ao enviar " + If(lDevol, "cliente " + SA1->(A1_COD + "/" + A1_LOJA), "fornecedor " + SA2->(A2_COD + "/" + A2_LOJA)) + CRLF + cRet
		lRet 	:= .F.
	Endif
Endif

// Restaura areas de trabalho.
(cAlias)->(sRestArea(aArea))
dbSelectArea(cAlias)

Return lRet


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Materiais
// Função   : WISSA4
// Descrição: Envia transportadora para o webservice WIS.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 10/03/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function WISSA4(cJson, cErrInt)

Local cAlias	:= Alias()
Local aArea		:= {}

Local oJson		:= NIL
Local cStsCode	:= ""
Local lRet		:= .T.
Local cRet		:= ""
Local cWisEmp		:= SuperGetMV("WIS_EMP",, "1")
Local cWisDep		:= SuperGetMV("WIS_DEP",, "001")

Default cJson	:= ""
Default cErrInt	:= ""

// Guarda posição das tabelas.
If empty(cAlias)
	cAlias := "SX3"
	dbSelectArea(cAlias)
Endif
aArea := sGetArea()

If SA4->(eof())
	cErrInt	:= "WIS - SA4 em EOF."
	lRet 	:= .F.
Else
	oJson := Array(#)
	oJson[#"usuario"] := "%cUser%"
	oJson[#"senha"]   := "%cPass%"
	oJson[#"transportadora"] := Array(#)
	oJson[#"transportadora"][#"cd_empresa"]             := cWisEmp
	oJson[#"transportadora"][#"cd_deposito"]            := cWisDep
	oJson[#"transportadora"][#"cd_transportadora"]      := SA4->A4_COD
	oJson[#"transportadora"][#"ds_transportadora"]      := SA4->A4_NOME
	oJson[#"transportadora"][#"nu_inscricao"]           := U_Numeros(SA4->A4_INSEST)
	oJson[#"transportadora"][#"cd_cnpj_transportadora"] := If(Empty(SA4->A4_CGC), StrZero(0, Len(SA4->A4_CGC)), SA4->A4_CGC)
	oJson[#"transportadora"][#"cd_cep"]                 := SA4->A4_CEP
	oJson[#"transportadora"][#"ds_endereco"]            := SA4->A4_END
	oJson[#"transportadora"][#"nu_endereco"]            := nil
	oJson[#"transportadora"][#"ds_complemento"]         := nil
	oJson[#"transportadora"][#"ds_bairro"]              := SA4->A4_BAIRRO
	oJson[#"transportadora"][#"ds_municipio"]           := SA4->A4_MUN
	oJson[#"transportadora"][#"cd_uf"]                  := SA4->A4_EST
	oJson[#"transportadora"][#"nu_telefone"]            := left(U_Numeros(SA4->A4_TEL), 12)
	oJson[#"transportadora"][#"nu_fax"]                 := nil
	oJson[#"transportadora"][#"cd_situacao"]            := If(SA4->A4_MSBLQL = "1", "16", "15")

	// Envia o Json para o webservice.
	cJson := ToJson(oJson, .T.)
	cRet := U_WISPost("/entrada/transportadora", oJson, "SA4", @cStsCode)
	FreeObj(oJson)

	If cStsCode <> "200"
		cErrInt := "Erro ao enviar transportadora " + SA4->A4_COD + "-" + SA4->A4_NOME + CRLF + cRet
		lRet 	:= .F.
	Endif
Endif

// Restaura areas de trabalho.
(cAlias)->(sRestArea(aArea))
dbSelectArea(cAlias)

Return lRet


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Materiais
// Função   : WISSF1
// Descrição: Envia nota ou pré-nota de entrada para o webservice WIS.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 16/03/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function WISSF1(cSituac, cJson, cErrInt)

Local aArea			:= GetArea()
Local aAreaSF1		:= SF1->(GetArea())
Local aAreaSD1		:= SD1->(GetArea())
Local aAreaSB1		:= SB1->(GetArea())
Local aAreaSA1		:= SA1->(GetArea())
Local aAreaSA2		:= SA2->(GetArea())
Local aAreaSC6		:= SC6->(GetArea())

Local lRet			:= .T.
Local cRet			:= ""
Local nX			:= 0

Local lDevol		:= .F.
Local lDevMont		:= .F.
Local cF1Wis		:= ""

Local oJson			:= NIL
Local oDetalhe		:= NIL
Local aEstrut[0]
Local aItens[0]
Local cStsCode   	:= ""
Local cTpEntrada	:= ""

Local cPorta		:= SuperGetMV("WIS_PORTDV",,"100")
Local cWisEmp		:= SuperGetMV("WIS_EMP",, "1")
Local cWisDep		:= SuperGetMV("WIS_DEP",, "001")

Local aItensNFE		:= {}
Local lDevFab		:= U_FabrBzl(SF1->F1_TIPO, SF1->F1_FORNECE, SF1->F1_LOJA, SF1->F1_SERIE, SF1->F1_DOC, @aItensNFE)
Local nItem			:= 0

Default cSituac		:= WMS_NF_ENVIAR
Default cJson		:= ""
Default cErrInt		:= ""

If SF1->(eof())
	cErrInt	:= "WIS - SF1 em EOF."
	lRet 	:= .F.
ElseIf (SF1->F1_XWIS == WMS_F1_NAOENVIA .or. SF1->F1_XWIS == WMS_F1_ENVIADO) .and. cSituac == WMS_NF_ENVIAR
	cErrInt	:= "WIS - SF1 " + SF1->(F1_DOC + F1_SERIE + F1_FORNECE + F1_LOJA) + " - Status WIS " + SF1->F1_XWIS + "-" + rtrim(X3Combo("F1_XWIS", SF1->F1_XWIS)) + "."
	lRet 	:= .F.
Else

	DbSelectArea("SD1")
	SD1->(DbSetOrder(1))  // D1_FILIAL, D1_DOC, D1_SERIE, D1_FORNECE, D1_LOJA, D1_COD, D1_ITEM.

	If lDevFab
		If Empty(aItensNFE) .OR. AScan(aItensNFE, {|x| Empty(x[01]) }) > 0
			lRet := .F.
			Aviso("WISSF1", "O Apontamento das OPs vinculadas a esta NF, não foi localizado. Não é possivel enviar os itens para conferencia no WIS.", {"Fechar"})
		Else
			SD1->(DbGoTo(aItensNFE[1][4]))		//Recno do Primeiro registro da Query
		EndIf
	Else
		SD1->(DbSeek(SF1->F1_FILIAL + SF1->F1_DOC + SF1->F1_SERIE + SF1->F1_FORNECE + SF1->F1_LOJA))
	EndIf

	If lRet
		// Indica se a NF é de devolução de cliente.
		lDevol := (aScan({'D', 'B'}, SF1->F1_TIPO) > 0)

		If lDevol
			DbSelectArea("SA1")
			SA1->(dbSetOrder(1))  // A1_FILIAL, A1_COD, A1_LOJA.
			If !SA1->(dbSeek(xFilial("SA1") + SF1->F1_FORNECE + SF1->F1_LOJA))
				cErrInt	:= "Cliente não localizado."
				lRet	:= .F.
			EndIf
		Else
			DbSelectArea("SA2")
			SA2->(dbSetOrder(1))  // A2_FILIAL, A2_COD, A2_LOJA.
			If !SA2->(dbSeek(xFilial("SA2") + SF1->F1_FORNECE + SF1->F1_LOJA))
				cErrInt	:= "Fornecedor não localizado."
				lRet	:= .F.
			EndIf
		EndIf
	EndIf

	// Envia o cliente ou fornecedor para o WIS.
	If lRet
		If cSituac == WMS_NF_ENVIAR
			lRet := U_WISSA2(lDevol)
			If !lRet
				cErrInt := "Erro ao enviar o " + If(lDevMont, "Cliente", "Fornecedor")
			EndIf
		Endif
	Endif

	If lRet
		If SF1->F1_FORNECE == "999999" .AND. !lDevol		//Transferencia (Beneficiamento não pode entrar como Transferencia)
			cTpEntrada := "TRA"
		Else
			If SF1->F1_EST == "EX"			//Importacao
				cTpEntrada := "IMP"
			Else
				If lDevol
					Do Case
					Case AllTrim(SD1->D1_CF) $ "|1910|2910|"		//Bonificacao
						cTpEntrada := "BON"
					Case AllTrim(SD1->D1_CF) $ "|1911|2911|"		//Amostra
						cTpEntrada := "AMO"
					Case AllTrim(SD1->D1_CF) $ "|1913|2913|"		//Demonstracao
						cTpEntrada := "DEM"
					Otherwise							//Outras Devolucoes
						cTpEntrada := "DEV"
					EndCase
				Else
					cTpEntrada := "FOR"					//NF de Compra
				EndIf
			EndIf
		EndIf

		oJson := Array(#)
		oJson[#"usuario"] := "%cUser%"
		oJson[#"senha"]   := "%cPass%"
		oJson[#"recebimento"] := Array(#)
		oJson[#"recebimento"][#"cd_empresa"]             := cWisEmp
		oJson[#"recebimento"][#"cd_deposito"]            := cWisDep
		oJson[#"recebimento"][#"cd_agenda"]              := nil
		oJson[#"recebimento"][#"nu_nota"]                := SF1->F1_DOC
		oJson[#"recebimento"][#"nu_serie_nota"]          := SF1->F1_SERIE
		oJson[#"recebimento"][#"cd_porta"]               := If(lDevol,cPorta,"001")
		oJson[#"recebimento"][#"cd_transportadora"]      := nil
		oJson[#"recebimento"][#"ds_transportadora"]      := nil
		oJson[#"recebimento"][#"cd_cnpj_transportadora"] := nil
		oJson[#"recebimento"][#"cd_fornecedor"]          := SF1->(F1_FORNECE + F1_LOJA) + Iif(lDevol,"C","")
		oJson[#"recebimento"][#"dt_emissao"]             := SF1->F1_EMISSAO
		oJson[#"recebimento"][#"ds_placa"]               := SF1->F1_DOC
		oJson[#"recebimento"][#"dt_agendamento"]         := SF1->F1_DTDIGIT
		oJson[#"recebimento"][#"cd_situacao"]            := cSituac
		oJson[#"recebimento"][#"cd_tipo_nota"]           := cTpEntrada
		oJson[#"recebimento"][#"nu_doc_erp"]             := SF1->(F1_DOC + F1_SERIE + F1_FORNECE + F1_LOJA)
		oJson[#"recebimento"][#"ds_area_erp"]            := If(lDevFab, "01", SD1->D1_LOCAL)
		oJson[#"recebimento"][#"cd_rav"]                 := nil
		oJson[#"recebimento"][#"detalhe"]                := {}  // Itens da NF.

		//NF de Retorno de Producao na Fabrica - Empresa 12 / Filial 11 - Leal
		//Envia para o WIS o produto acabado da OP - SD3
		If lDevFab
			// Monta itens para enviar ao WIS.
			For nItem :=  1 to Len(aItensNFE)
				If lRet
					// Envia o produto para o WIS.
					If cSituac == WMS_NF_ENVIAR
						SB1->(dbSetOrder(1))  // B1_FILIAL, B1_COD.
						SB1->(dbSeek(xFilial() + aItensNFE[nItem][1]))
						lRet := U_WISSB1(@cJson, @cErrInt)
					Endif

					// Adiciona um item de detalhe.
					aAdd(oJson[#"recebimento"][#"detalhe"], Array(#))
					oDetalhe := aTail(oJson[#"recebimento"][#"detalhe"])

					oDetalhe[#"cd_empresa"]         := cWisEmp
					oDetalhe[#"cd_deposito"]        := cWisDep
					oDetalhe[#"cd_cliente"]         := nil
					oDetalhe[#"cd_agenda"]          := nil
					oDetalhe[#"nu_nota"]            := SF1->F1_DOC
					oDetalhe[#"nu_serie_nota"]      := SF1->F1_SERIE
					oDetalhe[#"cd_fornecedor"]      := SF1->(F1_FORNECE + F1_LOJA)  + Iif(lDevol,"C","")
					oDetalhe[#"cd_cgc_fornecedor"]  := If(lDevol, SA1->A1_CGC, SA2->A2_CGC)
					oDetalhe[#"cd_situacao"]        := cSituac
					oDetalhe[#"nu_item_corp"]       := aItensNFE[nItem][3]
					oDetalhe[#"cd_produto"]         := aItensNFE[nItem][1]
					oDetalhe[#"qt_produto"]         := aItensNFE[nItem][2]
					oDetalhe[#"nu_lote"]            := nil
				Endif

				If lRet
					// Guarda item para marcar como enviado.
					aAdd(aItens, aItensNFE[nItem][4])
				EndIf
			Next nItem
		Else
			DbSelectArea("SD1")
			SD1->(DbSetOrder(1))  // D1_FILIAL, D1_DOC, D1_SERIE, D1_FORNECE, D1_LOJA, D1_COD, D1_ITEM.
			If SD1->(DbSeek(SF1->F1_FILIAL + SF1->F1_DOC + SF1->F1_SERIE + SF1->F1_FORNECE + SF1->F1_LOJA))
				While lRet .AND. !SD1->(Eof()) .AND. (SD1->D1_FILIAL + SD1->D1_DOC + SD1->D1_SERIE + SD1->D1_FORNECE + SD1->D1_LOJA + SD1->D1_TIPO == SF1->F1_FILIAL + SF1->F1_DOC + SF1->F1_SERIE + SF1->F1_FORNECE + SF1->F1_LOJA + SF1->F1_TIPO)

					// Verifica se o item movimenta estoque.
					// Itens ainda não classificados também são enviados para o WIS.
					If empty(SD1->D1_TES) .or. Posicione("SF4", 1, xFilial("SF4") + SD1->D1_TES, "F4_ESTOQUE") = "S"
						// Verifica se é uma devolução de item montado.
						lDevMont := .F.
						If SD1->D1_TIPO == "D"
							SD2->(dbSetOrder(3))  // D2_FILIAL, D2_DOC, D2_SERIE, D2_CLIENTE, D2_LOJA, D2_COD, D2_ITEM.
							If SD2->(dbSeek(xFilial() + SD1->(D1_NFORI + D1_SERIORI + D1_FORNECE + D1_LOJA + D1_COD + D1_ITEMORI), .F.))
								SC6->(dbSetOrder(1))  // C6_FILIAL, C6_NUM, C6_ITEM, C6_PRODUTO.
								lDevMont := SC6->(dbSeek(xFilial() + SD2->(D2_PEDIDO + D2_ITEMPV), .F.) .and. !empty(SC6->C6_NUMOP))
							Endif
						Endif
						aEstrut := {}
						If lDevMont
							aEstrut := U_WISStru(SD1->D1_COD, SD1->D1_QUANT)
						Endif
						If empty(aEstrut)
							aAdd(aEstrut, {SD1->D1_COD, SD1->D1_QUANT, 0, 0})
						Endif

						// Monta itens para enviar ao WIS.
						For nX := 1 to len(aEstrut)
							If lRet
								// Envia o produto para o WIS.
								If cSituac == WMS_NF_ENVIAR
									SB1->(dbSetOrder(1))  // B1_FILIAL, B1_COD.
									SB1->(dbSeek(xFilial() + aEstrut[nX, 1], .F.))
									lRet := U_WISSB1(@cJson, @cErrInt)
								Endif

								// Adiciona um item de detalhe.
								aAdd(oJson[#"recebimento"][#"detalhe"], Array(#))
								oDetalhe := aTail(oJson[#"recebimento"][#"detalhe"])

								oDetalhe[#"cd_empresa"]         := cWisEmp
								oDetalhe[#"cd_deposito"]        := cWisDep
								oDetalhe[#"cd_cliente"]         := nil
								oDetalhe[#"cd_agenda"]          := nil
								oDetalhe[#"nu_nota"]            := SD1->D1_DOC
								oDetalhe[#"nu_serie_nota"]      := SD1->D1_SERIE
								oDetalhe[#"cd_fornecedor"]      := SD1->(D1_FORNECE + D1_LOJA)  + Iif(lDevol,"C","")
								oDetalhe[#"cd_cgc_fornecedor"]  := If(lDevol, SA1->A1_CGC, SA2->A2_CGC)
								oDetalhe[#"cd_situacao"]        := cSituac
								oDetalhe[#"nu_item_corp"]       := SD1->D1_ITEM
								oDetalhe[#"cd_produto"]         := aEstrut[nX, 1]
								oDetalhe[#"qt_produto"]         := aEstrut[nX, 2]
								oDetalhe[#"nu_lote"]            := nil
								If !empty(SD1->D1_DFABRIC)
									oDetalhe[#"nu_lote_fornecedor"] := If(empty(SD1->D1_LOTEFOR), nil, SD1->D1_LOTEFOR)
								Endif
								If !empty(SD1->D1_DFABRIC)
									oDetalhe[#"dt_fabricacao"]      := SD1->D1_DFABRIC
								Endif
							Endif
						Next nX

						If lRet
							// Guarda item para marcar como enviado.
							aAdd(aItens, SD1->(RecNo()))
						EndIf
					Endif

					SD1->(dbSkip())
				End
			Else
				cErrInt := "Itens da NF " + SF1->F1_FILIAL + SF1->F1_DOC + SF1->F1_SERIE + SF1->F1_FORNECE + SF1->F1_LOJA + " não localizados."
				lRet	:= .F.
			EndIf
		EndIf

		If lRet
			// Verifica se há algum item na NF a ser enviado para o WIS.
			If len(oJson[#"recebimento"][#"detalhe"]) > 0
				// Envia o Json para o webservice.
				cJson	:= ToJson(oJson, .T.)
				cRet	:= U_WISPost("/entrada/recebimento", oJson, "SF1", @cStsCode)

				If cSituac == WMS_NF_ENVIAR
					If cRet = "SUCCESS"
						cF1Wis := WMS_F1_ENVIADO
					Else
						cF1Wis	:= WMS_F1_ERRO
						cErrInt	:= "Erro ao enviar documento " + SF1->(F1_FILIAL + "-" + F1_SERIE + "/" + F1_DOC) + ":" + CRLF + cRet
						lRet	:= .F.
					Endif
				Elseif cSituac == WMS_NF_CANCELAR
					If cRet = "SUCCESS"
						cF1Wis := WMS_F1_CANCEL
					Else
						cErrInt	:= "Erro ao cancelar documento " + SF1->(F1_FILIAL + "-" + F1_SERIE + "/" + F1_DOC) + " no WIS:" + CRLF + cRet
						lRet	:= .F.
					Endif
				Endif

				// Grava a NF como enviada.
				If !Empty(cF1Wis) .and. SF1->F1_XWIS <> cF1Wis
					RecLock("SF1", .F.)
						SF1->F1_XWIS := cF1Wis
					SF1->(msUnLock())

					For nX := 1 to len(aItens)
						SD1->(dbGoTo(aItens[nX]))
						RecLock("SD1", .F.)
							SD1->D1_XWIS := cF1Wis
						SD1->(msUnLock())
					Next nX
				Endif
			Else
				cErrInt := "Não há itens que movimentem estoque nesse documento."
				lRet	:= .F.
			Endif
		Endif
	Endif
Endif

// Restaura areas de trabalho.
RestArea(aAreaSC6)
RestArea(aAreaSB1)
RestArea(aAreaSD1)
RestArea(aAreaSF1)
RestArea(aAreaSA2)
RestArea(aAreaSA1)
RestArea(aArea)

Return lRet


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Materiais
// Função   : ESF1WIS
// Descrição: Envia nota ou pré-nota de entrada para o webservice WIS.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 16/03/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function ESF1WIS(nOpc)

If nOpc == 1  // Envia nota / pré-nota.
	If SF1->F1_XWIS <> WMS_F1_OK .and. SF1->F1_XWIS <> WMS_F1_ENVIADO
		// Valida controle especial de armazem.
		If SuperGetMV("WIS_JOBINT", NIL, .T.)
			MsAguarde({|| U_WISJINC("/interfacewis/entrada/recebimento", "SF1", SF1->(Recno()), "U_WISSF1", SF1->F1_FILIAL)}, "Aguarde", "Enviando nota...", .F.)  // 1=Inserir.
		Else
			MsAguarde({|| U_WISSF1(WMS_NF_ENVIAR)}, "Aguarde", "Enviando nota...", .F.)  // 1=Inserir.
		EndIf
	Else
		MsgAlert("Nota já enviada ao WIS ou com processo finalizado.")
	Endif

ElseIf nOpc == 2  // Retorno.
	msAguarde({|| U_WISNFEnt(SF1->F1_DOC)}, "Aguarde", "Consultando WIS...", .F.)
Endif

Return


// ##############################################################################
// Projeto  : Bunzl PROT-CAP
// Autor    : Felipe Raposo
// Modulo   : Materiais
// Função   : WISSC5
// Descrição: Envia pedido de venda para o webservice WIS.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 09/04/15 | Felipe Raposo     | Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function WISSC5(cSituac, lAuto, cJson, cErrInt)

Local lRet		:= .T.
Default lAuto	:= .F.
Default cJson	:= ""
Default cErrInt	:= ""

If SC5->C5_XBLQCON $ " |1"
	If IsInCallStack("msAguarde") .OR. lAuto
		lRet := WISSC5(cSituac, lAuto, @cJson, @cErrInt)
	Else
		msAguarde({|| lRet := WISSC5(cSituac, lAuto, @cJson, @cErrInt)}, "WIS - Enviando pedido " + SC5->C5_NUM + "...", "Aguarde", .F.)
	Endif
Else
	lRet := .F.
	HELP(' ',1,"Pedido bloqueado" ,,"Pedido não pode ser enviado para separação porque está bloqueado",2,0,,,,,, {"Solicite o desbloqueio para a gestão comercial."})
EndIf

Return lRet

Static Function WISSC5(cSituac, lAuto, cJson, cErrInt)

Local lRet       := .T.
Local cRet       := ""
Local cAlias     := Alias()
Local aArea      := {}
Local aAreaSC6   := {}
Local cAliasTRB  := ""
Local nX

Local cCliEntr   := ""
Local lDevol     := .F.
Local cObsLog    := ""
Local cC5Wis     := ""
Local nSaldoSB2  := 0
Local nTotal     := 0
Local cMsgErro   := ""
Local bAlert     := {|cMsg| If(lAuto, cErrInt += cMsg, MsgAlert(cMsg, "Atenção"))}
Local aEstoqWIS  := {}
Local lRuptEst   := .F.
Local nEstoqLin  := 0

Local oJson, oDetalhe, aEstrut[0]
Local cStsCode   := ""
Local cOcorLog   := ""
Local cDescLog   := ""
Local dDataLog, cHoraLog

Local cTo        := ""
Local cAssunto   := ""
Local cMsgMail   := ""
Local cTdStyle   := ""
Local cRowSpan   := ""
Local cProduto   := ""
Local cProdDescr := ""

Local cQuery     := ""
Local cAliasTop  := GetNextAlias()

Local cPictQtd   := "@E 999,999,999"
Local cPictVlr   := "@E 999,999,999.99"
Local cPictPer   := "@E 999.9"
Local cMsgCA	 := ""

Local lConsEst   := SuperGetMV("WIS_CONEST",, .T.)
Local cNBlqTPV	 := SuperGetMV("PC_NBLQTPV",,"BEP|BP1|DFQ|SRQ|RC2")	//Tipos de pedidos que nao bloqueia o pedido

Local cWisEmp		:= SuperGetMV("WIS_EMP",, "1")
Local cWisDep		:= SuperGetMV("WIS_DEP",, "001")

Local cMV_XPORTA	:= SuperGetMV("MV_XPORTA",, "2")
Local cFornFab 	:= SuperGetMV("BZ_FORNFAB", NIL, "|0001750002|")		//Fornecedor e Loja da Fabrica - Leal

Default cSituac	:= WMS_PV_ENVIAR
Default lAuto	:= .F.
Default cJson	:= ""
Default cErrInt	:= ""

// Guarda posição das tabelas.
If empty(cAlias)
	cAlias := "SX3"
	dbSelectArea(cAlias)
Endif
aArea := sGetArea()

If SC5->(eof())
	Eval(bAlert, "WIS - SC5 em EOF.")
	lRet := .F.
Else
	If cSituac == WMS_PV_ENVIAR
		If SC5->(C5_XWIS == WMS_C5_NAOENVIA .or. C5_XWIS == WMS_C5_PICK .or. C5_XWIS == WMS_C5_CONF)
			Eval(bAlert, "WIS - SC5 " + SC5->(C5_FILIAL + "-" + C5_NUM) + " - Ação inválida - status WIS " + SC5->C5_XWIS + "-" + rtrim(X3Combo("C5_XWIS", SC5->C5_XWIS)) + ".")
			lRet := .F.
		ElseIf SC5->C5_NOTA <> ' '
			Eval(bAlert, "WIS - SC5 " + SC5->(C5_FILIAL + "-" + C5_NUM) + " - pedido encerrado.")
			lRet := .F.
		Endif
	ElseIf cSituac == WMS_PV_CANCELAR
		If !lAuto .and. !MsgYesNo("Essa rotina irá cancelar o pedido de vendas no WIS. Deseja continuar?")
			Eval(bAlert, "WIS - SC5 " + SC5->(C5_FILIAL + "-" + C5_NUM) + " - envio de cancelamento abortado pelo usuário.")
			lRet := .F.
		Endif
	ElseIf cSituac == WMS_PV_ENV_NF
		If SC5->C5_NOTA = ' ' .or. SC5->C5_NOTA = 'XXXXXX'
			Eval(bAlert, "WIS - SC5 " + SC5->(C5_FILIAL + "-" + C5_NUM) + " - pedido não faturado ou cancelado.")
			lRet := .F.
		Endif
	ElseIf cSituac == WMS_PV_NF_CANC
		If SC5->C5_NOTA = ' ' .or. SC5->C5_NOTA = 'XXXXXX'
			Eval(bAlert, "WIS - SC5 " + SC5->(C5_FILIAL + "-" + C5_NUM) + " - pedido não faturado ou cancelado.")
			lRet := .F.
		Endif
	Endif
Endif

If lRet
	If SoftLock("SC5")
		If lRet .AND. !(cSituac == WMS_PV_CANCELAR .OR. cSituac == WMS_PV_NF_CANC)
			//Valida os CAs dos Produtos do Pedido
			If !(SC5->C5_XTIPO $ cNBlqTPV)	//Nao valida para os tipos de pedidos do parâmetro MV_NBLQTPV
				If !U_VldCAPed(SC5->C5_FILIAL, SC5->C5_NUM, @cMsgCA)
					Eval(bAlert, cMsgCA)
					lRet := .F.
				EndIf
			EndIf
		EndIf

		If lRet
			// Se for envio para separação, faz umas validações antes.
			If cSituac == WMS_PV_ENVIAR
				// Efetua a análise de crédito do pedido.
				If U_MA060Crd(.T.)
					// Efetua a liberação do pedido (gera SC9).
					If U_LibPV(, SC5->C5_TIPLIB = "1", .T.)
						// Verifica se o estoque foi completamente reservado para o pedido.
						If SC5->C5_XESTOQ <> RESERVADO
							Eval(bAlert, "WIS - SC5 " + SC5->(C5_FILIAL + "-" + C5_NUM) + " - pedido não reservado.")
							lRet := .F.
						Endif

						// Verifica se há restrição de crédito ou estoque.
						If lRet
							cAliasTRB := GetNextAlias()
							BeginSQL ALIAS cAliasTRB
								%noParser%
								select sum(SC9.C9_QTDLIB) QTD_BLQ
								from %table:SC9% SC9
								where %notdel%
								and SC9.C9_FILIAL  = %xFilial:SC9%
								and SC9.C9_PEDIDO  = %exp:SC5->C5_NUM%
								and SC9.C9_NFISCAL = ' '
								and not (SC9.C9_BLCRED = ' ' and SC9.C9_BLEST = ' ')
							EndSQL
							If (cAliasTRB)->QTD_BLQ > 0
								Eval(bAlert, "Pedido com restrição de crédito ou estoque. Favor verificar.")
								lRet := .F.
							Endif
							(cAliasTRB)->(dbCloseArea())
							dbSelectArea(cAlias)
						Endif
					Else
						Eval(bAlert, "Erro ao efetuar reserva das mercadorias. Efetue uma alteração no Pedido e envie novamente.")
						lRet := .F.
					Endif
				Else
					U_HistPV(SC5->C5_NUM, "4", "Não enviado para separação", "Pedido sem liberação de crédito", "Sistema")
					lRet := .F.
				Endif
			Endif

			If lRet
				// Cliente de entrega.
				cCliEntr := SC5->(C5_CLIENTE + If(empty(C5_LOJAENT), C5_LOJACLI, C5_LOJAENT))

				// Posiciona outras tabelas.
				sGetArea(aArea, "SA1")
				sGetArea(aArea, "SA2")
				sGetArea(aArea, "SC6")
				sGetArea(aArea, "SF2")
				SA1->(dbSetOrder(1))  // A1_FILIAL, A1_COD, A1_LOJA.
				SA1->(dbSeek(xFilial() + cCliEntr, .F.))
				SA2->(dbSetOrder(1))  // A2_FILIAL, A2_COD, A2_LOJA.
				SA2->(dbSeek(xFilial() + cCliEntr, .F.))
				SA4->(dbSetOrder(1))  // A4_FILIAL, A4_COD.
				SA4->(dbSeek(xFilial() + SC5->C5_TRANSP, .F.))

				// Indica se o pedido é de devolução para fornecedor.
				lDevol := (aScan({'D', 'B'}, SC5->C5_TIPO) > 0)

				// Envia o cliente ou fornecedor para o WIS.
				If cSituac == WMS_PV_ENVIAR
					lRet := lRet .AND. U_WISSA1(lDevol, @cJson, @cErrInt)
					lRet := lRet .AND. U_WISSA4(@cJson, @cErrInt)
				Endif

				If lRet
					oJson := Array(#)
					oJson[#"usuario"] := "%cUser%"
					oJson[#"senha"]   := "%cPass%"
					oJson[#"pedido"] := Array(#)
					oJson[#"pedido"][#"cd_empresa"]             := cWisEmp
					oJson[#"pedido"][#"cd_deposito"]            := cWisDep
					oJson[#"pedido"][#"nu_pedido_origem"]       := SC5->C5_NUM
					oJson[#"pedido"][#"cd_carga"]               := nil
					oJson[#"pedido"][#"cd_situacao"]            := cSituac
					oJson[#"pedido"][#"ds_tppedido"]            := nil
					oJson[#"pedido"][#"cd_porta"]               := IIF(SC5->C5_TPFRETE == "C", IIF(!Empty(SA4->A4_XPORTA), SA4->A4_XPORTA, "0"), cMV_XPORTA)
					oJson[#"pedido"][#"cd_transportadora"]      := SC5->C5_TRANSP
					oJson[#"pedido"][#"cd_cnpj_transportadora"] := SA4->A4_CGC
					oJson[#"pedido"][#"ds_transportadora"]      := SA4->A4_NOME
					oJson[#"pedido"][#"cd_cliente"]             := cCliEntr
					If lDevol
						oJson[#"pedido"][#"tp_pedido"]              := If(SA2->A2_COD + SA2->A2_LOJA == cFornFab, "ESP", "DEV")
						oJson[#"pedido"][#"ds_cliente"]             := SA2->A2_NOME
						oJson[#"pedido"][#"cd_cnpj_cliente"]        := SA2->A2_CGC
						oJson[#"pedido"][#"ds_cliente_entrega"]     := SA2->A2_NOME
						oJson[#"pedido"][#"ds_endereco_entrega"]    := SA2->A2_END
						oJson[#"pedido"][#"ds_bairro_entrega"]      := SA2->A2_BAIRRO
						oJson[#"pedido"][#"ds_municipio_entrega"]   := SA2->A2_MUN
						oJson[#"pedido"][#"cd_uf_entrega"]          := SA2->A2_EST
						oJson[#"pedido"][#"cd_cep_entrega"]         := SA2->A2_CEP
						oJson[#"pedido"][#"nu_telefone_entrega"]    := left(U_Numeros(SA2->A2_TEL), 12)
					Else
						// Tipos de pedido.
						//  A - Pedido de venda normal.
						//  B - Pedido de transferência de filiais.
						//  C - Pedido NetSuprimentos (website).
						//  F - Pedido de venda normal do tipo FOB.
						If SA1->A1_COD = "999999"
							oJson[#"pedido"][#"tp_pedido"] := "TRA"
						ElseIf !empty(SC5->C5_XIDVTV3)
							oJson[#"pedido"][#"tp_pedido"] := "ECO"
						ElseIf SC5->C5_TPFRETE == "F"
							oJson[#"pedido"][#"tp_pedido"] := "PVF"
						Else
							oJson[#"pedido"][#"tp_pedido"] := "PVC"
						Endif

						oJson[#"pedido"][#"ds_cliente"]             := SA1->A1_NOME
						oJson[#"pedido"][#"cd_cnpj_cliente"]        := SA1->A1_CGC
						oJson[#"pedido"][#"ds_cliente_entrega"]     := SA1->A1_NOME
						oJson[#"pedido"][#"nu_telefone_entrega"]    := left(U_Numeros(SA1->A1_TEL), 12)

						// Endereço de entrega.
						If !empty(SC5->C5_XEND)
							oJson[#"pedido"][#"ds_endereco_entrega"]    := SC5->C5_XEND
							oJson[#"pedido"][#"ds_bairro_entrega"]      := SC5->C5_XBAIRRO
							oJson[#"pedido"][#"ds_municipio_entrega"]   := SC5->C5_XCIDADE
							oJson[#"pedido"][#"cd_uf_entrega"]          := SC5->C5_XEST
							oJson[#"pedido"][#"cd_cep_entrega"]         := SC5->C5_XCEP
						ElseIf !empty(SA1->A1_ENDENT)
							oJson[#"pedido"][#"ds_endereco_entrega"]    := SA1->A1_ENDENT
							oJson[#"pedido"][#"ds_bairro_entrega"]      := SA1->A1_BAIRROE
							oJson[#"pedido"][#"ds_municipio_entrega"]   := SA1->A1_MUNE
							oJson[#"pedido"][#"cd_uf_entrega"]          := SA1->A1_ESTE
							oJson[#"pedido"][#"cd_cep_entrega"]         := SA1->A1_CEPE
						Else
							oJson[#"pedido"][#"ds_endereco_entrega"]    := SA1->A1_END
							oJson[#"pedido"][#"ds_bairro_entrega"]      := SA1->A1_BAIRRO
							oJson[#"pedido"][#"ds_municipio_entrega"]   := SA1->A1_MUN
							oJson[#"pedido"][#"cd_uf_entrega"]          := SA1->A1_EST
							oJson[#"pedido"][#"cd_cep_entrega"]         := SA1->A1_CEP
						Endif
					Endif
					oJson[#"pedido"][#"nu_endereco_entrega"]    := nil
					oJson[#"pedido"][#"ds_complemento_entrega"] := nil
					oJson[#"pedido"][#"nu_doc_erp"]             := SC5->C5_NUM
					oJson[#"pedido"][#"vl_nota"]                := nil
					oJson[#"pedido"][#"dt_entrega"]             := DtoC(Date()) + Space(1) + Time()
					oJson[#"pedido"][#"cd_agenda"]              := nil
					oJson[#"pedido"][#"nu_seq_entrega"]         := nil
					oJson[#"pedido"][#"cd_canal"]               := nil
					oJson[#"pedido"][#"ds_canal"]               := nil
					oJson[#"pedido"][#"cd_rota"]                := nil
					oJson[#"pedido"][#"ds_rota"]                := nil
					oJson[#"pedido"][#"id_solicita_controle"]   := nil
					oJson[#"pedido"][#"ds_area_erp"]            := nil
					oJson[#"pedido"][#"dt_faturamento"]         := nil
					oJson[#"pedido"][#"dt_entrada"]             := nil
					oJson[#"pedido"][#"item"]                   := {}  // Itens da NF.

					// Observação do pedido a sair na etiqueta.
					cObsLog := StrTran(StrTran(SC5->C5_XOBSLOG, char(13), ""), char(10), " ")
					cObsLog := "(" + AllTrim(Str(round(SC5->C5_XVALFAT / 1000, 1))) + "K) " + If(empty(SC5->C5_XFUNCAT), "", SC5->C5_XFUNCAT + " - ") + cObsLog
					cObsLog := noAcento(AnsiToOem(AllTrim(left(cObsLog, 375))))
					oJson[#"pedido"][#"ds_observacao"]          := cObsLog

					DbSelectArea("SC6")
					SC6->(dbSetOrder(1))  // C6_FILIAL, C6_NUM, C6_ITEM, C6_PRODUTO.
					If SC6->(dbSeek(SC5->C5_FILIAL + SC5->C5_NUM))
						While lRet .AND. !SC6->(Eof()) .AND. SC6->C6_FILIAL + SC6->C6_NUM == SC5->C5_FILIAL + SC5->C5_NUM

							aAdd(aEstoqWIS, {SC6->(RecNo()), {}}); nEstoqLin++

							// Verifica se o item movimenta estoque.
							SF4->(dbSetOrder(1))  // F4_FILIAL, F4_CODIGO.
							If SF4->(dbSeek(xFilial("SF4") + SC6->C6_TES) .AND. SF4->F4_ESTOQUE = "S" .AND. !(Left(SC6->C6_BLQ, 1) $ "R/S")) .OR. (cSituac == WMS_PV_CANCELAR)

								// Verifica se é uma devolução de item montado.
								aEstrut := {}
								If !empty(SC6->C6_NUMOP)
									aEstrut := U_WISStru(SC6->C6_PRODUTO, SC6->C6_QTDVEN)
								Endif
								If empty(aEstrut)
									aAdd(aEstrut, {SC6->C6_PRODUTO, SC6->C6_QTDVEN, 0, 0})
								Endif

								// Monta itens para enviar ao WIS.
								For nX := 1 to len(aEstrut)
									If lRet
										// Envia o produto para o WIS.
										If cSituac == WMS_PV_ENVIAR
											DbSelectArea("SB1")
											SB1->(dbSetOrder(1))  // B1_FILIAL, B1_COD.
											If SB1->(msSeek(xFilial() + aEstrut[nX, 1]))
												lRet := U_WISSB1(@cJson, @cErrInt)
											Else
												cErrInt := "Produto " + aEstrut[nX, 1] + " não localizado."
												lRet	:= .F.
											EndIf
										Endif

										If lRet .AND. lConsEst
											// Verifica o estoque disponível do produto no WIS.
											aAdd(aEstoqWIS[nEstoqLin, 2], {SB1->B1_COD, U_WISCEst(aEstrut[nX, 1], SC6->C6_LOCAL)})
											If cSituac == WMS_PV_ENVIAR .and. lConsEst .and. aEstoqWIS[nEstoqLin, 2, nX, 2] < aEstrut[nX, 2]
												Eval(bAlert, "Estoque não disponível no WIS - Item " + SC6->C6_ITEM + " - " + RTrim(aEstrut[nX, 1]) + " - qtde " + cValToChar(aEstrut[nX, 2]) + " (local " + SC6->C6_LOCAL + ")")
												lRet     := .F.
												lRuptEst := .T.
											Endif
										Endif

										// Adiciona um item de detalhe.
										If lRet
											aAdd(oJson[#"pedido"][#"item"], Array(#))
											oDetalhe := aTail(oJson[#"pedido"][#"item"])
											oDetalhe[#"cd_empresa"]            := cWisEmp
											oDetalhe[#"cd_deposito"]           := cWisDep
											oDetalhe[#"nu_pedido_origem"]      := SC6->C6_NUM
											oDetalhe[#"nu_item_corp"]          := SC6->C6_ITEM + If(empty(SC6->C6_NUMOP), '00', StrZero(nX, 2))
											oDetalhe[#"cd_produto"]            := aEstrut[nX, 1]
											oDetalhe[#"qt_separar"]            := aEstrut[nX, 2]
											oDetalhe[#"cd_situacao"]           := cSituac
											oDetalhe[#"cd_cliente"]            := cCliEntr
											oDetalhe[#"nu_lote"]               := nil
											oDetalhe[#"nu_nota"]               := If(SC5->C5_NOTA  = ' ', nil, SC5->C5_NOTA)
											oDetalhe[#"nu_serie_nota"]         := If(SC5->C5_SERIE = ' ', nil, SC5->C5_SERIE)
											oDetalhe[#"dt_nota_fiscal"]        := nil
											oDetalhe[#"id_embalagem_presente"] := "N"
											oDetalhe[#"campanha"]              := nil

											Do Case
											Case SC6->C6_LOCAL == "91"
												oJson[#"pedido"][#"tp_pedido"] := "AV"
											Case SC6->C6_LOCAL == "92"
												oJson[#"pedido"][#"tp_pedido"] := "PE"
											Case SC6->C6_LOCAL == "94"
												oJson[#"pedido"][#"tp_pedido"] := "DS"
											EndCase
										Endif
									Endif
								Next nX
							Endif

							SC6->(dbSkip())
						End
					Else
						cErrInt := "Itens do Pedido " + SC5->C5_FILIAL + SC5->C5_NUM + " não localizados."
						lRet	:= .F.
					EndIf

					// Se houve ruptura de estoque, envia e-mail para responsáveis.
					If lRuptEst .and. SC5->C5_XENVWIS <> '1' // Tratativa para enviá-lo somente uma vez.
						// Pega e-mail do operador do pedido.
						cTo := U_EMailPV(2, .T.,, .T.)

						If !empty(cTo)
							cAssunto := "Pedido " + SC5->(C5_FILIAL + "/" + C5_NUM) + " - Estoque não disponível no WIS"

							cMsgMail := U_Txt2HTML(cAssunto) + "<br/>"
							cMsgMail += U_Txt2HTML("Cliente: " + SC5->(C5_CLIENTE + "/" + C5_LOJACLI) + " " + RTrim(U_PVForCli("A1_NOME"))) + "<br/><br/>"

							// Cabeçalho da tabela.
							cMsgMail += "<table style='border:1px solid #002a5c; border-collapse:collapse'>"
							cMsgMail += " <tr>"
							cMsgMail += "  <th style='border:1px solid #002a5c; border-collapse:collapse; font-weight:bold; text-align:left'>Item</th>"
							cMsgMail += "  <th style='border:1px solid #002a5c; border-collapse:collapse; font-weight:bold; text-align:left'>Produto</th>"
							cMsgMail += "  <th style='border:1px solid #002a5c; border-collapse:collapse; font-weight:bold; text-align:left'>Descrição</th>"
							cMsgMail += "  <th style='border:1px solid #002a5c; border-collapse:collapse; font-weight:bold; text-align:center'>Qtde</th>"
							cMsgMail += "  <th style='border:1px solid #002a5c; border-collapse:collapse; font-weight:bold; text-align:right'>Vlr Unit.</th>"
							cMsgMail += "  <th style='border:1px solid #002a5c; border-collapse:collapse; font-weight:bold; text-align:right'>IPI</th>"
							cMsgMail += "  <th style='border:1px solid #002a5c; border-collapse:collapse; font-weight:bold; text-align:right'>ICMS</th>"
							cMsgMail += "  <th style='border:1px solid #002a5c; border-collapse:collapse; font-weight:bold; text-align:right'>Total</th>"
							cMsgMail += "  <th style='border:1px solid #002a5c; border-collapse:collapse; font-weight:bold; text-align:right'>Disp. Protheus</th>"
							cMsgMail += "  <th style='border:1px solid #002a5c; border-collapse:collapse; font-weight:bold; text-align:right'>Disp. WIS</th>"
							cMsgMail += " </tr>"

							nTotal := 0
							aAreaSC6 := SC6->(GetArea())
							For nEstoqLin := 1 to len(aEstoqWIS)
								SC6->(dbGoTo(aEstoqWIS[nEstoqLin, 1]))
								SB2->(dbSetOrder(1))   // B2_FILIAL, B2_COD, B2_LOCAL.
								SB2->(dbSeek(xFilial() + SC6->(C6_PRODUTO + C6_LOCAL), .F.))
								nSaldoSB2 := SaldoSB2()

								cQuery := "select sum(SC9.C9_QTDLIB) QtdLib " + CRLF
								cQuery += "from " + RetSQLName("SC9") + " SC9 " + CRLF
								cQuery += "where SC9.D_E_L_E_T_ = ' ' " + CRLF
								cQuery += "and SC9.C9_FILIAL  = '" + xFilial("SC9") + "' " + CRLF
								cQuery += "and SC9.C9_PEDIDO  = '" + SC6->C6_NUM + "' " + CRLF
								cQuery += "and SC9.C9_ITEM    = '" + SC6->C6_ITEM + "' " + CRLF
								cQuery += "and (SC9.C9_NFISCAL = ' ' and SC9.C9_BLCRED = ' ' and SC9.C9_BLEST = ' ') " + CRLF
								dbUseArea(.T., "TopConn", TCGenQry(,, cQuery), cAliasTop, .F., .F.)
								If (cAliasTop)->(!eof())
									nSaldoSB2 += (cAliasTop)->QtdLib
								Endif
								(cAliasTop)->(dbCloseArea())

								cTdStyle := " style='border:1px solid #002a5c; border-collapse:collapse"
								For nX := 1 to len(aEstoqWIS[nEstoqLin, 2])
									If len(aEstoqWIS[nEstoqLin, 2]) = 1
										cRowSpan := ""
									Else
										cRowSpan := " rowspan= '" + cValToChar(len(aEstoqWIS[nEstoqLin, 2])) + "'"
									Endif
									If len(aEstoqWIS[nEstoqLin, 2]) = 1
										cProduto   := RTrim(SC6->C6_PRODUTO)
										cProdDescr := RTrim(SC6->C6_DESCRI)
									Else
										SB1->(dbSetOrder(1))  // B1_FILIAL, B1_COD.
										SB1->(msSeek(xFilial() + aEstoqWIS[nEstoqLin, 2, nX, 1], .F.))
										cProduto   := RTrim(SC6->C6_PRODUTO) + " (" + RTrim(SB1->B1_COD) + ")"
										cProdDescr := RTrim(SB1->B1_DESC)
									Endif

									cMsgMail += " <tr>"
									If nX == 1
										cMsgMail += "  <td" + cTdStyle + "; text-align:left'"  + cRowSpan + ">" + SC6->C6_ITEM + "</td>"
										cMsgMail += "  <td" + cTdStyle + "; text-align:left'>" + StrTran(cProduto, " ", "&nbsp;") + "</td>"
										cMsgMail += "  <td" + cTdStyle + "; text-align:left'>" + cProdDescr + "</td>"
										cMsgMail += "  <td" + cTdStyle + "; text-align:right'" + cRowSpan + ">" +    AllTrim(Transform(SC6->C6_QTDVEN,  cPictQtd)) + "</td>"
										cMsgMail += "  <td" + cTdStyle + "; text-align:right'" + cRowSpan + ">R$ " + AllTrim(Transform(SC6->C6_PRCVEN,  cPictVlr)) + "</td>"
										cMsgMail += "  <td" + cTdStyle + "; text-align:right'" + cRowSpan + ">" +    AllTrim(Transform(SC6->C6_XALQIPI, cPictPer)) + "%</td>"
										cMsgMail += "  <td" + cTdStyle + "; text-align:right'" + cRowSpan + ">" +    AllTrim(Transform(SC6->C6_XALQICM, cPictPer)) + "%</td>"
										cMsgMail += "  <td" + cTdStyle + "; text-align:right'" + cRowSpan + ">R$ " + AllTrim(Transform(SC6->C6_VALOR,   cPictVlr)) + "</td>"
										cMsgMail += "  <td" + cTdStyle + "; text-align:right'" + cRowSpan + ">" +    AllTrim(Transform(nSaldoSB2,       cPictVlr)) + "</td>"
									Else
										cMsgMail += "  <td" + cTdStyle + "; text-align:left'>" + StrTran(cProduto, " ", "&nbsp;") + "</td>"
										cMsgMail += "  <td" + cTdStyle + "; text-align:left'>" + cProdDescr + "</td>"
									Endif
									cMsgMail += "  <td" + cTdStyle + "; text-align:right'>" + AllTrim(Transform(aEstoqWIS[nEstoqLin, 2, nX, 2], cPictVlr)) + "</td>"
									cMsgMail += " </tr>"
								Next nX
								nTotal += SC6->C6_VALOR
							Next nEstoqLin
							RestArea(aAreaSC6)

							cMsgMail += " <tr>"
							cMsgMail += "  <td" + cTdStyle + "; font-weight:bold; text-align:left' colspan='7'>Total</td>"
							cMsgMail += "  <td" + cTdStyle + "; font-weight:bold; text-align:right'>R$ " + trim(Transform(nTotal, cPictVlr)) + "</td>"
							cMsgMail += " </tr>"
							cMsgMail += "</table>"

							// Envia o e-mail caso o pedido não tenha produto disponível no WIS.
							U_EnvEMail(nil, cTo, cAssunto, cMsgMail,, .T.)

							// Após o envio do primeiro email, grava enviado igual a 1=Sim.
							RecLock("SC5", .F.)
							SC5->C5_XENVWIS := '1'
							SC5->(msUnLock())
						Endif
					Endif

					If lRet
						// Verifica se há algum item no pedido a ser enviado para o WIS.
						If len(oJson[#"pedido"][#"item"]) = 0
							Eval(bAlert, "Não há itens que movimentem estoque nesse pedido.")
							lRet := .F.
						Else
							// Envia o Json para o webservice.
							cJson	:= ToJson(oJson, .T.)
							cRet 	:= U_WISPost("/entrada/pedido", oJson, "SC5", @cStsCode)

							If cSituac == WMS_PV_ENVIAR
								If cRet = "SUCCESS"
									cC5Wis   	:= WMS_C5_PICK
									cOcorLog 	:= "4"  // 4-Logistica.
									cDescLog 	:= "Envio para separação"
								Else
									cC5Wis   	:= WMS_C5_ERRO
									cMsgErro 	:= "Erro ao enviar pedido " + SC5->(C5_FILIAL + "-" + C5_NUM) + "." + cRet
									lRet		:= .F.
								Endif
							Elseif cSituac == WMS_PV_CANCELAR
								If cRet = "SUCCESS"
									cC5Wis   	:= WMS_C5_CANCEL
									cOcorLog 	:= "4"  // 4-Logistica.
									cDescLog 	:= "Separação cancelada"
								Else
									cC5Wis   	:= WMS_C5_ERRO
									cMsgErro 	:= "Erro ao cancelar pedido " + SC5->(C5_FILIAL + "-" + C5_NUM) + " no WIS." + cRet
									lRet		:= .F.
								Endif
							Elseif cSituac == WMS_PV_ENV_NF
								If cRet = "SUCCESS"
									cC5Wis   := WMS_C5_FATUR
									cOcorLog := "9"  // 9-Faturamento.
									cDescLog := "NF-e " + SF2->(F2_SERIE + "/" + F2_DOC) + " autorizada"

									SF2->(dbSetOrder(1))  // F2_FILIAL, F2_DOC, F2_SERIE, F2_CLIENTE, F2_LOJA, F2_FORMUL, F2_TIPO.
									SF2->(dbSeek(xFilial() + SC5->(C5_NOTA + C5_SERIE + C5_CLIENTE + C5_LOJACLI), .F.))
									dDataLog := SF2->F2_DAUTNFE
									cHoraLog := SF2->F2_HAUTNFE
									//Grava data e hora do envio da NF para o WIS
									RecLock("SF2", .F.)
										SF2->F2_XDTWMSE := Date()
										SF2->F2_XHRWMSE := Time()
									SF2->(MSUnlock())
								Else
									cC5Wis   	:= WMS_C5_ERRO
									cMsgErro 	:= "Erro ao enviar nota " + SC5->(C5_FILIAL + "-" + C5_NUM + " - " + C5_SERIE + "-" + C5_NOTA) + " para o WIS." + cRet
									lRet		:= .F.
								Endif
							Elseif cSituac == WMS_PV_ENV_NF
								If cRet = "SUCCESS"
									cC5Wis		:= WMS_C5_CONF
									cOcorLog	:= "4"
									cDescLog	:= "NF Cancelada"
								Else
									cC5Wis		:= WMS_C5_ERRO
									cMsgErro	:= "Erro no envio do Cancelamento da NF ao WIS."
									lRet		:= .F.
								EndIf
							Endif

							// Grava status do pedido no WIS.
							If !empty(cC5Wis) .and. SC5->C5_XWIS <> cC5Wis
								RecLock("SC5", .F.)
								SC5->C5_XWIS := cC5Wis
								If cC5Wis == WMS_C5_PICK
									SC5->C5_XDTWIS := date()
									SC5->C5_XHRWIS := time()
								Endif
								SC5->(msUnLock())
							Endif

							// Grava log de alteração do pedido.
							If !empty(cOcorLog)
								U_HistPV(SC5->C5_NUM, cOcorLog, cDescLog,,, dDataLog, cHoraLog)
							Endif

							If !empty(cMsgErro)
								Eval(bAlert, cMsgErro)
							Endif
						Endif
					Endif
				Endif
			Endif
		Endif

		If !lRet
			U_HistPV(SC5->C5_NUM, "4", If(lRuptEst, "Estoque não disponível no WIS", "Integração WIS"), cErrInt)
		EndIf
		SC5->(MsUnlock())
	Else
		Help(" ", 1, "Help", "WISSC5", "Pedido em manutenção por outro usuário.", 3, 0)
	EndIf
EndIf

// Restaura areas de trabalho.
(cAlias)->(sRestArea(aArea))
dbSelectArea(cAlias)

Return(cRet=="SUCCESS")
//-------------------------------------------------------------------
/*/{Protheus.doc} WISJINC
Inclusao dos Registros para integracao no WIS

@author  Guilherme Santos
@since   20/02/2020
@version 12.1.25
/*/
//-------------------------------------------------------------------
User Function WISJINC(cInterface, cTabela, nRecID, cRotina, cFilReg, cParam)
	Local lRetorno 	:= .T.
	Local cQuery	:= ""
	Local cTabQry	:= ""
	Local cSeqZ12	:= ""
	Local cChave	:= ""
	Local cTipo		:= ""
	Local lIntSC5	:= SuperGetMV("WIS_INTPV", NIL, .F.)		//Determina se a integracao do Pedido de Venda sera Imediata (.T.) ou via Job (.F.)
	Local cJson		:= ""
	Local cErrInt	:= ""
	Local nRegZ12	:= 0
	Local lInvent	:= If(!Empty(cFilReg), SuperGetMV("BZ_INVENT", NIL, NIL, cFilReg), .F.)

	Default cInterface	:= ""
	Default cTabela		:= ""
	Default nRecID		:= 0
	Default cRotina		:= ""
	Default cFilReg		:= ""
	Default cParam		:= ""

	//Não criar fila para pedido com restrição de crédito no cliente
	If cParam == WMS_PV_ENVIAR
		If ! U_MA060Crd(.T.)
			lRetorno := .F.
			U_HistPV(SC5->C5_NUM, "4", "Não enviado para separação", "Pedido sem liberação de crédito", "Sistema")
			Help(" ", 1, "Help", "WISJINC", "Pedido sem liberação de crédito", 3, 0)
		EndIf
	EndIf

	If lRetorno
		If lInvent
			lRetorno := .F.
			Help(" ", 1, "Help", "WISJINC", "Unidade com parâmetro de inventário habilitado ('BZ_INVENT'). Integração não permitida.", 3, 0)
		Else
			DbSelectArea(cTabela)
			DbSetOrder(1)

			(cTabela)->(DbGoTo(nRecID))

			If (cTabela)->(Recno()) == nRecID
				cQuery += "SELECT 	Z10.Z10_FILIAL" + CRLF
				cQuery += ",		Z10.Z10_ID" + CRLF
				cQuery += ",		Z10.Z10_DESCR" + CRLF
				cQuery += "FROM 	" + RetSqlName("Z10") + " Z10" + CRLF
				cQuery += "WHERE 	Z10.Z10_DESCR = '" + Lower(cInterface) + "'" + CRLF
				If !Empty(cFilReg)
					cQuery += "AND	Z10.Z10_FILIAL = '" + cFilReg + "'" + CRLF
				EndIf
				cQuery += "AND		Z10.D_E_L_E_T_ = ''" + CRLF
				cQuery += "ORDER BY Z10.Z10_FILIAL, Z10.Z10_ID" + CRLF

				cTabQry := MpSysOpenQuery(cQuery)

				While !(cTabQry)->(Eof())
					//Chave do Registro para Integracao
					Do Case
					Case cRotina == "U_WISSA1"
						cChave	:= (cTabela)->A1_COD + (cTabela)->A1_LOJA
					Case cRotina == "U_WISSA2"
						cChave	:= (cTabela)->A2_COD + (cTabela)->A2_LOJA
					Case cRotina == "U_WISSA4"
						cChave	:= (cTabela)->A4_COD
					Case cRotina == "U_WISSB1"
						cChave	:= (cTabela)->B1_COD
					Case cRotina == "U_WISSF1"
						cChave := (cTabela)->F1_DOC + (cTabela)->F1_SERIE + (cTabela)->F1_FORNECE + (cTabela)->F1_LOJA + (cTabela)->F1_TIPO
					Case cRotina == "U_WISSC5"
						cChave := (cTabela)->C5_NUM + cParam	//cParam >> 1=WMS_PV_ENVIAR / 2=WMS_PV_CANCELAR / 3=WMS_PV_ENV_NF
					EndCase

					//Retorna se o Recno da Tabela informada já está na fila para Integracao
					If ChkInt((cTabQry)->Z10_FILIAL, (cTabQry)->Z10_ID, cChave, @nRegZ12)
						If cRotina == "U_WISSC5" .AND. lIntSC5
							//Posiciona na Z12
							Z12->(DbGoTo(nRegZ12))

							If nRegZ12 == Z12->(Recno())
								//Dispara a integracao do Pedido de Venda
								lRetorno := U_WISJEXEC(cRotina, Z12->(Recno()), @cJson, @cErrInt)

								If !lRetorno .AND. !IsInCallStack("U_MA061WMS") .AND. !IsInCallStack("U_MM020NFS")
									Help(" ", 1, "Help", "WISJINC", cErrInt, 3, 0)
								EndIf
							EndIf
						EndIf
					Else
						//Recupera a Proxima Sequencia disponivel
						cSeqZ12 := U_PROM650A((cTabQry)->Z10_FILIAL, (cTabQry)->Z10_ID, cChave)

						//Inclui o Registro na Fila para Integracao
						RecLock("Z12", .T.)
							Z12->Z12_FILIAL	:= (cTabQry)->Z10_FILIAL
							Z12->Z12_ID		:= (cTabQry)->Z10_ID
							Z12->Z12_CHAVE	:= cChave
							Z12->Z12_SEQ	:= cSeqZ12
							Z12->Z12_DTINC	:= Date()
							Z12->Z12_HRINC	:= Time()
							Z12->Z12_STPROC	:= "1"			//1=Incluido
							Z12->Z12_ERPFUN	:= cRotina
						MsUnlock()

						If cRotina == "U_WISSC5"
							Do Case
							Case cParam == WMS_PV_ENVIAR
								cTipo := "Enviar Pedido"
							Case cParam == WMS_PV_CANCELAR
								cTipo := "Cancelar Pedido"
							Case cParam == WMS_PV_ENV_NF
								cTipo := "Enviar NF"
							EndCase

							U_HistPV(SC5->C5_NUM, "4", "Integração WIS", "Pedido incluido na fila de envio para o WIS - " + cTipo)
							If !IsInCallStack("MA061ENV")
								Help(" ", 1, "Help", "WISJINC", "Pedido incluido na fila de envio para o WIS - " + cTipo, 3, 0)
							EndIf

							If lIntSC5
								//Dispara a integracao do Pedido de Venda
								lRetorno := U_WISJEXEC(cRotina, Z12->(Recno()), @cJson, @cErrInt)

								If !lRetorno .AND. !IsInCallStack("U_MA061WMS") .AND. !IsInCallStack("U_MM020NFS") .AND. !IsInCallStack("MA061ENV")
									Help(" ", 1, "Help", "WISJINC", cErrInt, 3, 0)
								EndIf
							EndIf
						EndIf
					EndIf

					(cTabQry)->(DbSkip())
				End

				If Select(cTabQry) > 0
					(cTabQry)->(DbCloseArea())
				EndIf
			EndIf
		EndIf
	EndIf

Return lRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} ChkInt
Retorna se o Recno da Tabela informada já está na fila para Integracao

@author  Guilherme Santos
@since   26/02/2020
@version 12.1.25
/*/
//-------------------------------------------------------------------
Static Function ChkInt(cFilInt, cID, cChave, nRegZ12)
	Local cTabQry	:= ""
	Local cQuery	:= ""
	Local lRetorno 	:= .F.

	Default nRegZ12	:= 0

	cQuery += "SELECT 	TOP 1 Z12.R_E_C_N_O_ REGZ12" + CRLF
	cQuery += "FROM		" + RetSQLName("Z12") + " Z12" + CRLF
	cQuery += "WHERE	Z12.Z12_FILIAL = '" + cFilInt + "'" + CRLF
	cQuery += "AND		Z12.Z12_ID = '" + cID + "'" + CRLF
	cQuery += "AND		Z12.Z12_CHAVE = '" + cChave + "'" + CRLF
	cQuery += "AND		Z12.Z12_STPROC IN ('1', '3')" + CRLF		//1-Incluido ou 3-Erro
	cQuery += "AND		Z12.D_E_L_E_T_ = ''" + CRLF
	cQuery += "ORDER BY Z12.R_E_C_N_O_ DESC"

	cTabQry := MpSysOpenQuery(cQuery)

	If !(cTabQry)->(Eof())
		nRegZ12 	:= (cTabQry)->REGZ12
		lRetorno 	:= .T.
	EndIf

	If Select(cTabQry) > 0
		(cTabQry)->(DbCloseArea())
	EndIf

Return lRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} WISJOB
Job de Integracao WIS

@author  Guilherme Santos
@since   26/02/2020
@version 12.1.25
/*/
//-------------------------------------------------------------------
User Function WISJOB(cRotina)
	Local aTabelas	:= {"Z10", "Z12", "Z13", "SA1", "SA2", "SA4", "SB1", "SC5", "SC6", "SC9", "SF1", "SD1"}
	Local cEmpJob	:= "01"
	Local cFilJob	:= "00"
	Local lContinua	:= .T.
	Default cRotina	:= ""

	If !Empty(cRotina)
		RpcSetEnv(cEmpJob, cFilJob, NIL, NIL, "FAT", NIL, aTabelas)

		If cRotina == "U_WISSC5"
			If !LockByName("MA061Man", .T., .F.)
				lContinua := .F.
			ENDIF
		EndIF

		//Cria o Semaforo para Processamento
		If lContinua .AND. MayIUseCode("U_WISJOB" + cRotina)

			U_WISJEXEC(cRotina)

			//Libera o Semaforo
			FreeUsedCode()

			If cRotina == "U_WISSC5"
				UnlockByName("MA061Man", .T., .F., .F.)
			EndIf
		EndIf
	EndIf

Return NIL
//-------------------------------------------------------------------
/*/{Protheus.doc} WISJEXEC
Executa o Job de Integracao do WIS da Rotina informada no parametro

@author  Guilherme Santos
@since   26/02/2020
@version 12.1.25
/*/
//-------------------------------------------------------------------
User Function WISJEXEC(cRotina, nRegZ12, cJson, cErrInt)
	Local cFilBkp	:= cFilAnt
	Local cQuery 	:= ""
	Local cTabQry	:= ""
	Local cChave	:= ""
	Local lEnviou	:= .F.

	Default cRotina := AllTrim(Z12->Z12_ERPFUN)
	Default nRegZ12	:= 0
	Default cJson	:= ""
	Default cErrInt	:= ""

	cQuery += "SELECT	Z12.Z12_FILIAL" + CRLF
	cQuery += ",		Z12.Z12_ID" + CRLF
	cQuery += ",		Z12.Z12_CHAVE" + CRLF
	cQuery += ",		Z12.Z12_SEQ" + CRLF
	cQuery += ",		Z12.R_E_C_N_O_ REGZ12" + CRLF
	cQuery += ",		Z10.Z10_TENTAT" + CRLF
	cQuery += "FROM		" + RetSqlName("Z12") + " Z12" + CRLF
	cQuery += "			INNER JOIN" + CRLF
	cQuery += "			" + RetSqlName("Z10") + " Z10" + CRLF
	cQuery += "			ON Z10.Z10_FILIAL = Z12.Z12_FILIAL" + CRLF
	cQuery += "			AND Z10.Z10_ID = Z12.Z12_ID" + CRLF
	cQuery += "			AND Z10.D_E_L_E_T_ = ''" + CRLF

	If !Empty(nRegZ12)
		cQuery += "WHERE	Z12.R_E_C_N_O_ = " + AllTrim(Str(nRegZ12)) + CRLF
	Else
		cQuery += "WHERE	Z12.Z12_FILIAL IN (" + U_GetFlWIS(1) + ")" + CRLF
		cQuery += "AND		Z12.Z12_ERPFUN = '" + cRotina + "'" + CRLF		//"U_WISSB1", "U_WISSA1", "U_WISSA2", "U_WISSA4", "U_WISSF1", "U_WISSC5"
		cQuery += "AND		Z12.Z12_STPROC IN ('1', '3')" + CRLF
		cQuery += "AND		Z12.D_E_L_E_T_ <> '*'" + CRLF
	EndIf

	cQuery += "ORDER BY Z12.R_E_C_N_O_" + CRLF

	cTabQry := MpSysOpenQuery(cQuery)

	While !(cTabQry)->(Eof())
		cFilAnt 	:= (cTabQry)->Z12_FILIAL
		cChave		:= (cTabQry)->Z12_CHAVE
		cJson		:= ""
		cErrInt		:= ""
		lEnviou		:= .F.
		cOpcao		:= ""

		//Gravacao do Inicio do Processamento
		U_PROM650E(1, (cTabQry)->REGZ12, cJson, cErrInt, 0, cRotina, (cTabQry)->Z10_TENTAT)

		Do Case
		Case cRotina == "U_WISSB1"
			DbSelectArea("SB1")
			DbSetOrder(1)		//B1_FILIAL, B1_COD

			If SB1->(DbSeek(xFilial("SB1") + Left(cChave, TamSX3("B1_COD")[1])))
				If SoftLock("SB1")
					If U_WISSB1(@cJson, @cErrInt)
						lEnviou := .T.
					EndIf
					SB1->(MsUnlock())
				Else
					cErrInt := "Produto " + AllTrim(SB1->B1_COD) + " bloqueado por outro usuario."
				EndIf
			EndIf
		Case cRotina == "U_WISSA1"
			DbSelectArea("SA1")
			DbSetOrder(1)		//A1_FILIAL, A1_COD, A1_LOJA

			If SA1->(DbSeek(xFilial("SA1") + Left(cChave, TamSX3("A1_COD")[1] + TamSX3("A1_LOJA")[1])))
				If SoftLock("SA1")
					If U_WISSA1(.F., @cJson, @cErrInt)
						lEnviou := .T.
					EndIf
					SA1->(MsUnlock())
				Else
					cErrInt := "Cliente " + AllTrim(SA1->A1_COD + SA1->A1_LOJA) + " bloqueado por outro usuario."
				EndIf
			EndIf
		Case cRotina == "U_WISSA2"
			DbSelectArea("SA2")
			DbSetOrder(1)		//A2_FILIAL, A2_COD, A2_LOJA

			If SA2->(DbSeek(xFilial("SA2") + Left(cChave, TamSX3("A2_COD")[1] + TamSX3("A2_LOJA")[1])))
				If SoftLock("SA2")
					If U_WISSA2(.F., @cJson, @cErrInt)
						lEnviou := .T.
					EndIf
					SA2->(MsUnlock())
				Else
					cErrInt := "Fornecedor " + AllTrim(SA2->A2_COD + SA2->A2_LOJA) + " bloqueado por outro usuario."
				EndIf
			EndIf
		Case cRotina == "U_WISSA4"
			DbSelectArea("SA4")
			DbSetOrder(1)		//A4_FILIAL, A4_COD

			If SA4->(DbSeek(xFilial("SA4") + Left(cChave, TamSX3("A4_COD")[1])))
				If SoftLock("SA4")
					If U_WISSA4(@cJson, @cErrInt)
						lEnviou := .T.
					EndIf
					SA4->(MsUnlock())
				Else
					cErrInt := "Transportadora " + AllTrim(SA4->A4_COD) + " bloqueada por outro usuario."
				EndIf
			EndIf
		Case cRotina == "U_WISSF1"
			DbSelectArea("SF1")
			DbSetOrder(1)		//F1_FILIAL, F1_DOC, F1_SERIE, F1_FORNECE, F1_LOJA, F1_TIPO

			If SF1->(DbSeek(xFilial("SF1") + Left(cChave, TamSX3("F1_DOC")[1] + TamSX3("F1_SERIE")[1] + TamSX3("F1_FORNECE")[1] + TamSX3("F1_LOJA")[1] + TamSX3("F1_TIPO")[1])))
				If SoftLock("SF1")
					If U_WISSF1("1", @cJson, @cErrInt)
						lEnviou := .T.
					EndIf
					SF1->(MsUnlock())
				Else
					cErrInt := "NF de Entrada " + SF1->F1_DOC + SF1->F1_SERIE + SF1->F1_FORNECE + SF1->F1_LOJA + SF1->F1_TIPO + " bloqueada por outro usuario."
				EndIf
			EndIf
		Case cRotina == "U_WISSC5"
			DbSelectArea("SC5")
			DbSetOrder(1)		//C5_FILIAL, C5_NUM

			If SC5->(DbSeek(xFilial("SC5") + Left(cChave, TamSX3("C5_NUM")[1])))
				cOpcao := SubStr(cChave, TamSX3("C5_NUM")[1] + 1, 1)		//WMS_PV_ENVIAR | WMS_PV_CANCELAR | WMS_PV_ENV_NF

				If cOpcao == "3" .AND. (Empty(SC5->C5_NOTA) .OR. SC5->C5_NOTA == "XXXXXXXXX")
					cErrInt := "Pedido não faturado"
				Else
					If SoftLock("SC5")
						If U_WISSC5(cOpcao, .T., @cJson, @cErrInt)
							lEnviou	:= .T.
						EndIf
						SC5->(MsUnlock())
					Else
						cErrInt := "Pedido " + AllTrim(SC5->C5_NUM) + " bloqueado por outro usuario."
					EndIf
				EndIf
			EndIf
		EndCase

		//Grava o Resultado da integracao nas tabelas Z12 e Z13
		U_PROM650E(If(lEnviou, 2, 3), (cTabQry)->REGZ12, cJson, cErrInt, 0, cRotina, (cTabQry)->Z10_TENTAT)

		cFilAnt := cFilBkp

		(cTabQry)->(DbSkip())
	End

Return lEnviou
//-------------------------------------------------------------------
/*/{Protheus.doc} GetFlWIS
Retorna as Filiais do Sistema que tem Integracao com o WIS

@author  Guilherme Santos
@since   26/02/2020
@version 12.1.25
@param nFormato, Formato do Retorno, 1=String ou 2=Array
/*/
//-------------------------------------------------------------------
User Function GetFlWIS(nFormato)
	Local aFilWIS 		:= {}
	Local aFilProc		:= {}
	Local cFilWIS		:= ""
	Local nFilProc		:= 0
	Default nFormato	:= 1

	U_Filiais("", aFilProc)

	For nFilProc := 1 to Len(aFilProc)
		If U_BZGetPar("BZ_WMS", "", aFilProc[nFilProc]) == "WIS"
			Aadd(aFilWIS, aFilProc[nFilProc])
			cFilWIS += If(!Empty(cFilWIS), ",", "") + "'" + aFilProc[nFilProc] + "'"
		EndIf
	Next nFilProc

Return If(nFormato == 1, cFilWIS, aFilWIS)
//-------------------------------------------------------------------
/*/{Protheus.doc} fBarForn
Retorna se os Codigos de Barras devem ser enviados para o WIS

@author  Guilherme Santos
@since   01/02/2021
@version 12.1.25
/*/
//-------------------------------------------------------------------
User Function fBarForn(cProduto)
	Local cQuery	:= ""
	Local cTabQry	:= ""
	Local lContinua	:= SA2->(FieldPos("A2_XWISBAR")) > 0
	Local lRetorno 	:= .T.

	If lContinua
		cQuery += "SELECT	COUNT(SA2.A2_XWISBAR) XWISBAR" + CRLF
		cQuery += "FROM		" + RetSqlName("SB1") + " SB1" + CRLF
		cQuery += "			INNER JOIN" + CRLF
		cQuery += "			" + RetSqlName("SA5") + " SA5" + CRLF
		cQuery += "			ON SA5.A5_FILIAL = '" + xFilial("SA5") + "'" + CRLF
		cQuery += "			AND SA5.A5_PRODUTO = SB1.B1_COD" + CRLF
		cQuery += "			AND SA5.A5_FORNECE <> '999999'" + CRLF
		cQuery += "			AND SA5.D_E_L_E_T_ = ''" + CRLF
		cQuery += "			INNER JOIN" + CRLF
		cQuery += "			" + RetSQLName("SA2") + " SA2" + CRLF
		cQuery += "			ON SA2.A2_FILIAL = ''" + CRLF
		cQuery += "			AND SA2.A2_COD = SA5.A5_FORNECE" + CRLF
		cQuery += "			AND SA2.A2_LOJA = SA5.A5_LOJA" + CRLF
		cQuery += "			AND SA2.A2_XWISBAR = '1'" + CRLF
		cQuery += "			AND SA2.D_E_L_E_T_ = ''" + CRLF
		cQuery += "WHERE	SB1.B1_FILIAL = '" + xFilial("SB1") + "'" + CRLF
		cQuery += "AND		SB1.B1_COD = '" + cProduto + "'" + CRLF
		cQuery += "AND		SB1.D_E_L_E_T_ = ''" + CRLF

		cTabQry := MpSysOpenQuery(cQuery)

		While !(cTabQry)->(Eof())
			If (cTabQry)->XWISBAR == 0
				lRetorno := .F.
			EndIf

			(cTabQry)->(DbSkip())
		End

		If Select(cTabQry) > 0
			(cTabQry)->(DbCloseArea())
		EndIf
	EndIf

Return lRetorno
//-------------------------------------------------------------------
/*/{Protheus.doc} FabrBzl
Retorno se e uma NF de Devolucao da Fabrica com OP - Empresa 12

@author  Guilherme Santos
@since   23/01/2024
@version 12.1.2210
/*/
//-------------------------------------------------------------------
User Function FabrBzl(cTipoNF, cFornece, cLojaFor, cSerie, cNumNFE, aItensNFE)
	Local lRetorno 	:= .T.
	Local cFornFab 	:= SuperGetMV("BZ_FORNFAB", NIL, "|0001750002|")		//Fornecedor e Loja da Fabrica - Leal
	Local cQuery 	:= ""
	Local cTabQry	:= ""

	//Tipo de NF Normal (Fornecedor) e Fornecedor da Fabrica
	If cTipoNF == "N" .AND. cFornece + cLojaFor $ cFornFab

		//Retorna somente o primeiro item da NF relacionado a OP
		//O retorno da conferencia do WIS sera gravado somente no
		//primeiro item da NF relacionado a OP

		//Verifica se os itens tem OP amarrada e se a OP está apontada
		//Retorna somente o primeiro item relacionado a OP

		cQuery += "SELECT	ISNULL(SD3.D3_COD, '')		D3_COD" + CRLF
		cQuery += ",		ISNULL(SD3.D3_QUANT, 0)		D3_QUANT" + CRLF
		cQuery += ",		ISNULL(SD3.D3_LOCAL, '')	D3_LOCAL" + CRLF
		cQuery += ",		SD1.D1_ITEM		" + CRLF
		cQuery += ",		SD1.REGSD1" + CRLF
		cQuery += "FROM	(	SELECT	SD1.D1_FILIAL" + CRLF
		cQuery += "			,		SD1.D1_DOC" + CRLF
		cQuery += "			,		SD1.D1_OP" + CRLF
		cQuery += "			,		MIN(SD1.D1_ITEM)	D1_ITEM		" + CRLF
		cQuery += "			,		MIN(SD1.R_E_C_N_O_)	REGSD1" + CRLF
		cQuery += "			FROM	" + RetSqlName("SD1") + " SD1 (NOLOCK)" + CRLF
		cQuery += "					INNER JOIN" + CRLF
		cQuery += "					" + RetSqlName("SF4") + " SF4 (NOLOCK)" + CRLF
		cQuery += "					ON SF4.F4_FILIAL = ''" + CRLF
		cQuery += "					AND SF4.F4_CODIGO = SD1.D1_TES" + CRLF
		cQuery += "					AND SF4.F4_ESTOQUE = 'S'" + CRLF
		cQuery += "					AND SF4.F4_PODER3 = 'D'" + CRLF
		cQuery += "					AND SF4.D_E_L_E_T_ = ''" + CRLF
		cQuery += "			WHERE	SD1.D1_FILIAL = '" + xFilial("SD1") + "'" + CRLF
		cQuery += "			AND		SD1.D1_DOC = '" + cNumNFE + "'" + CRLF
		cQuery += "			AND		SD1.D1_SERIE = '" + cSerie + "'" + CRLF
		cQuery += "			AND		SD1.D1_FORNECE = '" + cFornece + "'" + CRLF
		cQuery += "			AND		SD1.D1_LOJA = '" + cLojaFor + "'" + CRLF
		cQuery += "			AND		SD1.D1_TIPO = 'N'" + CRLF
		cQuery += "			AND		SD1.D1_OP <> ''" + CRLF
		cQuery += "			AND		SD1.D_E_L_E_T_ = ''" + CRLF
		cQuery += "			GROUP BY SD1.D1_FILIAL, SD1.D1_DOC, SD1.D1_OP) SD1" + CRLF
		cQuery += "		LEFT JOIN " + CRLF
		cQuery += "		(	SELECT	SD3.D3_FILIAL" + CRLF
		cQuery += "			,		SD3.D3_OP" + CRLF
		cQuery += "			,		SD3.D3_DOC" + CRLF
		cQuery += "			,		SD3.D3_COD" + CRLF
		cQuery += "			,		SD3.D3_QUANT" + CRLF
		cQuery += "			,		SD3.D3_LOCAL" + CRLF
		cQuery += "			FROM	" + RetSqlName("SD3") + " SD3 (NOLOCK)" + CRLF
		cQuery += "			WHERE	SD3.D3_FILIAL = '" + xFilial("SD3") + "'" + CRLF
		cQuery += "			AND		SD3.D3_CF = 'PR0'" + CRLF
		cQuery += "			AND		SD3.D3_ESTORNO = ''" + CRLF
		cQuery += "			AND		SD3.D_E_L_E_T_ = '') SD3" + CRLF
		cQuery += "			ON		SD3.D3_FILIAL = SD1.D1_FILIAL" + CRLF
		cQuery += "			AND		SD3.D3_OP = SD1.D1_OP" + CRLF
		cQuery += "ORDER BY SD1.D1_ITEM" + CRLF

		cTabQry := MpSysOpenQuery(cQuery)

		If (cTabQry)->(Eof())
			lRetorno := .F.
		Else
			While !(cTabQry)->(Eof())
				Aadd(aItensNFE, {	(cTabQry)->D3_COD,;
									(cTabQry)->D3_QUANT,;
									(cTabQry)->D1_ITEM,;		//Retorna somente o primeiro item da NF relacionado a OP
									(cTabQry)->REGSD1,;			//Recno do Registro para posicionar na montagem do JSon
									(cTabQry)->D3_LOCAL})		//Armazem de destino da Producao

				(cTabQry)->(DbSkip())
			End
		EndIf
	Else
		lRetorno := .F.
	EndIf

Return lRetorno
