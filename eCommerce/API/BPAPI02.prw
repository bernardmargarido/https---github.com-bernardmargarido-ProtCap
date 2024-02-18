#INCLUDE "PROTHEUS.CH"
#INCLUDE "TOTVS.CH"
#INCLUDE "RESTFUL.CH"
#INCLUDE "FWMVCDEF.CH"

#DEFINE CRLF CHR(13) + CHR(10)

//----------------+
// Dummy function |
//----------------+
User Function BpApi02()
Return Nil 

/**********************************************************************************************************/
/*/{Protheus.doc} DANA - Calculo de Impostos
    @description API - Realiza o calculo de impostos para checkout B2B
    @type  Function
    @author Bernard M. Margarido
    @since 30/06/2023
/*/
/**********************************************************************************************************/
WSRESTFUL CalculaImposto DESCRIPTION "Calcula Impostos para o checkout B2B."

    WSMETHOD POST CalculaImposto ;
    DESCRIPTION "Realiza o calculo dos impostos B2B." ;
    WSSYNTAX "/CalculaImposto" ;
    PATH "/CalculaImposto";
    PRODUCES APPLICATION_JSON
    
ENDWSRESTFUL

/**********************************************************************************************************/
/*/{Protheus.doc} POST - Realiza calculo dos impostos 
    @description API - Área Logada 
    @type  Function
    @author Bernard M. Margarido
    @since 10/12/2021
/*/
/**********************************************************************************************************/
WSMETHOD POST CalculaImposto  WSSERVICE CalculaImposto
Local _cBody        := ""
Local _cCgc         := ""
Local _cCod         := ""
Local _cLoja        := ""
Local _cTipo        := ""
Local _cTpOper      := GetNewPar("EC_TPOPEREC",,"01")
Local _cCondPg      := GetMv("DN_CPAGTAX",,"100")
Local _cTpFrete     := ""
Local _cError       := ""
Local _cJson        := ""
Local _cFilAux      := cFilAnt
Local _cFiLDef      := "05"
Local _cNatVnd      := ""

Local _nX           := 0 
Local _nValMerc     := 0
Local _nRecno       := 0
Local _nTotal       := 0
Local _nFrete       := 0
Local _nDespesa     := 0

Local _oOrder       := JSonObject():New()
Local _oImpostos    := Nil
Local _oItems       := Nil 

Local _lZeraFrete   := GetMv("DN_FRETZER",,.T.)
Local _lCorporate   := .F.

//--------------------------+
// Posiciona filial correta |
//--------------------------+
If cFilAnt <> _cFiLDef
    cFilAnt := _cFiLDef
EndIf 

Self:SetContentType("application/json")

//---------------------+
// Corpo da Requisição |
//---------------------+
_cBody := Self:GetContent()

If Empty(_cBody)
    SetRestFault(400,EncodeUTF8("Dados do pedido não recebido."))
    Return .F.
EndIf 

//------------------------------------+
// Cria objeto com os dados do pedido |
//------------------------------------+
_oOrder:fromJson(_cBody)

//-----------------------+
// Valida tipo de objeto |
//-----------------------+
If ValType(_oOrder) == "J" .And. Len(_oOrder) > 0 
    _oImpostos := _oOrder[1]
ElseIf ValType(_oOrder) == "J"
    _oImpostos := _oOrder
Else 
    SetRestFault(400,EncodeUTF8("Erro de parser do Objeto."))
    Return .F.
EndIf 

//---------------------+
// CPF/CNPJ do cliente |
//---------------------+
_lCorporate     := _oImpostos['clientProfileData']['isCorporate']

If _lCorporate
    _cCgc       := u_ECFORMAT(_oImpostos['clientProfileData']['corporateDocument'],"A1_CGC",.T.)
Else 
    _cCgc       := u_ECFORMAT(_oImpostos['clientProfileData']['document'],"A1_CGC",.T.)
EndIf 

//--------------------------+
// Valida se existe cliente |
//--------------------------+
If !BpApi02A(_cCgc,@_nRecno)

    SetRestFault(400,EncodeUTF8("Cliente não localizado em nossa base de dados."))
    Return .F.
    
EndIf 

//-------------------------+
// SA1 - Posiciona Cliente |
//-------------------------+
dbSelectArea("SA1")
SA1->( dbGoTo(_nRecno))

//------------------------------------+
// Parametros para calculo do imposto |
//------------------------------------+
_cCod       := SA1->A1_COD
_cLoja      := SA1->A1_LOJA
_cTipo      := SA1->A1_TIPO
_cTpFrete   := IIF(Empty(SA1->A1_TPFRET),"F",SA1->A1_TPFRET)

//----------------------------------------+
// Calcula imposto / cria json de retorno |
//----------------------------------------+
_oItems     := _oImpostos["items"]
_nTotal     := ConvertVlr(_oImpostos["value"],1)

For _nX := 1 To Len(_oImpostos["totalizers"])
    If _oImpostos["totalizers"][_nX]["id"] == "Items"
        _nValMerc   := ConvertVlr(_oImpostos["totalizers"][_nX]["value"],1)
    ElseIf _oImpostos["totalizers"][_nX]["id"] == "Shipping" 
        _nFrete     := ConvertVlr(_oImpostos["totalizers"][_nX]["value"],1)
    EndIf 
Next _nX 

_nFrete     := IIF(_lZeraFrete, 0, _nFrete)
_nDespesa   := 0

If !BpApi02B(_cCgc,_cCod,_cLoja,_cTipo,_cCondPg,_cTpFrete,_cTpOper,_nTotal,_nValMerc,_nFrete,_nDespesa,_oItems,@_cJson,@_cError)

    SetRestFault(400,EncondeUTF8(_cError))
    Return .F.

EndIf 

//-------------+
// Retorno API |
//-------------+
Self:SetResponse( _cJson )

//-----------------+
// Restaura filial |
//-----------------+
If cFilAnt <> _cFilAux
    cFilAnt := _cFilAux
EndIf 

Return .T. 

/**********************************************************************************************************/
/*/{Protheus.doc} BpApi02A
    @description Busca CNPJ/CPF 
    @type  Static Function
    @author Bernard M Margarido
    @since 19/07/2022
    @version version
/*/
/**********************************************************************************************************/
Static Function BpApi02A(_cCgc,_lProspect,_nRecno)
Local _cQuery   := ""
Local _cAlias   := ""

_cQuery := " SELECT " + CRLF 
_cQuery += "	RECNO, " + CRLF 
_cQuery += "	TIPO " + CRLF 
_cQuery += " FROM " + CRLF 
_cQuery += " ( " + CRLF 
_cQuery += "	SELECT " + CRLF 
_cQuery += "		A1.R_E_C_N_O_ RECNO, " + CRLF 
_cQuery += "		'CLI' TIPO " + CRLF 
_cQuery += "	FROM " + CRLF 
_cQuery += "		" + RetSqlName("SA1") + " A1 " + CRLF 
_cQuery += "	WHERE " + CRLF 
_cQuery += "		A1_CGC = '" + _cCgc + "' AND " + CRLF 
_cQuery += "		A1.D_E_L_E_T_ = '' " + CRLF 
_cQuery += " )CLI_PROS "

_cAlias := MPSysOpenQuery(_cQuery)

//-------------------------------------------------------------------+
// Caso não encontrado insere o cliente na base de dados do protheus |
//-------------------------------------------------------------------+
_nRecno     := (_cAlias)->RECNO

(_cAlias)->( dbCloseArea() )

Return IIF(_nRecno > 0 ,.T., .F.)

/**********************************************************************************************************/
/*/{Protheus.doc} BpApi02B
    @description Realiza o calculo dos impostos e retorna 
    @type  Static Function
    @author Bernard M Margarido
    @since 19/07/2022
    @version version
/*/
/**********************************************************************************************************/
Static Function BpApi02B(_cCgc,_cCod,_cLoja,_cTipo,_cCondPg,_cTpFrete,_cTpOper,_nTotal,_nValMerc,_nFrete,_nDespesa,_oItems,_cJson,_cError)
Local _cCodProd     := ""

Local _nX           := 0 
Local _nFretePL     := 0 
Local _aItensPV     := {}
Local _aTotais      := {}

Local _oJSon        := Nil
Local _oItImp       := Nil 

Local _lRet         := .T.

//Local _nTotPrcLista := 0 
//Local _nTotItIpiPl  := 0 
//Local _aItens       := {}

//-------------------------+
// SB1 - Posiciona produto |
//-------------------------+
dbSelectArea("SB1")
SB1->( dbSetOrder(1) )

//---------------------+
// SF4 - Posiciona TES |
//---------------------+
dbSelectArea("SF4")
SF4->( dbSetOrder(1) )

//------------------------------------+
// SE4 - Posiciona condição pagamento |
//------------------------------------+
dbSelectArea("SE4")
SE4->( dbSetOrder(1) )
If !SE4->( dbSeek(xFilial("SE4") + _cCondPg) )
    _cError := "Condição de pagamento não localizada."
    Return .F.
EndIf 

//---------------------------+
// Calcula total Preço Lista |
//---------------------------+
BpApi02C(_oItems,_nFrete,@_nValMerc,@_nFretePL) 

//-----------------------------+
// Calcula imposto preço venda |
//-----------------------------+
BpApi02D(_cCod,_cLoja,_cTipo,_cTpFrete,_cTpOper,_nTotal,_nValMerc,_nFretePL,_nFrete,_nDespesa,_oItems,@_aItensPV,@_aTotais)

//--------------------------+
// SB1 - Posiciona Produtos |
//--------------------------+
dbSelectArea("SB1")
SB1->( dbSetOrder(1) )

//----------------------+
// Gera JSON de retorno | 
//----------------------+
_oJSon                      := JsonObject():New()
_oJSon["itens"]             := {}

For _nX := 1 To Len(_aItensPV)

    //-------------------+
    // Posiciona produto |
    //-------------------+
    SB1->( dbSeek(xFilial("SB1") + _aItensPV[_nX][3]) )

    //------------+
    // Itens JSON |
    //------------+
    _cCodProd                       := SB1->B1_COD
    _oItImp                         := JsonObject():New()
    _oItImp["id"]                   := RTrim(_aItensPV[_nX][1])
    _oItImp["productId"]            := RTrim(_aItensPV[_nX][2])
    _oItImp["refId"]                := RTrim(_aItensPV[_nX][3])
    _oItImp["ean"]                  := RTrim(_aItensPV[_nX][4])
    _oItImp["price"]                := ConvertVlr(_aItensPV[_nX][6],2)

    //----------------------+        
    // Cria objeto impostos |
    //----------------------+        
    _oItImp["impostos"]             := JSonObject():New()
    _oImpostos                      := _oItImp["impostos"] 

    //------+
    // ICMS |
    //------+
    _oImpostos["base_icms"]            := ConvertVlr(_aItensPV[_nX][8],2)
    _oImpostos["aliq_icms"]            := ConvertVlr(_aItensPV[_nX][9],2)
    _oImpostos["valor_icms"]           := ConvertVlr(_aItensPV[_nX][10],2)

    
    //-----+
    // IPI |
    //-----+
    _oImpostos["base_ipi"]             := ConvertVlr(_aItensPV[_nX][11],2)
    _oImpostos["aliq_ipi"]             := ConvertVlr(_aItensPV[_nX][12],2)
    _oImpostos["valor_ipi"]            := ConvertVlr(_aItensPV[_nX][13],2)

    //-----+
    // PIS |
    //-----+
    _oImpostos["base_pis"]             := ConvertVlr(_aItensPV[_nX][14],2)
    _oImpostos["aliq_pis"]             := ConvertVlr(_aItensPV[_nX][15],2)
    _oImpostos["valor_pis"]            := ConvertVlr(_aItensPV[_nX][16],2)

    //--------+
    // COFINS |
    //--------+
    _oImpostos["base_cofins"]          := ConvertVlr(_aItensPV[_nX][17],2)
    _oImpostos["aliq_cofins"]          := ConvertVlr(_aItensPV[_nX][18],2)
    _oImpostos["valor_cofins"]         := ConvertVlr(_aItensPV[_nX][19],2)

    //---------+
    // ICMS ST |
    //---------+
    _oImpostos["base_solidario"]       := ConvertVlr(_aItensPV[_nX][20],2)
    _oImpostos["aliq_solidario"]       := ConvertVlr(_aItensPV[_nX][21],2)
    _oImpostos["valor_solidario"]      := ConvertVlr(_aItensPV[_nX][22],2)

    //-------------------+
    // ICMS Complementar |
    //-------------------+
    _oImpostos["aliq_icmscomp"]        := ConvertVlr(_aItensPV[_nX][23],2)
    _oImpostos["valor_icmscomp"]       := ConvertVlr(_aItensPV[_nX][24],2)

    //---------------+
    // DIFAL Destino | 
    //---------------+
    _oImpostos["valor_difal"]          := ConvertVlr(_aItensPV[_nX][25],2)

    //--------------------+
    // FECP DIFAL Destino |
    //--------------------+
    _oImpostos["valor_fecp_difal"]     := ConvertVlr(_aItensPV[_nX][26],2)

    //-------------+
    // ZONA FRANCA |
    //-------------+
    _oImpostos["desc_zona_franca"]     := ConvertVlr(_aItensPV[_nX][27],2)
                
    
    aAdd(_oJSon["itens"],_oItImp)

Next _nX 

//-----------------+
// Totais impostos |
//-----------------+
//_oJSon["valor_frete"]           := _aTotais[1][1]
//_oJSon["total_mercadorias"]     := _aTotais[1][2]
//_oJSon["valor_total"]           := _aTotais[1][3]
//_oJSon["valor_icms"]            := _aTotais[1][4]
//_oJSon["valor_ipi"]             := _aTotais[1][5]
//_oJSon["valor_solidario"]       := _aTotais[1][6]
//_oJSon["desc_zona_franca"]      := _aTotais[1][7]

_cJson                          := _oJSon:toJSON()

Return _lRet 

/**********************************************************************************************************/
/*/{Protheus.doc} BpApi02C
    @description Calcula total venda para preço de lista 
    @type  Static Function
    @author Bernard M Margarido
    @since 10/10/2022
    @version version
/*/
/**********************************************************************************************************/
Static Function BpApi02C(_oItems,_nFrete,_nValMerc,_nFretePL) 
Local _nX       := 0 
Local _nVlrItem := 0 
Local _nTotItem := 0 

//_nValMerc       := 0
_nFretePL       := 0 

For _nX := 1 To Len(_oItems)
    _nPrcLista      := _oItems[_nX]["listPrice"]
    _nQtdven        := _oItems[_nX]["quantity"]
    _nVlrItem       += _nQtdven * _nPrcLista
    _nTotItem       += _nQtdven * _nPrcLista
Next _nX 

_nFretePL           := IIF(_nFrete > 0, _nTotItem * _nFrete / _nVlrItem,0)
_nValMerc           := IIF(_nValMerc > 0, _nValMerc, _nTotItem)

Return Nil 


/**********************************************************************************************************/
/*/{Protheus.doc} BpApi02D
    @description Realiza o calculo dos impostos do orcamento
    @type  Static Function
    @author Bernard M Margarido
    @since 14/03/2023
    @version version
/*/
/**********************************************************************************************************/
Static Function BpApi02D(_cCod,_cLoja,_cTipo,_cTpFrete,_cTpOper,_nTotal,_nValMerc,_nFretePL,_nFrete,_nDespesa,_oItems,_aItensPV,_aTotais)
Local _cTesInt      := ""
Local _cCodProd     := ""

Local _nIdItem      := 0
Local _nItValMerc   := 0 
Local _nPrcTab      := 0
Local _nPesoLiq     := 0
Local _nProductId   := 0
Local _nPrcUnit     := 0
Local _nPrcLista    := 0
Local _nQtdven      := 0
Local _nQtdPeso     := 0
Local _nAcresFin    := 0
Local _nDesconto    := 0
Local _nVlrFreIt    := 0
Local _nVlrDespIt   := 0
Local _nX           := 0
Local _nTotItIcm    := 0
Local _nTotItIPI    := 0
Local _nTotItPis    := 0
Local _nTotItCof    := 0
Local _nTotItST     := 0
Local _nTotItComp   := 0
Local _nTotItDif    := 0
Local _nTotItVFCD   := 0
Local _nTotItZFran  := 0
Local _nTotReceita  := 0
Local _nTITem       := TamSx3("C6_ITEM")[1]
Local _nTProd       := TamSx3("B1_COD")[1]

Local _lPerDesc     := .F.

//-------------------------+
// SB1 - Posiciona produto |
//-------------------------+
dbSelectArea("SB1")
SB1->( dbSetOrder(1) )


//---------------------------+
// Inicia as funcoes fiscais |
//---------------------------+
MaFisEnd()  
MaFisClear()
MaFisIni(   _cCod       ,;		    // 1-Codigo Cliente/Fornecedor
            _cLoja      ,;			// 2-Loja do Cliente/Fornecedor
            "C"         ,;			// 3-C:Cliente , F:Fornecedor
            "N"         ,;			// 4-Tipo da NF
            _cTipo      ,;			// 5-Tipo do Cliente/Fornecedor
            Nil         ,;			// 6-Relacao de Impostos que suportados no arquivo
            Nil         ,;			// 7-Tipo de complemento
            Nil         ,;			// 8-Permite Incluir Impostos no Rodape .T./.F.
            Nil         ,;			// 9-Alias do Cadastro de Produtos - ("SBI" P/ Front Loja)
            "MATA461"   ,;			// 10-Nome da rotina que esta utilizando a funcao
            Nil         ,;			// 11-Tipo de documento
            Nil         ,;			// 12-Especie do documento 
            Nil         ,;			// 13-Codigo e Loja do Prospect 
            Nil         ,;			// 14-Grupo Cliente
            Nil         ,;			// 15-Recolhe ISS
            Nil         ,;			// 16-Codigo do cliente de entrega na nota fiscal de saida
            Nil         ,;			// 17-Loja do cliente de entrega na nota fiscal de saida
            Nil         ,;			// 18-Informacoes do transportador [01]-UF,[02]-TPTRANS
            Nil         ,;			// 19-Se esta emitindo nota fiscal ou cupom fiscal (Sigaloja)
            Nil         ,;			// 20-Define se calcula IPI (SIGALOJA)
            Nil         ,;			// 21-Pedido de Venda
            _cCod       ,;			// 22-Cliente do faturamento ( cCodCliFor é passado como o cliente de entrega, pois é o considerado na maioria das funções fiscais, exceto ao gravar o clinte nas tabelas do livro)
            _cLoja      ,;			// 23-Loja do cliente do faturamento
            Nil         ,;			// 24-Total do Pedido
            Nil         ,;			// 25-Data de emissão do documento inicialmente só é diferente de dDataBase nas notas de entrada (MATA103 e MATA910)
            _cTpFrete)				// 26-Tipo de Frete informado no pedido

For _nX := 1 To Len(_oItems)

    //-------------------+
    // Posiciona produto |
    //-------------------+
    SB1->( dbSeek(xFilial("SB1") + PadR(_oItems[_nX]["productRefId"],_nTProd) ))
    
    
    _cCodProd       := SB1->B1_COD
    _cCodBar        := SB1->B1_EAN
    _nPesoLiq       := SB1->B1_PESO
    _nProductId     := _oItems[_nX]["productId"]

    _cTesInt        := MaTesInt(2,_cTpOper,_cCod,_cLoja,"C",_cCodProd) //U_FAutoTes(xFilial("SZH"),_cNatVnd,_cCodProd,_cCod,_cLoja)
    
    _nIdItem        := _oItems[_nX]["id"]
    _nPrcUnit       := ConvertVlr(_oItems[_nX]["sellingPrice"],1)
    _nPrcLista      := ConvertVlr(_oItems[_nX]["listPrice"],1)
    _nQtdven        := _oItems[_nX]["quantity"]
    
    _lPerDesc	    := IIF(Len(_oItems[_nX]["priceTags"]) > 0 ,_oItems[_nX]["priceTags"][1]["isPercentual"],.F.)
	_nDesconto	    := IIF(Len(_oItems[_nX]["priceTags"]) > 0 ,IIF(_oItems[_nX]["priceTags"][1]["value"] > 0, 0, _oItems[_nX]["priceTags"][1]["value"] ),0)

    _nDesconto      := ConvertVlr(IIF(_lPerDesc, 0, _nDesconto * -1),1)
    _nPerDesc       := ConvertVlr(IIF(_lPerDesc, _nDesconto * -1, 0),1)
    
    _nQtdPeso       := _nQtdven * _nPesoLiq

    If _lPerDesc
        _nPrcTab    := ( _nPrcUnit * 100 ) / ( 100 - _nPerDesc)
        _nDesconto  := a410Arred( (_nPrcTab - _nPrcUnit) * _nQtdven, "C6_VALDESC")
    EndIf 

    _nItValMerc     := ( _nQtdven * _nPrcUnit ) + _nDesconto
    _nAcresFin      := A410Arred(_nPrcUnit * SE4->E4_ACRSFIN / 100,"D2_TOTAL")
    _nItValMerc     += A410Arred(_nAcresFin * _nQtdven,"D2_TOTAL")
    _nPrcUnit       += _nAcresFin
            
    _nVlrFreIt      := NoRound((( _nQtdven  * _nPrcUnit * _nFrete ) / _nTotal ) ,2)
    _nVlrDespIt     := NoRound((( _nQtdven  * _nPrcUnit * _nDespesa ) / _nTotal ) ,2)

    //----------------+
    // Preco de lista |
    //----------------+
    If ( _nPrcLista == 0 )
        _nPrcLista  := A410Arred(_nValItMerc/_nQtdven,"D2_TOTAL")
    EndIf
            
    //---------------------+
    // SF4 - Posiciona TES |
    //---------------------+
    SF4->( dbSeek(xFilial("SF4") + _cTesInt) )

    //------------------------------------+
    // Adiciona itens nas funcoes fiscais |
    //------------------------------------+
    MaFisAdd(	_cCodProd       ,;          // 1-Codigo do Produto ( Obrigatorio )
                _cTesInt        ,;          // 2-Codigo do TES ( Opcional )
                _nQtdven        ,;          // 3-Quantidade ( Obrigatorio )
                _nPrcUnit       ,;          // 4-Preco Unitario ( Obrigatorio )
                _nDesconto      ,;          // 5-Valor do Desconto ( Opcional )
                Nil             ,;          // 6 -Numero da NF Original ( Devolucao/Benef )
                Nil             ,;          // 7 -Serie da NF Original ( Devolucao/Benef )
                Nil             ,;          // 8 -RecNo da NF Original no arq SD1/SD2
                0               ,;          // 9 -Valor do Frete do Item ( Opcional )
                0               ,;			// 10-Valor da Despesa do item ( Opcional )						
                0               ,;			// 11-Valor do Seguro do item ( Opcional )	   					
                0               ,;			// 12-Valor do Frete Autonomo ( Opcional )	   					
                _nItValMerc     ,;	        // 13-Valor da Mercadoria ( Obrigatorio )			            
                0               ,;			// 14-Valor da Embalagem ( Opcional )	   					
                SB1->( Recno() ),;          // 15-RecNo do SB1              
                SF4->( Recno() ),;          // 16-RecNo do SF4        
                StrZero(_nX,_nTITem))       // 17-Item
    
    //------+
    // ICMS |
    //------+
    _nTotItIcm  := MaFisRet(_nX,"IT_VALICM") 

    //-----+
    // IPI |
    //-----+
    _nTotItIPI  := MaFisRet(_nX,"IT_VALIPI")

    //-----+
    // PIS |
    //-----+
    _nTotItPis  := MaFisRet(_nX,"IT_VALPS2")   

    //--------+
    // COFINS |
    //--------+
    _nTotItCof  := MaFisRet(_nX,"IT_VALCF2")

    //---------+
    // ICMS ST |
    //---------+
    _nTotItST   := MaFisRet(_nX,"IT_VALSOL")

    //-------------------+
    // ICMS Complementar |
    //-------------------+
    _nTotItComp := MaFisRet(_nX, "IT_VALCMP")

    //---------------+
    // DIFAL Destino | 
    //---------------+
    _nTotItDif  := MaFisRet(_nX, "IT_DIFAL")

    //--------------------+
    // FECP DIFAL Destino |
    //--------------------+
    _nTotItVFCD := MaFisRet(_nX,'IT_VFCPDIF')

    //-------------+
    // ZONA FRANCA |
    //-------------+
    _nTotItZFran:= MaFisRet(_nX,"IT_DESCZF")
    
    //----------------+
    // TOTAL IMPOSTOS |
    //----------------+
    _nTotReceita:= _nTotItIcm + _nTotItPis + _nTotItCof + _nTotItComp + _nTotItDif + _nTotItVFCD + _nTotItIPI + _nTotItST

    //-----------------+
    // Array com itens |
    //-----------------+
    aAdd(_aItensPV, {               ;
        _nIdItem                    ,;  // 01. Item 
        _nProductId                 ,;  // 02. ID do produto
        _cCodProd                   ,;  // 03. Codigo do Produto 
        _cCodBar                    ,;  // 04. Codigo de barras
        _nQtdven                    ,;  // 05. Quantidade
        _nPrcUnit                   ,;  // 06. Valor Unitario
        _nItValMerc                 ,;  // 07. Valor Mercadoria
        MaFisRet(_nX,"IT_BASEICM")  ,;  // 08. Base ICMS
        MaFisRet(_nX,"IT_ALIQICM")  ,;  // 09. Aliquota ICMS
        _nTotItIcm                  ,;  // 10. Valor ICMS
        MaFisRet(_nX,"IT_BASEIPI")  ,;  // 11. Base IPI
        MaFisRet(_nX,"IT_ALIQIPI")  ,;  // 12. Aliquota IPI 
        _nTotItIPI                  ,;  // 13. Valor IPI 
        MaFisRet(_nX,"IT_BASEPS2")  ,;  // 14. Base PIS 
        MaFisRet(_nX,"IT_ALIQPS2")  ,;  // 15. Aliquota PIS 
        _nTotItPis                  ,;  // 16. Valor PIS
        MaFisRet(_nX,"IT_BASECF2")  ,;  // 17. Base Cofins
        MaFisRet(_nX,"IT_ALIQCF2")  ,;  // 18. Aliquota Cofins 
        _nTotItCof                  ,;  // 19. Valor Cofins 
        MaFisRet(_nX,"IT_BASESOL")  ,;  // 20. Base Solidario
        MaFisRet(_nX,"IT_ALIQSOL")  ,;  // 21. Aliquota Solidario
        _nTotItST                   ,;  // 22. Valor Solidario
        MaFisRet(_nX, "IT_ALIQCMP") ,;  // 23. Aliquota ICMS Complementar 
        _nTotItComp                 ,;  // 24. Valor ICMS Complementar 
        _nTotItDif                  ,;  // 25. Valor DIFAL 
        _nTotItVFCD                 ,;  // 26. Valor DIFAL Destino FECP
        _nTotItZFran                ,;  // 27. Valor Zona Franca 
        _nTotReceita                })  // 28. Total Impostos

Next _nX 

aAdd(_aTotais,{                     ;
    MaFisRet(,"NF_FRETE")           ,;  // 01. Valor do Frete
    MaFisRet(,"NF_VALMERC")         ,;  // 02. Total de Mercadorias 
    MaFisRet(,"NF_TOTAL")           ,;  // 03. Valor Total 
    MaFisRet(,"NF_VALICM")          ,;  // 04. Total de ICMS
    MaFisRet(,"NF_VALIPI")          ,;  // 05. Total de IPI 
    MaFisRet(,"NF_VALSOL")          ,;  // 06. Total Solidario
    MaFisRet(,"NF_DESCZF")          })  // 07. Desconto Zona Franca


Return Nil 

/************************************************************************************************************/
/*/{Protheus.doc} ConvertVlr
    @description Converte valores B2B
    @type  Static Function
    @author Bernard M Margarido
    @since 04/07/2023
    @version version
/*/
/************************************************************************************************************/
Static Function ConvertVlr(nVlrUnit,_nType)
Local _xRet := Nil 

If _nType == 1
   _xRet := ( nVlrUnit / 100 )
ElseIf _nType == 2
     _xRet := ( nVlrUnit * 100 )
EndIf 
Return _xRet
