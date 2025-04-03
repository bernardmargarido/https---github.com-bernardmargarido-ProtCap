#INCLUDE "TOTVS.CH"
#INCLUDE "PROTHEUS.CH"
#INCLUDE "FWMVCDEF.CH"
#INCLUDE "AARRAY.CH"
#INCLUDE "JSON.CH"

#DEFINE CLR_CINZA RGB(230,230,230)
#DEFINE CLR_RED RGB(200,047,053)
#DEFINE CRLF CHR(13) + CHR(10)

/********************************************************************************************************************/
/*/{Protheus.doc} BPFINA14
    @description Consulta saldo disponivel eCommerce
    @type  Function
    @author Bernard M. Margarido
    @since 19/07/2021
/*/
/********************************************************************************************************************/
User Function BPFINA14()
Local _aArea    := GetArea()

Local _nReceber     := 0
Local _nDisponivel  := 0
Local _nTransferido := 0

Private _nOldLen    := SetVarNameLen(255) 

//----------------+
// Consulta saldo |
//----------------+
FWMsgRun(, {|| BPFINA14A(@_nReceber,@_nDisponivel,@_nTransferido) },"Aguarde...","Consultando saldo.")

If _nReceber > 0 .Or. _nDisponivel > 0 .Or. _nTransferido > 0 
    FwMsgRun(,{|| BPFINA14B(_nReceber,_nDisponivel,_nTransferido)},"Aguarde...","Criando tela.")
EndIf

SetVarNameLen(_nOldLen)

RestArea(_aArea)
Return Nil 

/********************************************************************************************************************/
/*/{Protheus.doc} BPFINA14A
    @description Consulta saldo atual pagamentos e-Commerce
    @type  Static Function
    @author Bernard M. Margarido
    @since 19/07/2021
/*/
/********************************************************************************************************************/
Static Function BPFINA14A(_nReceber,_nDisponivel,_nTransferido)
Local _cMsg     := ""

Local _oPagarMe := PagarMe():New()
Local _oJSon    := Nil 

//------------------------------+
// Available - disponivel       |
// Transferred - transferidos   |
// Waiting_funds - a receber    |
//------------------------------+

If _oPagarMe:Saldo()
    _oJSon      := JSonObject():New()
    _oJSon:FromJson(_oPagarMe:cRetJSon)
    If ValType(_oJSon) <> "U"
        _nReceber       := _oJSon["waiting_funds"]["amount"] / 100 
        _nDisponivel    := _oJSon["available"]["amount"] / 100
        _nTransferido   := _oJSon["transferred"]["amount"] / 100 
    Else 
        _cMsg := "Não foi possivel realizar a consulta de saldo. Problemas de comunicação, tente novamente mais tarde."    
    EndIf    
Else 
    _cMsg := _oPagarMe:cError
EndIf

If !Empty(_cMsg)
    MsgStop("Erro ao realizar consulta de saldo " + _cMsg,"Dana - Avisos")
EndIf

FreeObj(_oJSon)

Return Nil 

/********************************************************************************************************************/
/*/{Protheus.doc} BPFINA14B
    @description Tela contendo os saldo 
    @type  Static Function
    @author Bernard M. Margarido
    @since 19/07/2021
/*/
/********************************************************************************************************************/
Static Function BPFINA14B(_nReceber,_nDisponivel,_nTransferido)
Local _cTmpHtml := RTrim(GetTempPath())
Local _cTitulo  := "Consulta Saldos"
Local _cFileHtml:= ""

Local _oDlg     := Nil 
Local _oPanel01 := Nil
Local _oPanel02 := Nil 

//--------------------------------------------------------+
// 0 - Nenhum alinhamento (default) - CONTROL_ALIGN_NONE  |
// 1 - À esquerda - CONTROL_ALIGN_LEFT                    |
// 2 - À direita - CONTROL_ALIGN_RIGHT                    |
// 3 - No topo - CONTROL_ALIGN_TOP                        |
// 4 - No rodapé - CONTROL_ALIGN_BOTTOM                   |
// 5 - Em todo o parent - CONTROL_ALIGN_ALLCLIENT         |
//--------------------------------------------------------+

//-----------+
// Cria HTML |
//-----------+
_cFileHtml := _cTmpHtml + CriaTrab(Nil, .F.) + ".htm"
BPFINA14C(@_cFileHtml,_nReceber,_nDisponivel,_nTransferido)

_oDlg := MsDialog():New(100, 300, 655, 1220, _cTitulo,,,,DS_MODALFRAME,,,,,.T.)

    //-------------------------+
	// Painel para informações | 
	//-------------------------+
	_oPanel01 := TPanel():New(000,000,"",_oDlg, ,.T.,.T., ,CLR_CINZA,000,220,.T.,.T.)
    _oPanel01:Align := 3
    
    _oTiBrowse := TWebEngine():New(_oPanel01, 000, 000, 000, 000)
    _oTiBrowse:Align := CONTROL_ALIGN_ALLCLIENT
    _oTiBrowse:Navigate(_cFileHtml)
    _oPanel01:Refresh()
        
    _oPanel02 := TPanel():New(000,000,"",_oDlg, ,.T.,.F., ,/*CLR_RED*/,000,030,.T.,.F.)
	_oPanel02:Align := 4    

    _oBtnSair := TButton():New( 010, 000, "Sair", _oPanel02,{|| _oDlg:End() }	, 050,010,,,.F.,.T.,.F.,,.F.,,,.F. )
    _oBtnSair:Align := 2

    _oDlg:lEscClose := .T.    
    _oDlg:lCentered := .T.

_oDlg:Activate(,,,.T.,,,)
Return Nil 

/******************************************************************************************/
/*/{Protheus.doc} BPFINA14C
    @description Cria HTML de exibição
    @type  Static Function
    @author Bernard M. Margarido
    @since 20/07/2021
/*/
/******************************************************************************************/
Static Function BPFINA14C(_cFileHtml,_nReceber,_nDisponivel,_nTransferido)
Local _nHdl     := 0
Local _cTxtHtm  := ""

_cTxtHtm := '<table style="border:1px solid black;border-spacing:1px;text-align:center;width:100%">'
_cTxtHtm += '	<tr style="background-color:#ffffff">'
_cTxtHtm += '		<th colspan="3"><img src="https://bunzlepi.com.br/wp-content/uploads/2024/03/Logo-Bunzl-EPI-Fundo-Branco.png" style="width:200px;height:100px;" > </th>'
_cTxtHtm += '	</tr>'
_cTxtHtm += '	<tr style="color:#fff;font-family: verdana;font-size: 100%;background-color:#62B22D;">'
_cTxtHtm += '		<td> Á Receber </td>'
_cTxtHtm += '		<td> Disponível </td>'
_cTxtHtm += '		<td> Transferido </td>'
_cTxtHtm += '	</tr>'
_cTxtHtm += '	<tr style="color:#000;font-family: verdana;font-size: 100%;background-color:#fff;">'
_cTxtHtm += '		<td>' + Alltrim(TransForm(_nReceber,PesqPict("SF2","F2_VALMERC")))  + '</td>'
_cTxtHtm += '		<td>' + Alltrim(TransForm(_nDisponivel,PesqPict("SF2","F2_VALMERC")))  + '</td>'
_cTxtHtm += '		<td>' + Alltrim(TransForm(_nTransferido,PesqPict("SF2","F2_VALMERC")))  + '</td>'
_cTxtHtm += '	</tr>'
_cTxtHtm += '</table>'

_nHdl  := fCreate(_cFileHtml)
FWrite(_nHdl, _cTxtHtm)
FClose(_nHdl)

Return Nil 

