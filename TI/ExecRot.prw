#Include 'Protheus.ch'

#DEFINE TEXTOHTML	'<p><span style="color: #003366;"><strong>Atencao</strong></span>:</p>' + ;
					'<p>A execucao de rotinas em MVC exige a delaracao:<br />Private aRotina := FwLoadMenuDef("NomeFonte")</p>' + ;
					'<p><strong>Executar</strong>:</p>'
					
//-------------------------------------------------------------------
/*/{Protheus.doc} ExecRot                                                    
Rotina para execução de rotinas (Customizadas).
@author	Lucas.Brustolin
@since 	13/11/2018
@version P12.1.17
/*/
//-------------------------------------------------------------------
User Function ExecRot()

Local oExec		:= Nil
Local oDlg		:= Nil
Local bError	:= ErrorBlock( {|e| Help(" ",1,"ERR_FORM",,e:Description,3,1)  } )  
Local cExec		:= SPACE(200)          	
Local oFont 	:= TFont():New("Courier New",6,0)
Local xResult	:= Nil
Local lRet		:= .T.
Local lHtml 	:= .T.
Local oLayer 	:= FWLayer():new()

Private  lUserFunc	:= .T.

Begin Sequence
DEFINE MSDIALOG oDlg FROM 0,0 TO 190,460 PIXEL Style DS_MODALFRAME TITLE "Executa Rotina Especifica" FONT oFont COLOR CLR_BLACK, CLR_LIGHTGRAY PIXEL

	// ----------------------------------+
	// EXEMPLO DE LAYER COM BOX (WINDOW) |
	//-----------------------------------+
	oLayer:init(oDlg)
	oLayer:AddLine('LINE1',100)
	oLayer:AddCollumn('01',100,.T.,'LINE1')
	oLayer:AddWindow('01','SUPERIOR','Informe a rotina: ',100,.F.,.F.,,'LINE1',)
	oPanel 	:= oLayer:GetWinPanel( '01','SUPERIOR', 'LINE1' )   	


	//-- TITULO DA JANELA HTML 
	 oSay := TSay():New(05,10,{||TEXTOHTML},oPanel,,/*oFont*/,,,,.T.,,,150,30,,,,,,lHtml)

	//-- CheckBox
	oCheck1 := TCheckBox():New(05,165,'User Function (?)',{|u|if(PCount()>0,lUserFunc:= u,lUserFunc)},oPanel,100,210,,,,,,,,.T.,,,)

    //-- Campo
    @ 040,010 MSGET oExec VAR cExec PICTURE "@!" SIZE 150,13 PIXEL OF oPanel

	//-- Botao
    DEFINE SBUTTON FROM 042,165 TYPE 1 OF oPanel ENABLE ONSTOP "OK" ACTION (oDlg:End())

ACTIVATE MSDIALOG oDlg CENTER

cExec := AllTrim(cExec)

//  ------------------------------------+
//  INSERE O PREFIXO USER FUNCTION 'U_' |
// -------------------------------------+
If lUserFunc 
	If SubStr(cExec,1,2) == "U_" 
		If MsgYesNo("Deseja aplicar o prefixo 'U_' ?")
			cExec := "U_" + cExec
		EndIf
	Else
		cExec := "U_" + cExec
	EndIf
EndIf

//  ------------------------------------+
//  INSERE O PREFIXO NA FUNCAO '()'     |
// -------------------------------------+
If !( "(" $ cExec )
	cExec += "()"
EndIf

xResult := &( cExec  )

RECOVER
	lRet 	:= .F.
    cExec 	:= Space(100) 	
End Sequence

ErrorBlock(bError)

Return(lRet)


