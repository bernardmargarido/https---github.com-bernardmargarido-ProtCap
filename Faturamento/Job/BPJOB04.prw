#INCLUDE "PROTHEUS.CH"

/*****************************************************************************************/
/*/{Protheus.doc} BPJOB04
    @description Realiza a baixa de fornecedores na 4MDG 
    @type  Function
    @author Bernard M Margarido
    @since 11/09/2024
    @version version
/*/
/*****************************************************************************************/
User Function BPJOB04(_cEmpInt,_cFilInt)
Local _aArea        := GetArea()

Private _lJob       := IIF(!Empty(_cEmpInt) .And. !Empty(_cFilInt), .T., .F.)

Default _cEmpInt   := "01"
Default _cFilInt   := "00"

If _lJob
    RpcSetType(3)
	RpcSetEnv(_cEmpInt, _cFilInt,,,'FAT')
EndIf

//-------------------------+
// Baixa fornecedores 4MDG |
//-------------------------+
U_BPCOMM01()

If _lJob
    RpcClearEnv()
EndIf    

RestArea(_aArea)
Return .T.
