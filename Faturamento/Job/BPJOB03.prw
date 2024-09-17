#INCLUDE "PROTHEUS.CH"

/*****************************************************************************************/
/*/{Protheus.doc} BPJOB03
    @description Realiza a baixa de transportadoras na 4MDG 
    @type  Function
    @author Bernard M Margarido
    @since 11/09/2024
    @version version
/*/
/*****************************************************************************************/
User Function BPJOB03(_cEmpInt,_cFilInt)
Local _aArea        := GetArea()

Private _lJob       := IIF(!Empty(_cEmpInt) .And. !Empty(_cFilInt), .T., .F.)

Default _cEmpInt   := "01"
Default _cFilInt   := "00"

If _lJob
    RpcSetType(3)
	RpcSetEnv(_cEmpInt, _cFilInt,,,'FAT')
EndIf

//----------------------------+
// Baixa transportadoras 4MDG |
//----------------------------+
U_BPFATM05()

If _lJob
    RpcClearEnv()
EndIf    

RestArea(_aArea)
Return .T.
