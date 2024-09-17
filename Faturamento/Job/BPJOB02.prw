#INCLUDE "PROTHEUS.CH"

/*****************************************************************************************/
/*/{Protheus.doc} BPJOB02
    @description Realiza a baixa de clientes na 4MDG 
    @type  Function
    @author Bernard M Margarido
    @since 11/09/2024
    @version version
/*/
/*****************************************************************************************/
User Function BPJOB02(_cEmpInt,_cFilInt)
Local _aArea        := GetArea()

Private _lJob       := IIF(!Empty(_cEmpInt) .And. !Empty(_cFilInt), .T., .F.)

Default _cEmpInt   := "01"
Default _cFilInt   := "00"

If _lJob
    RpcSetType(3)
	RpcSetEnv(_cEmpInt, _cFilInt,,,'FAT')
EndIf

//---------------------+
// Baixa clientes 4MDG |
//---------------------+
U_BPFATM03()

If _lJob
    RpcClearEnv()
EndIf    

RestArea(_aArea)
Return .T.
