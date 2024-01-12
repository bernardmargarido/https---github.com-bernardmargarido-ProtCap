#INCLUDE "PROTHEUS.CH"

/******************************************************************************/
/*/{Protheus.doc} BZJOB001
    @descrption JOB - Integração Astrein 
    @type  Function
    @author Bernard M. Margarido
    @since 05/11/2020
    @version version
/*/
/******************************************************************************/
User Function BZJOB001(_cEmpInt,_cFilInt) 
Local _aArea        := GetArea()

Private _lJob       := IIF(!Empty(_cEmpInt) .And. !Empty(_cFilInt), .T., .F.)

Default _cEmpInt   := "01"
Default _cFilInt   := "00"

//------------------+
// Mensagem console |
//------------------+
CoNout("<< BZJOB001 >> - INICIO " + dTos( Date() ) + " - " + Time() )

//-----------------------+
// Abre empresa / filial | 
//-----------------------+
If _lJob
    RpcSetType(3)
	RpcSetEnv(_cEmpInt, _cFilInt,,,'FAT')
EndIf

//-----------------------------+
// Integração de dados Astrein |
//-----------------------------+
CoNout("<< BZJOB001 >> - INICIO INTEGRACAO ASTREIN " + dTos( Date() ) + " - " + Time() )
    U_BZAPI001()
CoNout("<< BZJOB001 >> - FIM INTEGRACAO ASTREIN " + dTos( Date() ) + " - " + Time() )

//------------------------+
// Fecha empresa / filial |
//------------------------+
If _lJob
    RpcClearEnv()
EndIf    

CoNout("<< BZJOB001 >> - FIM " + dTos( Date() ) + " - " + Time() )

RestArea(_aArea)
Return .T.