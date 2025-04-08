#INCLUDE "TOTVS.CH"

/*************************************************************************************/
/*/{Protheus.doc} F460CANC
    @description Ponto de Entrada - Utilizado no cancelamento da Liquida��o
    @type user function
    @author Bernard M Margarido
    @since 07/04/2025
    @version version
/*/
/*************************************************************************************/
User Function F460CANC()
Local _nOpct    := ParamIxb[1]
Local _cFilial  := SE1->E1_FILIAL
Local _cTitulo  := SE1->E1_NUM
Local _cPrefixo := SE1->E1_PREFIXO
Local _cParcela := SE1->E1_PARCELA
Local _cNumLiq  := SE1->E1_NUMLIQ

If _nOpct == 1

    //----------------------+
    // Deleta transferencia |
    //----------------------+
    FwMsgRun(,{|| U_BPFIN15D(_cFilial,_cTitulo,_cPrefixo)}, "Aguarde...", "Validando transferencia concilia��o.")

    //---------------------------+
    // Estorna baixa comcilia��o |
    //---------------------------+
    FwMsgRun(,{|| U_BPFIN11A(_cFilial,_cNumLiq)}, "Aguarde...", "Validando concilia��o.")

EndIf 

Return _nOpct 
