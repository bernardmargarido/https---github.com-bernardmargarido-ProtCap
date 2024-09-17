#INCLUDE "PROTHEUS.CH"
#INCLUDE "TOPCONN.CH"
#INCLUDE "TBICONN.CH"

// ##############################################################################
// Projeto  : BUNZL
// Autor    : Wilson A. Silva Jr
// Modulo   : Geral
// Função   : BZExeJob
// Descrição: JOB para baixa dos títulos a pagar do RH Corporativo.
// Retorno  : Nenhum.
// ---------+-------------------+------------------------------------------------
// Data     | Autor             | Descricao
// ---------+-------------------+------------------------------------------------
// 16/10/19 | Wilson A. Silva Jr| Desenvolvimento da rotina.
// ---------+-------------------+------------------------------------------------
User Function BZExeJob(cRotina, cWait, cEmpTrab, cFiliais, cIntervalo, cDia)

Local cFilTrab	  := ""
Local lWait		  := .T.
Local nIntervalo  := 0
Local nStep		  := 0 
Local nCount 	  := 0
Local nX

DEFAULT cRotina   := ""
DEFAULT cWait	  := "T" // Aguarda a execução de cada filial
DEFAULT cEmpTrab  := ""
DEFAULT cFiliais  := ""
DEFAULT cIntervalo:= "60000" // 60000 milisegundos = 1 minuto
DEFAULT cDia	  := "2,3,4,5,6,7" // 1==Domingo

lWait 	   := UPPER(LEFT(cWait,1)) == "T"
nIntervalo := Val(cIntervalo)

aFiliais := Separa(cFiliais,",",.F.)

For nX := 1 To Len(aFiliais)

	cFilTrab := AllTrim(aFiliais[nX])
	
	// Executa rotina
	StartJob( cRotina, GetEnvServer(), lWait, cEmpTrab, cFilTrab, "1000", cDia )

Next nX

nStep  := 1
nCount := nIntervalo/1000
While !KillApp() .AND. nStep <= nCount
	Sleep(1000) //Sleep de 1 segundo
	nStep++
EndDo

Return .T.
