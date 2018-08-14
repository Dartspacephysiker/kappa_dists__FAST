;2018/08/10
FUNCTION GET_SPREAD_FOR_MC_PARMS,mostProbVal,parmArr,parmStep,parmMinVal,binSize,frac, $
                                 N=N

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; initialize
  boundStep    = [-parmStep,parmStep]
  tmpBound     = mostProbVal + boundStep

  captured     = N_ELEMENTS( WHERE((parmArr GE tmpBound[0]) AND (parmArr LE tmpBound[1]),/NULL) )/FLOAT(N)

  ;; Loop until we've got it
  nTry = 0
  WHILE captured LT frac DO BEGIN

     tmpBound  = (tmpBound + boundStep) > parmMinVal
     captured  = N_ELEMENTS( WHERE((parmArr GE tmpBound[0]) AND (parmArr LE tmpBound[1]),/NULL) )/FLOAT(N)

     nTry++

     ;; IF ~(nTry MOD 10) THEN PRINT,FORMAT='("try ",I04,": ",F0.3,"/",F0.2)', $
     ;;                             nTry, $
     ;;                             captured, $
     ;;                             frac

     IF nTry EQ  1000 THEN PRINT,"Hit 1000 ..."
     IF nTry EQ  2000 THEN PRINT,"Hit 2000 ..."
     IF nTry EQ 30000 THEN BEGIN
        PRINT,"Hit 30000! STOP"
        PRINT,"Parmbounds: ",tmpBound[0],", ",tmpBound[1]
        STOP
     ENDIF

  ENDWHILE

  tmpBound -= mostProbVal

  RETURN,tmpBound

END  
