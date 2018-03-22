;2018/03/22
;; 'Posed to be called within KAPPA_FIT2D__LOOP
FUNCTION KAPPA__UPDATE_ENERGY_ELECTRONS,time, $
                                        tBounds, $
                                        energy_electron_arr, $
                                        curInd, $
                                        nTBounds, $
                                        UPDATED_INDEX=updateInd

  COMPILE_OPT IDL2,STRICTARRSUBS

  updateInd = curInd 
  IF time GT tBounds[1,updateInd] THEN BEGIN

     OK = 0
     updateInd += 1
     WHILE ~OK DO BEGIN
        OK = time LT tBounds[1,updateInd]
        IF OK THEN BEGIN
           BREAK
        ENDIF
        updateInd += 1

        IF updateInd EQ nTBounds THEN BEGIN
           PRINT,"FEIL!"
           STOP
        ENDIF
     ENDWHILE

  ENDIF

  RETURN,energy_electron_arr[*,updateInd]
  
END
