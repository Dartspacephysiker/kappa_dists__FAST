;2018/03/22
FUNCTION KAPPA__MAKE_ENERGY_ARR_FOR_SPECIFIED_TBOUNDS,times,struct,whoIAm

  COMPILE_OPT IDL2,STRICTARRSUBS

  nTElem = N_ELEMENTS(struct.forWhom)
  nTimes = N_ELEMENTS(times)
  IF ~(N_ELEMENTS(struct.tBounds[0,*]) EQ N_ELEMENTS(struct.energy) AND nTElem EQ N_ELEMENTS(struct.energy)) THEN BEGIN
     PRINT,"Malformed structure!"
     STOP
  ENDIF

  forMeInds = WHERE(struct.forWhom EQ whoIAm,nForMe)

  IF nForMe EQ 0 THEN BEGIN
     PRINT,"How arrived here?"
     STOP
  ENDIF
  
  energy_arr = MAKE_ARRAY(nTimes,/FLOAT,VALUE=struct.energy[forMeInds[0]])
  FOR k=0,nForMe-1 DO BEGIN
     strInd = forMeInds[k]

     tmpTRange = S2T(struct.tBounds[*,strInd])

     theseInds = WHERE(times GE tmpTRange[0] AND times LE tmpTRange[1],nMany)
     IF nMany GT 0 THEN BEGIN
        energy_arr[theseInds] = struct.energy[strInd]
     ENDIF

  ENDFOR

  RETURN,energy_arr

END
