;2018/08/10
FUNCTION MOST_PROB_FOR_MC_PARMS,parmArr,parmStep,binSize

  COMPILE_OPT IDL2,STRICTARRSUBS

  mostProbVal  = MEDIAN(parmArr)

  histVals     = HISTOGRAM(parmArr,BINSIZE=binSize,LOCATIONS=bins)

  IF N_ELEMENTS(bins) LE 5 THEN BEGIN
     
     maxParm = MAX(parmArr)
     minParm = MIN(parmArr)
     medParm = MEDIAN(parmArr)

     tmpBinSize = binSize

     WHILE N_ELEMENTS(bins) LE 15 DO BEGIN

        IF tmpBinSize GE (maxParm-minParm) THEN BEGIN
           PRINT,"Very narrow range of params!"
           BREAK
        ENDIF
        
        tmpBinSize /= 2.
        histVals = HISTOGRAM(parmArr,BINSIZE=tmpBinSize,LOCATIONS=bins)

     ENDWHILE
     
     binSize = tmpBinSize

  ENDIF

  IF N_ELEMENTS(bins) GE 501 THEN BEGIN
     
     tmpBinSize = binSize
     WHILE N_ELEMENTS(bins) GT 1000 DO BEGIN
        tmpBinSize *= 1.5
        histVals = HISTOGRAM(parmArr,BINSIZE=tmpBinSize,LOCATIONS=bins)

     ENDWHILE
     
     binSize = tmpBinSize

  ENDIF

  junk         = MAX(histVals,mostProbInd)
  mostProbVal  = bins[mostProbInd]+binSize/2.

  RETURN,mostProbVal
  
END
