;2018/04/06
PRO KAPPA_FIT2D__GAUSSFIT_MONTECARLO_PARAM_OUTPUT, $
   curParam, $
   origParamEst, $
   binSize, $
   layoutNommer, $
   REDUCE_STD_DEV_FACTOR=redStdDevFac, $
   XTITLE=xTitle, $
   XMAX=xMax, $
   WINDOW=window, $
   WINTITLE=winTitle, $
   OUT_ESTIMATE=gEst

  COMPILE_OPT IDL2,STRICTARRSUBS


;Arise from histo binning
  histVals = HISTOGRAM(curParam,BINSIZE=binSize,LOCATIONS=locs)

  IF N_ELEMENTS(locs) LE 5 THEN BEGIN
     
     tmpBinSize = binSize
     WHILE N_ELEMENTS(locs) LE 5 DO BEGIN
        tmpBinSize /= 2.
        histVals = HISTOGRAM(curParam,BINSIZE=tmpBinSize,LOCATIONS=locs)

     ENDWHILE
     
     binSize = tmpBinSize

  ENDIF

  IF N_ELEMENTS(locs) GE 1000 THEN BEGIN
     
     tmpBinSize = binSize
     WHILE N_ELEMENTS(locs) GT 1000 DO BEGIN
        tmpBinSize *= 1.5
        histVals = HISTOGRAM(curParam,BINSIZE=tmpBinSize,LOCATIONS=locs)

     ENDWHILE
     
     binSize = tmpBinSize

  ENDIF

;Normalize hist vals
  nHistVals = histVals/INT_TABULATED(locs,FLOAT(histVals))

  gMaxVal     = MAX(nHistVals,maxInd)
  gMaxValEst  = locs[maxInd]
  gMeanEst    = INT_TABULATED(locs,nHistVals*locs)/INT_TABULATED(locs,nHistVals)
  gStdDevEst1 = SQRT(INT_TABULATED(locs,nHistVals*(locs-gMeanEst)^2.)/INT_TABULATED(locs,nHistVals))
  gStdDevEst2 = SQRT(INT_TABULATED(locs,nHistVals*(locs^2.))/INT_TABULATED(locs,nHistVals)-gMeanEst^2.)
  IF ABS(gStdDevEst2-gStdDevEst1)/gStdDevEst1 GT 0.1 THEN STOP

  IF KEYWORD_SET(redStdDevFac) THEN gStdDevEst1 /= redStdDevFac
  gEstimates = [gMaxValEst,gMeanEst,gStdDevEst1]
  nTerms = 3
  gFit = GAUSSFIT(locs,nHistVals,gCoeffs,ESTIMATES=gEstimates,NTERMS=3)

  yRange = [0,MAX(nHistVals)*1.3]

  CASE layoutNommer OF
     1: BEGIN
        xTitle = "cm!U-3!N"
        yTitle = !NULL
     END
     2: BEGIN
        xTitle = "Potential (V)"
        yTitle = 'Probability density'
     END
     3: BEGIN
        xTitle = "Temperature (eV)"
        yTitle = !NULL
     END
     4: BEGIN
        xTitle = "Kappa"
        yTitle = 'Probability density'
     END
  ENDCASE

  nRow = 2
  nCol = 2

  IF layoutNommer EQ 2 THEN window = WINDOW(DIMENSIONS=[800,800],TITLE=winTitle)

  xRange = [MIN(locs),(KEYWORD_SET(xMax) ? (xMax < MAX(locs)) : MAX(locs))]

  p1 = PLOT(locs, nHistVals, $
            LAYOUT=[nCol,nRow,layoutNommer], $
            CURRENT=window, $
;;	DIMENSIONS=[800,200], $
            MARGIN=[0.1,0.1,0.1,0.2], $
            XRANGE=xRange, $
            YRANGE=yRange, $
            XTITLE=xTitle, $
            YTITLE=yTitle)

  p2 = PLOT(locs, gFit, THICK=2, /OVERPLOT,COLOR='RED',CURRENT=window)
  p3 = PLOT(REPLICATE(gMeanEst,20),LINDGEN(20)/19.*yRange[1],/OVERPLOT,LINESTYLE='--',COLOR='RED',CURRENT=window)
  p4 = PLOT(REPLICATE(origParamEst,20),LINDGEN(20)/19.*yRange[1],/OVERPLOT,LINESTYLE='--',COLOR='BLUE',CURRENT=window)

  gEst = {coeff: gCoeffs, $$
          locs: locs, $
          binSize : binSize, $
          nHistVals : nHistVals, $
          histVals : histVals, $
          init_coeff_est : gEstimates}

END
