;2017/03/17
PRO PLOT_TEMPERATURE_AND_DENSITY_TSERIES, $
   jvPlotData, $
   ;; USE_SOURCE_AVGS=use_source_avgs, $
   ORIGINAL_PLOTIDEE=orig_plotIdee, $
   YLOG_NDOWN=yLog_nDown, $
   USEI__TWOLUMPS=useInds__twoLumps, $
   USEINDS=useInds, $
   SAVEPLOT=savePlot, $
   SPNAME=spName, $
   ORIGINATING_ROUTINE=routName, $
   PLOTDIR=plotDir, $
   OUT_WINDOW=window1, $
   OVERPLOTALL=overplotAll, $
   OVERPLOT_WINDOW=overplot_window, $
   _EXTRA=e

  COMPILE_OPT IDL2,STRICTARRSUBS


  ;; errorBarFac     = KEYWORD_SET(errorBarFac) ? errorBarFac : 1.
  nPoints          = N_ELEMENTS(jvPlotData.time)

  rgbTable         = 4
  nColors          = 256
  transpose        = 1
  hammerCT         = COLORTABLE(rgbTable,STRETCH=stretch,NCOLORS=nColors,TRANSPOSE=transpose)

  tDownTitle         = 'T, downgoing e!U-!N (eV)'

  ;; nDownRange       = MINMAX(jvPlotData.nDown)
  nDownTitle       = 'N (cm!U-3!N)'

  errSym           = '.'
  errSym_size      = 3.0
  errSym_fill      = 0
  errSym_capSize   = 0.05

  ;;These all pertain to grid creation
  xGridStyle       = ':'
  yGridStyle       = ':'
  xTickLen         = 1
  yTickLen         = 1
  xSubTickLen      = 0.01
  ySubTickLen      = 0.01
  
  winDim           = [900,600]

  p1pos            = [0.10,0.08,0.95,0.50]
  p2pos            = [0.10,0.53,0.95,0.94]
  ;; p3pos            = [0.54,0.08,0.95,0.94]
  cbpos            = [0.10,0.97,0.95,0.99]

  orig_p1Pos       = [0.1,0.1,0.95,0.8]
  orig_cbPos       = [0.1,0.96,0.95,0.98]

  IF ~KEYWORD_SET(plotDir) THEN BEGIN
     plotDir = './'
  ENDIF

  timeTitle        = 'Seconds since ' + TIME_TO_STR(jvPlotData.time[0],/MS)

  CURANDPOT__SELECT_T_AND_N,jvPlotData,avgs_JVfit, $
                            TEMPERATURE=Temperature, $
                            DENSITY=Density, $
                            ERR_TEMPERATURE=TemperatureErr, $
                            ERR_DENSITY=DensityErr, $
                            /ARRAYS

  IF KEYWORD_SET(jvPlotData.use_source_avgs) THEN BEGIN
     ;; temperature    = jvPlotData.source.TDown
     ;; temperatureErr = jvPlotData.source.TDownErr
     ;; density        = jvPlotData.source.NDown
     ;; densityErr     = jvPlotData.source.NDownErr

     nDownRange     = [1e-2,1e0]
  ENDIF ELSE BEGIN
     ;; temperature    = jvPlotData.TDown
     ;; temperatureErr = jvPlotData.TDownErr
     ;; density        = jvPlotData.NDown
     ;; densityErr     = jvPlotData.NDownErr

     nDownRange     = [5e-3,1e0]
  ENDELSE

  tDownRange       = MINMAX(temperature)

  IF KEYWORD_SET(orig_plotIdee) THEN BEGIN
     window        = WINDOW(DIMENSIONS=winDim, $
                        BUFFER=savePlot)

     sPlot         = SCATTERPLOT(temperature, $
                             density, $
                             XRANGE=tDownRange, $
                             YRANGE=nDownRange, $
                             /YLOG, $
                             RGB_TABLE=rgbTable, $
                             MAGNITUDE=jvPlotData.tMag, $
                             SYMBOL='*', $
                             SYM_SIZE=2.0, $
                             SYM_THICK=2.0, $
                             SYM_TRANSPARENCY=70, $
                             XTITLE=tDownTitle, $
                             YTITLE=nDownTitle, $
                             XGRIDSTYLE=xGridStyle, $
                             YGRIDSTYLE=yGridStyle, $
                             XTICKLEN=xTickLen, $
                             YTICKLEN=yTickLen, $
                             XSUBTICKLEN=xSubTickLen, $
                             YSUBTICKLEN=ySubTickLen, $
                             /CURRENT, $
                             POSITION=orig_p1Pos, $
                             BUFFER=savePlot)

     ;;And a colorbar thing
     nTMarks       = 5
     tInds         = (INDGEN(nTMarks)*nPoints)/(nTMarks-1)
     tickValues    = tMag[safe_i[tInds]]
     tickTimes     = time[safe_i[tInds]]
     tickName      = STRMID(TIME_TO_STR(tickTimes),11,15)
     tMagRange     = [tMag[safe_i[0]],tMag[safe_i[-1]]]
     cb            = COLORBAR(RGB_TABLE=rgbTable, $
                            TICKNAME=tickName, $
                            TICKVALUES=tickValues, $
                            POSITION=orig_cbPos, $
                            RANGE=tMagRange, $
                            ;; TEXT_ORIENTATION=180, $
                            /NORMAL)
     STOP

  ENDIF

  IF ~KEYWORD_SET(overplotAll) THEN BEGIN
     window1          = WINDOW(DIMENSIONS=winDim, $
                               BUFFER=savePlot)
  ENDIF
  
  CTInds           = BYTSCL(jvPlotData.tMag)

  tRange           = [0,jvPlotdata.tDiff[-1]]
  errTDownRange    = TDownRange

  CASE 1 OF
     KEYWORD_SET(sans_errorBars): BEGIN

        ;;The old, error bar–less way

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;First plot

        ;; plot_1            = ERRORPLOT((jvPlotData.tDiff[inds]), $
        ;;                               temperature[inds], $
        ;;                               tmpErr, $
        ;;                               XRANGE=tRange, $
        ;;                               ;; YRANGE=TDownRange, $
        ;;                               YRANGE=errTDownRange, $
        ;;                               XTITLE='Seconds since ' + TIME_TO_STR(curPotList[0].jvPlotData.time[inds]), $
        ;;                               YTITLE='j!D||!N($\mu$A m!U-2!N)', $
        ;;                               /CURRENT, $
        ;;                               POSITION=p1pos)

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;Second plot

        plot_2            = SCATTERPLOT(jvPlotData.tDiff, $
                                        density, $
                                        ;; XTITLE='Seconds since ' + TIME_TO_STR(curPotList[0].time[safe_i[0]]), $
                                        YTITLE=nDownTitle, $
                                        RGB_TABLE=hammerCT, $
                                        MAGNITUDE=jvPlotData.tMag, $
                                        ;; LINESTYLE='', $
                                        SYMBOL='.', $
                                        SYM_SIZE=3.0, $
                                        /SYM_FILLED, $
                                        /CURRENT, $
                                        POSITION=p2pos)

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;Third plot
        ;; plot_3      = SCATTERPLOT(jvPlotData.cur, $
        ;;                           density, $
        ;;                           XRANGE=TDownRange, $
        ;;                           YRANGE=nDownRange, $
        ;;                           /YLOG, $
        ;;                           RGB_TABLE=hammerCT, $
        ;;                           MAGNITUDE=jvPlotData.tMag, $
        ;;                           SYMBOL=jvSym, $
        ;;                           SYM_SIZE=jvSymSize, $
        ;;                           SYM_THICK=jvSymThick, $
        ;;                           SYM_TRANSPARENCY=jvSymTransp, $
        ;;                           SYM_FILLED=jvSymFilled, $
        ;;                           YTICKVALUES=jv_tickValues, $
        ;;                           YTICKNAME=jv_tickNames, $
        ;;                           XTITLE='j!D||!N($\mu$A m!U-2!N)', $
        ;;                           YTITLE='$\Phi$ (V)', $
        ;;                           XGRIDSTYLE=':', $
        ;;                           YGRIDSTYLE=':', $
        ;;                           XTICKLEN=1, $
        ;;                           YTICKLEN=1, $
        ;;                           XSUBTICKLEN=0.01, $
        ;;                           YSUBTICKLEN=0.01, $
        ;;                           /CURRENT, $
        ;;                           POSITION=p3pos)

     END
     ELSE: BEGIN

        ;;Initialize things
        inds              = [0,1]
        tmpTDownErr         = temperatureErr[inds]

        plot_1            = ERRORPLOT((jvPlotData.tDiff[inds]), $
                                      temperature[inds], $
                                      tmpTDownErr, $
                                      XRANGE=tRange, $
                                      ;; YRANGE=TDownRange, $
                                      YRANGE=errTDownRange, $
                                      XTITLE=timeTitle, $
                                      YTITLE=tDownTitle, $
                                      RGB_TABLE=hammerCT, $
                                      ;; ERRORBAR_COLOR=hammerCT[*,CTInds[0]], $
                                      VERT_COLORS=CTInds[inds], $
                                      LINESTYLE='', $
                                      ERRORBAR_CAPSIZE=errSym_capSize, $
                                      SYMBOL=errSym, $
                                      SYM_SIZE=errSym_size, $
                                      SYM_FILLED=errSym_fill, $
                                      XGRIDSTYLE=xGridStyle, $
                                      YGRIDSTYLE=yGridStyle, $
                                      XTICKLEN=xTickLen, $
                                      YTICKLEN=yTickLen, $
                                      XSUBTICKLEN=xSubTickLen, $
                                      YSUBTICKLEN=ySubTickLen, $
                                      /CURRENT, $
                                      OVERPLOT=KEYWORD_SET(overplotAll), $
                                      POSITION=p1pos, $
                                      BUFFER=savePlot)

        ;;Now add all the other symbols
        FOR k=2,nPoints-1,2 DO BEGIN

           inds           = [k,((k+1) < (nPoints-1))]
           ;; tmpTDownErr      = tDownErr[*,inds]
           tmpTDownErr      = temperatureErr[inds]

           plot_1         = ERRORPLOT((jvPlotData.tDiff[inds]), $
                                      temperature[inds], $
                                      tmpTDownErr, $
                                      ;; RGB_TABLE=hammerCT, $
                                      VERT_COLORS=hammerCT[*,CTInds[inds]], $
                                      ERRORBAR_COLOR=hammerCT[*,CTInds[k]], $
                                      ERRORBAR_CAPSIZE=errSym_capSize, $
                                      LINESTYLE='', $
                                      SYMBOL=errSym, $
                                      SYM_SIZE=errSym_size, $
                                      SYM_FILLED=errSym_fill, $
                                      /CURRENT, $
                                      ;; POSITION=p1pos, $
                                      /OVERPLOT)
        ENDFOR

        IF KEYWORD_SET(useInds__twoLumps) AND KEYWORD_SET(useInds) THEN BEGIN
           
           minI      = MIN(useInds)
           maxI      = MAX(useInds)
           line1X    = [jvPlotData.tDiff[minI],jvPlotData.tDiff[minI]]
           line2X    = [jvPlotData.tDiff[maxI],jvPlotData.tDiff[maxI]]

           line1Y    = [MIN(TDownRange),MAX(TDownRange)]
           line2Y    = [MIN(TDownRange),MAX(TDownRange)]

           linePlot1 = PLOT(line1X, $
                            line1Y, $
                            RGB_TABLE=hammerCT, $
                            VERT_COLORS=jvPlotData.tMag[[minI,minI]], $
                            /OVERPLOT)
           
           linePlot2 = PLOT(line2X, $
                            line2Y, $
                            RGB_TABLE=hammerCT, $
                            VERT_COLORS=jvPlotData.tMag[[maxI,maxI]], $
                            /OVERPLOT)

        ENDIF

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;Second plot

        inds              = [0,1]
        tmpNDownErr         = densityErr[inds]

        errNDownRange       = MINMAX(density)
        plot_2            = ERRORPLOT((jvPlotData.tDiff[inds]), $
                                      density[inds], $
                                      tmpNDownErr, $
                                      XRANGE=tRange, $
                                      YLOG=yLog_nDown, $
                                      YRANGE=nDownRange, $
                                      YTITLE=nDownTitle, $
                                      RGB_TABLE=hammerCT, $
                                      ERRORBAR_COLOR=hammerCT[*,CTInds[0]], $
                                      VERT_COLORS=CTInds[inds], $
                                      LINESTYLE='', $
                                      ERRORBAR_CAPSIZE=errSym_capSize, $
                                      SYMBOL=errSym, $
                                      SYM_SIZE=errSym_size, $
                                      SYM_FILLED=errSym_fill, $
                                      XGRIDSTYLE=xGridStyle, $
                                      YGRIDSTYLE=yGridStyle, $
                                      XTICKLEN=xTickLen, $
                                      YTICKLEN=yTickLen, $
                                      XSUBTICKLEN=xSubTickLen, $
                                      YSUBTICKLEN=ySubTickLen, $
                                      /CURRENT, $
                                      OVERPLOT=KEYWORD_SET(overplotAll), $
                                      POSITION=p2pos, $
                                      XSHOWTEXT=0B)
        ;; plot_2.xshowtext  = 0B

        ;;Now add all the other symbols
        FOR k=2,nPoints-1,2 DO BEGIN

           inds           = [k,((k+1) < (nPoints-1))]
           tmpNDownErr      = densityErr[inds]

           plot_2         = ERRORPLOT((jvPlotData.tDiff[inds]), $
                                      density[inds], $
                                      tmpNDownErr, $
                                      YLOG=yLog_nDown, $
                                      VERT_COLORS=hammerCT[*,CTInds[inds]], $
                                      ERRORBAR_COLOR=hammerCT[*,CTInds[k]], $
                                      ERRORBAR_CAPSIZE=errSym_capSize, $
                                      LINESTYLE='', $
                                      SYMBOL=errSym, $
                                      SYM_SIZE=errSym_size, $
                                      SYM_FILLED=errSym_fill, $
                                      /CURRENT, $
                                      /OVERPLOT)
        ENDFOR

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;Third plot

        ;; inds              = [0,1]
        ;; ;; tmpTDownErr         = tDownErr[*,inds]
        ;; ;; tmpNDownErr         = nDownErr[*,inds]
        ;; tmpTDownErr         = temperatureErr[inds]
        ;; tmpNDownErr         = densityErr[inds]

        ;; plot_3      = ERRORPLOT(temperature[inds], $
        ;;                         density[inds], $
        ;;                         tmpTDownErr, $
        ;;                         tmpNDownErr, $
        ;;                         XRANGE=TDownRange, $
        ;;                         YRANGE=nDownRange, $
        ;;                         /YLOG, $
        ;;                         LINESTYLE='', $
        ;;                         ERRORBAR_COLOR=hammerCT[*,CTInds[0]], $
        ;;                         ERRORBAR_CAPSIZE=errSym_capSize, $
        ;;                         SYMBOL=errSym, $
        ;;                         SYM_SIZE=errSym_size, $
        ;;                         SYM_FILLED=errSym_fill, $
        ;;                         VERT_COLORS=CTInds[inds], $
        ;;                         YTICKVALUES=jv_tickValues, $
        ;;                         YTICKNAME=jv_tickNames, $
        ;;                         XTITLE=jv_xTitle, $
        ;;                         YTITLE=jv_yTitle, $
        ;;                         XGRIDSTYLE=xGridStyle, $
        ;;                         YGRIDSTYLE=yGridStyle, $
        ;;                         XTICKLEN=xTickLen, $
        ;;                         YTICKLEN=yTickLen, $
        ;;                         XSUBTICKLEN=xSubTickLen, $
        ;;                         YSUBTICKLEN=ySubTickLen, $
        ;;                         /CURRENT, $
        ;;                         POSITION=p3pos)

        ;; ;;Now add all the other symbols
        ;; FOR k=2,nPoints-1,2 DO BEGIN

        ;;    inds           = [k,k+1]
        ;;    tmpTDownErr      = temperatureErr[inds]
        ;;    tmpNDownErr      = densityErr[inds]

        ;;    plot_3         = ERRORPLOT((temperature[inds]), $
        ;;                               density[inds], $
        ;;                               tmpTDownErr, $
        ;;                               tmpNDownErr, $
        ;;                               ;; RGB_TABLE=hammerCT, $
        ;;                               LINESTYLE='', $
        ;;                               VERT_COLORS=hammerCT[*,CTInds[inds]], $
        ;;                               ERRORBAR_COLOR=hammerCT[*,CTInds[k]], $
        ;;                               ERRORBAR_CAPSIZE=errSym_capSize, $
        ;;                               SYMBOL=errSym, $
        ;;                               SYM_SIZE=errSym_size, $
        ;;                               SYM_FILLED=errSym_fill, $
        ;;                               /CURRENT, $
        ;;                               /OVERPLOT)
        ;; ENDFOR

     END
  ENDCASE

  IF KEYWORD_SET(useInds__twoLumps) AND KEYWORD_SET(useInds) THEN BEGIN
     
     minI      = MIN(useInds)
     maxI      = MAX(useInds)
     line1X    = [jvPlotData.tDiff[minI],jvPlotData.tDiff[minI]]
     line2X    = [jvPlotData.tDiff[maxI],jvPlotData.tDiff[maxI]]

     line1Y    = [MIN(nDownRange),MAX(nDownRange)]
     line2Y    = [MIN(nDownRange),MAX(nDownRange)]

     linePlot1 = PLOT(line1X, $
                      line1Y, $
                      RGB_TABLE=hammerCT, $
                      VERT_COLORS=jvPlotData.tMag[[minI,minI]], $
                      /OVERPLOT)
     
     linePlot2 = PLOT(line2X, $
                      line2Y, $
                      RGB_TABLE=hammerCT, $
                      VERT_COLORS=jvPlotData.tMag[[maxI,maxI]], $
                      /OVERPLOT)

  ENDIF

  ;;And a colorbar thing
  IF ~KEYWORD_SET(overplotAll) THEN BEGIN
     nTMarks     = 5
     tInds       = (INDGEN(nTMarks)*nPoints)/(nTMarks-1)
     IF tInds[-1] EQ nPoints THEN tInds[-1] -= 1
     tickValues  = jvPlotData.tMag[tInds]
     tickTimes   = jvPlotData.time[tInds]
     tickName    = STRMID(TIME_TO_STR(tickTimes),11,15)
     tMagRange   = [jvPlotData.tMag[0],jvPlotData.tMag[-1]]
     cb          = COLORBAR(RGB_TABLE=hammerCT, $
                            TICKNAME=tickName, $
                            TICKVALUES=tickValues, $
                            POSITION=cbpos, $
                            RANGE=tMagRange, $
                            /NORMAL)

     IF KEYWORD_SET(savePlot) THEN BEGIN

        IF ~KEYWORD_SET(sPName) THEN BEGIN
           sPName = routName + '-believeIt.png'
        ENDIF

        PRINT,"Saving to " + sPName + ' ...'

        window1.Save,plotDir+sPName

     ENDIF

  ENDIF



END
