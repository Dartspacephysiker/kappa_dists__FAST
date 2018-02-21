;;2017/03/04
FUNCTION THEORETISCH_CURVES,conductivities, $
   UPGOING=upgoing, $
   DOWNGOING=downgoing

  COMPILE_OPT IDL2,STRICTARRSUBS

  nCond         = N_ELEMENTS(conductivities)

  CASE 1 OF
     KEYWORD_SET(downgoing): BEGIN

        nCurs         = 100
        curRange      = [0,50]

        curs          = (INDGEN(nCurs)+1)/DOUBLE(nCurs) * (MAX(curRange)-MIN(curRange)) + MIN(curRange)
        curs          = curs # TRANSPOSE(MAKE_ARRAY(nCond,/LONG,VALUE=1L))

        j0            = [0.5,4.8] ;in microA/m^2

        pots          = MAKE_ARRAY(nCurs,nCond,/FLOAT)

        FOR k=0,nCond-1 DO BEGIN
           pots[*,k]  = (j0[k]*1D-6/ conductivities[k]) * EXP(curs[*,k]/j0[k])
        ENDFOR

     END
     KEYWORD_SET(upgoing): BEGIN

        nPots         = 50

        potRange      = [1,1D5]
        pots          = 10.D^( (INDGEN(nPots)+1)/DOUBLE(nPots) * $
                               (ALOG10(MAX(potRange))-ALOG10(MIN(potRange))) + ALOG10(MIN(potRange)))

        pots          = pots # TRANSPOSE(MAKE_ARRAY(nCond,/LONG,VALUE=1L))

        curs          = MAKE_ARRAY(nPots,nCond,/FLOAT)

        FOR k=0,nCond-1 DO BEGIN
           curs[*,k]  = conductivities[k]*pots[*,k]*(1D6)
        ENDFOR

     END
  ENDCASE

  RETURN,{pot:pots,cur:curs}

END

PRO PLOT_THREEPANEL_ANALOG_TO_FIG2_ELPHIC_ETAL_1998,jvPlotData, $
   ;; T1=t1, $
   ;; T2=t2, $
   ORIGINAL_PLOTIDEE=orig_plotIdee, $
   SAVEPLOT=savePlot, $
   SPNAME=spName, $
   ORIGINATING_ROUTINE=routName, $
   UPCURRENT_CONDUCTIVITIES=upCurrent_conductivities, $
   DOWNCURRENT_CONDUCTIVITIES=downCurrent_conductivities, $
   POT_ON_X_AXIS=pot_on_x_axis, $
   JUST_J_AND_V=just_j_and_v, $
   PLOTDIR=plotDir, $
   _EXTRA=e ;; , $
   ;; ERROR_BAR_FACTOR=errorBarFac
  
  COMPILE_OPT IDL2,STRICTARRSUBS

  ;;Check out potBar
  
  ;; !P.MULTI = [0,1,2,0,0]
  ;; binSize  = 0.2
  ;; posCur_i = WHERE(jvPlotData.cur LT 0,COMPLEMENT=negCur_i)
  ;; xRange   = [-2,5.5]
  ;; CGHISTOPLOT,ALOG10(jvplotdata.pbarall.ederr[negCur_i]/jvplotdata.pbarall.ed[negCur_i]), $
  ;;             BINSIZE=binSize, $
  ;;             MININPUT=xRange[0], $
  ;;             MAXINPUT=xRange[1]
  ;; CGHISTOPLOT,ALOG10(jvplotdata.pbarall.euerr[posCur_i]/jvplotdata.pbarall.eu[posCur_i]), $
  ;;             BINSIZE=binSize, $
  ;;             MININPUT=xRange[0], $
  ;;             MAXINPUT=xRange[1]

  ;;Temperature plot
  ;; TErr = TRANSPOSE([[curPotList[0].Terr < curPotList[0].T[3,*]],[curPotList[0].Terr]])
  ;; this = ERRORPLOT(curPotList[0].time-curPotList[0].time[0], $
  ;;                  curPotList[0].T[3,*], $
  ;;                  TErr, $
  ;;                  LINESTYLE='', $
  ;;                  SYMBOL='x', $
  ;;                  /YLOG, $
  ;;                  YRANGE=[1.,1e3])

  ;; errorBarFac     = KEYWORD_SET(errorBarFac) ? errorBarFac : 1.
  nPoints          = N_ELEMENTS(jvPlotData.time)

  rgbTable         = 4
  nColors          = 256
  transpose        = 1
  hammerCT         = COLORTABLE(rgbTable,STRETCH=stretch,NCOLORS=nColors,TRANSPOSE=transpose)

  curTitle         = 'j!D||!N($\mu$A m!U-2!N)'

  potRange         = KEYWORD_SET(just_j_and_v) ? MINMAX(jvPlotData.pot) : [1D2,3.1D4]
  potTitle         = '$\Phi$ (V)'

  errSym           = '.'
  errSym_size      = 3.0
  errSym_fill      = 0
  errSym_capSize   = 0.05
  errThick         = 2.0

  ;;J-V plot options
  jv_tickValues    = !NULL
  jv_tickNames     = !NULL
  IF ~KEYWORD_SET(just_j_and_v) THEN BEGIN
     logPotTmp        = ROUND_TO_NTH_DECIMAL_PLACE(ALOG10(potRange[1]))
     jv_tickValues    = (10L)^(LINDGEN(FIX(logPotTmp)-1)+2)
     jv_tickNames     = !NULL
     FOR k=1,logPotTmp-1 DO $
        jv_tickNames  = [jv_tickNames,STRING(FORMAT='("10!U",I0,"!N")',k+1)]
     ;; jv_tickNames  = STRING(FORMAT='('+STRCOMPRESS(N_ELEMENTS(logPotTmp-1),/REMOVE_ALL)+'())'
  ENDIF
  jvSym            = '*'
  jvSymSize        = 2.0
  jvSymThick       = 2.0
  jvSymTransp      = 70
  jvSymFilled      = 1
  jv_xTitle        = curTitle
  jv_yTitle        = potTitle

  curCondLStyle    = '--'

  font_size        = 18
  timeFont_size    = 15

  ;;These all pertain to grid creation
  xGridStyle       = ':'
  yGridStyle       = ':'
  xTickLen         = 1
  yTickLen         = 1
  xSubTickLen      = 0.01
  ySubTickLen      = 0.01
  
  winDim           = [1000,800]

  CASE 1 OF
     KEYWORD_SET(just_j_and_v): BEGIN
        p1pos            = !NULL
        p2pos            = !NULL
        p3pos            = [0.08,0.08,0.95,0.94]
        cbpos            = [0.10,0.97,0.95,0.99]
     END
     ELSE: BEGIN
        p1pos            = [0.10,0.08,0.46,0.50]
        p2pos            = [0.10,0.53,0.46,0.94]
        p3pos            = [0.54,0.08,0.95,0.94]
        cbpos            = [0.10,0.97,0.95,0.99]
     END
  ENDCASE

  orig_p1Pos       = [0.1,0.1,0.95,0.8]
  orig_cbPos       = [0.1,0.96,0.95,0.98]

  IF ~KEYWORD_SET(plotDir) THEN BEGIN
     plotDir       = './'
  ENDIF

  timeTitle        = 'Seconds since ' + TIME_TO_STR(jvPlotData.time[0])

  jRange           = MINMAX(jvPlotData.cur*(-1.D))

  ;;Amplify/diminish pot by factor(s) of T?
  pot              = jvPlotData.pot
  IF jvPlotData.info.pot.T_PMFac NE 0 THEN BEGIN
     CURANDPOT__SELECT_T_AND_N,jvPlotData,avgs_JVfit, $
                               TEMPERATURE=Temperature, $
                               DENSITY=Density, $
                               ERR_TEMPERATURE=TemperatureErr, $
                               ERR_DENSITY=DensityErr, $
                               DONT_MAP_SOURCEDENS=dont_map_sourceDens, $
                               ;; THESE_USEINDS=these_useInds, $
                               /SKIP_USEINDS, $
                               ARRAYS=arrays

     pot          += Temperature*jvPlotData.info.pot.T_PMFac
  ENDIF

  IF KEYWORD_SET(orig_plotIdee) THEN BEGIN
     window        = WINDOW(DIMENSIONS=winDim, $
                        BUFFER=savePlot)

     sPlot         = SCATTERPLOT(jvPlotData.cur, $
                             pot, $
                             XRANGE=jRange, $
                             YRANGE=potRange, $
                             /YLOG, $
                             RGB_TABLE=rgbTable, $
                             MAGNITUDE=jvPlotData.tMag, $
                             SYMBOL='*', $
                             SYM_SIZE=2.0, $
                             SYM_THICK=2.0, $
                             SYM_TRANSPARENCY=70, $
                             XTITLE=curTitle, $
                             YTITLE=potTitle, $
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
                              /NORMAL, $
                             FONT_SIZE=timeFont_size)
     STOP

  ENDIF

  window1          = WINDOW(DIMENSIONS=winDim, $
                        BUFFER=savePlot)

  CTInds           = BYTSCL(jvPlotData.tMag)

  tRange           = [0,jvPlotdata.tDiff[-1]]
  errJRange        = jRange

  CASE 1 OF
     KEYWORD_SET(sans_errorBars): BEGIN

        ;;The old, error bar–less way

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;First plot

        ;; plot_1            = ERRORPLOT((jvPlotData.tDiff[inds]), $
        ;;                               jvPlotData.cur[inds], $
        ;;                               tmpErr, $
        ;;                               XRANGE=tRange, $
        ;;                               ;; YRANGE=jRange, $
        ;;                               YRANGE=errJRange, $
        ;;                               XTITLE='Seconds since ' + TIME_TO_STR(curPotList[0].jvPlotData.time[inds]), $
        ;;                               YTITLE='j!D||!N($\mu$A m!U-2!N)', $
        ;;                               /CURRENT, $
        ;;                               POSITION=p1pos)

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;Second plot

        plot_2            = SCATTERPLOT(jvPlotData.tDiff, $
                                        pot, $
                                        ;; XTITLE='Seconds since ' + TIME_TO_STR(curPotList[0].time[safe_i[0]]), $
                                        YTITLE=potTitle, $
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
        plot_3      = SCATTERPLOT(jvPlotData.cur, $
                                  pot, $
                                  XRANGE=jRange, $
                                  YRANGE=potRange, $
                                  /YLOG, $
                                  RGB_TABLE=hammerCT, $
                                  MAGNITUDE=jvPlotData.tMag, $
                                  SYMBOL=jvSym, $
                                  SYM_SIZE=jvSymSize, $
                                  SYM_THICK=jvSymThick, $
                                  SYM_TRANSPARENCY=jvSymTransp, $
                                  SYM_FILLED=jvSymFilled, $
                                  YTICKVALUES=jv_tickValues, $
                                  YTICKNAME=jv_tickNames, $
                                  XTITLE='j!D||!N($\mu$A m!U-2!N)', $
                                  YTITLE='$\Phi$ (V)', $
                                  XGRIDSTYLE=':', $
                                  YGRIDSTYLE=':', $
                                  XTICKLEN=1, $
                                  YTICKLEN=1, $
                                  XSUBTICKLEN=0.01, $
                                  YSUBTICKLEN=0.01, $
                                  FONT_SIZE=font_size, $
                                  /CURRENT, $
                                  POSITION=p3pos)

     END
     ELSE: BEGIN

        IF ~KEYWORD_SET(just_j_and_v) THEN BEGIN
           ;;Initialize things
           inds              = [0,1]
           tmpCurErr         = jvPlotData.curErr[inds]

           plot_1            = ERRORPLOT((jvPlotData.tDiff[inds]), $
                                         jvPlotData.cur[inds]*(-1.D), $
                                         tmpCurErr, $
                                         XRANGE=tRange, $
                                         ;; YRANGE=jRange, $
                                         YRANGE=errJRange, $
                                         XTITLE=timeTitle, $
                                         YTITLE=curTitle, $
                                         RGB_TABLE=hammerCT, $
                                         ;; ERRORBAR_COLOR=hammerCT[*,CTInds[0]], $
                                         VERT_COLORS=CTInds[inds], $
                                         LINESTYLE='', $
                                         ERRORBAR_CAPSIZE=errSym_capSize, $
                                         ERRORBAR_THICK=errThick, $
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
                                         FONT_SIZE=font_size, $
                                         POSITION=p1pos, $
                                         BUFFER=savePlot)

           ;;Now add all the other symbols
           FOR k=2,nPoints-1,2 DO BEGIN

              inds           = [k,((k+1) < (nPoints-1))]
              ;; tmpCurErr      = curErr[*,inds]
              tmpCurErr      = jvPlotData.curErr[inds]

              plot_1         = ERRORPLOT((jvPlotData.tDiff[inds]), $
                                         jvPlotData.cur[inds]*(-1.D), $
                                         tmpCurErr, $
                                         ;; RGB_TABLE=hammerCT, $
                                         VERT_COLORS=hammerCT[*,CTInds[inds]], $
                                         ERRORBAR_COLOR=hammerCT[*,CTInds[k]], $
                                         ERRORBAR_CAPSIZE=errSym_capSize, $
                                         ERRORBAR_THICK=errThick, $
                                         LINESTYLE='', $
                                         SYMBOL=errSym, $
                                         SYM_SIZE=errSym_size, $
                                         SYM_FILLED=errSym_fill, $
                                         /CURRENT, $
                                         ;; POSITION=p1pos, $
                                         /OVERPLOT)
           ENDFOR

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           ;;Second plot

           inds              = [0,1]
           tmpPotErr         = jvPlotData.potErr[inds]

           errPotRange       = MINMAX(pot)
           plot_2            = ERRORPLOT((jvPlotData.tDiff[inds]), $
                                         pot[inds], $
                                         tmpPotErr, $
                                         XRANGE=tRange, $
                                         /YLOG, $
                                         YRANGE=potRange, $
                                         YTITLE=potTitle, $
                                         RGB_TABLE=hammerCT, $
                                         ERRORBAR_COLOR=hammerCT[*,CTInds[0]], $
                                         VERT_COLORS=CTInds[inds], $
                                         LINESTYLE='', $
                                         ERRORBAR_CAPSIZE=errSym_capSize, $
                                         ERRORBAR_THICK=errThick, $
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
                                         POSITION=p2pos, $
                                         FONT_SIZE=font_size, $
                                         XSHOWTEXT=0B)
           ;; plot_2.xshowtext  = 0B

           ;;Now add all the other symbols
           FOR k=2,nPoints-1,2 DO BEGIN

              inds           = [k,((k+1) < (nPoints-1))]
              tmpPotErr      = jvPlotData.potErr[inds]

              plot_2         = ERRORPLOT((jvPlotData.tDiff[inds]), $
                                         pot[inds], $
                                         tmpPotErr, $
                                         /YLOG, $
                                         VERT_COLORS=hammerCT[*,CTInds[inds]], $
                                         ERRORBAR_COLOR=hammerCT[*,CTInds[k]], $
                                         ERRORBAR_CAPSIZE=errSym_capSize, $
                                         ERRORBAR_THICK=errThick, $
                                         LINESTYLE='', $
                                         SYMBOL=errSym, $
                                         SYM_SIZE=errSym_size, $
                                         SYM_FILLED=errSym_fill, $
                                         /CURRENT, $
                                         /OVERPLOT)
           ENDFOR

        ENDIF
        
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;Third plot

        CASE 1 OF
           KEYWORD_SET(pot_on_x_axis): BEGIN
              xDat = pot
              xErr = jvPlotData.potErr

              yDat = jvPlotData.cur*(-1.D)
              yErr = jvPlotData.curErr

              xTitle = jv_yTitle
              xRange = KEYWORD_SET(just_j_AND_v) ? potRange : [potRange[0]+jvPlotData.potErr[0],potRange[1]-jvPlotData.potErr[1]]
              xLog   = 1
              xTickValues = jv_tickValues
              xTickName   = jv_tickNames

              yTitle = jv_xTitle
              yRange = jRange
              yLog   = 0
           END
           ELSE: BEGIN
              xDat = jvPlotData.cur*(-1.D)
              xErr = jvPlotData.curErr

              yDat = pot
              yErr = jvPlotData.potErr

              xTitle = jv_xTitle
              xRange = jRange
              xLog   = 0

              yTitle = jv_yTitle
              yRange = KEYWORD_SET(just_j_AND_v) ? potRange : [potRange[0]+jvPlotData.potErr[0],potRange[1]-jvPlotData.potErr[1]]
              yLog   = 1
              yTickValues = jv_tickValues
              yTickName   = jv_tickNames
           END
        ENDCASE

        inds     = [0,1]
        ;; tmpXErr         = curErr[*,inds]
        ;; tmpPotErr         = potErr[*,inds]
        tmpXErr  = xErr[inds]
        tmpYErr  = yErr[inds]

        plot_3   = ERRORPLOT(xDat[inds], $
                             yDat[inds], $
                             tmpXErr, $
                             tmpYErr, $
                             XRANGE=xRange, $
                             YRANGE=yRange, $
                             XLOG=xLog, $
                             YLOG=yLog, $
                             LINESTYLE='', $
                             ERRORBAR_COLOR=hammerCT[*,CTInds[0]], $
                             ERRORBAR_CAPSIZE=errSym_capSize, $
                             ERRORBAR_THICK=errThick, $
                             SYMBOL=errSym, $
                             SYM_SIZE=errSym_size, $
                             SYM_FILLED=errSym_fill, $
                             VERT_COLORS=CTInds[inds], $
                             XTICKVALUES=xTickValues, $
                             XTICKNAME=xTickName, $
                             YTICKVALUES=yTickValues, $
                             YTICKNAME=yTickName, $
                             XTITLE=xTitle, $
                             YTITLE=yTitle, $
                             XGRIDSTYLE=xGridStyle, $
                             YGRIDSTYLE=yGridStyle, $
                             XTICKLEN=xTickLen, $
                             YTICKLEN=yTickLen, $
                             XSUBTICKLEN=xSubTickLen, $
                             YSUBTICKLEN=ySubTickLen, $
                             FONT_SIZE=font_size, $
                             /CURRENT, $
                             POSITION=p3pos)

        ;;Now add all the other symbols
        FOR k=2,nPoints-1,2 DO BEGIN

           inds     = [k,((k+1) < (nPoints-1))]
           tmpXErr  = xErr[inds]
           tmpYErr  = yErr[inds]

           plot_3   = ERRORPLOT((xDat[inds]), $
                                yDat[inds], $
                                tmpXErr, $
                                tmpYErr, $
                                ;; RGB_TABLE=hammerCT, $
                                LINESTYLE='', $
                                VERT_COLORS=hammerCT[*,CTInds[inds]], $
                                ERRORBAR_COLOR=hammerCT[*,CTInds[k]], $
                                ERRORBAR_CAPSIZE=errSym_capSize, $
                                ERRORBAR_THICK=errThick, $
                                SYMBOL=errSym, $
                                SYM_SIZE=errSym_size, $
                                SYM_FILLED=errSym_fill, $
                                /CURRENT, $
                                /OVERPLOT)
        ENDFOR

     END
  ENDCASE

  CASE 1 OF
     N_ELEMENTS(upCurrent_conductivities) EQ 0: BEGIN  
        upCurrent_conductivities = [3.D-10,1.2D-9]
     END
     N_ELEMENTS(upCurrent_conductivities) EQ 1: BEGIN  
        IF (FIX(upCurrent_conductivities) - $
            upCurrent_conductivities) EQ 0 $
        THEN BEGIN 
           upCurrent_conductivities = [3.D-10,1.2D-9]
        ENDIF
     END
  ENDCASE
  
  IF KEYWORD_SET(upCurrent_conductivities) THEN BEGIN

     nCurCond          = N_ELEMENTS(upCurrent_conductivities)
     plot_upCurCondArr = MAKE_ARRAY(nCurCond,/OBJ)

     t_curPots         = THEORETISCH_CURVES(upCurrent_conductivities, $
                                            /UPGOING)
     
     CASE 1 OF
        KEYWORD_SET(pot_on_x_axis): BEGIN
           xDat = t_curPots.pot
           yDat = t_curPots.cur
        END
        ELSE: BEGIN
           xDat = t_curPots.cur
           yDat = t_curPots.pot
        END
     ENDCASE

     FOR k=0,nCurCond-1 DO BEGIN
        plot_upCurCondArr = PLOT(xDat[*,k], $
                                 yDat[*,k], $
                                 LINESTYLE=curCondLStyle, $
                                 /OVERPLOT)
     ENDFOR

  ENDIF

  IF (WHERE(jvPlotData.cur[inds] GT 0))[0] NE -1 THEN BEGIN
     CASE 1 OF
        N_ELEMENTS(upCurrent_conductivities) EQ 0: BEGIN  
           upCurrent_conductivities = [3.D-10,1.2D-9]
        END
        N_ELEMENTS(upCurrent_conductivities) EQ 1: BEGIN  
           IF (FIX(upCurrent_conductivities) - $
               upCurrent_conductivities) EQ 0 $
           THEN BEGIN 
              upCurrent_conductivities = [3.D-10,1.2D-9]
           ENDIF
        END
     ENDCASE

     downCurrent_conductivities  = [1.6D-6,2.D-5]

     t_curPots         = THEORETISCH_CURVES(downCurrent_conductivities, $
                                            /DOWNGOING)
     CASE 1 OF
        KEYWORD_SET(pot_on_x_axis): BEGIN
           xDat = t_curPots.pot
           yDat = t_curPots.cur
        END
        ELSE: BEGIN
           xDat = t_curPots.cur
           yDat = t_curPots.pot
        END
     ENDCASE

     FOR k=0,nCurCond-1 DO BEGIN
        plot_upCurCondArr = PLOT(xDat[*,k], $
                                 yDat[*,k], $
                                 LINESTYLE=curCondLStyle, $
                                 /OVERPLOT)
     ENDFOR

  ENDIF
  
  

  ;;And a colorbar thing
  nTMarks     = 5
  tInds       = (INDGEN(nTMarks)*nPoints)/(nTMarks-1)
  IF tInds[-1] EQ nPoints THEN tInds[-1] -= 1
  tickValues  = jvPlotData.tMag[tInds]
  tickTimes   = jvPlotData.time[tInds]
  tickName    = STRMID(TIME_TO_STR(tickTimes,/MS),11,15)
  tMagRange   = [jvPlotData.tMag[0],jvPlotData.tMag[-1]]
  cb          = COLORBAR(RGB_TABLE=hammerCT, $
                         TICKNAME=tickName, $
                         TICKVALUES=tickValues, $
                         POSITION=cbpos, $
                         RANGE=tMagRange, $
                         FONT_SIZE=timeFont_size, $
                         /NORMAL)

  IF KEYWORD_SET(savePlot) THEN BEGIN

     IF ~KEYWORD_SET(sPName) THEN BEGIN
        sPName = routName + '-believeIt.png'
     ENDIF

     PRINT,"Saving to " + sPName + ' ...'

     window1.Save,plotDir+sPName

  ENDIF

END
