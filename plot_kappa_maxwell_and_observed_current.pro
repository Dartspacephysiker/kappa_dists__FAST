FUNCTION PLOT_KAPPA_MAXWELL_AND_OBSERVED_CURRENT,kappa_current,gauss_current,obs_current, $
   ADD_LINEAR_FITS=add_linear_fits, $
   MAGRATIO=magRatio, $
   ORBIT=orbit, $
   POTTITLESTR=potTitleStr, $
   USE_POTSTR_AS_TITLE=use_potStr_as_title, $
   OBSNAME=obsName, $
   POSITION=position, $
   SUPPRESS_TITLE=suppress_title, $
   SUPPRESS_AXIS_TITLES=suppress_axis_titles, $
   SUPPRESS_LEGEND=suppress_legend, $
   SUPPRESS_XTICKMARKS=suppress_xTickMarks, $
   SUPPRESS_YTICKMARKS=suppress_yTickMarks, $
   SUPPRESS_SCATTER_LEGEND=suppress_scatter_legend, $
   LEGEND__FONT_SIZE=legend__font_size, $
   XLOG=xLog, $
   YLOG=yLog, $
   XRANGE=xRange, $
   YRANGE=yRange, $
   ;; PLOTNAME=plotName, $
   FOR_INTEGRATED_2DFIT_CURRENTS=for_2Dfit_currents, $
   WINDOW=window, $
   BUFFER=buffer

  COMPILE_OPT idl2


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;All kinds of plot preliminaries
  plotArr      = MAKE_ARRAY(2*(1+KEYWORD_SET(add_linear_fits)),/OBJ)

  ;;Titles
  modName      = KEYWORD_SET(for_2Dfit_currents) ? '2DFit' : "Model"

  IF ~KEYWORD_SET(suppress_title) THEN BEGIN
     CASE 1 OF
        KEYWORD_SET(use_potStr_as_title): BEGIN
           plotTitle = potTitleStr
        END
        ELSE: BEGIN
           plotTitle = modName + ' Currents vs. ' + obsName + ' Current' + $
                       (KEYWORD_SET(orbit) ? '!C(FAST orbit ' +STRCOMPRESS(orbit,/REMOVE_ALL) + ')' : '')
        END
     ENDCASE
  ENDIF
  IF ~KEYWORD_SET(suppress_axis_titles) THEN BEGIN
     xTitle    = 'Observed Current (' + CGGREEK('mu') + 'A/m!U2!N)'
     yTitle    = modName + ' Current (' + CGGREEK('mu') + 'A/m!U2!N)'
  ENDIF

  ;;Names
  kappaName    = modName + ' (Kappa)'
  gaussName    = modName + ' (Maxwellian)'
  
  kappaLName   = "K "
  gaussLName   = "G "

  ;;Bonus
  pSyms        = ['+','x']
  pColors      = ['blue','purple']
  lStyles      = [0,2]

  IF N_ELEMENTS(legend__font_size) EQ 0 THEN BEGIN     
     legFontSize  = 16
  ENDIF ELSE BEGIN
     legFontSize  = legend__font_size
  ENDELSE

  legPos       = [0.06,0.85]
  R_BPos       = [0.88,0.88]

  ;;Logplots?
  IF N_ELEMENTS(xLog) EQ 0 THEN BEGIN
     xLog      = 1
  ENDIF
  IF N_ELEMENTS(yLog) EQ 0 THEN BEGIN
     yLog      = 1
  ENDIF

  ;;Ranges
  IF ~KEYWORD_SET(xRange) THEN BEGIN
     IF KEYWORD_SET(xLog) THEN BEGIN
        xRange    = [0.001,100]
     ENDIF ELSE BEGIN
        xRange    = [MIN(obs_current),MAX(obs_current)]
     ENDELSE
  ENDIF
  IF ~KEYWORD_SET(yRange) THEN BEGIN
     IF KEYWORD_SET(yLog) THEN BEGIN
        yRange    = [0.001,100]
     ENDIF ELSE BEGIN
        yRange    = [MIN(kappa_current) < MIN(gauss_current), MAX(kappa_current) > MAX(gauss_current)]
     ENDELSE
  ENDIF

  plotArr[0]   = PLOT(obs_current,kappa_current, $
                      NAME=kappaName, $
                      TITLE=plotTitle, $
                      XTITLE=xTitle, $
                      YTITLE=yTitle, $
                      XRANGE=xRange, $
                      YRANGE=yRange, $
                      SYMBOL=pSyms[0], $
                      SYM_COLOR=pColors[0], $
                      FONT_SIZE=18, $
                      LINESTYLE=6, $
                      ;; XTICKNAME=KEYWORD_SET(suppress_xTickMarks) ? REPLICATE(' ',30) : !NULL, $
                      XSHOWTEXT=KEYWORD_SET(suppress_xTickMarks) ? 0 : !NULL, $
                      YSHOWTEXT=KEYWORD_SET(suppress_yTickMarks) ? 0 : !NULL, $
                      XLOG=xLog, $
                      YLOG=yLog, $
                      POSITION=position, $
                      CURRENT=window, $
                      BUFFER=buffer)

  plotArr[1]   = PLOT(obs_current,gauss_current, $
                      NAME=gaussName, $
                      TITLE=plotTitle, $
                      XTITLE=xTitle, $
                      YTITLE=yTitle, $
                      XRANGE=xRange, $
                      YRANGE=yRange, $
                      SYMBOL=pSyms[1], $
                      SYM_COLOR=pColors[1], $
                      LINESTYLE=6, $
                      ;; XTICKNAME=KEYWORD_SET(suppress_xTickMarks) ? REPLICATE(' ',30) : !NULL, $
                      XSHOWTEXT=KEYWORD_SET(suppress_xTickMarks) ? 0 : !NULL, $
                      YSHOWTEXT=KEYWORD_SET(suppress_yTickMarks) ? 0 : !NULL, $
                      XLOG=xLog, $
                      YLOG=yLog, $
                      POSITION=position, $
                      CURRENT=window, $
                      /OVERPLOT, $
                      BUFFER=buffer)


  IF KEYWORD_SET(add_linear_fits) THEN BEGIN

     sortCur_i    = SORT(obs_current)
     sortObsC     = obs_current[sortCur_i]

     kappaFit     = LINFIT(sortObsC,kappa_current[sortCur_i], $
                           YFIT=kappaLine, $
                           PROB=kappaProb, $
                           CHISQR=kappaChi)
     gaussFit     = LINFIT(sortObsC,gauss_current[sortCur_i], $
                           YFIT=gaussLine, $
                           PROB=gaussProb, $
                           CHISQR=gaussChi)

     kappaCorr    = CORRELATE(sortObsC,kappa_current[sortCur_i])
     gaussCorr    = CORRELATE(sortObsC,gauss_current[sortCur_i])

     kLineString  = STRING(FORMAT='("y=",F0.2,"x+",F0.2)',kappaFit[0],kappaFit[1])
     gLineString  = STRING(FORMAT='("y=",F0.2,"x+",F0.2)',gaussFit[0],gaussFit[1])

     ;; kLN          = kappaLName + STRING(FORMAT='(" (R=",F0.3,")")',kappaCorr)
     ;; gLN          = gaussLName + STRING(FORMAT='(" (R=",F0.3,")")',gaussCorr)

     kLN          = kappaLName + STRING(FORMAT='(" (R=",F0.2,", ",A0,")")', $
                                        kappaCorr, $
                                        kLineString)
     gLN          = gaussLName + STRING(FORMAT='(" (R=",F0.2,", ",A0,")")', $
                                        gaussCorr, $
                                        gLineString)

     plotArr[2]   = PLOT(sortObsC,kappaLine, $
                         NAME=kLN, $
                         TITLE=plotTitle, $
                         XTITLE=xTitle, $
                         YTITLE=yTitle, $
                         XRANGE=xRange, $
                         YRANGE=yRange, $
                         LINESTYLE=lStyles[0], $
                         COLOR=pColors[0], $
                         XSHOWTEXT=KEYWORD_SET(suppress_xTickMarks) ? 0 : !NULL, $
                         YSHOWTEXT=KEYWORD_SET(suppress_yTickMarks) ? 0 : !NULL, $
                         XLOG=xLog, $
                         YLOG=yLog, $
                         POSITION=position, $
                         CURRENT=window, $
                         /OVERPLOT, $
                         BUFFER=buffer)
     
     plotArr[3]   = PLOT(sortObsC,gaussLine, $
                         NAME=gLN, $
                         TITLE=plotTitle, $
                         XTITLE=xTitle, $
                         YTITLE=yTitle, $
                         XRANGE=xRange, $
                         YRANGE=yRange, $
                         LINESTYLE=lStyles[1], $
                         COLOR=pColors[1], $
                         XSHOWTEXT=KEYWORD_SET(suppress_xTickMarks) ? 0 : !NULL, $
                         YSHOWTEXT=KEYWORD_SET(suppress_yTickMarks) ? 0 : !NULL, $
                         XLOG=xLog, $
                         YLOG=yLog, $
                         POSITION=position, $
                         CURRENT=window, $
                         /OVERPLOT, $
                         BUFFER=buffer)
     

  ENDIF

  IF ~KEYWORD_SET(suppress_legend) THEN BEGIN
     IF N_ELEMENTS(position) GT 0 THEN BEGIN
        legPos[0] = position[0] + legPos[0]*(position[2]-position[0])
        legPos[1] = position[1] + legPos[1]*(position[3]-position[1])
     ENDIF
     CASE 1 OF
        (KEYWORD_SET(suppress_scatter_legend) AND KEYWORD_SET(add_linear_fits)): BEGIN
           legTarg = plotArr[2:3]
        END
        ELSE: BEGIN
           legTarg = plotArr[*]
        END
     ENDCASE
     legend       = LEGEND(TARGET=legTarg[*], $
                           POSITION=legPos, $
                           HORIZONTAL_ALIGNMENT=0.0, $
                           /NORMAL, $
                           FONT_SIZE=legFontSize)
  ENDIF

  IF KEYWORD_SET(magRatio) THEN BEGIN
     R_Bstring  = STRING(FORMAT='("R!DB!N = ",G0.2)',magRatio)
  ENDIF ELSE BEGIN
     R_Bstring  = ''
  ENDELSE
  IF KEYWORD_SET(potTitleStr) AND ~KEYWORD_SET(use_potStr_as_title) THEN BEGIN
     potString  = STRING(FORMAT='("!C",A0)',potTitleStr)
  ENDIF ELSE BEGIN
     potString  = ''
  ENDELSE

  IF ( KEYWORD_SET(magRatio) OR KEYWORD_SET(potTitleStr) ) THEN BEGIN
     IF N_ELEMENTS(position) GT 0 THEN BEGIN
        R_BPos[0] = position[0] + R_BPos[0]*(position[2]-position[0])
        R_BPos[1] = position[1] + R_BPos[1]*(position[3]-position[1]-0.1)
     ENDIF

     R_Btext    = TEXT(R_BPos[0],R_BPos[1], $
                       STRING(FORMAT='("R!DB!N  = ",I0,A0)',magRatio,potString), $
                       ALIGNMENT=0.0, $
                       /NORMAL, $
                       FONT_SIZE=legFontSize)
  ENDIF
  
  RETURN,plotArr
END