FUNCTION PLOT_KAPPA_MAXWELL_AND_OBSERVED_CURRENT,kappa_current,gauss_current,obs_current, $
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
   LEGEND__FONT_SIZE=legend__font_size, $
   XLOG=xLog, $
   YLOG=yLog, $
   XRANGE=xRange, $
   YRANGE=yRange, $
   ;; PLOTNAME=plotName, $
   WINDOW=window, $
   BUFFER=buffer

  COMPILE_OPT idl2


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;All kinds of plot preliminaries
  plotArr      = MAKE_ARRAY(2,/OBJ)

  ;;Titles
  IF ~KEYWORD_SET(suppress_title) THEN BEGIN
     CASE 1 OF
        KEYWORD_SET(use_potStr_as_title): BEGIN
           plotTitle = potTitleStr
        END
        ELSE: BEGIN
           plotTitle = 'Model Currents vs. ' + obsName + ' Current' + $
                       (KEYWORD_SET(orbit) ? '!C(FAST orbit ' +STRCOMPRESS(orbit,/REMOVE_ALL) + ')' : '')
        END
     ENDCASE
  ENDIF
  IF ~KEYWORD_SET(suppress_axis_titles) THEN BEGIN
     xTitle    = 'Observed Current (' + CGGREEK('mu') + 'A/m!U2!N)'
     yTitle    = 'Model Current (' + CGGREEK('mu') + 'A/m!U2!N)'
  ENDIF

  ;;Names
  kappaName    = "Kappa model"
  gaussName    = "Maxwellian model"

  ;;Bonus
  pSyms        = ['+','x']
  pColors      = ['blue','purple']

  IF N_ELEMENTS(legend__font_size) EQ 0 THEN BEGIN     
     legFontSize  = 16
  ENDIF ELSE BEGIN
     legFontSize  = legend__font_size
  ENDELSE

  legPos       = [0.06,0.85]
  R_BPos       = [0.06,0.7]

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
                      XLOG=xLog, $
                      YLOG=yLog, $
                      POSITION=position, $
                      CURRENT=window, $
                      /OVERPLOT, $
                      BUFFER=buffer)


  IF ~KEYWORD_SET(suppress_legend) THEN BEGIN
     IF N_ELEMENTS(position) GT 0 THEN BEGIN
        legPos[0] = position[0] + legPos[0]*(position[2]-position[0])
        legPos[1] = position[1] + legPos[1]*(position[3]-position[1])
     ENDIF
     legend       = LEGEND(TARGET=plotArr[*], $
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