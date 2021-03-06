PRO PLOT_KAPPA_FITS,orig,kappaFit,gaussFit,oneCurve, $
                    CLAMPED_TEMPERATURE=clamped_temperature, $
                    ;; TITLE=title, $
                    BOUNDS_I=bounds_i, $
                    XRANGE=xRange, $
                    YRANGE=yRange, $
                    XLOG=xLog, $
                    YLOG=yLog, $
                    STRINGS=strings, $
                    ADD_GAUSSIAN_ESTIMATE=add_gaussian_estimate, $
                    ADD_FITPARAMS_TEXT=add_fitParams_text, $
                    ADD_ANGLE_LABEL=add_angle_label, $
                    ADD_CHI_VALUE=add_chi_value, $
                    PROVIDED_CHI2REDK=provided_chi2RedK, $
                    PROVIDED_CHI2REDG=provided_chi2RedG, $
                    ADD_WINTITLE=add_winTitle, $
                    SAVE_FITPLOTS=save_fitPlots, $
                    PLOT_FULL_FIT=plot_full_fit, $
                    SKIP_BAD_FITS=skip_bad_fits, $
                    USING_SDT_DATA=using_SDT_data, $
                    VERTICAL_LINES=vertical_lines, $
                    ;; PLOT_SAVENAME=plotSN, $
                    CUSTOM_PLOTSN=custom_plotSN, $
                    CUSTOM_TITLE=custom_title, $
                    CUSTOM_PLOTSUFF=custom_plotSuff, $
                    USE_PSYM_FOR_DATA=psymData, $
                    PLOTDIR=plotDir, $
                    ADD_PLOTDIR_SUFF=add_plotDir_suff, $
                    OUT_WINDOWARR=windowArr, $
                    BUFFER=buffer, $
                    UNITS=units, $
                    POSTSCRIPT=postscript, $
                    DMSP=DMSP, $
                    OUT_TMPPLOTDIR=tmpDir, $
                    EPS=eps

  COMPILE_OPT IDL2,STRICTARRSUBS

  skipping             = KEYWORD_SET(skip_bad_fits)

  legPos               = [0.87,0.84]
  IF KEYWORD_SET(DMSP) THEN BEGIN
     legPos            = [0.45,0.52]
  ENDIF

  IF N_ELEMENTS(units) EQ 0 THEN BEGIN
     units             = 'eFlux'
  ENDIF

  CASE STRUPCASE(units) OF
     'EFLUX': BEGIN
        pPref          = '-eFlux_fit'
        unitTitle      = "e!E-!N energy flux"
        yTitle         = "Differential Energy Flux (eV/cm$^2$-s-sr-eV)"
        lowerBound     = 1.0e5
        upperBound     = 1.0e10
     END
     'FLUX':BEGIN
        pPref          = '-nFlux_fit'
        unitTitle      = "e!E-!N # flux"
        yTitle         = "Differential Number Flux (#/cm$^2$-s-sr-eV)"      
        lowerBound     = KEYWORD_SET(DMSP) ? 1.0D1 : 1.0D1
        upperBound     = 1.0D8
     END
     'JE_OVER_E': BEGIN
        pPref          = '-j_over_e_fit'
        unitTitle      = "e!E-!N # flux"
        yTitle         = "J(E)/E (#/cm$^2$-s-sr-eV!U2!N)"
        lowerBound     = 1.0D-3
        upperBound     = 1.0D7
     END
  ENDCASE
  ;;Need to know if OMNI2D is responsible for this, or something else
  pref                 = pPref + (KEYWORD_SET(using_sdt_data) ? '__SDT_data-' : '-')

  IF ~KEYWORD_SET(custom_plotSN) THEN BEGIN
     plotSN               = STRING(FORMAT='(A0,A0,A0,"-",A0,A0,"-orb_",I0,"__",A0,A0,A0,A0)', $
                                   strings.today, $
                                   pref, $
                                   strings.timeFNStrs[bounds_i], $
                                   strings.eeb_or_ees, $
                                   strings.avgStr, $
                                   strings.orbStr, $
                                   strings.orbDate[bounds_i], $
                                   strings.angleStr, $
                                   (KEYWORD_SET(clamped_temperature) ? '-clampT' : ''), $
                                   (KEYWORD_SET(custom_plotSuff    ) ? custom_plotSuff : ''))
     IF N_ELEMENTS(add_angle_label) GT 0  THEN BEGIN
        ;; plotSN           += STRING(FORMAT='("-angle_",F0.1)',kappaFit.A[6])
        plotSN      += STRING(FORMAT='("-angle_",F0.1)',add_angle_label)
     ENDIF
  ENDIF ELSE BEGIN
     plotSN = custom_plotSN
  ENDELSE

  CASE 1 OF
     KEYWORD_SET(eps): BEGIN
        fExt = '.eps'
     END
     KEYWORD_SET(postscript): BEGIN
        fExt = '.ps'
     END
     ELSE: BEGIN
        fExt = '.png'
     END
  ENDCASE
  plotSN         += fExt

  ;; title           = STRING(FORMAT='(A0,", (Orbit ",I0,", ",A0,")")', $
  ;;                          unitTitle, $
  ;;                          strings.orbStr, $
  ;;                          strings.orbDate[bounds_i])

  ;; title           = STRING(FORMAT='(A0,", (Orbit ",A0,")")', $
  ;;                          strings.timeFNStrs[bounds_i], $
  ;;                          strings.orbStr)
  IF KEYWORD_SET(custom_title) THEN BEGIN
     title = custom_title
  ENDIF ELSE BEGIN
     title           = STRING(FORMAT='("Orbit ",A0,", ",A0)', $
                              strings.orbStr, $
                              strings.yearStr+'/'+strings.timeStrs[bounds_i])
  ENDELSE

  ;;plot things
  nPlots          = KEYWORD_SET(orig)+ $
                    KEYWORD_SET(kappaFit)+ $
                    KEYWORD_SET(gaussFit)+ $
                    KEYWORD_SET(oneCurve)

  IF KEYWORD_SET(add_winTitle) THEN BEGIN
     winTitle     = STRING(FORMAT='(A0)', $
                           strings.orbDate[bounds_i])
     IF KEYWORD_SET(add_angle_label) THEN BEGIN
        winTitle  = winTitle + ', Angle ' + STRING(FORMAT='(F-8.1)',add_angle_label)
     ENDIF

     IF KEYWORD_SET(add_chi_value) THEN BEGIN
        winTitle  = winTitle + ', chi^2 ' + STRING(FORMAT='(G-9.4)',add_chi_value)
     ENDIF
  ENDIF
  window          = WINDOW(DIMENSION=[800,640],TITLE=winTitle,BUFFER=buffer)
  windowArr       = N_ELEMENTS(windowArr) GT 0 ? [windowArr,window] : window

  plotArr         = MAKE_ARRAY(nPlots,/OBJ) 
  targArr         = !NULL

  colorList       = LIST('BLACK','BLUE','RED','GRAY')

  ;;Silly string stuff
  xTitle          = "Energy (eV)"

  lineStyle       = ['','--','-.','-:']

  ;; yRange          = [(MIN(orig.y[WHERE(orig.y GT 0)]) < $
  ;;                    MIN(kappaFit.yFull[WHERE(kappaFit.yFull GT 0)])) > lowerBound, $
  ;;                    (MAX(orig.y[WHERE(orig.y GT 0)]) > $
  ;;                    MAX(kappaFit.yFull[WHERE(kappaFit.yFull GT 0)])) < upperBound]
  yRange          = [lowerBound, $
                     ( ( MAX(orig.y[WHERE(orig.y GT 0)]) * 1.02) > $
                       ( MAX(kappaFit.yFull[WHERE(kappaFit.yFull GT 0)]) * 1.02 )) $
                     < upperBound]
  xRange[1]       = 3.4D4

  iPlot           = 0

  title_font_size  = 18
  xTickFont_size   = 14
  yTickFont_size   = 14
  fitInfoFont_size = 14
  fitInfoFont_style = 0 ;bold

  ;;OneCount curve
  IF N_ELEMENTS(oneCurve) GT 0 THEN BEGIN
     plotArr[iPlot]    = PLOT(oneCurve.x, $
                              oneCurve.y, $
                              NAME=oneCurve.name, $
                              TITLE=title, $
                              XTITLE=xTitle, $
                              YTITLE=yTitle, $
                              XRANGE=xRange, $
                              YRANGE=yRange, $
                              FONT_SIZE=title_font_size, $
                              XTICKFONT_SIZE=xTickFont_size, $
                              YTICKFONT_SIZE=yTickFont_size, $
                              THICK=2.2, $
                              LINESTYLE=lineStyle[3], $
                              COLOR=colorList[3], $
                              ;; /OVERPLOT, $
                              CURRENT=window) 
     add_oneCurve_to_leg = 1
     iPlot++
  ENDIF
  

  ;;Data
  errInd          = -3
  STR_ELEMENT,orig,'yError',INDEX=errInd
  IF errInd GE 0 THEN BEGIN
     plotArr[iPlot]  = ERRORPLOT(orig.x, $ ;x, $
                                 orig.y, $
                                 orig.yError, $
                                 TITLE=title, $
                                 NAME=orig.name, $
                                 XTITLE=xTitle, $
                                 YTITLE=yTitle, $
                                 XRANGE=xRange, $
                                 YRANGE=yRange, $
                                 YLOG=1, $
                                 XLOG=1, $
                                 THICK=2.2, $
                                 ERRORBAR_LINESTYLE=':', $
                                 ERRORBAR_COLOR=KEYWORD_SET(psymData) ? colorList[0] : !NULL, $
                                 LINESTYLE=KEYWORD_SET(psymData) ? 6 : !NULL, $
                                 SYMBOL=KEYWORD_SET(psymData) ? 1 : !NULL, $
                                 SYM_COLOR=KEYWORD_SET(psymData) ? colorList[0] : !NULL, $
                                 COLOR=colorList[0], $
                                 OVERPLOT=iPlot GT 0, $
                                 CURRENT=window) 

     ;; plotArr[iPlot].errorBar_thick   = 1.0
     ;; plotArr[iPlot].errorBar_color     = KEYWORD_SET(psymData) ? colorList[0] : !NULL
     ;; plotArr[iPlot].errorBar_lineStyle = '--' ;;':'
     ;; plotArr[iPlot].errorBar_capSize   = 0.1

  ENDIF ELSE BEGIN
     plotArr[iPlot]  = PLOT(orig.x, $ ;x, $
                            orig.y, $
                            TITLE=title, $
                            NAME=orig.name, $
                            XTITLE=xTitle, $
                            YTITLE=yTitle, $
                            XRANGE=xRange, $
                            YRANGE=yRange, $
                            YLOG=1, $
                            XLOG=1, $
                            THICK=2.2, $
                            LINESTYLE=KEYWORD_SET(psymData) ? 6 : !NULL, $
                            SYMBOL=KEYWORD_SET(psymData) ? 1 : !NULL, $
                            SYM_COLOR=KEYWORD_SET(psymData) ? colorList[0] : !NULL, $
                            COLOR=colorList[0], $
                            ;; OVERPLOT=i GT 0, $
                            CURRENT=window) 
  ENDELSE  
  ;; targArr                 = [targArr,plotArr[iPlot]]
  iPlot++

  ;;Kappa fit
  IF N_ELEMENTS(kappaFit) GT 0 THEN BEGIN
     keep = skipping ? (kappaFit.fitStatus EQ 0) : 1

     IF keep THEN BEGIN
        plotArr[iPlot]    = PLOT(KEYWORD_SET(plot_full_fit) ? kappaFit.xFull : kappaFit.x, $ ;x, $
                                 KEYWORD_SET(plot_full_fit) ? kappaFit.yFull : kappaFit.y, $
                                 ;; TITLE=title, $
                                 NAME=kappaFit.name, $
                                 ;; XTITLE=xTitle, $
                                 ;; YTITLE=yTitle, $
                                 ;; XRANGE=kappaFit.xRange, $
                                 ;; YRANGE=kappaFit.yRange, $
                                 ;; YLOG=kappaFit.yLog, $
                                 ;; XLOG=kappaFit.xLog, $
                                 THICK=2.2, $
                                 LINESTYLE=lineStyle[1], $
                                 COLOR=colorList[1], $
                                 /OVERPLOT, $
                                 CURRENT=window) 
        targArr           = [targArr,plotArr[iPlot]]
        iPlot++
     ENDIF
  ENDIF

  ;;GaussFit
  IF N_ELEMENTS(gaussFit) GT 0 THEN BEGIN
     keep = skipping ? (gaussFit.fitStatus EQ 0) : 1

     IF keep THEN BEGIN
        plotArr[iPlot]    = PLOT(KEYWORD_SET(plot_full_fit) ? gaussFit.xFull : gaussFit.X, $
                                 KEYWORD_SET(plot_full_fit) ? gaussFit.yFull : gaussFit.y, $
                                 ;; TITLE=title, $
                                 NAME=gaussFit.name, $
                                 ;; XTITLE=xTitle, $
                                 ;; YTITLE=yTitle, $
                                 ;; XRANGE=gaussFit.xRange, $
                                 ;; YRANGE=gaussFit.yRange, $
                                 ;; YLOG=gaussFit.yLog, $
                                 ;; XLOG=gaussFit.xLog1, $
                                 THICK=2.2, $
                                 LINESTYLE=lineStyle[2], $
                                 COLOR=colorList[2], $
                                 /OVERPLOT, $
                                 CURRENT=window) 
        targArr        = [targArr,plotArr[iPlot]]
        iPlot++
     ENDIF
  ENDIF

  IF KEYWORD_SET(add_oneCurve_to_leg) THEN BEGIN
     targArr           = [targArr,plotArr[0]]
  ENDIF

  ;; legend               = LEGEND(TARGET=targArr[*],POSITION=[0.55,0.85],/NORMAL)
  legend               = LEGEND(TARGET=targArr[*],POSITION=legPos,/NORMAL)

  IF KEYWORD_SET(vertical_lines) THEN BEGIN
     minX              = MIN(kappaFit.x)*0.96
     maxX              = MAX(kappaFit.x)*1.04
     vY                = [MIN(yRange),MAX(yRange)]

     vTransp           = 75
     ;; vCol              = colorList[3]
     vCol              = 'Green'
     ;; vLineStyle        = lineStyle[3]
     ;; vLineStyle        = '--'
     ;; vLineStyle        = ':'
     vLineStyle        = '-'

     vPlot1            = PLOT([minX,minX], $
                              vY, $
                              ;; NAME=oneCurve.name, $
                              ;; THICK=1.5, $
                              TRANSPARENCY=vTransp, $
                              LINESTYLE=vLineStyle, $
                              COLOR=vCol, $
                              /OVERPLOT, $
                              CURRENT=window) 

     vPlot2            = PLOT([maxX,maxX], $
                              vY, $
                              ;; NAME=oneCurve.name, $
                              ;; THICK=1.5, $
                              TRANSPARENCY=vTransp, $
                              LINESTYLE=vLineStyle, $
                              COLOR=vCol, $
                              /OVERPLOT, $
                              CURRENT=window) 

  ENDIF

  IF KEYWORD_SET(add_fitParams_text) THEN BEGIN
     ;; fitTitle          = ["Bulk energy  (eV)","Plasma temp. (eV)","Kappa","Density     (cm^-3)"]
     ;; fitInfoStr        = [STRING(FORMAT='(F-15.2)',kappaFit.A[0]), $
     ;;                      STRING(FORMAT='(F-15.2)',kappaFit.A[1]), $
     ;;                      STRING(FORMAT='(F-7.3)',kappaFit.A[2]), $
     ;;                      STRING(FORMAT='(F-8.4)',kappaFit.A[3])]
     fitTitle          = ["Bulk energy  (eV)","Plasma temp. (eV)","Density     (cm^-3)"]
     fitInfoStr        = [STRING(FORMAT='(I-15)',kappaFit.A[0]), $
                          STRING(FORMAT='(I-15)',kappaFit.A[1]), $
                          ;; STRING(FORMAT='(F-7.3)',kappaFit.A[2]), $
                          STRING(FORMAT='(F-8.3)',kappaFit.A[3])]

     IF KEYWORD_SET(add_angle_label) THEN BEGIN
        fitTitle       = [fitTitle,"Angle         (deg)"]
        ;; fitInfoStr = [fitInfoStr,STRING(FORMAT='(F-8.4)',kappaFit.A[6])]
        fitInfoStr = [fitInfoStr,STRING(FORMAT='(F-8.1)',add_angle_label)]
     ENDIF

     IF KEYWORD_SET(add_chi_value) THEN BEGIN
        fitTitle = [fitTitle,'chi^2']
        ;; fitInfoStr = [fitInfoStr,STRING(FORMAT='(F-8.4)',kappaFit.A[6])]
        ;; fitInfoStr = [fitInfoStr,STRING(FORMAT='(G-9.4)',add_chi_value)]
        ;; fitInfoStr = [fitInfoStr,STRING(FORMAT='(G-9.4)',kappaFit.chi2)]
        fitInfoStr = [fitInfoStr,STRING(FORMAT='(G-9.3)', $
                                        KEYWORD_SET(provided_chi2RedK) ? $
                                        provided_chi2RedK              : $
                                        kappaFit.chi2/(N_ELEMENTS(kappaFit.x)-4))]
        ;; chiInd     = 4 + KEYWORD_SET(add_angle_label)
        chiInd     = 3 + KEYWORD_SET(add_angle_label)
     ENDIF

     ;; fitParamsText     = TEXT(0.17,0.22, $
     fitParamsText     = TEXT(0.17,0.25, $
                              STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[0],fitInfoStr[0]) + '!C' + $
                              STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[1],fitInfoStr[1]) + '!C' + $
                              STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[2],fitInfoStr[2]) + '!C' + $
                              ;; STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[3],fitInfoStr[3]) + '!C' + $
                              (KEYWORD_SET(add_angle_label) ? $
                               STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[3],fitInfoStr[3]) + '!C' : '') + $
                               ;; STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[4],fitInfoStr[4]) + '!C' : '') + $
                              (KEYWORD_SET(add_chi_value) ? $
                               STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[chiInd],fitInfoStr[chiInd]) + '!C' : ''), $; + $
                              ;; STRING(FORMAT='("Fit success",T20,": ",A0)',(kappaFit.fitStatus EQ 0 ? 'Y' : 'N')), $
                              FONT_SIZE=fitInfoFont_size, $
                              FONT_NAME='Courier', $
                              /NORMAL, $
                              FONT_COLOR=colorList[1], $
                              FONT_STYLE=fitInfoFont_style)

     IF KEYWORD_SET(add_gaussian_estimate) THEN BEGIN
        ;; fitTitle       = ["Bulk energy  (eV)","Plasma temp. (eV)","Kappa","Density     (cm^-3)"]
        fitTitle       = ["Bulk energy  (eV)","Plasma temp. (eV)","Density     (cm^-3)"]
        fitInfoStr     = [STRING(FORMAT='(I-15)',gaussFit.A[0]), $
                          STRING(FORMAT='(F-15.1)',gaussFit.A[1]), $
                          ;; STRING(FORMAT='(F-7.3)',gaussFit.A[2]), $
                          STRING(FORMAT='(F-8.3)',gaussFit.A[3])]

        IF KEYWORD_SET(add_angle_label) THEN BEGIN
           fitTitle    = [fitTitle,"Angle         (deg)"]
           ;; fitInfoStr  = [fitInfoStr,STRING(FORMAT='(F-8.4)',gaussFit.A[6])]
           fitInfoStr  = [fitInfoStr,STRING(FORMAT='(F-8.4)',add_angle_label)]
        ENDIF

        IF KEYWORD_SET(add_chi_value) THEN BEGIN
           fitTitle   = [fitTitle,'chi^2']
           ;; fitInfoStr = [fitInfoStr,STRING(FORMAT='(F-8.4)',kappaFit.A[6])]
           ;; fitInfoStr = [fitInfoStr,STRING(FORMAT='(G-9.4)',add_chi_value)]
           fitInfoStr = [fitInfoStr,STRING(FORMAT='(G-9.3)', $
                                           KEYWORD_SET(provided_chi2RedG) ? $
                                           provided_chi2RedG              : $
                                           gaussFit.chi2/(N_ELEMENTS(gaussFit.x)-3))]
           chiInd     = 3 + KEYWORD_SET(add_angle_label)
        ENDIF

        ;; fitParamsText  = TEXT(0.49,0.22, $
        fitParamsText     = TEXT(0.17,0.12, $
                              STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[0],fitInfoStr[0]) + '!C' + $
                              STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[1],fitInfoStr[1]) + '!C' + $
                              ;; STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[2],fitInfoStr[2]) + '!C' + $
                              STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[2],fitInfoStr[2]) + '!C' + $
                              (KEYWORD_SET(add_angle_label) ? $
                               STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[3],fitInfoStr[3]) + '!C' : '') + $
                              (KEYWORD_SET(add_chi_value) ? $
                               STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[chiInd],fitInfoStr[chiInd]) + '!C' : ''), $; + $
                              ;; STRING(FORMAT='("GaussFit success",T20,": ",A0)',(gaussFit.fitStatus EQ 0 ? 'Y' : 'N')), $
                              FONT_SIZE=fitInfoFont_size, $
                              FONT_NAME='Courier', $
                              /NORMAL, $
                              FONT_COLOR=colorList[2], $
                              FONT_STYLE=fitInfoFont_style)
     ENDIF

  ENDIF

  IF KEYWORD_SET(save_fitplots) THEN BEGIN

     IF N_ELEMENTS(plotDir) EQ 0 THEN BEGIN
        SET_PLOT_DIR,plotDir,/FOR_SDT,/ADD_TODAY,/VERBOSE
     ENDIF

     PRINT,'Saving plot to ' + plotSN + '...'

     tmpDir = plotDir
     IF KEYWORD_SET(add_plotDir_suff) THEN BEGIN
        
        tmpDir += '/' + add_plotDir_suff
        IF ~FILE_TEST(tmpDir,/DIRECTORY) THEN BEGIN
           PRINT,"Making directory " + tmpDir
           SPAWN,'mkdir -p ' + tmpDir
        ENDIF
     ENDIF


     window.Save,tmpDir + plotSN
     window.Close
  ENDIF

END
