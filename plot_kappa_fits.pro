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
                    ADD_WINTITLE=add_winTitle, $
                    SAVE_FITPLOTS=save_fitPlots, $
                    PLOT_FULL_FIT=plot_full_fit, $
                    SKIP_BAD_FITS=skip_bad_fits, $
                    USING_SDT_DATA=using_SDT_data, $
                    VERTICAL_LINES=vertical_lines, $
                    ;; PLOT_SAVENAME=plotSN, $
                    USE_PSYM_FOR_DATA=psymData, $
                    PLOTDIR=plotDir, $
                    POSTSCRIPT=postscript, $
                    OUT_WINDOWARR=windowArr, $
                    BUFFER=buffer, $
                    UNITS=units

  COMPILE_OPT IDL2,STRICTARRSUBS

  skipping             = KEYWORD_SET(skip_bad_fits)

  IF N_ELEMENTS(units) EQ 0 THEN BEGIN
     units             = 'eFlux'
  ENDIF

  CASE STRUPCASE(units) OF
     'EFLUX': BEGIN
        pPref          = '-eFlux_fit'
        unitTitle      = "e!U-!N energy flux"
        yTitle         = "Differential Energy Flux!C(eV/cm!U2!N-sr-s)"
        lowerBound     = 1.0e5
        upperBound     = 1.0e10
     END
     'FLUX':BEGIN
        pPref          = '-nFlux_fit'
        unitTitle      = "e!U-!N # flux"
        yTitle         = "Differential Number Flux!C(#/cm!U2!N-sr-s)"        
        lowerBound     = 1.0e1
        upperBound     = 1.0e7
     END
  ENDCASE
  ;;Need to know if OMNI2D is responsible for this, or something else
  pref                 = pPref + (KEYWORD_SET(using_sdt_data) ? '__SDT_data-' : '-')

  plotSN               = STRING(FORMAT='(A0,A0,A0,"-",A0,A0,"-orb_",I0,"__",A0,A0,A0)', $
                                strings.today, $
                                pref, $
                                strings.timeFNStrs[bounds_i], $
                                strings.eeb_or_ees, $
                                strings.avgStr, $
                                strings.orbStr, $
                                strings.orbDate[bounds_i], $
                                strings.angleStr, $
                                (KEYWORD_SET(clamped_temperature) ? '-clampT' : ''))
  IF N_ELEMENTS(add_angle_label) GT 0  THEN BEGIN
     ;; plotSN           += STRING(FORMAT='("-angle_",F0.1)',kappaFit.A[6])
     plotSN      += STRING(FORMAT='("-angle_",F0.1)',add_angle_label)
  ENDIF
  plotSN         += ( KEYWORD_SET(postscript) ? '.ps' : '.png' )

  title           = STRING(FORMAT='(A0,", (Orbit ",I0,", ",A0,")")', $
                           unitTitle, $
                           strings.orbStr, $
                           strings.orbDate[bounds_i])

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
  window          = WINDOW(DIMENSION=[800,600],TITLE=winTitle,BUFFER=buffer)
  windowArr       = N_ELEMENTS(windowArr) GT 0 ? [windowArr,window] : window

  plotArr         = MAKE_ARRAY(nPlots,/OBJ) 

  colorList       = LIST('RED','BLACK','BLUE','GRAY')

  ;;Silly string stuff
  xTitle          = "Energy (eV)"

  lineStyle       = ['','--','-.','-:']

  yRange          = [(MIN(orig.y[WHERE(orig.y GT 0)]) < $
                     MIN(kappaFit.yFull[WHERE(kappaFit.yFull GT 0)])) > lowerBound, $
                     (MAX(orig.y[WHERE(orig.y GT 0)]) > $
                     MAX(kappaFit.yFull[WHERE(kappaFit.yFull GT 0)])) < upperBound]

  iPlot           = 0
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
                                 ;; OVERPLOT=i GT 0, $
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
        iPlot++
     ENDIF
  ENDIF

  ;;OneCount curve
  IF N_ELEMENTS(oneCurve) GT 0 THEN BEGIN
     plotArr[iPlot]    = PLOT(oneCurve.x, $
                              oneCurve.y, $
                              NAME=oneCurve.name, $
                              THICK=2.2, $
                              LINESTYLE=lineStyle[3], $
                              COLOR=colorList[3], $
                              /OVERPLOT, $
                              CURRENT=window) 
     iPlot++
  ENDIF

  
  legend               = LEGEND(TARGET=plotArr[*],POSITION=[0.55,0.85],/NORMAL)

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
     fitTitle          = ["Bulk energy (eV)","Plasma temp. (eV)","Kappa","Density (cm^-3)"]
     fitInfoStr        = [STRING(FORMAT='(F-15.2)',kappaFit.A[0]), $
                          STRING(FORMAT='(F-15.2)',kappaFit.A[1]), $
                          STRING(FORMAT='(F-7.3)',kappaFit.A[2]), $
                          STRING(FORMAT='(F-8.4)',kappaFit.A[3])]
     IF KEYWORD_SET(add_angle_label) THEN BEGIN
        fitTitle = [fitTitle,"Angle (deg)"]
        ;; fitInfoStr = [fitInfoStr,STRING(FORMAT='(F-8.4)',kappaFit.A[6])]
        fitInfoStr = [fitInfoStr,STRING(FORMAT='(F-8.1)',add_angle_label)]
     ENDIF

     IF KEYWORD_SET(add_chi_value) THEN BEGIN
        fitTitle = [fitTitle,'chi^2']
        ;; fitInfoStr = [fitInfoStr,STRING(FORMAT='(F-8.4)',kappaFit.A[6])]
        ;; fitInfoStr = [fitInfoStr,STRING(FORMAT='(G-9.4)',add_chi_value)]
        ;; fitInfoStr = [fitInfoStr,STRING(FORMAT='(G-9.4)',kappaFit.chi2)]
        fitInfoStr = [fitInfoStr,STRING(FORMAT='(G-9.4)',kappaFit.chi2/(N_ELEMENTS(kappaFit.x-4)))]
     ENDIF

     fitParamsText     = TEXT(0.2,0.25, $
                              STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[0],fitInfoStr[0]) + '!C' + $
                              STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[1],fitInfoStr[1]) + '!C' + $
                              STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[2],fitInfoStr[2]) + '!C' + $
                              STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[3],fitInfoStr[3]) + '!C' + $
                              (KEYWORD_SET(add_angle_label) ? $
                               STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[4],fitInfoStr[4]) + '!C' : '') + $
                              (KEYWORD_SET(add_chi_value) ? $
                               STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[5],fitInfoStr[5]) + '!C' : '') + $
                              STRING(FORMAT='("Fit success",T20,": ",A0)',(kappaFit.fitStatus EQ 0 ? 'Y' : 'N')), $
                              FONT_SIZE=10, $
                              FONT_NAME='Courier', $
                              /NORMAL)

     IF KEYWORD_SET(add_gaussian_estimate) THEN BEGIN
        fitTitle       = ["Bulk energy (eV)","Plasma temp. (eV)","Kappa","Density (cm^-3)"]
        fitInfoStr     = [STRING(FORMAT='(F-15.2)',gaussFit.A[0]), $
                          STRING(FORMAT='(F-15.2)',gaussFit.A[1]), $
                          STRING(FORMAT='(F-7.3)',gaussFit.A[2]), $
                          STRING(FORMAT='(F-8.4)',gaussFit.A[3])]

        IF KEYWORD_SET(add_angle_label) THEN BEGIN
           fitTitle    = [fitTitle,"Angle (deg)"]
           ;; fitInfoStr  = [fitInfoStr,STRING(FORMAT='(F-8.4)',gaussFit.A[6])]
           fitInfoStr  = [fitInfoStr,STRING(FORMAT='(F-8.4)',add_angle_label)]
        ENDIF

        IF KEYWORD_SET(add_chi_value) THEN BEGIN
           fitTitle   = [fitTitle,'chi^2']
           ;; fitInfoStr = [fitInfoStr,STRING(FORMAT='(F-8.4)',kappaFit.A[6])]
           ;; fitInfoStr = [fitInfoStr,STRING(FORMAT='(G-9.4)',add_chi_value)]
           fitInfoStr = [fitInfoStr,STRING(FORMAT='(G-9.4)',gaussFit.chi2/(N_ELEMENTS(gaussFit.x-3)))]
        ENDIF

        fitParamsText  = TEXT(0.52,0.25, $
                              STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[0],fitInfoStr[0]) + '!C' + $
                              STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[1],fitInfoStr[1]) + '!C' + $
                              STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[2],fitInfoStr[2]) + '!C' + $
                              STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[3],fitInfoStr[3]) + '!C' + $
                              (KEYWORD_SET(add_angle_label) ? $
                               STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[4],fitInfoStr[4]) + '!C' : '') + $
                              (KEYWORD_SET(add_chi_value) ? $
                               STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[5],fitInfoStr[5]) + '!C' : '') + $
                              STRING(FORMAT='("GaussFit success",T20,": ",A0)',(gaussFit.fitStatus EQ 0 ? 'Y' : 'N')), $
                              FONT_SIZE=10, $
                              FONT_NAME='Courier', $
                              /NORMAL, $
                              FONT_COLOR=colorList[2])
     ENDIF

  ENDIF

  IF KEYWORD_SET(save_fitplots) THEN BEGIN
     IF N_ELEMENTS(plotDir) EQ 0 THEN BEGIN
        SET_PLOT_DIR,plotDir,/FOR_SDT,/ADD_TODAY,/VERBOSE
     ENDIF
     PRINT,'Saving plot to ' + plotSN + '...'
     window.Save,plotDir+plotSN
     window.Close
  ENDIF

END