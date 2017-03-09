;;09/05/16
FUNCTION PLOT_KAPPA_FIT2D_SUPPLEMENTS__TIME_SERIES,fit2D, $
   ADD_GAUSSFIT_SUPPLEMENTS=add_gaussFit, $
   GAUSS_FIT2D_STRUCT=fit2DG, $
   FITDENS=fitDens, $      
   ;; ERRMSG=errMsg, $       
   CHI2=chi2, $         
   NORMED_CHI2=normed_chi2, $
   CHI2_DIFF=chi2_diff, $
   ;; STATUS=status, $       
   NFEV=nfEv, $         
;; best_resid, $
   PFREE_INDEX=pFree_index, $  
;; best_fJac, $
   NPEGGED=nPegged, $      
   NFREE=nFree, $        
   DOF=dof, $          
   COVAR=covar, $        
   PERROR=pError, $       
   NITER=nIter, $        
   SUPPRESS_LINEPLOT=suppress_line, $
   SUPPRESS_SCATTERPLOT=suppress_scatter, $
   ;; SCATTERDATA=scatterData, $
   SCATTERSYMBOL=scatterSymbol, $
   SCATTERSYMCOLOR=scatterSymColor, $
   SCATTERSYMSIZE=scatterSymSize, $
   SCATTERSYMTRANSP=scatterSymTransp, $
   COLOR=color, $
   ADD_LEGEND=add_legend, $
   NXTICKS=nXTicks, $
   YLOG=yLog, $
   YRANGE=yRange, $
   XSHOWTEXT=xShowText, $
   XTHICK=xThick, $
   YTHICK=yThick, $
   XMINOR=xMinor, $
   LINESTYLE=lineStyle, $
   THICK=thick, $
   TITLE=title, $
   NAME=name, $
   NO_TIME_LABEL=no_time_label, $
   CURRENT=window, $
   LAYOUT=layout, $
   POSITION=position, $
   CLIP=clip, $
   MARGIN=margin, $
   BUFFER=buffer, $
   OVERPLOT=overplot, $
   SAVEPLOT=savePlot, $
   SPNAME=sPName, $
   PLOTNAMEPREF=pNamePref, $
   PLOTDIR=plotDir, $
   ORBIT=orbit, $
   CLOSE_WINDOW_AFTER_SAVE=close_window_after_save


  COMPILE_OPT IDL2,STRICTARRSUBS

  IF ~ISA(fit2D) THEN RETURN,-1

  IF ~ISA(window) THEN BEGIN
     window              = WINDOW(DIMENSIONS=[900,600])
  ENDIF

  nameArr                = ['Kappa','Gauss']
  colorArr               = ['black','red'] ;Kappa and gauss
  symbolArr              = ['*','+']

  orbStr                 = N_ELEMENTS(orbit) GT 0 ? 'orb_' + STRCOMPRESS(orbit,/REMOVE_ALL) : ''

  xTickFont_size         = 16
  xTickFont_style        =  1      
  yTickFont_size         = 16
  yTickFont_style        =  1      
  symSize                =  2.0            
  symThick               =  2.0           
  thick                  =  N_ELEMENTS(thick) GT 0 ? thick : 3.0
  xStyle                 = 1

  sigDig         = 1
  nPlots         = 0
  IF KEYWORD_SET(fitDens) THEN BEGIN
     nameAbbrev  = 'fitDens'
     yData       = fit2D.fitDens
     IF KEYWORD_SET(add_gauss) THEN BEGIN
        yData2     = fit2DG.fitDens
     ENDIF
     yTitle      = 'Fit Density (cm!U-3!N)'
     yRange      = [ROUND_NTH_SIG_DIGIT(MIN(fit2D.fitDens),sigDig,/FLOOR), $
                    ROUND_NTH_SIG_DIGIT(MAX(fit2D.fitDens),sigDig,/CEIL)]
     yLog        = KEYWORD_SET(log_plots)
     nPlots++
  ENDIF

  IF KEYWORD_SET(normed_chi2) THEN BEGIN
     nameAbbrev  = 'normed_chi2'
     yData       = fit2D.chi2/(fit2D.dof+fit2D.nFree)
     IF KEYWORD_SET(add_gauss) THEN BEGIN
        yData2     = fit2DG.chi2/(fit2D.dof+fit2D.nFree)
     ENDIF
     yTitle      = '$\Chi$!U2!N/nFitPoints'
     yRange      = [ROUND_NTH_SIG_DIGIT(MIN(fit2D.normed_chi),sigDig,/FLOOR), $
                    ROUND_NTH_SIG_DIGIT(MAX(fit2D.normed_chi),sigDig,/CEIL)]
     yLog        = KEYWORD_SET(log_plots)
     nPlots++
  ENDIF

  IF KEYWORD_SET(chi2_diff) THEN BEGIN
     nameAbbrev  = 'chi2_diff'
     IF N_ELEMENTS(fit2DG) EQ 0 THEN BEGIN
        PRINT,"Need both kappa and gauss fits for this!!"
        ;; RETURN,-1
        STOP
     ENDIF
     yData       = fit2DG.chi2/(fit2DG.dof-fit2DG.npegged)-$
                            fit2D.chi2/(fit2D.dof-fit2D.npegged)
     ;; IF KEYWORD_SET(add_gauss) THEN BEGIN
     ;;    yData2     = fit2DG.normed_chi
     ;; ENDIF
     yTitle      = '$\Delta \Chi$!U2!N/DOF (GaussFit - kappaFit)'
     yRange      = [ROUND_NTH_SIG_DIGIT(MIN(fit2D.normed_chi),sigDig,/FLOOR), $
                    ROUND_NTH_SIG_DIGIT(MAX(fit2D.normed_chi),sigDig,/CEIL)]
     yLog        = KEYWORD_SET(log_plots)
     nPlots++
  ENDIF

  IF KEYWORD_SET(chi2) THEN BEGIN
     nameAbbrev  = 'chi2'
     yData       = fit2D.chi2
     IF KEYWORD_SET(add_gauss) THEN BEGIN
        yData2     = fit2DG.chi2
     ENDIF
     yTitle      = '$\Chi$!U2!N'
     yRange      = [ROUND_NTH_SIG_DIGIT(MIN(fit2D.chi2),sigDig,/FLOOR), $
                    ROUND_NTH_SIG_DIGIT(MAX(fit2D.chi2),sigDig,/CEIL)]
     yLog        = KEYWORD_SET(log_plots)
     nPlots++
  ENDIF
  
  ;; IF KEYWORD_SET(errMsg) THEN BEGIN
  ;;    nameAbbrev  = 'errMsg'
  ;;    yData       = fit2D.errMsg
  ;;    IF KEYWORD_SET(add_gauss) THEN BEGIN
  ;;       yData2     = fit2DG.errMsg
  ;;    ENDIF
  ;;    yTitle      = 'errMsg'
  ;;    yRange      = [ROUND_NTH_SIG_DIGIT(MIN(fit2D.errMsg),sigDig,/FLOOR), $
  ;;                   ROUND_NTH_SIG_DIGIT(MAX(fit2D.errMsg),sigDig,/CEIL)]
  ;;    yLog        = KEYWORD_SET(log_plots)
  ;;    nPlots++
  ;; ENDIF
  
  IF KEYWORD_SET(status) THEN BEGIN
     nameAbbrev  = 'status'
     yData       = fit2D.status
     IF KEYWORD_SET(add_gauss) THEN BEGIN
        yData2     = fit2DG.status
     ENDIF
     yTitle      = 'MPFIT2DFUN Fit Status'
     yRange      = [ROUND_NTH_SIG_DIGIT(MIN(fit2D.status),sigDig,/FLOOR), $
                    ROUND_NTH_SIG_DIGIT(MAX(fit2D.status),sigDig,/CEIL)]
     yLog        = KEYWORD_SET(log_plots)
     nPlots++
  ENDIF
  
  IF KEYWORD_SET(nfEv) THEN BEGIN
     nameAbbrev  = 'nfEv'
     yData       = fit2D.nfEv
     IF KEYWORD_SET(add_gauss) THEN BEGIN
        yData2     = fit2DG.nfEv
     ENDIF
     yTitle      = 'nfEv'
     yRange      = [ROUND_NTH_SIG_DIGIT(MIN(fit2D.nfEv),sigDig,/FLOOR), $
                    ROUND_NTH_SIG_DIGIT(MAX(fit2D.nfEv),sigDig,/CEIL)]
     yLog        = KEYWORD_SET(log_plots)
     nPlots++
  ENDIF
  
  ;; IF KEYWORD_SET(pFree_index) THEN BEGIN
  ;;    nameAbbrev  = 'pFree_index'
  ;;    yData       = fit2D.pFree_index
  ;;    IF KEYWORD_SET(add_gauss) THEN BEGIN
  ;;       yData2     = fit2DG.pFree_index
  ;;    ENDIF
  ;;    yTitle      = 'pFree_index'
  ;;    yRange      = [ROUND_NTH_SIG_DIGIT(MIN(fit2D.pFree_index),sigDig,/FLOOR), $
  ;;                   ROUND_NTH_SIG_DIGIT(MAX(fit2D.pFree_index),sigDig,/CEIL)]
  ;;    yLog        = KEYWORD_SET(log_plots)
  ;;    nPlots++
  ;; ENDIF

  IF KEYWORD_SET(nPegged) THEN BEGIN
     nameAbbrev  = 'nPegged'
     yData       = fit2D.nPegged
     IF KEYWORD_SET(add_gauss) THEN BEGIN
        yData2     = fit2DG.nPegged
     ENDIF
     yTitle      = 'N Pegged Parameters'
     yRange      = [ROUND_NTH_SIG_DIGIT(MIN(fit2D.nPegged),sigDig,/FLOOR), $
                    ROUND_NTH_SIG_DIGIT(MAX(fit2D.nPegged),sigDig,/CEIL)]
     yLog        = KEYWORD_SET(log_plots)
     nPlots++
  ENDIF
  
  IF KEYWORD_SET(nFree) THEN BEGIN
     nameAbbrev  = 'nFree'
     yData       = fit2D.nFree
     IF KEYWORD_SET(add_gauss) THEN BEGIN
        yData2     = fit2DG.nFree
     ENDIF
     yTitle      = 'N Free Parameters'
     yRange      = [ROUND_NTH_SIG_DIGIT(MIN(fit2D.nFree),sigDig,/FLOOR), $
                    ROUND_NTH_SIG_DIGIT(MAX(fit2D.nFree),sigDig,/CEIL)]
     yLog        = KEYWORD_SET(log_plots)
     nPlots++
  ENDIF
  
  IF KEYWORD_SET(dof) THEN BEGIN
     nameAbbrev  = 'dof'
     yData       = fit2D.dof
     IF KEYWORD_SET(add_gauss) THEN BEGIN
        yData2     = fit2DG.dof
     ENDIF
     yTitle      = 'Degrees of Freedom'
     yRange      = [ROUND_NTH_SIG_DIGIT(MIN(fit2D.dof),sigDig,/FLOOR), $
                    ROUND_NTH_SIG_DIGIT(MAX(fit2D.dof),sigDig,/CEIL)]
     yLog        = KEYWORD_SET(log_plots)
     nPlots++
  ENDIF
  
  ;; IF KEYWORD_SET(covar) THEN BEGIN
  ;;    nameAbbrev  = 'covar'
  ;;    yData       = fit2D.covar
  ;;    IF KEYWORD_SET(add_gauss) THEN BEGIN
  ;;       yData2     = fit2DG.covar
  ;;    ENDIF
  ;;    yTitle      = 'covar'
  ;;    yRange      = [ROUND_NTH_SIG_DIGIT(MIN(fit2D.covar),sigDig,/FLOOR), $
  ;;                   ROUND_NTH_SIG_DIGIT(MAX(fit2D.covar),sigDig,/CEIL)]
  ;;    yLog        = KEYWORD_SET(log_plots)
  ;;    nPlots++
  ;; ENDIF
  
  ;; IF KEYWORD_SET(pError) THEN BEGIN
  ;;    nameAbbrev  = 'pError'
  ;;    yData       = fit2D.pError
  ;;    IF KEYWORD_SET(add_gauss) THEN BEGIN
  ;;       yData2     = fit2DG.pError
  ;;    ENDIF
  ;;    yTitle      = 'pError'
  ;;    yRange      = [ROUND_NTH_SIG_DIGIT(MIN(fit2D.pError),sigDig,/FLOOR), $
  ;;                   ROUND_NTH_SIG_DIGIT(MAX(fit2D.pError),sigDig,/CEIL)]
  ;;    yLog        = 0
  ;;    nPlots++
  ;; ENDIF
  
  IF KEYWORD_SET(nIter) THEN BEGIN
     nameAbbrev  = 'nIter'
     yData       = fit2D.nIter
     IF KEYWORD_SET(add_gauss) THEN BEGIN
        yData2     = fit2DG.nIter
     ENDIF
     yTitle      = 'Fit # of Iterations'
     yRange      = [ROUND_NTH_SIG_DIGIT(MIN(fit2D.nIter),sigDig,/FLOOR), $
                    ROUND_NTH_SIG_DIGIT(MAX(fit2D.nIter),sigDig,/CEIL)]
     yLog        = 0
     nPlots++
  ENDIF
  
  ;; nPlots          = (~KEYWORD_SET(suppress_line) + (~KEYWORD_SET(suppress_scatter))) * 
  nPlots          = nPlots * ( (~KEYWORD_SET(suppress_line) + $
                                (~KEYWORD_SET(suppress_scatter) ) ) ) $
                           * ( 1 + KEYWORD_SET(add_gauss) )

  plotArr         = MAKE_ARRAY(nPlots,/OBJ)

  IF N_ELEMENTS(scatterSymbol) EQ 0 THEN scatterSymbol = '+'

  ;; IF ~KEYWORD_SET(overplot) THEN BEGIN
  ;;    yRange          = KEYWORD_SET(yRange) ? $
  ;;                      yRange : [MIN(BPD.data[0,*]),-15]
  ;; ENDIF

  CASE 1 OF
     KEYWORD_SET(no_time_label): BEGIN
        x_values        = INDGEN(N_ELEMENTS(fit2D.SDT.time))
     END
     ELSE: BEGIN
        ;;For time axis
        ;; dummy           = LABEL_DATE(DATE_FORMAT=['%D-%M','%Y'])
        dummy           = LABEL_DATE(DATE_FORMAT=['%H:%I:%S'])
        x_values        = UTC_TO_JULDAY(fit2D.SDT.time)
        ;; xRange          = [MIN(x_values),MAX(x_values)]
        xRange          = [x_values[1]-(1/20864.),x_values[-1]+(1/20864.)]
     END
  ENDCASE
  ;; xTitle                = 'Time since ' + TIME_TO_STR(fit2D.SDT[0].time,/MSEC)
  xTitle                = 'Time since ' + TIME_TO_STR(fit2D.SDT[1].time-1,/MSEC)


  iParam                = 0
  FOR iPlot=(0-KEYWORD_SET(suppress_line)),nPlots-1 DO BEGIN


     IF ~KEYWORD_SET(suppress_line) THEN BEGIN
        plotArr[iPlot]      = PLOT(x_values, $
                                   yData, $
                                   XRANGE=xRange, $
                                   YRANGE=yRange, $
                                   YLOG=yLog[pIndex[iParam]], $
                                   NAME=nameArr[0], $
                                   XTITLE=xTitle, $
                                   YTITLE=yTitle, $
                                   XMAJOR=nXTicks, $
                                   XMINOR=xMinor, $
                                   XTICKVALUES=xTickValues, $
                                   XTICKFORMAT=KEYWORD_SET(no_time_label) ? $
                                   !NULL : 'LABEL_DATE', $
                                   XTICKUNITS=KEYWORD_SET(no_time_label) ? $
                                   !NULL : 'Time', $
                                   XTICKFONT_SIZE=xTickFont_size, $
                                   XTICKFONT_STYLE=xTickFont_style, $
                                   YTICKFONT_SIZE=yTickFont_size, $
                                   YTICKFONT_STYLE=yTickFont_style, $
                                   XSHOWTEXT=xShowText, $
                                   XTHICK=xThick, $
                                   YTHICK=yThick, $
                                   COLOR=color, $
                                   XSTYLE=xStyle, $
                                   LINESTYLE=lineStyle, $
                                   THICK=thick, $
                                   FILL_COLOR=fill_color, $
                                   LAYOUT=layout, $
                                   POSITION=position, $
                                   MARGIN=margin, $
                                   CLIP=clip, $
                                   BUFFER=buffer, $
                                   OVERPLOT=overplot, $
                                   CURRENT=window)

        IF KEYWORD_SET(add_gauss) THEN BEGIN
           iPlot++
           plotArr[iPlot]      = PLOT(x_values, $
                                      yData2, $
                                      XRANGE=xRange, $
                                      YRANGE=yRange, $
                                      YLOG=yLog, $
                                      NAME=nameArr[1], $
                                      XTITLE=xTitle, $
                                      YTITLE=yTitle, $
                                      XMAJOR=nXTicks, $
                                      XMINOR=xMinor, $
                                      XTICKVALUES=xTickValues, $
                                      XTICKFORMAT=KEYWORD_SET(no_time_label) ? $
                                      !NULL : 'LABEL_DATE', $
                                      XTICKUNITS=KEYWORD_SET(no_time_label) ? $
                                      !NULL : 'Time', $
                                      XTICKFONT_SIZE=xTickFont_size, $
                                      XTICKFONT_STYLE=xTickFont_style, $
                                      YTICKFONT_SIZE=yTickFont_size, $
                                      YTICKFONT_STYLE=yTickFont_style, $
                                      XSHOWTEXT=xShowText, $
                                      XTHICK=xThick, $
                                      YTHICK=yThick, $
                                      COLOR=colorArr[1], $
                                      XSTYLE=xStyle, $
                                      LINESTYLE=lineStyle, $
                                      THICK=thick, $
                                      FILL_COLOR=fill_color, $
                                      LAYOUT=layout, $
                                      POSITION=position, $
                                      MARGIN=margin, $
                                      CLIP=clip, $
                                      BUFFER=buffer, $
                                      OVERPLOT=overplot, $
                                      CURRENT=window)
        ENDIF

     ENDIF
     
     
     IF ~KEYWORD_SET(suppress_scatter) THEN BEGIN
        iPlot++

        plotArr[iPlot] = PLOT(x_values, $
                              yData, $
                              XRANGE=xRange, $
                              YRANGE=yRange, $
                              YLOG=yLog, $
                              NAME=nameArr[0], $
                              XTITLE=xTitle, $
                              YTITLE=yTitle, $
                              XMAJOR=nXTicks, $
                              SYMBOL=symbolArr[0], $
                              SYM_COLOR=colorArr[0], $
                              SYM_SIZE=scatterSymSize, $
                              SYM_TRANSPARENCY=scatterSymTransp, $
                              SYM_THICK=1.6, $
                              XTICKVALUES=xTickValues, $
                              XTICKFORMAT=KEYWORD_SET(no_time_label) ? $
                              !NULL : 'LABEL_DATE', $
                              XTICKUNITS=KEYWORD_SET(no_time_label) ? $
                              !NULL : 'Time', $
                              XTICKFONT_SIZE=xTickFont_size, $
                              XTICKFONT_STYLE=xTickFont_style, $
                              YTICKFONT_SIZE=yTickFont_size, $
                              YTICKFONT_STYLE=yTickFont_style, $
                              XSHOWTEXT=xShowText, $
                              XTHICK=xThick, $
                              YTHICK=yThick, $
                              XMINOR=xMinor, $
                              XSTYLE=xStyle, $
                              LINESTYLE='', $
                              LAYOUT=layout, $
                              POSITION=position, $
                              MARGIN=margin, $
                              CLIP=clip, $
                              OVERPLOT=KEYWORD_SET(overplot) ? $
                              1 : ~KEYWORD_SET(suppress_line), $
                              CURRENT=window)

        IF KEYWORD_SET(add_gauss) THEN BEGIN
           iPlot++
           plotArr[iPlot] = PLOT(x_values, $
                                 yData, $
                                 XRANGE=xRange, $
                                 YRANGE=yRange, $
                                 YLOG=yLog, $
                                 NAME=nameArr[1], $
                                 XTITLE=xTitle, $
                                 YTITLE=yTitle, $
                                 XMAJOR=nXTicks, $
                                 SYMBOL=symbolArr[1], $
                                 SYM_COLOR=colorArr[1], $
                                 SYM_SIZE=scatterSymSize, $
                                 SYM_TRANSPARENCY=scatterSymTransp, $
                                 SYM_THICK=1.6, $
                                 XTICKVALUES=xTickValues, $
                                 XTICKFORMAT=KEYWORD_SET(no_time_label) ? $
                                 !NULL : 'LABEL_DATE', $
                                 XTICKUNITS=KEYWORD_SET(no_time_label) ? $
                                 !NULL : 'Time', $
                                 XTICKFONT_SIZE=xTickFont_size, $
                                 XTICKFONT_STYLE=xTickFont_style, $
                                 YTICKFONT_SIZE=yTickFont_size, $
                                 YTICKFONT_STYLE=yTickFont_style, $
                                 XSHOWTEXT=xShowText, $
                                 XTHICK=xThick, $
                                 YTHICK=yThick, $
                                 XMINOR=xMinor, $
                                 XSTYLE=xStyle, $
                                 LINESTYLE='', $
                                 LAYOUT=layout, $
                                 POSITION=position, $
                                 MARGIN=margin, $
                                 CLIP=clip, $
                                 OVERPLOT=KEYWORD_SET(overplot) ? $
                                 1 : ~KEYWORD_SET(suppress_line), $
                                 CURRENT=window)

        ENDIF

     ENDIF

     iParam++
  ENDFOR

  IF KEYWORD_SET(savePlot) THEN BEGIN

     pNameSuff    = ''
     ;; FOR i=0,N_ELEMENTS(plots)-1 DO BEGIN
     pNameSuff += (KEYWORD_SET(plots[0]) ? nameAbbrev[0] + '--' : '' )
     ;; ENDFOR

     IF KEYWORD_SET(pNamePref) THEN BEGIN
        pNameSuff = pNamePref +  '--' + pnameSuff
     ENDIF

     ;; IF KEYWORD_SET(pNamePref) THEN BEGIN
     ;;    pNameSuff += '--' + pNamePref
     ;; ENDIF

     IF KEYWORD_SET(spName) THEN outName = spName ELSE BEGIN
        outName = GET_TODAY_STRING() + '--' + orbStr + '--' + pNameSuff + 'time_series.png'
     ENDELSE
     IF N_ELEMENTS(plotDir) GT 0 THEN BEGIN
        pDir = plotDir
     ENDIF ELSE BEGIN
        SET_PLOT_DIR,pDir,/ADD_TODAY,/FOR_KAPPA_DB
     ENDELSE

     PRINT,'Saving to ' + outName + '...'
     window.save,pDir+outName

     IF KEYWORD_SET(close_window_after_save) THEN BEGIN
        window.close
        window      = !NULL
     ENDIF

  ENDIF

  RETURN,plotArr
END
