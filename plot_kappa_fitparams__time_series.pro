;;09/05/16
FUNCTION PLOT_KAPPA_FITPARAMS__TIME_SERIES,fit2D, $
   ADD_GAUSSFIT=add_gaussFit, $
   GAUSS_FIT2D_STRUCT=fit2DG, $
   BULK_ENERGY=bulk_energy, $
   TEMPERATURE=temperature, $
   DENSITY=density, $
   ;; DENS_2D=dens_2d, $
   KAPPA=kappa, $
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
   SUPPRESS_LEGEND=suppress_legend, $
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
   PLOTNAMEPREF=plotNamePref, $
   PLOTDIR=plotDir, $
   ORBIT=orbit, $
   CLOSE_WINDOW_AFTER_SAVE=close_window_after_save

  COMPILE_OPT IDL2

  IF ~ISA(fit2D) THEN RETURN,-1

  IF ~ISA(window) THEN BEGIN
     window              = WINDOW(DIMENSIONS=[900,600])
  ENDIF

  orbStr                 = N_ELEMENTS(orbit) GT 0 ? 'orb_' + STRCOMPRESS(orbit,/REMOVE_ALL) : ''

  xTickFont_size         = 16
  xTickFont_style        =  1      
  yTickFont_size         = 16
  yTickFont_style        =  1      
  symSize                =  2.0            
  symThick               =  2.0           
  thick                  =  N_ELEMENTS(thick) GT 0 ? thick : 3.0
  xStyle                 = 1

  nameArr                = ['Kappa','Gauss']
  colorArr               = ['black','red'] ;Kappa and gauss
  symbolArr              = ['*','+']

  sigDig         = 1
  nPlots         = 0

  ;;For alt plots

  nameAbbrevList = LIST()
  yDataList      = LIST()
  yData2List     = LIST()
  yRangeList     = LIST()
  yTitleList     = LIST()
  yLogList       = LIST()
  skipGaussList  = LIST()
  IF KEYWORD_SET(fitDens) THEN BEGIN
     nameAbbrevList.Add, 'fitDens'
     yDataList.Add, fit2D.fitDens
     skipGaussList.Add,0
     IF KEYWORD_SET(add_gaussFit) THEN BEGIN
        yData2List.Add, fit2DG.fitDens
     ENDIF
     yTitleList.Add, 'Fit Density (cm!U-3!N)'
     yRangeList.Add, [ROUND_NTH_SIG_DIGIT(MIN(fit2D.fitDens),sigDig,/FLOOR), $
                    ROUND_NTH_SIG_DIGIT(MAX(fit2D.fitDens),sigDig,/CEIL)]
     yLogList.Add, KEYWORD_SET(log_plots)
     nPlots++
  ENDIF

  IF KEYWORD_SET(normed_chi2) THEN BEGIN
     nameAbbrevList.Add, 'normed_chi2'
     yDataList.Add, fit2D.chi2/(fit2D.dof+fit2D.nFree)
     skipGaussList.Add,0
     IF KEYWORD_SET(add_gaussFit) THEN BEGIN
        yData2List.Add, fit2DG.chi2/(fit2D.dof+fit2D.nFree)
     ENDIF
     yTitleList.Add, '$\Chi$!U2!N/nFitPoints'
     yRangeList.Add, [ROUND_NTH_SIG_DIGIT(MIN(yDataList[-1]),sigDig,/FLOOR), $
                    ROUND_NTH_SIG_DIGIT(MAX(yDataList[-1]),sigDig,/CEIL)]
     yLogList.Add, KEYWORD_SET(log_plots)
     nPlots++
  ENDIF

  IF KEYWORD_SET(chi2_diff) THEN BEGIN
     nameAbbrevList.Add, 'chi2_diff'
     IF N_ELEMENTS(fit2DG) EQ 0 THEN BEGIN
        PRINT,"Need both kappa and gauss fits for this!!"
        ;; RETURN,-1
        STOP
     ENDIF
     yDataList.Add, TOTAL(fit2DG.chi2/(fit2DG.dof-fit2DG.npegged)-$
                          fit2D.chi2/(fit2D.dof-fit2D.npegged), $
                          /CUMULATIVE)
     skipGaussList.Add,1
     ;; IF KEYWORD_SET(add_gaussFit) THEN BEGIN
     ;;    yData2List.Add, fit2DG.normed_chi
     ;; ENDIF
     yTitleList.Add, 'SUM($\Delta \Chi$!U2!N/DOF (GaussFit - kappaFit))'
     yRangeList.Add, [ROUND_NTH_SIG_DIGIT(MIN(yDataList[-1]),sigDig,/FLOOR), $
                    ROUND_NTH_SIG_DIGIT(MAX(yDataList[-1]),sigDig,/CEIL)]
     yLogList.Add, KEYWORD_SET(log_plots)
     nPlots++
  ENDIF

  IF KEYWORD_SET(chi2) THEN BEGIN
     nameAbbrevList.Add, 'chi2'
     yDataList.Add, fit2D.chi2
     skipGaussList.Add,0
     IF KEYWORD_SET(add_gaussFit) THEN BEGIN
        yData2List.Add, fit2DG.chi2
     ENDIF
     yTitleList.Add, '$\Chi$!U2!N'
     yRangeList.Add, [ROUND_NTH_SIG_DIGIT(MIN(fit2D.chi2),sigDig,/FLOOR), $
                    ROUND_NTH_SIG_DIGIT(MAX(fit2D.chi2),sigDig,/CEIL)]
     yLogList.Add, KEYWORD_SET(log_plots)
     nPlots++
  ENDIF
  
  ;; IF KEYWORD_SET(errMsg) THEN BEGIN
  ;;    nameAbbrevList.Add, 'errMsg'
  ;;    yDataList.Add, fit2D.errMsg
  ;;    IF KEYWORD_SET(add_gaussFit) THEN BEGIN
  ;;       yData2List.Add, fit2DG.errMsg
  ;;    ENDIF
  ;;    yTitleList.Add, 'errMsg'
  ;;    yRangeList.Add, [ROUND_NTH_SIG_DIGIT(MIN(fit2D.errMsg),sigDig,/FLOOR), $
  ;;                   ROUND_NTH_SIG_DIGIT(MAX(fit2D.errMsg),sigDig,/CEIL)]
  ;;    yLogList.Add, KEYWORD_SET(log_plots)
  ;;    nPlots++
  ;; ENDIF
  
  IF KEYWORD_SET(status) THEN BEGIN
     nameAbbrevList.Add, 'status'
     yDataList.Add, fit2D.status
     skipGaussList.Add,0
     IF KEYWORD_SET(add_gaussFit) THEN BEGIN
        yData2List.Add, fit2DG.status
     ENDIF
     yTitleList.Add, 'MPFIT2DFUN Fit Status'
     yRangeList.Add, [ROUND_NTH_SIG_DIGIT(MIN(fit2D.status),sigDig,/FLOOR), $
                    ROUND_NTH_SIG_DIGIT(MAX(fit2D.status),sigDig,/CEIL)]
     yLogList.Add, KEYWORD_SET(log_plots)
     nPlots++
  ENDIF
  
  IF KEYWORD_SET(nfEv) THEN BEGIN
     nameAbbrevList.Add, 'nfEv'
     yDataList.Add, fit2D.nfEv
     skipGaussList.Add,0
     IF KEYWORD_SET(add_gaussFit) THEN BEGIN
        yData2List.Add, fit2DG.nfEv
     ENDIF
     yTitleList.Add, 'nfEv'
     yRangeList.Add, [ROUND_NTH_SIG_DIGIT(MIN(fit2D.nfEv),sigDig,/FLOOR), $
                    ROUND_NTH_SIG_DIGIT(MAX(fit2D.nfEv),sigDig,/CEIL)]
     yLogList.Add, KEYWORD_SET(log_plots)
     nPlots++
  ENDIF
  
  ;; IF KEYWORD_SET(pFree_index) THEN BEGIN
  ;;    nameAbbrevList.Add, 'pFree_index'
  ;;    yDataList.Add, fit2D.pFree_index
  ;;    IF KEYWORD_SET(add_gaussFit) THEN BEGIN
  ;;       yData2List.Add, fit2DG.pFree_index
  ;;    ENDIF
  ;;    yTitleList.Add, 'pFree_index'
  ;;    yRangeList.Add, [ROUND_NTH_SIG_DIGIT(MIN(fit2D.pFree_index),sigDig,/FLOOR), $
  ;;                   ROUND_NTH_SIG_DIGIT(MAX(fit2D.pFree_index),sigDig,/CEIL)]
  ;;    yLogList.Add, KEYWORD_SET(log_plots)
  ;;    nPlots++
  ;; ENDIF

  IF KEYWORD_SET(nPegged) THEN BEGIN
     nameAbbrevList.Add, 'nPegged'
     yDataList.Add, fit2D.nPegged
     skipGaussList.Add,0
     IF KEYWORD_SET(add_gaussFit) THEN BEGIN
        yData2List.Add, fit2DG.nPegged
     ENDIF
     yTitleList.Add, 'N Pegged Parameters'
     yRangeList.Add, [ROUND_NTH_SIG_DIGIT(MIN(fit2D.nPegged),sigDig,/FLOOR), $
                    ROUND_NTH_SIG_DIGIT(MAX(fit2D.nPegged),sigDig,/CEIL)]
     yLogList.Add, KEYWORD_SET(log_plots)
     nPlots++
  ENDIF
  
  IF KEYWORD_SET(nFree) THEN BEGIN
     nameAbbrevList.Add, 'nFree'
     yDataList.Add, fit2D.nFree
     skipGaussList.Add,0
     IF KEYWORD_SET(add_gaussFit) THEN BEGIN
        yData2List.Add, fit2DG.nFree
     ENDIF
     yTitleList.Add, 'N Free Parameters'
     yRangeList.Add, [ROUND_NTH_SIG_DIGIT(MIN(fit2D.nFree),sigDig,/FLOOR), $
                    ROUND_NTH_SIG_DIGIT(MAX(fit2D.nFree),sigDig,/CEIL)]
     yLogList.Add, KEYWORD_SET(log_plots)
     nPlots++
  ENDIF
  
  IF KEYWORD_SET(dof) THEN BEGIN
     nameAbbrevList.Add, 'dof'
     yDataList.Add, fit2D.dof
     skipGaussList.Add,0
     IF KEYWORD_SET(add_gaussFit) THEN BEGIN
        yData2List.Add, fit2DG.dof
     ENDIF
     yTitleList.Add, 'Degrees of Freedom'
     yRangeList.Add, [ROUND_NTH_SIG_DIGIT(MIN(fit2D.dof),sigDig,/FLOOR), $
                    ROUND_NTH_SIG_DIGIT(MAX(fit2D.dof),sigDig,/CEIL)]
     yLogList.Add, KEYWORD_SET(log_plots)
     nPlots++
  ENDIF
  
  ;; IF KEYWORD_SET(covar) THEN BEGIN
  ;;    nameAbbrevList.Add, 'covar'
  ;;    yDataList.Add, fit2D.covar
  ;;    IF KEYWORD_SET(add_gaussFit) THEN BEGIN
  ;;       yData2List.Add, fit2DG.covar
  ;;    ENDIF
  ;;    yTitleList.Add, 'covar'
  ;;    yRangeList.Add, [ROUND_NTH_SIG_DIGIT(MIN(fit2D.covar),sigDig,/FLOOR), $
  ;;                   ROUND_NTH_SIG_DIGIT(MAX(fit2D.covar),sigDig,/CEIL)]
  ;;    yLogList.Add, KEYWORD_SET(log_plots)
  ;;    nPlots++
  ;; ENDIF
  
  ;; IF KEYWORD_SET(pError) THEN BEGIN
  ;;    nameAbbrevList.Add, 'pError'
  ;;    yDataList.Add, fit2D.pError
  ;;    IF KEYWORD_SET(add_gaussFit) THEN BEGIN
  ;;       yData2List.Add, fit2DG.pError
  ;;    ENDIF
  ;;    yTitleList.Add, 'pError'
  ;;    yRangeList.Add, [ROUND_NTH_SIG_DIGIT(MIN(fit2D.pError),sigDig,/FLOOR), $
  ;;                   ROUND_NTH_SIG_DIGIT(MAX(fit2D.pError),sigDig,/CEIL)]
  ;;    yLogList.Add, 0
  ;;    nPlots++
  ;; ENDIF
  
  IF KEYWORD_SET(nIter) THEN BEGIN
     nameAbbrevList.Add, 'nIter'
     yDataList.Add, fit2D.nIter
     skipGaussList.Add,0
     IF KEYWORD_SET(add_gaussFit) THEN BEGIN
        yData2List.Add, fit2DG.nIter
     ENDIF
     yTitleList.Add, 'Fit # of Iterations'
     yRangeList.Add, [ROUND_NTH_SIG_DIGIT(MIN(fit2D.nIter),sigDig,/FLOOR), $
                    ROUND_NTH_SIG_DIGIT(MAX(fit2D.nIter),sigDig,/CEIL)]
     yLogList.Add, 0
     nPlots++
  ENDIF

  IF KEYWORD_SET(bulk_energy) THEN BEGIN
     yDataList.Add,fit2D.fitParams[0,*]
     skipGaussList.Add,0
     IF KEYWORD_SET(add_gaussFit) THEN BEGIN
        yData2List.Add, fit2DG.fitParams[0,*]
     ENDIF
     yRangeList.Add,[MIN(fit2D.fitParams[0,*]),MAX(fit2D.fitParams[0,*])]
     nameAbbrevList.Add,'bulk_e'
     yTitleList.Add,"Bulk Energy (eV)"
     yLogList.Add,1
     nPlots++
  ENDIF

  IF KEYWORD_SET(temperature) THEN BEGIN
     yDataList.Add,fit2D.fitParams[1,*]
     skipGaussList.Add,0
     IF KEYWORD_SET(add_gaussFit) THEN BEGIN
        yData2List.Add, fit2DG.fitParams[1,*]
     ENDIF
     yRangeList.Add,[MIN(fit2D.fitParams[1,*]),MAX(fit2D.fitParams[1,*])]
     nameAbbrevList.Add,'temp'
     yTitleList.Add,"Temperature (eV)"
     yLogList.Add,1
     nPlots++
  ENDIF
  
  IF KEYWORD_SET(kappa) THEN BEGIN
     yDataList.Add,fit2D.fitParams[2,*]
     skipGaussList.Add,0
     IF KEYWORD_SET(add_gaussFit) THEN BEGIN
        yData2List.Add, fit2DG.fitParams[2,*]
     ENDIF
     ;; yRange = [MIN(fit2D.fitParams[2,*]),MAX(fit2D.fitParams[2,*])], $
     yRangeList.Add,[1,102]
     nameAbbrevList.Add,'kappa'
     yTitleList.Add,"Kappa"
     yLogList.Add,1
     nPlots++
  ENDIF

  IF KEYWORD_SET(density) THEN BEGIN
     yDataList.Add,fit2D.fitParams[3,*]
     skipGaussList.Add,0
     IF KEYWORD_SET(add_gaussFit) THEN BEGIN
        yData2List.Add, fit2DG.fitParams[3,*]
     ENDIF
     yRangeList.Add,[MIN(fit2D.fitParams[3,*]),MAX(fit2D.fitParams[3,*])]
     nameAbbrevList.Add,'density'
     yTitleList.Add,"Field-Aligned Density (cm!U-3!N)"
     yLogList.Add,1
     nPlots++
  ENDIF

  nPlots          = (~KEYWORD_SET(suppress_line) + (~KEYWORD_SET(suppress_scatter))) $
                    * nPlots * (1 + KEYWORD_SET(add_gaussFit))
  plotArr         = MAKE_ARRAY(nPlots,/OBJ)

  IF N_ELEMENTS(scatterSymbol) EQ 0 THEN scatterSymbol = '+'

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

  xTitle          = 'Time since ' + TIME_TO_STR(fit2D.SDT[1].time-1,/MSEC)

  iParam                = 0

  IF KEYWORD_SET(savePlot) THEN BEGIN
     IF N_ELEMENTS(plotDir) GT 0 THEN BEGIN
        pDir = plotDir
     ENDIF ELSE BEGIN
        SET_PLOT_DIR,pDir,/ADD_TODAY,/FOR_KAPPA_DB
     ENDELSE
  ENDIF

  FOR iPlot=0,nPlots-1 DO BEGIN

     ;;We open a new one each time we're doing a new plot
     IF ~ISA(window) THEN BEGIN
        window              = WINDOW(DIMENSIONS=[900,600])
     ENDIF


     PRINT,'Doing ' + nameAbbrevList[(iPlot EQ -1) ? 0 : iPlot] + ' ...'

     IF ~KEYWORD_SET(suppress_line) THEN BEGIN
        plotArr[iPlot]      = PLOT(x_values, $
                                   ;; fit2D.fitParams[pIndex[iParam],*], $
                                   yDataList[iPlot], $
                                   XRANGE=xRange, $
                                   YRANGE=yRangeList[iParam], $
                                   YLOG=yLogList[iParam], $
                                   NAME=nameArr[0], $
                                   XTITLE=xTitle, $
                                   YTITLE=yTitleList[iParam], $
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
                                   COLOR=colorArr[0], $
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

        IF KEYWORD_SET(add_gaussFit) AND ~skipGaussList[iParam] THEN BEGIN
           iPlot++
           plotArr[iPlot]      = PLOT(x_values, $
                                      yData2List[iParam], $
                                      XRANGE=xRange, $
                                      YRANGE=yRangeList[iParam], $
                                      YLOG=yLogList[iParam], $
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
                                      /OVERPLOT, $
                                      CURRENT=window)
        ENDIF
     ENDIF ELSE BEGIN
        IF iPlot EQ 0 THEN iPlot = -1
     ENDELSE
     
     
     IF ~KEYWORD_SET(suppress_scatter) THEN BEGIN
        iPlot++

        plotArr[iPlot] = PLOT(x_values, $
                              yDataList[iParam], $
                              XRANGE=xRange, $
                              YRANGE=yRangeList[iParam], $
                              YLOG=yLogList[iParam], $
                              NAME=nameArr[0], $
                              XTITLE=xTitle, $
                              YTITLE=yTitleList[iParam], $
                              XMAJOR=nXTicks, $
                              ;; SYMBOL=scatterSymbol, $
                              ;; SYM_COLOR=scatterSymColor, $
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


        IF KEYWORD_SET(add_gaussFit) AND ~skipGaussList[iParam] THEN BEGIN
           iPlot++
           plotArr[iPlot] = PLOT(x_values, $
                                 yData2List[iParam], $
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
                                 /OVERPLOT, $
                                 CURRENT=window)

        ENDIF
     ENDIF

     IF KEYWORD_SET(add_gaussFit) AND ~KEYWORD_SET(suppress_legend) THEN BEGIN
        legend    = LEGEND(TARGET=plotArr[*], $
                           /NORMAL, $
                           POSITION=[0.35,0.8])

     ENDIF

     IF KEYWORD_SET(savePlot) THEN BEGIN

        pNameSuff =  nameAbbrevList[iParam]

        IF KEYWORD_SET(plotNamePref) THEN BEGIN
           pNameSuff = plotNamePref + '--' + pNameSuff
        ENDIF

        IF KEYWORD_SET(spName) THEN outName = spName ELSE BEGIN
           outName = GET_TODAY_STRING() + '--' + orbStr + pNameSuff + '--time_series.png'
        ENDELSE


        PRINT,'Saving to ' + outName + '...'
        window.save,pDir+outName

        IF KEYWORD_SET(close_window_after_save) THEN BEGIN
           window.close
           window      = !NULL
        ENDIF

     ENDIF

     iParam++
  ENDFOR

  ;; IF KEYWORD_SET(savePlot) THEN BEGIN

  ;;    pNameSuff    = ''
  ;;    FOR i=0,N_ELEMENTS(plots)-1 DO BEGIN
  ;;       pNameSuff += (KEYWORD_SET(plots[i]) ? nameAbbrev[i] + '--' : '' )
  ;;    ENDFOR

  ;;    IF KEYWORD_SET(plotNamePref) THEN BEGIN
  ;;       pNameSuff = plotNamePref + pNameSuff
  ;;    ENDIF

  ;;    IF KEYWORD_SET(spName) THEN outName = spName ELSE BEGIN
  ;;       outName = GET_TODAY_STRING() + '--' + orbStr + pNameSuff + 'time_series.png'
  ;;    ENDELSE
  ;;    IF N_ELEMENTS(plotDir) GT 0 THEN BEGIN
  ;;       pDir = plotDir
  ;;    ENDIF ELSE BEGIN
  ;;       SET_PLOT_DIR,pDir,/ADD_TODAY,/FOR_KAPPA_DB
  ;;    ENDELSE

  ;;    PRINT,'Saving to ' + outName + '...'
  ;;    window.save,pDir+outName

  ;;    IF KEYWORD_SET(close_window_after_save) THEN BEGIN
  ;;       window.close
  ;;       window      = !NULL
  ;;    ENDIF

  ;; ENDIF

  RETURN,plotArr
END
