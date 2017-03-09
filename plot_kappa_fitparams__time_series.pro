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
   CHI2_CUMULATIVE_DIFF=chi2_cumDiff, $
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
   PLOT_KAPPA_GAUSS_DIFF__SCATTER=plot_kappa_gauss_diff__scatter, $
   PLOT_KAPPA_GAUSS_DIFF__LINE=plot_kappa_gauss_diff__line, $
   SMOOTHED_DIFFPLOT=smoothed_diffPlot, $
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

  COMPILE_OPT IDL2,STRICTARRSUBS

  IF KEYWORD_SET(suppress_line) AND KEYWORD_SET(suppress_scatter) THEN BEGIN
     PRINT,"Well, you've disabled everything. Great job."
     RETURN,-1
  ENDIF

  plot_kappa_gauss_diff = KEYWORD_SET(plot_kappa_gauss_diff__scatter) + $
                          KEYWORD_SET(plot_kappa_gauss_diff__line)


  IF KEYWORD_SET(plot_kappa_gauss_diff) OR KEYWORD_SET(add_gaussFit) THEN BEGIN
     IF SIZE(fit2DG,/TYPE) NE 8 THEN BEGIN
        PRINT,"Can't read Gaussian fit struct!"

        IF KEYWORD_SET(plot_kappa_gauss_diff) THEN BEGIN
           PRINT,'Disabling PLOT_KAPPA_GAUSS_DIFF ...'
           plot_kappa_gauss_diff          = 0
           plot_kappa_gauss_diff__scatter = 0
           plot_kappa_gauss_diff__line    = 0
        ENDIF
        IF KEYWORD_SET(add_gaussFit) THEN BEGIN
           PRINT,'Disabling ADD_GAUSSFIT ...'
           add_gaussFit                   = 0
        ENDIF
     ENDIF
  ENDIF

  IF ~ISA(fit2D) THEN RETURN,-1

  ;; IF ~ISA(window) THEN BEGIN
  ;;    window              = WINDOW(DIMENSIONS=[900,600])
  ;; ENDIF

  orbStr                 = N_ELEMENTS(orbit) GT 0 ? 'orb_' + STRCOMPRESS(orbit,/REMOVE_ALL) : ''

  xTickFont_size         = 16
  xTickFont_style        =  1      
  yTickFont_size         = 16
  yTickFont_style        =  1      
  symSize                =  2.0            
  symThick               =  2.0           
  thick                  =  N_ELEMENTS(thick) GT 0 ? thick : 3.0
  xStyle                 = 1

  CASE 1 OF
     KEYWORD_SET(margin): 
     KEYWORD_SET(plot_kappa_gauss_diff): BEGIN
        margin = 0.1
     END
     ELSE: margin = !NULL
  ENDCASE

  nameArr                = ['Kappa','Gauss','Difference']
  colorArr               = ['black','red','green'] ;Kappa and gauss
  symbolArr              = ['*','+','x']

  sigDig         = 1
  nPlots         = 0

  IF KEYWORD_SET(smoothed_diffPlot) THEN BEGIN
     nameArr[2] += ' (smooth)'
  ENDIF


  ;;For alt plots

  nameAbbrevList = LIST()
  yDataList      = LIST()
  yData2List     = LIST() ;For Gauss plots
  yData3List     = LIST() ;For diff plots
  yRangeList     = LIST()
  yRange3List    = LIST()
  yTitleList     = LIST()
  yLogList       = LIST()
  skipGaussList  = LIST()
  IF KEYWORD_SET(fitDens) THEN BEGIN
     PRINT,'Plotting fitDens ...'

     nameAbbrevList.Add,'fitDens'
     yDataList.Add,fit2D.fitDens
     skipGaussList.Add,0
     IF KEYWORD_SET(add_gaussFit) THEN BEGIN
        yData2List.Add,fit2DG.fitDens
     ENDIF
     yTitleList.Add,'Fit Density (cm!U-3!N)'
     yRangeList.Add,[ROUND_NTH_SIG_DIGIT(MIN(fit2D.fitDens),sigDig,/FLOOR), $
                    ROUND_NTH_SIG_DIGIT(MAX(fit2D.fitDens),sigDig,/CEIL)]
     yLogList.Add,KEYWORD_SET(log_plots)
     nPlots++
  ENDIF

  IF KEYWORD_SET(normed_chi2) THEN BEGIN
     PRINT,'Plotting normed_chi2 ...'

     nameAbbrevList.Add,'normed_chi2'
     yDataList.Add,fit2D.chi2/(fit2D.dof+fit2D.nFree)
     skipGaussList.Add,0
     IF KEYWORD_SET(add_gaussFit) THEN BEGIN
        yData2List.Add,fit2DG.chi2/(fit2D.dof+fit2D.nFree)
     ENDIF
     yTitleList.Add,'$\Chi$!U2!N/nFitPoints'
     yRangeList.Add,[ROUND_NTH_SIG_DIGIT(MIN(yDataList[-1]),sigDig,/FLOOR), $
                    ROUND_NTH_SIG_DIGIT(MAX(yDataList[-1]),sigDig,/CEIL)]
     yLogList.Add,KEYWORD_SET(log_plots)
     nPlots++
  ENDIF

  IF KEYWORD_SET(chi2_cumDiff) THEN BEGIN
     PRINT,'Plotting chi2_cumDiff ...'

     nameAbbrevList.Add,'chi2_cumDiff'
     IF N_ELEMENTS(fit2DG) EQ 0 THEN BEGIN
        PRINT,"Need both kappa and gauss fits for this!!"
        ;; RETURN,-1
        STOP
     ENDIF
     yDataList.Add,TOTAL(fit2DG.chi2/(fit2DG.dof-fit2DG.npegged)-$
                          fit2D.chi2/(fit2D.dof-fit2D.npegged), $
                          /CUMULATIVE)
     skipGaussList.Add,1
     ;; IF KEYWORD_SET(add_gaussFit) THEN BEGIN
     ;;    yData2List.Add,fit2DG.normed_chi
     ;; ENDIF
     yTitleList.Add,'SUM($\Delta \Chi$!U2!N/DOF (GaussFit - kappaFit))'
     yRangeList.Add,[ROUND_NTH_SIG_DIGIT(MIN(yDataList[-1]),sigDig,/FLOOR), $
                    ROUND_NTH_SIG_DIGIT(MAX(yDataList[-1]),sigDig,/CEIL)]
     yLogList.Add,KEYWORD_SET(log_plots)
     nPlots++
  ENDIF

  IF KEYWORD_SET(chi2_diff) THEN BEGIN
     PRINT,'Plotting chi2_diff ...'

     nameAbbrevList.Add,'chi2_diff'
     IF N_ELEMENTS(fit2DG) EQ 0 THEN BEGIN
        PRINT,"Need both kappa and gauss fits for this!!"
        ;; RETURN,-1
        STOP
     ENDIF
     yDataList.Add,fit2DG.chi2/(fit2DG.dof-fit2DG.npegged)-$
                         fit2D.chi2/(fit2D.dof-fit2D.npegged)
     skipGaussList.Add,1
     ;; IF KEYWORD_SET(add_gaussFit) THEN BEGIN
     ;;    yData2List.Add,fit2DG.normed_chi
     ;; ENDIF
     yTitleList.Add,'$\Delta \Chi$!U2!N/DOF (GaussFit - kappaFit)'
     yRangeList.Add,[ROUND_NTH_SIG_DIGIT(MIN(yDataList[-1]),sigDig,/FLOOR), $
                     ROUND_NTH_SIG_DIGIT(MAX(yDataList[-1]),sigDig,/CEIL)]
     yLogList.Add,KEYWORD_SET(log_plots)
     nPlots++
  ENDIF

  IF KEYWORD_SET(chi2) THEN BEGIN
     PRINT,'Plotting chi2 ...'

     nameAbbrevList.Add,'chi2'
     yDataList.Add,fit2D.chi2
     skipGaussList.Add,0
     IF KEYWORD_SET(add_gaussFit) THEN BEGIN
        yData2List.Add,fit2DG.chi2
     ENDIF
     yTitleList.Add,'$\Chi$!U2!N'
     yRangeList.Add,[ROUND_NTH_SIG_DIGIT(MIN(fit2D.chi2),sigDig,/FLOOR), $
                    ROUND_NTH_SIG_DIGIT(MAX(fit2D.chi2),sigDig,/CEIL)]
     yLogList.Add,KEYWORD_SET(log_plots)
     nPlots++
  ENDIF
  
  ;; IF KEYWORD_SET(errMsg) THEN BEGIN
  ;;    nameAbbrevList.Add,'errMsg'
  ;;    yDataList.Add,fit2D.errMsg
  ;;    IF KEYWORD_SET(add_gaussFit) THEN BEGIN
  ;;       yData2List.Add,fit2DG.errMsg
  ;;    ENDIF
  ;;    yTitleList.Add,'errMsg'
  ;;    yRangeList.Add,[ROUND_NTH_SIG_DIGIT(MIN(fit2D.errMsg),sigDig,/FLOOR), $
  ;;                   ROUND_NTH_SIG_DIGIT(MAX(fit2D.errMsg),sigDig,/CEIL)]
  ;;    yLogList.Add,KEYWORD_SET(log_plots)
  ;;    nPlots++
  ;; ENDIF
  
  IF KEYWORD_SET(status) THEN BEGIN
     PRINT,'Plotting status ...'

     nameAbbrevList.Add,'status'
     yDataList.Add,fit2D.status
     skipGaussList.Add,0
     IF KEYWORD_SET(add_gaussFit) THEN BEGIN
        yData2List.Add,fit2DG.status
     ENDIF
     yTitleList.Add,'MPFIT2DFUN Fit Status'
     yRangeList.Add,[ROUND_NTH_SIG_DIGIT(MIN(fit2D.status),sigDig,/FLOOR), $
                    ROUND_NTH_SIG_DIGIT(MAX(fit2D.status),sigDig,/CEIL)]
     yLogList.Add,KEYWORD_SET(log_plots)
     nPlots++
  ENDIF
  
  IF KEYWORD_SET(nfEv) THEN BEGIN
     PRINT,'Plotting nfEv ...'

     nameAbbrevList.Add,'nfEv'
     yDataList.Add,fit2D.nfEv
     skipGaussList.Add,0
     IF KEYWORD_SET(add_gaussFit) THEN BEGIN
        yData2List.Add,fit2DG.nfEv
     ENDIF
     yTitleList.Add,'nfEv'
     yRangeList.Add,[ROUND_NTH_SIG_DIGIT(MIN(fit2D.nfEv),sigDig,/FLOOR), $
                    ROUND_NTH_SIG_DIGIT(MAX(fit2D.nfEv),sigDig,/CEIL)]
     yLogList.Add,KEYWORD_SET(log_plots)
     nPlots++
  ENDIF
  
  ;; IF KEYWORD_SET(pFree_index) THEN BEGIN
  ;;    nameAbbrevList.Add,'pFree_index'
  ;;    yDataList.Add,fit2D.pFree_index
  ;;    IF KEYWORD_SET(add_gaussFit) THEN BEGIN
  ;;       yData2List.Add,fit2DG.pFree_index
  ;;    ENDIF
  ;;    yTitleList.Add,'pFree_index'
  ;;    yRangeList.Add,[ROUND_NTH_SIG_DIGIT(MIN(fit2D.pFree_index),sigDig,/FLOOR), $
  ;;                   ROUND_NTH_SIG_DIGIT(MAX(fit2D.pFree_index),sigDig,/CEIL)]
  ;;    yLogList.Add,KEYWORD_SET(log_plots)
  ;;    nPlots++
  ;; ENDIF

  IF KEYWORD_SET(nPegged) THEN BEGIN
     PRINT,'Plotting nPegged ...'

     nameAbbrevList.Add,'nPegged'
     yDataList.Add,fit2D.nPegged
     skipGaussList.Add,0
     IF KEYWORD_SET(add_gaussFit) THEN BEGIN
        yData2List.Add,fit2DG.nPegged
     ENDIF
     yTitleList.Add,'N Pegged Parameters'
     yRangeList.Add,[ROUND_NTH_SIG_DIGIT(MIN(fit2D.nPegged),sigDig,/FLOOR), $
                    ROUND_NTH_SIG_DIGIT(MAX(fit2D.nPegged),sigDig,/CEIL)]
     yLogList.Add,KEYWORD_SET(log_plots)
     nPlots++
  ENDIF
  
  IF KEYWORD_SET(nFree) THEN BEGIN
     PRINT,'Plotting nFree ...'

     nameAbbrevList.Add,'nFree'
     yDataList.Add,fit2D.nFree
     skipGaussList.Add,0
     IF KEYWORD_SET(add_gaussFit) THEN BEGIN
        yData2List.Add,fit2DG.nFree
     ENDIF
     yTitleList.Add,'N Free Parameters'
     yRangeList.Add,[ROUND_NTH_SIG_DIGIT(MIN(fit2D.nFree),sigDig,/FLOOR), $
                    ROUND_NTH_SIG_DIGIT(MAX(fit2D.nFree),sigDig,/CEIL)]
     yLogList.Add,KEYWORD_SET(log_plots)
     nPlots++
  ENDIF
  
  IF KEYWORD_SET(dof) THEN BEGIN
     PRINT,'Plotting dof ...'

     nameAbbrevList.Add,'dof'
     yDataList.Add,fit2D.dof
     skipGaussList.Add,0
     IF KEYWORD_SET(add_gaussFit) THEN BEGIN
        yData2List.Add,fit2DG.dof
     ENDIF
     yTitleList.Add,'Degrees of Freedom'
     yRangeList.Add,[ROUND_NTH_SIG_DIGIT(MIN(fit2D.dof),sigDig,/FLOOR), $
                    ROUND_NTH_SIG_DIGIT(MAX(fit2D.dof),sigDig,/CEIL)]
     yLogList.Add,KEYWORD_SET(log_plots)
     nPlots++
  ENDIF
  
  ;; IF KEYWORD_SET(covar) THEN BEGIN
  ;;    nameAbbrevList.Add,'covar'
  ;;    yDataList.Add,fit2D.covar
  ;;    IF KEYWORD_SET(add_gaussFit) THEN BEGIN
  ;;       yData2List.Add,fit2DG.covar
  ;;    ENDIF
  ;;    yTitleList.Add,'covar'
  ;;    yRangeList.Add,[ROUND_NTH_SIG_DIGIT(MIN(fit2D.covar),sigDig,/FLOOR), $
  ;;                   ROUND_NTH_SIG_DIGIT(MAX(fit2D.covar),sigDig,/CEIL)]
  ;;    yLogList.Add,KEYWORD_SET(log_plots)
  ;;    nPlots++
  ;; ENDIF
  
  ;; IF KEYWORD_SET(pError) THEN BEGIN
  ;;    nameAbbrevList.Add,'pError'
  ;;    yDataList.Add,fit2D.pError
  ;;    IF KEYWORD_SET(add_gaussFit) THEN BEGIN
  ;;       yData2List.Add,fit2DG.pError
  ;;    ENDIF
  ;;    yTitleList.Add,'pError'
  ;;    yRangeList.Add,[ROUND_NTH_SIG_DIGIT(MIN(fit2D.pError),sigDig,/FLOOR), $
  ;;                   ROUND_NTH_SIG_DIGIT(MAX(fit2D.pError),sigDig,/CEIL)]
  ;;    yLogList.Add,0
  ;;    nPlots++
  ;; ENDIF
  
  IF KEYWORD_SET(nIter) THEN BEGIN
     PRINT,'Plotting nIter ...'

     nameAbbrevList.Add,'nIter'
     yDataList.Add,fit2D.nIter
     skipGaussList.Add,0
     IF KEYWORD_SET(add_gaussFit) THEN BEGIN
        yData2List.Add,fit2DG.nIter
     ENDIF
     yTitleList.Add,'Fit # of Iterations'
     yRangeList.Add,[ROUND_NTH_SIG_DIGIT(MIN(fit2D.nIter),sigDig,/FLOOR), $
                    ROUND_NTH_SIG_DIGIT(MAX(fit2D.nIter),sigDig,/CEIL)]
     yLogList.Add,0
     nPlots++
  ENDIF

  IF KEYWORD_SET(bulk_energy) THEN BEGIN
     PRINT,'Plotting bulk_e ...'

     yDataList.Add,fit2D.fitParams[0,*]
     skipGaussList.Add,0
     IF KEYWORD_SET(add_gaussFit) THEN BEGIN
        yData2List.Add,fit2DG.fitParams[0,*]
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
        yData2List.Add,fit2DG.fitParams[1,*]
     ENDIF
     yRangeList.Add,[MIN(fit2D.fitParams[1,*]),MAX(fit2D.fitParams[1,*])]
     nameAbbrevList.Add,'temp'
     yTitleList.Add,"Temperature (eV)"
     yLogList.Add,1
     nPlots++
  ENDIF
  
  IF KEYWORD_SET(kappa) THEN BEGIN
     PRINT,'Plotting kappa ...'

     yDataList.Add,fit2D.fitParams[2,*]
     skipGaussList.Add,0
     IF KEYWORD_SET(add_gaussFit) THEN BEGIN
        yData2List.Add,fit2DG.fitParams[2,*]
     ENDIF
     ;; yRange = [MIN(fit2D.fitParams[2,*]),MAX(fit2D.fitParams[2,*])], $
     yRangeList.Add,[1,102]
     nameAbbrevList.Add,'kappa'
     yTitleList.Add,"Kappa"
     yLogList.Add,1
     nPlots++
  ENDIF

  IF KEYWORD_SET(density) THEN BEGIN
     PRINT,'Plotting density ...'

     yDataList.Add,fit2D.fitParams[3,*]
     skipGaussList.Add,0
     IF KEYWORD_SET(add_gaussFit) THEN BEGIN
        yData2List.Add,fit2DG.fitParams[3,*]
     ENDIF
     yRangeList.Add,[MIN(fit2D.fitParams[3,*]),MAX(fit2D.fitParams[3,*])]
     nameAbbrevList.Add,'density'
     yTitleList.Add,"Field-Aligned Density (cm!U-3!N)"
     yLogList.Add,1
     nPlots++
  ENDIF

  IF KEYWORD_SET(plot_kappa_gauss_diff) THEN BEGIN
     PRINT,'Calculating differences between kappa and Gauss quantities ...'
     FOR k=0,N_ELEMENTS(yDataList)-1 DO BEGIN
        yData3List.Add,yDataList[k]-yData2List[k]

        IF KEYWORD_SET(smoothed_diffPlot) THEN BEGIN
           yData3List[k] = SMOOTH(yData3List[k],9)
        ENDIF

        yRange3List.Add,[MIN(yData3List[k]),MAX(yData3List[k])]

     ENDFOR
  ENDIF

  nParams         = nPlots

  nPlots          = (~KEYWORD_SET(suppress_line) + (~KEYWORD_SET(suppress_scatter))) $
                    * nPlots * (1 + KEYWORD_SET(add_gaussFit) ) + $
                    nPlots * (KEYWORD_SET(plot_kappa_gauss_diff__scatter) + $
                              KEYWORD_SET(plot_kappa_gauss_diff__line))
  plotArr         = MAKE_ARRAY(nPlots,/OBJ)
  windowArr       = MAKE_ARRAY(nParams,/OBJ)

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
     ;; IF ~ISA(window) THEN BEGIN
     ;;    window              = WINDOW(DIMENSIONS=[900,600])
     ;; ENDIF
     windowArr[iParam] = WINDOW(DIMENSIONS=[900,600])

     PRINT,'Doing ' + nameAbbrevList[(iPlot EQ -1) ? 0 : iParam] + ' ...'

     thisBatch = !NULL ;keep track of inds used

     IF ~KEYWORD_SET(suppress_line) THEN BEGIN
        plotArr[iPlot] = PLOT(x_values, $
                              ;; fit2D.fitParams[pIndex[iParam],*], $
                              yDataList[iParam], $
                              XRANGE=xRange, $
                              YRANGE=yRangeList[iParam], $
                              YLOG=yLogList[iParam], $
                              NAME=nameArr[0], $
                              XTITLE=xTitle, $
                              YTITLE=yTitleList[iParam], $
                              AXIS_STYLE=KEYWORD_SET(plot_kappa_gauss_diff) ? 1 : !NULL, $
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
                              CURRENT=windowArr[iParam])

        thisBatch = [thisBatch,iPlot]
        iPlot++
        IF KEYWORD_SET(add_gaussFit) AND ~skipGaussList[iParam] THEN BEGIN
           plotArr[iPlot] = PLOT(x_values, $
                                 yData2List[iParam], $
                                 XRANGE=xRange, $
                                 YRANGE=yRangeList[iParam], $
                                 YLOG=yLogList[iParam], $
                                 NAME=nameArr[1], $
                                 XTITLE=xTitle, $
                                 YTITLE=yTitle, $
                                 AXIS_STYLE=KEYWORD_SET(plot_kappa_gauss_diff) ? 1 : !NULL, $
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
                                 CURRENT=windowArr[iParam])
           thisBatch = [thisBatch,iPlot]
           iPlot++
        ENDIF

     ENDIF;;  ELSE BEGIN
     ;;    IF iPlot EQ 0 THEN iPlot = -1
     ;; ENDELSE
     
     IF KEYWORD_SET(plot_kappa_gauss_diff__line) THEN BEGIN
        diffPlot_i = iPlot
        plotArr[iPlot] = PLOT(x_values, $
                              ;; fit2D.fitParams[pIndex[iParam],*], $
                              yData3List[iParam], $
                              XRANGE=xRange, $
                              YRANGE=yRange3List[iParam], $
                              ;; YLOG=yLogList[iParam], $
                              NAME=nameArr[2], $
                              XTITLE=xTitle, $
                              YTITLE=yTitleList[iParam], $
                              AXIS_STYLE=0, $
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
                              COLOR=colorArr[2], $
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
                              CURRENT=windowArr[iParam])
        ;; thisBatch = [thisBatch,iPlot]
        iPlot++
     ENDIF
     
     IF ~KEYWORD_SET(suppress_scatter) THEN BEGIN
        plotArr[iPlot] = PLOT(x_values, $
                              yDataList[iParam], $
                              XRANGE=xRange, $
                              YRANGE=yRangeList[iParam], $
                              YLOG=yLogList[iParam], $
                              NAME=nameArr[0], $
                              XTITLE=xTitle, $
                              YTITLE=yTitleList[iParam], $
                              AXIS_STYLE=KEYWORD_SET(plot_kappa_gauss_diff) ? 1 : !NULL, $
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
                              CURRENT=windowArr[iParam])
        thisBatch = [thisBatch,iPlot]
        iPlot++

        IF KEYWORD_SET(add_gaussFit) AND ~skipGaussList[iParam] THEN BEGIN
           plotArr[iPlot] = PLOT(x_values, $
                                 yData2List[iParam], $
                                 XRANGE=xRange, $
                                 YRANGE=yRange, $
                                 YLOG=yLogList[iParam], $
                                 NAME=nameArr[1], $
                                 XTITLE=xTitle, $
                                 YTITLE=yTitle, $
                                 AXIS_STYLE=KEYWORD_SET(plot_kappa_gauss_diff) ? 1 : !NULL, $
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
                                 CURRENT=windowArr[iParam])

           thisBatch = [thisBatch,iPlot]
           iPlot++
        ENDIF
     ENDIF

     IF KEYWORD_SET(plot_kappa_gauss_diff__scatter) THEN BEGIN
        diffPlot_i = iPlot
        plotArr[iPlot] = PLOT(x_values, $
                              yData3List[iParam], $
                              XRANGE=xRange, $
                              YRANGE=yRange3List[iParam], $
                              ;; YLOG=yLogList[iParam], $
                              NAME=nameArr[2], $
                              XTITLE=xTitle, $
                              YTITLE=yTitleList[iParam], $
                              AXIS_STYLE=0, $
                              XMAJOR=nXTicks, $
                              ;; SYMBOL=scatterSymbol, $
                              ;; SYM_COLOR=scatterSymColor, $
                              SYMBOL=symbolArr[2], $
                              SYM_COLOR=colorArr[2], $
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
                              CURRENT=windowArr[iParam])
        ;; thisBatch = [thisBatch,iPlot]
        iPlot++
     ENDIF

     IF KEYWORD_SET(plot_kappa_gauss_diff) THEN BEGIN
        yaxis = AXIS('Y', LOCATION='right', TARGET=plotArr[diffPlot_i], $
                     ;; TITLE=yTitle, $
                     ;; MAJOR=nMajorTicks+1, $
                     ;; MINOR=nMinorTicks, $
                     TICKFONT_SIZE=yTickFont_size, $
                     TICKFONT_STYLE=defHistoYtickfontstyle, $
                     ;; TICKFORMAT=KEYWORD_SET(yTickFormat) ? $
                     ;;            yTickFormat : defHistoTickFormat, $
                     TICKFORMAT=!NULL, $
                     TEXTPOS=1, $
                     ;; COLOR=KEYWORD_SET(color) ? color : defHistoColor)
                     COLOR=colorArr[2])
     ENDIF

     IF (KEYWORD_SET(add_gaussFit) OR KEYWORD_SET(plot_kappa_gauss_diff)) $
         AND ~KEYWORD_SET(suppress_legend) THEN BEGIN

        ;;Place difference plot at end of list
        IF KEYWORD_SET(plot_kappa_gauss_diff) THEN BEGIN
           thisBatch = [thisBatch,diffPlot_i]
        ENDIF

        legend    = LEGEND(TARGET=plotArr[thisBatch], $
                           /NORMAL, $
                           POSITION=[0.55,0.85])

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
        windowArr[iParam].save,pDir+outName

        IF KEYWORD_SET(close_window_after_save) THEN BEGIN
           windowArr[iParam].close
           ;; windowArr[iParam]      = !NULL
        ENDIF

     ENDIF

     iParam++
     iPlot--
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
