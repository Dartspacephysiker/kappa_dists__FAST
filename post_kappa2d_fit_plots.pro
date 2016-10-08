PRO POST_KAPPA2D_FIT_PLOTS,fit2DK,fit2DG,orbit,plotNamePref,plotDir,save_kappa_plot

  COMPILE_OPT idl2

     pap    = PLOT_KAPPA_FITPARAMS__TIME_SERIES(fit2DK, $
                                                /KAPPA, $
                                                ORBIT=orbit, $
                                                PLOTNAMEPREF=plotNamePref, $
                                                PLOTDIR=plotDir, $
                                                SAVEPLOT=save_kappa_plot, $
                                                /SUPPRESS_LINEPLOT)
     
     pap    = PLOT_KAPPA_FITPARAMS__TIME_SERIES(fit2DK, $
                                                ;; /ADD_GAUSSFIT, $
                                                GAUSS_FIT2D_STRUCT=fit2DG, $
                                                /CHI2_CUMULATIVE_DIFF, $
                                                ORBIT=orbit, $
                                                PLOTNAMEPREF=plotNamePref, $
                                                PLOTDIR=plotDir, $
                                                SAVEPLOT=save_kappa_plot, $
                                                /SUPPRESS_LINEPLOT)
     
     pap    = PLOT_KAPPA_FITPARAMS__TIME_SERIES(fit2DK, $
                                                ;; /ADD_GAUSSFIT, $
                                                GAUSS_FIT2D_STRUCT=fit2DG, $
                                                /CHI2_DIFF, $
                                                ORBIT=orbit, $
                                                PLOTNAMEPREF=plotNamePref, $
                                                PLOTDIR=plotDir, $
                                                SAVEPLOT=save_kappa_plot, $
                                                /SUPPRESS_LINEPLOT)
     
     pap    = PLOT_KAPPA_FITPARAMS__TIME_SERIES(fit2DK, $
                                                /ADD_GAUSSFIT, $
                                                GAUSS_FIT2D_STRUCT=fit2DG, $
                                                /FITDENS, $
                                                /TEMPERATURE, $
                                                /PLOT_KAPPA_GAUSS_DIFF__LINE, $
                                                /SMOOTHED_DIFFPLOT, $
                                                ORBIT=orbit, $
                                                PLOTNAMEPREF=plotNamePref, $
                                                PLOTDIR=plotDir, $
                                                SAVEPLOT=save_kappa_plot, $
                                                /SUPPRESS_LINEPLOT)

END
;;  $
;; ADD_GAUSSFIT=add_gaussFit, $
;; GAUSS_FIT2D_STRUCT=fit2DG, $
;; BULK_ENERGY=bulk_energy, $
;; TEMPERATURE=temperature, $
;; DENSITY=density, $
;; ;; DENS_2D=dens_2d, $
;; KAPPA=kappa, $
;; FITDENS=fitDens, $      
;; ERRMSG=errMsg, $       
;; CHI2=chi2, $         
;; NORMED_CHI2=normed_chi2, $
;; CHI2_CUMULATIVE_DIFF=chi2_cumDiff, $
;; CHI2_DIFF=chi2_diff, $         
;; STATUS=status, $       
;; NFEV=nfEv, $         
;; BEST_RESID=best_resid, $
;; PFREE_INDEX=pFree_index, $  
;; BEST_FJAC=best_fJac, $
;; NPEGGED=nPegged, $      
;; NFREE=nFree, $        
;; DOF=dof, $          
;; COVAR=covar, $        
;; PERROR=pError, $       
;; NITER=nIter, $        
;; PLOT_KAPPA_GAUSS_DIFF__SCATTER=plot_kappa_gauss_diff__scatter, $
;; PLOT_KAPPA_GAUSS_DIFF__LINE=plot_kappa_gauss_diff__line, $
;; SMOOTHED_DIFFPLOT=smoothed_diffPlot, $
;; SUPPRESS_LINEPLOT=suppress_line, $
;; SUPPRESS_SCATTERPLOT=suppress_scatter, $
;; SUPPRESS_LEGEND=suppress_legend, $
;; ;; SCATTERDATA=scatterData, $
;; SCATTERSYMBOL=scatterSymbol, $
;; SCATTERSYMCOLOR=scatterSymColor, $
;; SCATTERSYMSIZE=scatterSymSize, $
;; SCATTERSYMTRANSP=scatterSymTransp, $
;; COLOR=color, $
;; ADD_LEGEND=add_legend, $
;; NXTICKS=nXTicks, $
;; YLOG=yLog, $
;; YRANGE=yRange, $
;; XSHOWTEXT=xShowText, $
;; XTHICK=xThick, $
;; YTHICK=yThick, $
;; XMINOR=xMinor, $
;; LINESTYLE=lineStyle, $
;; THICK=thick, $
;; TITLE=title, $
;; NAME=name, $
;; NO_TIME_LABEL=no_time_label, $
;; CURRENT=window, $
;; LAYOUT=layout, $
;; POSITION=position, $
;; CLIP=clip, $
;; MARGIN=margin, $
;; BUFFER=buffer, $
;; OVERPLOT=overplot, $
;; SAVEPLOT=savePlot, $
;; SPNAME=sPName, $
;; PLOTNAMEPREF=plotNamePref, $
;; PLOTDIR=plotDir, $
;; ORBIT=orbit, $
;; CLOSE_WINDOW_AFTER_SAVE=close_window_after_save
