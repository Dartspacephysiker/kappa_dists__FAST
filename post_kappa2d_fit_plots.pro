PRO POST_KAPPA2D_FIT_PLOTS,fit2DK,fit2DG,orbit,plotNamePref,plotDir, $
                           save_kappa_plot, $
                           CLOSE_KAPPAPLOTS_AFTER_SAVE=close_kp_after_save


  COMPILE_OPT idl2

     pap    = PLOT_KAPPA_FITPARAMS__TIME_SERIES(fit2DK, $
                                                /KAPPA, $
                                                ORBIT=orbit, $
                                                PLOTNAMEPREF=plotNamePref, $
                                                PLOTDIR=plotDir, $
                                                SAVEPLOT=save_kappa_plot, $
                                                CLOSE_WINDOW_AFTER_SAVE=close_kp_after_save, $
                                                /SUPPRESS_LINEPLOT)
     
     pap    = PLOT_KAPPA_FITPARAMS__TIME_SERIES(fit2DK, $
                                                ;; /ADD_GAUSSFIT, $
                                                GAUSS_FIT2D_STRUCT=fit2DG, $
                                                /CHI2_CUMULATIVE_DIFF, $
                                                ORBIT=orbit, $
                                                PLOTNAMEPREF=plotNamePref, $
                                                PLOTDIR=plotDir, $
                                                SAVEPLOT=save_kappa_plot, $
                                                CLOSE_WINDOW_AFTER_SAVE=close_kp_after_save, $
                                                /SUPPRESS_LINEPLOT)
     
     pap    = PLOT_KAPPA_FITPARAMS__TIME_SERIES(fit2DK, $
                                                ;; /ADD_GAUSSFIT, $
                                                GAUSS_FIT2D_STRUCT=fit2DG, $
                                                /CHI2_DIFF, $
                                                ORBIT=orbit, $
                                                PLOTNAMEPREF=plotNamePref, $
                                                PLOTDIR=plotDir, $
                                                SAVEPLOT=save_kappa_plot, $
                                                CLOSE_WINDOW_AFTER_SAVE=close_kp_after_save, $
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
                                                CLOSE_WINDOW_AFTER_SAVE=close_kp_after_save, $
                                                /SUPPRESS_LINEPLOT)

END
