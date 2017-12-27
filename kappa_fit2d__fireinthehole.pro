;2017/12/27
PRO KAPPA_FIT2D__FIREINTHEHOLE,curDataStr, $
                               hadSuccessK, $
                               hadSuccessG, $
                               CURKAPPASTR=curKappaStr, $
                               CURGAUSSSTR=curGaussStr, $
                               ;; KAPPAFITS=kappaFits, $
                               ;; GAUSSFITS=gaussFits, $
                               KAPPAFITANGLE_INDEX=kappaFitAngle_index, $
                               GAUSSFITANGLE_INDEX=gaussFitAngle_index, $
                               SHIFTTHETA=shiftTheta, $
                               ERANGE_PEAK=eRange_peak, $
                               PEAK_ENERGY=peak_energy, $
                               TIMEFNSTR=timeFNStr, $
                               UNITS=units, $
                               EXTEND_FITSTRUCT_ERANGE=extend_fitStruct_eRange, $
                               BF_GF__NORMALIZE_TO_VALS_AT_FITTED_ANGLE=bF_gF__normalize_to_vals_at_fitted_angle, $
                               BF_GF__LOGSCALE_REDUCENEGFAC=bF_gF__logScale_reduceNegFac, $
                               BF_GF__PLOT_BULKE_MODEL=bF_gF__plot_bulke_model, $
                               BF_GF__PLOT_MODEL_BULKE_V_DATA_COMPARISON=bF_gF__plot_model_bulkE_v_data_comparison, $
                               PLOTDIR=plotDir, $
                               ORBIT=orbit, $
                               OUT_ESTIMATED_LC=estimated_lc, $
                               KAPPAPARAMSTRUCT=kappaParamStruct, $
                               GAUSSPARAMSTRUCT=gaussParamStruct, $
                               KFIT2DPARAMSTRUCT=kFit2DParamStruct, $
                               ;; KFIT2DPARAMSTRUCT=kFit2DParamStruct, $
                               FIT2DKAPPA_INF_LIST=fit2DKappa_inf_list, $
                               FIT2DGAUSS_INF_LIST=fit2DGauss_inf_list, $
                               FIT2D__SHOW_AND_PROMPT__EACH_CANDIDATE=fit2d__show_each_candidate, $
                               FIT2D__SHOW_ONLY_DATA=fit2D__show_only_data, $
                               FIT2D__PA_ZRANGE=fit2D__PA_zRange, $
                               FIT2D__SAVE_ALL_PLOTS=fit2D__save_all_plots, $
                               PRINT_2DFITINFO=print_2DFitInfo, $
                               PRINT_2DWININFO=print_2DWinInfo, $
                               SAVE_BFUNC_GFUNC_PLOTS=save_bFunc_gFunc_plots, $
                               EPS=eps, $
                               MONTE_CARLO_MODE=monte_carlo_mode, $
                               MC__OKSTATUS=MC__OKStatus
  
  COMPILE_OPT IDL2,STRICTARRSUBS

  ;;For updating K_EA__gFunc,K_EA__bFunc
  @common__kappa_flux2d__horseshoe__eanisotropy.pro

  ;;Get yourself KF2D__SDTData_opt,KF2D__Curvefit_opt, etc.
  @common__kappa_fit2d_structs.pro

  ;;Units for later
  INIT_KAPPA_UNITCONV,curDataStr

  UPDATE_KAPPA_FLUX2D__HORSESHOE__BFUNC_AND_GFUNC,curDataStr, $
     ;; bestAngle_i, $
     ;; angleBin_i, $
     !NULL, $
     kappaFitAngle_index, $
     ESTIMATE_LOSSCONE=KF2D__SDTData_opt.estimate_sourceCone_from_dist, $
     ;; /ESTIMATE_LOSSCONE, $
     NORMALIZE_TO_VALS_AT_FITTED_ANGLE=bF_gF__normalize_to_vals_at_fitted_angle, $
     PEAK_ENERGY=peak_energy, $
     TIME=timeFNStr, $
     LOGSCALE_REDUCENEGFAC=bF_gF__logScale_reduceNegFac, $
     PLOT_BULKE_MODEL=bF_gF__plot_bulke_model, $
     ;; PLOT_BULKE_FACTOR=plot_bulke_factor, $
     ;; /PLOT_BULKE_FACTOR, $
     ;; POLARPLOT_BULKE_FACTOR=polarPlot_bulke_factor, $
     ;; /POLARPLOT_BULKE_FACTOR, $
     PLOT_MODEL_BULKE_V_DATA_COMPARISON=bF_gF__plot_model_bulkE_v_data_comparison, $
     ;; PLOT_FLUX_PEAKS=plot_flux_peaks, $
     ;; /PLOT_FLUX_PEAKS, $
     PLOTDIR=plotDir, $
     ORBIT=orbit, $
     OUT_ESTIMATED_LC=estimated_lc, $
     SAVE_PLOTS=save_bFunc_gFunc_plots, $
     EPS=eps

  KAPPA_FIT2D__HORSESHOE,curDataStr, $
     hadSuccessK, $
     ERANGE_PEAK=eRange_peak, $
     KFITPARAMSTRUCT=kappaParamStruct, $
     KFIT2DPARAMSTRUCT=kFit2DParamStruct, $
     IN_ESTIMATED_LC=estimated_lc, $
     UNITS=units, $
     PRINT_2DFITINFO=print_2DFitInfo, $
     PRINT_2DWININFO=print_2DWinInfo, $
     EPS=eps

  IF N_ELEMENTS(fit2DKappa_inf_list) GT 0 THEN BEGIN

     KAPPA_FIT2D__FIRE_EXTRAS,curKappaStr,curDataStr, $
                              SHIFTTHETA=shiftTheta, $
                              EXTEND_FITSTRUCT_ERANGE=extend_fitStruct_eRange, $
                              FIT2D__SHOW_AND_PROMPT__EACH_CANDIDATE=show_and_prompt, $
                              FIT2D__SHOW_ONLY_DATA=fit2D__show_only_data, $
                              FIT2D__PA_ZRANGE=fit2D__PA_zRange, $
                              FIT2D__SAVE_ALL_PLOTS=fit2D__save_all_plots, $
                              FIT2D__SHOW__IS_MAXWELLIAN_FIT=0, $
                              FIT2D__SHOW__FITSTRING='Kappa'

  ENDIF

  IF hadSuccessK AND KEYWORD_SET(KF2D__Curvefit_opt.add_gaussian_estimate) THEN BEGIN

     KAPPA_FIT2D__HORSESHOE,curDataStr, $
        hadSuccessG, $
        /IS_MAXWELLIAN_FIT, $
        ERANGE_PEAK=eRange_peak, $
        KFITPARAMSTRUCT=gaussParamStruct, $
        KFIT2DPARAMSTRUCT=kFit2DParamStruct, $
        IN_ESTIMATED_LC=estimated_lc, $
        UNITS=units, $
        PRINT_2DFITINFO=print_2DFitInfo, $
        PRINT_2DWININFO=print_2DWinInfo, $
        EPS=eps
     
     IF N_ELEMENTS(fit2DGauss_inf_list) GT 0 THEN BEGIN

        KAPPA_FIT2D__FIRE_EXTRAS,curGaussStr,curDataStr, $
                                 SHIFTTHETA=shiftTheta, $
                                 EXTEND_FITSTRUCT_ERANGE=extend_fitStruct_eRange, $
                                 FIT2D__SHOW_AND_PROMPT__EACH_CANDIDATE=show_and_prompt, $
                                 FIT2D__SHOW_ONLY_DATA=fit2D__show_only_data, $
                                 FIT2D__PA_ZRANGE=fit2D__PA_zRange, $
                                 FIT2D__SAVE_ALL_PLOTS=fit2D__save_all_plots, $
                                 /FIT2D__SHOW__IS_MAXWELLIAN_FIT, $
                                 FIT2D__SHOW__FITSTRING='Maxwell'

     ENDIF

  ENDIF

END
