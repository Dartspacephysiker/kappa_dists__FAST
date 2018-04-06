;2017/12/27
PRO KAPPA_FIT2D__FIREINTHEHOLE,curDataStr, $
                               hadSuccessK, $
                               hadSuccessG, $
                               CURKAPPASTR=curKappaStr, $
                               CURGAUSSSTR=curGaussStr, $
                               ONLY_GAUSSIAN_ESTIMATE=only_Gaussian_estimate, $
                               ;; KAPPAFITS=kappaFits, $
                               ;; GAUSSFITS=gaussFits, $
                               KAPPAFITANGLE_INDEX=kappaFitAngle_index, $
                               GAUSSFITANGLE_INDEX=gaussFitAngle_index, $
                               ;; KEEPKAPPA_INDICES=keepKappa_indices, $
                               ;; KEEPGAUSS_INDICES=keepGauss_indices, $
                               SHIFTTHETA=shiftTheta, $
                               ERANGE_FIT=eRange_fit, $
                               PEAK_ENERGY=peak_energy, $
                               TIMEFNSTR=timeFNStr, $
                               UNITS=units, $
                               MAKE_FIT2D_INFO=make_fit2D_info, $
                               MAKE_FIT2DPARAMARRS=make_fit2DParamArrs, $
                               KAPPAFIT2DPARAMARR=kappaFit2DParamArr, $
                               GAUSSFIT2DPARAMARR=gaussFit2DParamArr, $
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
                               ;; FIT2DPARAMSTRUCT=fit2DParamStruct, $
                               FIT2DKAPPA_INF_LIST=fit2DKappa_inf_list, $
                               FIT2DGAUSS_INF_LIST=fit2DGauss_inf_list, $
                               OPTIONAL__KAPPAFIT1D=kappaFit, $
                               OPTIONAL__GAUSSFIT1D=gaussFit, $
                               FIT2D__SHOW_AND_PROMPT__EACH_CANDIDATE=fit2d__show_each_candidate, $
                               FIT2D__SHOW_ONLY_DATA=fit2D__show_only_data, $
                               FIT2D__PA_ZRANGE=fit2D__PA_zRange, $
                               FIT2D__SAVE_ALL_PLOTS=fit2D__save_all_plots, $
                               PRINT_2DFITINFO=print_2DFitInfo, $
                               MAKE_BFUNC_GFUNC_PLOTS=make_bFunc_gFunc_plots, $
                               SAVE_BFUNC_GFUNC_PLOTS=save_bFunc_gFunc_plots, $
                               EPS=eps, $
                               MONTE_CARLO_MODE=Monte_Carlo_mode, $
                               MONTE_CARLO_KFP2D_HOLDOVER=MC_KFP2D_HOLDOVER, $
                               MC__OKSTATUS=MC__OKStatus
  
  COMPILE_OPT IDL2,STRICTARRSUBS

  ;;For updating K_EA__gFunc,K_EA__bFunc
  @common__kappa_flux2d__horseshoe__eanisotropy.pro

  ;;Get yourself KF2D__SDTData_opt,KF2D__Curvefit_opt, etc.
  @common__kappa_fit2d_structs.pro

  ;;Units for later
  ;; INIT_KAPPA_UNITCONV,curDataStr

  SETUP_KAPPA_FIT2D__HORSESHOE, $
     eRange_fit, $
     curDataStr, $
     wtsForFit,X2D,Y2D,dataToFit, $
     fa, $
     IS_MAXWELLIAN_FIT=is_maxwellian_fit, $
     UNITS=units, $
     ;; IN_ESTIMATED_LC=estimated_lc, $
     OUT_FIT2D_DENS_ANGLEINFO=fit2D_dens_angleInfo

  UPDATE_KAPPA_FLUX2D__HORSESHOE__BFUNC_AND_GFUNC,curDataStr, $
     ;; bestAngle_i, $
     ;; angleBin_i, $
     !NULL, $
     X2D=X2D, $
     Y2D=Y2D, $
     KEYWORD_SET(only_Gaussian_estimate) ? gaussFitAngle_index : kappaFitAngle_index, $
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
     ;; OUT_ESTIMATED_LC=estimated_lc, $
     MAKE_PLOTS=make_bFunc_gFunc_plots, $
     SAVE_PLOTS=save_bFunc_gFunc_plots, $
     EPS=eps

  IF ~KEYWORD_SET(only_Gaussian_estimate) THEN BEGIN

     KAPPA_FIT2D__HORSESHOE,curDataStr, $
                            hadSuccessK, $
                            X2D=X2D, $
                            Y2D=Y2D, $
                            DATATOFIT=dataToFit, $
                            WEIGHTS=wtsForFit, $
                            FUNCTARGS=fa, $
                            ERANGE_FIT=eRange_fit, $
                            FITPARAMSTRUCT=kappaParamStruct, $
                            ;; FIT2DPARAMSTRUCT=fit2DParamStruct, $
                            ;; IN_ESTIMATED_LC=estimated_lc, $
                            FIT__LINEAR_ENERGY_SHIFT=KF2D__Curvefit_opt.fit__linear_energy_shift, $
                            ;; FIT__LES__TAKE_STOCK_OF_RB=KF2D__Curvefit_opt.fit__LES__take_stock_of_RB, $
                            UNITS=units, $
                            FIT2D_DENS_ANGLEINFO=fit2D_dens_angleInfo, $
                            OUT_FIT2DPARAMS=kappaFit2DParams, $
                            MAKE_FIT2D_INFO=make_fit2D_info, $
                            OUT_FIT2D_FITINFO=kappaFit2D_info, $
                            PRINT_2DFITINFO=print_2DFitInfo, $
                            FITSTRING='Kappa', $
                            MONTE_CARLO_MODE=Monte_Carlo_mode, $
                            MC__OKSTATUS=MC__OKStatus, $
                            MC__FITSTRUCT=curKappaStr

     IF KEYWORD_SET(make_fit2D_info) THEN BEGIN

        ;; IF STRMATCH(timeFNStr,'*09_26_57*') THEN STOP

        KAPPA_FIT2D__FIRE_EXTRAS,curKappaStr,curDataStr,hadSuccessK, $
                                 IN_FIT2D_PARAMS=kappaFit2DParams, $
                                 FIT2D_FITINFO=kappaFit2D_info, $
                                 ERANGE_FIT=eRange_fit, $
                                 SHIFTTHETA=shiftTheta, $
                                 FITANGLE_I=kappaFitAngle_index, $
                                 EXTEND_FITSTRUCT_ERANGE=extend_fitStruct_eRange, $
                                 UNITS=units, $
                                 OPTIONAL__FIT1DINFO=kappaFit, $
                                 FIT2D__SHOW_AND_PROMPT__EACH_CANDIDATE=fit2d__show_each_candidate, $
                                 FIT2D__SHOW_ONLY_DATA=fit2D__show_only_data, $
                                 FIT2D__PA_ZRANGE=fit2D__PA_zRange, $
                                 FIT2D__SAVE_ALL_PLOTS=fit2D__save_all_plots, $
                                 FIT2D__SHOW__IS_MAXWELLIAN_FIT=0, $
                                 FIT2D__SHOW__FITSTRING='Kappa', $
                                 PRINT_2DFITINFO=print_2DFitInfo, $
                                 TIMEFNSTR=timeFNStr, $
                                 EPS=eps


     ENDIF

  ENDIF ELSE BEGIN

     dispensation = 1

  ENDELSE

  IF KEYWORD_SET(hadSuccessK) OR KEYWORD_SET(dispensation) THEN BEGIN

     IF KEYWORD_SET(hadSuccessK) THEN BEGIN

        IF SIZE(fit2DKappa_inf_list,/TYPE) NE 0 THEN $
           fit2DKappa_inf_list.Add,TEMPORARY(kappaFit2D_info)

        IF KEYWORD_SET(make_fit2DParamArrs) THEN BEGIN

           ;; The following IF statement ensures that we don't beef up the fit2dParamArr if
           ;;the Gaussian fit turns out to be bad
           IF ~(KEYWORD_SET(Monte_Carlo_mode) AND $
                KEYWORD_SET(KF2D__Curvefit_opt.add_gaussian_estimate)) THEN BEGIN

              IF N_ELEMENTS(kappaFit2DParamArr) EQ 0 THEN BEGIN
                 kappaFit2DParamArr = kappaFit2DParams
              ENDIF ELSE BEGIN
                 kappaFit2DParamArr = [[kappaFit2DParamArr],[kappaFit2DParams]]
              ENDELSE

           ENDIF

        ENDIF

     ENDIF

     IF (KEYWORD_SET(KF2D__Curvefit_opt.add_gaussian_estimate) AND ~KEYWORD_SET(Monte_Carlo_mode)) OR KEYWORD_SET(only_Gaussian_estimate) THEN BEGIN
        
        KAPPA_FIT2D__HORSESHOE,curDataStr, $
                               hadSuccessG, $
                               X2D=X2D, $
                               Y2D=Y2D, $
                               DATATOFIT=dataToFit, $
                               WEIGHTS=wtsForFit, $
                               FUNCTARGS=fa, $
                               /IS_MAXWELLIAN_FIT, $
                               ERANGE_FIT=eRange_fit, $
                               FITPARAMSTRUCT=gaussParamStruct, $
                               ;; FIT2DPARAMSTRUCT=fit2DParamStruct, $
                               ;; IN_ESTIMATED_LC=estimated_lc, $
                               FIT__LINEAR_ENERGY_SHIFT=KF2D__Curvefit_opt.fit__linear_energy_shift, $
                               ;; FIT__LES__TAKE_STOCK_OF_RB=KF2D__Curvefit_opt.fit__LES__take_stock_of_RB, $
                               UNITS=units, $
                               FIT2D_DENS_ANGLEINFO=fit2D_dens_angleInfo, $
                               OUT_FIT2DPARAMS=gaussFit2DParams, $
                               MAKE_FIT2D_INFO=make_fit2D_info, $
                               OUT_FIT2D_FITINFO=gaussFit2D_info, $
                               PRINT_2DFITINFO=print_2DFitInfo, $
                               FITSTRING='Maxwellian', $
                               MONTE_CARLO_MODE=Monte_Carlo_mode, $
                               MC__OKSTATUS=MC__OKStatus, $
                               MC__FITSTRUCT=curGaussStr
        
        IF KEYWORD_SET(make_fit2D_info) THEN BEGIN

           KAPPA_FIT2D__FIRE_EXTRAS,curGaussStr,curDataStr,hadSuccessG, $
                                    IN_FIT2D_PARAMS=gaussFit2DParams, $
                                    FIT2D_FITINFO=gaussFit2D_info, $
                                    ERANGE_FIT=eRange_fit, $
                                    SHIFTTHETA=shiftTheta, $
                                    FITANGLE_I=gaussFitAngle_index, $
                                    EXTEND_FITSTRUCT_ERANGE=extend_fitStruct_eRange, $
                                    UNITS=units, $
                                    OPTIONAL__FIT1DINFO=gaussFit, $
                                    FIT2D__SHOW_AND_PROMPT__EACH_CANDIDATE=fit2d__show_each_candidate, $
                                    FIT2D__SHOW_ONLY_DATA=fit2D__show_only_data, $
                                    FIT2D__PA_ZRANGE=fit2D__PA_zRange, $
                                    FIT2D__SAVE_ALL_PLOTS=fit2D__save_all_plots, $
                                    /FIT2D__SHOW__IS_MAXWELLIAN_FIT, $
                                    FIT2D__SHOW__FITSTRING='Maxwell', $
                                    PRINT_2DFITINFO=print_2DFitInfo, $
                                    TIMEFNSTR=timeFNStr, $
                                    EPS=eps

        ENDIF

        
        IF hadSuccessG AND KEYWORD_SET(hadSuccessK) THEN BEGIN

           IF SIZE(fit2DGauss_inf_list,/TYPE) NE 0 THEN $
              fit2DGauss_inf_list.ADD,TEMPORARY(gaussFit2D_info)

           IF KEYWORD_SET(make_fit2DParamArrs) THEN BEGIN

              IF N_ELEMENTS(gaussFit2DParamArr) EQ 0 THEN BEGIN
                 gaussFit2DParamArr = gaussFit2DParams
              ENDIF ELSE BEGIN
                 gaussFit2DParamArr = [[gaussFit2DParamArr],[gaussFit2DParams]]
              ENDELSE

              ;; Pick up kappa params here because now we know both fits were good
              IF KEYWORD_SET(Monte_Carlo_mode) THEN BEGIN
                 IF N_ELEMENTS(kappaFit2DParamArr) EQ 0 THEN BEGIN
                    kappaFit2DParamArr = TEMPORARY(MC_KFP2D_HOLDOVER)
                 ENDIF ELSE BEGIN
                    kappaFit2DParamArr = [[kappaFit2DParamArr], $
                                          [TEMPORARY(MC_KFP2D_HOLDOVER)]]
                 ENDELSE
              ENDIF

           ENDIF

        ENDIF

     ENDIF ELSE IF KEYWORD_SET(Monte_Carlo_mode) THEN BEGIN
        MC_KFP2D_HOLDOVER = TEMPORARY(kappaFit2DParams)
     ENDIF

  ENDIF
  
END
