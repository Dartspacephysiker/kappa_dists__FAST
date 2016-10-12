;2016/07/19 Time to do it 2D style
PRO KAPPA_EFLUX_FIT2D, $
   T1=t1, $
   T2=t2, $
   SDT_TIME_INDS=bounds, $
   DO_ALL_TIMES=do_all_times, $
   ENERGY_ELECTRONS=energy_electrons, $
   LOAD_DAT_FROM_FILE=loadFile, $
   SAVE_DIFF_EFLUX_TO_FILE=save_diff_eFlux_to_file, $
   LOAD_DIR=loadDir, $
   EEB_OR_EES=eeb_or_ees, $
   SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
   MIN_PEAK_ENERGY=min_peak_energy, $
   N_ENERGIES_BELOW_PEAK=n_below_peak, $
   N_ENERGIES_ABOVE_PEAK=n_above_peak, $
   N_BELOW_PEAK2D=n_below_peak2D, $
   N_ABOVE_PEAK2D=n_above_peak2D, $
   CHECK_FOR_HIGHER_FLUX_PEAKS__SET_CORRESPONDING_PEAK_ENERGY=check_higher_peaks_set_peakEn, $
   TRIM_ENERGIES_BELOW_PEAK=trim_energies_below_peak, $
   DONT_FIT_BELOW_THRESH_VALUE=dont_fit_below_thresh_value, $
   DENSITY_EST=n_est, $
   TEMPERATURE_EST=T, $
   KAPPA_EST=kappa, $
   SDT_DAT=dat, $
   ESTIMATE_FITPARAMS_FROM_SDT_DAT=estimate_A_from_data, $
   ESTIMATE_FACTORS=estFacs, $
   DONT_PRINT_ESTIMATES=dont_print_estimates, $
   DONT_PRINT_FITINFO=dont_print_fitInfo, $
   FIT1D__MAX_ITERATIONS=max_iter, $
   FIT1D__TOLERANCE=fit_tol, $
   FIT1D__AVERAGE_OVER_ANGLERANGE=fit1d__average_over_angleRange, $
   FIT1D__SKIP_BAD_FITS=fit1D__skip_bad_fits, $
   FIT1D__SHOW_AND_PROMPT=fit1D__show_and_prompt, $
   FIT1D__USER_PROMPT_ON_FAIL=fit1D_fail__user_prompt, $
   FIT2D__ONLY_FIT_ERANGE_AROUND_PEAK=fit2D__only_fit_peak_eRange, $
   FIT2D__ONLY_FIT_ERANGE_ABOVE_MIN=fit2D__only_fit_aboveMin, $
   FIT2D__SHOW_AND_PROMPT__EACH_CANDIDATE=fit2D__show_each_candidate, $
   FIT2D__SAVE_ALL_CANDIDATE_PLOTS=fit2D__save_all_candidate_plots, $
   FIT2D__KEEP_WHOLEFIT=fit2D__keep_wholeFit, $
   FIT2D__TOLERANCE=fit2d_tol, $
   FIT2D__MAX_ITERATIONS=fit2D_max_iter, $
   FIT2D__ONLY_FIT_ELECTRON_ANGLES=fit2D__only_fit_eAngles, $
   FIT2D__LOSSCONE_ANGLE=fit2D__lossCone_angle, $
   FIT2D__USE_BULK_E_ANISOTROPY=fit2D__bulk_e_anisotropy, $
   FIT2D__BULK_E_ANISO_FACTOR=fit2D__bulk_e_anis_factor, $
   FIT2D__DENSITY_ANGLERANGE=fit2D__density_angleRange, $
   FIT2D__EXCLUDE_LCA_FROM_DENSCALC=fit2D__exclude_lca_from_densCalc, $
   FIT2D__DISABLE_BFUNC=fit2D__disable_bFunc, $
   FIT2D__PRINT_FITINFO=print_2DFitInfo, $
   ADD_GAUSSIAN_ESTIMATE=add_gaussian_estimate, $
   USE_SDT_GAUSSIAN_FIT=use_SDT_Gaussian_fit, $
   USE_MPFIT1D=use_mpFit1D, $
   ADD_ONECOUNT_CURVE=add_oneCount_curve, $
   ADD_FITPARAMS_TEXT=add_fitParams_text, $
   ADD_ANGLE_LABEL=add_angle_label, $
   FIT2D__ADD_BOUNDARIES=fit2D__add_boundaries, $
   ELECTRON_ANGLERANGE=electron_angleRange, $
   ELECTRON_LOSSCONE_ANGLE=electron_lca, $
   NO_PLOTS=no_plots, $
   SAVE_FITPLOTS=save_fitplots, $
   PLOT_FULL_FIT=plot_full_fit, $
   PLOTNAMEPREF=plotNamePref, $
   PLOTDIR=plotDir, $
   OUT_FITTED_PARAMS=out_kappaParams, $
   OUT_FITTED_GAUSS_PARAMS=out_gaussParams, $
   OUT_KAPPA_FIT_STRUCTS=kappaFits, $
   OUT_GAUSS_FIT_STRUCTS=gaussFits, $
   OUT_FIT2DKAPPA_INF_LIST=fit2dKappa_inf_list, $
   OUT_FIT2DGAUSS_INF_LIST=fit2dGauss_inf_list, $
   OUT_SYNTH_SDT_STRUCTS=synthPackage, $
   ADD_FULL_FITS=add_full_fits, $
   OUT_ERANGE_PEAK=out_eRange_peak, $
   OUT_PARAMSTR=out_paramStr, $
   OUT_STRINGS=strings, $
   TXTOUTPUTDIR=txtOutputDir
  
  COMPILE_OPT idl2

  @common__kappa_fit2d_structs.pro

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Defaults

  ;;Curve fitting options
  KF2D__Curvefit_opt = INIT_KAPPA_CURVEFIT_OPTIONS( $
                       FIT1D__TOLERANCE=fit_tol, $
                       FIT1D__MAX_ITERATIONS=max_iter, $
                       FIT2D__TOLERANCE=fit2d_tol, $
                       FIT2D__MAX_ITERATIONS=fit2D_max_iter, $
                       FIT2D__ONLY_FIT_ELECTRON_ANGLES=fit2D__only_fit_eAngles, $
                       FIT2D__KEEP_WHOLEFIT=fit2D__keep_wholeFit, $
                       FIT2D__ONLY_FIT_ERANGE_AROUND_PEAK=fit2D__only_fit_peak_eRange, $
                       FIT2D__ONLY_FIT_ERANGE_ABOVE_MIN=fit2D__only_fit_aboveMin, $
                       FIT2D__USE_BULK_E_ANISOTROPY=fit2D__bulk_e_anisotropy, $
                       FIT2D__BULK_E_ANISO_FACTOR=fit2D__bulk_e_anis_factor, $
                       FIT2D__EXCLUDE_LCA_FROM_DENSCALC=fit2D__exclude_lca_from_densCalc, $
                       FIT2D__DISABLE_BFUNC=fit2D__disable_bFunc, $
                       N_ENERGIES_BELOW_PEAK=n_below_peak, $
                       N_ENERGIES_ABOVE_PEAK=n_above_peak, $
                       N_BELOW_PEAK2D=n_below_peak2D, $
                       N_ABOVE_PEAK2D=n_above_peak2D, $
                       TRIM_ENERGIES_BELOW_PEAK=trim_energies_below_peak, $
                       MIN_PEAK_ENERGY=min_peak_energy, $
                       DONT_FIT_BELOW_THRESH_VALUE=dont_fit_below_thresh_value, $
                       ESTIMATE_A_FROM_DATA=estimate_A_from_data, $
                       ADD_GAUSSIAN_ESTIMATE=add_gaussian_estimate, $
                       USE_SDT_GAUSSIAN_FIT=use_SDT_Gaussian_fit, $
                       USE_MPFIT1D=use_mpFit1D, $
                       DENSITY_EST=n_est, $
                       TEMPERATURE_EST=T, $
                       KAPPA_EST=kappa) ;, $
  ;; BULK_OFFSET=bulk_offset)
  
  

  ;;SDT data options
  KF2D__SDTData_opt  = INIT_KAPPA_SDTDATA_OPTIONS( $
                       EEB_OR_EES=eeb_or_ees, $
                       SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                       DO_ALL_TIMES=do_all_times, $
                       ENERGY_ELECTRONS=energy_electrons, $
                       ELECTRON_ANGLERANGE=electron_angleRange, $
                       ELECTRON_LOSSCONE_ANGLE=electron_lca, $
                       FIT2D__DENSITY_ANGLERANGE=fit2D__density_angleRange)

  ;;Plot options
  KF2D__Plot_opt     = INIT_KAPPA_PLOT_OPTIONS( $
                       NO_PLOTS=no_plots, $
                       SAVE_FITPLOTS=save_fitplots, $
                       PLOT_FULL_FIT=plot_full_fit, $
                       PLOTDIR=plotDir, $
                       PLOTNAMEPREF=plotNamePref, $
                       ADD_ONECOUNT_CURVE=add_oneCount_curve, $
                       ADD_FITPARAMS_TEXT=add_fitParams_text, $
                       ADD_ANGLE_LABEL=add_angle_label, $
                       FIT2D__ADD_BOUNDARIES=fit2D__add_boundaries)



  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Defaults
  KAPPA_FIT2D_DEFAULTS, $
     BOUNDS=bounds              ;, $

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Get data
  ;; IF N_ELEMENTS(eSpec) EQ 0 OR N_ELEMENTS(diff_eFlux) EQ 0 THEN BEGIN
  ;; IF N_ELEMENTS(diff_eFlux) EQ 0 THEN BEGIN
  GET_LOSSCONE_AND_EFLUX_DATA,T1=t1,T2=t2, $
                              LOAD_DAT_FROM_FILE=loadFile, $
                              LOAD_DIR=loadDir, $
                              EEB_OR_EES=KF2D__SDTData_opt.eeb_or_ees, $
                              DIFF_EFLUX=diff_eFlux, $
                              SPECTRA_AVERAGE_INTERVAL=KF2D__SDTData_opt.spec_avg_intvl, $
                              OUT_ORB=orb, $
                              OUT_ANGLERANGE=e_angle, $
                              /FIT_EACH_ANGLE, $ ;Perma-set because we need all angles here
                              CUSTOM_E_ANGLERANGE=KF2D__SDTData_opt.electron_angleRange, $
                              ANGLESTR=angleStr, $
                              ESPECUNITS=eSpecUnits, $
                              ELECTRON_ENERGY_LIMS=KF2D__SDTData_opt.energy_electrons, $
                              SAVE_DIFF_EFLUX_TO_FILE=save_diff_eFlux_to_file

  orbStr                            = STRCOMPRESS(orb,/REMOVE_ALL)
  ;; ENDIF ELSE BEGIN
  ;;    orbStr                            = '???'
  ;; ENDELSE

  IF SIZE(diff_eFlux,/TYPE) NE 8 THEN BEGIN
     PRINT,"Couldn't get diff_eFlux! Quitting ..."
     RETURN
  ENDIF

  IF KEYWORD_SET(do_all_times) THEN BEGIN
     PRINT,"Doing all times ..."
     nBounds                           = N_ELEMENTS(diff_eFlux.time)
     bounds                            = INDGEN(nBounds)
  END

  ;;Onecount curve?
  IF KEYWORD_SET(add_oneCount_curve) THEN BEGIN
     GET_ONECOUNT_DIFF_EFLUX_CURVE,t1,t2, $
                                   ;; LOAD_DAT_FROM_FILE=loadFile, $ ;;handled through proto
                                   EEB_OR_EES=KF2D__SDTData_opt.EEB_or_EES, $
                                   SPECTRA_AVERAGE_INTERVAL=KF2D__SDTData_opt.spec_avg_intvl, $
                                   IN_PROTOSTRUCT=diff_eFlux, $
                                   SDT_NAME=dEF_oneCount_name, $
                                   ANGLE=e_angle, $
                                   ;; ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
                                   /FIT_EACH_ANGLE, $  ;Perma-set because we do all angles for 2D fitting
                                   OUT_ONEDAT=out_oneDat, $
                                   QUIET=quiet

     GET_DATA,dEF_oneCount_name,DATA=dEF_oneCount
  ENDIF

  ;;Times and strings
  times                                = diff_eFlux.time
  KF2D__strings                        = INIT_KAPPA_STRING_STRUCT(diff_eFlux, $
                                                                  times, $
                                                                  orbStr, $
                                                                  angleStr, $
                                                                  KF2D__SDTData_opt)


  KAPPA_FIT2D__LOOP,diff_eFlux,times,dEF_oneCount, $
                    BOUNDS=bounds, $
                    ESTFACS=estFacs, $
                    FIT1D__AVERAGE_OVER_ANGLERANGE=fit1d__average_over_angleRange, $
                    FIT1D__SKIP_BAD_FITS=fit1D__skip_bad_fits, $
                    FIT1D__SHOW_AND_PROMPT=fit1d__show_and_prompt, $
                    FIT1D__USER_PROMPT_ON_FAIL=fit1D_fail__user_prompt, $
                    FIT2D__SHOW_AND_PROMPT__EACH_CANDIDATE=fit2d__show_each_candidate, $
                    FIT2D__SAVE_ALL_CANDIDATE_PLOTS=fit2D__save_all_candidate_plots, $
                    FIT2D__PRINT_FITINFO=print_2DFitInfo, $
                    DONT_PRINT_ESTIMATES=dont_print_estimates, $
                    DONT_PRINT_FITINFO=dont_print_fitInfo, $
                    ;; E_ANGLE=e_angle, $
                    CHECK_FOR_HIGHER_FLUX_PEAKS=check_higher_peaks_set_peakEn, $
                    OUT_FITTED_PARAMS=out_kappaParams, $
                    OUT_FITTED_GAUSS_PARAMS=out_gaussParams, $
                    OUT_KAPPA_FIT_STRUCTS=kappaFits, $
                    OUT_GAUSS_FIT_STRUCTS=gaussFits, $
                    OUT_FIT2DKAPPA_INF_LIST=fit2dKappa_inf_list, $
                    OUT_FIT2DGAUSS_INF_LIST=fit2dGauss_inf_list, $
                    OUT_SYNTH_SDT_STRUCTS=synthPackage, $
                    OUT_ERANGE_PEAK=out_eRange_peak, $
                    OUT_PARAMSTR=out_paramStr, $
                    TXTOUTPUTDIR=txtOutputDir

  PRINT,'DONE!'

END
