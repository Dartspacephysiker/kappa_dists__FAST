;2016/07/19 Time to do it 2D style
PRO KAPPA_EFLUX_FIT2D, $
   T1=t1, $
   T2=t2, $
   ENERGY_ELECTRONS=energy_electrons, $
   LOAD_DAT_FROM_FILE=loadFile, $
   EEB_OR_EES=eeb_or_ees, $
   SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
   FIT_EACH_ANGLE=fit_each_angle, $
   FIT_EACH__AVERAGE_OVER_ANGLERANGE=fit_each__average_over_angleRange, $
   FIT_EACH__SYNTH_SDT_STRUCT=synthPackage, $
   FIT_EACH__SKIP_BAD_FITS=fit_each__skip_bad_fits, $
   FIT_EACH__MIN_ANGLEFITS_FOR_KEEP=min_anglefits_for_keep, $
   FIT_EACH__SHOW_AND_PROMPT=fit_each__show_and_prompt, $
   FIT_FAIL__USER_PROMPT=fit_fail__user_prompt, $
   SDT_TIME_INDS=bounds, $
   DO_ALL_TIMES=do_all_times, $
   MIN_PEAK_ENERGY=min_peak_energy, $
   DENSITY_EST=n_est, $
   TEMPERATURE_EST=T, $
   KAPPA_EST=kappa, $
   SDT_DAT=dat, $
   BULK_OFFSET=bulk_offset, $
   DONT_TAKE_STOCK_OF_BULKANGLE=dont_take_stock_of_bulkangle, $
   TREAT_FIELDALIGNED_AS_BULK=treat_fieldaligned_as_bulk, $
   ESTIMATE_FITPARAMS_FROM_SDT_DAT=estimate_A_from_data, $
   ESTIMATE_FACTORS=estFacs, $
   DONT_PRINT_ESTIMATES=dont_print_estimates, $
   TRIM_ENERGIES_BELOW_PEAK=trim_energies_below_peak, $
   DONT_FIT_BELOW_THRESH_VALUE=dont_fit_below_thresh_value, $
   N_ENERGIES_BELOW_PEAK=n_below_peak, $
   N_ENERGIES_ABOVE_PEAK=n_above_peak, $
   CHECK_FOR_HIGHER_FLUX_PEAKS__SET_CORRESPONDING_PEAK_ENERGY=check_for_higher_flux_peaks__set_corresponding_peak_energy, $
   FIT_TOLERANCE=fit_tol, $
   FIT2D_TOLERANCE=fit2d_tol, $
   MAX_ITERATIONS=max_iter, $
   FIT2D_MAX_ITERATIONS=fit2d_max_iter, $
   ADD_GAUSSIAN_ESTIMATE=add_gaussian_estimate, $
   USE_SDT_GAUSSIAN_FIT=use_SDT_Gaussian_fit, $
   ADD_ONECOUNT_CURVE=add_oneCount_curve, $
   ADD_FITPARAMS_TEXT=add_fitParams_text, $
   ADD_ANGLE_LABEL=add_angle_label, $
   ELECTRON_ANGLERANGE=electron_angleRange, $
   NO_PLOTS=no_plots, $
   SAVE_FITPLOTS=save_fitplots, $
   PLOT_FULL_FIT=plot_full_fit, $
   PLOTDIR=plotDir, $
   OUTPUT_DENSITY_ESTIMATES=output_density_estimates, $
   OUTPUT_DENSITY__ERANGE=dens_est_eRange, $
   OUTPUT_DENS__ENERGIES=output_dens__energies, $
   OUTPUT_DENS__ANGLES=output_dens__angles, $
   OUT_DENS_STRUCT=out_dens, $
   OUT_PEAK_DENS_STRUCT=out_peak_dens, $
   ONLY_DENS_ESTIMATES=only_dens_estimates, $
   OUT_FITTED_PARAMS=out_kappaParams, $
   OUT_FITTED_GAUSS_PARAMS=out_gaussParams, $
   OUT_KAPPA_FIT_STRUCTS=kappaFits, $
   OUT_GAUSS_FIT_STRUCTS=gaussFits, $
   ADD_FULL_FITS=add_full_fits, $
   OUT_ERANGE_PEAK=out_eRange_peak, $
   OUT_PARAMSTR=out_paramStr, $
   OUT_STRINGS=strings, $
   TXTOUTPUTDIR=txtOutputDir
  
  COMPILE_OPT idl2

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Defaults

  ;;Curve fitting options
  kCurvefit_opt = INIT_KAPPA_CURVEFIT_OPTIONS(FIT_TOLERANCE=fit_tol, $
                                              MAX_ITERATIONS=max_iter, $
                                              FIT2D_TOLERANCE=fit2d_tol, $
                                              FIT2D_MAX_ITERATIONS=fit2d_max_iter, $
                                              N_ENERGIES_BELOW_PEAK=n_below_peak, $
                                              N_ENERGIES_ABOVE_PEAK=n_above_peak, $
                                              TRIM_ENERGIES_BELOW_PEAK=trim_energies_below_peak, $
                                              MIN_PEAK_ENERGY=min_peak_energy, $
                                              DONT_FIT_BELOW_THRESH_VALUE=dont_fit_below_thresh_value, $
                                              ESTIMATE_A_FROM_DATA=estimate_A_from_data, $
                                              ADD_GAUSSIAN_ESTIMATE=add_gaussian_estimate, $
                                              USE_SDT_GAUSSIAN_FIT=use_SDT_Gaussian_fit, $
                                              DENSITY_EST=n_est, $
                                              TEMPERATURE_EST=T, $
                                              KAPPA_EST=kappa, $
                                              BULK_OFFSET=bulk_offset)
                  

  ;;SDT data options
  kSDTData_opt  = INIT_KAPPA_SDTDATA_OPTIONS(EEB_OR_EES=eeb_or_ees, $
                                            SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                                            DO_ALL_TIMES=do_all_times, $
                                            ENERGY_ELECTRONS=energy_electrons, $
                                            ELECTRON_ANGLERANGE=electron_angleRange)

  ;;Plot options
  kPlot_opt     = INIT_KAPPA_PLOT_OPTIONS(NO_PLOTS=no_plots, $
                                          SAVE_FITPLOTS=save_fitplots, $
                                          PLOT_FULL_FIT=plot_full_fit, $
                                          PLOTDIR=plotDir, $
                                          ADD_ONECOUNT_CURVE=add_oneCount_curve, $
                                          ADD_FITPARAMS_TEXT=add_fitParams_text, $
                                          ADD_ANGLE_LABEL=add_angle_label)



  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Defaults
  KAPPA_FIT2D_DEFAULTS, $
     BOUNDS=bounds ;, $

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Get data
  ;; IF N_ELEMENTS(eSpec) EQ 0 OR N_ELEMENTS(diff_eFlux) EQ 0 THEN BEGIN
  IF N_ELEMENTS(diff_eFlux) EQ 0 THEN BEGIN
     GET_LOSSCONE_AND_EFLUX_DATA,T1=t1,T2=t2, $
                                 LOAD_DAT_FROM_FILE=loadFile, $
                                 EEB_OR_EES=kSDTData_opt.eeb_or_ees, $
                                 DIFF_EFLUX=diff_eFlux, $
                                 SPECTRA_AVERAGE_INTERVAL=kSDTData_opt.spec_avg_intvl, $
                                 OUT_ORB=orb, $
                                 OUT_ANGLERANGE=e_angle, $
                                 /FIT_EACH_ANGLE, $ ;Perma-set because we need all angles here
                                 CUSTOM_E_ANGLERANGE=kSDTData_opt.electron_angleRange, $
                                 ANGLESTR=angleStr, $
                                 ESPECUNITS=eSpecUnits, $
                                 ELECTRON_ENERGY_LIMS=kSDTData_opt.energy_electrons

     orbStr                            = STRCOMPRESS(orb,/REMOVE_ALL)
  ENDIF ELSE BEGIN
     orbStr                            = '???'
  ENDELSE

  IF KEYWORD_SET(do_all_times) THEN BEGIN
     PRINT,"Doing all times ..."
     nBounds                           = N_ELEMENTS(diff_eFlux.time)
     bounds                            = INDGEN(nBounds)
  END

  ;;Onecount curve?
  IF KEYWORD_SET(add_oneCount_curve) THEN BEGIN
     GET_ONECOUNT_DIFF_EFLUX_CURVE,t1,t2, $
                                   EEB_OR_EES=kSDTData_opt.EEB_or_EES, $
                                   SPECTRA_AVERAGE_INTERVAL=kSDTData_opt.spec_avg_intvl, $
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
  kStrings                             = INIT_KAPPA_STRING_STRUCT(diff_eFlux, $
                                                                  times, $
                                                                  orbStr, $
                                                                  angleStr, $
                                                                  kSDTData_opt)


  KAPPA_FIT2D__LOOP,diff_eFlux,times,dEF_oneCount, $
                    KSDTDATA_OPT=kSDTData_opt, $
                    KCURVEFIT_OPT=kCurvefit_opt, $
                    KPLOT_OPT=kPlot_opt, $
                    STRINGS=kStrings, $
                    BOUNDS=bounds, $
                    ESTFACS=estFacs, $
                    DONT_PRINT_ESTIMATES=dont_print_estimates, $
                    E_ANGLE=e_angle, $
                    DONT_TAKE_STOCK_OF_BULKANGLE=dont_take_stock_of_bulkangle, $
                    TREAT_FIELDALIGNED_AS_BULK=treat_fieldaligned_as_bulk, $
                    CHECK_FOR_HIGHER_FLUX_PEAKS=check_for_higher_flux_peaks__set_corresponding_peak_energy, $
                    FIT_EACH__AVERAGE_OVER_ANGLERANGE=fit_each__average_over_angleRange, $
                    FIT_EACH__SYNTH_SDT_STRUCT=synthPackage, $
                    FIT_EACH__SKIP_BAD_FITS=fit_each__skip_bad_fits, $
                    FIT_EACH__MIN_ANGLEFITS_FOR_KEEP=min_anglefits_for_keep, $
                    FIT_EACH__SHOW_AND_PROMPT=fit_each__show_and_prompt, $
                    FIT_FAIL__USER_PROMPT=fit_fail__user_prompt, $
                    OUT_FITTED_PARAMS=out_kappaParams, $
                    OUT_FITTED_GAUSS_PARAMS=out_gaussParams, $
                    OUT_KAPPA_FIT_STRUCTS=kappaFits, $
                    OUT_GAUSS_FIT_STRUCTS=gaussFits, $
                    OUT_ERANGE_PEAK=out_eRange_peak, $
                    OUT_PARAMSTR=out_paramStr, $
                    TXTOUTPUTDIR=txtOutputDir

  PRINT,'DONE!'

END
