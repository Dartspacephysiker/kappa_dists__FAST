;2016/07/19 Time to do it 2D style
PRO KAPPA_EFLUX_FIT2D, $
   T1=t1, $
   T2=t2, $
   SDT_TIME_INDS=bounds, $
   DO_ALL_TIMES=do_all_times, $
   ENERGY_ELECTRONS=energy_electrons, $
   LOAD_DAT_FROM_FILE=loadFile, $
   SAVE_DIFF_EFLUX_TO_FILE=save_diff_eFlux_to_file, $
   JUST_DIFF_EFLUX=just_diff_eFlux, $
   DIFF_EFLUX=diff_eFlux, $
   DEF_ONECOUNT=dEF_oneCount, $
   LOAD_DIR=loadDir, $
   EEB_OR_EES=eeb_or_ees, $
   SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
   MIN_PEAK_ENERGY=min_peak_energy, $
   MAX_PEAK_ENERGY=max_peak_energy, $
   PEAK_ENERGY__START_AT_HIGHE=peak_energy__start_at_highE, $
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
   ONLY_1D_FITS=only_1D_fits, $
   FIT1D__MAX_ITERATIONS=max_iter, $
   FIT1D__TOLERANCE=fit_tol, $
   FIT1D__AVERAGE_OVER_ANGLERANGE=fit1D__average_over_angleRange, $
   FIT1D__SOURCECONE_ENERGY_SPECTRUM=fit1D__sourceCone_energy_spectrum, $
   FIT1D__NFLUX=fit1D__nFlux, $
   FIT1D__WEIGHTING=fit1D__weighting, $
   FIT1D__CLAMPTEMPERATURE=fit1D__clampTemperature, $
   FIT1D__CLAMPDENSITY=fit1D__clampDensity, $
   FIT1D__SKIP_BAD_FITS=fit1D__skip_bad_fits, $
   FIT1D__SHOW_AND_PROMPT=fit1D__show_and_prompt, $
   FIT1D__USER_PROMPT_ON_FAIL=fit1D_fail__user_prompt, $
   FIT1D__SAVE_PLOTSLICES=fit1D__save_plotSlices, $
   FIT2D__ONLY_FIT_ERANGE_AROUND_PEAK=fit2D__only_fit_peak_eRange, $
   FIT2D__ONLY_FIT_ERANGE_ABOVE_MIN=fit2D__only_fit_aboveMin, $
   FIT2D__SHOW_AND_PROMPT__EACH_CANDIDATE=fit2D__show_each_candidate, $
   FIT2D__SHOW_ONLY_DATA=fit2D__show_only_data, $
   FIT2D__PA_ZRANGE=fit2D__PA_zRange, $
   FIT2D__SAVE_ALL_CANDIDATE_PLOTS=fit2D__save_all_candidate_plots, $
   FIT2D__KEEP_WHOLEFIT=fit2D__keep_wholeFit, $
   FIT2D__WEIGHTING=fit2D__weighting, $
   FIT2D__CLAMPTEMPERATURE=fit2D__clampTemperature, $
   FIT2D__CLAMPDENSITY=fit2D__clampDensity, $
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
   FIT_EACH_ANGLE=fit_each_angle, $
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
   TXTOUTPUTDIR=txtOutputDir,$
   DEBUG__SKIP_TO_THIS_TIME=debug__skip_to_this_time, $
   DEBUG__BREAK_ON_THIS_TIME=debug__break_on_this_time
  
  COMPILE_OPT IDL2,STRICTARRSUBS

  @common__kappa_fit2d_structs.pro

  INIT__KAPPA_EFLUX_FIT1D_OR_FIT2D, $
     T1=t1, $
     T2=t2, $
     LOAD_DAT_FROM_FILE=loadFile, $
     LOAD_DIR=loadDir, $
     SAVE_DIFF_EFLUX_TO_FILE=save_diff_eFlux_to_file, $
     KF__CURVEFIT_OPT=KF2D__curveFit_opt, $
     KF__SDTDATA_OPT=KF2D__SDTData_opt, $
     KF__PLOT_OPT=KF2D__Plot_opt, $
     KF__STRINGS=KF2D__strings, $
     DIFF_EFLUX=diff_eFlux, $
     DEF_ONECOUNT=dEF_oneCount, $
     TIMES=times, $
     SDT_TIME_INDS=bounds, $
     ONLY_1D_FITS=only_1D_fits, $
     FIT1D__TOLERANCE=fit_tol, $
     FIT1D__MAX_ITERATIONS=max_iter, $
     FIT1D__SOURCECONE_ENERGY_SPECTRUM=fit1D__sourceCone_energy_spectrum, $
     FIT1D__NFLUX=fit1D__nFlux, $
     FIT1D__WEIGHTING=fit1D__weighting, $
     FIT1D__CLAMPTEMPERATURE=fit1D__clampTemperature, $
     FIT1D__CLAMPDENSITY=fit1D__clampDensity, $
     FIT2D__TOLERANCE=fit2d_tol, $
     FIT2D__MAX_ITERATIONS=fit2D_max_iter, $
     FIT2D__ONLY_FIT_ELECTRON_ANGLES=fit2D__only_fit_eAngles, $
     FIT2D__KEEP_WHOLEFIT=fit2D__keep_wholeFit, $
     FIT2D__WEIGHTING=fit2D__weighting, $
     FIT2D__CLAMPTEMPERATURE=fit2D__clampTemperature, $
     FIT2D__CLAMPDENSITY=fit2D__clampDensity, $
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
     MAX_PEAK_ENERGY=max_peak_energy, $
     PEAK_ENERGY__START_AT_HIGHE=peak_energy__start_at_highE, $
     DONT_FIT_BELOW_THRESH_VALUE=dont_fit_below_thresh_value, $
     ESTIMATE_A_FROM_DATA=estimate_A_from_data, $
     ADD_GAUSSIAN_ESTIMATE=add_gaussian_estimate, $
     USE_SDT_GAUSSIAN_FIT=use_SDT_Gaussian_fit, $
     USE_MPFIT1D=use_mpFit1D, $
     DENSITY_EST=n_est, $
     TEMPERATURE_EST=T, $
     KAPPA_EST=kappa, $
     EEB_OR_EES=eeb_or_ees, $
     SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
     DO_ALL_TIMES=do_all_times, $
     TIME_ARR=time_arr, $
     ENERGY_ELECTRONS=energy_electrons, $
     ELECTRON_ANGLERANGE=electron_angleRange, $
     ELECTRON_LOSSCONE_ANGLE=electron_lca, $
     FIT2D__DENSITY_ANGLERANGE=fit2D__density_angleRange, $
     NO_PLOTS=no_plots, $
     SAVE_FITPLOTS=save_fitplots, $
     PLOT_FULL_FIT=plot_full_fit, $
     PLOTDIR=plotDir, $
     PLOTNAMEPREF=plotNamePref, $
     ADD_ONECOUNT_CURVE=add_oneCount_curve, $
     FIT_EACH_ANGLE=fit_each_angle, $
     ADD_FITPARAMS_TEXT=add_fitParams_text, $
     ADD_ANGLE_LABEL=add_angle_label, $
     FIT2D__ADD_BOUNDARIES=fit2D__add_boundaries, $
     _REF_EXTRA=e

  IF KEYWORD_SET(just_diff_eFlux) THEN RETURN

  KAPPA_FIT2D__LOOP,diff_eFlux,times,dEF_oneCount, $
                    BOUNDS=bounds, $
                    ESTFACS=estFacs, $
                    FIT1D__AVERAGE_OVER_ANGLERANGE=fit1d__average_over_angleRange, $
                    FIT1D__SKIP_BAD_FITS=fit1D__skip_bad_fits, $
                    FIT1D__SHOW_AND_PROMPT=fit1d__show_and_prompt, $
                    FIT1D__USER_PROMPT_ON_FAIL=fit1D_fail__user_prompt, $
                    FIT1D__SAVE_PLOTSLICES=fit1D__save_plotSlices, $
                    FIT2D__SHOW_AND_PROMPT__EACH_CANDIDATE=fit2d__show_each_candidate, $
                    FIT2D__SHOW_ONLY_DATA=fit2D__show_only_data, $
                    FIT2D__PA_ZRANGE=fit2D__PA_zRange, $
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
                    TXTOUTPUTDIR=txtOutputDir,$
                    DEBUG__SKIP_TO_THIS_TIME=debug__skip_to_this_time, $
                    DEBUG__BREAK_ON_THIS_TIME=debug__break_on_this_time

  PRINT,'DONE!'

END
