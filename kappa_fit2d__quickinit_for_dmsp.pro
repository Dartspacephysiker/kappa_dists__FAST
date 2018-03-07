
  @common__kappa_fit2d_structs.pro

  eeb_or_ees   = 'ees'

  @kappa_fitter__defaults.pro

  only_1D_fits                       = 1
  fit1D__nFlux                       = 1
  add_oneCount_curve                 = 0
  fit1D__sourceCone_energy_spectrum  = 1
  estimate_A_from_data               = 0
  dont_print_fitinfo                 = 0
  fit__linear_energy_shift           = 1
  
  n_below_peak                       = -1

  INIT__KAPPA_EFLUX_FIT1D_OR_FIT2D, $
     /FOR_DMSP, $
     IN_DMSP=dmsp, $
     T1=t1, $
     T2=t2, $
     LOAD_DIFF_EFLUX_FILE=load_diff_eFlux_file ,$
     IN_DIFF_EFLUX_FILE=diff_eFlux_file, $
     LOAD_DIR=loadDir, $
     SAVE_DIFF_EFLUX_TO_FILE=save_diff_eFlux_to_file, $
     KF__CURVEFIT_OPT=KF2D__curveFit_opt, $
     KF__SDTDATA_OPT=KF2D__SDTData_opt, $
     KF__PLOT_OPT=KF2D__Plot_opt, $
     KF__STRINGS=KF2D__strings, $
     DIFF_EFLUX=diff_eFlux, $
     DEF_ONECOUNT=dEF_oneCount, $
     UPGOING=upgoing, $
     TIMES=times, $
     SDT_TIME_INDS=bounds, $
     FIT__LINEAR_ENERGY_SHIFT=fit__linear_energy_shift, $
     FIT__JE_OVER_E=fit__JE_over_E, $
     FIT__LES__TAKE_STOCK_OF_RB=fit__LES__take_stock_of_RB, $
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
     ;; FIT2D__EXCLUDE_LCA_FROM_DENSCALC=fit2D__exclude_lca_from_densCalc, $
     FIT2D__DISABLE_BFUNC=fit2D__disable_bFunc, $
     FIT2D__EXTEND_FITSTRUCT_ERANGE=fit2D__extend_fitStruct_eRange, $
     FIT2D__NFLUX=fit2D__nFlux, $
     N_ENERGIES_BELOW_PEAK=n_below_peak, $
     N_ENERGIES_ABOVE_PEAK=n_above_peak, $
     N_BELOW_PEAK2D=n_below_peak2D, $
     N_ABOVE_PEAK2D=n_above_peak2D, $
     TRIM_ENERGIES_BELOW_PEAK=trim_energies_below_peak, $
     MIN_PEAK_ENERGY=min_peak_energy, $
     MAX_PEAK_ENERGY=max_peak_energy, $
     PEAK_ENERGY__START_AT_HIGHE=peak_energy__start_at_highE, $
     PHI__USE_ENERGY_BEFORE_PEAK=phi__use_energy_before_peak, $
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
     MANUAL_ANGLE_CORRECTION=manual_angle_correction, $
     ;; ELECTRON_LOSSCONE_ANGLE=electron_lca, $
     FIT2D__DENSITY_ANGLERANGE=fit2D__density_angleRange, $
     FIT2D__TEMPERATURE_ANGLERANGE=fit2D__temperature_angleRange, $
     FIT2D__FACONDUCTANCE_ANGLERANGE=fit2D__faConductance_angleRange, $
     FIT2D__ESTIMATE_DENS_ARANGE_FROM_DIST=fit2D__estimate_sourceCone_from_dist, $
     FIT2D__TEMPERATURE_TYPE=fit2D__temperature_type, $
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
     OUT_SC_POT=sc_pot, $
     _REF_EXTRA=e
