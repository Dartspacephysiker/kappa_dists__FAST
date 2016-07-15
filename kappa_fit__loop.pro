PRO KAPPA_FIT__LOOP,times,energies,data,oneCount_data,angles, $
                    USING_SDT_DATA=using_sdt_data, $
                    KAPPA=kappa, $
                    BOUNDS=bounds, $
                    EEB_OR_EES=eeb_or_ees, $
                    SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                    ROUTINE=routine, $
                    ESTFACS=estFacs, $
                    TRIM_ENERGIES_BELOW_PEAK=trim_energies_below_peak, $
                    DONT_FIT_BELOW_THRESH_VALUE=dont_fit_below_thresh_value, $
                    N_ENERGIES_BELOW_PEAK=n_below_peak, $
                    N_ENERGIES_AFTER_PEAK=n_after_peak, $
                    ENERGY_ELECTRONS=energy_electrons, $
                    ESTIMATE_A_FROM_DATA=estimate_A_from_data, $
                    E_ANGLE=e_angle, $
                    BULK_OFFSET=bulk_offset, $
                    CHECK_FOR_HIGHER_FLUX_PEAKS=check_for_higher_flux_peaks__set_corresponding_peak_energy, $
                    MIN_PEAK_ENERGY=min_peak_energy, $
                    STRINGS=strings, $
                    FIT_TOLERANCE=fit_tol, $
                    MAX_ITERATIONS=max_iter, $
                    ADD_GAUSSIAN_ESTIMATE=add_gaussian_estimate, $
                    USE_SDT_GAUSSIAN_FIT=use_SDT_Gaussian_fit, $
                    ADD_ONECOUNT_CURVE=add_oneCount_curve, $
                    ADD_FITPARAMS_TEXT=add_fitParams_text, $
                    ADD_ANGLE_LABEL=add_angle_label, $
                    ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
                    FIT_EACH_ANGLE=fit_each_angle, $
                    FIT_EACH__AVERAGE_OVER_ANGLERANGE=fit_each__average_over_angleRange, $
                    FIT_EACH__SYNTH_SDT_STRUCT=synthPackage, $
                    EACH_ANGLE_DATA=diff_eFlux, $
                    EACH_ANGLE_ONECOUNT_DATA=dEF_oneCount, $
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
                    ;; OUT_DENS_FILEPREF=out_dens_filePref, $
                    ONLY_DENS_ESTIMATES=only_dens_estimates, $
                    OUT_FITTED_PARAMS=out_fitted_params, $
                    OUT_FITTED_GAUSS_PARAMS=out_fitted_Gauss_params, $
                    OUT_KAPPA_FIT_STRUCTS=out_kappa_fit_structs, $
                    OUT_GAUSS_FIT_STRUCTS=out_gauss_fit_structs, $
                    ADD_FULL_FITS=add_full_fits, $
                    OUT_ERANGE_PEAK=out_eRange_peak, $
                    OUT_PARAMSTR=out_paramStr, $
                    TXTOUTPUTDIR=txtOutputDir
  
  COMPILE_OPT idl2

  CASE 1 OF
     KEYWORD_SET(fit_each_angle): BEGIN
        KAPPA_FIT__LOOP__EACH_ANGLE,times,energies,data,oneCount_data,angles, $
                                    diff_eFlux,dEF_oneCount, $
                                    USING_SDT_DATA=using_sdt_data, $
                                    KAPPA=kappa, $
                                    BOUNDS=bounds, $
                                    EEB_OR_EES=eeb_or_ees, $
                                    SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                                    ROUTINE=routine, $
                                    ESTFACS=estFacs, $
                                    TRIM_ENERGIES_BELOW_PEAK=trim_energies_below_peak, $
                                    DONT_FIT_BELOW_THRESH_VALUE=dont_fit_below_thresh_value, $
                                    N_ENERGIES_BELOW_PEAK=n_below_peak, $
                                    N_ENERGIES_AFTER_PEAK=n_after_peak, $
                                    ENERGY_ELECTRONS=energy_electrons, $
                                    ESTIMATE_A_FROM_DATA=estimate_A_from_data, $
                                    E_ANGLE=e_angle, $
                                    BULK_OFFSET=bulk_offset, $
                                    CHECK_FOR_HIGHER_FLUX_PEAKS=check_for_higher_flux_peaks__set_corresponding_peak_energy, $
                                    MIN_PEAK_ENERGY=min_peak_energy, $
                                    STRINGS=strings, $
                                    FIT_TOLERANCE=fit_tol, $
                                    MAX_ITERATIONS=max_iter, $
                                    ADD_GAUSSIAN_ESTIMATE=add_gaussian_estimate, $
                                    USE_SDT_GAUSSIAN_FIT=use_SDT_Gaussian_fit, $
                                    ADD_ONECOUNT_CURVE=add_oneCount_curve, $
                                    ADD_FITPARAMS_TEXT=add_fitParams_text, $
                                    ADD_ANGLE_LABEL=add_angle_label, $
                                    ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
                                    FIT_EACH_ANGLE=fit_each_angle, $
                                    FIT_EACH__AVERAGE_OVER_ANGLERANGE=fit_each__average_over_angleRange, $
                                    FIT_EACH__SYNTH_SDT_STRUCT=synthPackage, $
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
                                    ;; OUT_DENS_FILEPREF=out_dens_filePref, $
                                    ONLY_DENS_ESTIMATES=only_dens_estimates, $
                                    OUT_FITTED_PARAMS=out_fitted_params, $
                                    OUT_FITTED_GAUSS_PARAMS=out_fitted_Gauss_params, $
                                    OUT_KAPPA_FIT_STRUCTS=out_kappa_fit_structs, $
                                    OUT_GAUSS_FIT_STRUCTS=out_gauss_fit_structs, $
                                    ADD_FULL_FITS=add_full_fits, $
                                    OUT_ERANGE_PEAK=out_eRange_peak, $
                                    OUT_PARAMSTR=out_paramStr, $
                                    TXTOUTPUTDIR=txtOutputDir
     END
     ELSE: BEGIN

        KAPPA_FIT__LOOP__ONE_ANGLE,times,energies,data,oneCount_data,angles, $
                                   USING_SDT_DATA=using_sdt_data, $
                                   KAPPA=kappa, $
                                   BOUNDS=bounds, $
                                   EEB_OR_EES=eeb_or_ees, $
                                   SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                                   ROUTINE=routine, $
                                   ESTFACS=estFacs, $
                                   TRIM_ENERGIES_BELOW_PEAK=trim_energies_below_peak, $
                                   DONT_FIT_BELOW_THRESH_VALUE=dont_fit_below_thresh_value, $
                                   N_ENERGIES_BELOW_PEAK=n_below_peak, $
                                   N_ENERGIES_AFTER_PEAK=n_after_peak, $
                                   ENERGY_ELECTRONS=energy_electrons, $
                                   ESTIMATE_A_FROM_DATA=estimate_A_from_data, $
                                   E_ANGLE=e_angle, $
                                   BULK_OFFSET=bulk_offset, $
                                   CHECK_FOR_HIGHER_FLUX_PEAKS=check_for_higher_flux_peaks__set_corresponding_peak_energy, $
                                   MIN_PEAK_ENERGY=min_peak_energy, $
                                   STRINGS=strings, $
                                   FIT_TOLERANCE=fit_tol, $
                                   MAX_ITERATIONS=max_iter, $
                                   ADD_GAUSSIAN_ESTIMATE=add_gaussian_estimate, $
                                   USE_SDT_GAUSSIAN_FIT=use_SDT_Gaussian_fit, $
                                   ADD_ONECOUNT_CURVE=add_oneCount_curve, $
                                   ADD_FITPARAMS_TEXT=add_fitParams_text, $
                                   ADD_ANGLE_LABEL=add_angle_label, $
                                   ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
                                   FIT_EACH_ANGLE=fit_each_angle, $
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
                                   ;; OUT_DENS_FILEPREF=out_dens_filePref, $
                                   ONLY_DENS_ESTIMATES=only_dens_estimates, $
                                   OUT_FITTED_PARAMS=out_fitted_params, $
                                   OUT_FITTED_GAUSS_PARAMS=out_fitted_Gauss_params, $
                                   OUT_KAPPA_FIT_STRUCTS=out_kappa_fit_structs, $
                                   OUT_GAUSS_FIT_STRUCTS=out_gauss_fit_structs, $
                                   ADD_FULL_FITS=add_full_fits, $
                                   OUT_ERANGE_PEAK=out_eRange_peak, $
                                   OUT_PARAMSTR=out_paramStr, $
                                   TXTOUTPUTDIR=txtOutputDir
     END
  ENDCASE


END