;2016/05/13
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   ;;From Kivelson & Russell, Table 2.2 "Properties of Typical Plasmas"
;;   ;;"Magnetosphere"
;;   T_def                                             = 1000L    ;eV
;;   n_def                                             = 1L       ;cm^-3
;;   E_b_def                                           = 500L     ;bulk energy, eV

;;   IF N_ELEMENTS(kappa)       EQ 0 THEN kappa        = 5        ;a guess
;;   IF N_ELEMENTS(T)           EQ 0 THEN T            = T_def    ;temperature guess in eV
;;   IF N_ELEMENTS(n_est)       EQ 0 THEN n_est        = n_def    ;n guess in cm^-3
;;   IF N_ELEMENTS(peak_energy) EQ 0 THEN peak_energy  = E_b_def  ;peak energy guess, eV    

PRO KAPPA_EFLUX_FIT, $
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
   FIT_EACH__START_FROM_FIELDALIGNED=start_from_fieldaligned, $
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
   N_ENERGIES_AFTER_PEAK=n_after_peak, $
   CHECK_FOR_HIGHER_FLUX_PEAKS__SET_CORRESPONDING_PEAK_ENERGY=check_for_higher_flux_peaks__set_corresponding_peak_energy, $
   FIT_TOLERANCE=fit_tol, $
   MAX_ITERATIONS=max_iter, $
   ADD_GAUSSIAN_ESTIMATE=add_gaussian_estimate, $
   USE_SDT_GAUSSIAN_FIT=use_SDT_Gaussian_fit, $
   ADD_ONECOUNT_CURVE=add_oneCount_curve, $
   ADD_FITPARAMS_TEXT=add_fitParams_text, $
   ADD_ANGLE_LABEL=add_angle_label, $
   ;; ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
   ELECTRON_ANGLERANGE=electron_angleRange, $
   GET_MASS_AND_DT=get_mass_and_dt, $
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

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Defaults
  KAPPA_FIT_DEFAULTS, $
     KAPPA=kappa, $
     BOUNDS=bounds, $
     ;; DO_ALL_TIMES=do_all_times, $
     FIT_EACH_ANGLE=fit_each_angle, $
     EEB_OR_EES=eeb_or_ees, $
     SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
     ROUTINE=routine, $
     TRIM_ENERGIES_BELOW_PEAK=trim_energies_below_peak, $
     N_ENERGIES_BELOW_PEAK=n_below_peak, $
     N_ENERGIES_AFTER_PEAK=n_after_peak, $
     ENERGY_ELECTRONS=energy_electrons, $
     ESTIMATE_A_FROM_DATA=estimate_A_from_data

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Get data
  ;; IF N_ELEMENTS(eSpec) EQ 0 OR N_ELEMENTS(diff_eFlux) EQ 0 THEN BEGIN
  IF N_ELEMENTS(diff_eFlux) EQ 0 THEN BEGIN
     GET_LOSSCONE_AND_EFLUX_DATA,T1=t1,T2=t2, $
                                 LOAD_DAT_FROM_FILE=loadFile, $
                                 EEB_OR_EES=eeb_or_ees, $
                                 DIFF_EFLUX=diff_eFlux, $
                                 SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                                 OUT_ORB=orb, $
                                 OUT_ANGLERANGE=e_angle, $
                                 ;; ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
                                 FIT_EACH_ANGLE=fit_each_angle, $
                                 CUSTOM_E_ANGLERANGE=electron_angleRange, $
                                 ANGLESTR=angleStr, $
                                 ESPECUNITS=eSpecUnits, $
                                 ELECTRON_ENERGY_LIMS=energy_electrons

     ;; GET_LOSSCONE_EN_SPEC_AND_NFLUX_DATA,T1=t1,T2=t2, $
     ;;                                     LOAD_DAT_FROM_FILE=loadFile, $
     ;;                                     EEB_OR_EES=eeb_or_ees, $
     ;;                                     ;; EN_SPEC=eSpec, $
     ;;                                     SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
     ;;                                     DIFF_EFLUX=diff_eFlux, $
     ;;                                     TRY_SYNTHETIC_SDT_STRUCT=try_synthetic_SDT_struct, $
     ;;                                     SYNTH_DIFF_EFLUX=diff_eFluxSDT, $
     ;;                                     ;; JE_EN=je_en, $
     ;;                                     OUT_ORB=orb, $
     ;;                                     OUT_ANGLERANGE=e_angle, $
     ;;                                     ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
     ;;                                     FIT_EACH_ANGLE=fit_each_angle, $
     ;;                                     CUSTOM_E_ANGLERANGE=electron_angleRange, $
     ;;                                     ANGLESTR=angleStr, $
     ;;                                     ELECTRON_ENERGY_LIMS=energy_electrons, $
     ;;                                     /GET_MASS_AND_DT, $
     ;;                                     OUT_MASS=mass

     ;; IF ~KEYWORD_SET(only_fit_fieldaligned_angle) AND ~KEYWORD_SET(fit_each_angle) THEN BEGIN
     IF KEYWORD_SET(treat_angular_range_as_fieldaligned) THEN BEGIN
        REDUCE_DIFF_EFLUX,diff_eFlux
     ENDIF

     orbStr                            = STRCOMPRESS(orb,/REMOVE_ALL)
  ENDIF

  IF KEYWORD_SET(do_all_times) THEN BEGIN
     PRINT,"Doing all times ..."
     nBounds                           = N_ELEMENTS(diff_eFlux.time)
     bounds                            = INDGEN(nBounds)
  END

  ;;Onecount curve?
  IF KEYWORD_SET(add_oneCount_curve) THEN BEGIN
     GET_ONECOUNT_DIFF_EFLUX_CURVE,t1,t2, $
                                   EEB_OR_EES=eeb_or_ees, $
                                   SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                                   ;; TRY_SYNTHETIC_SDT_STRUCT=try_synthetic_SDT_struct, $
                                   SDT_NAME=dEF_oneCount_name, $
                                   ANGLE=e_angle, $
                                   ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
                                   FIT_EACH_ANGLE=fit_each_angle, $
                                   OUT_ONEDAT=out_oneDat, $
                                   QUIET=quiet

     GET_DATA,dEF_oneCount_name,DATA=dEF_oneCount

     IF ~KEYWORD_SET(only_fit_fieldaligned_angle) THEN BEGIN
        IF ~KEYWORD_SET(fit_each_angle) THEN BEGIN
           REDUCE_DIFF_EFLUX,dEF_oneCount
        ENDIF

     ENDIF
  ENDIF

  ;;Times and strings
  times                                = diff_eFlux.time
  strings                              = {today:GET_TODAY_STRING(/DO_YYYYMMDD_FMT), $
                                          eeb_or_ees:eeb_or_ees, $
                                          avgStr:KEYWORD_SET(spectra_average_interval) ? $
                                          STRING(FORMAT='("--",I0,"_avgs")',spectra_average_interval) : '' , $
                                          orbStr:orbStr, $
                                          orbDate:STRMID(TIME_TO_STR(diff_eFlux.time),0,10), $
                                          yearStr:STRMID(TIME_TO_STR(times[0],/MSEC),0,10), $
                                          timeStrs:STRMID(TIME_TO_STR(times,/MSEC),11,11), $
                                          timeFNStrs:STRMID(TIME_TO_STR(times,/MSEC),11,11), $
                                          plotTimes:STRMID(TIME_TO_STR(diff_eFlux.time,/MSEC),11,12), $
                                          angleStr:angleStr}
  strings.timeFNStrs                   = strings.timeFNStrs.REPLACE(':', '_')
  strings.timeFNStrs                   = strings.timeFNStrs.REPLACE('.', '__')


  ;;Loop over provided indices, plot data as well as fit, and optionally save
  routine                              = 'get_fa_'+eeb_or_ees
  IF KEYWORD_SET(spectra_average_interval) THEN routine += '_ts'

 ;;  IF KEYWORD_SET(add_oneCount_curve) THEN BEGIN
 ;;     CASE 1 OF
 ;;        KEYWORD_SET(fit_each_angle): BEGIN
 ;;           dEF_oneCountMod                = REFORM(dEF_oneCount.data[*,*,bounds])
 ;;        END
 ;;        ELSE: BEGIN
 ;;           dEF_oneCountMod                = dEF_oneCount.y[bounds,*]
 ;;        END
 ;;     ENDCASE
 ;;     ;; dEF_oneCountMod                   = dEF_oneCount.data[bounds,*]
 ;;     yMin                              = MIN(dEF_oneCountMod[WHERE(dEF_oneCountMod GT 0)])
 ;;     yMin                              = 10.^(FLOOR(ALOG10(yMin)))

 ;;     IF KEYWORD_SET(try_synthetic_SDT_struct) THEN BEGIN
 ;;        dEF_oneCountModSDT             = dEF_oneCountSDT.data[bounds,*]
 ;;     ENDIF
 ;;  ENDIF ELSE BEGIN
 ;; ;; ELSE BEGIN
 ;;     CASE 1 OF
 ;;        KEYWORD_SET(fit_each_angle): BEGIN
 ;;           yMin                        = MIN(diff_eFlux.data[WHERE(diff_eFlux.data GT 0)])
 ;;        END
 ;;        ELSE: BEGIN
 ;;           yMin                        = MIN(diff_eFlux.y[WHERE(diff_eFlux.y GT 0)])
 ;;        END
 ;;     ENDCASE
 ;;     ;; ENDELSE
 ;;     IF KEYWORD_SET(try_synthetic_SDT_struct) THEN BEGIN
 ;;        yMinSDT                        = MIN(diff_eFluxSDT.data[WHERE(diff_eFluxSDT.data GT 0)])
 ;;     ENDIF
 ;;  ENDELSE


     CASE 1 OF
        KEYWORD_SET(fit_each_angle): BEGIN
                 ;; energies                          = TRANSPOSE(diff_eFlux.x)
                 ;; data                              = TRANSPOSE(diff_eFlux.y)
                 ;; oneCount_data                     = KEYWORD_SET(add_oneCount_curve) ? TRANSPOSE(dEF_oneCount.y) : !NULL
                 ;; angles                            = diff_eFlux.angles
        END
        ELSE: BEGIN
           CASE 1 OF
              ;; KEYWORD_SET(try_synthetic_SDT_struct): BEGIN 
              ;;    energiesSDT                       = TRANSPOSE(diff_eFluxSDT.energy)
              ;;    dataSDT                           = TRANSPOSE(diff_eFluxSDT.data)
              ;;    oneCount_dataSDT                  = KEYWORD_SET(add_oneCount_curve) ? TRANSPOSE(dEF_oneCountSDT.data) : !NULL
              ;;    anglesSDT                         = diff_eFluxSDT.theta

              ;;    energies                       = TRANSPOSE(diff_eFluxSDT.energy)
              ;;    data                           = TRANSPOSE(diff_eFluxSDT.data)
              ;;    oneCount_data                  = KEYWORD_SET(add_oneCount_curve) ? TRANSPOSE(dEF_oneCountSDT.data) : !NULL
              ;;    angles                         = diff_eFluxSDT.theta
              ;; END
              ELSE: BEGIN
                 energies                          = TRANSPOSE(diff_eFlux.x)
                 data                              = TRANSPOSE(diff_eFlux.y)
                 oneCount_data                     = KEYWORD_SET(add_oneCount_curve) ? TRANSPOSE(dEF_oneCount.y) : !NULL
                 angles                            = diff_eFlux.angles
              END
           ENDCASE
        END
     ENDCASE  


  KAPPA_FIT__LOOP,times,energies,data,oneCount_data,angles, $
                  USING_SDT_DATA=0, $
                  KAPPA=kappa, $
                  BOUNDS=bounds, $
                  EEB_OR_EES=eeb_or_ees, $
                  SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                  ROUTINE=routine, $
                  ADD_GAUSSIAN_ESTIMATE=add_gaussian_estimate, $
                  USE_SDT_GAUSSIAN_FIT=use_SDT_Gaussian_fit, $
                  ESTFACS=estFacs, $
                  TRIM_ENERGIES_BELOW_PEAK=trim_energies_below_peak, $
                  DONT_FIT_BELOW_THRESH_VALUE=dont_fit_below_thresh_value, $
                  N_ENERGIES_BELOW_PEAK=n_below_peak, $
                  N_ENERGIES_AFTER_PEAK=n_after_peak, $
                  ENERGY_ELECTRONS=energy_electrons, $
                  ESTIMATE_A_FROM_DATA=estimate_A_from_data, $
                  DONT_PRINT_ESTIMATES=dont_print_estimates, $
                  E_ANGLE=e_angle, $
                  BULK_OFFSET=bulk_offset, $
                  DONT_TAKE_STOCK_OF_BULKANGLE=dont_take_stock_of_bulkangle, $
                  TREAT_FIELDALIGNED_AS_BULK=treat_fieldaligned_as_bulk, $
                  CHECK_FOR_HIGHER_FLUX_PEAKS=check_for_higher_flux_peaks__set_corresponding_peak_energy, $
                  MIN_PEAK_ENERGY=min_peak_energy, $
                  STRINGS=strings, $
                  FIT_TOLERANCE=fit_tol, $
                  MAX_ITERATIONS=max_iter, $
                  ADD_ONECOUNT_CURVE=add_oneCount_curve, $
                  ADD_FITPARAMS_TEXT=add_fitParams_text, $
                  ADD_ANGLE_LABEL=add_angle_label, $
                  ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
                  FIT_EACH_ANGLE=fit_each_angle, $
                  FIT_EACH__AVERAGE_OVER_ANGLERANGE=fit_each__average_over_angleRange, $
                  FIT_EACH__SYNTH_SDT_STRUCT=synthPackage, $
                  FIT_EACH__SKIP_BAD_FITS=fit_each__skip_bad_fits, $
                  FIT_EACH__START_FROM_FIELDALIGNED=start_from_fieldaligned, $
                  FIT_EACH__SHOW_AND_PROMPT=fit_each__show_and_prompt, $
                  FIT_FAIL__USER_PROMPT=fit_fail__user_prompt, $
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
                  ONLY_DENS_ESTIMATES=only_dens_estimates, $
                  OUT_FITTED_PARAMS=out_kappaParams, $
                  OUT_FITTED_GAUSS_PARAMS=out_gaussParams, $
                  OUT_KAPPA_FIT_STRUCTS=kappaFits, $
                  OUT_GAUSS_FIT_STRUCTS=gaussFits, $
                  ADD_FULL_FITS=add_full_fits, $
                  OUT_ERANGE_PEAK=out_eRange_peak, $
                  OUT_PARAMSTR=out_paramStr, $
                  TXTOUTPUTDIR=txtOutputDir

  ;; IF KEYWORD_SET(try_synthetic_SDT_struct) THEN BEGIN 
  ;;    IF ARG_PRESENT(synthStr_SDT_kappa) THEN BEGIN
  ;;       synthStr_SDT_kappa = diff_eFluxSDT
  ;;       FOR i=0,N_ELEMENTS(synthStr_SDT_kappa)-1 DO BEGIN
  ;;          synthStr_SDT_kappa.energy[i,*] = TRANSPOSE(kappaFits[i].xFull)
  ;;          synthStr_SDT_kappa.data[i,*] = TRANSPOSE(kappaFits[i].yFull)
  ;;       ENDFOR
  ;;    ENDIF

  ;;    IF ARG_PRESENT(synthStr_SDT_gauss) THEN BEGIN
  ;;       synthStr_SDT_gauss = diff_eFluxSDT
  ;;       FOR i=0,N_ELEMENTS(synthStr_SDT_gauss)-1 DO BEGIN
  ;;          synthStr_SDT_gauss.energy[i,*] = TRANSPOSE(gaussFits[i].xFull)
  ;;          synthStr_SDT_gauss.data[i,*] = TRANSPOSE(gaussFits[i].yFull)
  ;;       ENDFOR
  ;;    ENDIF
  ;; ENDIF

  PRINT,'DONE!'

END
