
;;02/14/17
PRO INIT__KAPPA_EFLUX_FIT1D_OR_FIT2D, $
   KF__CURVEFIT_OPT=KF__curveFit_opt, $
   KF__SDTDATA_OPT=KF__SDTData_opt, $
   KF__PLOT_OPT=KF__Plot_opt, $
   KF__STRINGS=KF__strings, $
   DIFF_EFLUX=diff_eFlux, $
   DEF_ONECOUNT=dEF_oneCount, $
   TIMES=times, $
   SDT_TIME_INDS=bounds, $
   DO_ALL_TIMES=do_all_times, $
   TIME_ARR=time_arr, $
   ADD_ONECOUNT_CURVE=add_oneCount_curve, $
   _REF_EXTRA=e

  COMPILE_OPT IDL2

  KF__Curvefit_opt = INIT_KAPPA_CURVEFIT_OPTIONS( $
                     ONLY_1D_FITS=only_1D_fits, $
                     FIT1D__TOLERANCE=fit_tol, $
                     FIT1D__MAX_ITERATIONS=max_iter, $
                     FIT1D__SOURCECONE_ENERGY_SPECTRUM=fit1D__sourceCone_energy_spectrum, $
                     FIT1D__NFLUX=fit1D__nFlux, $
                     FIT1D__WEIGHTING=fit1D__weighting, $
                     FIT2D__TOLERANCE=fit2d_tol, $
                     FIT2D__MAX_ITERATIONS=fit2D_max_iter, $
                     FIT2D__ONLY_FIT_ELECTRON_ANGLES=fit2D__only_fit_eAngles, $
                     FIT2D__KEEP_WHOLEFIT=fit2D__keep_wholeFit, $
                     FIT2D__WEIGHTING=fit2D__weighting, $
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
                     UNITS=units, $
                     _EXTRA=e)  ;, $
  ;; BULK_OFFSET=bulk_offset)

  ;;SDT data options
  KF__SDTData_opt  = INIT_KAPPA_SDTDATA_OPTIONS( $
                     EEB_OR_EES=eeb_or_ees, $
                     SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                     DO_ALL_TIMES=do_all_times, $
                     ENERGY_ELECTRONS=energy_electrons, $
                     ELECTRON_ANGLERANGE=electron_angleRange, $
                     ELECTRON_LOSSCONE_ANGLE=electron_lca, $
                     FIT2D__DENSITY_ANGLERANGE=fit2D__density_angleRange, $
                     _EXTRA=e)

  ;;Plot options
  KF__Plot_opt     = INIT_KAPPA_PLOT_OPTIONS( $
                     NO_PLOTS=no_plots, $
                     SAVE_FITPLOTS=save_fitplots, $
                     PLOT_FULL_FIT=plot_full_fit, $
                     PLOTDIR=plotDir, $
                     PLOTNAMEPREF=plotNamePref, $
                     ADD_ONECOUNT_CURVE=add_oneCount_curve, $
                     ADD_FITPARAMS_TEXT=add_fitParams_text, $
                     ADD_ANGLE_LABEL=add_angle_label, $
                     FIT2D__ADD_BOUNDARIES=fit2D__add_boundaries, $
                     _EXTRA=e)

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
                              EEB_OR_EES=KF__SDTData_opt.eeb_or_ees, $
                              DIFF_EFLUX=diff_eFlux, $
                              SPECTRA_AVERAGE_INTERVAL=KF__SDTData_opt.spec_avg_intvl, $
                              OUT_ORB=orb, $
                              OUT_ANGLERANGE=e_angle, $
                              /FIT_EACH_ANGLE, $ ;Perma-set because we need all angles here
                              CUSTOM_E_ANGLERANGE=KF__SDTData_opt.electron_angleRange, $
                              ANGLESTR=angleStr, $
                              ESPECUNITS=KF__Curvefit_opt.units, $
                              ELECTRON_ENERGY_LIMS=KF__SDTData_opt.energy_electrons, $
                              SAVE_DIFF_EFLUX_TO_FILE=save_diff_eFlux_to_file, $
                              _EXTRA=e

  orbStr                            = STRCOMPRESS(orb,/REMOVE_ALL)
  ;; ENDIF ELSE BEGIN
  ;;    orbStr                            = '???'
  ;; ENDELSE

  IF SIZE(diff_eFlux,/TYPE) NE 8 THEN BEGIN
     PRINT,"Couldn't get diff_eFlux! Quitting ..."
     RETURN
  ENDIF

  nBounds                           = N_ELEMENTS(diff_eFlux.time)
  CASE 1 OF
     KEYWORD_SET(do_all_times): BEGIN
        PRINT,"Doing all times ..."
        bounds                      = INDGEN(nBounds)
     END
     KEYWORD_SET(time_arr): BEGIN
        dims = SIZE(time_arr,/DIMENSIONS)
        CASE N_ELEMENTS(dims) OF
           1: BEGIN
           END
           2: BEGIN
              IF dims[0] NE 2 THEN STOP
              bounds = !NULL
              FOR k=0,dims[1] DO BEGIN
                 extr_i = !NULL
                 inds   = VALUE_CLOSEST2(diff_eFlux.time,time_arr[*,k],EXTREME_I=extr_i)
                 IF (extr_i[0] NE -1) OR $
                    ( (WHERE((inds LT 0) OR (inds GE nBounds)))[0] NE -1) $
                 THEN STOP
                 bounds = [bounds,[inds[0]:inds[1]]]
              ENDFOR
           END
           ELSE: BEGIN
              STOP
           ENDELSE
        ENDCASE
     END
  ENDCASE

  ;;Onecount curve?
  IF KEYWORD_SET(KF__Plot_opt.add_oneCount_curve) THEN BEGIN
     GET_ONECOUNT_DIFF_EFLUX_CURVE,t1,t2, $
                                   ;; LOAD_DAT_FROM_FILE=loadFile, $ ;;handled through proto
                                   EEB_OR_EES=KF__SDTData_opt.EEB_or_EES, $
                                   SPECTRA_AVERAGE_INTERVAL=KF__SDTData_opt.spec_avg_intvl, $
                                   IN_PROTOSTRUCT=diff_eFlux, $
                                   SDT_NAME=dEF_oneCount_name, $
                                   ANGLE=e_angle, $
                                   ESPECUNITS=KF__Curvefit_opt.units, $
                                   ;; ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
                                   /FIT_EACH_ANGLE, $ ;Perma-set because we do all angles for 2D fitting
                                   OUT_ONEDAT=out_oneDat, $
                                   DEF_ONECOUNT=dEF_oneCount, $
                                   QUIET=quiet

     IF KEYWORD_SET(old_mode) THEN BEGIN
        GET_DATA,dEF_oneCount_name,DATA=dEF_oneCount
     ENDIF
  ENDIF

  ;;Times and strings
  times                              = diff_eFlux.time
  KF__strings                        = INIT_KAPPA_STRING_STRUCT(diff_eFlux, $
                                                                  times, $
                                                                  orbStr, $
                                                                  angleStr, $
                                                                  KF__SDTData_opt)


END