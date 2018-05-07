;;10/08/16
PRO KAPPA_FITTER_BLACKBOX,orbit, $
                          ELECTRON_SOURCECONEANGLE=electron_angleRange, $
                          ;; ELECTRON_LOSSCONEANGLE=electron_lca, $
                          MANUAL_ANGLE_CORRECTION=manual_angle_correction, $
                          ENERGY_ELECTRONS=energy_electrons, $
                          ENERGY_ELECTRON_TBOUNDS=energy_electron_tBounds, $
                          JUST_DIFF_EFLUX=just_diff_eFlux, $
                          DIFF_EFLUX=diff_eFlux, $
                          DEF_ONECOUNT=dEF_oneCount, $
                          UPGOING=upgoing, $
                          MIN_PEAK_ENERGY=min_peak_energy, $
                          MAX_PEAK_ENERGY=max_peak_energy, $
                          PEAK_ENERGY__START_AT_HIGHE=peak_energy__start_at_highE, $
                          PHI__USE_ENERGY_BEFORE_PEAK=phi__use_energy_before_peak, $
                          EEB_OR_EES=eeb_or_ees, $
                          ENFORCE_DIFF_EFLUX_SRATE=enforce_diff_eFlux_sRate, $
                          SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                          SOUTH=south, $
                          CHI2_THRESHOLD=chi2_thresh, $
                          CHI2_OVER_DOF_THRESHOLD=chi2_over_dof_thresh, $
                          HIGHDENSITY_THRESHOLD=highDens_thresh, $
                          LOWDENSITY_THRESHOLD=lowDens_thresh, $
                          DIFFEFLUX_THRESHOLD=diffEflux_thresh, $
                          N_PEAKS_ABOVE_DEF_THRESHOLD=nPkAbove_dEF_thresh, $
                          RESTORE_FITFILE=restore_fitFile, $
                          T1STR=t1Str, $
                          T2STR=t2Str, $
                          SHOW_POST_PLOTS=show_post_plots, $
                          FIT__LINEAR_ENERGY_SHIFT=fit__linear_energy_shift, $
                          FIT__JE_OVER_E=fit__JE_over_E, $
                          FIT__LES__TAKE_STOCK_OF_RB=fit__LES__take_stock_of_RB, $
                          ONLY_1D_FITS=only_1D_fits, $
                          FIT1D__N_BELOW_PEAK=n_below_peak1D, $
                          FIT1D__N_ABOVE_PEAK=n_above_peak1D, $
                          FIT1D__SOURCECONE_ENERGY_SPECTRUM=fit1D__sourceCone_energy_spectrum, $
                          FIT1D__NFLUX=fit1D__nFlux, $
                          FIT1D__WEIGHTING=fit1D__weighting, $
                          FIT1D__CLAMPTEMPERATURE=fit1D__clampTemperature, $
                          FIT1D__CLAMPDENSITY=fit1D__clampDensity, $
                          FIT1D__SAVE_PLOTSLICES=fit1D__save_plotSlices, $
                          FIT1D__SAVE_EVERY_NTH_PLOT=fit1D__save_every_nth_plot, $
                          FIT1D__SAVE_IF_KAPPA_BELOW=fit1D__save_if_kappa_below, $
                          FIT1D__COMBINE_PLOTSLICES_IN_PDF=fit1D__combine_plotslices_in_PDF, $
                          FIT2D__N_BELOW_PEAK=n_below_peak2D, $
                          FIT2D__N_ABOVE_PEAK=n_above_peak2D, $
                          FIT2D__EXTEND_FITSTRUCT_ERANGE=fit2D__extend_fitStruct_eRange, $
                          FIT2D__NFLUX=fit2D__nFlux, $
                          FIT2D__WEIGHTING=fit2D__weighting, $
                          FIT2D__CLAMPTEMPERATURE=fit2D__clampTemperature, $
                          FIT2D__CLAMPDENSITY=fit2D__clampDensity, $
                          FIT2D__SHOW_EACH_CANDIDATE=fit2D__show_each_candidate, $
                          FIT2D__SHOW_ONLY_DATA=fit2D__show_only_data, $
                          FIT2D__PA_ZRANGE=fit2D__PA_zRange, $
                          FIT2D__SAVE_ALL_PLOTS=fit2D__save_all_plots, $
                          FIT2D__DENSITY_ANGLERANGE=fit2D__density_angleRange, $
                          FIT2D__TEMPERATURE_ANGLERANGE=fit2D__temperature_angleRange, $
                          FIT2D__FACONDUCTANCE_ANGLERANGE=fit2D__faConductance_angleRange, $
                          FIT2D__ESTIMATE_DENS_ARANGE_FROM_DIST=fit2D__estimate_sourceCone_from_dist, $
                          FIT2D__TEMPERATURE_TYPE=fit2D__temperature_type, $
                          ADD_ONECOUNT_CURVE=add_oneCount_curve, $
                          SAVE_POSTKAPPA_PLOTS=save_postKappa_plot, $
                          ADD_FITPARAMS_TEXT=add_fitParams_text, $
                          SAVEKAPPA_BONUSPREF=bonusPref, $
                          CLOSE_KAPPAPLOTS_AFTER_SAVE=close_kp_after_save, $
                          PLOTDIR=plotDir, $
                          SHOW_STRANGEWAY_SUMMARY=show_Strangeway_summary, $
                          SWAY__SAVE_PS=sway__save_ps, $
                          SWAY__SAVE_PNG=sway__save_png, $
                          SWAY__ADD_KAPPA_PANEL=sway__add_kappa_panel, $
                          SWAY__ADD_CHARE_PANEL=sway__add_chare_panel, $
                          SWAY__ADD_NEWELL_PANEL=sway__add_Newell_panel, $
                          SWAY__SAVE_NEWELL_DATA=sway__save_Newell_data, $
                          SWAY__NEWELL_INTERP=sway__Newell_interp, $
                          SWAY__LOG_KAPPAPLOT=sway__log_kappaPlot, $
                          SWAY__ADD_IU_POT=sway__add_iu_pot, $
                          SWAY__SPECTROGRAM_UNITS=sway__spectrogram_units, $
                          SWAY__CHECKFORIONBEAMS=sway__checkForIonBeams, $
                          SHOW_KAPPA_SUMMARY=show_kappa_summary, $
                          KSUM__EANGLE=kSum__eAngle, $
                          KSUM__SAVE_PS=kSum__save_ps, $
                          KSUM__SAVE_PNG=kSum__save_png, $
                          KSUM__CONV_DESPECS_TO_NEWELL_INTERP=kSum__convert_to_Newell_interp, $
                          KSUM__ADD_CHI2_LINE=kSum__add_chi2_line, $
                          KSUM__CHI2BOUNDS=kSum__chi2Bounds, $
                          KSUM__ADD_MEASURED_T_AND_N=kSum__add_meas_T_and_N, $
                          KSUM__ADD_ONLY_MEAS_N=kSum__add_only_meas_N, $
                          KSUM__INCLUDE_ELECTRON_PA_SPEC=kSum__include_electron_pa_spec, $
                          KSUM__GRL=kSum__GRL, $
                          KSUM__OPLOT_POT=kSum__oPlot_pot, $
                          KSUM__SPECTROGRAM_UNITS=kSum__spectrogram_units, $
                          KSUM__ADD_PARM_ERRORS_FROM_FILE=kSum__add_parm_errors_from_file, $
                          KSUM__ADD_PARM_ERRORS__NROLLS=kSum__add_parm_errors__nRolls, $
                          KSUM__ADD_PARM_ERRORS__USE_MOST_PROB=kSum__add_parm_errors__use_most_prob, $
                          KSUM__TIMEBAR_FROM_ION_BEAMS=kSum__timeBar_from_ion_beams, $
                          OUT_FIT2DK=fit2DK, $
                          OUT_FIT2DGAUSS=fit2DG, $
                          OUT_KAPPAFIT1DSTRUCTS=kappaFit1Ds, $
                          OUT_GAUSSFIT1DSTRUCTS=gaussFit1Ds, $
                          FIT2D_KAPPA_INF_LIST=fit2DKappa_inf_list, $
                          FIT2D_GAUSS_INF_LIST=fit2DGauss_inf_list, $
                          SAVE_DIFF_EFLUX_FILE=save_diff_eFlux_file, $
                          LOAD_DIFF_EFLUX_FILE=load_diff_eFlux_file, $
                          MCFADDEN_DIFF_EFLUX=McFadden_diff_eFlux, $
                          KAPPA_STATS__SAVE_STUFF=kStats__save_stuff, $
                          KAPPA_STATS__INCLUDE_THESE_STARTSTOPS=kStats__include_these_startstops,$
                          DEBUG__SKIP_TO_THIS_TIME=debug__skip_to_this_time, $
                          DEBUG__BREAK_ON_THIS_TIME=debug__break_on_this_time, $
                          ORIGINATING_ROUTINE=routName, $
                          CURANDPOT_ANALYSIS=curAndPot_analysis, $
                          CAP_STRUCT=cAP_struct, $
                          TIMEBARS=timeBars, $
                          EPS=eps, $
                          BATCH_MODE=batch_mode
  
  COMPILE_OPT IDL2,STRICTARRSUBS

  @common__kappa_fit2d_structs.pro

  t1                            = STR_TO_TIME(t1Str)
  t2                            = STR_TO_TIME(t2Str)

  IF N_ELEMENTS(plotDir) EQ 0 THEN BEGIN
     SET_PLOT_DIR,plotDir, $
                  /FOR_SDT, $
                  /ADD_TODAY, $
                  ADD_SUFF='/kappa_fits/Orbit_' + STRCOMPRESS(orbit,/REMOVE_ALL)
  ENDIF

  outDir                        = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/'

  @kappa_fitter__defaults.pro
  
  ;;... And strings!!!!
  KAPPA_FITTER__FSTRINGS, $
     T1=t1, $
     T2=t2, $
     ORBIT=orbit, $
     EEB_OR_EES=eeb_or_ees, $
     ELECTRON_ANGLERANGE=electron_angleRange ,$
     BONUSPREF=bonusPref ,$
     SAVE_DIFF_EFLUX_FILE=save_diff_eFlux_file ,$
     SAVE_DIFF_EFLUX_TO_FILE=save_diff_eFlux_to_file ,$
     LOAD_DIFF_EFLUX_FILE=load_diff_eFlux_file ,$
     MCFADDEN_DIFF_EFLUX=McFadden_diff_eFlux, $
     OUT_DIFF_EFLUX_FILE=diff_eFlux_file, $
     FIT1D__CLAMPTEMPERATURE=fit1D__clampTemperature, $
     FIT1D__CLAMPDENSITY=fit1D__clampDensity, $
     FIT2D__CLAMPTEMPERATURE=fit2D__clampTemperature, $
     FIT2D__CLAMPDENSITY=fit2D__clampDensity, $
     FIT2D__ONLY_FIT_PEAK_ERANGE=fit2D__only_fit_peak_eRange ,$
     FIT2D__ONLY_FIT_ABOVEMIN=fit2D__only_fit_aboveMin ,$
     MIN_PEAK_ENERGY=min_peak_energy, $
     FIT2D__DISABLE_BFUNC=fit2D__disable_bFunc ,$
     ;; FIT2D__EXCLUDE_LCA_FROM_DENSCALC=fit2D__exclude_lca_from_densCalc ,$
     SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
     ENFORCE_DIFF_EFLUX_SRATE=enforce_diff_eFlux_sRate, $
     FITFILE=fitFile, $
     FIT2DPARMERRFILE=fit2DParmErrFile, $
     LOADDIR=outDir

  restored_fitFile = 0B
  IF KEYWORD_SET(restore_fitFile) THEN BEGIN

     searchbackward1month = 1

     remaining = KEYWORD_SET(searchbackward1month) ? 30 : 0

        foundFitFile = 0
        tmpFitFile   = fitFile
        WHILE ~foundFitFile AND remaining GT -1 DO BEGIN

           IF FILE_TEST(outDir+tmpFitFile) THEN BEGIN

              foundFitFile = 1
              fitFile      = TEMPORARY(tmpFitFile)
              
              PRINT,'Restoring ' + fitFile + ' ...'
              RESTORE,outDir+fitFile
              restored_fitFile = 1B
              ;; just_diff_eFlux  = 1B

              ;;And diff eFlux
              ;; RESTORE,outDir+'/diff_eFlux/'+diff_eFlux_file

           ENDIF ELSE BEGIN
              ;; PRINT,"Couldn't get file!"
              ;; STOP
              date = (STRSPLIT(tmpFitFile,'-',/EXTRACT))
              rest = STRJOIN(date[1:-1],'-')
              date = date[0]
              dd   = FIX(STRMID(date,STRLEN(date)-2,2))
              mm   = FIX(STRMID(date,STRLEN(date)-4,2))
              jahr = FIX(STRMID(date,STRLEN(date)-8,4))

              CASE 1 OF
                 ((dd EQ 1) AND (mm EQ 1)): BEGIN
                    jahr--
                    dd = 31
                    mm = 12
                 END
                 (dd EQ 1): BEGIN
                    dd = 31
                    mm--
                 END
                 ELSE: BEGIN
                    dd--
                 END
              ENDCASE
              
              tmpFitFile = STRJOIN([STRING(FORMAT='(I04,I02,I02)',jahr,mm,dd),rest],'-')


              remaining--

           ENDELSE
           
        ENDWHILE

        IF ~foundFitFile THEN BEGIN
           PRINT,"Couldn't get file!"
           IF KEYWORD_SET(batch_mode) THEN RETURN ELSE STOP
        ENDIF
        
  ENDIF ELSE BEGIN

     KAPPA_EFLUX_FIT2D, $
        T1=t1, $
        T2=t2, $
        SDT_TIME_INDS=bounds, $
        DO_ALL_TIMES=do_all_times, $
        ENERGY_ELECTRONS=energy_electrons, $
        ENERGY_ELECTRON_TBOUNDS=energy_electron_tBounds, $
        JUST_DIFF_EFLUX=just_diff_eFlux, $
        DIFF_EFLUX=diff_eFlux, $
        DEF_ONECOUNT=dEF_oneCount, $
        UPGOING=upgoing, $
        LOAD_DIFF_EFLUX_FILE=load_diff_eFlux_file ,$
        IN_DIFF_EFLUX_FILE=diff_eFlux_file, $
        MCFADDEN_DIFF_EFLUX=McFadden_diff_eFlux, $
        LOAD_DIR=outDir, $
        SAVE_DIFF_EFLUX_TO_FILE=save_diff_eFlux_to_file, $
        EEB_OR_EES=eeb_or_ees, $
        SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
        ENFORCE_DIFF_EFLUX_SRATE=enforce_diff_eFlux_sRate, $
        MIN_PEAK_ENERGY=min_peak_energy, $
        MAX_PEAK_ENERGY=max_peak_energy, $
        PEAK_ENERGY__START_AT_HIGHE=peak_energy__start_at_highE, $
        PHI__USE_ENERGY_BEFORE_PEAK=phi__use_energy_before_peak, $
        N_ENERGIES_BELOW_PEAK=n_below_peak1D, $
        N_ENERGIES_ABOVE_PEAK=n_above_peak1D, $
        N_BELOW_PEAK2D=n_below_peak2D, $
        N_ABOVE_PEAK2D=n_above_peak2D, $
        CHECK_FOR_HIGHER_FLUX_PEAKS__SET_CORRESPONDING_PEAK_ENERGY=check_higher_peaks_set_peakEn, $
        TRIM_ENERGIES_BELOW_PEAK=trim_energies_below_peak, $
        DONT_FIT_BELOW_THRESH_VALUE=dont_fit_below_thresh_value, $
        DENSITY_EST=n_est, $
        TEMPERATURE_EST=T, $
        KAPPA_EST=kappa_est, $
        SDT_DAT=dat, $
        ESTIMATE_FITPARAMS_FROM_SDT_DAT=estimate_A_from_data, $
        ESTIMATE_FACTORS=estFacs, $
        DONT_PRINT_ESTIMATES=dont_print_estimates, $
        DONT_PRINT_FITINFO=dont_print_fitInfo, $
        FIT__LINEAR_ENERGY_SHIFT=fit__linear_energy_shift, $
        FIT__JE_OVER_E=fit__JE_over_E, $
        FIT__LES__TAKE_STOCK_OF_RB=fit__LES__take_stock_of_RB, $
        ONLY_1D_FITS=only_1D_fits, $
        FIT1D__MAX_ITERATIONS=max_iter, $
        FIT1D__TOLERANCE=fit_tol, $
        FIT1D__AVERAGE_OVER_ANGLERANGE=average_over_angleRange, $
        FIT1D__SOURCECONE_ENERGY_SPECTRUM=fit1D__sourceCone_energy_spectrum, $
        FIT1D__NFLUX=fit1D__nFlux, $
        FIT1D__WEIGHTING=fit1D__weighting, $
        FIT1D__CLAMPTEMPERATURE=fit1D__clampTemperature, $
        FIT1D__CLAMPDENSITY=fit1D__clampDensity, $
        FIT1D__SKIP_BAD_FITS=fit1D__skip_bad_fits, $
        FIT1D__SHOW_AND_PROMPT=fit1D__show_and_prompt, $
        FIT1D__USER_PROMPT_ON_FAIL=fit1D_fail__user_prompt, $
        FIT1D__SAVE_PLOTSLICES=fit1D__save_plotSlices, $
        FIT1D__SAVE_EVERY_NTH_PLOT=fit1D__save_every_nth_plot, $
        FIT1D__SAVE_IF_KAPPA_BELOW=fit1D__save_if_kappa_below, $
        FIT1D__COMBINE_PLOTSLICES_IN_PDF=fit1D__combine_plotslices_in_PDF, $
        FIT2D__KEEP_WHOLEFIT=fit2D__keep_wholeFit, $
        FIT2D__EXTEND_FITSTRUCT_ERANGE=fit2D__extend_fitStruct_eRange, $
        FIT2D__NFLUX=fit2D__nFlux, $
        FIT2D__WEIGHTING=fit2D__weighting, $
        FIT2D__CLAMPTEMPERATURE=fit2D__clampTemperature, $
        FIT2D__CLAMPDENSITY=fit2D__clampDensity, $
        FIT2D__ONLY_FIT_ERANGE_AROUND_PEAK=fit2D__only_fit_peak_eRange, $
        FIT2D__ONLY_FIT_ERANGE_ABOVE_MIN=fit2D__only_fit_aboveMin, $
        FIT2D__SHOW_AND_PROMPT__EACH_CANDIDATE=fit2D__show_each_candidate, $
        FIT2D__SHOW_ONLY_DATA=fit2D__show_only_data, $
        FIT2D__PA_ZRANGE=fit2D__PA_zRange, $
        FIT2D__SAVE_ALL_PLOTS=fit2D__save_all_plots, $
        FIT2D__PRINT_FITINFO=print_2DFitInfo, $
        FIT2D__TOLERANCE=fit2D_tol, $
        FIT2D__MAX_ITERATIONS=fit2D_max_iter, $
        FIT2D__ONLY_FIT_ELECTRON_ANGLES=fit2D__only_fit_eAngles, $
        FIT2D__LOSSCONE_ANGLE=fit2D__lossCone_angle, $
        FIT2D__USE_BULK_E_ANISOTROPY=fit2D__bulk_e_anisotropy, $
        FIT2D__BULK_E_ANISO_FACTOR=fit2D__bulk_e_anis_factor, $
        FIT2D__DENSITY_ANGLERANGE=fit2D__density_angleRange, $
        FIT2D__TEMPERATURE_ANGLERANGE=fit2D__temperature_angleRange, $
        FIT2D__FACONDUCTANCE_ANGLERANGE=fit2D__faConductance_angleRange, $
        FIT2D__ESTIMATE_DENS_ARANGE_FROM_DIST=fit2D__estimate_sourceCone_from_dist, $
        FIT2D__TEMPERATURE_TYPE=fit2D__temperature_type, $
        ;; FIT2D__EXCLUDE_LCA_FROM_DENSCALC=fit2D__exclude_lca_from_densCalc, $
        FIT2D__DISABLE_BFUNC=fit2D__disable_bFunc, $
        ADD_GAUSSIAN_ESTIMATE=add_gaussian_estimate, $
        USE_SDT_GAUSSIAN_FIT=use_SDT_Gaussian_fit, $
        USE_MPFIT1D=use_mpFit1D, $
        ADD_ONECOUNT_CURVE=add_oneCount_curve, $
        FIT_EACH_ANGLE=fit_each_angle, $
        ADD_FITPARAMS_TEXT=add_fitParams_text, $
        ADD_ANGLE_LABEL=add_angle_label, $
        FIT2D__ADD_BOUNDARIES=fit2D__add_boundaries, $
        ELECTRON_ANGLERANGE=electron_angleRange, $
        MANUAL_ANGLE_CORRECTION=manual_angle_correction, $
        ;; ELECTRON_LOSSCONE_ANGLE=electron_lca, $
        NO_PLOTS=no_plots, $
        SAVE_FITPLOTS=save_fitplots, $
        PLOT_FULL_FIT=plot_full_fit, $
        PLOTNAMEPREF=plotNamePref, $
        PLOTDIR=plotDir, $
        OUT_FITTED_PARAMS=out_kappaParams, $
        OUT_FITTED_GAUSS_PARAMS=out_gaussParams, $
        OUT_KAPPAFIT1DSTRUCTS=kappaFit1Ds, $
        OUT_GAUSSFIT1DSTRUCTS=gaussFit1Ds, $
        OUT_FIT2DKAPPA_INF_LIST=fit2DKappa_inf_list, $
        OUT_FIT2DGAUSS_INF_LIST=fit2DGauss_inf_list, $
        OUT_SYNTH_SDT_STRUCTS=synthPackage, $
        OUT_SC_POT=sc_pot, $
        ADD_FULL_FITS=add_full_fits, $
        OUT_ERANGE_FIT=out_eRange_fit, $
        OUT_PARAMSTR=out_paramStr, $
        OUT_STRINGS=strings, $
        TXTOUTPUTDIR=txtOutputDir,$
        DEBUG__SKIP_TO_THIS_TIME=debug__skip_to_this_time, $
        DEBUG__BREAK_ON_THIS_TIME=debug__break_on_this_time, $
        EPS=eps

     IF KEYWORD_SET(saveData) THEN BEGIN

        saveStr  = 'SAVE,'

        saveStr += 'KF2D__SDTData_opt,KF2D__Curvefit_opt,KF2D__strings,KF2D__plot_opt,'
        
        IF N_ELEMENTS(je) GT 0 THEN BEGIN
           saveStr += 'je,'
        ENDIF
        IF N_ELEMENTS(jee) GT 0 THEN BEGIN
           saveStr += 'jee,'
        ENDIF

        IF N_ELEMENTS(electron_angleRange) GT 0 THEN BEGIN
           saveStr += 'electron_angleRange,'
        ENDIF

        IF N_ELEMENTS(energy_electrons) GT 0 THEN BEGIN
           saveStr += 'energy_electrons,'
        ENDIF

        IF N_ELEMENTS(energy_electron_tBounds) GT 0 THEN BEGIN
           saveStr += 'energy_electron_tBounds,'
        ENDIF

        IF N_ELEMENTS(kappaFit1Ds) GT 0 THEN BEGIN
           saveStr += 'kappaFit1Ds,'
        ENDIF

        IF N_ELEMENTS(gaussFit1Ds) GT 0 THEN BEGIN
           saveStr += 'gaussFit1Ds,'
        ENDIF

        IF N_ELEMENTS(synthPackage) GT 0 THEN BEGIN
           saveStr += 'synthPackage,'
        ENDIF

        ;; IF N_ELEMENTS(strings) GT 0 THEN BEGIN
        ;;    saveStr += 'strings,'
        ;; ENDIF

        IF N_ELEMENTS(fit2DKappa_inf_list) GT 0 THEN BEGIN
           saveStr += 'fit2DKappa_inf_list,'
        ENDIF

        IF N_ELEMENTS(fit2DGauss_inf_list) GT 0 THEN BEGIN
           saveStr += 'fit2DGauss_inf_list,'
        ENDIF

        IF KEYWORD_SET(error_estimates) THEN BEGIN
           saveStr += 'nErr,jErr,TErr,errors,'
        ENDIF

        PRINT,'Saving ' + fitFile + ' ...'

        saveStr += 'FILENAME=outDir+fitFile'
        good     = EXECUTE(saveStr)

     ENDIF

     PRINT,"DONE!"

  ENDELSE

  STR_ELEMENT_FROM_LIST_OF_STRUCTS,fit2DKappa_inf_list,'status',VALUE=kappaFit2DStatus
  STR_ELEMENT_FROM_LIST_OF_STRUCTS,fit2DGauss_inf_list,'status',VALUE=gaussFit2DStatus
  PRINT_KAPPA_LOOP_FIT_SUMMARY,kappaFit2DStatus,gaussFit2DStatus,/IS2D

  IF ISA(KF2D__SDTData_opt) THEN BEGIN
     electron_angleRange = KF2D__SDTData_opt.electron_angleRange
  ENDIF

  ;;2017/03/21
  ;;LOOK: All you've got to do is figure out a way to consistently get the ion contribution in here, and you're golden. Understand?
  ;;THEN you can see what it's really like
  IF KEYWORD_SET(curAndPot_analysis) THEN BEGIN

     plot_times           = [t1Str,t2Str]
     
     IF KEYWORD_SET(cAP_struct) THEN IF (WHERE(TAG_NAMES(cAP_struct) EQ 'TRANGES'))[0] NE -1 THEN BEGIN
        tRanges           = cAP_struct.tRanges
        useInds__twoLumps = 1
     ENDIF

     CURRENT_AND_POTENTIAL_WRAPPER_FOR_KAPPA_FITTER_BLACKBOX, $
        ORBIT=orbit, $
        EEB_OR_EES=eeb_or_ees, $
        ELECTRON_ANGLERANGE=electron_angleRange, $
        MANUAL_ANGLE_CORRECTION=manual_angle_correction, $
        MOMENT_ENERGYARR=cAP_moment_energyArr, $
        ORIGINATING_ROUTINE=routName, $
        REMAKE_MASTERFILE=cAP_remake_masterFile, $
        MAP_TO_100KM=cAP_map_to_100km, $
        USE_ALL_CURRENTS=cAP_use_all_currents, $
        USE_DOWNGOING_ELECTRON_CURRENT=cAP_use_ed_current, $
        USE_UPGOING_ION_CURRENT=cAP_use_iu_current, $
        USE_UPGOING_ELECTRON_CURRENT=cAP_use_eu_current, $
        USE_MAGNETOMETER_CURRENT=cAP_use_mag_current, $
        USE_CHAR_EN_FOR_DOWNPOT=cAP_use_charE_for_downPot, $
        USE_PEAK_EN_FOR_DOWNPOT=cAP_use_peakE_for_downPot, $
        ;; ADD_UPGOING_ION_POT=cAP_add_iu_pot, $
        ADD_IU_POT=cAP_add_iu_pot, $
        PLOT_TIMES=plot_times, $
        IN_BONUSPREF=bonusPref, $
        USEI__RELCHANGE=useInds__relChange, $
        USEI__TWOLUMPS=useInds__twoLumps, $
        FIT_TRANGES=tRanges, $
        PLOT_J_V_POTBAR=cAP_plot_j_v_potBar, $
        PLOT_JV_A_LA_ELPHIC=cAP_plot_jv_a_la_Elphic, $
        PLOT_T_AND_N=cAP_plot_T_and_N, $
        PLOT_J_V_AND_THEORY=cAP_plot_j_v_and_theory, $
        PLOT_J_V__FIXED_T_AND_N=cAP_plot_j_v__fixed_t_and_n, $
        PLOT_J_V_MAP__R_B_AND_KAPPA__FIXED_T_AND_N=cAP_plot_j_v_map__r_b_and_kappa__fixed_t_and_n, $
        PLOT_EN_SPECS=cAP_plot_en_specs, $
        EN_SPECS__MOVIE=cAP_en_specs__movie, $
        JV_THEOR__R_B_INIT=jv_theor__R_B_init, $
        JV_THEOR__KAPPA_INIT=jv_theor__kappa_init, $
        JV_THEOR__KAPPALIMS=kappaLims, $   
        JV_THEOR__TEMPLIMS=TempLims, $    
        JV_THEOR__DENSLIMS=DensLims, $    
        JV_THEOR__MAGRATIOLIMS=magRatioLims, $
        JV_THEOR__FIT_JE=jv_theor__fit_je, $
        JV_THEOR__FIT_BOTH=jv_theor__fit_both, $
        USE_MSPH_SOURCECONE_FOR_DENS=cAP_use_msph_sourcecone_for_dens, $
        USE_MSPH_SOURCECONE_FOR_TEMP=cAP_use_msph_sourcecone_for_temp, $
        ALL_PITCHANGLES=cAP_all_pitchAngles, $
        ALLPITCH_EXCEPT_ATM_LC=cAP_allPitch_except_atm_lc, $
        JV_THEOR__INITIAL_SOURCE_R_E=jv_theor__initial_source_R_E, $
        JV_THEOR__INITIAL_SOURCE__POLARSAT=jv_theor__initial_source__Polar, $
        JV_THEOR__INITIAL_SOURCE__EQUATOR=jv_theor__initial_source__equator, $
        ;; JV_THEOR__ITERATIVE_DENSITY_AND_R_B_GAME=jv_theor__iterative_game, $
        JV_THEOR__ITERATIVE_RBDENS_GAME=jv_theor__iterative_RBDens_game, $
        ;; JV_THEOR__ITERATIVE_GAME__DENSITY_INCREASE=jv_theor__itergame_NFac, $
        JV_THEOR__ITERGAME_DENSFAC=jv_theor__itergame_densFac, $
        JV_THEOR__ITERGAME_TIE_R_B_AND_DENS=jv_theor__itergame_tie_R_B_and_dens, $
        MAP__MULTI_MAGRATIO_ARRAY=cAP_map__multi_magRatio_array, $
        MAP__MULTI_KAPPA_ARRAY=cAP_map__multi_kappa_array, $
        MAP__2D=cAP_map__2D, $
        LOAD_DIFF_EFLUX_FILE=load_diff_eFlux_file ,$
        SAVE_DIFF_EFLUX_FILE=save_diff_eFlux_file ,$
        SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
        ENFORCE_DIFF_EFLUX_SRATE=enforce_diff_eFlux_sRate, $
        ARANGE__MOMENTS_LIST=aRange__moments_list, $
        ARANGE__PEAKEN_LIST=aRange__peakEn_list, $
        OUT_CURPOTLIST=curPotList, $
        OUT_JVPLOTDATA=jvPlotData, $
        OUT_AVGS_FOR_FITTING=avgs_JVfit, $
        SC_POT=sc_pot, $
        EPS=eps, $
        CAP_STRUCT=cAP_struct, $
        BATCH_MODE=batch_mode

  ENDIF


  lastFile = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/lastFile.sav'
  ;; String of contents updated 2018/04/03
  PRINT,"Saving lastFile stuff: "
  PRINT,"avgs_JVfit,jvPlotData,curPotList,cAP_struct,fitFile,KF2D__SDTData_opt,KF2D__Curvefit_opt,KF2D__strings,KF2D__plot_opt,electron_angleRange,energy_electrons,energy_electron_tBounds,kappaFit1Ds,gaussFit1Ds,fit2DKappa_inf_list,fit2DGauss_inf_list,use_mpFit1D,nPkAbove_dEF_thresh,diffEflux_thresh,lowDens_thresh,highDens_thresh,chi2_over_dof_thresh,chi2_thresh"
  SAVE,avgs_JVfit,jvPlotData,curPotList,cAP_struct,fitFile,KF2D__SDTData_opt,KF2D__Curvefit_opt,KF2D__strings,KF2D__plot_opt,electron_angleRange,energy_electrons,energy_electron_tBounds,kappaFit1Ds,gaussFit1Ds,fit2DKappa_inf_list,fit2DGauss_inf_list,use_mpFit1D,nPkAbove_dEF_thresh,diffEflux_thresh,lowDens_thresh,highDens_thresh,chi2_over_dof_thresh,chi2_thresh, $
       FILENAME=lastFile

  ;;Can we do 2D action?
  canDo2D = ~KEYWORD_SET(only_1D_fits) AND $
            N_ELEMENTS(fit2DKappa_inf_list) GT 0
  IF KEYWORD_SET(add_gaussian_estimate) THEN BEGIN
     canDo2D = canDo2D AND N_ELEMENTS(fit2DGauss_inf_list) GT 0
  ENDIF
  IF canDo2D THEN BEGIN

     ;;Proof that it's better with Papa John's

     ;; Shrink these fools?
     STR_ELEMENT_FROM_LIST_OF_STRUCTS,kappaFit1Ds,'time_index',VALUE=k1DFitsTimeInd
     STR_ELEMENT_FROM_LIST_OF_STRUCTS,gaussFit1Ds,'time_index',VALUE=g1DFitsTimeInd
     junk1DTimeInds = CGSETINTERSECTION(k1DFitsTimeInd,g1DFitsTimeInd, $
                                        POSITIONS=k1DTInds)
     junk1DTimeInds = CGSETINTERSECTION(g1DFitsTimeInd,k1DFitsTimeInd, $
                                        POSITIONS=g1DTInds)

     IF ~ARRAY_EQUAL(k1DTInds,g1DTInds) THEN IF ~KEYWORD_SET(batch_mode) THEN STOP
     
     kappaFit1Ds = kappaFit1Ds[k1DTInds]
     gaussFit1Ds = gaussFit1Ds[g1DTInds]

     PARSE_KAPPA_FIT_STRUCTS,kappaFit1Ds, $
                             A=a, $
                             STRUCT_A=Astruct, $
                             TIME=kappaTime, $
                             NAMES_A=A_names, $
                             CHI2=chi2, $
                             PVAL=pVal, $
                             FITSTATUS=fitStatus, $
                             USE_MPFIT1D=use_mpFit1D

     PARSE_KAPPA_FIT_STRUCTS,gaussFit1Ds, $
                             A=AGauss, $
                             STRUCT_A=AStructGauss, $
                             TIME=gaussTime, $
                             NAMES_A=AGauss_names, $
                             CHI2=chi2Gauss, $
                             PVAL=pValGauss, $
                             FITSTATUS=gaussFit1DStatus, $
                             USE_MPFIT1D=use_mpFit1D

     STR_ELEMENT_FROM_LIST_OF_STRUCTS,kappaFit1Ds,'A',/PRESERVE_DIMENSIONALITY,VALUE=As
     STR_ELEMENT_FROM_LIST_OF_STRUCTS,GaussFit1Ds,'A',/PRESERVE_DIMENSIONALITY,VALUE=GaussAs
     ;; STR_ELEMENT_FROM_LIST_OF_STRUCTS,fit2dkappa_inf_list,'fitdens',VALUE=fitDens
     ;; STR_ELEMENT_FROM_LIST_OF_STRUCTS,fit2dkappa_inf_list,'SDT',VALUE=sdt
     ;; tid = SDT[*].time
     ;; these = VALUE_CLOSEST2(jvPlotdata.time,tid)
     ;; PRINT,jvplotdata.ndown[these]/fitDens
     ;; PRINT,jvPlotData.nDown[these]/As[*,3]

     kFit2DParam_struct = 1
     gFit2DParam_struct = 1

     fit2DK = PARSE_KAPPA_FIT2D_INFO_LIST_V2(fit2DKappa_inf_list, $
                                             SOUTH=south, $
                                             FIT_TYPE='Kappa', $
                                             HIGHDENSITY_THRESHOLD=highDens_thresh, $
                                             LOWDENSITY_THRESHOLD=lowDens_thresh, $
                                             CHI2_OVER_DOF_THRESHOLD=chi2_over_dof_thresh, $
                                             CHI2_THRESHOLD=chi2_thresh, $
                                             DIFFEFLUX_THRESHOLD=diffEflux_thresh, $
                                             N_PEAKS_ABOVE_DEF_THRESHOLD=nPkAbove_dEF_thresh, $
                                             OUT_GOOD_I=includeK_i, $
                                             OUT_FITPARAM_STRUCT=kFit2DParam_struct, $
                                             /DONT_SHRINK_PARSED_STRUCT)

     fit2DG = PARSE_KAPPA_FIT2D_INFO_LIST_V2(fit2DGauss_inf_list, $
                                             SOUTH=south, $
                                             FIT_TYPE='Maxwellian', $
                                             HIGHDENSITY_THRESHOLD=highDens_thresh, $
                                             LOWDENSITY_THRESHOLD=lowDens_thresh, $
                                             CHI2_OVER_DOF_THRESHOLD=chi2_over_dof_thresh, $
                                             CHI2_THRESHOLD=chi2_thresh, $
                                             DIFFEFLUX_THRESHOLD=diffEflux_thresh, $
                                             N_PEAKS_ABOVE_DEF_THRESHOLD=nPkAbove_dEF_thresh, $
                                             IN_GOOD_I=includeK_i, $
                                             OUT_GOOD_I=includeG_i, $
                                             OUT_FITPARAM_STRUCT=gFit2DParam_struct, $
                                             /DONT_SHRINK_PARSED_STRUCT) 
     
     PRINT_KAPPA_FIT2D_STATS_FOR_CURANDPOT_TRANGES,fit2DK,fit2DG,cAP_struct,jvPlotData, $
        /SOK_IF_INDS_DONT_MATCH, $
        BATCH_MODE=batch_mode
        ;; /ALSO_PARAM_STRUCTS, $
        ;; KFIT2DPARAM_STRUCT=kFit2DParam_struct, $
        ;; GFIT2DPARAM_STRUCT=gFit2DParam_struct

     ;;Now shrink everyone
     IF ~( ARRAY_EQUAL(includeK_i,includeG_i)                          AND $
           (N_ELEMENTS(kappaFit1Ds)  EQ N_ELEMENTS(gaussFit1Ds)           ) AND $
           (N_ELEMENTS(kappaFit1Ds)  EQ N_ELEMENTS(fit2DKappa_inf_list) ) AND $
           (N_ELEMENTS(includeK_i) EQ N_ELEMENTS(fit2DKappa_inf_list) ) ) THEN BEGIN

        IF N_ELEMENTS(kappaFit1Ds) NE N_ELEMENTS(gaussFit1Ds) THEN IF ~KEYWORD_SET(batch_mode) THEN STOP
        IF N_ELEMENTS(fit2DKappa_inf_list) NE N_ELEMENTS(fit2DGauss_inf_list) THEN IF ~KEYWORD_SET(batch_mode) THEN STOP
        IF N_ELEMENTS(kappaFit1Ds) NE N_ELEMENTS(fit2DKappa_inf_list) THEN IF ~KEYWORD_SET(batch_mode) THEN STOP

        include_i = CGSETINTERSECTION(includeK_i,includeG_i)

        fit2DK            = {SDT          : fit2DK.SDT         [include_i], $
                             fitParams    : fit2DK.fitParams   [*,include_i], $
                             obsMoms      : fit2DK.obsMoms     [include_i], $
                             fitMoms      : fit2DK.fitMoms     [include_i], $
                             moment_info  : fit2DK.moment_info [include_i], $
                             ;; fitDens      : fit2DK.fitDens     [include_i], $
                             chi2         : fit2DK.chi2        [include_i], $
                             errMsg       : fit2DK.errMsg      [include_i], $
                             status       : fit2DK.status      [include_i], $
                             nfEv         : fit2DK.nfEv        [include_i], $
                             ;; best_resid   : best_resid      [include_i], $
                             pFree_index  : fit2DK.pFree_index [*,include_i], $
                             ;; best_fJac    : best_fJac       [include_i], $
                             nPegged      : fit2DK.nPegged     [include_i], $
                             nFree        : fit2DK.nFree       [include_i], $
                             dof          : fit2DK.dof         [include_i], $
                             covar        : fit2DK.covar       [*,*,include_i], $
                             pError       : fit2DK.pError      [*,include_i], $
                             nIter        : fit2DK.nIter       [include_i]}
        
        fit2DG            = {SDT          : fit2DG.SDT         [include_i], $
                             fitParams    : fit2DG.fitParams   [*,include_i], $
                             obsMoms      : fit2DG.obsMoms     [include_i], $
                             fitMoms      : fit2DG.fitMoms     [include_i], $
                             moment_info  : fit2DG.moment_info [include_i], $
                             chi2         : fit2DG.chi2        [include_i], $
                             errMsg       : fit2DG.errMsg      [include_i], $
                             status       : fit2DG.status      [include_i], $
                             nfEv         : fit2DG.nfEv        [include_i], $
                             ;; best_resid   : best_resid      [include_i], $
                             pFree_index  : fit2DG.pFree_index [*,include_i], $
                             ;; best_fJac    : best_fJac       [include_i], $
                             nPegged      : fit2DG.nPegged     [include_i], $
                             nFree        : fit2DG.nFree       [include_i], $
                             dof          : fit2DG.dof         [include_i], $
                             covar        : fit2DG.covar       [*,*,include_i], $
                             pError       : fit2DG.pError      [*,include_i], $
                             nIter        : fit2DG.nIter       [include_i]}

        fit2DKappa_inf_list = fit2DKappa_inf_list[include_i]
        fit2DGauss_inf_list = fit2DGauss_inf_list[include_i]

        AStruct      = {bulk_energy : AStruct.bulk_energy[include_i], $
                        temperature : AStruct.temperature[include_i], $
                        kappa       : AStruct.kappa[include_i], $
                        N           : AStruct.N[include_i], $
                        bulk_angle  : AStruct.bulk_angle[include_i]}

        AStructGauss = {bulk_energy : AStructGauss.bulk_energy[include_i], $
                        temperature : AStructGauss.temperature[include_i], $
                        kappa       : AStructGauss.kappa[include_i], $
                        N           : AStructGauss.N[include_i], $
                        bulk_angle  : AStructGauss.bulk_angle[include_i]}

        kFit2DParam_struct = {bulk_energy  : kFit2DParam_struct.bulk_energy[include_i], $
                              temperature  : kFit2DParam_struct.temperature[include_i], $
                              kappa        : kFit2DParam_struct.kappa      [include_i], $
                              n            : kFit2DParam_struct.n          [include_i]}

        gFit2DParam_struct = {bulk_energy  : gFit2DParam_struct.bulk_energy[include_i], $
                              temperature  : gFit2DParam_struct.temperature[include_i], $
                              kappa        : gFit2DParam_struct.kappa      [include_i], $
                              n            : gFit2DParam_struct.n          [include_i]}

     ENDIF

     IF KEYWORD_SET(show_post_plots) THEN BEGIN

        POST_KAPPA2D_FIT_PLOTS,fit2DK,fit2DG,orbit,plotNamePref,plotDir,save_postKappa_plot, $
                               CLOSE_KAPPAPLOTS_AFTER_SAVE=close_kp_after_save                            

     ENDIF


     ;; pap   = PLOT(time-time[0],this.fitParams[2,*],YLOG=1,SYMBOL='*',LINESTYLE='')
     PRINT,FORMAT='("(N w/ k ≤ 2.5)/nTot : ",I0,"/",I0)', $
           N_ELEMENTS(WHERE(fit2DK.fitParams[2,*] LE 2.5)), $
           N_ELEMENTS(fit2DK.nIter)
     IF KEYWORD_SET(curAndPot_analysis) THEN BEGIN
        theseInds = WHERE(fit2DK.SDT.time GE STR_TO_TIME(tRanges[0]) AND $
                          fit2DK.SDT.time LE STR_TO_TIME(tRanges[1]),nHjar)
        IF nHjar EQ 0 THEN IF ~KEYWORD_SET(batch_mode) THEN STOP
        FOR k=0,nHjar-1 DO BEGIN
           PRINT,FORMAT='(A0,T30,F0.2)',TIME_TO_STR(fit2DK.SDT[theseInds[k]].time),fit2DK.fitParams[2,theseInds[k]]
        ENDFOR
        PRINT,''
        PRINT,"Avg    kappa value: ",nHjar GT 1 ? MEAN(fit2DK.fitParams[2,theseInds]) : fit2DK.fitParams[2,theseInds]
        PRINT,"Median kappa value: ",nHjar GT 1 ? MEDIAN(fit2DK.fitParams[2,theseInds]) : fit2DK.fitParams[2,theseInds]
     ENDIF

     IF ~KEYWORD_SET(batch_mode) THEN PRINT_J_V_INFO_FOR_MATHEMATICA, $
        JVPLOTDATA=jvPlotData, $
        AVGS_JVFIT=avgs_jvFit, $
        KFIT2DPARAM_STRUCT=kFit2DParam_struct, $
        GFIT2DPARAM_STRUCT=gFit2DParam_struct, $
        FIT2DKAPPA_INF_LIST=fit2DKappa_inf_list, $
        FIT2DGAUSS_INF_LIST=fit2DGauss_inf_list, $
        ;; KAPPA2D=fit2DK, $
        ;; GAUSS2D=fit2DG, $
        CAP_STRUCT=cAP_struct, $
        ADD_PARM_ERRORS_FROM_FILE=kSum__add_parm_errors_from_file, $
        ADD_PARM_ERRORS__NROLLS=kSum__add_parm_errors__nRolls, $
        ADD_PARM_ERRORS__USE_MOST_PROB=kSum__add_parm_errors__use_most_prob, $
        FIT2DPARMERRFILE=fit2DParmErrFile, $
        FIT2DPARMERRDIR=outDir

     IF KEYWORD_SET(sway__checkForIonBeams) THEN BEGIN
        GET_FA_IESA_ION_BEAMS,STR_TO_TIME(t1Str),STR_TO_TIME(t2Str), $
                              ORBIT=orbit, $
                              NEWELL_2009_INTERP=Newell_2009_interp, $
                              ION_ANGLERANGE=curPotList[2].angles.peakEn, $
                              ION_ENERGYRANGE=curPotList[2].energy, $
                              SPECTROGRAM_UNITS=spectrogram_units, $
                              EEB_OR_EES=eeb_or_ees, $
                              SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                              ENFORCE_DIFF_EFLUX_SRATE=enforce_diff_eFlux_sRate, $
                              SC_POT=sc_pot, $
                              OUT_SC_POTAVG=sc_potAvg, $
                              OUT_IONEVENTS=ionEvents, $
                              BATCH_MODE=batch_mode
     ENDIF

     IF KEYWORD_SET(show_Strangeway_summary) THEN BEGIN

        SINGLE_RJS_SUMMARY,STR_TO_TIME(t1Str),STR_TO_TIME(t2Str), $
                           TPLT_VARS=tPlt_vars, $
                           ORBIT=orbit, $
                           EEB_OR_EES=eeb_or_ees, $
                           ENERGY_ELECTRONS=energy_electrons, $
                           ENERGY_ELECTRON_TBOUNDS=energy_electron_tBounds, $
                           ELECTRON_ANGLERANGE=electron_angleRange, $
                           TLIMIT_NORTH=tlimit_north, $
                           TLIMIT_SOUTH=tlimit_south, $
                           TLIMIT_ALL=tlimit_all, $
                           /SCREEN_PLOT, $
                           ADD_KAPPA_PANEL=sway__add_kappa_panel, $
                           ADD_CHARE_PANEL=sway__add_chare_panel, $
                           ADD_NEWELL_PANEL=sway__add_Newell_panel, $
                           NEWELL_2009_INTERP=sway__Newell_interp, $
                           SAVE_NEWELL_DATA=sway__save_Newell_data, $
                           ADD_IU_POT=sway__add_iu_pot, $
                           SPECTROGRAM_UNITS=sway__spectrogram_units, $
                           LOG_KAPPAPLOT=sway__log_kappaPlot, $
                           USE_FAC_V=use_fac_v, $
                           USE_FAC_NOT_V=use_fac, $
                           NO_BLANK_PANELS=no_blank_panels, $
                           FIT2DKAPPA_INF_LIST=fit2DKappa_inf_list, $
                           FIT2DGAUSS_INF_LIST=fit2DGauss_inf_list, $
                           KAPPAFIT1DS=kappaFit1Ds, $
                           GAUSSFIT1DS=gaussFit1Ds, $
                           CHI2_THRESHOLD=chi2_thresh, $
                           CHI2_OVER_DOF_THRESHOLD=chi2_over_dof_thresh, $
                           HIGHDENSITY_THRESHOLD=highDens_thresh, $
                           LOWDENSITY_THRESHOLD=lowDens_thresh, $
                           DIFFEFLUX_THRESHOLD=diffEflux_thresh, $
                           N_PEAKS_ABOVE_DEF_THRESHOLD=nPkAbove_dEF_thresh, $
                           ION_ANGLERANGE=curPotList[2].angles.peakEn, $
                           ION_ENERGYRANGE=curPotList[2].energy, $
                           CURPOTLIST=curPotList, $
                           CAP_STRUCT=cAP_struct, $
                           SAVE_PS=sway__save_ps, $
                           SAVE_PNG=sway__save_png, $
                           EPS=eps, $
                           OUTPLOT_BONUSPREF=bonusPref, $
                           SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                           ENFORCE_DIFF_EFLUX_SRATE=enforce_diff_eFlux_sRate, $
                           IONEVENTS=ionEvents, $
                           SC_POTAVG=sc_potAvg, $
                           ;; GRL=sway__GRL, $
                           ;; SC_POT=sc_pot, $
                           CHECKFORIONBEAMS=sway__checkForIonBeams, $
                           BATCH_MODE=batch_mode, $
                           PLOTDIR=plotDir

     ENDIF

     IF KEYWORD_SET(show_kappa_summary) THEN BEGIN
        
        IF KEYWORD_SET(show_Strangeway_summary) THEN tPlt_vars = !NULL ;Clear 'em out

        SINGLE_KAPPA_SUMMARY,STR_TO_TIME(t1Str),STR_TO_TIME(t2Str), $
                             TPLT_VARS=tPlt_vars, $
                             ORBIT=orbit, $
                             EEB_OR_EES=eeb_or_ees, $
                             ENERGY_ELECTRONS=energy_electrons, $
                             ENERGY_ELECTRON_TBOUNDS=energy_electron_tBounds, $
                             ELECTRON_ANGLERANGE=KEYWORD_SET(kSum__eAngle) ? kSum__eAngle : electron_angleRange, $
                             TLIMIT_NORTH=tlimit_north, $
                             TLIMIT_SOUTH=tlimit_south, $
                             TLIMIT_ALL=tlimit_all, $
                             /SCREEN_PLOT, $
                             USE_FAC_V=use_fac_v, $
                             USE_FAC_NOT_V=use_fac, $
                             NO_BLANK_PANELS=no_blank_panels, $
                             ADD_CHI2_LINE=kSum__add_chi2_line, $
                             CHI2BOUNDS=kSum__chi2Bounds, $
                             FIT2DKAPPA_INF_LIST=fit2DKappa_inf_list, $
                             FIT2DGAUSS_INF_LIST=fit2DGauss_inf_list, $
                             KAPPA2D=fit2DK, $
                             GAUSS2D=fit2DG, $
                             KAPPA_FITPARAM_STRUCT=kFit2DParam_struct, $
                             GAUSS_FITPARAM_STRUCT=gFit2DParam_struct, $
                             KAPPAFIT1DS=kappaFit1Ds, $
                             GAUSSFIT1DS=gaussFit1Ds, $
                             DIFF_EFLUX=diff_eFlux, $
                             JVPLOTDATA=jvPlotData, $
                             ADD_MEASURED_T_AND_N=kSum__add_meas_T_and_N, $
                             ADD_ONLY_MEAS_N=kSum__add_only_meas_N, $
                             SC_POT=sc_pot, $
                             CONVERT_DESPECS_TO_NEWELL_INTERP=kSum__convert_to_Newell_interp, $
                             SAVE_PS=kSum__save_ps, $
                             SAVE_PNG=kSum__save_png, $
                             EPS=eps, $
                             SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                             ENFORCE_DIFF_EFLUX_SRATE=enforce_diff_eFlux_sRate, $
                             OUTPLOT_BONUSPREF=bonusPref, $
                             PLOTDIR=plotDir, $
                             SAVE_FOR_OFFLINE=save_for_offline, $
                             LOAD_FROM_OFFLINE=load_from_offline, $
                             KAPPA_STATS__SAVE_STUFF=kStats__save_stuff, $
                             KAPPA_STATS__INCLUDE_THESE_STARTSTOPS=kStats__include_these_startstops, $
                             INCLUDE_ELECTRON_PA_SPEC=kSum__include_electron_pa_spec, $
                             GRL=kSum__GRL, $
                             OPLOT_POT=kSum__oPlot_pot, $
                             SPECTROGRAM_UNITS=kSum__spectrogram_units, $
                             ADD_PARM_ERRORS_FROM_FILE=kSum__add_parm_errors_from_file, $
                             ADD_PARM_ERRORS__NROLLS=kSum__add_parm_errors__nRolls, $
                             ADD_PARM_ERRORS__USE_MOST_PROB=kSum__add_parm_errors__use_most_prob, $
                             IONEVENTS=ionEvents, $                             
                             FIT2DPARMERRFILE=fit2DParmErrFile, $
                             FIT2DPARMERRDIR=outDir, $
                             TIMEBARS=timeBars, $
                             TIMEBAR_FROM_ION_BEAMS=kSum__timeBar_from_ion_beams

     ENDIF

  ENDIF

  ;;I'm sure something was supposed to happen here, but I don't know what it was. I think I was trying to fit the J-V curve using
  ;;parameter values from best fits at each time step, but it never quite worked. So I give up/try an alternate route
  
  ;; IF KEYWORD_SET(curAndPot_analysis) THEN BEGIN

  ;;    ;;See if the time series align
  ;;    STR_ELEMENT_FROM_LIST_OF_STRUCTS,kappaFit1Ds,'time',VALUE=kTimes
  ;;    STR_ELEMENT_FROM_LIST_OF_STRUCTS,gaussFit1Ds,'time',VALUE=gTimes
  ;;    STR_ELEMENT_FROM_LIST_OF_STRUCTS,kappaFit1Ds,'chi2',VALUE=kChi2
  ;;    STR_ELEMENT_FROM_LIST_OF_STRUCTS,kappaFit1Ds,'chi2',VALUE=gChi2

  ;;    sAIFac  = KEYWORD_SET(spectra_average_interval) ? spectra_average_interval : 1
  ;;    maxDiff = (STRMATCH(STRUPCASE(eeb_or_ees),'*ES') ? 0.7 : 0.1) * sAIFac

  ;;    datUseInds = avgs_JVfit.useInds
  ;;    nDat    = N_ELEMENTS(curPotList[0].time[datUseInds])
  ;;    nKappa  = N_ELEMENTS(kTimes)
  ;;    nGauss  = N_ELEMENTS(gTimes)
  ;;    IF nDat EQ nKappa THEN BEGIN

  ;;       IF (WHERE(ABS(kTimes-curPotList[0].time[datUseInds]) GT maxDiff))[0] NE -1 THEN STOP
  ;;       kFitUseInds = datUseInds

  ;;    ENDIF ELSE BEGIN

  ;;       CASE 1 OF
  ;;          (nDat GT nKappa): BEGIN

  ;;             datUseIndsII = VALUE_CLOSEST2(curPotList[0].time[datUseInds],kTimes,/CONSTRAINED)
  ;;             datUseInds   = datUseInds[datUseIndsII]
  ;;             kFitUseInds  = LINDGEN(nKappa)

  ;;          END
  ;;          (nDat LT nKappa): BEGIN
  ;;             kFitUseInds = VALUE_CLOSEST2(kTimes,curPotList[0].time[datUseInds],/CONSTRAINED)

  ;;          END
  ;;       ENDCASE

  ;;       checker = WHERE(ABS(kTimes[kFitUseInds]-curPotList[0].time[datUseInds]) GT maxDiff,nChecker, $
  ;;                       COMPLEMENT=realKeep)

  ;;       IF nChecker GT 0 THEN BEGIN
  ;;          kFitUseInds = kFitUseInds[realKeep]
  ;;          datUseInds  = datUseInds[realKeep]
  ;;       ENDIF

  ;;       nDat    = N_ELEMENTS(curPotList[0].time[datUseInds])
  ;;       nKappa  = N_ELEMENTS(kTimes)

  ;;    ENDELSE

  ;;    IF nDat EQ nGauss THEN BEGIN
  ;;       IF (WHERE(ABS(gTimes-curPotList[0].time[datUseInds]) GT maxDiff))[0] NE -1 THEN STOP
  ;;       gFitUseInds = datUseInds
  ;;       ;; gFitUseInds = LINDGEN(datUseInds
  ;;    ENDIF ELSE BEGIN

  ;;       CASE 1 OF
  ;;          (nDat GT nGauss): BEGIN

  ;;             datUseIndsII = VALUE_CLOSEST2(curPotList[0].time[datUseInds],gTimes,/CONSTRAINED)
  ;;             datUseInds   = datUseInds[datUseIndsII]
  ;;             gFitUseinds  = LINDGEN(nGauss)

  ;;             ;; IF (WHERE(ABS(gTimes-curPotList[0].time[datUseInds]) GT maxDiff))[0] NE -1 THEN STOP

  ;;          END
  ;;          (nDat LT nGauss): BEGIN
  ;;             gFitUseInds = VALUE_CLOSEST2(gTimes,curPotList[0].time[datUseInds],/CONSTRAINED)

  ;;             ;; IF (WHERE(ABS(gTimes[gFitUseInds]-curPotList[0].time[datUseInds]) GT maxDiff))[0] NE -1 THEN STOP

  ;;          END
  ;;       ENDCASE

  ;;       checker = WHERE(ABS(gTimes[gFitUseInds]-curPotList[0].time[datUseInds]) GT maxDiff,nChecker, $
  ;;                       COMPLEMENT=realKeep)

  ;;       IF nChecker GT 0 THEN BEGIN
  ;;          gFitUseInds = gFitUseInds[realKeep]
  ;;          datUseInds  = datUseInds[realKeep]
  ;;       ENDIF

  ;;       nDat    = N_ELEMENTS(curPotList[0].time[datUseInds])
  ;;       nKappa  = N_ELEMENTS(kTimes)

  ;;    ENDELSE

  ;;    jvPD_inds  = VALUE_CLOSEST2(jvPlotData.time,curPotList[0].time[datUseInds])


  ;;    fitUseII   = WHERE(((kChi2[kFitUseInds] LE 1000) OR (gChi2[gFitUseInds] LE 1000)) AND $
  ;;                     (ABS(jvPlotData.cur[jvPD_inds]) GE 0.5),nFitUseII)
  ;;    IF nFitUseII LE 1 THEN STOP

  ;;    kFitUseInds = kFitUseInds[fitUseII]
  ;;    gFitUseInds = gFitUseInds[fitUseII]
  ;;    ;; datUseInds  = datUseInds[fitUseII]
  ;;    jvPD_inds  = jvPD_inds[fitUseII]

  ;;    ion     = 0
  ;;    pot     = jvPlotData.pot[jvPD_inds]
  ;;    cur     = jvPlotData.cur[jvPD_inds]*(ion ? 1.D : -1.D)
  ;;    potErr  = jvPlotData.potErr[jvPD_inds]
  ;;    curErr  = jvPlotData.curErr[jvPD_inds]
  ;;    T       = jvPlotData.TDown[jvPD_inds]
  ;;    N       = jvPlotData.NDown[jvPD_inds]

  ;;    kappas     = AStruct.kappa[kFitUseInds]

  ;;    kappa_fitT = AStruct.temperature[kFitUseInds]
  ;;    gauss_fitT = AStructGauss.temperature[gFitUseInds]
     
  ;;    kappa_fitN = AStruct.N[kFitUseInds]
  ;;    gauss_fitN = AStructGauss.N[gFitUseInds]
     
  ;;    ;; kappa_fitT = jvPlotData.TDown[jvPD_inds]
  ;;    ;; gauss_fitT = jvPlotData.TDown[jvPD_inds]
     
  ;;    ;; kappa_fitN = jvPlotData.NDown[jvPD_inds]
  ;;    ;; gauss_fitN = jvPlotData.NDown[jvPD_inds]
     
  ;;    kappa_fixA = [0,1,1,0]
  ;;    Gauss_fixA = [1,1,1,0]

  ;;    FIT_JV_TS_WITH_THEORETICAL_CURVES,pot,cur, $
  ;;                                      potErr,curErr, $
  ;;                                      T,N, $
  ;;                                      ;; USEINDS=useInds, $
  ;;                                      ;; /FLIP_CURRENT_SIGN, $
  ;;                                      KAPPA_FIXA=kappa_fixA, $
  ;;                                      GAUSS_FIXA=gauss_fixA, $
  ;;                                      KAPPA_A=kappa_A, $
  ;;                                      GAUSS_A=Gauss_A, $
  ;;                                      FIT_KAPPAS=kappas, $
  ;;                                      KAPPA_FIT_TEMPERATURE=kappa_fitT, $
  ;;                                      GAUSS_FIT_TEMPERATURE=gauss_fitT, $
  ;;                                      KAPPA_FIT_DENSITY=kappa_fitN, $
  ;;                                      GAUSS_FIT_DENSITY=gauss_fitN, $
  ;;                                      MAXITER=maxIter, $
  ;;                                      FTOL=fTol, $
  ;;                                      GTOL=gTol, $
  ;;                                      OUT_FITKAPPA_A=fitKappa_A, $
  ;;                                      OUT_FITGAUSS_A=fitGauss_A, $
  ;;                                      OUT_FITKAPPA_CUR=fitKappa_cur, $
  ;;                                      OUT_FITGAUSS_CUR=fitGauss_cur

  ;;    titleStr         = STRING(FORMAT='(A0," (T=",F0.1," eV, N=",G0.3," cm!U-3!N)")', $
  ;;                              'Orbit ' + STRCOMPRESS(orbit,/REMOVE_ALL),avgs_JVfit.T.avg,avgs_JVfit.N.avg)
  ;;    kappaName        = STRING(FORMAT='("$\kappa$=",F0.2,", R!DB!N=",G0.3)',fitKappa_A[0],fitKappa_A[3])
  ;;    gaussName        = STRING(FORMAT='("Maxwell, R!DB!N=",G0.3)',fitGauss_A[3])

  ;;    window1          = WINDOW(DIMENSION=[1000,800],BUFFER=savePlot)

  ;;    dSym = '*'
  ;;    kSym = 'tu'
  ;;    gSym = 'td'

  ;;    ;; that             = ERRORPLOT(X,Y,XError,YError, $
  ;;    that             = ERRORPLOT(pot,Cur,curErr, $
  ;;                                 SYMBOL=dSym, $
  ;;                                 LINESTYLE='', $
  ;;                                 NAME='Data', $
  ;;                                 TITLE=titleStr, $
  ;;                                 XTITLE='$\Phi$ (V)', $
  ;;                                 YTITLE='Current Density at 100 km ($\mu$A/m!U2!N)', $
  ;;                                 /CURRENT)
     
  ;;    ;; that          = PLOT(X,Y,SYMBOL='*',LINESTYLE='')
  ;;    this             = PLOT(pot,fitKappa_cur, $
  ;;                            NAME=kappaName, $
  ;;                            LINESTYLE='', $
  ;;                            SYMBOL=kSym, $
  ;;                            SYM_COLOR='BLUE', $
  ;;                            /OVERPLOT)
  ;;    those            = PLOT(pot,fitGauss_cur, $
  ;;                            NAME=gaussName, $
  ;;                            LINESTYLE='', $
  ;;                            SYMBOL=gSym, $
  ;;                            SYM_COLOR='Brown', $
  ;;                            /OVERPLOT)

  ;;    ;; legPos__data  = [(MAX(X)-MIN(X))*0.2+MIN(X),(MAX(Y)-MIN(Y))*0.95+MIN(Y)]
  ;;    ;; legPos           = [0.5,0.85]
  ;;    legPos           = [0.5,0.5]
  ;;    ;; leg           = LEGEND(TARGET=[that,this,those],POSITION=legPos__data,/DATA)
  ;;    leg              = LEGEND(TARGET=[that,this,those], $
  ;;                              POSITION=legPos)

  ;;    ;; fitKappa_A[3]    = 589
  ;;    ;; fitGauss_A[3]    = 1D3

  ;;    ;; R_B__Mguess      = fitGauss_A[3]
  ;;    ;; ;; fitGauss_cur     = KNIGHT_RELATION__DORS_KLETZING_4(gauss_fitT, $
  ;;    ;; fitGauss_cur     = KNIGHT_RELATION__DORS_KLETZING_4(T, $
  ;;    ;;                                                     gauss_fitN, $
  ;;    ;;                                                     pot, $
  ;;    ;;                                                     R_B__Mguess, $
  ;;    ;;                                                     /NO_MULT_BY_CHARGE)*1D6

  ;;    ;; R_B__Kguess      = fitKappa_A[3]
  ;;    ;; fitKappa_cur     = KNIGHT_RELATION__DORS_KLETZING_11(kappas, $
  ;;    ;;                                                      ;; kappa_fitT, $
  ;;    ;;                                                      T, $
  ;;    ;;                                                      kappa_fitN, $
  ;;    ;;                                                      pot, $
  ;;    ;;                                                      R_B__Kguess, $
  ;;    ;;                                                      /NO_MULT_BY_CHARGE)*1D6

  ;; ENDIF

END
