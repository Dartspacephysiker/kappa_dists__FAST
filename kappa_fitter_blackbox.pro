;;10/08/16
PRO KAPPA_FITTER_BLACKBOX,orbit, $
                          ELECTRON_SOURCECONEANGLE=electron_angleRange, $
                          ELECTRON_LOSSCONEANGLE=electron_lca, $
                          ENERGY_ELECTRONS=energy_electrons, $
                          MIN_PEAK_ENERGY=min_peak_energy, $
                          EEB_OR_EES=eeb_or_ees, $
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
                          ONLY_1D_FITS=only_1D_fits, $
                          FIT1D__SOURCECONE_ENERGY_SPECTRUM=fit1D__sourceCone_energy_spectrum, $
                          FIT1D__NFLUX=fit1D__nFlux, $
                          FIT1D__WEIGHTING=fit1D__weighting, $
                          FIT1D__SAVE_PLOTSLICES=fit1D__save_plotSlices, $
                          FIT2D__WEIGHTING=fit2D__weighting, $
                          FIT2D__SHOW_EACH_CANDIDATE=fit2D__show_each_candidate, $
                          FIT2D__SHOW_ONLY_DATA=fit2D__show_only_data, $
                          FIT2D__PA_ZRANGE=fit2D__PA_zRange, $
                          FIT2D__SAVE_ALL_CANDIDATE_PLOTS=fit2D__save_all_candidate_plots, $
                          FIT2D__DENSITY_ANGLERANGE=fit2D__density_angleRange, $
                          ADD_ONECOUNT_CURVE=add_oneCount_curve, $
                          SAVE_KAPPA_PLOTS=save_kappa_plot, $
                          SAVEKAPPA_BONUSPREF=bonusPref, $
                          CLOSE_KAPPAPLOTS_AFTER_SAVE=close_kp_after_save, $
                          PLOTDIR=plotDir, $
                          SHOW_STRANGEWAY_SUMMARY=show_Strangeway_summary, $
                          SWAY__SAVE_PS=sway__save_ps, $
                          SWAY__SAVE_PNG=sway__save_png, $
                          SWAY__ADD_KAPPA_PANEL=sway__add_kappa_panel, $
                          SWAY__ADD_CHARE_PANEL=sway__add_chare_panel, $
                          SWAY__ADD_NEWELL_PANEL=sway__add_Newell_panel, $
                          SWAY__NEWELL_INTERP=sway__Newell_interp, $
                          SWAY__LOG_KAPPAPLOT=sway__log_kappaPlot, $
                          SHOW_KAPPA_SUMMARY=show_kappa_summary, $
                          KSUM__SAVE_PS=kSum__save_ps, $
                          KSUM__SAVE_PNG=kSum__save_png, $
                          KSUM__CONV_DESPECS_TO_NEWELL_INTERP=kSum__convert_to_Newell_interp, $
                          KSUM__ADD_CHI2_LINE=kSum__add_chi2_line, $
                          OUT_FIT2DK=fit2DK, $
                          OUT_FIT2DGAUSS=fit2DG, $
                          OUT_KAPPA_FIT_STRUCTS=kappaFits, $
                          OUT_GAUSS_FIT_STRUCTS=gaussFits, $
                          FIT2D_KAPPA_INF_LIST=fit2DKappa_inf_list, $
                          FIT2D_GAUSS_INF_LIST=fit2DGauss_inf_list, $
                          SAVE_DIFF_EFLUX_FILE=save_diff_eFlux_file, $
                          LOAD_DIFF_EFLUX_FILE=load_diff_eFlux_file, $
                          KAPPA_STATS__SAVE_STUFF=kStats__save_stuff, $
                          KAPPA_STATS__INCLUDE_THESE_STARTSTOPS=kStats__include_these_startstops,$
                          DEBUG__SKIP_TO_THIS_TIME=debug__skip_to_this_time, $
                          DEBUG__BREAK_ON_THIS_TIME=debug__break_on_this_time
  
  COMPILE_OPT IDL2

  IF N_ELEMENTS(plotDir) EQ 0 THEN BEGIN
     SET_PLOT_DIR,plotDir, $
                  /FOR_SDT, $
                  /ADD_TODAY, $
                  ADD_SUFF='/kappa_fits/Orbit_' + STRCOMPRESS(orbit,/REMOVE_ALL)
  ENDIF

  IF SIZE(load_diff_eFlux_file,/TYPE) NE 7 THEN BEGIN
     outDir               = '~/software/sdt/batch_jobs/saves_output_etc/'
  ENDIF

  IF STRUPCASE(eeb_or_ees) EQ 'EEB' THEN BEGIN
     IF N_ELEMENTS(spectra_average_interval) EQ 0 THEN spectra_average_interval = 4
  ENDIF

  ;; bounds                    = [160:210:50]/spectra_avg_interval & bounds  = bounds[uniq(bounds)]
  ;; bounds                    = [126:138]/spectra_avg_interval & bounds  = bounds[uniq(bounds)]
  ;; ;; bounds                    = [126:226:2]/spectra_avg_interval

  ;; bounds                       = [87,88,89,90,91,95,99]
  ;; bounds                        = [88:187]
  ;; bounds  = INDGEN(5)
  ;; Use survey bounds
  ;; 16  1997-02-07/20:49:41.338
  ;; 28  1997-02-07/20:49:48.934
  ;; eeb_or_ees                = 'ees'
  ;; bounds                    = [46:54:spectra_avg_interval]

  do_all_times                  = 1
  add_full_fits                 = 1
  fit2D__only_fit_peak_eRange   = 0
  fit2D__only_fit_aboveMin      = 1
  fit2D__only_fit_eAngles       = 1
  fit2D__keep_wholeFit          = 1  
  fit2D__bulk_e_anisotropy      = 1
  fit2D__exclude_lca_from_densCalc = 1
  fit2D__disable_bFunc          = 1
  ;; fit2D__bulk_e_anis_factor  = 0.3
  IF N_ELEMENTS(fit2D__density_angleRange) EQ 0 THEN fit2D__density_angleRange     = [-32,32]

  use_mpFit1D                   = 1

  fit1D__skip_bad_fits          = 1
  fit1D__show_and_prompt        = 0
  IF ~KEYWORD_SET(fit2D__show_each_candidate) THEN fit2D__show_each_candidate = 0
  fit2D__add_boundaries         = 1
  fit_fail__user_prompt         = 0

  synthPackage                  = 1
  average_over_angleRange       = 1

  t1                            = STR_TO_TIME(t1Str)
  t2                            = STR_TO_TIME(t2Str)

  estimate_A_from_data          = 1
  dont_print_estimates          = 1
  dont_print_fitinfo            = 1
  print_2DFitInfo               = 1

  n_below_peak                  = 3
  n_above_peak                  = 15
  n_below_peak2D                = 3
  n_above_peak2D                = 15
  dont_fit_below_thresh_value   = 0
  bulk_offset                   = 0

  add_gaussian_estimate         = 1
  ;; add_oneCount_curve            = 0

  no_plots                      = 1
  save_fitPlots                 = 1
  saveData                      = 1
  plot_full_fit                 = 1
  add_fitParams_text            = 1
  add_angle_label               = 1

  max_iter                      = 10000
  fit2D_max_iter                = 10000

  fit_tol                       = 1e-3
  fit2D_tol                     = 1e-5

  kappa_est                     = 10

  ;; T_est_fac                     = 1.3
  ;; N_est_fac                     = 7.0
  ;; bulkE_est_fac                 = 1.0

  ;; TGauss_est_fac                = 0.3
  ;; NGauss_est_fac                = 1.0
  ;; bulkEGauss_est_fac            = 1.0

  T_est_fac                     = 1.0
  N_est_fac                     = 1.0
  bulkE_est_fac                 = 1.0

  TGauss_est_fac                = 1.0
  NGauss_est_fac                = 1.0
  bulkEGauss_est_fac            = 1.0

  estFacs                       = {T:T_est_fac, $
                                   N:N_est_fac, $
                                   B_E:bulkE_est_fac, $
                                   TGauss:TGauss_est_fac, $
                                   NGauss:NGauss_est_fac, $
                                   B_EGauss:bulkEGauss_est_fac}


  ;;... And strings!!!!
  plotNamePref    = KEYWORD_SET(bonusPref) ? bonusPref : ''
  CASE 1 OF
     KEYWORD_SET(fit2D__only_fit_peak_eRange): BEGIN
        plotNamePref += '--only_fit_peak_eRange'
     END
     KEYWORD_SET(fit2D__only_fit_aboveMin): BEGIN
        plotNamePref += STRING(FORMAT='("--fit_above_",I0,"_eV")',min_peak_energy)
     END
     ELSE: BEGIN
     END
  ENDCASE
  
  IF KEYWORD_SET(fit2D__disable_bFunc) THEN BEGIN
     plotNamePref    += '--No_bFunc'
  ENDIF

  IF KEYWORD_SET(fit2D__exclude_lca_from_densCalc) THEN BEGIN
     plotNamePref    += '--exc_LCA'
  ENDIF

  fitFile              = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + '--' + 'orb_' + STRCOMPRESS(orbit,/REMOVE_ALL) + $
                         '--Kappa_fits_and_Gauss_fits--' + eeb_or_ees + '--horseshoe2d'

  IF KEYWORD_SET(save_diff_eFlux_file) THEN BEGIN
     save_diff_eFlux_to_file = 'orb_' + STRCOMPRESS(orbit,/REMOVE_ALL) + '--diff_eflux--' + eeb_or_ees + $
                               plotNamePref + '.sav'
  ENDIF
  IF KEYWORD_SET(load_diff_eFlux_file) THEN BEGIN
     CASE SIZE(load_diff_eFlux_file,/TYPE) OF
        7: BEGIN
           IF FILE_TEST(load_diff_eFlux_file) THEN BEGIN
              diff_eFlux_file = load_diff_eFlux_file
           ENDIF ELSE BEGIN
              PRINT,"Couldn't find file: " + load_diff_eFlux_file
              STOP
           ENDELSE
        END
        ELSE: BEGIN
           diff_eFlux_file   = 'orb_' + STRCOMPRESS(orbit,/REMOVE_ALL) + '--diff_eflux--' + eeb_or_ees + $
                               plotNamePref + '.sav'
        END
     ENDCASE
  ENDIF

  fitFile                       = fitFile + plotNamePref + '.sav'

  IF KEYWORD_SET(restore_fitFile) THEN BEGIN

     PRINT,'Restoring ' + fitFile + ' ...'
     RESTORE,outDir+fitFile

  ENDIF ELSE BEGIN

     KAPPA_EFLUX_FIT2D, $
        T1=t1, $
        T2=t2, $
        SDT_TIME_INDS=bounds, $
        DO_ALL_TIMES=do_all_times, $
        ENERGY_ELECTRONS=energy_electrons, $
        LOAD_DAT_FROM_FILE=diff_eFlux_file, $
        LOAD_DIR=outDir, $
        SAVE_DIFF_EFLUX_TO_FILE=save_diff_eFlux_to_file, $
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
        KAPPA_EST=kappa_est, $
        SDT_DAT=dat, $
        ESTIMATE_FITPARAMS_FROM_SDT_DAT=estimate_A_from_data, $
        ESTIMATE_FACTORS=estFacs, $
        DONT_PRINT_ESTIMATES=dont_print_estimates, $
        DONT_PRINT_FITINFO=dont_print_fitInfo, $
        ONLY_1D_FITS=only_1D_fits, $
        FIT1D__MAX_ITERATIONS=max_iter, $
        FIT1D__TOLERANCE=fit_tol, $
        FIT1D__AVERAGE_OVER_ANGLERANGE=average_over_angleRange, $
        FIT1D__SOURCECONE_ENERGY_SPECTRUM=fit1D__sourceCone_energy_spectrum, $
        FIT1D__NFLUX=fit1D__nFlux, $
        FIT1D__WEIGHTING=fit1D__weighting, $
        FIT1D__SKIP_BAD_FITS=fit1D__skip_bad_fits, $
        FIT1D__SHOW_AND_PROMPT=fit1D__show_and_prompt, $
        FIT1D__USER_PROMPT_ON_FAIL=fit1D_fail__user_prompt, $
        FIT1D__SAVE_PLOTSLICES=fit1D__save_plotSlices, $
        FIT2D__KEEP_WHOLEFIT=fit2D__keep_wholeFit, $
        FIT2D__WEIGHTING=fit2D__weighting, $
        FIT2D__ONLY_FIT_ERANGE_AROUND_PEAK=fit2D__only_fit_peak_eRange, $
        FIT2D__ONLY_FIT_ERANGE_ABOVE_MIN=fit2D__only_fit_aboveMin, $
        FIT2D__SHOW_AND_PROMPT__EACH_CANDIDATE=fit2D__show_each_candidate, $
        FIT2D__SHOW_ONLY_DATA=fit2D__show_only_data, $
        FIT2D__PA_ZRANGE=fit2D__PA_zRange, $
        FIT2D__SAVE_ALL_CANDIDATE_PLOTS=fit2D__save_all_candidate_plots, $
        FIT2D__PRINT_FITINFO=print_2DFitInfo, $
        FIT2D__TOLERANCE=fit2D_tol, $
        FIT2D__MAX_ITERATIONS=fit2D_max_iter, $
        FIT2D__ONLY_FIT_ELECTRON_ANGLES=fit2D__only_fit_eAngles, $
        FIT2D__LOSSCONE_ANGLE=fit2D__lossCone_angle, $
        FIT2D__USE_BULK_E_ANISOTROPY=fit2D__bulk_e_anisotropy, $
        FIT2D__BULK_E_ANISO_FACTOR=fit2D__bulk_e_anis_factor, $
        FIT2D__DENSITY_ANGLERANGE=fit2D__density_angleRange, $
        FIT2D__EXCLUDE_LCA_FROM_DENSCALC=fit2D__exclude_lca_from_densCalc, $
        FIT2D__DISABLE_BFUNC=fit2D__disable_bFunc, $
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
        OUT_FIT2DKAPPA_INF_LIST=fit2DKappa_inf_list, $
        OUT_FIT2DGAUSS_INF_LIST=fit2DGauss_inf_list, $
        OUT_SYNTH_SDT_STRUCTS=synthPackage, $
        ADD_FULL_FITS=add_full_fits, $
        OUT_ERANGE_PEAK=out_eRange_peak, $
        OUT_PARAMSTR=out_paramStr, $
        OUT_STRINGS=strings, $
        TXTOUTPUTDIR=txtOutputDir,$
        DEBUG__SKIP_TO_THIS_TIME=debug__skip_to_this_time, $
        DEBUG__BREAK_ON_THIS_TIME=debug__break_on_this_time

     CASE eeb_or_ees OF
        'eeb': BEGIN
           GET_2DT_TS,'j_2d_b','fa_eeb',T1=t1,T2=t2,NAME='Je',ENERGY=energy_electrons,ANGLE=electron_angleRange
           GET_2DT_TS,'je_2d_b','fa_eeb',T1=t1,T2=t2,NAME='Jee',ENERGY=energy_electrons,ANGLE=electron_angleRange
        END
        'ees': BEGIN
           GET_2DT_TS,'j_2d_b','fa_ees',T1=t1,T2=t2,NAME='Je',ENERGY=energy_electrons,ANGLE=electron_angleRange
           GET_2DT_TS,'je_2d_b','fa_ees',T1=t1,T2=t2,NAME='Jee',ENERGY=energy_electrons,ANGLE=electron_angleRange
        END
     ENDCASE

     GET_DATA,'Je',DATA=je
     GET_DATA,'Jee',DATA=jee

     PARSE_KAPPA_FIT_STRUCTS,kappaFits, $
                             A=a, $
                             STRUCT_A=Astruct, $
                             TIME=time, $
                             NAMES_A=A_names, $
                             CHI2=chi2, $
                             PVAL=pVal, $
                             FITSTATUS=fitStatus, $
                             USE_MPFIT1D=use_mpFit1D

     PARSE_KAPPA_FIT_STRUCTS,gaussFits, $
                             A=AGauss, $
                             STRUCT_A=AStructGauss, $
                             TIME=time, $
                             NAMES_A=AGauss_names, $
                             CHI2=chi2Gauss, $
                             PVAL=pValGauss, $
                             FITSTATUS=gaussfitStatus, $
                             USE_MPFIT1D=use_mpFit1D


     PRINT_KAPPA_LOOP_FIT_SUMMARY,fitStatus,gaussfitStatus


     IF KEYWORD_SET(saveData) THEN BEGIN
        saveStr = 'SAVE,'
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

        IF N_ELEMENTS(kappaFits) GT 0 THEN BEGIN
           saveStr += 'kappaFits,'
        ENDIF

        IF N_ELEMENTS(gaussFits) GT 0 THEN BEGIN
           saveStr += 'gaussFits,'
        ENDIF

        IF N_ELEMENTS(synthPackage) GT 0 THEN BEGIN
           saveStr += 'synthPackage,'
        ENDIF

        IF N_ELEMENTS(strings) GT 0 THEN BEGIN
           saveStr += 'strings,'
        ENDIF

        IF N_ELEMENTS(fit2DKappa_inf_list) GT 0 THEN BEGIN
           saveStr += 'fit2DKappa_inf_list,'
        ENDIF

        IF N_ELEMENTS(fit2DGauss_inf_list) GT 0 THEN BEGIN
           saveStr += 'fit2DGauss_inf_list,'
        ENDIF

        PRINT,'Saving ' + fitFile + ' ...'

        saveStr += 'FILENAME=outDir+fitFile'
        good     = EXECUTE(saveStr)
     ENDIF

     PRINT,"DONE!"

  ENDELSE

  IF ~KEYWORD_SET(only_1D_fits) THEN BEGIN

     fit2DK = PARSE_KAPPA_FIT2D_INFO_LIST_V2(fit2DKappa_inf_list, $
                                             SOUTH=south, $
                                             FIT_TYPE='Kappa', $
                                             HIGHDENSITY_THRESHOLD=highDens_thresh, $
                                             LOWDENSITY_THRESHOLD=lowDens_thresh, $
                                             CHI2_OVER_DOF_THRESHOLD=chi2_over_dof_thresh, $
                                             CHI2_THRESHOLD=chi2_thresh, $
                                             DIFFEFLUX_THRESHOLD=diffEflux_thresh, $
                                             N_PEAKS_ABOVE_DEF_THRESHOLD=nPkAbove_dEF_thresh, $
                                             DENS_ANGLERANGE=fit2D__density_angleRange, $
                                             OUT_GOOD_I=includeK_i, $
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
                                             DENS_ANGLERANGE=fit2D__density_angleRange, $
                                             IN_GOOD_I=includeK_i, $
                                             OUT_GOOD_I=includeG_i, $
                                             /DONT_SHRINK_PARSED_STRUCT) 
     

     ;;Now shrink everyone
     IF ~( ARRAY_EQUAL(includeK_i,includeG_i)                          AND $
           (N_ELEMENTS(kappaFits)  EQ N_ELEMENTS(gaussFits)           ) AND $
           (N_ELEMENTS(kappaFits)  EQ N_ELEMENTS(fit2DKappa_inf_list) ) AND $
           (N_ELEMENTS(includeK_i) EQ N_ELEMENTS(fit2DKappa_inf_list) ) ) THEN BEGIN

        IF N_ELEMENTS(kappaFits) NE N_ELEMENTS(gaussFits) THEN STOP
        IF N_ELEMENTS(fit2DKappa_inf_list) NE N_ELEMENTS(fit2DGauss_inf_list) THEN STOP
        IF N_ELEMENTS(kappaFits) NE N_ELEMENTS(fit2DKappa_inf_list) THEN STOP

        include_i = CGSETINTERSECTION(includeK_i,includeG_i)

        fit2DK            = {SDT          : fit2dK.SDT [include_i], $
                             fitParams    : fit2DK.fitParams[*,include_i], $
                             fitDens      : fit2DK.fitDens  [include_i], $
                             chi2         : fit2DK.chi2     [include_i], $
                             errMsg       : fit2DK.errMsg   [include_i], $
                             status       : fit2DK.status   [include_i], $
                             nfEv         : fit2DK.nfEv     [include_i], $
                             ;; best_resid   : best_resid [include_i], $
                             pFree_index  : fit2DK.pFree_index[*,include_i], $
                             ;; best_fJac    : best_fJac  [include_i], $
                             nPegged      : fit2DK.nPegged    [include_i], $
                             nFree        : fit2DK.nFree      [include_i], $
                             dof          : fit2DK.dof        [include_i], $
                             covar        : fit2DK.covar  [*,*,include_i], $
                             pError       : fit2DK.pError   [*,include_i], $
                             nIter        : fit2DK.nIter      [include_i]}
        
        fit2DG            = {SDT          : fit2DG.SDT [include_i], $
                             fitParams    : fit2DG.fitParams[*,include_i], $
                             fitDens      : fit2DG.fitDens  [include_i], $
                             chi2         : fit2DG.chi2     [include_i], $
                             errMsg       : fit2DG.errMsg   [include_i], $
                             status       : fit2DG.status   [include_i], $
                             nfEv         : fit2DG.nfEv     [include_i], $
                             ;; best_resid   : best_resid [include_i], $
                             pFree_index  : fit2DG.pFree_index[*,include_i], $
                             ;; best_fJac    : best_fJac  [include_i], $
                             nPegged      : fit2DG.nPegged    [include_i], $
                             nFree        : fit2DG.nFree      [include_i], $
                             dof          : fit2DG.dof        [include_i], $
                             covar        : fit2DG.covar  [*,*,include_i], $
                             pError       : fit2DG.pError   [*,include_i], $
                             nIter        : fit2DG.nIter      [include_i]}

        fit2DKappa_inf_list = fit2DKappa_inf_list[include_i]
        fit2DGauss_inf_list = fit2DGauss_inf_list[include_i]

        kappaFits = kappaFits[include_i]
        gaussFits = gaussFits[include_i]

     ENDIF

     IF KEYWORD_SET(show_post_plots) THEN BEGIN

        POST_KAPPA2D_FIT_PLOTS,fit2DK,fit2DG,orbit,plotNamePref,plotDir,save_kappa_plot, $
                               CLOSE_KAPPAPLOTS_AFTER_SAVE=close_kp_after_save                            

     ENDIF


     ;; pap   = PLOT(time-time[0],this.fitParams[2,*],YLOG=1,SYMBOL='*',LINESTYLE='')
     PRINT,FORMAT='("(N w/ k ≤ 2.5)/nTot : ",I0,"/",I0)', $
           N_ELEMENTS(WHERE(fit2DK.fitParams[2,*] LE 2.5)), $
           N_ELEMENTS(fit2DK.nIter)

     IF KEYWORD_SET(show_Strangeway_summary) THEN BEGIN
        SINGLE_RJS_SUMMARY,STR_TO_TIME(t1Str),STR_TO_TIME(t2Str), $
                           TPLT_VARS=tPlt_vars, $
                           EEB_OR_EES=eeb_OR_ees, $
                           ENERGY_ELECTRONS=energy_electrons, $
                           TLIMIT_NORTH=tlimit_north, $
                           TLIMIT_SOUTH=tlimit_south, $
                           TLIMIT_ALL=tlimit_all, $
                           /SCREEN_PLOT, $
                           ADD_KAPPA_PANEL=sway__add_kappa_panel, $
                           ADD_CHARE_PANEL=sway__add_chare_panel, $
                           ADD_NEWELL_PANEL=sway__add_Newell_panel, $
                           NEWELL_2009_INTERP=sway__Newell_interp, $
                           LOG_KAPPAPLOT=sway__log_kappaPlot, $
                           USE_FAC_V=use_fac_v, $
                           USE_FAC_NOT_V=use_fac, $
                           NO_BLANK_PANELS=no_blank_panels, $
                           FIT2DKAPPA_INF_LIST=fit2DKappa_inf_list, $
                           FIT2DGAUSS_INF_LIST=fit2DGauss_inf_list, $
                           KAPPAFITS=kappaFits, $
                           GAUSSFITS=gaussFits, $
                           CHI2_THRESHOLD=chi2_thresh, $
                           CHI2_OVER_DOF_THRESHOLD=chi2_over_dof_thresh, $
                           HIGHDENSITY_THRESHOLD=highDens_thresh, $
                           LOWDENSITY_THRESHOLD=lowDens_thresh, $
                           DIFFEFLUX_THRESHOLD=diffEflux_thresh, $
                           N_PEAKS_ABOVE_DEF_THRESHOLD=nPkAbove_dEF_thresh, $
                           SAVE_PS=sway__save_ps, $
                           SAVE_PNG=sway__save_png, $
                           SAVEKAPPA_BONUSPREF=bonusPref, $
                           PLOTDIR=plotDir

     ENDIF

     IF KEYWORD_SET(show_kappa_summary) THEN BEGIN
        
        IF KEYWORD_SET(show_Strangeway_summary) THEN tPlt_vars = !NULL ;Clear 'em out

        SINGLE_KAPPA_SUMMARY,STR_TO_TIME(t1Str),STR_TO_TIME(t2Str), $
                             TPLT_VARS=tPlt_vars, $
                             EEB_OR_EES=eeb_or_ees, $
                             ENERGY_ELECTRONS=energy_electrons, $
                             TLIMIT_NORTH=tlimit_north, $
                             TLIMIT_SOUTH=tlimit_south, $
                             TLIMIT_ALL=tlimit_all, $
                             /SCREEN_PLOT, $
                             USE_FAC_V=use_fac_v, $
                             USE_FAC_NOT_V=use_fac, $
                             NO_BLANK_PANELS=no_blank_panels, $
                             ADD_CHI2_LINE=kSum__add_chi2_line, $
                             FIT2DKAPPA_INF_LIST=fit2DKappa_inf_list, $
                             FIT2DGAUSS_INF_LIST=fit2DGauss_inf_list, $
                             KAPPAFITS=kappaFits, $
                             GAUSSFITS=gaussFits, $
                             CHI2_THRESHOLD=chi2_thresh, $
                             CHI2_OVER_DOF_THRESHOLD=chi2_over_dof_thresh, $
                             HIGHDENSITY_THRESHOLD=highDens_thresh, $
                             LOWDENSITY_THRESHOLD=lowDens_thresh, $
                             DIFFEFLUX_THRESHOLD=diffEflux_thresh, $
                             N_PEAKS_ABOVE_DEF_THRESHOLD=nPkAbove_dEF_thresh, $
                             CONVERT_DESPECS_TO_NEWELL_INTERP=kSum__convert_to_Newell_interp, $
                             SAVE_PS=kSum__save_ps, $
                             SAVE_PNG=kSum__save_png, $
                             SAVEKAPPA_BONUSPREF=bonusPref, $
                             PLOTDIR=plotDir, $
                             SAVE_FOR_OFFLINE=save_for_offline, $
                             LOAD_FROM_OFFLINE=load_from_offline, $
                             KAPPA_STATS__SAVE_STUFF=kStats__save_stuff, $
                             KAPPA_STATS__INCLUDE_THESE_STARTSTOPS=kStats__include_these_startstops

     ENDIF

  ENDIF

END
