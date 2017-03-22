;;10/08/16
PRO KAPPA_FITTER_BLACKBOX,orbit, $
                          ELECTRON_SOURCECONEANGLE=electron_angleRange, $
                          ELECTRON_LOSSCONEANGLE=electron_lca, $
                          ENERGY_ELECTRONS=energy_electrons, $
                          JUST_DIFF_EFLUX=just_diff_eFlux, $
                          DIFF_EFLUX=diff_eFlux, $
                          DEF_ONECOUNT=dEF_oneCount, $
                          UPGOING=upgoing, $
                          MIN_PEAK_ENERGY=min_peak_energy, $
                          MAX_PEAK_ENERGY=max_peak_energy, $
                          PEAK_ENERGY__START_AT_HIGHE=peak_energy__start_at_highE, $
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
  
  COMPILE_OPT IDL2,STRICTARRSUBS

  t1                            = STR_TO_TIME(t1Str)
  t2                            = STR_TO_TIME(t2Str)

  IF N_ELEMENTS(plotDir) EQ 0 THEN BEGIN
     SET_PLOT_DIR,plotDir, $
                  /FOR_SDT, $
                  /ADD_TODAY, $
                  ADD_SUFF='/kappa_fits/Orbit_' + STRCOMPRESS(orbit,/REMOVE_ALL)
  ENDIF

  outDir                        = '~/software/sdt/batch_jobs/saves_output_etc/'

  @kappa_fitter__defaults.pro
  
  ;;... And strings!!!!
  KAPPA_FITTER__FSTRINGS, $
     ORBIT=orbit, $
     EEB_OR_EES=eeb_or_ees, $
     ELECTRON_ANGLERANGE=electron_angleRange ,$
     BONUSPREF=bonusPref ,$
     SAVE_DIFF_EFLUX_FILE=save_diff_eFlux_file ,$
     SAVE_DIFF_EFLUX_TO_FILE=save_diff_eFlux_to_file ,$
     LOAD_DIFF_EFLUX_FILE=load_diff_eFlux_file ,$
     OUT_DIFF_EFLUX_FILE=diff_eFlux_file, $
     FIT2D__ONLY_FIT_PEAK_ERANGE=fit2D__only_fit_peak_eRange ,$
     FIT2D__ONLY_FIT_ABOVEMIN=fit2D__only_fit_aboveMin ,$
     MIN_PEAK_ENERGY=min_peak_energy, $
     FIT2D__DISABLE_BFUNC=fit2D__disable_bFunc ,$
     FIT2D__EXCLUDE_LCA_FROM_DENSCALC=fit2D__exclude_lca_from_densCalc ,$
     SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
     FITFILE=fitFile, $
     LOADDIR=outDir

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
        JUST_DIFF_EFLUX=just_diff_eFlux, $
        DIFF_EFLUX=diff_eFlux, $
        DEF_ONECOUNT=dEF_oneCount, $
        LOAD_DAT_FROM_FILE=diff_eFlux_file, $
        LOAD_DIR=outDir, $
        SAVE_DIFF_EFLUX_TO_FILE=save_diff_eFlux_to_file, $
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

       MOMENT_SUITE_2D,diff_eFlux, $
                       ENERGY=energy, $
                       ARANGE__MOMENTS=aRange__moments, $
                       ARANGE__CHARE=aRange__charE, $
                       SC_POT=sc_pot, $
                       EEB_OR_EES=eeb_or_ees, $
                       ERROR_ESTIMATES=error_estimates, $
                       /MAP_TO_100KM, $ 
                       ORBIT=orbit, $
                       QUIET=quiet, $
                       OUT_N=n, $
                       OUT_J_=j, $
                       OUT_JE=je, $
                       OUT_T=T, $
                       OUT_CHARE=charE, $
                       OUT_CURRENT=cur, $
                       OUT_JJE_COVAR=jje_coVar, $
                       OUT_ERRORS=errors, $
                       OUT_ERR_N=nErr, $
                       OUT_ERR_J_=jErr, $
                       OUT_ERR_JE=jeErr, $
                       OUT_ERR_T=TErr, $
                       OUT_ERR_CHARE=charEErr, $
                       OUT_ERR_CURRENT=curErr

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

     ;;2017/03/21
     ;;LOOK: All you've got to do is figure out a way to consistently get the ion contribution in here, and you're golden. Understand?
     ;;THEN you can see what it's really like
     R_B__Mguess = 900
     gaussCur = KNIGHT_RELATION__DORS_KLETZING_4(AStructGauss.temperature, $
                                                 AStructGauss.N, $
                                                 jvplotdata.pot[useInds], $
                                                 R_B__Mguess, $
                                                 /NO_MULT_BY_CHARGE)

     R_B__Kguess = 900
     kappaCur = KNIGHT_RELATION__DORS_KLETZING_11(AStruct.kappa,AStruct.temperature, $
                                                  AStruct.N, $
                                                  jvplotdata.pot[useInds], $
                                                  R_B__guess, $
                                                  /NO_MULT_BY_CHARGE)


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

        IF KEYWORD_SET(error_estimates) THEN BEGIN
           saveStr += 'nErr,jErr,TErr,errors,'
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
