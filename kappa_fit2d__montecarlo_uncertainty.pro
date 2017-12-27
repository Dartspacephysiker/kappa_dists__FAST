;2017/12/21
;; Takes an "orig" struct (called "data" in this routine) from a kappaFits list generated as output from KAPPA_EFLUX_FIT2D.
;;
;; data has the following members
;; x: energy bins (eV)
;; y: differential number flux (#/cm^2-s-sr-eV)
;; yerror: uncertainty derived from counting statistics, then converted to differential number flux (#/cm^2-s-sr-eV)
;; energy_inds: The indices of the high- and low-energy bins, in that order, that were used to do the initial fit
;; Foundations laid in JOURNAL__20171221__BOOTSTRAP_ORB_1773_DISTS_TO_GET_BESTFIT_PARAM_ERRORS
PRO KAPPA_FIT2D__MONTECARLO_UNCERTAINTY,curDataStr,Pkappa,Pgauss,Pobs, $
                                        TIDFNSTR=tidFNStr, $
                                        NROLLS=nRolls, $
                                        NOT_MPFIT1D=not_mpFit1D, $
                                        KCURVEFIT_OPT=kCurvefit_opt, $
                                        OBSERVED_DIST=observed_dist, $
                                        BOOTSTRAP_MODE=bootstrap, $
                                        ADD_GAUSSIAN_ESTIMATE=add_gaussian_estimate, $
                                        MASS=mass, $
                                        FIT2DKAPPA_INFO=fit2DKappa_info, $
                                        SAVEFILE=saveFile, $
                                        SAVEDIR=saveDir

  COMPILE_OPT IDL2,STRICTARRSUBS

  @common__kappa_fit2d_structs.pro

  eRange_peak    = fit2DKappa_info.moment_info.obs_eRange

  CASE 1 OF
     KEYWORD_SET(KF2D__Curvefit_opt.fit1D__sc_eSpec): BEGIN

        ;;Get energy spectrum, if that's what you're into
        eSpec = GET_EN_SPEC__SINGLE_STRUCT( $
                curDataStr, $
                /RETRACE, $
                ANGLE=KF2D__SDTData_opt.electron_angleRange, $
                UNITS=units1D, $
                OUT_AVGFACTORARR=avgFactorArr, $
                OUT_NORMARR=normArr)

        ;; nAngles          = 1
        ;; nReqSCAngles     = 1

     END
     ELSE: BEGIN

     END
  ENDCASE  

        KAPPA__GET_PEAK_IND_AND_PEAK_ENERGY, $
           eSpec.v,eSpec.y, $
           peak_ind,peak_energy, $
           BULK_OFFSET=KF2D__Curvefit_opt.bulk_offset, $
           CHECK_FOR_HIGHER_FLUX_PEAKS=check_higher_peaks_set_peakEn, $
           MIN_PEAK_ENERGY=KF2D__Curvefit_opt.min_peak_energy, $
           MAX_PEAK_ENERGY=TAG_EXIST(KF2D__Curvefit_opt,'max_peak_energy') ? KF2D__Curvefit_opt.max_peak_energy : !NULL, $
           PEAK_ENERGY__START_AT_HIGHE=KF2D__Curvefit_opt.peak_energy__start_at_highE, $
           PHI__USE_ENERGY_BEFORE_PEAK=TAG_EXIST(KF2D__Curvefit_opt,'phi__use_energy_before_peak') ? KF2D__Curvefit_opt.phi__use_energy_before_peak : !NULL, $
           /CONTINUE_IF_NOMATCH

  ;; Get the data we want
  ;; energy_inds = data.energy_inds
  ;; IF KEYWORD_SET(observed_dist) THEN BEGIN
  ;;    Xin       = data.x[data.energy_inds[0]:data.energy_inds[1]]
  ;;    Yin       = data.y[data.energy_inds[0]:data.energy_inds[1]]
  ;;    Yin_error = data.yerror[data.energy_inds[0]:data.energy_inds[1]]
  ;;    ;; energy_inds = 
  ;; ENDIF ELSE BEGIN
  ;;    Xin       = data.x
  ;;    Yin       = data.y
  ;;    Yin_error = data.yerror
  ;; ENDELSE
  ;; Norig     = N_ELEMENTS(Xin)

  ;; Copy best-fit params
  A      = Pkappa
  AGauss = Pgauss

  A[1]   = Pobs[1]
  A[3]   = Pobs[3]

  IF N_ELEMENTS(kCurvefit_opt) GT 0 THEN BEGIN

     kCurvefit_opt.trim_energies_below_peak = 0 ; Make sure this is turned off
     ;; kCurvefit_opt.fit1D__sc_eSpec = 0          ;and this

     kCurvefit_opt.add_gaussian_estimate = KEYWORD_SET(add_gaussian_estimate)
     
     ;; 2017/12/27 Someday we'll fit both by nFlux, you see
     ;; CASE 1 OF
     ;;    KEYWORD_SET(kCurvefit_opt.fit1D__nFlux): BEGIN
     ;;       units = 'flux'
     ;;    END
     ;;    ELSE: BEGIN
           units = 'eFlux'
     ;;    END
     ;; ENDCASE
  ENDIF

  ;; How many sims?
  default_Nsim = 200
  Nsim = N_ELEMENTS(nRolls) GT 0 ? nRolls : default_Nsim

  ;; Generate random inds, kappas 'twixt 1.5 and 35 for data
  IF KEYWORD_SET(bootstrap) THEN BEGIN
     randomInds = FIX(Norig * RANDOMU(seed__eIndex,Norig,Nsim))
  ENDIF
  simKappas = 1.5 + (33.5 * RANDOMU(seed__kappa,Nsim))

  data_gaussError = RANDOMN(seed__data_error,curDataStr.NEnergy,curDataStr.NBins,Nsim)
  error_gaussEror = RANDOMN(seed__error_error,curDataStr.NEnergy,curDataStr.NBins,Nsim)

  ;; If not bootstrapping, set X and Yerror once and for all
  ;; IF ~KEYWORD_SET(bootstrap) THEN BEGIN
  ;;    Xorig = Xin
  ;;    Yorig_error = Yin_error
  ;; ENDIF

  ;; Init fitParam structs for MPFIT1D stuff
  ATmp              = DOUBLE([1e3,100.,3.0,0.01,0]) ;bulk E, T, kappa, dens, angle (useless)
  fixA              = [0,0,0,0,1]
  kappaParamStruct  = INIT_KAPPA_FITPARAM_INFO(ATmp,fixA)

  gaussParamStruct  = INIT_KAPPA_FITPARAM_INFO(TEMPORARY(ATmp),fixA)

  IF kappaParamStruct[1].limited[1] EQ 0 THEN BEGIN
     ;; When the upperbound of T is not limited, xTol will become irreducible in
     ;; the course of the fit and status 7 will be reported. To avoid this, we
     ;; allow xTol = unminimizable to be an allowable outcome
     ;;See INIT_KAPPA_FITPARAM_INFO<f> for more informaciones

     MC__OKStatus  = [1,2,3,4,7]    ;These are the acceptable outcomes of fitting with MPFIT2DFUN

  ENDIF ELSE BEGIN
     MC__OKStatus  = [1,2,3,4]
  ENDELSE

  keepK_iTime          = !NULL ;Keep track of which fits get to come home with us
  keepG_iTime          = !NULL ;Keep track of which fits get to come home with us
  successesK           = 0
  successesG           = 0
  totSuccessesK        = 0
  totSuccessesG        = 0
  ;; fit2DKappa_inf_list  = LIST()
  ;; fit2DGauss_inf_list  = LIST()
  FOR k=0,Nsim-1 DO BEGIN

     bounds_i = k

     ;;Units for later
     INIT_KAPPA_UNITCONV,curDataStr

     angle = 0 ;in degrees
     IF ~KEYWORD_SET(hemi) THEN BEGIN

        PRINT,"Assuming Northern Hemisphere ..."
        hemi = 'NORTH'
     ENDIF 

     CASE STRUPCASE(hemi) OF
        'NORTH': BEGIN
        END
        'SOUTH': BEGIN
           angle = 180
        END
     ENDCASE

     IF KEYWORD_SET(upgoing) THEN BEGIN
        angle += 180
     ENDIF

     IF angle EQ 360 THEN angle = 0

     CASE angle OF
        0: BEGIN

           junk = MIN(ABS(curDataStr.theta[curDataStr.NEnergy/2,*]),angle_i)

        END
        180: BEGIN

           junk = MIN(ABS(curDataStr.theta[curDataStr.NEnergy/2,*])-180,angle_i)
           
        END
     ENDCASE

     PRINT,FORMAT='(A0,F0.2," (",I0,")")',"Min angle (ind): ",curDataStr.theta[angle_i],angle_i

     shiftTheta   = 0           ;Not clear why shift is necessary, but makes things come out right
                                ;2017/12/27 Not sure why I thought this
     units2D      = 'eFlux'
     KAPPA_FIT2D__FIREINTHEHOLE, $
        curDataStr, $
        hadSuccessK, $
        hadSuccessG, $
        CURKAPPASTR=curKappaStr, $
        CURGAUSSSTR=curGaussStr, $
        ;; KAPPAFITS=kappaFits, $
        ;; GAUSSFITS=gaussFits, $
        KAPPAFITANGLE_INDEX=angle_i, $
        GAUSSFITANGLE_INDEX=angle_i, $
        TIMEFNSTR=tidFNStr, $
        UNITS=units2D, $
        SHIFTTHETA=shiftTheta, $
        PEAK_ENERGY=peak_energy, $
        ERANGE_PEAK=eRange_peak, $
        EXTEND_FITSTRUCT_ERANGE=extend_fitStruct_eRange, $
        /BF_GF__NORMALIZE_TO_VALS_AT_FITTED_ANGLE, $
        BF_GF__LOGSCALE_REDUCENEGFAC=bF_gF__logScale_reduceNegFac, $
        BF_GF__PLOT_BULKE_MODEL=bF_gF__plot_bulke_model, $
        BF_GF__PLOT_MODEL_BULKE_V_DATA_COMPARISON=bF_gF__plot_model_bulkE_v_data_comparison, $
        SAVE_BFUNC_GFUNC_PLOTS=save_bFunc_gFunc_plots, $
        PLOTDIR=plotDir, $
        ORBIT=orbit, $
        OUT_ESTIMATED_LC=estimated_lc, $
        KAPPAPARAMSTRUCT=kappaParamStruct, $
        GAUSSPARAMSTRUCT=gaussParamStruct, $
        KFIT2DPARAMSTRUCT=kFit2DParamStruct, $
        ;; KFIT2DPARAMSTRUCT=kFit2DParamStruct, $
        FIT2DKAPPA_INF_LIST=fit2DKappa_inf_list, $
        FIT2DGAUSS_INF_LIST=fit2DGauss_inf_list, $
        FIT2D__SHOW_AND_PROMPT__EACH_CANDIDATE=fit2d__show_each_candidate, $
        FIT2D__SHOW_ONLY_DATA=fit2D__show_only_data, $
        FIT2D__PA_ZRANGE=fit2D__PA_zRange, $
        FIT2D__SAVE_ALL_PLOTS=fit2D__save_all_plots, $
        PRINT_2DFITINFO=print_2DFitInfo, $
        PRINT_2DWININFO=print_2DWinInfo, $
        EPS=eps, $
        /MONTE_CARLO_MODE, $
        MC__OKSTATUS=MC__OKStatus

     ;;generate synthetic data
     ;; simulated data set
     IF KEYWORD_SET(bootstrap) THEN BEGIN
        Xorig = Xin[randomInds[*,k]]
        Yorig = Yin[randomInds[*,k]] + Yin_error[randomInds[*,k]]*data_gaussError[*,k]
        Yorig_error = Yin_error[randomInds[*,k]]*error_gaussError[*,k]
     ENDIF ELSE BEGIN
        Yorig = Yin + Yin_error*data_gaussError[*,k]
     ENDELSE

     ;; Update kappa
     A[2] = simKappas[k]

     IF ((k MOD 100) EQ 0) THEN PRINT,FORMAT='(A0,I05)',"iter_",k

     KAPPA_FIT2D__HORSESHOE,curDataStr, $
                            hadSuccess, $
                            ;; IS_MAXWELLIAN_FIT=is_maxwellian_fit, $
                            KFITPARAMSTRUCT=kappaParamStruct, $
                            FIT2D_INF_LIST=fit2DKappa_inf_list, $
                            ;; FIT2D__SHOW_AND_PROMPT__EACH_CANDIDATE=show_and_prompt, $
                            ;; FIT2D__SHOW_ONLY_DATA=fit2D__show_only_data, $
                            ;; FIT2D__PA_ZRANGE=fit2D__PA_zRange, $
                            ;; FIT2D__SAVE_ALL_PLOTS=fit2D__save_all_plots, $
                            ;; FIT2D__SHOW__IS_MAXWELLIAN_FIT=is_Maxwellian_fit, $
                            ;; FIT2D__SHOW__FITSTRING=fitString, $
                            PRINT_2DFITINFO=print_2DFitInfo, $
                            PRINT_2DWININFO=print_2DWinInfo, $
                            IN_ESTIMATED_LC=estimated_lc, $
                            EXTEND_FITSTRUCT_ERANGE=extend_fitStruct_eRange, $
                            UNITS=units, $
                            EPS=eps, $
                            /MONTE_CARLO_MODE, $
                            MC__OKSTATUS=MC__OKStatus

     ;; IF k EQ 0 THEN BEGIN
     ;;    kappaFits = REPLICATE(kappaFit,Nsim)
     ;;    IF KEYWORD_SET(add_gaussian_estimate) THEN gaussFits = REPLICATE(gaussFit,Nsim)
     ;; ENDIF
                                ;

     kappaFits[k] = kappaFit
     IF KEYWORD_SET(add_gaussian_estimate) THEN gaussFits[k] = gaussFit

  ENDFOR

  IF KEYWORD_SET(saveFile) THEN BEGIN
     PRINT,"Saving param ests to " + saveFile + " ..."
     IF N_ELEMENTS(saveDir) EQ 0 THEN saveDir = './'
     IF KEYWORD_SET(add_gaussian_estimate) THEN BEGIN
        SAVE,kappaFits,gaussFits,FILENAME=saveDir+saveFile
     ENDIF ELSE BEGIN
        SAVE,kappaFits,FILENAME=saveDir+saveFile
     ENDELSE
  ENDIF

END
