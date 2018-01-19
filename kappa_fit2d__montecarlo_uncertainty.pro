;2017/12/21
;; Takes an "orig" struct (called "data" in this routine) from a kappaFits list generated as output from KAPPA_EFLUX_FIT2D.
;;
;; data has the following members
;; x: energy bins (eV)
;; y: differential number flux (#/cm^2-s-sr-eV)
;; yerror: uncertainty derived from counting statistics, then converted to differential number flux (#/cm^2-s-sr-eV)
;; energy_inds: The indices of the high- and low-energy bins, in that order, that were used to do the initial fit
;; Foundations laid in JOURNAL__20171221__BOOTSTRAP_ORB_1773_DISTS_TO_GET_BESTFIT_PARAM_ERRORS
PRO KAPPA_FIT2D__MONTECARLO_UNCERTAINTY,kappaDataStr,gaussDataStr,Pkappa,Pgauss, $
                                        TIDFNSTR=tidFNStr, $
                                        NROLLS=nRolls, $
                                        NOT_MPFIT1D=not_mpFit1D, $
                                        KCURVEFIT_OPT=kCurvefit_opt, $
                                        OBSERVED_DIST=observed_dist, $
                                        BOOTSTRAP_MODE=bootstrap, $
                                        ADD_GAUSSIAN_ESTIMATE=add_gaussian_estimate, $
                                        MASS=mass, $
                                        FIT2DKAPPA_INFO=fit2DKappa_info, $
                                        FIT2DGAUSS_INFO=fit2DGauss_info, $
                                        MAKE_FIT2D_INFO=make_fit2D_info, $
                                        MAKE_FIT2DPARAMARRS=make_fit2DParamArrs, $
                                        MAKE_BFUNC_GFUNC_PLOTS=make_bFunc_gFunc_plots, $
                                        SAVE_BFUNC_GFUNC_PLOTS=save_bFunc_gFunc_plots, $
                                        SAVEFILE=saveFile, $
                                        SAVEDIR=saveDir, $
                                        PRINT_2DFITINFO=print_2DFitInfo, $
                                        PRINT_2DWININFO=print_2DWinInfo

  COMPILE_OPT IDL2,STRICTARRSUBS

  @common__kappa_fit2d_structs.pro

  IF ~(KEYWORD_SET(make_fit2D_info) OR KEYWORD_SET(make_fit2DParamArrs)) THEN BEGIN
     PRINT,"Why are we here, then?"
     STOP
  ENDIF

  nEnergiesK      = kappaDataStr.nEnergy
  ;; IF KEYWORD_SET(add_gaussian_estimate) THEN BEGIN
  nEnergiesG      = gaussDataStr.nEnergy

  eRange_peak    = fit2DKappa_info.extra_info.eRange_peak
  ;; make_fit2D_info = 1

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

  KAPPA_FIT2D__MONTECARLO__1DINIT,kappaDataStr, $
                                  IS_MAXWELLIAN=0, $
                                  ;; PEAK_IND=peak_indK, $
                                  NENERGIES=nEnergiesK, $
                                  PEAK_ENERGY=peak_energyK, $
                                  ERANGE_PEAK=eRange_peakK, $
                                  OUT_1DPARAMSTRUCT=kappaParamStruct, $
                                    MC__OKStatus=MC__OKStatus
                                  
  IF KEYWORD_SET(add_gaussian_estimate) THEN BEGIN

     KAPPA_FIT2D__MONTECARLO__1DINIT,gaussDataStr, $
                                     /IS_MAXWELLIAN, $
                                     ;; PEAK_IND=peak_indK, $
                                     NENERGIES=nEnergiesG, $
                                     PEAK_ENERGY=peak_energyG, $
                                     ERANGE_PEAK=eRange_peakG, $
                                     OUT_1DPARAMSTRUCT=gaussParamStruct, $
                                     MC__OKStatus=MC__OKStatus
     

  ENDIF

  ;; How many sims?
  default_Nsim = 200
  Nsim = N_ELEMENTS(nRolls) GT 0 ? nRolls : default_Nsim

  extraFactor = 3.0             ;In case some fits fail
  nMaxTry     = LONG(Nsim*extraFactor)

  ;; Generate random inds, kappas 'twixt 1.5 and 35 for data
  IF KEYWORD_SET(bootstrap) THEN BEGIN
     randomInds = FIX(Norig * RANDOMU(seed__eIndex,Norig, $
                                      nMaxTry))
  ENDIF
  simKappas = 1.5 + (33.5 * RANDOMU(seed__kappa,nMaxTry))

  dataK_gaussError = RANDOMN(seed__data_error,kappaDataStr.NEnergy,kappaDataStr.NBins, $
                            nMaxTry)
  ;; errorK_gaussError = RANDOMN(seed__error_error,kappaDataStr.NEnergy,kappaDataStr.NBins, $
                            ;; nMaxTry)
  ;; IF KEYWORD_SET(add_gaussian_estimate) THEN BEGIN
  dataG_gaussError = RANDOMN(seed__data_error,kappaDataStr.NEnergy,kappaDataStr.NBins, $
                            nMaxTry)
  ;; errorG_gaussError = RANDOMN(seed__error_error,kappaDataStr.NEnergy,kappaDataStr.NBins, $
  ;;                           nMaxTry)

  ;; If not bootstrapping, set X and Yerror once and for all
  ;; IF ~KEYWORD_SET(bootstrap) THEN BEGIN
  ;;    XorigK = Xin
  ;;    YorigK_error = Yin_error
  ;; ENDIF

  ;;Units for later
  INIT_KAPPA_UNITCONV,kappaDataStr
  ;; IF KEYWORD_SET(add_gaussian_estimate) THEN BEGIN
  INIT_KAPPA_UNITCONV,gaussDataStr

  angle = 0                     ;in degrees
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

        junk = MIN(ABS(kappaDataStr.theta[kappaDataStr.NEnergy/2,*]),angle_i)

     END
     180: BEGIN

        junk = MIN(ABS(kappaDataStr.theta[kappaDataStr.NEnergy/2,*])-180,angle_i)
        
     END
  ENDCASE

  PRINT,FORMAT='(A0,F0.2," (",I0,")")',"Min angle (ind): ", $
        kappaDataStr.theta[kappaDataStr.NEnergy/2,angle_i],angle_i

  shiftTheta     = 0              ;Not clear why shift is necessary, but makes things come out right
  ;;2017/12/27 Not sure why I thought this should be the case
  units2D        = 'eFlux'

  curKDataStr     = kappaDataStr   ;Here's the one that gets thrown around
  ;; IF KEYWORD_SET(add_gaussian_estimate) THEN BEGIN
  curGDataStr     = gaussDataStr   ;Here's the one that gets thrown around

  keepK_iTime    = !NULL        ;Keep track of which fits get to come home with us
  keepG_iTime    = !NULL        ;Keep track of which fits get to come home with us
  successesK     = 0
  successesG     = 0
  totSuccessesK  = 0
  totSuccessesG  = 0
  fit2DKappa_inf_list  = KEYWORD_SET(make_fit2D_info) ? LIST() : !NULL
  fit2DGauss_inf_list  = KEYWORD_SET(make_fit2D_info) ? LIST() : !NULL
  FOR k=0,nMaxTry-1 DO BEGIN

     bounds_i = k

     ;;generate synthetic data
     ;; simulated data set
     IF KEYWORD_SET(bootstrap) THEN BEGIN
        ;; curKDataStr.energy = kappaDataStr.energy[randomInds[*,*,k]]
        curKDataStr.data = kappaDataStr.data[randomInds[*,*,k]] + kappaDataStr.ddata[randomInds[*,*,k]]*dataK_gaussError[*,*,k]
        YorigK_error = Yin_error[randomInds[*,*,k]]*errorK_gaussError[*,*,k]

        curGDataStr.data = gaussDataStr.data[randomInds[*,*,k]] + gaussDataStr.ddata[randomInds[*,*,k]]*dataK_gaussError[*,*,k]
        YorigG_error = Yin_error[randomInds[*,*,k]]*errorG_gaussError[*,*,k]
     ENDIF ELSE BEGIN
        curKDataStr.data = kappaDataStr.data + kappaDataStr.ddata*dataK_gaussError[*,*,k] > 0.

        curGDataStr.data = gaussDataStr.data + gaussDataStr.ddata*dataG_gaussError[*,*,k] > 0.
     ENDELSE

     ;;copy paramStruct
     tmpKappaParamStruct = kappaParamStruct
     tmpGaussParamStruct = gaussParamStruct

     ;; Update kappa
     tmpKappaParamStruct[2].value = simKappas[k]
     
     IF ((k MOD 100) EQ 0) THEN PRINT,FORMAT='(A0,I05)',"iter_",k

     curKappaStr = curKDataStr

     KAPPA_FIT2D__FIREINTHEHOLE, $
        curKDataStr, $
        hadSuccessK, $
        hadSuccessG, $
        CURKAPPASTR=curKappaStr, $
        ;; CURGAUSSSTR=curGaussStr, $
        ;; KAPPAFITS=kappaFits, $
        ;; GAUSSFITS=gaussFits, $
        KAPPAFITANGLE_INDEX=angle_i, $
        ;; GAUSSFITANGLE_INDEX=angle_i, $
        TIMEFNSTR=tidFNStr, $
        UNITS=units2D, $
        SHIFTTHETA=shiftTheta, $
        PEAK_ENERGY=peak_energyK, $
        ERANGE_PEAK=eRange_peakK, $
        EXTEND_FITSTRUCT_ERANGE=extend_fitStruct_eRange, $
        MAKE_FIT2D_INFO=make_fit2D_info, $
        MAKE_FIT2DPARAMARRS=make_fit2DParamArrs, $
        KAPPAFIT2DPARAMARR=kappaFit2DParamArr, $
        GAUSSFIT2DPARAMARR=gaussFit2DParamArr, $
        /BF_GF__NORMALIZE_TO_VALS_AT_FITTED_ANGLE, $
        BF_GF__LOGSCALE_REDUCENEGFAC=bF_gF__logScale_reduceNegFac, $
        BF_GF__PLOT_BULKE_MODEL=bF_gF__plot_bulke_model, $
        BF_GF__PLOT_MODEL_BULKE_V_DATA_COMPARISON=bF_gF__plot_model_bulkE_v_data_comparison, $
        MAKE_BFUNC_GFUNC_PLOTS=make_bFunc_gFunc_plots, $
        SAVE_BFUNC_GFUNC_PLOTS=save_bFunc_gFunc_plots, $
        PLOTDIR=plotDir, $
        ORBIT=orbit, $
        OUT_ESTIMATED_LC=estimated_lc, $
        KAPPAPARAMSTRUCT=tmpKappaParamStruct, $
        GAUSSPARAMSTRUCT=tmpGaussParamStruct, $
        FIT2DKAPPA_INF_LIST=fit2DKappa_inf_list, $
        FIT2DGAUSS_INF_LIST=fit2DGauss_inf_list, $
        FIT2D__SHOW_AND_PROMPT__EACH_CANDIDATE=fit2d__show_each_candidate, $
        FIT2D__SHOW_ONLY_DATA=fit2D__show_only_data, $
        FIT2D__PA_ZRANGE=fit2D__PA_zRange, $
        FIT2D__SAVE_ALL_PLOTS=fit2D__save_all_plots, $
        PRINT_2DFITINFO=print_2DFitInfo, $
        EPS=eps, $
        /MONTE_CARLO_MODE, $
        MONTE_CARLO_KFP2D_HOLDOVER=MC_KFP2D_HOLDOVER, $
        MC__OKSTATUS=MC__OKStatus

     IF kCurvefit_opt.add_gaussian_estimate THEN BEGIN
        curGaussStr = curGDataStr

        KAPPA_FIT2D__FIREINTHEHOLE, $
           curGDataStr, $
           hadSuccessK, $
           hadSuccessG, $
           ;; CURKAPPASTR=curKappaStr, $
           CURGAUSSSTR=curGaussStr, $
           /ONLY_GAUSSIAN_ESTIMATE, $
           ;; KAPPAFITS=kappaFits, $
           ;; GAUSSFITS=gaussFits, $
           ;; KAPPAFITANGLE_INDEX=angle_i, $
           GAUSSFITANGLE_INDEX=angle_i, $
           TIMEFNSTR=tidFNStr, $
           UNITS=units2D, $
           SHIFTTHETA=shiftTheta, $
           PEAK_ENERGY=peak_energyG, $
           ERANGE_PEAK=eRange_peakG, $
           EXTEND_FITSTRUCT_ERANGE=extend_fitStruct_eRange, $
           MAKE_FIT2D_INFO=make_fit2D_info, $
           MAKE_FIT2DPARAMARRS=make_fit2DParamArrs, $
           KAPPAFIT2DPARAMARR=kappaFit2DParamArr, $
           GAUSSFIT2DPARAMARR=gaussFit2DParamArr, $
           /BF_GF__NORMALIZE_TO_VALS_AT_FITTED_ANGLE, $
           BF_GF__LOGSCALE_REDUCENEGFAC=bF_gF__logScale_reduceNegFac, $
           BF_GF__PLOT_BULKE_MODEL=bF_gF__plot_bulke_model, $
           BF_GF__PLOT_MODEL_BULKE_V_DATA_COMPARISON=bF_gF__plot_model_bulkE_v_data_comparison, $
           MAKE_BFUNC_GFUNC_PLOTS=make_bFunc_gFunc_plots, $
           SAVE_BFUNC_GFUNC_PLOTS=save_bFunc_gFunc_plots, $
           PLOTDIR=plotDir, $
           ORBIT=orbit, $
           OUT_ESTIMATED_LC=estimated_lc, $
           ;; KAPPAPARAMSTRUCT=tmpKappaParamStruct, $
           GAUSSPARAMSTRUCT=tmpGaussParamStruct, $
           FIT2DKAPPA_INF_LIST=fit2DKappa_inf_list, $
           FIT2DGAUSS_INF_LIST=fit2DGauss_inf_list, $
           FIT2D__SHOW_AND_PROMPT__EACH_CANDIDATE=fit2d__show_each_candidate, $
           FIT2D__SHOW_ONLY_DATA=fit2D__show_only_data, $
           FIT2D__PA_ZRANGE=fit2D__PA_zRange, $
           FIT2D__SAVE_ALL_PLOTS=fit2D__save_all_plots, $
           PRINT_2DFITINFO=print_2DFitInfo, $
           EPS=eps, $
           /MONTE_CARLO_MODE, $
           MONTE_CARLO_KFP2D_HOLDOVER=MC_KFP2D_HOLDOVER, $
           MC__OKSTATUS=MC__OKStatus

     ENDIF

     fail = KEYWORD_SET(kCurvefit_opt.add_gaussian_estimate) ? $
            ~(hadSuccessK AND KEYWORD_SET(hadSuccessG))      : $
            ~hadSuccessK

     IF fail THEN BEGIN

        PRINT,"Adding 'nother ..."

     ENDIF

     ;; IF N_ELEMENTS(kappaFit2DParamArr[0,*]) GE 990 THEN STOP
     
     IF KEYWORD_SET(make_fit2DParamArrs) THEN BEGIN

        works = N_ELEMENTS(kappaFit2DParamArr) EQ Nsim*5
        
        IF works AND KEYWORD_SET(kCurvefit_opt.add_gaussian_estimate) THEN BEGIN
           works = N_ELEMENTS(gaussFit2DParamArr) EQ Nsim*5

           IF ~works THEN STOP  ;Stop us if kappa is OK but Maxwellian isn't---'cause that's garbage
        ENDIF

        IF works THEN BREAK

     ENDIF

     IF KEYWORD_SET(make_fit2D_info) THEN BEGIN

        works = N_ELEMENTS(fit2DKappa_inf_list) EQ Nsim
        
        IF works AND KEYWORD_SET(kCurvefit_opt.add_gaussian_estimate) THEN BEGIN
           works = N_ELEMENTS(fit2DGauss_inf_list) EQ Nsim

           IF ~works THEN STOP ;Stop us if kappa is OK but Maxwellian isn't---'cause that's garbage
        ENDIF

        IF works THEN BREAK

     ENDIF

  ENDFOR
  
  IF KEYWORD_SET(saveFile) THEN BEGIN

     IF ~works THEN saveFile = saveFile.Replace('.sav','__BAD.sav')

     PRINT,"Saving param ests to " + saveFile + " ..."
     IF N_ELEMENTS(saveDir) EQ 0 THEN saveDir = './'

     saveStr = 'SAVE,'

     saveStr += 'fit2DKappa_info,fit2DGauss_info,'

     IF KEYWORD_SET(make_fit2DParamArrs) THEN BEGIN
        saveStr += 'kappaFit2DParamArr,'
        IF KEYWORD_SET(add_gaussian_estimate) THEN BEGIN
           saveStr += 'gaussFit2DParamArr,'
        ENDIF
     ENDIF

     IF KEYWORD_SET(make_fit2D_info) THEN BEGIN
        saveStr += 'fit2DKappa_inf_list,'
        IF KEYWORD_SET(add_gaussian_estimate) THEN BEGIN
           saveStr += 'fit2DGauss_inf_list,'
        ENDIF
     ENDIF

     saveStr += 'FILENAME=saveDir+saveFile'

     OK = EXECUTE(saveStr)
     IF ~OK THEN STOP

     ;; IF KEYWORD_SET(add_gaussian_estimate) THEN BEGIN
     ;;    SAVE,fit2DKappa_inf_list,fit2DGauss_inf_list,FILENAME=saveDir+saveFile
     ;; ENDIF ELSE BEGIN
     ;;    SAVE,fit2DKappa_inf_list,FILENAME=saveDir+saveFile
     ;; ENDELSE

  ENDIF

END
