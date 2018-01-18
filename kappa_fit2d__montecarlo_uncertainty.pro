;2017/12/21
;; Takes an "orig" struct (called "data" in this routine) from a kappaFits list generated as output from KAPPA_EFLUX_FIT2D.
;;
;; data has the following members
;; x: energy bins (eV)
;; y: differential number flux (#/cm^2-s-sr-eV)
;; yerror: uncertainty derived from counting statistics, then converted to differential number flux (#/cm^2-s-sr-eV)
;; energy_inds: The indices of the high- and low-energy bins, in that order, that were used to do the initial fit
;; Foundations laid in JOURNAL__20171221__BOOTSTRAP_ORB_1773_DISTS_TO_GET_BESTFIT_PARAM_ERRORS
PRO KAPPA_FIT2D__MONTECARLO_UNCERTAINTY,realDataStr,Pkappa,Pgauss, $
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

  nEnergies       = realDataStr.nEnergy
  eRange_peak    = fit2DKappa_info.extra_info.eRange_peak
  ;; make_fit2D_info = 1

  CASE 1 OF
     KEYWORD_SET(KF2D__Curvefit_opt.fit1D__sc_eSpec): BEGIN

        ;;Get energy spectrum, if that's what you're into
        eSpec = GET_EN_SPEC__SINGLE_STRUCT( $
                realDataStr, $
                /RETRACE, $
                ANGLE=KF2D__SDTData_opt.electron_angleRange, $
                UNITS=units1D, $
                OUT_AVGFACTORARR=avgFactorArr, $
                OUT_NORMARR=normArr)

        Xorig    = eSpec.v
        Yorig    = eSpec.y
        worig    = eSpec.yerr
        ;; nAngles          = 1
        ;; nReqSCAngles     = 1

     END
     ELSE: BEGIN

     END
  ENDCASE  

  KAPPA__GET_PEAK_IND_AND_PEAK_ENERGY, $
     Xorig,Yorig, $
     peak_ind,peak_energy, $
     BULK_OFFSET=KF2D__Curvefit_opt.bulk_offset, $
     CHECK_FOR_HIGHER_FLUX_PEAKS=check_higher_peaks_set_peakEn, $
     MIN_PEAK_ENERGY=KF2D__Curvefit_opt.min_peak_energy, $
     MAX_PEAK_ENERGY=TAG_EXIST(KF2D__Curvefit_opt,'max_peak_energy') ? KF2D__Curvefit_opt.max_peak_energy : !NULL, $
     PEAK_ENERGY__START_AT_HIGHE=KF2D__Curvefit_opt.peak_energy__start_at_highE, $
     /CONTINUE_IF_NOMATCH, $
     ONECOUNT_STR=oneCurve     

  IF peak_energy EQ -1 THEN STOP

  maxEInd           = (peak_ind + KF2D__Curvefit_opt.n_below_peak) < (nEnergies-1)
  minEInd           = (peak_ind - KF2D__Curvefit_opt.n_above_peak) > 0

  IF KEYWORD_SET(KF2D__Curvefit_opt.estimate_A_from_data) THEN BEGIN 

     KAPPA__GET_A_ESTIMATES,realDataStr,Xorig,Yorig, $
                            minEInd,maxEInd,nEnergies, $
                            peak_ind,peak_energy,eRange_peak, $
                            KAPPA_EST=KF2D__Curvefit_opt.fitA[2], $
                            ;; MASS=mass, $
                            E_ANGLE=KF2D__SDTData_opt.electron_angleRange, $
                            ANGLES=tempAngleEstRange, $
                            N_ANGLES_IN_RANGE=nAngles, $
                            ;; BULKANGLE_STRUCT=angleStr, $
                            ;; DONT_TAKE_STOCK_OF_BULKANGLE=dont_take_stock_of_bulkangle, $
                            ADD_GAUSSIAN_ESTIMATE=KF2D__Curvefit_opt.add_gaussian_estimate, $
                            USE_SDT_GAUSSIAN_FIT=KF2D__Curvefit_opt.use_SDT_Gaussian_fit, $
                            ESTFACS=estFacs, $
                            PHI__USE_ENERGY_BEFORE_PEAK=TAG_EXIST(KF2D__Curvefit_opt,'phi__use_energy_before_peak') ? KF2D__Curvefit_opt.phi__use_energy_before_peak : !NULL, $
                            A_OUT=A, $
                            AGAUSS_OUT=AGauss, $
                            DONT_PRINT_ESTIMATES=dont_print_estimates, $
                            /TEST_NOREV, $
                            TEMPERATURE_TYPE=KF2D__SDTData_opt.fit2D__temperature_type, $
                            UNITS=units1D

     ;; 2018/01/10 Use bulk energy (set by PHI__USE_ENERGY_BEFORE_PEAK) for 2D bulkE initial estimate
     bulkEOrigEstimate = A[0]

  ENDIF ELSE BEGIN
     A           = DOUBLE([peak_energy,T,kappa,n_est,0.000001,5.68e-6,0])
  ENDELSE
  
  energy_inds    = [minEInd,maxEInd]

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

  ;; Init fitParam structs for MPFIT1D stuff
  kappa_fixA        = [0, $     ;Vary bulk E [0]                              
                       KF2D__Curvefit_opt.fit1D__clampTemperature, $ ;Temperature [1] (maybe)                         
                       0, $                                          ;kappa       [2]
                       KF2D__CurveFit_opt.fit1D__clampDensity    , $ ;and density [3] (but not angle)
                       1] 
  
  gauss_fixA        = [0, $                                                ;Vary bulk E [0]
                       KF2D__Curvefit_opt.fit1D__clampTemperature, $       ;Temperature [1] (maybe)
                       1, $
                       KF2D__CurveFit_opt.fit1D__clampDensity    , $ ;and density [3] (but not kappa or angle)
                       1]
  ATmp              = DOUBLE([1e3,100.,3.0,0.01,0])
  kappaParamStruct  = INIT_KAPPA_FITPARAM_INFO(ATmp,kappa_fixA)

  gaussParamStruct  = INIT_KAPPA_FITPARAM_INFO(TEMPORARY(ATmp),gauss_fixA)

  ;; ATmp              = DOUBLE([1e3,100.,3.0,0.01,0]) ;bulk E, T, kappa, dens, angle (useless)
  ;; fixA              = [0,0,0,0,1]
  ;; kappaParamStruct  = INIT_KAPPA_FITPARAM_INFO(ATmp,fixA)

  ;; gaussParamStruct  = INIT_KAPPA_FITPARAM_INFO(TEMPORARY(ATmp),fixA)

  IF kappaParamStruct[1].limited[1] EQ 0 THEN BEGIN
     ;; When the upperbound of T is not limited, xTol will become irreducible in
     ;; the course of the fit and status 7 will be reported. To avoid this, we
     ;; allow xTol = unminimizable to be an allowable outcome
     ;;See INIT_KAPPA_FITPARAM_INFO<f> for more informaciones

     MC__OKStatus  = [1,2,3,4,7] ;These are the acceptable outcomes of fitting with MPFIT2DFUN

  ENDIF ELSE BEGIN
     MC__OKStatus  = [1,2,3,4]
  ENDELSE

  KAPPA__CONVERT_A_AND_FIXA_TO_MPFITFUN1D_FORMAT,A,kappa_fixA
  KAPPA__CONVERT_A_AND_FIXA_TO_MPFITFUN1D_FORMAT,AGauss,gauss_fixA

  KAPPA__GET_FITS__MPFIT1D,Xorig,Yorig, $
                           orig,kappaFit1D,gaussFit1D, $
                           YORIG_ERROR=worig, $
                           KCURVEFIT_OPT=KF2D__Curvefit_opt, $
                           KFITPARAMSTRUCT=kappaParamStruct, $
                           GFITPARAMSTRUCT=gaussParamStruct, $
                           ENERGY_INDS=energy_inds, $
                           ERANGE_PEAK=eRange_peak, $
                           PEAK_IND=peak_ind, $
                           BOUNDS_I=0, $
                           KAPPA_A=A, $
                           GAUSS_A=AGauss, $
                           KAPPA_FIXA=kappa_fixA, $
                           GAUSS_FIXA=gauss_fixA, $
                           YMAX=yMax, $
                           STRINGS=KF2D__strings, $
                           ;; OUT_FITTED_PARAMS=out_kappaParams, $
                           ;; OUT_FITTED_GAUSS_PARAMS=out_gaussParams, $
                           ;; OUT_KAPPAFIT1DSTRUCTS=kappaFit1Ds, $
                           ;; OUT_GAUSSFIT1DSTRUCTS=gaussFit1Ds, $
                           ADD_FULL_FITS=fit1Denergies, $
                           ADD_ANGLESTR=angleStr, $
                           ;; OUT_ERANGE_PEAK=eRange_peakArr, $
                           OUT_PARAMSTR=out_paramStr, $
                           DONT_PRINT_FITINFO=dont_print_fitInfo, $
                           FIT_FAIL__USER_PROMPT=fit1D_fail__user_prompt, $
                           UNITS=units1D, $
                           MASS=realDataStr.mass, $
                           AVGFACTORARR=avgFactorArr, $
                           /MONTE_CARLO_MODE

  ;; Copy best-fit params

  ;; A      = Pkappa
  ;; AGauss = Pgauss

  ;; FOR kk=0,4 DO BEGIN
  ;;    kappaParamStruct[kk].value = Pkappa[kk]
  ;;    gaussParamStruct[kk].value = Pgauss[kk]
  ;; ENDFOR

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

  extraFactor = 1.4             ;In case some fits fail

  ;; Generate random inds, kappas 'twixt 1.5 and 35 for data
  IF KEYWORD_SET(bootstrap) THEN BEGIN
     randomInds = FIX(Norig * RANDOMU(seed__eIndex,Norig, $
                                      LONG(Nsim*extraFactor)))
  ENDIF
  simKappas = 1.5 + (33.5 * RANDOMU(seed__kappa,LONG(Nsim*extraFactor)))

  data_gaussError = RANDOMN(seed__data_error,realDataStr.NEnergy,realDataStr.NBins, $
                            LONG(Nsim*extraFactor))
  error_gaussEror = RANDOMN(seed__error_error,realDataStr.NEnergy,realDataStr.NBins, $
                            LONG(Nsim*extraFactor))

  ;; If not bootstrapping, set X and Yerror once and for all
  ;; IF ~KEYWORD_SET(bootstrap) THEN BEGIN
  ;;    Xorig = Xin
  ;;    Yorig_error = Yin_error
  ;; ENDIF

  ;;Units for later
  INIT_KAPPA_UNITCONV,realDataStr

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

        junk = MIN(ABS(realDataStr.theta[realDataStr.NEnergy/2,*]),angle_i)

     END
     180: BEGIN

        junk = MIN(ABS(realDataStr.theta[realDataStr.NEnergy/2,*])-180,angle_i)
        
     END
  ENDCASE

  PRINT,FORMAT='(A0,F0.2," (",I0,")")',"Min angle (ind): ", $
        realDataStr.theta[realDataStr.NEnergy/2,angle_i],angle_i

  shiftTheta     = 0              ;Not clear why shift is necessary, but makes things come out right
  ;;2017/12/27 Not sure why I thought this should be the case
  units2D        = 'eFlux'

  curDataStr     = realDataStr   ;Here's the one that gets thrown around

  keepK_iTime    = !NULL        ;Keep track of which fits get to come home with us
  keepG_iTime    = !NULL        ;Keep track of which fits get to come home with us
  successesK     = 0
  successesG     = 0
  totSuccessesK  = 0
  totSuccessesG  = 0
  fit2DKappa_inf_list  = KEYWORD_SET(make_fit2D_info) ? LIST() : !NULL
  fit2DGauss_inf_list  = KEYWORD_SET(make_fit2D_info) ? LIST() : !NULL
  FOR k=0,Nsim-1 DO BEGIN

     bounds_i = k

     ;;generate synthetic data
     ;; simulated data set
     IF KEYWORD_SET(bootstrap) THEN BEGIN
        ;; curDataStr.energy = realDataStr.energy[randomInds[*,*,k]]
        curDataStr.data = realDataStr.data[randomInds[*,*,k]] + realDataStr.ddata[randomInds[*,*,k]]*data_gaussError[*,*,k]
        Yorig_error = Yin_error[randomInds[*,*,k]]*error_gaussError[*,*,k]
     ENDIF ELSE BEGIN
        curDataStr.data = realDataStr.data + realDataStr.ddata*data_gaussError[*,*,k] > 0.
     ENDELSE

     ;;copy paramStruct
     tmpKappaParamStruct          = kappaParamStruct

     ;; Update kappa
     tmpKappaParamStruct[2].value = simKappas[k]
     
     IF ((k MOD 100) EQ 0) THEN PRINT,FORMAT='(A0,I05)',"iter_",k

     curKappaStr = curDataStr
     IF kCurvefit_opt.add_gaussian_estimate THEN BEGIN
        curGaussStr = curDataStr
     ENDIF

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
        KAPPAPARAMSTRUCT=kappaParamStruct, $
        GAUSSPARAMSTRUCT=gaussParamStruct, $
        FIT2DKAPPA_INF_LIST=fit2DKappa_inf_list, $
        FIT2DGAUSS_INF_LIST=fit2DGauss_inf_list, $
        FIT2D__SHOW_AND_PROMPT__EACH_CANDIDATE=fit2d__show_each_candidate, $
        FIT2D__SHOW_ONLY_DATA=fit2D__show_only_data, $
        FIT2D__PA_ZRANGE=fit2D__PA_zRange, $
        FIT2D__SAVE_ALL_PLOTS=fit2D__save_all_plots, $
        PRINT_2DFITINFO=print_2DFitInfo, $
        EPS=eps, $
        /MONTE_CARLO_MODE, $
        MC__OKSTATUS=MC__OKStatus

     ;; kappaFits[k] = kappaFit
     ;; IF KEYWORD_SET(add_gaussian_estimate) THEN gaussFits[k] = gaussFit

     fail = KEYWORD_SET(kCurvefit_opt.add_gaussian_estimate) ? $
            ~(hadSuccessK AND hadSuccessG)                   : $
            ~hadSuccessK
     IF fail THEN BEGIN
        PRINT,"Adding 'nother ..."
        Nsim++
     ENDIF

  ENDFOR

  IF KEYWORD_SET(saveFile) THEN BEGIN

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
