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
                                        CURDATASTR=curDataStr, $
                                        TIDFNSTR=tidFNStr, $
                                        NROLLS=nRolls, $
                                        HEMI=hemi, $
                                        FACTOR_BY_WHICH_TO_INCREASE_UNCERT_ARRAY=extraFactor, $
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
  nEnergiesG      = gaussDataStr.nEnergy

  nA              = curDataStr.nEnergy
  nB              = curDataStr.nBins

  eRange_fit    = fit2DKappa_info.extra_info.eRange_fit
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

  swapEnergyBounds = TAG_EXIST(KF2D__SDTData_opt,'energy_electron_tBounds')
  IF swapEnergyBounds THEN BEGIN

     PRINT,"Need to figure out what to do with KF2D__SDTData_opt.energy_electrons_curInd when (I presume) it comes to this routine after having already been altered by KAPPA_FIT2D__LOOP."
     PRINT,"Maybe just reinitialize????"
     PRINT,"I've already got the machinery located inside KAPPA_FIT2D__MONTECARLO__1DINIT; you just need to fickit!"
     STOP
  ENDIF

  IF KEYWORD_SET(KF2D__Curvefit_opt.fit__linear_energy_shift) THEN BEGIN

     nEnergies         = N_ELEMENTS(kappaDataStr.energy[*,0])
     ATmp              = DOUBLE([1e3,100.,3.0,0.01,0])

     fixA        = [0, $                                                ;Vary bulk E [0]
                    KF2D__Curvefit_opt.fit1D__clampTemperature, $       ;Temperature [1] (maybe)
                    0, $
                    KF2D__CurveFit_opt.fit1D__clampDensity    , $ ;and density [3] (but not kappa or angle)
                    1]

     fixAGauss   = [0, $                                                ;Vary bulk E [0]                              
                    KF2D__Curvefit_opt.fit1D__clampTemperature, $       ;Temperature [1] (maybe)                         
                    1, $                                                ;kappa       [2]
                    KF2D__CurveFit_opt.fit1D__clampDensity    , $       ;and density [3] (but not angle)
                    1] 

     kappaParamStruct  = INIT_KAPPA_FITPARAM_INFO(ATmp,fixA)
     gaussParamStruct  = INIT_KAPPA_FITPARAM_INFO(ATmp,fixAGauss)

     IF kappaParamStruct[1].limited[1] EQ 0 THEN BEGIN
        ;; When the upperbound of T is not limited, xTol will become irreducible in
        ;; the course of the fit and status 7 will be reported. To avoid this, we
        ;; allow xTol = unminimizable to be an allowable outcome
        ;;See INIT_KAPPA_FITPARAM_INFO<f> for more informaciones

        MC__OKStatus  = [1,2,3,4,7] ;These are the acceptable outcomes of fitting with MPFIT2DFUN

     ENDIF ELSE BEGIN
        MC__OKStatus  = [1,2,3,4]
     ENDELSE

     CASE 1 OF
        KEYWORD_SET(KF2D__Curvefit_opt.fit1D__nFlux): BEGIN
           units1D = 'flux'
           eSpecUnits = 'flux'
        END
        KEYWORD_SET(KF2D__Curvefit_opt.fit__JE_over_E): BEGIN
           units1D = 'je_over_E'
           eSpecUnits = 'flux'
        END
        ELSE: BEGIN
           units1D = 'eFlux'
           eSpecUnits = 'eFlux'
        END
     ENDCASE

     CASE 1 OF
        KEYWORD_SET(KF2D__Curvefit_opt.fit1D__sc_eSpec): BEGIN

           shouldBePos = WHERE(curDataStr.energy GE Pkappa[0],nShouldBePos)
           IF nShouldBePos EQ 0 THEN STOP
           IF (WHERE(~FINITE(curDataStr.data[shouldBePos]) OR $
                     (curDataStr.data[shouldBePos] LT 0)))[0] NE -1 $
           THEN STOP

           ;;Get energy spectrum, if that's what you're into
           eSpec = GET_EN_SPEC__SINGLE_STRUCT( $
                   curDataStr, $
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
        NENERGIES=nEnergies, $
        MAXEIND=maxEInd, $
        MINEIND=minEInd, $
        ENERGY_INDS=energy_inds, $
        ERANGE_FIT=eRange_fit, $
        N_BELOW_PEAK=KF2D__Curvefit_opt.n_below_peak, $
        N_ABOVE_PEAK=KF2D__Curvefit_opt.n_above_peak, $
        BULK_OFFSET=KF2D__Curvefit_opt.bulk_offset, $
        CHECK_FOR_HIGHER_FLUX_PEAKS=check_higher_peaks_set_peakEn, $
        MIN_PEAK_ENERGY=KF2D__Curvefit_opt.min_peak_energy, $
        MAX_PEAK_ENERGY=TAG_EXIST(KF2D__Curvefit_opt,'max_peak_energy') ? KF2D__Curvefit_opt.max_peak_energy : !NULL, $
        PEAK_ENERGY__START_AT_HIGHE=KF2D__Curvefit_opt.peak_energy__start_at_highE, $
        /CONTINUE_IF_NOMATCH, $
        /TEST_NOREV, $
        ONECOUNT_STR=oneCurve, $
        WHICHWY=whichWy

     kappaParamStruct.value = pKappa
     gaussParamStruct.value = pGauss

     tmpMaxEInd = (peak_ind + KF2D__Curvefit_opt.n_below_peak $
                   - KF2D__Curvefit_opt.peakE_indShift[0]*whichWy) < (nEnergies-1)
     tmpMinEInd = (peak_ind - 47 - KF2D__Curvefit_opt.peakE_indShift[1]*whichWy) > 0
     
     KF2D__Curvefit_opt.tmpEBoundsForMom = [Xorig[tmpMaxEInd], $
                                            Xorig[tmpMinEInd]]

     ;; Need to reduce these by number of fitAngles---'s'way too high!
     ;; Decided to do this in JOURNAL__20180406__BOOTSTRAP_ORB_1612_2D_DISTS_TO_GET_BESTFIT_PARAM_ERRORS
     ;; extraDensDivFac = 5.
     ;; kappaParamStruct[3].value = kappaParamStruct[3].value / fit2DKappa_info.nAngle / extraDensDivFac
     ;; gaussParamStruct[3].value = gaussParamStruct[3].value / fit2DGauss_info.nAngle / extraDensDivFac
     
     ;; If we get stopped here, we are about to fit utter garbage below the peak value.
     ;; NEITHER distribution function gives anything meaningful for E < Φ_0
     IF eRange_fit[0] LT Pkappa[0] THEN eRange_fit[0] = Pkappa[0]
     IF eRange_fit[0] LT Pgauss[0] THEN eRange_fit[0] = Pgauss[0]

     peak_energyK           = peak_energy
     peak_energyG           = peak_energy
     eRange_fitK            = eRange_fit
     eRange_fitG            = eRange_fit

  ENDIF ELSE BEGIN

     KAPPA_FIT2D__MONTECARLO__1DINIT,kappaDataStr, $
                                     IS_MAXWELLIAN=0, $
                                     ;; PEAK_IND=peak_indK, $
                                     NENERGIES=nEnergiesK, $
                                     PEAK_ENERGY=peak_energyK, $
                                     ERANGE_FIT=eRange_fitK, $
                                     OUT_1DPARAMSTRUCT=kappaParamStruct, $
                                     MC__OKStatus=MC__OKStatus
     
     IF KEYWORD_SET(add_gaussian_estimate) THEN BEGIN

        KAPPA_FIT2D__MONTECARLO__1DINIT,gaussDataStr, $
                                        /IS_MAXWELLIAN, $
                                        ;; PEAK_IND=peak_indK, $
                                        NENERGIES=nEnergiesG, $
                                        PEAK_ENERGY=peak_energyG, $
                                        ERANGE_FIT=eRange_fitG, $
                                        OUT_1DPARAMSTRUCT=gaussParamStruct, $
                                        MC__OKStatus=MC__OKStatus
     ENDIF


  ENDELSE

  ;; How many sims?
  default_Nsim = 200
  Nsim = N_ELEMENTS(nRolls) GT 0 ? nRolls : default_Nsim

  extraFactor = KEYWORD_SET(extraFactor) ? extraFactor : 3.0 ;In case some fits fail
  nMaxTry     = LONG(Nsim*extraFactor)

  ;; Generate random inds, kappas 'twixt 1.5 and 35 for data
  IF KEYWORD_SET(bootstrap) THEN BEGIN
     randomInds = FIX(Norig * RANDOMU(seed__eIndex,Norig, $
                                      nMaxTry))
  ENDIF
  simKappas = 1.5 + (33.5 * RANDOMU(seed__kappa,nMaxTry))

  dataK_gaussError = RANDOMN(seed__data_error,nA,nB, $
                            nMaxTry)
  ;; errorK_gaussError = RANDOMN(seed__error_error,nA,nB, $
                            ;; nMaxTry)
  ;; IF KEYWORD_SET(add_gaussian_estimate) THEN BEGIN
  dataG_gaussError = RANDOMN(seed__data_error,nA,nB, $
                            nMaxTry)
  ;; errorG_gaussError = RANDOMN(seed__error_error,nA,nB, $
  ;;                           nMaxTry)

  ;; If not bootstrapping, set X and Yerror once and for all
  ;; IF ~KEYWORD_SET(bootstrap) THEN BEGIN
  ;;    XorigK = Xin
  ;;    YorigK_error = Yin_error
  ;; ENDIF

  ;;Units for later
  ;; INIT_KAPPA_UNITCONV,kappaDataStr
  ;; IF KEYWORD_SET(add_gaussian_estimate) THEN BEGIN
  ;; INIT_KAPPA_UNITCONV,gaussDataStr

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

        junk = MIN(ABS(kappaDataStr.theta[nA/2,0:nB-1]),angle_i)

     END
     180: BEGIN

        junk = MIN(ABS(kappaDataStr.theta[nA/2,0:nB-1]-180),angle_i)
        
     END
  ENDCASE

  PRINT,FORMAT='(A0,F0.2," (",I0,")")',"Min angle (ind): ", $
        kappaDataStr.theta[nA/2,angle_i],angle_i

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
        PRINT,"MÅ OPPDATERES SLIK AT DET BRUKES BARE GYLDIG VERDIMENGDE VINKLER"
        STOP
        ;; curKDataStr.energy = kappaDataStr.energy[randomInds[*,*,k]]
        curKDataStr.data = kappaDataStr.data[randomInds[*,*,k]] + kappaDataStr.ddata[randomInds[*,*,k]]*dataK_gaussError[*,*,k]
        YorigK_error = Yin_error[randomInds[*,*,k]]*errorK_gaussError[*,*,k]

        curGDataStr.data = gaussDataStr.data[randomInds[*,*,k]] + gaussDataStr.ddata[randomInds[*,*,k]]*dataK_gaussError[*,*,k]
        YorigG_error = Yin_error[randomInds[*,*,k]]*errorG_gaussError[*,*,k]
     ENDIF ELSE BEGIN
        curKDataStr.data[0:nA-1,0:nB-1] = kappaDataStr.data[0:nA-1,0:nB-1] + kappaDataStr.ddata[0:nA-1,0:nB-1]*dataK_gaussError[*,*,k] > 0.

        curGDataStr.data[0:nA-1,0:nB-1] = gaussDataStr.data[0:nA-1,0:nB-1] + gaussDataStr.ddata[0:nA-1,0:nB-1]*dataG_gaussError[*,*,k] > 0.
     ENDELSE

     shouldBePos = WHERE(curKDataStr.energy[0:nA-1,0:nB-1] GE eRange_fitK[0],nShouldBePos)
     IF nShouldBePos EQ 0 THEN STOP
     IF (WHERE(~FINITE((curKDataStr.data[0:nA-1,0:nB-1])[shouldBePos]) OR $
               ((curKDataStr.data[0:nA-1,0:nB-1])[shouldBePos] LT 0)))[0] NE -1 $
     THEN PRINT,"BOGUS"
     shouldBePos = WHERE((curGDataStr.energy[0:nA-1,0:nB-1]) GE eRange_fitK[0], $
                         nShouldBePos)
     IF nShouldBePos EQ 0 THEN STOP
     IF (WHERE(~FINITE((curGDataStr.data[0:nA-1,0:nB-1])[shouldBePos]) OR $
               ((curGDataStr.data[0:nA-1,0:nB-1])[shouldBePos] LT 0)))[0] NE -1 $
     THEN PRINT,"BOGUS"

     ;;copy paramStruct
     tmpKappaParamStruct = kappaParamStruct
     tmpGaussParamStruct = gaussParamStruct

     ;; Update kappa
     tmpKappaParamStruct[2].value = simKappas[k]
     
     IF ((k MOD 100) EQ 0) THEN PRINT,FORMAT='(A0,I05)',"iter_",k

     ;; tmpCurDataStr = curDataStr
     ;; CONVERT_ESA_UNITS2,tmpCurDataStr,'FLUX'
     ;; this = ERRORPLOT(tmpCurDataStr.energy[*,42], $
     ;;                  tmpCurDataStr.data[*,42], $
     ;;                  tmpCurDataStr.ddata[*,42], $
     ;;                  /XLOG, $
     ;;                  /YLOG, $
     ;;                  YRANGE=[1e5,max(tmpCurDataStr.data[*,42])*1.2], $
     ;;                  XRANGE=MINMAX(tmpCurDataStr.energy[*,42]))

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
        ERANGE_FIT=eRange_fitK, $
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
           ERANGE_FIT=eRange_fitG, $
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

        works = N_ELEMENTS(kappaFit2DParamArr[0,*]) EQ Nsim
        
        IF works AND KEYWORD_SET(kCurvefit_opt.add_gaussian_estimate) THEN BEGIN
           works = N_ELEMENTS(gaussFit2DParamArr[0,*]) EQ Nsim

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
