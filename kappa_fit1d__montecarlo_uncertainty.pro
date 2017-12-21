;2017/12/21
;; Takes an "orig" struct (called "data" in this routine) from a kappaFits list generated as output from KAPPA_EFLUX_FIT2D.
;;
;; data has the following members
;; x: energy bins (eV)
;; y: differential number flux (#/cm^2-s-sr-eV)
;; yerror: uncertainty derived from counting statistics, then converted to differential number flux (#/cm^2-s-sr-eV)
;; energy_inds: The indices of the high- and low-energy bins, in that order, that were used to do the initial fit
;; Foundations laid in JOURNAL__20171221__BOOTSTRAP_ORB_1773_DISTS_TO_GET_BESTFIT_PARAM_ERRORS
PRO KAPPA_FIT1D__MONTECARLO_UNCERTAINTY,data,Pkappa,Pgauss,Pobs, $
                                        NROLLS=nRolls, $
                                        NOT_MPFIT1D=not_mpFit1D, $
                                        KCURVEFIT_OPT=kCurvefit_opt, $
                                        OBSERVED_DIST=observed_dist, $
                                        BOOTSTRAP_MODE=bootstrap, $
                                        ADD_GAUSSIAN_ESTIMATE=add_gaussian_estimate, $
                                        MASS=mass

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; Get the data we want
  ;; energy_inds = data.energy_inds
  IF KEYWORD_SET(observed_dist) THEN BEGIN
     Xin       = data.x[data.energy_inds[0]:data.energy_inds[1]]
     Yin       = data.y[data.energy_inds[0]:data.energy_inds[1]]
     Yin_error = data.yerror[data.energy_inds[0]:data.energy_inds[1]]
     ;; energy_inds = 
  ENDIF ELSE BEGIN
     Xin       = data.x
     Yin       = data.y
     Yin_error = data.yerror
  ENDELSE
  Norig     = N_ELEMENTS(Xin)

  ;; Copy best-fit params
  A      = Pkappa
  AGauss = Pgauss

  A[1]   = Pobs[1]
  A[3]   = Pobs[3]

  IF N_ELEMENTS(kCurvefit_opt) GT 0 THEN BEGIN

     kCurvefit_opt.trim_energies_below_peak = 0 ; Make sure this is turned off
     kCurvefit_opt.fit1D__sc_eSpec = 0 ;and this

     kCurvefit_opt.add_gaussian_estimate = KEYWORD_SET(add_gaussian_estimate)
     
     CASE 1 OF
        KEYWORD_SET(kCurvefit_opt.fit1D__nFlux): BEGIN
           units = 'flux'
        END
        ELSE: BEGIN
           units = 'eFlux'
        END
     ENDCASE
  ENDIF

  ;; How many sims?
  default_Nsim = 10000
  Nsim = N_ELEMENTS(nRolls) GT 0 ? nRolls : default_Nsim

  ;; Generate random inds, kappas 'twixt 1.5 and 35 for data
  randomInds = FIX(Norig * RANDOMU(seed__eIndex,Norig,Nsim))
  simKappas = 1.5 + (33.5 * RANDOMU(seed__kappa,Nsim))
  data_gaussError = RANDOMN(seed__data_error,Norig,Nsim) ;RANDOMN gives rGaussian num, mean=0 & stdev=1
  error_gaussEror = RANDOMN(seed__error_error,Norig,Nsim)

  ;; If not bootstrapping, set X and Yerror once and for all
  IF ~KEYWORD_SET(bootstrap) THEN BEGIN
     Xorig = Xin
     Yorig_error = Yin_error
  ENDIF

  ;; Init fitParam structs for MPFIT1D stuff
  ATmp              = DOUBLE([1e3,100.,3.0,0.01,0]) ;bulk E, T, kappa, dens, angle (useless)
  fixA              = [0,0,0,0,1]
  kappaParamStruct  = INIT_KAPPA_FITPARAM_INFO(ATmp,fixA)

  gaussParamStruct  = INIT_KAPPA_FITPARAM_INFO(TEMPORARY(ATmp),fixA)

  FOR k=0,Nsim-1 DO BEGIN
     
     bounds_i = k

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

     PRINT,FORMAT='(A0,I05)',"iter_",k

     KAPPA__GET_FITS__MPFIT1D,Xorig,Yorig, $
                              orig,kappaFit,gaussFit, $
                              YORIG_ERROR=Yorig_error, $
                              NO_ORIG=no_orig, $ ;don't make orig struct
                              KCURVEFIT_OPT=kCurvefit_opt, $
                              KFITPARAMSTRUCT=kappaParamStruct, $
                              GFITPARAMSTRUCT=gaussParamStruct, $
                              ENERGY_INDS=energy_inds, $
                              ERANGE_PEAK=eRange_peak, $
                              ;; PEAK_IND=peak_ind, $
                              BOUNDS_I=bounds_i, $
                              KAPPA_A=A, $
                              GAUSS_A=AGauss, $
                              KAPPA_FIXA=fixA, $
                              GAUSS_FIXA=fixA, $
                              YMAX=yMax, $
                              STRINGS=strings, $
                              OUT_FITTED_PARAMS=out_kappaParams, $
                              OUT_FITTED_GAUSS_PARAMS=out_gaussParams, $
                              ;; OUT_KAPPA_FIT_STRUCTS=kappaFits, $
                              ;; OUT_GAUSS_FIT_STRUCTS=gaussFits, $
                              ADD_FULL_FITS=add_full_fits, $
                              EXTEND_FITSTRUCT_ERANGE=extend_fitStruct_eRange, $
                              ADD_ANGLESTR=add_angleStr, $
                              OUT_ERANGE_PEAK=out_eRange_peak, $
                              OUT_PARAMSTR=out_paramStr, $
                              ;; DONT_PRINT_ESTIMATES=dont_print_estimates, $
                              ;; DONT_PRINT_FITINFO=dont_print_fitinfo, $
                              /DONT_PRINT_FITINFO, $
                              FIT_FAIL__USER_PROMPT=fit_fail__user_prompt, $
                              UNITS=units, $
                              MASS=mass, $
                              AVGFACTORARR=avgFactorArr, $
                              /MONTE_CARLO_MODE
     
     IF k EQ 0 THEN BEGIN
        kappaFits = REPLICATE(kappaFit,Nsim)
        gaussFits = REPLICATE(gaussFit,Nsim)
     ENDIF

     kappaFits[k] = kappaFit
     gaussFits[k] = gaussFit

  ENDFOR

  STOP

END
