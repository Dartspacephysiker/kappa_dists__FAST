;2016/08/05
; Tired of CURVEFIT. I need a real man in my life.
; Mods:
;
PRO KAPPA__GET_FITS__MPFIT1D,Xorig,Yorig, $
                             orig,kappaFit,gaussFit, $
                             YORIG_ERROR=Yorig_error, $
                             NO_ORIG=no_orig, $
                             KCURVEFIT_OPT=kCurvefit_opt, $
                             KFITPARAMSTRUCT=kappaParamStruct, $
                             GFITPARAMSTRUCT=gaussParamStruct, $
                             ENERGY_INDS=energy_inds, $
                             ERANGE_FIT=eRange_fit, $
                             ERANGE_PHI=eRange_phi, $
                             BOUNDS_I=bounds_i, $
                             KAPPA_A=A, $
                             GAUSS_A=AGauss, $
                             KAPPA_FIXA=kappa_fixA, $
                             GAUSS_FIXA=gauss_fixA, $
                             YMAX=yMax, $
                             STRINGS=strings, $
                             ;; OUT_FITTED_PARAMS=out_kappaParams, $
                             ;; OUT_FITTED_GAUSS_PARAMS=out_gaussParams, $
                             FIT__LINEAR_ENERGY_SHIFT=fit__linear_energy_shift, $
                             FIT__JE_OVER_E=fit__JE_over_E, $
                             ;; FIT__LES__TAKE_STOCK_OF_RB=fit__LES__take_stock_of_RB, $
                             ADD_FULL_FITS=add_full_fits, $
                             EXTEND_FITSTRUCT_ERANGE=extend_fitStruct_eRange, $
                             ADD_ANGLESTR=add_angleStr, $
                             ;; OUT_ERANGE_FIT=out_eRange_fit, $
                             OUT_PARAMSTR=out_paramStr, $
                             ;; DONT_PRINT_ESTIMATES=dont_print_estimates, $
                             DONT_PRINT_FITINFO=dont_print_fitinfo, $
                             FIT_FAIL__USER_PROMPT=fit_fail__user_prompt, $
                             UNITS=units, $
                             MASS=mass, $
                             AVGFACTORARR=avgFactorArr, $
                             ONLY_GAUSSIAN_ESTIMATE=only_Gaussian_estimate, $
                             MONTE_CARLO_MODE=monte_carlo_mode

  COMPILE_OPT idl2,STRICTARRSUBS

  ;; COMMON FIT_MASS,mass

  have_curveFit_opt = N_ELEMENTS(kCurvefit_opt) GT 0

  CASE 1 OF
     KEYWORD_SET(fit__linear_energy_shift): BEGIN
        IF KEYWORD_SET(fit__JE_over_E) THEN BEGIN
           kappaFunc  = 'KAPPA_FLUX__LINEAR_SHIFT_IN_ENERGY__JE_OVER_E'
           gaussFunc  = 'MAXWELL_FLUX__LINEAR_SHIFT_IN_ENERGY__JE_OVER_E'
        ENDIF ELSE BEGIN
           kappaFunc  = 'KAPPA_FLUX__LINEAR_SHIFT_IN_ENERGY'
           gaussFunc  = 'MAXWELL_FLUX__LINEAR_SHIFT_IN_ENERGY'
        ENDELSE
     END
     ELSE: BEGIN
        kappaFunc  = 'KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__CONV_TO_F__FUNC'
        gaussFunc  = 'MAXWELL_FLUX__FUNC'
     END
  ENDCASE

  OKStatus   = [1,2,3,4] ;These are all the acceptable outcomes of fitting with MPFIT2DFUN
  IF KEYWORD_SET(fit__JE_over_E) THEN BEGIN

     Yorig /= Xorig
     IF KEYWORD_SET(Yorig_error) THEN Yorig_error /= Xorig

  ENDIF ELSE BEGIN

     IF ~KEYWORD_SET(units) THEN BEGIN
        units   = 'eFlux'
     ENDIF
     ;; fa         = {units : units}
     fa         = {units : units, $
                   mass  : mass   }

  ENDELSE
  
  timeStr    = KEYWORD_SET(monte_carlo_mode) ? STRING(FORMAT='("iter_",I02)',bounds_i): $
               STR_TO_TIME(strings.yearstr+'/'+strings.plotTimes[bounds_i])

  IF ~KEYWORD_SET(no_orig) THEN BEGIN
     orig    = {x:Xorig, $
                y:Yorig, $
                name:KEYWORD_SET(monte_carlo_mode) ? timeStr : strings.plotTimes[bounds_i]}
  ENDIF
  
  IF KEYWORD_SET(Yorig_error) THEN BEGIN
     STR_ELEMENT,orig,'yError',Yorig_error,/ADD_REPLACE
  ENDIF

  contKappa  = 0

  ;;Keep the original guesses
  IF ~KEYWORD_SET(only_Gaussian_estimate) THEN Aorig = A
  add_gauss = 0
  IF have_curveFit_opt THEN IF (KEYWORD_SET(kCurvefit_opt.add_gaussian_estimate) AND ~KEYWORD_SET(monte_carlo_mode)) OR KEYWORD_SET(only_Gaussian_estimate) THEN BEGIN
     add_gauss = 1
     AGaussOrig = AGauss
  ENDIF
  
  WHILE ~contKappa DO BEGIN

     weighting            = 1   ;default linear weighting
     IF have_curveFit_opt THEN BEGIN
        weighting         = kCurvefit_opt.fit1D__weighting
     ENDIF

     IF N_ELEMENTS(Yorig_error) NE 0 THEN BEGIN

        IF have_curveFit_opt THEN IF KEYWORD_SET(kCurvefit_opt.trim_energies_below_peak) THEN BEGIN 
           Yerror         = Yorig_error[energy_inds[0]:energy_inds[1]]
        ENDIF ELSE BEGIN
           Yerror         = Yorig_error
        ENDELSE

        thesens           = WHERE(Yerror GT 0.)
        weights           = MAKE_ARRAY(N_ELEMENTS(Yerror),/FLOAT,VALUE=0.)
        CASE weighting OF
           1: BEGIN             ;linear
              weights[thesens]  = 1./ABS(Yerror[thesens])
           END
           2: BEGIN
              weights[thesens]  = 1./ABS(Yerror[thesens])^2
           END
        ENDCASE
     ENDIF ELSE BEGIN
        thesens           = WHERE(Yorig GT 0.,COMPLEMENT=bad,/NULL)
        weights           = MAKE_ARRAY(N_ELEMENTS(Yorig),/FLOAT,VALUE=1.)

        ;; CASE weighting OF
        ;;    1: BEGIN             ;linear
        ;;       weights[thesens]  = 1./ABS(Y[thesens])
        ;;    END
        ;;    2: BEGIN
        ;;       weights[thesens]  = 1./ABS(Y[thesens])^2
        ;;    END
        ;; ENDCASE

        weights[bad]      = 0.

        weights           = weights[energy_inds[0]:energy_inds[1]]

     ENDELSE

     ;;Trim energies vector if attempting to fit below peak
     IF have_curveFit_opt THEN IF KEYWORD_SET(kCurvefit_opt.trim_energies_below_peak) THEN BEGIN 
        X  = Xorig[energy_inds[0]:energy_inds[1]] 
        Y  = Yorig[energy_inds[0]:energy_inds[1]] 
     ENDIF ELSE BEGIN
        X = Xorig
        Y = Yorig
     ENDELSE

     IF have_curveFit_opt THEN IF KEYWORD_SET(kCurvefit_opt.thresh_eFlux) THEN BEGIN
        
        CASE STRUPCASE(units) OF
           'EFLUX': BEGIN
              thresh   = 1.e4
           END
           'FLUX': BEGIN
              thresh   = weights > 10.
           END
        ENDCASE

        badBoys  = WHERE((Y - thresh) LT 0, $
                         nBad, $
                         COMPLEMENT=goodBoys, $
                         NCOMPLEMENT=nGood)
        IF nBad GT 0 THEN BEGIN
           X                     = X[goodBoys]
           Y                     = Y[goodBoys]
           weights               = weights[goodBoys]

           ;; PRINT,"Dropped " + STRCOMPRESS(nBad,/REMOVE_ALL) + " bad boys from the club."
        ENDIF

     ENDIF

     IF ~KEYWORD_SET(only_Gaussian_estimate) THEN BEGIN

        UPDATE_KAPPA_FITPARAM_INFO,kappaParamStruct,A,kappa_fixA, $
                                   eRange_phi, $
                                   NO_ERANGE_FIT=monte_carlo_mode
        
        ;;Tell routine which units we like
        ;; kCurvefit_opt.fit_tol = 0.00001D
        ;;perror =       0.0000000       7.3913775       3.5255372     0.014240319       0.0000000
        ;; kCurvefit_opt.fit_tol = 0.000001D
        ;;perror =       0.0000000       7.3985375       3.4759219     0.014240447       0.0000000
        ;; kCurvefit_opt.fit_tol = 0.000001D
        ;;A      =       940.79987       258.02326       12.311067      0.53685875       0.0000000
        ;;perror =       0.0000000       7.3985375       3.4759219     0.014240447       0.0000000
        ;; kCurvefit_opt.fit_tol = 1D-7
        ;;A      =       940.79987       258.05509       12.296171      0.53685272       0.0000000
        ;;perror =       0.0000000       7.4008254       3.4602508     0.014240512       0.0000000
        ;; kCurvefit_opt.fit_tol = 1D-8
        ;;A      =       940.79987       258.06528       12.291403      0.53685078       0.0000000
        ;;perror =       0.0000000       7.4015570       3.4552592     0.014240502       0.0000000
        ;; kCurvefit_opt.fit_tol = 1D-10
        ;;A      =       940.79987       258.06982       12.289280      0.53684992       0.0000000
        ;;perror =       0.0000000       7.4018841       3.4530399     0.014240532       0.0000000
        kCurvefit_opt.fit_tol = 1D-13
        ;;A      =       940.79987       258.06982       12.289280      0.53684992       0.0000000
        ;;perror =       0.0000000       7.4018841       3.4530399     0.014240532       0.0000000
        A        = MPFITFUN(kappaFunc, $
                            X,Y, $
                            WEIGHTS=weights, $
                            FUNCTARGS=fa, $
                            BESTNORM=bestNorm, $
                            NFEV=nfev, $
                            FTOL=have_curveFit_opt ? $
                            (KEYWORD_SET(kCurvefit_opt.fit_tol) ? $
                             kCurvefit_opt.fit_tol : 1D-13) $
                            : 1D-13, $
                            GTOL=1D-13, $
                            XTOL=1D-13, $
                            STATUS=status, $
                            BEST_RESID=best_resid, $
                            PFREE_INDEX=ifree, $
                            CALC_FJAC=calc_fjac, $
                            BEST_FJAC=best_fjac, $
                            PARINFO=kappaParamStruct, $
                            QUERY=query, $
                            NPEGGED=npegged, $
                            NFREE=nfree, $
                            DOF=dof, $
                            COVAR=covar, $
                            PERROR=perror, $
                            MAXITER=have_curveFit_opt ? $
                            ( KEYWORD_SET(kCurvefit_opt.max_iter) ? $
                              kCurvefit_opt.max_iter : 150) $
                            : 150, $
                            NITER=itNum, $
                            YFIT=yFit, $
                            /QUIET, $
                            ERRMSG=errMsg, $
                            _EXTRA=extra)

        ;; IF (status LE 0) OR (status GE 5) THEN PRINT,MPFITFUN__IDENTIFY_ERROR(status)
        ;; IF status EQ -16 THEN STOP
        ;; IF status EQ 0 THEN STOP

        IF nPegged GT 0 THEN BEGIN
           temperaturePegged   = (WHERE(ifree EQ 1))[0] EQ -1
           kappaPegged         = (WHERE(ifree EQ 2))[0] EQ -1
           IF kappaPegged AND temperaturePegged THEN BEGIN
              PRINT,"PEGGED!"
              iBePegged = 1
           ENDIF ELSE BEGIN
              iBePegged  = 0
           ENDELSE
           ;; IF ABS(A[2]-kappaParamStruct[2].limits[0]) LT 0.001 THEN BEGIN
           ;;    iBePegged = 1
           ;; ENDIF ELSE BEGIN
           ;;    iBePegged = 0
           ;; ENDELSE
        ENDIF ELSE BEGIN
           iBePegged    = 0
        ENDELSE

        ;; IF ((WHERE(status EQ OKStatus))[0] NE -1) THEN BEGIN
        ;; IF ((WHERE(status EQ OKStatus))[0] NE -1) AND (nPegged EQ 0 ) THEN BEGIN
        IF ((WHERE(status EQ OKStatus))[0] NE -1) AND ~KEYWORD_SET(iBePegged) THEN BEGIN
           fitStatus = 0 
           kappaParamStruct[*].value = A
        ENDIF ELSE BEGIN
           fitStatus = 1
        ENDELSE

        IF KEYWORD_SET(fitStatus) THEN BEGIN
           IF ~KEYWORD_SET(dont_print_fitInfo) THEN PRINT,'kappaFit failure ...'
           chi2            = -1
           pVal            = -1
        ENDIF ELSE BEGIN

           chi2           = TOTAL( (Y-yFit)^2 * ABS(weights) * ( (weighting EQ 1) ? ABS(weights) : 1.D) )

           IF FINITE(chi2) THEN BEGIN
              pVal                        = 1 - CHISQR_PDF(chi2,N_ELEMENTS(X)-N_ELEMENTS(WHERE(~kappa_fixA))) ;Subtract number of free params
              checkMath = CHECK_MATH()
              IF (checkMath AND 16) OR (checkMath AND 32) THEN STOP
           ENDIF ELSE BEGIN
              pVal                        = -1
           ENDELSE

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           ;;Calculate chi and things if it checks out

           ;;need to adjust Y bounds?
           ;; yMax                        = MAX(yFit) 

           IF ~KEYWORD_SET(dont_print_fitInfo) THEN BEGIN
              PRINT,"Fitted spectral properties: "
              PRINT_KAPPA_FLUX_FIT_PARAMS__MPFITFUN,A
              PRINT,''
           ENDIF
        ENDELSE

        IF KEYWORD_SET(fit_fail__user_prompt) AND fitStatus GT 0 THEN BEGIN
           
           tmpA             = [A[0],A[1],A[2],A[3],0,0,A[4]]
           tmpfixA          = ~[kappa_fixA[0],kappa_fixA[1],kappa_fixA[2],kappa_fixA[3],1,1,kappa_fixA[4]]
           KAPPA__FIT_FAIL_USER_PROMPT,tmpA,tmpfixA,energy_inds, $
                                       Xorig,X,Yorig,Y, $
                                       STRINGS=strings, $
                                       BOUNDS_I=bounds_i

        ENDIF ELSE BEGIN
           contKappa                = 1
        ENDELSE

     ENDIF ELSE BEGIN
        contKappa = 1
     ENDELSE

  ENDWHILE

  IF ~KEYWORD_SET(monte_carlo_mode) THEN BEGIN
     out_paramStr           = STRING(FORMAT='(A0,"--",A0,A0,"--orb_",A0,"__",A0,"--",I0,"-",I0,".txt")', $
                                     strings.timeFNStrs[bounds_i], $
                                     strings.eeb_or_ees, $
                                     strings.avgStr, $
                                     strings.orbStr, $
                                     strings.orbDate)
  ENDIF

  IF ~KEYWORD_SET(only_Gaussian_estimate) AND ~KEYWORD_SET(monte_carlo_mode) THEN BEGIN

     kappaFit = {x           : X, $
                 y           : yFit, $
                 ;; NAME     : "Kappa distribution", $
                 NAME        : STRING(FORMAT='(A0,F0.2)',"$\kappa$ = ",A[2]), $
                 A           : A, $
                 A_initGuess : TEMPORARY(Aorig), $
                 time        : timeStr, $
                 time_index  : bounds_i, $
                 fitStatus   : TEMPORARY(fitStatus), $
                 chi2        : TEMPORARY(chi2), $
                 pVal        : TEMPORARY(pVal)}

     IF KEYWORD_SET(add_full_fits) THEN BEGIN
        ;; xFull = add_full_fits
        ;; xFull = Xorig
        ;; IF KEYWORD_SET(extend_fitStruct_eRange) THEN BEGIN
        ;;    xFull = [extend_fitStruct_eRange,xFull]
        ;; ENDIF
        ;; IF KEYWORD_SET(extend_fitStruct_eRange) THEN BEGIN
        ;;    energyStep = Xorig[0]/Xorig[1]
        ;;    xFull = [REVERSE(Xorig[0]*energyStep^(INDGEN(3)+1)), $
        ;;             Xorig]
        ;; ENDIF ELSE BEGIN
        ;;    xFull = Xorig
        ;; ENDELSE

        yFull = CALL_FUNCTION(kappaFunc, $
                              add_full_fits, $
                              A, $
                              UNITS=units, $
                              MASS=mass)

        ;; yFull = KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__CONV_TO_F__FUNC(add_full_fits,A, $
        ;;                                                                UNITS=units, $
        ;;                                                                MASS=mass)

        kappaFit                 = CREATE_STRUCT(kappaFit,"xFull",add_full_fits,"yFull",yFull)
     ENDIF

     IF KEYWORD_SET(add_angleStr) THEN BEGIN
        ADD_STR_ELEMENT,kappaFit,'bulkAngleInf',add_angleStr
     ENDIF

     ;; multiply density by number of angles over which spectrum is averaged
     IF have_curveFit_opt THEN IF KEYWORD_SET(kCurvefit_opt.fit1D__sc_eSpec) THEN BEGIN
        A[3] *= avgFactorArr[bounds_i]
        kappaFit.A[3] *= avgFactorArr[bounds_i]
     ENDIF

     ;; ENDIF

     add_orig = ~KEYWORD_SET(monte_carlo_mode)
     IF KEYWORD_SET(add_orig) THEN BEGIN
        IF have_curveFit_opt THEN IF KEYWORD_SET(kCurvefit_opt.trim_energies_below_peak) THEN $
           orig        = CREATE_STRUCT(orig,'energy_inds',energy_inds)
        kappaFit       = CREATE_STRUCT(kappaFit,"orig",orig)
     ENDIF

  ENDIF

  IF KEYWORD_SET(add_gauss) THEN BEGIN

     UPDATE_KAPPA_FITPARAM_INFO,gaussParamStruct,AGauss,gauss_fixA,eRange_phi, $
                                NO_ERANGE_FIT=monte_carlo_mode

     AGauss        = MPFITFUN(gaussFunc, $
                              X,Y, $
                              WEIGHTS=weights, $
                              FUNCTARGS=fa, $
                              BESTNORM=bestNorm, $
                              NFEV=nfev, $
                              FTOL=have_curveFit_opt ? $
                              (KEYWORD_SET(kCurvefit_opt.fit_tol) ? $
                               kCurvefit_opt.fit_tol : 1e-3) $
                              : 1e-3, $
                              GTOL=1e-13, $
                              STATUS=gaussStatus, $
                              BEST_RESID=best_resid, $
                              PFREE_INDEX=iGaussFree, $
                              CALC_FJAC=calc_fjac, $
                              BEST_FJAC=best_fjac, $
                              PARINFO=gaussParamStruct, $
                              QUERY=query, $
                              NPEGGED=nGaussPegged, $
                              NFREE=nGaussFree, $
                              DOF=gaussDOF, $
                              COVAR=covar, $
                              PERROR=perror, $
                              MAXITER=have_curveFit_opt ? $
                              (KEYWORD_SET(kCurvefit_opt.max_iter) ? $
                               kCurvefit_opt.max_iter : 150) $
                              : 150, $
                              NITER=itNum, $
                              YFIT=yGaussFit, $
                              /QUIET, $
                              ERRMSG=errMsg, $
                              _EXTRA=extra)

     ;;Little update
     IF ~KEYWORD_SET(dont_print_fitInfo) THEN BEGIN
        IF (gaussStatus LE 0) OR (gaussStatus GE 5) THEN PRINT,MPFITFUN__IDENTIFY_ERROR(gaussStatus)
        IF gaussStatus EQ 0 THEN STOP
     ENDIF

     ;; IF nGaussPegged GT 0 THEN PRINT,"GAUSSPEGGED!"
     IF nGaussFree LT (3-KEYWORD_SET(fit__JE_over_E)) THEN PRINT,"GAUSSPEGGED!"

     ;; IF nPegged GT 0 THEN BEGIN
     ;;    temperaturePegged   = (WHERE(ifree EQ 1))[0] EQ -1
     ;;    kappaPegged         = (WHERE(ifree EQ 2))[0] EQ -1
     ;;    IF kappaPegged AND temperaturePegged THEN BEGIN
     ;;       PRINT,"PEGGED!"
     ;;       iBePegged = 1
     ;;    ENDIF ELSE BEGIN
     ;;       iBePegged  = 0
     ;;    ENDELSE
     ;;    ;; IF ABS(A[2]-kappaParamStruct[2].limits[0]) LT 0.001 THEN BEGIN
     ;;    ;;    iBePegged = 1
     ;;    ;; ENDIF ELSE BEGIN
     ;;    ;;    iBePegged = 0
     ;;    ;; ENDELSE
     ;; ENDIF ELSE BEGIN
     ;;    iBePegged    = 0
     ;; ENDELSE

     ;; IF (WHERE(gaussStatus EQ OKStatus))[0] NE -1 THEN BEGIN
     ;; IF ((WHERE(status EQ OKStatus))[0] NE -1) AND (nGaussPegged EQ 0 ) THEN BEGIN
     IF ((WHERE(gaussStatus EQ OKStatus))[0] NE -1) OR (nGaussFree LT 3) THEN BEGIN
        gaussFitStatus = 0 
        gaussParamStruct[*].value = AGauss
     ENDIF ELSE BEGIN
        gaussFitStatus = 1
     ENDELSE

     IF KEYWORD_SET(gaussFitStatus) THEN BEGIN
        IF ~KEYWORD_SET(dont_print_fitInfo) THEN PRINT,'gaussFit failure ...'
        chi2            = -1
        pValGauss       = -1
     ENDIF ELSE BEGIN

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;Calculate chi and things if it checks out

        ;;need to adjust Y bounds?
        ;; yMax               = MAX(yGaussFit) > yMax


        chi2           = TOTAL( (Y-yGaussFit)^2 * ABS(weights) * ( (weighting EQ 1) ? ABS(weights) : 1.D) )

        IF FINITE(chi2) THEN BEGIN
           pValGauss       = 1 - CHISQR_PDF(chi2,N_ELEMENTS(X)-3) ;3 for the 3 params that were allowed to participate in this fit
        ENDIF ELSE BEGIN
           pValGauss       = -1
        ENDELSE

        IF ~KEYWORD_SET(dont_print_fitInfo) THEN BEGIN
           PRINT,"Gaussian fitted spectral properties: "
           PRINT_KAPPA_FLUX_FIT_PARAMS__MPFITFUN,AGauss
        ENDIF
     ENDELSE

     IF ~KEYWORD_SET(monte_carlo_mode) THEN BEGIN
        gaussFit = {x           : KEYWORD_SET(use_SDT_Gaussian_fit) ? X_SDT : X, $
                    y           : yGaussFit, $
                    name        : "Maxwellian" + $
                    (KEYWORD_SET(use_SDT_Gaussian_fit) ? "_SDT" : ''), $
                    A           : AGauss, $
                    A_initGuess : TEMPORARY(AGaussOrig), $
                    time        : timeStr, $
                    time_index  : bounds_i, $
                    fitStatus   : TEMPORARY(gaussFitStatus), $
                    chi2        : TEMPORARY(chi2), $
                    pVal        : TEMPORARY(pValGauss), $
                    is_sdt_fit  : KEYWORD_SET(use_SDT_Gaussian_fit)}

        IF KEYWORD_SET(add_full_fits) THEN BEGIN
           CASE 1 OF
              KEYWORD_SET(use_SDT_Gaussian_fit): BEGIN
                 MAXWELLIAN_1,Xorig,AGauss_SDT,yGaussFull
              END
              ELSE: BEGIN
                 ;; yGaussFull = KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__CONV_TO_F__FUNC(Xorig,AGauss, $
                 ;;                                                                     UNITS=units, $
                 ;;                                                                     MASS=mass)
                 ;; yGaussFull = MAXWELL_FLUX__FUNC(add_full_fits,AGauss, $
                 ;;                                 UNITS=units, $
                 ;;                                 MASS=mass)
                 yGaussFull = CALL_FUNCTION(gaussFunc,add_full_fits,AGauss, $
                                            UNITS=units, $
                                            MASS=mass)
              END
           ENDCASE
           gaussFit = CREATE_STRUCT(gaussFit,"xFull", $
                                    add_full_fits, $
                                    "yFull",yGaussFull)

        ENDIF

        IF have_curveFit_opt THEN IF KEYWORD_SET(kCurvefit_opt.fit1D__sc_eSpec) THEN BEGIN
           AGauss[3] *= avgFactorArr[bounds_i]
           gaussFit.A[3] *= avgFactorArr[bounds_i]
        ENDIF

        IF KEYWORD_SET(add_angleStr) THEN BEGIN
           ADD_STR_ELEMENT,gaussFit,'bulkAngleInf',add_angleStr
        ENDIF

     ENDIF

     out_gaussParams  = N_ELEMENTS(out_gaussParams) GT 0 ? $
                        [[out_gaussParams],[AGauss]] : $
                        AGauss
     ;; ENDIF

  ENDIF

END
