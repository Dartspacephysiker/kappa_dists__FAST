;2016/08/05
; Tired of CURVEFIT. I need a real man in my life.
; Mods:
;
PRO KAPPA__GET_FITS__MPFIT1D,Xorig,Yorig, $
                             orig,kappaFit,gaussFit, $
                             KCURVEFIT_OPT=kCurvefit_opt, $
                             ENERGY_INDS=energy_inds, $
                             ERANGE_PEAK=eRange_peak, $
                             PEAK_IND=peak_ind, $
                             BOUNDS_I=bounds_i, $
                             KAPPA_A=A, $
                             GAUSS_A=AGauss, $
                             KAPPA_FIXA=kappa_fixA, $
                             GAUSS_FIXA=gauss_fixA, $
                             YMAX=yMax, $
                             STRINGS=strings, $
                             OUT_FITTED_PARAMS=out_kappaParams, $
                             OUT_FITTED_GAUSS_PARAMS=out_gaussParams, $
                             OUT_KAPPA_FIT_STRUCTS=kappaFits, $
                             OUT_GAUSS_FIT_STRUCTS=gaussFits, $
                             ADD_FULL_FITS=add_full_fits, $
                             ADD_ANGLESTR=add_angleStr, $
                             OUT_ERANGE_PEAK=out_eRange_peak, $
                             OUT_PARAMSTR=out_paramStr, $
                             ;; DONT_PRINT_ESTIMATES=dont_print_estimates, $
                             DONT_PRINT_FITINFO=dont_print_fitinfo, $
                             FIT_FAIL__USER_PROMPT=fit_fail__user_prompt

  COMMON FIT_MASS,mass

  COMPILE_OPT idl2

  OKStatus                    = [1,2,3,4] ;These are all the acceptable outcomes of fitting with MPFIT2DFUN

  IF N_ELEMENTS(kappa_fixA) EQ 0 THEN BEGIN
     kappa_fixA               = [0,0,0,0,1] ;Vary bulk E [0], Temperature [1], kappa [2], and density [3] (but not angle)
  ENDIF
  
  IF N_ELEMENTS(gauss_fixA) EQ 0 THEN BEGIN
     gauss_fixA               = [0,0,1,0,1] ;Vary bulk E [0], Temperature [1], and density [3] (but not kappa or angle)
  ENDIF

  orig                        = {x:Xorig, $
                                 y:Yorig, $
                                 name:strings.plotTimes[bounds_i]}

  contKappa             = 0

  WHILE ~contKappa DO BEGIN

     ;;Trim energies vector if attempting to fit below peak
     IF KEYWORD_SET(kCurvefit_opt.trim_energies_below_peak) THEN BEGIN 
        X                        = Xorig[energy_inds[0]:energy_inds[1]] 
        Y                        = Yorig[energy_inds[0]:energy_inds[1]] 
     ENDIF

     IF KEYWORD_SET(kCurvefit_opt.thresh_eFlux) THEN BEGIN
        
        thresh                   = 1e4
        badBoys                  = WHERE(Y LE thresh, $
                                         nBad, $
                                         COMPLEMENT=goodBoys, $
                                         NCOMPLEMENT=nGood)
        IF nBad GT 0 THEN BEGIN
           X                     = X[goodBoys]
           Y                     = Y[goodBoys]
           ;; PRINT,"Dropped " + STRCOMPRESS(nBad,/REMOVE_ALL) + " bad boys from the club."
        ENDIF
     ENDIF

     ;; weights                     = 1./ABS(Y)^2
     ;; fixMe                       = WHERE(~FINITE(weights),nFixMe)
     ;; IF nFixMe GT 0 THEN BEGIN
     ;;    weights[fixMe]           = 0.0
     ;; ENDIF

     ;;Alternate
     weights                     = MAKE_ARRAY(N_ELEMENTS(Y),/FLOAT,VALUE=0.)
     weights[WHERE(Y GT 0.)]     = 1./ABS(Y[WHERE(Y GT 0.)])^2

     kappaParams = INIT_KAPPA_FITPARAM_INFO(A,kappa_fixA, $
                                            ERANGE_PEAK=eRange_peak)

     A        = MPFITFUN('KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__CONV_TO_F__FUNC', $
                         X,Y, $
                         WEIGHTS=weights, $
                         FUNCTARGS=fa, $
                         BESTNORM=bestNorm, $
                         NFEV=nfev, $
                         FTOL=KEYWORD_SET(kCurvefit_opt.fit_tol) ? $
                         kCurvefit_opt.fit_tol : 1e-3, $
                         GTOL=1e-13, $
                         STATUS=status, $
                         BEST_RESID=best_resid, $
                         PFREE_INDEX=ifree, $
                         CALC_FJAC=calc_fjac, $
                         BEST_FJAC=best_fjac, $
                         PARINFO=kappaParams, $
                         QUERY=query, $
                         NPEGGED=npegged, NFREE=nfree, DOF=dof, $
                         COVAR=covar, $
                         PERROR=perror, $
                         MAXITER=KEYWORD_SET(kCurvefit_opt.max_iter) ? $
                         kCurvefit_opt.max_iter : 150, $
                         NITER=itNum, $
                         YFIT=yFit, $
                         /QUIET, $
                         ERRMSG=errMsg, $
                         _EXTRA=extra)

     IF (status LE 0) OR (status GE 5) THEN PRINT,MPFITFUN__IDENTIFY_ERROR(status)
     ;; IF status EQ -16 THEN STOP
     IF status EQ 0 THEN STOP

     IF (WHERE(status EQ OKStatus))[0] NE -1 THEN BEGIN
        fitStatus = 0 
     ENDIF ELSE BEGIN
        fitStatus = 1
     ENDELSE

     IF KEYWORD_SET(fitStatus) THEN BEGIN
        IF ~KEYWORD_SET(dont_print_fitInfo) THEN PRINT,'kappaFit failure ...'
        chi2            = -1
        pValGauss       = -1
     ENDIF ELSE BEGIN

        chi2           = TOTAL( (Y-yFit)^2 * ABS(weights) )

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
        yMax                        = MAX(yFit) 
        IF ~KEYWORD_SET(dont_print_fitInfo) THEN BEGIN
           PRINT,"Fitted spectral properties: "
           PRINT_KAPPA_FLUX_FIT_PARAMS__MPFITFUN,A
           PRINT,''
        ENDIF
     ENDELSE

     IF KEYWORD_SET(fit_fail__user_prompt) AND fitStatus GT 0 THEN BEGIN
        
        KAPPA__FIT_FAIL_USER_PROMPT,A,kappa_fixA,energy_inds

     ENDIF ELSE BEGIN
        contKappa                = 1
     ENDELSE

  ENDWHILE

     out_kappaParams           = N_ELEMENTS(out_kappaParams) GT 0 ? $
                                 [[out_kappaParams],[A]] : A
     out_eRange_peak             = N_ELEMENTS(out_eRange_peak) GT 0 ? $
                                   [[out_eRange_peak],[eRange_peak]] : eRange_peak
     out_paramStr                = STRING(FORMAT='(A0,"--",A0,A0,"--orb_",A0,"__",A0,"--",I0,"-",I0,".txt")', $
                                          strings.timeFNStrs[bounds_i], $
                                          strings.eeb_or_ees, $
                                          strings.avgStr, $
                                          strings.orbStr, $
                                          strings.orbDate)


     kappaFit                    = {x:X, $
                                    y:yFit, $
                                    NAME:"Fitted spectrum", $
                                    A:A, $
                                    time:STR_TO_TIME(strings.yearstr+'/'+strings.plotTimes[bounds_i]), $
                                    time_index:bounds_i, $
                                    fitStatus:fitStatus, $
                                    chi2:chi2, $
                                    pVal:pVal}

     IF KEYWORD_SET(add_full_fits) THEN BEGIN
        yFull = KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__CONV_TO_F__FUNC(Xorig,A)
        kappaFit                 = CREATE_STRUCT(kappaFit,"xFull",Xorig,"yFull",yFull)
     ENDIF

     IF KEYWORD_SET(add_angleStr) THEN BEGIN
        ADD_STR_ELEMENT,kappaFit,'bulkAngleInf',add_angleStr
     ENDIF


     IF N_ELEMENTS(kappaFits) EQ 0 THEN BEGIN
        kappaFits    = LIST(kappaFit)
     ENDIF ELSE BEGIN
        kappaFits.Add,kappaFit
     ENDELSE

  ;; ENDIF

  IF KEYWORD_SET(kCurvefit_opt.add_gaussian_estimate) THEN BEGIN
     ;; weights                  = 1./ABS(Y)^2
     ;; fixMe                       = WHERE(~FINITE(weights),nFixMe)
     ;; IF nFixMe GT 0 THEN BEGIN
     ;;    weights[fixMe]           = 0.0
     ;; ENDIF

     ;;Alternate
     weights                     = MAKE_ARRAY(N_ELEMENTS(Y),/FLOAT,VALUE=0.)
     weights[WHERE(Y GT 0.)]     = 1./ABS(Y[WHERE(Y GT 0.)])^2

     gaussParams = INIT_KAPPA_FITPARAM_INFO(AGauss,gauss_fixA, $
                                            ERANGE_PEAK=eRange_peak)

     AGauss        = MPFITFUN('KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__CONV_TO_F__FUNC', $
                              X,Y, $
                              WEIGHTS=weights, $
                              FUNCTARGS=fa, $
                              BESTNORM=bestNorm, $
                              NFEV=nfev, $
                              FTOL=KEYWORD_SET(kCurvefit_opt.fit_tol) ? $
                              kCurvefit_opt.fit_tol : 1e-3, $
                              GTOL=1e-13, $
                              STATUS=gaussStatus, $
                              BEST_RESID=best_resid, $
                              PFREE_INDEX=ifree, $
                              CALC_FJAC=calc_fjac, $
                              BEST_FJAC=best_fjac, $
                              PARINFO=gaussParams, $
                              QUERY=query, $
                              NPEGGED=npegged, NFREE=nfree, DOF=dof, $
                              COVAR=covar, $
                              PERROR=perror, $
                              MAXITER=KEYWORD_SET(kCurvefit_opt.max_iter) ? $
                              kCurvefit_opt.max_iter : 150, $
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

     IF (WHERE(gaussStatus EQ OKStatus))[0] NE -1 THEN BEGIN
        gaussFitStatus = 0 
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
        yMax               = MAX(yGaussFit) > yMax

        chi2           = TOTAL( (Y-yGaussFit)^2 * ABS(weights) )

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


     gaussFit              = {x:KEYWORD_SET(use_SDT_Gaussian_fit) ? X_SDT : X, $
                              y:yGaussFit, $
                              name:"Gaussian Fitted spectrum" + (KEYWORD_SET(use_SDT_Gaussian_fit) ? "_SDT" : ''), $
                              A:AGauss, $
                              time:STR_TO_TIME(strings.yearstr+'/'+strings.plotTimes[bounds_i]), $
                              time_index:bounds_i, $
                              fitStatus:gaussFitStatus, $
                              chi2:chi2, $
                              pVal:pValGauss, $
                              is_sdt_fit:KEYWORD_SET(use_SDT_Gaussian_fit)}

        IF KEYWORD_SET(add_full_fits) THEN BEGIN
           CASE 1 OF
              KEYWORD_SET(use_SDT_Gaussian_fit): BEGIN
                 MAXWELLIAN_1,Xorig,AGauss_SDT,yGaussFull
              END
              ELSE: BEGIN
                 yGaussFull = KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__CONV_TO_F__FUNC(Xorig,AGauss)
              END
           ENDCASE
           gaussFit                 = CREATE_STRUCT(gaussFit,"xFull",Xorig,"yFull",yGaussFull)

        ENDIF

        IF KEYWORD_SET(add_angleStr) THEN BEGIN
           ADD_STR_ELEMENT,gaussFit,'bulkAngleInf',add_angleStr
        ENDIF

        IF N_ELEMENTS(gaussFits) EQ 0 THEN BEGIN
           gaussFits    = LIST(gaussFit)
        ENDIF ELSE BEGIN
           gaussFits.Add,gaussFit
        ENDELSE

        out_gaussParams  = N_ELEMENTS(out_gaussParams) GT 0 ? $
                           [[out_gaussParams],[AGauss]] : $
                           AGauss
     ;; ENDIF

  ENDIF

END
