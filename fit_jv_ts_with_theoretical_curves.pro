;2017/03/22
PRO FIT_JV_TS_WITH_THEORETICAL_CURVES,pot,cur, $
                                      potErr,curErr, $
                                      T,N, $
                                      ;; USEINDS=useInds, $
                                      FIT_JE=fit_je, $
                                      FLIP_CURRENT_SIGN=flip_current_sign, $
                                      KAPPA_FIXA=kappa_fixA, $
                                      GAUSS_FIXA=gauss_fixA, $
                                      KAPPA_A=kappa_A, $
                                      GAUSS_A=Gauss_A, $
                                      FIT_KAPPAS=kappas, $
                                      KAPPA_FIT_TEMPERATURE=kappa_fitT, $
                                      GAUSS_FIT_TEMPERATURE=gauss_fitT, $
                                      KAPPA_FIT_DENSITY=kappa_fitN, $
                                      GAUSS_FIT_DENSITY=gauss_fitN, $
                                      MAXITER=maxIter, $
                                      FTOL=fTol, $
                                      GTOL=gTol, $
                                      OUT_FITKAPPA_A=fitKappa_A, $
                                      OUT_FITGAUSS_A=fitGauss_A, $
                                      OUT_FITKAPPA_CUR=fitKappa_cur, $
                                      OUT_FITGAUSS_CUR=fitGauss_cur, $
                                      KAPPALIMS=kappaLims, $   
                                      TEMPLIMS=TempLims, $    
                                      DENSLIMS=DensLims, $    
                                      MAGRATIOLIMS=magRatioLims
  
  COMPILE_OPT IDL2,STRICTARRSUBS

  jvFitFunc   = KEYWORD_SET(fit_je) ? 'JE_V_CURVE_FIT__MAXWELL_KAPPA' : 'JV_CURVE_FIT__MAXWELL_KAPPA'
  OKStatus    = [1,2,3,4]       ;These are all the acceptable outcomes of fitting with MPFIT2DFUN

  maxIter     = KEYWORD_SET(maxIter ) ? maxIter : 5000
  fTol        = KEYWORD_SET(fTol    ) ? fTol    : 1D-15
  gTol        = KEYWORD_SET(gTol    ) ? gTol    : 1D-15

  IF N_ELEMENTS(kappa_A) EQ 0 THEN BEGIN
     ;;        kappa, Temp,Dens, R_B
     kappa_A = [  10,  300, 0.1, 9D2]
  ENDIF

  IF N_ELEMENTS(Gauss_A) EQ 0 THEN BEGIN
     Gauss_A = kappa_A
  ENDIF
  
  kappaParamStruct = INIT_JV_FITPARAM_INFO(kappa_A,kappa_fixA, $
                                           KAPPALIMS=kappaLims, $   
                                           TEMPLIMS=TempLims, $    
                                           DENSLIMS=DensLims, $    
                                           MAGRATIOLIMS=magRatioLims)
  gaussParamStruct = INIT_JV_FITPARAM_INFO(Gauss_A,gauss_fixA, $
                                           KAPPALIMS=kappaLims, $   
                                           TEMPLIMS=TempLims, $    
                                           DENSLIMS=DensLims, $    
                                           MAGRATIOLIMS=magRatioLims)

  fa_kappa    = {no_mult_by_charge : 1B, $
                 is_Maxwellian_fit : 0B}

  fa_Gauss    = {no_mult_by_charge : 1B, $
                 is_Maxwellian_fit : 1B}

  ;;First both
  IF KEYWORD_SET(T) THEN BEGIN
     STR_ELEMENT,fa_kappa,'in_temperatures',T,/ADD_REPLACE
     STR_ELEMENT,fa_Gauss,'in_temperatures',T,/ADD_REPLACE
  ENDIF

  IF KEYWORD_SET(N) THEN BEGIN
     STR_ELEMENT,fa_kappa,'in_densities',N,/ADD_REPLACE
     STR_ELEMENT,fa_Gauss,'in_densities',N,/ADD_REPLACE
  ENDIF

  ;;Now just kappas
  IF KEYWORD_SET(kappa_fitT) THEN BEGIN
     STR_ELEMENT,fa_kappa,'in_temperatures',kappa_fitT,/ADD_REPLACE
  ENDIF

  IF KEYWORD_SET(kappa_fitN) THEN BEGIN
     STR_ELEMENT,fa_kappa,'in_densities',kappa_fitN,/ADD_REPLACE
  ENDIF

  IF KEYWORD_SET(kappas) THEN BEGIN
     STR_ELEMENT,fa_kappa,'in_kappas',kappas,/ADD_REPLACE
  ENDIF

  ;;Now just Maxwellians
  IF KEYWORD_SET(Gauss_fitT) THEN BEGIN
     STR_ELEMENT,fa_Gauss,'in_temperatures',Gauss_fitT,/ADD_REPLACE
  ENDIF

  IF KEYWORD_SET(Gauss_fitN) THEN BEGIN
     STR_ELEMENT,fa_Gauss,'in_densities',Gauss_fitN,/ADD_REPLACE
  ENDIF

  tmpSort     = SORT(pot)
  X           = pot[tmpSort]
  Y           = (cur*(KEYWORD_SET(flip_current_sign) ? -1D : 1D))[tmpSort]
  XError      = potErr[tmpSort]
  YError      = curErr[tmpSort]
  weights     = (1./ABS(curErr)^2)[tmpSort]

  fitKappa_A  = MPFITFUN(jvFitFunc, $
                         X,Y, $
                         /NAN, $
                         WEIGHTS=weights, $
                         FUNCTARGS=fa_kappa, $
                         BESTNORM=bestNorm, $
                         NFEV=nfev, $
                         FTOL=fTol, $
                         GTOL=gTol, $
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
                         MAXITER=maxIter, $
                         NITER=itNum, $
                         YFIT=fitKappa_cur, $
                         /QUIET, $
                         ERRMSG=errMsg, $
                         _EXTRA=extra)

  fitGauss_A  = MPFITFUN(jvFitFunc, $
                         X,Y, $
                         /NAN, $
                         WEIGHTS=weights, $
                         FUNCTARGS=fa_Gauss, $
                         BESTNORM=bestNorm, $
                         NFEV=nfev, $
                         FTOL=fTol, $
                         GTOL=gTol, $
                         STATUS=gaussStatus, $
                         BEST_RESID=best_resid, $
                         PFREE_INDEX=ifree, $
                         CALC_FJAC=calc_fjac, $
                         BEST_FJAC=best_fjac, $
                         PARINFO=gaussParamStruct, $
                         QUERY=query, $
                         NPEGGED=npegged, $
                         NFREE=nfree, $
                         DOF=dof, $
                         COVAR=covar, $
                         PERROR=perror, $
                         MAXITER=maxIter, $
                         NITER=itNum, $
                         YFIT=fitGauss_cur, $
                         /QUIET, $
                         ERRMSG=errMsg, $
                         _EXTRA=extra)

  PRINT,"TIME SERIES: Kappa fitparams : "
  PRINT_JV_FIT_PARAMS,fitKappa_A
  PRINT,""
  PRINT,"TIME SERIES: Gauss fitparams: "
  PRINT_JV_FIT_PARAMS,fitGauss_A
  PRINT,""

END
