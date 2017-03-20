;2017/03/03
;Here are the winners from 2017/03/04's headbangerfest:
;  aRange__moments_e_down  = [330.,30.]
;  energyArr               = [[300,3.0e4],[0,3.0e4],[0,2.4e4]]

;2017/03/17
;A      = vector of function params:

; A[0]: E_b,       Plasma bulk energy (eV)
; A[1]: T,         Plasma kinetic temperature (eV)
; A[2]: kappa,     Kappa (of course!)--or more specifically 3D kappa index, so that kappa = kappa_0 + 3/2
; A[3]: n,         Plasma density (cm^-3)
; A[4]: bulkAngle, Angle between bulk velocity, u_b, and velocity in direction for which we're interested in the distribution

FUNCTION INIT_JV_FITPARAM_INFO,A,fixA;; , $
                                  ;; ERANGE_PEAK=eRange_peak

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; ;;And don't let kappa get out of hand
  ;; AMaxStep[0]     = 1.0

  ;; ;;And don't let temperature get out of hand
  ;; AMaxStep[1]     = 30.

  ;; ;;And don't let DENSITY get out of hand!
  ;; AMaxStep[2]     = 0.5
  AMaxStep        = DOUBLE([1.0, $
                            30., $
                            0.5, $
                            1000])

  Alimited         = [[1,1], $
                      [1,1], $
                      [1,1], $
                      [1,1]]
  
  Alimits         = [[1.501D   ,3.0   ] , $ ;kappa 
                     [10      ,3.0e4 ] , $ ;Temp
                     [1e-4    ,100   ] , $ ;N
                     [1       ,300 ]]    ;R_B

  ;;Make 'em play nice
  ;; FOR k=0,N_ELEMENTS(A)-1 DO BEGIN
  ;;    IF A[k] LT Alimits[0,k] THEN A[k] = Alimits[0,k]
  ;;    IF A[k] GT Alimits[1,k] THEN A[k] = Alimits[1,k]
  ;; ENDFOR

  Alimited        = TRANSPOSE(Alimited)
  Alimits         = TRANSPOSE(Alimits)

  paramInfo = REPLICATE({value:0.D       , $
                       fixed:0B        , $
                       parname:''      , $
                       ;; relstep:0.D     , $
                       ;; mpmaxstep:0.D   , $
                       limited:[0B,0]   , $
                       limits:[0.D,0]} , $
                      ;; 7)
                      4)

  ;;Starting values
  paramInfo[*].value = A

  ;;Which ones are fixednnn?
  paramInfo[*].fixed = fixA

  ;;And their names?
  paramInfo[*].parName = ["kappa","T","N","R_B"]

  ;;Got it. What about anything like, you know, a max step size?
  ;; paramInfo[*].mpmaxstep  = AMaxStep

  ;;So certain values can't be exceeded?
  paramInfo[*].limited[0] = Alimited[*,0]
  paramInfo[*].limited[1] = Alimited[*,1]

  ;;What are the limits, then?
  paramInfo[*].limits[0] = Alimits[*,0]
  paramInfo[*].limits[1] = Alimits[*,1]

  RETURN,paramInfo

END

PRO PRINT_JV_FIT_PARAMS,A
  
  PRINT,FORMAT='("Kappa",T10,"Plasma temp. (eV)",T30,"Density (cm^-3)",T45,"R_B")'
  PRINT,FORMAT='(F-7.3,T10,F-15.3,T30,F-8.4,T45,G-10.2)', $
        A[0], $
        A[1], $
        A[2], $
        A[3]

END

;2017/03/18
PRO ESTIMATE_JV_CURVE_FROM_AVERAGE_PARAMS,jvPlotData,avgs_JVfit

  COMPILE_OPT IDL2,STRICTARRSUBS

  maxIter     = 150
  fit_tol     = 1D-15
  gTol        = 1e-15

  ;;            kappa,            Temp,            Dens,  R_B
  A           = [  2.99,avgs_JVfit.T.avg,avgs_JVfit.N.avg, 20]

  ;;Keep the original guesses
  Aorig       = A
  AGaussOrig  = A

  kappa_fixA  = [1,1,1,0]
  gauss_fixA  = [1,1,1,0]

  kappaParamStruct = INIT_JV_FITPARAM_INFO(           A,kappa_fixA)
  gaussParamStruct = INIT_JV_FITPARAM_INFO(TEMPORARY(A),gauss_fixA)

  fa_kappa    = {no_mult_by_charge : 1B, $
                 is_Maxwellian_fit : 0B  }

  fa_Gauss    = {no_mult_by_charge : 1B, $
                 is_Maxwellian_fit : 1B  }

  jvFitFunc   = 'JV_CURVE_FIT__MAXWELL_KAPPA'
  OKStatus    = [1,2,3,4]        ;These are all the acceptable outcomes of fitting with MPFIT2DFUN

  X           = jvPlotData.pot[avgs_JVfit.useInds]
  Y           = jvPlotData.cur[avgs_JVfit.useInds]*(-1D)
  XError      = jvPlotData.potErr[avgs_JVfit.useInds]
  YError      = jvPlotData.curErr[avgs_JVfit.useInds]
  weights     = 1./ABS(jvPlotData.curErr[avgs_JVfit.useInds])^2

  A           = MPFITFUN(jvFitFunc, $
                         X,Y, $
                         /NAN, $
                         WEIGHTS=weights, $
                         FUNCTARGS=fa_kappa, $
                         BESTNORM=bestNorm, $
                         NFEV=nfev, $
                         FTOL=fit_tol, $
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
                         YFIT=yFit, $
                         /QUIET, $
                         ERRMSG=errMsg, $
                         _EXTRA=extra)

  AGauss      = MPFITFUN(jvFitFunc, $
                         X,Y, $
                         /NAN, $
                         WEIGHTS=weights, $
                         FUNCTARGS=fa_Gauss, $
                         BESTNORM=bestNorm, $
                         NFEV=nfev, $
                         FTOL=fit_tol, $
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
                         YFIT=yGaussFit, $
                         /QUIET, $
                         ERRMSG=errMsg, $
                         _EXTRA=extra)

  PRINT,"Kappa fitparams: "
  PRINT_JV_FIT_PARAMS,A
  PRINT,""
  PRINT,"Gauss fitparams: "
  PRINT_JV_FIT_PARAMS,AGauss
  PRINT,""

  ;; that = ERRORPLOT(X,Y,XError,YError,SYMBOL='*',LINESTYLE='')
  that = ERRORPLOT(X,Y,YError,SYMBOL='*',LINESTYLE='')
  ;; that = PLOT(X,Y,SYMBOL='*',LINESTYLE='')
  this = PLOT(X,YFit,COLOR='BLUE',/OVERPLOT)
  those = PLOT(X,yGaussFit,COLOR='Brown',/OVERPLOT)

END
