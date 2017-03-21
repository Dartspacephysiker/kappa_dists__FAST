;2017/03/18
PRO ESTIMATE_JV_CURVE_FROM_AVERAGE_PARAMS,jvPlotData,avgs_JVfit, $
                                          ORBIT=orbit, $
                                          A_IN=A_in, $
                                          ORIGINATING_ROUTINE=routName, $
                                          SAVEPLOT=savePlot, $
                                          SPNAME=spName

  COMPILE_OPT IDL2,STRICTARRSUBS

  orbPref     = ''
  IF KEYWORD_SET(orbit) THEN BEGIN
     orbPref  = 'Orbit ' + STRCOMPRESS(orbit,/REMOVE_ALL)
  ENDIF

  maxIter     = 150
  fit_tol     = 1D-15
  gTol        = 1D-15

                             ;;            kappa,            Temp,            Dens,  R_B
  A           = KEYWORD_SET(A_in) ? A_in : [  10,avgs_JVfit.T.avg,avgs_JVfit.N.avg, 1D4]

  ;;Keep the original guesses
  Aorig       = A
  AGaussOrig  = A

  kappa_fixA  = [0,1,1,0]
  gauss_fixA  = [1,1,1,0]

  PRINT,"Kappa startGuess: "
  PRINT_JV_FIT_PARAMS,A
  PRINT,"Gauss startGuess: "
  PRINT_JV_FIT_PARAMS,AGaussOrig

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

  titleStr         = STRING(FORMAT='(A0," (T=",F0.1," eV, N=",G0.3," cm!U-3!N)")', $
                            orbPref,avgs_JVfit.T.avg,avgs_JVfit.N.avg)
  kappaName        = STRING(FORMAT='("$\kappa$=",F0.2,", R!DB!N=",G0.3)',A[0],A[3])
  gaussName        = STRING(FORMAT='("Maxwell, R!DB!N=",G0.3)',AGauss[3])

  window1          = WINDOW(DIMENSION=[1000,800],BUFFER=savePlot)

  ;; that             = ERRORPLOT(X,Y,XError,YError, $
  that             = ERRORPLOT(X,Y,YError, $
                               SYMBOL='*', $
                               LINESTYLE='', $
                               NAME='Data', $
                               TITLE=titleStr, $
                               XTITLE='$\Phi$ (V)', $
                               YTITLE='Current Density at 100 km ($\mu$A/m!U2!N)', $
                               /CURRENT)
  
  ;; that          = PLOT(X,Y,SYMBOL='*',LINESTYLE='')
  this             = PLOT(X,YFit, $
                          NAME=kappaName, $
                          COLOR='BLUE', $
                          /OVERPLOT)
  those            = PLOT(X,yGaussFit, $
                          NAME=gaussName, $
                          COLOR='Brown', $
                          /OVERPLOT)

  ;; legPos__data  = [(MAX(X)-MIN(X))*0.2+MIN(X),(MAX(Y)-MIN(Y))*0.95+MIN(Y)]
  ;; legPos           = [0.5,0.85]
  legPos           = [0.5,0.5]
  ;; leg           = LEGEND(TARGET=[that,this,those],POSITION=legPos__data,/DATA)
  leg              = LEGEND(TARGET=[that,this,those], $
                            POSITION=legPos)


  IF KEYWORD_SET(savePlot) THEN BEGIN

     IF ~KEYWORD_SET(sPName) THEN BEGIN
        sPName     = routName + '-JV_fixedTandN.png'
     ENDIF

     IF ~KEYWORD_SET(plotDir) THEN BEGIN
        pDirSuff   = '/cur_and_pot_analysis'
        SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF=pDirSuff
     ENDIF

     PRINT,"Saving to " + sPName + ' ...'

     window1.Save,plotDir+sPName

  ENDIF

END
