;2017/03/18
PRO ESTIMATE_JV_CURVE_FROM_AVERAGE_PARAMS,jvPlotData,avgs_JVfit, $
                                          USE_SOURCE_AVGS=use_source_avgs, $
                                          A_IN=A_in, $
                                          KAPPALIMS=kappaLims, $   
                                          TEMPLIMS=TempLims, $    
                                          DENSLIMS=DensLims, $    
                                          MAGRATIOLIMS=magRatioLims, $
                                          MULTI_MAGRATIO_MODE=multi_magRatio_mode, $
                                          MAP__MULTI_MAGRATIO_ARRAY=multi_magRatio_array, $
                                          MAP__MULTI_KAPPA_ARRAY=multi_kappa_array, $
                                          MAP__2D=map__2D, $
                                          ORIGINATING_ROUTINE=routName, $
                                          OUT_KAPPA_A=A, $
                                          OUT_GAUSS_A=AGauss, $
                                          OUT_PLOTDATA=pData, $
                                          OUT_MULTI_MAGRATIO=mMagDat, $
                                          _EXTRA=e

  COMPILE_OPT IDL2,STRICTARRSUBS

  maxIter     = 3000
  fit_tol     = 1D-15
  gTol        = 1D-15

  IF KEYWORD_SET(use_source_avgs) THEN BEGIN
     Temperature = avgs_JVfit.T_SC.avg
     Density     = avgs_JVfit.N_SC.avg
  ENDIF ELSE BEGIN
     Temperature = avgs_JVfit.T.avg
     Density     = avgs_JVfit.N.avg
  ENDELSE
                             ;;            kappa,       Temp,   Dens, R_B
  A           = KEYWORD_SET(A_in) ? A_in : [  10,Temperature,Density, 1D4]

  ;;Keep the original guesses
  Aorig       = A
  AGaussOrig  = A

  kappa_fixA  = [0,1,1,0]
  gauss_fixA  = [1,1,1,0]

  PRINT,"Kappa startGuess: "
  PRINT_JV_FIT_PARAMS,A
  PRINT,"Gauss startGuess: "
  PRINT_JV_FIT_PARAMS,AGaussOrig

  kappaParamStruct = INIT_JV_FITPARAM_INFO(A,kappa_fixA, $
                                           KAPPALIMS=kappaLims, $   
                                           TEMPLIMS=TempLims, $    
                                           DENSLIMS=DensLims, $    
                                           MAGRATIOLIMS=magRatioLims)
  gaussParamStruct = INIT_JV_FITPARAM_INFO(TEMPORARY(A),gauss_fixA, $
                                           KAPPALIMS=kappaLims, $   
                                           TEMPLIMS=TempLims, $    
                                           DENSLIMS=DensLims, $    
                                           MAGRATIOLIMS=magRatioLims)

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

  CASE 1 OF
     KEYWORD_SET(multi_magRatio_mode): BEGIN

        CASE 1 OF
           KEYWORD_SET(map__2D): BEGIN


              dof = N_ELEMENTS(X); - 4

              ;;Fix kappa [0] and magratio [3]
              kappaParamStruct[0].fixed = 1B
              kappaParamStruct[3].fixed = 1B

              nMagRatio = N_ELEMENTS(multi_magRatio_array)
              nKappa    = N_ELEMENTS(multi_kappa_array)
              ;; m2D_multi_kappa_array     = multi_kappa_array              # MAKE_ARRAY(nMagRatio,/FLOAT,VALUE=1.0)
              ;; m2D_multi_magRatio_array  = MAKE_ARRAY(nKappa,/FLOAT,VALUE=1.0) # multi_magRatio_array

              kappaArr  = multi_kappa_array                   # MAKE_ARRAY(nMagRatio,/FLOAT,VALUE=1.0)
              magRatArr = MAKE_ARRAY(nKappa,/FLOAT,VALUE=1.0) # multi_magRatio_array

              chi2Arr   = MAKE_ARRAY(nKappa,nMagRatio,/FLOAT)

              ;;Reclaim
              A         = kappaParamStruct[*].value

              FOR j=0,nKappa-1 DO BEGIN

                 FOR k=0,nMagRatio-1 DO BEGIN

                    ;; kappaParamStruct[3].value     = magRatArr[j,k]
                    ;; kappaParamStruct[0].value     = kappaArr[j,k]

                    yFit = KNIGHT_RELATION__DORS_KLETZING_11(kappaArr[j,k], $
                                                             A[1], $
                                                             A[2], $
                                                             X, $
                                                             magRatArr[j,k], $
                                                             IN_POTBAR=in_potBar, $
                                                             OUT_POTBAR=potBar, $
                                                             /NO_MULT_BY_CHARGE)*1D6
                    
                    ;; chi2 = TOTAL( (Y-yFit)^2 * ABS(weights) * ( (kCurvefit_opt.fit1D__weighting EQ 1) ? ABS(weights) : 1.D) )
                    ;; chi2        = TOTAL( (Y-yFit)^2 * ABS(weights) )
                    chi2        = TOTAL( (Y-yFit)^2 * ABS(weights) ) / dof
                    chi2Arr[j,k]  = TEMPORARY(chi2) ;[chi2Arr,TEMPORARY(chi2)]
                    ;; kappaArr[j,k] = A[0]

                 ENDFOR

              ENDFOR

           END
           ELSE: BEGIN

              kappaParamStruct[3].fixed = 1B

              nMagRatio = N_ELEMENTS(multi_magRatio_array)

              magRatArr = multi_magRatio_array
              kappaArr  = MAKE_ARRAY(nMagRatio,/FLOAT)
              chi2Arr   = MAKE_ARRAY(nMagRatio,/FLOAT)

              FOR k=0,nMagRatio-1 DO BEGIN

                 kappaParamStruct[3].value     = multi_magRatio_array[k]
                 kappaParamStruct[3].limits[1] = multi_magRatio_array[k]+.1

                 A      = MPFITFUN(jvFitFunc, $
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
                 
                 ;; chi2 = TOTAL( (Y-yFit)^2 * ABS(weights) * ( (kCurvefit_opt.fit1D__weighting EQ 1) ? ABS(weights) : 1.D) )
                 ;; chi2        = TOTAL( (Y-yFit)^2 * ABS(weights) )
                 chi2        = TOTAL( (Y-yFit)^2 * ABS(weights) ) / dof
                 chi2Arr[k]  = TEMPORARY(chi2) ;[chi2Arr,TEMPORARY(chi2)]
                 kappaArr[k] = A[0]

              ENDFOR


           END
        ENDCASE

        mMagDat = {kappa  : TEMPORARY(kappaArr), $
                   T      : Temperature, $
                   N      : Density, $
                   magRat : magRatArr, $
                   chi2   : TEMPORARY(chi2Arr)}
     END
     ELSE: BEGIN


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

        pData = {X         : X      , $
                 Y         : Y      , $
                 Yerror    : Yerror , $
                 YFit      : Yfit   , $
                 YGaussFit : YGaussFit}


     END
  ENDCASE

END
