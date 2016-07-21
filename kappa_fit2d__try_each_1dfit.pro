PRO KAPPA_FIT2D__TRY_EACH_1DFIT,keeper_bounds_i,iTime, $
                                nEnergies,nTotAngles, $
                                successes, $
                                curFitStr,fits,curDataStr, $
                                good_angleBin_i,good_fits_i,iWin, $
                                KCURVEFIT_OPT=kCurvefit_opt, $
                                KSDTDATA_OPT=kSDTData_opt, $
                                FIT2D_INF_LIST=fit2D_inf_list

  COMPILE_OPT idl2

  nGoodFits                                      = N_ELEMENTS(good_fits_i)

  testArrays                                     = MAKE_ARRAY(nEnergies,nTotAngles,nGoodFits,/FLOAT)

  chiArray                                       = !NULL
  dofArray                                       = !NULL
  fitDensArray                                   = !NULL
  densEstArray                                   = !NULL
  errMsgArray                                    = !NULL
  statusArray                                    = !NULL
  FOR iWin=0,nGoodFits-1 DO BEGIN

     SETUP_KAPPA_FIT2D_TEST,good_angleBin_i,good_fits_i,iWin, $
                            nEnergies,nTotAngles, $
                            curFitStr,fits,curDataStr, $
                            iAngle,iFit,testFitStr,testFitParams,testArray, $
                            craptest, $
                            wts,X2D,Y2D,dataToFit, $
                            fa,dens_param

     FitDens                                 = MPFIT2DFUN('KAPPA_FLUX2D__SCALE_DENSITY',X2D,Y2D,dataToFit, $
                                                          err, $
                                                          dens_param, $
                                                          WEIGHTS=wts, $
                                                          FUNCTARGS=fa, $
                                                          BESTNORM=bestNorm, $
                                                          NFEV=nfev, $
                                                          FTOL=kCurvefit_opt.fit2d_tol, $
                                                          STATUS=status, $
                                                          BEST_RESID=best_resid, $
                                                          PFREE_INDEX=ifree, $
                                                          CALC_FJAC=calc_fjac, $
                                                          BEST_FJAC=best_fjac, $
                                                          PARINFO=parinfo, QUERY=query, $
                                                          NPEGGED=npegged, NFREE=nfree, DOF=dof, $
                                                          COVAR=covar, PERROR=perror, $
                                                          MAXITER=kCurvefit_opt.fit2d_max_iter, $
                                                          NITER=niter, $
                                                          YFIT=yfit, $
                                                          QUIET=quiet, $
                                                          ERRMSG=errMsg, $
                                                          _EXTRA=extra)

     craptest.data                               = testArray
     densEst                                     = CALL_FUNCTION(kSDTData_opt.densFunc,craptest, $
                                                                 ENERGY=kSDTData_opt.energy_electrons, $
                                                                 ANGLE=kSDTData_opt.fit2D_dens_aRange)
     densEstArray                                = [densEstArray,densEst]
     errMsgArray                                 = [errMsgArray,errMsg]
     statusArray                                 = [statusArray,status]
     chiArray                                    = [chiArray,bestNorm]
     dofArray                                    = [dofArray,dof]
     fitDensArray                                = [fitDensArray,fitDens]
     IF status GT 0 THEN BEGIN
        testArrays[*,*,iWin]                     = yFit
     ENDIF ELSE BEGIN
        testArrays[*,*,iWin]                     = 0.0
     ENDELSE
  ENDFOR
  
  ;;Now decide who is most awesome
  success_i                                      = WHERE(statusArray EQ 1,nSuccess)
  minTemp                                        = MIN(chiArray,minChi_i)
  win_i                                          = CGSETINTERSECTION(success_i,minChi_i,COUNT=haveWin)

  IF KEYWORD_SET(haveWin) THEN BEGIN
     successes++

     iAngleWin                              = good_angleBin_i[win_i]
     fitWin                                 = fits[good_fits_i[win_i]]
     winStruct                              = curDataStr
     winStruct.data                         = testArrays[*,*,win_i]

     keeper_bounds_i                        = [keeper_bounds_i,iTime]

     tmpKeeper                              = {bestFitStr      :winStruct, $
                                               bestFit1DParams :fitWin, $
                                               chiArray        :chiArray    , $
                                               dofArray        :dofArray    , $
                                               densEstArray    :densEstArray, $
                                               fitDensArray    :fitDensArray, $
                                               errMsgArray     :errMsgArray , $
                                               statusArray     :statusArray}

     fit2D_inf_list.ADD,                 tmpKeeper                             
  ENDIF ELSE BEGIN
     winStruct                              = !NULL
  ENDELSE

END