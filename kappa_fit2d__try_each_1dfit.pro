PRO KAPPA_FIT2D__TRY_EACH_1DFIT,keep_iTime,iTime, $
                                nEnergies,out_eRange_peak, $
                                allAngles,nTotAngles,useTheseAnglesIndex, $
                                successes, $
                                curFitStr,fits,curDataStr, $
                                good_angleBin_i,good_fits_i,iWin, $
                                ;; JUST_ERANGE_PEAK=just_eRange_peak, $
                                ESTFACS=estFacs, $
                                KCURVEFIT_OPT=kCurvefit_opt, $
                                KFITPARAMSTRUCT=kFitParamStruct, $
                                KFIT2DPARAMSTRUCT=kFit2DParamStruct, $
                                KPLOT_OPT=kPlot_opt, $
                                KSDTDATA_OPT=kSDTData_opt, $
                                KSTRINGS=kStrings, $
                                FIT2D_INF_LIST=fit2D_inf_list, $
                                FIT2D__SHOW_AND_PROMPT__EACH_CANDIDATE=show_and_prompt, $
                                FIT2D__SHOW__IS_MAXWELLIAN_FIT=is_Maxwellian_fit, $
                                FIT2D__SHOW__FITSTRING=fitString, $
                                PRINT_2DFITINFO=print_2DFitInfo, $
                                PRINT_2DWININFO=print_2DWinInfo

  COMPILE_OPT idl2

  ;; just_eRange_peak  = 1

  OKStatus          = [1,2,3,4] ;These are all the acceptable outcomes of fitting with MPFIT2DFUN

  func              = KEYWORD_SET(kCurvefit_opt.fit1D_dens__each_angle) ? $
                      'KAPPA_FLUX2D__SCALE_DENSITY__INDIVIDUAL_1D_ESTS' : $
                      'KAPPA_FLUX2D__SCALE_DENSITY'

  nGoodFits         = N_ELEMENTS(good_fits_i)

  testArrays        = MAKE_ARRAY(nEnergies,nTotAngles,nGoodFits,/FLOAT)

  chiArray          = !NULL
  dofArray          = !NULL
  fitDensArray      = !NULL
  densEstArray      = !NULL
  errMsgArray       = !NULL
  statusArray       = !NULL

  FOR iWin=0,nGoodFits-1 DO BEGIN

     ;; CASE 1 OF
     ;;    KEYWORD_SET(kCurvefit_opt.fit2d_only_eAngles): BEGIN


     ;;    END
     ;;    ELSE: BEGIN
     SETUP_KAPPA_FIT2D_TEST,good_angleBin_i,good_fits_i,iWin, $
                            nEnergies,out_eRange_peak, $
                            allAngles,nTotAngles,useTheseAnglesIndex, $
                            curFitStr,fits,curDataStr, $
                            iAngle,iFit,testFitStr,testFit1DInfo,testArray, $
                            craptest, $
                            wtsForFit,X2D,Y2D,dataToFit, $
                            fa, $
                            ;; dens_param, $
                            pre_densEst, $
                            ITIME=iTime, $
                            ;; JUST_ERANGE_PEAK=just_eRange_peak, $
                            ESTFACS=estFacs, $
                            KCURVEFIT_OPT=kCurvefit_opt, $
                            KFITPARAMSTRUCT=kFitParamStruct, $
                            KSDTDATA_OPT=kSDTData_opt, $
                            KSTRINGS=kStrings, $
                            OUT_FIT2D_DENS_ANGLEINFO=fit2D_dens_angleInfo, $
                            ;; OUT_ANGLE_I=angle_i, $
                            OUT_ERANGE_I=eRange_i

     ;; kFit2DParamStruct.value    = dens_param
     UPDATE_KAPPA_FIT2D_PARAM_INFO,kFit2DParamStruct,pre_densEst

     fitDens        = MPFIT2DFUN(func,X2D,Y2D,dataToFit, $
                                 err, $
                                 ;; dens_param, $
                                 WEIGHTS=wtsForFit, $
                                 FUNCTARGS=fa, $
                                 BESTNORM=bestNorm, $
                                 NFEV=nfev, $
                                 ;; FTOL=kCurvefit_opt.fit2d_tol, $
                                 FTOL=1e-4, $
                                 GTOL=1e-13, $
                                 STATUS=status, $
                                 BEST_RESID=best_resid, $
                                 PFREE_INDEX=ifree, $
                                 CALC_FJAC=calc_fjac, $
                                 BEST_FJAC=best_fjac, $
                                 PARINFO=kFit2DParamStruct, $
                                 QUERY=query, $
                                 NPEGGED=npegged, NFREE=nfree, DOF=dof, $
                                 COVAR=covar, PERROR=perror, $
                                 MAXITER=kCurvefit_opt.fit2d_max_iter, $
                                 NITER=niter, $
                                 YFIT=yfit, $
                                 /QUIET, $
                                 ERRMSG=errMsg, $
                                 _EXTRA=extra)


     IF KEYWORD_SET(kCurvefit_opt.fit2d_just_eRange_peak) THEN BEGIN
        oldcraptest = craptest
        FOR m=0,N_ELEMENTS(yFit[*,0])-1 DO BEGIN
           craptest.data[eRange_i[m],fit2D_dens_angleInfo.angle_i] = yFit[m,*]
        ENDFOR
        ;; STOP
     ENDIF ELSE BEGIN
        craptest.data[*,fit2D_dens_angleInfo.angle_i]  = yFit
     ENDELSE
     densEst        = CALL_FUNCTION(kSDTData_opt.densFunc,craptest, $
                                    ENERGY=kSDTData_opt.energy_electrons, $
                                    ANGLE=kSDTData_opt.fit2D_dens_aRange)
     densEstArray   = [densEstArray,densEst]
     errMsgArray    = [errMsgArray,errMsg]
     statusArray    = [statusArray,status]
     chiArray       = [chiArray,bestNorm]
     dofArray       = [dofArray,dof]
     fitDensArray   = [fitDensArray,fitDens]
     IF (WHERE(status EQ OKStatus))[0] NE -1 THEN BEGIN

        ;;Info?
        IF KEYWORD_SET(print_2DFitInfo) THEN BEGIN
           testFit1DInfo.A[3] = densEst
           PRINT_KAPPA_FLUX_FIT_PARAMS,testFit1DInfo.A,bestNorm
        ENDIF

        CASE 1 OF
           kCurvefit_opt.fit2d_only_dens_angles: BEGIN
              CASE 1 OF
                 KEYWORD_SET(kCurvefit_opt.fit2d_just_eRange_peak): BEGIN
                    FOR m=0,N_ELEMENTS(yFit[*,0])-1 DO BEGIN
                       testArrays[eRange_i[m],fit2D_dens_angleInfo.angle_i,iWin] = yFit[m,*]
                    ENDFOR
                    
                 END
                 ELSE: BEGIN
                    testArrays[*,fit2D_dens_angleInfo.angle_i,iWin]  = yFit
                 END
              ENDCASE
           END
           kCurvefit_opt.fit2d_only_eAngles: BEGIN
              CASE 1 OF
                 KEYWORD_SET(kCurvefit_opt.fit2d_just_eRange_peak): BEGIN
                    FOR m=0,N_ELEMENTS(yFit[*,0])-1 DO BEGIN
                       testArrays[eRange_i[m],fit2D_dens_angleInfo.angle_i,iWin] = yFit[m,*]
                    ENDFOR
                    
                 END
                 ELSE: BEGIN
                    testArrays[*,fit2D_dens_angleInfo.angle_i,iWin]  = yFit
                 END
              ENDCASE
           END
           ELSE: BEGIN
              testArrays[*,*,iWin]        = yFit
           END
        ENDCASE

     ENDIF ELSE BEGIN
        IF KEYWORD_SET(print_2DFitInfo) THEN BEGIN
           PRINT,fitString + ' 2DFit failure ...'
        ENDIF

        testArrays[*,*,iWin]              = 0.0
     ENDELSE
     ;;    END
     ;; ENDCASE



     IF KEYWORD_SET(show_and_prompt) THEN BEGIN
        tmp2DInfoStruct = {bestFitStr      :crapTest     , $
                           bestFit1DParams :fits[iFit]   , $
                           bestAngle_i     :iAngle       , $
                           bestDens        :densEst      , $
                           bestChi2        :bestNorm}

        KAPPA_FIT2D__SHOW_AND_PROMPT__EACH_CANDIDATE,curDataStr,tmp2DInfoStruct, $
           iWin, $
           iTime, $
           IS_MAXWELLIAN_FIT=is_Maxwellian_fit, $
           KCURVEFIT_OPT=kCurvefit_opt, $
           KPLOT_OPT=kPlot_opt, $
           KSDTDATA_OPT=kSDTData_opt, $
           KSTRINGS=kStrings, $
           PROMPT__CONT_TO_NEXT_FIT=prompt__cont_to_next_fit, $
           PROMPT__CONT_UNTIL_FIT_EQ=prompt__cont_until_fit_eq, $
           FINISH_AND_SAVE_ALL=finish_and_save_all, $
           KAPPA_FIT__SHOW__QUIT=show__quit

     ENDIF

  ENDFOR
  
  ;; IF KEYWORD_SET(kCurvefit_opt.postfit_remaining_angles) THEN BEGIN

  ;; ENDIF

  datDens             = CALL_FUNCTION(kSDTData_opt.densFunc,curDatastr, $
                                      ENERGY=kSDTData_opt.energy_electrons, $
                                      ANGLE=kSDTData_opt.fit2D_dens_aRange)

  ;;Now decide who is most awesome
  success_i           = WHERE(statusArray EQ 1 OR $
                              statusArray EQ 2 OR $ 
                              statusArray EQ 3 OR $
                              statusArray EQ 4, $
                              nSuccess)
  IF nSuccess GT 0 THEN BEGIN

     minTemp          = MIN(chiArray[success_i],minChi_ii)
     ;; win_i               = CGSETINTERSECTION(success_i,minChi_i,COUNT=haveWin)
     win_i            = success_i[minChi_ii]
  ;; ENDIF

  ;; IF KEYWORD_SET(haveWin) THEN BEGIN
     successes++

     iAngleWin        = good_angleBin_i[win_i]
     fitWin           = fits[good_fits_i[win_i]]
     chi2win          = chiArray[win_i]
     densWin          = densEstArray[win_i]
     winStruct        = curDataStr
     winStruct.data   = testArrays[*,*,win_i]

     keep_iTime       = [keep_iTime,iTime]

     tmpKeeper        = {bestFitStr      :winStruct    , $
                         bestFit1DParams :fitWin       , $
                         bestAngle_i     :iAngleWin    , $
                         bestDens        :densWin      , $
                         bestChi2        :chi2win      , $
                         datDens         :datDens      , $
                         chiArray        :chiArray     , $
                         dofArray        :dofArray     , $
                         densEstArray    :densEstArray , $
                         fitDensArray    :fitDensArray , $
                         errMsgArray     :errMsgArray  , $
                         statusArray     :statusArray}

     fit2D_inf_list.ADD,tmpKeeper                             

     IF KEYWORD_SET(print_2DWinInfo) THEN BEGIN
        tmpFit1D_A   = fitWin.A
        tmpFit1D_A[3] = densWin

        PRINT,''
        PRINT,'******************************'
        PRINT,FORMAT='("WINNER (",A0," #",I0,")")',fitString,iTime
        PRINT,''
        PRINT_KAPPA_FLUX_FIT_PARAMS,tmpFit1D_A,chi2win
        PRINT,'******************************'
     ENDIF

  ENDIF ELSE BEGIN
     winStruct        = !NULL
  ENDELSE

END