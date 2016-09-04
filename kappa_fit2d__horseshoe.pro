;2016/09/02
PRO KAPPA_FIT2D__HORSESHOE,keep_iTime,iTime, $
                           out_eRange_peak, $
                           nTotAngles, $
                           successes, $
                           curFitStr,fits,curDataStr, $
                           fitAngle_i, $
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

  @common__kappa_flux2d__horseshoe__eanisotropy.pro

  OKStatus     = [1,2,3,4]      ;These are all the acceptable outcomes of fitting with MPFIT2DFUN

  SETUP_KAPPA_FIT2D__HORSESHOE_TEST, $
     out_eRange_peak, $
     nTotAngles, $
     curFitStr,curDataStr, $
     wtsForFit,X2D,Y2D,dataToFit, $
     fa, $
     ITIME=iTime, $
     KCURVEFIT_OPT=kCurvefit_opt, $
     KFITPARAMSTRUCT=kFitParamStruct, $
     KSDTDATA_OPT=kSDTData_opt, $
     KSTRINGS=kStrings, $
     OUT_FIT2D_DENS_ANGLEINFO=fit2D_dens_angleInfo, $
     OUT_ERANGE_I=eRange_i
  
  CASE 1 OF
     KEYWORD_SET(kCurvefit_opt.fit2D__bulk_e_anisotropy): BEGIN
        func   = 'KAPPA_FLUX2D__HORSESHOE__ENERGY_ANISOTROPY__COMMON'

        fitAngle_i = 0.0       ;As we learn later
        
        ;; func   = 'KAPPA_FLUX2D__HORSESHOE__ENERGY_ANISOTROPY'

        ;; factor =   KAPPA_EFLUX__ANISOTROPY_DIST( $
        ;;            curDataStr.energy, $
        ;;            curDataStr.theta, $
        ;;            curDataStr.data, $
        ;;            fitAngle_i, $
        ;;            BULK_ENERGY=kFitParamStruct[0].value, $
        ;;            MIN_ENERGY=kCurvefit_opt.min_peak_energy, $
        ;;            REDUCENEGFAC=kCurvefit_opt.fit2D__bulk_e_anis_factor, $
        ;;            LOGSCALE_REDUCENEGFAC=logScale_reduceNegFac, $
        ;;            PLOT_FACTOR=plot_factor, $
        ;;            PLOT_COMPARISON=plot_comparison, $
        ;;            OUT_PEAK_ENERGIES=peakEn__en, $
        ;;            OUT_ANGLES=peakEn__angle)

        ;; fa     = CREATE_STRUCT(fa,"BULK_E_ANISOTROPY",factor, $
        ;;                       "BULK_E_ANGLE",peakEn__angle)
     END
     ELSE: BEGIN
        func   = 'KAPPA_FLUX2D__HORSESHOE'

     END
  ENDCASE


  ;; FOR kDog=0,N_ELEMENTS(kFitParamStruct)-1 DO BEGIN

  fit2DParams  = MPFIT2DFUN(func,X2D,Y2D,dataToFit, $
                            err, $
                            WEIGHTS=wtsForFit, $
                            FUNCTARGS=fa, $
                            BESTNORM=bestNorm, $
                            NFEV=nfev, $
                            ;; FTOL=kCurvefit_opt.fit2d_tol, $
                            FTOL=1e-4, $
                            GTOL=1e-13, $
                            STATUS=status, $
                            BEST_RESID=best_resid, $
                            PFREE_INDEX=iFree, $
                            /CALC_FJAC, $
                            BEST_FJAC=best_fJac, $
                            ;; PARINFO=kFit2DParamStruct, $
                            PARINFO=kFitParamStruct, $
                            QUERY=query, $
                            NPEGGED=nPegged, $
                            NFREE=nFree, $
                            DOF=dof, $
                            COVAR=covar, PERROR=pError, $
                            MAXITER=kCurvefit_opt.fit2d_max_iter, $
                            NITER=nIter, $
                            YFIT=yFit, $
                            /QUIET, $
                            ERRMSG=errMsg, $
                            _EXTRA=extra)

  IF (WHERE(status EQ OKStatus))[0] NE -1 THEN BEGIN

     ;;Info?
     IF KEYWORD_SET(print_2DFitInfo) THEN BEGIN
        PRINT_KAPPA_FLUX_FIT_PARAMS,fit2DParams,bestNorm
     ENDIF
     nSuccess     = 1

  ENDIF ELSE BEGIN
     IF KEYWORD_SET(print_2DFitInfo) THEN BEGIN
        PRINT,fitString + ' 2DFit failure ...'
     ENDIF
     nSuccess     = 0
  ENDELSE

  fit2DStr        = curFitStr

  CASE 1 OF
     KEYWORD_SET(kCurvefit_opt.fit2d_just_eRange_peak): BEGIN
        oldfit2DStr = fit2DStr
        FOR m=0,N_ELEMENTS(yFit[*,0])-1 DO BEGIN
           fit2DStr.data[eRange_i[m],fit2D_dens_angleInfo.angle_i] = yFit[m,*]
        ENDFOR
     END
     KEYWORD_SET(kCurvefit_opt.fit2D_fit_above_minE): BEGIN
        ;; oldfit2DStr = fit2DStr
        FOR m=0,N_ELEMENTS(yFit[*,0])-1 DO BEGIN
           fit2DStr.data[eRange_i[m],fit2D_dens_angleInfo.angle_i] = yFit[m,*]
        ENDFOR
     END
     ELSE: BEGIN
        fit2DStr.data[*,fit2D_dens_angleInfo.angle_i]  = yFit
     END
  ENDCASE


  IF KEYWORD_SET(show_and_prompt) THEN BEGIN
     densEst        = CALL_FUNCTION(kSDTData_opt.densFunc,fit2DStr, $
                                    ENERGY=kSDTData_opt.energy_electrons, $
                                    ANGLE=kSDTData_opt.fit2D_dens_aRange)

     tmp2DInfoStruct = {bestFitStr      :fit2DStr     , $
                        bestFit1DParams :fit2DParams  , $
                        fitAngle_i      :0.0          , $
                        bestDens        :densEst      , $
                        bestChi2        :bestNorm}

     KAPPA_FIT2D__SHOW_AND_PROMPT__EACH_CANDIDATE,curDataStr,tmp2DInfoStruct, $
        iWin, $
        iTime, $
        /FOR_HORSESHOE_FIT, $
        IS_MAXWELLIAN_FIT=is_Maxwellian_fit, $
        KCURVEFIT_OPT=kCurvefit_opt, $
        KPLOT_OPT=kPlot_opt, $
        KSDTDATA_OPT=kSDTData_opt, $
        KSTRINGS=kStrings, $
        PROMPT__CONT_TO_NEXT_FIT=prompt__cont_to_next_fit, $
        PROMPT__CONT_UNTIL_FIT_EQ=prompt__cont_until_fit_eq, $
        FINISH_AND_SAVE_ALL=finish_and_save_all, $
        ;; /FINISH_AND_SAVE_ALL, $
        KAPPA_FIT__SHOW__QUIT=show__quit

  ENDIF

  ;; ENDFOR

  ;;Now decide who is most awesome
  ;; success_i   = WHERE(statusArray EQ 1 OR $
  ;;                     statusArray EQ 2 OR $ 
  ;;                     statusArray EQ 3 OR $
  ;;                     statusArray EQ 4, $
  ;;                     nSuccess)

  IF nSuccess GT 0 THEN BEGIN
     
     successes++

     keep_iTime      = [keep_iTime,iTime]

     tmpKeeper       = {fitStr       : fit2DStr   , $
                        fitParams    : fit2DParams, $
                        chi2         : bestNorm   , $
                        errMsg       : errMsg     , $
                        status       : status     , $
                        nfEv         : nfEv       , $
                        best_resid   : best_resid , $
                        pFree_index  : iFree      , $
                        best_fJac    : best_fJac  , $
                        nPegged      : nPegged    , $
                        nFree        : nFree      , $
                        dof          : dof        , $
                        covar        : covar      , $
                        pError       : pError     , $
                        nIter        : nIter      }


     fit2D_inf_list.ADD,tmpKeeper                             

     IF KEYWORD_SET(print_2DWinInfo) THEN BEGIN
        PRINT,''
        PRINT,'******************************'
        PRINT,FORMAT='("WINNER (",A0," #",I0,")")',fitString,iTime
        PRINT,''
        PRINT_KAPPA_FLUX2D_HORSESHOE_PARAMS,fit2DParams,bestNorm
        PRINT,'******************************'
     ENDIF

  ENDIF ELSE BEGIN
     winStruct        = !NULL
  ENDELSE

END