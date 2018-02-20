;2016/09/02
PRO KAPPA_FIT2D__HORSESHOE,curDataStr, $
                           hadSuccess, $
                           IS_MAXWELLIAN_FIT=is_maxwellian_fit, $
                           ;; TIMEFNSTR=timeFNStr, $
                           ERANGE_FIT=eRange_fit, $
                           FITPARAMSTRUCT=fitParamStruct, $
                           FIT__LINEAR_ENERGY_SHIFT=fit__linear_energy_shift, $
                           ;; FIT2DPARAMSTRUCT=fit2DParamStruct, $
                           PRINT_2DFITINFO=print_2DFitInfo, $
                           FITSTRING=fitString, $
                           ;; IN_ESTIMATED_LC=estimated_lc, $
                           UNITS=units, $
                           OUT_FIT2DPARAMS=fit2DParams, $
                           MAKE_FIT2D_INFO=make_fit2D_info, $
                           OUT_FIT2D_FITINFO=fit2D_info, $
                           MONTE_CARLO_MODE=Monte_Carlo_mode, $
                           MC__OKSTATUS=MC__OKStatus, $
                           MC__FITSTRUCT=fit2DStr

  COMPILE_OPT IDL2,STRICTARRSUBS

  @common__kappa_flux2d__horseshoe__eanisotropy.pro

  @common__kappa_fit2d_structs.pro

  IF KEYWORD_SET(fit2D__save_all_plots) THEN finish_and_save_all = 1

  IF KEYWORD_SET(monte_carlo_mode) THEN BEGIN
     OKStatus = MC__OKStatus
  ENDIF ELSE BEGIN

     IF fitParamStruct[1].limited[1] EQ 0 THEN BEGIN
        ;; When the upperbound of T is not limited, xTol will become irreducible in
        ;; the course of the fit and status 7 will be reported. To avoid this, we
        ;; allow xTol = unminimizable to be an allowable outcome
        ;;See INIT_KAPPA_FITPARAM_INFO<f> for more informaciones

        OKStatus  = [1,2,3,4,7] ;These are all acceptable outcomes of fitting with MPFIT2DFUN

     ENDIF ELSE BEGIN
        OKStatus  = [1,2,3,4]
     ENDELSE

  ENDELSE

  SETUP_KAPPA_FIT2D__HORSESHOE, $
     eRange_fit, $
     curDataStr, $
     wtsForFit,X2D,Y2D,dataToFit, $
     fa, $
     IS_MAXWELLIAN_FIT=is_maxwellian_fit, $
     UNITS=units, $
     ;; IN_ESTIMATED_LC=estimated_lc, $
     OUT_FIT2D_DENS_ANGLEINFO=fit2D_dens_angleInfo
  
  IF STRUPCASE(fitParamStruct[1].parName) EQ 'T' THEN BEGIN
     fitParamStruct[1].fixed = KF2D__curveFit_opt.fit2D__clampTemperature
     fitParamStruct[3].fixed = KF2D__curveFit_opt.fit2D__clampDensity
  ENDIF ELSE BEGIN
     STOP
  ENDELSE

  CASE 1 OF
     KEYWORD_SET(KF2D__curveFit_opt.fit2D__bulk_e_anisotropy): BEGIN

        IF KEYWORD_SET(KF2D__curveFit_opt.fit__linear_energy_shift) THEN BEGIN
           func = 'KAPPA_FLUX2D__HORSESHOE__ENERGY_ANISOTROPY__LINEAR_ENERGY_SHIFT__COMMON'
        ENDIF ELSE BEGIN
           func   = 'KAPPA_FLUX2D__HORSESHOE__ENERGY_ANISOTROPY__COMMON'
        ENDELSE

        ;; fitAngle_i = 0.0       ;As we learn later
        
        ;; func   = 'KAPPA_FLUX2D__HORSESHOE__ENERGY_ANISOTROPY'

        ;; factor =   KAPPA_EFLUX__ANISOTROPY_DIST( $
        ;;            curDataStr.energy, $
        ;;            curDataStr.theta, $
        ;;            curDataStr.data, $
        ;;            fitAngle_i, $
        ;;            BULK_ENERGY=fitParamStruct[0].value, $
        ;;            MIN_ENERGY=KF2D__curveFit_opt.min_peak_energy, $
        ;;            REDUCENEGFAC=KF2D__curveFit_opt.fit2D__bulk_e_anis_factor, $
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

  fit2DParams  = MPFIT2DFUN(func,X2D,Y2D,dataToFit, $
                            err, $
                            WEIGHTS=wtsForFit, $
                            FUNCTARGS=fa, $
                            BESTNORM=bestNorm, $
                            NFEV=nfev, $
                            ;; FTOL=KF2D__curveFit_opt.fit2d_tol, $
                            FTOL=KF2D__curveFit_opt.fit2D_tol, $
                            GTOL=1e-10, $
                            STATUS=status, $
                            BEST_RESID=best_resid, $
                            PFREE_INDEX=iFree, $
                            /CALC_FJAC, $
                            BEST_FJAC=best_fJac, $
                            ;; PARINFO=fit2DParamStruct, $
                            PARINFO=fitParamStruct, $
                            QUERY=query, $
                            NPEGGED=nPegged, $
                            NFREE=nFree, $
                            DOF=dof, $
                            COVAR=covar, $
                            PERROR=pError, $
                            MAXITER=KF2D__curveFit_opt.fit2d_max_iter, $
                            NITER=nIter, $
                            YFIT=yFit, $
                            /QUIET, $
                            ERRMSG=errMsg, $
                            _EXTRA=extra)

  hadSuccess = (WHERE(status EQ OKStatus))[0] NE -1 

  IF ~hadSuccess THEN BEGIN

     IF KEYWORD_SET(print_2DFitInfo) THEN PRINT,fitString + $
        STRING(FORMAT='(A0,I0,A0)',' 2DFit failure (',status,') ...')

     pError = !VALUES.F_NaN
     nIter  = -1

  ENDIF

  IF KEYWORD_SET(make_fit2D_info) THEN BEGIN

     fit2D_info = {chi2         : bestNorm   , $
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
                   nIter        : nIter      , $
                   angleRange   : fit2D_dens_angleInfo.aRange}

  ENDIF

  ;; We still need a density estimate
  IF KEYWORD_SET(Monte_Carlo_mode) THEN BEGIN
     
     ;; To calc dens, we first need a synthed fitStruct
     CASE 1 OF
        KEYWORD_SET(KF2D__curveFit_opt.fit2D__keep_wholeFit): BEGIN

           fit2DStr.data = CALL_FUNCTION(func, $
                                         fit2DStr.energy, $
                                         ;; SHIFT(fit2DStr.theta,0,shiftTheta), $
                                         fit2DStr.theta, $ ;Assume shiftTheta isn't necessary
                                         fit2DParams, $
                                         UNITS=units, $
                                         MASS=curDataStr.mass)

           ;; fit2DStr.data = KAPPA_FLUX2D__HORSESHOE__ENERGY_ANISOTROPY__COMMON( $
           ;;                 fit2DStr.energy, $
           ;;                 ;; SHIFT(fit2DStr.theta,0,shiftTheta), $
           ;;                 fit2DStr.theta, $ ;Assume shiftTheta isn't necessary
           ;;                 fit2DParams, $
           ;;                 UNITS=units, $
           ;;                 MASS=curDataStr.mass)

           ;; Not necessary for getting the density (but is used in KAPPA_FIT2D__FIRE_EXTRAS
           ;; 
           ;; tmpStr          = CONV_UNITS(fit2DStr,'counts')
           ;; tmpStr.ddata    = (tmpStr.data)^.5
           ;; fit2DStr        = CONV_UNITS(tmpStr,units)

        END
     ENDCASE

     IF N_ELEMENTS(estimated_lc) GT 0 THEN BEGIN
        tmpDensSourceConeRange    = [MIN([KF2D__SDTData_opt.fit2D_dens_aRange[0],estimated_lc[0]]), $
                                     MAX([KF2D__SDTData_opt.fit2D_dens_aRange[1],estimated_lc[1]])]
     ENDIF ELSE BEGIN
        tmpDensSourceConeRange    = KF2D__SDTData_opt.fit2D_dens_aRange
     ENDELSE

     fit_scDens  = CALL_FUNCTION(KF2D__SDTData_opt.densFunc,fit2DStr, $
                                 ;; ENERGY=KF2D__SDTData_opt.energy_electrons, $
                                 ;; ENERGY=eRange_fit, $
                                 ANGLE=tmpDensSourceConeRange)

     fit2DParams[3] = TEMPORARY(fit_scDens)

  ENDIF

END
