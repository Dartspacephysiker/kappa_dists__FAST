;2016/09/02
PRO KAPPA_FIT2D__HORSESHOE,keep_iTime,iTime, $
                           out_eRange_peak, $
                           nEnergies, $
                           nTotAngles, $
                           successes, $
                           ;; curFitStr,fits,curDataStr, $
                           fit2DStr,fits,curDataStr, $
                           fitAngle_i, $
                           hadSuccess, $
                           ;; IS_MAXWELLIAN_FIT=is_maxwellian_fit, $
                           KFITPARAMSTRUCT=kFitParamStruct, $
                           KFIT2DPARAMSTRUCT=kFit2DParamStruct, $
                           FIT2D_INF_LIST=fit2D_inf_list, $
                           FIT2D__SHOW_AND_PROMPT__EACH_CANDIDATE=show_and_prompt, $
                           FIT2D__SHOW_ONLY_DATA=fit2D__show_only_data, $
                           FIT2D__PA_ZRANGE=fit2D__PA_zRange, $
                           FIT2D__SAVE_ALL_PLOTS=fit2D__save_all_plots, $
                           FIT2D__SHOW__IS_MAXWELLIAN_FIT=is_Maxwellian_fit, $
                           FIT2D__SHOW__FITSTRING=fitString, $
                           PRINT_2DFITINFO=print_2DFitInfo, $
                           PRINT_2DWININFO=print_2DWinInfo, $
                           IN_ESTIMATED_LC=estimated_lc, $
                           EXTEND_FITSTRUCT_ERANGE=extend_fitStruct_eRange, $
                           UNITS=units, $
                           EPS=eps

  COMPILE_OPT IDL2,STRICTARRSUBS

  @common__kappa_flux2d__horseshoe__eanisotropy.pro
  @common__kappa_fit2d_structs.pro

  IF KEYWORD_SET(fit2D__save_all_plots) THEN finish_and_save_all = 1

  OKStatus     = [1,2,3,4]      ;These are all the acceptable outcomes of fitting with MPFIT2DFUN
  shiftTheta   = -1             ;Not currently clear why the shift is necessary, but it makes things come out right

  eRange_peak  = out_eRange_peak[*,-1]

  SETUP_KAPPA_FIT2D__HORSESHOE_TEST, $
     eRange_peak, $
     nEnergies, $
     nTotAngles, $
     ;; curFitStr,curDataStr, $
     fit2DStr,curDataStr, $
     wtsForFit,X2D,Y2D,dataToFit, $
     fa, $
     IS_MAXWELLIAN_FIT=is_maxwellian_fit, $
     ITIME=iTime, $
     UNITS=units, $
     IN_ESTIMATED_LC=estimated_lc, $
     OUT_FIT2D_DENS_ANGLEINFO=fit2D_dens_angleInfo
  
  IF STRUPCASE(kFitParamStruct[1].parName) EQ 'T' THEN BEGIN
     kFitParamStruct[1].fixed = KF2D__curveFit_opt.fit2D__clampTemperature
     kFitParamStruct[3].fixed = KF2D__curveFit_opt.fit2D__clampDensity
  ENDIF ELSE BEGIN
     STOP
  ENDELSE

  CASE 1 OF
     KEYWORD_SET(KF2D__curveFit_opt.fit2D__bulk_e_anisotropy): BEGIN
        func   = 'KAPPA_FLUX2D__HORSESHOE__ENERGY_ANISOTROPY__COMMON'

        ;; fitAngle_i = 0.0       ;As we learn later
        
        ;; func   = 'KAPPA_FLUX2D__HORSESHOE__ENERGY_ANISOTROPY'

        ;; factor =   KAPPA_EFLUX__ANISOTROPY_DIST( $
        ;;            curDataStr.energy, $
        ;;            curDataStr.theta, $
        ;;            curDataStr.data, $
        ;;            fitAngle_i, $
        ;;            BULK_ENERGY=kFitParamStruct[0].value, $
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


  ;; FOR kDog=0,N_ELEMENTS(kFitParamStruct)-1 DO BEGIN

  fit2DParams  = MPFIT2DFUN(func,X2D,Y2D,dataToFit, $
                            err, $
                            WEIGHTS=wtsForFit, $
                            FUNCTARGS=fa, $
                            BESTNORM=bestNorm, $
                            NFEV=nfev, $
                            ;; FTOL=KF2D__curveFit_opt.fit2d_tol, $
                            FTOL=KF2D__curveFit_opt.fit2D_tol, $
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
                            COVAR=covar, $
                            PERROR=pError, $
                            MAXITER=KF2D__curveFit_opt.fit2d_max_iter, $
                            NITER=nIter, $
                            YFIT=yFit, $
                            /QUIET, $
                            ERRMSG=errMsg, $
                            _EXTRA=extra)

  nFit            = N_ELEMENTS(yFit)

  IF (WHERE(status EQ OKStatus))[0] NE -1 THEN BEGIN

     ;;Info?
     nSuccess     = 1

  ENDIF ELSE BEGIN

     IF KEYWORD_SET(print_2DFitInfo) THEN BEGIN

        PRINT,fitString + ' 2DFit failure ...'

     ENDIF

     nSuccess     = 0

  ENDELSE

  ;; fit2DStr        = curFitStr

  IF KEYWORD_SET(extend_fitStruct_eRange) THEN BEGIN
     
     ;; energyStep = fit2DStr.energy[0,*]/fit2DStr.energy[1,*]

     ;; tmpEnergy = [[fit2DStr.energy[0,*]]*energyStep^3,[fit2DStr.energy[0,*]]*energyStep^2,[fit2DStr.energy[0,*]]*energyStep,fit2DStr.energy]
     ;; tmpdEnergy = [[tmpEnergy[0,*]]*0.0,tmpEnergy[0:-2,*]-tmpEnergy[1:-1,*]]

     ;; tmpEff = [0,0,0,fit2DStr.eff]
     ;; tmpTheta  = [[fit2DStr.theta[0,*]],[fit2DStr.theta[0,*]],[fit2DStr.theta[0,*]],fit2DStr.theta]
     ;; tmpGeom  = [[fit2DStr.geom[0,*]],[fit2DStr.geom[0,*]],[fit2DStr.geom[0,*]],fit2DStr.geom]

     ;; STR_ELEMENT,fit2DStr,'data',tmpEnergy*0.0,/ADD_REPLACE
     ;; STR_ELEMENT,fit2DStr,'ddata',tmpEnergy*0.0,/ADD_REPLACE
     ;; STR_ELEMENT,fit2DStr,'energy',TEMPORARY(tmpEnergy),/ADD_REPLACE
     ;; STR_ELEMENT,fit2DStr,'denergy',TEMPORARY(tmpDEnergy),/ADD_REPLACE
     ;; STR_ELEMENT,fit2DStr,'theta',TEMPORARY(tmpTheta),/ADD_REPLACE
     ;; STR_ELEMENT,fit2DStr,'geom',TEMPORARY(tmpGeom),/ADD_REPLACE
     ;; STR_ELEMENT,fit2DStr,'eff',TEMPORARY(tmpEff),/ADD_REPLACE

     ;; fit2DStr.nEnergy = 51
     
  ENDIF

  CASE 1 OF
     KEYWORD_SET(kF2D__curveFit_opt.fit2D__keep_wholeFit): BEGIN
        ;; fit2DStr.data = KAPPA_FLUX2D__HORSESHOE__ENERGY_ANISOTROPY__COMMON(fit2DStr.energy,fit2DStr.theta,fit2DParams)
        fit2DStr.data = KAPPA_FLUX2D__HORSESHOE__ENERGY_ANISOTROPY__COMMON(fit2DStr.energy, $
                                                                           SHIFT(fit2DStr.theta,0,shiftTheta), $
                                                                           fit2DParams, $
                                                                           UNITS=units, $
                                                                           MASS=curDataStr.mass)
     END
     ;; KEYWORD_SET(KF2D__curveFit_opt.fit2d_just_eRange_peak): BEGIN
     ;;    oldfit2DStr = fit2DStr
     ;;    FOR m=0,N_ELEMENTS(yFit[*,0])-1 DO BEGIN
     ;;       fit2DStr.data[eRange_i[m],fit2D_dens_angleInfo.angle_i] = yFit[m,*]
     ;;    ENDFOR
     ;; END
     ;; KEYWORD_SET(KF2D__curveFit_opt.fit2D_fit_above_minE): BEGIN
     ;;    ;; oldfit2DStr = fit2DStr
     ;;    FOR m=0,N_ELEMENTS(yFit[*,0])-1 DO BEGIN
     ;;       fit2DStr.data[eRange_i[m],fit2D_dens_angleInfo.angle_i] = yFit[m,*]
     ;;    ENDFOR
     ;; END
     ;; ELSE: BEGIN
     ;;    fit2DStr.data[*,fit2D_dens_angleInfo.angle_i]  = yFit
     ;; END
  ENDCASE

  IF N_ELEMENTS(estimated_lc) GT 0 THEN BEGIN
     tmpSourceConeRange= [MIN([KF2D__SDTData_opt.fit2D_dens_aRange[0],estimated_lc[0]]), $
                          MAX([KF2D__SDTData_opt.fit2D_dens_aRange[1],estimated_lc[1]])]
  ENDIF ELSE BEGIN
     tmpSourceConeRange= KF2D__SDTData_opt.fit2D_dens_aRange
  ENDELSE

  ;; tmpSourceConeRange= [MIN([KF2D__SDTData_opt.fit2D_dens_aRange[0],estimated_lc[0]]), $
  ;;                      MAX([KF2D__SDTData_opt.fit2D_dens_aRange[1],estimated_lc[1]])]

  fit_scDens         = CALL_FUNCTION(KF2D__SDTData_opt.densFunc,fit2DStr, $
                                 ;; ENERGY=KF2D__SDTData_opt.energy_electrons, $
                                 ;; ENERGY=out_eRange_peak, $
                                 ANGLE=tmpSourceConeRange)

  fit_scTemp         = (T_2D(fit2DStr, $
                            ;; ENERGY=KF2D__SDTData_opt.energy_electrons, $
                            ;; ENERGY=out_eRange_peak, $
                            ANGLE=tmpSourceConeRange))[3]

  obs_scDens         = CALL_FUNCTION(KF2D__SDTData_opt.densFunc,curDataStr, $
                                 ;; ENERGY=KF2D__SDTData_opt.energy_electrons, $
                                 ENERGY=eRange_peak, $
                                 ANGLE=tmpSourceConeRange)

  obs_scTemp         = (T_2D(curDataStr, $
                            ;; ENERGY=KF2D__SDTData_opt.energy_electrons, $
                            ENERGY=eRange_peak, $
                            ANGLE=tmpSourceConeRange))[3]

  ;;field-aligned conductances
  fFAConduct        = OLSSON_JANHUNEN_1998_EQ_5__FA_CONDUCTANCE_2D_B( $
                      fit2DStr, $
                      ;; ENERGY=KF2D__SDTData_opt.energy_electrons, $
                      ;; ENERGY=eRange_peak, $
                      ANGLE=tmpSourceConeRange)

  oFAConduct        = OLSSON_JANHUNEN_1998_EQ_5__FA_CONDUCTANCE_2D_B( $
                      curDataStr, $
                      ;; ENERGY=KF2D__SDTData_opt.energy_electrons, $
                      ENERGY=eRange_peak, $
                      ANGLE=tmpSourceConeRange)
  ;; IF iTime GE 14 THEN BEGIN


  ;; ENDIF

  IF KEYWORD_SET(show_and_prompt) AND ~KEYWORD_SET(fit2D__show_only_data) THEN BEGIN
     ;; densEst        = CALL_FUNCTION(KF2D__SDTData_opt.densFunc,fit2DStr, $
     ;;                                ENERGY=KF2D__SDTData_opt.energy_electrons, $
     ;;                                ANGLE=tmpSourceConeRange)

     ;; CASE 1 OF
        ;; KEYWORD_SET(fit2D__show_only_data): BEGIN
        ;;       ;; tmp2DInfoStruct = {bestFitStr      :fit2DStr     , $
        ;;       ;;                    bestFit1DParams :fit2DParams  , $
        ;;       ;;                    fitAngle_i      :fitAngle_i   , $
        ;;       ;;                    bestDens        :obs_scDens    , $
        ;;       ;;                    bestChi2        :bestNorm/(dof-nPegged), $
        ;;       ;;                    eRange_peak     :eRange_peak[*,-1]}

        ;;       KAPPA_FIT2D__SHOW_AND_PROMPT__EACH_CANDIDATE,curDataStr, $
        ;;          tmp2DInfoStruct, $
        ;;          iWin, $
        ;;          iTime, $
        ;;          /FOR_HORSESHOE_FIT, $
        ;;          /ONLY_DATA, $
        ;;          IS_MAXWELLIAN_FIT=is_Maxwellian_fit, $
        ;;          PROMPT__CONT_TO_NEXT_FIT=prompt__cont_to_next_fit, $
        ;;          PROMPT__CONT_UNTIL_FIT_EQ=prompt__cont_until_fit_eq, $
        ;;          FINISH_AND_SAVE_ALL=finish_and_save_all, $
        ;;          KAPPA_FIT__SHOW__QUIT=show__quit, $
        ;;          FIT2D__PA_ZRANGE=fit2D__PA_zRange, $
        ;;          EPS=eps


        ;; END
        ;; ELSE: BEGIN
           IF nSuccess GT 0 THEN BEGIN
              tmp2DInfoStruct = {bestFitStr      :fit2DStr     , $
                                 bestFit1DParams :fit2DParams  , $
                                 fitAngle_i      :fitAngle_i   , $
                                 bestDens        :fit_scDens    , $
                                 bestChi2        :bestNorm/(dof-nPegged), $
                                 eRange_peak     :eRange_peak[*,-1]}

              KAPPA_FIT2D__SHOW_AND_PROMPT__EACH_CANDIDATE,curDataStr, $
                 tmp2DInfoStruct, $
                 iWin, $
                 iTime, $
                 /FOR_HORSESHOE_FIT, $
                 IS_MAXWELLIAN_FIT=is_Maxwellian_fit, $
                 PROMPT__CONT_TO_NEXT_FIT=prompt__cont_to_next_fit, $
                 PROMPT__CONT_UNTIL_FIT_EQ=prompt__cont_until_fit_eq, $
                 FINISH_AND_SAVE_ALL=finish_and_save_all, $
                 KAPPA_FIT__SHOW__QUIT=show__quit, $
                 FIT2D__PA_ZRANGE=fit2D__PA_zRange, $
                 EPS=eps

           ENDIF
     ;;    END
     ;; ENDCASE

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

     tmpKeeper       = {SDT          : fit2DStr   , $
                        fitParams    : fit2DParams, $
                        obsMoms      : {scDens   : TEMPORARY(obs_scDens), $
                                        scTemp   : TEMPORARY(obs_scTemp), $
                                        scFAConduct : oFAConduct}, $
                        fitMoms      : {scDens   : TEMPORARY(fit_scDens), $
                                        scTemp   : TEMPORARY(fit_scTemp), $
                                        SCFAConduct : fFAConduct}, $
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
                        nIter        : nIter      , $
                        moment_info  : {estimated_sc : tmpSourceConeRange, $
                                        obs_eRange   : eRange_peak}}


     fit2D_inf_list.ADD,tmpKeeper                             

     IF KEYWORD_SET(print_2DWinInfo) THEN BEGIN

        PRINT,''
        PRINT,'******************************'
        PRINT,FORMAT='("WINNER (",A0," #",I0,")")',fitString,iTime
        PRINT,''
        PRINT_KAPPA_FLUX2D_HORSESHOE_PARAMS,fit2DParams,bestNorm/(dof-nPegged)
        PRINT,'******************************'

     ENDIF

     IF KEYWORD_SET(print_2DFitInfo) THEN BEGIN

        tmpParams    = tmpKeeper.fitParams
        tmpParams[3] = tmpKeeper.obsMoms.scDens

        ;; PRINT,kfitparamstruct[*].value ;Diagnostic kind

        PRINT_KAPPA_FLUX_FIT_PARAMS,tmpParams,bestNorm/(dof-nPegged), $
                                    IS_MAXWELLIAN_FIT=is_Maxwellian_fit

     ENDIF

     hadSuccess = 1

  ENDIF ELSE BEGIN
     winStruct  = !NULL
     hadSuccess = 0
  ENDELSE

END