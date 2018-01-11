;2017/12/27
PRO KAPPA_FIT2D__FIRE_EXTRAS,fit2DStr,curDataStr,hadSuccess, $
                             IN_FIT2D_PARAMS=fit2DParams, $
                             FIT2D_FITINFO=fit2D_info, $
                             ERANGE_PEAK=eRange_peak, $
                             SHIFTTHETA=shiftTheta, $
                             FITANGLE_I=fitAngle_i, $
                             EXTEND_FITSTRUCT_ERANGE=extend_fitStruct_eRange, $
                             UNITS=units, $
                             OPTIONAL__FIT1DINFO=fit1DInfo, $
                             FIT2D__SHOW_AND_PROMPT__EACH_CANDIDATE=show_and_prompt, $
                             FIT2D__SHOW_ONLY_DATA=fit2D__show_only_data, $
                             FIT2D__PA_ZRANGE=fit2D__PA_zRange, $
                             FIT2D__SAVE_ALL_PLOTS=fit2D__save_all_plots, $
                             FIT2D__SHOW__IS_MAXWELLIAN_FIT=is_Maxwellian_fit, $
                             FIT2D__SHOW__FITSTRING=fitString, $
                             PRINT_2DFITINFO=print_2DFitInfo, $
                             TIMEFNSTR=timeFNStr

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;;For updating K_EA__gFunc,K_EA__bFunc
  @common__kappa_flux2d__horseshoe__eanisotropy.pro

  ;;Get yourself KF2D__SDTData_opt,KF2D__Curvefit_opt, etc.
  @common__kappa_fit2d_structs.pro

  ;; IF KEYWORD_SET(extend_fitStruct_eRange) THEN BEGIN
     
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
     
  ;; ENDIF

  CASE 1 OF
     KEYWORD_SET(KF2D__curveFit_opt.fit2D__keep_wholeFit): BEGIN
        fit2DStr.data = KAPPA_FLUX2D__HORSESHOE__ENERGY_ANISOTROPY__COMMON( $
                        fit2DStr.energy, $
                        SHIFT(fit2DStr.theta,0,shiftTheta), $
                        fit2DParams, $
                        UNITS=units, $
                        MASS=curDataStr.mass)

        tmpStr          = CONV_UNITS(fit2DStr,'counts')
        tmpStr.ddata    = (tmpStr.data)^.5
        fit2DStr        = CONV_UNITS(tmpStr,units)

        ;; A means of checking what we go
        ;; FOR k=0,fit2DStr.nenergy-1 DO BEGIN
        ;;    PRINT,FORMAT='(A0,F0.2,A0)','*********** ',fit2DStr.energy[k,0],' eV *************'
        ;;    PRINT,fit2DStr.data[k,*]
        ;;    PRINT,''
        ;; ENDFOR

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
     tmpDensSourceConeRange    = [MIN([KF2D__SDTData_opt.fit2D_dens_aRange[0],estimated_lc[0]]), $
                                  MAX([KF2D__SDTData_opt.fit2D_dens_aRange[1],estimated_lc[1]])]
     tmpTempSourceConeRange    = [MIN([KF2D__SDTData_opt.fit2D_temp_aRange[0],estimated_lc[0]]), $
                                  MAX([KF2D__SDTData_opt.fit2D_temp_aRange[1],estimated_lc[1]])]
     tmpFaCondSourceConeRange  = [MIN([KF2D__SDTData_opt.fit2D_faCond_aRange[0],estimated_lc[0]]), $
                                  MAX([KF2D__SDTData_opt.fit2D_faCond_aRange[1],estimated_lc[1]])]
  ENDIF ELSE BEGIN
     tmpDensSourceConeRange    = KF2D__SDTData_opt.fit2D_dens_aRange
     tmpTempSourceConeRange    = KF2D__SDTData_opt.fit2D_temp_aRange
     tmpFaCondSourceConeRange  = KF2D__SDTData_opt.fit2D_faCond_aRange
  ENDELSE

  fit_scDens  = CALL_FUNCTION(KF2D__SDTData_opt.densFunc,fit2DStr, $
                              ;; ENERGY=KF2D__SDTData_opt.energy_electrons, $
                              ;; ENERGY=eRange_peak, $
                              ANGLE=tmpDensSourceConeRange)

  fit_scTemp  = (T_2D_FS(fit2DStr, $
                      ;; ENERGY=KF2D__SDTData_opt.energy_electrons, $
                      ;; ENERGY=eRange_peak, $
                      ANGLE=tmpTempSourceConeRange))[KF2D__SDTData_opt.fit2D__temperature_type]

  obs_scDens  = CALL_FUNCTION(KF2D__SDTData_opt.densFunc,curDataStr, $
                              ;; ENERGY=KF2D__SDTData_opt.energy_electrons, $
                              ENERGY=eRange_peak, $
                              ANGLE=tmpDensSourceConeRange)

  obs_scTemp  = (T_2D_FS(curDataStr, $
                      ;; ENERGY=KF2D__SDTData_opt.energy_electrons, $
                      ENERGY=eRange_peak, $
                      ANGLE=tmpTempSourceConeRange))[KF2D__SDTData_opt.fit2D__temperature_type]

  ;;field-aligned conductances
  fFAConduct  = OLSSON_JANHUNEN_1998_EQ_5__FA_CONDUCTANCE_2D_B( $
                fit2DStr, $
                ;; ENERGY=KF2D__SDTData_opt.energy_electrons, $
                ;; ENERGY=eRange_peak, $
                ANGLE=tmpFaCondSourceConeRange)

  oFAConduct  = OLSSON_JANHUNEN_1998_EQ_5__FA_CONDUCTANCE_2D_B( $
                curDataStr, $
                ;; ENERGY=KF2D__SDTData_opt.energy_electrons, $
                ENERGY=eRange_peak, $
                ANGLE=tmpFaCondSourceConeRange)

  NK_EA        = N_ELEMENTS(K_EA__gFunc)
  tmpArr       = MAKE_ARRAY(fit2DStr.NBins,VALUE=-1987.1987,/FLOAT)
  anisotropy   = {gFunc   : tmpArr, $
                  bFunc   : tmpArr, $
                  angles  : tmpArr, $
                  angle_i : tmpArr, $
                  N       : 0L}
  anisotropy.gFunc  [0:NK_EA-1] = K_EA__gFunc
  anisotropy.bFunc  [0:NK_EA-1] = K_EA__bFunc
  anisotropy.angles [0:NK_EA-1] = K_EA__angles
  anisotropy.angle_i[0:NK_EA-1] = K_EA__angle_i
  anisotropy.N                  = NK_EA

  obsMoms = {scDens      : FLOAT(TEMPORARY(obs_scDens)), $
             scTemp      : FLOAT(TEMPORARY(obs_scTemp)), $
             scFAConduct : FLOAT(oFAConduct)}

  fitMoms = {scDens      : FLOAT(TEMPORARY(fit_scDens)), $
             scTemp      : FLOAT(TEMPORARY(fit_scTemp)), $
             SCFAConduct : FLOAT(fFAConduct)}

  extra_info = {estimated_sc : {dens   : FLOAT(tmpDensSourceConeRange  ), $
                                temp   : FLOAT(tmpTempSourceConeRange  ), $
                                faCond : FLOAT(tmpFaCondSourceConeRange)}, $
                anisotropy   : TEMPORARY(anisotropy), $
                fitAngle_i   : fitAngle_i, $
                eRange_peak  : eRange_peak, $
                energy       :  $
                {fit_above_minE   : KF2D__curveFit_opt.fit2D_fit_above_minE, $
                 just_eRange_peak : KF2D__curveFit_opt.fit2d_just_eRange_peak, $
                 only_electrAngles: KF2D__curveFit_opt.fit2D_only_eAngles}}

  doString  = "fit2D_info = CREATE_STRUCT('SDT',TEMPORARY(fit2DStr)," + $
              "'fitParams',fit2DParams,fit2D_info,"

  IF SIZE(fit1DInfo,/TYPE) EQ 8 THEN BEGIN
     doString += "'fit1D',fit1DInfo,"
  ENDIF
  doString += "'obsMoms',TEMPORARY(obsMoms)," + $
              "'fitMoms',TEMPORARY(fitMoms)," + $
              "'extra_info',TEMPORARY(extra_info))"

  good = EXECUTE(doString)
  IF ~good THEN STOP

  ;; fit2D_info   = CREATE_STRUCT( $
  ;;                'SDT'       , TEMPORARY(fit2DStr), $
  ;;                'fitParams' , fit2DParams, $
  ;;                fit2D_info  , $
  ;;                'obsMoms'   ,{scDens   : TEMPORARY(obs_scDens), $
  ;;                              scTemp   : TEMPORARY(obs_scTemp), $
  ;;                              scFAConduct : oFAConduct}, $
  ;;                'fitMoms'   ,{scDens   : TEMPORARY(fit_scDens), $
  ;;                              scTemp   : TEMPORARY(fit_scTemp), $
  ;;                              SCFAConduct : fFAConduct}, $
  ;;                'extra_info',{estimated_sc : tmpSourceConeRange, $
  ;;                              anisotropy   : TEMPORARY(anisotropy), $
  ;;                              fitAngle_i   : fitAngle_i, $
  ;;                              eRange_peak  : eRange_peak, $
  ;;                              energy       :  $
  ;;                              {fit_above_minE   : KF2D__curveFit_opt.fit2D_fit_above_minE, $
  ;;                               just_eRange_peak : KF2D__curveFit_opt.fit2d_just_eRange_peak, $
  ;;                               only_electrAngles: KF2D__curveFit_opt.fit2D_only_eAngles}})

  ;; IF KEYWORD_SET(print_2DFitInfo) THEN BEGIN

  ;;    PRINT,''
  ;;    PRINT,'******************************'
  ;;    PRINT,FORMAT='("WINNER ",A0)',fitString
  ;;    PRINT,''
  ;;    PRINT_KAPPA_FLUX2D_HORSESHOE_PARAMS,fit2DParams,fit2D_info.chi2/(fit2D_info.dof-fit2D_info.nPegged)
  ;;    PRINT,'******************************'

  ;; ENDIF

  IF KEYWORD_SET(print_2DFitInfo) AND hadSuccess THEN BEGIN

     tmpParams    = fit2D_info.fitParams
     tmpParams[3] = fit2D_info.obsMoms.scDens

     ;; PRINT,kfitparamstruct[*].value ;Diagnostic kind

     PRINT_KAPPA_FLUX_FIT_PARAMS,tmpParams, $
                                 fit2D_info.chi2/(fit2D_info.dof-fit2D_info.nPegged), $
                                 IS_MAXWELLIAN_FIT=is_Maxwellian_fit

  ENDIF

  IF KEYWORD_SET(show_and_prompt) AND ~KEYWORD_SET(fit2D__show_only_data) THEN BEGIN

     ;; tmp2DInfoStruct = {bestFitStr      :fit2D_info.SDT, $
     ;;                    bestFitParams   :fit2DParams  , $
     ;;                    fitAngle_i      :fitAngle_i   , $
     ;;                    bestDens        :fit2D_info.fitMoms.scDens, $
     ;;                    bestredChi2     :fit2D_info.chi2/(fit2D_info.dof-fit2D_info.nPegged), $
     ;;                    eRange_peak     :fit2D_info.extra_info.eRange_peak}

     KAPPA_FIT2D__SHOW_AND_PROMPT__EACH_CANDIDATE,curDataStr, $
        fit2D_info, $
        TIMEFNSTR=timeFNStr, $
        /FOR_HORSESHOE_FIT, $
        IS_MAXWELLIAN_FIT=is_Maxwellian_fit, $
        PROMPT__CONT_TO_NEXT_FIT=prompt__cont_to_next_fit, $
        PROMPT__CONT_UNTIL_FIT_EQ=prompt__cont_until_fit_eq, $
        PROMPT__NTOT2DFITS=iWin, $
        FINISH_AND_SAVE_ALL=KEYWORD_SET(fit2D__save_all_plots), $
        KAPPA_FIT__SHOW__QUIT=show__quit, $
        FIT2D__PA_ZRANGE=fit2D__PA_zRange, $
        EPS=eps

  ENDIF

END
