;2017/12/27
PRO KAPPA_FIT2D__FIRE_EXTRAS,fit2DStr,curDataStr,hadSuccess, $
                             IN_FIT2D_PARAMS=fit2DParams, $
                             FIT2D_FITINFO=fit2D_info, $
                             ERANGE_PEAK=eRange_peak, $
                             SHIFTTHETA=shiftTheta, $
                             FITANGLE_I=fitAngle_i, $
                             EXTEND_FITSTRUCT_ERANGE=extend_fitStruct_eRange, $
                             UNITS=units, $
                             FIT2D__SHOW_AND_PROMPT__EACH_CANDIDATE=show_and_prompt, $
                             FIT2D__SHOW_ONLY_DATA=fit2D__show_only_data, $
                             FIT2D__PA_ZRANGE=fit2D__PA_zRange, $
                             FIT2D__SAVE_ALL_PLOTS=fit2D__save_all_plots, $
                             FIT2D__SHOW__IS_MAXWELLIAN_FIT=is_Maxwellian_fit, $
                             FIT2D__SHOW__FITSTRING=fitString, $
                             PRINT_2DFITINFO=print_2DFitInfo

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

  ;; IF hadSuccess THEN BEGIN
     
     IF N_ELEMENTS(estimated_lc) GT 0 THEN BEGIN
        tmpSourceConeRange= LONG([MIN([KF2D__SDTData_opt.fit2D_dens_aRange[0],estimated_lc[0]]), $
                                  MAX([KF2D__SDTData_opt.fit2D_dens_aRange[1],estimated_lc[1]])])
     ENDIF ELSE BEGIN
        tmpSourceConeRange= LONG(KF2D__SDTData_opt.fit2D_dens_aRange)
     ENDELSE

     fit_scDens  = CALL_FUNCTION(KF2D__SDTData_opt.densFunc,fit2DStr, $
                                 ;; ENERGY=KF2D__SDTData_opt.energy_electrons, $
                                 ;; ENERGY=eRange_peak, $
                                 ANGLE=tmpSourceConeRange)

     fit_scTemp  = (T_2D(fit2DStr, $
                         ;; ENERGY=KF2D__SDTData_opt.energy_electrons, $
                         ;; ENERGY=eRange_peak, $
                         ANGLE=tmpSourceConeRange))[3]

     obs_scDens  = CALL_FUNCTION(KF2D__SDTData_opt.densFunc,curDataStr, $
                                 ;; ENERGY=KF2D__SDTData_opt.energy_electrons, $
                                 ENERGY=eRange_peak, $
                                 ANGLE=tmpSourceConeRange)

     obs_scTemp  = (T_2D(curDataStr, $
                         ;; ENERGY=KF2D__SDTData_opt.energy_electrons, $
                         ENERGY=eRange_peak, $
                         ANGLE=tmpSourceConeRange))[3]

     ;;field-aligned conductances
     fFAConduct  = OLSSON_JANHUNEN_1998_EQ_5__FA_CONDUCTANCE_2D_B( $
                   fit2DStr, $
                   ;; ENERGY=KF2D__SDTData_opt.energy_electrons, $
                   ;; ENERGY=eRange_peak, $
                   ANGLE=tmpSourceConeRange)

     oFAConduct  = OLSSON_JANHUNEN_1998_EQ_5__FA_CONDUCTANCE_2D_B( $
                   curDataStr, $
                   ;; ENERGY=KF2D__SDTData_opt.energy_electrons, $
                   ENERGY=eRange_peak, $
                   ANGLE=tmpSourceConeRange)

     fit2D_info   = CREATE_STRUCT( $
                    'SDT', fit2DStr   , $
                    'fitParams', fit2DParams, $
                    fit2D_info, $
                    'obsMoms',{scDens   : TEMPORARY(obs_scDens), $
                               scTemp   : TEMPORARY(obs_scTemp), $
                               scFAConduct : oFAConduct}, $
                    'fitMoms',{scDens   : TEMPORARY(fit_scDens), $
                               scTemp   : TEMPORARY(fit_scTemp), $
                               SCFAConduct : fFAConduct}, $
                    'extra_info',{estimated_sc : tmpSourceConeRange, $
                                  anisotropy   : {gFunc : K_EA__gFunc, $
                                                  bFunc : K_EA__bFunc, $
                                                  angles : K_EA__angles, $
                                                  angle_i : K_EA__angle_i}, $
                                  fitAngle_i   : fitAngle_i, $
                                  eRange_peak  : eRange_peak, $
                                  energy       :  $
                                  {fit_above_minE   : KF2D__curveFit_opt.fit2D_fit_above_minE, $
                                   just_eRange_peak : KF2D__curveFit_opt.fit2d_just_eRange_peak, $
                                   only_electrAngles: KF2D__curveFit_opt.fit2D_only_eAngles}})

     ;; IF KEYWORD_SET(print_2DFitInfo) THEN BEGIN

     ;;    PRINT,''
     ;;    PRINT,'******************************'
     ;;    PRINT,FORMAT='("WINNER ",A0)',fitString
     ;;    PRINT,''
     ;;    PRINT_KAPPA_FLUX2D_HORSESHOE_PARAMS,fit2DParams,fit2D_info.chi2/(fit2D_info.dof-fit2D_info.nPegged)
     ;;    PRINT,'******************************'

     ;; ENDIF

     IF KEYWORD_SET(print_2DFitInfo) THEN BEGIN

        tmpParams    = fit2D_info.fitParams
        tmpParams[3] = fit2D_info.obsMoms.scDens

        ;; PRINT,kfitparamstruct[*].value ;Diagnostic kind

        PRINT_KAPPA_FLUX_FIT_PARAMS,tmpParams, $
                                    fit2D_info.chi2/(fit2D_info.dof-fit2D_info.nPegged), $
                                    IS_MAXWELLIAN_FIT=is_Maxwellian_fit

     ENDIF

     IF KEYWORD_SET(show_and_prompt) AND ~KEYWORD_SET(fit2D__show_only_data) THEN BEGIN

        tmp2DInfoStruct = {bestFitStr      :fit2DStr     , $
                           bestFit1DParams :fit2DParams  , $
                           fitAngle_i      :fitAngle_i   , $
                           bestDens        :fit_scDens   , $
                           bestChi2        :bestNorm/(dof-nPegged), $
                           eRange_peak     :eRange_peak}

        KAPPA_FIT2D__SHOW_AND_PROMPT__EACH_CANDIDATE,curDataStr, $
           tmp2DInfoStruct, $
           TIMEFNSTR=timeFNStr, $
           /FOR_HORSESHOE_FIT, $
           IS_MAXWELLIAN_FIT=is_Maxwellian_fit, $
           PROMPT__CONT_TO_NEXT_FIT=prompt__cont_to_next_fit, $
           PROMPT__CONT_UNTIL_FIT_EQ=prompt__cont_until_fit_eq, $
           PROMPT__NTOT2DFITS=iWin, $
           FINISH_AND_SAVE_ALL=finish_and_save_all, $
           KAPPA_FIT__SHOW__QUIT=show__quit, $
           FIT2D__PA_ZRANGE=fit2D__PA_zRange, $
           EPS=eps

     ENDIF

  ;; ENDIF

END
