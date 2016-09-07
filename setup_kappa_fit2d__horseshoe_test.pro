PRO SETUP_KAPPA_FIT2D__HORSESHOE_TEST, $
   out_eRange_peak, $
   nEnergies, $
   nTotAngles, $
   curFitStr,curDataStr, $
   wtsForFit,X2D,Y2D,dataToFit, $
   fa, $
   IS_MAXWELLIAN_FIT=is_maxwellian_fit, $
   ITIME=iTime, $
   KCURVEFIT_OPT=kCurvefit_opt, $
   KFITPARAMSTRUCT=kFitParamStruct, $
   KSDTDATA_OPT=kSDTData_opt, $
   KSTRINGS=kStrings, $
   OUT_FIT2D_DENS_ANGLEINFO=fit2D_dens_angleInfo, $
   OUT_ERANGE_I=eRange_i
  
  COMPILE_OPT idl2

  eRange_peak        = out_eRange_peak[*,-1]

  ;;Uhh, use data for fit weighting. Not fit data.
  wts                = curDataStr.ddata
  wts[*]             = 0.0D
  nz_i               = WHERE(curDataStr.ddata GT 0,/NULL)
  wts[nz_i]          = 1.D/(curDataStr.ddata[nz_i])^2

  CASE 1 OF
     KEYWORD_SET(kCurvefit_opt.fit2d_just_eRange_peak): BEGIN
        eRange_i        = WHERE((curFitStr.energy[*,nTotAngles/2] GE eRange_peak[0]) AND $
                                (curFitStr.energy[*,nTotAngles/2] LE eRange_peak[1]),nEnKeep)
        IF nEnKeep EQ 0 THEN STOP
     END
     KEYWORD_SET(kCurvefit_opt.fit2D_fit_above_minE): BEGIN
        eRange_i        = WHERE(curFitStr.energy[*,nTotAngles/2] GE $
                                kCurvefit_opt.min_peak_energy,nEnKeep)
        IF nEnKeep EQ 0 THEN STOP
     END
     ELSE: BEGIN
        nEnKeep         = N_ELEMENTS(curFitStr.energy[*,nTotAngles/2])
        eRange_i        = INDGEN(nEnKeep)
     END
  ENDCASE

  CASE 1 OF
     KEYWORD_SET(kCurvefit_opt.fit2d__exclude_lca_from_fit): BEGIN
        aRange_i        = WHERE((curFitStr.theta[nEnergies/2,*] GE kSDTData_opt.electron_lca[0]) AND $
                                (curFitStr.theta[nEnergies/2,*] LE kSDTData_opt.electron_lca[1]),nAnKeep)
        IF nAnKeep EQ 0 THEN STOP
     END
     ;; KEYWORD_SET(kCurvefit_opt.fit2D_fit_above_minE): BEGIN
     ;;    aRange_i        = WHERE(curFitStr.energy[*,nTotAngles/2] GE $
     ;;                            kCurvefit_opt.min_peak_energy,nEnKeep)
     ;;    IF nEnKeep EQ 0 THEN STOP
     ;; END
     ELSE: BEGIN
        nAnKeep         = N_ELEMENTS(curFitStr.theta[nEnergies/2,*])
        aRange_i        = INDGEN(nAnKeep)
     END
  ENDCASE

  ;;First drop some energies
  X2D                = curFitStr.energy[eRange_i,*]
  Y2D                = curFitStr.theta [eRange_i,*]
  dataToFit          = curDataStr.data [eRange_i,*]
  wtsForFit          = wts             [eRange_i,*]

  ;;Now drop some angles
  X2D                = X2D      [*,aRange_i]
  Y2D                = Y2D      [*,aRange_i]
  dataToFit          = dataToFit[*,aRange_i]
  wtsForFit          = wtsForFit[*,aRange_i]

  ;; angle_i            = INDGEN(nTotAngles)

  fit2D_dens_angleInfo = {angle_i:aRange_i, $
                          ;; nAKeep:nAKeep, $
                          ;; remAngle_i:remAngle_i, $
                          ;; nARem:nARem, $
                          type:'a good type'}


  fa                 = {mu_0              : COS(kSDTData_opt.electron_lca/180.*!PI), $
                        Bingham_style     : 1, $
                        is_maxwellian_fit : KEYWORD_SET(is_maxwellian_fit)}

END