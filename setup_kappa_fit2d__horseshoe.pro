PRO SETUP_KAPPA_FIT2D__HORSESHOE, $
   eRange_fit, $
   curDataStr, $
   wtsForFit,X2D,Y2D,dataToFit, $
   fa, $
   IS_MAXWELLIAN_FIT=is_maxwellian_fit, $
   UNITS=units, $
   ;; MASS=mass, $
   ;; IN_ESTIMATED_LC=estimated_lc, $
   OUT_FIT2D_DENS_ANGLEINFO=fit2D_dens_angleInfo
  
  COMPILE_OPT IDL2,STRICTARRSUBS

  @common__kappa_fit2d_structs.pro

  ;;Uhh, use data for fit weighting. Not fit data.
  wts                = curDataStr.ddata
  wts[*]             = 0.0D
  nz_i               = WHERE(curDataStr.ddata GT 0,/NULL)
  CASE KF2D__curveFit_opt.fit2D__weighting OF
     1: BEGIN
        wts[nz_i]    = 1.D/ABS(curDataStr.ddata[nz_i])
     END
     2: BEGIN
        wts[nz_i]    = 1.D/(curDataStr.ddata[nz_i])^2
     END
  ENDCASE
  ;; wts[nz_i]          = 1.D/(curDataStr.ddata[nz_i])

  CASE 1 OF
     KEYWORD_SET(KF2D__curveFit_opt.fit2d_just_eRange_fit): BEGIN
        eRange_i        = WHERE((curDataStr.energy[*,curDataStr.nBins/2] GE eRange_fit[0]) AND $
                                (curDataStr.energy[*,curDataStr.nBins/2] LE eRange_fit[1]),nEnKeep)
        IF nEnKeep EQ 0 THEN STOP
     END
     KEYWORD_SET(KF2D__curveFit_opt.fit2D_fit_above_minE): BEGIN
        eRange_i        = WHERE(curDataStr.energy[*,curDataStr.nBins/2] GE $
                                KF2D__curveFit_opt.min_peak_energy,nEnKeep)
        IF nEnKeep EQ 0 THEN STOP
     END
     ELSE: BEGIN
        nEnKeep         = N_ELEMENTS(curDataStr.energy[*,curDataStr.nBins/2])
        eRange_i        = INDGEN(nEnKeep)
     END
  ENDCASE

  CASE 1 OF
     KEYWORD_SET(KF2D__curveFit_opt.fit2D_only_eAngles): BEGIN
        bro         = KF2D__SDTData_opt.electron_angleRange
        aRange_i    = WHERE((curDataStr.theta[curDataStr.nEnergy/2,*] GE bro[0]) AND $
                            (curDataStr.theta[curDataStr.nEnergy/2,*] LE bro[1]),nAnKeep)
        aRange      = [MIN(curDataStr.theta[curDataStr.nEnergy/2,aRange_i]), $
                       MAX(curDataStr.theta[curDataStr.nEnergy/2,aRange_i])]
     END
     ;; KEYWORD_SET(KF2D__curveFit_opt.fit2d__exclude_lca_from_densCalc): BEGIN
     ;;    bro         = KF2D__SDTData_opt.electron_lca
     ;;    IF bro[0] LT bro[1] THEN BEGIN
     ;;       aRange_i = WHERE((curDataStr.theta[curDataStr.nEnergy/2,*] LE bro[0]) OR $
     ;;                        (curDataStr.theta[curDataStr.nEnergy/2,*] GE bro[1]),nAnKeep)
     ;;    ENDIF ELSE BEGIN
     ;;       aRange_i = WHERE((curDataStr.theta[curDataStr.nEnergy/2,*] LE bro[0]) AND $
     ;;                        (curDataStr.theta[curDataStr.nEnergy/2,*] GE bro[1]),nAnKeep)
     ;;    ENDELSE
     ;;    ;; PRINT,"Angles for 2D fit: ",curDataStr.theta[curDataStr.nEnergy/2,aRange_i]
     ;;    IF nAnKeep EQ 0 THEN STOP
     ;; END
     ;; KEYWORD_SET(KF2D__curveFit_opt.fit2D_fit_above_minE): BEGIN
     ;;    aRange_i        = WHERE(curDataStr.energy[*,curDataStr.nBins/2] GE $
     ;;                            KF2D__curveFit_opt.min_peak_energy,nEnKeep)
     ;;    IF nEnKeep EQ 0 THEN STOP
     ;; END
     ELSE: BEGIN
        nAnKeep         = N_ELEMENTS(curDataStr.theta[curDataStr.nEnergy/2,*])
        aRange_i        = INDGEN(nAnKeep)
        aRange          = curDataStr.theta[curDataStr.nEnergy/2,aRange_i]
        ;; PRINT,'Angles for 2D fit: ALL'
     END
  ENDCASE

  ;;First drop some energies
  X2D                = curDataStr.energy[eRange_i,*]
  Y2D                = curDataStr.theta [eRange_i,*]
  dataToFit          = curDataStr.data  [eRange_i,*]
  wtsForFit          = wts              [eRange_i,*]

  ;;Now drop some angles
  X2D                = X2D      [*,aRange_i]
  Y2D                = Y2D      [*,aRange_i]
  dataToFit          = dataToFit[*,aRange_i]
  wtsForFit          = wtsForFit[*,aRange_i]

  ;; angle_i            = INDGEN(curDataStr.nBins)

  fit2D_dens_angleInfo = {angle_i:TEMPORARY(aRange_i), $
                          aRange:TEMPORARY(aRange), $
                          ;; nAKeep:nAKeep, $
                          ;; remAngle_i:remAngle_i, $
                          ;; nARem:nARem, $
                          type:'a good type'}


  ;; fa  = {mu_0              : COS(KF2D__SDTData_opt.electron_lca/180.*!PI), $
  ;; fa  = {mu_0              : COS(MAX([ABS(estimated_lc),ABS(KF2D__SDTData_opt.fit2D_dens_aRange)])/180.*!PI), $
  IF N_ELEMENTS(estimated_lc) GT 0 THEN BEGIN
     angler = MAX([ABS(estimated_lc),ABS(KF2D__SDTData_opt.fit2D_dens_aRange)])
  ENDIF ELSE BEGIN
     angler = KF2D__SDTData_opt.fit2D_dens_aRange
  ENDELSE
  
  fa  = {mu_0              : COS(angler/180.*!PI), $
         ;; Bingham_style     : 1, $ ;not being used as of 2017/12/28 in KAPPA_FLUX2D__HORSESHOE__ENERGY_ANISOTROPY__COMMON
         is_maxwellian_fit : KEYWORD_SET(is_maxwellian_fit), $
         units             : units, $
         mass              : curDataStr.mass}

END
