;;09/03/16
FUNCTION UPKAP_GET_MIN_ANGLE,sorters,peak_angle,peak_flux,fitAngle_ii,SUCCESS=success

  success       = 0

  thresh        = 0.05

  nHere         = N_ELEMENTS(sorters)

  tmpSort_i     = sorters[SORT(ABS(peak_angle[sorters]))]

  tmpAngle      = ABS(peak_angle[tmpSort_i])
  tmpFPeakNorm  = peak_flux[tmpSort_i]/peak_flux[fitAngle_ii]

  tmpFPeakDiff  = [tmpFPeakNorm[1:-1]-tmpFPeakNorm[0:-2],0]

  checkThese    = WHERE(tmpFPeakNorm LE thresh,nCheck)
  IF nCheck LT 1 THEN BEGIN
     RETURN,-1
  ENDIF

  min_angle     = MIN(tmpAngle[checkThese],ind)

  minStreakToCheckForSlope = 3
  ;; CASE 1 OF
  ;;    ;; ind EQ (nHere-1): BEGIN
     ;; ind GE (nHere-1-minStreakToCheckForSlope): BEGIN
     ;;    posSlopeStreaks = 0
  ;;    END
  ;;    ELSE: BEGIN

  ;;    END
  ;; ENDCASE
  posSlopeStreaks = ind LE (nCheck-1-minStreakToCheckForSlope)

  tmp_inds = [(checkThese[ind]+1):(nHere-1)]

  WHILE posSlopeStreaks DO BEGIN

     lastInd = ind

     ;; tmp_inds = checkThese[(ind+1):(nCheck-1)]
     nTmp     = N_ELEMENTS(tmp_inds)

     IF (MAX(tmpFPeakNorm[tmp_inds],tmptmpMaxInd) GT thresh) AND $
        (tmptmpMaxInd LT (nTmp-1)) $
     THEN BEGIN

        boveMax_i       = tmp_inds[tmptmpMaxInd:(nTmp-1)]

        checkTheseii    = WHERE(tmpFPeakNorm[boveMax_i] LE thresh,nCheck)

        IF nCheck LT 1 THEN BEGIN
           success = 0
           RETURN,-1
        ENDIF

        min_angle     = MIN(tmpAngle[boveMax_i[checkTheseii]],ind)
        ;; ind           = checkThese[tmp_inds[checkTheseii]]

        ind      = boveMax_i[checkTheseii[ind]]
        tmp_inds = [(ind+1):(nHere-1)]

        posSlopeStreaks = (ind LE (nHere-1-minStreakToCheckForSlope)) AND ~(lastInd EQ ind)

     ENDIF ELSE BEGIN
        posSlopeStreaks = 0
     ENDELSE

  ENDWHILE

  success = 1
  
  RETURN,min_angle

END
PRO UPDATE_KAPPA_FLUX2D__HORSESHOE__BFUNC_AND_GFUNC,curDataStr, $
   angleBin_i, $
   fitAngle_i, $
   ESTIMATE_LOSSCONE=estimate_lossCone, $
   PEAK_ENERGY=peak_energy, $
   NORMALIZE_TO_VALS_AT_FITTED_ANGLE=normalize_to_fitAngle_vals, $
   TIME=time, $
   LOGSCALE_REDUCENEGFAC=logScale_reduceNegFac, $
   PLOT_BULKE_MODEL=plot_bulke_model, $
   PLOT_BULKE_FACTOR=plot_bulke_factor, $
   POLARPLOT_BULKE_FACTOR=polarPlot_bulke_factor, $
   PLOT_MODEL_BULKE_V_DATA_COMPARISON=plot_comparison, $
   PLOT_FLUX_PEAKS=plot_flux_peaks, $
   PLOTDIR=plotDir, $
   ORBIT=orbit, $
   OUT_ESTIMATED_LC=estimated_lc, $
   MAKE_PLOTS=make_plots, $
   SAVE_PLOTS=save_plots, $
   EPS=eps
   
  COMPILE_OPT IDL2,STRICTARRSUBS

  @common__kappa_flux2d__horseshoe__eanisotropy.pro
  @common__kappa_fit2d_structs.pro

  IF KEYWORD_SET(save_plots) THEN BEGIN
     IF ~KEYWORD_SET(plotDir) THEN SET_PLOT_DIR,plotDir,/FOR_KAPPA_DB,/ADD_TODAY

     fExt               = KEYWORD_SET(eps) ? '.eps' : '.png'

     orbStr             = KEYWORD_SET(orbit)  ? STRING(FORMAT='("--",I0)',orbit) : ''
     IF KEYWORD_SET(time) THEN BEGIN

        CASE SIZE(time,/TYPE) OF
           7: BEGIN
              timeStr   = time
           END
           ELSE: BEGIN
              timeStr   = STRING(FORMAT='("--",A0)',TIME_TO_STR(time,/MS))
           END
        ENDCASE
     ENDIF

     minEnStr           = KEYWORD_SET(minEn)  ? STRING(FORMAT='("--minEn_",I0)',minEn)  : ''

     plotNames = {plotDir : plotDir ,$
                  bFuncSPName      : 'kappa_anisotropy-bFunc'     +orbStr+timeStr+minEnStr+fExt, $
                  bFuncModelSPName : 'kappa_anisotropy-bFuncModel'+orbStr+timeStr+minEnStr+fExt, $
                  bFuncPolarSPName :'kappa_anisotropy-bFunc_polar'+orbStr+timeStr+minEnStr+fExt, $
                  gFuncSPName      : 'kappa_anisotropy-gFunc'     +orbStr+timeStr+minEnStr+fExt, $
                  bulkVDataSPName  : 'bulkV-data_comparison'      +orbStr+timeStr+minEnStr+fExt}

  ENDIF


  IF KEYWORD_SET(make_plots) THEN BEGIN
     plot_bulke_model = 1
     polarPlot_bulke_factor = 1
     plot_bulke_factor = 1
     plot_flux_peaks = 1
     buffer = 1
  ENDIF

  junk =  KAPPA_EFLUX__ANISOTROPY_DIST( $
          curDataStr.energy, $
          curDataStr.theta, $
          curDataStr.data, $
          angleBin_i, $
          fitAngle_i, $
          NORMALIZE_TO_VALS_AT_FITTED_ANGLE=normalize_to_fitAngle_vals, $
          BULK_ENERGY=peak_energy, $
          MIN_ENERGY=KF2D__Curvefit_opt.min_peak_energy, $
          NENERGY=curDataStr.nEnergy, $
          REDUCENEGFAC=KF2D__Curvefit_opt.fit2D__bulk_e_anis_factor, $
          LOGSCALE_REDUCENEGFAC=logScale_reduceNegFac, $
          DONT_ALLOW_SHIFT_IN_PEAK_ENERGY=KEYWORD_SET(KF2D__Curvefit_opt.fit2D__disable_bFunc), $
          PLOT_BULKE_MODEL=plot_bulke_model, $
          PLOT_BULKE_FACTOR=plot_bulke_factor, $
          POLARPLOT_BULKE_FACTOR=polarPlot_bulke_factor, $
          PLOT_MODEL_BULKE_V_DATA_COMPARISON=plot_comparison, $
          PLOT_FLUX_PEAKS=plot_flux_peaks, $
          BUFFER=buffer, $
          OUT_PEAK_ENERGIES=peak_en, $
          OUT_PEAK_FLUXES=peak_flux, $
          OUT_ANGLES=peak_angle, $
          OUT_ANGLE_I=peak_angle_i, $
          OUT_FITANGLE_II=fitAngle_ii, $
          PRINT=print, $
          SAVE_PLOTS=save_plots, $
          PLOTNAMES=plotNames, $
          EPS=eps)

  K_EA__bFunc   = peak_en   / peak_en[fitAngle_ii]
  K_EA__gFunc   = peak_flux / peak_flux[fitAngle_ii]

  K_EA__angles  = peak_angle  
  K_EA__angle_i = peak_angle_i

  CASE 1 OF
     KEYWORD_SET(KF2D__curveFit_opt.fit2D_only_eAngles): BEGIN
        bro              = KF2D__SDTData_opt.electron_angleRange
        K_EA__fitAngle_i = WHERE((K_EA__angles GE FLOOR(bro[0])) AND $
                                 (K_EA__angles LE CEIL(bro[1])),nAnKeep)
        ;; K_EA__fitAngles  = [MIN(K_EA__angles[K_EA__fitAngle_i]), $
        ;;                     MAX(K_EA__angles[K_EA__fitAngle_i])]
        K_EA__fitAngles  = K_EA__angles[K_EA__fitAngle_i]
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
        nAnKeep          = N_ELEMENTS(K_EA__angles)
        K_EA__fitAngle_i = INDGEN(nAnKeep)
        K_EA__fitAngles  = [curDataStr.nEnergy/2,aRange_i]
        ;; PRINT,'Angles for 2D fit: ALL'
     END
  ENDCASE

  IF KEYWORD_SET(estimate_lossCone) THEN BEGIN

     LT180Ind = WHERE(peak_angle LT 0,nLT180, $
                      COMPLEMENT=GE180Ind, $
                      NCOMPLEMENT=nGE180)

     IF nLT180 LE 1 THEN STOP ELSE BEGIN

        min_negAngle = -1.* UPKAP_GET_MIN_ANGLE(LT180Ind,peak_angle,peak_flux,fitAngle_ii, $
                                               SUCCESS=success)

        IF ~success THEN min_negAngle = KF2D__SDTData_opt.fit2D_dens_aRange[0]

     ENDELSE

     IF nGE180 LE 1 THEN STOP ELSE BEGIN

        min_posAngle = UPKAP_GET_MIN_ANGLE(GE180Ind,peak_angle,peak_flux,fitAngle_ii, $
                                          SUCCESS=success)

        IF ~success THEN min_posAngle = KF2D__SDTData_opt.fit2D_dens_aRange[1]

     ENDELSE

     ;; estimated_lc = [min_negAngle,min_posAngle]

     pickAngle    = ABS(min_negAngle) > min_posAngle
     estimated_lc = [-1.D,1.D]*pickAngle

  ENDIF

  ;; PRINT,"Kappa anisotropy angles: "
  ;; FOR k=0,N_ELEMENTS(K_EA__angles)-1 DO BEGIN
  ;;    PRINT,FORMAT='(I0,T10,F0.2)',K_EA__angle_i[k],K_EA__angles[k]
  ;; ENDFOR

  IF KEYWORD_SET(KF2D__Curvefit_opt.fit2D__disable_bFunc) THEN BEGIN
     K_EA__bFunc[*] = 1.0
  ENDIF

END
