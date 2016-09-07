;;09/03/16
PRO UPDATE_KAPPA_FLUX2D__HORSESHOE__BFUNC_AND_GFUNC,curDataStr,fitAngle_i, $
   ;; KAPPAPARAMSTRUCT=kappaParamStruct, $
   ;; GAUSSPARAMSTRUCT=gaussParamStruct, $
   ;; FITPARAMSTRUCT=fitParamStruct, $
   PEAK_ENERGY=peak_energy, $
   NORMALIZE_TO_VALS_AT_FITTED_ANGLE=normalize_to_fitAngle_vals, $
   KCURVEFIT_OPT=kCurvefit_opt, $
   KSDTDATA_OPT=kSDTData_opt, $
   KSTRINGS=kStrings, $
   ITIME=iTime, $
   LOGSCALE_REDUCENEGFAC=logScale_reduceNegFac, $
   PLOT_BULKE_MODEL=plot_bulke_model, $
   PLOT_BULKE_FACTOR=plot_bulke_factor, $
   POLARPLOT_BULKE_FACTOR=polarPlot_bulke_factor, $
   PLOT_MODEL_BULKE_V_DATA_COMPARISON=plot_comparison, $
   PLOT_FLUX_PEAKS=plot_flux_peaks, $
   PLOTDIR=plotDir, $
   ORBIT=orbit, $
   SAVE_PLOTS=save_plots
   
  COMPILE_OPT IDL2

  @common__kappa_flux2d__horseshoe__eanisotropy.pro

  junk =  KAPPA_EFLUX__ANISOTROPY_DIST( $
          curDataStr.energy, $
          curDataStr.theta, $
          curDataStr.data, $
          fitAngle_i, $
          NORMALIZE_TO_VALS_AT_FITTED_ANGLE=normalize_to_fitAngle_vals, $
          ;; BULK_ENERGY=kappaParamStruct[0].value, $
          ;; BULK_ENERGY=kappaParamStruct[0].value, $
          BULK_ENERGY=peak_energy, $
          MIN_ENERGY=kCurvefit_opt.min_peak_energy, $
          REDUCENEGFAC=kCurvefit_opt.fit2D__bulk_e_anis_factor, $
          LOGSCALE_REDUCENEGFAC=logScale_reduceNegFac, $
          PLOT_BULKE_MODEL=plot_bulke_model, $
          PLOT_BULKE_FACTOR=plot_bulke_factor, $
          POLARPLOT_BULKE_FACTOR=polarPlot_bulke_factor, $
          PLOT_MODEL_BULKE_V_DATA_COMPARISON=plot_comparison, $
          PLOT_FLUX_PEAKS=plot_flux_peaks, $
          PLOTDIR=plotDir, $
          ORBIT=orbit, $
          TIME=kStrings.timeFNStrs[iTime], $
          SAVE_PLOTS=save_plots, $
          ;; OUT_PEAK_ENERGIES=peak_en_kappa, $
          ;; OUT_PEAK_FLUXES=peak_flux_kappa, $
          ;; OUT_ANGLES=peak_angle_kappa, $
          OUT_PEAK_ENERGIES=peak_en, $
          OUT_PEAK_FLUXES=peak_flux, $
          OUT_ANGLES=peak_angle, $
          OUT_ANGLE_I=peak_angle_i, $
          PRINT=print)

  ;; K_EA__bFunc_kappa = peak_en_kappa / peak_en_kappa[fitAngle_i]
  ;; K_EA__gFunc_kappa = peak_flux_kappa / peak_flux_kappa[fitAngle_i]

  keepAngles_i  = WHERE(peak_angle GE kSDTData_opt.electron_lca[0] AND $
                        peak_angle LE kSDTData_opt.electron_lca[1])
  
  IF (WHERE(fitAngle_i EQ keepAngles_i))[0] EQ -1 THEN STOP

  K_EA__bFunc   = peak_en  [keepAngles_i]/ peak_en[fitAngle_i]
  K_EA__gFunc   = peak_flux[keepAngles_i]/ peak_flux[fitAngle_i]

  K_EA__angles  = peak_angle  [keepAngles_i]
  K_EA__angle_i = peak_angle_i[keepAngles_i]

  IF KEYWORD_SET(kCurvefit_opt.fit2D__disable_bFunc) THEN BEGIN
     K_EA__bFunc[*] = 0.0
  ENDIF

  ;; IF KEYWORD_SET(kCurvefit_opt.add_gaussian_estimate) THEN BEGIN
  ;;    junk =  KAPPA_EFLUX__ANISOTROPY_DIST( $
  ;;            curDataStr.energy, $
  ;;            curDataStr.theta, $
  ;;            curDataStr.data, $
  ;;            fitAngle_i, $
  ;;            ;; BULK_ENERGY=gaussParamStruct[0].value, $
  ;;            BULK_ENERGY=, $
  ;;            MIN_ENERGY=kCurvefit_opt.min_peak_energy, $
  ;;            REDUCENEGFAC=kCurvefit_opt.fit2D__bulk_e_anis_factor, $
  ;;            LOGSCALE_REDUCENEGFAC=logScale_reduceNegFac, $
  ;;            PLOT_BULKE_MODEL=plot_bulke_model, $
  ;;            PLOT_BULKE_FACTOR=plot_bulke_factor, $
  ;;            POLARPLOT_BULKE_FACTOR=polarPlot_bulke_factor, $
  ;;            PLOT_BULKE_MODEL_V_DATA_COMPARISON=plot_comparison, $
  ;;            PLOT_FLUX_PEAKS=plot_flux_peaks, $
  ;;            PLOTDIR=plotDir, $
  ;;            ORBIT=orbit, $
  ;;            TIME=curDataStr.time, $
  ;;            SAVE_PLOTS=save_plots, $
  ;;            OUT_PEAK_ENERGIES=peak_en_gauss, $
  ;;            OUT_PEAK_FLUXES=peak_flux_gauss, $
  ;;            OUT_ANGLES=peak_angle_gauss, $
  ;;            PRINT=print)

  ;;    K_EA__bFunc_gauss = peak_en_gauss   / peak_en_gauss[fitAngle_i]
  ;;    K_EA__gFunc_gauss = peak_flux_gauss / peak_flux_gauss[fitAngle_i]
  
  ;; ENDIF

END
