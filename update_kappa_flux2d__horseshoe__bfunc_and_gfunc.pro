;;09/03/16
PRO UPDATE_KAPPA_FLUX2D__HORSESHOE__BFUNC_AND_GFUNC,curDataStr, $
   angleBin_i, $
   fitAngle_i, $
   PEAK_ENERGY=peak_energy, $
   NORMALIZE_TO_VALS_AT_FITTED_ANGLE=normalize_to_fitAngle_vals, $
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
   
  COMPILE_OPT IDL2,STRICTARRSUBS

  @common__kappa_flux2d__horseshoe__eanisotropy.pro
  @common__kappa_fit2d_structs.pro

  junk =  KAPPA_EFLUX__ANISOTROPY_DIST( $
          curDataStr.energy, $
          curDataStr.theta, $
          curDataStr.data, $
          angleBin_i, $
          fitAngle_i, $
          NORMALIZE_TO_VALS_AT_FITTED_ANGLE=normalize_to_fitAngle_vals, $
          BULK_ENERGY=peak_energy, $
          MIN_ENERGY=KF2D__Curvefit_opt.min_peak_energy, $
          REDUCENEGFAC=KF2D__Curvefit_opt.fit2D__bulk_e_anis_factor, $
          LOGSCALE_REDUCENEGFAC=logScale_reduceNegFac, $
          PLOT_BULKE_MODEL=plot_bulke_model, $
          PLOT_BULKE_FACTOR=plot_bulke_factor, $
          POLARPLOT_BULKE_FACTOR=polarPlot_bulke_factor, $
          PLOT_MODEL_BULKE_V_DATA_COMPARISON=plot_comparison, $
          PLOT_FLUX_PEAKS=plot_flux_peaks, $
          PLOTDIR=plotDir, $
          ORBIT=orbit, $
          TIME=KF2D__strings.timeFNStrs[iTime], $
          SAVE_PLOTS=save_plots, $
          DONT_ALLOW_SHIFT_IN_PEAK_ENERGY=KEYWORD_SET(KF2D__Curvefit_opt.fit2D__disable_bFunc), $
          OUT_PEAK_ENERGIES=peak_en, $
          OUT_PEAK_FLUXES=peak_flux, $
          OUT_ANGLES=peak_angle, $
          OUT_ANGLE_I=peak_angle_i, $
          OUT_FITANGLE_II=fitAngle_ii, $
          PRINT=print)

  K_EA__bFunc   = peak_en   / peak_en[fitAngle_ii]
  K_EA__gFunc   = peak_flux / peak_flux[fitAngle_ii]

  K_EA__angles  = peak_angle  
  K_EA__angle_i = peak_angle_i

  ;; PRINT,"Kappa anisotropy angles: "
  ;; FOR k=0,N_ELEMENTS(K_EA__angles)-1 DO BEGIN
  ;;    PRINT,FORMAT='(I0,T10,F0.2)',K_EA__angle_i[k],K_EA__angles[k]
  ;; ENDFOR

  IF KEYWORD_SET(KF2D__Curvefit_opt.fit2D__disable_bFunc) THEN BEGIN
     K_EA__bFunc[*] = 1.0
  ENDIF

END
