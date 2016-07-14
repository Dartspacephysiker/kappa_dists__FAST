PRO KAPPA_FIT__LOOP__ONE_ANGLE,times,energies,data,oneCount_data,angles, $
                               USING_SDT_DATA=using_sdt_data, $
                               KAPPA=kappa, $
                               BOUNDS=bounds, $
                               EEB_OR_EES=eeb_or_ees, $
                               SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                               ROUTINE=routine, $
                               ESTFACS=estFacs, $
                               TRIM_ENERGIES_BELOW_PEAK=trim_energies_below_peak, $
                               DONT_FIT_BELOW_THRESH_VALUE=dont_fit_below_thresh_value, $
                               N_ENERGIES_BELOW_PEAK=n_below_peak, $
                               N_ENERGIES_AFTER_PEAK=n_after_peak, $
                               ENERGY_ELECTRONS=energy_electrons, $
                               ESTIMATE_A_FROM_DATA=estimate_A_from_data, $
                               E_ANGLE=e_angle, $
                               BULK_OFFSET=bulk_offset, $
                               CHECK_FOR_HIGHER_FLUX_PEAKS=check_for_higher_flux_peaks__set_corresponding_peak_energy, $
                               MIN_PEAK_ENERGY=min_peak_energy, $
                               STRINGS=strings, $
                               FIT_TOLERANCE=fit_tol, $
                               MAX_ITERATIONS=max_iter, $
                               ADD_GAUSSIAN_ESTIMATE=add_gaussian_estimate, $
                               USE_SDT_GAUSSIAN_FIT=use_SDT_Gaussian_fit, $
                               ADD_ONECOUNT_CURVE=add_oneCount_curve, $
                               ADD_FITPARAMS_TEXT=add_fitParams_text, $
                               ADD_ANGLE_LABEL=add_angle_label, $
                               ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
                               FIT_EACH_ANGLE=fit_each_angle, $
                               ELECTRON_ANGLERANGE=electron_angleRange, $
                               NO_PLOTS=no_plots, $
                               SAVE_FITPLOTS=save_fitplots, $
                               PLOT_FULL_FIT=plot_full_fit, $
                               PLOTDIR=plotDir, $
                               OUTPUT_DENSITY_ESTIMATES=output_density_estimates, $
                               OUTPUT_DENSITY__ERANGE=dens_est_eRange, $
                               OUTPUT_DENS__ENERGIES=output_dens__energies, $
                               OUTPUT_DENS__ANGLES=output_dens__angles, $
                               OUT_DENS_STRUCT=out_dens, $
                               OUT_PEAK_DENS_STRUCT=out_peak_dens, $
                               ;; OUT_DENS_FILEPREF=out_dens_filePref, $
                               ONLY_DENS_ESTIMATES=only_dens_estimates, $
                               OUT_FITTED_PARAMS=out_fitted_params, $
                               OUT_FITTED_GAUSS_PARAMS=out_fitted_Gauss_params, $
                               OUT_KAPPA_FIT_STRUCTS=out_kappa_fit_structs, $
                               OUT_GAUSS_FIT_STRUCTS=out_gauss_fit_structs, $
                               ADD_FULL_FITS=add_full_fits, $
                               OUT_ERANGE_PEAK=out_eRange_peak, $
                               OUT_PARAMSTR=out_paramStr, $
                               TXTOUTPUTDIR=txtOutputDir


  COMPILE_OPT idl2

  ;;Loop over provided indices, plot data as well as fit, and optionally save
  IF KEYWORD_SET(add_oneCount_curve) THEN BEGIN
     ;; CASE 1 OF
     ;;    KEYWORD_SET(fit_each_angle): BEGIN
     ;;       oneCount_omniMod    = oneCount_data[bounds,*,*]
     ;;    END
     ;;    ELSE: BEGIN
     oneCount_omniMod    = oneCount_data[*,bounds]
     ;;    END
     ;; ENDCASE
     yMin                      = MIN(oneCount_omniMod[WHERE(oneCount_omniMod GT 0)])
     yMin                      = 10.^(FLOOR(ALOG10(yMin)))
  ENDIF ELSE BEGIN
     yMin                      = MIN(data[WHERE(data GT 0)])
  ENDELSE

  FOR i=0,N_ELEMENTS(bounds)-1 DO BEGIN

     t                         = times[bounds[i]]

     ;; CASE 1 OF
     ;;    KEYWORD_SET(fit_each_angle): BEGIN
     ;;    END
     ;;    ELSE: BEGIN
     Xorig                     = REVERSE(energies[*,bounds[i]])
     Yorig                     = REVERSE(data[*,bounds[i]])
     nEnergies                 = N_ELEMENTS(Xorig)
     ;;    END
     ;; ENDCASE

     KAPPA__GET_PEAK_IND_AND_PEAK_ENERGY,Xorig,Yorig,peak_ind,peak_energy, $
                                         BULK_OFFSET=bulk_offset, $
                                         CHECK_FOR_HIGHER_FLUX_PEAKS=check_for_higher_flux_peaks__set_corresponding_peak_energy, $
                                         MIN_PEAK_ENERGY=min_peak_energy
     
     minEInd                   = (peak_ind - n_below_peak) > 0
     maxEInd                   = (peak_ind + n_after_peak) < nEnergies-1

     IF KEYWORD_SET(dont_fit_below_thresh_value) THEN BEGIN
        
        nAbove                    = nEnergies-1-maxEInd
        killIt                    = WHERE( (Xorig GE peak_energy) AND (Yorig LE 1e5),nStink)
        IF (nAbove GE 4) AND nStink NE 0 THEN BEGIN
           PRINT,'Old nAbove : ' + STRCOMPRESS(nAbove,/REMOVE_ALL)
           PRINT,'Old maxEInd: ' + STRCOMPRESS(maxEInd,/REMOVE_ALL)
           maxEInd                = maxEInd < MIN(killIt)
           PRINT,'New nAbove: ' + STRCOMPRESS(nEnergies-1-maxEInd,/REMOVE_ALL)
           PRINT,'New maxEInd: ' + STRCOMPRESS(maxEInd,/REMOVE_ALL)

        ENDIF
     ENDIF

     ;;Get the data for various purposes
     IF KEYWORD_SET(estimate_A_from_data) OR $
        KEYWORD_SET(output_density_estimates) THEN BEGIN 

        IF KEYWORD_SET(spectra_average_interval) THEN BEGIN
           dat                    = CALL_FUNCTION(routine,t,CALIB=calib,NPTS=spectra_average_interval)
           dat                    = AVERAGE_SUM3D(dat,spectra_average_interval)
        ENDIF ELSE BEGIN
           dat                    = CALL_FUNCTION(routine,t,CALIB=calib)
        ENDELSE

     ENDIF

     ;;estimate from the data!
     IF KEYWORD_SET(estimate_A_from_data) THEN BEGIN 

        KAPPA__GET_A_ESTIMATES,dat,Xorig,Yorig, $
                               minEInd,maxEInd,nEnergies, $
                               peak_ind,peak_energy,eRange_peak, $
                               KAPPA_EST=kappa, $
                               ;; MASS=mass, $
                               E_ANGLE=e_angle, $
                               ANGLES=angles[bounds[i]], $
                               ADD_GAUSSIAN_ESTIMATE=add_gaussian_estimate, $
                               USE_SDT_GAUSSIAN_FIT=use_SDT_Gaussian_fit, $
                               ESTFACS=estFacs, $
                               A_OUT=A, $
                               AGAUSS_OUT=AGauss

     ENDIF ELSE BEGIN
        A                      = DOUBLE([peak_energy,T,kappa,n_est,0.000001,5.68e-6,0])
     ENDELSE
     

     xRange                    = [MIN(Xorig[WHERE(Xorig GT 0)]),MAX(Xorig)]
     yRange                    = [yMin,MAX(data)]

     IF KEYWORD_SET(output_density_estimates) THEN BEGIN
        KAPPA__GET_DENSITY_ESTIMATES,dat, $
                                     OUTPUT_DENS__ANGLES=output_dens__angles, $
                                     OUTPUT_DENS__ENERGIES=output_dens__energies, $
                                     ERANGE_PEAK=eRange_peak, $
                                     DENS_EST_ERANGE=dens_est_eRange, $
                                     STRINGS=strings, $
                                     TXTOUTPUTDIR=txtOutputDir

        IF KEYWORD_SET(only_dens_estimates) THEN CONTINUE
     ENDIF
     
     KAPPA__GET_FITS,Xorig,Yorig, $
                     orig,kappaFit,gaussFit, $
                     ADD_GAUSSIAN_ESTIMATE=add_gaussian_estimate, $
                     USE_SDT_GAUSSIAN_FIT=use_SDT_Gaussian_fit, $
                     BOUNDS_I=bounds[i], $
                     ENERGY_INDS=[minEInd,maxEInd], $
                     ERANGE_PEAK=eRange_peak, $
                     PEAK_IND=peak_ind, $
                     KAPPA_A=A, $
                     GAUSS_A=AGauss, $
                     YMAX=yMax, $
                     MAX_ITER=max_iter, $
                     FIT_TOL=fit_tol, $
                     STRINGS=strings, $
                     TRIM_ENERGIES_BELOW_PEAK=trim_energies_below_peak, $
                     OUT_FITTED_PARAMS=out_fitted_params, $
                     OUT_FITTED_GAUSS_PARAMS=out_fitted_Gauss_params, $
                     OUT_KAPPA_FIT_STRUCTS=out_kappa_fit_structs, $
                     OUT_GAUSS_FIT_STRUCTS=out_gauss_fit_structs, $
                     ADD_FULL_FITS=KEYWORD_SET(add_full_fits) OR KEYWORD_SET(plot_full_fit), $
                     OUT_ERANGE_PEAK=out_eRange_peak, $
                     OUT_PARAMSTR=out_paramStr

     ;;Update yRange based on fits
     yRange[1]                 = yRange[1] > yMax

     IF KEYWORD_SET(add_oneCount_curve) THEN BEGIN
        oneCurve               = {x:Xorig, $
                                  y:REVERSE( oneCount_data[*,bounds[i]] ), $
                                  NAME:"One Count"}
     ENDIF

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Now do plots
     IF ~KEYWORD_SET(no_plots) THEN BEGIN
        PLOT_KAPPA_FITS,orig,kappaFit,gaussFit,oneCurve, $
                        ;; TITLE=title, $
                        BOUNDS_I=bounds[i], $
                        XRANGE=xRange, $
                        YRANGE=yRange, $
                        XLOG=xLog, $
                        YLOG=yLog, $
                        STRINGS=strings, $
                        ADD_FITPARAMS_TEXT=add_fitParams_text, $
                        ADD_ANGLE_LABEL=add_angle_label, $
                        ADD_GAUSSIAN_ESTIMATE=add_gaussian_estimate, $
                        SAVE_FITPLOTS=save_fitPlots, $ ;, $
                        PLOT_FULL_FIT=plot_full_fit, $
                        USING_SDT_DATA=using_sdt_data, $
                        ;; PLOT_SAVENAME=plotSN
                        PLOTDIR=plotDir
     ENDIF
  ENDFOR

END