;+
; NAME:               KAPPA_FIT2D__LOOP
;
; PURPOSE:            Get both kappa and Maxwellian fits of FAST electron ESA data, plot, etc.
;
; CATEGORY:           Kappa stuff
;
; CALLING SEQUENCE:   Gets called by KAPPA_EFLUX_FIT2D
;
; INPUTS:             A million
;
; OPTIONAL INPUTS:    A million
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;
; COMMON BLOCKS:
;
; MODIFICATION HISTORY: 2016/07 Born somewhere in here
;
;-
PRO KAPPA_FIT2D__LOOP,diff_eFlux,times,dEF_oneCount, $
                      KSDTDATA_OPT=kSDTData_opt, $
                      KCURVEFIT_OPT=kCurvefit_opt, $
                      KPLOT_OPT=kPlot_opt, $
                      STRINGS=kStrings, $
                      BOUNDS=bounds, $
                      ESTFACS=estFacs, $
                      DONT_PRINT_ESTIMATES=dont_print_estimates, $
                      DONT_PRINT_FITINFO=dont_print_fitInfo, $
                      E_ANGLE=e_angle, $
                      DONT_TAKE_STOCK_OF_BULKANGLE=dont_take_stock_of_bulkangle, $
                      TREAT_FIELDALIGNED_AS_BULK=treat_fieldaligned_as_bulk, $
                      CHECK_FOR_HIGHER_FLUX_PEAKS=check_for_higher_flux_peaks__set_corresponding_peak_energy, $
                      FIT_EACH__AVERAGE_OVER_ANGLERANGE=fit_each__average_over_angleRange, $
                      FIT_EACH__SYNTH_SDT_STRUCT=synthPackage, $
                      FIT_EACH__SKIP_BAD_FITS=fit_each__skip_bad_fits, $
                      FIT_EACH__MIN_ANGLEFITS_FOR_KEEP=min_anglefits_for_keep, $
                      FIT_EACH__START_FROM_FIELDALIGNED=start_from_fieldaligned, $
                      FIT_EACH__VARY_BULK_ENERGY=vary_bulk_energy, $
                      FIT_EACH__SHOW_AND_PROMPT=fit_each__show_and_prompt, $
                      FIT2D__SHOW_AND_PROMPT__EACH_CANDIDATE=fit2d__show_each_candidate, $
                      FIT_FAIL__USER_PROMPT=fit_fail__user_prompt, $
                      OUT_FITTED_PARAMS=out_kappaParams, $
                      OUT_FITTED_GAUSS_PARAMS=out_gaussParams, $
                      OUT_KAPPA_FIT_STRUCTS=kappaFits, $
                      OUT_GAUSS_FIT_STRUCTS=gaussFits, $
                      OUT_FIT2DKAPPA_INF_LIST=fit2dKappa_inf_list, $
                      OUT_FIT2DGAUSS_INF_LIST=fit2dGauss_inf_list, $
                      OUT_ERANGE_PEAK=out_eRange_peak, $
                      OUT_PARAMSTR=out_paramStr, $
                      TXTOUTPUTDIR=txtOutputDir
  
  COMPILE_OPT idl2

  ;; RESOLVE_ROUTINE,'mpfit2dfun',/IS_FUNCTION

  ;;So the order becomes [angle,energy,time] for each of these arrays
  energies                          = TRANSPOSE(diff_eFlux.energy,[1,0,2])
  data                              = TRANSPOSE(diff_eFlux.data,[1,0,2])
  oneCount_data                     = KEYWORD_SET(kPlot_opt.add_oneCount_curve) ? TRANSPOSE(dEF_oneCount.data,[1,0,2]) : !NULL
  angles                            = TRANSPOSE(diff_eFlux.theta,[1,0,2])

  ;;In order to get back to how things were, just 
  IF KEYWORD_SET(synthPackage) THEN BEGIN
     synthKappa                     = diff_eFlux
     synthKappa.data[*]             = 0.0
     IF KEYWORD_SET(kCurvefit_opt.add_gaussian_estimate) THEN BEGIN
        synthGauss                  = diff_eFlux
        synthGauss.data[*]          = 0.0
     ENDIF
  ENDIF

  IF KEYWORD_SET(kPlot_opt.add_oneCount_curve) THEN BEGIN
     yMin                      = MIN(oneCount_data[WHERE(oneCount_data GT 0)])
     yMin                      = 10.^(FLOOR(ALOG10(yMin)))
  ENDIF ELSE BEGIN
     yMin                      = MIN(data[WHERE(data GT 0)])
  ENDELSE

  
  CASE N_ELEMENTS(kSDTData_opt.electron_angleRange) OF
     0: BEGIN
        tempRange    = [-180,180]
     END
     1: BEGIN
        tempRange    = [(-1.)*ABS(kSDTData_opt.electron_angleRange),ABS(kSDTData_opt.electron_angleRange)]
     END
     2: BEGIN
        tempRange    = kSDTData_opt.electron_angleRange

        CASE 1 OF
           tempRange[0] LT tempRange[1]: BEGIN
              alleyOop = 0
           END
           ELSE: alleyOop = 1
        ENDCASE
        IF tempRange[0] LT -179.99999 AND tempRange[1] GT 179.99999 THEN BEGIN
           tempRange = [0,360]
        ENDIF ELSE BEGIN
           IF tempRange[0] LT -180. THEN tempRange[0] = tempRange[0] + 360.
           IF tempRange[1] LT -180. THEN tempRange[1] = tempRange[1] + 360.
        ENDELSE
     END
  ENDCASE

  keepK_iTime                              = !NULL ;Keep track of which fits get to come home with us
  keepG_iTime                              = !NULL ;Keep track of which fits get to come home with us
  successesK                               = 0
  successesG                               = 0
  totSuccessesK                            = 0
  totSuccessesG                            = 0
  fit2DKappa_inf_list                      = LIST()
  fit2DGauss_inf_list                      = LIST()
  FOR i=0,N_ELEMENTS(bounds)-1 DO BEGIN

     successesK                            = 0
     successesG                            = 0

     iTime                                 = bounds[i]
     t                                     = times[iTime]

     ;;And now the order becomes [angle,energy] for each of these arrays
     XorigArr                              = energies[*,*,iTime]
     YorigArr                              = data[*,*,iTime]
     IF KEYWORD_SET(kPlot_opt.add_oneCount_curve) THEN BEGIN
        oneCountArr                        = oneCount_data[*,*,iTime]
     END
     AorigArr                              = angles[*,*,iTime]
     nEnergies                             = N_ELEMENTS(XorigArr[0,*])
     nTotAngles                            = N_ELEMENTS(XorigArr[*,0])

     IF KEYWORD_SET(kCurvefit_opt.min_peak_energy) THEN BEGIN
        ;; min_peak_ind                       = MIN(WHERE(REVERSE(REFORM(XorigArr[0,*])) GE kCurvefit_opt.min_peak_energy)) 

        ;;Try taking it from the top
        min_peak_ind                       = MAX(WHERE(REFORM(XorigArr[0,*]) GE kCurvefit_opt.min_peak_energy)) 
        IF min_peak_ind EQ -1 THEN BEGIN
           STOP
        ENDIF
     ENDIF ELSE BEGIN
        ;; min_peak_ind                       = 0
        min_peak_ind                       = nEnergies-1
     ENDELSE

     dat                                   = MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(diff_eFlux,iTime)

     ;;Here's an interesting exercise. This should probably be dealt with at some point
     nAnglesPerEnergy = MAKE_ARRAY(nEnergies,/INTEGER)
     FOR k=0,nEnergies-1 DO BEGIN
        nAnglesPerEnergy[k]                = N_ELEMENTS(WHERE((dat.theta[k,*] GE tempRange[0]) AND (dat.theta[k,*] GE tempRange[1])))
     ENDFOR
     junk                                  = MIN(nAnglesPerEnergy,useTheseAnglesIndex)
     tempAllAngles                         = AorigArr[*,useTheseAnglesIndex]

     ;; ;;Just get the angles for this time, and we'll assume the middle of the energy range is representative
     ;; tempAllAngles                         = AorigArr[*,nEnergies/2]


     CASE alleyOop OF
        0: BEGIN
           angleBins                       = tempAllAngles GE tempRange[0] AND tempAllAngles LE tempRange[1]
        END
        1: BEGIN
           angleBins                       = tempAllAngles GE tempRange[0] OR tempAllAngles LE tempRange[1]
        END
        
     ENDCASE

     angleBin_i                            = WHERE(angleBins,nAngles)
     nLoopAngles                           = nAngles ;This needs to be distinguished from nAngles because of the change in sweep direction that occurs when using START_FROM_FIELDALIGNED keyword

     IF KEYWORD_SET(start_from_fieldaligned) THEN BEGIN
        distance_from_lastGood             = 0
        distance_from_lastGood_gauss       = 0
        gotFirst                           = 0

        angleBin_sort_i                    = angleBin_i[SORT(tempAllangles[angleBin_i])] ;;Sort so they're ascending
        fieldAlignedAngle                  = MIN(ABS(tempAllAngles),fieldAligned_i)

        FA_angleBin_ii                     = WHERE(angleBin_sort_i EQ fieldAligned_i)
        IF FA_angleBin_ii NE -1 THEN BEGIN
           FA_angleBin_i                   = angleBin_sort_i[FA_angleBin_ii]
           startPosSweep_i                 = FA_angleBin_ii+1
           startNegSweep_i                 = FA_angleBin_ii-1
        ENDIF

        nLoopAngles++
        
        angle_order                        = [fieldAligned_i, $
                                              anglebin_sort_i[startPosSweep_i:-1], $
                                              fieldAligned_i, $
                                              anglebin_sort_i[startNegSweep_i:0:-1]]
        change_sweep_i                     = (WHERE(angle_order EQ fieldAligned_i))[1]
     ENDIF

     IF KEYWORD_SET(fit_each__average_over_angleRange) THEN BEGIN
        tempY                              = TOTAL(YorigArr[angleBin_i,*],1)/DOUBLE(nAngles)
        FOR iAngle=0,nAngles-1 DO BEGIN
           YorigArr[angleBin_i[iAngle],*]  = tempY
        ENDFOR
     ENDIF

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;See if there's a particular angle the deserves to be called the "bulk angle"
     ;;(i.e., the angle at which most of the electrons appear to be moving)
     threshRatio                           = 3

     ;;Order of dat.data is [energy,angle] when coming from SDT
     ;;In tempDat I'm just making the energies monotonically increase instead of decrease, and I'm including all angles with *
     ;; tempDat                               = (REVERSE(dat.data,1))[min_peak_ind:-1,angleBin_i]
     tempDat                               = dat.data[0:min_peak_ind,angleBin_i]
     angleDist                             = TOTAL(tempDat,1)/TOTAL(tempDat)
     testRatio                             = MAX(angleDist,maxInd_angleDist)/MIN(angleDist,minInd_angleDist)
     CASE 1 OF
        KEYWORD_SET(treat_fieldaligned_as_bulk): BEGIN
           bulkAngle                       = fieldAlignedAngle
           useMe                           = 1
           angleTreatment                  = 'treat_fieldaligned_as_bulk'
        END
        KEYWORD_SET(dont_take_stock_of_bulkangle): BEGIN
           bulkAngle                       = 0.00
           useMe                           = 0S
           angleTreatment                  = 'treat_all_as_bulk'
        END
        ELSE: BEGIN
           bulkAngle                       = AorigArr[maxInd_angleDist,nEnergies/2]
           bulkAngle                       = AorigArr[maxInd_angleDist,useTheseAnglesIndex]
           useMe                           = testRatio GE threshRatio
           angleTreatment                  = 'search_for_angleDist_peak'
        END
     ENDCASE
     angleStr                              = {bulkAngle:bulkAngle, $
                                              useMe:useMe, $
                                              dist:angleDist, $
                                              angles:tempAllAngles[angleBin_i], $
                                              SDTAngle:FLOAT(0.0), $
                                              angleTreatment:angleTreatment, $
                                              maxInd:maxInd_angleDist, $
                                              minInd:minInd_angleDist, $
                                              min_peak_ind:min_peak_ind}
     
     nGoodFits_tempK                       = 0
     nGoodFits_tempG                       = 0
     good_angleBinK_i                      = !NULL
     good_angleBinG_i                      = !NULL
     good_kappaFits_i                      = !NULL
     good_gaussFits_i                      = !NULL

     ;;Now loop over each angle and attemp to fit 
     ;;We'll keep all the good fits, and do a comparison among them at the end
     shoutOut                              = 1
     FOR iiAngle=0,nLoopAngles-1 DO BEGIN

        iAngle                        = (KEYWORD_SET(start_from_fieldaligned) ? angle_order : angleBin_i)[iiAngle]
        ;; iAngle                     = angleBin_i[iiAngle]

        ;;Start sweeping backwards, if we made it to swap location
        IF KEYWORD_SET(start_from_fieldaligned) THEN BEGIN
           IF iAngle EQ change_sweep_i THEN BEGIN
              change_sweep_direction  = 1
              iAngle                  = angle_order[iiAngle++]
           ENDIF ELSE BEGIN
              change_sweep_direction  = 0
           ENDELSE
        ENDIF

        ;;Here's the data we're working with for this loop iteration
        ;; Xorig                         = REVERSE(REFORM(XorigArr[iAngle,*]))
        ;; Yorig                         = REVERSE(REFORM(YorigArr[iAngle,*]))
        ;; Aorig                         = REVERSE(REFORM(AorigArr[iAngle,*]))

        Xorig                         = REFORM(XorigArr[iAngle,*])
        Yorig                         = REFORM(YorigArr[iAngle,*])
        Aorig                         = REFORM(AorigArr[iAngle,*])

        tempAngle                     = tempAllAngles[iAngle]
        tempAngleEstRange             = [tempAngle-1.0,tempAngle+1.0]
        angleStr.SDTAngle             = tempAngle

        IF KEYWORD_SET(kPlot_opt.add_oneCount_curve) THEN BEGIN
           oneCurve                   = {x:Xorig, $
                                         ;; y:REVERSE(REFORM(oneCountArr[iAngle,*])), $
                                         y:REFORM(oneCountArr[iAngle,*]), $
                                         NAME:"One Count"}
        ENDIF

        KAPPA__GET_PEAK_IND_AND_PEAK_ENERGY,Xorig,Yorig,peak_ind,peak_energy, $
                                            BULK_OFFSET=kCurvefit_opt.bulk_offset, $
                                            CHECK_FOR_HIGHER_FLUX_PEAKS=check_for_higher_flux_peaks__set_corresponding_peak_energy, $
                                            MIN_PEAK_ENERGY=kCurvefit_opt.min_peak_energy
        
        ;; minEInd                                     = (peak_ind - kCurvefit_opt.n_below_peak) > 0
        ;; maxEInd                                     = (peak_ind + kCurvefit_opt.n_above_peak) < nEnergies-1

        ;;Note that while these are called maxE and minE, suggesting they refer to the max energy and min energy, they do NOT. 
        ;;Rather, they refer to the lowest and highest indices falling within the user-specified parameters for fitting—namely, n_below_peak and n_above_peak
        maxEInd                                     = (peak_ind + kCurvefit_opt.n_below_peak) < nEnergies-1
        minEInd                                     = (peak_ind - kCurvefit_opt.n_above_peak) > 0

        ;; IF KEYWORD_SET(kCurvefit_opt.dont_fit_below_thresh_value) THEN BEGIN
           
        ;;    nAbove                                   = nEnergies-1-maxEInd
        ;;    killIt                                   = WHERE( (Xorig GE peak_energy) AND (Yorig LE 1e5),nStink)
        ;;    IF (nAbove GE 4) AND nStink NE 0 THEN BEGIN
        ;;       maxEInd                               = maxEInd < MIN(killIt)
        ;;    ENDIF
        ;; ENDIF

        ;;estimate from the data!
        IF KEYWORD_SET(kCurvefit_opt.estimate_A_from_data) THEN BEGIN 

           CASE 1 OF
              KEYWORD_SET(start_from_fieldaligned): BEGIN

                 tryNext                            = (iiAngle EQ 0) OR ~gotFirst

                 CASE 1 OF
                    tryNext: BEGIN ;Get field-aligned estimates

                       kappa_fixA                   = [1,1,1,1,0,0,0]
                       gauss_fixA                   = [1,1,0,1,0,0,0]

                       KAPPA__GET_A_ESTIMATES,dat,Xorig,Yorig, $
                                              minEInd,maxEInd,nEnergies, $
                                              peak_ind,peak_energy,eRange_peak, $
                                              KAPPA_EST=kCurvefit_opt.fitA[2], $
                                              ;; MASS=mass, $
                                              E_ANGLE=kSDTData_opt.electron_angleRange, $
                                              ANGLES=tempAngleEstRange, $
                                              N_ANGLES_IN_RANGE=nAngles, $
                                              BULKANGLE_STRUCT=angleStr, $
                                              DONT_TAKE_STOCK_OF_BULKANGLE=dont_take_stock_of_bulkangle, $
                                              ADD_GAUSSIAN_ESTIMATE=kCurvefit_opt.add_gaussian_estimate, $
                                              USE_SDT_GAUSSIAN_FIT=kCurvefit_opt.use_SDT_Gaussian_fit, $
                                              ESTFACS=estFacs, $
                                              A_OUT=A, $
                                              AGAUSS_OUT=AGauss, $
                                              DONT_PRINT_ESTIMATES=dont_print_estimates, $
                                              /TEST_NOREV
                       A_FA_initEst                 = A
                       AGauss_FA_initEst            = AGauss

                    END
                    change_sweep_direction: BEGIN
                       A                            = A_FA_initEst
                       AGauss                       = AGauss_FA_initEst
                    END
                    ELSE: BEGIN
                       A                            = lastGood_A
                       AGauss                       = lastGood_AGauss

                       kappa_fixA                   = [KEYWORD_SET(vary_bulk_energy),1,0,1,0,0,0]
                       gauss_fixA                   = [KEYWORD_SET(vary_bulk_energy),1,0,1,0,0,0]

                    END
                 ENDCASE
              END
              ELSE: BEGIN
                 KAPPA__GET_A_ESTIMATES,dat,Xorig,Yorig, $
                                        minEInd,maxEInd,nEnergies, $
                                        peak_ind,peak_energy,eRange_peak, $
                                        KAPPA_EST=kCurvefit_opt.fitA[2], $
                                        ;; MASS=mass, $
                                        E_ANGLE=kSDTData_opt.electron_angleRange, $
                                        ANGLES=tempAngleEstRange, $
                                        N_ANGLES_IN_RANGE=nAngles, $
                                        BULKANGLE_STRUCT=angleStr, $
                                        DONT_TAKE_STOCK_OF_BULKANGLE=dont_take_stock_of_bulkangle, $
                                        ADD_GAUSSIAN_ESTIMATE=kCurvefit_opt.add_gaussian_estimate, $
                                        USE_SDT_GAUSSIAN_FIT=kCurvefit_opt.use_SDT_Gaussian_fit, $
                                        ESTFACS=estFacs, $
                                        A_OUT=A, $
                                        AGAUSS_OUT=AGauss, $
                                        DONT_PRINT_ESTIMATES=dont_print_estimates, $
                                        /TEST_NOREV


              END
           ENDCASE

        ENDIF ELSE BEGIN
           A                                        = DOUBLE([peak_energy,T,kappa,n_est,0.000001,5.68e-6,0])
        ENDELSE
        

        xRange                                      = [MIN(Xorig[WHERE(Xorig GT 0)]),MAX(Xorig)]
        yRange                                      = [yMin,MAX(data)]
        energy_inds                                 = [minEInd,maxEInd]
        CASE 1 OF
           KEYWORD_SET(kCurvefit_opt.use_mpFit1D): BEGIN

              IF shoutOut THEN BEGIN
                 PRINT,"Time: " + kStrings.timeStrs[iTime]
                 shoutOut = 0

              ENDIF
              A      = [A[0],A[1],A[2],A[3],A[6]]
              AGauss = [AGauss[0],AGauss[1], $
                        AGauss[2],AGauss[3], $
                        AGauss[6]]

              IF N_ELEMENTS(kappa_fixA) GE 7 THEN BEGIN
                 kappa_fixA = [kappa_fixA[0],kappa_fixA[1], $
                               kappa_fixA[2],kappa_fixA[3], $
                               kappa_fixA[6]]
              ENDIF
              IF N_ELEMENTS(gauss_fixA) GE 7 THEN BEGIN
                 gauss_fixA = [gauss_fixA[0],gauss_fixA[1], $
                               gauss_fixA[2],gauss_fixA[3], $
                               gauss_fixA[6]]
              ENDIF
              KAPPA__GET_FITS__MPFIT1D,Xorig,Yorig, $
                                       orig,kappaFit,gaussFit, $
                                       KCURVEFIT_OPT=kCurvefit_opt, $
                                       ENERGY_INDS=energy_inds, $
                                       ERANGE_PEAK=eRange_peak, $
                                       PEAK_IND=peak_ind, $
                                       BOUNDS_I=iTime, $
                                       KAPPA_A=A, $
                                       GAUSS_A=AGauss, $
                                       KAPPA_FIXA=kappa_fixA, $
                                       GAUSS_FIXA=gauss_fixA, $
                                       YMAX=yMax, $
                                       STRINGS=kStrings, $
                                       OUT_FITTED_PARAMS=out_kappaParams, $
                                       OUT_FITTED_GAUSS_PARAMS=out_gaussParams, $
                                       OUT_KAPPA_FIT_STRUCTS=kappaFits, $
                                       OUT_GAUSS_FIT_STRUCTS=gaussFits, $
                                       /ADD_FULL_FITS, $
                                       ADD_ANGLESTR=angleStr, $
                                       OUT_ERANGE_PEAK=out_eRange_peak, $
                                       OUT_PARAMSTR=out_paramStr, $
                                       ;; DONT_PRINT_ESTIMATES=dont_print_estimates, $
                                       DONT_PRINT_FITINFO=dont_print_fitInfo, $
                                       FIT_FAIL__USER_PROMPT=fit_fail__user_prompt

           END
           ELSE: BEGIN
              KAPPA__GET_FITS,Xorig,Yorig, $
                              orig,kappaFit,gaussFit, $
                              ADD_GAUSSIAN_ESTIMATE=kCurvefit_opt.add_gaussian_estimate, $
                              USE_SDT_GAUSSIAN_FIT=kCurvefit_opt.use_SDT_Gaussian_fit, $
                              BOUNDS_I=iTime, $
                              ENERGY_INDS=energy_inds, $
                              ERANGE_PEAK=eRange_peak, $
                              PEAK_IND=peak_ind, $
                              KAPPA_A=A, $
                              GAUSS_A=AGauss, $
                              KAPPA_FIXA=kappa_fixA, $
                              GAUSS_FIXA=gauss_fixA, $
                              YMAX=yMax, $
                              MAX_ITER=kCurvefit_opt.max_iter, $
                              FIT_TOL=kCurvefit_opt.fit_tol, $
                              STRINGS=kStrings, $
                              TRIM_ENERGIES_BELOW_PEAK=kCurvefit_opt.trim_energies_below_peak, $
                              OUT_FITTED_PARAMS=out_kappaParams, $
                              OUT_FITTED_GAUSS_PARAMS=out_gaussParams, $
                              OUT_KAPPA_FIT_STRUCTS=kappaFits, $
                              OUT_GAUSS_FIT_STRUCTS=gaussFits, $
                              /ADD_FULL_FITS, $
                              ADD_ANGLESTR=angleStr, $
                              OUT_ERANGE_PEAK=out_eRange_peak, $
                              OUT_PARAMSTR=out_paramStr, $
                              ;; DONT_PRINT_ESTIMATES=dont_print_estimates, $
                              DONT_PRINT_FITINFO=dont_print_fitInfo, $
                              FIT_FAIL__USER_PROMPT=fit_fail__user_prompt

           END
        ENDCASE


        ;;Update yRange based on fits
        yRange[1]                                   = yRange[1] > yMax

        CASE 1 OF
           KEYWORD_SET(start_from_fieldaligned): BEGIN
              IF kappaFit.fitStatus EQ 0 THEN BEGIN
                 lastGood_A                         = kappaFit.A
                 distance_from_lastGood             = 0

                 IF ~gotFirst THEN BEGIN
                    gotFirst                        = gotFirst
                 ENDIF
              ENDIF ELSE BEGIN
                 distance_from_lastGood++
              END

              IF KEYWORD_SET(kCurvefit_opt.add_gaussian_estimate) THEN BEGIN
                 IF gaussFit.fitStatus EQ 0 THEN BEGIN
                    lastGood_AGauss                 = gaussFit.A
                    distance_from_lastGood_gauss    = 0
                 ENDIF ELSE BEGIN
                    distance_from_lastGood_gauss++
                 ENDELSE
              ENDIF 
           END
           ELSE: BEGIN

           END
        ENDCASE

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;Now do plots
        ;; IF ~KEYWORD_SET(kPlot_opt.no_plots) THEN BEGIN
        ;;    PLOT_KAPPA_FITS,orig,kappaFit,gaussFit,oneCurve, $
        ;;                    ;; TITLE=title, $
        ;;                    BOUNDS_I=iTime, $
        ;;                    XRANGE=xRange, $
        ;;                    YRANGE=yRange, $
        ;;                    XLOG=xLog, $
        ;;                    YLOG=yLog, $
        ;;                    STRINGS=kStrings, $
        ;;                    ADD_FITPARAMS_TEXT=kPlot_opt.add_fitParams_text, $
        ;;                    ADD_GAUSSIAN_ESTIMATE=kCurvefit_opt.add_gaussian_estimate, $
        ;;                    ADD_ANGLE_LABEL=KEYWORD_SET(kPlot_opt.add_angle_label) ? tempAngle : !NULL, $
        ;;                    SAVE_FITPLOTS=kPlot_opt.save_fitPlots, $ ;, $
        ;;                    SKIP_BAD_FITS=fit_each__skip_bad_fits, $
        ;;                    PLOT_FULL_FIT=kPlot_opt.plot_full_fit, $
        ;;                    ;; USING_SDT_DATA=using_sdt_data, $
        ;;                    PLOTDIR=kPlot_opt.plotDir
        ;; ENDIF

        ;;Now that we've finished with all these angles, let's see about recovering them
        IF KEYWORD_SET(synthPackage) THEN BEGIN

           ;;ARRAY_EQUAL(REFORM(diff_eFlux.theta[nEnergies/2,*,bounds[i+1]]),tempAllAngles)
           ;;0
           ;;ARRAY_EQUAL(REFORM(diff_eFlux.theta[nEnergies/2,*,iTime]),tempAllAngles)
           ;;1
           
           ;;Determine whether we're keeping this guy or not, for plotting purposes and for building synthetic SDT structs
           ;; skipKappa                             = (kappaFit.fitStatus GT 0) AND KEYWORD_SET(fit_each__skip_bad_fits)
           skipKappa                                = (kappaFit.fitStatus GT 0)

           IF ~skipKappa THEN BEGIN
              ;; synthKappa.data[*,iAngle,iTime]       = REVERSE(kappaFit.yFull)
              ;; synthKappa.energy[*,iAngle,iTime]     = REVERSE(kappaFit.xFull)
              synthKappa.data[*,iAngle,iTime]       = kappaFit.yFull
              synthKappa.energy[*,iAngle,iTime]     = kappaFit.xFull
              nGoodFits_tempK++
              good_angleBinK_i                      = [good_angleBinK_i,iAngle]
              good_kappaFits_i                      = [good_kappaFits_i,N_ELEMENTS(kappaFits)-1]
           ENDIF

           IF KEYWORD_SET(kCurvefit_opt.add_gaussian_estimate) THEN BEGIN
              ;; skipGauss                          = (gaussFit.fitStatus GT 0) AND KEYWORD_SET(fit_each__skip_bad_fits)
              skipGauss                             = (gaussFit.fitStatus GT 0)

              IF ~skipGauss THEN BEGIN
                 ;; synthGauss.data[*,iAngle,iTime]    = REVERSE(gaussFit.yFull)
                 ;; synthGauss.energy[*,iAngle,iTime]  = REVERSE(gaussFit.xFull)
                 synthGauss.data[*,iAngle,iTime]    = gaussFit.yFull
                 synthGauss.energy[*,iAngle,iTime]  = gaussFit.xFull
                 nGoodFits_tempG++
                 good_angleBinG_i                   = [good_angleBinG_i,iAngle]
                 good_gaussFits_i                   = [good_gaussFits_i,N_ELEMENTS(gaussFits)-1]
              ENDIF
           ENDIF
        ENDIF

     ENDFOR

     ;;OK, now that we've got all the fits that succeeded, let's see how they do in the mosh pit
     curKappaStr                                    = MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(synthKappa,iTime)
     curGaussStr                                    = MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(synthGauss,iTime)
     curDataStr                                     = MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(diff_eFlux,iTime)

     CASE 1 OF
        KEYWORD_SET(kCurvefit_opt.add_gaussian_estimate): BEGIN
           proceed                                  = ((nGoodFits_tempK GT 0) AND $
                                                       (nGoodFits_tempG GT 0))
        END
        ELSE: BEGIN
           proceed                                  = nGoodFits_tempK GT 0
        END
     ENDCASE

     IF proceed THEN BEGIN      ;skip if we lose

        KAPPA_FIT2D__TRY_EACH_1DFIT,keepK_iTime,iTime, $
                                    nEnergies,out_eRange_peak, $
                                    tempAllAngles,nTotAngles,useTheseAnglesIndex, $
                                    successesK, $
                                    curKappaStr,kappaFits,curDataStr, $
                                    good_angleBinK_i,good_kappaFits_i,iWin, $
                                    ESTFACS=estFacs, $
                                    KCURVEFIT_OPT=kCurvefit_opt, $
                                    KPLOT_OPT=kPlot_opt, $
                                    KSDTDATA_OPT=kSDTData_opt, $
                                    KSTRINGS=kStrings, $
                                    FIT2D_INF_LIST=fit2DKappa_inf_list, $
                                    FIT2D__SHOW_AND_PROMPT__EACH_CANDIDATE=fit2d__show_each_candidate, $
                                    FIT2D__SHOW__IS_MAXWELLIAN_FIT=0

        totSuccessesK += successesK

        PRINT,FORMAT='("KAPPA_FIT2D__TRY_EACH SUCCESSES (TOT) : ",I0," (",I0,")")', $
              successesK, $
              totSuccessesK

        kBest = fit2DKappa_inf_list[-1].bestFit1DParams.A[2]

     ENDIF ELSE BEGIN
        kBest       = 999
     ENDELSE

     IF proceed AND KEYWORD_SET(kCurvefit_opt.add_gaussian_estimate) THEN BEGIN

        KAPPA_FIT2D__TRY_EACH_1DFIT,keepG_iTime,iTime, $
                                    nEnergies,out_eRange_peak, $
                                    tempAllAngles,nTotAngles,useTheseAnglesIndex, $
                                    successesG, $
                                    curGaussStr,gaussFits,curDataStr, $
                                    good_angleBinG_i,good_gaussFits_i,iWin, $
                                    ESTFACS=estFacs, $
                                    KCURVEFIT_OPT=kCurvefit_opt, $
                                    KPLOT_OPT=kPlot_opt, $
                                    KSDTDATA_OPT=kSDTData_opt, $
                                    KSTRINGS=kStrings, $
                                    FIT2D_INF_LIST=fit2DGauss_inf_list, $
                                    FIT2D__SHOW_AND_PROMPT__EACH_CANDIDATE=fit2d__show_each_candidate, $
                                    /FIT2D__SHOW__IS_MAXWELLIAN_FIT
        
        totSuccessesG += successesG

        PRINT,FORMAT='("GAUSS_FIT2D__TRY_EACH SUCCESSES (TOT) : ",I0," (",I0,")")', $
              successesG, $
              totSuccessesG

     ENDIF

     IF proceed AND KEYWORD_SET(fit_each__show_and_prompt) AND kBest LT 3.0 THEN BEGIN

        KAPPA_FIT2D__SHOW_AND_PROMPT__SELECTED2DFIT,curDataStr,fit2DKappa_inf_list[-1], $
           totSuccessesK, $
           iTime, $
           IS_MAXWELLIAN_FIT=0, $
           KSTRINGS=kStrings, $
           KPLOT_OPT=kPlot_opt, $
           KCURVEFIT_OPT=kCurvefit_opt, $
           PROMPT__CONT_TO_NEXT_FIT=Kprompt__cont_to_next_fit, $
           PROMPT__CONT_UNTIL_FIT_EQ=Kprompt__cont_until_fit_eq, $
           FINISH_AND_SAVE_ALL=Kfinish_and_save_all, $
           KAPPA_FIT__SHOW__QUIT=Kshow__quit
        
        IF KEYWORD_SET(kCurvefit_opt.add_gaussian_estimate) THEN BEGIN
           KAPPA_FIT2D__SHOW_AND_PROMPT__SELECTED2DFIT,curDataStr,fit2DGauss_inf_list[-1], $
              totSuccessesG, $
              iTime, $
              IS_MAXWELLIAN_FIT=1, $
              KSTRINGS=kStrings, $
              KPLOT_OPT=kPlot_opt, $
              KCURVEFIT_OPT=kCurvefit_opt, $
              PROMPT__CONT_TO_NEXT_FIT=Gprompt__cont_to_next_fit, $
              PROMPT__CONT_UNTIL_FIT_EQ=Gprompt__cont_until_fit_eq, $
              FINISH_AND_SAVE_ALL=Gfinish_and_save_all, $
              KAPPA_FIT__SHOW__QUIT=Gshow__quit
        ENDIF
     ENDIF

  ENDFOR

  IF KEYWORD_SET(synthPackage) THEN BEGIN
     

     IF KEYWORD_SET(fit_each__skip_bad_fits) THEN BEGIN
        tmpTimeK_i = keepK_iTime
        tmpTimeG_i = keepG_iTime
        tmpTime_i  = CGSETINTERSECTION(keepK_iTime,keepG_iTime,COUNT=nTotKeepTimes)

        nKappaTimes = N_ELEMENTS(keepK_iTime)
        nGaussTimes = N_ELEMENTS(keepG_iTime)

        PRINT,"N KEEP TIMES"
        PRINT,FORMAT='("Kappa",T15,"Gauss",T30,"Total")'
        PRINT,FORMAT='(I0,T15,I0,T30,I0)',nKappaTimes,nGaussTimes,nTotKeepTimes
     ENDIF ELSE BEGIN
        tmpTimeK_i = bounds
        tmpTimeG_i = bounds
     ENDELSE

     synthPackage = LIST(MAKE_ARRAY_OF_SDT_STRUCTS_FROM_PREPPED_EFLUX(diff_eFlux, $
                                                                      TIME_INDS=tmpTime_i), $
                         MAKE_ARRAY_OF_SDT_STRUCTS_FROM_PREPPED_EFLUX(synthKappa, $
                                                                      TIME_INDS=tmpTime_i, $
                                                                      /RECALCULATE_DDATA, $
                                                                      APPEND_DATA_NAME=' (Kappa fits)'))
     IF KEYWORD_SET(kCurvefit_opt.add_gaussian_estimate) THEN BEGIN
        synthPackage.Add,MAKE_ARRAY_OF_SDT_STRUCTS_FROM_PREPPED_EFLUX(synthGauss, $
                                                                      TIME_INDS=tmpTime_i, $
                                                                      /RECALCULATE_DDATA, $
                                                                      APPEND_DATA_NAME=' (Gauss fits)')
     ENDIF
  ENDIF
END
