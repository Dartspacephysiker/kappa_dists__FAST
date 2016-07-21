PRO KAPPA_FIT2D__LOOP,diff_eFlux,times,dEF_oneCount, $
                      KSDTDATA_OPT=kSDTData_opt, $
                      KCURVEFIT_OPT=kCurvefit_opt, $
                      KPLOT_OPT=kPlot_opt, $
                      STRINGS=kStrings, $
                      BOUNDS=bounds, $
                      ESTFACS=estFacs, $
                      DONT_PRINT_ESTIMATES=dont_print_estimates, $
                      E_ANGLE=e_angle, $
                      DONT_TAKE_STOCK_OF_BULKANGLE=dont_take_stock_of_bulkangle, $
                      TREAT_FIELDALIGNED_AS_BULK=treat_fieldaligned_as_bulk, $
                      CHECK_FOR_HIGHER_FLUX_PEAKS=check_for_higher_flux_peaks__set_corresponding_peak_energy, $
                      FIT_EACH__AVERAGE_OVER_ANGLERANGE=fit_each__average_over_angleRange, $
                      FIT_EACH__SYNTH_SDT_STRUCT=synthPackage, $
                      FIT_EACH__SKIP_BAD_FITS=fit_each__skip_bad_fits, $
                      FIT_EACH__MIN_ANGLEFITS_FOR_KEEP=min_anglefits_for_keep, $
                      FIT_EACH__START_FROM_FIELDALIGNED=start_from_fieldaligned, $
                      FIT_EACH__SHOW_AND_PROMPT=fit_each__show_and_prompt, $
                      FIT_FAIL__USER_PROMPT=fit_fail__user_prompt, $
                      OUT_FITTED_PARAMS=out_kappaParams, $
                      OUT_FITTED_GAUSS_PARAMS=out_gaussParams, $
                      OUT_KAPPA_FIT_STRUCTS=kappaFits, $
                      OUT_GAUSS_FIT_STRUCTS=gaussFits, $
                      OUT_ERANGE_PEAK=out_eRange_peak, $
                      OUT_PARAMSTR=out_paramStr, $
                      TXTOUTPUTDIR=txtOutputDir
  
  COMPILE_OPT idl2

  RESOLVE_ROUTINE,'mpfit2dfun',/IS_FUNCTION

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

  IF KEYWORD_SET(fit_each__skip_bad_fits) THEN BEGIN
     keeper_bounds_i                       = !NULL

     IF KEYWORD_SET(min_anglefits_for_keep) THEN BEGIN
        min_aFits_for_keep                 = min_anglefits_for_keep
     ENDIF ELSE BEGIN
        min_aFits_for_keep                 = 0
     ENDELSE
  ENDIF

  successes = 0
  FOR i=0,N_ELEMENTS(bounds)-1 DO BEGIN

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
        min_peak_ind                       = MIN(WHERE(REVERSE(REFORM(XorigArr[0,*])) GE kCurvefit_opt.min_peak_energy)) 
        IF min_peak_ind EQ -1 THEN BEGIN
           STOP
        ENDIF
     ENDIF ELSE BEGIN
        min_peak_ind                       = 0
     ENDELSE

     dat                                   = MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(diff_eFlux,iTime)

     ;;Just get the angles for this time, and we'll assume the middle of the energy range is representative
     tempAllAngles                         = AorigArr[*,nEnergies/2]

     CASE alleyOop OF
        0: BEGIN
           angleBins                       = tempAllAngles GE tempRange[0] AND tempAllAngles LE tempRange[1]
        END
        1: BEGIN
           angleBins                       = tempAllAngles GE tempRange[0] OR tempAllAngles LE tempRange[1]
        END
        
     ENDCASE

     angleBin_i                            = WHERE(angleBins,nAngles)
     nLoopAngles                           = nAngles ;This needs to be distinguished from nAngles

     IF KEYWORD_SET(start_from_fieldaligned) THEN BEGIN
        distance_from_lastGood             = 0
        distance_from_lastGood_gauss       = 0

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
     tempDat                               = (REVERSE(dat.data,1))[min_peak_ind:-1,angleBin_i]
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
     
     
     ;; tangles                            = (REVERSE(dat.theta,1))[min_peak_ind:-1,*]/N_ELEMENTS([min_peak_ind:47])
     ;; checkitout                         = plot(total(tangles,1),angleDist,symbol='*',linestyle=6)

     ;;Here's some code for checking it out
     ;; this                               = MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(diff_eFlux,iTime)
     ;;For orbit 1849 (JOURNAL__20160714), these are informative pitch angle dists
     ;; contour2d,MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(diff_eFlux,bounds[41]),/polar
     ;; contour2d,MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(diff_eFlux,bounds[65]),/polar


     nGoodFits_tempK                       = 0
     nGoodFits_tempG                       = 0
     good_angleBin_i                       = !NULL
     good_kappaFits_i                      = !NULL
     ;;Now loop over each angle and attemp to fit 
     ;;We'll keep all the good fits, and do a comparison among them at the end
     FOR iiAngle=0,nLoopAngles-1 DO BEGIN

        iAngle                             = (KEYWORD_SET(start_from_fieldaligned) ? angle_order : angleBin_i)[iiAngle]
        iAngle                             = angleBin_i[iiAngle]

        ;;Start sweeping backwards, if we made it to swap location
        IF KEYWORD_SET(start_from_fieldaligned) THEN BEGIN
           IF iAngle EQ change_sweep_i THEN BEGIN
              change_sweep_direction       = 1
              iAngle                       = angle_order[iiAngle++]
           ENDIF ELSE BEGIN
              change_sweep_direction       = 0
           ENDELSE
        ENDIF

        ;;Here's the data we're working with for this loop iteration
        Xorig                              = REVERSE(REFORM(XorigArr[iAngle,*]))
        Yorig                              = REVERSE(REFORM(YorigArr[iAngle,*]))
        Aorig                              = REVERSE(REFORM(AorigArr[iAngle,*]))

        tempAngle                          = tempAllAngles[iAngle]
        tempAngleEstRange                  = [tempAngle-1.0,tempAngle+1.0]
        angleStr.SDTAngle                  = tempAngle

        IF KEYWORD_SET(kPlot_opt.add_oneCount_curve) THEN BEGIN
           oneCurve                        = {x:Xorig, $
                                              y:REVERSE(REFORM(oneCountArr[iAngle,*])), $
                                              NAME:"One Count"}
        ENDIF

        KAPPA__GET_PEAK_IND_AND_PEAK_ENERGY,Xorig,Yorig,peak_ind,peak_energy, $
                                            BULK_OFFSET=kCurvefit_opt.bulk_offset, $
                                            CHECK_FOR_HIGHER_FLUX_PEAKS=check_for_higher_flux_peaks__set_corresponding_peak_energy, $
                                            MIN_PEAK_ENERGY=kCurvefit_opt.min_peak_energy
        
        minEInd                            = (peak_ind - kCurvefit_opt.n_below_peak) > 0
        maxEInd                            = (peak_ind + kCurvefit_opt.n_above_peak) < nEnergies-1

        IF KEYWORD_SET(dont_fit_below_thresh_value) THEN BEGIN
           
           nAbove                          = nEnergies-1-maxEInd
           killIt                          = WHERE( (Xorig GE peak_energy) AND (Yorig LE 1e5),nStink)
           IF (nAbove GE 4) AND nStink NE 0 THEN BEGIN
              maxEInd                      = maxEInd < MIN(killIt)
           ENDIF
        ENDIF

        ;;estimate from the data!
        IF KEYWORD_SET(kCurvefit_opt.estimate_A_from_data) THEN BEGIN 
           CASE 1 OF
              KEYWORD_SET(start_from_fieldaligned): BEGIN
                 CASE 1 OF
                    (iiAngle EQ 0): BEGIN ;Get field-aligned estimates

                       kappa_fixA = [1,1,1,1,0,0,0]
                       gauss_fixA = [1,1,0,1,0,0,0]

                       KAPPA__GET_A_ESTIMATES,dat,Xorig,Yorig, $
                                              minEInd,maxEInd,nEnergies, $
                                              peak_ind,peak_energy,eRange_peak, $
                                              KAPPA_EST=kCurvefit_opt.fitA[2], $
                                              ;; MASS=mass, $
                                              E_ANGLE=kCurvefit_opt.electron_angleRange, $
                                              ANGLES=tempAngleEstRange, $
                                              N_ANGLES_IN_RANGE=nAngles, $
                                              BULKANGLE_STRUCT=angleStr, $
                                              DONT_TAKE_STOCK_OF_BULKANGLE=dont_take_stock_of_bulkangle, $
                                              ADD_GAUSSIAN_ESTIMATE=kCurvefit_opt.add_gaussian_estimate, $
                                              USE_SDT_GAUSSIAN_FIT=kCurvefit_opt.use_SDT_Gaussian_fit, $
                                              ESTFACS=estFacs, $
                                              A_OUT=A, $
                                              AGAUSS_OUT=AGauss, $
                                              DONT_PRINT_ESTIMATES=dont_print_estimates
                       A_FA_initEst       = A
                       AGauss_FA_initEst  = AGauss

                    END
                    change_sweep_direction: BEGIN
                       A                  = A_FA_initEst
                       AGauss             = AGauss_FA_initEst
                    END
                    ELSE: BEGIN
                       A      = lastGood_A
                       AGauss = lastGood_AGauss

                       kappa_fixA = [0,1,0,1,0,0,0]
                       gauss_fixA = [0,1,0,1,0,0,0]

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
                                        DONT_PRINT_ESTIMATES=dont_print_estimates


              END
           ENDCASE

        ENDIF ELSE BEGIN
           A                               = DOUBLE([peak_energy,T,kappa,n_est,0.000001,5.68e-6,0])
        ENDELSE
        

        xRange                             = [MIN(Xorig[WHERE(Xorig GT 0)]),MAX(Xorig)]
        yRange                             = [yMin,MAX(data)]

        KAPPA__GET_FITS,Xorig,Yorig, $
                        orig,kappaFit,gaussFit, $
                        ADD_GAUSSIAN_ESTIMATE=kCurvefit_opt.add_gaussian_estimate, $
                        USE_SDT_GAUSSIAN_FIT=kCurvefit_opt.use_SDT_Gaussian_fit, $
                        BOUNDS_I=iTime, $
                        ENERGY_INDS=[minEInd,maxEInd], $
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
                        DONT_PRINT_ESTIMATES=dont_print_estimates, $
                        FIT_FAIL__USER_PROMPT=fit_fail__user_prompt


        ;;Update yRange based on fits
        yRange[1]                          = yRange[1] > yMax

        CASE 1 OF
           KEYWORD_SET(start_from_fieldaligned): BEGIN
              IF kappaFit.fitStatus EQ 0 THEN BEGIN
                 lastGood_A             = kappaFit.A
                 distance_from_lastGood = 0
              ENDIF ELSE BEGIN
                 distance_from_lastGood++
              END

              IF KEYWORD_SET(kCurvefit_opt.add_gaussian_estimate) THEN BEGIN
                 IF gaussFit.fitStatus EQ 0 THEN BEGIN
                    lastGood_AGauss = gaussFit.A
                    distance_from_lastGood_gauss = 0
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
           ;; skipKappa  = (kappaFit.fitStatus GT 0) AND KEYWORD_SET(fit_each__skip_bad_fits)
           skipKappa  = (kappaFit.fitStatus GT 0)

           IF ~skipKappa THEN BEGIN
              synthKappa.data[*,iAngle,iTime]   = REVERSE(kappaFit.yFull)
              synthKappa.energy[*,iAngle,iTime] = REVERSE(kappaFit.xFull)
              nGoodFits_tempK++
              good_angleBin_i                   = [good_angleBin_i,iAngle]
              good_kappaFits_i                  = [good_kappaFits_i,N_ELEMENTS(kappaFits)-1]
           ENDIF

           IF KEYWORD_SET(add_gaussian_estimate) THEN BEGIN
              ;; skipGauss  = (gaussFit.fitStatus GT 0) AND KEYWORD_SET(fit_each__skip_bad_fits)
              skipGauss  = (gaussFit.fitStatus GT 0)

              IF ~skipGauss THEN BEGIN
                 synthGauss.data[*,iAngle,iTime]   = REVERSE(gaussFit.yFull)
                 synthGauss.energy[*,iAngle,iTime] = REVERSE(gaussFit.xFull)
                 nGoodFits_tempG++
              ENDIF
           ENDIF
        ENDIF

     ENDFOR

     ;;OK, now that we've got all the fits that succeeded, let's see how they do in the mosh pit
     curKappaStr        = MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(synthKappa,iTime)
     curDataStr         = MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(diff_eFlux,iTime)

     PRINT,"SUCCESSES : " + STRCOMPRESS(successes,/REMOVE_ALL)
     ;;Make as many testArrays as we have successful fits

     IF nGoodFits_tempK EQ 0 THEN CONTINUE ;skip if we lose

     testArrays         = MAKE_ARRAY(nEnergies,nTotAngles,nGoodFits_tempK,/FLOAT)

     chiArray           = !NULL
     dofArray           = !NULL
     fitparams2dArray   = !NULL
     errMsgArray        = !NULL
     statusArray        = !NULL
     FOR iWin=0,nGoodFits_tempK-1 DO BEGIN

        SETUP_KAPPA_FIT2D_TEST,good_angleBin_i,good_kappaFits_i,iWin, $
                               nEnergies,nTotAngles, $
                               curKappaStr,kappaFits,curDataStr, $
                               iAngle,iKappa,testKappa,testKappaFit,testArray, $
                               craptest, $
                               wts,X2D,Y2D,dataToFit, $
                               fa,dens_param

        Fitparams2D     = MPFIT2DFUN('KAPPA_FLUX2D__SCALE_DENSITY',X2D,Y2D,dataToFit, $
                                     err, $
                                     dens_param, $
                                     WEIGHTS=wts, $
                                     FUNCTARGS=fa, $
                                     BESTNORM=bestNorm, $
                                     NFEV=nfev, $
                                     FTOL=kCurvefit_opt.fit2d_tol, $
                                     STATUS=status, $
                                     best_resid=best_resid, $
                                     pfree_index=ifree, $
                                     calc_fjac=calc_fjac, $
                                     best_fjac=best_fjac, $
                                     parinfo=parinfo, query=query, $
                                     npegged=npegged, nfree=nfree, dof=dof, $
                                     covar=covar, perror=perror, $
                                     MAXITER=kCurvefit_opt.fit2d_max_iter, $
                                     niter=niter, $
                                     YFIT=yfit, $
                                     quiet=quiet, $
                                     ERRMSG=errMsg, $
                                     _EXTRA=extra)

        errMsgArray      = [errMsgArray,errMsg]
        statusArray      = [statusArray,status]
        chiArray         = [chiArray,bestNorm]
        dofArray         = [dofArray,dof]
        fitparams2dArray = [fitparams2dArray,fitparams2d]
        IF status GT 0 THEN BEGIN
           testArrays[*,*,iWin] = yFit
        ENDIF ELSE BEGIN
           testArrays[*,*,iWin] = 0.0
        ENDELSE
     ENDFOR
     
     ;;Now decide who is most awesome
     successes++

     testMe0                                 = curdataStr
     testMe0.data                            = testArrays[*,*,0]
     aDiffMe0                                = testMe0
     aDiffMe0.data                           = ABS(testMe0.data-curDataStr.data)
     diffMe0                                 = testMe0
     diffMe0.data                            = testMe0.data-curDataStr.data
     diffMe0.data[WHERE(diffMe0.data LE 0)]  = 0.0

     testMe1                                 = curdataStr
     testMe1.data                            = testArrays[*,*,1]
     aDiffMe1                                = testMe1
     aDiffMe1.data                           = ABS(testMe1.data-curDataStr.data)
     diffMe1                                 = testMe1
     diffMe1.data                            = testMe1.data-curDataStr.data
     diffMe1.data[WHERE(diffMe1.data LE 0)]  = 0.0

     ;;Decide whether we're going to keep this time in the synthetic structs
     IF KEYWORD_SET(fit_each__skip_bad_fits) THEN BEGIN
        IF min_aFits_for_keep GT nAngles THEN BEGIN
           PRINT,"min_aFits can't be greater than nAngles (= " + STRCOMPRESS(nAngles,/REMOVE_ALL) + ")!"
           min_aFits_for_keep = nAngles
        ENDIF

        IF (nGoodFits_tempK GT min_aFits_for_keep) OR (nGoodFits_tempG GT min_aFits_for_keep) THEN BEGIN
           keeper_bounds_i = [keeper_bounds_i,iTime]
        ENDIF
     ENDIF

  ENDFOR

     ;; IF KEYWORD_SET(fit_each__show_and_prompt) THEN BEGIN
     ;;    CASE 1 OF
     ;;       KEYWORD_SET(prompt__cont_to_next_fit): BEGIN
     ;;          IF nGoodFits_tempK GT 0 THEN BEGIN
     ;;             cont = 0
     ;;             prompt__cont_to_next_fit = 0
     ;;          ENDIF ELSE BEGIN
     ;;             cont = 1
     ;;          ENDELSE
     ;;       END
     ;;       KEYWORD_SET(prompt__cont_until_fit_eq): BEGIN
     ;;          IF nGoodFits_tempK GE prompt__cont_until_fit_eq THEN BEGIN
     ;;             cont = 0
     ;;          ENDIF ELSE BEGIN
     ;;             cont = 1
     ;;          ENDELSE
     ;;       END
     ;;       ELSE: BEGIN
     ;;          cont    = 0
     ;;       END
     ;;    ENDCASE

     ;;    input         = ''
     ;;    showFit       = nGoodFits_tempK GT 0

     ;;    IF ~KEYWORD_SET(finish_and_save_all) THEN BEGIN
     ;;       IF showFit THEN BEGIN
     ;;          CONTOUR2D,MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(synthKappa,iTime),/POLAR,/FILL
     ;;       ENDIF
     ;;       CONTOUR2D,MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(diff_eFlux,iTime),/POLAR,OVERPLOT=showFit
     ;;    ENDIF

     ;;    WHILE ~cont DO BEGIN
     ;;       PRINT,"(C)ontinue    / Cont. to (N)ext fit   / Cont. (U)ntil fitNum Eq  / S(T)op and inspect / "
     ;;       PRINT,"Sp(E)ctrum    / (FE) Fitted spectrum  / (P)itch angle / "
     ;;       PRINT,"(S)ave        / (F)inish and save all / (Q)uit ?"
     ;;       IF KEYWORD_SET(finish_and_save_all) THEN input = 's' ELSE READ,input
     ;;       CASE STRLOWCASE(input) OF
     ;;          "c": BEGIN
     ;;             cont = 1
     ;;          END
     ;;          "n": BEGIN
     ;;             prompt__cont_to_next_fit = 1
     ;;             cont = 1
     ;;          END
     ;;          "u": BEGIN
     ;;             cont2 = 0
     ;;             WHILE ~cont2 DO BEGIN
     ;;                PRINT,"Min fits for stopping: "
     ;;                READ,minNumFits
     ;;                IF minNumFits GT nAngles THEN BEGIN
     ;;                   PRINT,"Can't select higher number than there are angles being fit!"
     ;;                ENDIF ELSE BEGIN
     ;;                   prompt__cont_until_fit_eq = minNumFits
     ;;                   cont2 = 1
     ;;                ENDELSE
     ;;             ENDWHILE
     ;;             cont = 1
     ;;          END
     ;;          "t": BEGIN
     ;;             STOP
     ;;          END
     ;;          "e": BEGIN
     ;;             cont = 0
     ;;             ;; tempFN = STRING(FORMAT='("contour2d--data_and_kappa_fit--orb_",A0,"--",A0)',kStrings.orbStr,kStrings.timeFNStrs[iTime])
     ;;             ;; PRINT,"Saving to " + tempFN + ' ...'
     ;;             ;; POPEN,(KEYWORD_SET(kPlot_opt.plotDir) ? kPlot_opt.plotDir : './') + tempFN
     ;;             ;; IF showFit THEN CONTOUR2D,MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(synthKappa,iTime),/POLAR,/FILL
     ;;             SPEC2D,MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(diff_eFlux,iTime),/LABEL,LIMITS={yrange:[1e6,1e10]} ;,/POLAR,OVERPLOT=showFit
     ;;             ;; PCLOSE
     ;;          END
     ;;          "fe": BEGIN
     ;;             cont = 0
     ;;             ;; tempFN = STRING(FORMAT='("contour2d--data_and_kappa_fit--orb_",A0,"--",A0)',kStrings.orbStr,kStrings.timeFNStrs[iTime])
     ;;             ;; PRINT,"Saving to " + tempFN + ' ...'
     ;;             ;; POPEN,(KEYWORD_SET(kPlot_opt.plotDir) ? kPlot_opt.plotDir : './') + tempFN
     ;;             ;; IF showFit THEN CONTOUR2D,MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(synthKappa,iTime),/POLAR,/FILL
     ;;             IF showFit THEN BEGIN
     ;;                SPEC2D,MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(synthKappa,iTime),/LABEL,LIMITS={yrange:[1e6,1e10]} 
     ;;             ENDIF ELSE PRINT,"No good fits!"
     ;;             ;; PCLOSE
     ;;          END
     ;;          "p": BEGIN
     ;;             cont = 0
     ;;             IF showFit THEN CONTOUR2D,MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(synthKappa,iTime),/POLAR,/FILL
     ;;             CONTOUR2D,MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(diff_eFlux,iTime),/POLAR,OVERPLOT=showFit
     ;;          END
     ;;          "s": BEGIN
     ;;             cont = 1
     ;;             tempFN = STRING(FORMAT='("contour2d--data_and_kappa_fit--orb_",A0,"--",A0)',kStrings.orbStr,kStrings.timeFNStrs[iTime])
     ;;             PRINT,"Saving to " + tempFN + ' ...'
     ;;             POPEN,(KEYWORD_SET(kPlot_opt.plotDir) ? kPlot_opt.plotDir : './') + tempFN
     ;;             IF showFit THEN CONTOUR2D,MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(synthKappa,iTime),/POLAR,/FILL
     ;;             CONTOUR2D,MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(diff_eFlux,iTime),/POLAR,OVERPLOT=showFit
     ;;             PCLOSE
     ;;          END
     ;;          "f": BEGIN
     ;;             cont = 1
     ;;             finish_and_save_all = 1
     ;;             tempFN = STRING(FORMAT='("contour2d--data_and_kappa_fit--orb_",A0,"--",A0)',kStrings.orbStr,kStrings.timeFNStrs[iTime])
     ;;             PRINT,"Saving to " + tempFN + ' ...'
     ;;             POPEN,(KEYWORD_SET(kPlot_opt.plotDir) ? kPlot_opt.plotDir : './') + tempFN
     ;;             IF showFit THEN CONTOUR2D,MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(synthKappa,iTime),/POLAR,/FILL
     ;;             CONTOUR2D,MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(diff_eFlux,iTime),/POLAR,OVERPLOT=showFit
     ;;             PCLOSE
     ;;          END
     ;;          "q": BEGIN
     ;;             PRINT,"Returning ..."
     ;;             quit = 1
     ;;             cont = 1
     ;;             ;; RETURN
     ;;          END
     ;;          ELSE: BEGIN
     ;;             PRINT,"Invalid option: " + input
     ;;          END
     ;;       ENDCASE

     ;;    ENDWHILE
     ;; ENDIF

     ;; IF KEYWORD_SET(quit) THEN BEGIN
     ;;    PRINT,"Breaking out of loop ..."
     ;;    BREAK
     ;; ENDIF

  ;; ENDFOR

  IF KEYWORD_SET(synthPackage) THEN BEGIN
     

     IF KEYWORD_SET(fit_each__skip_bad_fits) THEN BEGIN
        tmpTime_i = keeper_bounds_i
     ENDIF ELSE BEGIN
        tmpTime_i = bounds
     ENDELSE

     synthPackage = LIST(MAKE_ARRAY_OF_SDT_STRUCTS_FROM_PREPPED_EFLUX(diff_eFlux, $
                                                                      TIME_INDS=tmpTime_i), $
                         MAKE_ARRAY_OF_SDT_STRUCTS_FROM_PREPPED_EFLUX(synthKappa, $
                                                                      TIME_INDS=tmpTime_i, $
                                                                      /RECALCULATE_DDATA, $
                                                                      APPEND_DATA_NAME=' (Kappa fits)'))
     IF KEYWORD_SET(add_gaussian_estimate) THEN BEGIN
        synthPackage.Add,MAKE_ARRAY_OF_SDT_STRUCTS_FROM_PREPPED_EFLUX(synthGauss, $
                                                                      TIME_INDS=tmpTime_i, $
                                                                      /RECALCULATE_DDATA, $
                                                                      APPEND_DATA_NAME=' (Gauss fits)')
     ENDIF
  ENDIF
END
