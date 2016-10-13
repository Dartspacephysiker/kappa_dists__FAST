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
                      BOUNDS=bounds, $
                      ESTFACS=estFacs, $
                      FIT1D__AVERAGE_OVER_ANGLERANGE=fit1D__average_over_angleRange, $
                      FIT1D__SKIP_BAD_FITS=fit1D__skip_bad_fits, $
                      FIT1D__SHOW_AND_PROMPT=fit1D__show_and_prompt, $
                      FIT1D__USER_PROMPT_ON_FAIL=fit1D_fail__user_prompt, $
                      FIT2D__SHOW_AND_PROMPT__EACH_CANDIDATE=fit2d__show_each_candidate, $
                      FIT2D__SAVE_ALL_CANDIDATE_PLOTS=fit2D__save_all_candidate_plots, $
                      FIT2D__PRINT_FITINFO=print_2DFitInfo, $
                      DONT_PRINT_ESTIMATES=dont_print_estimates, $
                      DONT_PRINT_FITINFO=dont_print_fitInfo, $
                      ;; E_ANGLE=e_angle, $
                      CHECK_FOR_HIGHER_FLUX_PEAKS=check_higher_peaks_set_peakEn, $
                      OUT_FITTED_PARAMS=out_kappaParams, $
                      OUT_FITTED_GAUSS_PARAMS=out_gaussParams, $
                      OUT_KAPPA_FIT_STRUCTS=kappaFits, $
                      OUT_GAUSS_FIT_STRUCTS=gaussFits, $
                      OUT_FIT2DKAPPA_INF_LIST=fit2dKappa_inf_list, $
                      OUT_FIT2DGAUSS_INF_LIST=fit2dGauss_inf_list, $
                      OUT_SYNTH_SDT_STRUCTS=synthPackage, $
                      OUT_ERANGE_PEAK=out_eRange_peak, $
                      OUT_PARAMSTR=out_paramStr, $
                      TXTOUTPUTDIR=txtOutputDir
  
  COMPILE_OPT idl2

  ;;For updating K_EA__gFunc,K_EA__bFunc
  @common__kappa_flux2d__horseshoe__eanisotropy.pro

  ;;Get yourself KF2D__SDTData_opt,KF2D__Curvefit_opt, etc.
  @common__kappa_fit2d_structs.pro

  ;; RESOLVE_ROUTINE,'mpfit2dfun',/IS_FUNCTION

  ;;So the order becomes [angle,energy,time] for each of these arrays
  energies                          = TRANSPOSE(diff_eFlux.energy,[1,0,2])
  data                              = TRANSPOSE(diff_eFlux.data,[1,0,2])
  oneCount_data                     = KEYWORD_SET(KF2D__Plot_opt.add_oneCount_curve) ? TRANSPOSE(dEF_oneCount.data,[1,0,2]) : !NULL
  angles                            = TRANSPOSE(diff_eFlux.theta,[1,0,2])

  nEnergies                         = N_ELEMENTS(energies[0,*,0])
  nTotAngles                        = N_ELEMENTS(angles[*,0,0])

  ;;In order to get back to how things were, just 
  IF KEYWORD_SET(synthPackage) THEN BEGIN
     synthKappa                     = diff_eFlux
     synthKappa.data[*]             = 0.0
     IF KEYWORD_SET(KF2D__Curvefit_opt.add_gaussian_estimate) THEN BEGIN
        synthGauss                  = diff_eFlux
        synthGauss.data[*]          = 0.0
     ENDIF
  ENDIF

  IF KEYWORD_SET(KF2D__Plot_opt.add_oneCount_curve) THEN BEGIN
     yMin                      = MIN(oneCount_data[WHERE(oneCount_data GT 0)])
     yMin                      = 10.^(FLOOR(ALOG10(yMin)))
  ENDIF ELSE BEGIN
     yMin                      = MIN(data[WHERE(data GT 0)])
  ENDELSE
  
  ;;Init source-cone angles
  INIT_KAPPA_FIT2D_PRELIM_ANGLEBIN_I,tempSCRange,alleyOop,nEnergies, $
                                     ANGLERANGE=KF2D__SDTData_opt.electron_angleRange, $
                                     AORIGARR=angles[*,*,0], $
                                     OUT_NREQ_ANGLES=nReqSCAngles, $
                                     OUT_USETHESEANGLESINDEX=useTheseAnglesIndex

  ;;Init loss-cone angles
  ;; INIT_KAPPA_FIT2D_PRELIM_ANGLEBIN_I,tempLCRange,alleyOop,nEnergies, $
  ;;                                    ANGLERANGE=KF2D__SDTData_opt.electron_lca, $
  ;;                                    AORIGARR=angles[*,*,0], $
  ;;                                    OUT_NREQ_ANGLES=nReqSCAngles, $
  ;;                                    OUT_USETHESEANGLESINDEX=useTheseAnglesIndex

  ;;Init fitParams structs
  CASE 1 OF
     KEYWORD_SET(KF2D__Curvefit_opt.use_mpFit1D): BEGIN
        ;;Vary bulk E [0], Temperature [1], kappa [2], and density [3] (but not angle)
        kappa_fixA        = [0,0,0,0,1] 
        
        ;;Vary bulk E [0], Temperature [1], and density [3] (but not kappa or angle)
        gauss_fixA        = [0,0,1,0,1]
        ATmp              = DOUBLE([1e3,100.,3.0,0.01,0])
        kappaParamStruct  = INIT_KAPPA_FITPARAM_INFO(ATmp,kappa_fixA)

        gaussParamStruct  = INIT_KAPPA_FITPARAM_INFO(TEMPORARY(ATmp),gauss_fixA)
     END
     ELSE: BEGIN
     END
  ENDCASE

  ;;We won't want to use this anymore. We're outrightly fitting 2D, you know
  ;; INIT_KAPPA_FIT2DPARAMS_INFO,kFit2DParamStruct

  keepK_iTime          = !NULL ;Keep track of which fits get to come home with us
  keepG_iTime          = !NULL ;Keep track of which fits get to come home with us
  successesK           = 0
  successesG           = 0
  totSuccessesK        = 0
  totSuccessesG        = 0
  fit2DKappa_inf_list  = LIST()
  fit2DGauss_inf_list  = LIST()
  FOR i=0,N_ELEMENTS(bounds)-1 DO BEGIN

     successesK       = 0
     successesG       = 0

     iTime            = bounds[i]
     t                = times[iTime]

     ;;And now the order becomes [angle,energy] for each of these arrays
     XorigArr         = energies[*,*,iTime]
     YorigArr         = data[*,*,iTime]
     IF KEYWORD_SET(KF2D__Plot_opt.add_oneCount_curve) THEN BEGIN
        oneCountArr   = oneCount_data[*,*,iTime]
     END
     AorigArr         = angles[*,*,iTime]

     IF KEYWORD_SET(KF2D__Curvefit_opt.min_peak_energy) THEN BEGIN
        ;;Try taking it from the top
        min_peak_ind  = MAX(WHERE(REFORM(XorigArr[0,*]) GE KF2D__Curvefit_opt.min_peak_energy)) 
        IF min_peak_ind EQ -1 THEN BEGIN
           STOP
        ENDIF
     ENDIF ELSE BEGIN
        min_peak_ind  = nEnergies-1
     ENDELSE

     tempAllAngles    = AorigArr[*,useTheseAnglesIndex]

     CASE alleyOop OF
        0: BEGIN
           angleBins  = tempAllAngles GE tempSCRange[0] AND tempAllAngles LE tempSCRange[1]
        END
        1: BEGIN
           angleBins  = tempAllAngles GE tempSCRange[0] OR tempAllAngles LE tempSCRange[1]
        END
        
     ENDCASE

     ;;NOTE: AOrigArr[angleBin_i,usetheseanglesindex]-tempallangles[angleBin_i] = 0 EVERYWHERE
     ;;This is good; you can use angleBin_i with AOrigArr
     angleBin_i       = WHERE(angleBins,nAngles)

     ;;2016/09/02 What on earth am I doing here?
     ;;Oh yeah, making sure that we have enough angles to do stuff
     WHILE nAngles LT nReqSCAngles DO BEGIN
        ;;Sort the angles, get indices for unsorting them, find out which angle to include next
        sort_i   = SORT(tempAllAngles)
        tAngles  = tempAllAngles[sort_i]
        unsort_i = VALUE_LOCATE(tAngles,tempAllAngles)
        minAngle = MIN(tempAllAngles[angleBin_i])
        maxAngle = MAX(tempAllAngles[angleBin_i])
        IF ~ARRAY_EQUAL(tAngles[unsort_i],tempAllAngles) THEN STOP

        ;;Get the next angles on either side
        curMinMax_i = VALUE_LOCATE(tAngles, $
                                   [minAngle, $
                                    maxAngle])
        curMinMax_i[0] -= 1
        curMinMax_i[1] += 1
        this = MIN(ABS([tAngles[curMinMax_i[0]]-minAngle,$
                        tAngles[curMinMax_i[1]]-maxAngle]),bin_i)

        angleBin_i = [unsort_i[curMinMax_i[bin_i]],angleBin_i]


        nAngles++

     ENDWHILE

     IF N_ELEMENTS(UNIQ(angleBin_i)) LT nReqSCAngles THEN STOP

     IF KEYWORD_SET(fit1D__average_over_angleRange) THEN BEGIN
        tempY                              = TOTAL(YorigArr[angleBin_i,*],1)/DOUBLE(nAngles)
        FOR iAngle=0,nAngles-1 DO BEGIN
           YorigArr[angleBin_i[iAngle],*]  = tempY
        ENDFOR
     ENDIF

     ;;Make angles start from field aligned, go back and forth
     ;;Doesn't change which angles we use, but it does change the order
     angleBin_i = angleBin_i[SORT(ABS(tempAllAngles[angleBin_i]))]

     ;;Order of dat.data is [energy,angle] when coming from SDT
     dat                   = MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(diff_eFlux,iTime)

     nGoodFits_tempK       = 0
     nGoodFits_tempG       = 0
     good_angleBinK_i      = !NULL
     good_angleBinG_i      = !NULL
     good_kappaFits_i      = !NULL
     good_gaussFits_i      = !NULL

     gotKappa              = 0
     gotGauss              = 0
     gotEm                 = 0

     shoutOut              = 1
     FOR iiAngle=0,nAngles-1 DO BEGIN

        iAngle             = angleBin_i[iiAngle]

        ;;Here's the data we're working with for this loop iteration
        Xorig              = REFORM(XorigArr[iAngle,*])
        Yorig              = REFORM(YorigArr[iAngle,*])
        Aorig              = REFORM(AorigArr[iAngle,*])

        tempAngle          = tempAllAngles[iAngle]
        tempAngleEstRange  = [tempAngle-1.0,tempAngle+1.0]

        IF KEYWORD_SET(KF2D__Plot_opt.add_oneCount_curve) THEN BEGIN
           oneCurve        = {x:Xorig, $
                              y:REFORM(oneCountArr[iAngle,*]), $
                              NAME:"One Count"}
        ENDIF

        KAPPA__GET_PEAK_IND_AND_PEAK_ENERGY, $
           Xorig,Yorig, $
           peak_ind,peak_energy, $
           BULK_OFFSET=KF2D__Curvefit_opt.bulk_offset, $
           CHECK_FOR_HIGHER_FLUX_PEAKS=check_higher_peaks_set_peakEn, $
           MIN_PEAK_ENERGY=KF2D__Curvefit_opt.min_peak_energy
        
        ;; minEInd        = (peak_ind - KF2D__Curvefit_opt.n_below_peak) > 0
        ;; maxEInd        = (peak_ind + KF2D__Curvefit_opt.n_above_peak) < nEnergies-1

        ;;Note that while these are called maxE and minE, suggesting they refer to the max energy and min energy, they do NOT. 
        ;;Rather, they refer to the lowest and highest indices falling within the user-specified parameters 
        ;;  for fitting—namely, n_below_peak and n_above_peak
        maxEInd           = (peak_ind + KF2D__Curvefit_opt.n_below_peak) < nEnergies-1
        minEInd           = (peak_ind - KF2D__Curvefit_opt.n_above_peak) > 0

        ;; IF KEYWORD_SET(KF2D__Curvefit_opt.dont_fit_below_thresh_value) THEN BEGIN
           
        ;;    nAbove      = nEnergies-1-maxEInd
        ;;    killIt      = WHERE( (Xorig GE peak_energy) AND (Yorig LE 1e5),nStink)
        ;;    IF (nAbove GE 4) AND nStink NE 0 THEN BEGIN
        ;;       maxEInd  = maxEInd < MIN(killIt)
        ;;    ENDIF
        ;; ENDIF

        ;;estimate from the data!
        IF KEYWORD_SET(KF2D__Curvefit_opt.estimate_A_from_data) THEN BEGIN 

           KAPPA__GET_A_ESTIMATES,dat,Xorig,Yorig, $
                                  minEInd,maxEInd,nEnergies, $
                                  peak_ind,peak_energy,eRange_peak, $
                                  KAPPA_EST=KF2D__Curvefit_opt.fitA[2], $
                                  ;; MASS=mass, $
                                  E_ANGLE=KF2D__SDTData_opt.electron_angleRange, $
                                  ANGLES=tempAngleEstRange, $
                                  N_ANGLES_IN_RANGE=nAngles, $
                                  ;; BULKANGLE_STRUCT=angleStr, $
                                  DONT_TAKE_STOCK_OF_BULKANGLE=dont_take_stock_of_bulkangle, $
                                  ADD_GAUSSIAN_ESTIMATE=KF2D__Curvefit_opt.add_gaussian_estimate, $
                                  USE_SDT_GAUSSIAN_FIT=KF2D__Curvefit_opt.use_SDT_Gaussian_fit, $
                                  ESTFACS=estFacs, $
                                  A_OUT=A, $
                                  AGAUSS_OUT=AGauss, $
                                  DONT_PRINT_ESTIMATES=dont_print_estimates, $
                                  /TEST_NOREV

        ENDIF ELSE BEGIN
           A                                        = DOUBLE([peak_energy,T,kappa,n_est,0.000001,5.68e-6,0])
        ENDELSE
        

        xRange                                      = [MIN(Xorig[WHERE(Xorig GT 0)]),MAX(Xorig)]
        yRange                                      = [yMin,MAX(data)]
        energy_inds                                 = [minEInd,maxEInd]
        CASE 1 OF
           KEYWORD_SET(KF2D__Curvefit_opt.use_mpFit1D): BEGIN

              IF shoutOut THEN BEGIN
                 PRINT,FORMAT='("Time  (",I0,"/",I0,") : ",A0)', $
                       iTime+1, $
                       N_ELEMENTS(bounds), $
                       KF2D__strings.timeStrs[iTime]

                 shoutOut = 0

              ENDIF
              KAPPA__CONVERT_A_AND_FIXA_TO_MPFITFUN1D_FORMAT,A,kappa_fixA
              KAPPA__CONVERT_A_AND_FIXA_TO_MPFITFUN1D_FORMAT,AGauss,gauss_fixA

              KAPPA__GET_FITS__MPFIT1D,Xorig,Yorig, $
                                       orig,kappaFit,gaussFit, $
                                       KCURVEFIT_OPT=KF2D__Curvefit_opt, $
                                       KFITPARAMSTRUCT=kappaParamStruct, $
                                       GFITPARAMSTRUCT=gaussParamStruct, $
                                       ENERGY_INDS=energy_inds, $
                                       ERANGE_PEAK=eRange_peak, $
                                       PEAK_IND=peak_ind, $
                                       BOUNDS_I=iTime, $
                                       KAPPA_A=A, $
                                       GAUSS_A=AGauss, $
                                       KAPPA_FIXA=kappa_fixA, $
                                       GAUSS_FIXA=gauss_fixA, $
                                       YMAX=yMax, $
                                       STRINGS=KF2D__strings, $
                                       OUT_FITTED_PARAMS=out_kappaParams, $
                                       OUT_FITTED_GAUSS_PARAMS=out_gaussParams, $
                                       ;; OUT_KAPPA_FIT_STRUCTS=kappaFits, $
                                       ;; OUT_GAUSS_FIT_STRUCTS=gaussFits, $
                                       /ADD_FULL_FITS, $
                                       ADD_ANGLESTR=angleStr, $
                                       OUT_ERANGE_PEAK=out_eRange_peak, $
                                       OUT_PARAMSTR=out_paramStr, $
                                       DONT_PRINT_FITINFO=dont_print_fitInfo, $
                                       FIT_FAIL__USER_PROMPT=fit1D_fail__user_prompt

           END
           ELSE: BEGIN
              KAPPA__GET_FITS,Xorig,Yorig, $
                              orig,kappaFit,gaussFit, $
                              ADD_GAUSSIAN_ESTIMATE=KF2D__Curvefit_opt.add_gaussian_estimate, $
                              USE_SDT_GAUSSIAN_FIT=KF2D__Curvefit_opt.use_SDT_Gaussian_fit, $
                              BOUNDS_I=iTime, $
                              ENERGY_INDS=energy_inds, $
                              ERANGE_PEAK=eRange_peak, $
                              PEAK_IND=peak_ind, $
                              KAPPA_A=A, $
                              GAUSS_A=AGauss, $
                              KAPPA_FIXA=kappa_fixA, $
                              GAUSS_FIXA=gauss_fixA, $
                              YMAX=yMax, $
                              MAX_ITER=KF2D__Curvefit_opt.max_iter, $
                              FIT_TOL=KF2D__Curvefit_opt.fit_tol, $
                              STRINGS=KF2D__strings, $
                              TRIM_ENERGIES_BELOW_PEAK=KF2D__Curvefit_opt.trim_energies_below_peak, $
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
                              FIT_FAIL__USER_PROMPT=fit1D_fail__user_prompt

           END
        ENDCASE

        ;;Now handle the adding of stuff
        IF KEYWORD_SET(KF2D__Curvefit_opt.add_gaussian_estimate) THEN BEGIN

           IF ~kappaFit.fitStatus AND ~gaussFit.fitStatus THEN BEGIN

              IF N_ELEMENTS(kappaFits) EQ 0 THEN BEGIN
                 kappaFits    = LIST(kappaFit)
              ENDIF ELSE BEGIN
                 kappaFits.Add,kappaFit
              ENDELSE

              IF N_ELEMENTS(gaussFits) EQ 0 THEN BEGIN
                 gaussFits    = LIST(gaussFit)
              ENDIF ELSE BEGIN
                 gaussFits.Add,gaussFit
              ENDELSE

           ENDIF
        ENDIF ELSE BEGIN

           IF N_ELEMENTS(kappaFits) EQ 0 THEN BEGIN
              kappaFits    = LIST(kappaFit)
           ENDIF ELSE BEGIN
              kappaFits.Add,kappaFit
           ENDELSE

        ENDELSE

        ;;Now that we've finished with all these angles, let's see about recovering them
        IF KEYWORD_SET(synthPackage) THEN BEGIN

           ;;Determine whether we're keeping this guy or not, for plotting purposes and for building synthetic SDT structs
           gaussDecision                         = KEYWORD_SET(KF2D__Curvefit_opt.add_gaussian_estimate) ? $
                                                   (gaussFit.fitStatus GT 0) : 0
           skipKappa                             = (kappaFit.fitStatus GT 0) OR gaussDecision

           IF ~skipKappa AND ~gotKappa THEN BEGIN
              synthKappa.data[*,iAngle,iTime]    = kappaFit.yFull
              synthKappa.energy[*,iAngle,iTime]  = kappaFit.xFull
              nGoodFits_tempK++
              good_angleBinK_i                   = [good_angleBinK_i,iAngle]
              good_kappaFits_i                   = [good_kappaFits_i,N_ELEMENTS(kappaFits)-1]

              gotKappa                           = 1
              PRINT,"Got kappa 1D fit at angle " + STRCOMPRESS(tempAngle,/REMOVE_ALL)
           ENDIF

           IF KEYWORD_SET(KF2D__Curvefit_opt.add_gaussian_estimate) THEN BEGIN
              skipGauss                             = (gaussFit.fitStatus GT 0) OR (kappaFit.fitStatus GT 0)

              IF ~skipGauss AND ~gotGauss THEN BEGIN
                 synthGauss.data[*,iAngle,iTime]    = gaussFit.yFull
                 synthGauss.energy[*,iAngle,iTime]  = gaussFit.xFull
                 nGoodFits_tempG++
                 good_angleBinG_i                   = [good_angleBinG_i,iAngle]
                 good_gaussFits_i                   = [good_gaussFits_i,N_ELEMENTS(gaussFits)-1]

                 gotGauss                           = 1
              
                 PRINT,"Got Gauss 1D fit at angle " + STRCOMPRESS(tempAngle,/REMOVE_ALL)
              ENDIF
           ENDIF
        ENDIF

        ;; IF gotKappa THEN BEGIN
        ;;    ;; PRINT,"Got our 1D fit at angle " + STRCOMPRESS(tempAngle,/REMOVE_ALL)
        ;;    IF ~gotGauss THEN PRINT,'Sorry, Gauss is missing out.'
        ;;    BREAK
        ;; ENDIF

        CASE 1 OF
           KEYWORD_SET(KF2D__Curvefit_opt.add_gaussian_estimate): BEGIN
              IF gotKappa AND gotGauss THEN BEGIN
                 gotEm                              = 1
              ENDIF
           END
           ELSE: BEGIN
              IF gotKappa THEN BEGIN
                 gotEm                              = 1
              ENDIF
           END
        ENDCASE

        IF gotEm THEN BREAK

     ENDFOR

     IF ~gotEm THEN BEGIN
        PRINT,"Couldn't muster good fits for iTime = " + STRCOMPRESS(iTime,/REMOVE_ALL) + '!!!'
        PRINT,'SKIPPING'
        CONTINUE
     ENDIF

     ;; stopMe1      = '97-02-01/09:26:13.22'
     ;; stopMe2      = '97-02-01/09:26:18.91'
     ;; sm1          = STR_TO_TIME(stopMe1)
     ;; sm2          = STR_TO_TIME(stopMe2)

     ;;orb 1789
     ;; stopMe1      = '97-02-02/21:02:20.41'
     ;; stopMe2      = '97-02-02/21:02:21.84'
     ;; sm1          = STR_TO_TIME(stopMe1)
     ;; sm2          = STR_TO_TIME(stopMe2)

     ;; IF (ABS(t-sm1) LT 0.1) OR (ABS(t-sm2) LT 0.1) THEN BEGIN

     ;;OK, now that we've got all the fits that succeeded, let's see how they do in the mosh pit
     curKappaStr  = MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(synthKappa,iTime)
     curGaussStr  = MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(synthGauss,iTime)
     curDataStr   = MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(diff_eFlux,iTime)

     ;;Units for later
     INIT_KAPPA_UNITCONV,curDataStr

     UPDATE_KAPPA_FLUX2D__HORSESHOE__BFUNC_AND_GFUNC,curDataStr, $
        ;; bestAngle_i, $
        ;; angleBin_i, $
        !NULL, $
        good_angleBinK_i[0], $
        /NORMALIZE_TO_VALS_AT_FITTED_ANGLE, $
        PEAK_ENERGY=peak_energy, $
        ITIME=iTime, $
        LOGSCALE_REDUCENEGFAC=logScale_reduceNegFac, $
        PLOT_BULKE_MODEL=plot_bulke_model, $
        ;; PLOT_BULKE_FACTOR=plot_bulke_factor, $
        ;; /PLOT_BULKE_FACTOR, $
        ;; POLARPLOT_BULKE_FACTOR=polarPlot_bulke_factor, $
        ;; /POLARPLOT_BULKE_FACTOR, $
        PLOT_MODEL_BULKE_V_DATA_COMPARISON=plot_comparison, $
        ;; PLOT_FLUX_PEAKS=plot_flux_peaks, $
        ;; /PLOT_FLUX_PEAKS, $
        PLOTDIR=KF2D__Plot_opt.plotDir, $
        ORBIT=orbit, $
        SAVE_PLOTS=save_plots
     ;; /SAVE_PLOTS

     ;; ENDIF

     ;; IF ~(ABS(t-sm1) LT 0.1) AND ~(ABS(t-sm2) LT 0.1) THEN CONTINUE

     CASE 1 OF
        KEYWORD_SET(KF2D__Curvefit_opt.add_gaussian_estimate): BEGIN
           proceed                                  = ((nGoodFits_tempK GT 0) AND $
                                                       (nGoodFits_tempG GT 0))
        END
        ELSE: BEGIN
           proceed                                  = nGoodFits_tempK GT 0
        END
     ENDCASE

     IF proceed THEN BEGIN      ;skip if we lose

        KAPPA_FIT2D__HORSESHOE, $
           keepK_iTime,iTime, $
           out_eRange_peak, $
           nEnergies, $
           nTotAngles, $
           successesK, $
           curKappaStr,kappaFits,curDataStr, $
           good_angleBinK_i, $
           hadSuccessK, $
           KFITPARAMSTRUCT=kappaParamStruct, $
           KFIT2DPARAMSTRUCT=kFit2DParamStruct, $
           FIT2D_INF_LIST=fit2DKappa_inf_list, $
           FIT2D__SHOW_AND_PROMPT__EACH_CANDIDATE=fit2d__show_each_candidate, $
           FIT2D__SAVE_ALL_CANDIDATE_PLOTS=fit2D__save_all_candidate_plots, $
           FIT2D__SHOW__IS_MAXWELLIAN_FIT=0, $
           FIT2D__SHOW__FITSTRING='Kappa', $
           PRINT_2DFITINFO=print_2DFitInfo, $
           PRINT_2DWININFO=print_2DWinInfo

        totSuccessesK += successesK

        ;; PRINT,FORMAT='("KAPPA_FIT2D__TRY_EACH SUCCESSES (TOT) : ",I0," (",I0,")")', $
        ;;       successesK, $
        ;;       totSuccessesK

        ;; kBest = fit2DKappa_inf_list[-1].bestFit1DParams.A[2]

     ENDIF ELSE BEGIN
        kBest       = 999
     ENDELSE

     proceed = proceed AND hadSuccessK

     IF proceed AND KEYWORD_SET(KF2D__Curvefit_opt.add_gaussian_estimate) THEN BEGIN

        KAPPA_FIT2D__HORSESHOE, $
           keepG_iTime,iTime, $
           out_eRange_peak, $
           nEnergies, $
           nTotAngles, $
           successesG, $
           curGaussStr,gaussFits,curDataStr, $
           good_angleBinG_i, $
           hadSuccessG, $
           KFITPARAMSTRUCT=gaussParamStruct, $
           KFIT2DPARAMSTRUCT=kFit2DParamStruct, $
           FIT2D_INF_LIST=fit2DGauss_inf_list, $
           FIT2D__SHOW_AND_PROMPT__EACH_CANDIDATE=fit2d__show_each_candidate, $
           FIT2D__SAVE_ALL_CANDIDATE_PLOTS=fit2D__save_all_candidate_plots, $
           /FIT2D__SHOW__IS_MAXWELLIAN_FIT, $
           FIT2D__SHOW__FITSTRING='Maxwell', $
           PRINT_2DFITINFO=print_2DFitInfo, $
           PRINT_2DWININFO=print_2DWinInfo
        
        totSuccessesG += successesG



        ;; PRINT,FORMAT='("GAUSS_FIT2D__TRY_EACH SUCCESSES (TOT) : ",I0," (",I0,")")', $
        ;;       successesG, $
        ;;       totSuccessesG

     ENDIF

     IF ~(KEYWORD_SET(hadSuccessK) AND KEYWORD_SET(hadSuccessG)) THEN BEGIN
        CASE (N_ELEMENTS(kappaFits) - N_ELEMENTS(fit2DKappa_inf_list)) OF
           0:                             ;excellent
           1: kappaFits = kappaFits[0:-2] ;Well... I guess we'll trim one
           ELSE: STOP
        ENDCASE

        IF KEYWORD_SET(KF2D__Curvefit_opt.add_gaussian_estimate) THEN BEGIN
           CASE (N_ELEMENTS(gaussFits) - N_ELEMENTS(fit2DGauss_inf_list)) OF
              0:                             ;excellent
              1: gaussFits = gaussFits[0:-2] ;Well... I guess we'll trim one
              ELSE: STOP
           ENDCASE
        ENDIF

     ENDIF
     ;; IF proceed AND KEYWORD_SET(fit1D__show_and_prompt) AND kBest LT 3.0 THEN BEGIN
     ;; IF proceed AND KEYWORD_SET(fit1D__show_and_prompt) THEN BEGIN
     IF 0 THEN BEGIN

        KAPPA_FIT2D__SHOW_AND_PROMPT__SELECTED2DFIT,curDataStr,fit2DKappa_inf_list[-1], $
           totSuccessesK, $
           iTime, $
           IS_MAXWELLIAN_FIT=0, $
           PROMPT__CONT_TO_NEXT_FIT=Kprompt__cont_to_next_fit, $
           PROMPT__CONT_UNTIL_FIT_EQ=Kprompt__cont_until_fit_eq, $
           FINISH_AND_SAVE_ALL=Kfinish_and_save_all, $
           KAPPA_FIT__SHOW__QUIT=Kshow__quit
        
        IF KEYWORD_SET(kCurvefit_opt.add_gaussian_estimate) THEN BEGIN
           KAPPA_FIT2D__SHOW_AND_PROMPT__SELECTED2DFIT,curDataStr,fit2DGauss_inf_list[-1], $
              totSuccessesG, $
              iTime, $
              IS_MAXWELLIAN_FIT=1, $
              PROMPT__CONT_TO_NEXT_FIT=Gprompt__cont_to_next_fit, $
              PROMPT__CONT_UNTIL_FIT_EQ=Gprompt__cont_until_fit_eq, $
              FINISH_AND_SAVE_ALL=Gfinish_and_save_all, $
              KAPPA_FIT__SHOW__QUIT=Gshow__quit
        ENDIF
     ENDIF

  ENDFOR

  IF KEYWORD_SET(synthPackage) THEN BEGIN
     
     IF KEYWORD_SET(fit1D__skip_bad_fits) THEN BEGIN
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

     synthPackage = LIST(MAKE_ARRAY_OF_SDT_STRUCTS_FROM_PREPPED_EFLUX( $
                           diff_eFlux, $
                           TIME_INDS=tmpTime_i), $
                         MAKE_ARRAY_OF_SDT_STRUCTS_FROM_PREPPED_EFLUX( $
                           synthKappa, $
                           TIME_INDS=tmpTime_i, $
                          /RECALCULATE_DDATA, $
                          APPEND_DATA_NAME=' (Kappa fits)'))

     IF KEYWORD_SET(KF2D__Curvefit_opt.add_gaussian_estimate) THEN BEGIN
        synthPackage.Add,MAKE_ARRAY_OF_SDT_STRUCTS_FROM_PREPPED_EFLUX( $
                          synthGauss, $
                          TIME_INDS=tmpTime_i, $
                          /RECALCULATE_DDATA, $
                          APPEND_DATA_NAME=' (Gauss fits)')
     ENDIF

  ENDIF
END
