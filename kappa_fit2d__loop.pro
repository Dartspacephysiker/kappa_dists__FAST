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
                      FIT1D__SAVE_PLOTSLICES=fit1D__save_plotSlices, $
                      FIT2D__SHOW_AND_PROMPT__EACH_CANDIDATE=fit2d__show_each_candidate, $
                      FIT2D__SHOW_ONLY_DATA=fit2D__show_only_data, $
                      FIT2D__PA_ZRANGE=fit2D__PA_zRange, $
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
                      TXTOUTPUTDIR=txtOutputDir, $
                      DEBUG__SKIP_TO_THIS_TIME=debug__skip_to_this_time, $
                      DEBUG__BREAK_ON_THIS_TIME=debug__break_on_this_time
  
  COMPILE_OPT idl2

  ;;For updating K_EA__gFunc,K_EA__bFunc
  @common__kappa_flux2d__horseshoe__eanisotropy.pro

  ;;Get yourself KF2D__SDTData_opt,KF2D__Curvefit_opt, etc.
  @common__kappa_fit2d_structs.pro

  ;; RESOLVE_ROUTINE,'mpfit2dfun',/IS_FUNCTION

  ;;So the order becomes [angle,energy,time] for each of these arrays
  energies                          = TRANSPOSE(diff_eFlux.energy,[1,0,2])
  data                              = TRANSPOSE(diff_eFlux.data,[1,0,2])
  ddata                             = TRANSPOSE(diff_eFlux.ddata,[1,0,2])
  oneCount_data                     = KEYWORD_SET(KF2D__Plot_opt.add_oneCount_curve) ? TRANSPOSE(dEF_oneCount.data,[1,0,2]) : !NULL
  angles                            = TRANSPOSE(diff_eFlux.theta,[1,0,2])

  nEnergies                         = N_ELEMENTS(energies[0,*,0])
  nTotAngles                        = N_ELEMENTS(angles[*,0,0])

  IF KEYWORD_SET(debug__skip_to_this_time) THEN BEGIN
     CASE SIZE(debug__skip_to_this_time,/TYPE) OF
        7: skipTime  = STR_TO_TIME(debug__skip_to_this_time)
        5: skipTime  = debug__skip_to_this_time
        ELSE: STOP
     ENDCASE
  ENDIF

  IF KEYWORD_SET(debug__break_on_this_time) THEN BEGIN
     CASE SIZE(debug__BREAK_on_this_time,/TYPE) OF
        7: breakTime = STR_TO_TIME(debug__BREAK_on_this_time)
        5: breakTime = debug__BREAK_on_this_time
        ELSE: STOP
     ENDCASE
  ENDIF

  CASE 1 OF
     KEYWORD_SET(KF2D__Curvefit_opt.fit1D__nFlux): BEGIN
        units = 'flux'
     END
     ELSE: BEGIN
        units = 'eFlux'
     END
  ENDCASE

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
  CASE 1 OF
     KEYWORD_SET(KF2D__Curvefit_opt.fit1D__sc_eSpec): BEGIN

        ;;Get energy spectrum, if that's what you're into
        eSpec = GET_EN_SPEC__FROM_DIFF_EFLUX( $
                diff_eFlux, $
                /RETRACE, $
                ANGLE=KF2D__SDTData_opt.electron_angleRange, $
                UNITS=units)

        tempAllAngles    = [0.]
        tempAngle        = 0.
        ;; junk             = MIN(ABS(AorigArr[*,*]),useTheseAnglesIndex)
        junk             = MIN(ABS(angles[*,*,0]),useTheseAnglesIndex)
        angleBin_i       = [0]
        nAngles          = 1
        nReqSCAngles     = 1

        IF KEYWORD_SET(KF2D__Plot_opt.add_oneCount_curve) THEN BEGIN
           oneCount_eSpec = GET_EN_SPEC__FROM_DIFF_EFLUX( $
                            dEF_oneCount, $
                            /RETRACE, $
                            ANGLE=KF2D__SDTData_opt.electron_angleRange, $
                            UNITS=units)
           ;; oneCountArr   = oneCount_data[*,*,iTime]
        END

        ;;Need to peel one ind off end to junk retrace
        ;; IF KEYWORD_SET(synthPackage) THEN BEGIN

        ;;    IF KEYWORD_SET(KF2D__Curvefit_opt.add_gaussian_estimate) THEN BEGIN
        ;;       synthGauss                  = diff_eFlux
        ;;       synthGauss.data[*]          = 0.0
        ;;    ENDIF
        ;; ENDIF

     END
     ELSE: BEGIN
        INIT_KAPPA_FIT2D_PRELIM_ANGLEBIN_I, $
           tempSCRange,alleyOop,nEnergies, $
           ANGLERANGE=KF2D__SDTData_opt.electron_angleRange, $
           AORIGARR=angles[*,*,0], $
           OUT_NREQ_ANGLES=nReqSCAngles, $
           OUT_USETHESEANGLESINDEX=useTheseAnglesIndex

        ;;Init loss-cone angles
        ;; INIT_KAPPA_FIT2D_PRELIM_ANGLEBIN_I, $
        ;;    tempLCRange,alleyOop,nEnergies, $
        ;;    ANGLERANGE=KF2D__SDTData_opt.electron_lca, $
        ;;    AORIGARR=angles[*,*,0], $
        ;;    OUT_NREQ_ANGLES=nReqSCAngles, $
        ;;    OUT_USETHESEANGLESINDEX=useTheseAnglesIndex
     END
  ENDCASE

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

     IF N_ELEMENTS(skipTime) GT 0 THEN BEGIN
        IF t LT skipTime THEN BEGIN
           PRINT,FORMAT='("Skipping Time  ",I0,"/",I0," : ",A0)', $
                 iTime+1, $
                 N_ELEMENTS(bounds), $
                 KF2D__strings.timeStrs[iTime]
           CONTINUE
        ENDIF
     ENDIF

     IF N_ELEMENTS(breakTime) GT 0 THEN BEGIN
        FOR jjBeak=0,N_ELEMENTS(breakTime)-1 DO BEGIN
           IF ABS(t-breakTime[jjBeak]) LT 0.5 THEN STOP
        ENDFOR
     ENDIF

     ;;And now the order becomes [angle,energy] for each of these arrays
     XorigArr         = energies[*,*,iTime]
     YorigArr         = data[*,*,iTime]
     worigArr         = ddata[*,*,iTime]
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

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Decide—energy spectrum or angle stuff?
     IF ~KEYWORD_SET(KF2D__Curvefit_opt.fit1D__sc_eSpec) THEN BEGIN

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
           IF curMinMax_i[0] EQ 0                       THEN curMinMax_i[0] += 1 ELSE curMinMax_i[0] -= 1
           IF curMinMax_i[1] GE (N_ELEMENTS(tAngles)-1) THEN curMinMax_i[1] -= 1 ELSE curMinMax_i[1] += 1
           this = MIN(ABS([tAngles[curMinMax_i[0]]-minAngle,$
                           tAngles[curMinMax_i[1]]-maxAngle]),bin_i)

           angleBin_i = [unsort_i[curMinMax_i[bin_i]],angleBin_i]


           nAngles++

        ENDWHILE

        ;; IF N_ELEMENTS(UNIQ(angleBin_i)) LT nReqSCAngles THEN STOP
        IF N_ELEMENTS(UNIQ(angleBin_i)) LT nReqSCAngles THEN PRINT,'WHOA!'

        IF KEYWORD_SET(fit1D__average_over_angleRange) THEN BEGIN
           tempY                              = TOTAL(YorigArr[angleBin_i,*],1)/DOUBLE(nAngles)
           FOR iAngle=0,nAngles-1 DO BEGIN
              YorigArr[angleBin_i[iAngle],*]  = tempY
           ENDFOR
        ENDIF

        ;;Make angles start from field aligned, go back and forth
        ;;Doesn't change which angles we use, but it does change the order
        angleBin_i = angleBin_i[SORT(ABS(tempAllAngles[angleBin_i]))]

     ENDIF

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

        CASE 1 OF
           KEYWORD_SET(KF2D__Curvefit_opt.fit1D__sc_eSpec): BEGIN
              Xorig    = REFORM(eSpec.v[iTime,*])
              Yorig    = REFORM(eSpec.y[iTime,*])
              worig    = REFORM(eSpec.yerr[iTime,*])
              Aorig    = REFORM(AorigArr[iAngle,*])
              IF KEYWORD_SET(KF2D__Plot_opt.add_oneCount_curve) THEN BEGIN
                 ;; PRINT,"Umm, how will you handle this??"
                 ;; PRINT,"just make sure below works/is companionable and compatible with sanity"
                 ;; STOP
                 oneCurve           = {x:Xorig, $
                                       y:REFORM(oneCount_eSpec.y[iTime,*]), $
                                       NAME:"One Count"}
              ENDIF
           END
           ELSE: BEGIN

              ;;Here's the data we're working with for this loop iteration
              Xorig              = REFORM(XorigArr[iAngle,*])
              Yorig              = REFORM(YorigArr[iAngle,*])
              worig              = REFORM(worigArr[iAngle,*])
              Aorig              = REFORM(AorigArr[iAngle,*])

              tempAngle          = tempAllAngles[iAngle]
              tempAngleEstRange  = [tempAngle-1.0,tempAngle+1.0]

              IF KEYWORD_SET(KF2D__Plot_opt.add_oneCount_curve) THEN BEGIN
                 oneCurve           = {x:Xorig, $
                                       y:REFORM(oneCountArr[iAngle,*]), $
                                       NAME:"One Count"}
              ENDIF

           END
        ENDCASE

        KAPPA__GET_PEAK_IND_AND_PEAK_ENERGY, $
           Xorig,Yorig, $
           peak_ind,peak_energy, $
           BULK_OFFSET=KF2D__Curvefit_opt.bulk_offset, $
           CHECK_FOR_HIGHER_FLUX_PEAKS=check_higher_peaks_set_peakEn, $
           MIN_PEAK_ENERGY=KF2D__Curvefit_opt.min_peak_energy, $
           MAX_PEAK_ENERGY=TAG_EXIST(KF2D__Curvefit_opt,'max_peak_energy') ? KF2D__Curvefit_opt.max_peak_energy : !NULL, $
           PEAK_ENERGY__START_AT_HIGHE=KF2D__Curvefit_opt.peak_energy__start_at_highE, $
           ONECOUNT_STR=oneCurve
        
        IF peak_energy EQ -1 THEN CONTINUE

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
                                  ;; DONT_TAKE_STOCK_OF_BULKANGLE=dont_take_stock_of_bulkangle, $
                                  ADD_GAUSSIAN_ESTIMATE=KF2D__Curvefit_opt.add_gaussian_estimate, $
                                  USE_SDT_GAUSSIAN_FIT=KF2D__Curvefit_opt.use_SDT_Gaussian_fit, $
                                  ESTFACS=estFacs, $
                                  A_OUT=A, $
                                  AGAUSS_OUT=AGauss, $
                                  DONT_PRINT_ESTIMATES=dont_print_estimates, $
                                  /TEST_NOREV, $
                                  UNITS=units

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
                                       YORIG_ERROR=worig, $
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
                                       FIT_FAIL__USER_PROMPT=fit1D_fail__user_prompt, $
                                       UNITS=units

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
                              FIT_FAIL__USER_PROMPT=fit1D_fail__user_prompt, $
                              UNITS=units

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
              CASE 1 OF
                 KEYWORD_SET(KF2D__Curvefit_opt.fit1D__sc_eSpec): BEGIN
                    synthKappa.data[*,iAngle,iTime]    = [0,kappaFit.yFull]
                    synthKappa.energy[*,iAngle,iTime]  = [MAX(XorigArr[iAngle,*]),kappaFit.xFull]
                 END
                 ELSE: BEGIN
                    synthKappa.data[*,iAngle,iTime]    = kappaFit.yFull
                    synthKappa.energy[*,iAngle,iTime]  = kappaFit.xFull
                 END
              ENDCASE
              nGoodFits_tempK++
              good_angleBinK_i                   = [good_angleBinK_i,iAngle]
              good_kappaFits_i                   = [good_kappaFits_i,N_ELEMENTS(kappaFits)-1]

              gotKappa                           = 1
              PRINT,"Got kappa 1D fit at angle " + STRCOMPRESS(tempAngle,/REMOVE_ALL)
           ENDIF

           IF KEYWORD_SET(KF2D__Curvefit_opt.add_gaussian_estimate) THEN BEGIN
              skipGauss                             = (gaussFit.fitStatus GT 0) OR (kappaFit.fitStatus GT 0)

              IF ~skipGauss AND ~gotGauss THEN BEGIN
                 CASE 1 OF
                    KEYWORD_SET(KF2D__Curvefit_opt.fit1D__sc_eSpec): BEGIN
                       synthGauss.data[*,iAngle,iTime]    = [0,gaussFit.yFull]
                       synthGauss.energy[*,iAngle,iTime]  = [MAX(XorigArr[iAngle,*]),gaussFit.xFull]
                    END
                    ELSE: BEGIN
                       synthGauss.data[*,iAngle,iTime]    = gaussFit.yFull
                       synthGauss.energy[*,iAngle,iTime]  = gaussFit.xFull
                    END
                 ENDCASE
                 ;; synthGauss.data[*,iAngle,iTime]    = gaussFit.yFull
                 ;; synthGauss.energy[*,iAngle,iTime]  = gaussFit.xFull
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

     IF KEYWORD_SET(KF2D__Curvefit_opt.only_1D_fits) THEN BEGIN
        hadSuccessK  = gotKappa
        hadSuccessG  = gotGauss
     ENDIF ELSE BEGIN
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

        IF proceed THEN BEGIN   ;skip if we lose

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
              FIT2D__SHOW_ONLY_DATA=fit2D__show_only_data, $
              FIT2D__PA_ZRANGE=fit2D__PA_zRange, $
              FIT2D__SAVE_ALL_CANDIDATE_PLOTS=fit2D__save_all_candidate_plots, $
              FIT2D__SHOW__IS_MAXWELLIAN_FIT=0, $
              FIT2D__SHOW__FITSTRING='Kappa', $
              PRINT_2DFITINFO=print_2DFitInfo, $
              PRINT_2DWININFO=print_2DWinInfo, $
              UNITS=units

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
              FIT2D__SHOW_AND_PROMPT__EACH_CANDIDATE=KEYWORD_SET(fit2d__show_each_candidate) AND ~KEYWORD_SET(fit2D__show_only_data), $
              FIT2D__PA_ZRANGE=fit2D__PA_zRange, $
              FIT2D__SAVE_ALL_CANDIDATE_PLOTS=fit2D__save_all_candidate_plots, $
              /FIT2D__SHOW__IS_MAXWELLIAN_FIT, $
              FIT2D__SHOW__FITSTRING='Maxwell', $
              PRINT_2DFITINFO=print_2DFitInfo, $
              PRINT_2DWININFO=print_2DWinInfo, $
              UNITS=units
           
           totSuccessesG += successesG



           ;; PRINT,FORMAT='("GAUSS_FIT2D__TRY_EACH SUCCESSES (TOT) : ",I0," (",I0,")")', $
           ;;       successesG, $
           ;;       totSuccessesG

        ENDIF

        IF ~(KEYWORD_SET(hadSuccessK) AND KEYWORD_SET(hadSuccessG)) THEN BEGIN
           CASE (N_ELEMENTS(kappaFits) - N_ELEMENTS(fit2DKappa_inf_list)) OF
              0:                ;excellent
              1: BEGIN
                 IF N_ELEMENTS(kappaFits) GT 1 THEN BEGIN
                    kappaFits = kappaFits[0:-2] ;Well... I guess we'll trim one
                 ENDIF ELSE BEGIN
                    kappaFits = !NULL
                 ENDELSE
              END
              ELSE: STOP
           ENDCASE

           IF KEYWORD_SET(KF2D__Curvefit_opt.add_gaussian_estimate) THEN BEGIN
              CASE (N_ELEMENTS(gaussFits) - N_ELEMENTS(fit2DGauss_inf_list)) OF
                 0:             ;excellent
                 1: BEGIN
                    IF N_ELEMENTS(gaussFits) GT 1 THEN BEGIN
                       gaussFits = gaussFits[0:-2] ;Well... I guess we'll trim one
                    ENDIF ELSE BEGIN
                       gaussFits = !NULL
                    ENDELSE
                 END
                 ELSE: STOP
              ENDCASE
           ENDIF

        ENDIF

     ENDELSE

     IF KEYWORD_SET(fit1D__save_plotSlices) AND $
        N_ELEMENTS(kappaFits) GT 0 AND $
        hadSuccessK THEN BEGIN
        PLOT_KAPPA_FITS,orig,kappaFits[-1], $
                        KEYWORD_SET(KF2D__Curvefit_opt.add_gaussian_estimate) ? $
                        gaussFits[-1] : $
                        !NULL, $
                        oneCurve, $
                        ;; TITLE=title, $
                        BOUNDS_I=iTime, $
                        XRANGE=xRange, $
                        YRANGE=yRange, $
                        XLOG=xLog, $
                        YLOG=yLog, $
                        STRINGS=KF2D__strings, $
                        ADD_GAUSSIAN_ESTIMATE=KEYWORD_SET(KF2D__Curvefit_opt.add_gaussian_estimate), $
                        /ADD_FITPARAMS_TEXT, $
                        ;; /ADD_ANGLE_LABEL, $
                        ;; ADD_ANGLE_LABEL=KEYWORD_SET(KF2D__Curvefit_opt.fit1D__sc_eSpec) ? MEAN(KF2D__SDTData_opt.electron_angleRange) : , $
                        ADD_ANGLE_LABEL=MEAN(KF2D__SDTData_opt.electron_angleRange), $
                        /ADD_CHI_VALUE, $
                        ADD_WINTITLE=add_winTitle, $
                        /SAVE_FITPLOTS, $
                        /PLOT_FULL_FIT, $
                        ;; SKIP_BAD_FITS=skip_bad_fits, $
                        USING_SDT_DATA=using_SDT_data, $
                        ;; VERTICAL_LINES=vertical_lines, $
                        /VERTICAL_LINES, $
                        ;; PLOT_SAVENAME=plotSN, $
                        /USE_PSYM_FOR_DATA, $
                        PLOTDIR=plotDir, $
                        /POSTSCRIPT, $
                        ;; OUT_WINDOWARR=windowArr, $
                        /BUFFER, $
                        UNITS=units
     ENDIF

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

  IF KEYWORD_SET(synthPackage) AND ~KEYWORD_SET(KF2D__Curvefit_opt.only_1D_fits) THEN BEGIN
     
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

  PRINT,"Fini"
  
END
