;2018/03/07
;;Fit DMSP obs with kappa dists!
PRO JOURNAL__20180307__BADDELEY_ET_AL_2017__FIT_KAPPAS, $
   ALSO_AACGM=also_AACGM, $
   PS=ps, $
   EPS=eps, $
   OUTPLOTNAME=outPlotName

  COMPILE_OPT IDL2,STRICTARRSUBS

  dir  = '/SPENCEdata/Research/database/DMSP/20071227/'
  file = 'dms_20071227_16e.001__Baddeley_et_al_2007.sav'
  file2 = 'dms_20071227_16s1.001__Baddeley_et_al_2007.sav'

  ;; t1 = S2T('2007-12-27/16:05:20')
  ;; t1 = S2T('2007-12-27/16:06:02')
  ;; t1 = S2T('2007-12-27/16:06:45')
  ;; t2 = S2T('2007-12-27/16:07:20')
  
  ;; center lil' peak thing
  min_peak_energy = 700
  t1 = S2T('2007-12-27/16:06:20')
  t2 = S2T('2007-12-27/16:06:35')

  min_peak_energy = 1000
  t1 = S2T('2007-12-27/16:06:35')
  t2 = S2T('2007-12-27/16:06:50')

  fit1D__save_plotSlices = 1

  IF FILE_TEST(dir+file) THEN BEGIN

     PRINT,"Restoring " + file + ' ...'
     RESTORE,dir+file
     
  ENDIF ELSE BEGIN

     PRINT,"USE JOURNAL__20180307__BADDELEY_ET_AL_2017__READ_HDF5_FILE__MAKE_IDL_SAV!"
     RETURN

  ENDELSE

  ;; Look out, this is going to shrink the dmsp struct
  @kappa_fit2d__quickinit_for_dmsp.pro

  times          = dmsp.time
  fit1Denergies  = REFORM(dmsp.ch_energy[0,*])
  nEnergies      = N_ELEMENTS(fit1Denergies)
  avgFactorArr   = REPLICATE(1.,N_ELEMENTS(times))

  IF FILE_TEST(dir+file2) THEN BEGIN
     RESTORE,dir+file2
     matchie = VALUE_CLOSEST2(dmsp2.time,dmsp.time,/CONSTRAINED)
  ENDIF

  IF KEYWORD_SET(dmsp) THEN BEGIN
     diff_eFlux = {energy: dmsp.ch_energy, $
                   data  : dmsp.el_d_flux}

     energies   = fit1DEnergies
     data       = diff_eFlux.data
  ENDIF ELSE BEGIN

     energies       = TRANSPOSE(diff_eFlux.energy,[1,0,2])
     data           = TRANSPOSE(diff_eFlux.data,[1,0,2])
     ddata          = TRANSPOSE(diff_eFlux.ddata,[1,0,2])
     oneCount_data  = (KEYWORD_SET(KF2D__Plot_opt.add_oneCount_curve) ? $
                       TRANSPOSE(dEF_oneCount.data,[1,0,2])           : $
                       !NULL)
     angles         = TRANSPOSE(diff_eFlux.theta,[1,0,2])

  ENDELSE
  ;; IF KEYWORD_SET(debug__skip_to_this_time) THEN BEGIN
  ;;    CASE SIZE(debug__skip_to_this_time,/TYPE) OF
  ;;       7: skipTime  = STR_TO_TIME(debug__skip_to_this_time)
  ;;       5: skipTime  = debug__skip_to_this_time
  ;;       ELSE: STOP
  ;;    ENDCASE
  ;; ENDIF

  ;; IF KEYWORD_SET(debug__break_on_this_time) THEN BEGIN
  ;;    CASE SIZE(debug__BREAK_on_this_time,/TYPE) OF
  ;;       7: breakTime = STR_TO_TIME(debug__BREAK_on_this_time)
  ;;       5: breakTime = debug__BREAK_on_this_time
  ;;       ELSE: STOP
  ;;    ENDCASE
  ;; ENDIF

  CASE 1 OF
     KEYWORD_SET(KF2D__Curvefit_opt.fit1D__nFlux): BEGIN
        units1D = 'flux'
        eSpecUnits = 'flux'
     END
     KEYWORD_SET(KF2D__Curvefit_opt.fit__JE_over_E): BEGIN
        units1D = 'je_over_E'
        eSpecUnits = 'flux'
     END
     ELSE: BEGIN
        units1D = 'eFlux'
        eSpecUnits = 'eFlux'
     END
  ENDCASE

  ;; synthKappa                     = diff_eFlux
  ;; synthKappa.data[*]             = 0.0

  ;;Init source-cone angles
  ;;Init angles we want to hang around
  ;; INIT_KAPPA_FIT2D_PRELIM_ANGLEBIN_I, $
  ;;    tempSCRange,alleyOop,nEnergies, $
  ;;    ANGLERANGE=KF2D__SDTData_opt.electron_angleRange, $
  ;;    AORIGARR=angles[*,*,0], $
  ;;    OUT_NREQ_ANGLES=nReqSCAngles, $
  ;;    OUT_USEANGLES__ENERGYIND=useAngles__energyInd

  CASE 1 OF
     KEYWORD_SET(KF2D__Curvefit_opt.fit1D__sc_eSpec): BEGIN

        ;;Get energy spectrum, if that's what you're into

        eSpec = {x:dmsp.time, $
                 y:(KEYWORD_SET(fit1D__nFlux) ? dmsp.el_d_flux : dmsp.el_d_ener), $
                 v:dmsp.ch_energy}

        angleBin_i       = 0

        nAngles          = 1
        nReqSCAngles     = 1

        ;; IF KEYWORD_SET(KF2D__Plot_opt.add_oneCount_curve) THEN BEGIN
        ;;    oneCount_eSpec = GET_EN_SPEC__FROM_DIFF_EFLUX( $
        ;;                     dEF_oneCount, $
        ;;                     /RETRACE, $
        ;;                     ANGLE=KF2D__SDTData_opt.electron_angleRange, $
        ;;                     UNITS=eSpecUnits)
        ;; END


     END
     ELSE: BEGIN

     END
  ENDCASE

  ;;Init fitParams structs
  CASE 1 OF
     KEYWORD_SET(KF2D__Curvefit_opt.use_mpFit1D): BEGIN
        ;;Vary bulk E [0], Temperature [1], kappa [2], and density [3] (but not angle)
        kappa_fixA        = [KF2D__Curvefit_opt.fit__JE_over_E, $          ;Vary bulk E [0] 
                             KF2D__Curvefit_opt.fit1D__clampTemperature, $ ;Temperature [1] (maybe)
                             0, $                                          ;kappa       [2]
                             KF2D__CurveFit_opt.fit1D__clampDensity    , $ ;and density [3] (but not angle)
                             1] 
        
        gauss_fixA        = [KF2D__Curvefit_opt.fit__JE_over_E, $          ;Vary bulk E [0]
                             KF2D__Curvefit_opt.fit1D__clampTemperature, $ ;Temperature [1] (maybe)
                             1, $
                             KF2D__CurveFit_opt.fit1D__clampDensity    , $ ;and density [3] (not kappa or angle)
                             1]
        ATmp              = DOUBLE([1e3,100.,3.0,0.01,0])

        explicit_derivatives = KEYWORD_SET(KF2D__Curvefit_opt.fit__JE_over_E)
        kappaParamStruct  = INIT_KAPPA_FITPARAM_INFO(ATmp,kappa_fixA, $
                                                    EXPLICIT_DERIVATIVES=explicit_derivatives)

        gaussParamStruct  = INIT_KAPPA_FITPARAM_INFO(TEMPORARY(ATmp),gauss_fixA, $
                                                     EXPLICIT_DERIVATIVES=explicit_derivatives)
     END
     ELSE: BEGIN
     END
  ENDCASE

  ;;xRange (energy range)

  ;;Some plot things
  ;; xRange                                      = [MIN(Xorig[WHERE(Xorig GT 0)]),MAX(Xorig)]
  ;; xRange                                      = [MIN(Xorig[WHERE(Xorig GT 0)]),defXRange[1]]

  IF KEYWORD_SET(KF2D__Plot_opt.add_oneCount_curve) THEN BEGIN
     yMin                      = MIN(oneCount_data[WHERE(oneCount_data GT 0)])
     yMin                      = 10.^(FLOOR(ALOG10(yMin)))
  ENDIF ELSE BEGIN
     yMin                      = MIN(data[WHERE(data GT 0)])
  ENDELSE

  xRange                                      = [MIN(fit1Denergies[WHERE(fit1Denergies GT 0)]),MAX(fit1Denergies)*1.05]
  yRange                                      = [yMin,MAX(data)]

  SET_PLOT_DIR,plotDir, $
               FOR_SDT=~KEYWORD_SET(dmsp), $
               FOR_KAPPA_DB=KEYWORD_SET(dmsp), $
               /ADD_TODAY, $
               ADD_SUFF=KEYWORD_SET(dmsp) ? '/dmsp' : !NULL

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
        FOR jjBleakerStreet=0,N_ELEMENTS(breakTime)-1 DO BEGIN
           IF ABS(t-breakTime[jjBleakerStreet]) LT 0.5 THEN STOP
        ENDFOR
     ENDIF

     ;;And now the order becomes [angle,energy] for each of these arrays

     IF ~KEYWORD_SET(dmsp) THEN BEGIN
        XorigArr         = energies[*,*,iTime]
        YorigArr         = data[*,*,iTime]
        worigArr         = ddata[*,*,iTime]
        IF KEYWORD_SET(KF2D__Plot_opt.add_oneCount_curve) THEN BEGIN
           oneCountArr   = oneCount_data[*,*,iTime]
        END
        AorigArr         = angles[*,*,iTime]
     ENDIF
     

     ;; IF KEYWORD_SET(KF2D__SDTData_opt.manual_angle_correction) THEN BEGIN
     ;;    junk          = MIN(ABS(KF2D__SDTData_opt.manual_angle_correction-AorigArr[*,nEnergies/2]),minInd)
     ;;    junk          = MIN(ABS(AorigArr[*,nEnergies/2]),zeroInd)
     ;;    shiftInd      = (minInd-zeroInd) MOD nTotAngles
     ;;    AorigArr      = SHIFT(AorigArr,shiftInd,0)
     ;; ENDIF

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Decide—energy spectrum or angle stuff?
     IF ~KEYWORD_SET(KF2D__Curvefit_opt.fit1D__sc_eSpec) THEN BEGIN

        KAPPA_LOOP__GET_ANGLES,AorigArr, $
                               YorigArr, $
                               useAngles__energyInd, $
                               tempSCRange, $
                               alleyOop, $
                               nReqSCAngles, $
                               FIT1D__AVERAGE_OVER_ANGLERANGE=fit1D__average_over_angleRange, $
                               FIT1D__USING_SC_SPEC=KF2D__Curvefit_opt.fit1D__sc_eSpec, $
                               OUT_ANGLEBIN_I=angleBin_i, $
                               OUT_TMPUSEANGLES=tmpUseAngles

     ENDIF

     ;;Order of dat.data is [energy,angle] when coming from SDT
     IF ~KEYWORD_SET(dmsp) THEN curDataStr = MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(diff_eFlux,iTime) ;; , $
                                                                ;; UNITS=units2D)

     nGoodFits_tempK       = 0
     nGoodFits_tempG       = 0
     good_angleBinK_i      = !NULL
     good_angleBinG_i      = !NULL
     ;; good_1DkappaFits_i    = !NULL
     ;; good_1DgaussFits_i    = !NULL

     gotKappa              = 0
     gotGauss              = 0
     gotEm                 = 0

     shoutOut              = 1
     FOR iiAngle=0,nAngles-1 DO BEGIN ;if using eSpec, there is only one (averaged-over) "angle"

        IF ~KEYWORD_SET(dmsp) THEN BEGIN
           iAngle             = angleBin_i[iiAngle]
           tempAngle          = tmpUseAngles[iAngle]
        ENDIF

        CASE 1 OF
           KEYWORD_SET(KF2D__Curvefit_opt.fit1D__sc_eSpec): BEGIN
              Xorig    = REFORM(eSpec.v[iTime,*])
              Yorig    = REFORM(eSpec.y[iTime,*])
              IF ~KEYWORD_SET(dmsp) THEN worig    = REFORM(eSpec.yerr[iTime,*])
              IF ~KEYWORD_SET(dmsp) THEN Aorig    = REFORM(AorigArr[iAngle,*])
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
           NENERGIES=nEnergies, $
           MAXEIND=maxEInd, $
           MINEIND=minEInd, $
           ENERGY_INDS=energy_inds, $
           ERANGE_FIT=eRange_fit, $
           N_BELOW_PEAK=KF2D__Curvefit_opt.n_below_peak, $
           N_ABOVE_PEAK=KF2D__Curvefit_opt.n_above_peak, $
           BULK_OFFSET=KF2D__Curvefit_opt.bulk_offset, $
           CHECK_FOR_HIGHER_FLUX_PEAKS=check_higher_peaks_set_peakEn, $
           MIN_PEAK_ENERGY=KF2D__Curvefit_opt.min_peak_energy, $
           MAX_PEAK_ENERGY=TAG_EXIST(KF2D__Curvefit_opt,'max_peak_energy') ? KF2D__Curvefit_opt.max_peak_energy : !NULL, $
           PEAK_ENERGY__START_AT_HIGHE=KF2D__Curvefit_opt.peak_energy__start_at_highE, $
           /CONTINUE_IF_NOMATCH, $
           TEST_NOREV=~KEYWORD_SET(dmsp), $
           ;; /TEST_NOREV, $
           FOR_DMSP=KEYWORD_SET(dmsp), $
           ONECOUNT_STR=oneCurve
        
        IF peak_energy EQ -1 THEN CONTINUE

        ;;estimate from the data!
        IF KEYWORD_SET(KF2D__Curvefit_opt.estimate_A_from_data) THEN BEGIN 

           KAPPA__GET_A_ESTIMATES,curDataStr,Xorig,Yorig, $
                                  peak_ind,peak_energy, $
                                  MAXEIND=maxEInd, $
                                  MINEIND=minEInd, $
                                  ERANGE_FIT=eRange_fit, $
                                  KAPPA_EST=KF2D__Curvefit_opt.fitA[2], $
                                  ;; MASS=mass, $
                                  E_ANGLE=KF2D__SDTData_opt.electron_angleRange, $
                                  ANGLES=tempAngleEstRange, $
                                  ;; N_ANGLES_IN_RANGE=nAngles, $
                                  ;; BULKANGLE_STRUCT=angleStr, $
                                  ;; DONT_TAKE_STOCK_OF_BULKANGLE=dont_take_stock_of_bulkangle, $
                                  ADD_GAUSSIAN_ESTIMATE=KF2D__Curvefit_opt.add_gaussian_estimate, $
                                  USE_SDT_GAUSSIAN_FIT=KF2D__Curvefit_opt.use_SDT_Gaussian_fit, $
                                  ESTFACS=estFacs, $
                                  PHI__USE_ENERGY_BEFORE_PEAK=TAG_EXIST(KF2D__Curvefit_opt,'phi__use_energy_before_peak') ? KF2D__Curvefit_opt.phi__use_energy_before_peak : !NULL, $
                                  A_OUT=A, $
                                  AGAUSS_OUT=AGauss, $
                                  DONT_PRINT_ESTIMATES=dont_print_estimates, $
                                  TEMPERATURE_TYPE=KF2D__SDTData_opt.fit2D__temperature_type, $
                                  UNITS=units1D

           ;; 2018/01/10 Use bulk energy (set by PHI__USE_ENERGY_BEFORE_PEAK) for 2D bulkE initial estimate
           bulkEOrigEstimate = A[0]

        ENDIF ELSE BEGIN
           kappa       = 10
           ;; IF N_ELEMENTS(dmsp2) GT 0 THEN BEGIN
           ;;    STOP
           ;; ENDIF ELSE BEGIN
              n_est       = 0.4
           ;; ENDELSE

           A           = DOUBLE([peak_energy,T,kappa,n_est,0.000001,5.68e-6,0])
           AGauss      = A
        ENDELSE
        
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

              ;; tmpFit1Denergies = fit1denergies[0:((peak_ind+1) < (nEnergies -1))] ;for 1-D plots
              tmpFit1Denergies = fit1denergies[energy_inds[1]:energy_inds[0]:-1]
              eRange_phi       = (KEYWORD_SET(KF2D__Curvefit_opt.fit__linear_energy_shift) ? $
                                  ;; [bulkEOrigEstimate,eRange_fit[0]]: $
                                  (KEYWORD_SET(dmsp) ? $
                                   eRange_fit        : $
                                   [bulkEOrigEstimate,tmpFit1Denergies[1]] ) : $
                                  eRange_fit)

              KAPPA__GET_FITS__MPFIT1D,Xorig,Yorig, $
                                       orig,kappaFit1D,gaussFit1D, $
                                       YORIG_ERROR=worig, $
                                       KCURVEFIT_OPT=KF2D__Curvefit_opt, $
                                       KFITPARAMSTRUCT=kappaParamStruct, $
                                       GFITPARAMSTRUCT=gaussParamStruct, $
                                       ENERGY_INDS=energy_inds, $
                                       ERANGE_FIT=eRange_fit, $
                                       ERANGE_PHI=eRange_phi, $
                                       BOUNDS_I=iTime, $
                                       KAPPA_A=A, $
                                       GAUSS_A=AGauss, $
                                       KAPPA_FIXA=kappa_fixA, $
                                       GAUSS_FIXA=gauss_fixA, $
                                       YMAX=yMax, $
                                       STRINGS=KF2D__strings, $
                                       ;; OUT_FITTED_PARAMS=out_kappaParams, $
                                       ;; OUT_FITTED_GAUSS_PARAMS=out_gaussParams, $
                                       ;; OUT_KAPPAFIT1DSTRUCTS=kappaFit1Ds, $
                                       ;; OUT_GAUSSFIT1DSTRUCTS=gaussFit1Ds, $
                                       FIT__LINEAR_ENERGY_SHIFT=KF2D__Curvefit_opt.fit__linear_energy_shift, $
                                       FIT__JE_OVER_E=KF2D__Curvefit_opt.fit__JE_over_E, $                                       
                                       ;; FIT__LES__TAKE_STOCK_OF_RB=KF2D__Curvefit_opt.fit__LES__take_stock_of_RB, $
                                       ADD_FULL_FITS=tmpFit1Denergies, $
                                       ADD_ANGLESTR=angleStr, $
                                       ;; OUT_ERANGE_FIT=eRange_fitArr, $
                                       OUT_PARAMSTR=out_paramStr, $
                                       DONT_PRINT_FITINFO=dont_print_fitInfo, $
                                       FIT_FAIL__USER_PROMPT=fit1D_fail__user_prompt, $
                                       UNITS=units1D, $
                                       MASS=KEYWORD_SET(dmsp) ? 5.68566e-6 : curDataStr.mass, $
                                       AVGFACTORARR=avgFactorArr

           END
           ELSE: BEGIN
              KAPPA__GET_FITS,Xorig,Yorig, $
                              orig,kappaFit1D,gaussFit1D, $
                              ADD_GAUSSIAN_ESTIMATE=KF2D__Curvefit_opt.add_gaussian_estimate, $
                              USE_SDT_GAUSSIAN_FIT=KF2D__Curvefit_opt.use_SDT_Gaussian_fit, $
                              BOUNDS_I=iTime, $
                              ENERGY_INDS=energy_inds, $
                              ;; ERANGE_FIT=eRange_fit, $
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
                              ;; OUT_KAPPAFIT1DSTRUCTS=kappaFit1Ds, $
                              ;; OUT_GAUSSFIT1DSTRUCTS=gaussFit1Ds, $
                              /ADD_FULL_FITS, $
                              ADD_ANGLESTR=angleStr, $
                              ;; OUT_ERANGE_FIT=eRange_fitArr, $
                              OUT_PARAMSTR=out_paramStr, $
                              ;; DONT_PRINT_ESTIMATES=dont_print_estimates, $
                              DONT_PRINT_FITINFO=dont_print_fitInfo, $
                              FIT_FAIL__USER_PROMPT=fit1D_fail__user_prompt, $
                              UNITS=units1D
              
           END
        ENDCASE

        eRange_fitArr = N_ELEMENTS(eRange_fitArr) GT 0 ? $
                         [[eRange_fitArr],[eRange_fit]] : eRange_fit

        ;;Now handle the adding of kappa/gaussFit1D structs to ze lists
        ;; IF KEYWORD_SET(KF2D__Curvefit_opt.add_gaussian_estimate) THEN BEGIN

        ;;    IF ~kappaFit1D.fitStatus AND ~gaussFit1D.fitStatus THEN BEGIN

        ;;       ;; IF N_ELEMENTS(kappaFit1Ds) EQ 0 THEN BEGIN
        ;;       ;;    kappaFit1Ds    = LIST(kappaFit1D)
        ;;       ;; ENDIF ELSE BEGIN
        ;;       ;;    kappaFit1Ds.Add,kappaFit1D
        ;;       ;; ENDELSE

        ;;       ;; IF N_ELEMENTS(gaussFit1Ds) EQ 0 THEN BEGIN
        ;;       ;;    gaussFit1Ds    = LIST(gaussFit1D)
        ;;       ;; ENDIF ELSE BEGIN
        ;;       ;;    gaussFit1Ds.Add,gaussFit1D
        ;;       ;; ENDELSE

        ;;    ENDIF
        ;; ENDIF ELSE BEGIN

        ;;    ;; IF N_ELEMENTS(kappaFit1Ds) EQ 0 THEN BEGIN
        ;;    ;;    kappaFit1Ds    = LIST(kappaFit1D)
        ;;    ;; ENDIF ELSE BEGIN
        ;;    ;;    kappaFit1Ds.Add,kappaFit1D
        ;;    ;; ENDELSE

        ;; ENDELSE

        ;;Now that we've finished with all these angles, let's see about recovering them
        ;;Determine whether we're keeping this guy or not, for plotting purposes and for building synthetic SDT structs
        gaussDecision = KEYWORD_SET(KF2D__Curvefit_opt.add_gaussian_estimate) ? $
                        (gaussFit1D.fitStatus GT 0) : 0
        skipKappa     = (kappaFit1D.fitStatus GT 0) OR gaussDecision

        IF ~skipKappa AND ~gotKappa THEN BEGIN

           nGoodFits_tempK++
           IF ~KEYWORD_SET(dmsp) THEN good_angleBinK_i   = [good_angleBinK_i,iAngle]
           ;; good_1DkappaFits_i = [good_1DkappaFits_i,N_ELEMENTS(kappaFit1Ds)-1]

           gotKappa           = 1
           IF KEYWORD_SET(dmsp) THEN $
              PRINT,FORMAT='("Got kappa 1D fit")' $
           ELSE $
              PRINT,FORMAT='("Got kappa 1D fit at angle ",F0.2)',tempAngle

           ;; CASE 1 OF
           ;;    KEYWORD_SET(KF2D__Curvefit_opt.fit1D__sc_eSpec): BEGIN
           ;;       ;; synthKappa.data[*,iAngle,iTime]    = [0,kappaFit1D.yFull]
           ;;       ;; synthKappa.energy[*,iAngle,iTime]  = [MAX(XorigArr[iAngle,*]),kappaFit1D.xFull]
           ;;       synthKappa.data[*,iAngle,iTime]    = kappaFit1D.yFull
           ;;       synthKappa.energy[*,iAngle,iTime]  = kappaFit1D.xFull
           ;;    END
           ;;    ELSE: BEGIN
           ;;       synthKappa.data[*,iAngle,iTime]    = kappaFit1D.yFull
           ;;       synthKappa.energy[*,iAngle,iTime]  = kappaFit1D.xFull
           ;;    END
           ;; ENDCASE
        ENDIF

        IF KEYWORD_SET(KF2D__Curvefit_opt.add_gaussian_estimate) THEN BEGIN
           skipGauss        = (gaussFit1D.fitStatus GT 0) OR (kappaFit1D.fitStatus GT 0)

           IF ~skipGauss AND ~gotGauss THEN BEGIN

              nGoodFits_tempG++
              IF ~KEYWORD_SET(dmsp) THEN good_angleBinG_i   = [good_angleBinG_i,iAngle]
              ;; good_1DgaussFits_i = [good_1DgaussFits_i,N_ELEMENTS(gaussFit1Ds)-1]

              gotGauss           = 1

           IF KEYWORD_SET(dmsp) THEN $
              PRINT,FORMAT='("Got Gauss 1D fit")' $
           ELSE $
              PRINT,FORMAT='("Got Gauss 1D fit at angle ",F0.2)',tempAngle


              ;; CASE 1 OF
              ;;    KEYWORD_SET(KF2D__Curvefit_opt.fit1D__sc_eSpec): BEGIN
              ;;       ;; synthGauss.data[*,iAngle,iTime]    = [0,gaussFit1D.yFull]
              ;;       ;; synthGauss.energy[*,iAngle,iTime]  = [MAX(XorigArr[iAngle,*]),gaussFit1D.xFull]
              ;;       synthGauss.data[*,iAngle,iTime]    = gaussFit1D.yFull
              ;;       synthGauss.energy[*,iAngle,iTime]  = gaussFit1D.xFull
              ;;    END
              ;;    ELSE: BEGIN
              ;;       synthGauss.data[*,iAngle,iTime]    = gaussFit1D.yFull
              ;;       synthGauss.energy[*,iAngle,iTime]  = gaussFit1D.xFull
              ;;    END
              ;; ENDCASE
              ;; synthGauss.data[*,iAngle,iTime]    = gaussFit1D.yFull
              ;; synthGauss.energy[*,iAngle,iTime]  = gaussFit1D.xFull
           ENDIF
        ENDIF

        ;; IF gotKappa THEN BEGIN
        ;;    ;; PRINT,"Got our 1D fit at angle " + STRCOMPRESS(tempAngle,/REMOVE_ALL)
        ;;    IF ~gotGauss THEN PRINT,'Sorry, Gauss is missing out.'
        ;;    BREAK
        ;; ENDIF

        gotEm = (KEYWORD_SET(KF2D__Curvefit_opt.add_gaussian_estimate) ? $
                 gotKappa AND gotGauss                                 : $
                 gotKappa)

        IF gotEm THEN BREAK

     ENDFOR

     IF KEYWORD_SET(fit2D__show_only_data) AND KEYWORD_SET(fit2D__save_all_plots) THEN BEGIN

        KAPPA_FIT2D__SHOW_AND_PROMPT__EACH_CANDIDATE, $
           MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(diff_eFlux,iTime), $
           tmp2DInfoStruct, $
           TIMEFNSTR=KF2D__strings.timeFNStrs[iTime], $
           /FOR_HORSESHOE_FIT, $
           /ONLY_DATA, $
           IS_MAXWELLIAN_FIT=is_Maxwellian_fit, $
           PROMPT__CONT_TO_NEXT_FIT=prompt__cont_to_next_fit, $
           PROMPT__CONT_UNTIL_FIT_EQ=prompt__cont_until_fit_eq, $
           PROMPT__NTOT2DFITS=totSuccessesK, $
           /FINISH_AND_SAVE_ALL, $
           KAPPA_FIT__SHOW__QUIT=show__quit, $
           FIT2D__PA_ZRANGE=fit2D__PA_zRange, $
           EPS=eps

     ENDIF 

     IF ~TEMPORARY(gotEm) THEN BEGIN
        PRINT,"Couldn't muster good fits for iTime = " + STRCOMPRESS(iTime,/REMOVE_ALL) + '!!!'
        PRINT,'SKIPPING'
        CONTINUE
     ENDIF

     IF KEYWORD_SET(KF2D__Curvefit_opt.only_1D_fits) THEN BEGIN
        hadSuccessK  = KEYWORD_SET(gotKappa)
        hadSuccessG  = KEYWORD_SET(gotGauss)

        IF hadSuccessK THEN BEGIN
           IF N_ELEMENTS(kappaFit1Ds) EQ 0 THEN BEGIN
              kappaFit1Ds    = LIST(TEMPORARY(kappaFit1D))
           ENDIF ELSE BEGIN
              kappaFit1Ds.Add,TEMPORARY(kappaFit1D)
           ENDELSE
        ENDIF

        IF hadSuccessG THEN BEGIN
           IF N_ELEMENTS(gaussFit1Ds) EQ 0 THEN BEGIN
              gaussFit1Ds    = LIST(TEMPORARY(gaussFit1D))
           ENDIF ELSE BEGIN
              gaussFit1Ds.Add,TEMPORARY(gaussFit1D)
           ENDELSE
        ENDIF


     ENDIF ELSE BEGIN
        ;;OK, now that we've got all the fits that succeeded, let's see how they do in the mosh pit
        ;; curDataStr   = MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(diff_eFlux,iTime)
        curKappaStr  = MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(synthKappa,iTime)
        ;; curGaussStr  = MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(synthGauss,iTime)
        curGaussStr  = curKappaStr
        
        ;; CASE 1 OF
        ;;    KEYWORD_SET(KF2D__Curvefit_opt.add_gaussian_estimate): BEGIN
        ;;       proceed  = ((nGoodFits_tempK GT 0) AND $
        ;;                   (nGoodFits_tempG GT 0))
        ;;    END
        ;;    ELSE: BEGIN
        ;;       proceed  = nGoodFits_tempK GT 0
        ;;    END
        ;; ENDCASE

        IF (KEYWORD_SET(KF2D__Curvefit_opt.add_gaussian_estimate) ? $
            ((nGoodFits_tempK GT 0) AND $
             (nGoodFits_tempG GT 0) )                            : $
            (nGoodFits_tempK GT 0)) THEN BEGIN ;skip if we lose

           shiftTheta   = 0 ;Not clear why shift is necessary, but makes things come out right
                                ;2017/12/27 Not sure why I thought this
           hadSuccessK  = 0
           hadSuccessG  = 0

           ;;2018/01/10 Try the whole "energy right below peak" thing
           kappaParamStruct[0].value = bulkEOrigEstimate
           kappaParamStruct[0].limits[0] = bulkEOrigEstimate

           KAPPA_FIT2D__FIREINTHEHOLE, $
              curDataStr, $
              hadSuccessK, $
              hadSuccessG, $
              CURKAPPASTR=curKappaStr, $
              CURGAUSSSTR=curGaussStr, $
              KAPPAFITANGLE_INDEX=good_angleBinK_i[-1], $
              GAUSSFITANGLE_INDEX=good_angleBinG_i[-1], $
              ;; KEEPKAPPA_INDICES=keepK_iTime, $
              ;; KEEPGAUSS_INDICES=keepG_iTime, $
              TIMEFNSTR=KF2D__strings.timeFNStrs[iTime], $
              UNITS=units2D, $
              SHIFTTHETA=shiftTheta, $
              PEAK_ENERGY=peak_energy, $
              ERANGE_FIT=eRange_fitArr[*,-1], $
              EXTEND_FITSTRUCT_ERANGE=extend_fitStruct_eRange, $
              /MAKE_FIT2D_INFO, $
              /BF_GF__NORMALIZE_TO_VALS_AT_FITTED_ANGLE, $
              BF_GF__LOGSCALE_REDUCENEGFAC=bF_gF__logScale_reduceNegFac, $
              BF_GF__PLOT_BULKE_MODEL=bF_gF__plot_bulke_model, $
              BF_GF__PLOT_MODEL_BULKE_V_DATA_COMPARISON=bF_gF__plot_model_bulkE_v_data_comparison, $
              MAKE_BFUNC_GFUNC_PLOTS=make_bFunc_gFunc_plots, $
              SAVE_BFUNC_GFUNC_PLOTS=save_bFunc_gFunc_plots, $
              PLOTDIR=plotDir, $
              ORBIT=orbit, $
              OUT_ESTIMATED_LC=estimated_lc, $
              KAPPAPARAMSTRUCT=kappaParamStruct, $
              GAUSSPARAMSTRUCT=gaussParamStruct, $
              ;; FIT2DPARAMSTRUCT=fit2DParamStruct, $
              FIT2DKAPPA_INF_LIST=fit2DKappa_inf_list, $
              FIT2DGAUSS_INF_LIST=fit2DGauss_inf_list, $
              OPTIONAL__KAPPAFIT1D=kappaFit1D, $
              OPTIONAL__GAUSSFIT1D=gaussFit1D, $
              FIT2D__SHOW_AND_PROMPT__EACH_CANDIDATE=fit2d__show_each_candidate, $
              FIT2D__SHOW_ONLY_DATA=fit2D__show_only_data, $
              FIT2D__PA_ZRANGE=fit2D__PA_zRange, $
              FIT2D__SAVE_ALL_PLOTS=fit2D__save_all_plots, $
              PRINT_2DFITINFO=print_2DFitInfo, $
              EPS=eps

           IF hadSuccessK THEN BEGIN

              keepK_iTime  = N_ELEMENTS(keepK_iTime) GT 0 ? [keepK_iTime,iTime] : iTime
              ;; synthKappa[iTime] = TEMPORARY(curKappaStr)
              ;; synthKappa[iTime] = fit2DKappa_inf_list[-1].SDT

              successesK++
              totSuccessesK++

              IF N_ELEMENTS(kappaFit1Ds) EQ 0 THEN BEGIN
                 kappaFit1Ds    = LIST(TEMPORARY(kappaFit1D))
              ENDIF ELSE BEGIN
                 kappaFit1Ds.Add,TEMPORARY(kappaFit1D)
              ENDELSE

           ENDIF

           ;; CASE 1 OF
           IF KEYWORD_SET(KF2D__Curvefit_opt.add_gaussian_estimate) THEN IF hadSuccessG THEN BEGIN
              keepG_iTime  = N_ELEMENTS(keepG_iTime) GT 0 ? [keepG_iTime,iTime] : iTime
              ;; synthGauss[iTime] = TEMPORARY(curGaussStr)
              ;; synthGauss[iTime] = fit2DGauss_inf_list[-1].SDT

              successesG++
              totSuccessesG++

              IF N_ELEMENTS(gaussFit1Ds) EQ 0 THEN BEGIN
                 gaussFit1Ds    = LIST(TEMPORARY(gaussFit1D))
              ENDIF ELSE BEGIN
                 gaussFit1Ds.Add,TEMPORARY(gaussFit1D)
              ENDELSE

           ENDIF

           ;;    END
           ;;    ELSE: BEGIN
           ;;       IF hadSuccessK THEN BEGIN
           ;;          keep_iTime  = N_ELEMENTS(keep_iTime) GT 0 ? [keep_iTime,iTime] : iTime

           ;;          successesK++
           ;;          totSuccessesK++
           ;;       ENDIF
           ;;    END
           ;; ENDCASE
     
        ENDIF ELSE BEGIN
           kBest       = 999
        ENDELSE

        ;; 2017/12/28 This code was supposed to kill any kappaFit1D struct corresponding to a nice 1D fit in the absence of a nice 2D fit
        ;; I believe it has been obviated
        
        ;; IF ~(KEYWORD_SET(hadSuccessK) AND KEYWORD_SET(hadSuccessG)) THEN BEGIN

        ;;    CASE (N_ELEMENTS(kappaFit1Ds) - N_ELEMENTS(fit2DKappa_inf_list)) OF
        ;;       0:                ;excellent
        ;;       1: BEGIN
        ;;          IF N_ELEMENTS(kappaFit1Ds) GT 1 THEN BEGIN
        ;;             kappaFit1Ds = kappaFit1Ds[0:-2] ;Well... I guess we'll trim one
        ;;          ENDIF ELSE BEGIN
        ;;             kappaFit1Ds = !NULL
        ;;          ENDELSE
        ;;       END
        ;;       ELSE: STOP
        ;;    ENDCASE

        ;;    IF KEYWORD_SET(KF2D__Curvefit_opt.add_gaussian_estimate) THEN BEGIN
        ;;       CASE (N_ELEMENTS(gaussFit1Ds) - N_ELEMENTS(fit2DGauss_inf_list)) OF
        ;;          0:             ;excellent
        ;;          1: BEGIN
        ;;             IF N_ELEMENTS(gaussFit1Ds) GT 1 THEN BEGIN
        ;;                gaussFit1Ds = gaussFit1Ds[0:-2] ;Well... I guess we'll trim one
        ;;             ENDIF ELSE BEGIN
        ;;                gaussFit1Ds = !NULL
        ;;             ENDELSE
        ;;          END
        ;;          ELSE: STOP
        ;;       ENDCASE
        ;;    ENDIF

        ;; ENDIF

     ENDELSE

     IF KEYWORD_SET(fit1D__save_plotSlices) AND $
        N_ELEMENTS(kappaFit1Ds) GT 0 AND $
        (KEYWORD_SET(dmsp) ? 1 : hadSuccessK) THEN BEGIN

        ;; 2018/02/16 Save thyself!
        ;; kappaFit1D=kappafit1ds[-1]
        ;; gaussFit1D=gaussFit1Ds[-1]
        ;; SAVE,KF2D__Curvefit_opt, $
        ;;      kappaFit1D, $
        ;;      gaussFit1D, $
        ;;      kappaParamStruct, $
        ;;      gaussParamStruct, $
        ;;      oneCurve, $
        ;;      iTime, $
        ;;      xRange, $
        ;;      yRange, $
        ;;      KF2D__strings, $
        ;;      eps, $
        ;;      units1D, $
        ;;      orig, $
        ;;      Xorig, $
        ;;      Yorig, $
        ;;      worig, $
        ;;      energy_inds, $
        ;;      eRange_fit, $
        ;;      peak_ind, $
        ;;      A, $
        ;;      AGauss, $
        ;;      kappa_fixA, $
        ;;      gauss_fixA, $
        ;;      tmpfit1denergies, $
        ;;      out_paramStr, $
        ;;      curdatastr, $
        ;;      avgfactorarr, $
        ;;      FILENAME='/SPENCEdata/Research/Satellites/FAST/kappa_dists/saves_output_etc/20180216--soscrewed.sav'


        ;; qualified = 1
        IF KEYWORD_SET(fit1D__save_if_kappa_below) THEN BEGIN
           qualKappaBelow = (fit2DKappa_inf_list[-1].fitParams[2] LE fit1D__save_if_kappa_below) OR $
                            (kappaFit1Ds[-1].A[2]               LE fit1D__save_if_kappa_below)
        ENDIF

        IF KEYWORD_SET(fit1D__save_every_nth_plot) THEN BEGIN
           qualKappaNth = ((bounds[i] MOD fit1D__save_every_nth_plot) EQ 0)
        ENDIF

        CASE 1 OF
           KEYWORD_SET(fit1D__save_if_kappa_below) AND KEYWORD_SET(fit1D__save_every_nth_plot): BEGIN
              qualified  = qualKappaBelow OR qualKappaNth
           END
           KEYWORD_SET(fit1D__save_if_kappa_below): BEGIN
              qualified  = qualKappaBelow
           END
           KEYWORD_SET(fit1D__save_every_nth_plot): BEGIN
              qualified  = qualKappaNth
           END
           ELSE: BEGIN
              qualified  = 1
           END
        ENDCASE

        IF qualified GT 0 THEN BEGIN
           PLOT_KAPPA_FITS,orig,kappaFit1Ds[-1], $
                           KEYWORD_SET(KF2D__Curvefit_opt.add_gaussian_estimate) ? $
                           gaussFit1Ds[-1] : $
                           !NULL, $
                           oneCurve, $
                           CLAMPED_TEMPERATURE=KF2D__Curvefit_opt.fit1D__clampTemperature, $
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
                           ;; ADD_ANGLE_LABEL=MEAN(KF2D__SDTData_opt.electron_angleRange), $
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
                           ADD_PLOTDIR_SUFF=STRING(FORMAT='("kappa_fits/Orbit_",A0,"/1DFits/",I0,"avg/")', $
                                                   KF2D__strings.orbStr, $
                                                   KF2D__SDTData_opt.spec_avg_intvl), $
                           POSTSCRIPT=~KEYWORD_SET(eps), $
                           ;; OUT_WINDOWARR=windowArr, $
                           /BUFFER, $
                           UNITS=units1D, $
                           DMSP=KEYWORD_SET(dmsp), $
                           EPS=eps
        ENDIF

     ENDIF

     IF 0 THEN BEGIN

        KAPPA_FIT2D__SHOW_AND_PROMPT__SELECTED2DFIT,curDataStr,fit2DKappa_inf_list[-1], $
           totSuccessesK, $
           iTime, $
           IS_MAXWELLIAN_FIT=0, $
           PROMPT__CONT_TO_NEXT_FIT=Kprompt__cont_to_next_fit, $
           PROMPT__CONT_UNTIL_FIT_EQ=Kprompt__cont_until_fit_eq, $
           FINISH_AND_SAVE_ALL=Kfinish_and_save_all, $
           KAPPA_FIT__SHOW__QUIT=Kshow__quit, $
           EPS=eps
        
        IF KEYWORD_SET(kCurvefit_opt.add_gaussian_estimate) THEN BEGIN

           KAPPA_FIT2D__SHOW_AND_PROMPT__SELECTED2DFIT,curDataStr,fit2DGauss_inf_list[-1], $
              totSuccessesG, $
              iTime, $
              IS_MAXWELLIAN_FIT=1, $
              PROMPT__CONT_TO_NEXT_FIT=Gprompt__cont_to_next_fit, $
              PROMPT__CONT_UNTIL_FIT_EQ=Gprompt__cont_until_fit_eq, $
              FINISH_AND_SAVE_ALL=Gfinish_and_save_all, $
              KAPPA_FIT__SHOW__QUIT=Gshow__quit, $
              EPS=eps

        ENDIF
     ENDIF

  ENDFOR

  STOP

END

