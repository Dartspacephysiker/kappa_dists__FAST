;2018/01/19
PRO KAPPA_FIT2D__MONTECARLO__1DINIT,curDataStr, $
                                    IS_MAXWELLIAN=is_Maxwellian, $
                                    ;; PEAK_IND=peak_ind, $
                                    NENERGIES=nEnergies, $
                                    PEAK_ENERGY=peak_energy, $
                                    ERANGE_PEAK=eRange_peak, $
                                    OUT_1DPARAMSTRUCT=paramStruct, $
                                    MC__OKStatus=MC__OKStatus

  COMPILE_OPT IDL2,STRICTARRSUBS

  @common__kappa_fit2d_structs.pro

  CASE 1 OF
     KEYWORD_SET(KF2D__Curvefit_opt.fit1D__sc_eSpec): BEGIN

        ;;Get energy spectrum, if that's what you're into
        eSpec = GET_EN_SPEC__SINGLE_STRUCT( $
                curDataStr, $
                /RETRACE, $
                ANGLE=KF2D__SDTData_opt.electron_angleRange, $
                UNITS=units1D, $
                OUT_AVGFACTORARR=avgFactorArr, $
                OUT_NORMARR=normArr)

        Xorig    = eSpec.v
        Yorig    = eSpec.y
        worig    = eSpec.yerr
        ;; nAngles          = 1
        ;; nReqSCAngles     = 1

     END
     ELSE: BEGIN

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
     /CONTINUE_IF_NOMATCH, $
     ONECOUNT_STR=oneCurve     

  IF peak_energy EQ -1 THEN STOP

  maxEInd           = (peak_ind + KF2D__Curvefit_opt.n_below_peak) < (nEnergies-1)
  minEInd           = (peak_ind - KF2D__Curvefit_opt.n_above_peak) > 0

  IF KEYWORD_SET(KF2D__Curvefit_opt.estimate_A_from_data) THEN BEGIN 

     KAPPA__GET_A_ESTIMATES,curDataStr,Xorig,Yorig, $
                            minEInd,maxEInd,nEnergies, $
                            peak_ind,peak_energy,eRange_peak, $
                            KAPPA_EST=KF2D__Curvefit_opt.fitA[2], $
                            ;; MASS=mass, $
                            E_ANGLE=KF2D__SDTData_opt.electron_angleRange, $
                            ;; ANGLES=tempAngleEstRange, $
                            ;; N_ANGLES_IN_RANGE=nAngles, $
                            ;; BULKANGLE_STRUCT=angleStr, $
                            ;; DONT_TAKE_STOCK_OF_BULKANGLE=dont_take_stock_of_bulkangle, $
                            ADD_GAUSSIAN_ESTIMATE=is_Maxwellian, $
                            ONLY_GAUSSIAN_ESTIMATE=is_Maxwellian, $

                            ;; USE_SDT_GAUSSIAN_FIT=KF2D__Curvefit_opt.use_SDT_Gaussian_fit, $
                            ;; ESTFACS=estFacs, $
                            PHI__USE_ENERGY_BEFORE_PEAK=TAG_EXIST(KF2D__Curvefit_opt,'phi__use_energy_before_peak') ? KF2D__Curvefit_opt.phi__use_energy_before_peak : !NULL, $
                            A_OUT=A, $
                            AGAUSS_OUT=AGauss, $
                            DONT_PRINT_ESTIMATES=dont_print_estimates, $
                            /TEST_NOREV, $
                            TEMPERATURE_TYPE=KF2D__SDTData_opt.fit2D__temperature_type;; , $
                            ;; UNITS=units1D

  ENDIF ELSE BEGIN
     A           = DOUBLE([peak_energy,T,kappa,n_est,0.000001,5.68e-6,0])
  ENDELSE
  
  energy_inds    = [minEInd,maxEInd]

  ;; Init fitParam structs for MPFIT1D stuff
  ATmp              = DOUBLE([1e3,100.,3.0,0.01,0])
  IF KEYWORD_SET(is_Maxwellian) THEN BEGIN
     fixA        = [0, $  ;Vary bulk E [0]
                          KF2D__Curvefit_opt.fit1D__clampTemperature, $ ;Temperature [1] (maybe)
                          1, $
                          KF2D__CurveFit_opt.fit1D__clampDensity    , $ ;and density [3] (but not kappa or angle)
                          1]

  ENDIF ELSE BEGIN
     fixA        = [0, $  ;Vary bulk E [0]                              
                          KF2D__Curvefit_opt.fit1D__clampTemperature, $ ;Temperature [1] (maybe)                         
                          0, $  ;kappa       [2]
                          KF2D__CurveFit_opt.fit1D__clampDensity    , $ ;and density [3] (but not angle)
                          1] 
  ENDELSE
  
  paramStruct  = INIT_KAPPA_FITPARAM_INFO(TEMPORARY(ATmp),fixA)

  IF paramStruct[1].limited[1] EQ 0 THEN BEGIN
     ;; When the upperbound of T is not limited, xTol will become irreducible in
     ;; the course of the fit and status 7 will be reported. To avoid this, we
     ;; allow xTol = unminimizable to be an allowable outcome
     ;;See INIT_KAPPA_FITPARAM_INFO<f> for more informaciones

     MC__OKStatus  = [1,2,3,4,7] ;These are the acceptable outcomes of fitting with MPFIT2DFUN

  ENDIF ELSE BEGIN
     MC__OKStatus  = [1,2,3,4]
  ENDELSE

  CASE 1 OF
     KEYWORD_SET(is_Maxwellian): KAPPA__CONVERT_A_AND_FIXA_TO_MPFITFUN1D_FORMAT,AGauss,fixA
     ELSE: KAPPA__CONVERT_A_AND_FIXA_TO_MPFITFUN1D_FORMAT,A,fixA
  ENDCASE
  
  KAPPA__GET_FITS__MPFIT1D,Xorig,Yorig, $
                           orig,kappaFit1D,gaussFit1D, $
                           YORIG_ERROR=worig, $
                           KCURVEFIT_OPT=KF2D__Curvefit_opt, $
                           KFITPARAMSTRUCT=paramStruct, $
                           GFITPARAMSTRUCT=paramStruct, $
                           ENERGY_INDS=energy_inds, $
                           ERANGE_PEAK=eRange_peak, $
                           PEAK_IND=peak_ind, $
                           BOUNDS_I=0, $
                           KAPPA_A=A, $
                           GAUSS_A=AGauss, $
                           KAPPA_FIXA=fixA, $
                           GAUSS_FIXA=fixA, $
                           ;; YMAX=yMax, $
                           STRINGS=KF2D__strings, $
                           ;; ADD_FULL_FITS=fit1Denergies, $
                           ;; ADD_ANGLESTR=angleStr, $
                           ;; OUT_PARAMSTR=out_paramStr, $
                           DONT_PRINT_FITINFO=dont_print_fitInfo, $
                           ;; FIT_FAIL__USER_PROMPT=fit1D_fail__user_prompt, $
                           UNITS=units1D, $
                           MASS=curDataStr.mass, $
                           AVGFACTORARR=avgFactorArr, $
                           ONLY_GAUSSIAN_ESTIMATE=is_Maxwellian, $
                           /MONTE_CARLO_MODE

END
