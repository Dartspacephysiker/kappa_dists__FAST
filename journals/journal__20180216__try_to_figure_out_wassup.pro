;2018/02/16
PRO JOURNAL__20180216__TRY_TO_FIGURE_OUT_WASSUP, $
   JUNKFOOD=junkFood, $
   SHIFT_LOWER_ENERGY_BIN_DOWNWARD_BY=shift_lower_energy_bin_downward_by

  COMPILE_OPT IDL2,STRICTARRSUBS

  save_fitPlots = 0
  buffer        = 0

  doNewFit      = 1

  ;; The file with the evidences
  dir = '/SPENCEdata/Research/Satellites/FAST/kappa_dists/saves_output_etc/'
  fil = '20180216--soscrewed.sav'
  ;; It contains everything you should need for fitting and plotting. Namely:
  ;; KF2D__Curvefit_opt,
  ;; kappaFit1D,gaussFit1D,oneCurve,
  ;; iTime,xRange,yRange,
  ;; KF2D__strings,eps,units1D,
  ;; orig,Xorig,Yorig,worig,
  ;; energy_inds,erange_peak,peak_ind,
  ;; A,AGauss,
  ;; kappa_fixA,gauss_fixA,
  ;; tmpfit1denergies,
  ;; out_paramStr,
  ;; curdatastr,
  ;; avgfactorarr

  RESTORE,dir+fil

  IF KEYWORD_SET(doNewFit) THEN BEGIN

     tmpfit1denergies = orig.x

     
     IF KEYWORD_SET(shift_lower_energy_bin_downward_by) THEN BEGIN
        energy_inds[1] += shift_lower_energy_bin_downward_by
        eRange_peak[0] = xorig[energy_inds[1]]
     ENDIF

     KAPPA__GET_FITS__MPFIT1D,Xorig,Yorig, $
                              orig,kappaFit1D,gaussFit1D, $
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
                              ;; OUT_FITTED_PARAMS=out_kappaParams, $
                              ;; OUT_FITTED_GAUSS_PARAMS=out_gaussParams, $
                              ;; OUT_KAPPAFIT1DSTRUCTS=kappaFit1Ds, $
                              ;; OUT_GAUSSFIT1DSTRUCTS=gaussFit1Ds, $
                              FIT__LINEAR_ENERGY_SHIFT=KF2D__Curvefit_opt.fit__linear_energy_shift, $
                              ;; FIT__LES__TAKE_STOCK_OF_RB=KF2D__Curvefit_opt.fit__LES__take_stock_of_RB, $
                              ADD_FULL_FITS=tmpFit1Denergies, $
                              ADD_ANGLESTR=angleStr, $
                              ;; OUT_ERANGE_PEAK=eRange_peakArr, $
                              OUT_PARAMSTR=out_paramStr, $
                              DONT_PRINT_FITINFO=dont_print_fitInfo, $
                              FIT_FAIL__USER_PROMPT=fit1D_fail__user_prompt, $
                              UNITS=units1D, $
                              MASS=curDataStr.mass, $
                              AVGFACTORARR=avgFactorArr

  ENDIF

  IF KEYWORD_SET(junkFood) THEN BEGIN
     kappaFit1D.A    = junkFood
     kappaFit1D.name = STRING(FORMAT='("$\kappa$ = ",F0.2)',kappaFit1D.A[2])
  ENDIF

  ;; kappaFunc  = 'KAPPA_FLUX__LINEAR_SHIFT_IN_ENERGY'

  ;; kappaFit1D.yFull = CALL_FUNCTION(kappaFunc, $
  ;;                                  tmpFit1Denergies, $
  ;;                                  kappaFit1D.A, $
  ;;                                  UNITS=units1D, $
  ;;                                  MASS=curDataStr.mass)

  ;; gaussFit1D.yFull = KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__CONV_TO_F__FUNC(tmpFit1Denergies,kappaFit1D.A, $
  ;;                                                                           UNITS=units1D, $
  ;;                                                                           MASS=curDataStr.mass)


  PLOT_KAPPA_FITS,orig,kappaFit1D, $
                  KEYWORD_SET(KF2D__Curvefit_opt.add_gaussian_estimate) ? $
                  gaussFit1D : $
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
                  SAVE_FITPLOTS=save_fitPlots, $
                  /PLOT_FULL_FIT, $
                  ;; SKIP_BAD_FITS=skip_bad_fits, $
                  USING_SDT_DATA=using_SDT_data, $
                  ;; VERTICAL_LINES=vertical_lines, $
                  /VERTICAL_LINES, $
                  ;; PLOT_SAVENAME=plotSN, $
                  /USE_PSYM_FOR_DATA, $
                  PLOTDIR=plotDir, $
                  POSTSCRIPT=~KEYWORD_SET(eps), $
                  ;; OUT_WINDOWARR=windowArr, $
                  BUFFER=buffer, $
                  UNITS=units1D, $
                  EPS=eps

  STOP

END

