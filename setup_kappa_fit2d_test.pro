PRO SETUP_KAPPA_FIT2D_TEST,good_angleBin_i,good_fits_i,iWin, $
                           nEnergies,out_eRange_peak, $
                           allAngles,nTotAngles,useTheseAnglesIndex, $
                           curFitStr,fitInfoArr,curDataStr, $
                           iAngle,iFit,testFitStr,testFit1DInfo,testArray, $
                           craptest, $
                           wtsForFit,X2D,Y2D,dataToFit, $
                           fa, $
                           ;; dens_param, $
                           pre_densEst, $
                           ITIME=iTime, $
                           ESTFACS=estFacs, $
                           ;; JUST_ERANGE_PEAK=just_eRange_peak, $
                           KCURVEFIT_OPT=kCurvefit_opt, $
                           KFITPARAMSTRUCT=kFitParamStruct, $
                           KSDTDATA_OPT=kSDTData_opt, $
                           KSTRINGS=kStrings, $
                           ;; OUT_ANGLE_I=angle_i, $
                           OUT_FIT2D_DENS_ANGLEINFO=fit2D_dens_angleInfo, $
                           OUT_ERANGE_I=eRange_i
  
  COMPILE_OPT IDL2,STRICTARRSUBS

  iAngle             = good_angleBin_i[iWin]
  iFit               = good_fits_i[iWin]

  testFitStr         = curFitStr
  testFit1DInfo      = fitInfoArr[iFit]
  eRange_peak        = out_eRange_peak[*,iFit]


  testArray          = MAKE_ARRAY(nEnergies,nTotAngles)
  CASE 1 OF
     KEYWORD_SET(kCurvefit_opt.fit1D_dens__each_angle): BEGIN
        
        ;;Let them express some individuality in this spot
        ;;NOTE, while we fit to each angle, we only fit to energies falling within eRange_peak
        KAPPA_FIT2D__1DFIT_EACH_ANGLE,curDataStr,testFitStr, $
                                      allAngles, $
                                      eRange_peak, $
                                      testFit1DInfo, $
                                      ITIME=iTime, $
                                      ESTFACS=estFacs, $
                                      KCURVEFIT_OPT=kCurvefit_opt, $
                                      KFITPARAMSTRUCT=kFitParamStruct, $
                                      KSDTDATA_OPT=kSDTData_opt, $
                                      KSTRINGS=kStrings, $
                                      FIT2D_INF_LIST=fit2D_inf_list, $
                                      OUT_1D_DENS_ESTS=out_1D_dens_ests

        testArray    = testFitStr.data
     END
     ELSE: BEGIN
        ;;Just treat them as all having the same density
        FOR k=0,nTotAngles-1 DO BEGIN
           testData        = testFitStr.data[*,iAngle]
           testArray[*,k]  = testData
        ENDFOR
     END
  ENDCASE

  pre_densEst        = CALL_FUNCTION(kSDTData_opt.densFunc,testFitStr, $
                                     ENERGY=kSDTData_opt.energy_electrons, $
                                     ANGLE=kSDTData_opt.fit2D_dens_aRange)

  craptest           = testFitStr
  ;; craptest.data      = testArray

  ;;Uhh, use data for fit weighting. Not fit data.
  wts                = curDataStr.ddata
  wts[*]             = 0.0D
  nz_i               = WHERE(curDataStr.ddata GT 0,/NULL)
  wts[nz_i]          = 1.D/(curDataStr.ddata[nz_i])^2

  ;; just_eRange_peak   = 1
  IF KEYWORD_SET(kCurvefit_opt.fit2d_just_eRange_peak) THEN BEGIN
     eRange_i        = WHERE((testFitStr.energy[*,nTotAngles/2] GE eRange_peak[0]) AND $
                             (testFitStr.energy[*,nTotAngles/2] LE eRange_peak[1]),nEnKeep)
     IF nEnKeep EQ 0 THEN STOP
  ENDIF ELSE BEGIN
     nEnKeep         = N_ELEMENTS(testFitStr.energy[*,nTotAngles/2])
     eRange_i        = INDGEN(nEnKeep)
  ENDELSE

  ;;First drop some energies
  X2D                = testFitStr.energy[eRange_i,*]
  Y2D                = testFitStr.theta[eRange_i,*]
  dataToFit          = curDataStr.data[eRange_i,*]
  wtsForFit          = wts[eRange_i,*]

  CASE 1 OF
     kCurvefit_opt.fit2d_only_dens_angles: BEGIN
        angles             = REFORM(testFitStr.theta[useTheseAnglesIndex,*])
        angle_i            = WHERE( (angles GE kSDTData_opt.fit2D_dens_aRange[0]) AND $
                                    (angles LE kSDTData_opt.fit2D_dens_aRange[1]), $
                                    nAKeep, $
                                    COMPLEMENT=remAngle_i, $
                                    NCOMPLEMENT=nARem)
        fit2D_dens_angleInfo = {angle_i:angle_i, $
                               nAKeep:nAKeep, $
                               remAngle_i:remAngle_i, $
                               nARem:nARem, $
                               type:'Only densAngles'}
        IF nAKeep GT 0 THEN BEGIN
           ;;NOTE, we may have (intentionally) dropped some energies in {X,Y}2D by this point!
           ;;See lines above dealing with {X,Y}2D, dataToFit
           X2D                = X2D[*,angle_i]
           Y2D                = Y2D[*,angle_i]
           dataToFit          = dataToFit[*,angle_i]
           wtsForFit          = wtsForFit[*,angle_i]
        ENDIF ELSE BEGIN
           PRINT,"What? Did you provide a bogus fit2D_dens_aRange?"
           STOP
        ENDELSE
     END
     kCurvefit_opt.fit2d_only_eAngles: BEGIN
        angles             = REFORM(testFitStr.theta[useTheseAnglesIndex,*])
        angle_i            = WHERE( (angles GE kSDTData_opt.electron_angleRange[0]) AND $
                                    (angles LE kSDTData_opt.electron_angleRange[1]), $
                                    nAKeep, $
                                    COMPLEMENT=remAngle_i, $
                                    NCOMPLEMENT=nARem)
        fit2D_dens_angleInfo = {angle_i:angle_i, $
                               nAKeep:nAKeep, $
                               remAngle_i:remAngle_i, $
                               nARem:nARem, $
                               type:'Only eAngles'}
        IF nAKeep GT 0 THEN BEGIN
           ;;NOTE, we may have (intentionally) dropped some energies in {X,Y}2D by this point!
           ;;See lines above dealing with {X,Y}2D, dataToFit
           X2D                = X2D[*,angle_i]
           Y2D                = Y2D[*,angle_i]
           dataToFit          = dataToFit[*,angle_i]
           wtsForFit          = wtsForFit[*,angle_i]
        ENDIF ELSE BEGIN
           PRINT,"What? Did you provide a bogus electron_angleRange?"
           STOP
        ENDELSE
     END
     ELSE: BEGIN
        angle_i            = INDGEN(nTotAngles)
     END
  ENDCASE

  fa                 = {kappa_1d_fitparams:testFit1DInfo.A}

  IF KEYWORD_SET(kCurvefit_opt.fit1D_dens__each_angle) THEN BEGIN
     CASE 1 OF
        kCurvefit_opt.fit2d_only_dens_angles: BEGIN
           fa              = CREATE_STRUCT(fa,"dens_1D_ests",out_1D_dens_ests[angle_i])
        END
        kCurvefit_opt.fit2d_only_eAngles: BEGIN
           fa              = CREATE_STRUCT(fa,"dens_1D_ests",out_1D_dens_ests[angle_i])
        END
        ELSE: BEGIN
           fa              = CREATE_STRUCT(fa,"dens_1D_ests",out_1D_dens_ests)
        END
     ENDCASE

  ENDIF
END