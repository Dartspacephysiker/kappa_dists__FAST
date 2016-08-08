PRO SETUP_KAPPA_FIT2D_TEST,good_angleBin_i,good_kappaFits_i,iWin, $
                           nEnergies,out_eRange_peak, $
                           allAngles,nTotAngles,useTheseAnglesIndex, $
                           curKappaStr,kappaFits,curDataStr, $
                           iAngle,iKappa,testKappa,testKappaFit,testArray, $
                           craptest, $
                           wts,X2D,Y2D,dataToFit, $
                           fa,dens_param,pre_densEst, $
                           ITIME=iTime, $
                           ESTFACS=estFacs, $
                           JUST_ERANGE_PEAK=just_eRange_peak, $
                           KCURVEFIT_OPT=kCurvefit_opt, $
                           KSDTDATA_OPT=kSDTData_opt, $
                           KSTRINGS=kStrings, $
                           OUT_ANGLE_I=angle_i, $
                           OUT_ERANGE_I=eRange_i
  
  COMPILE_OPT idl2

  iAngle             = good_angleBin_i[iWin]
  iKappa             = good_kappaFits_i[iWin]

  testKappa          = curKappaStr
  testKappaFit       = kappaFits[iKappa]
  eRange_peak        = out_eRange_peak[*,iKappa]


  testArray          = MAKE_ARRAY(nEnergies,nTotAngles)
  CASE 1 OF
     KEYWORD_SET(kCurvefit_opt.fit1D_dens__each_angle): BEGIN
        
        ;;Let them express some individuality in this spot
        KAPPA_FIT2D__1DFIT_EACH_ANGLE,curDataStr,testKappa, $
                                      allAngles, $
                                      eRange_peak, $
                                      testKappaFit, $
                                      ITIME=iTime, $
                                      ESTFACS=estFacs, $
                                      CURVEFIT_OPT=kCurvefit_opt, $
                                      SDTDATA_OPT=kSDTData_opt, $
                                      STRINGS=kStrings, $
                                      FIT2D_INF_LIST=fit2D_inf_list, $
                                      OUT_1D_DENS_ESTS=out_1D_dens_ests

        testArray    = testKappa.data
     END
     ELSE: BEGIN
        ;;Just treat them as all having the same density
        FOR k=0,nTotAngles-1 DO BEGIN
           testData        = testKappa.data[*,iAngle]
           testArray[*,k]  = testData
        ENDFOR
     END
  ENDCASE

  pre_densEst        = CALL_FUNCTION(kSDTData_opt.densFunc,testKappa, $
                                     ENERGY=kSDTData_opt.energy_electrons, $
                                     ANGLE=kSDTData_opt.fit2D_dens_aRange)

  craptest           = testKappa
  ;; craptest.data      = testArray

  ;;Get some better weights
  ;; testKappa          = CONV_UNITS(testKappa,'counts')
  ;; testKappa.ddata    = (testKappa.data)^.5
  ;; testKappa          = CONV_UNITS(testKappa,units)

  ;;wts, calcked from fit
  ;; wts                = 1.D/(testKappa.ddata)^2
  ;; fixMe              = WHERE(~FINITE(wts),nFixMe)
  ;; IF nFixMe GT 0 THEN BEGIN
  ;;    wts[fixMe]      = 0.0
  ;; ENDIF

  ;;Uhh, use data for fit weighting. Not fit data.
  wts                = curDataStr.ddata
  wts[*]             = 0.0D
  nz_i               = WHERE(curDataStr.ddata GT 0,/NULL)
  wts[nz_i]          = 1.D/(curDataStr.ddata[nz_i])^2

  ;; just_eRange_peak   = 1
  IF KEYWORD_SET(just_eRange_peak) THEN BEGIN
     eRange_i        = WHERE((testKappa.energy[*,nTotAngles/2] GE eRange_peak[0]) AND $
                             (testKappa.energy[*,nTotAngles/2] LE eRange_peak[1]),nEnKeep)
     IF nEnKeep EQ 0 THEN STOP
  ENDIF ELSE BEGIN
     nEnKeep         = N_ELEMENTS(testKappa.energy[*,nTotAngles/2])
     eRange_i        = INDGEN(nEnKeep)
  ENDELSE

  ;;First drop some energies
  X2D                = testKappa.energy[eRange_i,*]
  Y2D                = testKappa.theta[eRange_i,*]
  dataToFit          = curDataStr.data[eRange_i,*]

  CASE 1 OF
     kCurvefit_opt.fit2d_only_dens_angles: BEGIN
        angles             = REFORM(testKappa.theta[useTheseAnglesIndex,*])
        angle_i            = WHERE( (angles GE kSDTData_opt.fit2D_dens_aRange[0]) AND (angles LE kSDTData_opt.fit2D_dens_aRange[1]),nAKeep)
        IF nAKeep GT 0 THEN BEGIN
           X2D                = X2D[*,angle_i]
           Y2D                = Y2D[*,angle_i]
           dataToFit          = dataToFit[*,angle_i]

        ENDIF ELSE BEGIN
           PRINT,"What? Did you provide a bogus fit2D_dens_aRange?"
           STOP
        ENDELSE
     END
     ELSE: BEGIN
        angle_i            = INDGEN(nTotAngles)
     END
  ENDCASE

  fa                 = {kappa_1d_fitparams:testKappaFit.A}
  dens_param         = testKappaFit.A[3]

  IF KEYWORD_SET(kCurvefit_opt.fit1D_dens__each_angle) THEN BEGIN
     CASE 1 OF
        kCurvefit_opt.fit2d_only_dens_angles: BEGIN
           fa              = CREATE_STRUCT(fa,"dens_1D_ests",out_1D_dens_ests[angle_i])
        END
        ELSE: BEGIN
           fa              = CREATE_STRUCT(fa,"dens_1D_ests",out_1D_dens_ests)
        END
     ENDCASE

  ENDIF
END