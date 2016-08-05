PRO SETUP_KAPPA_FIT2D_TEST,good_angleBin_i,good_kappaFits_i,iWin, $
                           nEnergies,out_eRange_peak, $
                           allAngles,nTotAngles, $
                           curKappaStr,kappaFits,curDataStr, $
                           iAngle,iKappa,testKappa,testKappaFit,testArray, $
                           craptest, $
                           wts,X2D,Y2D,dataToFit, $
                           fa,dens_param, $
                           KCURVEFIT_OPT=kCurvefit_opt, $
                           KSDTDATA_OPT=kSDTData_opt, $
                           OUT_ANGLE_I=angle_i
  
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
                                      CURVEFIT_OPT=kCurvefit_opt, $
                                      SDTDATA_OPT=SDTData_opt, $
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

  craptest           = testKappa
  ;; craptest.data      = testArray

  ;;Get some better weights
  testKappa          = CONV_UNITS(testKappa,'counts')
  testKappa.ddata    = (testKappa.data)^.5
  testKappa          = CONV_UNITS(testKappa,units)

  ;;wts, calcked from fit
  wts                = 1.D/(testKappa.ddata)^2
  fixMe              = WHERE(~FINITE(wts),nFixMe)
  IF nFixMe GT 0 THEN BEGIN
     wts[fixMe]      = 0.0
  ENDIF

  CASE 1 OF
     kCurvefit_opt.fit2d_only_dens_angles: BEGIN
        angles             = REFORM(testKappa.theta[nEnergies/2,*])
        angle_i            = WHERE( (angles GE kSDTData_opt.fit2D_dens_aRange[0]) AND (angles LE kSDTData_opt.fit2D_dens_aRange[1]),nAKeep)
        IF nAKeep GT 0 THEN BEGIN
           X2D                = testKappa.energy[*,angle_i]
           Y2D                = testKappa.theta[*,angle_i]
           dataToFit          = curDataStr.data[*,angle_i]
        ENDIF ELSE BEGIN
           PRINT,"What? Did you provide a bogus fit2D_dens_aRange?"
           STOP
        ENDELSE
     END
     ELSE: BEGIN
        angle_i            = INDGEN(nTotAngles)
        X2D                = testKappa.energy
        Y2D                = testKappa.theta
        dataToFit          = curDataStr.data
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