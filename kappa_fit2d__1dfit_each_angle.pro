PRO KAPPA_FIT2D__1DFIT_EACH_ANGLE,curDataStr,curFitStr, $
                                  allAngles, $
                                  eRange_peak, $
                                  testFit1DInfo, $
                                  ITIME=iTime, $
                                  ESTFACS=estFacs, $
                                  KCURVEFIT_OPT=kCurvefit_opt, $
                                  KFITPARAMSTRUCT=kFitParamStruct, $
                                  KSTRINGS=kStrings, $
                                  KSDTDATA_OPT=kSDTData_opt, $
                                  FIT2D_INF_LIST=fit2D_inf_list, $
                                  OUT_1D_DENS_ESTS=out_1D_dens_ests

  COMPILE_OPT IDL2,STRICTARRSUBS

  OKStatus         = [1,2,3,4] ;These are all the acceptable outcomes of fitting with MPFIT2DFUN
  nTotAngles       = N_ELEMENTS(allAngles)

  good_1DFits      = 0

  A_1D             = testFit1DInfo.A

  ;;Prepare a temp testFit1DInfo so we can load it with new fits
  tmpFit1DInfo   = testFit1DInfo ;Use this for updating fit status, fit params
  fitTags          = STRUPCASE(TAG_NAMES(testFit1DInfo))
  ;; junkMe           = WHERE(fitTags EQ 'NAME',doJunk)
  ;; IF doJunk GT 0 THEN STRUCT_DELETE_FIELD,tmpFit1DInfo,'NAME' ;If 
  tmpFit1DInfo.name   = '1DFit'
  junkMe           = WHERE(fitTags EQ 'Y',doJunk)
  IF doJunk GT 0 THEN STRUCT_DELETE_FIELD,tmpFit1DInfo,'Y'
  junkMe           = WHERE(fitTags EQ 'X',doJunk)
  IF doJunk GT 0 THEN STRUCT_DELETE_FIELD,tmpFit1DInfo,'X'

  out_1D_dens_ests = MAKE_ARRAY(nTotAngles,VALUE=tmpFit1DInfo.A[3],/FLOAT)
  
  CASE 1 OF
     KEYWORD_SET(kCurvefit_opt.use_mpFit1D): BEGIN
        tmpFitParamStruct = kFitParamStruct
        ;;We can't budge anything but density
        fixA__each_1dfit    = [1,1,1,0,1]
        tmpFitParamStruct[*].fixed = fixA__each_1dfit
     END
     ELSE: BEGIN
        ;;We can't budge anything but density
        fixA__each_1dfit    = [0,0,0,1,0,0,0]
     END
  ENDCASE

  curFitStr.data[*]     = 0.0D

  FOR iAngle=0,nTotAngles-1 DO BEGIN

     ;;Here's the data we're working with for this loop iteration
     Xorig              = curDataStr.energy[*,iAngle]
     Yorig              = curDataStr.data[*,iAngle]
     DYorig             = curDataStr.ddata[*,iAngle]
     Aorig              = curDataStr.theta[*,iAngle]

     orig               = {x:Xorig, $
                           y:Yorig, $
                           name:kStrings.plotTimes[iTime]}
     
     tmpA_1D            = A_1D

     tempAngle          = allAngles[iAngle]
     tempAngleEstRange  = [tempAngle-1.0,tempAngle+1.0]

     ;; tmpA_1D[3]         = N_2D_FS(curDataStr,ENERGY=eRange_peak, $
     ;;                              ANGLE=tempAngleEstRange)*estFacs.N
     tmpA_1D[3]         = N_2D_FS(curDataStr,ENERGY=eRange_peak, $
                                  ANGLE=tempAngleEstRange)

     IF KEYWORD_SET(kCurvefit_opt.trim_energies_below_peak) THEN BEGIN 
        ;; X               = Xorig[WHERE(Xorig GE eRange_peak[0] AND Xorig LE eRange_peak[1])]
        ;; Y               = Yorig[WHERE(Xorig GE eRange_peak[0] AND Xorig LE eRange_peak[1])] 
        X               = Xorig[WHERE(Xorig GE eRange_peak[0] AND Xorig LE eRange_peak[1])]
        Y               = Yorig[WHERE(Xorig GE eRange_peak[0] AND Xorig LE eRange_peak[1])] 
        DY              = DYorig[WHERE(Xorig GE eRange_peak[0] AND Xorig LE eRange_peak[1])] 
     ENDIF ELSE BEGIN
        X               = Xorig
        Y               = Yorig
        DY              = DYorig
     ENDELSE

     IF KEYWORD_SET(kCurvefit_opt.thresh_eFlux) THEN BEGIN
        
        thresh                   = 1e4
        badBoys                  = WHERE(Y LE thresh, $
                                         nBad, $
                                         COMPLEMENT=goodBoys, $
                                         NCOMPLEMENT=nGood)
        IF nBad GT 0 THEN BEGIN
           X                     = X[goodBoys]
           Y                     = Y[goodBoys]
           DY                    = DY[goodBoys]
           ;; PRINT,"Dropped " + STRCOMPRESS(nBad,/REMOVE_ALL) + " bad boys from the club."
        ENDIF
     ENDIF

     weights            = DY
     weights[*]         = 0.0D
     nz_i               = WHERE(DY GT 0,/NULL)
     weights[nz_i]      = 1./(DY[nz_i])^2

     CASE kCurvefit_opt.use_mpFit1D OF
        0: BEGIN

           ;;Was trying to figure something out 2016/08/06. Should be OK now
           ;; testA_1D            = MAKE_ARRAY(7,VALUE=0.)
           ;; testA_1D[0] = tmpA_1D[0] & testA_1D[1] = tmpA_1D[1] & testA_1D[2] = tmpA_1D[2] & testA_1D[3] = tmpA_1D[3] & tmpA_1D=testA_1D

           yFit               = CURVEFIT(X,Y,weights,tmpA_1D,sigma, $
                                         FUNCTION_NAME='KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__CONV_TO_F' , $
                                         /DOUBLE, $
                                         FITA=fixA__each_1dfit, $
                                         ITMAX=1000, $
                                         CHI2=chi2, $
                                         ITER=itNum, $
                                         TOL=1e-3, $
                                         STATUS=fitStatus)
           CASE fitStatus OF 
              0: BEGIN 
                 good_1DFits++
                 KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__CONV_TO_F,Xorig,tmpA_1D,yFull
                 curFitStr.data[*,iAngle] = yFull
                 out_1D_dens_ests[iAngle] = tmpA_1D[3]
              END 
              1: BEGIN 
              END 
              2: BEGIN 
              END 
           ENDCASE
        END
        1: BEGIN

           IF N_ELEMENTS(X) LE 5 THEN BEGIN
              PRINT,"Skipping! Too few elements to fit meaningfully ..."
              skip   = 1
              status = 0
           ENDIF

           IF ~KEYWORD_SET(skip) THEN BEGIN

              UPDATE_KAPPA_FITPARAM_INFO,tmpFitParamStruct,tmpA_1D, $
                                /NO_FIXA, $
                                /NO_ERANGE_PEAK ;,kappa_fixA,eRange_peak
              ;; tmpParams = INIT_KAPPA_FITPARAM_INFO(tmpA_1D,fixA__each_1dfit, $
              ;;                                      ERANGE_PEAK=eRange_peak)
              
              tmpA_1D  = MPFITFUN('KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__CONV_TO_F__FUNC', $
                                  X,Y, $
                                  WEIGHTS=weights, $
                                  FUNCTARGS=fa, $
                                  BESTNORM=bestNorm, $
                                  AUTODERIVATIVE=0, $
                                  NFEV=nfev, $
                                  ;; FTOL=KEYWORD_SET(kCurvefit_opt.fit_tol) ? $
                                  ;; kCurvefit_opt.fit_tol : 1e-3, $
                                  FTOL=1e-9, $
                                  GTOL=1e-13, $
                                  STATUS=status, $
                                  BEST_RESID=best_resid, $
                                  PFREE_INDEX=ifree, $
                                  CALC_FJAC=calc_fjac, $
                                  BEST_FJAC=best_fjac, $
                                  PARINFO=tmpFitParamStruct, $
                                  QUERY=query, $
                                  NPEGGED=npegged, NFREE=nfree, DOF=dof, $
                                  COVAR=covar, $
                                  PERROR=perror, $
                                  MAXITER=KEYWORD_SET(kCurvefit_opt.max_iter) ? $
                                  kCurvefit_opt.max_iter : 150, $
                                  NITER=itNum, $
                                  YFIT=yFit, $
                                  /QUIET, $
                                  ERRMSG=errMsg, $
                                  _EXTRA=extra)

              IF ~KEYWORD_SET(dont_print_estimates) THEN BEGIN
                 IF (status LE 0) OR (status GE 5) THEN PRINT,MPFITFUN__IDENTIFY_ERROR(status)
                 IF status EQ 0 THEN STOP
              ENDIF

           ENDIF

           IF (WHERE(status EQ OKStatus))[0] NE -1 THEN BEGIN
              fitStatus                       = 0 
              chi2                            = TOTAL( (Y-yFit)^2 * ABS(weights) )

              IF FINITE(chi2) THEN BEGIN
                 pVal                        = 1 - CHISQR_PDF(chi2,N_ELEMENTS(X)-N_ELEMENTS(WHERE(~fixA__each_1dfit,/NULL))) ;Subtract number of free params
              ENDIF ELSE BEGIN
                 pVal                        = -1
              ENDELSE
           ENDIF ELSE BEGIN
              fitStatus                      = 1
              chi2                           = -1
              pVal                           = -1
           ENDELSE

           ;;Update tmpFitStr before plotting
           tmpTmpFit                         = CREATE_STRUCT('X',X, $
                                                             'Y',yFit, $
                                                             ;; 'NAME','1DFit', $
                                                             tmpFit1DInfo)
           ;;Just for plots
           tmpTmpFit.A                       = tmpA_1D
           tmpTmpFit.fitStatus               = fitStatus
           

           ;; PLOT_KAPPA_FITS,orig,tmpFit1DInfo, $
           ;; PLOT_KAPPA_FITS,{x:X,y:Y,name:kStrings.plotTimes[iTime]},tmpTmpFit, $
           ;;                 BOUNDS_I=iTime, $
           ;;                 XRANGE=[30,3.5e4], $
           ;;                 YRANGE=[1e5,5e9], $
           ;;                 /XLOG, $
           ;;                 /YLOG, $
           ;;                 STRINGS=kStrings, $
           ;;                 /ADD_FITPARAMS_TEXT, $
           ;;                 PLOT_FULL_FIT=plot_full_fit, $
           ;;                 ;; ADD_ANGLE_LABEL=testFit1DInfo.bulkAngleInf.SDTAngle
           ;;                 ADD_ANGLE_LABEL=tempAngle, $
           ;;                 ADD_CHI_VALUE=chi2, $
           ;;                 /USE_PSYM_FOR_DATA, $
           ;;                 /ADD_WINTITLE, $
           ;;                 OUT_WINDOWARR=windowArr

           ;; PRINT,'Status : ' + STRCOMPRESS(status,/REMOVE_ALL)
           ;; PRINT,'itNum  : ' + STRCOMPRESS(itNum,/REMOVE_ALL)
           ;; STOP

           CASE fitStatus OF 
              0: BEGIN 
                 good_1DFits++
                 yFull = KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__CONV_TO_F__FUNC(Xorig,tmpA_1D)
                 curFitStr.data[*,iAngle] = yFull
                 out_1D_dens_ests[iAngle] = tmpA_1D[3]
              END 
              1: BEGIN 
              END 
              2: BEGIN 
              END 
           ENDCASE
        END
     ENDCASE


  ENDFOR

  IF N_ELEMENTS(windowArr) NE 0 THEN BEGIN
     FOR k=0,N_ELEMENTS(windowArr)-1 DO windowArr[k].Close
     windowArr = !NULL
  ENDIF

  ;; PRINT,FORMAT='(I0," dists at an angle, out of ",I0," in total, allowed me to do a 1D fit")', $
  ;;       good_1DFits,nTotAngles

END