PRO KAPPA_FIT2D__1DFIT_EACH_ANGLE,curDataStr,curFitStr, $
                                  allAngles, $
                                  eRange_peak, $
                                  fitStr, $
                                  CURVEFIT_OPT=curvefit_opt, $
                                  SDTDATA_OPT=SDTData_opt, $
                                  FIT2D_INF_LIST=fit2D_inf_list, $
                                  OUT_1D_DENS_ESTS=out_1D_dens_ests

  COMPILE_OPT idl2

  OKStatus         = [1,2,3,4] ;These are all the acceptable outcomes of fitting with MPFIT2DFUN
  nTotAngles       = N_ELEMENTS(allAngles)

  good_1DFits      = 0

  A_1D             = fitStr.A
  out_1D_dens_ests = MAKE_ARRAY(nTotAngles,VALUE=fitStr.A[3],/FLOAT)
  FOR iAngle=0,nTotAngles-1 DO BEGIN

     ;;Here's the data we're working with for this loop iteration
     Xorig              = curDataStr.energy[*,iAngle]
     Yorig              = curDataStr.data[*,iAngle]
     Aorig              = curDataStr.theta[*,iAngle]

     tmpA_1D            = A_1D

     tempAngle          = allAngles[iAngle]
     tempAngleEstRange  = [tempAngle-1.0,tempAngle+1.0]

     tmpA_1D[3]         = N_2D_FS(curDataStr,ENERGY=eRange_peak, $
                                  ANGLE=tempAngleEstRange)


     IF KEYWORD_SET(curvefit_opt.trim_energies_below_peak) THEN BEGIN 
        ;; X               = Xorig[WHERE(Xorig GE eRange_peak[0] AND Xorig LE eRange_peak[1])]
        ;; Y               = Yorig[WHERE(Xorig GE eRange_peak[0] AND Xorig LE eRange_peak[1])] 
        X               = Xorig[WHERE(Xorig GE eRange_peak[0] AND Xorig LE eRange_peak[1])]
        Y               = Yorig[WHERE(Xorig GE eRange_peak[0] AND Xorig LE eRange_peak[1])] 
     ENDIF ELSE BEGIN
        X               = Xorig
        Y               = Yorig
     ENDELSE

     IF KEYWORD_SET(curvefit_opt.thresh_eFlux) THEN BEGIN
        
        thresh                   = 1e6
        badBoys                  = WHERE(Y LE thresh, $
                                         nBad, $
                                         COMPLEMENT=goodBoys, $
                                         NCOMPLEMENT=nGood)
        IF nBad GT 0 THEN BEGIN
           X                     = X[goodBoys]
           Y                     = Y[goodBoys]
           ;; PRINT,"Dropped " + STRCOMPRESS(nBad,/REMOVE_ALL) + " bad boys from the club."
        ENDIF
     ENDIF

     weights            = 1./ABS(Y)
     fixMe              = WHERE(~FINITE(weights),nFixMe)
     IF nFixMe GT 0 THEN BEGIN
        weights[fixMe]  = 0.0
     ENDIF

     CASE curvefit_opt.use_mpFit1D OF
        0: BEGIN

           ;;We can't budge anything but density
           fixA__each_1dfit    = [0,0,0,1,0,0,0]

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

           ;;We can't budge anything but density
           fixA__each_1dfit    = [1,1,1,0,1]

           tmpParams = INIT_KAPPA_FITPARAM_INFO(tmpA_1D,fixA__each_1dfit, $
                                                ERANGE_PEAK=eRange_peak)
           
           A        = MPFITFUN('KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__CONV_TO_F__FUNC', $
                               X,Y, $
                               WEIGHTS=weights, $
                               FUNCTARGS=fa, $
                               BESTNORM=bestNorm, $
                               NFEV=nfev, $
                               FTOL=KEYWORD_SET(curvefit_opt.fit_tol) ? $
                               curvefit_opt.fit_tol : 1e-3, $
                               GTOL=1e-13, $
                               STATUS=status, $
                               BEST_RESID=best_resid, $
                               PFREE_INDEX=ifree, $
                               CALC_FJAC=calc_fjac, $
                               BEST_FJAC=best_fjac, $
                               PARINFO=tmpParams, $
                               QUERY=query, $
                               NPEGGED=npegged, NFREE=nfree, DOF=dof, $
                               COVAR=covar, $
                               PERROR=perror, $
                               MAXITER=KEYWORD_SET(curvefit_opt.max_iter) ? $
                               curvefit_opt.max_iter : 150, $
                               NITER=itNum, $
                               YFIT=yFit, $
                               /QUIET, $
                               ERRMSG=errMsg, $
                               _EXTRA=extra)

           chi2           = TOTAL( (Y-yFit)^2 * ABS(weights) )

           IF FINITE(chi2) THEN BEGIN
              pVal                        = 1 - CHISQR_PDF(chi2,N_ELEMENTS(X)-N_ELEMENTS(WHERE(~fixA__each_1dfit,/NULL))) ;Subtract number of free params
           ENDIF ELSE BEGIN
              pVal                        = -1
           ENDELSE

           IF ~KEYWORD_SET(dont_print_estimates) THEN BEGIN
              IF (status LE 0) OR (status GE 5) THEN PRINT,MPFITFUN__IDENTIFY_ERROR(status)
              IF status EQ 0 THEN STOP
           ENDIF

           IF (WHERE(status EQ OKStatus))[0] NE -1 THEN BEGIN
              fitStatus = 0 
           ENDIF ELSE BEGIN
              fitStatus = 1
           ENDELSE

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

  ;; PRINT,FORMAT='(I0," dists at an angle, out of ",I0," in total, allowed me to do a 1D fit")', $
  ;;       good_1DFits,nTotAngles

END