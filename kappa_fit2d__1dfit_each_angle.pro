PRO KAPPA_FIT2D__1DFIT_EACH_ANGLE,curDataStr,curFitStr, $
                                  allAngles, $
                                  eRange_peak, $
                                  CURVEFIT_OPT=curvefit_opt, $
                                  ;; SDTDATA_OPT=SDTData_opt, $
                                  FIT2D_INF_LIST=fit2D_inf_list

  COMPILE_OPT idl2

  nTotAngles       = N_ELEMENTS(allAngles)

  ;;We can't budge anything but density
  fixA__each_1dfit    = [0,0,0,1,0,0,0]

  good_1DFits      = 0
  A_1D             = fit2D_inf_list[-1].bestFit1DParams.A
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

     weights            = 1./ABS(Y)
     fixMe              = WHERE(~FINITE(weights),nFixMe)
     IF nFixMe GT 0 THEN BEGIN
        weights[fixMe]  = 0.0
     ENDIF

     yFit               = CURVEFIT(X,Y,weights,A_1D,sigma, $
                                   FUNCTION_NAME='KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__CONV_TO_F' , $
                                   /DOUBLE, $
                                   FITA=fixA__each_1dfit, $
                                   ITMAX=1000, $
                                   CHI2=chi2, $
                                   ITER=itNum, $
                                   ;; TOL=KEYWORD_SET(fit_tol) ? fit_tol : 1e-3, $
                                   TOL=1e-3, $
                                   STATUS=fitStatus)
     CASE fitStatus OF 
        0: BEGIN 
           good_1DFits++
           KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__CONV_TO_F,Xorig,A_1D,yFull
           curFitStr.data[*,iAngle] = yFull
        END 
        1: BEGIN 
        END 
        2: BEGIN 
        END 
     ENDCASE

  ENDFOR

  PRINT,FORMAT='(I0," dists at an angle, out of ",I0," in total, allowed me to do a 1D fit")', $
        good_1DFits,nTotAngles

END