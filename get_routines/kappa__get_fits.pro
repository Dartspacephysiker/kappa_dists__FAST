PRO KAPPA__GET_FITS,Xorig,Yorig, $
                    orig,kappaFit,gaussFit, $
                    ADD_GAUSSIAN_ESTIMATE=add_gaussian_estimate, $
                    USE_SDT_GAUSSIAN_FIT=use_SDT_Gaussian_fit, $
                    ENERGY_INDS=energy_inds, $
                    ERANGE_PEAK=eRange_peak, $
                    PEAK_IND=peak_ind, $
                    BOUNDS_I=bounds_i, $
                    KAPPA_A=A, $
                    GAUSS_A=AGauss, $
                    KAPPA_FIXA=kappa_fixA, $
                    GAUSS_FIXA=gauss_fixA, $
                    YMAX=yMax, $
                    MAX_ITER=max_iter, $
                    FIT_TOL=fit_tol, $
                    STRINGS=strings, $
                    TRIM_ENERGIES_BELOW_PEAK=trim_energies_below_peak, $
                    OUT_FITTED_PARAMS=out_kappaParams, $
                    OUT_FITTED_GAUSS_PARAMS=out_gaussParams, $
                    OUT_KAPPA_FIT_STRUCTS=kappaFits, $
                    OUT_GAUSS_FIT_STRUCTS=gaussFits, $
                    ADD_FULL_FITS=add_full_fits, $
                    ADD_ANGLESTR=add_angleStr, $
                    OUT_ERANGE_PEAK=out_eRange_peak, $
                    OUT_PARAMSTR=out_paramStr, $
                    DONT_PRINT_ESTIMATES=dont_print_estimates, $
                    FIT_FAIL__USER_PROMPT=fit_fail__user_prompt

  COMMON FIT_MASS,mass

  COMPILE_OPT idl2

  IF N_ELEMENTS(kappa_fixA) EQ 0 THEN BEGIN
     kappa_fixA               = [1,1,1,1,0,0,0] ;Vary bulk E [0], Temperature [1], kappa [2], and density [3]
  ENDIF
  
  IF N_ELEMENTS(gauss_fixA) EQ 0 THEN BEGIN
     gauss_fixA               = [1,1,0,1,0,0,0] ;Vary bulk E [0], Temperature [1], and density [3]
  ENDIF
  

  orig                        = {x:Xorig, $
                                 y:Yorig, $
                                 name:strings.plotTimes[bounds_i]}
  
  ;; CASE 1 OF
  ;;    KEYWORD_SET(fit_fail__user_prompt): BEGIN
  ;;       contKappa             = 0
  ;;    END
  ;;    ELSE: BEGIN
  ;;       contKappa             = 1
  ;;    END
  ;; ENDCASE
  contKappa             = 0

  WHILE ~contKappa DO BEGIN

     ;; IF KEYWORD_SET(failedlasttime) THEN BEGIN
     ;;    STOP
     ;; ENDIF

     ;;Trim energies vector if attempting to fit below peak
     IF KEYWORD_SET(trim_energies_below_peak) THEN BEGIN 
        X                        = Xorig[energy_inds[0]:energy_inds[1]] 
        Y                        = Yorig[energy_inds[0]:energy_inds[1]] 
     ENDIF

     weights                     = 1./ABS(Y)
     yFit                        = CURVEFIT(X,Y,weights,A,SIGMA, $
                                            FUNCTION_NAME='KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__CONV_TO_F' , $
                                            /DOUBLE, $
                                            FITA=kappa_fixA, $
                                            ITMAX=KEYWORD_SET(max_iter) ? max_iter : 150, $
                                            CHI2=chi2, $
                                            ITER=itNum, $
                                            TOL=KEYWORD_SET(fit_tol) ? fit_tol : 1e-3, $
                                            STATUS=fitStatus)
     IF FINITE(chi2) THEN BEGIN
        pVal                        = 1 - CHISQR_PDF(chi2,N_ELEMENTS(X)-4) ;4 for the 4 params that were allowed to participate in this fit
     ENDIF ELSE BEGIN
        pVal                        = -1
     ENDELSE

     ;;need to adjust Y bounds?
     yMax                        = MAX(yFit) 

     IF ~KEYWORD_SET(dont_print_estimates) THEN BEGIN
        PRINT,"Fitted spectral properties: "
        PRINT_KAPPA_FLUX_FIT_PARAMS,A
        PRINT,''
     ENDIF

     IF ~KEYWORD_SET(dont_print_estimates) THEN BEGIN
        CASE fitStatus OF 
           0: BEGIN 
              PRINT,'Fit success!' 
           END 
           1: BEGIN 
              PRINT,'Fit failure! Chi-square increasing without bound!' 
           END 
           2: BEGIN 
              PRINT,'Fit failure! No convergence in ' + STRCOMPRESS(itNum,/REMOVE_ALL) + ' iterations!' 
           END 
        ENDCASE
     ENDIF

     IF KEYWORD_SET(fit_fail__user_prompt) AND fitStatus GT 0 THEN BEGIN
        
        failedlasttime = 1

        input = ''

        cont       = 0

        origA      = A
        orig_fixA  = kappa_fixA
        orig_eInds = energy_inds

        WHILE ~cont DO BEGIN
           PRINT,"(A)djust A  / Adjust (E) inds  / Adjust (F)ixed fit params        / (P)lot sitiation / (PF) Plot full fit"
           PRINT,"(R)estore original settings    /(T)ry again / (S)top and inspect  / (C)ontinue to next fit?"
           READ,input
           CASE STRLOWCASE(input) OF
              "a": BEGIN
                 PRINT,"Here are your failed fit params"
                 PRINT_KAPPA_FLUX_FIT_PARAMS,A
                 PRINT,""
                 
                 testA  = A
                 input2 = ''
                 cont2  = 0
                 WHILE ~cont2 DO BEGIN
                    PRINT,'Edit (B)ulk energy  / Plasma (T)emp / (K)appa   / (D)ensity / (A)ngle offset / '
                    PRINT,'(P)rint A estimates / (S)ave and finish editing / (F)inish editing, no save '
                    READ,input2
                    CASE STRLOWCASE(input2) OF
                       "b": BEGIN
                          READ,newBulk,PROMPT="Enter new bulk energy estimate: "
                          testA[0] = newBulk
                       END
                       "t": BEGIN
                          READ,newTemp,PROMPT="Enter new temperature estimate: "
                          testA[1] = newTemp
                       END
                       "k": BEGIN
                          READ,newKappa,PROMPT="Enter new kappa estimate: "
                          testA[2] = newKappa
                       END
                       "d": BEGIN
                          READ,newDens,PROMPT="Enter new density estimate: "
                          testA[3] = newDens
                       END
                       "a": BEGIN
                          ;; READ,newAngle,PROMPT=STRING(FORMAT='(%"Enter new angle estimate: \r")','')
                          READ,newAngle,PROMPT="Enter new angle estimate: "
                          testA[6] = newAngle
                       END
                       "p": BEGIN
                          PRINT,"Current fit params"
                          PRINT_KAPPA_FLUX_FIT_PARAMS,testA
                          ;; PRINT,STRING(FORMAT='(%"\r")','')
                          PRINT,''
                       END
                       "s": BEGIN
                          A     = testA
                          cont2 = 1
                       END
                       "f": BEGIN
                          cont2  = 1
                       END
                       ELSE: BEGIN
                       END
                    ENDCASE
                 ENDWHILE
              END
              "e": BEGIN
                 test_eInds = energy_inds
                 input2 = ''
                 cont2  = 0

                 PRINT,FORMAT='("Current energy inds: [",I0,I0,"]")',test_eInds
                 PRINT,FORMAT='("Current energies   : [",G0.2," ,",G0.2,"]")',Xorig[test_eInds]
                 PRINT,'Edit (E)nergy inds        / (P)rint Current energy ind info /'
                 PRINT,'(S)ave and finish editing / (F)inish editing, no save        '
                 READ,input2
                 CASE STRLOWCASE(input2) OF
                    "e": BEGIN
                       READ,newEnergyInd,PROMPT="Enter new energy ind 0: "
                       test_eInds[0] = newEnergyInd
                       READ,newEnergyInd,PROMPT="Enter new energy ind 1: "
                       test_eInds[1] = newEnergyInd
                    END
                    "p": BEGIN
                       PRINT,FORMAT='("Current energy inds: [",I0,I0,"]")',test_eInds
                       PRINT,FORMAT='("Current energies   : [",G0.2," ,",G0.2,"]")',Xorig[test_eInds]
                    END
                    "s": BEGIN
                       energy_inds = test_eInds
                    END
                    "f": BEGIN
                       cont2       = 1
                    END
                 ENDCASE
              END
              "f": BEGIN
                 PRINT,"Here are your currently variable params"
                 PRINT_KAPPA_FLUX_FIT_PARAMS,kappa_fixA
                 PRINT,""

                 test_fixA = kappa_fixA
                 input2 = ''
                 cont2  = 0
                 WHILE ~cont2 DO BEGIN
                    PRINT,'Vary (B)ulk energy   / Plasma (T)emp / (K)appa   / (D)ensity / (A)ngle offset / '
                    PRINT,'(P)rint Fixed params / (S)ave and finish editing / (F)inish editing, no save '
                    READ,input2
                    CASE STRLOWCASE(input2) OF
                       "b": BEGIN
                          READ,newBulk,PROMPT="Bulk energy can vary (1/0)?: "
                          test_fixA[0] = newBulk
                       END
                       "t": BEGIN
                          READ,newTemp,PROMPT="Temperature can vary (1/0)?: "
                          test_fixA[1] = newTemp
                       END
                       "k": BEGIN
                          READ,newKappa,PROMPT="Kappa can vary (1/0)?: "
                          test_fixA[2] = newKappa
                       END
                       "d": BEGIN
                          READ,newDens,PROMPT="Density can vary (1/0)? : "
                          test_fixA[3] = newDens
                       END
                       "a": BEGIN
                          ;; READ,newAngle,PROMPT=STRING(FORMAT='(%"Enter new angle estimate: \r")','')
                          READ,newAngle,PROMPT="Angle can vary (1/0)?: "
                          test_fixA[6] = newAngle
                       END
                       "p": BEGIN
                          PRINT,"Current variable vals"
                          PRINT_KAPPA_FLUX_FIT_PARAMS,test_fixA
                          ;; PRINT,STRING(FORMAT='(%"\r")','')
                          PRINT,''
                       END
                       "s": BEGIN
                          PRINT,"Updating kappa_fixA ..."
                          kappa_fixA = test_fixA
                          cont2  = 1
                       END
                       "f": BEGIN
                          cont2  = 1
                       END
                       ELSE: BEGIN
                       END
                    ENDCASE
                 ENDWHILE
              END
              "p": BEGIN
                 cont = 0
                 IF N_ELEMENTS(tempWindow) EQ 0 THEN BEGIN
                    tempWindow = WINDOW(DIMENSIONS=[900,700])
                 ENDIF ELSE BEGIN
                    tempWindow.Erase
                 ENDELSE

                 KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__CONV_TO_F,X,A,yTemp

                 tempPlot  = PLOT(Xorig,Yorig,/XLOG,/YLOG, $
                                  TITLE=strings.plotTimes[bounds_i], $
                                  XTITLE='Energy (eV)',YTITLE='eFlux (eV/cm!U2!N-s-sr-eV)', $
                                  NAME="eFlux Spectrum", $
                                  COLOR='RED', $
                                  CURRENT=tempWindow)
                 tempPlot2 = PLOT(X,yTemp, $
                                  /XLOG,/YLOG, $
                                  /OVERPLOT, $
                                  NAME="Test Kappa dist", $
                                  COLOR='BLACK', $
                                  LINESTYLE='--', $
                                  CURRENT=tempWindow)
                 legend   = LEGEND(TARGET=[tempPlot,tempPlot2], $
                                   /NORMAL, $
                                   POSITION=[0.5,0.8])
              END
              "pf": BEGIN
                 cont = 0
                 IF N_ELEMENTS(tempWindow) EQ 0 THEN BEGIN
                    tempWindow = WINDOW(DIMENSIONS=[400,400])
                 ENDIF ELSE BEGIN
                    tempWindow.Erase
                 ENDELSE

                 KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__CONV_TO_F,Xorig,A,yTemp

                 tempPlot  = PLOT(Xorig,Yorig,/XLOG,/YLOG, $
                                  TITLE=strings.plotTimes[bounds_i], $
                                  XTITLE='Energy (eV)',YTITLE='eFlux (eV/cm!U2!N-s-sr-eV)', $
                                  NAME="eFlux Spectrum", $
                                  COLOR='RED', $
                                  CURRENT=tempWindow)
                 tempPlot2 = PLOT(Xorig,yTemp, $
                                  /XLOG,/YLOG, $
                                  /OVERPLOT, $
                                  NAME="Test Kappa dist", $
                                  COLOR='BLACK', $
                                  LINESTYLE='--', $
                                  CURRENT=tempWindow)
                 legend   = LEGEND(TARGET=[tempPlot,tempPlot2], $
                                   /NORMAL, $
                                   POSITION=[0.5,0.8])
              END
              "r": BEGIN
                 PRINT,'Restoring original settings ...'
                 A           = origA
                 kappa_fixA  = orig_fixA 
                 energy_inds = orig_eInds
              END
              "t": BEGIN
                 cont = 1
                 chi2 = 0.0
              END
              "s": BEGIN
                 STOP
              END
              "c": BEGIN
                 cont      = 1
                 contKappa = 1
              END
              ELSE: BEGIN
                 PRINT,"Invalid option: " + input
              END
           ENDCASE
        ENDWHILE
     ENDIF ELSE BEGIN
        contKappa                = 1
     ENDELSE

  ENDWHILE


  out_kappaParams           = N_ELEMENTS(out_kappaParams) GT 0 ? $
                                [[out_kappaParams],[A]] : A
  out_eRange_peak             = N_ELEMENTS(out_eRange_peak) GT 0 ? $
                                [[out_eRange_peak],[eRange_peak]] : eRange_peak
  out_paramStr                = STRING(FORMAT='(A0,"--",A0,A0,"--orb_",A0,"__",A0,"--",I0,"-",I0,".txt")', $
                                       strings.timeFNStrs[bounds_i], $
                                       strings.eeb_or_ees, $
                                       strings.avgStr, $
                                       strings.orbStr, $
                                       strings.orbDate)


  kappaFit                    = {x:X, $
                                 y:yFit, $
                                 NAME:"Fitted spectrum", $
                                 A:A, $
                                 ;; time:STR_TO_TIME(strings.yearstr[bounds_i]+'/'+strings.plotTimes[bounds_i]), $
                                 time:STR_TO_TIME(strings.yearstr+'/'+strings.plotTimes[bounds_i]), $
                                 time_index:bounds_i, $
                                 ;; XRANGE:xRange, $
                                 ;; YRANGE:yRange, $
                                 ;; XLOG:1, $
                                 ;; YLOG:1, $
                                 fitStatus:fitStatus, $
                                 chi2:chi2, $
                                 pVal:pVal}

  IF KEYWORD_SET(add_full_fits) THEN BEGIN
     KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__CONV_TO_F,Xorig,A,yFull
     kappaFit                 = CREATE_STRUCT(kappaFit,"xFull",Xorig,"yFull",yFull)
  ENDIF

  IF KEYWORD_SET(add_angleStr) THEN BEGIN
     ADD_STR_ELEMENT,kappaFit,'bulkAngleInf',add_angleStr
  ENDIF


  ;; kappaFits       = N_ELEMENTS(kappaFits) EQ 0 ? kappaFit : [kappaFits,kappaFit]
  IF N_ELEMENTS(kappaFits) EQ 0 THEN BEGIN
     kappaFits    = LIST(kappaFit)
  ENDIF ELSE BEGIN
     kappaFits.Add,kappaFit
  ENDELSE

  IF KEYWORD_SET(add_gaussian_estimate) THEN BEGIN
     ;; weights               = SQRT(ABS(Y))
     weights                  = 1./ABS(Y)
     ;; weights[0]            = SQRT(SQRT(weights[0]))
     ;; weights[0:-1]         = 1.

     CASE 1 OF
        KEYWORD_SET(use_SDT_Gaussian_fit): BEGIN
           X_SDT              = Xorig[peak_ind-2:energy_inds[1]]
           Y_SDT              = Yorig[peak_ind-2:energy_inds[1]]
           yGaussFit          = CURVEFIT(X_SDT, Y_SDT, weights, AGauss, sigma, FUNCTION_NAME='MAXWELLIAN_1', $
                                         /DOUBLE, $
                                         ITMAX=KEYWORD_SET(max_iter) ? max_iter : 150, $
                                         ITER=itNum, $
                                         CHI2=chi2, $
                                         TOL=KEYWORD_SET(fit_tol) ? fit_tol : 1e-3, $
                                         STATUS=gaussFitStatus)

           IF ~KEYWORD_SET(dont_print_estimates) THEN BEGIN
              PRINT,'Final fit     : afit=',AGauss
           ENDIF

           IF FINITE(chi2) THEN BEGIN
              pValGauss       = 1 - CHISQR_PDF(chi2,N_ELEMENTS(X_SDT)-2) ;2 for the 2 params that participated in this fit
              real_chi2       = TOTAL((yGaussFit-Y_SDT)^2*weights)/(N_ELEMENTS(yGaussFit) - 3) ; 3 for the number that could have participated
              real_pValGauss  = 1 - CHISQR_PDF(real_chi2,N_ELEMENTS(X_SDT) - 3)
           ENDIF ELSE BEGIN
              pValGauss       = -1
              real_chi2       = -1
              real_pValGauss  = -1
           ENDELSE
        END
        ELSE: BEGIN
           yGaussFit          = CURVEFIT(X, Y, weights, AGauss, sigma, FUNCTION_NAME='KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__CONV_TO_F' , $
                                         /DOUBLE, $
                                         FITA=gauss_fixA, $
                                         ITMAX=KEYWORD_SET(max_iter) ? max_iter : 150, $
                                         ITER=itNum, $
                                         CHI2=chi2, $
                                         TOL=KEYWORD_SET(fit_tol) ? fit_tol : 1e-3, $
                                         STATUS=gaussFitStatus)
           IF FINITE(chi2) THEN BEGIN
              pValGauss       = 1 - CHISQR_PDF(chi2,N_ELEMENTS(X)-3) ;3 for the 3 params that were allowed to participate in this fit
           ENDIF ELSE BEGIN
              pValGauss       = -1
           ENDELSE
        END
     ENDCASE

     ;;need to adjust Y bounds?
     ;; yRange[1]             = MAX(yGaussFit) > yRange[1]
     yMax                     = MAX(yGaussFit) > yMax

     IF ~KEYWORD_SET(dont_print_estimates) THEN BEGIN
        PRINT,''

        CASE gaussFitStatus OF 
           0: BEGIN 
              PRINT,'GaussFit success!' 
           END 
           1: BEGIN 
              PRINT,'GaussFit failure! Chi-square increasing without bound!' 
           END 
           2: BEGIN 
              PRINT,'GaussFit failure! No convergence in ' + STRCOMPRESS(itNum,/REMOVE_ALL) + ' iterations!' 
           END 
        ENDCASE
     ENDIF

     IF KEYWORD_SET(use_SDT_Gaussian_fit) THEN BEGIN
        T  = -1./AGauss[1]
        f0 = AGauss[0]
        density = AGauss[0]*exp(AGauss[1]*X_SDT[1])*1.e-15 * (-1.*2.*3.1416*1.6e-12/(AGauss[1]*(mass)))^1.5 

        ;;Get AGauss in the form we like it
        maxThing              = MAX(yGaussFit,maxGaussInd)
        AGauss_SDT            = AGauss
        AGauss                = [X_SDT[maxGaussInd],T,999,density,A[4],A[5],A[6]]

     ENDIF

     IF ~KEYWORD_SET(dont_print_estimates) THEN BEGIN
        PRINT,"Gaussian fitted spectral properties: "
        PRINT_KAPPA_FLUX_FIT_PARAMS,AGauss
     ENDIF

     gaussFit                 = {x:KEYWORD_SET(use_SDT_Gaussian_fit) ? X_SDT : X, $
                                 y:yGaussFit, $
                                 name:"Gaussian Fitted spectrum" + (KEYWORD_SET(use_SDT_Gaussian_fit) ? "_SDT" : ''), $
                                 A:AGauss, $
                                 ;; time:STR_TO_TIME(strings.yearstr[bounds_i]+'/'+strings.plotTimes[bounds_i]), $
                                 time:STR_TO_TIME(strings.yearstr+'/'+strings.plotTimes[bounds_i]), $
                                 time_index:bounds_i, $
                                 ;; XRANGE:xRange, $
                                 ;; YRANGE:yRange, $
                                 ;; XLOG:1, $
                                 ;; YLOG:1, $
                                 fitStatus:gaussFitStatus, $
                                 chi2:chi2, $
                                 pVal:pValGauss, $
                                 is_sdt_fit:KEYWORD_SET(use_SDT_Gaussian_fit)}

     IF KEYWORD_SET(use_SDT_Gaussian_fit) THEN BEGIN
        gaussFit              = CREATE_STRUCT(gaussFit,"chi2_3vars",real_chi2,"pVal_3vars",real_pValGauss)
     ENDIF

     IF KEYWORD_SET(add_full_fits) THEN BEGIN
        CASE 1 OF
           KEYWORD_SET(use_SDT_Gaussian_fit): BEGIN
              MAXWELLIAN_1,Xorig,AGauss_SDT,yGaussFull
           END
           ELSE: BEGIN
              KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__CONV_TO_F,Xorig,AGauss,yGaussFull
           END
        ENDCASE
        gaussFit                 = CREATE_STRUCT(gaussFit,"xFull",Xorig,"yFull",yGaussFull)

     ENDIF

     IF KEYWORD_SET(add_angleStr) THEN BEGIN
        ADD_STR_ELEMENT,gaussFit,'bulkAngleInf',add_angleStr
     ENDIF


     ;; gaussFits    = N_ELEMENTS(gaussFits) EQ 0 ? gaussFit : [gaussFits,gaussFit]
     IF N_ELEMENTS(gaussFits) EQ 0 THEN BEGIN
        gaussFits    = LIST(gaussFit)
     ENDIF ELSE BEGIN
        gaussFits.Add,gaussFit
     ENDELSE

     out_gaussParams  = N_ELEMENTS(out_gaussParams) GT 0 ? $
                                [[out_gaussParams],[AGauss]] : $
                                AGauss


  ENDIF

END
