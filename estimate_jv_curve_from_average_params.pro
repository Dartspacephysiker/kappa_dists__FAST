;2017/03/18
PRO ESTIMATE_JV_CURVE_FROM_AVERAGE_PARAMS, $
   jvPlotData,avgs_JVfit, $
   ;; USE_SOURCE_AVGS=use_source_avgs, $
   A_IN=A_in, $
   KAPPALIMS=kappaLims, $   
   TEMPLIMS=TempLims, $    
   DENSLIMS=DensLims, $    
   MAGRATIOLIMS=magRatioLims, $
   MULTI_MAGRATIO_MODE=multi_magRatio_mode, $
   ITERATIVE_GAME_MODE=iterative_game_mode, $
   JV_THEOR__ITERGAME_DENSFAC=jv_theor__itergame_densFac, $
   JV_THEOR__ITERGAME_TIE_R_B_AND_DENS=jv_theor__itergame_tie_R_B_and_dens, $
   ;; ITERATIVE_GAME__DENSITY_INCREASE=itergame_densFac, $
   ;; ITERATIVE_GAME__TIE_RB_AND_DENS=itergame_tie_R_B_and_dens, $
   MAP_TO_100KM=map_to_100km, $
   MAP__MULTI_MAGRATIO_ARRAY=multi_magRatio_array, $
   MAP__MULTI_KAPPA_ARRAY=multi_kappa_array, $
   MAP__2D=map__2D, $
   ORIGINATING_ROUTINE=routName, $
   EFLUX_NOT_NFLUX=eFlux_not_nFlux, $
   OUT_KAPPA_A=A, $
   OUT_GAUSS_A=AGauss, $
   OUT_PLOTDATA=pData, $
   OUT_MULTI_MAGRATIO=mMagDat, $
   _EXTRA=e

  COMPILE_OPT IDL2,STRICTARRSUBS

  all_temps   = 0

  individual_Barbosa_factors = 0

  maxIter     = 3000
  fTol        = 1D-15
  gTol        = 1D-15

  CURANDPOT__SELECT_T_AND_N,jvPlotData,avgs_JVfit, $
                            TEMPERATURE=temperature, $
                            DENSITY=density, $
                            DONT_MAP_SOURCEDENS=KEYWORD_SET(jv_theor__itergame_tie_R_B_and_dens)
  
                             ;;            kappa,       Temp,   Dens, R_B
  A           = KEYWORD_SET(A_in) ? A_in : [  10,Temperature,Density, 1D4]

  ;;Keep the original guesses
  Aorig       = A
  AGaussOrig  = [1000,A[1],A[2],A[3]]

  kappa_fixA  = [0,1,1,0]
  gauss_fixA  = [1,1,1,0]

  PRINT,"Kappa startGuess: "
  PRINT_JV_FIT_PARAMS,A
  PRINT,"Gauss startGuess: "
  PRINT_JV_FIT_PARAMS,AGaussOrig

  kappaParamStruct = INIT_JV_FITPARAM_INFO(A,kappa_fixA, $
                                           KAPPALIMS=kappaLims, $   
                                           TEMPLIMS=TempLims, $    
                                           DENSLIMS=DensLims, $    
                                           MAGRATIOLIMS=magRatioLims)
  gaussParamStruct = INIT_JV_FITPARAM_INFO(AGaussOrig,gauss_fixA, $
                                           KAPPALIMS=kappaLims, $   
                                           TEMPLIMS=TempLims, $    
                                           DENSLIMS=DensLims, $    
                                           MAGRATIOLIMS=magRatioLims)

  fa_kappa    = {no_mult_by_charge : 1B, $
                 is_Maxwellian_fit : 0B  }

  fa_Gauss    = {no_mult_by_charge : 1B, $
                 is_Maxwellian_fit : 1B  }

  jvFitFunc   = 'JV_CURVE_FIT__MAXWELL_KAPPA'
  OKStatus    = [1,2,3,4]        ;These are all the acceptable outcomes of fitting with MPFIT2DFUN

  X           = jvPlotData.pot[avgs_JVfit.useInds]
  XError      = jvPlotData.potErr[avgs_JVfit.useInds]
  CASE 1 OF
     KEYWORD_SET(eFlux_not_nFlux): BEGIN
        Y           = jvPlotData.je[avgs_JVfit.useInds]
        YError      = jvPlotData.jeErr[avgs_JVfit.useInds]
        weights     = 1./ABS(jvPlotData.jeErr[avgs_JVfit.useInds])^2
        yTitle      = '$j_{E \parallel ,i}$ (mW/m!U2!N)'
     END
     ELSE: BEGIN
        Y           = jvPlotData.cur[avgs_JVfit.useInds]*(-1D)
        YError      = jvPlotData.curErr[avgs_JVfit.useInds]
        weights     = 1./ABS(jvPlotData.curErr[avgs_JVfit.useInds])^2
        yTitle      = '$j_{\parallel,i}$ ($\mu$A/m!U2!N)'
     END
  ENDCASE

  nPts        = N_ELEMENTS(X)

  electronPot = jvPlotData.only_downE_pot[avgs_JVfit.useInds]

  IF jvPlotData.info.pot.T_PMFac NE 0 THEN BEGIN
     CURANDPOT__SELECT_T_AND_N,jvPlotData,avgs_JVfit, $
                               TEMPERATURE=T, $
                               ;; DENSITY=Density, $
                               ERR_TEMPERATURE=TErr, $
                               ;; ERR_DENSITY=DensityErr, $
                               ;; DONT_MAP_SOURCEDENS=dont_map_sourceDens, $
                               ;; /SKIP_USEINDS, $
                               /ARRAYS

     X           += T*jvPlotData.info.pot.T_PMFac
     electronPot += T*jvPlotData.info.pot.T_PMFac

  ENDIF


  IF KEYWORD_SET(jv_theor__itergame_tie_R_B_and_dens) THEN BEGIN

     COMMON tieRB,tRB_RBpairs,tRB_fLine,tRB_nFAST,tRB_nFLine,tRB_fLineRE

     STR_ELEMENT,fa_kappa,'tie_R_B_and_dens',jv_theor__itergame_tie_R_B_and_dens,/ADD_REPLACE
     STR_ELEMENT,fa_Gauss,'tie_R_B_and_dens',jv_theor__itergame_tie_R_B_and_dens,/ADD_REPLACE

     IF KEYWORD_SET(all_temps) THEN BEGIN
        tRB_nFAST = jvPlotData.source.NDown[avgs_JVfit.useInds]
        tRB_TFAST = jvPlotData.source.TDown[avgs_JVfit.useInds]

     ENDIF ELSE BEGIN
        tRB_nFAST = avgs_JVfit.N_SC.avg
     ENDELSE

  ENDIF

  CASE 1 OF
     KEYWORD_SET(multi_magRatio_mode): BEGIN

        ;; 2018/01/15 Don't want meanPot!! Want to be TRUE, baby
        CASE 1 OF
           KEYWORD_SET(all_temps): BEGIN
              barbosaPot   = electronPot
           END
           KEYWORD_SET(individual_Barbosa_factors): BEGIN

              barbosaPot   = electronPot

           END
           ELSE: BEGIN

              ;; barbosaPot   = 10.^(MEAN(ALOG10(electronPot)))
              barbosaPot   = MEAN(electronPot)

           END
        ENDCASE

        CASE 1 OF
           KEYWORD_SET(map__2D): BEGIN

              dofK = nPts - 2; treat kappa and R_B as free; - 4
              dofG = nPts - 1; treat R_B as free

              ;;Fix kappa [0] and magratio [3]
              kappaParamStruct[0].fixed = 1B
              kappaParamStruct[3].fixed = 1B

              nMagRatio = N_ELEMENTS(multi_magRatio_array)
              nKappa    = N_ELEMENTS(multi_kappa_array)

              ;; 2018/01/15 Get real! Need to let dens vary with potential to be true to physics
              IF KEYWORD_SET(all_temps) OR KEYWORD_SET(individual_Barbosa_factors) THEN BEGIN
                 densArr   = MAKE_ARRAY(nPts,nMagRatio,/DOUBLE)
              ENDIF ELSE BEGIN
                 densArr   = MAKE_ARRAY(nMagRatio,/DOUBLE)
              ENDELSE
              ;; densArr   = MAKE_ARRAY(nPts,nMagRatio,/DOUBLE)

              kappaArr  = multi_kappa_array                     # MAKE_ARRAY(nMagRatio,/DOUBLE,VALUE=1.0D)
              magRatArr = MAKE_ARRAY(nKappa,/DOUBLE,VALUE=1.0D) # multi_magRatio_array

              chi2ArrK  = MAKE_ARRAY(nKappa,nMagRatio,/DOUBLE)
              chi2ArrG  = MAKE_ARRAY(nMagRatio,/DOUBLE)        ;'cause kappa is fixed at infinity, o' course
              wtSSRK    = MAKE_ARRAY(nKappa,nMagRatio,/DOUBLE) ;weighted sum of squared residuals
              wtSSRG    = MAKE_ARRAY(nMagRatio,/DOUBLE)        ;likevel

              fastR_BFac= jvPlotData.mRatio.R_B.FAST[0]/jvPlotData.mRatio.R_B.ionos[0]

              ;; potBarArrK = MAKE_ARRAY(N_ELEMENTS(X),nKappa,nMagRatio,/DOUBLE)
              ;; potBarArrG = MAKE_ARRAY(N_ELEMENTS(X),nMagRatio,/DOUBLE)

              ;;Reclaim
              A         = kappaParamStruct[*].value
              AGauss    = kappaParamStruct[*].value
              AGauss[0] = 10000.D
              
              ;;Get all dens values
              CASE 1 OF 
                 KEYWORD_SET(all_temps): BEGIN

                    FOR k=0,nMagRatio-1 DO BEGIN

                       densArr[*,k] = DENSITY_FACTOR__BARBOSA_1977(barbosaPot, $
                                                                   tRB_TFAST, $
                                                                   0, $
                                                                   tRB_nFAST, $
                                                                   multi_magRatio_array[k]*fastR_BFac)
                    ENDFOR

                 END
                 KEYWORD_SET(individual_Barbosa_factors): BEGIN

                    FOR k=0,nMagRatio-1 DO BEGIN

                       densArr[*,k] = DENSITY_FACTOR__BARBOSA_1977(barbosaPot, $
                                                                   A[1], $
                                                                   0, $
                                                                   avgs_JVfit.N_SC.avg, $
                                                                   multi_magRatio_array[k]*fastR_BFac)
                    ENDFOR

                 END
                 ELSE: BEGIN
                    FOR k=0,nMagRatio-1 DO BEGIN

                       ;; 2018/01/15 Goodness no! (Think shoes-wearin' cat)
                       ;; densArr[k] = MEAN(DENSITY_FACTOR__BARBOSA_1977(barbosaPot, $
                       ;;                                                A[1], $
                       ;;                                                0, $
                       ;;                                                avgs_JVfit.N_SC.avg, $
                       ;;                                                multi_magRatio_array[k]*fastR_BFac))
                       densArr[k] = DENSITY_FACTOR__BARBOSA_1977(barbosaPot, $
                                                                   A[1], $
                                                                   0, $
                                                                 avgs_JVfit.N_SC.avg, $
                                                                   multi_magRatio_array[k]*fastR_BFac)

                    ENDFOR
                 END
              ENDCASE


              ;;First kappa

              T_thisRound = KEYWORD_SET(all_temps) ? tRB_TFAST : A[1]

              FOR j=0,nKappa-1 DO BEGIN

                 FOR k=0,nMagRatio-1 DO BEGIN

                    N_thisRound = KEYWORD_SET(all_temps) OR KEYWORD_SET(individual_Barbosa_factors) ?  $
                                  densArr[*,k] : $
                                  densArr[k]

                    ;; PRINT,kappaArr[j,k],A[1],MEAN(dens),magRatArr[j,k]
                    ;; PRINT,kappaArr[j,k],A[1],densArr[k],magRatArr[j,k]

                    CASE 1 OF
                       KEYWORD_SET(eFlux_not_nFlux): BEGIN

                          yFit = KAPPA_1__DORS_KLETZING_EQ_15__EFLUX(kappaArr[j,k], $
                                                                     T_thisRound, $
                                                                     N_thisRound, $
                                                                     X, $
                                                                     magRatArr[j,k])

                       END
                       ELSE: BEGIN

                          yFit = KNIGHT_RELATION__DORS_KLETZING_11(kappaArr[j,k], $
                                                                   T_thisRound, $
                                                                   N_thisRound, $
                                                                   X, $
                                                                   magRatArr[j,k], $
                                                                   OUT_POTBAR=potBar, $
                                                                   /NO_MULT_BY_CHARGE)*1D6
                          ;; potBarArrK[*,j,k] = TEMPORARY(potBar)

                       END
                    ENDCASE

                    ;; chi2 = TOTAL( (Y-yFit)^2 * ABS(weights) * ( (kCurvefit_opt.fit1D__weighting EQ 1) ? ABS(weights) : 1.D) )
                    ;; chi2        = TOTAL( (Y-yFit)^2 * ABS(weights) )
                    wtSSRK[j,k]    = TOTAL( (Y-yFit)^2 * ABS(weights) )
                    ;; chi2           = wtSSRK[j,k] / dofK
                    chi2ArrK[j,k]  = wtSSRK[j,k] / dofK ;[chi2Arr,TEMPORARY(chi2)]
                    ;; kappaArr[j,k] = A[0]

                 ENDFOR

              ENDFOR

              ;;Now Gauss
              PRINT,"Now Gauss 1-D map ..."
              T_thisRound = KEYWORD_SET(all_temps) ? tRB_TFAST : AGauss[1]

              FOR k=0,nMagRatio-1 DO BEGIN

                 N_thisRound = KEYWORD_SET(all_temps) OR KEYWORD_SET(individual_Barbosa_factors) ?  $
                               densArr[*,k] : $
                               densArr[k]


                 CASE 1 OF
                    KEYWORD_SET(eFlux_not_nFlux): BEGIN
                       
                       yFit = KAPPA_1__DORS_KLETZING_EQ_14__EFLUX__MAXWELL( $
                              T_thisRound, $
                              N_thisRound, $
                              X, $
                              multi_magRatio_array[k], $
                              OUT_POTBAR=potBar, $
                              MASS=mass)

                       ;; potBarArrG[*,k] = TEMPORARY(potBar)

                    END
                    ELSE: BEGIN

                       yFit = KNIGHT_RELATION__DORS_KLETZING_4(T_thisRound, $
                                                               N_thisRound, $
                                                               X, $
                                                               multi_magRatio_array[k], $
                                                               OUT_POTBAR=potBar, $
                                                               /NO_MULT_BY_CHARGE)*1D6

                       ;; potBarArrG[*,k] = TEMPORARY(potBar)

                    END
                 ENDCASE
                 
                 ;; chi2 = TOTAL( (Y-yFit)^2 * ABS(weights) * ( (kCurvefit_opt.fit1D__weighting EQ 1) ? ABS(weights) : 1.D) )
                 ;; chi2        = TOTAL( (Y-yFit)^2 * ABS(weights) )
                 ;; chi2           = TOTAL( (Y-yFit)^2 * ABS(weights) ) / dofG
                 ;; chi2ArrG[k]    = TEMPORARY(chi2) ;[chi2Arr,TEMPORARY(chi2)]
                 wtSSRG[k]      = TOTAL( (Y-yFit)^2 * ABS(weights) )
                 chi2ArrG[k]    = wtSSRG[k] / dofG ;[chi2Arr,TEMPORARY(chi2)]
                 ;; kappaArr[j,k] = A[0]
              ENDFOR

           END
           ELSE: BEGIN

              kappaParamStruct[3].fixed = 1B

              nMagRatio = N_ELEMENTS(multi_magRatio_array)

              magRatArr = multi_magRatio_array
              kappaArr  = MAKE_ARRAY(nMagRatio,/FLOAT)
              chi2ArrK  = MAKE_ARRAY(nMagRatio,/FLOAT)

              IF KEYWORD_SET(all_temps) THEN BEGIN
                 STR_ELEMENT,fa_kappa,'in_densities',jvPlotData.source.NDown[avgs_JVfit.useInds],/ADD_REPLACE
                 STR_ELEMENT,fa_gauss,'in_densities',jvPlotData.source.NDown[avgs_JVfit.useInds],/ADD_REPLACE
                 STR_ELEMENT,fa_kappa,'in_temperatures',jvPlotData.source.TDown[avgs_JVfit.useInds],/ADD_REPLACE
                 STR_ELEMENT,fa_gauss,'in_temperatures',jvPlotData.source.TDown[avgs_JVfit.useInds],/ADD_REPLACE

                 kappaParamStruct[2].fixed = 1B
                 gaussParamStruct[2].fixed = 1B
              ENDIF


              ;;Save it for senere
              A_in      = kappaParamStruct[*].value

              FOR k=0,nMagRatio-1 DO BEGIN

                 kappaParamStruct[2].value     = DENSITY_FACTOR__BARBOSA_1977(barbosaPot, $
                                                                              kappaParamStruct[1].value, $
                                                                              0, $
                                                                              tRB_nFAST, $
                                                                              magRatArr[j,k])


                 kappaParamStruct[3].value     = multi_magRatio_array[k]
                 kappaParamStruct[3].limits[1] = multi_magRatio_array[k]+.1

                 A      = MPFITFUN(jvFitFunc, $
                                   X,Y, $
                                   /NAN, $
                                   WEIGHTS=weights, $
                                   FUNCTARGS=fa_kappa, $
                                   BESTNORM=bestNorm, $
                                   NFEV=nfev, $
                                   FTOL=fTol, $
                                   GTOL=gTol, $
                                   STATUS=status, $
                                   BEST_RESID=best_resid, $
                                   PFREE_INDEX=ifree, $
                                   CALC_FJAC=calc_fjac, $
                                   BEST_FJAC=best_fjac, $
                                   PARINFO=kappaParamStruct, $
                                   QUERY=query, $
                                   NPEGGED=npegged, $
                                   NFREE=nfree, $
                                   DOF=dof, $
                                   COVAR=covar, $
                                   PERROR=perror, $
                                   MAXITER=maxIter, $
                                   NITER=itNum, $
                                   YFIT=yFit, $
                                   /QUIET, $
                                   ERRMSG=errMsg, $
                                   _EXTRA=extra)
                 
                 ;; chi2 = TOTAL( (Y-yFit)^2 * ABS(weights) * ( (kCurvefit_opt.fit1D__weighting EQ 1) ? ABS(weights) : 1.D) )
                 ;; chi2        = TOTAL( (Y-yFit)^2 * ABS(weights) )
                 chi2        = TOTAL( (Y-yFit)^2 * ABS(weights) ) / dof
                 chi2Arr[k]  = TEMPORARY(chi2) ;[chi2Arr,TEMPORARY(chi2)]
                 kappaArr[k] = A[0]

              ENDFOR


           END
        ENDCASE

        ;;Kappa
        junk      = MIN(chi2ArrK,indK)
        indK2D    = ARRAY_INDICES(chi2ArrK,indK)

        bestKappa = kappaArr[indK]
        bestRBK   = magRatArr[indK]
        potVals   = X;        potVals   = jvPlotData.pot[avgs_jvFit.useinds]
        extPotVals = POWGEN(1D2,1D5,1.05)

        IF KEYWORD_SET(all_temps) OR KEYWORD_SET(individual_Barbosa_factors) THEN BEGIN
           bestDensK = densArr[*,indK2D[1]]
        ENDIF ELSE BEGIN
           bestDensK = densArr[indK2D[1]] ;bestDensK = densArr[indK2D[1]]
        ENDELSE

        yFit = KNIGHT_RELATION__DORS_KLETZING_11(bestKappa, $
                                                 A[1], $
                                                 bestDensK, $
                                                 potVals, $
                                                 bestRBK, $
                                                 /NO_MULT_BY_CHARGE)*1D6

        yExtFit = KNIGHT_RELATION__DORS_KLETZING_11(bestKappa, $
                                                 A[1], $
                                                 bestDensK, $
                                                 extPotVals, $
                                                 bestRBK, $
                                                 /NO_MULT_BY_CHARGE)*1D6

        A[0]      = bestKappa
        ;; A[1]      = A[1]
        A[2]      = KEYWORD_SET(all_temps) OR KEYWORD_SET(individual_Barbosa_factors) ? MEAN(bestDensK) : bestDensK
        A[3]      = bestRBK

        ;;Gauss
        junkGauss = MIN(chi2ArrG,indG)
        bestRBG   = multi_magRatio_array[indG]
        IF KEYWORD_SET(all_temps) OR KEYWORD_SET(individual_Barbosa_factors) THEN BEGIN
           bestDensG = densArr[*,indG]
        ENDIF ELSE BEGIN
           bestDensG = densArr[indG] ; densArr[indG]
        ENDELSE

        YGaussFit = KNIGHT_RELATION__DORS_KLETZING_4(AGauss[1], $
                                                     bestDensG, $
                                                     potVals, $
                                                     bestRBG, $
                                                     /NO_MULT_BY_CHARGE, $
                                                     MASS=mass)*1D6

        YGaussExtFit = KNIGHT_RELATION__DORS_KLETZING_4(AGauss[1], $
                                                     bestDensG, $
                                                     extPotVals, $
                                                     bestRBG, $
                                                     /NO_MULT_BY_CHARGE, $
                                                     MASS=mass)*1D6

        AGauss[2] = KEYWORD_SET(all_temps) OR KEYWORD_SET(individual_Barbosa_factors) ? MEAN(bestDensG) : bestDensG
        AGauss[3] = bestRBG


        mMagDat = {K :    {kappa  : TEMPORARY(kappaArr), $
                           T      : A[1], $
                           ;; N      : MEDIAN(bestDensK), $
                           N      : bestDensK, $
                           magRat : magRatArr, $
                           chi2   : TEMPORARY(chi2ArrK), $
                           nParms : 4, $
                           nPts   : nPts, $
                           wtdSSR : TEMPORARY(wtSSRK), $
                           wts    : weights}, $
                   G :    {T      : AGauss[1], $
                           ;; N      : MEDIAN(bestDensG), $
                           N      : bestDensG, $
                           magRat : multi_magRatio_array, $
                           chi2   : TEMPORARY(chi2ArrG), $
                           nParms : 3, $
                           nPts   : nPts, $
                           wtdSSR : TEMPORARY(wtSSRG), $
                           wts    : weights}}

     END
     KEYWORD_SET(iterative_game_mode) AND ~KEYWORD_SET(jv_theor__itergame_tie_R_B_and_dens): BEGIN

        IF ~KEYWORD_SET(jvPlotData.use_source_dens) THEN STOP

        A = ESTIMATE_JV_CURVE_FROM_AVERAGES__ITERATIVE_GAME_MODE(X,Y,XError,YError, $
                                                                 DENSITY_INCREASE=jv_theor__itergame_densFac, $
                                                                 TIE_RB_AND_DENS=jv_theor__itergame_tie_R_B_and_dens, $
                                                                 A_IN=Aorig, $
                                                                 WEIGHTS=weights, $
                                                                 JVPLOTDATA=jvPlotData, $
                                                                 AVGS_JVFIT=avgs_jvFit, $
                                                                 JVFITFUNC=jvFitFunc, $
                                                                 FUNCTARGS=fa_kappa, $
                                                                 PARINFO=kappaParamStruct, $
                                                                 MAXITER=maxIter, $
                                                                 FTOL=fTol, $
                                                                 GTOL=gtol, $
                                                                 OUT_YFIT=YFit, $
                                                                 OUT_INFOSTRUCT=gameFitInfoK, $
                                                                 OUT_PARINFO=kappaParamStructNye)

        AGauss = ESTIMATE_JV_CURVE_FROM_AVERAGES__ITERATIVE_GAME_MODE(X,Y,XError,YError, $
                                                                      DENSITY_INCREASE=jv_theor__itergame_densFac, $
                                                                      A_IN=Aorig, $
                                                                      WEIGHTS=weights, $
                                                                      JVPLOTDATA=jvPlotData, $
                                                                      AVGS_JVFIT=avgs_jvFit, $
                                                                      JVFITFUNC=jvFitFunc, $
                                                                      FUNCTARGS=fa_Gauss, $
                                                                      PARINFO=gaussParamStruct, $
                                                                      MAXITER=maxIter, $
                                                                      FTOL=fTol, $
                                                                      GTOL=gtol, $
                                                                      OUT_YFIT=YGaussFit, $
                                                                      OUT_INFOSTRUCT=gameFitInfoG, $
                                                                      OUT_PARINFO=gaussParamStructNye)


        potVals = POWGEN(1D2,1D5,1.05)

        YFit       = CALL_FUNCTION(jvFitFunc, $
                                   potVals, $
                                   A, $
                                   IS_MAXWELLIAN_FIT=fa_kappa.is_Maxwellian_fit, $
                                   NO_MULT_BY_CHARGE=fa_kappa.no_mult_by_charge)
        YGaussFit  = CALL_FUNCTION(jvFitFunc, $
                                   potVals, $
                                   AGauss, $
                                   IS_MAXWELLIAN_FIT=fa_Gauss.is_Maxwellian_fit, $
                                   NO_MULT_BY_CHARGE=fa_Gauss.no_mult_by_charge)

     END
     ELSE: BEGIN


        A           = MPFITFUN(jvFitFunc, $
                               X,Y, $
                               /NAN, $
                               WEIGHTS=weights, $
                               FUNCTARGS=fa_kappa, $
                               BESTNORM=bestNorm, $
                               NFEV=nfev, $
                               FTOL=fTol, $
                               GTOL=gTol, $
                               STATUS=status, $
                               BEST_RESID=best_resid, $
                               PFREE_INDEX=ifree, $
                               CALC_FJAC=calc_fjac, $
                               BEST_FJAC=best_fjac, $
                               PARINFO=kappaParamStruct, $
                               QUERY=query, $
                               NPEGGED=npegged, $
                               NFREE=nfree, $
                               DOF=dof, $
                               COVAR=covar, $
                               PERROR=perror, $
                               MAXITER=maxIter, $
                               NITER=itNum, $
                               YFIT=yFit, $
                               /QUIET, $
                               XTOL=fTol, $
                               ERRMSG=errMsg, $
                               _EXTRA=extra)

        AGauss      = MPFITFUN(jvFitFunc, $
                               X,Y, $
                               /NAN, $
                               WEIGHTS=weights, $
                               FUNCTARGS=fa_Gauss, $
                               BESTNORM=bestNorm, $
                               NFEV=nfev, $
                               FTOL=fTol, $
                               GTOL=gTol, $
                               STATUS=gaussStatus, $
                               BEST_RESID=best_resid, $
                               PFREE_INDEX=ifree, $
                               CALC_FJAC=calc_fjac, $
                               BEST_FJAC=best_fjac, $
                               PARINFO=gaussParamStruct, $
                               QUERY=query, $
                               NPEGGED=npegged, $
                               NFREE=nfree, $
                               DOF=dof, $
                               COVAR=covar, $
                               PERROR=perror, $
                               MAXITER=maxIter, $
                               NITER=itNum, $
                               YFIT=yGaussFit, $
                               /QUIET, $
                               ERRMSG=errMsg, $
                               _EXTRA=extra)

        ;;Update density with the REAL fitparam if we were playing a game
        IF KEYWORD_SET(jv_theor__itergame_tie_R_B_and_dens) THEN BEGIN

           @common__jv_curve_fit__tie_r_b_and_dens.pro

           R_B_ionos  = tRB_RBpairs[1,VALUE_CLOSEST2(tRB_RBpairs[1,*],A[3],/CONSTRAINED)]
           IF ABS(R_B_ionos - A[3])/R_B_ionos GT 0.2 THEN STOP

           R_B_FAST   = tRB_RBpairs[0,VALUE_CLOSEST2(tRB_RBpairs[1,*],A[3],/CONSTRAINED)]

           A[2]       = JV_CURVE_FIT__TIE_R_B_AND_DENS__GET_DENS(X,A[1],A[3])
           AGauss[2]  = JV_CURVE_FIT__TIE_R_B_AND_DENS__GET_DENS(X,AGauss[1],AGauss[3])

        ENDIF

        PRINT,"Kappa fitparams: "
        PRINT_JV_FIT_PARAMS,A
        PRINT,""
        PRINT,"Gauss fitparams: "
        PRINT_JV_FIT_PARAMS,AGauss
        PRINT,""

        potVals = POWGEN(1D2,1D5,1.05)

        YFit       = CALL_FUNCTION(jvFitFunc, $
                                   potVals, $
                                   A, $
                                   IS_MAXWELLIAN_FIT=fa_kappa.is_Maxwellian_fit, $
                                   NO_MULT_BY_CHARGE=fa_kappa.no_mult_by_charge)
        YGaussFit  = CALL_FUNCTION(jvFitFunc, $
                                   potVals, $
                                   AGauss, $
                                   IS_MAXWELLIAN_FIT=fa_Gauss.is_Maxwellian_fit, $
                                   NO_MULT_BY_CHARGE=fa_Gauss.no_mult_by_charge)

     END
  ENDCASE

  pData = {X              : X      , $
           Y              : Y      , $
           Yerror         : Yerror , $
           XFit           : potVals, $
           YFit           : Yfit   , $
           YGaussFit      : YGaussFit, $
           yTitle         : TEMPORARY(yTitle), $
           AKappa         : A, $
           AGauss         : AGauss, $
           is_sourceDens  : KEYWORD_SET(iterative_game_mode) OR KEYWORD_SET(jv_theor__itergame_tie_R_B_and_dens)}

  IF KEYWORD_SET(extPotVals) THEN BEGIN
     pData = CREATE_STRUCT(pData, $
                           "extended",{pot: extPotVals, $
                                       yfit: yExtFit, $
                                       ygaussfit: ygaussextfit})
  ENDIF


END
