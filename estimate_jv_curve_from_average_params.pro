;2017/03/18
PRO ESTIMATE_JV_CURVE_FROM_AVERAGE_PARAMS,jvPlotData,avgs_JVfit, $
                                          ;; USE_SOURCE_AVGS=use_source_avgs, $
                                          A_IN=A_in, $
                                          KAPPALIMS=kappaLims, $   
                                          TEMPLIMS=TempLims, $    
                                          DENSLIMS=DensLims, $    
                                          MAGRATIOLIMS=magRatioLims, $
                                          MULTI_MAGRATIO_MODE=multi_magRatio_mode, $
                                          ITERATIVE_GAME_MODE=iterative_game_mode, $
                                          ITERATIVE_GAME__DENSITY_INCREASE=itergame_NFac, $
                                          ITERATIVE_GAME__TIE_RB_AND_DENS=itergame_tie_R_B_and_dens, $
                                          MAP_TO_100KM=map_to_100km, $
                                          MAP__MULTI_MAGRATIO_ARRAY=multi_magRatio_array, $
                                          MAP__MULTI_KAPPA_ARRAY=multi_kappa_array, $
                                          MAP__2D=map__2D, $
                                          ORIGINATING_ROUTINE=routName, $
                                          OUT_KAPPA_A=A, $
                                          OUT_GAUSS_A=AGauss, $
                                          OUT_PLOTDATA=pData, $
                                          OUT_MULTI_MAGRATIO=mMagDat, $
                                          _EXTRA=e

  COMPILE_OPT IDL2,STRICTARRSUBS

  all_temps   = 0

  maxIter     = 3000
  fTol        = 1D-15
  gTol        = 1D-15

  CURANDPOT__SELECT_T_AND_N,jvPlotData,avgs_JVfit, $
                            TEMPERATURE=temperature, $
                            DENSITY=density, $
                            DONT_MAP_SOURCEDENS=KEYWORD_SET(itergame_tie_R_B_and_dens)
  
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
  Y           = jvPlotData.cur[avgs_JVfit.useInds]*(-1D)
  XError      = jvPlotData.potErr[avgs_JVfit.useInds]
  YError      = jvPlotData.curErr[avgs_JVfit.useInds]
  weights     = 1./ABS(jvPlotData.curErr[avgs_JVfit.useInds])^2

  IF KEYWORD_SET(itergame_tie_R_B_and_dens) THEN BEGIN

     COMMON tieRB,tRB_RBpairs,tRB_fLine,tRB_nFAST,tRB_nFLine,tRB_fLineRE

     STR_ELEMENT,fa_kappa,'tie_R_B_and_dens',itergame_tie_R_B_and_dens,/ADD_REPLACE
     STR_ELEMENT,fa_Gauss,'tie_R_B_and_dens',itergame_tie_R_B_and_dens,/ADD_REPLACE

     IF KEYWORD_SET(all_temps) THEN BEGIN
        tRB_nFAST = jvPlotData.source.NDown[avgs_JVfit.useInds]
        tRB_TFAST = jvPlotData.source.TDown[avgs_JVfit.useInds]

        nDens     = N_ELEMENTS(avgs_JVfit.useInds)
     ENDIF ELSE BEGIN
        tRB_nFAST = avgs_JVfit.N_SC.avg
     ENDELSE

  ENDIF

  CASE 1 OF
     KEYWORD_SET(multi_magRatio_mode): BEGIN

        IF KEYWORD_SET(all_temps) THEN BEGIN
           meanPot   = X
        ENDIF ELSE BEGIN
           meanPot   = 10.^(MEAN(ALOG10(X)))
        ENDELSE

        CASE 1 OF
           KEYWORD_SET(map__2D): BEGIN

              dofK = N_ELEMENTS(X) - 2; treat kappa and R_B as free; - 4
              dofG = N_ELEMENTS(X) - 1; treat R_B as free

              ;;Fix kappa [0] and magratio [3]
              kappaParamStruct[0].fixed = 1B
              kappaParamStruct[3].fixed = 1B

              nMagRatio = N_ELEMENTS(multi_magRatio_array)
              nKappa    = N_ELEMENTS(multi_kappa_array)
              ;; m2D_multi_kappa_array     = multi_kappa_array              # MAKE_ARRAY(nMagRatio,/FLOAT,VALUE=1.0)
              ;; m2D_multi_magRatio_array  = MAKE_ARRAY(nKappa,/FLOAT,VALUE=1.0) # multi_magRatio_array

              IF KEYWORD_SET(all_temps) THEN BEGIN
                 densArr   = MAKE_ARRAY(nDens,nMagRatio,/DOUBLE)
              ENDIF ELSE BEGIN
                 densArr   = MAKE_ARRAY(nMagRatio,/DOUBLE)
              ENDELSE
              ;; densMoms  = MAKE_ARRAY(4,nMagRatio,/DOUBLE)

              kappaArr  = multi_kappa_array                     # MAKE_ARRAY(nMagRatio,/DOUBLE,VALUE=1.0D)
              magRatArr = MAKE_ARRAY(nKappa,/DOUBLE,VALUE=1.0D) # multi_magRatio_array

              chi2ArrK  = MAKE_ARRAY(nKappa,nMagRatio,/DOUBLE)
              chi2ArrG  = MAKE_ARRAY(nMagRatio,/DOUBLE) ;'cause kappa is fixed at infinity, o' course

              fastR_BFac= KEYWORD_SET(map_to_100km) ? $
                          jvPlotData.mRatio.R_B.FAST[0]/jvPlotData.mRatio.R_B.ionos[0] : $
                          1.D

              ;;Reclaim
              A         = kappaParamStruct[*].value
              AGauss    = kappaParamStruct[*].value
              AGauss[0] = 10000.D
              
              ;;Get all dens values
              IF KEYWORD_SET(all_temps) THEN BEGIN
                 FOR k=0,nMagRatio-1 DO BEGIN

                    densArr[*,k] = DENSITY_FACTOR__BARBOSA_1977(meanPot, $
                                                                     tRB_TFAST, $
                                                                     0, $
                                                                     tRB_nFAST, $
                                                                     multi_magRatio_array[k]*fastR_BFac)
                 ENDFOR
              ENDIF ELSE BEGIN
                 FOR k=0,nMagRatio-1 DO BEGIN

                    densArr[k] = MEAN(DENSITY_FACTOR__BARBOSA_1977(meanPot, $
                                                                   A[1], $
                                                                   0, $
                                                                   avgs_JVfit.N_SC.avg, $
                                                                   multi_magRatio_array[k]*fastR_BFac))
                    ;; densMoms[*,k] = MOMENT(DENSITY_FACTOR__BARBOSA_1977(meanPot, $
                    ;;                                                A[1], $
                    ;;                                                0, $
                    ;;                                                avgs_JVfit.N_SC.avg, $
                    ;;                                                multi_magRatio_array[k]*fastR_BFac))
                    ;; IF SQRT(densMoms[1,k])
                 ENDFOR
              ENDELSE

              ;;First kappa
              FOR j=0,nKappa-1 DO BEGIN

                 FOR k=0,nMagRatio-1 DO BEGIN

                    ;; dens = DENSITY_FACTOR__BARBOSA_1977(meanPot, $
                    ;;                                     A[1], $
                    ;;                                     0, $
                    ;;                                     avgs_JVfit.N_SC.avg, $
                    ;;                                     magRatArr[j,k]*fastR_BFac) ;, $
                    ;;                                     ;; MAGICFAC1_OUT=magicFac1, $
                    ;;                                     ;; MAGICFAC2_OUT=magicFac2, $
                    ;;                                     ;; /EXHAUSTIVE_LIMITCHECK)

                    ;; dens = densArr[k]

                    ;; PRINT,kappaArr[j,k],A[1],MEAN(dens),magRatArr[j,k]
                    ;; PRINT,kappaArr[j,k],A[1],densArr[k],magRatArr[j,k]

                    IF KEYWORD_SET(all_temps) THEN BEGIN
                       yFit = KNIGHT_RELATION__DORS_KLETZING_11(kappaArr[j,k], $
                                                                tRB_TFAST, $
                                                                densArr[*,k], $
                                                                ;; A[2], $
                                                                X, $
                                                                magRatArr[j,k], $
                                                                ;; IN_POTBAR=in_potBar, $
                                                                ;; OUT_POTBAR=potBar, $
                                                                /NO_MULT_BY_CHARGE)*1D6
                    ENDIF ELSE BEGIN
                       yFit = KNIGHT_RELATION__DORS_KLETZING_11(kappaArr[j,k], $
                                                                A[1], $
                                                                ;; DENSITY_FACTOR__BARBOSA_1977(X, $
                                                                ;;                              A[1], $
                                                                ;;                              avgs_JVfit.N_SC.avg, $
                                                                ;;                              0, $
                                                                ;;                              magRatArr[j,k], $
                                                                ;;                              MAGICFAC1_OUT=magicFac1, $
                                                                ;;                              MAGICFAC2_OUT=magicFac2, $
                                                                ;;                              /EXHAUSTIVE_LIMITCHECK), $
                                                                densArr[k], $
                                                                ;; A[2], $
                                                                X, $
                                                                magRatArr[j,k], $
                                                                ;; IN_POTBAR=in_potBar, $
                                                                ;; OUT_POTBAR=potBar, $
                                                                /NO_MULT_BY_CHARGE)*1D6

                    ENDELSE
                    
                    ;; chi2 = TOTAL( (Y-yFit)^2 * ABS(weights) * ( (kCurvefit_opt.fit1D__weighting EQ 1) ? ABS(weights) : 1.D) )
                    ;; chi2        = TOTAL( (Y-yFit)^2 * ABS(weights) )
                    chi2           = TOTAL( (Y-yFit)^2 * ABS(weights) ) / dofK
                    chi2ArrK[j,k]  = TEMPORARY(chi2) ;[chi2Arr,TEMPORARY(chi2)]
                    ;; kappaArr[j,k] = A[0]

                 ENDFOR

              ENDFOR

              ;;Now Gauss
              PRINT,"Now Gauss 1-D map ..."
              FOR k=0,nMagRatio-1 DO BEGIN

                    IF KEYWORD_SET(all_temps) THEN BEGIN
                       yFit = KNIGHT_RELATION__DORS_KLETZING_4(tRB_TFAST, $
                                                               densArr[*,k], $
                                                               X, $
                                                               multi_magRatio_array[k], $
                                                               ;; IN_POTBAR=in_potBar, $
                                                               ;; OUT_POTBAR=potBar, $
                                                               /NO_MULT_BY_CHARGE)*1D6
                    ENDIF ELSE BEGIN
                       yFit = KNIGHT_RELATION__DORS_KLETZING_4(AGauss[1], $
                                                               densArr[k], $
                                                               X, $
                                                               multi_magRatio_array[k], $
                                                               ;; IN_POTBAR=in_potBar, $
                                                               ;; OUT_POTBAR=potBar, $
                                                               /NO_MULT_BY_CHARGE)*1D6
                    ENDELSE
                       
                       ;; chi2 = TOTAL( (Y-yFit)^2 * ABS(weights) * ( (kCurvefit_opt.fit1D__weighting EQ 1) ? ABS(weights) : 1.D) )
                       ;; chi2        = TOTAL( (Y-yFit)^2 * ABS(weights) )
                       chi2           = TOTAL( (Y-yFit)^2 * ABS(weights) ) / dofG
                       chi2ArrG[k]    = TEMPORARY(chi2) ;[chi2Arr,TEMPORARY(chi2)]
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
                 STR_ELEMENT,fa_kappa,'in_densities',jvPlotData.source.NDown,/ADD_REPLACE
                 STR_ELEMENT,fa_gauss,'in_densities',jvPlotData.source.NDown,/ADD_REPLACE
                 STR_ELEMENT,fa_kappa,'in_temperatures',jvPlotData.source.TDown,/ADD_REPLACE
                 STR_ELEMENT,fa_gauss,'in_temperatures',jvPlotData.source.TDown,/ADD_REPLACE

                 kappaParamStruct[2].fixed = 1B
                 gaussParamStruct[2].fixed = 1B
              ENDIF


              ;;Save it for senere
              A_in      = kappaParamStruct[*].value

              FOR k=0,nMagRatio-1 DO BEGIN

                 kappaParamStruct[2].value     = DENSITY_FACTOR__BARBOSA_1977(meanPot, $
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
        potVals   = jvPlotData.pot[avgs_jvFit.useinds]

        IF KEYWORD_SET(all_temps) THEN BEGIN
           ;; bestDensK = MEAN(densArr[*,indK2D[1]])
           bestDensK = densArr[*,indK2D[1]]
        ENDIF ELSE BEGIN
           bestDensK = densArr[indK2D[1]]
        ENDELSE

        ;; plotDens = DENSITY_FACTOR__BARBOSA_1977(meanPot, $
        ;;                                         A[1], $
        ;;                                         0, $
        ;;                                         avgs_JVfit.N_SC.avg, $
        ;;                                         bestRB)

        yFit = KNIGHT_RELATION__DORS_KLETZING_11(bestKappa, $
                                                 A[1], $
                                                 bestDensK, $
                                                 potVals, $
                                                 bestRBK, $
                                                 /NO_MULT_BY_CHARGE)*1D6

        A[0]      = bestKappa
        ;; A[1]      = A[1]
        A[2]      = KEYWORD_SET(all_temps) ? MEAN(bestDensK) : bestDensK
        A[3]      = bestRBK

        ;;Gauss

        ;; junkGauss = MIN(mMagDat.chi2[WHERE(mMagDat.kappa GE 8)],indG)
        ;; indG      = (WHERE(mMagDat.kappa GE 8))[indG]
        junkGauss = MIN(chi2ArrG,indG)
        bestRBG   = multi_magRatio_array[indG]
        IF KEYWORD_SET(all_temps) THEN BEGIN
           bestDensG = densArr[*,indG]
        ENDIF ELSE BEGIN
           bestDensG = densArr[indG]
        ENDELSE

        ;; plotDensG = DENSITY_FACTOR__BARBOSA_1977(meanPot, $
        ;;                                         Temperature, $
        ;;                                         0, $
        ;;                                         avgs_JVfit.N_SC.avg, $
        ;;                                         bestRBG)

        YGaussFit = KNIGHT_RELATION__DORS_KLETZING_4(AGauss[1], $
                                                     bestDensG, $
                                                     potVals, $
                                                     bestRBG, $
                                                     /NO_MULT_BY_CHARGE, $
                                                     MASS=mass)*1D6

        AGauss[2] = KEYWORD_SET(all_temps) ? MEAN(bestDensG) : bestDensG
        AGauss[3] = bestRBG


        mMagDat = {K :    {kappa  : TEMPORARY(kappaArr), $
                           T      : A[1], $
                           N      : bestDensK, $
                           magRat : magRatArr, $
                           chi2   : TEMPORARY(chi2ArrK)}, $
                   G :    {T      : AGauss[1], $
                           N      : bestDensG, $
                           magRat : multi_magRatio_array, $
                           chi2   : TEMPORARY(chi2ArrG)}}

     END
     KEYWORD_SET(iterative_game_mode) AND ~KEYWORD_SET(itergame_tie_R_B_and_dens): BEGIN

        IF ~KEYWORD_SET(jvPlotData.use_source_dens) THEN STOP

        A = ESTIMATE_JV_CURVE_FROM_AVERAGES__ITERATIVE_GAME_MODE(X,Y,XError,YError, $
                                                                 DENSITY_INCREASE=itergame_NFac, $
                                                                 TIE_RB_AND_DENS=itergame_tie_R_B_and_dens, $
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
                                                                      DENSITY_INCREASE=itergame_NFac, $
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


        ;; pData = {X         : X      , $
        ;;          Y         : Y      , $
        ;;          Yerror    : Yerror , $
        ;;          YFit      : Yfit   , $
        ;;          YGaussFit : YGaussfit, $
        ;;          AKappa    : A, $
        ;;          AGauss    : AGauss}

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
        IF KEYWORD_SET(itergame_tie_R_B_and_dens) THEN BEGIN

           @common__jv_curve_fit__tie_r_b_and_dens.pro

           R_B_ionos  = tRB_RBpairs[1,VALUE_CLOSEST2(tRB_RBpairs[1,*],A[3],/CONSTRAINED)]
           IF ABS(R_B_ionos - A[3])/R_B_ionos GT 0.2 THEN STOP

           R_B_FAST   = tRB_RBpairs[0,VALUE_CLOSEST2(tRB_RBpairs[1,*],A[3],/CONSTRAINED)]

           A[2]       = JV_CURVE_FIT__TIE_R_B_AND_DENS__GET_DENS(X,A[1],A[3])
           AGauss[2]  = JV_CURVE_FIT__TIE_R_B_AND_DENS__GET_DENS(X,AGauss[1],AGauss[3])

           ;; A[2]       = DENSITY_FACTOR__BARBOSA_1977(10.D^(MEAN(ALOG10(X))), $
           ;;                                           ;; tRB_nFLine = DENSITY_FACTOR__BARBOSA_1977(10.D^(MEAN(ALOG10(pot))), $
           ;;                                           A[1], $
           ;;                                           0, $
           ;;                                           tRB_nFAST, $
           ;;                                           R_B_FAST)

           ;; R_B_ionos  = tRB_RBpairs[1,VALUE_CLOSEST2(tRB_RBpairs[1,*],AGauss[3],/CONSTRAINED)]
           ;; IF ABS(R_B_ionos - AGauss[3])/R_B_ionos GT 0.2 THEN STOP

           ;; R_B_FAST   = tRB_RBpairs[0,VALUE_CLOSEST2(tRB_RBpairs[1,*],AGauss[3],/CONSTRAINED)]
           ;; AGauss[2]  = DENSITY_FACTOR__BARBOSA_1977(10.D^(MEAN(ALOG10(X))), $
           ;;                                           ;; tRB_nFLine = DENSITY_FACTOR__BARBOSA_1977(10.D^(MEAN(ALOG10(pot))), $
           ;;                                           AGauss[1], $
           ;;                                           0, $
           ;;                                           tRB_nFAST, $
           ;;                                           R_B_FAST)
        ENDIF

           ;; tRB_RBpairs[1,*])



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

  pData = {X         : X      , $
           Y         : Y      , $
           Yerror    : Yerror , $
           XFit      : potVals, $
           YFit      : Yfit   , $
           YGaussFit : YGaussFit, $
           AKappa    : A, $
           AGauss    : AGauss, $
           is_sourceDens : KEYWORD_SET(iterative_game_mode)}



END
