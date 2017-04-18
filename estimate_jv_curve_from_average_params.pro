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

  maxIter     = 3000
  fTol     = 1D-15
  gTol        = 1D-15

  CURANDPOT__SELECT_T_AND_N,jvPlotData,avgs_JVfit, $
                            TEMPERATURE=temperature, $
                            DENSITY=density
  
  ;; IF KEYWORD_SET(jvPlotData.use_source_avgs) THEN BEGIN
  ;;    Temperature = avgs_JVfit.T_SC.avg
  ;;    Density     = avgs_JVfit.N_SC.avg
  ;; ENDIF ELSE BEGIN
  ;;    Temperature = avgs_JVfit.T.avg
  ;;    Density     = avgs_JVfit.N.avg
  ;; ENDELSE
                             ;;            kappa,       Temp,   Dens, R_B
  A           = KEYWORD_SET(A_in) ? A_in : [  10,Temperature,Density, 1D4]

  ;;Keep the original guesses
  Aorig       = A
  AGaussOrig  = A

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
  gaussParamStruct = INIT_JV_FITPARAM_INFO(TEMPORARY(A),gauss_fixA, $
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

  CASE 1 OF
     KEYWORD_SET(multi_magRatio_mode): BEGIN

        CASE 1 OF
           KEYWORD_SET(map__2D): BEGIN


              dof = N_ELEMENTS(X); - 4

              ;;Fix kappa [0] and magratio [3]
              kappaParamStruct[0].fixed = 1B
              kappaParamStruct[3].fixed = 1B

              nMagRatio = N_ELEMENTS(multi_magRatio_array)
              nKappa    = N_ELEMENTS(multi_kappa_array)
              ;; m2D_multi_kappa_array     = multi_kappa_array              # MAKE_ARRAY(nMagRatio,/FLOAT,VALUE=1.0)
              ;; m2D_multi_magRatio_array  = MAKE_ARRAY(nKappa,/FLOAT,VALUE=1.0) # multi_magRatio_array

              kappaArr  = multi_kappa_array                   # MAKE_ARRAY(nMagRatio,/FLOAT,VALUE=1.0)
              magRatArr = MAKE_ARRAY(nKappa,/FLOAT,VALUE=1.0) # multi_magRatio_array

              chi2Arr   = MAKE_ARRAY(nKappa,nMagRatio,/FLOAT)

              ;;Reclaim
              A         = kappaParamStruct[*].value

              FOR j=0,nKappa-1 DO BEGIN

                 FOR k=0,nMagRatio-1 DO BEGIN

                    ;; kappaParamStruct[3].value     = magRatArr[j,k]
                    ;; kappaParamStruct[0].value     = kappaArr[j,k]

                    yFit = KNIGHT_RELATION__DORS_KLETZING_11(kappaArr[j,k], $
                                                             A[1], $
                                                             A[2], $
                                                             X, $
                                                             magRatArr[j,k], $
                                                             IN_POTBAR=in_potBar, $
                                                             OUT_POTBAR=potBar, $
                                                             /NO_MULT_BY_CHARGE)*1D6
                    
                    ;; chi2 = TOTAL( (Y-yFit)^2 * ABS(weights) * ( (kCurvefit_opt.fit1D__weighting EQ 1) ? ABS(weights) : 1.D) )
                    ;; chi2        = TOTAL( (Y-yFit)^2 * ABS(weights) )
                    chi2        = TOTAL( (Y-yFit)^2 * ABS(weights) ) / dof
                    chi2Arr[j,k]  = TEMPORARY(chi2) ;[chi2Arr,TEMPORARY(chi2)]
                    ;; kappaArr[j,k] = A[0]

                 ENDFOR

              ENDFOR

           END
           ELSE: BEGIN

              kappaParamStruct[3].fixed = 1B

              nMagRatio = N_ELEMENTS(multi_magRatio_array)

              magRatArr = multi_magRatio_array
              kappaArr  = MAKE_ARRAY(nMagRatio,/FLOAT)
              chi2Arr   = MAKE_ARRAY(nMagRatio,/FLOAT)

              FOR k=0,nMagRatio-1 DO BEGIN

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

        mMagDat = {kappa  : TEMPORARY(kappaArr), $
                   T      : Temperature, $
                   N      : Density, $
                   magRat : magRatArr, $
                   chi2   : TEMPORARY(chi2Arr)}
     END
     KEYWORD_SET(iterative_game_mode): BEGIN

        IF ~KEYWORD_SET(jvPlotData.use_source_avgs) THEN STOP

        ;; dRB_arr     = !NULL
        ;; RB_arr      = !NULL
        ;; RE_arr      = !NULL
        ;; RLim_arr    = !NULL
        ;; dens_arr    = !NULL

        ;; done        = 0
        ;; count       = 0

        ;; ;;initialize old mirror ratio
        ;; oldRB       = MEAN(jvPlotData.mRatio.R_B_IGRF.ionos)

        ;; ;;GEOPACKers
        ;; traceErr = 0.0001       ;Check out http://geo.phys.spbu.ru/~tsyganenko/Examples1_and_2.html. 0.0001 is what Tsyganenko himself uses. Baller, right?
        ;; dsMax    = 0.05         ;Max R_E step size

        ;; __TRACE_ANTIPARALLEL_B = 1
        ;; __TRACE_PARALLEL_B     = -1

        ;; time_epoch  = UTC_TO_CDF_EPOCH(jvPlotData.mRatio.time)
        ;; arbInd      = N_ELEMENTS(jvPlotData.mRatio.DOY.Year)/2
        ;; GEOPACK_RECALC_08,jvPlotData.mRatio.DOY.Year[arbInd], $
        ;;                   jvPlotData.mRatio.DOY.Month[arbInd], $
        ;;                   jvPlotData.mRatio.DOY.Day[arbInd], $
        ;;                   jvPlotData.mRatio.DOY.Hour[arbInd], $
        ;;                   jvPlotData.mRatio.DOY.Min[arbInd], $
        ;;                   jvPlotData.mRatio.DOY.Sec[arbInd], $
        ;;                   /DATE, $
        ;;                   VGSE=jvPlotData.mRatio.swDat.v_SW[*,arbInd], $
        ;;                   TILT=thisTilt
        ;; tmpPos = [jvPlotData.mRatio.fa_pos[arbInd,0],jvPlotData.mRatio.fa_pos[arbInd,1],jvPlotData.mRatio.fa_pos[arbInd,2]]
        ;; IF jvPlotData.mRatio.ilat[arbInd] GT 0 THEN BEGIN
        ;;    trace_to_equator = __TRACE_ANTIPARALLEL_B
        ;;    trace_to_ionos   = __TRACE_PARALLEL_B
        ;; ENDIF ELSE BEGIN
        ;;    trace_to_equator = __TRACE_PARALLEL_B
        ;;    trace_to_ionos   = __TRACE_ANTIPARALLEL_B
        ;; ENDELSE

        ;; GEOPACK_CONV_COORD_08,tmpPos[0],tmpPos[1],tmpPos[2], $
        ;;                       FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
        ;;                       /FROM_GSE,/TO_GSW,EPOCH=time_epoch[arbInd]

        ;; GEOPACK_IGRF_GSW_08,FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
        ;;                     FAST_Bx_IGRF,FAST_By_IGRF,FAST_Bz_IGRF, $
        ;;                     EPOCH=time_epoch[arbInd]

        ;; FAST_GSM       = [FAST_GSM_x,FAST_GSM_y,FAST_GSM_z]
        ;; FAST_RE        = SQRT(TOTAL(FAST_GSM^2))
        ;; FAST_B_IGRF    = [FAST_Bx_IGRF,FAST_By_IGRF,FAST_Bz_IGRF]
        ;; FAST_B_IGRFMag = SQRT(TOTAL(FAST_B_IGRF^2))

        ;; CASE 1 OF
        ;;    KEYWORD_SET(jvPlotData.mRatio.BModelInfo.T89): BEGIN
        ;;       tmpParMod = jvPlotData.mRatio.BModelInfo.IOPT_89[arbInd]
        ;;    END
        ;;    KEYWORD_SET(jvPlotData.mRatio.BModelInfo.T01): BEGIN
        ;;       tmpParMod = jvPlotData.mRatio.BModelInfo.parMod[*,arbInd]
        ;;       tmpParMod[4:5] = jvPlotData.mRatio.BModelInfo.GParms[arbInd,*]
        ;;    END
        ;;    KEYWORD_SET(jvPlotData.mRatio.BModelInfo.TS04): BEGIN
        ;;       tmpParMod = jvPlotData.mRatio.BModelInfo.parMod[*,arbInd]
        ;;    END
        ;; ENDCASE

        ;; oldRLim = MEAN(jvPlotData.mRatio.R_E.downTail)
        ;; oldoldRLim = 0
        ;; ;; RLim_arr = [oldRLim]
        ;; WHILE ~done DO BEGIN

        ;;    A           = MPFITFUN(jvFitFunc, $
        ;;                           X,Y, $
        ;;                           /NAN, $
        ;;                           WEIGHTS=weights, $
        ;;                           FUNCTARGS=fa_kappa, $
        ;;                           BESTNORM=bestNorm, $
        ;;                           NFEV=nfev, $
        ;;                           FTOL=fTol, $
        ;;                           GTOL=gTol, $
        ;;                           STATUS=status, $
        ;;                           BEST_RESID=best_resid, $
        ;;                           PFREE_INDEX=ifree, $
        ;;                           CALC_FJAC=calc_fjac, $
        ;;                           BEST_FJAC=best_fjac, $
        ;;                           PARINFO=kappaParamStruct, $
        ;;                           QUERY=query, $
        ;;                           NPEGGED=npegged, $
        ;;                           NFREE=nfree, $
        ;;                           DOF=dof, $
        ;;                           COVAR=covar, $
        ;;                           PERROR=perror, $
        ;;                           MAXITER=maxIter, $
        ;;                           NITER=itNum, $
        ;;                           YFIT=yFit, $
        ;;                           /QUIET, $
        ;;                           ERRMSG=errMsg, $
        ;;                           _EXTRA=extra)

        ;;    ;; AGauss      = MPFITFUN(jvFitFunc, $
        ;;    ;;                        X,Y, $
        ;;    ;;                        /NAN, $
        ;;    ;;                        WEIGHTS=weights, $
        ;;    ;;                        FUNCTARGS=fa_Gauss, $
        ;;    ;;                        BESTNORM=bestNorm, $
        ;;    ;;                        NFEV=nfev, $
        ;;    ;;                        FTOL=fTol, $
        ;;    ;;                        GTOL=gTol, $
        ;;    ;;                        STATUS=gaussStatus, $
        ;;    ;;                        BEST_RESID=best_resid, $
        ;;    ;;                        PFREE_INDEX=ifree, $
        ;;    ;;                        CALC_FJAC=calc_fjac, $
        ;;    ;;                        BEST_FJAC=best_fjac, $
        ;;    ;;                        PARINFO=gaussParamStruct, $
        ;;    ;;                        QUERY=query, $
        ;;    ;;                        NPEGGED=npegged, $
        ;;    ;;                        NFREE=nfree, $
        ;;    ;;                        DOF=dof, $
        ;;    ;;                        COVAR=covar, $
        ;;    ;;                        PERROR=perror, $
        ;;    ;;                        MAXITER=maxIter, $
        ;;    ;;                        NITER=itNum, $
        ;;    ;;                        YFIT=yGaussFit, $
        ;;    ;;                        /QUIET, $
        ;;    ;;                        ERRMSG=errMsg, $
        ;;    ;;                        _EXTRA=extra)


        ;;    ;; IF ( (count MOD 5) EQ 0 ) THEN STOP

        ;;    newRB = A[3]

        ;;    dRB   = newRB-oldRB
        ;;    PRINT,FORMAT='("Kappa fitparams (",I0,", dRB = ",F0.2,")")',count++,dRB
        ;;    PRINT_JV_FIT_PARAMS,A
        ;;    PRINT,""

        ;;    checkRB    = oldRB
        ;;    PRINT,FORMAT='("Trying to get R_B = ",F0.2," (starting with R_B = ",F0.2,")")',newRB,oldRB
        ;;    oldMaxRLimStep = 0.2
        ;;    maxRLimStep = 0.1   
        ;;    nRepeats    = 0
        ;;    WHILE ABS(checkRB-newRB) GT 4 DO BEGIN

        ;;       maxRLimStep = ( ABS(checkRB-newRB)/100. ) < maxRLimStep

        ;;       comeNearer = checkRB GT newRB
        ;;       RLim       = oldRLim + ( maxRLimStep ) * ( comeNearer ? -1 : 1 ) 

        ;;       IF (maxRLimStep EQ oldMaxRLimStep) AND (RLim EQ oldoldRLim) THEN BEGIN

        ;;          nRepeats++

        ;;          IF nRepeats GE 2 THEN BEGIN

        ;;             maxRLimStep *= 0.99
        ;;             RLim         = MEAN([oldRLim,oldoldRLim])
        ;;             nRepeats     = 0
        ;;          ENDIF
                 
        ;;       ENDIF ELSE BEGIN
        ;;          nRepeats = 0
        ;;       ENDELSE

        ;;       GEOPACK_TRACE_08,FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
        ;;                        ;; trace_to_equator,(KEYWORD_SET(TS04) ? tmpParMod : !NULL), $
        ;;                        trace_to_equator,tmpParMod, $
        ;;                        downTail_GSM_x,downTail_GSM_y,downTail_GSM_z, $
        ;;                        /REFINE, $
        ;;                        EQUATOR=to_equator, $
        ;;                        R0=R0, $
        ;;                        RLIM=RLim, $
        ;;                        FLINE=fLine_toEq, $
        ;;                        T89=T89, $
        ;;                        T01=T01, $
        ;;                        TS04=TS04, $
        ;;                        ;; IGRF=IGRF, $
        ;;                        /IGRF, $
        ;;                        TILT=thisTilt, $ ;should be in degrees
        ;;                        EPOCH=time_epoch[arbInd], $
        ;;                        DSMAX=dsMax, $
        ;;                        ERR=traceErr

        ;;       GEOPACK_TRACE_08,FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
        ;;                        ;; trace_to_ionos,(KEYWORD_SET(TS04) ? tmpParMod : !NULL), $
        ;;                        trace_to_ionos,tmpParMod, $
        ;;                        ionos_GSM_x,ionos_GSM_y,ionos_GSM_z, $
        ;;                        /REFINE, $
        ;;                        R0=R0, $
        ;;                        RLIM=RLim, $
        ;;                        FLINE=fLine_toIonos, $
        ;;                        /IONOSPHERE, $
        ;;                        T89=T89, $
        ;;                        T01=T01, $
        ;;                        TS04=TS04, $
        ;;                        ;; IGRF=IGRF, $
        ;;                        /IGRF, $
        ;;                        TILT=thisTilt, $ ;should be in degrees
        ;;                        EPOCH=time_epoch[arbInd], $
        ;;                        DSMAX=dsMax, $
        ;;                        ERR=traceErr

        ;;       ;; CASE 1 OF
        ;;       ;;    KEYWORD_SET(jvPlotData.BModelInfo.T89): BEGIN

        ;;       ;;       GEOPACK_T89,tmpParMod,downTail_GSM_x,downTail_GSM_y,downTail_GSM_z, $
        ;;       ;;                   downTail_Bx,downTail_By,downTail_Bz, $
        ;;       ;;                   TILT=thisTilt, $
        ;;       ;;                   EPOCH=time_epoch[arbInd]

        ;;       ;;       GEOPACK_T89,tmpParMod,FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
        ;;       ;;                   FAST_Bx,FAST_By,FAST_Bz, $
        ;;       ;;                   TILT=thisTilt, $
        ;;       ;;                   EPOCH=time_epoch[arbInd]

        ;;       ;;       GEOPACK_T89,tmpParMod,ionos_GSM_x,ionos_GSM_y,ionos_GSM_z, $
        ;;       ;;                   ionos_Bx,ionos_By,ionos_Bz, $
        ;;       ;;                   TILT=thisTilt, $
        ;;       ;;                   EPOCH=time_epoch[arbInd]

        ;;       ;;    END
        ;;       ;;    ELSE: BEGIN

        ;;       ;;       GEOPACK_TS04,tmpParMod,downTail_GSM_x,downTail_GSM_y,downTail_GSM_z, $
        ;;       ;;                    downTail_Bx,downTail_By,downTail_Bz, $
        ;;       ;;                    TILT=thisTilt, $
        ;;       ;;                    EPOCH=time_epoch[arbInd], $
        ;;       ;;                    IOPGEN=IOPGen, $
        ;;       ;;                    IOPT=IOPT, $
        ;;       ;;                    IOPB=IOPB, $
        ;;       ;;                    IOPR=IOPR

        ;;       ;;       GEOPACK_TS04,tmpParMod,FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
        ;;       ;;                    FAST_Bx,FAST_By,FAST_Bz, $
        ;;       ;;                    TILT=thisTilt, $
        ;;       ;;                    EPOCH=time_epoch[arbInd], $
        ;;       ;;                    IOPGEN=IOPGen, $
        ;;       ;;                    IOPT=IOPT, $
        ;;       ;;                    IOPB=IOPB, $
        ;;       ;;                    IOPR=IOPR

        ;;       ;;       GEOPACK_TS04,tmpParMod,ionos_GSM_x,ionos_GSM_y,ionos_GSM_z, $
        ;;       ;;                    ionos_Bx,ionos_By,ionos_Bz, $
        ;;       ;;                    TILT=thisTilt, $
        ;;       ;;                    EPOCH=time_epoch[arbInd], $
        ;;       ;;                    IOPGEN=IOPGen, $
        ;;       ;;                    IOPT=IOPT, $
        ;;       ;;                    IOPB=IOPB, $
        ;;       ;;                    IOPR=IOPR

        ;;       ;;    END
        ;;       ;; ENDCASE

        ;;       GEOPACK_IGRF_GSW_08,downTail_GSM_x,downTail_GSM_y,downTail_GSM_z, $
        ;;                           downTail_Bx_IGRF,downTail_By_IGRF,downTail_Bz_IGRF, $
        ;;                           EPOCH=time_epoch[arbInd]

        ;;       GEOPACK_IGRF_GSW_08,ionos_GSM_x,ionos_GSM_y,ionos_GSM_z, $
        ;;                           ionos_Bx_IGRF,ionos_By_IGRF,ionos_Bz_IGRF, $
        ;;                           EPOCH=time_epoch[arbInd]


        ;;       downTail_GSM = [downTail_GSM_x,downTail_GSM_y,downTail_GSM_z]
        ;;       ionos_GSM    = [ionos_GSM_x,ionos_GSM_y,ionos_GSM_z]

        ;;       downTail_RE         = SQRT(TOTAL(downTail_GSM^2))
        ;;       ionos_RE            = SQRT(TOTAL(ionos_GSM^2))

        ;;       ;; FAST_km             = (FAST_RE     - 1.D ) * RE_to_km
        ;;       ;; downTail_km         = (downTail_RE - 1.D ) * RE_to_km
        ;;       ;; ionos_km            = (ionos_RE    - 1.D ) * RE_to_km

        ;;       ;; FAST_B              = [FAST_Bx,FAST_By,FAST_Bz]
        ;;       ;; downTail_B          = [downTail_Bx,downTail_By,downTail_Bz]
        ;;       ;; ionos_B             = [ionos_Bx,ionos_By,ionos_Bz]

        ;;       ;; FAST_BMag           = SQRT(TOTAL(FAST_B^2))
        ;;       ;; downTail_BMag       = SQRT(TOTAL(downTail_B^2))
        ;;       ;; ionos_BMag          = SQRT(TOTAL(ionos_B^2))

        ;;       ;; R_B_FAST            = FAST_BMag/downTail_BMag
        ;;       ;; R_B_ionos           = ionos_BMag/downTail_BMag

        ;;       downTail_B_IGRF     = [downTail_Bx_IGRF,downTail_By_IGRF,downTail_Bz_IGRF]
        ;;       ionos_B_IGRF        = [ionos_Bx_IGRF,ionos_By_IGRF,ionos_Bz_IGRF]

        ;;       downTail_B_IGRFMag  = SQRT(TOTAL(downTail_B_IGRF^2))
        ;;       ionos_B_IGRFMag     = SQRT(TOTAL(ionos_B_IGRF^2))

        ;;       R_B_IGRF_FAST       = FAST_B_IGRFMag/downTail_B_IGRFMag
        ;;       R_B_IGRF_ionos      = ionos_B_IGRFMag/downTail_B_IGRFMag

        ;;       checkRB             = R_B_IGRF_ionos
        ;;       oldoldRLim          = oldRLim
        ;;       oldRLim             = RLim

        ;;       oldMaxRLimStep      = maxRLimStep
        ;;       PRINT,"MaxRLIMSTEP (RLIM) ",maxRLimStep,'(' + STRCOMPRESS(RLim,/REMOVE_ALL) + ')'

        ;;       IF maxRLimStep LT 1D-5 THEN BEGIN
        ;;          PRINT,"Nuclear"
        ;;          BREAK
        ;;       ENDIF
        ;;    ENDWHILE

        ;;    PRINT,"Cool, settled on " + STRCOMPRESS(downTail_RE,/REMOVE_ALL) + " R_E"

        ;;    ;;See if we're done (no R_B_IGRF_FAST means we skipped the loop, children)
        ;;    done = N_ELEMENTS(R_B_IGRF_FAST) EQ 0 
        ;;    IF done THEN BREAK
        ;;    jvPlotData.mRatio.R_B_IGRF.FAST[*]  = TEMPORARY(R_B_IGRF_FAST)
        ;;    jvPlotData.mRatio.R_B_IGRF.ionos[*] = TEMPORARY(R_B_IGRF_ionos)

        ;;    ;;Update B ratio
        ;;    kappaParamStruct[0].value = A[0]
        ;;    kappaParamStruct[2].value = avgs_JVfit.N_SC.avg/MEAN(jvPlotData.mRatio.R_B_IGRF.FAST[avgs_JVfit.useInds])

        ;;    dRB_arr                = [dRB_arr,(checkRB-newRB)]
        ;;    RB_arr                 = [RB_arr ,newRB]
        ;;    RLim_arr               = [RLim_arr,RLim]
        ;;    RE_arr                 = [RE_arr ,downTail_RE]
        ;;    dens_arr               = [dens_arr,kappaParamStruct[2].value]

        ;;    IF count GT 2 THEN BEGIN

        ;;       PRINT,FORMAT='("dRB_arr  : ",20(F0.2,:,", "))',dRB_arr
        ;;       PRINT,FORMAT='("RB_arr   : ",20(F0.2,:,", "))',RB_arr
        ;;       PRINT,FORMAT='("RLim_arr : ",20(F0.2,:,", "))',RLim_arr
        ;;       PRINT,FORMAT='("RE_arr   : ",20(F0.2,:,", "))',RE_arr
        ;;       PRINT,FORMAT='("dens_arr : ",20(G0.4,:,", "))',dens_arr
        ;;       WAIT,1
        ;;    ENDIF

        ;;    oldRLim                = RLim
        ;;    oldRB                  = newRB
        ;;    oldRE                  = downTail_RE
        ;; ENDWHILE

        A = ESTIMATE_JV_CURVE_FROM_AVERAGES__ITERATIVE_GAME_MODE(X,Y,XError,YError, $
                                                                 DENSITY_INCREASE=itergame_NFac, $
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


        pData = {X         : X      , $
                 Y         : Y      , $
                 Yerror    : Yerror , $
                 YFit      : Yfit   , $
                 YGaussFit : YGaussfit}

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

        PRINT,"Kappa fitparams: "
        PRINT_JV_FIT_PARAMS,A
        PRINT,""
        PRINT,"Gauss fitparams: "
        PRINT_JV_FIT_PARAMS,AGauss
        PRINT,""

        pData = {X         : X      , $
                 Y         : Y      , $
                 Yerror    : Yerror , $
                 YFit      : Yfit   , $
                 YGaussFit : YGaussFit}


     END
  ENDCASE

END
