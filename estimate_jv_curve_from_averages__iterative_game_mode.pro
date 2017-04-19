;2017/04/17
FUNCTION ESTIMATE_JV_CURVE_FROM_AVERAGES__ITERATIVE_GAME_MODE,X,Y,XError,YError, $
   DENSITY_INCREASE=NFac, $
   A_IN=A_in, $
   WEIGHTS=weights, $
   JVPLOTDATA=jvPlotData, $
   AVGS_JVFIT=avgs_jvFit, $
   JVFITFUNC=jvFitFunc, $
   FUNCTARGS=functArgs, $
   PARINFO=parInfo, $
   MAXITER=maxIter, $
   FTOL=fTol, $
   GTOL=gtol, $
   OUT_YFIT=YFit, $
   OUT_INFOSTRUCT=gameFitInfo, $
   OUT_PARINFO=parInfoNye

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;;GEOPACKers
  traceErr = 0.0001             ;Check out http://geo.phys.spbu.ru/~tsyganenko/Examples1_and_2.html. 0.0001 is what Tsyganenko himself uses. Baller, right?
  dsMax    = 0.05               ;Max R_E step size

  __TRACE_ANTIPARALLEL_B = 1
  __TRACE_PARALLEL_B     = -1

  CASE 1 OF
     KEYWORD_SET(functArgs.is_Maxwellian_fit): BEGIN
        fit_type = 'Maxwellian'
        defMaxRLimStep = 0.1

     END
     ELSE: BEGIN
        fit_type = 'Kappa'
        defMaxRLimStep = 0.1
     END
  ENDCASE

  A           = A_in

  done        = 0
  count       = 0

  useInds     = avgs_JVfit.useInds

  ;;initialize old mirror ratio
  oldRB       = MEAN(jvPlotData.mRatio.R_B.FAST[useInds])
  parInfoNye  = parInfo

  IF KEYWORD_SET(NFac) THEN BEGIN
     PRINT,FORMAT='(A0,F0.2)',"Iterative game density increase factor : ",NFac
     NFactor  = NFac
     parInfoNye[2].value *= NFactor
  ENDIF ELSE BEGIN
     NFactor  = 1
  ENDELSE

  ;;First two get updated later
  dRB_arr     = 0
  RB_arr      = oldRB
  RE_arr      = MEAN(jvPlotData.mRatio.R_E.downTail[useInds])
  RLim_arr    = MEAN(jvPlotData.mRatio.R_E.downTail[useInds])
  ;; dens_arr    = parInfoNye[2].value
  dens_arr    = DENSITY_FACTOR__BARBOSA_1977(10.D^(MEAN(ALOG10(jvPlotData.pot[useInds]))), $
                                             parInfoNye[1].value, $
                                             0, $
                                             parInfoNye[2].value, $
                                             MEAN(jvPlotData.mRatio.R_B.FAST[useInds]))

  ;;Talk about it
  PRINT,FORMAT='(A0,A0,A0,F0.2)','Beginning game mode for ',fit_type,' fit with R_B = ',oldRB
  

  time_epoch  = UTC_TO_CDF_EPOCH(jvPlotData.mRatio.time)
  arbInd      = N_ELEMENTS(jvPlotData.mRatio.DOY.Year[useInds])/2
  arbInd      = useInds[arbInd] 
  GEOPACK_RECALC_08,jvPlotData.mRatio.DOY.Year[arbInd], $
                    jvPlotData.mRatio.DOY.Month[arbInd], $
                    jvPlotData.mRatio.DOY.Day[arbInd], $
                    jvPlotData.mRatio.DOY.Hour[arbInd], $
                    jvPlotData.mRatio.DOY.Min[arbInd], $
                    jvPlotData.mRatio.DOY.Sec[arbInd], $
                    /DATE, $
                    VGSE=jvPlotData.mRatio.swDat.v_SW[*,arbInd], $
                    TILT=thisTilt

  tmpPos = [jvPlotData.mRatio.fa_pos[arbInd,0],jvPlotData.mRatio.fa_pos[arbInd,1],jvPlotData.mRatio.fa_pos[arbInd,2]]

  IF jvPlotData.mRatio.ilat[arbInd] GT 0 THEN BEGIN
     trace_to_equator = __TRACE_ANTIPARALLEL_B
     trace_to_ionos   = __TRACE_PARALLEL_B
  ENDIF ELSE BEGIN
     trace_to_equator = __TRACE_PARALLEL_B
     trace_to_ionos   = __TRACE_ANTIPARALLEL_B
  ENDELSE

  CASE 1 OF
     KEYWORD_SET(jvPlotData.mRatio.BModelInfo.T89): BEGIN
        tmpParMod = jvPlotData.mRatio.BModelInfo.IOPT_89[arbInd]

        T89       = 1
        T96       = 0
        T01       = 0
        TS04      = 0

     END
     KEYWORD_SET(jvPlotData.mRatio.BModelInfo.T96): BEGIN
        tmpParMod = jvPlotData.mRatio.BModelInfo.parMod[*,arbInd]

        T89       = 0
        T96       = 1
        T01       = 0
        TS04      = 0

     END
     KEYWORD_SET(jvPlotData.mRatio.BModelInfo.T01): BEGIN
        tmpParMod = jvPlotData.mRatio.BModelInfo.parMod[*,arbInd]
        tmpParMod[4:5] = jvPlotData.mRatio.BModelInfo.GParms[arbInd,*]

        T89       = 0
        T96       = 0
        T01       = 1
        TS04      = 0

     END
     KEYWORD_SET(jvPlotData.mRatio.BModelInfo.TS04): BEGIN
        tmpParMod = jvPlotData.mRatio.BModelInfo.parMod[*,arbInd]

        T89       = 0
        T96       = 0
        T01       = 0
        TS04      = 1

     END
  ENDCASE

  GEOPACK_CONV_COORD_08,tmpPos[0],tmpPos[1],tmpPos[2], $
                        FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
                        /FROM_GSE,/TO_GSW,EPOCH=time_epoch[arbInd]

  GEOPACK_IGRF_GSW_08,FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
                      FAST_Bx_IGRF,FAST_By_IGRF,FAST_Bz_IGRF, $
                      EPOCH=time_epoch[arbInd]

  CASE 1 OF
     KEYWORD_SET(T89): BEGIN

        GEOPACK_T89,tmpParMod,FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
                    FAST_Bx,FAST_By,FAST_Bz, $
                    TILT=thisTilt, $
                    EPOCH=time_epoch[arbInd]

     END
     KEYWORD_SET(T96): BEGIN

        GEOPACK_T96,tmpParMod,FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
                    FAST_Bx,FAST_By,FAST_Bz, $
                    TILT=thisTilt, $
                    EPOCH=time_epoch[arbInd]

     END
     KEYWORD_SET(T01): BEGIN

        GEOPACK_T01,tmpParMod,FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
                    FAST_Bx,FAST_By,FAST_Bz, $
                    TILT=thisTilt, $
                    EPOCH=time_epoch[arbInd]

     END
     ELSE: BEGIN

        GEOPACK_TS04,tmpParMod,FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
                     FAST_Bx,FAST_By,FAST_Bz, $
                     TILT=thisTilt, $
                     EPOCH=time_epoch[arbInd], $
                     IOPGEN=IOPGen, $
                     IOPT=IOPT, $
                     IOPB=IOPB, $
                     IOPR=IOPR

     END
  ENDCASE

  FAST_GSM       = [FAST_GSM_x,FAST_GSM_y,FAST_GSM_z]
  FAST_RE        = SQRT(TOTAL(FAST_GSM^2))
  FAST_B_IGRF    = [FAST_Bx_IGRF,FAST_By_IGRF,FAST_Bz_IGRF]
  ;; FAST_B_IGRFMag = SQRT(TOTAL(FAST_B_IGRF^2))

  FAST_B         = [FAST_Bx,FAST_By,FAST_Bz] + FAST_B_IGRF
  FAST_BMag      = SQRT(TOTAL((FAST_B + FAST_B_IGRF)^2))

  oldRLim = MEAN(jvPlotData.mRatio.R_E.downTail[useInds])
  oldoldRLim = 0

  ;; RLim_arr = [oldRLim]

  oldMaxRLimStep  = 0.2
  maxRLimStep     = defMaxRLimStep

  nNukes  = 0
  WHILE ~done DO BEGIN

     A           = MPFITFUN(jvFitFunc, $
                            X,Y, $
                            /NAN, $
                            WEIGHTS=weights, $
                            FUNCTARGS=functArgs, $
                            BESTNORM=bestNorm, $
                            NFEV=nfev, $
                            FTOL=fTol, $
                            GTOL=gTol, $
                            STATUS=status, $
                            BEST_RESID=best_resid, $
                            PFREE_INDEX=ifree, $
                            /CALC_FJAC, $
                            BEST_FJAC=best_fjac, $
                            PARINFO=parInfoNye, $
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

     ;; IF ( (count MOD 5) EQ 0 ) THEN STOP

     newRB = A[3]

     dRB   = newRB-oldRB
     PRINT,FORMAT='(A0," (possibly inconsistent) fitparams --- count = ",I0,", dRB = ",F0.2,", R_B = ",F0.2)',fit_type,count++,dRB,newRB
     PRINT_JV_FIT_PARAMS,A
     PRINT,""

     checkRB    = oldRB
     PRINT,FORMAT='("Trying to get R_B = ",F0.2," ...")',newRB

     wasSatisfied = ABS(oldRB-newRB) LE 4

     IF wasSatisfied THEN BEGIN
        RLim            = oldRLim
        dsMax           = 0.01
        traceErr        = 0.00001
        ;; oldMaxRLimStep  = 0.2
        ;; maxRLimStep     = 0.1
     ENDIF ELSE BEGIN

        dsMax           = 0.05
        traceErr        = 0.0001
        ;; oldMaxRLimStep  = 0.2
        ;; maxRLimStep     = defMaxRLimStep

     ENDELSE
     ;; WHILE ABS(checkRB-newRB) GT 4 DO BEGIN

     nRepeats    = 0
     count2      = 0
     done2       = 0
     WHILE ~done2 DO BEGIN
        
        maxRLimStep = ( ABS(checkRB-newRB)/100. ) < maxRLimStep
        comeNearer  = checkRB GT newRB
        RLim        = oldRLim + ( maxRLimStep ) * ( comeNearer ? -1 : 1 ) 

        IF ((maxRLimStep EQ oldMaxRLimStep) AND (RLim EQ oldoldRLim)) OR $
           (checkRB EQ newRB) $
        THEN BEGIN

           nRepeats++

           IF nRepeats GE 2 THEN BEGIN

              maxRLimStep *= 0.95
              RLim         = MEAN([oldRLim,oldoldRLim])
              nRepeats     = 0
           ENDIF
           
        ENDIF ELSE BEGIN
           nRepeats = 0
        ENDELSE

        GEOPACK_TRACE_08,FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
                         ;; trace_to_equator,(KEYWORD_SET(TS04) ? tmpParMod : !NULL), $
                         trace_to_equator,tmpParMod, $
                         downTail_GSM_x,downTail_GSM_y,downTail_GSM_z, $
                         /REFINE, $
                         EQUATOR=to_equator, $
                         R0=R0, $
                         RLIM=RLim, $
                         FLINE=fLine_toEq, $
                         T89=T89, $
                         T01=T01, $
                         TS04=TS04, $
                         /IGRF, $
                         TILT=thisTilt, $ ;should be in degrees
                         EPOCH=time_epoch[arbInd], $
                         DSMAX=dsMax, $
                         ERR=traceErr

        GEOPACK_TRACE_08,FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
                         ;; trace_to_ionos,(KEYWORD_SET(TS04) ? tmpParMod : !NULL), $
                         trace_to_ionos,tmpParMod, $
                         ionos_GSM_x,ionos_GSM_y,ionos_GSM_z, $
                         /REFINE, $
                         R0=R0, $
                         RLIM=RLim, $
                         FLINE=fLine_toIonos, $
                         /IONOSPHERE, $
                         T89=T89, $
                         T01=T01, $
                         TS04=TS04, $
                         /IGRF, $
                         TILT=thisTilt, $ ;should be in degrees
                         EPOCH=time_epoch[arbInd], $
                         DSMAX=dsMax, $
                         ERR=traceErr

        ;;Calculate external contribution
        CASE 1 OF
           KEYWORD_SET(T89): BEGIN

              GEOPACK_T89,tmpParMod,downTail_GSM_x,downTail_GSM_y,downTail_GSM_z, $
                          downTail_Bx,downTail_By,downTail_Bz, $
                          TILT=thisTilt, $
                          EPOCH=time_epoch[arbInd]

              GEOPACK_T89,tmpParMod,FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
                          FAST_Bx,FAST_By,FAST_Bz, $
                          TILT=thisTilt, $
                          EPOCH=time_epoch[arbInd]

              GEOPACK_T89,tmpParMod,ionos_GSM_x,ionos_GSM_y,ionos_GSM_z, $
                          ionos_Bx,ionos_By,ionos_Bz, $
                          TILT=thisTilt, $
                          EPOCH=time_epoch[arbInd]

           END
           KEYWORD_SET(T96): BEGIN

              GEOPACK_T96,tmpParMod,downTail_GSM_x,downTail_GSM_y,downTail_GSM_z, $
                          downTail_Bx,downTail_By,downTail_Bz, $
                          TILT=thisTilt, $
                          EPOCH=time_epoch[arbInd]

              GEOPACK_T96,tmpParMod,FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
                          FAST_Bx,FAST_By,FAST_Bz, $
                          TILT=thisTilt, $
                          EPOCH=time_epoch[arbInd]

              GEOPACK_T96,tmpParMod,ionos_GSM_x,ionos_GSM_y,ionos_GSM_z, $
                          ionos_Bx,ionos_By,ionos_Bz, $
                          TILT=thisTilt, $
                          EPOCH=time_epoch[arbInd]

           END
           KEYWORD_SET(T01): BEGIN

              GEOPACK_T01,tmpParMod,downTail_GSM_x,downTail_GSM_y,downTail_GSM_z, $
                          downTail_Bx,downTail_By,downTail_Bz, $
                          TILT=thisTilt, $
                          EPOCH=time_epoch[arbInd]

              GEOPACK_T01,tmpParMod,FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
                          FAST_Bx,FAST_By,FAST_Bz, $
                          TILT=thisTilt, $
                          EPOCH=time_epoch[arbInd]

              GEOPACK_T01,tmpParMod,ionos_GSM_x,ionos_GSM_y,ionos_GSM_z, $
                          ionos_Bx,ionos_By,ionos_Bz, $
                          TILT=thisTilt, $
                          EPOCH=time_epoch[arbInd]

           END
           ELSE: BEGIN

              GEOPACK_TS04,tmpParMod,downTail_GSM_x,downTail_GSM_y,downTail_GSM_z, $
                           downTail_Bx,downTail_By,downTail_Bz, $
                           TILT=thisTilt, $
                           EPOCH=time_epoch[arbInd], $
                           IOPGEN=IOPGen, $
                           IOPT=IOPT, $
                           IOPB=IOPB, $
                           IOPR=IOPR

              GEOPACK_TS04,tmpParMod,FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
                           FAST_Bx,FAST_By,FAST_Bz, $
                           TILT=thisTilt, $
                           EPOCH=time_epoch[arbInd], $
                           IOPGEN=IOPGen, $
                           IOPT=IOPT, $
                           IOPB=IOPB, $
                           IOPR=IOPR

              GEOPACK_TS04,tmpParMod,ionos_GSM_x,ionos_GSM_y,ionos_GSM_z, $
                           ionos_Bx,ionos_By,ionos_Bz, $
                           TILT=thisTilt, $
                           EPOCH=time_epoch[arbInd], $
                           IOPGEN=IOPGen, $
                           IOPT=IOPT, $
                           IOPB=IOPB, $
                           IOPR=IOPR

           END
        ENDCASE

        ;;Internal contribution
        GEOPACK_IGRF_GSW_08,downTail_GSM_x,downTail_GSM_y,downTail_GSM_z, $
                            downTail_Bx_IGRF,downTail_By_IGRF,downTail_Bz_IGRF, $
                            EPOCH=time_epoch[arbInd]

        GEOPACK_IGRF_GSW_08,ionos_GSM_x,ionos_GSM_y,ionos_GSM_z, $
                            ionos_Bx_IGRF,ionos_By_IGRF,ionos_Bz_IGRF, $
                            EPOCH=time_epoch[arbInd]


        downTail_GSM        = [downTail_GSM_x,downTail_GSM_y,downTail_GSM_z]
        ionos_GSM           = [ionos_GSM_x,ionos_GSM_y,ionos_GSM_z]

        downTail_RE         = SQRT(TOTAL(downTail_GSM^2))
        ionos_RE            = SQRT(TOTAL(ionos_GSM^2))

        downTail_B          = [downTail_Bx,downTail_By,downTail_Bz]
        ionos_B             = [ionos_Bx,ionos_By,ionos_Bz]

        downTail_B_IGRF     = [downTail_Bx_IGRF,downTail_By_IGRF,downTail_Bz_IGRF]
        ionos_B_IGRF        = [ionos_Bx_IGRF,ionos_By_IGRF,ionos_Bz_IGRF]

        ;; downTail_B_IGRFMag  = SQRT(TOTAL(downTail_B_IGRF^2))
        ;; ionos_B_IGRFMag     = SQRT(TOTAL(ionos_B_IGRF^2))

        downTail_BMag       = SQRT(TOTAL((downTail_B + downTail_B_IGRF)^2))
        ionos_BMag          = SQRT(TOTAL((ionos_B + ionos_B_IGRF)^2))

        R_B_FAST            = FAST_BMag/downTail_BMag
        R_B_ionos           = ionos_BMag/downTail_BMag

        ;; R_B_IGRF_FAST       = FAST_B_IGRFMag/downTail_B_IGRFMag
        ;; R_B_IGRF_ionos      = ionos_B_IGRFMag/downTail_B_IGRFMag

        ;; checkRB             = R_B_IGRF_ionos
        checkRB             = R_B_ionos
        oldoldRLim          = oldRLim
        oldRLim             = RLim

        oldMaxRLimStep      = maxRLimStep
        IF (count2++ MOD 25) EQ 0 THEN  PRINT,"MaxRLIMSTEP (RLIM) ",maxRLimStep,'(' + STRCOMPRESS(RLim,/REMOVE_ALL) + ')'

        isSatisfied         = ABS(checkRB-oldRB) LE 4

        IF wasSatisfied THEN BEGIN
           tmpString = "I was satisfied ..."
           IF isSatisfied THEN BEGIN
              tmpString += ' and am satisfied!'
              PRINT,tmpString
              BREAK
           ENDIF ELSE BEGIN
              tmpString += ' and am no longer satisfied!'
              PRINT,tmpString
              ;; STOP
           ENDELSE
        ENDIF

        IF maxRLimStep LT 1D-5 THEN BEGIN
           PRINT,"Nuclear"
           nNukes++
           BREAK
        ENDIF

        IF RLim GT 200 THEN BEGIN
           PRINT,"This is definitely bogus; RLim > 200"
           STOP
        ENDIF
           
        IF downTail_RE LT FAST_RE THEN BEGIN
           PRINT,"At FAST! Bogusssss"
           BREAK
        ENDIF

     ENDWHILE

     CASE count2 OF
        0: BEGIN
           PRINT,"Flew right through with " + STRCOMPRESS(downTail_RE,/REMOVE_ALL) + " R_E"
        END
        ELSE: BEGIN
           PRINT,"Cool, settled on " + STRCOMPRESS(downTail_RE,/REMOVE_ALL) + " R_E"

        END
     ENDCASE

     ;;Update B ratio
     IF STRMATCH(STRUPCASE(fit_type),'KAPPA') THEN parInfoNye[0].value = A[0]

     ;; jvPlotData.mRatio.R_B_IGRF.FAST[*]  = TEMPORARY(R_B_FAST)
     ;; jvPlotData.mRatio.R_B_IGRF.ionos[*] = TEMPORARY(R_B_ionos)

     ;; jvPlotData.mRatio.R_B.FAST[useInds]  = TEMPORARY(R_B_FAST)
     newRB                               = TEMPORARY(R_B_FAST) > 1
     ;; jvPlotData.mRatio.R_B.ionos[useInds] = TEMPORARY(R_B_ionos)

     ;; parInfoNye[2].value                 = avgs_JVfit.N_SC.avg/MEAN(jvPlotData.mRatio.R_B_IGRF.FAST[avgs_JVfit.useInds])*NFactor
     ;; parInfoNye[2].value                 = avgs_JVfit.N_SC.avg/MEAN(jvPlotData.mRatio.R_B.FAST[avgs_JVfit.useInds])*NFactor

     ;;Use A[2] or parInfoNye[2].value in what's below?
     ;; PRINT,A[2]
     ;; PRINT,parInfoNye[2].value
     
     parInfoNye[2].value                 = DENSITY_FACTOR__BARBOSA_1977(10.D^(MEAN(ALOG10(jvPlotData.pot[useInds]))), $
                                                                        parInfoNye[1].value, $
                                                                        0, $
                                                                        avgs_JVfit.N_SC.avg, $ ;??????????
                                                                        newRB > 1)

     dens_arr                            = [dens_arr,parInfoNye[2].value]
     RLim_arr                            = [RLim_arr,RLim]
     RE_arr                              = [RE_arr ,downTail_RE]
     dRB_arr                             = [dRB_arr,(oldRB-newRB)]
     RB_arr                              = [RB_arr ,newRB]

     ;;See if we're done (no R_B_FAST means we skipped the loop, children)
     ;; done = (N_ELEMENTS(R_B_FAST)) EQ 0 AND ( ABS((dens_arr[-1]-dens_arr[-2])/dens_arr[-2]) LE 0.1 )
     done = isSatisfied AND ( ABS((dens_arr[-1]-dens_arr[-2])/dens_arr[-2]) LE 0.1 )

     IF done THEN BEGIN
        PRINT,"Did it!"
        BREAK
     ENDIF

     IF nNukes GT 5 THEN BEGIN
        IF N_ELEMENTS(WHERE(ABS(dRB_arr) LT 0.00001)) GT 4 THEN BEGIN
           PRINT,"Can't handle it!! Getting out!"
           BREAK
        ENDIF
     ENDIF
     
     IF count GT 2 THEN BEGIN

        PRINT,FORMAT='("dRB_arr  : ",20(F0.2,:,", "))',dRB_arr
        PRINT,FORMAT='("RB_arr   : ",20(F0.2,:,", "))',RB_arr
        PRINT,FORMAT='("RLim_arr : ",20(F0.2,:,", "))',RLim_arr
        PRINT,FORMAT='("RE_arr   : ",20(F0.2,:,", "))',RE_arr
        PRINT,FORMAT='("dens_arr : ",20(G0.4,:,", "))',dens_arr
        WAIT,1
     ENDIF

     oldRLim                = RLim
     oldRB                  = newRB
     oldRE                  = downTail_RE
  ENDWHILE

  history                   = {dRB  : TEMPORARY(dRB_arr ), $
                               RB   : TEMPORARY(RB_arr  ), $
                               RLim : TEMPORARY(RLim_arr), $
                               RE   : TEMPORARY(RE_arr  ), $
                               dens : TEMPORARY(dens_arr)}


  ;; IF N_ELEMENTS(RLim_arr) GT 0 THEN BEGIN
  ;;    STR_ELEMENT,history,'RLim',RLim_arr,/ADD_REPLACE
  ;; ENDIF

  ;; IF N_ELEMENTS(RE_arr) GT 0 THEN BEGIN
  ;;    STR_ELEMENT,history,'RE',RE_arr,/ADD_REPLACE
  ;; ENDIF

  ;; IF N_ELEMENTS(dens_arr) EQ 0 THEN BEGIN
  ;;    dens_arr = 
  ;;    STR_ELEMENT,history,'dens',dens_arr,/ADD_REPLACE
  ;; ENDIF ELSE BEGIN

  gameFitInfo  = {type        : fit_type   , $
                  history     : TEMPORARY(history), $                  
                  bestNorm    : bestNorm   , $
                  nfev        : nfev       , $
                  fTol        : fTol       , $
                  gTol        : gTol       , $
                  status      : status     , $
                  best_resid  : best_resid , $
                  ifree       : ifree      , $
                  best_fjac   : best_fjac  , $
                  parInfoNye  : parInfoNye , $
                  npegged     : npegged    , $
                  nfree       : nfree      , $
                  dof         : dof        , $
                  covar       : covar      , $
                  perror      : perror     , $
                  pcerror     : perror * SQRT(bestnorm / dof), $
                  maxIter     : maxIter    , $
                  itNum       : itNum      , $
                  yFit        : yFit       , $
                  errMsg      : errMsg}

  PRINT,"Et nous avons fini. Voilà le " + fit_type + " fit: "
  PRINT_JV_FIT_PARAMS,A
  PRINT_ITERATIVE_GAME_MODE_FITHISTORY,gameFitInfo

  A[3] = A[3] > 1

  RETURN,A
  
END
