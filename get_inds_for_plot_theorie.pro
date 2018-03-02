;2017/03/21
FUNCTION GET_INDS_FOR_PLOT_THEORIE,JVPlotData, $
                                   USEINDS__INCLUDE_POSCURRENT=useInds__include_posCurrent, $
                                   USEINDS__RELCHANGE=useInds__relChange, $
                                   FRACCHANGE_NDOWN=fracChange_NDown, $
                                   FRACCHANGE_JDOWN=fracChange_JDown, $
                                   FRACCHANGE_TDOWN=fracChange_TDown, $
                                   FRACERROR_NDOWN=fracError_NDown, $
                                   FRACERROR_JDOWN=fracError_JDown, $
                                   FRACERROR_TDOWN=fracError_TDown, $
                                   USE_FRACERROR_NDOWN=use_fracError_NDown, $
                                   USE_FRACERROR_JDOWN=use_fracError_JDown, $
                                   USE_FRACERROR_TDOWN=use_fracError_TDown, $
                                   USEINDS__TWOLUMPS=useInds__twoLumps, $
                                   MAX_TDOWN=max_TDown, $
                                   MIN_TDOWN=min_TDown, $
                                   MAX_NDOWN=max_NDown, $
                                   MIN_NDOWN=min_NDown, $
                                   TRANGES=tRanges, $
                                   BATCH_MODE=batch_mode

  COMPILE_OPT IDL2,STRICTARRSUBS

  IF KEYWORD_SET(use_fracError_jDown) THEN BEGIN

     outSideInds = WHERE(ABS(JVPlotData.curErr) LE fracError_jDown)

  ENDIF

  CASE 1 OF
     KEYWORD_SET(useInds__relChange): BEGIN

        relChange_NDown = (JVPlotData.NDown [1:-1]-JVPlotData.NDown [0:-2])/JVPlotData.NDown [0:-2]
        relChange_JDown = (JVPlotData.cur   [1:-1]-JVPlotData.cur   [0:-2])/JVPlotData.cur   [0:-2]
        relChange_TDown = (JVPlotData.TDown [1:-1]-JVPlotData.TDown [0:-2])/JVPlotData.TDown [0:-2]

        smochange_NDown = WHERE(ABS(relChange_NDown) LE fracChange_NDown)
        smochange_JDown = WHERE(ABS(relChange_JDown) LE fracChange_JDown)
        smochange_TDown = WHERE(ABS(relChange_TDown) LE fracChange_TDown)

        useInds1        = CGSETINTERSECTION(smochange_NDown,smochange_TDown,COUNT=nUsers)
        useInds1        = CGSETINTERSECTION(useInds1,smochange_JDown,COUNT=nUsers)

        ;;Any otras condiciones?
        useInds2        = WHERE((ABS(JVPlotData.TDownErr/JVPlotData.TDown) LE fracError_TDown) AND $
                                (ABS(JVPlotData.NDownErr/JVPlotData.NDown) LE fracError_NDown) AND $
                                (ABS(JVPlotData.curErr  /JVPlotData.cur  ) LE fracError_JDown) )

        useInds         = CGSETINTERSECTION(TEMPORARY(useInds1),TEMPORARY(useInds2),COUNT=nUsers)

     END
     KEYWORD_SET(useInds__twoLumps): BEGIN

        nTRanges = N_ELEMENTS(tRanges[0,*])
        useInds  = !NULL
        FOR k=0,nTRanges-1 DO BEGIN

           tmpInds = WHERE(JVPlotData.time GE STR_TO_TIME(tRanges[0,k]) AND $
                           JVPlotData.time LE STR_TO_TIME(tRanges[1,k]),nTmp)

           IF nTmp GT 0 THEN BEGIN
              useInds = [useInds,tmpInds]
           ENDIF ELSE BEGIN
              PRINT,"Ingenting her!"
              IF ~KEYWORD_SET(batch_mode) THEN STOP
           ENDELSE
           
        ENDFOR

        nUsers = N_ELEMENTS(useInds)

     END
     ELSE: useInds = !NULL
  ENDCASE

  IF N_ELEMENTS(outSideInds) GT 0 THEN BEGIN

     useInds = CGSETINTERSECTION(useInds,outSideInds,COUNT=nUsers)

     IF nUsers LE 1 THEN STOP

  ENDIF

  IF KEYWORD_SET(max_TDown) THEN BEGIN
     useInds = CGSETINTERSECTION(useInds, $
                                 WHERE(JVPlotData.TDown LE max_TDown), $
                                 COUNT=nUsers)
  ENDIF

  IF KEYWORD_SET(min_TDown) THEN BEGIN
     useInds = CGSETINTERSECTION(useInds, $
                                 WHERE(JVPlotData.TDown GE min_TDown), $
                                 COUNT=nUsers)
  ENDIF

  IF KEYWORD_SET(max_NDown) THEN BEGIN
     useInds = CGSETINTERSECTION(useInds, $
                                 WHERE(JVPlotData.NDown LE max_NDown), $
                                 COUNT=nUsers)
  ENDIF

  IF KEYWORD_SET(min_NDown) THEN BEGIN
     useInds = CGSETINTERSECTION(useInds, $
                                 WHERE(JVPlotData.NDown GE min_NDown), $
                                 COUNT=nUsers)
  ENDIF

  IF ~KEYWORD_SET(useInds__include_posCurrent) THEN BEGIN
     useInds = CGSETINTERSECTION(useInds, $
                                 WHERE(JVPlotData.cur LE 0.), $
                                 COUNT=nUsers)
  ENDIF


  IF nUsers LE 1 THEN IF ~KEYWORD_SET(batch_mode) THEN STOP

  RETURN,useInds

END
