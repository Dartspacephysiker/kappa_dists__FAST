;2018/03/01
PRO PRINT_MMA_FITSTATS,tRanges,time,stat,kStat,gStat,useInds, $
                           fmtStrs

        nTRanges = N_ELEMENTS(tRanges[0,*])

        PRINT,""

        PRINT,FORMAT=fmtStrs[0],fmtStrs[1],fmtStrs[2],fmtStrs[3],fmtStrs[4],fmtStrs[5]

        FOR j=0,nTRanges-1 DO BEGIN

           tmpInds = WHERE(time[useInds] GE STR_TO_TIME(tRanges[0,j]) AND $
                           time[useInds] LE STR_TO_TIME(tRanges[1,j]),nTmp)

           IF nTmp GT 0 THEN BEGIN

              FOR k=0,N_ELEMENTS(tmpInds)-1 DO $
                 PRINT,FORMAT='(I3,TR1,A22,TR5,F6.2,TR5,F6.2,TR5,F6.2)', $
                       k, $
                       T2S(time[useInds[tmpInds[k]]],/MS), $
                       stat [useInds[tmpInds[k]]], $
                       kStat[useInds[tmpInds[k]]], $
                       gStat[useInds[tmpInds[k]]]

              PRINT,FORMAT='(A6,TR25,F6.2,TR5,F6.2,TR5,F6.2)', $
                    'MEAN', $
                    MEAN(stat [useInds[tmpInds]],/NAN), $
                    MEAN(kStat[useInds[tmpInds]],/NAN), $
                    MEAN(gStat[useInds[tmpInds]],/NAN)
              PRINT,FORMAT='(A6,TR25,F6.2,TR5,F6.2,TR5,F6.2)', $
                    'MEDIAN', $
                    MEDIAN(stat [useInds[tmpInds]]), $
                    MEDIAN(kStat[useInds[tmpInds]]), $
                    MEDIAN(gStat[useInds[tmpInds]])
              PRINT,FORMAT='(A6,TR25,F6.2,TR5,F6.2,TR5,F6.2)', $
                    'MIN', $
                    MIN(stat [useInds[tmpInds]]), $
                    MIN(kStat[useInds[tmpInds]]), $
                    MIN(gStat[useInds[tmpInds]])
              PRINT,FORMAT='(A6,TR25,F6.2,TR5,F6.2,TR5,F6.2)', $
                    'MAX', $
                    MAX(stat [useInds[tmpInds]],/NAN), $
                    MAX(kStat[useInds[tmpInds]],/NAN), $
                    MAX(gStat[useInds[tmpInds]],/NAN)
              PRINT,FORMAT='(A6,TR25,F6.2,TR5,F6.2,TR5,F6.2)', $
                    'STDDEV', $
                    STDDEV(stat [useInds[tmpInds]],/NAN), $
                    STDDEV(kStat[useInds[tmpInds]],/NAN), $
                    STDDEV(gStat[useInds[tmpInds]],/NAN)

              PRINT,''
           ENDIF

        ENDFOR


END
PRO PRINT_J_V_INFO_FOR_MATHEMATICA, $
   JVPLOTDATA=jvPlotData, $
   AVGS_JVFIT=avgs_jvFit, $
   KFIT2DPARAM_STRUCT=kFit2DParam_struct, $
   GFIT2DPARAM_STRUCT=gFit2DParam_struct, $
   FIT2DKAPPA_INF_LIST=fit2DKappa_inf_list, $
   FIT2DGAUSS_INF_LIST=fit2DGauss_inf_list, $
   ;; KAPPA2D=fit2DK, $
   ;; GAUSS2D=fit2DG, $
   CAP_STRUCT=cAP_struct, $
   ADD_PARM_ERRORS_FROM_FILE=add_parm_errors_from_file, $
   ADD_PARM_ERRORS__NROLLS=add_parm_errors__nRolls, $
   ADD_PARM_ERRORS__USE_MOST_PROB=add_parm_errors__use_most_prob, $
   FIT2DPARMERRFILE=fit2DParmErrFile, $
   FIT2DPARMERRDIR=fit2DParmErrDir

  COMPILE_OPT IDL2,STRICTARRSUBS

  nHer = N_ELEMENTS(jvPlotData.time)
  inds = LINDGEN(nHer)
  nHerStr = STRING(FORMAT='(I0)',nHer)

  k2DParms = kFit2DParam_struct
  g2DParms = gFit2DParam_struct
  
  nHereK    = N_ELEMENTS(fit2DKappa_inf_list)
  nHereG    = N_ELEMENTS(fit2DGauss_inf_list)
  
  kappa2DTime = MAKE_ARRAY(nHereK,/DOUBLE,VALUE=0.0D)
  gauss2DTime = MAKE_ARRAY(nHereG,/DOUBLE,VALUE=0.0D)

  FOR k=0,nHereK-1 DO $
     kappa2DTime[k] = fit2DKappa_inf_list[k].sdt.time 

  FOR k=0,nHereG-1 DO $
     gauss2DTime[k] = fit2DGauss_inf_list[k].sdt.time

  IF KEYWORD_SET(add_parm_errors_from_file) THEN BEGIN

     ParmUncertainty_2D = 1
     ParmUncert_2D__useMostProbK = KEYWORD_SET(add_parm_errors__use_most_prob)
     ParmUncert_2D__useMostProbG = KEYWORD_SET(add_parm_errors__use_most_prob)

     ;; fit2DParmErrFile = fit2DParmErrFile.Replace('20180118',GET_TODAY_STRING)
     fit2DParmErrFileIn = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + $
                          STRMID(fit2DParmErrFile, $
                                 8, $
                                 STRLEN(fit2DParmErrFile)-8)
     IF KEYWORD_SET(ParmUncertainty_2D) THEN BEGIN
        fit2DParmErrFileIn = fit2DParmErrFileIn.Replace('-2DPARMERRORS','-2DPARMERRORS_TWOSIDED')
     ENDIF
     nRolls = KEYWORD_SET(add_parm_errors__nRolls) ? $
              add_parm_errors__nRolls              : $
              1000
     fit2DParmErrFileIn = fit2DParmErrFileIn.Replace('.sav', $
                                                     STRING(FORMAT='("-",I0,"Rolls.sav")',nRolls))

     searchbackward1month = 1

     remaining = KEYWORD_SET(searchbackward1month) ? 30 : 0

     foundParmErrFile = 0
     tmpParmErrFile   = fit2DParmErrFileIn
     WHILE ~foundParmErrFile AND remaining GT -1 DO BEGIN

        IF FILE_TEST(fit2DParmErrDir+tmpParmErrFile) THEN BEGIN

           foundParmErrFile = 1
           fit2DParmErrFileIn      = TEMPORARY(tmpParmErrFile)
           
           PRINT,'Restoring ' + fit2DParmErrFileIn + ' ...'
           RESTORE,fit2DParmErrDir+fit2DParmErrFileIn
           restored_fit2DParmErrFileIn = 1B
           ;; just_diff_eFlux  = 1B

           ;;And diff eFlux
           ;; RESTORE,fit2DParmErrDir+'/diff_eFlux/'+diff_eFlux_file

        ENDIF ELSE BEGIN
           ;; PRINT,"Couldn't get file!"
           ;; STOP
           date = (STRSPLIT(tmpParmErrFile,'-',/EXTRACT))
           rest = STRJOIN(date[1:-1],'-')
           date = date[0]
           dd   = FIX(STRMID(date,STRLEN(date)-2,2))
           mm   = FIX(STRMID(date,STRLEN(date)-4,2))
           jahr = FIX(STRMID(date,STRLEN(date)-8,4))

           CASE 1 OF
              ((dd EQ 1) AND (mm EQ 1)): BEGIN
                 jahr--
                 dd = 31
                 mm = 12
              END
              (dd EQ 1): BEGIN
                 dd = 31
                 mm--
              END
              ELSE: BEGIN
                 dd--
              END
           ENDCASE
           
           tmpParmErrFile = STRJOIN([STRING(FORMAT='(I04,I02,I02)',jahr,mm,dd),rest],'-')

           remaining--

        ENDELSE
        
     ENDWHILE

     IF ~foundParmErrFile THEN BEGIN
        PRINT,"Couldn't get file!"
        IF KEYWORD_SET(batch_mode) THEN RETURN ELSE STOP
     ENDIF

     RESTORE,fit2DParmErrDir+fit2DParmErrFileIn

     matchieKinit = VALUE_CLOSEST2(k2DParmErr.time,kappa2DTime,/CONSTRAINED)
     matchieGinit = VALUE_CLOSEST2(g2DParmErr.time,Gauss2DTime,/CONSTRAINED)

     matchieK     = WHERE(ABS(k2DParmErr.time[matchieKinit]-kappa2DTime) LT 0.05,nMatchieK)
     matchieG     = WHERE(ABS(g2DParmErr.time[matchieKinit]-gauss2DTime) LT 0.05,nMatchieG)

     matchieKinit2 = VALUE_CLOSEST2(kappa2DTime,k2DParmErr.time,/CONSTRAINED)
     matchieGinit2 = VALUE_CLOSEST2(Gauss2DTime,g2DParmErr.time,/CONSTRAINED)

     matchieK2     = WHERE(ABS(kappa2DTime[matchieKinit2]-k2DParmErr.time) LT 0.05,nMatchieK)
     matchieG2     = WHERE(ABS(gauss2DTime[matchieKinit2]-g2DParmErr.time) LT 0.05,nMatchieG)

     IF nMatchieK EQ 0 OR nMatchieG EQ 0 THEN STOP
     
     IF KEYWORD_SET(ParmUncert_2D__useMostProbK) THEN BEGIN

        k2DPArms.kappa      [matchieK]  = k2DParmErr.mostProb.kappa[matchieK2]
        k2DParms.bulk_energy[matchieK]  = k2DParmErr.mostProb.bulk_energy[matchieK2]
        k2DParms.temperature[matchieK]  = k2DParmErr.mostProb.temperature[matchieK2]
        k2DParms.N          [matchieK]  = k2DParmErr.mostProb.N[matchieK2]

     ENDIF

     IF KEYWORD_SET(ParmUncert_2D__useMostProbG) THEN BEGIN

        g2DParms.bulk_energy[matchieG]  = g2DParmErr.mostProb.bulk_energy[matchieG2]
        g2DParms.temperature[matchieG]  = g2DParmErr.mostProb.temperature[matchieG2]
        g2DParms.N          [matchieG]  = g2DParmErr.mostProb.N[matchieG2]

     ENDIF

  ENDIF

  CSVstyle = 1
  IF KEYWORD_SET(CSVstyle) THEN BEGIN

     PRINT,"Tid,pot,potErr,cur,curErr,je,jeerr,nDown,nDownErr,downEpot,downEpotErr"
     FOR k=0,N_ELEMENTS(inds)-1 DO BEGIN
        i=inds[k]
        PRINT,FORMAT='(A0,",",F0.2,",",F0.2,","' $
              + ',F0.3,",",F0.3,",",F0.3,",",F0.3,","' $
              + ',F0.3,",",F0.3,",",F0.3,",",F0.3)', $
              '"'+STRMID(T2S(jvPlotData.time[i],/MS),11,23)+'"', $
              jvPlotData.pot[i], $
              jvPlotData.potErr[i], $
              jvPlotData.cur[i], $
              jvPlotData.curErr[i], $
              jvPlotData.je[i], $
              jvPlotData.jeerr[i], $
              jvPlotData.source.nDown[i], $
              jvPlotData.source.nDownErr[i], $
              jvPlotData.only_downe_pot[i], $
              jvPlotData.only_downe_poterr[i]
     ENDFOR

  ENDIF ELSE BEGIN

     ;; PRINT,FORMAT='("time={",' + nHerStr + '(A25,:,", "),"}")','"'+T2S(jvPlotData.time[inds],/MS)+'"'
     PRINT,STRING(FORMAT='("time={",' + nHerStr + '(A0,:,", "),"}")','"'+STRMID(T2S(jvPlotData.time[inds],/MS),11,23)+'"')+'};'
     PRINT,STRING(FORMAT='("pots={",' + nHerStr + '(F-0.2,:,", "),"}")',    jvPlotData.pot[inds])+'};'
     PRINT,STRING(FORMAT='("potErrs={",' + nHerStr + '(F-0.2,:,", "),"}")', jvPlotData.potErr[inds])+'};'
     PRINT,STRING(FORMAT='("curs={",' + nHerStr + '(F-0.3,:,", "),"}")',    jvPlotData.cur[inds]*(-1.D))+'};'
     PRINT,STRING(FORMAT='("curErrs={",' + nHerStr + '(F-0.3,:,", "),"}")', jvPlotData.curErr[inds])+'};'
     PRINT,STRING(FORMAT='("jes={",' + nHerStr + '(F-0.3,:,", "),"}")',     jvPlotData.je[inds])+'};'
     PRINT,STRING(FORMAT='("jeErrs={",' + nHerStr + '(F-0.3,:,", "),"}")',  jvPlotData.jeerr[inds])+'};'
     PRINT,STRING(FORMAT='("dens={",' + nHerStr + '(F-0.3,:,", "),"}")',    jvPlotData.source.nDown[inds])+'};'
     PRINT,STRING(FORMAT='("densErr={",' + nHerStr + '(F-0.3,:,", "),"}")', jvPlotData.source.nDownErr[inds])+'};'
     PRINT,STRING(FORMAT='("downepots={",' + nHerStr + '(F-0.3,:,", "),"}")',jvPlotData.only_downe_pot[inds])+'};'
     IF N_ELEMENTS(k2DParms.bulk_energy) EQ nHer THEN BEGIN
        PRINT,STRING(FORMAT='("gaussbulkEs={",' + nHerStr + '(F-0.2,:,", "),"}")',g2DParms.bulk_energy[inds])+'};'
        PRINT,STRING(FORMAT='("kappabulkEs={",' + nHerStr + '(F-0.2,:,", "),"}")',k2DParms.bulk_energy[inds])+'};'
     ENDIF

  ENDELSE

  IF N_ELEMENTS(fit2DKappa_inf_list) GT 0 AND N_ELEMENTS(fit2DGauss_inf_list) GT 0 THEN BEGIN

     ;; kStart = 42
     ;; FOR k=0,kStart DO PRINT,k,",",T2S(jvplotdata.time[avgs_jvfit.useinds[k]])
     ;; inds=[0:29]
     STR_ELEMENT_FROM_LIST_OF_STRUCTS,fit2DKappa_inf_list,'obsmoms',VALUE=kObsMoms
     STR_ELEMENT_FROM_LIST_OF_STRUCTS,fit2DGauss_inf_list,'obsmoms',VALUE=gObsMoms
     PRINT,STRING(FORMAT='("kFAConduct={",' $
                  + STRING(N_ELEMENTS(kObsMoms)) + '(G-0.4,:,", "),"}")',kObsMoms.scfaconduct) $
           + '};'
     PRINT,STRING(FORMAT='("gFAConduct={",' $
                  + STRING(N_ELEMENTS(gObsMoms)) + '(G-0.4,:,", "),"}")',gObsMoms.scfaconduct) $
           + '};'
  ENDIF

  IF N_ELEMENTS(cAP_struct) GT 0 THEN BEGIN

     tRanges = !NULL
     STR_ELEMENT,cAP_struct,'tRanges',tRanges
     IF N_ELEMENTS(tRanges) GT 0 THEN IF SIZE(tRanges,/TYPE) EQ 7 THEN BEGIN

        ;; Match 'em up
        kMatchieInd = VALUE_CLOSEST2(jvPlotData.time,kappa2DTime,/CONSTRAINED)
        gMatchieInd = VALUE_CLOSEST2(jvPlotData.time,gauss2DTime,/CONSTRAINED)

        kKeep       = WHERE((ABS(jvPlotData.time[kMatchieInd]-kappa2DTime) $
                             / MEAN(jvPlotData.time[1:-1]-jvPlotData.time[0:-2])) $
                            LT 0.5,nKKeep)
        gKeep       = WHERE((ABS(jvPlotData.time[gMatchieInd]-gauss2DTime) $
                             / MEAN(jvPlotData.time[1:-1]-jvPlotData.time[0:-2])) $
                            LT 0.5,nGKeep)

        nTime       = N_ELEMENTS(jvPlotData.time)
        kTemp       = MAKE_ARRAY(nTime,/FLOAT,VALUE=!VALUES.F_NaN)
        gTemp       = MAKE_ARRAY(nTime,/FLOAT,VALUE=!VALUES.F_NaN)
        kDens       = MAKE_ARRAY(nTime,/FLOAT,VALUE=!VALUES.F_NaN)
        gDens       = MAKE_ARRAY(nTime,/FLOAT,VALUE=!VALUES.F_NaN)
        kKappa      = MAKE_ARRAY(nTime,/FLOAT,VALUE=!VALUES.F_NaN)
        
        kTemp[kMatchieInd[kKeep]] = k2DParms.temperature
        gTemp[gMatchieInd[gKeep]] = g2DParms.temperature
        kDens[kMatchieInd[kKeep]] = k2DParms.N
        gDens[gMatchieInd[gKeep]] = g2DParms.N
        kKappa[kMatchieInd[kKeep]] = k2DParms.kappa

        ;; Now temperatures
        fmtStrs = ['(A3,TR1,A22,TR5,A6,TR5,A6,TR5,A6)', $
              'k', $
              'Time', $
              'Tcalc', $
              'TKappa', $
              'TGauss']

        PRINT_MMA_FITSTATS,tRanges, $
                           jvPlotData.time, $
                           jvPlotData.tDown, $
                           kTemp, $
                           gTemp, $
                           avgs_jvFit.useInds, $
                           fmtStrs

        ;; Now density
        fmtStrs=['(A3,TR1,A22,TR5,A6,TR5,A6,TR5,A6)', $
              'k', $
              'Time', $
              'Ncalc', $
              'NKappa', $
              'NGauss']

        PRINT_MMA_FITSTATS,tRanges, $
                           jvPlotData.time, $
                           jvPlotData.source.NDown, $
                           kDens, $
                           gDens, $
                           avgs_jvFit.useInds, $
                           fmtStrs

        ;; Now kappa
        fmtStrs=['(A3,TR1,A22,TR5,A6,TR5,A6,TR5,A6)', $
              'k', $
              'Time', $
              '', $
              'Kappa', $
              '']

        PRINT_MMA_FITSTATS,tRanges, $
                           jvPlotData.time, $
                           REPLICATE(0.,N_ELEMENTS(kKappa)), $
                           kKappa, $
                           REPLICATE(100.,N_ELEMENTS(kKappa)), $
                           avgs_jvFit.useInds, $
                           fmtStrs

     ENDIF

  ENDIF

END
