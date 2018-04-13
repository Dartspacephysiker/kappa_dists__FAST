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
                       stat[useInds[tmpInds[k]]], $
                       kStat[useInds[tmpInds[k]]], $
                       gStat[useInds[tmpInds[k]]]

              PRINT,FORMAT='(A6,TR25,F6.2,TR5,F6.2,TR5,F6.2)', $
                    'MEAN', $
                    MEAN(stat[useInds[tmpInds]]), $
                    MEAN(kStat[useInds[tmpInds]]), $
                    MEAN(gStat[useInds[tmpInds]])
              PRINT,FORMAT='(A6,TR25,F6.2,TR5,F6.2,TR5,F6.2)', $
                    'MEDIAN', $
                    MEDIAN(stat[useInds[tmpInds]]), $
                    MEDIAN(kStat[useInds[tmpInds]]), $
                    MEDIAN(gStat[useInds[tmpInds]])
              PRINT,FORMAT='(A6,TR25,F6.2,TR5,F6.2,TR5,F6.2)', $
                    'MIN', $
                    MIN(stat[useInds[tmpInds]]), $
                    MIN(kStat[useInds[tmpInds]]), $
                    MIN(gStat[useInds[tmpInds]])
              PRINT,FORMAT='(A6,TR25,F6.2,TR5,F6.2,TR5,F6.2)', $
                    'MAX', $
                    MAX(stat[useInds[tmpInds]]), $
                    MAX(kStat[useInds[tmpInds]]), $
                    MAX(gStat[useInds[tmpInds]])
              PRINT,FORMAT='(A6,TR25,F6.2,TR5,F6.2,TR5,F6.2)', $
                    'STDDEV', $
                    STDDEV(stat[useInds[tmpInds]]), $
                    STDDEV(kStat[useInds[tmpInds]]), $
                    STDDEV(gStat[useInds[tmpInds]])

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
  
  IF KEYWORD_SET(add_parm_errors_from_file) THEN BEGIN

     nHereK    = N_ELEMENTS(fit2DKappa_inf_list)
     nHereG    = N_ELEMENTS(fit2DGauss_inf_list)
     
     kappa2DTime = MAKE_ARRAY(nHereK,/DOUBLE,VALUE=0.0D)
     gauss2DTime = MAKE_ARRAY(nHereG,/DOUBLE,VALUE=0.0D)

     FOR k=0,nHereK-1 DO $
        kappa2DTime[k] = fit2DKappa_inf_list[k].sdt.time 

     FOR k=0,nHereG-1 DO $
        gauss2DTime[k] = fit2DGauss_inf_list[k].sdt.time

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

  ;; PRINT,FORMAT='("time={",' + nHerStr + '(A25,:,", "),"}")','"'+T2S(jvPlotData.time[inds],/MS)+'"'
  PRINT,STRING(FORMAT='("time={",' + nHerStr + '(A0,:,", "),"}")','"'+STRMID(T2S(jvPlotData.time[inds],/MS),11,23)+'"')+'};'
  PRINT,STRING(FORMAT='("pots={",' + nHerStr + '(F-0.2,:,", "),"}")',jvPlotData.pot[inds])+'};'
  PRINT,STRING(FORMAT='("potErrs={",' + nHerStr + '(F-0.2,:,", "),"}")',jvPlotData.potErr[inds])+'};'
  PRINT,STRING(FORMAT='("curs={",' + nHerStr + '(F-0.3,:,", "),"}")',jvPlotData.cur[inds]*(-1.D))+'};'
  PRINT,STRING(FORMAT='("curErrs={",' + nHerStr + '(F-0.3,:,", "),"}")',jvPlotData.curErr[inds])+'};'
  PRINT,STRING(FORMAT='("jes={",' + nHerStr + '(F-0.3,:,", "),"}")',jvPlotData.je[inds])+'};'
  PRINT,STRING(FORMAT='("jeErrs={",' + nHerStr + '(F-0.3,:,", "),"}")',jvPlotData.jeerr[inds])+'};'
  PRINT,STRING(FORMAT='("dens={",' + nHerStr + '(F-0.3,:,", "),"}")',jvPlotData.source.nDown[inds])+'};'
  PRINT,STRING(FORMAT='("densErr={",' + nHerStr + '(F-0.3,:,", "),"}")',jvPlotData.source.nDownErr[inds])+'};'
  PRINT,STRING(FORMAT='("downepots={",' + nHerStr + '(F-0.3,:,", "),"}")',jvPlotData.only_downe_pot[inds])+'};'
  IF N_ELEMENTS(k2DParms.bulk_energy) EQ nHer THEN BEGIN
     PRINT,STRING(FORMAT='("gaussbulkEs={",' + nHerStr + '(F-0.2,:,", "),"}")',g2DParms.bulk_energy[inds])+'};'
     PRINT,STRING(FORMAT='("kappabulkEs={",' + nHerStr + '(F-0.2,:,", "),"}")',k2DParms.bulk_energy[inds])+'};'
  ENDIF

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
                           k2DParms.temperature, $
                           g2DParms.temperature, $
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
                           k2DParms.N, $
                           g2DParms.N, $
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
                           REPLICATE(0.,N_ELEMENTS(k2DParms.kappa)), $
                           k2DParms.kappa, $
                           REPLICATE(100.,N_ELEMENTS(k2DParms.kappa)), $
                           avgs_jvFit.useInds, $
                           fmtStrs

     ENDIF

  ENDIF

END
