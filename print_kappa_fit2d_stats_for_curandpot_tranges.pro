;2018/02/22
PRO PRINT_KAPPA_FIT2D_STATS_FOR_CURANDPOT_TRANGES,fit2DK,fit2DG,cAP_struct,jvPlotData, $
   FIT2DINDS=fit2DInds, $
   JVPLOTINDS=jvPlotInds, $
   SOK_IF_INDS_DONT_MATCH=sok;, $
   ;; ALSO_PARAM_STRUCTS=also_param_structs, $
   ;; KFIT2DPARAM_STRUCT=kFit2DParam_struct, $
   ;; GFIT2DPARAM_STRUCT=gFit2DParam_struct

  COMPILE_OPT IDL2,STRICTARRSUBS

     ;; Maybe some stats
  IF N_ELEMENTS(fit2DInds) EQ 0 OR N_ELEMENTS(jvPlotInds) EQ 0 THEN BEGIN

     n_tRanges = N_ELEMENTS(cAP_struct.tRanges[0,*])
     fit2DInds = !NULL
     jvPlotInds = !NULL
     FOR bro=0,n_tRanges-1 DO BEGIN
        PRINT,FORMAT='("tRange ",I02,": ",2(A0,:,"-"))',bro+1,cAP_struct.tRanges[*,bro]
        tmp_inds1 = WHERE((fit2DK.sdt.time GE S2T(cAP_struct.tRanges[0,bro])) AND $
                          (fit2DK.sdt.time LE S2T(cAP_struct.tRanges[1,bro])),/NULL)
        fit2DInds = [fit2DInds,tmp_inds1]
        tmp_inds2 = WHERE((jvPlotData.time GE S2T(cAP_struct.tRanges[0,bro])) AND $
                          (jvPlotData.time LE S2T(cAP_struct.tRanges[1,bro])),/NULL)
        jvPlotInds = [jvPlotInds,tmp_inds2]
     ENDFOR

  ENDIF

  nfit2DInds = N_ELEMENTS(fit2DInds)
  njvPlotInds = N_ELEMENTS(jvPlotInds)
  
  IF (nfit2DInds NE njvPlotInds) AND ~KEYWORD_SET(sok) THEN STOP

  PRINT,FORMAT='(A0,T5,A0,T30,A0,T40,A0,T50,A0,T60,A0,T70,A0,T80,A0,T90,A0,T100,A0,T110,A0)', $
        ;; 'i','Time','PotBove','T','kappa','n','TFracErr','NFracErr','JFracErr','potBar'
        'i','Time','PotTot','PotBove','T','kappa','n','potBov','T','kappa','n'


;; inds = [0:102] & PRINT,FORMAT='("pots={",103(F-8.3,:,","),"}")',jvPlotData.pot[inds] & PRINT,FORMAT='("cur={",103(F-8.3,:,","),"}")',jvPlotData.cur[inds]*(-1.D) & PRINT,FORMAT='("curErrs={",103(F-8.3,:,","),"}")',jvPlotData.curErr[inds]

  FOR k=0,nfit2DInds-1 DO BEGIN
     tmpInd = fit2DInds[k]
     tmpKVal = fit2DK.fitparams[*,tmpInd]
     tmpGVal = fit2DG.fitparams[*,tmpInd]
     PRINT,FORMAT='(I0,T5,A0,T30,F-8.1,T40,F-8.1,T50,F-8.1,T60,F-8.3,T70,F-8.4,T80,F-8.1,T90,F-8.1,T100,I-8,T110,F-8.4)', $
           k, $
           TIME_TO_STR(fit2DK.sdt[tmpInd].time,/MS), $
           jvPlotData.pot[jvPlotInds[k]], $
           tmpKVal[0], $
           tmpKVal[1], $
           tmpKVal[2], $
           tmpKVal[3], $
           tmpGVal[0], $
           tmpGVal[1], $
           tmpGVal[2], $
           tmpGVal[3]
     
  ENDFOR

  PRINT,FORMAT='(A0,T5,A0,T30,A0,T40,A0,T50,A0,T60,A0,T70,A0,T80,A0,T90,A0,T100,A0,T110,A0)', $
        ;; 'i','Time','PotBove','T','kappa','n','TFracErr','NFracErr','JFracErr','potBar'
        'i','Time','PotTot','PotBove','T','kappa','n','potBov','T','kappa','n'

  ;; PRINT,FORMAT='(A0,T30,F-8.1,T40,F-8.1,T50,F-8.3,T60,F-8.4,T70,F-8.1,T80,F-8.1,T90,I-8,T100,F-8.4)', $
  PRINT,FORMAT='(A0,T30,F-8.1,T40,F-8.1,T50,F-8.1,T60,F-8.3,T70,F-8.4,T80,F-8.1,T90,F-8.1,T100,I-8,T110,F-8.4)', $
        "AVG", $
        MEAN(jvPlotData.pot[jvPlotInds]), $
        MEAN(fit2DK.fitparams[0,fit2DInds]), $
        MEAN(fit2DK.fitparams[1,fit2DInds]), $
        MEAN(fit2DK.fitparams[2,fit2DInds]), $
        MEAN(fit2DK.fitparams[3,fit2DInds]), $
        MEAN(fit2DG.fitparams[0,fit2DInds]), $
        MEAN(fit2DG.fitparams[1,fit2DInds]), $
        MEAN(fit2DG.fitparams[2,fit2DInds]), $
        MEAN(fit2DG.fitparams[3,fit2DInds])
  ;; PRINT,FORMAT='(A0,T30,F-8.1,T40,F-8.1,T50,F-8.3,T60,F-8.4,T70,F-8.1,T80,F-8.1,T90,I-8,T100,F-8.4)', $
  PRINT,FORMAT='(A0,T30,F-8.1,T40,F-8.1,T50,F-8.1,T60,F-8.3,T70,F-8.4,T80,F-8.1,T90,F-8.1,T100,I-8,T110,F-8.4)', $
        "MEDIAN", $
        MEDIAN(jvPlotData.pot[jvPlotInds]), $
        MEDIAN(fit2DK.fitparams[0,fit2DInds]), $
        MEDIAN(fit2DK.fitparams[1,fit2DInds]), $
        MEDIAN(fit2DK.fitparams[2,fit2DInds]), $
        MEDIAN(fit2DK.fitparams[3,fit2DInds]), $
        MEDIAN(fit2DG.fitparams[0,fit2DInds]), $
        MEDIAN(fit2DG.fitparams[1,fit2DInds]), $
        MEDIAN(fit2DG.fitparams[2,fit2DInds]), $
        MEDIAN(fit2DG.fitparams[3,fit2DInds])
  ;; PRINT,FORMAT='(A0,T30,F-8.1,T40,F-8.1,T50,F-8.3,T60,F-8.4,T70,F-8.1,T80,F-8.1,T90,I-8,T100,F-8.4)', $
  PRINT,FORMAT='(A0,T30,F-8.1,T40,F-8.1,T50,F-8.1,T60,F-8.3,T70,F-8.4,T80,F-8.1,T90,F-8.1,T100,I-8,T110,F-8.4)', $
        "STDDEV", $
        STDDEV(jvPlotData.pot[jvPlotInds]), $
        STDDEV(fit2DK.fitparams[0,fit2DInds]), $
        STDDEV(fit2DK.fitparams[1,fit2DInds]), $
        STDDEV(fit2DK.fitparams[2,fit2DInds]), $
        STDDEV(fit2DK.fitparams[3,fit2DInds]), $
        STDDEV(fit2DG.fitparams[0,fit2DInds]), $
        STDDEV(fit2DG.fitparams[1,fit2DInds]), $
        STDDEV(fit2DG.fitparams[2,fit2DInds]), $
        STDDEV(fit2DG.fitparams[3,fit2DInds])
  PRINT,FORMAT='(A0,T30,F-8.1,T40,F-8.1,T50,F-8.1,T60,F-8.3,T70,F-8.4,T80,F-8.1,T90,F-8.1,T100,I-8,T110,F-8.4)', $
        "MIN", $
        MIN(jvPlotData.pot[jvPlotInds]), $
        MIN(fit2DK.fitparams[0,fit2DInds]), $
        MIN(fit2DK.fitparams[1,fit2DInds]), $
        MIN(fit2DK.fitparams[2,fit2DInds]), $
        MIN(fit2DK.fitparams[3,fit2DInds]), $
        MIN(fit2DG.fitparams[0,fit2DInds]), $
        MIN(fit2DG.fitparams[1,fit2DInds]), $
        MIN(fit2DG.fitparams[2,fit2DInds]), $
        MIN(fit2DG.fitparams[3,fit2DInds])
  ;; PRINT,FORMAT='(A0,T30,F-8.1,T40,F-8.1,T50,F-8.3,T60,F-8.4,T70,F-8.1,T80,F-8.1,T90,I-8,T100,F-8.4)', $
  PRINT,FORMAT='(A0,T30,F-8.1,T40,F-8.1,T50,F-8.1,T60,F-8.3,T70,F-8.4,T80,F-8.1,T90,F-8.1,T100,I-8,T110,F-8.4)', $
        "MAX", $
        MAX(jvPlotData.pot[jvPlotInds]), $
        MAX(fit2DK.fitparams[0,fit2DInds]), $
        MAX(fit2DK.fitparams[1,fit2DInds]), $
        MAX(fit2DK.fitparams[2,fit2DInds]), $
        MAX(fit2DK.fitparams[3,fit2DInds]), $
        MAX(fit2DG.fitparams[0,fit2DInds]), $
        MAX(fit2DG.fitparams[1,fit2DInds]), $
        MAX(fit2DG.fitparams[2,fit2DInds]), $
        MAX(fit2DG.fitparams[3,fit2DInds])


  IF KEYWORD_SET(also_param_structs) THEN BEGIN

     PRINT,""
     PRINT,""

  PRINT,FORMAT='(A0,T30,A0,T40,A0,T50,A0,T60,A0,T70,A0,T80,A0,T90,A0,T100,A0)', $
        ;; 'i','Time','PotBove','T','kappa','n','TFracErr','NFracErr','JFracErr','potBar'
        'i','PotBove','T','kappa','n','potBov','T','kappa','n'


  FOR k=0,nCap_inds-1 DO BEGIN
     tmpInd = cap_inds[k]
     tmpKVal = fit2DK.fitparams[*,tmpInd]
     tmpGVal = fit2DG.fitparams[*,tmpInd]
     PRINT,FORMAT='(I0,T30,F-8.1,T40,F-8.1,T50,F-8.3,T60,F-8.4,T70,F-8.1,T80,F-8.1,T90,I-8,T100,F-8.4)', $
           k, $
           kFit2DParam_struct.bulk_energy[tmpInd], $
           kFit2DParam_struct.temperature[tmpInd], $
           kFit2DParam_struct.kappa[tmpInd], $
           kFit2DParam_struct.N[tmpInd], $
           gFit2DParam_struct.bulk_energy[tmpInd], $
           gFit2DParam_struct.temperature[tmpInd], $
           gFit2DParam_struct.kappa[tmpInd], $
           gFit2DParam_struct.N[tmpInd]
     
  ENDFOR

  PRINT,FORMAT='(A0,T30,F-8.1,T40,F-8.1,T50,F-8.3,T60,F-8.4,T70,F-8.1,T80,F-8.1,T90,I-8,T100,F-8.4)', $
        "AVG", $
        MEAN(kFit2DParam_struct.bulk_energy[cap_inds]), $
        MEAN(kFit2DParam_struct.temperature[cap_inds]), $
        MEAN(kFit2DParam_struct.kappa[cap_inds]), $
        MEAN(kFit2DParam_struct.N[cap_inds]), $
        MEAN(gFit2DParam_struct.bulk_energy[cap_inds]), $
        MEAN(gFit2DParam_struct.temperature[cap_inds]), $
        MEAN(gFit2DParam_struct.kappa[cap_inds]), $
        MEAN(gFit2DParam_struct.N[cap_inds])
  PRINT,FORMAT='(A0,T30,F-8.1,T40,F-8.1,T50,F-8.3,T60,F-8.4,T70,F-8.1,T80,F-8.1,T90,I-8,T100,F-8.4)', $
        "MEDIAN", $
        MEDIAN(kFit2DParam_struct.bulk_energy[cap_inds]), $
        MEDIAN(kFit2DParam_struct.temperature[cap_inds]), $
        MEDIAN(kFit2DParam_struct.kappa[cap_inds]), $
        MEDIAN(kFit2DParam_struct.N[cap_inds]), $
        MEDIAN(gFit2DParam_struct.bulk_energy[cap_inds]), $
        MEDIAN(gFit2DParam_struct.temperature[cap_inds]), $
        MEDIAN(gFit2DParam_struct.kappa[cap_inds]), $
        MEDIAN(gFit2DParam_struct.N[cap_inds])
  PRINT,FORMAT='(A0,T30,F-8.1,T40,F-8.1,T50,F-8.3,T60,F-8.4,T70,F-8.1,T80,F-8.1,T90,I-8,T100,F-8.4)', $
        "STDDEV", $
        STDDEV(kFit2DParam_struct.bulk_energy[cap_inds]), $
        STDDEV(kFit2DParam_struct.temperature[cap_inds]), $
        STDDEV(kFit2DParam_struct.kappa[cap_inds]), $
        STDDEV(kFit2DParam_struct.N[cap_inds]), $
        STDDEV(gFit2DParam_struct.bulk_energy[cap_inds]), $
        STDDEV(gFit2DParam_struct.temperature[cap_inds]), $
        STDDEV(gFit2DParam_struct.kappa[cap_inds]), $
        STDDEV(gFit2DParam_struct.N[cap_inds])

  ENDIF

END
