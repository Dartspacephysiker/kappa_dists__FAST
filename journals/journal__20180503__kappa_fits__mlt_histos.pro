;2018/05/03
;; 2018/05/14 COMMAND FOR DIST OVERPLOT
;; FOR k=5,5 DO JOURNAL__20180503__KAPPA_FITS__MLT_HISTOS,MINALT=300,MAXALT=4300,/NORMED,GOVERK='decile='+STRING(FORMAT='(I1)',k),MAXKCHI2=5,DSTCUTOFF=-25,/HISTOTITLE__USE_GOVERK_DECILE_STRING,SHOW_EARLYLATE_BEAMS_TOGETHER=0,/MAKEKAPPAHISTOPLOT,EARLYBEAM__OVERPLOT_INVERSEGAUSSIAN_PARMS=[5.36033,4.27511],LATEBEAM__OVERPLOT_INVERSEGAUSSIAN_PARMS=[7.49282,6.86371],EARLYNOBEAM__OVERPLOT_FRECHET_PARMS=[1.87611,4.41171,-1.54853],LATENOBEAM__OVERPLOT_FRECHET_PARMS=[2.12565,6.04772,-2.37716]
FUNCTION J20180503__SETUP_OVERPLOT, $
   op_ig_parms, $
   op_frech_parms, $
   STATS=stats

     CASE 1 OF
        KEYWORD_SET(op_ig_parms) AND KEYWORD_SET(op_frech_parms): BEGIN
           PRINT,"Can't do both!"
           STOP
        END
        KEYWORD_SET(op_ig_parms): BEGIN
           kappasM1p5 = FINDGEN(1001)/1000. * 20. ;"kappas minus 1.5"

           ;;mu     = op_ig_parms[0]
           ;;lambda = op_ig_parms[1]
           dato = INVERSEGAUSSIANDIST(kappasM1p5, $
                                      op_ig_parms[0], $
                                      op_ig_parms[1], $
                                      STATS=stats)
           name = STRING(FORMAT='(A0,F0.3,"; ",F0.3,")")', $
                         'f!DIG!N($\kappa$-1.5; ', $
                         op_ig_parms[0], $
                         op_ig_parms[1])

        END
        KEYWORD_SET(op_frech_parms): BEGIN

           kappasM1p5 = FINDGEN(1001)/1000. * 20. ;"kappas minus 1.5"

           dato = FRECHETDIST(kappasM1p5, $
                              op_frech_parms[0], $
                              op_frech_parms[1], $
                              op_frech_parms[2], $
                              STATS=stats)

           name = STRING(FORMAT='(A0,F0.3,"; ",F0.3,"; ",F0.3,")")', $
                         'f!DFr!N($\kappa$-1.5; ', $
                         op_frech_parms[0], $
                         op_frech_parms[1], $
                         op_frech_parms[2])

        END
        ELSE:
     ENDCASE

     RETURN,{name:name, $
             x: kappasM1p5+1.5, $
             y: dato, $
             stats: stats}

END
PRO J20180503__ESTIMATE_MU_AND_SIGMA, $
   names,histList,binList,dataList, $
   NORMALIZE_HIST=normalize_hist, $
   BINSIZE=kHBinSize
  
  PRINT,FORMAT='(A15,TR5,A6,TR5,A6,TR5,A6)', $
        'Name','Mu','Sigma','Mode'

  FOR jj=0,N_ELEMENTS(histList)-1 DO BEGIN

     name = names[jj]
     hist = histList[jj]
     bins = binList[jj]+kHBinSize/2.
     data = dataList[jj]

     IF KEYWORD_SET(normalize_hist) THEN BEGIN

        normFac = INT_TABULATED(bins,FLOAT(hist))

        hist    = FLOAT(hist)/normFac

     ENDIF

     mu   = MEAN(ALOG10(data-1.5))
     junk = MAX(hist,modeInd)
     mode = bins[modeInd]-1.5

     sigma = SQRT(mu-ALOG10(mode))

     PRINT,FORMAT='(A15,TR5,F6.3,TR5,F6.3,TR5,F6.3)', $
           name,mu,sigma,mode

  ENDFOR

END
PRO JOURNAL__20180503__KAPPA_FITS__MLT_HISTOS, $
   EXCLUDE_IONS=exclude_ions, $
   REQUIRE_IONS=require_ions, $
   KHIST_BINSIZE=kHist_binSize, $
   KHIST_MIN=kHist_min, $
   MHIST_BINSIZE=mHist_binSize, $
   MHIST_MIN=mHist_min, $
   NORMED=normed, $
   GOVERK=GoverK, $
   MAXKCHI2=maxKChi2, $
   MINALT=minA, $
   MAXALT=maxA, $
   DSTCUTOFF=dstCutoff, $
   MAKEKAPPAHISTOPLOT=makeKappaHistoPlot, $
   MAKEMETASTABPLOT=makeMetaStabPlot, $
   SHOW_EARLYLATE_BEAMS_TOGETHER=show_earlyLate_beams_together, $
   HISTOTITLE__USE_GOVERK_DECILE_STRING=histoTitle__use_GoverK_decile_string, $
   PRINT_HISTOS=print_histos, $
   WRITE_DATA_TO_FILES=write_data_to_files, $
   ESTIMATE_MU_AND_SIGMA=estimate_mu_and_sigma, $
   EARLYBEAM__OVERPLOT_INVERSEGAUSSIAN_PARMS=eb__op_ig_parms, $
   EARLYBEAM__OVERPLOT_FRECHET_PARMS=eb__op_frech_parms, $
   EARLYNOBEAM__OVERPLOT_INVERSEGAUSSIAN_PARMS=en__op_ig_parms, $
   EARLYNOBEAM__OVERPLOT_FRECHET_PARMS=en__op_frech_parms, $
   LATEBEAM__OVERPLOT_INVERSEGAUSSIAN_PARMS=lb__op_ig_parms, $
   LATEBEAM__OVERPLOT_FRECHET_PARMS=lb__op_frech_parms, $
   LATENOBEAM__OVERPLOT_INVERSEGAUSSIAN_PARMS=ln__op_ig_parms, $
   LATENOBEAM__OVERPLOT_FRECHET_PARMS=ln__op_frech_parms

  COMPILE_OPT IDL2,STRICTARRSUBS

  minM = -3.5
  maxM = 1.5
  divM = -1.

  minI  = 60
  maxI  = 90
  hemi  = 'BOTH'

  minA  = 300
  maxA  = 4300

  LOAD_KAPPAFIT_DB,andre, $
                   KF2DPARMS=KF2DParms, $
                   GF2DPARMS=GF2DParms, $
                   TOTT=totT, $
                   ORBARR=orbArr

  bonusPlotSuff = ''

  requireIons = N_ELEMENTS(require_ions) GT 0 ? require_ions : 0
  excludeIons = N_ELEMENTS(exclude_ions) GT 0 ? exclude_ions : 0

  ;; makeKappaHistoPlot    = 1
  ;; makeMetaStabPlot      = 1
  ;; makeMLTILATplot        = 1
  bufferPlots            = 1
  savePlots              = 1
  saveEmAll              = KEYWORD_SET(bufferPlots) OR KEYWORD_SET(savePlots)

  GoverKReq = N_ELEMENTS(GoverK  ) GT 0 ? GoverK   : 'decile=1'
  KChi2Max  = N_ELEMENTS(maxKChi2) GT 0 ? maxKChi2 : 4

  kHBinSize = N_ELEMENTS(kHist_binSize) GT 0 ? kHist_binSize : 0.75
  kHistMin  = N_ELEMENTS(kHist_min    ) GT 0 ? kHist_min     : 1.5

  mHBinSize = N_ELEMENTS(mHist_binSize) GT 0 ? mHist_binSize : 0.05
  mHistMin  = N_ELEMENTS(mHist_min    ) GT 0 ? mHist_min     : 0.

  CASE 1 OF
     SIZE(GoverK,/TYPE) EQ 7: BEGIN
        CASE 1 OF 
           STRMATCH(GoverKReq,'ventile*'): BEGIN
              space = 8
              len   = STRLEN(GoverKReq) EQ 10 ? 2 : 1
              str   = 'Ven'
           END
           ELSE: BEGIN
              space = 7
              len   = 1
              str   = 'Dec'
           END
        ENDCASE
        GoverKStr     = STRING(FORMAT='("-GK",A0,I0)',str,LONG(STRMID(GoverKReq,space,len)))
     END
     ELSE: BEGIN
        GoverKStr     = (STRING(FORMAT='("-GK",F0.1)',GoverKReq)).Replace('.','_')
     END
  ENDCASE
  CASE 1 OF
     SIZE(Kchi2Max,/TYPE) EQ 7: BEGIN
        Kchi2MaxStr   = STRING(FORMAT='("-Kc2Dec",I1)',LONG(STRMID(Kchi2Max,7,1)))
     END
     ELSE: BEGIN
        Kchi2MaxStr   = (STRING(FORMAT='("-Kchi2Max",F0.1)',Kchi2Max)).Replace('.','_')
     END
  ENDCASE
  kHBinSizeStr  = (STRING(FORMAT='("-binSz",F0.2)',kHBinSize)).Replace('.','_')
  mHBinSizeStr  = (STRING(FORMAT='("-binSz",F0.3)',mHBinSize)).Replace('.','_')

  ionSuff       = '-allObs'

  SET_PLOT_DIR,plotDir,/FOR_KAPPA_DB,/ADD_TODAY

  IF maxM GT 18 THEN BEGIN
     minM = 24 + minM
     divM = 24 + divM
  ENDIF

  earlyMinM = minM
  earlyMaxM = divM
  lateMinM  = divM
  lateMaxM  = maxM

  IF (earlyMinM LT 0) AND (earlyMaxM LE 0) THEN BEGIN
     earlyMinM = 24 + earlyMinM
     earlyMaxM = 24 + earlyMaxM
  ENDIF

  IF (lateMinM LT 0) AND (lateMaxM LE 0) THEN BEGIN
     lateMinM  = 24 + lateMinM
     lateMaxM  = 24 + lateMaxM
  ENDIF

  early_i = GET_KAPPADB_INDS( $
            andre, $
            KF2DParms, $
            GF2DParms, $
            OUT_COUNT=count, $
            GOVERKREQ=GoverKReq, $
            KCHI2MAX=KChi2Max, $
            MINMLT=earlyMinM, $
            MAXMLT=earlyMaxM, $
            MINILAT=minI, $
            MAXILAT=maxI, $
            MINALT=minA, $
            MAXALT=maxA, $
            MLTSTR=earlyMLTStr, $
            ALTSTR=altStr, $
            DSTSTR=dstStr, $
            HEMI=hemi, $
            NORTH=north, $
            SOUTH=south, $
            BOTH_HEMIS=both_hemis, $
            GLOBE=globe, $
            DAYSIDE=dayside, $
            NIGHTSIDE=nightside, $
            DSTCUTOFF=dstCutoff)

  late_i = GET_KAPPADB_INDS( $
           andre, $
           KF2DParms, $
           GF2DParms, $
           OUT_COUNT=count, $
           GOVERKREQ=GoverKReq, $
           KCHI2MAX=KChi2Max, $
           MINMLT=lateMinM, $
           MAXMLT=lateMaxM, $
           MINILAT=minI, $
           MAXILAT=maxI, $
           MINALT=minA, $
           MAXALT=maxA, $
           MLTSTR=lateMLTStr, $
           ALTSTR=altStr, $
           DSTSTR=dstStr, $
           HEMI=hemi, $
           NORTH=north, $
           SOUTH=south, $
           BOTH_HEMIS=both_hemis, $
           GLOBE=globe, $
           DAYSIDE=dayside, $
           NIGHTSIDE=nightside, $
           DSTCUTOFF=dstCutoff)

  ion_i       = WHERE(andre.ionBeam EQ 1 OR andre.ionBeam EQ 2,nReq)

  earlyReq_i  = CGSETINTERSECTION(early_i,ion_i, $
                                  COUNT=nEarlyReq)
  earlyExc_i  = CGSETDIFFERENCE(early_i,ion_i, $
                                COUNT=nEarlyExc)

  lateReq_i   = CGSETINTERSECTION(late_i,ion_i, $
                                  COUNT=nLateReq)
  lateExc_i   = CGSETDIFFERENCE(late_i,ion_i, $
                                COUNT=nLateExc)

  ionSuff     = '-combo'
  ;; ENDIF

  parmStr           = GoverKStr + KChi2MaxStr + ionSuff

  ;; What about a metastability measure?
  qVals = 1.+1./KF2DParms.kappa
  MVals = 4.*(qVals-1.)/(qVals+1.)

  magicQVal = 1.+1./2.45
  magicMVal = 4.*(magicQVal-1.)/(magicQVal+1.)
  checkM = magicMVal-(mHBinSize)*(DOUBLE(ROUND(magicMVal/mHBinSize)))

  ;; IF KEYWORD_SET(makeMLTILATplot) THEN BEGIN
  ;;    MLTs = andre.mlt
  ;;    MLTs[WHERE(MLTs GT 18)] = MLTs[WHERE(MLTs GT 18)] - 24.
  ;; ENDIF

  k245LineCol = 'GREEN'
  yHistoTitle = KEYWORD_SET(normed) ? 'Percent' : 'Count'


  PRINT,FORMAT='("Working with ",I0, " EarlyRequInds")',nEarlyReq
  PRINT,FORMAT='("Working with ",I0, " EarlyExclInds")',nEarlyExc
  PRINT,''
  PRINT,FORMAT='("Working with ",I0, " LateRequInds")',nLateReq
  PRINT,FORMAT='("Working with ",I0, " LateExclInds")',nLateExc

  earlyAARName    = 'AAR' + STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+STRING(FORMAT='("(N = ",I5,")")',nEarlyReq)
  earlyBelAARName = 'Below AAR'                                  + STRING(9B)+STRING(FORMAT='("(N = ",I5,")")',nEarlyExc)
  lateAARName    = 'AAR' + STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+STRING(FORMAT='("(N = ",I5,")")',nLateReq)
  lateBelAARName = 'Below AAR'                                  + STRING(9B)+STRING(FORMAT='("(N = ",I5,")")',nLateExc)
  ;; AARName    = 'AAR' + STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+STRING(FORMAT='("(N = ",I5,")")',nReq)
  ;; belAARName = 'Below AAR'                                  + STRING(9B)+STRING(FORMAT='("(N = ",I5,")")',nExc)
  AARMedName = 'AAR' + STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+"(Median)"
  belAARMedName = 'Below AAR'                               + STRING(9B)+"(Median)"


  kHistEarlyReq = HISTOGRAM(KF2DParms.kappa[earlyReq_i], $
                            BINSIZE=kHBinSize, $
                            MIN=kHistMin, $
                            LOCATIONS=kBinsEarlyReq, $
                            REVERSE_INDICES=rKIndsEarlyReq)

  mHistEarlyReq = HISTOGRAM(MVals[earlyReq_i], $
                            BINSIZE=mHBinSize, $
                            MIN=MIN([mHistMin,checkM]), $
                            LOCATIONS=mBinsEarlyReq, $
                            REVERSE_INDICES=rMIndsEarlyReq)

  kHistEarlyExc = HISTOGRAM(KF2DParms.kappa[earlyExc_i], $
                            BINSIZE=kHBinSize, $
                            MIN=kHistMin, $
                            LOCATIONS=kBinsEarlyExc, $
                            REVERSE_INDICES=rKIndsEarlyExc)

  mHistEarlyExc = HISTOGRAM(MVals[earlyExc_i], $
                            BINSIZE=mHBinSize, $
                            MIN=MIN([mHistMin,checkM]), $
                            LOCATIONS=mBinsEarlyExc, $
                            REVERSE_INDICES=rMIndsEarlyExc)


  kHistLateReq = HISTOGRAM(KF2DParms.kappa[lateReq_i], $
                           BINSIZE=kHBinSize, $
                           MIN=kHistMin, $
                           LOCATIONS=kBinsLateReq, $
                           REVERSE_INDICES=rKIndsLateReq)

  mHistLateReq = HISTOGRAM(MVals[lateReq_i], $
                           BINSIZE=mHBinSize, $
                           MIN=MIN([mHistMin,checkM]), $
                           LOCATIONS=mBinsLateReq, $
                           REVERSE_INDICES=rMIndsLateReq)

  kHistLateExc = HISTOGRAM(KF2DParms.kappa[lateExc_i], $
                           BINSIZE=kHBinSize, $
                           MIN=kHistMin, $
                           LOCATIONS=kBinsLateExc, $
                           REVERSE_INDICES=rKIndsLateExc)

  mHistLateExc = HISTOGRAM(MVals[lateExc_i], $
                           BINSIZE=mHBinSize, $
                           MIN=MIN([mHistMin,checkM]), $
                           LOCATIONS=mBinsLateExc, $
                           REVERSE_INDICES=rMIndsLateExc)

     IF KEYWORD_SET(eb__op_ig_parms) $
        OR KEYWORD_SET(eb__op_frech_parms) $
     THEN BEGIN
        ebStat = J20180503__SETUP_OVERPLOT(eb__op_ig_parms,eb__op_frech_parms, $
                                           STATS=stats)
     ENDIF
     IF KEYWORD_SET(en__op_ig_parms) $
        OR KEYWORD_SET(en__op_frech_parms) $
     THEN BEGIN
        enStat = J20180503__SETUP_OVERPLOT(en__op_ig_parms,en__op_frech_parms, $
                                           STATS=stats)
     ENDIF
     IF KEYWORD_SET(lb__op_ig_parms) $
        OR KEYWORD_SET(lb__op_frech_parms) $
     THEN BEGIN
        lbStat = J20180503__SETUP_OVERPLOT(lb__op_ig_parms,lb__op_frech_parms, $
                                           STATS=stats)
     ENDIF
     IF KEYWORD_SET(ln__op_ig_parms) $
        OR KEYWORD_SET(ln__op_frech_parms) $
     THEN BEGIN
        lnStat = J20180503__SETUP_OVERPLOT(ln__op_ig_parms,ln__op_frech_parms, $
                                           STATS=stats)
     ENDIF

  ;; Print for mathematica???
  IF KEYWORD_SET(estimate_mu_and_sigma) $
     OR KEYWORD_SET(print_histos         ) $
  THEN BEGIN
     names    = ['kHistEarlyReq','kHistEarlyExc', $
                 'kHistLateReq','kHistLateExc']
     histList = LIST(kHistEarlyReq,kHistEarlyExc, $
                     kHistLateReq,kHistLateExc)
     binList  = LIST(kBinsEarlyReq,kBinsEarlyExc, $
                     kBinsLateReq,kBinsLateExc)
     dataList = LIST(KF2DParms.kappa[earlyReq_i], $
                     KF2DParms.kappa[earlyExc_i], $
                     KF2DParms.kappa[lateReq_i], $
                     KF2DParms.kappa[lateExc_i])
  ENDIF

  IF KEYWORD_SET(print_histos) THEN BEGIN
     HISTO_PRINT_FOR_MATHEMATICA, $
        names,histList,binList,dataList, $
        /NORMALIZE_HIST, $
        /ALSO_PRINT_DATA, $
        WRITE_TO_FILE=write_data_to_files, $
        FILEPREFS=[earlyMLTStr+'-ionBeam-',earlyMLTStr+'-noBeam-', $
                   lateMLTStr+'-ionBeam-',lateMLTStr+'-noBeam-'], $
        FILESUFF=STRING(FORMAT='(A0,"-",A0,A0,A0)', $
                 altStr+dstStr, $
                 hemi,parmStr,bonusPlotSuff), $
        WRITEDIR='/SPENCEdata/Research/Satellites/FAST/kappa_dists/txtOutput/', $
        BINSIZE=kHBinSize
  ENDIF
  IF KEYWORD_SET(estimate_mu_and_sigma) THEN BEGIN
     J20180503__ESTIMATE_MU_AND_SIGMA, $
        names,histList,binList,dataList, $
        NORMALIZE_HIST=normalize_hist, $
        BINSIZE=kHBinSize
  ENDIF
  
  IF kBinsLateExc[0] LT 1.5 THEN kBinsLateExc[0] = 1.5
  IF kBinsLateReq[0] LT 1.5 THEN kBinsLateReq[0] = 1.5
  IF kBinsEarlyExc[0] LT 1.5 THEN kBinsEarlyExc[0] = 1.5
  IF kBinsEarlyReq[0] LT 1.5 THEN kBinsEarlyReq[0] = 1.5
  IF mBinsEarlyReq[0] LT 1.5 THEN mBinsEarlyReq[0] = 1.5
  IF mBinsEarlyExc[0] LT 1.5 THEN mBinsEarlyExc[0] = 1.5
  IF mBinsLateReq[0] LT 1.5 THEN mBinsLateReq[0] = 1.5
  IF mBinsLateExc[0] LT 1.5 THEN mBinsLateExc[0] = 1.5

  IF KEYWORD_SET(normed) THEN BEGIN
     kTotEarlyReq  = INT_TABULATED(kBinsEarlyReq,FLOAT(kHistEarlyReq))
     mTotEarlyReq  = INT_TABULATED(mBinsEarlyReq,FLOAT(mHistEarlyReq))

     kHistEarlyReq = FLOAT(kHistEarlyReq)/kTotEarlyReq
     mHistEarlyReq = FLOAT(mHistEarlyReq)/mTotEarlyReq

     kTotEarlyExc  = INT_TABULATED(kBinsEarlyExc,FLOAT(kHistEarlyExc))
     mTotEarlyExc  = INT_TABULATED(mBinsEarlyExc,FLOAT(mHistEarlyExc))

     kHistEarlyExc = FLOAT(kHistEarlyExc)/kTotEarlyExc
     mHistEarlyExc = FLOAT(mHistEarlyExc)/mTotEarlyExc

     kTotLateReq  = INT_TABULATED(kBinsLateReq,FLOAT(kHistLateReq))
     mTotLateReq  = INT_TABULATED(mBinsLateReq,FLOAT(mHistLateReq))

     kHistLateReq = FLOAT(kHistLateReq)/kTotLateReq
     mHistLateReq = FLOAT(mHistLateReq)/mTotLateReq

     kTotLateExc  = INT_TABULATED(kBinsLateExc,FLOAT(kHistLateExc))
     mTotLateExc  = INT_TABULATED(mBinsLateExc,FLOAT(mHistLateExc))

     kHistLateExc = FLOAT(kHistLateExc)/kTotLateExc
     mHistLateExc = FLOAT(mHistLateExc)/mTotLateExc

     ;; kTotEarlyReq = TOTAL(kHistEarlyReq)
     ;; mTotEarlyReq = TOTAL(mHistEarlyReq)

     ;; kHistEarlyReq = FLOAT(kHistEarlyReq)/FLOAT(kTotEarlyReq)*100.
     ;; mHistEarlyReq = FLOAT(mHistEarlyReq)/FLOAT(mTotEarlyReq)*100.

     ;; kTotEarlyExc = TOTAL(kHistEarlyExc)
     ;; mTotEarlyExc = TOTAL(mHistEarlyExc)

     ;; kHistEarlyExc = FLOAT(kHistEarlyExc)/FLOAT(kTotEarlyExc)*100.
     ;; mHistEarlyExc = FLOAT(mHistEarlyExc)/FLOAT(mTotEarlyExc)*100.

     ;; kTotLateReq = TOTAL(kHistLateReq)
     ;; mTotLateReq = TOTAL(mHistLateReq)

     ;; kHistLateReq = FLOAT(kHistLateReq)/FLOAT(kTotLateReq)*100.
     ;; mHistLateReq = FLOAT(mHistLateReq)/FLOAT(mTotLateReq)*100.

     ;; kTotLateExc = TOTAL(kHistLateExc)
     ;; mTotLateExc = TOTAL(mHistLateExc)

     ;; kHistLateExc = FLOAT(kHistLateExc)/FLOAT(kTotLateExc)*100.
     ;; mHistLateExc = FLOAT(mHistLateExc)/FLOAT(mTotLateExc)*100.

     parmStr += '-normed'
  ENDIF

  ILATkappaBelTransp = 85
  ILATkappaAARTransp = 65
  belAARCol = 'BLACK'
  AARCol    = 'RED'

  belAARSym = 'x'
  AARSym    = '+'

  earlyTitle = 'Early'
  lateTitle = 'Late'

  legFontSize = 14
  fontSize = 20

  AAR_with_AAR = N_ELEMENTS(show_earlyLate_beams_together) GT 0 ? show_earlyLate_beams_together : 1
  IF KEYWORD_SET(AAR_with_AAR) THEN BEGIN

     kBinsTmp = kBinsEarlyExc
     kHistTmp = kHistEarlyExc
     nameTmp  = earlyBelAARName

     kBinsEarlyExc = kBinsLateReq
     kHistEarlyExc = kHistLateReq
     earlyBelAARName = lateAARName

     kBinsLateReq = kBinsTmp
     kHistLateReq = kHistTmp
     lateAARName  = nameTmp

     earlyTitle = 'AAR'
     lateTitle  = 'Below AAR'

     ;; earlyAARName    = earlyAARName.Replace('AAR','Early')   
     ;; earlyBelAARName = earlyBelAARName.Replace('AAR','Late')
     ;; lateAARName     = lateAARName.Replace('Below AAR','Early')    
     ;; lateBelAARName  = lateBelAARName.Replace('Below AAR','Late') 

     earlyAARName    = earlyAARName.Replace('AAR',earlyMLTStr.Replace('MLT',' MLT'))   
     earlyBelAARName = earlyBelAARName.Replace('AAR',lateMLTStr.Replace('MLT',' MLT'))

     ;; Secretly, these are the below AAR 
     lateAARName     = lateAARName.Replace('Below AAR',earlyMLTStr.Replace('MLT',' MLT'))    
     lateBelAARName  = lateBelAARName.Replace('Below AAR',lateMLTStr.Replace('MLT',' MLT')) 
     lateAARName     = lateAARName.Replace('_','.')
     lateBelAARName  = lateBelAARName.Replace('_','.')

     earlyAARName    = earlyAARName.Replace(STRING(9B)+STRING(9B)+STRING(9B),'')
     earlyBelAARName = earlyBelAARName.Replace(STRING(9B)+STRING(9B)+STRING(9B),'')
     earlyAARName    = earlyAARName.Replace('_','.')
     earlyBelAARName = earlyBelAARName.Replace('_','.')

     earlyMLTStr     = 'AAR'
     lateMLTStr      = 'belAAR'

  ENDIF ELSE BEGIN

     earlyTitle = earlyMLTStr
     lateTitle = lateMLTStr

  ENDELSE

  IF KEYWORD_SET(histoTitle__use_GoverK_decile_string) THEN BEGIN
           CASE 1 OF 
              STRMATCH(GoverKReq,'ventile*'): BEGIN
                 space = 8
                 len   = STRLEN(GoverKReq) EQ 10 ? 2 : 1
                 str   = ' Ventile'
              END
              ELSE: BEGIN
                 space = 7
                 len   = 1
                 str   = ' Decile'
              END
           ENDCASE
     titleSuff  = '!C' $
                  + CARDINAL_TO_ORDINAL_STRING(LONG(STRMID(GoverKReq,space,len)),/TOUPCASE) $
                  + str
     earlyTitle += titleSuff
     lateTitle  += titleSuff
  ENDIF

  xRange   = [1.5,20]

  IF KEYWORD_SET(makeKappaHistoPlot) THEN BEGIN
     earlyWinder   = WINDOW(DIMENSIONS=[800,800],BUFFER=bufferPlots)

     legTargets = !NULL
     yRange   = [0,(MAX(kHistEarlyExc)>MAX(kHistEarlyReq))*1.1]
     kHistPlotEarlyReq = PLOT(kBinsEarlyReq,kHistEarlyReq,/HISTOGRAM, $
                              XRANGE=xRange, $
                              YRANGE=yRange, $
                              NAME=earlyAARName, $
                              ;; LINESTYLE='
                              TITLE=earlyTitle, $
                              XTITLE='$\kappa$', $
                              YTITLE=yHistoTitle, $
                              FONT_SIZE=fontSize,THICK=2.5, $
                              CURRENT=earlyWinder)

     legTargets = [legTargets,kHistPlotEarlyReq]

     IF KEYWORD_SET(eb__op_ig_parms) $
        OR KEYWORD_SET(eb__op_frech_parms) $
     THEN BEGIN
        ebStatPlot = PLOT(ebStat.x,ebStat.y, $
                                 XRANGE=xRange, $
                                 YRANGE=yRange, $
                                 LINESTYLE='__', $
                                 NAME=ebStat.name, $
                                 ;; COLOR='GRAY', $
                                 THICK=3.0, $
                                 /OVERPLOT, $
                                 CURRENT=earlyWinder)

        legTargets = [legTargets,ebStatPlot]
     ENDIF

     kHistPlotEarlyExc = PLOT(kBinsEarlyExc,kHistEarlyExc,/HISTOGRAM, $
                              XRANGE=xRange, $
                              YRANGE=yRange, $
                              LINESTYLE='--', $
                              NAME=earlyBelAARName, $
                              COLOR='GRAY', $
                              THICK=2.5, $
                              /OVERPLOT, $
                              CURRENT=earlyWinder)

     legTargets = [legTargets,kHistPlotEarlyExc]

     IF KEYWORD_SET(en__op_ig_parms) $
        OR KEYWORD_SET(en__op_frech_parms) $
     THEN BEGIN

        enStatPlot = PLOT(enStat.x,enStat.y, $
                                 XRANGE=xRange, $
                                 YRANGE=yRange, $
                                 LINESTYLE='-.', $
                                 NAME=enStat.name, $
                                 COLOR='GRAY', $
                                 THICK=3.0, $
                                 /OVERPLOT, $
                                 CURRENT=earlyWinder)


        legTargets = [legTargets,enStatPlot]
     ENDIF

     histLegend  = LEGEND(TARGET=legTargets, $
                          /NORMAL, $
                          FONT_SIZE=legFontSize, $
                          POSITION=[0.85,0.7])

     kLinePlot = PLOT(REPLICATE(2.45,11),FINDGEN(11)/10.*MAX(kHistEarlyReq)*2, $
                      YRANGE=yRange, $
                      THICK=2., $
                      COLOR=k245LineCol, $
                      LINESTYLE=':', $
                      /OVERPLOT, $
                      CURRENT=earlyWinder)

     kText     = TEXT(0.24,0.22,"$\kappa_t$ = 2.45", $
                      /NORMAL, $
                      FONT_SIZE=fontSize, $
                      FONT_COLOR=k245LineCol, $
                      TARGET=earlyWinder)

     IF saveEmAll THEN BEGIN
        outPlotName = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) $
                      + STRING(FORMAT='("-kappaStats_",A0,A0,"-",A0,A0,A0,A0,".png")', $
                               earlyMLTStr,altStr+dstStr, $
                               hemi,parmStr,kHBinSizeStr,bonusPlotSuff)
        PRINT,"Saving to " + outPlotName
        earlyWinder.Save,plotDir+outPlotName
     ENDIF

     ;; NOW LATE
     lateWinder   = WINDOW(DIMENSIONS=[800,800],BUFFER=bufferPlots)

     legTargets = !NULL
     yRange   = [0,(MAX(kHistLateExc)>MAX(kHistLateReq))*1.1]

     kHistPlotLateReq = PLOT(kBinsLateReq,kHistLateReq,/HISTOGRAM, $
                             XRANGE=xRange, $
                             YRANGE=yRange, $
                             NAME=lateAARName, $
                             TITLE=lateTitle, $
                             ;; LINESTYLE='
                             XTITLE='$\kappa$', $
                             YTITLE=yHistoTitle, $
                             FONT_SIZE=fontSize,THICK=2.5, $
                             CURRENT=lateWinder)

     legTargets = [legTargets,kHistPlotLateReq]

     IF KEYWORD_SET(lb__op_ig_parms) $
        OR KEYWORD_SET(lb__op_frech_parms) $
     THEN BEGIN

        lbStatPlot = PLOT(lbStat.x,lbStat.y, $
                                 XRANGE=xRange, $
                                 YRANGE=yRange, $
                                 LINESTYLE='__', $
                                 NAME=lbStat.name, $
                                 ;; COLOR='GRAY', $
                                 THICK=3.0, $
                                 /OVERPLOT, $
                                 CURRENT=lateWinder)


        legTargets = [legTargets,lbStatPlot]

     ENDIF

     kHistPlotLateExc = PLOT(kBinsLateExc,kHistLateExc,/HISTOGRAM, $
                             XRANGE=xRange, $
                             YRANGE=yRange, $
                             LINESTYLE='--', $
                             NAME=lateBelAARName, $
                             COLOR='GRAY', $
                             THICK=2.5, $
                             /OVERPLOT, $
                             CURRENT=lateWinder)

     legTargets = [legTargets,kHistPlotLateExc]

     IF KEYWORD_SET(ln__op_ig_parms) $
        OR KEYWORD_SET(ln__op_frech_parms) $
     THEN BEGIN

        lnStatPlot = PLOT(lnStat.x,lnStat.y, $
                                 XRANGE=xRange, $
                                 YRANGE=yRange, $
                                 LINESTYLE='-.', $
                                 NAME=lnStat.name, $
                                 COLOR='GRAY', $
                                 THICK=3.0, $
                                 /OVERPLOT, $
                                 CURRENT=lateWinder)


        legTargets = [legTargets,lnStatPlot]
     ENDIF

     histLegend  = LEGEND(TARGET=legTargets, $
                          /NORMAL, $
                          FONT_SIZE=legFontSize, $
                          POSITION=[0.85,0.7])

     kLinePlot = PLOT(REPLICATE(2.45,11),FINDGEN(11)/10.*MAX(kHistLateReq)*2, $
                      YRANGE=yRange, $
                      THICK=2., $
                      COLOR=k245LineCol, $
                      LINESTYLE=':', $
                      /OVERPLOT, $
                      CURRENT=lateWinder)

     kText     = TEXT(0.24,0.22,"$\kappa_t$ = 2.45", $
                      /NORMAL, $
                      FONT_SIZE=fontSize, $
                      FONT_COLOR=k245LineCol, $
                      TARGET=lateWinder)

     IF saveEmAll THEN BEGIN
        outPlotName = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) $
                      + STRING(FORMAT='("-kappaStats_",A0,A0,"-",A0,A0,A0,A0,".png")', $
                               lateMLTStr,altStr+dstStr,hemi,parmStr,kHBinSizeStr,bonusPlotSuff)
        PRINT,"Saving to " + outPlotName

        lateWinder.Save,plotDir+outPlotName
     ENDIF

  ENDIF

  IF ~KEYWORD_SET(bufferPlots) THEN STOP

  IF KEYWORD_SET(makeKappaHistoPlot) THEN BEGIN
     earlyWinder.Close
     lateWinder.Close
  ENDIF

END
