;2018/05/03
PRO J20180503__PRINT_FOR_MATHEMATICA, $
   kHistEarlyReq,kHistEarlyExc,kHistLateReq,kHistLateExc, $
   kBinsEarlyReq,kBinsEarlyExc,kBinsLateReq,kBinsLateExc, $
   BINSIZE=kHBinSize
  

  names = ['kHistEarlyReq','kHistEarlyExc','kHistLateReq','kHistLateExc']
  hists = LIST(kHistEarlyReq,kHistEarlyExc,kHistLateReq,kHistLateExc)
  bins  = LIST(kBinsEarlyReq,kBinsEarlyExc,kBinsLateReq,kBinsLateExc)
  FOR jj=0,N_ELEMENTS(hists)-1 DO BEGIN

     name = names[jj]
     hist = hists[jj]
     bin = bins[jj]+kHBinSize/2.
     PRINT,FORMAT='(A0," = {")',name

     FOR k=0,N_ELEMENTS(hist)-1 DO BEGIN
        PRINT,FORMAT='("{",F0.2,",",F0.2,"}",A0)', $
              bin[k], $
              hist[k], $
              (k EQ (N_ELEMENTS(hist)-1) ? '' : ',')
     ENDFOR

     PRINT,'}'

  ENDFOR

END
PRO JOURNAL__20180503__KAPPA_FITS__MLT_HISTOS, $
   EXCLUDE_IONS=exclude_ions, $
   REQUIRE_IONS=require_ions, $
   COMBINED_HISTOS=combined_histos, $
   KHIST_BINSIZE=kHist_binSize, $
   KHIST_MIN=kHist_min, $
   MHIST_BINSIZE=mHist_binSize, $
   MHIST_MIN=mHist_min, $
   NORMED=normed, $
   GOVERK=GoverK, $
   MAXKCHI2=maxKChi2, $
   MINALT=minA, $
   MAXALT=maxA, $
   MAKEKAPPAHISTOPLOT=makeKappaHistoPlot, $
   MAKEMETASTABPLOT=makeMetaStabPlot

  COMPILE_OPT IDL2,STRICTARRSUBS

  minM = -3.5
  maxM = 1.5
  divM = -1.5

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

  kHBinSize = N_ELEMENTS(kHist_binSize) GT 0 ? kHist_binSize : 0.5
  kHistMin  = N_ELEMENTS(kHist_min    ) GT 0 ? kHist_min     : 1.45

  mHBinSize = N_ELEMENTS(mHist_binSize) GT 0 ? mHist_binSize : 0.05
  mHistMin  = N_ELEMENTS(mHist_min    ) GT 0 ? mHist_min     : 0.

  CASE 1 OF
     SIZE(GoverK,/TYPE) EQ 7: BEGIN
        GoverKStr     = STRING(FORMAT='("-GKDec",I1)',LONG(STRMID(GoverKReq,7,1)))
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
  kHBinSizeStr  = (STRING(FORMAT='("-binSz",F0.1)',kHBinSize)).Replace('.','_')
  mHBinSizeStr  = (STRING(FORMAT='("-binSz",F0.3)',mHBinSize)).Replace('.','_')

  ionSuff       = '-allObs'

  plotDir       = '/SPENCEdata/Research/Satellites/FAST/kappa_dists/plots/'

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
            HEMI=hemi, $
            NORTH=north, $
            SOUTH=south, $
            BOTH_HEMIS=both_hemis, $
            GLOBE=globe, $
            DAYSIDE=dayside, $
            NIGHTSIDE=nightside)

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
           HEMI=hemi, $
           NORTH=north, $
           SOUTH=south, $
           BOTH_HEMIS=both_hemis, $
           GLOBE=globe, $
           DAYSIDE=dayside, $
           NIGHTSIDE=nightside)


  ;; IF KEYWORD_SET(requireIons) THEN BEGIN
  ;;    ion_i       = WHERE(andre.ionBeam EQ 1 OR andre.ionBeam EQ 2,nIonBeam, $
  ;;                        NCOMPLEMENT=nNotIonBeam)
  ;;    early_i     = CGSETINTERSECTION(early_i,ion_i, $
  ;;                                    COUNT=count)
  ;;    late_i      = CGSETINTERSECTION(late_i,ion_i, $
  ;;                                    COUNT=count)
  ;;    ionSuff     = '-onlyIonBeams'
  ;; ENDIF ELSE IF KEYWORD_SET(excludeIons) THEN BEGIN
  ;;    ion_i       = WHERE(andre.ionBeam EQ 1 OR andre.ionBeam EQ 2,nIonBeam, $
  ;;                        NCOMPLEMENT=nNotIonBeam)
  ;;    early_i     = CGSETDIFFERENCE(early_i,ion_i, $
  ;;                                  COUNT=count)
  ;;    late_i      = CGSETDIFFERENCE(late_i,ion_i, $
  ;;                                  COUNT=count)
  ;;    ionSuff     = '-excludeIons'
  ;; ENDIF ELSE IF KEYWORD_SET(combined_histos) THEN BEGIN
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

  ;; Print for mathematica???
  J20180503__PRINT_FOR_MATHEMATICA, $
     kHistEarlyReq,kHistEarlyExc,kHistLateReq,kHistLateExc, $
     kBinsEarlyReq,kBinsEarlyExc,kBinsLateReq,kBinsLateExc, $
     BINSIZE=kHBinSize

  IF kBinsLateExc[0] LT 1.5 THEN kBinsLateExc[0] = 1.5
  IF kBinsLateReq[0] LT 1.5 THEN kBinsLateReq[0] = 1.5
  IF kBinsEarlyExc[0] LT 1.5 THEN kBinsEarlyExc[0] = 1.5
  IF kBinsEarlyReq[0] LT 1.5 THEN kBinsEarlyReq[0] = 1.5
  IF mBinsEarlyReq[0] LT 1.5 THEN mBinsEarlyReq[0] = 1.5
  IF mBinsEarlyExc[0] LT 1.5 THEN mBinsEarlyExc[0] = 1.5
  IF mBinsLateReq[0] LT 1.5 THEN mBinsLateReq[0] = 1.5
  IF mBinsLateExc[0] LT 1.5 THEN mBinsLateExc[0] = 1.5

  IF KEYWORD_SET(normed) THEN BEGIN
     kTotEarlyReq = TOTAL(kHistEarlyReq)
     mTotEarlyReq = TOTAL(mHistEarlyReq)

     kHistEarlyReq = FLOAT(kHistEarlyReq)/FLOAT(kTotEarlyReq)*100.
     mHistEarlyReq = FLOAT(mHistEarlyReq)/FLOAT(mTotEarlyReq)*100.

     kTotEarlyExc = TOTAL(kHistEarlyExc)
     mTotEarlyExc = TOTAL(mHistEarlyExc)

     kHistEarlyExc = FLOAT(kHistEarlyExc)/FLOAT(kTotEarlyExc)*100.
     mHistEarlyExc = FLOAT(mHistEarlyExc)/FLOAT(mTotEarlyExc)*100.

     kTotLateReq = TOTAL(kHistLateReq)
     mTotLateReq = TOTAL(mHistLateReq)

     kHistLateReq = FLOAT(kHistLateReq)/FLOAT(kTotLateReq)*100.
     mHistLateReq = FLOAT(mHistLateReq)/FLOAT(mTotLateReq)*100.

     kTotLateExc = TOTAL(kHistLateExc)
     mTotLateExc = TOTAL(mHistLateExc)

     kHistLateExc = FLOAT(kHistLateExc)/FLOAT(kTotLateExc)*100.
     mHistLateExc = FLOAT(mHistLateExc)/FLOAT(mTotLateExc)*100.

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

  combine_like = 1
  IF KEYWORD_SET(combine_like) THEN BEGIN

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

  IF KEYWORD_SET(makeKappaHistoPlot) THEN BEGIN
     earlyWinder   = WINDOW(DIMENSIONS=[800,800],BUFFER=bufferPlots)

     legFontSize = 14
     fontSize = 20

     xRange   = [1.5,15]
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

     kHistPlotEarlyExc = PLOT(kBinsEarlyExc,kHistEarlyExc,/HISTOGRAM, $
                              XRANGE=xRange, $
                              YRANGE=yRange, $
                              LINESTYLE='--', $
                              NAME=earlyBelAARName, $
                              COLOR='GRAY', $
                              THICK=2.5, $
                              /OVERPLOT, $
                              CURRENT=earlyWinder)

     histLegend  = LEGEND(TARGET=[kHistPlotEarlyReq,kHistPlotEarlyExc], $
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
                               earlyMLTStr,altStr, $
                               hemi,parmStr,kHBinSizeStr,bonusPlotSuff)
        PRINT,"Saving to " + outPlotName
        earlyWinder.Save,plotDir+outPlotName
     ENDIF

     ;; NOW LATE
     lateWinder   = WINDOW(DIMENSIONS=[800,800],BUFFER=bufferPlots)

     xRange   = [1.5,15]
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

     kHistPlotLateExc = PLOT(kBinsLateExc,kHistLateExc,/HISTOGRAM, $
                             XRANGE=xRange, $
                             YRANGE=yRange, $
                             LINESTYLE='--', $
                             NAME=lateBelAARName, $
                             COLOR='GRAY', $
                             THICK=2.5, $
                             /OVERPLOT, $
                             CURRENT=lateWinder)

     histLegend  = LEGEND(TARGET=[kHistPlotLateReq,kHistPlotLateExc], $
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
                               lateMLTStr,altStr,hemi,parmStr,kHBinSizeStr,bonusPlotSuff)
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
