;2018/04/19
PRO JOURNAL__20180419__ESSAYE_AVEC_DES_KAPPA_FIT_FILES, $
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
  makeMLTILATplot        = 0
  makeILATKappaplot      = 0
  makeMLTKappaplot       = 0
  bufferPlots            = 1

  GoverKReq = KEYWORD_SET(GoverK)   ? GoverK   : 1.5
  KChi2Max  = KEYWORD_SET(maxKChi2) ? maxKChi2 : 5

  minM  = -3.5
  maxM  = 1.5
  notMLT = 0
  minI  = 60
  maxI  = 90
  hemi  = 'BOTH'

  kHBinSize = N_ELEMENTS(kHist_binSize) GT 0 ? kHist_binSize : 1.0
  kHistMin  = N_ELEMENTS(kHist_min    ) GT 0 ? kHist_min     : 1.4

  mHBinSize = N_ELEMENTS(mHist_binSize) GT 0 ? mHist_binSize : 0.05
  mHistMin  = N_ELEMENTS(mHist_min    ) GT 0 ? mHist_min     : 0.


  GoverKStr     = (STRING(FORMAT='("-GoverK",F0.1)',GoverKReq)).Replace('.','_')
  KChi2MaxStr   = (STRING(FORMAT='("-Kchi2Max",F0.1)',KChi2Max)).Replace('.','_')
  kHBinSizeStr  = (STRING(FORMAT='("-binSz",F0.1)',kHBinSize)).Replace('.','_')
  mHBinSizeStr  = (STRING(FORMAT='("-binSz",F0.3)',mHBinSize)).Replace('.','_')

  ionSuff       = '-allObs'

  plotDir       = '/SPENCEdata/Research/Satellites/FAST/kappa_dists/plots/'

  final_i = GET_KAPPADB_INDS( $
            andre, $
            KF2DParms, $
            GF2DParms, $
            OUT_COUNT=count, $
            GOVERKREQ=GoverKReq, $
            KCHI2MAX=KChi2Max, $
            MINMLT=minM, $
            MAXMLT=maxM, $
            BINMLT=binM, $
            SHIFTMLT=shiftM, $
            MINILAT=minI, $
            MAXILAT=maxI, $
            MINALT=minA, $
            MAXALT=maxA, $
            MLTSTR=mltStr, $
            ALTSTR=altStr, $
            HEMI=hemi, $
            NORTH=north, $
            SOUTH=south, $
            BOTH_HEMIS=both_hemis, $
            GLOBE=globe, $
            DAYSIDE=dayside, $
            NIGHTSIDE=nightside)

  ;; Disqualified
  ;; disqual_i         = WHERE((KF2DParms.kappa LE 2.0) $
  ;;                           AND (GF2DParms.chi2red/KF2DParms.chi2red LE 5),nDisqual)
  ;; IF nDisqual GT 0 THEN BEGIN
  ;;    final_i = CGSETDIFFERENCE(final_i,disqual_i)
  ;; ENDIF

  IF KEYWORD_SET(requireIons) AND KEYWORD_SET(excludeIons) THEN BEGIN
     PRINT,"Now you're on my turf."
     STOP
  ENDIF

  IF KEYWORD_SET(requireIons) THEN BEGIN
     ion_i          = WHERE(andre.ionBeam EQ 1 OR andre.ionBeam EQ 2,nIonBeam, $
                            NCOMPLEMENT=nNotIonBeam)
     final_i        = CGSETINTERSECTION(final_i,ion_i, $
                                        COUNT=count)
     ionSuff        = '-onlyIonBeams'
  ENDIF ELSE IF KEYWORD_SET(excludeIons) THEN BEGIN
     ion_i          = WHERE(andre.ionBeam EQ 1 OR andre.ionBeam EQ 2,nIonBeam, $
                            NCOMPLEMENT=nNotIonBeam)
     final_i        = CGSETDIFFERENCE(final_i,ion_i, $
                                      COUNT=count)
     ionSuff        = '-excludeIons'
  ENDIF ELSE IF KEYWORD_SET(combined_histos) THEN BEGIN
     ion_i          = WHERE(andre.ionBeam EQ 1 OR andre.ionBeam EQ 2,nReq)

     req_i          = CGSETINTERSECTION(final_i,ion_i, $
                                        COUNT=nReq)
     exc_i          = CGSETDIFFERENCE(final_i,ion_i, $
                                      COUNT=nExc)

     ionSuff        = '-onlyAndExcl'
  ENDIF

  parmStr           = GoverKStr + KChi2MaxStr + ionSuff

  ;; What about a metastability measure?
  qVals = 1.+1./KF2DParms.kappa
  MVals = 4.*(qVals-1.)/(qVals+1.)

  magicQVal = 1.+1./2.45
  magicMVal = 4.*(magicQVal-1.)/(magicQVal+1.)
  checkM = magicMVal-(mHBinSize)*(DOUBLE(ROUND(magicMVal/mHBinSize)))

  IF KEYWORD_SET(makeMLTILATplot) OR KEYWORD_SET(makeMLTKappaplot) THEN BEGIN
     MLTs = andre.mlt
     MLTs[WHERE(MLTs GT 18)] = MLTs[WHERE(MLTs GT 18)] - 24.
  ENDIF

  ;; Unique low kappa-ers
  lowKappa_i        = WHERE(KF2DParms.kappa LE 2.0,nLowKappa, $
                            COMPLEMENT=notLowKappa_i, $
                            NCOMPLEMENT=nNotLowKappa)
  lowKappa_i        = CGSETINTERSECTION(lowKappa_i,final_i, $
                                        COUNT=nLowKappa)
  lowKappaOrbs      = andre.orbit[lowkappa_i[UNIQ(andre.orbit[lowkappa_i], $
                                                  SORT(andre.orbit[lowkappa_i]))]]
  allOrbs           = andre.orbit[UNIQ(andre.orbit, $
                                       SORT(andre.orbit))]

  ;; nNorth            = FLOAT(N_ELEMENTS(WHERE(andre.ilat[final_i] GE 0)))
  ;; nSouth            = FLOAT(N_ELEMENTS(WHERE(andre.ilat[final_i] LT 0)))
  ;; pctNorth          = nNorth/count
  ;; pctSouth          = nSouth/count

  k245LineCol = 'GREEN'
  yHistoTitle = KEYWORD_SET(normed) ? 'Percent' : 'Count'

  IF ~KEYWORD_SET(combined_histos) THEN BEGIN

     PRINT,FORMAT='("Working with ",I0, " inds")',count

     plot_i            = final_i

     ;; !P.multi = [0,1,1,0,0]
     ;; WINDOW,1,XSIZE=600,YSIZE=600
     ;; CGHISTOPLOT,KF2DParms.kappa[plot_i],BINSIZE=0.15,MININPUT=1.5,MAXINPUT=10

     ;; !P.multi = [0,2,2,0,0]
     ;; WINDOW,0,XSIZE=600,YSIZE=600
     ;; CGHISTOPLOT,ALOG10(GF2DParms.chi2red[plot_i]),TITLE="Maxwellian"
     ;; CGHISTOPLOT,ALOG10(KF2DParms.chi2red[plot_i]),TITLE='Kappa'
     ;; CGHISTOPLOT,GF2DParms.chi2red[plot_i]/KF2DParms.chi2red[plot_i],TITLE='Ratio G/K',MAXINPUT=10,binsize=0.1

     kHist = HISTOGRAM(KF2DParms.kappa[plot_i], $
                       BINSIZE=kHBinSize, $
                       MIN=kHistMin, $
                       LOCATIONS=kBins, $
                       REVERSE_INDICES=rKInds)

     mHist = HISTOGRAM(MVals[plot_i], $
                       BINSIZE=mHBinSize, $
                       MIN=MIN([mHistMin,checkM]), $
                       LOCATIONS=mBins, $
                       REVERSE_INDICES=rMInds)

     IF KEYWORD_SET(normed) THEN BEGIN
        kTot = TOTAL(kHist)
        mTot = TOTAL(mHist)

        kHist = FLOAT(kHist)/FLOAT(kTot)*100.
        mHist = FLOAT(mHist)/FLOAT(mTot)*100.

        parmStr += '-normed'
     ENDIF

     ;; Where are they less than .245?
     lt245inds = !NULL
     bin245 = VALUE_CLOSEST2(kBins,2.45,/CONSTRAINED)
     FOR k=0,bin245-1 DO BEGIN
        IF rKInds[k] NE rKInds[k+1] THEN $
           lt245inds = [lt245inds,rKInds[rKInds[k] : rKInds[k+1]-1]]
     ENDFOR

     ;; Kappa plot
     ;; titleStr = STRING(FORMAT='(I02,"-",I02," ' + mltSuff + ', ",I0,"$^\circ$ < |ILAT| < ",I0,"$^\circ$!C!COrbits ",I0,"-",I0," (",I0,"/",I0," considered)")', $
     ;;                   minM+24,maxM, $
     ;;                   minI,maxI, $
     ;;                   MIN(orbArr),MAX(orbArr), $
     ;;                   N_ELEMENTS(orbArr),MAX(orbArr)-MIN(orbArr))
     titleStr = STRING(FORMAT='(I02,"-",I02," ' + mltSuff + ', ",I0,"$^\circ$ < |ILAT| < ",I0,"$^\circ$!C!COrbits ",I0,"-",I0)', $
                       minM+24,maxM, $
                       minI,maxI, $
                       MIN(orbArr),MAX(orbArr))
     winder   = WINDOW(DIMENSIONS=[800,800],BUFFER=bufferPlots)

     xRange   = [1.5,15]
     yRange   = [0,MAX(kHist)*1.1]
     kHistPlot = PLOT(kBins,kHist,/HISTOGRAM, $
                      XRANGE=xRange, $
                      YRANGE=yRange, $
                      XTITLE='$\kappa$', $
                      YTITLE=yHistoTitle, $
                      TITLE=titleStr, $
                      FONT_SIZE=16,THICK=2.5, $
                      CURRENT=winder)

     kLinePlot = PLOT(REPLICATE(2.45,11),FINDGEN(11)/10.*MAX(kHist)*2, $
                      YRANGE=yRange, $
                      THICK=2.,COLOR='GREEN',/OVERPLOT, $
                      CURRENT=winder)

     kText     = TEXT(0.25,0.8,"$\kappa_t$ = 2.45",/NORMAL, $
                      FONT_SIZE=16,FONT_COLOR='GREEN', $
                      TARGET=winder)

     outPlotName = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) $
                   + STRING(FORMAT='("-kappaStats_",A0,"-",A0,A0,A0,A0,".png")', $
                            mltStr,hemi,parmStr,kHBinSizeStr,bonusPlotSuff)
     PRINT,"Saving to " + outPlotName
     winder.Save,plotDir+outPlotName

     IF KEYWORD_SET(makeMetaStabPlot) THEN BEGIN

        ;; titleStr = STRING(FORMAT='(I02,"-",I02," ' + mltSuff + ', ",I0,"$^\circ$ < |ILAT| < ",I0,"$^\circ$!C!COrbits ",I0,"-",I0," (",I0,"/",I0," considered)")', $
        ;;                   minM+24,maxM, $
        ;;                   minI,maxI, $
        ;;                   MIN(orbArr),MAX(orbArr), $
        ;;                   N_ELEMENTS(orbArr),MAX(orbArr)-MIN(orbArr))
        titleStr = STRING(FORMAT='(I02,"-",I02," ' $
                          + mltSuff $
                          + ', ",I0,"$^\circ$ < |ILAT| < ",I0,"$^\circ$!C!COrbits ",I0,"-",I0)', $
                          minM+24,maxM, $
                          minI,maxI, $
                          MIN(orbArr),MAX(orbArr))
        winderM  = WINDOW(DIMENSIONS=[800,800],BUFFER=bufferPlots)

        xRange   = [0.,1.]
        yRange   = [0,MAX(mHist)*1.1]
        mPosition = [0.1,0.1,0.9,0.8]
        mHistPlot = PLOT(mBins,mHist,/HISTOGRAM, $
                         XRANGE=xRange, $
                         YRANGE=yRange, $
                         XTITLE='$M_q$', $
                         YTITLE=yHistoTitle, $
                         TITLE=titleStr, $
                         FONT_SIZE=16,THICK=2.5, $
                         CURRENT=winderM, $
                         POSITION=mPosition)
        mLinePlot = PLOT(REPLICATE(magicMVal,11), $
                         FINDGEN(11)/10.*MAX(mHist)*2, $
                         YRANGE=yRange, $
                         THICK=2., $
                         COLOR='GREEN', $
                         /OVERPLOT, $
                         CURRENT=winderM)
        mText     = TEXT(0.7,0.8,"$\kappa_t$ = 2.45", $
                         /NORMAL, $
                         FONT_SIZE=16, $
                         FONT_COLOR='GREEN', $
                         TARGET=winderM)

        MPlotName = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) $
                    + STRING(FORMAT='("-MetaStab_",A0,"-",A0,A0,A0,A0,".png")', $
                             mltStr, $
                             hemi, $
                             parmStr, $
                             mHBinSizeStr, $
                             bonusPlotSuff)

        PRINT,"Saving to " + MPlotName
        winderM.Save,plotDir+MPlotName

     ENDIF

     IF KEYWORD_SET(makeMLTILATplot) THEN BEGIN

        winder2 = WINDOW(DIMENSIONS=[800,800],BUFFER=bufferPlots)

        MLTILATplot = SCATTERPLOT(MLTs[final_i], $
                                  ABS(andre.ilat[final_i]), $
                                  XTITLE='mlt', $
                                  YTITLE='ilat', $
                                  TRANSP=50, $
                                  CURRENT=winder2)

        scatPlotName = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) $
                       + "-kS-" + mltSuff + "_ILAT_coverage" + "-" + hemi + parmStr + bonusPlotSuff + ".png"

        PRINT,"Saving to " + scatPlotName
        winder2.Save,plotDir+scatPlotName

     ENDIF  

     IF KEYWORD_SET(makeILATKappaplot) THEN BEGIN

        winder3 = WINDOW(DIMENSIONS=[800,800],BUFFER=bufferPlots)

        ILATkappaplot = SCATTERPLOT(ABS(andre.ilat[final_i]), $
                                    KF2DParms.kappa[plot_i], $
                                    XTITLE='ILAT (deg)', $
                                    YTITLE='Kappa', $
                                    YRANGE=[1.5,15], $
                                    TRANSP=50, $
                                    CURRENT=winder3)

        ilatPlotName = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) $
                       + "-kS-ILATkappa" $
                       + "-" + mltStr +altStr $
                       + "-" + hemi + parmStr + bonusPlotSuff + ".png"

        PRINT,"Saving to " + ilatPlotName
        winder3.Save,plotDir+ilatPlotName

     ENDIF  

  ENDIF ELSE BEGIN

     PRINT,FORMAT='("Working with ",I0, " RequInds")',nReq
     PRINT,FORMAT='("Working with ",I0, " ExclInds")',nExc

     AARName    = 'AAR' + STRING(9B) + STRING(9B) + STRING(9B) + STRING(9B) + STRING(FORMAT='("(N = ",I5,")")',nReq)
     belAARName = 'Below AAR'                                  + STRING(9B) + STRING(FORMAT='("(N = ",I5,")")',nExc)
     AARMedName = 'AAR' + STRING(9B) + STRING(9B) + STRING(9B) + STRING(9B) + "(Median)"
     belAARMedName = 'Below AAR'                               + STRING(9B) + "(Median)"

     ;; req_i          = WHERE(andre.ionBeam EQ 1 OR andre.ionBeam EQ 2,nReq)
     ;; exc_i          = WHERE(andre.ionBeam EQ 1 OR andre.ionBeam EQ 2,nExc)

     ;; Unique low kappa-ers
     ;; lowKappa_i        = WHERE(KF2DParms.kappa LE 2,nLowKappa,COMPLEMENT=notLowKappa_i,NCOMPLEMENT=nNotLowKappa)
     ;; lowKappa_i        = CGSETINTERSECTION(lowKappa_i,final_i,COUNT=nLowKappa)
     ;; lowKappaOrbs      = andre.orbit[lowkappa_i[UNIQ(andre.orbit[lowkappa_i],SORT(andre.orbit[lowkappa_i]))]]

     ;; plot_i            = final_i

     kHistReq = HISTOGRAM(KF2DParms.kappa[req_i], $
                       BINSIZE=kHBinSize, $
                       MIN=kHistMin, $
                       LOCATIONS=kBinsReq, $
                       REVERSE_INDICES=rKIndsReq)

     mHistReq = HISTOGRAM(MVals[req_i], $
                       BINSIZE=mHBinSize, $
                       MIN=MIN([mHistMin,checkM]), $
                       LOCATIONS=mBinsReq, $
                       REVERSE_INDICES=rMIndsReq)

     kHistExc = HISTOGRAM(KF2DParms.kappa[exc_i], $
                       BINSIZE=kHBinSize, $
                       MIN=kHistMin, $
                       LOCATIONS=kBinsExc, $
                       REVERSE_INDICES=rKIndsExc)

     mHistExc = HISTOGRAM(MVals[exc_i], $
                       BINSIZE=mHBinSize, $
                       MIN=MIN([mHistMin,checkM]), $
                       LOCATIONS=mBinsExc, $
                       REVERSE_INDICES=rMIndsExc)

     IF KEYWORD_SET(normed) THEN BEGIN
        kTotReq = TOTAL(kHistReq)
        mTotReq = TOTAL(mHistReq)

        kHistReq = FLOAT(kHistReq)/FLOAT(kTotReq)*100.
        mHistReq = FLOAT(mHistReq)/FLOAT(mTotReq)*100.

        kTotExc = TOTAL(kHistExc)
        mTotExc = TOTAL(mHistExc)

        kHistExc = FLOAT(kHistExc)/FLOAT(kTotExc)*100.
        mHistExc = FLOAT(mHistExc)/FLOAT(mTotExc)*100.

        parmStr += '-normed'
     ENDIF

     IF kBinsReq[0] LT 1.5 THEN kBinsReq[0] = 1.5
     IF kBinsExc[0] LT 1.5 THEN kBinsExc[0] = 1.5
     IF mBinsReq[0] LT 1.5 THEN mBinsReq[0] = 1.5
     IF mBinsExc[0] LT 1.5 THEN mBinsExc[0] = 1.5

     BelTransp = 85
     AARTransp = 65
     belAARCol = 'BLACK'
     AARCol    = 'RED'

     belAARSym = 'x'
     AARSym    = '+'

     ;; Where are they less than .245?
     ;; lt245inds = !NULL
     ;; bin245 = VALUE_CLOSEST2(kBins,2.45,/CONSTRAINED)
     ;; FOR k=0,bin245-1 DO BEGIN
     ;;    IF rKInds[k] NE rKInds[k+1] THEN $
     ;;       lt245inds = [lt245inds,rKInds[rKInds[k] : rKInds[k+1]-1]]
     ;; ENDFOR

     binI              = 4
     binMinforInclusion = 10
     ILATReqHS = HISTOGRAM_BINSTATS(ABS(andre.ilat[req_i]), $
                                    KF2DParms.kappa[req_i], $
                                    BINSIZE=binI, $
                                    MIN=minI, $
                                    MAX=maxI, $
                                    /NAN, $
                                    BINMINFORINCLUSION=binMinforInclusion)
     ILATExcHS = HISTOGRAM_BINSTATS(ABS(andre.ilat[exc_i]), $
                                    KF2DParms.kappa[exc_i], $
                                    BINSIZE=binI, $
                                    MIN=minI, $
                                    MAX=maxI, $
                                    /NAN, $
                                    BINMINFORINCLUSION=binMinforInclusion)
     ;; Kappa plot
     titleStr = STRING(FORMAT='(A0,", ",I0,"$^\circ$ < |ILAT| < ",I0,"$^\circ$!C!COrbits ",I0,"-",I0," (",I0,"/",I0," considered)")', $
                       mltStr, $
                       minI,maxI, $
                       MIN(orbArr),MAX(orbArr), $
                       N_ELEMENTS(orbArr),MAX(orbArr)-MIN(orbArr))

     IF KEYWORD_SET(makeKappaHistoPlot) THEN BEGIN
        winder   = WINDOW(DIMENSIONS=[800,800],BUFFER=bufferPlots)

        xRange   = [1.5,15]
        yRange   = [0,(MAX(kHistExc)>MAX(kHistReq))*1.1]
        kHistPlotReq = PLOT(kBinsReq,kHistReq,/HISTOGRAM, $
                            XRANGE=xRange, $
                            YRANGE=yRange, $
                            NAME=AARName, $
                            ;; LINESTYLE='
                            XTITLE='$\kappa$', $
                            YTITLE=yHistoTitle, $
                            TITLE=titleStr, $
                            FONT_SIZE=16,THICK=2.5, $
                            CURRENT=winder)

        kHistPlotExc = PLOT(kBinsExc,kHistExc,/HISTOGRAM, $
                            XRANGE=xRange, $
                            YRANGE=yRange, $
                            LINESTYLE='--', $
                            NAME=belAARName, $
                            COLOR='GRAY', $
                            THICK=2.5, $
                            /OVERPLOT, $
                            CURRENT=winder)

        histLegend  = LEGEND(TARGET=[kHistPlotReq,kHistPlotExc], $
                             /NORMAL, $
                             POSITION=[0.85,0.7])

        kLinePlot = PLOT(REPLICATE(2.45,11),FINDGEN(11)/10.*MAX(kHistReq)*2, $
                         YRANGE=yRange, $
                         THICK=2., $
                         COLOR=k245LineCol, $
                         /OVERPLOT, $
                         CURRENT=winder)

        kText     = TEXT(0.25,0.85,"$\kappa_t$ = 2.45", $
                         /NORMAL, $
                         FONT_SIZE=16, $
                         FONT_COLOR=k245LineCol, $
                         TARGET=winder)

        outPlotName = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) $
                      + STRING(FORMAT='("-kappaStats_",A0,A0,"-",A0,A0,A0,A0,".png")', $
                               mltStr,altStr,hemi,parmStr,kHBinSizeStr,bonusPlotSuff)
        PRINT,"Saving to " + outPlotName
        winder.Save,plotDir+outPlotName

     ENDIF

     IF KEYWORD_SET(makeMetaStabPlot) THEN BEGIN

        ;; Kappa plot
        titleStr = STRING(FORMAT='(A0,", ",I0,"$^\circ$ < |ILAT| < ",I0,"$^\circ$!C!COrbits ",I0,"-",I0," (",I0,"/",I0," considered)")', $
                          mltStr, $
                          minI,maxI, $
                          MIN(orbArr),MAX(orbArr), $
                          N_ELEMENTS(orbArr),MAX(orbArr)-MIN(orbArr))
        winderM  = WINDOW(DIMENSIONS=[800,800],BUFFER=bufferPlots)

        xRange   = [0.,1.]
        yRange   = [0,(MAX(mHistExc)>MAX(mHistReq))*1.1]
        mPosition = [0.1,0.1,0.9,0.8]
        mHistPlotReq = PLOT(mBinsReq,mHistReq, $
                            /HISTOGRAM, $
                            XRANGE=xRange, $
                            YRANGE=yRange, $
                            NAME=AARName, $
                            XTITLE='$M_q$', $
                            YTITLE=yHistoTitle, $
                            TITLE=titleStr, $
                            FONT_SIZE=16,THICK=2.5, $
                            CURRENT=winderM, $
                            POSITION=mPosition)

        mHistPlotExc = PLOT(mBinsExc,mHistExc,$
                            /HISTOGRAM, $
                            XRANGE=xRange, $
                            YRANGE=yRange, $
                            LINESTYLE='--', $
                            NAME=belAARName, $
                            COLOR='GRAY', $
                            THICK=2.5, $
                            CURRENT=winderM, $
                            /OVERPLOT, $
                            POSITION=mPosition)

        metaHistLegend  = LEGEND(TARGET=[mHistPlotReq,mHistPlotExc], $
                                 /NORMAL, $
                                 POSITION=[0.85,0.7])

        mLinePlot = PLOT(REPLICATE(magicMVal,11), $
                         FINDGEN(11)/10.*MAX(mHistReq)*2, $
                         YRANGE=yRange, $
                         THICK=2., $
                         COLOR=k245LineCol, $
                         /OVERPLOT, $
                         CURRENT=winderM)
        mText     = TEXT(0.7,0.72,"$\kappa_t$ = 2.45", $
                         /NORMAL, $
                         FONT_SIZE=16, $
                         FONT_COLOR=k245LineCol, $
                         TARGET=winderM)

        MPlotName = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) $
                    + STRING(FORMAT='("-MetaStab_",A0,A0,"-",A0,A0,A0,A0,".png")', $
                             mltStr, $
                             altStr, $
                             hemi, $
                             parmStr, $
                             mHBinSizeStr, $
                             bonusPlotSuff)

        PRINT,"Saving to " + MPlotName
        winderM.Save,plotDir+MPlotName

     ENDIF

     IF KEYWORD_SET(makeMLTILATplot) THEN BEGIN

        winder2 = WINDOW(DIMENSIONS=[800,800],BUFFER=bufferPlots)

        MLTILATplot1 = SCATTERPLOT(MLTs[exc_i], $
                                  ABS(andre.ilat[exc_i]), $
                                  XTITLE='mlt', $
                                  YTITLE='ilat', $
                                     SYM_COLOR=belAARCol, $
                                     SYMBOL=belAARSym, $
                                     NAME=belAARName, $
                                  TRANSP=BelTransp, $
                                  CURRENT=winder2)

        MLTILATplot2 = SCATTERPLOT(MLTs[req_i], $
                                  ABS(andre.ilat[req_i]), $
                                   SYM_COLOR=AARCol, $
                                   SYMBOL=AARSym, $
                                   NAME=AARName, $
                                  TRANSP=AARTransp, $
                                   CURRENT=winder2, $
                                  /OVERPLOT)

        scatPlotName = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) $
                       + "-kS-MLT_ILAT_coverage" + "-" + hemi + parmStr + bonusPlotSuff + ".png"

        PRINT,"Saving to " + scatPlotName
        winder2.Save,plotDir+scatPlotName

     ENDIF  

     IF KEYWORD_SET(makeILATKappaplot) THEN BEGIN

        winder3 = WINDOW(DIMENSIONS=[800,800],BUFFER=bufferPlots)

        ;; ILATRange = MINMAX(ABS(andre.ilat[exc_i]))
        ILATRange = [minI,maxI]
        ILATkappaXRange = [1.5,20]
        ILATkappaplot1 = SCATTERPLOT(KF2DParms.kappa[exc_i], $
                                     ABS(andre.ilat[exc_i]), $
                                     XTITLE='Kappa', $
                                     YTITLE='ILAT (deg)', $
                                     YRANGE=ILATRange, $
                                     SYM_COLOR=belAARCol, $
                                     SYMBOL=belAARSym, $
                                     NAME=belAARName, $
                                     XRANGE=ILATkappaXRange, $
                                     TRANSP=BelTransp, $
                                     CURRENT=winder3)

        ILATkappaplot2 = SCATTERPLOT(KF2DParms.kappa[req_i], $
                                     ABS(andre.ilat[req_i]), $
                                     YRANGE=ILATRange, $
                                     SYM_COLOR=AARCol, $
                                     SYMBOL=AARSym, $
                                     NAME=AARName, $
                                     XRANGE=ILATkappaXRange, $
                                     TRANSP=AARTransp, $
                                     /OVERPLOT, $
                                     CURRENT=winder3)

        bpdStuff = 1
        IF KEYWORD_SET(bpdStuff) THEN BEGIN
           ILATEx = ILATExcHS[*].bpd[2]
           ;; ILATEy = ILATExcHS.lEdge+binI/2.+binI/10.
           ILATEy = ILATExcHS.lEdge+binI/2.-0.25
           ILATEXerr = ABS(TRANSPOSE([[ILATExcHS[*].bpd[1]-ILATEx], $
                                  [ILATExcHS[*].bpd[3]-ILATEx]]))
           ILATEYerr = MAKE_ARRAY(N_ELEMENTS(ILATExcHS.stdDev),VALUE=0)

           ILATRx = ILATReqHS[*].bpd[2]
           ;; ILATRy = ILATReqHS.lEdge+binI/2.+binI/20.
           ILATRy = ILATReqHS.lEdge+binI/2.+0.25
           ILATRXerr = ABS(TRANSPOSE([[ILATReqHS[*].bpd[1]-ILATRx], $
                                      [ILATReqHS[*].bpd[3]-ILATRx]]))
           ILATRYerr = MAKE_ARRAY(N_ELEMENTS(ILATReqHS.stdDev),VALUE=0)
        ENDIF ELSE BEGIN
           ILATEx = ILATExcHS.mean
           ILATEy = ILATExcHS.lEdge+binI/2.+binI/20.
           ILATEXerr = ILATExcHS.stdDev
           ILATEYerr = MAKE_ARRAY(N_ELEMENTS(ILATExcHS.stdDev),VALUE=0)

           ILATRx = ILATReqHS.mean
           ILATRy = ILATReqHS.lEdge+binI/2.+binI/20.
           ILATRXerr = ILATReqHS.stdDev
           ILATRYerr = MAKE_ARRAY(N_ELEMENTS(ILATReqHS.stdDev),VALUE=0)
        ENDELSE

        ILATkappaEMedianPlot = ERRORPLOT(ILATEx, $
                                         ILATEy, $
                                         ILATEXerr, $
                                         ILATEYerr, $
                                         NAME=belAARMedName, $
                                         COLOR=belAARCol, $
                                         TRANSP=30, $
                                         THICK=2., $
                                         ERRORBAR_THICK=2., $
                                         SYMBOL=belAARSym, $
                                         SYM_THICK=2.0, $
                                         SYM_SIZE=1.5, $
                                         /OVERPLOT, $
                                         CURRENT=winder3)

        ILATkappaRMedianPlot = ERRORPLOT(ILATRx, $
                                         ILATRy, $
                                         ILATRXerr, $
                                         ILATRYerr, $
                                         NAME=AARMedName, $
                                         COLOR=AARCol, $
                                         TRANSP=30, $
                                         THICK=2., $
                                         ERRORBAR_THICK=2., $
                                         SYMBOL=AARSym, $
                                         SYM_THICK=2.0, $
                                         SYM_SIZE=1.5, $
                                         /OVERPLOT, $
                                         CURRENT=winder3)

        ILATKappaLegend  = LEGEND(TARGET=[ILATkappaplot1, $
                                          ILATkappaEMedianPlot, $
                                          ILATkappaplot2, $
                                          ILATkappaRMedianPlot], $
                                 /NORMAL, $
                                 POSITION=[0.85,0.8])

        ILATkLinePlot = PLOT(REPLICATE(2.45,11),FINDGEN(11)/10.*maxI, $
                             YRANGE=ILATRange, $
                             THICK=2., $
                             COLOR=k245LineCol, $
                             TRANSP=60, $
                             LINESTYLE='--', $
                             /OVERPLOT, $
                             CURRENT=winder3)

        kText     = TEXT(0.15,0.1,"$\kappa_t$ = 2.45", $
                         /NORMAL, $
                         FONT_SIZE=16, $
                         FONT_COLOR=k245LineCol, $
                         TARGET=winder3)

        ilatPlotName = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) $
                       + "-kS-ILATkappa" $
                       + "-" + mltStr + altStr $
                       + "-" + hemi + parmStr + bonusPlotSuff + ".png"

        PRINT,"Saving to " + ilatPlotName
        winder3.Save,plotDir+ilatPlotName

     ENDIF  

     IF KEYWORD_SET(makeMLTKappaplot) THEN BEGIN

        winder4 = WINDOW(DIMENSIONS=[800,800],BUFFER=bufferPlots)

        MLTkappaXRange = [1.5,20]
        MLTkappaplot1 = SCATTERPLOT(KF2DParms.kappa[exc_i], $
                                     MLTs[exc_i], $
                                     XTITLE='Kappa', $
                                     YTITLE='MLT', $
                                     SYM_COLOR=belAARCol, $
                                     SYMBOL=belAARSym, $
                                     NAME=belAARName, $
                                     XRANGE=MLTkappaXRange, $
                                     TRANSP=BelTransp, $
                                     CURRENT=winder4)

        MLTkappaplot2 = SCATTERPLOT(KF2DParms.kappa[req_i], $
                                     MLTs[req_i], $
                                     SYM_COLOR=AARCol, $
                                     SYMBOL=AARSym, $
                                     NAME=AARName, $
                                     XRANGE=MLTkappaXRange, $
                                     TRANSP=60, $
                                     /OVERPLOT, $
                                     CURRENT=winder4)

        mltPlotName = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) $
                       + "-kS-MLTkappa" + altStr + "-" + hemi + parmStr + bonusPlotSuff + ".png"

        PRINT,"Saving to " + mltPlotName
        winder4.Save,plotDir+mltPlotName

     ENDIF  

  ENDELSE

  IF ~KEYWORD_SET(bufferPlots) THEN STOP

  IF KEYWORD_SET(makeKappaHistoPlot) THEN BEGIN
     winder.Close
  ENDIF
  IF KEYWORD_SET(makeMetaStabPlot) THEN BEGIN
     winderM.Close
  ENDIF
  IF KEYWORD_SET(makeMLTILATplot) THEN BEGIN
     winder2.Close
  ENDIF
  IF KEYWORD_SET(makeILATkappaplot) THEN BEGIN
     winder3.Close
  ENDIF

  IF KEYWORD_SET(makeMLTkappaplot) THEN BEGIN
     winder4.Close
  ENDIF

END
