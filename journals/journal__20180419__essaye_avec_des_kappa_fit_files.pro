;2018/04/19
PRO J20180419__PRINT_DECILES, $
   histoStats,decileInd, $
   DATANAME=dataName, $
   BINSIZE=binSize

  dName = KEYWORD_SET(dataName) ? dataName   : "kchi2reddat"

  bHalf = KEYWORD_SET(binSize ) ? binSize/2. : (histoStats[1].ledge-histoStats[0].ledge)/2.

  PRINT,FORMAT='(A0,I1," = {")', $
        dataName, $
        decileInd+1

  FOR k=0,N_ELEMENTS(histoStats)-1 DO BEGIN
     IF FINITE(histoStats[k].decile.val[decileInd]) THEN BEGIN
        PRINT,FORMAT='("{",F0.3,",",G0.5,"}",A0)', $
              histoStats[k].ledge+bHalf, $
              histoStats[k].decile.val[decileInd], $
              (k EQ (N_ELEMENTS(histoStats)-1) ? '' : ',')
     ENDIF
  ENDFOR

  PRINT,'};'

END
PRO J20180419__PRINT_FOR_MATHEMATICA, $
   kHistReq,kHistExc, $
   kBinsReq,kBinsExc, $
   BINSIZE=kHBinSize

  names = ['kHistReq','kHistExc']
  hists = LIST(kHistReq,kHistExc)
  bins  = LIST(kBinsReq,kBinsExc)
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

     PRINT,'};'

  ENDFOR

END
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
   DSTCUTOFF=dstCutoff, $
   MAKEKAPPAHISTOPLOT=makeKappaHistoPlot, $
   HISTOTITLE__USE_GOVERK_DECILE_STRING=histoTitle__use_GoverK_decile_string, $
   MAKEMETASTABPLOT=makeMetaStabPlot, $
   MAKEMLTILATPLOT=makeMLTILATplot, $
   MAKEILATKAPPAPLOT=makeILATKappaplot, $
   MAKEMLTKAPPAPLOT=makeMLTKappaplot, $
   MAKEGOVERKVSKAPPAPLOT=makeGoverKvsKappaPlot, $
   MAKEGOVERK__LOG=GoverKLog, $
   MAKECHI2REDVSKAPPAPLOT=makeChi2RedvsKappaPlot, $
   MAKECHI2RED__LOG=chi2RedLog, $
   STATS__GIVE_DECILE=stats__give_decile, $
   STATS__GIVE_VENTILE=stats__give_ventile

  COMPILE_OPT IDL2,STRICTARRSUBS

  LOAD_KAPPAFIT_DB,andre, $
                   KF2DPARMS=KF2DParms, $
                   GF2DPARMS=GF2DParms, $
                   TOTT=totT, $
                   ORBARR=orbArr

  bonusPlotSuff = ''

  requireIons = N_ELEMENTS(require_ions) GT 0 ? require_ions : 0
  excludeIons = N_ELEMENTS(exclude_ions) GT 0 ? exclude_ions : 0

  legFontSize = 14
  fontSize = 20

  ;; makeKappaHistoPlot    = 1
  ;; makeMetaStabPlot      = 1
  ;; makeMLTILATplot        = 1
  ;; makeILATKappaplot      = 1
  ;; makeMLTKappaplot       = 1
  bufferPlots            = 1

  GoverKReq = N_ELEMENTS(GoverK  ) GT 0 ? GoverK   : 'decile=1'
  KChi2Max  = N_ELEMENTS(maxKChi2) GT 0 ? maxKChi2 : 4

  minM  = -3.5
  maxM  = 1.5
  ;; minM  = 20.5
  ;; maxM  = 23.5
  notMLT = 0
  minI  = 60
  maxI  = 90
  hemi  = 'NORTH'

  kHBinSize = N_ELEMENTS(kHist_binSize) GT 0 ? kHist_binSize : 0.75
  kHistMin  = N_ELEMENTS(kHist_min    ) GT 0 ? kHist_min     : 1.5

  mHBinSize = N_ELEMENTS(mHist_binSize) GT 0 ? mHist_binSize : 0.05
  mHistMin  = N_ELEMENTS(mHist_min    ) GT 0 ? mHist_min     : 0.


  CASE 1 OF
     SIZE(GoverK,/TYPE) EQ 7: BEGIN
        GoverKStr     = STRING(FORMAT='("-GKDec",I1)',LONG(STRMID(GoverKReq,7,1)))
     END
     ELSE: BEGIN
        GoverKStr     = (STRING(FORMAT='("-GoverK",F0.1)',GoverKReq)).Replace('.','_')
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

  SET_PLOT_DIR,plotDir,/FOR_KAPPA_DB,/ADD_TODAY

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
            NIGHTSIDE=nightside, $
            DSTCUTOFF=dstCutoff)

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

     ionSuff        = '-combo'
  ENDIF

  parmStr           = GoverKStr + KChi2MaxStr + ionSuff

  decileStr         = ''

  statName          = "Median"
  IF KEYWORD_SET(stats__give_decile) THEN BEGIN
     decileStr      = '-deciles'
     statName       = 'Deciles 1–3'
  ENDIF
  IF KEYWORD_SET(stats__give_ventile) THEN BEGIN
     decileStr      = '-ventiles'
     statName       = 'Ventiles 1, 3, 5'
  ENDIF
    
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

  IF KEYWORD_SET(makeGoverKvsKappaPlot) THEN BEGIN
     ratio = GF2DParms.chi2red/KF2DParms.chi2red
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

  BelTransp = 85
  AARTransp = 65
  belAARCol = 'BLUE'
  AARCol    = 'DARK ORANGE'
  k245LineCol = 'GREEN'

  medianstyle = 1
  medianTransp = 0
  bpdStuff = 1


  belAARSym = 'x'
  AARSym    = '+'

  yHistoTitle = KEYWORD_SET(normed) ? 'Percent' : 'Count'

  kappaPlotRange = [1.5,15]
  metaPlotRange  = [0.,1.]
        

  IF ~KEYWORD_SET(combined_histos) THEN BEGIN

     PRINT,FORMAT='("Working with ",I0, " inds")',count

     allName       = 'All' + STRING(9B) + STRING(FORMAT='("(N = ",I5,")")',count)
     allMedName    = 'All' + STRING(9B) + "(" + statName + ")"

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
     ;; titleStr = STRING(FORMAT='(A0,I0,"$^\circ$ < |ILAT| < ",I0,"$^\circ$!C!COrbits ",I0,"-",I0," (",I0,"/",I0," considered)")', $
     ;;                   mltStr, $
     ;;                   minI,maxI, $
     ;;                   MIN(orbArr),MAX(orbArr), $
     ;;                   N_ELEMENTS(orbArr),MAX(orbArr)-MIN(orbArr))

     IF KEYWORD_SET(makeKappaHistoPlot) THEN BEGIN

        titleStr = STRING(FORMAT='(A0,", ",I0,"$^\circ$ < |ILAT| < ",I0,"$^\circ$!C!COrbits ",I0,"-",I0)', $
                          mltStr, $
                          minI,maxI, $
                          MIN(orbArr),MAX(orbArr))
        IF KEYWORD_SET(histoTitle__use_GoverK_decile_string) THEN BEGIN
           
           titleStr = CARDINAL_TO_ORDINAL_STRING(LONG(STRMID(GoverKReq,7,1)),/TOUPCASE) + $
                      ' Decile'
        ENDIF

        winder   = WINDOW(DIMENSIONS=[800,800],BUFFER=bufferPlots)

        yRange   = [0,MAX(kHist)*1.1]
        kHistPlot = PLOT(kBins,kHist,/HISTOGRAM, $
                         XRANGE=kappaPlotRange, $
                         YRANGE=yRange, $
                         XTITLE='$\kappa$', $
                         YTITLE=yHistoTitle, $
                         TITLE=titleStr, $
                         FONT_SIZE=fontSize,THICK=2.5, $
                         CURRENT=winder)

        kLinePlot = PLOT(REPLICATE(2.45,11),FINDGEN(11)/10.*MAX(kHist)*2, $
                         YRANGE=yRange, $
                         THICK=2., $
                         COLOR=k245LineCol, $
                         LINESTYLE=':', $
                         /OVERPLOT, $
                         CURRENT=winder)

        kText     = TEXT(0.24,0.22,"$\kappa_t$ = 2.45", $
                         /NORMAL, $
                         FONT_SIZE=fontSize, $
                         FONT_COLOR=k245LineCol, $
                         TARGET=winder)

        outPlotName = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) $
                      + STRING(FORMAT='("-kappaStats_",A0,"-",A0,A0,A0,A0,".png")', $
                               mltStr,hemi,parmStr,kHBinSizeStr,bonusPlotSuff)
        PRINT,"Saving to " + outPlotName
        winder.Save,plotDir+outPlotName

     ENDIF
     
     IF KEYWORD_SET(makeMetaStabPlot) THEN BEGIN

        ;; titleStr = STRING(FORMAT='(A0,", ",I0,"$^\circ$ < |ILAT| < ",I0,"$^\circ$!C!COrbits ",I0,"-",I0," (",I0,"/",I0," considered)")', $
        ;;                   mltStr, $
        ;;                   minI,maxI, $
        ;;                   MIN(orbArr),MAX(orbArr), $
        ;;                   N_ELEMENTS(orbArr),MAX(orbArr)-MIN(orbArr))
        titleStr = STRING(FORMAT='(A0,", ",I0,"$^\circ$ < |ILAT| < ",I0,"$^\circ$!C!COrbits ",I0,"-",I0)', $
                          mltStr, $
                          minI,maxI, $
                          MIN(orbArr),MAX(orbArr))

        IF KEYWORD_SET(histoTitle__use_GoverK_decile_string) THEN BEGIN
           
           titleStr = CARDINAL_TO_ORDINAL_STRING(LONG(STRMID(GoverKReq,7,1)),/TOUPCASE) + $
                      ' Decile'
        ENDIF

        winderM  = WINDOW(DIMENSIONS=[800,800],BUFFER=bufferPlots)

        yRange   = [0,MAX(mHist)*1.1]
        mPosition = [0.1,0.1,0.9,0.8]
        mHistPlot = PLOT(mBins,mHist,/HISTOGRAM, $
                         XRANGE=metaPlotRange, $
                         YRANGE=yRange, $
                         XTITLE='$M_q$', $
                         YTITLE=yHistoTitle, $
                         TITLE=titleStr, $
                         FONT_SIZE=fontSize,THICK=2.5, $
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
                         FONT_SIZE=fontSize, $
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

        MLTILATplot = SCATTERPLOT(MLTs[plot_i], $
                                  ABS(andre.ilat[plot_i]), $
                                  XTITLE='mlt', $
                                  YTITLE='ilat', $
                                  TRANSP=50, $
                                  CURRENT=winder2)

        scatPlotName = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) $
                       + "-kS-" + mltStr + "_ILAT" + "-" + hemi + parmStr + bonusPlotSuff + ".png"

        PRINT,"Saving to " + scatPlotName
        winder2.Save,plotDir+scatPlotName

     ENDIF  

     IF KEYWORD_SET(makeILATKappaplot) THEN BEGIN

        winder3 = WINDOW(DIMENSIONS=[800,800],BUFFER=bufferPlots)

        ILATkappaplot = SCATTERPLOT(KF2DParms.kappa[plot_i], $
                                    ABS(andre.ilat[plot_i]), $
                                    NAME=allName, $
                                    XTITLE='Kappa', $
                                    YTITLE='ILAT (deg)', $
                                    SYM_COLOR=belAARCol, $
                                    SYMBOL=belAARSym, $                                    
                                    XRANGE=kappaPlotRange, $
                                    TRANSP=BelTransp, $
                                    CURRENT=winder3)

        IF KEYWORD_SET(medianstyle) THEN BEGIN

           binI              = 4
           NMinBinForInclusion = 10
           ILATHS = HISTOGRAM_BINSTATS( $
                    ABS(andre.ilat[plot_i]), $
                    KF2DParms.kappa[plot_i], $
                    BINSIZE=binI, $
                    MIN=minI, $
                    MAX=maxI, $
                    /NAN, $
                    NMINBINFORINCLUSION=NMinBinForInclusion, $
                    GIVE_DECILES=stats__give_decile, $
                    GIVE_VENTILES=stats__give_ventile)

           IF KEYWORD_SET(bpdStuff) THEN BEGIN
              ILATEy = ILATHS.lEdge+binI/2.-0.05
              ILATEyErr = MAKE_ARRAY(N_ELEMENTS(ILATHS.stdDev),VALUE=0)

              CASE 1 OF
                 KEYWORD_SET(stats__give_decile): BEGIN
                    ILATEx = ILATHS[*].decile.val[1]
                    ILATExErr = ABS(TRANSPOSE( $
                               [[ILATHS[*].decile.val[0]-ILATEx], $
                                [ILATHS[*].decile.val[2]-ILATEx]]))
                 END
                 KEYWORD_SET(stats__give_ventile): BEGIN
                    ILATEx = ILATHS[*].ventile.val[2]
                    ILATExErr = ABS(TRANSPOSE( $
                               [[ILATHS[*].ventile.val[0]-ILATEx], $
                                [ILATHS[*].ventile.val[4]-ILATEx]]))
                 END
                 ELSE: BEGIN
                    ILATEx = ILATHS[*].bpd[2]
                    ILATExErr = ABS(TRANSPOSE([[ILATHS[*].bpd[1]-ILATEx], $
                                              [ILATHS[*].bpd[3]-ILATEx]]))
                 END
              ENDCASE

           ENDIF ELSE BEGIN
              ILATEx = ILATHS.mean
              ILATEy = ILATHS.lEdge+binI/2.+binI/20.
              ILATExErr = ILATHS.stdDev
              ILATEyErr = MAKE_ARRAY(N_ELEMENTS(ILATHS.stdDev),VALUE=0)
           ENDELSE

           ILATkappaStatPlot = ERRORPLOT(ILATEx, $
                                         ILATEy, $
                                         ILATExErr, $
                                         ILATEyErr, $
                                         NAME=allMedName, $
                                         COLOR=belAARCol, $
                                         TRANSP=medianTransp, $
                                         THICK=2., $
                                         ERRORBAR_THICK=2., $
                                         SYMBOL=belAARSym, $
                                         SYM_THICK=2.0, $
                                         SYM_SIZE=1.5, $
                                         /OVERPLOT, $
                                         CURRENT=winder4)

           ILATKappaLegend  = LEGEND(TARGET=[ILATkappaplot, $
                                            ILATkappaStatPlot], $
                                    /NORMAL, $
                                    POSITION=[0.85,0.8])

           ILATkLinePlot = PLOT(REPLICATE(2.45,11),FINDGEN(11)/10.*(maxI-minI)+minI, $
                               YRANGE=ILATRange, $
                               THICK=2., $
                               COLOR=k245LineCol, $
                               TRANSP=35, $
                               LINESTYLE='--', $
                               /OVERPLOT, $
                               CURRENT=winder4)

           kText     = TEXT(0.15,0.1,"$\kappa_t$ = 2.45", $
                            /NORMAL, $
                            FONT_SIZE=fontSize, $
                            FONT_COLOR=k245LineCol, $
                            TARGET=winder4)

        ENDIF

        ilatPlotName = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) $
                       + "-kS-ILATkappa" $
                       + "-" + mltStr +altStr $
                       + "-" + hemi + parmStr + bonusPlotSuff + ".png"

        PRINT,"Saving to " + ilatPlotName
        winder3.Save,plotDir+ilatPlotName

     ENDIF  

     IF KEYWORD_SET(makeMLTKappaplot) THEN BEGIN

        winder4 = WINDOW(DIMENSIONS=[800,800],BUFFER=bufferPlots)

        MLTRange       = [minM-0.1,maxM+0.1]
        MLTkappaplot   = SCATTERPLOT(KF2DParms.kappa[plot_i], $
                                    MLTs[plot_i], $
                                    XTITLE='Kappa', $
                                    YTITLE='MLT', $
                                    SYM_COLOR=belAARCol, $
                                    SYMBOL=belAARSym, $
                                    NAME=belAARName, $
                                    XRANGE=kappaPlotRange, $
                                    YRANGE=MLTRange, $
                                    XMINOR=1, $
                                    YMINOR=3, $
                                    TRANSP=BelTransp, $
                                    CURRENT=winder4)

        IF KEYWORD_SET(medianstyle) THEN BEGIN

           histBinM           = 0.5
           NMinBinForInclusion = 10
           MLTHS = HISTOGRAM_BINSTATS( $
                      MLTs[plot_i], $
                      KF2DParms.kappa[plot_i], $
                      BINSIZE=histBinM, $
                      MIN=minM, $
                      MAX=maxM, $
                      /NAN, $
                      NMINBINFORINCLUSION=NMinBinForInclusion, $
                      GIVE_DECILES=stats__give_decile, $
                      GIVE_VENTILES=stats__give_ventile)

           IF KEYWORD_SET(bpdStuff) THEN BEGIN
              MLTEy = MLTHS.lEdge+histBinM/2.-0.05
              MLTEyErr = MAKE_ARRAY(N_ELEMENTS(MLTHS.stdDev),VALUE=0)

              CASE 1 OF
                 KEYWORD_SET(stats__give_decile): BEGIN
                    deciles = [0,1,2]

                    MLTEx = MLTHS[*].decile.val[deciles[1]]
                    MLTExErr = ABS(TRANSPOSE( $
                               [[MLTHS[*].decile.val[deciles[0]]-MLTEx], $
                                [MLTHS[*].decile.val[deciles[2]]-MLTEx]]))
                 END
                 KEYWORD_SET(stats__give_ventile): BEGIN
                    ventiles = [0,2,4]

                    MLTEx = MLTHS[*].ventile.val[deciles[1]]
                    MLTExErr = ABS(TRANSPOSE( $
                               [[MLTHS[*].ventile.val[deciles[0]]-MLTEx], $
                                [MLTHS[*].ventile.val[deciles[2]]-MLTEx]]))
                 END
                 ELSE: BEGIN
                    MLTEx = MLTHS[*].bpd[2]
                    MLTExErr = ABS(TRANSPOSE([[MLTHS[*].bpd[1]-MLTEx], $
                                              [MLTHS[*].bpd[3]-MLTEx]]))
                 END
              ENDCASE

           ENDIF ELSE BEGIN
              MLTEx = MLTHS.mean
              MLTEy = MLTHS.lEdge+histBinM/2.+histBinM/20.
              MLTExErr = MLTHS.stdDev
              MLTEyErr = MAKE_ARRAY(N_ELEMENTS(MLTHS.stdDev),VALUE=0)
           ENDELSE

           MLTkappaStatPlot = ERRORPLOT(MLTEx, $
                                        MLTEy, $
                                        MLTExErr, $
                                        MLTEyErr, $
                                        NAME=belAARMedName, $
                                        COLOR=belAARCol, $
                                        TRANSP=medianTransp, $
                                        THICK=2., $
                                        ERRORBAR_THICK=2., $
                                        SYMBOL=belAARSym, $
                                        SYM_THICK=2.0, $
                                        SYM_SIZE=1.5, $
                                        /OVERPLOT, $
                                        CURRENT=winder4)

           MLTKappaLegend  = LEGEND(TARGET=[MLTkappaplot, $
                                            MLTkappaStatPlot], $
                                    /NORMAL, $
                                    POSITION=[0.85,0.8])

           MLTkLinePlot = PLOT(REPLICATE(2.45,11),FINDGEN(11)/10.*(maxM-minM)+minM, $
                                YRANGE=MLTRange, $
                                THICK=2., $
                                COLOR=k245LineCol, $
                                TRANSP=35, $
                                LINESTYLE='--', $
                                /OVERPLOT, $
                                CURRENT=winder4)

           kText     = TEXT(0.15,0.1,"$\kappa_t$ = 2.45", $
                            /NORMAL, $
                            FONT_SIZE=fontSize, $
                            FONT_COLOR=k245LineCol, $
                            TARGET=winder4)

        ENDIF

        mltPlotName = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) $
                      + "-kS-MLTkappa" + altStr + "-" + $
                      hemi + parmStr + decileStr + bonusPlotSuff + ".png"

        PRINT,"Saving to " + mltPlotName
        winder4.Save,plotDir+mltPlotName


     ENDIF

     IF KEYWORD_SET(makeGoverKvsKappaPlot) THEN BEGIN

        winder5 = WINDOW(DIMENSIONS=[800,800],BUFFER=bufferPlots)

        GoverKchi2Range       = KEYWORD_SET(GoverKLog) ? [0.9,MAX(ratio)] : [0.5,7]

        medianStyle = 1
        addDecileLine = 0

        ;; tmpColor = KEYWORD_SET(addDecileLine) ? 'Orange' : belAARCol
        tmpColor = 'Orange' 

        GoverKchi2Kappaplot   = SCATTERPLOT(KF2DParms.kappa[plot_i], $
                                            ratio[plot_i], $
                                            XTITLE='Kappa', $
                                            YTITLE='G over K', $
                                            YLOG=GoverKLog, $
                                            SYM_COLOR=tmpColor, $
                                            SYMBOL=belAARSym, $
                                            NAME=allName, $
                                            XRANGE=kappaPlotRange, $
                                            YRANGE=GoverKchi2Range, $
                                            TRANSP=90, $
                                            CURRENT=winder5)

        IF KEYWORD_SET(medianstyle) THEN BEGIN

           binK               = 0.2
           NMinBinForInclusion = 10

           GoverKHS = HISTOGRAM_BINSTATS( $
                      KF2DParms.kappa[plot_i], $
                      ratio[plot_i], $
                      BINSIZE=binK, $
                      MIN=minK, $
                      MAX=maxK, $
                      /NAN, $
                      NMINBINFORINCLUSION=NMinBinForInclusion, $
                      GIVE_DECILES=stats__give_decile, $
                      GIVE_VENTILES=stats__give_ventile)

           IF KEYWORD_SET(GoverK__print_deciles) THEN BEGIN
              ;; decileInds = [0,1,2]
              ;; decileInds = [3,4,5]
              ;; decileInds = [6,7,8]
              decileInds = [0,1,2,3,4,5,6,7,8]
              FOR jjj=0,N_ELEMENTS(decileInds)-1 DO BEGIN
                 J20180419__PRINT_DECILES,GoverKHS,decileInds[jjj], $
                                          DATANAME="goverkdat", $
                                          BINSIZE=binK
              ENDFOR
           ENDIF
           
           IF KEYWORD_SET(bpdStuff) THEN BEGIN
              GoverKx = GoverKHS.lEdge+binK/2.-0.05
              GoverKxErr = MAKE_ARRAY(N_ELEMENTS(GoverKHS.stdDev),VALUE=0)

              CASE 1 OF
                 KEYWORD_SET(stats__give_decile): BEGIN
                    deciles = [0,1,2]
                    deciles = [3,4,5]

                    GoverKy = GoverKHS[*].decile.val[deciles[1]]
                    GoverKyErr = ABS(TRANSPOSE( $
                                 [[GoverKHS[*].decile.val[deciles[0]]-GoverKy], $
                                  [GoverKHS[*].decile.val[deciles[2]]-GoverKy]]))
                 END
                 KEYWORD_SET(stats__give_ventile): BEGIN
                    GoverKy = GoverKHS[*].ventile.val[2]
                    GoverKyErr = ABS(TRANSPOSE( $
                                  [[GoverKHS[*].ventile.val[0]-GoverKy], $
                                   [GoverKHS[*].ventile.val[4]-GoverKy]]))
                 END
                 ELSE: BEGIN
                    GoverKy = GoverKHS[*].bpd[2]
                    GoverKyErr = ABS(TRANSPOSE([[GoverKHS[*].bpd[1]-GoverKy], $
                                                 [GoverKHS[*].bpd[3]-GoverKy]]))

                 END
              ENDCASE

           ENDIF ELSE BEGIN
              GoverKy = GoverKHS.mean
              GoverKx = GoverKHS.lEdge+binK/2.+binK/20.
              GoverKyErr = GoverKHS.stdDev
              GoverKxErr = MAKE_ARRAY(N_ELEMENTS(GoverKHS.stdDev),VALUE=0)
           ENDELSE

           GoverKchi2KappaStatPlot = ERRORPLOT(GoverKx, $
                                               GoverKy, $
                                               GoverKxErr, $
                                               GoverKyErr, $
                                               NAME=allMedName, $
                                               COLOR='Dark Slate Gray', $
                                               ;; COLOR='Black', $
                                               TRANSP=medianTransp, $
                                               THICK=2., $
                                               ERRORBAR_THICK=2., $
                                               SYMBOL=belAARSym, $
                                               SYM_THICK=2.0, $
                                               SYM_SIZE=1.5, $
                                               /OVERPLOT, $
                                               CURRENT=winder5)

           GoverKKappaLegend  = LEGEND(TARGET=[GoverKchi2Kappaplot, $
                                               GoverKchi2KappaStatPlot], $
                                       /NORMAL, $
                                       POSITION=[0.85,0.8])

           ;; GoverKkLinePlot = PLOT(REPLICATE(2.45,11),FINDGEN(11)/10.*(maxK-minK)+minK, $
           ;;                      YRANGE=GoverKchi2Range, $
           ;;                      THICK=2., $
           ;;                      COLOR=k245LineCol, $
           ;;                      TRANSP=35, $
           ;;                      LINESTYLE='--', $
           ;;                      /OVERPLOT, $
           ;;                      CURRENT=winder5)

           ;; kText     = TEXT(0.15,0.1,"$\kappa_t$ = 2.45", $
           ;;                  /NORMAL, $
           ;;                  FONT_SIZE=fontSize, $
           ;;                  FONT_COLOR=k245LineCol, $
           ;;                  TARGET=winder5)

        ENDIF

        IF KEYWORD_SET(addDecileLine) THEN BEGIN
           kVals = FINDGEN(160)/8.+1.5

           ;; decilePlot1 = PLOT(kVals, $
           ;;                    GOVERK_CHI2FUNC(kVals, $
           ;;                                    DECILE='decile=1'), $
           ;;                    NAME='1!Ust!N Decile', $
           ;;                    LINESTYLE='--', $
           ;;                    THICK=2., $
           ;;                    COLOR='Black', $
           ;;                    /OVERPLOT, $
           ;;                    CURRENT=winder5)
           ;; decilePlot2 = PLOT(kVals, $
           ;;                    GOVERK_CHI2FUNC(kVals, $
           ;;                                    DECILE='decile=2'), $
           ;;                    NAME='2!Und!N Decile', $
           ;;                    ;; LINESTYLE='--', $
           ;;                    THICK=2., $
           ;;                    COLOR='Black', $
           ;;                    /OVERPLOT, $
           ;;                    CURRENT=winder5)

           ;; decilePlot3 = PLOT(kVals, $
           ;;                    GOVERK_CHI2FUNC(kVals, $
           ;;                                    DECILE='decile=3'), $
           ;;                    NAME='3!Urd!N Decile', $
           ;;                    LINESTYLE=':', $
           ;;                    THICK=2., $
           ;;                    COLOR='Black', $
           ;;                    /OVERPLOT, $
           ;;                    CURRENT=winder5)

           decilePlot4 = PLOT(kVals, $
                              GOVERK_CHI2FUNC(kVals, $
                                              DECILE='decile=4'), $
                              NAME='4!Uth!N Decile', $
                              LINESTYLE='--', $
                              THICK=2., $
                              COLOR='Dark Slate Gray', $
                              /OVERPLOT, $
                              CURRENT=winder5)
           decilePlot5 = PLOT(kVals, $
                              GOVERK_CHI2FUNC(kVals, $
                                              DECILE='decile=5'), $
                              NAME='5!Uth!N Decile', $
                              ;; LINESTYLE='--', $
                              THICK=2., $
                              COLOR='Dark Slate Gray', $
                              /OVERPLOT, $
                              CURRENT=winder5)

           decilePlot6 = PLOT(kVals, $
                              GOVERK_CHI2FUNC(kVals, $
                                              DECILE='decile=6'), $
                              NAME='6!Uth!N Decile', $
                              LINESTYLE=':', $
                              THICK=2., $
                              COLOR='Dark Slate Gray', $
                              /OVERPLOT, $
                              CURRENT=winder5)

           ;; decilePlot7 = PLOT(kVals, $
           ;;                    GOVERK_CHI2FUNC(kVals, $
           ;;                                    DECILE='decile=7'), $
           ;;                    NAME='7!Uth!N Decile', $
           ;;                    LINESTYLE='--', $
           ;;                    THICK=2., $
           ;;                    COLOR='Dark Slate Blue', $
           ;;                    /OVERPLOT, $
           ;;                    CURRENT=winder5)
           ;; decilePlot8 = PLOT(kVals, $
           ;;                    GOVERK_CHI2FUNC(kVals, $
           ;;                                    DECILE='decile=8'), $
           ;;                    NAME='8!Uth!N Decile', $
           ;;                    ;; LINESTYLE='--', $
           ;;                    THICK=2., $
           ;;                    COLOR='Dark Slate Blue', $
           ;;                    /OVERPLOT, $
           ;;                    CURRENT=winder5)

           ;; decilePlot9 = PLOT(kVals, $
           ;;                    GOVERK_CHI2FUNC(kVals, $
           ;;                                    DECILE='decile=9'), $
           ;;                    NAME='9!Uth!N Decile', $
           ;;                    LINESTYLE=':', $
           ;;                    THICK=2., $
           ;;                    COLOR='Dark Slate Blue', $
           ;;                    /OVERPLOT, $
           ;;                    CURRENT=winder5)

           legend = LEGEND(TARGET=[ $;; decilePlot9,decilePlot8,decilePlot7, $
                                   decilePlot6,decilePlot5,decilePlot4], $ ;, $
                                   ;; decilePlot3,decilePlot2,decilePlot1], $
                           /NORMAL, $
                           POSITION=[0.85,0.8])

        ENDIF

        GoverKPlotName = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) $
                         + "-kS-GoverKchi2vsKappa" + altStr + "-" + $
                         (KEYWORD_SET(GoverKLog) ? 'log-' : '') + $
                          ;; hemi + parmStr + decileStr + bonusPlotSuff + ".png"
                          hemi + parmStr + bonusPlotSuff + ".png"

        PRINT,"Saving to " + GoverKPlotName
        winder5.Save,plotDir+GoverKPlotName

     ENDIF

     IF KEYWORD_SET(makeChi2RedvsKappaPlot) THEN BEGIN

        winder6 = WINDOW(DIMENSIONS=[800,800],BUFFER=bufferPlots)

        chi2Deciles        = [6,7,8]

        chi2MedName        = 'All' + STRING(9B) + "(Deciles " + $
                             STRING(FORMAT='(3(I1,:,", "))',chi2Deciles+1) + ")"
        
        chi2RedRange       = KEYWORD_SET(chi2RedLog)      ? $
                             ;; [0.1,MAX(KF2DParms.chi2Red)] : $
                             [0.1,200] : $
                             [0.1,11]
        chi2RedKappaplot   = SCATTERPLOT(KF2DParms.kappa[plot_i], $
                                         KF2DParms.chi2Red[plot_i], $
                                         XTITLE='Kappa', $
                                         YTITLE='$\chi^2_{red}$', $
                                         YLOG=chi2RedLog, $
                                         SYM_COLOR=belAARCol, $
                                         SYMBOL=belAARSym, $
                                         NAME=allName, $
                                         XRANGE=kappaPlotRange, $
                                         YRANGE=chi2RedRange, $
                                         TRANSP=BelTransp, $
                                         CURRENT=winder6)

        IF KEYWORD_SET(medianstyle) THEN BEGIN

           binK              = 0.25
           NMinBinForInclusion = 10
           chi2RedHS = HISTOGRAM_BINSTATS( $
                       KF2DParms.kappa[plot_i], $
                       KF2DParms.chi2Red[plot_i], $
                       BINSIZE=binK, $
                       MIN=minK, $
                       MAX=maxK, $
                       /NAN, $
                       NMINBINFORINCLUSION=NMinBinForInclusion, $
                       GIVE_DECILES=stats__give_decile, $
                       GIVE_VENTILES=stats__give_ventile)

           ;; Print stats?
           ;; decileInds = [3,4,5]
           ;; FOR jjj=0,N_ELEMENTS(decileInds)-1 DO BEGIN
           ;;    J20180419__PRINT_DECILES,chi2RedHS,decileInds[jjj], $
           ;;                             DATANAME="kchi2reddat", $
           ;;                             BINSIZE=binK
           ;; ENDFOR

           IF KEYWORD_SET(bpdStuff) THEN BEGIN
              chi2Redx = chi2RedHS.lEdge+binK/2.-0.05
              chi2RedxErr = MAKE_ARRAY(N_ELEMENTS(chi2RedHS.stdDev),VALUE=0)

              CASE 1 OF
                 KEYWORD_SET(stats__give_decile): BEGIN
                    chi2Redy = chi2RedHS[*].decile.val[chi2Deciles[1]]
                    chi2RedyErr = ABS(TRANSPOSE( $
                                   [[chi2RedHS[*].decile.val[chi2Deciles[0]]-chi2Redy], $
                                    [chi2RedHS[*].decile.val[chi2Deciles[2]]-chi2Redy]]))
                 END
                 KEYWORD_SET(stats__give_ventile): BEGIN
                    chi2Redy = chi2RedHS[*].ventile.val[2]
                    chi2RedyErr = ABS(TRANSPOSE( $
                                   [[chi2RedHS[*].ventile.val[0]-chi2Redy], $
                                    [chi2RedHS[*].ventile.val[4]-chi2Redy]]))
                 END
                 ELSE: BEGIN
                    chi2Redy = chi2RedHS[*].bpd[2]
                    chi2RedyErr = ABS(TRANSPOSE([[chi2RedHS[*].bpd[1]-chi2Redy], $
                                                  [chi2RedHS[*].bpd[3]-chi2Redy]]))

                 END
              ENDCASE

           ENDIF ELSE BEGIN
              chi2Redy = chi2RedHS.mean
              chi2Redx = chi2RedHS.lEdge+binK/2.+binK/20.
              chi2RedyErr = chi2RedHS.stdDev
              chi2RedxErr = MAKE_ARRAY(N_ELEMENTS(chi2RedHS.stdDev),VALUE=0)
           ENDELSE

           chi2RedKappaStatPlot = ERRORPLOT(chi2Redx, $
                                            chi2Redy, $
                                            chi2RedxErr, $
                                            chi2RedyErr, $
                                            NAME=chi2MedName, $
                                            COLOR=belAARCol, $
                                            TRANSP=medianTransp, $
                                            THICK=2., $
                                            ERRORBAR_THICK=2., $
                                            SYMBOL=belAARSym, $
                                            SYM_THICK=2.0, $
                                            SYM_SIZE=1.5, $
                                            /OVERPLOT, $
                                            CURRENT=winder6)

           chi2RedKappaLegend  = LEGEND(TARGET=[chi2RedKappaplot, $
                                                chi2RedKappaStatPlot], $
                                        /NORMAL, $
                                        POSITION=[0.85,0.8])

        ENDIF

        chi2RedPlotName = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) $
                         + "-kS-chi2RedvsKappa" + altStr + "-" + $
                         (KEYWORD_SET(chi2RedLog) ? 'log-' : '') + $
                          ;; hemi + parmStr + decileStr + bonusPlotSuff + ".png"
                          hemi + parmStr + bonusPlotSuff + ".png"

        PRINT,"Saving to " + chi2RedPlotName
        winder6.Save,plotDir+chi2RedPlotName

     ENDIF

  ENDIF ELSE BEGIN

     PRINT,FORMAT='("Working with ",I0, " RequInds")',nReq
     PRINT,FORMAT='("Working with ",I0, " ExclInds")',nExc

     AARName       = 'AAR' + STRING(9B) + STRING(9B) + STRING(9B) $
                     + STRING(9B) + STRING(FORMAT='("(N = ",I5,")")',nReq)
     belAARName    = 'Below AAR'                                  $
                     + STRING(9B) + STRING(FORMAT='("(N = ",I5,")")',nExc)
     AARMedName    = 'AAR' + STRING(9B) + STRING(9B) + STRING(9B) $
                     + STRING(9B) + "(" + statName + ")"
     belAARMedName = 'Below AAR'                               $
                     + STRING(9B) + "(" + statName + ")"

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

  ;; Print for mathematica???
     J20180419__PRINT_FOR_MATHEMATICA, $
        kHistReq,kHistExc, $
        kBinsReq,kBinsExc, $
        BINSIZE=kHBinSize

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

     ;; Kappa plot
     titleStr = STRING(FORMAT='(A0,", ",I0,"$^\circ$ < |ILAT| < ",I0,"$^\circ$!C!COrbits ",I0,"-",I0," (",I0,"/",I0," considered)")', $
                       mltStr, $
                       minI,maxI, $
                       MIN(orbArr),MAX(orbArr), $
                       N_ELEMENTS(orbArr),MAX(orbArr)-MIN(orbArr))

     IF KEYWORD_SET(histoTitle__use_GoverK_decile_string) THEN BEGIN
        
        titleStr = CARDINAL_TO_ORDINAL_STRING(LONG(STRMID(GoverKReq,7,1)),/TOUPCASE) + $
                   ' Decile'
     ENDIF

     IF KEYWORD_SET(makeKappaHistoPlot) THEN BEGIN
        winder   = WINDOW(DIMENSIONS=[800,800],BUFFER=bufferPlots)

        yRange   = [0,(MAX(kHistExc)>MAX(kHistReq))*1.1]
        kHistPlotReq = PLOT(kBinsReq,kHistReq,/HISTOGRAM, $
                            XRANGE=kappaPlotRange, $
                            YRANGE=yRange, $
                            NAME=AARName, $
                            ;; LINESTYLE='
                            XTITLE='$\kappa$', $
                            YTITLE=yHistoTitle, $
                            TITLE=titleStr, $
                            FONT_SIZE=fontSize,THICK=2.5, $
                            CURRENT=winder)

        kHistPlotExc = PLOT(kBinsExc,kHistExc,/HISTOGRAM, $
                            XRANGE=kappaPlotRange, $
                            YRANGE=yRange, $
                            LINESTYLE='--', $
                            NAME=belAARName, $
                            COLOR='GRAY', $
                            THICK=2.5, $
                            /OVERPLOT, $
                            CURRENT=winder)

        histLegend  = LEGEND(TARGET=[kHistPlotReq,kHistPlotExc], $
                             /NORMAL, $
                             FONT_SIZE=legFontSize, $
                             POSITION=[0.85,0.7])

        kLinePlot = PLOT(REPLICATE(2.45,11),FINDGEN(11)/10.*MAX(kHistReq)*2, $
                         YRANGE=yRange, $
                         THICK=2., $
                         LINESTYLE=':', $
                         COLOR=k245LineCol, $
                         /OVERPLOT, $
                         CURRENT=winder)

        kText     = TEXT(0.24,0.22,"$\kappa_t$ = 2.45", $
                         /NORMAL, $
                         FONT_SIZE=fontSize, $
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
                            XRANGE=metaPlotRange, $
                            YRANGE=yRange, $
                            NAME=AARName, $
                            XTITLE='$M_q$', $
                            YTITLE=yHistoTitle, $
                            TITLE=titleStr, $
                            FONT_SIZE=fontSize,THICK=2.5, $
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
                                 FONT_SIZE=legFontSize, $
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
                         FONT_SIZE=fontSize, $
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
        ILATkappaplot1 = SCATTERPLOT(KF2DParms.kappa[exc_i], $
                                     ABS(andre.ilat[exc_i]), $
                                     XTITLE='Kappa', $
                                     YTITLE='ILAT (deg)', $
                                     XRANGE=kappaPlotRange, $
                                     YRANGE=ILATRange, $
                                     SYM_COLOR=belAARCol, $
                                     SYMBOL=belAARSym, $
                                     NAME=belAARName, $
                                     TRANSP=BelTransp, $
                                     CURRENT=winder3)

        ILATkappaplot2 = SCATTERPLOT(KF2DParms.kappa[req_i], $
                                     ABS(andre.ilat[req_i]), $
                                     XRANGE=kappaPlotRange, $
                                     YRANGE=ILATRange, $
                                     SYM_COLOR=AARCol, $
                                     SYMBOL=AARSym, $
                                     NAME=AARName, $
                                     TRANSP=AARTransp, $
                                     /OVERPLOT, $
                                     CURRENT=winder3)

        IF KEYWORD_SET(bpdStuff) THEN BEGIN

           binI               = 5
           NMinBinForInclusion = 20
           ILATReqHS = HISTOGRAM_BINSTATS( $
                       ABS(andre.ilat[req_i]), $
                       KF2DParms.kappa[req_i], $
                       BINSIZE=binI, $
                       MIN=minI, $
                       MAX=maxI, $
                       /NAN, $
                       NMINBINFORINCLUSION=NMinBinForInclusion, $
                       GIVE_DECILES=stats__give_decile, $
                       GIVE_VENTILES=stats__give_ventile)
           ILATExcHS = HISTOGRAM_BINSTATS( $
                       ABS(andre.ilat[exc_i]), $
                       KF2DParms.kappa[exc_i], $
                       BINSIZE=binI, $
                       MIN=minI, $
                       MAX=maxI, $
                       /NAN, $
                       NMINBINFORINCLUSION=NMinBinForInclusion, $
                       GIVE_DECILES=stats__give_decile, $
                       GIVE_VENTILES=stats__give_ventile)

           ;; ILATEy = ILATExcHS.lEdge+binI/2.+binI/10.
           ILATEy = ILATExcHS.lEdge+binI/2.-0.25
           ILATEyErr = MAKE_ARRAY(N_ELEMENTS(ILATExcHS.stdDev),VALUE=0)
           ;; ILATRy = ILATReqHS.lEdge+binI/2.+binI/20.
           ILATRy = ILATReqHS.lEdge+binI/2.+0.25
           ILATRyErr = MAKE_ARRAY(N_ELEMENTS(ILATReqHS.stdDev),VALUE=0)

              CASE 1 OF
                 KEYWORD_SET(stats__give_decile): BEGIN
                    deciles = [0,1,2]

                    ILATEx = ILATExcHS[*].decile.val[deciles[1]]
                    ILATExErr = ABS(TRANSPOSE( $
                               [[ILATExcHS[*].decile.val[deciles[0]]-ILATEx], $
                                [ILATExcHS[*].decile.val[deciles[2]]-ILATEx]]))

                    ILATRx = ILATReqHS[*].decile.val[deciles[1]]
                    ILATRxErr = ABS(TRANSPOSE( $
                               [[ILATReqHS[*].decile.val[deciles[0]]-ILATRx], $
                                [ILATReqHS[*].decile.val[deciles[2]]-ILATRx]]))

                 END
                 KEYWORD_SET(stats__give_ventile): BEGIN
                    ILATEx = ILATExcHS[*].ventile.val[2]
                    ILATExErr = ABS(TRANSPOSE( $
                               [[ILATExcHS[*].ventile.val[0]-ILATEx], $
                                [ILATExcHS[*].ventile.val[4]-ILATEx]]))

                    ILATRx = ILATReqHS[*].ventile.val[2]
                    ILATRxErr = ABS(TRANSPOSE( $
                               [[ILATReqHS[*].ventile.val[0]-ILATRx], $
                                [ILATReqHS[*].ventile.val[4]-ILATRx]]))

                 END
                 ELSE: BEGIN
                    ILATEx = ILATExcHS[*].bpd[2]
                    ILATExErr = ABS(TRANSPOSE([[ILATExcHS[*].bpd[1]-ILATEx], $
                                              [ILATExcHS[*].bpd[3]-ILATEx]]))

                    ILATRx = ILATReqHS[*].bpd[2]
                    ILATRxErr = ABS(TRANSPOSE([[ILATReqHS[*].bpd[1]-ILATRx], $
                                              [ILATReqHS[*].bpd[3]-ILATRx]]))
                 END
              ENDCASE

           ;; ILATEx = ILATExcHS[*].bpd[2]
           ;; ;; ILATEy = ILATExcHS.lEdge+binI/2.+binI/10.
           ;; ILATEy = ILATExcHS.lEdge+binI/2.-0.25
           ;; ILATExErr = ABS(TRANSPOSE([[ILATExcHS[*].bpd[1]-ILATEx], $
           ;;                        [ILATExcHS[*].bpd[3]-ILATEx]]))
           ;; ILATEyErr = MAKE_ARRAY(N_ELEMENTS(ILATExcHS.stdDev),VALUE=0)

              ;; ILATRx = ILATReqHS[*].bpd[2]
              ;; ;; ILATRy = ILATReqHS.lEdge+binI/2.+binI/20.
              ;; ILATRy = ILATReqHS.lEdge+binI/2.+0.25
              ;; ILATRxErr = ABS(TRANSPOSE([[ILATReqHS[*].bpd[1]-ILATRx], $
              ;;                            [ILATReqHS[*].bpd[3]-ILATRx]]))
              ;; ILATRyErr = MAKE_ARRAY(N_ELEMENTS(ILATReqHS.stdDev),VALUE=0)

           ENDIF ELSE BEGIN
              ILATEx = ILATExcHS.mean
              ILATEy = ILATExcHS.lEdge+binI/2.+binI/20.
              ILATExErr = ILATExcHS.stdDev
              ILATEyErr = MAKE_ARRAY(N_ELEMENTS(ILATExcHS.stdDev),VALUE=0)

              ILATRx = ILATReqHS.mean
              ILATRy = ILATReqHS.lEdge+binI/2.+binI/20.
              ILATRxErr = ILATReqHS.stdDev
              ILATRyErr = MAKE_ARRAY(N_ELEMENTS(ILATReqHS.stdDev),VALUE=0)
           ENDELSE

           ILATkappaEStatPlot = ERRORPLOT(ILATEx, $
                                          ILATEy, $
                                          ILATExErr, $
                                          ILATEyErr, $
                                          NAME=belAARMedName, $
                                          COLOR=belAARCol, $
                                          TRANSP=medianTransp, $
                                          THICK=2., $
                                          ERRORBAR_THICK=2., $
                                          SYMBOL=belAARSym, $
                                          SYM_THICK=2.0, $
                                          SYM_SIZE=1.5, $
                                          /OVERPLOT, $
                                          CURRENT=winder3)

           ILATkappaRStatPlot = ERRORPLOT(ILATRx, $
                                          ILATRy, $
                                          ILATRxErr, $
                                          ILATRyErr, $
                                          NAME=AARMedName, $
                                          COLOR=AARCol, $
                                          TRANSP=medianTransp, $
                                          THICK=2., $
                                          ERRORBAR_THICK=2., $
                                          SYMBOL=AARSym, $
                                          SYM_THICK=2.0, $
                                          SYM_SIZE=1.5, $
                                          /OVERPLOT, $
                                          CURRENT=winder3)

           ILATKappaLegend  = LEGEND(TARGET=[ILATkappaplot1, $
                                             ILATkappaEStatPlot, $
                                             ILATkappaplot2, $
                                             ILATkappaRStatPlot], $
                                     FONT_SIZE=legFontSize, $
                                     /NORMAL, $
                                     POSITION=[0.85,0.8])

        ILATkLinePlot = PLOT(REPLICATE(2.45,11),FINDGEN(11)/10.*(maxI-minI)+minI, $
                             YRANGE=ILATRange, $
                             THICK=2., $
                             COLOR=k245LineCol, $
                             TRANSP=60, $
                             LINESTYLE='--', $
                             /OVERPLOT, $
                             CURRENT=winder3)

        kText     = TEXT(0.15,0.1,"$\kappa_t$ = 2.45", $
                         /NORMAL, $
                         FONT_SIZE=fontSize, $
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

        MLTRange      = [minM-0.1,maxM+0.1]
        MLTkappaplot1 = SCATTERPLOT(KF2DParms.kappa[exc_i], $
                                    MLTs[exc_i], $
                                    XTITLE='Kappa', $
                                    YTITLE='MLT', $
                                    SYM_COLOR=belAARCol, $
                                    SYMBOL=belAARSym, $
                                    NAME=belAARName, $
                                    XRANGE=kappaPlotRange, $
                                    YRANGE=MLTRange, $
                                    XMINOR=1, $
                                    YMINOR=3, $
                                    TRANSP=BelTransp, $
                                    CURRENT=winder4)

        MLTkappaplot2 = SCATTERPLOT(KF2DParms.kappa[req_i], $
                                    MLTs[req_i], $
                                    SYM_COLOR=AARCol, $
                                    SYMBOL=AARSym, $
                                    NAME=AARName, $
                                    TRANSP=60, $
                                    /OVERPLOT, $
                                    CURRENT=winder4)

        IF KEYWORD_SET(medianstyle) THEN BEGIN

           histBinM            = 0.5
           NMinBinForInclusion = 10
           MLTReqHS = HISTOGRAM_BINSTATS( $
                      MLTs[req_i], $
                      KF2DParms.kappa[req_i], $
                      BINSIZE=histBinM, $
                      MIN=minM, $
                      MAX=maxM, $
                      /NAN, $
                      NMINBINFORINCLUSION=NMinBinForInclusion, $
                      GIVE_DECILES=stats__give_decile, $
                      GIVE_VENTILES=stats__give_ventile)
           MLTExcHS = HISTOGRAM_BINSTATS( $
                      MLTs[exc_i], $
                      KF2DParms.kappa[exc_i], $
                      BINSIZE=histBinM, $
                      MIN=minM, $
                      MAX=maxM, $
                      /NAN, $
                      NMINBINFORINCLUSION=NMinBinForInclusion, $
                      GIVE_DECILES=stats__give_decile, $
                      GIVE_VENTILES=stats__give_ventile)

           IF KEYWORD_SET(bpdStuff) THEN BEGIN
              ;; MLTEy = MLTExcHS.lEdge+histBinM/2.+histBinM/10.
              MLTEy = MLTExcHS.lEdge+histBinM/2.-0.05
              MLTEyErr = MAKE_ARRAY(N_ELEMENTS(MLTExcHS.stdDev),VALUE=0)
              ;; MLTRy = MLTReqHS.lEdge+histBinM/2.+histBinM/20.
              MLTRy = MLTReqHS.lEdge+histBinM/2.+0.05
              MLTRyErr = MAKE_ARRAY(N_ELEMENTS(MLTReqHS.stdDev),VALUE=0)

              CASE 1 OF
                 KEYWORD_SET(stats__give_decile): BEGIN
                    deciles = [0,1,2]

                    MLTEx = MLTExcHS[*].decile.val[deciles[1]]
                    MLTExErr = ABS(TRANSPOSE( $
                               [[MLTExcHS[*].decile.val[deciles[0]]-MLTEx], $
                                [MLTExcHS[*].decile.val[deciles[2]]-MLTEx]]))

                    MLTRx = MLTReqHS[*].decile.val[deciles[1]]
                    MLTRxErr = ABS(TRANSPOSE( $
                               [[MLTReqHS[*].decile.val[deciles[0]]-MLTRx], $
                                [MLTReqHS[*].decile.val[deciles[2]]-MLTRx]]))

                 END
                 KEYWORD_SET(stats__give_ventile): BEGIN
                    MLTEx = MLTExcHS[*].ventile.val[2]
                    MLTExErr = ABS(TRANSPOSE( $
                               [[MLTExcHS[*].ventile.val[0]-MLTEx], $
                                [MLTExcHS[*].ventile.val[4]-MLTEx]]))

                    MLTRx = MLTReqHS[*].ventile.val[2]
                    MLTRxErr = ABS(TRANSPOSE( $
                               [[MLTReqHS[*].ventile.val[0]-MLTRx], $
                                [MLTReqHS[*].ventile.val[4]-MLTRx]]))

                 END
                 ELSE: BEGIN
                    MLTEx = MLTExcHS[*].bpd[2]
                    MLTExErr = ABS(TRANSPOSE([[MLTExcHS[*].bpd[1]-MLTEx], $
                                              [MLTExcHS[*].bpd[3]-MLTEx]]))

                    MLTRx = MLTReqHS[*].bpd[2]
                    MLTRxErr = ABS(TRANSPOSE([[MLTReqHS[*].bpd[1]-MLTRx], $
                                              [MLTReqHS[*].bpd[3]-MLTRx]]))
                 END
              ENDCASE

           ENDIF ELSE BEGIN
              MLTEx = MLTExcHS.mean
              MLTEy = MLTExcHS.lEdge+histBinM/2.+histBinM/20.
              MLTExErr = MLTExcHS.stdDev
              MLTEyErr = MAKE_ARRAY(N_ELEMENTS(MLTExcHS.stdDev),VALUE=0)

              MLTRx = MLTReqHS.mean
              MLTRy = MLTReqHS.lEdge+histBinM/2.+histBinM/20.
              MLTRxErr = MLTReqHS.stdDev
              MLTRyErr = MAKE_ARRAY(N_ELEMENTS(MLTReqHS.stdDev),VALUE=0)
           ENDELSE

           MLTkappaEStatPlot = ERRORPLOT(MLTEx, $
                                         MLTEy, $
                                         MLTExErr, $
                                         MLTEyErr, $
                                         NAME=belAARMedName, $
                                         COLOR=belAARCol, $
                                         TRANSP=medianTransp, $
                                         THICK=2., $
                                         ERRORBAR_THICK=2., $
                                         SYMBOL=belAARSym, $
                                         SYM_THICK=2.0, $
                                         SYM_SIZE=1.5, $
                                         /OVERPLOT, $
                                         CURRENT=winder4)

           MLTkappaRStatPlot = ERRORPLOT(MLTRx, $
                                         MLTRy, $
                                         MLTRxErr, $
                                         MLTRyErr, $
                                         NAME=AARMedName, $
                                         COLOR=AARCol, $
                                         TRANSP=medianTransp, $
                                         THICK=2., $
                                         ERRORBAR_THICK=2., $
                                         SYMBOL=AARSym, $
                                         SYM_THICK=2.0, $
                                         SYM_SIZE=1.5, $
                                         /OVERPLOT, $
                                         CURRENT=winder4)

           MLTKappaLegend  = LEGEND(TARGET=[MLTkappaplot1, $
                                            MLTkappaEStatPlot, $
                                            MLTkappaplot2, $
                                            MLTkappaRStatPlot], $
                                    FONT_SIZE=legFontSize, $
                                    /NORMAL, $
                                    POSITION=[0.92,0.9])

           MLTkLinePlot = PLOT(REPLICATE(2.45,11),FINDGEN(11)/10.*(maxM-minM)+minM, $
                                YRANGE=MLTRange, $
                                THICK=2., $
                                COLOR=k245LineCol, $
                                TRANSP=35, $
                                LINESTYLE='--', $
                                /OVERPLOT, $
                                CURRENT=winder4)

           kText     = TEXT(0.15,0.1,"$\kappa_t$ = 2.45", $
                            /NORMAL, $
                            FONT_SIZE=fontSize, $
                            FONT_COLOR=k245LineCol, $
                            TARGET=winder4)

        ENDIF

        mltPlotName = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) $
                      + "-kS-MLTkappa" + altStr + "-" + $
                      hemi + parmStr + decileStr + bonusPlotSuff + ".png"

        PRINT,"Saving to " + mltPlotName
        winder4.Save,plotDir+mltPlotName


     ENDIF  

     IF KEYWORD_SET(makeGoverKvsKappaPlot) THEN BEGIN

        winder5 = WINDOW(DIMENSIONS=[800,800],BUFFER=bufferPlots)

        GoverKchi2Range      = KEYWORD_SET(GoverKLog) ? [0.9,MAX(ratio)] : [0.9,11]
        GoverKchi2Kappaplot1 = SCATTERPLOT(KF2DParms.kappa[exc_i], $
                                           ratio[exc_i], $
                                           XTITLE='Kappa', $
                                           YTITLE='G over K', $
                                           YLOG=GoverKLog, $
                                           SYM_COLOR=belAARCol, $
                                           SYMBOL=belAARSym, $
                                           NAME=belAARName, $
                                           XRANGE=kappaPlotRange, $
                                           YRANGE=GoverKchi2Range, $
                                           TRANSP=BelTransp, $
                                           CURRENT=winder5)

        GoverKchi2Kappaplot2 = SCATTERPLOT(KF2DParms.kappa[req_i], $
                                           ratio[req_i], $
                                           YLOG=GoverKLog, $
                                           SYM_COLOR=AARCol, $
                                           SYMBOL=AARSym, $
                                           NAME=AARName, $
                                           TRANSP=AARTransp, $
                                           /OVERPLOT, $
                                           CURRENT=winder5)

        IF KEYWORD_SET(medianstyle) THEN BEGIN

           binK              = 0.2
           NMinBinForInclusion = 10
           GoverKReqHS = HISTOGRAM_BINSTATS( $
                         KF2DParms.kappa[req_i], $
                         ratio[req_i], $
                         BINSIZE=binK, $
                         MIN=minK, $
                         MAX=maxK, $
                         /NAN, $
                         NMINBINFORINCLUSION=NMinBinForInclusion, $
                         GIVE_DECILES=stats__give_decile, $
                         GIVE_VENTILES=stats__give_ventile)
           GoverKExcHS = HISTOGRAM_BINSTATS( $
                         KF2DParms.kappa[exc_i], $
                         ratio[exc_i], $
                         BINSIZE=binK, $
                         MIN=minK, $
                         MAX=maxK, $
                         /NAN, $
                         NMINBINFORINCLUSION=NMinBinForInclusion, $
                         GIVE_DECILES=stats__give_decile, $
                         GIVE_VENTILES=stats__give_ventile)

           IF KEYWORD_SET(bpdStuff) THEN BEGIN
              ;; GoverKEx = GoverKExcHS.lEdge+binK/2.+binK/10.
              GoverKEx = GoverKExcHS.lEdge+binK/2.-0.05
              GoverKExErr = MAKE_ARRAY(N_ELEMENTS(GoverKExcHS.stdDev),VALUE=0)
              ;; GoverKRx = GoverKReqHS.lEdge+binK/2.+binK/20.
              GoverKRx = GoverKReqHS.lEdge+binK/2.+0.05
              GoverKRxErr = MAKE_ARRAY(N_ELEMENTS(GoverKReqHS.stdDev),VALUE=0)

              CASE 1 OF
                 KEYWORD_SET(stats__give_decile): BEGIN
                    deciles = [0,1,2]
                    ;; deciles = [3,4,5]

                    GoverKEy = GoverKExcHS[*].decile.val[deciles[1]]
                    GoverKEyErr = ABS(TRANSPOSE( $
                               [[GoverKExcHS[*].decile.val[deciles[0]]-GoverKEy], $
                                [GoverKExcHS[*].decile.val[deciles[2]]-GoverKEy]]))

                    GoverKRy = GoverKReqHS[*].decile.val[deciles[1]]
                    GoverKRyErr = ABS(TRANSPOSE( $
                               [[GoverKReqHS[*].decile.val[deciles[0]]-GoverKRy], $
                                [GoverKReqHS[*].decile.val[deciles[2]]-GoverKRy]]))

                 END
                 KEYWORD_SET(stats__give_ventile): BEGIN
                    GoverKEy = GoverKExcHS[*].ventile.val[2]
                    GoverKEyErr = ABS(TRANSPOSE( $
                               [[GoverKExcHS[*].ventile.val[0]-GoverKEy], $
                                [GoverKExcHS[*].ventile.val[4]-GoverKEy]]))

                    GoverKRy = GoverKReqHS[*].ventile.val[2]
                    GoverKRyErr = ABS(TRANSPOSE( $
                               [[GoverKReqHS[*].ventile.val[0]-GoverKRy], $
                                [GoverKReqHS[*].ventile.val[4]-GoverKRy]]))

                 END
                 ELSE: BEGIN
                    GoverKEy = GoverKExcHS[*].bpd[2]
                    GoverKEyErr = ABS(TRANSPOSE([[GoverKExcHS[*].bpd[1]-GoverKEy], $
                                              [GoverKExcHS[*].bpd[3]-GoverKEy]]))

                    GoverKRy = GoverKReqHS[*].bpd[2]
                    GoverKRyErr = ABS(TRANSPOSE([[GoverKReqHS[*].bpd[1]-GoverKRy], $
                                              [GoverKReqHS[*].bpd[3]-GoverKRy]]))
                 END
              ENDCASE

           ENDIF ELSE BEGIN
              GoverKEy = GoverKExcHS.mean
              GoverKEx = GoverKExcHS.lEdge+binK/2.+binK/20.
              GoverKEyErr = GoverKExcHS.stdDev
              GoverKExErr = MAKE_ARRAY(N_ELEMENTS(GoverKExcHS.stdDev),VALUE=0)

              GoverKRy = GoverKReqHS.mean
              GoverKRx = GoverKReqHS.lEdge+binK/2.+binK/20.
              GoverKRyErr = GoverKReqHS.stdDev
              GoverKRxErr = MAKE_ARRAY(N_ELEMENTS(GoverKReqHS.stdDev),VALUE=0)
           ENDELSE

           GoverKchi2KappaEStatPlot = ERRORPLOT(GoverKEx, $
                                                GoverKEy, $
                                                GoverKExErr, $
                                                GoverKEyErr, $
                                                NAME=belAARMedName, $
                                                COLOR=belAARCol, $
                                                TRANSP=medianTransp, $
                                                THICK=2., $
                                                ERRORBAR_THICK=2., $
                                                SYMBOL=belAARSym, $
                                                SYM_THICK=2.0, $
                                                SYM_SIZE=1.5, $
                                                /OVERPLOT, $
                                                CURRENT=winder5)

           GoverKchi2KappaRStatPlot = ERRORPLOT(GoverKRx, $
                                                GoverKRy, $
                                                GoverKRxErr, $
                                                GoverKRyErr, $
                                                NAME=AARMedName, $
                                                COLOR=AARCol, $
                                                TRANSP=medianTransp, $
                                                THICK=2., $
                                                ERRORBAR_THICK=2., $
                                                SYMBOL=AARSym, $
                                                SYM_THICK=2.0, $
                                                SYM_SIZE=1.5, $
                                                /OVERPLOT, $
                                                CURRENT=winder5)

           GoverKKappaLegend  = LEGEND(TARGET=[GoverKchi2Kappaplot1, $
                                               GoverKchi2KappaEStatPlot, $
                                               GoverKchi2Kappaplot2, $
                                               GoverKchi2KappaRStatPlot], $
                                       FONT_SIZE=legFontSize, $
                                       /NORMAL, $
                                       POSITION=[0.85,0.8])

           ;; GoverKkLinePlot = PLOT(REPLICATE(2.45,11),FINDGEN(11)/10.*(maxK-minK)+minK, $
           ;;                      YRANGE=GoverKchi2Range, $
           ;;                      THICK=2., $
           ;;                      COLOR=k245LineCol, $
           ;;                      TRANSP=35, $
           ;;                      LINESTYLE='--', $
           ;;                      /OVERPLOT, $
           ;;                      CURRENT=winder5)

           ;; kText     = TEXT(0.15,0.1,"$\kappa_t$ = 2.45", $
           ;;                  /NORMAL, $
           ;;                  FONT_SIZE=fontSize, $
           ;;                  FONT_COLOR=k245LineCol, $
           ;;                  TARGET=winder5)

        ENDIF

        GoverKPlotName = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) $
                         + "-kS-GoverKchi2vsKappa" + altStr + "-" + $
                         (KEYWORD_SET(GoverKLog) ? 'log-' : '') + $
                          ;; hemi + parmStr + decileStr + bonusPlotSuff + ".png"
                          hemi + parmStr + bonusPlotSuff + ".png"

        PRINT,"Saving to " + GoverKPlotName
        winder5.Save,plotDir+GoverKPlotName


     ENDIF  

     IF KEYWORD_SET(makeChi2RedvsKappaPlot) THEN BEGIN

        winder6 = WINDOW(DIMENSIONS=[800,800],BUFFER=bufferPlots)

        chi2RedRange       = KEYWORD_SET(chi2RedLog)      ? $
                             ;; [0.1,MAX(KF2DParms.chi2Red)] : $
                             [0.1,200] : $
                             [0.1,11]
        chi2RedKappaplot1 = SCATTERPLOT(KF2DParms.kappa[exc_i], $
                                        KF2DParms.chi2Red[exc_i], $
                                        XTITLE='Kappa', $
                                        YTITLE='$\chi^2_{red}$', $
                                        YLOG=chi2RedLog, $
                                        SYM_COLOR=belAARCol, $
                                        SYMBOL=belAARSym, $
                                        NAME=belAARName, $
                                        XRANGE=kappaPlotRange, $
                                        YRANGE=chi2RedRange, $
                                        TRANSP=BelTransp, $
                                        CURRENT=winder6)

        chi2RedKappaplot2 = SCATTERPLOT(KF2DParms.kappa[req_i], $
                                        KF2DParms.chi2Red[req_i], $
                                        YLOG=chi2RedLog, $
                                        SYM_COLOR=AARCol, $
                                        SYMBOL=AARSym, $
                                        NAME=AARName, $
                                        TRANSP=AARTransp, $
                                        /OVERPLOT, $
                                        CURRENT=winder6)

        IF KEYWORD_SET(medianstyle) THEN BEGIN

           binK              = 0.2
           NMinBinForInclusion = 10
           chi2RedReqHS = HISTOGRAM_BINSTATS( $
                          KF2DParms.kappa[req_i], $
                          KF2DParms.chi2Red[req_i], $
                          BINSIZE=binK, $
                          MIN=minK, $
                          MAX=maxK, $
                          /NAN, $
                          NMINBINFORINCLUSION=NMinBinForInclusion, $
                          GIVE_DECILES=stats__give_decile, $
                          GIVE_VENTILES=stats__give_ventile)
           chi2RedExcHS = HISTOGRAM_BINSTATS( $
                          KF2DParms.kappa[exc_i], $
                          KF2DParms.chi2Red[exc_i], $
                          BINSIZE=binK, $
                          MIN=minK, $
                          MAX=maxK, $
                          /NAN, $
                          NMINBINFORINCLUSION=NMinBinForInclusion, $
                          GIVE_DECILES=stats__give_decile, $
                          GIVE_VENTILES=stats__give_ventile)

           IF KEYWORD_SET(bpdStuff) THEN BEGIN
              ;; chi2RedEx = chi2RedExcHS.lEdge+binK/2.+binK/10.
              chi2RedEx = chi2RedExcHS.lEdge+binK/2.-0.05
              chi2RedExErr = MAKE_ARRAY(N_ELEMENTS(chi2RedExcHS.stdDev),VALUE=0)
              ;; chi2RedRx = chi2RedReqHS.lEdge+binK/2.+binK/20.
              chi2RedRx = chi2RedReqHS.lEdge+binK/2.+0.05
              chi2RedRxErr = MAKE_ARRAY(N_ELEMENTS(chi2RedReqHS.stdDev),VALUE=0)

              CASE 1 OF
                 KEYWORD_SET(stats__give_decile): BEGIN
                    deciles = [0,1,2]

                    chi2RedEy = chi2RedExcHS[*].decile.val[deciles[1]]
                    chi2RedEyErr = ABS(TRANSPOSE( $
                               [[chi2RedExcHS[*].decile.val[deciles[0]]-chi2RedEy], $
                                [chi2RedExcHS[*].decile.val[deciles[2]]-chi2RedEy]]))

                    chi2RedRy = chi2RedReqHS[*].decile.val[deciles[1]]
                    chi2RedRyErr = ABS(TRANSPOSE( $
                               [[chi2RedReqHS[*].decile.val[deciles[0]]-chi2RedRy], $
                                [chi2RedReqHS[*].decile.val[deciles[2]]-chi2RedRy]]))

                 END
                 KEYWORD_SET(stats__give_ventile): BEGIN
                    chi2RedEy = chi2RedExcHS[*].ventile.val[2]
                    chi2RedEyErr = ABS(TRANSPOSE( $
                               [[chi2RedExcHS[*].ventile.val[0]-chi2RedEy], $
                                [chi2RedExcHS[*].ventile.val[4]-chi2RedEy]]))

                    chi2RedRy = chi2RedReqHS[*].ventile.val[2]
                    chi2RedRyErr = ABS(TRANSPOSE( $
                               [[chi2RedReqHS[*].ventile.val[0]-chi2RedRy], $
                                [chi2RedReqHS[*].ventile.val[4]-chi2RedRy]]))

                 END
                 ELSE: BEGIN
                    chi2RedEy = chi2RedExcHS[*].bpd[2]
                    chi2RedEyErr = ABS(TRANSPOSE([[chi2RedExcHS[*].bpd[1]-chi2RedEy], $
                                              [chi2RedExcHS[*].bpd[3]-chi2RedEy]]))

                    chi2RedRy = chi2RedReqHS[*].bpd[2]
                    chi2RedRyErr = ABS(TRANSPOSE([[chi2RedReqHS[*].bpd[1]-chi2RedRy], $
                                              [chi2RedReqHS[*].bpd[3]-chi2RedRy]]))
                 END
              ENDCASE

           ENDIF ELSE BEGIN
              chi2RedEy = chi2RedExcHS.mean
              chi2RedEx = chi2RedExcHS.lEdge+binK/2.+binK/20.
              chi2RedEyErr = chi2RedExcHS.stdDev
              chi2RedExErr = MAKE_ARRAY(N_ELEMENTS(chi2RedExcHS.stdDev),VALUE=0)

              chi2RedRy = chi2RedReqHS.mean
              chi2RedRx = chi2RedReqHS.lEdge+binK/2.+binK/20.
              chi2RedRyErr = chi2RedReqHS.stdDev
              chi2RedRxErr = MAKE_ARRAY(N_ELEMENTS(chi2RedReqHS.stdDev),VALUE=0)
           ENDELSE

           chi2RedKappaEStatPlot = ERRORPLOT(chi2RedEx, $
                                            chi2RedEy, $
                                            chi2RedExErr, $
                                            chi2RedEyErr, $
                                            NAME=belAARMedName, $
                                            COLOR=belAARCol, $
                                            TRANSP=medianTransp, $
                                            THICK=2., $
                                            ERRORBAR_THICK=2., $
                                            SYMBOL=belAARSym, $
                                            SYM_THICK=2.0, $
                                            SYM_SIZE=1.5, $
                                            /OVERPLOT, $
                                            CURRENT=winder6)

           chi2RedKappaRStatPlot = ERRORPLOT(chi2RedRx, $
                                             chi2RedRy, $
                                             chi2RedRxErr, $
                                             chi2RedRyErr, $
                                             NAME=AARMedName, $
                                             COLOR=AARCol, $
                                             TRANSP=medianTransp, $
                                             THICK=2., $
                                             ERRORBAR_THICK=2., $
                                             SYMBOL=AARSym, $
                                             SYM_THICK=2.0, $
                                             SYM_SIZE=1.5, $
                                             /OVERPLOT, $
                                             CURRENT=winder6)

           chi2RedKappaLegend  = LEGEND(TARGET=[chi2RedKappaplot1, $
                                                chi2RedKappaEStatPlot, $
                                                chi2RedKappaplot2, $
                                                chi2RedKappaRStatPlot], $
                                        /NORMAL, $
                                        FONT_SIZE=legFontSize, $
                                        POSITION=[0.85,0.8])

           ;; chi2RedkLinePlot = PLOT(REPLICATE(2.45,11),FINDGEN(11)/10.*(maxK-minK)+minK, $
           ;;                      YRANGE=chi2RedRange, $
           ;;                      THICK=2., $
           ;;                      COLOR=k245LineCol, $
           ;;                      TRANSP=35, $
           ;;                      LINESTYLE='--', $
           ;;                      /OVERPLOT, $
           ;;                      CURRENT=winder6)

           ;; kText     = TEXT(0.15,0.1,"$\kappa_t$ = 2.45", $
           ;;                  /NORMAL, $
           ;;                  FONT_SIZE=fontSize, $
           ;;                  FONT_COLOR=k245LineCol, $
           ;;                  TARGET=winder6)

        ENDIF

        chi2RedPlotName = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) $
                         + "-kS-chi2RedvsKappa" + altStr + "-" + $
                         (KEYWORD_SET(chi2RedLog) ? 'log-' : '') + $
                          ;; hemi + parmStr + decileStr + bonusPlotSuff + ".png"
                          hemi + parmStr + bonusPlotSuff + ".png"

        PRINT,"Saving to " + chi2RedPlotName
        winder6.Save,plotDir+chi2RedPlotName


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

  IF KEYWORD_SET(makeGoverKvsKappaPlot) THEN BEGIN
     winder5.Close
  ENDIF

  IF KEYWORD_SET(makeChi2RedvsKappaPlot) THEN BEGIN
     winder6.Close
  ENDIF


END
