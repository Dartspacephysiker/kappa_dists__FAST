;2018/05/04
PRO JOURNAL__20180504__KAPPA_FITS__MLT_VS_TIME, $
   EXCLUDE_IONS=exclude_ions, $
   REQUIRE_IONS=require_ions, $
   COMBINED_HISTOS=combined_histos, $
   GOVERK=GoverK, $
   MAXKCHI2=maxKChi2, $
   MINALT=minA, $
   MAXALT=maxA

  COMPILE_OPT IDL2,STRICTARRSUBS

  minM = -3.5
  maxM = 1.5
  divM = -1.5

  minI  = 60
  maxI  = 90
  hemi  = 'Both'

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

  bufferPlots            = 0
  savePlots              = 1
  saveEmAll              = KEYWORD_SET(bufferPlots) OR KEYWORD_SET(savePlots)

  GoverKReq = N_ELEMENTS(GoverK  ) GT 0 ? GoverK   : 'decile=1'
  KChi2Max  = N_ELEMENTS(maxKChi2) GT 0 ? maxKChi2 : 4

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

  MLTs = andre.mlt
  MLTs[WHERE(MLTs GT 18)] = MLTs[WHERE(MLTs GT 18)] - 24.

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

  PRINT,FORMAT='("Working with ",I0, " RequInds")',nReq
  PRINT,FORMAT='("Working with ",I0, " ExclInds")',nExc

  AARName    = 'AAR' + STRING(9B) + STRING(9B) + STRING(FORMAT='("(N = ",I5,")")',nReq)
  belAARName = 'Below' + STRING(9B) + STRING(FORMAT='("(N = ",I5,")")',nExc)

  ;; req_i          = WHERE(andre.ionBeam EQ 1 OR andre.ionBeam EQ 2,nReq)
  ;; exc_i          = WHERE(andre.ionBeam EQ 1 OR andre.ionBeam EQ 2,nExc)

  ;; Unique low kappa-ers
  ;; lowKappa_i        = WHERE(KF2DParms.kappa LE 2,nLowKappa,COMPLEMENT=notLowKappa_i,NCOMPLEMENT=nNotLowKappa)
  ;; lowKappa_i        = CGSETINTERSECTION(lowKappa_i,final_i,COUNT=nLowKappa)
  ;; lowKappaOrbs      = andre.orbit[lowkappa_i[UNIQ(andre.orbit[lowkappa_i],SORT(andre.orbit[lowkappa_i]))]]

  ;; plot_i            = final_i

     BelTransp = 85
     AARTransp = 90
     belAARCol = 'BLACK'
     AARCol    = 'RED'

     belAARSym = 'x'
     AARSym    = '+'

  ;; Create format strings for a time axis
  dummy = LABEL_DATE(DATE_FORMAT=['%N-%Z'])

  ;; finalTime=UTC2JULDAY(MAX(andre.time))
  julTidReq = UTC_TO_JULDAY(andre.time[req_i])
  julTidExc = UTC_TO_JULDAY(andre.time[exc_i])

  ;; Set up time axis
  finalTime = JULDAY(06,01,1999)
  axisTimes = TIMEGEN(START=JULDAY(9,1,1996), $
                      FINAL=finalTime, $
                      UNITS="Years", $
                      MONTHS=[3,6,9,12])

  xTickNames = MAKE_ARRAY(N_ELEMENTS(axisTimes),/STRING,VALUE='')
  CALDAT,axisTimes[1:-1:2],month,day,year

  tmpString = !NULL
  FOR k=0,N_ELEMENTS(month)-1 DO tmpString=[tmpString, $
                                            STRING(FORMAT='(I02,"-",I02)', $
                                                   year[k]-1900, $
                                                   month[k])]
  xTickNames[1:-1:2] = tmpString

  winder   = WINDOW(DIMENSIONS=[800,800],BUFFER=bufferPlots)

     legFontSize = 14
     fontSize = 20

     CASE 1 OF
        (STRUPCASE(hemi) EQ 'NORTH') OR (STRUPCASE(hemi) EQ 'SOUTH'): BEGIN
           title = STRUPCASE(STRMID(hemi,0,1)) + $
                   STRLOWCASE(STRMID(hemi,1,4)) + 'ern Hemisphere Obs'
        END
        ELSE: BEGIN
           title = "Obs in both hemispheres"
        END
     ENDCASE

  excMLTPlot = SCATTERPLOT(julTidExc,MLTs[exc_i], $
                           NAME=belAARName, $
                           TITLE=title, $
                           YTITLE='MLT', $
                           XRANGE=MINMAX(axisTimes), $
                           XTICKFORMAT='LABEL_DATE', $
                           XTICKVALUES=axisTimes, $
                           XTICKNAME=xTickNames, $
                           CURRENT=winder, $
                           XTICKUNITS='Years', $
                           XTICKINTERVAL=0.5, $
                           XTICKFONT_SIZE=14, $
                           XMINOR=2, $
                           SYM_COLOR=belAARCol, $
                           SYMBOL=belAARSym, $
                           TRANSP=belTransp, $
                          FONT_SIZE=fontSize)


  reqMLTPlot = SCATTERPLOT(julTidReq,MLTs[req_i], $
                           NAME=AARName, $
                           XRANGE=MINMAX(axisTimes), $
                           ;; XTICKFORMAT='LABEL_DATE', $
                           ;; XTICKVALUES=axisTimes, $
                           CURRENT=winder, $
                           ;; XTICKUNITS='Years', $
                           ;; XTICKINTERVAL=0.5, $
                           ;; XMINOR=2, $
                           SYM_COLOR=AARCol, $
                           SYMBOL=AARSym, $
                           TRANSP=AARTransp, $
                           /OVERPLOT)

     histLegend  = LEGEND(TARGET=[reqMLTPlot,excMLTPlot], $
                          /NORMAL, $
                          FONT_SIZE=legFontSize, $
                          POSITION=[0.3,0.85])


     IF saveEmAll THEN BEGIN
        outPlotName = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) $
                      + STRING(FORMAT='("-kS-MLT_v_time-",A0,A0,"-",A0,A0,A0,".png")', $
                               MLTStr,altStr, $
                               hemi,parmStr,bonusPlotSuff)
        PRINT,"Saving to " + outPlotName
        winder.Save,plotDir+outPlotName
     ENDIF

     IF ~KEYWORD_SET(bufferPlots) THEN STOP

     winder.Close

END
