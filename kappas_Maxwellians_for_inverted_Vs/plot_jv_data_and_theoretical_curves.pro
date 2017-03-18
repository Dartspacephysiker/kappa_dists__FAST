;2017/03/18
PRO PLOT_JV_DATA_AND_THEORETICAL_CURVES,jvPlotData, $
                                        CURPOTLIST=curPotList, $
                                        MINPOT=minPot, $
                                        MAXPOT=maxPot, $
                                        MINCUR=minCur, $
                                        MAXCUR=maxCur, $
                                        USEINDS=useInds, $
                                        PLOT_J_RATIOS=plot_j_ratios, $
                                        PLOT_ION_ELEC_RATIOS=plot_ion_elec_ratios, $
                                        ORIGINATING_ROUTINE=routName, $
                                        PLOTDIR=plotDir, $
                                        SAVEPLOT=savePlot, $
                                        SPNAME=spName, $
                                        OUT_AVGS_FOR_FITTING=avgs_JVfit

  COMPILE_OPT IDL2,STRICTARRSUBS

  IF KEYWORD_SET(plot_ion_elec_ratios) THEN BEGIN
     nListMem        = N_ELEMENTS(curPotList)         
     nHere           = N_ELEMENTS(curPotList[0].time)
     errorBarFac     = KEYWORD_SET(errorBarFac) ? errorBarFac : 1.

     looking         = nListMem
     ind             = 0

     WHILE (looking GT 0) DO BEGIN
        IF STRMATCH(STRUPCASE(curPotList[ind].label),'*DOWN*E') THEN BEGIN
           looking--
           edind = ind
        ENDIF

        IF STRMATCH(STRUPCASE(curPotList[ind].label),'*UP*E') THEN BEGIN
           looking--
           euind = ind
        ENDIF

        IF STRMATCH(STRUPCASE(curPotList[ind].label),'*UP*I') THEN BEGIN
           looking--
           iuind = ind
        ENDIF
        ind++
     ENDWHILE

     ji_je_ratio           = curPotList[iuind].cur/curPotList[edind].cur

  ENDIF

  negcur_i                    = WHERE(jvplotdata.cur LE 0)
  negcur_i                    = negcur_i[SORT(jvplotdata.pot[negcur_i])]

  minPot                      = KEYWORD_SET(minPot) ? minPot : 0.D
  maxPot                      = KEYWORD_SET(maxPot) ? maxPot : 4000

  minCur                      = KEYWORD_SET(minCur) ? minCur : 1D-6
  maxCur                      = KEYWORD_SET(maxCur) ? maxCur : 1D-3


  IF N_ELEMENTS(useInds) EQ 0 THEN BEGIN
     ;;The points that have a clear affinity for kappa = 2
     thesepointslovekappa_ii  = WHERE((jvplotdata.pot[negcur_i] LE maxPot) AND $
                                      (jvplotdata.pot[negcur_i] GE minPot) AND $
                                      (jvplotdata.cur[negcur_i]*(-1D-6) GE minCur) AND $
                                      (jvplotdata.cur[negcur_i]*(-1D-6) LE maxCur),nLovers)
     PRINT,"THESE POINTS LOVE KAPPA=2.0"
     loveKappa_i              = negcur_i[thesepointslovekappa_ii]
     GET_STREAKS,loveKappa_i[SORT(loveKappa_i)],START_I=loveKappa_iStrt_ii,STOP_I=loveKappa_iStop_ii,OUT_STREAKLENS=streakLens
     times                    = TIME_TO_STR(jvplotdata.time[loveKappa_i[SORT(jvplotdata.time[loveKappa_i])]],/MS)
     FOR k=0,nLovers-1 DO BEGIN
        PRINT,TIME_TO_STR(jvplotdata.time[loveKappa_i[k]])
     ENDFOR

     ;; useInds               = negcur_i
     useInds                  = loveKappa_i

  ENDIF

  useInds                     = useInds[SORT(jvplotdata.pot[useInds])]
  
  curDat                      = jvplotdata.cur*(-1D-6) / (KEYWORD_SET(plot_j_ratios) ? jvplotdata.cur*(-1D-6) : 1.D)
  divDat                      = jvplotdata.cur*(-1D-6)
  ;; SAVE,KnightRelat30,KnightRelat300,KnightRelat3000,jvplotdata,FILENAME=
  ;; RESTORE,'
  ;; R_Bs__M                  = [30,300,3000]
  R_Bs__M                     = [100,10000,100,10000]
  ;; R_Bs__K                  = [30,300,3000]
  ;; kappas                   = [2.0,2.0,2.0,1.6]
  TmultFac__Maxwell           = [1,1,10,10]
  R_Bs__K                     = [100,100,10000,10000]
  kappas                      = [2.0,1.8,2.0,1.8]
  TmultFac__kappa             = [1,1,1,1]

  nR_Bs__M                    = N_ELEMENTS(R_Bs__M)
  nR_Bs__K                    = N_ELEMENTS(R_Bs__K)
  nDer                        = N_ELEMENTS(useInds)

  maxwellJVs                  = MAKE_ARRAY(nR_Bs__M,nDer,/DOUBLE)
  kappaJVs                    = MAKE_ARRAY(nR_Bs__K,nDer,/DOUBLE)

  MaxwellTransp               = 30
  MaxwellSym                  = '*'
  MaxwellColors               = ['Red','Brown','Dark Green','Dark Orange']
  MaxwellLinestyle            = ['']
  MaxwellNames                = MAKE_ARRAY(nR_Bs__M,/STRING)

  kappaTransp                 = 30
  kappaSym                    = ['x','tu','+','td']
  kappaColors                 = ['Purple','Brown','Gray',"Violet"]
  kappaLinestyle              = ['']
  kappaNames                  = MAKE_ARRAY(nR_Bs__K,/STRING)

  FOR k=0,nR_Bs__M-1 DO BEGIN
     maxwellJVs[k,*] = KNIGHT_RELATION__DORS_KLETZING_4(jvplotdata.tdown[useInds], $
                                                        jvplotdata.ndown[useInds], $
                                                        jvplotdata.pot[useInds], $
                                                        R_Bs__M[k], $
                                                        /NO_MULT_BY_CHARGE)

     MaxwellNames[k] = 'R!DB!N = ' + STRING(FORMAT='(I0)',R_Bs__M[k])
     MaxwellNames[k] = STRING(FORMAT='("R!DB!N = ",I0," (T*=",I0,")")', $
                            R_Bs__M[k],TmultFac__Maxwell[k])

     IF KEYWORD_SET(plot_j_ratios) THEN BEGIN
        maxwellJVs[k,*] /= divDat[useInds]
     ENDIF
  ENDFOR

  FOR k=0,nR_Bs__K-1 DO BEGIN
     kappaJVs[k,*] = KNIGHT_RELATION__DORS_KLETZING_11(kappas[k],jvplotdata.tdown[useInds], $
                                                       jvplotdata.ndown[useInds], $
                                                       jvplotdata.pot[useInds], $
                                                       R_Bs__K[k], $
                                                       /NO_MULT_BY_CHARGE)

     kappaNames[k] = STRING(FORMAT='("R!DB!N = ",I0," ($\kappa$=",F0.2,",T*=",I0,")")', $
                            R_Bs__K[k],kappas[k],TmultFac__kappa[k])

     IF KEYWORD_SET(plot_j_ratios) THEN BEGIN
        kappaJVs[k,*] /= divDat[useInds]
     ENDIF
  ENDFOR

  MaxwellPlots = MAKE_ARRAY(nR_Bs__M,/OBJ)
  kappaPlots   = MAKE_ARRAY(nR_Bs__K,/OBJ)
  window1      = WINDOW(DIMENSIONS=[1000,800], $
                        BUFFER=savePlot)
  yLog         = 0
  dataLStyle   = ''
  dataSym      = 'o'
  dataName     = 'Data'
  xTitle       = 'Potential (V)'
  yTitle       = KEYWORD_SET(plot_j_ratios) ? "J!D||,obs!N / J!D||,theor!N" : $
                 'Current density ($\mu$A/m!U2!N), mapped to 100km'

  ji_je_lStyle = ''
  ji_je_sym    = 's'
  ji_je_name   = 'J!Di!N over J!De!N'

  dataplot     = PLOT(jvplotdata.pot[useInds], $
                  curDat[useInds], $
                  LINESTYLE=dataLStyle, $
                  SYMBOL=dataSym, $
                  XTITLE=xTitle, $
                  YTITLE=yTitle, $
                  NAME=dataName, $
                  YLOG=yLog, $
                  /CURRENT)

  FOR k=0,nR_Bs__M-1 DO BEGIN
     MaxwellPlots[k] = PLOT(jvplotdata.pot[useInds], $
                            MaxwellJVs[k,*], $
                            TRANSPARENCY=MaxwellTransp, $
                            LINESTYLE=MaxwellLinestyle, $
                            SYMBOL=MaxwellSym, $
                            COLOR=MaxwellColors[k], $
                            /OVERPLOT, $
                            NAME=MaxwellNames[k])
  ENDFOR

  FOR k=0,nR_Bs__K-1 DO BEGIN
     kappaPlots[k] = PLOT(jvplotdata.pot[useInds], $
                        kappaJVs[k,*], $
                        TRANSPARENCY=kappaTransp, $
                        LINESTYLE='', $
                        SYMBOL=kappaSym[k], $
                        COLOR=kappaColors[k], $
                        /OVERPLOT, $
                        NAME=kappaNames[k])
  ENDFOR

  legArr = [dataplot, $
            MaxwellPlots, $
            kappaPlots]

  IF KEYWORD_SET(ji_je_ratio) THEN BEGIN

     ji_je_plot = PLOT(jvplotdata.pot[useInds], $
                       ji_je_ratio[useInds], $
                       LINESTYLE=ji_je_lStyle, $
                       SYMBOL=ji_je_sym, $
                       ;; XTITLE=xTitle, $
                       ;; YTITLE=yTitle, $
                       NAME=ji_je_name, $
                       YLOG=yLog, $
                       /CURRENT, $
                       /OVERPLOT)

     legArr = [legArr,ji_je_plot]

  ENDIF
  leg = LEGEND(TARGET=legArr)

  IF KEYWORD_SET(savePlot) THEN BEGIN

     IF ~KEYWORD_SET(sPName) THEN BEGIN
        sPName = routName + '-JV_Theor.png'
     ENDIF

     IF ~KEYWORD_SET(plotDir) THEN BEGIN
        pDirSuff      = '/cur_and_pot_analysis'
        SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF=pDirSuff
     ENDIF

     PRINT,"Saving to " + sPName + ' ...'

     window1.Save,plotDir+sPName

  ENDIF

  fmtStr = '(A0," (min, max,stdDev) ",T35,": ",F0.2," (",F0.2,", ",F0.2,", ",F0.2,")")'

  quantL = LIST(jvplotdata.Tdown[useInds],jvplotdata.Ndown[useInds])
  navn   = ['T avg','N avg']
  sNavn  = ['T','N']
  IF KEYWORD_SET(ji_je_ratio) THEN BEGIN
     quantL.Add,ji_je_ratio[useInds]
     navn  = [navn,'Ji/Je avg']
     sNavn = [sNavn,'JiJeRat']
  ENDIF
  
  avgs_JVfit = {useInds : useInds}

  FOR k=0,N_ELEMENTS(quantL)-1 DO BEGIN
     tmpQuant = quantL[k]
     PRINT,FORMAT=fmtStr, $
           navn[k], $
           MEAN(tmpQuant), $
           MIN(tmpQuant), $
           MAX(tmpQuant), $
           STDDEV(tmpQuant)

     execStr = sNavn[k] + ' = {avg:MEAN(tmpQuant),stddev:STDDEV(tmpQuant),min:MIN(tmpQuant),max:MAX(tmpQuant)}'
     IF ~EXECUTE(execStr) THEN STOP

     IF N_ELEMENTS(avgs_JVfit) EQ 0 THEN BEGIN
        exec2Str = 'avgs_JVfit = {' + sNavn[k] + ' : ' + sNavn[k] + '}'
     ENDIF ELSE BEGIN
        exec2Str = 'avgs_JVfit = CREATE_STRUCT(avgs_JVfit,"' + sNavn[k] + '",' + sNavn[k] + ')'
     ENDELSE

     IF ~EXECUTE(exec2Str) THEN STOP

  ENDFOR

END
