;;2017/03/04
PRO ERROR_PHIBAR,phi,T,N,phiErr,errors,phiBarErr

  Tavg = REFORM(T[3,*])
  PPar = REFORM(T[2,*])*N
  PPrp = REFORM(T[0,*])*N

  sigma_N_PPar     = errors.R[*,0,6]*(errors.N*N)*(errors.Pzz*PPar)
  sigma_N_PPrp     = errors.R[*,0,4]*(errors.N*N)*(errors.Pxx*PPrp)
  sigma_PPar_PPrp  = errors.R[*,6,4]*(errors.Pzz*PPar)*(errors.Pxx*PPrp)

  phiBarErr = SQRT((phiErr/Tavg)^2.D + $
                   phi^2.D * ( ( ( 1.D / 9.D / N^2.D / Tavg^4.D ) * $
                                 ( (errors.Pzz*Ppar)^2.D + 4.D*(errors.Pxx*PPrp)^2.D + 4.D*sigma_PPar_PPrp ) ) + $
                               (errors.N/Tavg)^2.D                                                             - $
                               ( ( 2.D / 3.D / N^2.D / Tavg^3.D ) * $
                                 ( sigma_N_PPar + 2.D * sigma_N_PPrp ) )                                         ))

END
PRO CURRENT_AND_POTENTIAL_PLOTDATA_PREP,curPotList,jvPlotData, $
                                        T1=t1, $
                                        T2=t2, $
                                        USE_ALL_CURRENTS=use_all_currents, $
                                        USE_DOWNGOING_ELECTRON_CURRENT=use_ed_current, $
                                        USE_UPGOING_ION_CURRENT=use_iu_current, $
                                        USE_UPGOING_ELECTRON_CURRENT=use_eu_current, $
                                        USE_MAGNETOMETER_CURRENT=use_mag_current, $
                                        USE_CHAR_EN_FOR_DOWNPOT=use_charE_for_downPot, $
                                        USE_PEAK_EN_FOR_DOWNPOT=use_peakE_for_downPot, $
                                        ADD_UPGOING_ION_POT=add_iu_pot, $
                                        ALSO_MSPH_SOURCECONE=also_msph_sourcecone, $
                                        USE_MSPH_SOURCE=use_msph_source, $
                                        SPNAME=spName, $
                                        OUT_SPNAME=out_spName, $
                                        ERROR_BAR_FACTOR=errorBarFac, $
                                        USEI__INCLUDE_POSCURRENT=useInds__include_posCurrent, $
                                        USEI__RELCHANGE=useInds__relChange, $
                                        FRACCHANGE_NDOWN=fracChange_NDown, $
                                        FRACCHANGE_JDOWN=fracChange_JDown, $
                                        FRACCHANGE_TDOWN=fracChange_TDown, $
                                        FRACERROR_NDOWN=fracError_NDown, $
                                        FRACERROR_JDOWN=fracError_JDown, $
                                        FRACERROR_TDOWN=fracError_TDown, $
                                        USE_FRACERROR_NDOWN=use_fracError_NDown, $
                                        USE_FRACERROR_JDOWN=use_fracError_JDown, $
                                        USE_FRACERROR_TDOWN=use_fracError_TDown, $
                                        USEI__TWOLUMPS=useInds__twoLumps, $
                                        MAX_TDOWN=max_TDown, $
                                        MIN_TDOWN=min_TDown, $
                                        MAX_NDOWN=max_NDown, $
                                        MIN_NDOWN=min_NDown, $
                                        TRANGES=tRanges, $
                                        MINPOT=minPot, $
                                        MAXPOT=maxPot, $
                                        MINCUR=minCur, $
                                        MAXCUR=maxCur, $
                                        USEINDS=useInds, $
                                        PLOT_J_RATIOS=plot_j_ratios, $
                                        IN_MAGCURRENT=magCurrent, $
                                        OUT_AVGS_FOR_FITTING=avgs_JVfit, $
                                        _EXTRA=e
                                   
  COMPILE_OPT IDL2,STRICTARRSUBS

  nListMem        = N_ELEMENTS(curPotList)         
  nHere           = N_ELEMENTS(curPotList[0].time)
  errorBarFac     = KEYWORD_SET(errorBarFac) ? errorBarFac : 1.

  looking         = nListMem
  ind             = 0

  ;;Use source cone stuff?
  have_sourceCone = 0
  IF N_ELEMENTS(also_msph_sourcecone) GT 0 THEN BEGIN
     IF (WHERE(also_msph_sourcecone))[0] NE -1 THEN BEGIN
        have_sourceCone = 1
     ENDIF
  ENDIF

  IF ~(KEYWORD_SET(use_charE_for_downPot) OR KEYWORD_SET(use_peakE_for_downPot)) THEN BEGIN
     use_charE_for_downPot = 1B
     use_peakE_for_downPot = 0B
  ENDIF

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

  IF ~(ARRAY_EQUAL(curPotList[0].time,curPotList[1].time) AND $
       ARRAY_EQUAL(curPotList[0].time,curPotList[2].time))    $
  THEN BEGIN
     IF (nHere NE N_ELEMENTS(curPotList[1].time)) OR $
        (nHere NE N_ELEMENTS(curPotList[2].time))    $
     THEN BEGIN
        PRINT,"Death!"
        STOP
     ENDIF

     IF ((WHERE(ABS(curPotList[0].time-curPotList[1].time) GT 1.))[0] NE -1) OR $
        ((WHERE(ABS(curPotList[0].time-curPotList[2].time) GT 1.))[0] NE -1)    $
     THEN BEGIN
        PRINT,"Whoa!"
        STOP
     ENDIF
  ENDIF

  ;;Time, the time
  time        = curPotList[edind].time
  tDiff       = (time-time[0])
  tMag        = tDiff/tDiff[-1]


  ;;Current for plotting
  IF ~(KEYWORD_SET(use_all_currents) OR KEYWORD_SET(use_ed_current) OR $
       KEYWORD_SET(use_eu_current  ) OR KEYWORD_SET(use_iu_current) OR $
       KEYWORD_SET(use_mag_current )) $
  THEN BEGIN
     use_all_currents = 1B
  ENDIF

  IF KEYWORD_SET(use_all_currents) THEN BEGIN

     cur      = curPotList[edind].cur+curPotList[euind].cur+curPotList[iuind].cur
     curErr   = [[curPotList[edind].curErr],[curPotList[euind].curErr],[curPotList[iuind].curErr]]

     je       = curPotList[edind].je+curPotList[euind].je+curPotList[iuind].je
     jeErr    = [[curPotList[edind].jeErr],[curPotList[euind].jeErr],[curPotList[iuind].jeErr]]

  ENDIF ELSE BEGIN

     cur      = MAKE_ARRAY(nHere,/DOUBLE,VALUE=0.D)
     curErr   = MAKE_ARRAY(nHere,nListMem,/DOUBLE,VALUE=0.D) ;You'll see why
     curErr_i = 0

     je       = MAKE_ARRAY(nHere,/DOUBLE,VALUE=0.D)
     jeErr    = MAKE_ARRAY(nHere,nListMem,/DOUBLE,VALUE=0.D) ;You'll see why
     jeErr_i  = 0

     CASE 1 OF
        KEYWORD_SET(use_mag_current): BEGIN
           ;; cur                  += (TEMPORARY(magCurrent)).y
           cur                     += magCurrent
        END
        ELSE: BEGIN

           IF KEYWORD_SET(use_ed_current) THEN BEGIN
              cur                  += curPotList[edind].cur
              curErr[*,curErr_i++]  = curPotList[edind].curErr

              je                   += curPotList[edind].je
              jeErr[*,jeErr_i++]    = curPotList[edind].jeErr
           ENDIF

           IF KEYWORD_SET(use_eu_current) THEN BEGIN
              cur                  += curPotList[euind].cur
              curErr[*,curErr_i++]  = curPotList[euind].curErr

              je                   += curPotList[euind].je
              jeErr[*,jeErr_i++]    = curPotList[euind].jeErr
           ENDIF

           IF KEYWORD_SET(use_iu_current) THEN BEGIN
              cur                  += curPotList[iuind].cur
              curErr[*,curErr_i++]  = curPotList[iuind].curErr

              je                   += curPotList[iuind].je
              jeErr[*,jeErr_i++]    = curPotList[iuind].jeErr
           ENDIF

           ;;Now square all participating jerent errors for each time, sum them, and take the square root
           curErr = curErr[*,0:(curErr_i-1)]
           jeErr  = jeErr[*,0:(jeErr_i-1)]

        END
     ENDCASE

     CASE NDIMEN(curErr) OF
        1: BEGIN

        END
        ELSE: BEGIN
           curErr = SQRT(TOTAL(curErr^2.D,2,/DOUBLE))
           jeErr  = SQRT(TOTAL(jeErr^2.D ,2,/DOUBLE))
        END
     ENDCASE

  ENDELSE

  ;;Errors
  ;; curErr      = ABS(curPotList[edind].curErr) * errorBarFac

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Et potential
  potErr      = MAKE_ARRAY(nHere,1+KEYWORD_SET(add_iu_pot),/DOUBLE,VALUE=0.D) ;You'll see why
  CASE 1 OF
     KEYWORD_SET(use_charE_for_downPot): BEGIN

        pot = curPotList[edind].charE

        ;;Nope, going to have to cop out for the time being
        potErr[*,0] = curPotList[edind].charEErr
        ;; potErr[*,0] = curPotList[edind].peakErr

        IF KEYWORD_SET(add_iu_pot) THEN BEGIN

           pot        += curPotList[iuind].charE

           ;; potErr[*,1] = curPotList[iuind].peakErr
           potErr[*,1] = curPotList[iuind].charEErr

        ENDIF

     END
     KEYWORD_SET(use_peakE_for_downPot): BEGIN

        pot = curPotList[edind].peakE

        potErr[*,0] = curPotList[edind].peakErr

        IF KEYWORD_SET(add_iu_pot) THEN BEGIN
           pot += curPotList[iuind].peakE

           potErr[*,1] = curPotList[iuind].peakErr
        ENDIF

     END
  ENDCASE

  IF KEYWORD_SET(add_iu_pot) THEN BEGIN

     ;;Now square all participating current errors for each time, sum them, and take the square root
     potErr             = SQRT(TOTAL(potErr^2.D,2,/DOUBLE))

  ENDIF

  ;;Calculate phiBar and phiBarError for upgoing and downgoing electrons
  phiBar_ed             = pot/curPotList[edind].T[3,*]
  phiBar_eu             = curPotList[euind].peakE/curPotList[euind].T[3,*]

  ERROR_PHIBAR,pot,curPotList[edind].T,curPotList[edind].N,potErr,curPotList[edind].errors,phiBarErr_ed
  ERROR_PHIBAR,pot,curPotList[euind].T,curPotList[euind].N,curPotList[euind].peakErr,curPotList[euind].errors,phiBarErr_eu

  phiBar                = phiBar_ed
  phiBarErr             = phiBarErr_ed

  ;;Where do we have positive current? Adjust according to Elphic et al. [1998].
  ;;That is, use the estimated peak energy from the upgoing electrons
  posC_i                = WHERE(cur GT 0,nPos, $
                                COMPLEMENT=negC_i, $
                                NCOMPLEMENT=nNeg)
  IF nPos GT 0 THEN BEGIN

     pot[posC_i]        = curPotList[euind].peakE[posC_i]
     potErr[posC_i]     = curPotList[euind].peakErr[posC_i]
     
     phiBar[posC_i]     = phiBar_eu[posC_i]         
     phiBarErr[posC_i]  = phiBarErr_eu[posC_i]         
  ENDIF

  ;; safe_i                = WHERE((curPotList[edind].peakE GE 0.) OR  $
  ;;                               (curPotList[euind].peakE GE 0.) OR  $
  ;;                               (curPotList[euind].peakE GE 0.),    $
  ;;                               nSafe)
  safe_i                = WHERE((curPotList[edind].peakE GE 0.) AND  $
                                (curPotList[euind].peakE GE 0.) AND  $
                                (curPotList[euind].peakE GE 0.) AND  $
                                ABS(curErr/cur) LE 5,     $
                                nSafe)

  ;; IF nSafe LT 3 THEN STOP

  ;; safe_i                = CGSETINTERSECTION(safe_i, $
  ;;                                           WHERE((curpotlist[0].n/curpotlist[0].n1 GT 3) OR $
  ;;                                                 (curpotlist[1].n/curpotlist[1].n1 GT 3) OR $
  ;;                                                 (curpotlist[2].n/curpotlist[2].n1 GT 3)))

  time_i                = WHERE(curPotList[edind].time GE t1 AND $
                                curPotList[edind].time LE t2,nTime)
  IF nTime LT 3 THEN STOP
  safe_i                = CGSETINTERSECTION(safe_i,time_i,COUNT=nSafe,NORESULT=-1)
  
  IF nSafe LT 3 THEN STOP

  jvPlotData            = {time       : time      [safe_i] , $
                           cur        : cur       [safe_i] , $
                           je         : je        [safe_i] , $
                           pot        : pot       [safe_i] , $
                           curErr     : curErr    [safe_i] , $
                           jeErr      : jeErr     [safe_i] , $
                           potErr     : potErr    [safe_i] , $
                           magCur     : magCurrent[safe_i] , $
                           tMag       : tMag      [safe_i] , $
                           tDiff      : tDiff     [safe_i] , $
                           phiBar     : phiBar    [safe_i] , $
                           phiBarErr  : phiBarErr [safe_i] , $
                           pBarAll    : {ed    : phiBar_ed[safe_i], $
                                         eu    : phiBar_eu[safe_i], $
                                         edErr : phiBarErr_ed[safe_i], $
                                         euErr : phiBarErr_eu[safe_i]}, $
                           NDown      : curPotList[edind].N[safe_i], $
                           NDownErr   : curPotList[edind].Nerr[safe_i], $
                           TDown      : REFORM(curPotList[edind].T[3,safe_i]), $
                           TDownErr   : curPotList[edind].Terr[safe_i], $
                           info       : {cur          : {ed  : KEYWORD_SET(use_ed_current) OR KEYWORD_SET(use_all_currents), $
                                                         eu  : KEYWORD_SET(use_eu_current) OR KEYWORD_SET(use_all_currents), $
                                                         iu  : KEYWORD_SET(use_iu_current) OR KEYWORD_SET(use_all_currents), $
                                                         mag : KEYWORD_SET(use_mag_current)}, $
                                         pot          : {chare : KEYWORD_SET(use_charE_for_downPot), $
                                                         peak_en : KEYWORD_SET(use_peakE_for_downPot), $
                                                         add_iu_pot : KEYWORD_SET(add_iu_pot)}}, $
                           use_source_avgs : KEYWORD_SET(use_msph_source) AND have_sourceCone}

  IF have_sourceCone THEN BEGIN

     source            = {cur        : curPotList[edind].source.cur    [safe_i] , $
                          curErr     : curPotList[edind].source.curErr [safe_i] , $
                          je         : curPotList[edind].source.je     [safe_i] , $
                          jeErr      : curPotList[edind].source.jeErr  [safe_i] , $
                          NDown      : curPotList[edind].source.N      [safe_i], $
                          NDownErr   : curPotList[edind].source.Nerr   [safe_i], $
                          TDown      : REFORM(curPotList[edind].source.T[3,safe_i]), $
                          TDownErr   : curPotList[edind].source.Terr   [safe_i]}

     STR_ELEMENT,jvPlotData,'source',TEMPORARY(source),/ADD_REPLACE

     ;; jvPlotData.use_source_avgs = 1B

  ENDIF


  useInds = GET_INDS_FOR_PLOT_THEORIE(JvPlotData, $
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
                                      TRANGES=tRanges)

  negcur_i                    = WHERE(jvPlotData.cur LE 0)
  negcur_i                    = negcur_i[SORT(jvPlotData.pot[negcur_i])]

  minPot                      = KEYWORD_SET(minPot) ? minPot : 0.D
  maxPot                      = KEYWORD_SET(maxPot) ? maxPot : 4000

  minCur                      = KEYWORD_SET(minCur) ? minCur : 1D-6
  maxCur                      = KEYWORD_SET(maxCur) ? maxCur : 1D-3


  IF N_ELEMENTS(useInds) EQ 0 THEN BEGIN
     ;;The points that have a clear affinity for kappa = 2
     thesepointslovekappa_ii  = WHERE((jvPlotData.pot[negcur_i] LE maxPot) AND $
                                      (jvPlotData.pot[negcur_i] GE minPot) AND $
                                      (jvPlotData.cur[negcur_i]*(-1D-6) GE minCur) AND $
                                      (jvPlotData.cur[negcur_i]*(-1D-6) LE maxCur),nLovers)
     PRINT,"THESE POINTS LOVE KAPPA=2.0"
     loveKappa_i              = negcur_i[thesepointslovekappa_ii]
     GET_STREAKS,loveKappa_i[SORT(loveKappa_i)],START_I=loveKappa_iStrt_ii,STOP_I=loveKappa_iStop_ii,OUT_STREAKLENS=streakLens
     times                    = TIME_TO_STR(jvPlotData.time[loveKappa_i[SORT(jvPlotData.time[loveKappa_i])]],/MS)
     FOR k=0,nLovers-1 DO BEGIN
        PRINT,TIME_TO_STR(jvPlotData.time[loveKappa_i[k]])
     ENDFOR

     ;; useInds               = negcur_i
     useInds = loveKappa_i

  ENDIF

  avgs_JVfit  = {useInds : useInds}

  useInds     = useInds[SORT(jvPlotData.pot[useInds])]
  
  fmtStr      = '(A0," (min, max,stdDev) ",T35,": ",F0.2," (",F0.2,", ",F0.2,", ",F0.2,")")'

  quantL      = LIST(jvPlotData.Tdown[useInds],jvPlotData.Ndown[useInds])
  navn        = ['T avg','N avg']
  sNavn       = ['T','N']
  IF KEYWORD_SET(ji_je_ratio) THEN BEGIN
     quantL.Add,ji_je_ratio[useInds]
     navn     = [navn,'Ji/Je avg']
     sNavn    = [sNavn,'JiJeRat']
  ENDIF
  
  IF have_sourceCone THEN BEGIN
     quantL.Add,jvPlotData.source.Tdown[useInds]
     quantL.Add,jvPlotData.source.Ndown[useInds]
     navn  = [navn,'T_SC avg','N_SC avg']
     sNavn = [sNavn,'T_SC'   ,'N_SC'    ]
  ENDIF



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

     STR_ELEMENT,avgs_JVfit,'use_source_avgs',jvPlotData.use_source_avgs,/ADD_REPLACE

  ENDFOR


END
