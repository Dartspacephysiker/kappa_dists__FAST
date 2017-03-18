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
                                        USE_CHAR_EN_FOR_DOWNPOT=use_charE_for_downPot, $
                                        USE_PEAK_EN_FOR_DOWNPOT=use_peakE_for_downPot, $
                                        ADD_UPGOING_ION_POT=add_iu_pot, $
                                        SPNAME=spName, $
                                        OUT_SPNAME=out_spName, $
                                        ERROR_BAR_FACTOR=errorBarFac
                                   
  COMPILE_OPT IDL2,STRICTARRSUBS

  nListMem        = N_ELEMENTS(curPotList)         
  nHere           = N_ELEMENTS(curPotList[0].time)
  errorBarFac     = KEYWORD_SET(errorBarFac) ? errorBarFac : 1.

  looking         = nListMem
  ind             = 0

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
       KEYWORD_SET(use_eu_current) OR KEYWORD_SET(use_iu_current))     $
  THEN BEGIN
     use_all_currents = 1B
  ENDIF

  IF KEYWORD_SET(use_all_currents) THEN BEGIN

     cur      = curPotList[edind].cur+curPotList[euind].cur+curPotList[iuind].cur
     curErr   = [[curPotList[edind].curErr],[curPotList[euind].curErr],[curPotList[iuind].curErr]]

  ENDIF ELSE BEGIN

     cur      = MAKE_ARRAY(nHere,/DOUBLE,VALUE=0.D)
     curErr   = MAKE_ARRAY(nHere,nListMem,/DOUBLE,VALUE=0.D) ;You'll see why
     curErr_i = 0

     IF KEYWORD_SET(use_ed_current) THEN BEGIN
        cur  += curPotList[edind].cur
        curErr[*,curErr_i++] = curPotList[edind].curErr
     ENDIF

     IF KEYWORD_SET(use_eu_current) THEN BEGIN
        cur  += curPotList[euind].cur
        curErr[*,curErr_i++] = curPotList[euind].curErr
     ENDIF

     IF KEYWORD_SET(use_iu_current) THEN BEGIN
        cur  += curPotList[iuind].cur
        curErr[*,curErr_i++] = curPotList[iuind].curErr
     ENDIF

     ;;Now square all participating current errors for each time, sum them, and take the square root
     curErr = curErr[*,0:(curErr_i-1)]
     curErr = SQRT(TOTAL(curErr^2.D,2,/DOUBLE))

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
                                (curPotList[euind].peakE GE 0.),     $
                                nSafe)

  IF nSafe LT 3 THEN STOP

  safe_i                = CGSETINTERSECTION(safe_i, $
                                            WHERE((curpotlist[0].n/curpotlist[0].n1 GT 3) OR $
                                                  (curpotlist[1].n/curpotlist[1].n1 GT 3) OR $
                                                  (curpotlist[2].n/curpotlist[2].n1 GT 3)))

  time_i                = WHERE(curPotList[edind].time GE t1 AND $
                                curPotList[edind].time LE t2,nTime)
  IF nTime LT 3 THEN STOP
  safe_i                = CGSETINTERSECTION(safe_i,time_i,COUNT=nSafe,NORESULT=-1)
  
  IF nSafe LT 3 THEN STOP

  jvPlotData            = {time       : time      [safe_i] , $
                           cur        : cur       [safe_i] , $
                           pot        : pot       [safe_i] , $
                           curErr     : curErr    [safe_i] , $
                           potErr     : potErr    [safe_i] , $
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
                           TDownErr   : curPotList[edind].Terr[safe_i]}

END
