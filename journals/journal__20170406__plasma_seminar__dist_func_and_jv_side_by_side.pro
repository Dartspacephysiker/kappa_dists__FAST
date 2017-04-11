;2017/04/06
PRO JOURNAL__20170406__PLASMA_SEMINAR__DIST_FUNC_AND_JV_SIDE_BY_SIDE

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;;Curve params
  N        = 0.1               ;cm^-3
  T        = 500                ;eV
  R_B      = 1000
  E_b      = 1D3                ;Bulk energy

  kappas   = [0,5,2.5,2.0,1.8,1.6]
  nKappa   = N_ELEMENTS(kappas)

  potBarTop = 2D3
  increm    = 1.1D
  nPot      = FIX(ALOG10(potBarTop*T)/ALOG10(increm))
  pot       = increm^(INDGEN(nPot)) ;in energy units ( charge*potential )


  Pthing   = [E_b,T,0,N,0]      ; E_b, T, kappa, n, bulkAngle

  ;; distUnits = 'eFlux'
  distUnits = 'flux' ;nflux, that is


  IF N_ELEMENTS(N  ) EQ 1 THEN N   = REPLICATE(N  ,nKappa)
  IF N_ELEMENTS(T  ) EQ 1 THEN T   = REPLICATE(T  ,nKappa)
  IF N_ELEMENTS(R_B) EQ 1 THEN R_B = REPLICATE(R_B,nKappa)

  ;;Plot opts, things
  outPlotPref = 'kappa_seminar__' + GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + '_'
  SET_PLOT_DIR,plotDir,/FOR_KAPPA_DB,/ADD_TODAY
  save_eps  = 0
  theorPArr = MAKE_ARRAY(nKappa,/OBJ)
  distFPArr = MAKE_ARRAY(nKappa,/OBJ)

  lStyles   = ['-','--','-:',':','__','-']
  colors    = ['black','purple','blue','dark green','orange','red']

  distFName = 'Maxwellian'
  ;; FOR k=0,nKappa-1 DO distFName = [distFName,STRING(FORMAT='(A0,F0.2)','$/kappa$ = ',kappas[k])]
  muLetter = '!4l!X'
  FOR k=1,nKappa-1 DO distFName = [distFName,STRING(FORMAT='(A0,F0.2)',muLetter+' = ',kappas[k])]


  ;;Get datas first
  curArr   = !NULL
  distFArr = !NULL
  FOR k=0,nKappa-1 DO BEGIN

     Pthing[2]      = kappas[k]

     CASE kappas[k] OF
        0: BEGIN
           tmpCur   = KNIGHT_RELATION__DORS_KLETZING_4(T[k],N[k],pot,R_B[k], $
                                                       OUT_POTBAR=potBar, $
                                                       /NO_MULT_BY_CHARGE)

           tmpDistF = MAXWELL_FLUX__FUNC(pot,Pthing, $
                                         UNITS=distUnits)
        END
        ELSE: BEGIN
           tmpCur = KNIGHT_RELATION__DORS_KLETZING_11(kappas[k],T[k],N[k],pot,R_B[k], $
                                                       OUT_POTBAR=potBar, $
                                                      /NO_MULT_BY_CHARGE)

           tmpDistF = KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__CONV_TO_F__FUNC(pot,Pthing, $
                                                                             UNITS=distUnits)
        END
     ENDCASE

     curArr   = [curArr,TRANSPOSE(tmpCur)]
     distFArr = [distFArr,TRANSPOSE(tmpDistF)]

  ENDFOR


  window          = WINDOW(DIMENSIONS=[1000,750],BUFFER=KEYWORD_SET(save_eps))

  nCol            = 2
  nRow            = 1

  prem            = 1
  deux            = 2
  
  theorieInd      = deux
  distFInd        = prem
  theorLayout     = [nCol,nRow,theorieInd]
  distFLayout     = [nCol,nRow,distFInd]

  distFPosition   = [0.09,0.08,0.485,0.92]
  theorPosition   = [0.595,0.08,0.99,0.92]

  distFPeakVal    = MAX(distFArr)
  distFRange      = [10.D^(ALOG10(distFPeakVal)-7D),10.D^(ALOG10(distFPeakVal)+0.2D)]

  ;; energyRange  = [1,MAX(pot)]
  energyRange     = [1,4D4]

  potBarRange     = [0.1,MAX(potBar)]
  curRange        = MINMAX(curArr)*1D6

  potLog          = 1
  energyLog       = 1
  curLog          = 1
  distFLog        = 1

  font_size       = 16
  
  CASE STRUPCASE(distUnits) OF
     'EFLUX': BEGIN
        pPref          = '-eFlux_fit'
        unitTitle      = "e!U-!N energy flux"
        fluxTitle      = "Differential Energy Flux (eV/cm!U2!N-sr-s)"
        lowerBound     = 1.0e5
        upperBound     = 1.0e10
     END
     'FLUX':BEGIN
        pPref          = '-nFlux_fit'
        unitTitle      = "e!U-!N # flux"
        fluxTitle      = "Differential Number Flux (#/cm!U2!N-sr-s)"        
        lowerBound     = 1.0e1
        upperBound     = 1.0e7
     END
  ENDCASE

  ;;j-v curves
  FOR k=0,nKappa-1 DO BEGIN


     CASE k OF
        0: BEGIN

           theorPArr[k] = PLOT(potBar,TRANSPOSE(curArr[k,*])*1D6, $
                               XTITLE='e $\phi$ / k!DB!NT', $
                               YTITLE='Current Density ($\mu$A/m!U2!N)', $
                               XLOG=potLog, $
                               YLOG=curLog, $
                               XRANGE=potBarRange, $
                               YRANGE=curRange, $
                               XSTYLE=1, $
                               XTICKFORMAT='exponentlabel', $
                               YTICKFORMAT='exponentlabel', $
                               XTICKLEN=1, $
                               XSUBTICKLEN=0.01, $
                               ;; XGRIDSTYLE=':', $
                               XGRIDSTYLE=[3,'ED6E'X], $
                               YTICKLEN=1, $
                               YSUBTICKLEN=0.01, $
                               ;; XGRIDSTYLE=':', $
                               YGRIDSTYLE=[3,'ED6E'X], $
                               COLOR=colors[k], $
                               LINESTYLE=lStyles[k], $
                               FONT_SIZE=font_size, $
                               ;; LAYOUT=theorLayout, $
                               OVERPLOT=k NE 0, $
                               POSITION=theorPosition, $
                               /CURRENT)

        END
        ELSE: BEGIN

           theorPArr[k] = PLOT(potBar,TRANSPOSE(curArr[k,*])*1D6, $
                               COLOR=colors[k], $
                               LINESTYLE=lStyles[k], $
                               ;; LAYOUT=theorLayout, $
                               POSITION=theorPosition, $
                               /OVERPLOT, $
                               /CURRENT)

        END
     ENDCASE


  ENDFOR

  ;;distF curves
  FOR k=0,nKappa-1 DO BEGIN


     CASE k OF
        0: BEGIN

           ;;energy style
           distFPArr[k] = PLOT(pot,TRANSPOSE(distFArr[k,*]), $
                               NAME=distFName[k], $
                               XTITLE='Energy (eV)', $
                               YTITLE=fluxTitle, $
                               XLOG=energyLog, $
                               YLOG=distFLog, $
                               XRANGE=energyRange, $
                               YRANGE=distFRange, $
                               XTICKFORMAT='exponentlabel', $
                               YTICKFORMAT='exponentlabel', $
                               COLOR=colors[k], $
                               LINESTYLE=lStyles[k], $
                               FONT_SIZE=font_size, $
;                               ;; LAYOUT=distFLayout, $
                               POSITION=distFPosition, $
                               OVERPLOT=k NE 0, $
                               /CURRENT)

        END
        ELSE: BEGIN

           ;;energy style
           distFPArr[k] = PLOT(pot,TRANSPOSE(distFArr[k,*]), $
                               COLOR=colors[k], $
                               LINESTYLE=lStyles[k], $
                               ;; LAYOUT=distFLayout, $
                               POSITION=distFPosition, $
                               /OVERPLOT, $
                               /CURRENT)


        END
     ENDCASE


  ENDFOR

  legend = LEGEND(TARGET=distFPArr, $
                  POSITION=[0.01,0.8], $
                  /NORMAL)

  IF KEYWORD_SET(save_eps) THEN BEGIN

  ENDIF

  STOP

END
