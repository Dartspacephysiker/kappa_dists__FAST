;2017/04/06
PRO JOURNAL__20170418__DIST_FUNC_AND_JE_V_SIDE_BY_SIDE, $
   TRUNCATE_AT=truncate_at, $
   SAVE_EPS=save_eps

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;;Curve params
  N        = 0.1               ;cm^-3
  T        = 500                ;eV
  E_b      = 1D3                ;Bulk energy

  ;; R_B      = 15
  ;; RBSuff   = " (src ~1-2 R!D E!N)"
  R_B      = 1000
  RBSuff   = " (src ~13-15 R!D E!N)"

  kappas   = [0,5,2.5,1.9,1.8,1.6]
  nKappa   = N_ELEMENTS(kappas)

  potBarTop = 2D4
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
  outPlotPref = 'kappa__dist_Je_V_'
  SET_PLOT_DIR,plotDir,/FOR_KAPPA_DB,/ADD_TODAY
  IF N_ELEMENTS(save_eps) EQ 0 THEN save_eps = 0
  theorPArr = MAKE_ARRAY(nKappa,/OBJ)
  distFPArr = MAKE_ARRAY(nKappa,/OBJ)

  lStyles   = ['-','--','-:',':','__','-']
  colors    = ['black','purple','blue','dark green','orange','red']
  thick     = REPLICATE(2.0,nKappa)

  ;; muLetter = '!4l!X'
  ;; muLetter = 'kappa!Z(03BA)!3'
  ;; FOR k=1,nKappa-1 DO distFName = [distFName,STRING(FORMAT='(A0,F0.2)',muLetter+' = ',kappas[k])]

  ;;Get datas first
  jeArr   = !NULL
  distFArr = !NULL
  FOR k=0,nKappa-1 DO BEGIN

     Pthing[2]      = kappas[k]

     CASE kappas[k] OF
        0: BEGIN

           tmpJe = KAPPA_1__DORS_KLETZING_EQ_14__EFLUX__MAXWELL(T[k],N[k],pot,R_B[k], $
                                                             OUT_POTBAR=potBar)
           ;; tmpJe   = KNIGHT_RELATION__DORS_KLETZING_4(T[k],N[k],pot,R_B[k], $
           ;;                                             OUT_POTBAR=potBar, $
           ;;                                             /NO_MULT_BY_CHARGE)

           tmpDistF = MAXWELL_FLUX__FUNC(pot,Pthing, $
                                         UNITS=distUnits)
        END
        ELSE: BEGIN
           ;; IF kappas[k] EQ 2.0 THEN STOP
           tmpJe = KAPPA_1__DORS_KLETZING_EQ_15__EFLUX(kappas[k],T[k],N[k],pot,R_B[k], $
                                                    OUT_POTBAR=potBar, $
                                                    MASS=mass)
           ;; tmpJe = KNIGHT_RELATION__DORS_KLETZING_11(kappas[k],T[k],N[k],pot,R_B[k], $
           ;;                                             OUT_POTBAR=potBar, $
           ;;                                            /NO_MULT_BY_CHARGE)

           tmpDistF = KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__CONV_TO_F__FUNC(pot,Pthing, $
                                                                             UNITS=distUnits)
        END
     ENDCASE

     jeArr   = [jeArr,TRANSPOSE(tmpJe)]
     distFArr = [distFArr,TRANSPOSE(tmpDistF)]

  ENDFOR

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
  jeRange        = MINMAX(jeArr[*,WHERE(potBar GE potBarRange[0])])*1D6
  jeRange[1]    *= 1.1

  potLog          = 1
  energyLog       = 1
  jeLog          = 1
  distFLog        = 1

  font_size       = 16
  
  truncate_at     = KEYWORD_SET(truncate_at) ? truncate_at : 6

  junk            = MIN(ABS(potBar-10),kbTeq10Ind)
  prosjent        = (jeArr[*,kbTeq10Ind]-jeArr[0,kbTeq10Ind])/jeArr[0,kbTeq10Ind]

  distFName = 'Maxw(+0%)'
  FOR k=1,nKappa-1 DO distFName = [distFName,STRING(FORMAT='(A0,F0.1," (+",I0,"%)")','$\kappa$=',kappas[k],100.*prosjent[k])]

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

  ;;Winder
  titleString     = STRING(FORMAT='("N = ",F0.2," cm!U-3!N, T = ",I0," eV, R!DB!N = ",F0.1,A0)', $
                           N[0],T[0],R_B[0],RBSuff)
  window          = WINDOW(DIMENSIONS=[1000,750],BUFFER=KEYWORD_SET(save_eps), $
                           TITLE=titleString, $
                           FONT_SIZE=font_size*1.4)

  ;;j-v curves
  FOR k=0,(nKappa < truncate_at)-1 DO BEGIN


     CASE k OF
        0: BEGIN

           theorPArr[k] = PLOT(potBar,TRANSPOSE(jeArr[k,*])*1D6, $
                               NAME=distFName[k], $
                               XTITLE='e $\phi$ / k!DB!NT', $
                               YTITLE='Energy flux (mW/m!U2!N)', $
                               XLOG=potLog, $
                               YLOG=jeLog, $
                               XRANGE=potBarRange, $
                               YRANGE=jeRange, $
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
                               THICK=thick[k], $
                               FONT_SIZE=font_size, $
                               ;; LAYOUT=theorLayout, $
                               OVERPLOT=k NE 0, $
                               POSITION=theorPosition, $
                               /CURRENT)

        END
        ELSE: BEGIN

           theorPArr[k] = PLOT(potBar,TRANSPOSE(jeArr[k,*])*1D6, $
                               NAME=distFName[k], $
                               COLOR=colors[k], $
                               LINESTYLE=lStyles[k], $
                               THICK=thick[k], $
                               ;; LAYOUT=theorLayout, $
                               POSITION=theorPosition, $
                               /OVERPLOT, $
                               /CURRENT)

        END
     ENDCASE


  ENDFOR

  ;;distF curves
  FOR k=0,(nKappa < truncate_at)-1 DO BEGIN


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
                               THICK=thick[k], $
                               FONT_SIZE=font_size, $
;                               ;; LAYOUT=distFLayout, $
                               POSITION=distFPosition, $
                               OVERPLOT=k NE 0, $
                               /CURRENT)

        END
        ELSE: BEGIN

           ;;energy style
           distFPArr[k] = PLOT(pot,TRANSPOSE(distFArr[k,*]), $
                               NAME=distFName[k], $
                               COLOR=colors[k], $
                               LINESTYLE=lStyles[k], $
                               THICK=thick[k], $
                               ;; LAYOUT=distFLayout, $
                               POSITION=distFPosition, $
                               /OVERPLOT, $
                               /CURRENT)


        END
     ENDCASE


  ENDFOR

  ;; legend = LEGEND(TARGET=distFPArr[0:(truncate_at-1)], $
  legend = LEGEND(TARGET=theorPArr[0:(truncate_at-1)], $
                  POSITION=[0.41,0.355], $
                  FONT_SIZE=font_size*0.9, $
                  /NORMAL)

  IF KEYWORD_SET(save_eps) THEN BEGIN

     filNavn = outPlotPref + STRING(FORMAT='(I02,"_RB",I0)',truncate_at,R_B[0]) + '.eps'
     
     PRINT,"Saving " + filNavn + ' ...'
     
     window.Save,plotDir+filNavn
     window.Close
     window = !NULL

  ENDIF

END

