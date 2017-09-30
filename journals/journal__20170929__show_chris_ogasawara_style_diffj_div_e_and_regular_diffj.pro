;; 2017/09/29
;; The idea is that Chris is not sure why our plots of the kappa distribution
;; don't look quite like a power law. Ogasawara et al. [2017] curiously
;; plot (differential J)/(E), so I'm trying it here.
;; Want something like Dors and Kletzing fig 1? Try this
PRO JOURNAL__20170929__SHOW_CHRIS_OGASAWARA_STYLE_DIFFJ_DIV_E_AND_REGULAR_DIFFJ, $
   TRUNCATE_AT=truncate_at, $
   SAVE_EPS=save_eps, $
   UNITS=units, $
   DIVIDE_DIST_BY_E=divideByE, $
   POTLOG=potLog, $
   ENERGYLOG=energyLog, $
   DISTFLOG=distFLog, $
   DISTFRANGE=distFRange, $
   DKFIG1=DKFig1, $
   LOGDKFIG1=LogDKFig1, $
   ;; YLOG=yLog, $
   ENERGYRANGE=eRange, $
   NORM_E_BY_T=norm_E_by_T

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;;Curve params
  N        = 0.1D              ;cm^-3
  T        = 500                ;eV
  E_b      = 1D3                ;Bulk energy

  CASE 1 OF
     KEYWORD_SET(DKFig1): BEGIN
        N        = 1.0D         ;cm^-3
        T        = 500          ;eV
        E_b      = 0.D          ;Bulk energy

        units    = 'DFSTD'     
        norm_E_by_T = 1
        eRange   = [0,10]
        distFRange = [1.D-21,1.D-15]
        energyLog = 0
     END
     KEYWORD_SET(LogDKFig1): BEGIN
        N        = 1.0D         ;cm^-3
        T        = 500          ;eV
        E_b      = 0.D          ;Bulk energy

        units    = 'DFSTD'     
        norm_E_by_T = 1
        eRange   = [0.01,1E3]
        distFRange = [1.D-30,1.D-15]
        energyLog = 1
     END
     ELSE: 
  ENDCASE
  DKFiggin = KEYWORD_SET(DKFig1) OR KEYWORD_SET(LogDKFig1)
  ;; E_b      = 0                ;Bulk energy

  ;; R_B      = 15
  ;; RBSuff   = " (src ~1-2 R!D E!N)"
  ;; R_B      = 1000
  ;; RBSuff   = " (src ~13-15 R!D E!N)"
  R_B      = 100
  RBSuff   = " (src ~5 R!D E!N)"

  kappas   = [0,5,3.0,2.0,1.8,1.6] ;kappa = 0 means "plot Maxwellian" to this pro
  nKappa   = N_ELEMENTS(kappas)

  ;; potBarTop = 2D4
  potBarTop = 1D5
  increm    = 1.1D
  nPot      = FIX(ALOG10(potBarTop*T)/ALOG10(increm))
  pot       = increm^(INDGEN(nPot)) ;in energy units ( charge*potential )


  Pthing   = [E_b,T,0,N,0]      ; E_b, T, kappa, n, bulkAngle

  ;; distUnits = 'eFlux'
  distUnits = N_ELEMENTS(units) GT 0 ? units : 'flux' ;nflux, that is
  ;; divideByE = 1

  IF N_ELEMENTS(N  ) EQ 1 THEN N   = REPLICATE(N  ,nKappa)
  IF N_ELEMENTS(T  ) EQ 1 THEN T   = REPLICATE(T  ,nKappa)
  IF N_ELEMENTS(R_B) EQ 1 THEN R_B = REPLICATE(R_B,nKappa)

  ;;Plot opts, things
  outPlotPref = 'kappa_seminar__dist_JV_'
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

     IF KEYWORD_SET(divideByE) THEN BEGIN
        tmpDistF /= pot
     ENDIF

     curArr   = [curArr,TRANSPOSE(tmpCur)]
     distFArr = [distFArr,TRANSPOSE(tmpDistF)]

  ENDFOR

  IF KEYWORD_SET(DKFiggin) THEN BEGIN

     nCol            = 1
     nRow            = 1

     prem            = 1
     deux            = 2
     
     distFInd        = prem

     distFLayout     = [nCol,nRow,distFInd]
     distFPosition   = [0.12,0.08,0.95,0.92]

     distFPeakVal    = MAX(distFArr)
     distFRange      = N_ELEMENTS(distFRange) GT 0 ? distFRange : [10.D^(ALOG10(distFPeakVal)-7D),10.D^(ALOG10(distFPeakVal)+0.2D)]

     ;; energyRange  = [1,MAX(pot)]
     ;; energyRange     = [1,4D4]
     energyRange     = KEYWORD_SET(eRange) ? eRange : [1,1D5]





     potLog          = N_ELEMENTS(potLog) GT 0 ? potLog : 1
     eLog            = N_ELEMENTS(energyLog) GT 0 ? energyLog : 1

     distFLog        = N_ELEMENTS(distFLog) GT 0 ? distFLog : 1

     font_size       = 16
     
     truncate_at     = KEYWORD_SET(truncate_at) ? truncate_at : 6

     junk            = MIN(ABS(potBar-10),kbTeq10Ind)


     distFName = 'Maxwellian'
     FOR k=1,nKappa-1 DO distFName = [distFName,STRING(FORMAT='(A0,F0.1)','$\kappa$=',kappas[k])]

     CASE STRUPCASE(distUnits) OF
        'EFLUX': BEGIN
           pPref          = '-eFlux_fit'
           unitTitle      = "e!U-!N energy flux"
           IF KEYWORD_SET(divideByE) THEN BEGIN
              fluxTitle   = "(dJ_E/dE)/E ( ( eV/cm!U2!N-sr-s) / E)"
           ENDIF ELSE BEGIN
              fluxTitle   = "Differential Energy Flux (eV/cm!U2!N-s-sr-eV)"
           ENDELSE
           lowerBound     = 1D-21
           upperBound     = 1D-15
        END
        'FLUX':BEGIN
           pPref          = '-nFlux_fit'
           unitTitle      = "e!U-!N # flux"
           IF KEYWORD_SET(divideByE) THEN BEGIN
              fluxTitle   = "(dJ/dE)/E ( ( #/cm!U2!N-sr-s) / E)"
           ENDIF ELSE BEGIN
              fluxTitle   = "Differential Number Flux (#/cm!U2!N-s-sr-eV)"        
           ENDELSE
           lowerBound     = 1.0e1
           upperBound     = 1.0e7
        END
        'DFSTD':BEGIN
           pPref          = '-dfStd_fit'
           unitTitle      = "e!U-!N # flux"
           IF KEYWORD_SET(divideByE) THEN BEGIN
              fluxTitle   = "(df)/E ( ( #/cm!U2!N-sr-s) / E)"
           ENDIF ELSE BEGIN
              fluxTitle   = "Phase space density (s!U3!Nm!U-6!N)"
           ENDELSE
           lowerBound     = 1.0e1
           upperBound     = 1.0e7
        END
     ENDCASE

     ;;Winder

     titleString  = STRING(FORMAT='("N = ",F0.2," cm!U-3!N, T = ",I0," eV")', $
                           N[0],T[0])
     window          = WINDOW(DIMENSIONS=[1000,750],BUFFER=KEYWORD_SET(save_eps), $
                              TITLE=titleString, $
                              FONT_SIZE=font_size*1.4)

     ;;distF curves
     FOR k=0,(nKappa < truncate_at)-1 DO BEGIN

        x = KEYWORD_SET(norm_E_by_T) ? pot/T[0] : pot
        xTitle = KEYWORD_SET(norm_E_by_T) ? 'E / (k!DB!NT)' : 'Energy (eV)'

        CASE k OF
           0: BEGIN

              ;;energy style
              distFPArr[k] = PLOT(x,TRANSPOSE(distFArr[k,*]), $
                                  NAME=distFName[k], $
                                  XTITLE=xTitle, $
                                  YTITLE=fluxTitle, $
                                  XLOG=eLog, $
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
              distFPArr[k] = PLOT(x,TRANSPOSE(distFArr[k,*]), $
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

     legend = LEGEND(TARGET=distFPArr[0:(truncate_at-1)], $
     ;; legend = LEGEND(TARGET=PArr[0:(truncate_at-1)], $
                     POSITION=[0.33,0.355], $
                     FONT_SIZE=font_size*0.9, $
                     /NORMAL)


  ENDIF ELSE BEGIN

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
     distFRange      = N_ELEMENTS(distFRange) GT 0 ? distFRange : [10.D^(ALOG10(distFPeakVal)-7D),10.D^(ALOG10(distFPeakVal)+0.2D)]

     ;; energyRange  = [1,MAX(pot)]
     ;; energyRange     = [1,4D4]
     energyRange     = KEYWORD_SET(eRange) ? eRange : [1,1D5]

     potBarRange     = [0.1,MAX(potBar)]
     curRange        = MINMAX(curArr[*,WHERE(potBar GE potBarRange[0])])*1D6
     curRange[1]    *= 1.1

     potLog          = N_ELEMENTS(potLog) GT 0 ? potLog : 1
     eLog            = N_ELEMENTS(energyLog) GT 0 ? energyLog : 1
     curLog          = 1
     distFLog        = N_ELEMENTS(distFLog) GT 0 ? distFLog : 1

     font_size       = 16
     
     truncate_at     = KEYWORD_SET(truncate_at) ? truncate_at : 6

     junk            = MIN(ABS(potBar-10),kbTeq10Ind)
     prosjent        = (curArr[*,kbTeq10Ind]-curArr[0,kbTeq10Ind])/curArr[0,kbTeq10Ind]

     distFName = 'Maxw(+0%)'
     FOR k=1,nKappa-1 DO distFName = [distFName,STRING(FORMAT='(A0,F0.1," (+",I0,"%)")','$\kappa$=',kappas[k],100.*prosjent[k])]

     CASE STRUPCASE(distUnits) OF
        'EFLUX': BEGIN
           pPref          = '-eFlux_fit'
           unitTitle      = "e!U-!N energy flux"
           IF KEYWORD_SET(divideByE) THEN BEGIN
              fluxTitle   = "(dJ_E/dE)/E ( ( eV/cm!U2!N-sr-s) / E)"
           ENDIF ELSE BEGIN
              fluxTitle   = "Differential Energy Flux (eV/cm!U2!N-s-sr-eV)"
           ENDELSE
           lowerBound     = 1D-21
           upperBound     = 1D-15
        END
        'FLUX':BEGIN
           pPref          = '-nFlux_fit'
           unitTitle      = "e!U-!N # flux"
           IF KEYWORD_SET(divideByE) THEN BEGIN
              fluxTitle   = "(dJ/dE)/E ( ( #/cm!U2!N-sr-s) / E)"
           ENDIF ELSE BEGIN
              fluxTitle   = "Differential Number Flux (#/cm!U2!N-s-sr-eV)"        
           ENDELSE
           lowerBound     = 1.0e1
           upperBound     = 1.0e7
        END
        'DFSTD':BEGIN
           pPref          = '-dfStd_fit'
           unitTitle      = "e!U-!N # flux"
           IF KEYWORD_SET(divideByE) THEN BEGIN
              fluxTitle   = "(df)/E ( ( #/cm!U2!N-sr-s) / E)"
           ENDIF ELSE BEGIN
              fluxTitle   = "Phase space density (s!U3!Nm!U-6!N)"
           ENDELSE
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

              theorPArr[k] = PLOT(potBar,TRANSPOSE(curArr[k,*])*1D6, $
                                  NAME=distFName[k], $
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
                                  THICK=thick[k], $
                                  FONT_SIZE=font_size, $
                                  ;; LAYOUT=theorLayout, $
                                  OVERPLOT=k NE 0, $
                                  POSITION=theorPosition, $
                                  /CURRENT)

           END
           ELSE: BEGIN

              theorPArr[k] = PLOT(potBar,TRANSPOSE(curArr[k,*])*1D6, $
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

        x = KEYWORD_SET(norm_E_by_T) ? pot/T[0] : pot
        xTitle = KEYWORD_SET(norm_E_by_T) ? 'E / (k!DB!NT)' : 'Energy (eV)'

        CASE k OF
           0: BEGIN

              ;;energy style
              distFPArr[k] = PLOT(x,TRANSPOSE(distFArr[k,*]), $
                                  NAME=distFName[k], $
                                  XTITLE=xTitle, $
                                  YTITLE=fluxTitle, $
                                  XLOG=eLog, $
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
              distFPArr[k] = PLOT(x,TRANSPOSE(distFArr[k,*]), $
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
                     POSITION=[0.33,0.355], $
                     FONT_SIZE=font_size*0.9, $
                     /NORMAL)

  ENDELSE

  IF KEYWORD_SET(save_eps) THEN BEGIN

     filNavn = outPlotPref + STRING(FORMAT='(I02,"_RB",I0)',truncate_at,R_B[0]) + '.eps'
     
     PRINT,"Saving " + filNavn + ' ...'
     
     window.Save,plotDir+filNavn
     window.Close
     window = !NULL

  ENDIF

END

