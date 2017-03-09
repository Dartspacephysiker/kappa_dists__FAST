;;11/10/16
PRO KAPPA_FIT_STATISTICS,kappa2d,gauss2d,potStruct, $
                         LOAD_FROM_THIS_FILE=loadFile, $
                         SAVE_PLOT=save_plot, $
                         FA_CONDUCTANCE_PLOT=fa_conductance_plot, $
                         USE_TOTAL_J_AND_V=use_total_J_and_V, $
                         COMBINE_STATS_FROM_EACH_TIME=combine_stats_from_each_time, $
                         CHITHRESH=chiThresh, $
                         BUFFER=buffer

  COMPILE_OPT IDL2,STRICTARRSUBS

  defChiThresh = 5.0

  IF N_ELEMENTS(chiThresh) EQ 0 THEN chiThresh = defChiThresh

  kappaColor   = 'Blue'
  kappaSym     = '*'
  kappaLSty    = '--'
  gaussColor    = 'Red'
  gaussSym      = '+'
  gaussLSty     = '-'

  firstSym     = 'tu'
  secondSym    = 'd'

  secondColor  = 'Purple'
  firstColor   = 'Gray'

  coulour      = ['Purple','Gray','Brown','Pink']
  symbole      = ['tu','d','*','+']

  sym_size     = 1.3
  sym_thick    = 1.5
  sym_transp   = 50

  axisFontSize = 16
  legFontSize  = 14

  statIIList = LIST()
  CVNameList = LIST()
  IF N_ELEMENTS(loadFile) EQ 0 THEN BEGIN
     loadFile  = '/home/spencerh/software/sdt/batch_jobs/saves_output_etc/20161110--orbit_1773--potential_drop__kappa_interp.sav'
     use_money_event = 1
     orbString = '1773'
  ENDIF

  
  RESTORE,loadFile

  chi2Red = kappa2D.chi2/(kappa2D.dof+kappa2D.nFree)

  statI   = WHERE(chi2Red LE chiThresh,nStat)

  IF nStat LT 3 THEN STOP

  ;;If user said nothing, then we're using the moniest event that we possibly can
  IF KEYWORD_SET(use_money_event) THEN BEGIN
     firstHalfEnd    = STR_TO_TIME('1997-02-01/09:26:23')
     secondHalfStart = STR_TO_TIME('1997-02-01/09:26:53')

     statIIList.Add,WHERE(kappa2d.sdt[stati].time LE firstHalfEnd)
     statIIList.Add,WHERE(kappa2d.sdt[stati].time GE secondHalfStart)

     CVnameList.Add,STRMID(TIME_TO_STR(kappa2d.sdt[stati[statIIList[0]]].time),14,5)
     CVnameList.Add,STRMID(TIME_TO_STR(kappa2d.sdt[stati[statIIList[1]]].time),14,5)
     CVnameList[0] = (CVnameList[0])[0] + ' - ' + (CVnameList[0])[-1]
     CVnameList[1] = (CVnameList[1])[0] + ' - ' + (CVnameList[1])[-1]

     nCVDatPlots   = N_ELEMENTS(CVnameList)

     xRange        = [0.1,200]

  ENDIF ELSE BEGIN

     IF N_ELEMENTS(startStop_t) GT 0 THEN BEGIN

        dims = SIZE(startStop_t,/DIMENSIONS)
        CASE N_ELEMENTS(dims) OF
           2: BEGIN
              t    = REFORM(STR_TO_TIME(TEMPORARY(startStop_t)), $
                            dims[0], $
                            dims[1])
              nCVDatPlots = N_ELEMENTS(t[*,0])              
           END
           1: BEGIN
              t    = STR_TO_TIME(TEMPORARY(startStop_t))
              nCVDatPlots = 1
           END
        ENDCASE

        FOR k=0,nCVDatPlots-1 DO BEGIN
           candidato = WHERE(kappa2d.sdt[stati].time GE t[0,k] AND $
                                kappa2d.sdt[stati].time LE t[1,k],tempNStatII)
           IF tempNStatII GE 2 THEN BEGIN
              statIIList.Add,TEMPORARY(candidato)
              CVnameList.Add,STRMID(TIME_TO_STR(kappa2d.sdt[stati[statIIList[k]]].time),14,5)
              CVnameList[k] = (CVnameList[k])[0] + ' - ' + (CVnameList[k])[-1]
           ENDIF ELSE BEGIN
              PRINT,'Sorry, nothing doing with the event you requested.'
              nCVDatPlots--
           ENDELSE
        ENDFOR

     ENDIF ELSE BEGIN
        PRINT,"DEEEEEE"
     ENDELSE
        
     xRange        = [0.1,200]

     CASE orbString OF
        '5805': BEGIN
           yRange  = [1e-3,1e1]
        END
        '5825': BEGIN
           yRange  = [1e-3,1e1]
        END
        '1770': BEGIN
           yRange  = [4e-4,5e-1]
        END
        '1773': BEGIN
           yRange  = [4e-3,1e1]
        END
        ELSE: BEGIN
           yRange  = [4e-4,5e-1]
        END
     ENDCASE
     

  ENDELSE

  ;;… And some stats
  kappaMom     = MAKE_ARRAY(5,nCVDatPlots)
  tempKMom     = MAKE_ARRAY(5,nCVDatPlots)
  densKMom     = MAKE_ARRAY(5,nCVDatPlots)

  tempMMom     = MAKE_ARRAY(5,nCVDatPlots)
  densMMom     = MAKE_ARRAY(5,nCVDatPlots)

  kappa        = MAKE_ARRAY(nCVDatPlots)
  tempK        = MAKE_ARRAY(nCVDatPlots)
  densK        = MAKE_ARRAY(nCVDatPlots)

  tempM        = MAKE_ARRAY(nCVDatPlots)
  densM        = MAKE_ARRAY(nCVDatPlots)
  ;; kappa_potBar = LIST()
  ;; gauss_potBar = LIST()
  ;; obs_potBar   = LIST()

  ;; combine_stats_from_each_time = 0
  IF KEYWORD_SET(combine_stats_from_each_time) THEN BEGIN

     nFitGroups = 1

     tmpJunk   = MAKE_ARRAY(nCVDatPlots,VALUE=1.0,/FLOAT)
     kappaMom  = TRANSPOSE(tmpJunk # $
                           ([MOMENT(kappa2d.fitparams[2,statI]), $
                             MEDIAN(kappa2d.fitparams[2,statI])]))

     tempKMom  = TRANSPOSE(tmpJunk # $
                           ([MOMENT(kappa2d.fitparams[1,statI]), $
                             MEDIAN(kappa2d.fitparams[1,statI])]))
     densKMom  = TRANSPOSE(tmpJunk # $
                           ([MOMENT(kappa2d.fitparams[3,statI]), $
                             MEDIAN(kappa2d.fitparams[3,statI])]))

     tempMMom  = TRANSPOSE(tmpJunk # $
                           ([MOMENT(gauss2d.fitparams[1,statI]), $
                             MEDIAN(gauss2d.fitparams[1,statI])]))
     densMMom  = TRANSPOSE(tmpJunk # $
                           ([MOMENT(gauss2d.fitparams[3,statI]), $
                             MEDIAN(gauss2d.fitparams[3,statI])]))

     kappa     = tmpJunk # kappaMom[0]
     tempK     = tmpJunk # tempKMom[0]
     densK     = tmpJunk # densKMom[0]

     tempM     = tmpJunk # tempMMom[0]
     densM     = tmpJunk # densMMom[0]

  ENDIF ELSE BEGIN

     nFitGroups = nCVDatPlots
     FOR k=0,nCVDatPlots-1 DO BEGIN

        kappaMom[*,k] = [MOMENT(kappa2d.fitparams[2,statI[statIIList[k]]]), $
                         MEDIAN(kappa2d.fitparams[2,statI[statIIList[k]]])]
        tempKMom[*,k]  = [MOMENT(kappa2d.fitparams[1,statI[statIIList[k]]]), $
                          MEDIAN(kappa2d.fitparams[1,statI[statIIList[k]]])]
        densKMom[*,k]  = [MOMENT(kappa2d.fitparams[3,statI[statIIList[k]]]), $
                          MEDIAN(kappa2d.fitparams[3,statI[statIIList[k]]])]

        tempMMom[*,k]  = [MOMENT(gauss2d.fitparams[1,statI[statIIList[k]]]), $
                          MEDIAN(gauss2d.fitparams[1,statI[statIIList[k]]])]
        densMMom[*,k]  = [MOMENT(gauss2d.fitparams[3,statI[statIIList[k]]]), $
                          MEDIAN(gauss2d.fitparams[3,statI[statIIList[k]]])]

        kappa[k]       = kappaMom[0,k]
        tempK[k]       = tempKMom[0,k]
        densK[k]       = densKMom[0,k]

        tempM[k]       = tempMMom[0,k]
        densM[k]       = densMMom[0,k]
     ENDFOR
  ENDELSE
  ;; kappa_potBar     = kappa2d.fitparams[0,statI]/REFORM(kappa2d.fitparams[1,statI])
  ;; gauss_potBar     = gauss2d.fitparams[0,statI]/REFORM(gauss2d.fitparams[1,statI])

  IF KEYWORD_SET(use_total_J_and_V) THEN BEGIN
     obs_potBar       = setup.chartot / kappa2d.obsTemp
     obsCurrent       = (setup.je+setup.ji)[statI]
  ENDIF ELSE BEGIN
     ;; obs_potBar       = (kappa2d.fitparams[0,statI]+gauss2d.fitparams[0,statI]) / $
     ;;                    2. / kappa2d.obsTemp
     obs_potBar       = setup.chare / kappa2d.obsTemp
     obsCurrent       = setup.je[statI]
  ENDELSE
  ;; kappa_potBar  = setup.charTot[statI]/REFORM(kappa2d.fitparams[1,statI])
  ;; gauss_potBar  = setup.charTot[statI]/REFORM(gauss2d.fitparams[1,statI])


  R_B          = REVERSE([10,30,100,300])
  lStyle       = REVERSE(['-','--','-.',':'])

  nR_B         = N_ELEMENTS(R_B)
  CVDatPlots   = MAKE_ARRAY(nCVDatPlots,/OBJ)
  CVKappa      = MAKE_ARRAY(nR_B,nFitGroups,/OBJ)
  CVGauss      = MAKE_ARRAY(nR_B,nFitGroups,/OBJ)
  legend       = MAKE_ARRAY(nFitGroups,/OBJ)

  window           = WINDOW(DIMENSIONS=[1000,600], $
                        BUFFER=buffer)

     

  in_potBar        = 10.D^(DOUBLE(INDGEN(97)/16.-2))

  FOR plotInd=0,nFitGroups-1 DO BEGIN

     kHere  = !NULL
     startK = (nFitGroups GT 1 ? plotInd : 0)
     stopK  = (nFitGroups GT 1 ? plotInd : nCVDatPlots-1)
     FOR k=startK,stopK DO BEGIN

        
        CVDatPlots[k] = PLOT(obs_potBar[statIIList[k]], $
                             ;; (-1.D)*obsCurrent[statI[statIIList[k]]], $
                             (-1.D)*obsCurrent[statIIList[k]], $
                             ;; XTITLE='$\Chi$', $
                             NAME=CVnameList[k], $
                             XRANGE=xRange, $
                             YRANGE=yRange, $
                             XTITLE='$\Delta \phi$/k!DB!NT', $
                             ;; YTITLE=((plotInd EQ 0) AND (k EQ 0) ? 'Field-aligned current ( $\mu$A/m!U2!N)' : !NULL), $
                             ;; YSHOWTEXT=(nFitGroups GT 1 ? (k EQ 0) : !NULL), $
                             YSHOWTEXT=((plotInd EQ 0) AND (k EQ 0)) ? !NULL : 0, $
                             LINESTYLE=' ', $
                             SYMBOL=symbole[k], $
                             SYM_SIZE=sym_size, $
                             SYM_THICK=sym_thick, $
                             SYM_TRANSPARENCY=sym_transp, $
                             COLOR=coulour[k], $
                             FONT_SIZE=axisFontSize, $
                             XLOG=1, $
                             YLOG=1, $
                             LAYOUT=(nFitGroups GT 1 ? $
                                     [CEIL((nFitGroups + 1)/2.),FLOOR(nFitGroups/2),plotInd+1] $
                                     : !NULL), $
                             OVERPLOT=k GT startK, $
                             CURRENT=window)
     ENDFOR


     FOR k=0,nR_B-1 DO BEGIN
        tempR_B  = R_B[k]
        kappaCur = KNIGHT_RELATION__DORS_KLETZING_11(kappa[plotInd], $
                                                     tempK[plotInd], $
                                                     densK[plotInd], $
                                                     pot,tempR_B, $
                                                     IN_POTBAR=in_potBar, $
                                                     ;; IN_POTBAR=kappa_potBar, $
                                                     OUT_POTBAR=kappa_potBar, $
                                                     /NO_MULT_BY_CHARGE)

        gaussCur  = KNIGHT_RELATION__DORS_KLETZING_4(tempM[plotInd], $
                                                     densM[plotInd], $
                                                     pot,tempR_B, $
                                                     IN_POTBAR=in_potBar, $
                                                     ;; IN_POTBAR=gauss_potBar, $
                                                     OUT_POTBAR=gauss_potBar, $
                                                     /NO_MULT_BY_CHARGE)


        kappaName  = 'Kappa'
        gaussName  = 'Maxwellian'

        CVkappa[k,plotInd]  = PLOT(in_potBar, $
                                 kappaCur*1.e6, $
                                 ;; XTITLE='Wolts', $
                                 ;; YTITLE='FAC ($\mu$A/m!U2!N)', $
                                 XRANGE=xRange, $
                                 NAME=kappaName, $
                                 LINESTYLE=lStyle[k], $
                                 ;; LINESTYLE=kappaLSty, $
                                 COLOR=kappaColor, $
                                 XLOG=1, $
                                 YLOG=1, $
                                 /OVERPLOT, $
                                 LAYOUT=(nFitGroups GT 1 ? $
                                         [CEIL(nFitGroups/2),FLOOR(nFitGroups/2),plotInd+1] $
                                         : !NULL), $
                                 CURRENT=window)

        ;; CVgauss  = PLOT(gauss_potBar, $
        CVgauss[k,plotInd]  = PLOT(in_potBar, $
                                 gaussCur*1.e6, $
                                 ;; XTITLE='Wolts', $
                                 ;; YTITLE='FAC ($\mu$A/m!U2!N)', $
                                 XRANGE=xRange, $
                                 NAME=gaussName, $
                                 ;; LINESTYLE=gaussLSty, $
                                 LINESTYLE=lStyle[k], $
                                 COLOR=gaussColor, $
                                 XLOG=1, $
                                 YLOG=1, $
                                 /OVERPLOT, $
                                 LAYOUT=(nFitGroups GT 1 ? $
                                         [CEIL(nFitGroups/2),FLOOR(nFitGroups/2),plotInd+1] $
                                         : !NULL), $
                                 CURRENT=window)

     ENDFOR

     kappaStr  = STRING(FORMAT='("K (T = ",F0.1,' + $
                        ;; kappaStr  = STRING(FORMAT='("K (T = 2.8e4",' + $
                        ;; '" eV, N = ",G5.1," cm!U-3!N, $\kappa$ = ",F0.2,")")', $
                        '", n = ",G7.2,", $\kappa$ = ",F0.2,")")', $
                        tempK[plotInd], $
                        densK[plotInd], $
                        kappa[plotInd])
     gaussStr  = STRING(FORMAT='("M (T = ",F0.1,' + $
                        ;; '" eV, N = ",G0.1," cm!U-3!N")', $
                        '", n = ",G7.2,")")', $
                        tempM[plotInd], $
                        densM[plotInd])

     PRINT,kappaStr
     PRINT,gaussStr

     kappa_potBar     = kappa2d.fitparams[0,statI[statIIList[plotInd]]] / $
                        REFORM(kappa2d.fitparams[1,statI[statIIList[plotInd]]])
     gauss_potBar     = gauss2d.fitparams[0,statI[statIIList[plotInd]]] / $
                        REFORM(gauss2d.fitparams[1,statI[statIIList[plotInd]]])
     obsTemp_potBar   = obs_potBar[statIIList[plotInd]]

     meanKappaPotBar  = MEAN(kappa_potBar)
     meanGaussPotBar  = MEAN(gauss_potBar)
     meanObsPotBar    = MEAN(obsTemp_potBar)

     ratKappaObs      = (meanKappaPotBar/meanObsPotBar) > (meanObsPotBar/meanKappaPotBar)
     ratGaussObs      = (meanGaussPotBar/meanObsPotBar) > (meanObsPotBar/meanGaussPotBar)
     PRINT,FORMAT='("Kappa, Max, obs <potBar> : ",F0.2,T35,F0.2,T42,F0.2)', $
           meanKappaPotBar, $
           meanGaussPotBar, $
           meanObsPotBar
     PRINT,FORMAT='(A0," wins! (",F0.2," vs. ",F0.2,")")', $
           (ratKappaObs LT ratGaussObs ? "Kappa" : "Gauss"), $
           ratKappaObs, $
           ratGaussObs

  ENDFOR

  legend[0] = LEGEND(TARGET=[CVDatPlots,CVkappa[-1,-1],CVgauss[-1,-1]], $
                           FONT_SIZE=legFontSize, $
                           POSITION=(nFitGroups GT 1 ? [0.58,0.83] : [0.35,0.83]), $
                           /NORMAL)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Plots


  IF KEYWORD_SET(save_plot) THEN BEGIN
     plotName = "~/Desktop/Current-voltage__orbit_" + orbString + ".ps"
     PRINT,"Saving plot to " + plotName
     window.Save,plotName
  ENDIF

  STOP

  IF KEYWORD_SET(fa_conductance_plot) THEN BEGIN
     this = PLOT(REFORM(kappa2d.fitParams[2,*]), $
                 kappa2d.fitfaconduct/gauss2d.fitfaconduct, $
                 LINESTYLE=' ', $
                 SYMBOL='*', $
                 XLOG=0, $
                 XRANGE=[1.4,10])
  ENDIF
END
