;;11/10/16
PRO KAPPA_FIT_STATISTICS,kappa2d,gauss2d,potStruct, $
                         LOAD_FROM_THIS_FILE=loadFile, $
                         SAVE_PLOT=save_plot, $
                         FA_CONDUCTANCE_PLOT=fa_conductance_plot, $
                         BUFFER=buffer

  COMPILE_OPT IDL2

  defChiThresh = 5.0

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

  sym_size     = 1.3
  sym_thick    = 1.5
  sym_transp   = 50

  axisFontSize = 16
  legFontSize  = 14

  loadFile     = '/home/spencerh/software/sdt/batch_jobs/saves_output_etc/20161110--orbit_1773--potential_drop__kappa_interp.sav'

  IF KEYWORD_SET(loadFile) THEN BEGIN
     RESTORE,loadFile
  ENDIF

  firstHalfEnd    = STR_TO_TIME('1997-02-01/09:26:23')
  secondHalfStart = STR_TO_TIME('1997-02-01/09:26:53')

  chi2Red = kappa2D.chi2/(kappa2D.dof+kappa2D.nFree)

  statI   = WHERE(chi2Red LE defChiThresh,nStat)

  IF nStat LT 3 THEN STOP

  kappaMom = [MOMENT(kappa2d.fitparams[2,statI]),MEDIAN(kappa2d.fitparams[2,statI])]
  tempKMom = [MOMENT(kappa2d.fitparams[1,statI]),MEDIAN(kappa2d.fitparams[1,statI])]
  densKMom = [MOMENT(kappa2d.fitparams[3,statI]),MEDIAN(kappa2d.fitparams[3,statI])]

  tempMMom = [MOMENT(gauss2d.fitparams[1,statI]),MEDIAN(gauss2d.fitparams[1,statI])]
  densMMom = [MOMENT(gauss2d.fitparams[3,statI]),MEDIAN(gauss2d.fitparams[3,statI])]

  kappa    = kappaMom[0]
  tempK    = tempKMom[0]
  densK    = densKMom[0]

  tempM    = tempMMom[0]
  densM    = densMMom[0]

  ;; CVplot   = PLOT(setup.charTot[statI],(-1.D)*setup.je[statI], $
  ;;                 XTITLE='Wolts', $
  ;;                 YTITLE='FAC ($\mu$A/m!U2!N)', $
  ;;                 LINESTYLE=' ', $
  ;;                 SYMBOL='*', $
  ;;                 XLOG=1, $
  ;;                 YLOG=1)

  ;; kappa_potBar = setup.charTot[statI]/REFORM(kappa2d.fitparams[1,statI])
  ;; gauss_potBar  = setup.charTot[statI]/REFORM(gauss2d.fitparams[1,statI])

  kappa_potBar = kappa2d.fitparams[0,statI]/REFORM(kappa2d.fitparams[1,statI])
  gauss_potBar  = gauss2d.fitparams[0,statI]/REFORM(gauss2d.fitparams[1,statI])

  obs_potBar   = (kappa2d.fitparams[0,statI]+gauss2d.fitparams[0,statI]) / 2. / kappa2d.obsTemp

  ;; statIIFirstH  = CGSETINTERSECTION(WHERE(kappa2d.sdt[stati].time LE firstHalfEnd),statI)
  ;; statIISecondH = CGSETINTERSECTION(WHERE(kappa2d.sdt[stati].time GT firstHalfEnd),statI)

  statIIFirstH  = WHERE(kappa2d.sdt[stati].time LE firstHalfEnd)

  ;; statIISecondH = WHERE(kappa2d.sdt[stati].time GT firstHalfEnd)
  statIISecondH = WHERE(kappa2d.sdt[stati].time GE secondHalfStart)

  xRange       = [0.1,200]
  yRange       = [4e-3,1e1]

  window       = WINDOW(DIMENSIONS=[1000,600], $
                        BUFFER=buffer)

  CVname1      = STRMID(TIME_TO_STR(kappa2d.sdt[stati[statIIFirstH]].time),14,5)
  CVname2      = STRMID(TIME_TO_STR(kappa2d.sdt[stati[statIISecondH]].time),14,5)

  CVname1      = CVname1[0] + ' - ' + CVname1[-1]
  CVname2      = CVname2[0] + ' - ' + CVname2[-1]

  CVplot2       = PLOT(obs_potBar, $
                       (-1.D)*setup.je[statI[statIISecondH]], $
                       ;; XTITLE='$\Chi$', $
                       NAME=CVname2, $
                       XRANGE=xRange, $
                       YRANGE=yRange, $
                       XTITLE='$\Delta \phi$/k!DB!NT', $
                       YTITLE='FAC ($\mu$A/m!U2!N)', $
                       LINESTYLE=' ', $
                       SYMBOL=secondSym, $
                       SYM_SIZE=sym_size, $
                       SYM_THICK=sym_thick, $
                       SYM_TRANSPARENCY=sym_transp, $
                       COLOR=secondColor, $
                       FONT_SIZE=axisFontSize, $
                       XLOG=1, $
                       YLOG=1, $
                       CURRENT=window)

  CVplot1      = PLOT(obs_potBar, $
                      (-1.D)*setup.je[statI[statIIFirstH]], $
                      ;; XTITLE='$\Chi$', $
                      NAME=CVname1, $
                      XRANGE=xRange, $
                      YRANGE=yRange, $
                      XTITLE='$\Delta \phi$/k!DB!NT', $
                      YTITLE='Field-aligned current ( $\mu$A/m!U2!N)', $
                      LINESTYLE=' ', $
                      SYMBOL=firstSym, $
                      SYM_SIZE=sym_size, $
                      SYM_THICK=sym_thick, $
                      SYM_TRANSPARENCY=sym_transp, $
                      COLOR=firstColor, $
                      FONT_SIZE=axisFontSize, $
                      ;; COLOR=kappaColor, $
                      XLOG=1, $
                      YLOG=1, $
                      /OVERPLOT, $
                      CURRENT=window)

  kappa_potBar = kappa_potBar[SORT(kappa_potBar)]
  gauss_potBar  = gauss_potBar[SORT(gauss_potBar)]


  ;; pot       = setup.charTot[statI]
  in_potBar = 10.D^(DOUBLE(INDGEN(25)/4.-2))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Plots

  R_B       = REVERSE([10,30,100,300])
  lStyle    = REVERSE(['-','--','-.',':'])
  FOR k=0,N_ELEMENTS(R_B)-1 DO BEGIN
     tempR_B  = R_B[k]
     kappaCur = KNIGHT_RELATION__DORS_KLETZING_11(kappa,tempK,densK,pot,tempR_B, $
                                                  IN_POTBAR=in_potBar, $
                                                  ;; IN_POTBAR=kappa_potBar, $
                                                  OUT_POTBAR=potBar, $
                                                  /NO_MULT_BY_CHARGE)

     gaussCur  = KNIGHT_RELATION__DORS_KLETZING_4(tempM,densM,pot,tempR_B, $
                                                 IN_POTBAR=in_potBar, $
                                                 ;; IN_POTBAR=gauss_potBar, $
                                                 OUT_POTBAR=potBar, $
                                                 /NO_MULT_BY_CHARGE)


     ;; kappaName  = STRING(FORMAT='("K (T = ",F0.1,' + $
     ;; kappaName  = STRING(FORMAT='("K (T = 2.8e4",' + $
     ;;                     ;; '" eV, N = ",G5.1," cm!U-3!N, $\kappa$ = ",F0.2,")")', $
     ;;                     '", n = ",G5.2,", $\kappa$ = ",F0.2,")")', $
     ;;                     ;; tempK, $
     ;;                     densK, $
     ;;                     kappa)
     ;; gaussName  = STRING(FORMAT='("M (T = ",F0.1,' + $
     ;;                     ;; '" eV, N = ",G0.1," cm!U-3!N")', $
     ;;                     '", n = ",G5.2,")")', $
     ;;                     tempM, $
     ;;                     densM)
     kappaName  = 'Kappa'
     gaussName  = 'Maxwellian'

     CVkappa  = PLOT(in_potBar, $
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
                     CURRENT=window)

     ;; CVgauss  = PLOT(gauss_potBar, $
     CVgauss  = PLOT(in_potBar, $
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
                    CURRENT=window)

  ENDFOR

  legend = LEGEND(TARGET=[CVplot1,CVplot2,CVkappa,CVgauss], $
                  FONT_SIZE=legFontSize, $
                  POSITION=[0.35,0.83], $
                  /NORMAL)

  IF KEYWORD_SET(save_plot) THEN BEGIN
     plotName = "~/Desktop/Current-voltage__orbit_1773.ps"
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
