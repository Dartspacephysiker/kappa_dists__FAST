;2017/04/12
PRO PLOT_J_V_MAP__R_B_AND_KAPPA__FIXED_T_AND_N,mMagDat,jvPlotData,avgs_JVFit, $
   MAP__2D=map__2D, $
   MAP2D__LOG_KAPPA=map2D__log_kappa, $
   ORBIT=orbit, $
   IN_KAPPA_A=A, $
   IN_GAUSS_A=AGauss, $
   OUT_YBEST=out_yBest, $
   SAVEPLOT=savePlot

  COMPILE_OPT IDL2,STRICTARRSUBS

  rgbTable         = 4
  nColors          = 256
  transpose        = 1
  hammerCT         = COLORTABLE(rgbTable,STRETCH=stretch,NCOLORS=nColors,TRANSPOSE=transpose)

  log_cbRange = 0
  IF log_cbRange THEN BEGIN

     zVar             = ALOG10(mMagDat.chi2)
     nCBTicks         = 5

     cbRange          = ALOG10(MINMAX(mMagDat.chi2 < 1.0D3))
     ;; cbRange[0]       = cbRange[0] < 0.0
     cbRange[0]       = cbRange[0]
     tickValues       = FINDGEN(nCBTicks+1)/nCBTicks*(cbRange[1]-cbRange[0])+cbRange[0]


     ;; tickName         = STRING(FORMAT='(F0.2)',10.^tickValues)
     IF (WHERE(tickValues) LT 0.)[0] NE -1 THEN BEGIN
        cbRange[0]    = FLOOR(cbRange[0])
        tickValues    = FLOOR(tickValues)
        tickValues    = tickValues[UNIQ(tickValues,SORT(tickValues))]
        tickName      = STRING(FORMAT='(F0.2)',10.^tickValues)
     ENDIF ELSE BEGIN
        tickName      = STRING(FORMAT='(I0)',10.^tickValues)
     ENDELSE
  ENDIF ELSE BEGIN

     zVar             = mMagDat.chi2
     nCBTicks         = 10

     ;; cbRange          = MINMAX(mMagDat.chi2 < 1.0D2)
     cbRange          = [0,MAX(mMagDat.chi2 < 1.0D2)]
     tickValues       = FINDGEN(nCBTicks+1)/nCBTicks*(cbRange[1]-cbRange[0])+cbRange[0]


     ;; tickName         = STRING(FORMAT='(F0.2)',10.^tickValues)
     IF (WHERE(tickValues) LT 0.)[0] NE -1 THEN BEGIN
        cbRange[0]    = FLOOR(cbRange[0])
        tickValues    = FLOOR(tickValues)
        tickValues    = tickValues[UNIQ(tickValues,SORT(tickValues))]
        tickName      = STRING(FORMAT='(F0.2)',tickValues)
     ENDIF ELSE BEGIN
        tickName      = STRING(FORMAT='(I0)',tickValues)
     ENDELSE

  ENDELSE

  ;;Winder
  winDim           = [900,600]
  window1          = WINDOW(DIMENSIONS=winDim, $
                            BUFFER=savePlot)

  ;;These all pertain to grid creation
  xGridStyle       = ':'
  yGridStyle       = ':'
  xTickLen         = 1
  yTickLen         = 1
  xSubTickLen      = 0.01
  ySubTickLen      = 0.01

  ;; cbpos            = [0.10,0.97,0.95,0.99]
  ;; contPos          = [0.10,0.06,0.95,0.81]
  ;;Supposing a vertical color bar
  cbpos            = [0.95,0.06,0.97,0.92]
  contPos          = [0.10,0.09,0.85,0.92]


  orbPref          = ''
  IF KEYWORD_SET(orbit) THEN BEGIN
     orbPref       = 'Orbit ' + STRCOMPRESS(orbit,/REMOVE_ALL)
  ENDIF

  CURANDPOT__SELECT_T_AND_N,jvPlotData,avgs_JVfit, $
                            TEMPERATURE=Temperature, $
                            DENSITY=Density ;, $

  titleStr         = STRING(FORMAT='(A0," (T=",F0.1," eV, N=",G0.3," cm!U-3!N)")', $
                            orbPref,Temperature,Density)
  CASE 1 OF
     KEYWORD_SET(map__2D): BEGIN


        yVar       = mMagDat.kappa
        yTitle     = 'Kappa'

        instead_q  = 0
        IF KEYWORD_SET(instead_q) THEN BEGIN
           yVar    = 1D + 1D/(mMagDat.kappa-1.5D)
           yTitle  = 'q'
        ENDIF

        yRange     = MINMAX(yVar)

        nContours  = 201
        c_values   = FINDGEN(nContours+1)/nContours*(cbRange[1]-cbRange[0])+cbRange[0]

        ;; nPoints     = N_ELEMENTS(mMagDat.magRat)

        ;; IF KEYWORD_SET(minSurface) THEN BEGIN

        ;;    R = MIN_CURVE_SURF(ALOG10(mMagDat.chi2),mMagDat.magRat,mMagDat.kappa, $
        ;;                       YOUT=mMagDat.kappa[*,0], $
        ;;                       XOUT=REFORM(mMagDat.magRat[0,*]))

        ;; ENDIF ELSE BEGIN

        contPlot    = CONTOUR(zVar,mMagDat.magRat, $
                              yVar, $
                              XRANGE=MINMAX(mMagDat.magRat), $
                              XTITLE='Mirror ratio', $
                              YTITLE=yTitle, $
                              /XLOG, $
                              YLOG=map2D__log_kappa AND ~instead_q, $
                              AXIS_STYLE=2, $
                              C_VALUE=c_values, $
                              YSTYLE=1, $
                              YRANGE=yRange, $
                              TITLE=titleStr, $
                              /FILL, $
                              XGRIDSTYLE=xGridStyle, $
                              YGRIDSTYLE=yGridStyle, $
                              XTICKLEN=xTickLen, $
                              YTICKLEN=yTickLen, $
                              XSUBTICKLEN=xSubTickLen, $
                              YSUBTICKLEN=ySubTickLen, $
                              ;; C_THICK=4.0, $
                              FONT_SIZE=16, $
                              XTICKFONT_SIZE=14, $
                              YTICKFONT_SIZE=14, $
                              RGB_TABLE=hammerCT, $
                              POSITION=contPos, $
                              /CURRENT)

        ;; ENDELSE
        
        cb          = COLORBAR(TITLE='$\chi$!U2!Dred!N', $
                               ORIENTATION=1, $
                               TEXT_ORIENTATION=0, $
                               RGB_TABLE=hammerCT, $
                               TICKNAME=tickName, $
                               TICKVALUES=tickValues, $
                               POSITION=cbpos, $
                               RANGE=cbRange, $
                               FONT_SIZE=14, $
                               /NORMAL)


        junk = MIN(mMagDat.chi2,ind)
        PRINT,"WIN2D"
        PRINT,FORMAT='(A0,T10,A0,T20,A0)', $
              'Chi^2_red',yTitle,'R_B'
        PRINT,FORMAT='(F0.2,T10,F0.2,T20,F0.2)', $
              mMagDat.chi2[ind],yVar[ind],mMagDat.magRat[ind]
        PRINT,''

        ;;GET THAT CURVE!!

        ;; yBest = KNIGHT_RELATION__DORS_KLETZING_11(mMagDat.kappa[ind], $
        ;;                                           Temperature, $
        ;;                                           DENSITY_FACTOR__BARBOSA_1977(jvplotdata.pot[avgs_jvfit.useinds], $
        ;;                                                                        Temperature, $
        ;;                                                                        0, $
        ;;                                                                        avgs_JVfit.N_SC.avg, $
        ;;                                                                        mMagDat.magRat[ind]), $
        ;;                                           jvplotdata.pot[avgs_jvfit.useinds], $
        ;;                                           mMagDat.magRat[ind], $
        ;;                                           /NO_MULT_BY_CHARGE)*1D6


        ;; plot1 = PLOT(jvplotdata.pot[avgs_jvfit.useinds],yBest)

     END
     ELSE: BEGIN

        ;; nPoints     = N_ELEMENTS(mMagDat.magRat)
        
        p1pos            = [0.10,0.08,0.95,0.50]
        p2pos            = [0.10,0.53,0.95,0.94]
        cbpos            = [0.10,0.97,0.95,0.99]

        plot_1           = SCATTERPLOT(mMagDat.magRat, $
                                       mMagDat.kappa, $
                                       ;; XRANGE=tRange, $
                                       ;; YRANGE=TDownRange, $
                                       ;; YRANGE=errTDownRange, $
                                       XTITLE="Mirror ratio", $
                                       YTITLE="Kappa", $
                                       /XLOG, $
                                       /YLOG, $
                                       SYMBOL='.', $
                                       SYM_SIZE=3.0, $
                                       /SYM_FILLED, $
                                       RGB_TABLE=hammerCT, $
                                       MAGNITUDE=mMagDat.chi2, $
                                       XGRIDSTYLE=xGridStyle, $
                                       YGRIDSTYLE=yGridStyle, $
                                       XTICKLEN=xTickLen, $
                                       YTICKLEN=yTickLen, $
                                       XSUBTICKLEN=xSubTickLen, $
                                       YSUBTICKLEN=ySubTickLen, $
                                       /CURRENT, $
                                       POSITION=p1pos)

        plot_2            = SCATTERPLOT(mMagDat.magRat, $
                                        mMagDat.chi2, $
                                        XTICKFORMAT="(A1)", $
                                        ;; XTITLE="Mirror ratio", $
                                        YTITLE="$\Chi$!U2!Dred!N", $
                                        /XLOG, $
                                        /YLOG, $
                                        SYMBOL='.', $
                                        SYM_SIZE=3.0, $
                                        /SYM_FILLED, $
                                        XGRIDSTYLE=xGridStyle, $
                                        YGRIDSTYLE=yGridStyle, $
                                        XTICKLEN=xTickLen, $
                                        YTICKLEN=yTickLen, $
                                        XSUBTICKLEN=xSubTickLen, $
                                        YSUBTICKLEN=ySubTickLen, $
                                        /CURRENT, $
                                        POSITION=p2pos)

        ;; cb_i        = (INDGEN(nCBTicks)*nPoints)/(nCBTicks-1)
        ;; IF cb_i[-1] EQ nPoints THEN cb_i[-1] -= 1
        ;; tickValues  = (mMagDat.chi2[SORT(mMagDat.chi2)])[cb_i]
        ;; tickName    = STRING(FORMAT='(F0.2)',tickValues)
        ;; tMagRange   = ALOG10(MINMAX(mMagDat.chi2))
        ;; tickValues  = FINDGEN(nCBTicks+1)/nCBTicks*(tMagRange[1]-tMagRange[0])+tMagRange[0]
        ;; tickName    = STRING(FORMAT='(F0.2)',10.^tickValues)
        cb          = COLORBAR(TITLE='$\chi$!U2!Dred!N', $
                               RGB_TABLE=hammerCT, $
                               TICKNAME=tickName, $
                               TICKVALUES=tickValues, $
                               POSITION=cbpos, $
                               RANGE=cbRange, $
                               /NORMAL)

     END
  ENDCASE

  IF KEYWORD_SET(savePlot) THEN BEGIN

     

  ENDIF

END
