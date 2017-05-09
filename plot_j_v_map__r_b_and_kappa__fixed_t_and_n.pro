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

  ;;For adding an R_E axis, if that's wassup
  @common__jv_curve_fit__tie_r_b_and_dens.pro

  rgbTable         = 4
  nColors          = 256
  transpose        = 1
  hammerCT         = COLORTABLE(rgbTable,STRETCH=stretch,NCOLORS=nColors,TRANSPOSE=transpose)

  log_cbRange = 0
  IF log_cbRange THEN BEGIN

     zVar             = ALOG10(mMagDat.K.chi2)
     nCBTicks         = 5

     cbRange          = ALOG10(MINMAX(mMagDat.K.chi2 < 1.0D3))
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

     zVar             = mMagDat.K.chi2
     nCBTicks         = 10

     ;; cbRange          = MINMAX(mMagDat.K.chi2 < 1.0D2)
     cbRange          = [5,MAX(mMagDat.K.chi2 < 2.0D1)]
     tickValues       = FINDGEN(nCBTicks+1)/nCBTicks*(cbRange[1]-cbRange[0])+cbRange[0]


     ;; tickName         = STRING(FORMAT='(F0.2)',10.^tickValues)
     IF (WHERE(tickValues) LT 0.)[0] NE -1 THEN BEGIN
        cbRange[0]    = FLOOR(cbRange[0])
        ;; tickValues    = FLOOR(tickValues)
        tickValues    = ROUND_TO_NTH_DECIMAL_PLACE(tickValues,-1)
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
  contPos          = [0.07,0.19,0.82,0.92]


  orbPref          = ''
  IF KEYWORD_SET(orbit) THEN BEGIN
     orbPref       = 'Orbit ' + STRCOMPRESS(orbit,/REMOVE_ALL)
  ENDIF

  CURANDPOT__SELECT_T_AND_N,jvPlotData,avgs_JVfit, $
                            TEMPERATURE=Temperature, $
                            DENSITY=Density ;, $

  titleStr         = STRING(FORMAT='(A0," (T=",F0.1," eV, N=",G0.3," cm!U-3!N)")', $
                            orbPref,Temperature,Density)

  ;;Are we going to add an R_E axis?

  ;;ionosphere R_B values - ind 1
  ;;FAST R_B values       - ind 0

  IF N_ELEMENTS(tRB_fLineRE) GT 0 THEN BEGIN
     make_R_E_axis    = 1
     ;; R_E_vals  = INTERPOL(tRB_fLineRE,REFORM(tRB_RBpairs[1,*]),REFORM(mMagDat.K.magRat[0,*]))
     ;; IF MIN(mMagDat.magRat) LT 10 THEN BEGIN
     R_B_axis_vals    = [MIN(mMagDat.K.magRat),(MIN(mMagDat.K.magRat) LT 10 ? 10 : 100)]
     ;; ENDIF ELSE BEGIN
     ;;    R_B_axis_vals = [MIN(mMagDat.K.magRat),100]
     ;; ENDELSE

     WHILE MAX(R_B_axis_vals) LT MAX(mMagDat.K.magRat) DO BEGIN
        R_B_axis_vals = [R_B_axis_vals,(R_B_axis_vals[-1]*10) < MAX(mMagDat.K.magRat)]
     ENDWHILE

     R_B_axis_names   = [R_B_axis_vals,(R_B_axis_vals[-1]*10) < MAX(mMagDat.K.magRat)]

     R_B_axis_vals    = STRING(FORMAT='(F0.1)',R_B_axis_vals)
     ;; R_B_FAST  = INTERPOL(REFORM(tRB_RBpairs[0,*]),REFORM(tRB_RBpairs[1,*]),R_B)
     nVals            = N_ELEMENTS(R_E_axis_vals)
     ;; nValsStr         = STRING('(I0)',nVals)
     ;; R_E_axis_names   = STRING(FORMAT='('+nValsStr+'(F0.1))',R_E_axis_vals)
     R_E_axis_names   = STRING(FORMAT='(F0.1)',R_E_axis_vals)

  ENDIF

  ;; xTitle = 'Mirror ratio'
  xTitle = 'R!DB!N'

  CASE 1 OF
     KEYWORD_SET(map__2D): BEGIN


        yVar       = mMagDat.K.kappa
        ;; yTitle     = 'Kappa'
        yTitle     = '$\kappa$'

        instead_q  = 0
        IF KEYWORD_SET(instead_q) THEN BEGIN
           yVar    = 1D + 1D/(mMagDat.K.kappa-1.5D)
           yTitle  = 'q'
        ENDIF

        yRange     = MINMAX(yVar)
        yRange     = [yRange[0],5]

        nContours  = 201
        c_values   = FINDGEN(nContours+1)/nContours*(cbRange[1]-cbRange[0])+cbRange[0]

        ;; nPoints     = N_ELEMENTS(mMagDat.K.magRat)

        ;; IF KEYWORD_SET(minSurface) THEN BEGIN

        ;;    R = MIN_CURVE_SURF(ALOG10(mMagDat.K.chi2),mMagDat.K.magRat,mMagDat.K.kappa, $
        ;;                       YOUT=mMagDat.K.kappa[*,0], $
        ;;                       XOUT=REFORM(mMagDat.K.magRat[0,*]))

        ;; ENDIF ELSE BEGIN

        contPlot    = CONTOUR(zVar,mMagDat.K.magRat, $
                              yVar, $
                              XRANGE=MINMAX(mMagDat.K.magRat), $
                              XTITLE=xTitle, $
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
                              XTICKNAME=R_B_axis_names, $
                              XTICKVALUES=R_B_axis_vals, $
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


        IF KEYWORD_SET(make_R_E_axis) THEN BEGIN
           R_Eaxis = AXIS('X', $
                          TARGET=contPlot, $
                          ;; LOCATION="bottom", $
                          LOCATION=[0,MIN(contPlot.yrange)-0.88,0], $
                          TITLE='R!DE!N', $
                          SUBTICKLEN=0.0, $
                          TICKLEN=0.015, $
                          TICKFONT_SIZE=14, $
                          TICKVALUES=R_B_axis_vals, $
                          TICKNAME=R_E_axis_names);, $
                          ;; AXIS_RANGE=MINMAX(R_E_axis_vals))
        ENDIF


        junkK = MIN(mMagDat.K.chi2,indK)
        junkG = MIN(mMagDat.G.chi2,indG)

        PRINT,"WIN2D"
        PRINT,"******"
        PRINT,"Kappa"
        PRINT,"******"
        PRINT,FORMAT='(A0,T15,A0,T25,A0)', $
              'Chi^2_red','R_B',yTitle
        PRINT,FORMAT='(F0.2,T15,F0.2,T25,F0.2)', $
              mMagDat.K.chi2[indK],mMagDat.K.magRat[indK],yVar[indK]
        PRINT,''
        PRINT,"******"
        PRINT,"Maxwell"
        PRINT,"******"
        PRINT,FORMAT='(A0,T15,A0)', $
              'Chi^2_red','R_B'
        PRINT,FORMAT='(F0.2,T15,F0.2)', $
              mMagDat.G.chi2[indG],mMagDat.G.magRat[indG]
        PRINT,''

     END
     ELSE: BEGIN

        ;; nPoints     = N_ELEMENTS(mMagDat.K.magRat)
        
        p1pos            = [0.10,0.08,0.95,0.50]
        p2pos            = [0.10,0.53,0.95,0.94]
        cbpos            = [0.10,0.97,0.95,0.99]

        plot_1           = SCATTERPLOT(mMagDat.K.magRat, $
                                       mMagDat.K.kappa, $
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
                                       MAGNITUDE=mMagDat.K.chi2, $
                                       XGRIDSTYLE=xGridStyle, $
                                       YGRIDSTYLE=yGridStyle, $
                                       XTICKLEN=xTickLen, $
                                       YTICKLEN=yTickLen, $
                                       XSUBTICKLEN=xSubTickLen, $
                                       YSUBTICKLEN=ySubTickLen, $
                                       /CURRENT, $
                                       POSITION=p1pos)

        plot_2            = SCATTERPLOT(mMagDat.K.magRat, $
                                        mMagDat.K.chi2, $
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
        ;; tickValues  = (mMagDat.K.chi2[SORT(mMagDat.K.chi2)])[cb_i]
        ;; tickName    = STRING(FORMAT='(F0.2)',tickValues)
        ;; tMagRange   = ALOG10(MINMAX(mMagDat.K.chi2))
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
