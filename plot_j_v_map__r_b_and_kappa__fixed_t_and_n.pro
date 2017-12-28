;2017/04/12
PRO PLOT_J_V_MAP__R_B_AND_KAPPA__FIXED_T_AND_N,mMagDat,jvPlotData,avgs_JVFit, $
   MAP__2D=map__2D, $
   MAP2D__LOG_KAPPA=map2D__log_kappa, $
   ORBIT=orbit, $
   IN_KAPPA_A=A, $
   IN_GAUSS_A=AGauss, $
   OUT_YBEST=out_yBest, $
   SAVEPLOT=savePlot, $
   SPNAME=SPName, $
   PLOTDIR=plotDir, $
   IS_EFLUX=is_eFlux, $
   ZOOM_ON_EXTREME_KAPPA=zoom_on_extreme_kappa, $
   _EXTRA=e

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;;For adding an R_E axis, if that's wassup
  @common__jv_curve_fit__tie_r_b_and_dens.pro

  contChiPlotName  = 'contChiPlot'

  rgbTable         = 0
  ctBottom         = 30
  nColorsIn        = 256
  nContours        = 5
  transpose        = 1
  ;; hammerCT         = COLORTABLE(rgbTable,STRETCH=stretch,NCOLORS=nColors,TRANSPOSE=transpose)
  ;; hammerCT         = COLORTABLE(rgbTable,STRETCH=stretch,NCOLORS=nColors,TRANSPOSE=transpose)
  LOADCT,rgbTable,NCOLORS=nColors,RGB_TABLE=hammerCT
  hammerCT         = [hammerCT[ctBottom:-1,0],hammerCT[ctBottom:-1,1],hammerCT[ctBottom:-1,2]]
  hammerCT         = CONGRID(REFORM(hammerCT,N_ELEMENTS(hammerCT)/3,3),256,3,/INTERP)
  nColors          = N_ELEMENTS(hammerCT[*,0])
  ;; ctInds           = FIX(FINDGEN(nContours+1)/(nContours)*(nColors-1))+ctBottom
  ;; nKillEm          = 40
  ;; hammerCT         = hammerCT[*,nKillEm:-1]
  ;; ctInds           = FIX(FINDGEN(nContours)/(nContours-1)*(nColors-1-nKillEm))
  include_TTaper   = 1
  include_BTaper   = 0
  CASE 1 OF
     include_TTaper AND include_BTaper: BEGIN
        taper      = 1
     END
     include_TTaper: BEGIN
        taper      = 3
     END
     include_BTaper: BEGIN
        taper      = 2
     END
  ENDCASE

  defFontSize      = 14
  defBigFontSize   = 18
  defMidFontSize   = 16

  nLinCBTicks      = 10
  nLogCBTicks      = 5

  add_surface_inl  = 0
  surfKappaRange   = [MIN(mMagDat.K.kappa),1.8]
  xSRange          = [40,MAX(mMagDat.K.magRat)]

  ;;First get CB range
  log_cbRange = 1
  IF log_cbRange THEN BEGIN

     zVar          = ALOG10(mMagDat.K.chi2)

     cbRange       = ALOG10(MINMAX(mMagDat.K.chi2 < 1D2))

     cbRange[0]    = cbRange[0] < 0.0
     ;; cbRange[0]    = cbRange[0]
  ENDIF ELSE BEGIN

     zVar          = mMagDat.K.chi2
     ;; cbRange       = MINMAX(mMagDat.K.chi2 < 1.0D2)
     cbRange       = MINMAX(mMagDat.K.chi2 < 30.05)
     ;; cbRange       = [5.,MAX(mMagDat.K.chi2 < 15.)]
     ;; cbRange       = MINMAX(mMagDat.K.chi2) < 15.05
     ;; cbRange       = MINMAX(mMagDat.K.chi2) < 10.05
     ;; cbRange       = MINMAX(mMagDat.K.chi2) < 5.05

  ENDELSE

  ;;Now do contour things
  tickValues       = (FINDGEN(nContours+include_TTaper+include_BTaper))/(nContours-1+include_TTaper+include_BTaper)*(cbRange[1]-cbRange[0])+cbRange[0]
  c_values         = FINDGEN(nContours+include_TTaper+include_BTaper)/(nContours-1+include_TTaper+include_BTaper)*(cbRange[1]-cbRange[0])+cbRange[0]

  cb_use_c_vals    = 1
  ;; PRINT,"C values"
  ;; PRINT,c_values


  ;;Now finish the colorbar job
  IF log_cbRange THEN BEGIN

     nCBTicks      = nLogCBTicks
     ;; tickValues    = KEYWORD_SET(cb_use_c_vals) ? c_values : FINDGEN(nCBTicks+1)/nCBTicks*(cbRange[1]-cbRange[0])+cbRange[0]
     IF ~KEYWORD_SET(cb_use_c_vals) THEN BEGIN
        tickValues    = FINDGEN(nCBTicks+1)/nCBTicks*(cbRange[1]-cbRange[0])+cbRange[0]
     ENDIF

     ;; tickName   = STRING(FORMAT='(F0.2)',10.^tickValues)
     IF (WHERE(tickValues LT 0.))[0] NE -1 THEN BEGIN
        cbRange[0] = FLOOR(cbRange[0])
        tickValues = FLOOR(tickValues)
        tickValues = tickValues[UNIQ(tickValues,SORT(tickValues))]
        tickName   = STRING(FORMAT='(F0.2)',10.^tickValues)
     ENDIF ELSE BEGIN
        tickName   = STRING(FORMAT='(F0.1)',10.^tickValues)
     ENDELSE
  ENDIF ELSE BEGIN

     nCBTicks      = nLinCBTicks
     tickValues    = KEYWORD_SET(cb_use_c_vals) ? c_values : FINDGEN(nCBTicks+1)/nCBTicks*(cbRange[1]-cbRange[0])+cbRange[0]

     ;; tickName   = STRING(FORMAT='(F0.2)',10.^tickValues)
     IF (WHERE(tickValues LT 0.))[0] NE -1 THEN BEGIN
        cbRange[0] = FLOOR(cbRange[0])
        ;; tickValues    = FLOOR(tickValues)
        tickValues = ROUND_TO_NTH_DECIMAL_PLACE(tickValues,-1,/FLOOR)
        tickValues = tickValues[UNIQ(tickValues,SORT(tickValues))]
        tickName   = STRING(FORMAT='(F0.2)',tickValues)
     ENDIF ELSE BEGIN
        tickName   = STRING(FORMAT='(F0.1)',tickValues)
     ENDELSE

  ENDELSE

  IF include_TTaper THEN tickName = [tickName,'bro']
  ;; IF include_BTaper THEN tickName = ['bro',tickName]

  ;;Winder
  savePlot = 1
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
  cbpos            = [0.96,0.06,0.975,0.92]
  contPos          = [0.09,0.19,0.82,0.92]
  surfPos          = [0.50,0.40,0.75,0.80]


  orbPref          = ''
  IF KEYWORD_SET(orbit) THEN BEGIN
     orbPref       = 'Orbit ' + STRCOMPRESS(orbit,/REMOVE_ALL)
  ENDIF

  CURANDPOT__SELECT_T_AND_N,jvPlotData,avgs_JVfit, $
                            TEMPERATURE=Temperature, $
                            DENSITY=Density, $
                            /DONT_MAP_SOURCEDENS ;, $

  titleStr         = STRING(FORMAT='(A0," (T!DF!N=",I0," eV, n!DF!N=",G0.3," cm!U-3!N)")', $
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

     ;; R_B_axis_names   = [R_B_axis_vals,(R_B_axis_vals[-1]*10) < MAX(mMagDat.K.magRat)]

     R_B_axis_names   = STRING(FORMAT='(I0)',R_B_axis_vals)
     R_E_axis_vals    = INTERPOL(tRB_fLineRE,REFORM(tRB_RBpairs[1,*]),R_B_axis_vals)
     ;; R_B_FAST  = INTERPOL(REFORM(tRB_RBpairs[0,*]),REFORM(tRB_RBpairs[1,*]),R_B)
     nVals            = N_ELEMENTS(R_E_axis_vals)
     ;; nValsStr         = STRING('(I0)',nVals)
     ;; R_E_axis_names   = STRING(FORMAT='('+nValsStr+'(F0.1))',R_E_axis_vals)
     R_E_axis_names   = STRING(FORMAT='(F0.2)',R_E_axis_vals)

  ENDIF

  ;; xTitle = 'Mirror ratio'
  xTitle = 'R!DB!N'

  CASE 1 OF
     KEYWORD_SET(map__2D): BEGIN


        xVar       = mMagDat.K.magRat
        yVar       = mMagDat.K.kappa
        ;; yTitle     = 'Kappa'
        yTitle     = '$\kappa$'

        instead_q  = 0
        IF KEYWORD_SET(instead_q) THEN BEGIN
           yVar    = 1D + 1D/(mMagDat.K.kappa-1.5D)
           yTitle  = 'q'
           yRange  = MINMAX(yVar)
           ySRange = 1D + 1D/(surfKappaRange-1.5D)
        ENDIF ELSE BEGIN
           yRange  = MINMAX(yVar)
           yRange  = [yRange[0],KEYWORD_SET(zoom_on_extreme_kappa) ? 1.8 : 3.0]
           ySRange = surfKappaRange
        ENDELSE

        surfInds   = WHERE((yVar GE ySRange[0]) AND (yVar LE ySRange[1]) AND $
                           (xVar GE xSRange[0]) AND (xVar LE xSRange[1]))
        zSRange    = MINMAX(zVar[surfInds]) < MAX(cbRange)


        ;; nPoints     = N_ELEMENTS(mMagDat.K.magRat)

        ;; IF KEYWORD_SET(minSurface) THEN BEGIN

        ;;    R = MIN_CURVE_SURF(ALOG10(mMagDat.K.chi2),mMagDat.K.magRat,mMagDat.K.kappa, $
        ;;                       YOUT=mMagDat.K.kappa[*,0], $
        ;;                       XOUT=REFORM(mMagDat.K.magRat[0,*]))

        ;; ENDIF ELSE BEGIN

        contPlot    = CONTOUR(zVar, $
                              xVar, $
                              yVar, $
                              NAME=contChiPlotName, $
                              XRANGE=MINMAX(mMagDat.K.magRat), $
                              XTITLE=xTitle, $
                              YTITLE=yTitle, $
                              /XLOG, $
                              YLOG=map2D__log_kappa OR instead_q, $
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
                              FONT_SIZE=defBigFontSize, $
                              XTICKFONT_SIZE=defMidFontSize, $
                              YTICKFONT_SIZE=defMidFontSize, $
                              RGB_TABLE=hammerCT, $
                              POSITION=contPos, $
                              /CURRENT)

        ;; ENDELSE
        
        cb          = COLORBAR(TITLE='$\chi$!U2!Dred!N', $
                               TARGET=contChiPlotName, $
                               ORIENTATION=1, $
                               TEXT_ORIENTATION=0, $
                               ;; RGB_TABLE=hammerCT[*,ctInds], $
                               ;; TEXTPOS=1, $
                               TAPER=taper, $
                               TICKDIR=1, $
                               /BORDER, $
                               MAJOR=nContours, $
                               TICKNAME=tickName, $
                               ;; TICKVALUES=tickValues[1:-1], $
                               POSITION=cbpos, $
                               ;; RANGE=cbRange, $
                               FONT_SIZE=defMidFontSize, $
                               /NORMAL)


        IF KEYWORD_SET(make_R_E_axis) THEN BEGIN
           R_Eaxis = AXIS('X', $
                          TARGET=contPlot, $
                          ;; LOCATION="bottom", $
                          LOCATION=[0,MIN(contPlot.yrange)-0.48*(MAX(yRange)/5.0)^1.6,0], $
                          TITLE='R!DE!N', $
                          SUBTICKLEN=0.0, $
                          TICKLEN=0.015, $
                          TICKFONT_SIZE=defMidFontSize, $
                          TICKVALUES=R_B_axis_vals, $
                          TICKNAME=R_E_axis_names);, $
                          ;; AXIS_RANGE=MINMAX(R_E_axis_vals))
        ENDIF


        checkMe = mMagDat.K.chi2
        checkMeG = mMagDat.G.chi2
        ;; checkMe = SMOOTH(mMagDat.K.chi2,[11,11])
        ;; checkMeG = mMagDat.G.chi2
        junkK = MIN(checkMe,indK)
        junkG = MIN(checkMeG,indG)

        close_i     = WHERE(mMagDat.K.magRat LT 20)
        junkKClose  = MIN(checkMe[close_i],indKClose)

        IF N_ELEMENTS(tRB_fLineRE) GT 0 THEN BEGIN
           winKR_E  = INTERPOL(tRB_fLineRE,REFORM(tRB_RBpairs[1,*]),mMagDat.K.magRat[indK])
           winGR_E  = INTERPOL(tRB_fLineRE,REFORM(tRB_RBpairs[1,*]),mMagDat.G.magRat[indG])
           winKCR_E = INTERPOL(tRB_fLineRE,REFORM(tRB_RBpairs[1,*]),mMagDat.K.magRat[close_i[indKClose]])
        ENDIF ELSE BEGIN
           winKR_E  = 0.
           winGR_E  = 0.
           winKCR_E = 0.
        ENDELSE

        winK        = {chi2   : checkMe[indK], $
                       magRat : mMagDat.K.magRat[indK], $
                       yVar   : yVar[indK], $
                       R_E    : winKR_E}
        winG        = {chi2   : checkMeG[indG], $
                       magRat : mMagDat.G.magRat[indG], $
                       R_E    : winGR_E}
        winKClose   = {chi2   : checkMe[close_i[indKClose]], $
                       magRat : mMagDat.K.magRat[close_i[indKClose]], $
                       yVar   : yVar[close_i[indKClose]], $
                       R_E    : winKCR_E}

        PRINT,"WIN2D"
        PRINT,"******"
        PRINT,"Kappa"
        PRINT,"******"
        PRINT,FORMAT='(A0,T15,A0,T25,A0,T35,A0)', $
              'Chi^2_red','R_B','R_E',yTitle
        PRINT,FORMAT='(F0.2,T15,F0.2,T25,F0.2,T35,F0.2)', $
              winK.chi2,winK.magRat,winK.R_E,winK.yVar
        PRINT,''
        PRINT,"******"
        PRINT,"Maxwell"
        PRINT,"******"
        PRINT,FORMAT='(A0,T15,A0,T25,A0)', $
              'Chi^2_red','R_B','R_E'
        PRINT,FORMAT='(F0.2,T15,F0.2,T25,F0.2)', $
              winG.chi2,winG.magRat,winG.R_E
        PRINT,''
        PRINT,"******"
        PRINT,"Close (R_B < 20)"
        PRINT,"******"
        PRINT,FORMAT='(A0,T15,A0,T25,A0,T35,A0)', $
              'Chi^2_red','R_B','R_E',yTitle
        PRINT,FORMAT='(F0.2,T15,F0.2,T25,F0.2,T35,F0.2)', $
              winKClose.chi2,winKClose.magRat,winKClose.R_E,winKClose.yVar
        PRINT,''

        addWinSym = 1
        IF KEYWORD_SET(addWinSym) THEN BEGIN

           ;; winX = [winK.magRat,winG.magRat,winKClose.magRat] > (MIN(mMagDat.K.magRat)*1.01) < (MAX(mMagDat.K.magRat)*0.88)
           ;; winY = [winK.yVar,KEYWORD_SET(instead_q) ? MIN(yRange) : MAX(yRange),winKClose.yVar] < (MAX(yRange)*0.99) > (MIN(yRange)*1.01)
           ;; winX = [winK.magRat,winG.magRat,winKClose.magRat]
           ;; winY = [winK.yVar,KEYWORD_SET(instead_q) ? MIN(yRange) : MAX(yRange),winKClose.yVar]
           winX = [winK.magRat,winG.magRat]
           winY = [winK.yVar,KEYWORD_SET(instead_q) ? MIN(yRange) : MAX(yRange)+0.08*(MAX(yRange)/5.0)^1.5]

           ;; labels = 'Min $\chi$!U2!N!Dred,' + ['K','M'] + '!N'
           ;; labels = 'Min $\chi$!U2!N!Dred!N,' + ['$\kappa$','M'] + '!N'
           ;; labels = 'Min $\chi$!U2!N!Dred!N (' + ['Kappa','Maxwellian'] + ')'
           labels = 'Min $\chi$!U2!N!Dred,' + ['Kappa','Maxwellian ($\kappa \rightarrow \infty$)'] + '!N'

           ;;For not-local analysis
           ;; labPos   = ['TL','BR']
           ;; labColor = ['black','black']

           ;;For local analysis
           ;; labPos = ['T','BL']
           labPos = ['TR','BL']
           labColor = ['white','white']
           ;; labColor = ['light gray','light gray']

           nSyms = N_ELEMENTS(labPos)

           ;; mySymChoiceIsMINE = 'Star'
           ;; mySymChoiceIsMINE = '*'
           ;; symThick = 3.0
           ;; mySymChoiceIsMINE = 'tu' ;triangle up
           ;; mySymChoiceIsMINE = REPLICATE('d',nSyms) ;diamond
           mySymChoiceIsMINE = ['d','tu']
           symColor = REPLICATE('Black',nSyms)
           ;; symFill  = 'White'
           symFill  = REPLICATE('Light Gray',nSyms)
           symSize  = REPLICATE(2,nSyms)
           labFontStyle = REPLICATE(1,nSyms) ;bold
           labelFontSize = defFontSize+2
           ;; symColor = 'Gray'
           ;; symFill  = 'Black'

           FOR k=0,nSyms-1 DO BEGIN

              syms = SYMBOL(winX[k], $
                            winY[k], $
                            mySymChoiceisMINE[k], $
                            LABEL_COLOR=labColor[k], $
                            LABEL_STRING=labels[k], $
                            LABEL_POSITION=labPos[k], $
                            SYM_COLOR=symColor[k], $
                            SYM_FILL_COLOR=symFill[k], $
                            /DATA, $
                            /SYM_FILLED, $
                            SYM_SIZE=symSize[k], $
                            SYM_THICK=N_ELEMENTS(symThick) GT 0 ? symThick[k] : !NULL, $
                            LABEL_FONT_SIZE=labelFontSize, $
                            ;; LABEL_FONT_STYLE=N_ELEMENTS(labFontStyle) GT 0 ? labFontStyle[k] : !NULL, $
                            CLIP=0)

           ENDFOR

        ENDIF


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

  IF KEYWORD_SET(add_surface_inl) THEN BEGIN
     zSInds   = ARRAY_INDICES(zVar,surfInds)
     ;; zSInds   = [zSInds[0,*],zSInds[1,*]]

     ;; max(zsinds[0,*])
     ;; 54
     ;; min(zsinds[0,*])
     ;; 0
     ;; max(zsinds[1,*])
     ;; 180
     ;; min(zsinds[1,*])
     ;; 71

     indables = [MAX(zSInds[1,*])-MIN(zSInds[1,*]),MAX(zSInds[0,*])-MIN(zSInds[0,*])]+1
     zSurf    = REFORM(zVar[zSInds[0,*],zSInds[1,*]],indables)
     xSurf    = REFORM(xVar[zSInds[0,*],zSInds[1,*]],indables)
     ySurf    = REFORM(yVar[zSInds[0,*],zSInds[1,*]],indables)

     ;; surfPlot = SURFACE(TRI_SURF(zSurf, $
     ;;                             XVALUES=xSurf[UNIQ(xSurf,SORT(xSurf))], $
     ;;                             YVALUES=ySurf[UNIQ(ySurf,SORT(ySurf))],/LINEAR), $
     ;;                             XVALUES=REFORM(xSurf), $
     ;;                             YVALUES=REFORM(ySurf),/LINEAR), $
     ;; surfPlot = SURFACE(MIN_CURVE_SURF(zSurf, $
     ;;                                   XVALUES=xSurf[UNIQ(xSurf,SORT(xSurf))], $
     ;;                                   YVALUES=ySurf[UNIQ(ySurf,SORT(ySurf))], $
     ;;                                   XPOUT=xOut, $
     ;;                                   YPOUT=yOut), $
     ;; surfPlot = SURFACE(zVar[[zSInds[0,*],zSInds[1,*]]],xVar[[zSInds[0,*],zSInds[1,*]]],yVar[[zSInds[0,*],zSInds[1,*]]], $
     ;; surfPlot = SURFACE(MIN_CURVE_SURF(zVar[zSInds[0,*],zSInds[1,*]]), $
     ;; surfPlot = SURFACE(SMOOTH(zVar[zSInds[0,*],zSInds[1,*]],[5,1]), $
     ;; surfPlot = SURFACE((SMOOTH(zVar,[5,5]))[zSInds[0,*],zSInds[1,*]], $
     surfPlot = SURFACE(SMOOTH(zSurf,[11,11]), $
                        xSurf, $
                        ySurf, $
                        XRANGE=xSRange, $
                        YRANGE=ySRange, $
                        ZRANGE=zSRange, $
                        /CURRENT, $
                        POSITION=surfPos)

  ENDIF

  IF KEYWORD_SET(savePlot) THEN BEGIN

     IF N_ELEMENTS(plotDir) EQ 0 THEN BEGIN
        ;; SET_PLOT_DIR,plotDir,/FOR_KAPPA_DB,/ADD_TODAY
        plotDir = './'
     ENDIF

     filNavn = KEYWORD_SET(SPName) ? SPName : 'kappa_rB_map' + $
               (KEYWORD_SET(is_eFlux) ? '-eFlux' : '') + '.png'
     filTmp  = STRSPLIT(filNavn,'.',/EXTRACT)
     filPref = (filTmp)[0]
     filSuff = N_ELEMENTS(filTmp) GT 1 ? '.' + filTmp[1] : '.png'
     IF KEYWORD_SET(is_eFlux) THEN filPref += '-eFlux'

     count = 0
     WHILE FILE_TEST(plotDir+filNavn) DO BEGIN
        count++
        filNavn = STRING(FORMAT='(A0,I02,A0)', $
                         filPref, $
                         count, $
                         filSuff)
     ENDWHILE

     PRINT,"Saving " + filNavn + ' ...'
     window1.Save,plotDir + filNavn

     window1.Close
     window1 = !NULL
  ENDIF

END
