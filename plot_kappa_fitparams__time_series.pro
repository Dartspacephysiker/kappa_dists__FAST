;;09/05/16
FUNCTION PLOT_KAPPA_FITPARAMS__TIME_SERIES,fit2D, $
   BULK_ENERGY=bulk_energy, $
   TEMPERATURE=temperature, $
   DENSITY=density, $
   KAPPA=kappa, $
   SUPPRESS_LINEPLOT=suppress_line, $
   SUPPRESS_SCATTERPLOT=suppress_scatter, $
   ;; SCATTERDATA=scatterData, $
   SCATTERSYMBOL=scatterSymbol, $
   SCATTERSYMCOLOR=scatterSymColor, $
   SCATTERSYMSIZE=scatterSymSize, $
   SCATTERSYMTRANSP=scatterSymTransp, $
   COLOR=color, $
   ADD_LEGEND=add_legend, $
   NXTICKS=nXTicks, $
   YLOG=yLog, $
   YRANGE=yRange, $
   XSHOWTEXT=xShowText, $
   XTHICK=xThick, $
   YTHICK=yThick, $
   XMINOR=xMinor, $
   LINESTYLE=lineStyle, $
   THICK=thick, $
   TITLE=title, $
   NAME=name, $
   NO_TIME_LABEL=no_time_label, $
   CURRENT=window, $
   LAYOUT=layout, $
   POSITION=position, $
   CLIP=clip, $
   MARGIN=margin, $
   BUFFER=buffer, $
   OVERPLOT=overplot, $
   SAVEPLOT=savePlot, $
   SPNAME=sPName, $
   PLOTDIR=plotDir, $
   ORBIT=orbit, $
   CLOSE_WINDOW_AFTER_SAVE=close_window_after_save


  COMPILE_OPT IDL2

  IF ~ISA(fit2D) THEN RETURN,-1

  IF ~ISA(window) THEN BEGIN
     window              = WINDOW(DIMENSIONS=[900,600])
  ENDIF

  orbStr                 = N_ELEMENTS(orbit) GT 0 ? 'orb_' + STRCOMPRESS(orbit,/REMOVE_ALL) : ''

  xTickFont_size         = 16
  xTickFont_style        =  1      
  yTickFont_size         = 16
  yTickFont_style        =  1      
  symSize                =  2.0            
  symThick               =  2.0           
  thick                  =  N_ELEMENTS(thick) GT 0 ? thick : 3.0
  xStyle                 = 1

  plots           = [KEYWORD_SET(bulk_energy), KEYWORD_SET(temperature), $
                     KEYWORD_SET(kappa)      , KEYWORD_SET(density)      ]
  pIndex          = WHERE(plots,nKWPlot)

  yRanges         = [ $
                    [MIN(fit2D.fitParams[0,*]),MAX(fit2D.fitParams[0,*])], $
                    [MIN(fit2D.fitParams[1,*]),MAX(fit2D.fitParams[1,*])], $
                    ;; [MIN(fit2D.fitParams[2,*]),MAX(fit2D.fitParams[2,*])], $
                    [1                        ,102                      ], $
                    [MIN(fit2D.fitParams[3,*]),MAX(fit2D.fitParams[3,*])] $
                    ]

  nameAbbrev      = [ $
                    'bulk_e', $
                    'temp', $
                    'kappa', $
                    'density' $
                    ]
  
  yTitle          = [ $
                    "Bulk Energy (eV)", $
                    "Temperature (eV)", $
                    "Kappa", $
                    "Density (cm!U-3!N)" $
                    ]

  yLog            = [1,1,1,1]

  xTitle          = 'Time since ' + TIME_TO_STR(fit2D.SDT[0].time,/MSEC)

  nPlots          = (~KEYWORD_SET(suppress_line) + (~KEYWORD_SET(suppress_scatter))) * nKWPlot
  plotArr         = MAKE_ARRAY(nPlots,/OBJ)

  IF N_ELEMENTS(scatterSymbol) EQ 0 THEN scatterSymbol = '+'

  ;; IF ~KEYWORD_SET(overplot) THEN BEGIN
  ;;    yRange          = KEYWORD_SET(yRange) ? $
  ;;                      yRange : [MIN(BPD.data[0,*]),-15]
  ;; ENDIF

  CASE 1 OF
     KEYWORD_SET(no_time_label): BEGIN
        x_values        = INDGEN(N_ELEMENTS(fit2D.SDT.time))
     END
     ELSE: BEGIN
        ;;For time axis
        ;; dummy           = LABEL_DATE(DATE_FORMAT=['%D-%M','%Y'])
        dummy           = LABEL_DATE(DATE_FORMAT=['%H:%I:%S'])
        x_values        = UTC_TO_JULDAY(fit2D.SDT.time)
        ;; xRange          = [MIN(x_values),MAX(x_values)]
        xRange          = [x_values[1]-(1/20864.),x_values[-1]+(1/20864.)]
     END
  ENDCASE

  iParam                = 0
  FOR iPlot=(0-KEYWORD_SET(suppress_line)),nPlots-1 DO BEGIN


     ;; PRINT,'Doing' + plotNames[i] + '...'

     IF ~KEYWORD_SET(suppress_line) THEN BEGIN
        plotArr[iPlot]      = PLOT(x_values, $
                                   fit2D.fitParams[pIndex[iParam],*], $
                                   XRANGE=xRange, $
                                   YRANGE=yRanges[*,pIndex[iParam]], $
                                   YLOG=yLog[pIndex[iParam]], $
                                   NAME=KEYWORD_SET(name) ? name[pIndex[iParam]] : !NULL, $
                                   XTITLE=xTitle, $
                                   YTITLE=yTitle[pIndex[iParam]], $
                                   XMAJOR=nXTicks, $
                                   XMINOR=xMinor, $
                                   XTICKVALUES=xTickValues, $
                                   XTICKFORMAT=KEYWORD_SET(no_time_label) ? $
                                   !NULL : 'LABEL_DATE', $
                                   XTICKUNITS=KEYWORD_SET(no_time_label) ? $
                                   !NULL : 'Time', $
                                   XTICKFONT_SIZE=xTickFont_size, $
                                   XTICKFONT_STYLE=xTickFont_style, $
                                   YTICKFONT_SIZE=yTickFont_size, $
                                   YTICKFONT_STYLE=yTickFont_style, $
                                   XSHOWTEXT=xShowText, $
                                   XTHICK=xThick, $
                                   YTHICK=yThick, $
                                   COLOR=color, $
                                   XSTYLE=xStyle, $
                                   LINESTYLE=lineStyle, $
                                   THICK=thick, $
                                   FILL_COLOR=fill_color, $
                                   LAYOUT=layout, $
                                   POSITION=position, $
                                   MARGIN=margin, $
                                   CLIP=clip, $
                                   BUFFER=buffer, $
                                   OVERPLOT=overplot, $
                                   CURRENT=window)
     ENDIF
     
     
     IF ~KEYWORD_SET(suppress_scatter) THEN BEGIN
        iPlot++

        plotArr[iPlot] = PLOT(x_values, $
                              fit2D.fitParams[pIndex[iParam],*], $
                              XRANGE=xRange, $
                              YRANGE=yRanges[*,pIndex[iParam]], $
                              YLOG=yLog[pIndex[iParam]], $
                              NAME=KEYWORD_SET(name) ? name[pIndex[iParam]] : !NULL, $
                              XTITLE=xTitle, $
                              YTITLE=yTitle[pIndex[iParam]], $
                              XMAJOR=nXTicks, $
                              SYMBOL=scatterSymbol, $
                              SYM_COLOR=scatterSymColor, $
                              SYM_SIZE=scatterSymSize, $
                              SYM_TRANSPARENCY=scatterSymTransp, $
                              SYM_THICK=1.6, $
                              XTICKVALUES=xTickValues, $
                              XTICKFORMAT=KEYWORD_SET(no_time_label) ? $
                              !NULL : 'LABEL_DATE', $
                              XTICKUNITS=KEYWORD_SET(no_time_label) ? $
                              !NULL : 'Time', $
                              XTICKFONT_SIZE=xTickFont_size, $
                              XTICKFONT_STYLE=xTickFont_style, $
                              YTICKFONT_SIZE=yTickFont_size, $
                              YTICKFONT_STYLE=yTickFont_style, $
                              XSHOWTEXT=xShowText, $
                              XTHICK=xThick, $
                              YTHICK=yThick, $
                              XMINOR=xMinor, $
                              XSTYLE=xStyle, $
                              LINESTYLE='', $
                              LAYOUT=layout, $
                              POSITION=position, $
                              MARGIN=margin, $
                              CLIP=clip, $
                              OVERPLOT=KEYWORD_SET(overplot) ? $
                              1 : ~KEYWORD_SET(suppress_line), $
                              CURRENT=window)

     ENDIF

     iParam++
  ENDFOR

  IF KEYWORD_SET(savePlot) THEN BEGIN

     pNamePref    = ''
     FOR i=0,N_ELEMENTS(plots)-1 DO BEGIN
        pNamePref += (KEYWORD_SET(plots[i]) ? nameAbbrev[i] + '--' : '' )
     ENDFOR

     IF KEYWORD_SET(spName) THEN outName = spName ELSE BEGIN
        outName = GET_TODAY_STRING() + '--' + orbStr + pNamePref + 'time_series.png'
     ENDELSE
     IF N_ELEMENTS(plotDir) GT 0 THEN BEGIN
        pDir = plotDir
     ENDIF ELSE BEGIN
        SET_PLOT_DIR,pDir,/ADD_TODAY,/FOR_KAPPA_DB
     ENDELSE

     PRINT,'Saving to ' + outName + '...'
     window.save,pDir+outName

     IF KEYWORD_SET(close_window_after_save) THEN BEGIN
        window.close
        window      = !NULL
     ENDIF

  ENDIF

  RETURN,plotArr
END
