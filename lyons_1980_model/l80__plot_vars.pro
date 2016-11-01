;;10/31/16
PRO L80__PLOT_VARS

  COMPILE_OPT IDL2

  @~/idl/lib/hatch_idl_utils/knight_relation_funcs/common__dk_ode18.pro
  @common__l80_model.pro

  ;; nCol = 3
  ;; nRow = 2
  ;; dims = [600,900]

  nCol = 3
  nRow = 2
  dims = [1000,600]

  IF N_ELEMENTS(l80Window) EQ 0 OR ~ISA(l80Window) THEN BEGIN
     l80Window = WINDOW(DIMENSIONS=dims)
  ENDIF ELSE BEGIN
     l80Window.Erase
     l80Window.setCurrent
  ENDELSE

  xRange = [l80xi[0],l80xi[-1]]/1.e3

  nPlots =7
  plotArr = MAKE_ARRAY(nPlots,/OBJ)
  ;;The potential
  iPlot = 0
  plotArr[iPlot] = PLOT(l80xi/1.e3,l80iPot/1.e3, $
                        XTITLE='Transverse Distance (km)', $
                        YTITLE='Potential (kV)', $
                        YLOG=0, $
                        XRANGE=xRange, $
                        YRANGE=[MIN([l80iPot,l80mPot]),MAX([l80iPot,l80mPot])]/1.e3, $
                        LAYOUT=[nCol,nRow,1], $
                        /CURRENT)
  iPlot++
  
  plotArr[iPlot] = PLOT(l80xi/1.e3,l80mPot/1.e3, $
                        LAYOUT=[nCol,nRow,1], $
                        /CURRENT, $
                        /OVERPLOT, $
                        LINESTYLE='--', $
                        COLOR=color)
  iPlot++


  ;;The potential difference
  plotArr[iPlot] = PLOT(l80xi/1.e3,(l80iPot-l80mPot)/1.e3, $
                        XTITLE='Transverse Distance (km)', $
                        YTITLE='Potential Difference (kV)', $
                        YLOG=1, $
                        XRANGE=xRange, $
                        YRANGE=[0.01,50], $
                        LAYOUT=[nCol,nRow,2], $
                        /CURRENT)
  iPlot++

  ;;The current density
  plotArr[iPlot] = PLOT(l80xi/1.e3,l80je, $
                        XTITLE='Transverse Distance (km)', $
                        YTITLE='Current Density (A m!U-2!N)', $
                        YLOG=1, $
                        XRANGE=xRange, $
                        YRANGE=[1e-7,1e-5], $
                        LAYOUT=[nCol,nRow,3], $
                        /CURRENT)
  iPlot++

  ;;The current density
  plotArr[iPlot] = PLOT(l80xi/1.e3,l80jEe, $
                        XTITLE='Transverse Distance (km)', $
                        YTITLE='Energy Flux (J m!U-2!Ns!U-1!N)', $
                        YLOG=1, $
                        XRANGE=xRange, $
                        YRANGE=[1e-4,5e-1], $
                        LAYOUT=[nCol,nRow,4], $
                        /CURRENT)
  iPlot++

  ;;The current density
  plotArr[iPlot] = PLOT(l80xi/1.e3,l80Sigma_P, $
                        XTITLE='Transverse Distance (km)', $
                        YTITLE='Pedersen Conductivity (mho)', $
                        YLOG=1, $
                        XRANGE=xRange, $
                        YRANGE=[2.0,100], $
                        LAYOUT=[nCol,nRow,5], $
                        /CURRENT)
  iPlot++

  ;;The second deriv
  plotArr[iPlot] = PLOT(l80xi/1.e3,l802deriv, $
                        XTITLE='Transverse Distance (km)', $
                        YTITLE='Second derivative', $
                        YLOG=1, $
                        XRANGE=xRange, $
                        YRANGE=[1e-11,1e-6], $
                        LAYOUT=[nCol,nRow,6], $
                        /CURRENT)
  iPlot++

END
