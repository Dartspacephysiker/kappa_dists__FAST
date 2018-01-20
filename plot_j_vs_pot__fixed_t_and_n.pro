;2017/04/12
PRO PLOT_J_VS_POT__FIXED_T_AND_N,jvPlotData,avgs_JVfit,pData, $
                                 ;; USE_SOURCE_AVGS=use_source_avgs, $
                                 KAPPA_A=A, $
                                 GAUSS_A=AGauss, $
                                 ORBIT=orbit, $
                                 ORIGINATING_ROUTINE=routName, $
                                 SAVEPLOT=savePlot, $
                                 SPNAME=sPName, $
                                 PLOTDIR=plotDir, $
                                 IS_EFLUX=is_eFlux, $
                                 J_V__FIXTANDN__SAVEPLOTDATA=j_v__fixTandN__savePlotData, $
                                 J_V__FIXTANDN__DATAFILENAME=j_v__fixTandN__dataFilename, $
                                 NO_TITLE=no_title, $
                                 IN_MMAGDAT=mMagDat, $
                                 _EXTRA=e

  COMPILE_OPT IDL2,STRICTARRSUBS

  orbPref     = ''
  IF KEYWORD_SET(orbit) THEN BEGIN
     orbPref  = 'Orbit ' + STRCOMPRESS(orbit,/REMOVE_ALL)
  ENDIF

  ;; j_v__fixTandN__savePlotData = 1
  IF KEYWORD_SET(j_v__fixTandN__savePlotData) THEN BEGIN

     @common__jv_curve_fit__tie_r_b_and_dens.pro

     dir   = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/cur_and_pot_analysis/'
     fNamePref = KEYWORD_SET(j_v__fixTandN__dataFilename) ? j_v__fixTandN__dataFilename : 'fixTandN_plotData'
     count = 0
     fName = fNamePref + '.sav'
     WHILE FILE_TEST(dir+fName) DO BEGIN
        fName = STRING(FORMAT='(A0,"_",I02,".sav")',fNamePref,count+1)
        count++
     ENDWHILE

     PRINT,"Saving fixTandN plotdata file: " + fName

     SAVE,jvPlotData,avgs_JVfit,pData, $
          A, $
          AGauss, $
          orbit, $
          routName, $
          mMagDat, $
          tRB_RBpairs, $
          tRB_fLine, $
          tRB_nFAST, $
          tRB_nFLine, $
          tRB_fLineRE, $
          FILENAME=dir+fName
  ENDIF
  
  symbol      = '+'
  sym_thick   = 2.0
  thick       = 2.2
  font_size   = 18
  legFont_size = 16

  CURANDPOT__SELECT_T_AND_N,jvPlotData,avgs_JVfit, $
                            TEMPERATURE=temperature, $
                            DENSITY=density, $
                            /DONT_MAP_SOURCEDENS

  ;; IF KEYWORD_SET(avgs_JVfit.use_source_avgs) THEN BEGIN
  ;;    Temperature = avgs_JVfit.T_SC.avg
  ;;    Density     = avgs_JVfit.N_SC.avg
  ;; ENDIF ELSE BEGIN
  ;;    Temperature = avgs_JVfit.T.avg
  ;;    Density     = avgs_JVfit.N.avg
  ;; ENDELSE

  ;; showDens         = KEYWORD_SET(pData.is_sourceDens)

  ;; IF showDens THEN BEGIN
  CASE 1 OF
     KEYWORD_SET(no_title): BEGIN
        ;; titleStr      = ''
     END
     KEYWORD_SET(pData.is_sourceDens): BEGIN
        titleStr         = STRING(FORMAT='(A0," (T=",F0.1," eV, N!DFAST!N=",G0.3," cm!U-3!N)")', $
                                  orbPref,Temperature,Density)
     END
     ELSE: BEGIN
        titleStr         = STRING(FORMAT='(A0," (T=",F0.1," eV)")', $
                                  orbPref,Temperature)
     END
  ENDCASE
  ;; kappaName        = STRING(FORMAT='("$\kappa$=",F0.2,", R!DB!N=",G0.3)',A[0],A[3])
  ;; gaussName        = STRING(FORMAT='("Maxwell, R!DB!N=",G0.3)',AGauss[3])
  ;; kappaName        = STRING(FORMAT='("$\kappa$=",F0.2,", R!DB!N=",G0.3,", N=",G0.3,", T=",G0.3)',A[0],A[3],A[2],A[1])
  ;; gaussName        = STRING(FORMAT='("Maxwell, R!DB!N=",G0.3,", N=",G0.3,", T=",G0.3)',AGauss[3],AGauss[2],AGauss[1])
  ;; dataName         = 'Data'
  Nstring          = 'n' + (KEYWORD_SET(pData.is_sourceDens) ? '!Dm!N' : '')

  msec             = 1
  t1Str            = (STRSPLIT(TIME_TO_STR(MIN(jvPlotData.time[avgs_JVFit.useInds]),MSEC=msec),'/',/EXTRACT))[1]
  t2Str            = (STRSPLIT(TIME_TO_STR(MAX(jvPlotData.time[avgs_JVFit.useInds]),MSEC=msec),'/',/EXTRACT))[1]
  dataName         = STRING(FORMAT='(A0,"–",A0)',t1Str,t2Str)
  kappaName        = STRING(FORMAT='("$\kappa$=",F0.2,", R!DB!N=",G0.3,", ",A0,"=",G0.2," cm!U-3!N")',A[0],A[3],Nstring,A[2])
  gaussName        = STRING(FORMAT='("Maxwell, R!DB!N=",G0.3,", ",A0,"=",G0.2," cm!U-3!N")',AGauss[3],Nstring,AGauss[2])

  window1          = WINDOW(DIMENSION=[1000,800],BUFFER=savePlot)

  sortie           = SORT(pData.x)
  sortieFit        = SORT(pData.xFit)

  xFrac            = 0.1
  yFrac            = 0.1
  xRange           = MINMAX(pData.X)
  xRange           = [xRange[0]*((1.D)-xFrac),xRange[1]*((1.D)+xFrac)]

  yRFitInds        = WHERE((pData.XFit GE xRange[0]) AND (pData.XFit LE xRange[1]))
  yRange           = [MIN([pData.Y,pData.YFit[yRFitInds],pData.YGaussFit[yRFitInds]]),MAX(pData.Y+pData.Yerror)]
  yRange           = [yRange[0]*((1.D)-yFrac),yRange[1]*((1.D)+yFrac)]
  ;; yRange[0]        = 0 < yRange[0]

  ;; that             = ERRORPLOT(X,Y,XError,YError, $
  that             = ERRORPLOT(pData.X[sortie],pData.Y[sortie],pData.YError[sortie], $
                               SYMBOL=symbol, $
                               LINESTYLE='', $
                               NAME=dataName, $
                               TITLE=titleStr, $
                               XTITLE='$\Delta \Phi$ (V)', $
                               ;; YTITLE='Current Density at 100 km ($\mu$A/m!U2!N)', $
                               YTITLE=pData.yTitle, $
                               ;; XRANGE=xRange, $
                               XSTYLE=2, $
                               SYM_THICK=sym_thick, $
                               ;; YRANGE=MINMAX(pData.Y+pData.YError), $
                               YRANGE=yRange, $
                               FONT_SIZE=font_size, $
                               /CURRENT)
  
  ;; that          = PLOT(X,Y,SYMBOL='*',LINESTYLE='')
  this             = PLOT(pData.XFit[sortieFit],pData.YFit[sortieFit], $
                          NAME=kappaName, $
                          LINESTYLE='--', $
                          THICK=thick, $
                          FONT_SIZE=font_size, $
                          COLOR='BLUE', $
                          /OVERPLOT)

  those            = PLOT(pData.XFit[sortieFit],pData.yGaussFit[sortieFit], $
                          NAME=gaussName, $
                          LINESTYLE='-:', $
                          THICK=thick, $
                          FONT_SIZE=font_size, $
                          COLOR='RED', $
                          /OVERPLOT)

  ;; legPos__data  = [(MAX(X)-MIN(X))*0.2+MIN(X),(MAX(Y)-MIN(Y))*0.95+MIN(Y)]
  ;; legPos           = [0.5,0.85]
  legPos           = [0.53,0.83]
  ;; leg           = LEGEND(TARGET=[that,this,those],POSITION=legPos__data,/DATA)
  leg              = LEGEND(TARGET=[that,this,those], $
                            POSITION=legPos, $
                            FONT_SIZE=legFont_size)


  IF KEYWORD_SET(savePlot) THEN BEGIN

     IF ~KEYWORD_SET(plotDir) THEN BEGIN
        plotDir = './'
     ENDIF

     filNavn = KEYWORD_SET(SPName) ? SPName : (routName + '-JV_fixedTandN')
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

     PRINT,"Saving to " + filNavn + ' ...'

     window1.Save,plotDir+filNavn

     window1.Close
     window1=!NULL

  ENDIF


END
