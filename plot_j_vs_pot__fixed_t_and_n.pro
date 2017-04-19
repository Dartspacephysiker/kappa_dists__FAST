;2017/04/12
PRO PLOT_J_VS_POT__FIXED_T_AND_N,jvPlotData,avgs_JVfit,pData, $
                                 ;; USE_SOURCE_AVGS=use_source_avgs, $
                                 KAPPA_A=A, $
                                 GAUSS_A=AGauss, $
                                 ORBIT=orbit, $
                                 ORIGINATING_ROUTINE=routName, $
                                 SAVEPLOT=savePlot, $
                                 SPNAME=sPName, $
                                  _EXTRA=e

  COMPILE_OPT IDL2,STRICTARRSUBS

  orbPref     = ''
  IF KEYWORD_SET(orbit) THEN BEGIN
     orbPref  = 'Orbit ' + STRCOMPRESS(orbit,/REMOVE_ALL)
  ENDIF

  CURANDPOT__SELECT_T_AND_N,jvPlotData,avgs_JVfit, $
                            TEMPERATURE=temperature, $
                            DENSITY=density

  ;; IF KEYWORD_SET(avgs_JVfit.use_source_avgs) THEN BEGIN
  ;;    Temperature = avgs_JVfit.T_SC.avg
  ;;    Density     = avgs_JVfit.N_SC.avg
  ;; ENDIF ELSE BEGIN
  ;;    Temperature = avgs_JVfit.T.avg
  ;;    Density     = avgs_JVfit.N.avg
  ;; ENDELSE

  titleStr         = STRING(FORMAT='(A0," (T=",F0.1," eV, N=",G0.3," cm!U-3!N)")', $
                            orbPref,Temperature,Density)
  ;; kappaName        = STRING(FORMAT='("$\kappa$=",F0.2,", R!DB!N=",G0.3)',A[0],A[3])
  ;; gaussName        = STRING(FORMAT='("Maxwell, R!DB!N=",G0.3)',AGauss[3])
  kappaName        = STRING(FORMAT='("$\kappa$=",F0.2,", R!DB!N=",G0.3,", N=",G0.3,", T=",G0.3)',A[0],A[3],A[2],A[1])
  gaussName        = STRING(FORMAT='("Maxwell, R!DB!N=",G0.3,", N=",G0.3,", T=",G0.3)',AGauss[3],AGauss[2],AGauss[1])

  window1          = WINDOW(DIMENSION=[1000,800],BUFFER=savePlot)

  sortie           = SORT(pData.x)

  ;; that             = ERRORPLOT(X,Y,XError,YError, $
  that             = ERRORPLOT(pData.X[sortie],pData.Y[sortie],pData.YError[sortie], $
                               SYMBOL='*', $
                               LINESTYLE='', $
                               NAME='Data', $
                               TITLE=titleStr, $
                               XTITLE='$\Phi$ (V)', $
                               YTITLE='Current Density at 100 km ($\mu$A/m!U2!N)', $
                               /CURRENT)
  
  ;; that          = PLOT(X,Y,SYMBOL='*',LINESTYLE='')
  this             = PLOT(pData.X[sortie],pData.YFit[sortie], $
                          NAME=kappaName, $
                          COLOR='BLUE', $
                          /OVERPLOT)
  those            = PLOT(pData.X[sortie],pData.yGaussFit[sortie], $
                          NAME=gaussName, $
                          COLOR='Brown', $
                          /OVERPLOT)

  ;; legPos__data  = [(MAX(X)-MIN(X))*0.2+MIN(X),(MAX(Y)-MIN(Y))*0.95+MIN(Y)]
  ;; legPos           = [0.5,0.85]
  legPos           = [0.5,0.5]
  ;; leg           = LEGEND(TARGET=[that,this,those],POSITION=legPos__data,/DATA)
  leg              = LEGEND(TARGET=[that,this,those], $
                            POSITION=legPos)


  IF KEYWORD_SET(savePlot) THEN BEGIN

     IF ~KEYWORD_SET(sPName) THEN BEGIN
        sPName     = routName + '-JV_fixedTandN.png'
     ENDIF

     IF ~KEYWORD_SET(plotDir) THEN BEGIN
        pDirSuff   = '/cur_and_pot_analysis'
        SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF=pDirSuff
     ENDIF

     PRINT,"Saving to " + sPName + ' ...'

     window1.Save,plotDir+sPName

     window1.Close
     window1=!NULL

  ENDIF


END
