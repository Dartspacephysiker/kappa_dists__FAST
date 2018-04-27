;2018/04/27
PRO JOURNAL__20180427__Q_METASTABILITY_PLOT__LIVADIOTIS_AND_MCCOMAS_2009

  COMPILE_OPT IDL2,STRICTARRSUBS

  buff         = 1

  plotDir      = '/SPENCEdata/Research/Satellites/FAST/kappa_dists/plots/'


  kappa = POWGEN(1.5D,1D3,1.2D)

  q     = 1.+1./kappa

  M     = 4.*(q-1.)/(q+1.)

  qWin  = WINDOW(BUFFER=buff)
  qPlot = PLOT(q,M, $
               XTITLE='q', $
               YTITLE='M', $
               CURRENT=qWin)

  qPlot.Save,plotDir+'MandqPlot.png'

  kWin  = WINDOW(BUFFER=buff)
  kPlot = PLOT(kappa,M, $
               /XLOG, $
               XTITLE='$\kappa$', $
               YTITLE='M', $
               CURRENT=kWin)

  kPlot.Save,plotDir+'MandkappaPlot.png'

END
