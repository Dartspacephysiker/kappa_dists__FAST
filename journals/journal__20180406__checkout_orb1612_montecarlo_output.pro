;2018/04/06
PRO JOURNAL__20180406__CHECKOUT_ORB1612_MONTECARLO_OUTPUT

  COMPILE_OPT IDL2,STRICTARRSUBS

  dir            = '/SPENCEdata/Research/Satellites/FAST/kappa_dists/saves_output_etc/20180406/'
  nRolls         = 1000
  ;; nRolls         = 10000

  nRollsString   = STRING(FORMAT='(I0)',nRolls)
  file           = 'orb1612_2DMCarlo_ests__12_01_12__044_synthetic_wGauss-' $
                   + nRollsString + 'Rolls-fit2DParams.sav'
  ;; for spectra_average_interval = 3
  file           = 'orb1612_2DMCarlo_ests__12_01_12__359_synthetic_wGauss-1000Rolls-fit2DParams.sav'

  RESTORE,dir+file

  gP             = TEMPORARY(gaussFit2DParamArr)
  kP             = TEMPORARY(kappaFit2DParamArr)

  kOrigEsts      = fit2DKappa_info.fitParams
  gOrigEsts      = fit2DGauss_info.fitParams

; P[0]: E_b,       Plasma bulk energy (eV)
; P[1]: T,         Plasma kinetic temperature (eV)
; P[2]: kappa,     Kappa (of course!)--or more specifically 3D kappa index, so that kappa eq kappa_0 + 3/2
; P[3]: n,         Plasma density (cm^-3)
  layoutNommers  = [2,3,4,1]
  binSizes       = [5.,5.,0.5,0.05]
  xMaxList       = LIST(!NULL,!NULL,35.,30.)
  redStdDevFacs  = LIST(10.,!NULL,10.,10.)

  p              = kP
  ests           = kOrigEsts
  winTitle       = 'Kappa (' + T2S(fit2DKappa_info.sdt.time,/MS) + ')'
  
  gEstListK      = LIST()
  gEstListG      = LIST()

  maxwellian     = 1
  FOR k=0,3 DO BEGIN

     IF KEYWORD_SET(nowMaxwellian) AND k EQ 2 THEN k++

     curParam      = REFORM(p[k,*])
     origParamEst  = ests[k]
     layoutNommer  = layoutNommers[k]
     binSize       = binSizes[k]

     KAPPA_FIT2D__GAUSSFIT_MONTECARLO_PARAM_OUTPUT, $
        curParam, $
        origParamEst, $
        binSize, $
        layoutNommer, $
        ;; XTITLE=xTitle, $
        REDUCE_STD_DEV_FACTOR=redStdDevFacs[k], $
        XMAX=xMaxList[k], $
        WINDOW=window, $
        WINTITLE=winTitle, $
        OUT_ESTIMATE=gEst

     gEstListK.Add,gEst
  
  ENDFOR

  IF KEYWORD_SET(maxwellian) THEN BEGIN
  
     nowMaxwellian = 1
     p           = gP
     ests        = gOrigEsts
     winTitle    = 'Maxwellian (' + T2S(fit2DKappa_info.sdt.time,/MS) + ')'
     
     FOR k=0,3 DO BEGIN

        IF KEYWORD_SET(nowMaxwellian) AND k EQ 2 THEN k++

        curParam      = REFORM(p[k,*])
        origParamEst  = ests[k]
        layoutNommer  = layoutNommers[k]
        binSize       = binSizes[k]

        KAPPA_FIT2D__GAUSSFIT_MONTECARLO_PARAM_OUTPUT, $
           curParam, $
           origParamEst, $
           binSize, $
           layoutNommer, $
           ;; XTITLE=xTitle, $
           REDUCE_STD_DEV_FACTOR=redStdDevFacs[k], $
           XMAX=xMaxList[k], $
           WINDOW=gWindow, $
           WINTITLE=winTitle, $
           OUT_ESTIMATE=gEst

        gEstListG.Add,gEst
        
     ENDFOR

  ENDIF

  STOP
END
