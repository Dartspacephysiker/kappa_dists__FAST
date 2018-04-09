;2018/04/06
PRO JOURNAL__20180406__CHECKOUT_ORB1612_MONTECARLO_OUTPUT

  COMPILE_OPT IDL2,STRICTARRSUBS

  dir            = '/SPENCEdata/Research/Satellites/FAST/kappa_dists/saves_output_etc/20180409/'
  ;; nRolls         = 1000
  nRolls         = 10000

  maxwellian     = 0

  ;; for spectra_average_interval = 3
  spectra_average_interval = 2

; P[0]: E_b,       Plasma bulk energy (eV)
; P[1]: T,         Plasma kinetic temperature (eV)
; P[2]: kappa,     Kappa (of course!)--or more specifically 3D kappa index, so that kappa eq kappa_0 + 3/2
; P[3]: n,         Plasma density (cm^-3)
  layoutNommers  = [2,3,4,1]
  binSizes       = [5.,5.,0.025,0.2]
  xMaxList       = LIST(!NULL,!NULL,4,30.)
  ;; redStdDevFacs  = LIST(10.,!NULL,10.,10.)
  ;; redStdDevFacs  = LIST(!NULL,!NULL,!NULL,!NULL)
  val = 4. & redStdDevFacs  = LIST(val,val,val,val)


  nRollsString   = STRING(FORMAT='(I0)',nRolls)
  avgItvlStr = STRING(FORMAT='("-avg_itvl",I0)',spectra_average_interval)
  CASE spectra_average_interval OF
     2: BEGIN
        file           = 'orb1612_2DMCarlo_ests__12_01_12__044_synthetic_wGauss-' $
                         + nRollsString + 'Rolls-fit2DParams' $
                         + avgItvlStr + '.sav'
        ;; file           = 'orb1612_2DMCarlo_ests__12_01_12__675_synthetic_wGauss-1000Rolls-fit2DParams-avg_itvl2.sav'
        file           = 'orb1612_2DMCarlo_ests__12_01_12__044_synthetic_wGauss-' $
                         + nRollsString + 'Rolls-fit2DParams' $
                         + avgItvlStr + '.sav'
        file           = 'orb1612_2DMCarlo_ests__12_01_12__675_synthetic_wGauss-10000Rolls-fit2DParams-avg_itvl2.sav'
        file           = 'orb1612_2DMCarlo_ests__12_01_13__305_synthetic_wGauss-10000Rolls-fit2DParams-avg_itvl2.sav'
        file           = 'orb1612_2DMCarlo_ests__12_01_24__027_synthetic_wGauss-10000Rolls-fit2DParams-avg_itvl2.sav'
     END
     3: BEGIN
        ;; file           = 'orb1612_2DMCarlo_ests__12_01_12__359_synthetic_wGauss-1000Rolls-fit2DParams.sav'
        ;; file           = 'orb1612_2DMCarlo_ests__12_01_12__675_synthetic_wGauss-1000Rolls-fit2DParams' $
        ;;                  + avgItvlStr + '.sav'
        ;; file           = 'orb1612_2DMCarlo_ests__12_01_13__305_synthetic_wGauss-1000Rolls-fit2DParams' $
        ;;                  + avgItvlStr + '.sav'
     END
  ENDCASE

  dateToCheck = '20180409'

  checkForFiles = 1
  dirForCheck = '/SPENCEdata/Research/Satellites/FAST/kappa_dists/saves_output_etc/' + dateToCheck + '/'
  filsPref    = 'orb1612_2DMCarlo_ests__12_0'
  orbDate     = '1997-01-17'
  ;; tBoundsStr  = orbDate + '/' + ['12:01:22.766','12:01:29.703']
  ;; tBoundsStr  = orbDate + '/' + ['12:01:15.766','12:01:29.703']
  tBoundsStr  = orbDate + '/' + ['12:01:00.00','12:01:14.703']
  tBounds     = S2T(tBoundsStr)
  
  IF FILE_TEST(dirForCheck,/DIRECTORY) AND KEYWORD_SET(checkForFiles) THEN BEGIN

     files = FILE_SEARCH(dirForCheck,filsPref + '*' + nRollsString + 'Rolls*' $
                            + avgItvlStr + '*')
     fileTids = STRMID(files, $
                        STRLEN(dirForCheck + 'orb1612_2DMCarlo_ests__'), $
                        13)

     fileTids = fileTids.Replace('__','.')
     fileTids = orbDate + '/' + fileTids.Replace('_',':')

     inds      = WHERE(S2T(fileTids) GE tBounds[0] AND $
                       S2T(fileTids) LE tBounds[1],nInds)
     IF nInds LT 1 THEN STOP
     theseFiles = files[inds]

  ENDIF

  CASE 1 OF
     KEYWORD_SET(checkForFiles): BEGIN

        FOR jj=0,nInds-1 DO BEGIN

           file = theseFiles[jj]
           RESTORE,file

           gP             = TEMPORARY(gaussFit2DParamArr)
           kP             = TEMPORARY(kappaFit2DParamArr)

           kOrigEsts      = fit2DKappa_info.fitParams
           gOrigEsts      = fit2DGauss_info.fitParams

           p              = kP
           ests           = kOrigEsts
           winTitle       = 'Kappa (' + T2S(fit2DKappa_info.sdt.time,/MS) + ')'
           
           gEstListK      = LIST()
           gEstListG      = LIST()

           nowMaxwellian  = 0
           FOR k=0,3 DO BEGIN

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


        ENDFOR

     END
     ELSE: BEGIN

        RESTORE,dir+file

        gP             = TEMPORARY(gaussFit2DParamArr)
        kP             = TEMPORARY(kappaFit2DParamArr)

        kOrigEsts      = fit2DKappa_info.fitParams
        gOrigEsts      = fit2DGauss_info.fitParams

        p              = kP
        ests           = kOrigEsts
        winTitle       = 'Kappa (' + T2S(fit2DKappa_info.sdt.time,/MS) + ')'
        
        gEstListK      = LIST()
        gEstListG      = LIST()


        FOR k=0,3 DO BEGIN

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

     END
  ENDCASE

  STOP
END
