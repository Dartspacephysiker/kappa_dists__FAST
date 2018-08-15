;2018/08/14
;; Red dashed line gives the coefficient from a Gaussian fit to the parameter histogram
;; Green dot-dashed line gives a simple mean
;; Blue long-dashed line gives the original estimate

;As of 2018/08/14 I have added the USE_DENSITY_MOMENT__NOT_FIT_PARAM keyword. This is because the actual density fit parameter may not be what we want to report.
PRO JOURNAL__20180814__CHECKOUT_ORB4682_MONTECARLO_OUTPUT, $
   ;; BUFFER=buffer, $
   SAVEPLOT=savePlot

  COMPILE_OPT IDL2,STRICTARRSUBS

  dir            = '/SPENCEdata/Research/Satellites/FAST/kappa_dists/saves_output_etc/20180815/'
  nRolls         = 1000

  maxwellian     = 0

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


  IF KEYWORD_SET(savePlot) THEN BEGIN

     IF N_ELEMENTS(plotDir) EQ 0 THEN BEGIN
        SET_PLOT_DIR,plotDir,/FOR_KAPPA_DB,/ADD_TODAY,/VERBOSE
     ENDIF

     MCPlotSuff = "-2DMC_histos"
     fExt       = '.png'

     tmpDir = plotDir
     IF KEYWORD_SET(add_plotDir_suff) THEN BEGIN
        
        tmpDir += '/' + add_plotDir_suff
        IF ~FILE_TEST(tmpDir,/DIRECTORY) THEN BEGIN
           PRINT,"Making directory " + tmpDir
           SPAWN,'mkdir -p ' + tmpDir
        ENDIF
     ENDIF

  ENDIF

  nRollsString   = STRING(FORMAT='(I0)',nRolls)
  avgItvlStr = "-sRate1_25"
  ;; file =
  
  dateToCheck = '20180815'

  use_density_moment__not_fit_param = 1

  checkForFiles = 1
  dirForCheck = '/SPENCEdata/Research/Satellites/FAST/kappa_dists/saves_output_etc/' + dateToCheck + '/'
  filsPref    = 'orb4682_2DMCarlo_ests__09'
  orbDate     = '1997-10-28'
  tBoundsStr  = orbDate + '/' + ['09:06:31','09:06:32'] ;fokusert
  ;; tBoundsStr  = orbDate + '/' + ['09:06:51','09:06:52'] ;fokusert
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

           IF N_ELEMENTS(kP[*,0]) EQ 6 AND KEYWORD_SET(use_density_moment__not_fit_param) THEN BEGIN

              tmpDensK = kP[5,*]
              kP[5,*] = kP[3,*]
              kP[3,*] = TEMPORARY(tmpDensK)

              tmpDensG = gP[5,*]
              gP[5,*] = gP[3,*]
              gP[3,*] = TEMPORARY(tmpDensG)

           ENDIF

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
                 ;; BUFFER=buffer, $
                 WINTITLE=winTitle, $
                 SAVEPLOT=savePlot, $
                 OUT_ESTIMATE=gEst

              gEstListK.Add,gEst
              
           ENDFOR

           IF KEYWORD_SET(savePlot) THEN BEGIN

              timeString = T2S(fit2DKappa_info.sdt.time,/MS)
              timeString = STRMID(timeString,11,(12 < (STRLEN(timeString)-11)))
              plotSN = "Kappa-"+(timeString.Replace(":","_")).Replace(".","__")+MCPlotSuff+fExt
              
              PRINT,'Saving plot to ' + plotSN + '...'

              window.Save,tmpDir + plotSN
              window.Close

           ENDIF

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


           IF KEYWORD_SET(savePlot) THEN BEGIN

              timeString = T2S(fit2DKappa_info.sdt.time,/MS)
              timeString = STRMID(timeString,11,(12 < (STRLEN(timeString)-11)))
              plotSN = "Maxwellian-"+(timeString.Replace(":","_")).Replace(".","__")+MCPlotSuff+fExt
              
              PRINT,'Saving plot to ' + plotSN + '...'

              gWindow.Save,tmpDir + plotSN
              gWindow.Close

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

