;2018/03/13
PRO JOURNAL__20180313__INSPECT_EACH_ANGLE_FOR_DAWNSECTOR_BUMP_TIMES

  COMPILE_OPT IDL2,STRICTARRSUBS

  SET_PLOT_DIR,plotDir,/FOR_KAPPA_DB,/ADD_TODAY

  dir = '/home/spencerh/software/sdt/batch_jobs/saves_output_etc/'
  fils = ['orb_2586-diff_eflux-ees-avg_itvl2-14_51_33__000-14_53_46__000.sav', $
          'orb_2347-diff_eflux-ees-avg_itvl2-12_27_20__000-12_29_39__000.sav', $
          'orb_2390-diff_eflux-ees-avg_itvl2-11_55_24__000-11_57_33__000.sav']
  orbs = [2586, $
          2347, $
          2390]
  units1D    = 'FLUX'

  fitSuff = '-Kappa_fits_and_Gauss_fits-ees-horseshoe2d-TESTRUN-20180302-only_fit_peak_eRange-avg_itvl2.sav'
  fitFils = ['20180312-orb_2586', $
             '20180312-orb_2347', $
             '20180312-orb_2390'] + fitSuff

  tids = ['1997-04-17/14:52:41.02', $
          '1997-03-26/12:29:02.65', $
          '1997-03-30/11:57:01.56']

  ;; k = 0
  FOR k=0,2 DO BEGIN
     RESTORE,dir+fils[k]
     RESTORE,dir+fitFils[k]

     STR_ELEMENT_FROM_LIST_OF_STRUCTS,kappaFit1Ds,'time',VALUE=kTimes
     STR_ELEMENT_FROM_LIST_OF_STRUCTS,gaussFit1Ds,'time',VALUE=gTimes

     junk = MIN(ABS(S2T(tids[k])-diff_eFlux.time),minInd)
     junk = MIN(ABS(S2T(tids[k])-kTimes),kFitInd)
     junk = MIN(ABS(S2T(tids[k])-gTimes),gFitInd)
     PRINT,T2S(diff_eFlux.time[k],/MS)
     
     ;; this = SDT
     curDataStr = MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(diff_eFlux,minInd)
     CONVERT_ESA_UNITS2,curDataStr,units1D

     kappaFit1D = kappaFit1Ds[kFitInd]
     gaussFit1D = gaussFit1Ds[gFitInd]
     ;; nEnergy = diff_eFlux.nEnergy
     ;; nAngle  = diff_eFlux.Bins
     ;; energy  = REFORM(diff_eFlux.energy[*,*,minInd],nEnergy,nAngle[minInd])
     ;; theta   = REFORM(diff_eFlux.theta [*,*,minInd],nEnergy,nAngle[minInd])
     ;; data    = REFORM(diff_eFlux.data  [*,*,minInd],nEnergy,nAngle[minInd])
     ;; ddata   = REFORM(diff_eFlux.ddata [*,*,minInd],nEnergy,nAngle[minInd])
     ;; dtheta  = REFORM(diff_eFlux.dtheta[*,*,minInd],nEnergy,nAngle[minInd])

     customPSNPref = STRING(FORMAT='("Orb_",I0,"__")',orbs[k])

     nAngles = curDataStr.nBins
     orig = kappaFit1D.orig
     FOR j=0,nAngles-1 DO BEGIN

        custom_plotSN = (STRING(FORMAT='(A0,I02,"__",F0.1,A0)',customPSNPref,j,curDataStr.theta[-1,j],"__bumpers")).Replace(".","_")+'.png'
        custom_title  = STRING(FORMAT='("Orbit ",I04,", #",I0,": ",F0.2)',orbs[k],j,curDataStr.theta[-1,j])

        print,custom_plotSN
        print,custom_title
        
        tmpOrig   = orig
        tmpOrig.Y      = curDataStr.data[1:-1,j]
        tmpOrig.Yerror = curDataStr.ddata[1:-1,j]
        PLOT_KAPPA_FITS,tmpOrig,kappaFit1D, $
                        gaussFit1D, $
                        ;; oneCurve, $
                        ;; TITLE=title, $
                        BOUNDS_I=iTime, $
                        XRANGE=xRange, $
                        YRANGE=yRange, $
                        XLOG=xLog, $
                        YLOG=yLog, $
                        STRINGS=KF2D__strings, $
                        /ADD_GAUSSIAN_ESTIMATE, $
                        /ADD_FITPARAMS_TEXT, $
                        ;; /ADD_ANGLE_LABEL, $
                        ;; ADD_ANGLE_LABEL=KEYWORD_SET(KF2D__Curvefit_opt.fit1D__sc_eSpec) ? MEAN(KF2D__SDTData_opt.electron_angleRange) : , $
                        ;; ADD_ANGLE_LABEL=MEAN(KF2D__SDTData_opt.electron_angleRange), $
                        /ADD_CHI_VALUE, $
                        ADD_WINTITLE=add_winTitle, $
                        /SAVE_FITPLOTS, $
                        /PLOT_FULL_FIT, $
                        ;; SKIP_BAD_FITS=skip_bad_fits, $
                        USING_SDT_DATA=using_SDT_data, $
                        ;; VERTICAL_LINES=vertical_lines, $
                        /VERTICAL_LINES, $
                        ;; PLOT_SAVENAME=plotSN, $
                        CUSTOM_PLOTSN=custom_plotSN, $
                        CUSTOM_TITLE=custom_title, $
                        /USE_PSYM_FOR_DATA, $
                        PLOTDIR=plotDir, $
                        ;; ADD_PLOTDIR_SUFF=STRING(FORMAT='("kappa_fits/Orbit_",A0,"/1DFits/",I0,"avg/")', $
                        ;;                         KF2D__strings.orbStr, $
                        ;;                         KF2D__SDTData_opt.spec_avg_intvl), $
                        POSTSCRIPT=~KEYWORD_SET(eps), $
                        ;; OUT_WINDOWARR=windowArr, $
                        /BUFFER, $
                        UNITS=units1D, $
                        EPS=eps

     ENDFOR

  ENDFOR

     STOP

END
