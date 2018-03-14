;2018/03/14
PRO KAPPA_FITS__PLOT_EACH_ANGLE,orbit, $
                                DIFFEFLUXFILE=diffEFluxFile, $
                                FITFILE=fitFile, $
                                TIMESTRINGARR=times, $
                                SPEC_AVG_ITVL=spec_avg_itvl, $
                                EFLUX_UNITS_INSTEAD=eFlux_units_instead

  COMPILE_OPT IDL2,STRICTARRSUBS

  SET_PLOT_DIR,plotDir,/FOR_SDT,/ADD_TODAY

  dir = '/home/spencerh/software/sdt/batch_jobs/saves_output_etc/'
  killFit = 0
  CASE 1 OF
     KEYWORD_SET(eFlux_units_instead): BEGIN
        units1D    = 'EFLUX'
        killFit    = 1
     END
     ELSE: BEGIN
        units1D    = 'FLUX'
     END
  ENDCASE
  
  xRange          = [20,3.4D4]  ;Because lame
  
  nTimes = N_ELEMENTS(times)
  FOR k=0,nTimes-1 DO BEGIN

     PRINT,times[k]
     
     RESTORE,dir+diffEFluxFile
     RESTORE,dir+fitFile

     STR_ELEMENT_FROM_LIST_OF_STRUCTS,kappaFit1Ds,'time',VALUE=kTimes
     STR_ELEMENT_FROM_LIST_OF_STRUCTS,gaussFit1Ds,'time',VALUE=gTimes

     junk1 = MIN(ABS(S2T(times[k])-diff_eFlux.time),minInd)
     junk2 = MIN(ABS(S2T(times[k])-kTimes),kFitInd)
     junk3 = MIN(ABS(S2T(times[k])-gTimes),gFitInd)
     PRINT,T2S(diff_eFlux.time[k],/MS)
     PRINT,FORMAT='(A0,F0.2)',"diffEflux deltaT: ",junk1
     PRINT,FORMAT='(A0,F0.2)',"kTimes    deltaT: ",junk2
     PRINT,FORMAT='(A0,F0.2)',"gTimes    deltaT: ",junk3

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
     IF killFit THEN BEGIN
        kappaFit1D.y = 1.
        kappaFit1D.yFull = 1.
        gaussFit1D.y = 1.
        gaussFit1D.yFull = 1.
     ENDIF

     customPSNPref = STRING(FORMAT='("Orb_",I0,"__")',orbit)
     orbStr        = STRING(FORMAT='(I0)',orbit)

     nAngles = curDataStr.nBins
     orig = kappaFit1D.orig

     tidStr = (((times[k]).Replace('/','_')).Replace('.','__')).Replace(":",'-')
     FOR j=0,nAngles-1 DO BEGIN

        custom_plotSN = (STRING(FORMAT='(A0,I02,"__",F0.1,"__",A0)',customPSNPref,j,curDataStr.theta[-1,j],STRLOWCASE(units1D))).Replace(".","_")
        custom_title  = STRING(FORMAT='("Orbit ",I04,", #",I0,": ",F0.2)',orbit,j,curDataStr.theta[-1,j])

        print,custom_plotSN
        print,custom_title
        
        ;; tmpOrig        = orig
        orig.Y      = curDataStr.data[1:-1,j]
        orig.Yerror = curDataStr.ddata[1:-1,j]
        PLOT_KAPPA_FITS,orig,kappaFit1D, $
                        gaussFit1D, $
                        ;; oneCurve, $
                        ;; TITLE=title, $
                        BOUNDS_I=iTime, $
                        XRANGE=xRange, $
                        YRANGE=yRange, $
                        XLOG=xLog, $
                        YLOG=yLog, $
                        STRINGS=KF2D__strings, $
                        ADD_GAUSSIAN_ESTIMATE=~killFit, $
                        ADD_FITPARAMS_TEXT=~killFit, $
                        ;; /ADD_ANGLE_LABEL, $
                        ;; ADD_ANGLE_LABEL=KEYWORD_SET(KF2D__Curvefit_opt.fit1D__sc_eSpec) ? MEAN(KF2D__SDTData_opt.electron_angleRange) : , $
                        ;; ADD_ANGLE_LABEL=MEAN(KF2D__SDTData_opt.electron_angleRange), $
                        ADD_CHI_VALUE=~killFit, $
                        ADD_WINTITLE=add_winTitle, $
                        /SAVE_FITPLOTS, $
                        PLOT_FULL_FIT=~killFit, $
                        ;; SKIP_BAD_FITS=skip_bad_fits, $
                        USING_SDT_DATA=using_SDT_data, $
                        ;; VERTICAL_LINES=vertical_lines, $
                        /VERTICAL_LINES, $
                        ;; PLOT_SAVENAME=plotSN, $
                        CUSTOM_PLOTSN=custom_plotSN, $
                        CUSTOM_TITLE=custom_title, $
                        /USE_PSYM_FOR_DATA, $
                        PLOTDIR=plotDir, $
                        ADD_PLOTDIR_SUFF= $
                        STRING(FORMAT='("kappa_fits/Orbit_",A0,"/1DFits/",I0,"avg/",A0,"/",A0,"/")', $
                               orbStr, $
                               spec_avg_itvl, $
                               tidStr, $
                               STRLOWCASE(units1D)), $
                        POSTSCRIPT=~KEYWORD_SET(eps), $
                        ;; OUT_WINDOWARR=windowArr, $
                        /BUFFER, $
                        UNITS=units1D, $
                        OUT_TMPPLOTDIR=tmpDir, $
                        /EPS

     ENDFOR

     PRINT,"Converting all plots to single pdf ..."
     SPAWN,'export PS1=dude; . /home/spencerh/.bashrc; cd ' + tmpDir+ $
           '; pwd; convert_and_unite_eps ' + $
           STRING(FORMAT='("orb",I0,"_",A0)',orbit,tidStr)+'.pdf'

  ENDFOR



END
