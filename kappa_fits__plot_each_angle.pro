;2018/03/14
PRO KAPPA_FITS__PLOT_EACH_ANGLE,orbit, $
                                DIFFEFLUXFILE=diffEFluxFile, $
                                FITFILE=fitFile, $
                                TIMESTRINGARR=times, $
                                SPEC_AVG_ITVL=spec_avg_itvl, $
                                USE_2D_FIT_INFO=use_2D_fit_info, $
                                EFLUX_UNITS_INSTEAD=eFlux_units_instead, $
                                JUST_LOSSCONE_ANGLES=just_losscone_angles

  COMPILE_OPT IDL2,STRICTARRSUBS

  SET_PLOT_DIR,plotDir,/FOR_SDT,/ADD_TODAY

  dir = '/home/spencerh/software/sdt/batch_jobs/saves_output_etc/'
  SHELLCMDINIT = 'export PS1=dude; . /home/spencerh/.bashrc;'
  nye_plotSuff = '_NYE'

  ;; Check for files and return if no dice
  IF ~(FILE_TEST(dir+diffEFluxFile) AND FILE_TEST(dir+fitFile)) THEN BEGIN
     PRINT,"BOGUS!"
     RETURN
  ENDIF
  RESTORE,dir+diffEFluxFile
  RESTORE,dir+fitFile

  ;; See if we're doing eFlux. If so, kill fit (for now)
  killFit = 0
  CASE 1 OF
     KEYWORD_SET(eFlux_units_instead): BEGIN
        units1D    = 'EFLUX'
        ;; killFit    = 1
     END
     ELSE: BEGIN
        units1D    = 'FLUX'
     END
  ENDCASE
  
  xRange          = [20,3.4D4]  ;Because lame
  
  ;; Use 1D or 2D fit info?
  ;; CASE 1 OF
  fitDimStr = '1DFits'
  IF KEYWORD_SET(use_2D_fit_info) THEN BEGIN
     fitDimStr = '2DFits'

     nK     = N_ELEMENTS(fit2DKappa_inf_list)
     nG     = N_ELEMENTS(fit2DGauss_inf_list)
     kTimes2D = MAKE_ARRAY(nK,/DOUBLE)
     gTimes2D = MAKE_ARRAY(nG,/DOUBLE)
     FOR jo=0,nK-1 DO kTimes2D[jo] = fit2DKappa_inf_list[jo].sdt.time
     FOR jo=0,nG-1 DO gTimes2D[jo] = fit2DGauss_inf_list[jo].sdt.time
  ENDIF
     ;; ELSE: BEGIN
  ;; Need this in any case
  STR_ELEMENT_FROM_LIST_OF_STRUCTS,kappaFit1Ds,'time',VALUE=kTimes1D
  STR_ELEMENT_FROM_LIST_OF_STRUCTS,gaussFit1Ds,'time',VALUE=gTimes1D
  ;;    END
  ;; ENDCASE

  ;; Get oneCount
  oneCount = MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(diff_eFlux,0)
  ;; oneCount = curDatastr
  CALL_PROCEDURE,oneCount.units_procedure,oneCount,'COUNTS'
  oneCount.data[*,*] = 1
  oneCount = PREP_EFLUX_DATA( $
             oneCount, $
             UNITS=units1D, $          
             RETRACE=retrace, $
             VEL=vel, $
             ANGLE=an, $
             ARANGE=ar, $
             BINS=bins, $
             NO_SORT=no_sort)
  oneCurve = {x    : oneCount.energy[*,0], $
              y    : oneCount.data[*,0], $
              name : "One Count"}

  ;; Begin loop
  nTimes = N_ELEMENTS(times)
  FOR k=0,nTimes-1 DO BEGIN

     tmpTime = S2T(times[k])
     PRINT,times[k]
     
     customPSNPref = STRING(FORMAT='("Orb_",I0,"__")',orbit)
     orbStr        = STRING(FORMAT='(I0)',orbit)

     ;; Get closest times 
     junk1 = MIN(ABS(tmpTime-diff_eFlux.time),minInd)
     junkK1D = MIN(ABS(tmpTime-kTimes1D),kFitInd)
     junkG1D = MIN(ABS(tmpTime-gTimes1D),gFitInd)
     PRINT,T2S(diff_eFlux.time[minInd],/MS)
     PRINT,FORMAT='(A0,F0.4)',"diffEflux deltaT: ",junk1
     PRINT,FORMAT='(A0,F0.4)',"kTimes1D  deltaT: ",junkK1D
     PRINT,FORMAT='(A0,F0.4)',"gTimes1D  deltaT: ",junkG1D

     curDataStr = MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(diff_eFlux,minInd)

     IF STRUPCASE(units1D) NE 'EFLUX' THEN CONVERT_ESA_UNITS2,curDataStr,units1D

     ;; Pick up 1D fit structure
     kappaFit1D = kappaFit1Ds[kFitInd]
     gaussFit1D = gaussFit1Ds[gFitInd]

     orig          = kappaFit1D.orig

     ;; Replace info if user wants 2D fit info
     ;; CASE 1 OF
     IF KEYWORD_SET(use_2D_fit_info) THEN BEGIN
        junkK2D = MIN(ABS(tmpTime-kTimes2D),match2DKInd)
        junkG2D = MIN(ABS(tmpTime-gTimes2D),match2DGInd)
        PRINT,FORMAT='(A0,F0.4)',"kTimes2D  deltaT: ",junkK2D
        PRINT,FORMAT='(A0,F0.4)',"gTimes2D  deltaT: ",junkG2D

        kappaFit2D = fit2DKappa_inf_list[match2DKInd]
        gaussFit2D = fit2DGauss_inf_list[match2DGInd]
        k2DParms = kappaFit2D.fitparams
        g2DParms = gaussFit2D.fitparams

        kappaFit1D.chi2 = kappaFit2D.chi2/(kappaFit2D.dof-$
                                           kappaFit2D.nPegged)
        gaussFit1D.chi2 = gaussFit2D.chi2/(gaussFit2D.dof-$
                                           gaussFit2D.nPegged)
        provided_chi2RedK = kappaFit1D.chi2
        provided_chi2RedG = gaussFit1D.chi2
        
        ;; Do this for fitParams text
        kappaFit1D.A = k2DParms
        gaussFit1D.A = g2DParms

        kappaFit1D.yFull = KAPPA_FLUX__LINEAR_SHIFT_IN_ENERGY(kappaFit1D.xFull,k2DParms,UNITS=units1D)
        gaussFit1D.yFull = MAXWELL_FLUX__LINEAR_SHIFT_IN_ENERGY(gaussFit1D.xFull,g2DParms,UNITS=units1D)

     ENDIF ELSE IF STRUPCASE(units1D) NE 'FLUX' THEN BEGIN
        kappaFit1D.yFull = KAPPA_FLUX__LINEAR_SHIFT_IN_ENERGY(kappaFit1D.xFull,kappaFit1D.A,UNITS=units1D)
        gaussFit1D.yFull = MAXWELL_FLUX__LINEAR_SHIFT_IN_ENERGY(gaussFit1D.xFull,gaussFit1D.A,UNITS=units1D)
        ;; kappaFit1D.y = 1.
        ;; kappaFit1D.yFull = 1.
        ;; gaussFit1D.y = 1.
        ;; gaussFit1D.yFull = 1.
     ENDIF
     ;;    ELSE: BEGIN
     ;;    END
     ;; ENDCASE
     ;; nEnergy = diff_eFlux.nEnergy
     ;; nAngle  = diff_eFlux.Bins
     ;; energy  = REFORM(diff_eFlux.energy[*,*,minInd],nEnergy,nAngle[minInd])
     ;; theta   = REFORM(diff_eFlux.theta [*,*,minInd],nEnergy,nAngle[minInd])
     ;; data    = REFORM(diff_eFlux.data  [*,*,minInd],nEnergy,nAngle[minInd])
     ;; ddata   = REFORM(diff_eFlux.ddata [*,*,minInd],nEnergy,nAngle[minInd])
     ;; dtheta  = REFORM(diff_eFlux.dtheta[*,*,minInd],nEnergy,nAngle[minInd])
     ;; IF killFit THEN BEGIN
        
     ;; ENDIF

     tidStr = (((times[k]).Replace('/','_')).Replace('.','__')).Replace(":",'-')

     CASE 1 OF
        KEYWORD_SET(just_losscone_angles): BEGIN
           nAngles   = 2
        END
        ELSE: BEGIN
           nAngles   = curDataStr.nBins
           angleInds = LINDGEN(N_ELEMENTS(curDataStr.theta[-1,*]))
        END
     ENDCASE
     FOR j=0,nAngles-1 DO BEGIN
     ;; FOR j=0,3 DO BEGIN

        custom_plotSN = (STRING(FORMAT='(A0,I02,"__",F0.1,"__",A0)',customPSNPref,j,curDataStr.theta[-1,j],STRLOWCASE(units1D))).Replace(".","_")+nye_plotSuff
        custom_title  = STRING(FORMAT='("Orbit ",I04,", #",I0,": ",F0.1)',orbit,j,curDataStr.theta[-1,j])

        print,custom_plotSN
        print,custom_title
        
        ;; tmpOrig        = orig
        orig.Y      = curDataStr.data[1:-1,j]
        orig.Yerror = curDataStr.ddata[1:-1,j]
        PLOT_KAPPA_FITS,orig,kappaFit1D, $
                        gaussFit1D, $
                        oneCurve, $
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
                        PROVIDED_CHI2REDK=provided_chi2RedK, $
                        PROVIDED_CHI2REDG=provided_chi2RedG, $
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
                        STRING(FORMAT='("kappa_fits/Orbit_",A0,"/",A0,"/",I0,"avg/",A0,"/",A0,"/")', $
                               orbStr, $
                               fitDimStr, $
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
     SPAWN,SHELLCMDINIT + ' cd ' + tmpDir + '; ' $
           + 'pwd; convert_and_unite_eps ' $
           + STRING(FORMAT='("orb",I0,"_",A0)',orbit,tidStr)+'.pdf' $
           + " " + STRING(FORMAT='(A0,A0,A0)',"'*",nye_plotSuff,".eps'")
     ;; mv the nye_plotSuffs to regular file thing
     SPAWN,SHELLCMDINIT + ' cd ' + tmpDir + '; ' $
           + STRING(FORMAT='(A0,A0,A0,A0,A0)', $
                    'for brud in *', $
                    nye_plotSuff, $
                    '.eps; do mv ${brud} ${brud%%', $
                    nye_plotSuff, $
                   '.eps}.eps; done')

  ENDFOR



END
