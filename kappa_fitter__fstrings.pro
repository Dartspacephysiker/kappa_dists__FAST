;;2017/02/22
PRO KAPPA_FITTER__FSTRINGS, $
   T1=t1, $
   T2=t2, $
   ORBIT=orbit, $
   EEB_OR_EES=eeb_or_ees, $
   ELECTRON_ANGLERANGE=electron_angleRange ,$
   FIT_EACH_ANGLE=fit_each_angle, $
   BONUSPREF=bonusPref ,$
   ;; ADD_ONECOUNT_STATS=add_oneCount_stats, $
   SAVE_DIFF_EFLUX_TO_FILE=save_diff_eFlux_to_file ,$
   SAVE_DIFF_EFLUX_FILE=save_diff_eFlux_file ,$
   LOAD_DIFF_EFLUX_FILE=load_diff_eFlux_file ,$
   MCFADDEN_DIFF_EFLUX=McFadden_diff_eFlux, $
   OUT_DIFF_EFLUX_FILE=diff_eFlux_file, $
   FIT1D__CLAMPTEMPERATURE=fit1D__clampTemperature, $
   FIT1D__CLAMPDENSITY=fit1D__clampDensity, $
   FIT2D__CLAMPTEMPERATURE=fit2D__clampTemperature, $
   FIT2D__CLAMPDENSITY=fit2D__clampDensity, $
   FIT2D__ONLY_FIT_PEAK_ERANGE=fit2D__only_fit_peak_eRange ,$
   FIT2D__ONLY_FIT_ABOVEMIN=fit2D__only_fit_aboveMin ,$
   MIN_PEAK_ENERGY=min_peak_energy, $
   FIT2D__DISABLE_BFUNC=fit2D__disable_bFunc ,$
   ;; FIT2D__EXCLUDE_LCA_FROM_DENSCALC=fit2D__exclude_lca_from_densCalc ,$
   ENFORCE_DIFF_EFLUX_SRATE=enforce_diff_eFlux_sRate, $
   SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
   FITFILE=fitFile, $
   FIT2DPARMERRFILE=fit2DParmErrFile, $
   LOADDIR=loadDir

  COMPILE_OPT IDL2,STRICTARRSUBS

;; fit2D__only_fit_peak_eRange
;; fit2D__only_fit_aboveMin
;; fit2D__disable_bFunc
;; fit2D__exclude_lca_from_densCalc
;; save_diff_eFlux_file
;; load_diff_eFlux_file
;; bonusPref
;; fitFile
;; electron_angleRange

  dEFlux_allAngles = KEYWORD_SET(fit_each_angle) OR (N_ELEMENTS(fit_each_angle) EQ 0)

  IF ~KEYWORD_SET(orbit     ) THEN STOP
  IF ~KEYWORD_SET(eeb_or_ees) THEN STOP

  plotNamePref    = KEYWORD_SET(bonusPref) ? bonusPref : ''
  CASE 1 OF
     KEYWORD_SET(fit2D__only_fit_peak_eRange): BEGIN
        plotNamePref += '-only_fit_peak_eRange'
     END
     KEYWORD_SET(fit2D__only_fit_aboveMin): BEGIN
        plotNamePref += STRING(FORMAT='("-fit_above_",I0,"_eV")',min_peak_energy)
     END
     ELSE: BEGIN
     END
  ENDCASE
  
  ;; IF KEYWORD_SET(add_oneCount_stats) THEN BEGIN
  ;;    plotNamePref    += '-w_1Count'
  ;; ENDIF

  clampStr  = ''
  isClamped = 0B
  IF KEYWORD_SET(fit1D__clampTemperature) THEN BEGIN
     isClamped = 1B
     clampStr += '1D'
  ENDIF

  IF KEYWORD_SET(fit2D__clampTemperature) THEN BEGIN
     isClamped = 1B
     clampStr += '2D'
  ENDIF

  IF isClamped THEN BEGIN
     clampStr = '-' + clampStr + 'clampT'
  ENDIF

  clampStr  = ''
  isClamped = 0B
  IF KEYWORD_SET(fit1D__clampDensity) THEN BEGIN
     isClamped = 1B
     clampStr += '1D'
  ENDIF

  IF KEYWORD_SET(fit2D__clampDensity) THEN BEGIN
     isClamped = 1B
     clampStr += '2D'
  ENDIF

  IF isClamped THEN BEGIN
     clampStr = '-' + clampStr + 'clampN'
  ENDIF

  IF ~KEYWORD_SET(fit2D__disable_bFunc) THEN BEGIN
     plotNamePref    += '-has_bFunc'
  ENDIF

  ;; IF ~KEYWORD_SET(fit2D__exclude_lca_from_densCalc) THEN BEGIN
  ;;    plotNamePref    += '-dens_w_LCA'
  ;; ENDIF

  avgItvlStr       = ''
  CASE 1 OF
     KEYWORD_SET(enforce_diff_eFlux_sRate): BEGIN
        avgItvlStr    = '-sRate' + (STRING(FORMAT='(F0.2)',enforce_diff_eFlux_sRate)).Replace('.','_')
        plotNamePref += avgItvlStr
     END
     KEYWORD_SET(spectra_average_interval): BEGIN
        avgItvlStr    = '-avg_itvl' + STRING(FORMAT='(I0)',spectra_average_interval)
        plotNamePref += avgItvlStr
     END
  ENDCASE
  ;; IF KEYWORD_SET(spectra_average_interval) THEN BEGIN
  ;;    avgItvlStr    = '-avg_itvl' + STRING(FORMAT='(I0)',spectra_average_interval)
  ;;    plotNamePref += avgItvlStr
  ;; ENDIF

  fitAngleStr      = ''
  ;; dEFAngleStr         = '-allAngles'
  dEFAngleStr      = ''
  ;; CASE 1 OF
  ;; (N_ELEMENTS(electron_angleRange) EQ 2): BEGIN
  CASE SIZE(electron_angleRange,/TYPE) OF
     7: BEGIN
        IF STRMATCH(STRUPCASE(electron_angleRange[0]),'*LC') THEN BEGIN
           LCStr = 'LC'
           IF STRLEN(electron_angleRange[0]) GT 2 THEN BEGIN
              factor = FLOAT(STRSPLIT(STRUPCASE(electron_angleRange[0]),'LC',/EXTRACT))
              LCStr = STRING(FORMAT='(F0.1)',factor) + LCStr
           ENDIF
           fitAngleStr = "-e_angle_" + LCStr.Replace('.','_')
        ENDIF ELSE BEGIN
           STOP
        ENDELSE
     END
     ELSE: BEGIN
        IF (N_ELEMENTS(electron_angleRange) EQ 2) THEN BEGIN
           fitAngleStr         = STRING(FORMAT='("-e_angle_",F0.1,"-",F0.1)', $
                                        electron_angleRange[0], $
                                        electron_angleRange[1])
        ENDIF
     END
  ENDCASE

  IF ~dEFlux_allAngles THEN BEGIN
     dEFAngleStr = fitAngleStr
  ENDIF

  ;; tString files
  t1Str = 'unspecified'
  t2Str = ''
  
  IF N_ELEMENTS(t1) GT 0 THEN BEGIN
     CASE SIZE(t1,/TYPE) OF
        5: BEGIN
           t1Str = T2S(t1,/MS)
        END
        7: BEGIN
           t1Str = t1
        END
        ELSE: BEGIN
           STOP
        END
     ENDCASE

     CASE SIZE(t2,/TYPE) OF
        5: BEGIN
           t2Str = T2S(t2,/MS)
        END
        7: BEGIN
           t2Str = t2
        END
        ELSE: BEGIN
           STOP
        END
     ENDCASE

     t1Str = '-' + (STRSPLIT(t1Str,'/',/EXTRACT))[1]
     t1Str = STRJOIN(STRSPLIT(t1Str,':',/EXTRACT),'_')
     t1Str = STRJOIN(STRSPLIT(t1Str,'.',/EXTRACT),'__')

     t2Str = '-' + (STRSPLIT(t2Str,'/',/EXTRACT))[1]
     t2Str = STRJOIN(STRSPLIT(t2Str,':',/EXTRACT),'_')
     t2Str = STRJOIN(STRSPLIT(t2Str,'.',/EXTRACT),'__')

  ENDIF
  
  McFaddenString = KEYWORD_SET(McFadden_diff_eFlux) ? '-McFaddenStyle' : ''

  defEFluxFile = 'orb_' + STRCOMPRESS(orbit,/REMOVE_ALL) + '-diff_eflux' + McFaddenString $
                 + '-' + eeb_or_ees + avgItvlStr + dEFAngleStr + t1Str + t2Str + '.sav'

  IF KEYWORD_SET(save_diff_eFlux_file) THEN BEGIN
     CASE SIZE(save_diff_eFlux_file,/TYPE) OF
        7: BEGIN
           IF ~(FILE_TEST(save_diff_eFlux_file) OR FILE_TEST(loadDir + save_diff_eFlux_file)) THEN BEGIN
              PRINT,"Couldn't find " + save_diff_eFlux_file + '!'
              STOP
           ENDIF
        END
        ELSE: BEGIN
           save_diff_eFlux_to_file = defEFluxFile
        END
     ENDCASE
  ENDIF

  ;; IF KEYWORD_SET(load_diff_eFlux_file) THEN BEGIN
     CASE SIZE(load_diff_eFlux_file,/TYPE) OF
        7: BEGIN
           IF ~(FILE_TEST(load_diff_eFlux_file) OR FILE_TEST(loadDir+load_diff_eFlux_file)) THEN BEGIN
              PRINT,"Couldn't find file: " + load_diff_eFlux_file
              STOP
           ENDIF ELSE BEGIN
              diff_eFlux_file = load_diff_eFlux_file
           ENDELSE
        END
        ELSE: BEGIN
           diff_eFlux_file    = defEFluxFile
        END
     ENDCASE
  ;; ENDIF

  defFitFile           = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + '-' + 'orb_' + STRCOMPRESS(orbit,/REMOVE_ALL) + $
                        '-KandGfits' + McFaddenString + '-' + eeb_or_ees + plotNamePref + t1Str + t2Str + '.sav'
  ;; IF KEYWORD_SET(fitFile) THEN BEGIN
     CASE SIZE(fitFile,/TYPE) OF
        7: BEGIN
           IF ~(FILE_TEST(fitFile) OR FILE_TEST(loadDir+fitFile)) THEN BEGIN
              PRINT,"Couldn't find file: " + fitFile
              STOP
           ENDIF ELSE BEGIN
              fitFile = defFitFile
           ENDELSE
        END
        ELSE: BEGIN
           fitFile    = defFitFile
        END
     ENDCASE
  ;; ENDIF

     fit2DParmErrFile = fitFile.Replace(".sav",'-2DPARMERRORS.sav')

END
