;;2017/02/22
PRO KAPPA_FITTER__FSTRINGS, $
   ORBIT=orbit, $
   EEB_OR_EES=eeb_or_ees, $
   ELECTRON_ANGLERANGE=electron_angleRange ,$
   FIT_EACH_ANGLE=fit_each_angle, $
   BONUSPREF=bonusPref ,$
   ;; ADD_ONECOUNT_STATS=add_oneCount_stats, $
   SAVE_DIFF_EFLUX_TO_FILE=save_diff_eFlux_to_file ,$
   SAVE_DIFF_EFLUX_FILE=save_diff_eFlux_file ,$
   LOAD_DIFF_EFLUX_FILE=load_diff_eFlux_file ,$
   OUT_DIFF_EFLUX_FILE=diff_eFlux_file, $
   FIT2D__ONLY_FIT_PEAK_ERANGE=fit2D__only_fit_peak_eRange ,$
   FIT2D__ONLY_FIT_ABOVEMIN=fit2D__only_fit_aboveMin ,$
   MIN_PEAK_ENERGY=min_peak_energy, $
   FIT2D__DISABLE_BFUNC=fit2D__disable_bFunc ,$
   FIT2D__EXCLUDE_LCA_FROM_DENSCALC=fit2D__exclude_lca_from_densCalc ,$
   FITFILE=fitFile, $
   LOADDIR=loadDir

  COMPILE_OPT IDL2

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

  IF KEYWORD_SET(fit2D__disable_bFunc) THEN BEGIN
     plotNamePref    += '-No_bFunc'
  ENDIF

  IF KEYWORD_SET(fit2D__exclude_lca_from_densCalc) THEN BEGIN
     plotNamePref    += '-exc_LCA'
  ENDIF

  fitAngleStr         = ''
  dEFAngleStr         = '-allAngles'
  ;; CASE 1 OF
  ;; (N_ELEMENTS(electron_angleRange) EQ 2): BEGIN
  IF (N_ELEMENTS(electron_angleRange) EQ 2) THEN BEGIN
     fitAngleStr         = STRING(FORMAT='("-e_angle_",F0.1,"-",F0.1)', $
                               electron_angleRange[0], $
                               electron_angleRange[1])
     IF ~dEFlux_allAngles THEN BEGIN
        dEFAngleStr = fitAngleStr
     ENDIF
  ENDIF
     ;; END
     ;; dEFlux_allAngles: BEGIN
     ;;    angleStr      = '-allAngles'
     ;; END
  ;; ENDCASE


  defEFluxFile = 'orb_' + STRCOMPRESS(orbit,/REMOVE_ALL) + '-diff_eflux-' + $
                 eeb_or_ees + dEFAngleStr + $
                 plotNamePref + '.sav'

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
                        '-Kappa_fits_and_Gauss_fits-' + eeb_or_ees + '-horseshoe2d' + plotNamePref + '.sav'
  IF KEYWORD_SET(fitFile) THEN BEGIN
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
           fitFile    = defEFluxFile
        END
     ENDCASE
  ENDIF

END
