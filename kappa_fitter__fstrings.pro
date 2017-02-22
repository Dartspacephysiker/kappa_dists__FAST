;;2017/02/22
PRO KAPPA_FITTER__FSTRINGS, $
   ORBIT=orbit, $
   EEB_OR_EES=eeb_or_ees, $
   ELECTRON_ANGLERANGE=electron_angleRange ,$
   BONUSPREF=bonusPref ,$
   SAVE_DIFF_EFLUX_FILE=save_diff_eFlux_file ,$
   LOAD_DIFF_EFLUX_FILE=load_diff_eFlux_file ,$
   FIT2D__ONLY_FIT_PEAK_ERANGE=fit2D__only_fit_peak_eRange ,$
   FIT2D__ONLY_FIT_ABOVEMIN=fit2D__only_fit_aboveMin ,$
   MIN_PEAK_ENERGY=min_peak_energy, $
   FIT2D__DISABLE_BFUNC=fit2D__disable_bFunc ,$
   FIT2D__EXCLUDE_LCA_FROM_DENSCALC=fit2D__exclude_lca_from_densCalc ,$
   FITFILE=fitFile

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
  
  IF KEYWORD_SET(fit2D__disable_bFunc) THEN BEGIN
     plotNamePref    += '-No_bFunc'
  ENDIF

  IF KEYWORD_SET(fit2D__exclude_lca_from_densCalc) THEN BEGIN
     plotNamePref    += '-exc_LCA'
  ENDIF

  fitFile             = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + '-' + 'orb_' + STRCOMPRESS(orbit,/REMOVE_ALL) + $
                         '-Kappa_fits_and_Gauss_fits-' + eeb_or_ees + '-horseshoe2d'

  angleStr            = ''
  IF N_ELEMENTS(electron_angleRange) EQ 2 THEN BEGIN
     angleStr         = STRING(FORMAT='("-e_angle_",F0.1,"-",F0.1)', $
                                electron_angleRange[0], $
                                electron_angleRange[1])
  ENDIF  

  IF KEYWORD_SET(save_diff_eFlux_file) THEN BEGIN
     save_diff_eFlux_to_file = 'orb_' + STRCOMPRESS(orbit,/REMOVE_ALL) + '-diff_eflux-' + eeb_or_ees + angleStr + $
                               plotNamePref + '.sav'
  ENDIF
  IF KEYWORD_SET(load_diff_eFlux_file) THEN BEGIN
     CASE SIZE(load_diff_eFlux_file,/TYPE) OF
        7: BEGIN
           IF FILE_TEST(load_diff_eFlux_file) THEN BEGIN
              diff_eFlux_file = load_diff_eFlux_file
           ENDIF ELSE BEGIN
              PRINT,"Couldn't find file: " + load_diff_eFlux_file
              STOP
           ENDELSE
        END
        ELSE: BEGIN
           diff_eFlux_file   = 'orb_' + STRCOMPRESS(orbit,/REMOVE_ALL) + '-diff_eflux-' + eeb_or_ees + angleStr + $
                               plotNamePref + '.sav'
        END
     ENDCASE
  ENDIF

  fitFile                       = fitFile + plotNamePref + '.sav'


END
