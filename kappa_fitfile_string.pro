PRO KAPPA_FITFILE_STRING,outSuff, $
                         R_B=R_B, $
                         USE_DATA_DENS=use_data_dens, $
                         BOTH_USE_KAPPA_BULKENERGY=both_use_kappa_bulkEnergy, $
                         BOTH_USE_MAXWELL_BULKENERGY=both_use_maxwell_bulkEnergy, $
                         CALC_FITDENS_OVER_ARANGE=calc_fitDens__aRange, $
                         NO_CHARI_FOR_POT=no_charI_for_pot, $
                         SDT_CALC__NO_MODEL=SDT_calc__no_model, $
                         LKAPPA_THRESH=lKappa_thresh, $
                         HKAPPA_THRESH=hKappa_thresh, $
                         HIGHDENS_THRESH=highDens_thresh, $
                         USE_MPFIT1D=use_mpFit1D, $
                         OUT_PARED_SUFF=paredSuff
                         

  COMPILE_OPT idl2

  IF N_ELEMENTS(R_B) GT 0 THEN magRatio = R_B ELSE magRatio = 3.0

  IF N_ELEMENTS(calc_fitDens__aRange) EQ 2 THEN BEGIN
     aString        = STRING(FORMAT='("--",I0,"_to_",I0)',calc_fitDens__aRange)
  ENDIF ELSE BEGIN
     aString        = '--150_to_150'
  ENDELSE

  outSuff           = STRING(FORMAT='("--RB_",G0.0)',magRatio) + aString + $
                      (KEYWORD_SET(use_data_dens) ? "--data_dens" : "") + $
                      (KEYWORD_SET(SDT_calc__no_model) ? "--SDT_calcked_current" : "")
  
  paredSuff         =  (KEYWORD_SET(use_data_dens) ? "--data_dens" : "") + $
                       (KEYWORD_SET(SDT_calc__no_model) ? "--SDT_calcked_current" : "")
  

  IF N_ELEMENTS(lKappa_thresh) GT 0 THEN BEGIN
     outSuff       += STRING(FORMAT='("--minKappa_",F0.1)',lKappa_thresh)
     paredSuff     += STRING(FORMAT='("--minKappa_",F0.1)',lKappa_thresh)
  ENDIF

  IF N_ELEMENTS(hKappa_thresh) GT 0 THEN BEGIN
     outSuff       += STRING(FORMAT='("--maxKappa_",F0.1)',hKappa_thresh)
     paredSuff     += STRING(FORMAT='("--maxKappa_",F0.1)',hKappa_thresh)
  ENDIF

  IF N_ELEMENTS(highDens_thresh) GT 0 THEN BEGIN
     outSuff       += STRING(FORMAT='("--maxDens_",F0.1)',highDens_thresh)
     paredSuff     += STRING(FORMAT='("--maxDens_",F0.1)',highDens_thresh)
  ENDIF

  IF KEYWORD_SET(use_mpFit1D) THEN BEGIN
     outSuff       += "--mpFit1D"
     paredSuff     += "--mpFit1D"
  ENDIF

  ;; firstSuff         = '--'
  ;; latterSuff        = '_'
  IF KEYWORD_SET(both_use_kappa_bulkEnergy) THEN BEGIN
     outSuff       += "--kappa_bulkE"
  ENDIF

  IF KEYWORD_SET(both_use_maxwell_bulkEnergy) THEN BEGIN
     outSuff       += "--Gauss_bulkE"
  ENDIF

  IF KEYWORD_SET(no_charI_for_pot) THEN BEGIN
     outSuff       += '--noCharI_in_pot'
  ENDIF

END