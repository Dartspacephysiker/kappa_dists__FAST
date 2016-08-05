PRO KAPPA_FITFILE_STRING,outSuff, $
                         R_B=R_B, $
                         USE_DATA_DENS=use_data_dens, $
                         SDT_CALC__NO_MODEL=SDT_calc__no_model, $
                         LKAPPA_THRESH=lKappa_thresh, $
                         HKAPPA_THRESH=hKappa_thresh, $
                         HIGHDENS_THRESH=highDens_thresh, $
                         USE_MPFIT1D=use_mpFit1D

  COMPILE_OPT idl2

  IF N_ELEMENTS(R_B) GT 0 THEN magRatio = R_B ELSE magRatio = 3.0

  outSuff            = STRING(FORMAT='("--RB_",G0.0)',magRatio) + '--150_to_150' + $
                       (KEYWORD_SET(use_data_dens) ? "--data_dens" : "") + $
                       (KEYWORD_SET(SDT_calc__no_model) ? "--SDT_calcked_current" : "")

  IF N_ELEMENTS(lKappa_thresh) GT 0 THEN BEGIN
     outSuff       += STRING(FORMAT='("--minKappa_",F0.1)',lKappa_thresh)
  ENDIF

  IF N_ELEMENTS(hKappa_thresh) GT 0 THEN BEGIN
     outSuff       += STRING(FORMAT='("--maxKappa_",F0.1)',hKappa_thresh)
  ENDIF

  IF N_ELEMENTS(highDens_thresh) GT 0 THEN BEGIN
     outSuff       += STRING(FORMAT='("--maxDens_",F0.1)',highDens_thresh)
  ENDIF

  IF KEYWORD_SET(use_mpFit1D) THEN BEGIN
     outSuff       += "--mpFit1D"
  ENDIF

END