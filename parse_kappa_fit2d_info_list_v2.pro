;2016/07/22
FUNCTION PARSE_KAPPA_FIT2D_INFO_LIST_V2,fit2D_inf_list, $
                                     ;; FITS1D_LIST=fits1D_list, $
                                     HIGHDENSITY_THRESHOLD=highDens_thresh, $
                                     KAPPA_LOWTHRESHOLD=lKappa_thresh, $
                                     KAPPA_HIGHTHRESHOLD=hKappa_thresh, $
                                     DESTROY_INFO_LIST=destroy, $
                                     OUT_GOOD_I=include_i, $
                                     OUT_GOOD_T=include_t, $
                                     OUT_BAD_I=exclude_i, $
                                     OUT_BAD_T=exclude_t
  
                                ;; BESTFIT_SDTSTRUCTS=fitStrs, $
                                ;; BESTFIT_1DPARAMS=bestFit_1DParams, $
                                ;; BESTFIT_DENS=bestFit_dens, $
                                ;; BESTFIT_ANGLE=bestFit_angle, $
                                ;; BESTFIT_CHI=bestFit_chi, $
                                ;; BESTFIT_2DCHI=bestFit_2DChi

  COMPILE_OPT idl2

  ;; IF ~KEYWORD_SET(highDens_thresh) THEN BEGIN
  ;;    highDens_thresh = 10        ;We want nothing to do with it if it's above 5 cm^-3
  ;; ENDIF

  IF ~KEYWORD_SET(lKappa_thresh) THEN BEGIN
     lKappa_thresh = 1.5D       ;We want nothing to do with it if kappa GT 1.5
  ENDIF


  nBestFits            = N_ELEMENTS(fit2D_inf_list)

  IF nBestFits EQ 0 THEN RETURN,-1

  fitStrs          = !NULL
  fitparams        = LIST()

  nExcluded_highDens   = 0
  nExcluded_lKappa     = 0
  nExcluded_hKappa     = 0
  exclude_i            = !NULL
  FOR k=0,nBestFits-1 DO BEGIN
     excluded          = 0
     ;; tmpDens           = fit2D_inf_list[k].bestDens
     tmpKappa          = fit2D_inf_list[k].fitParams[2]

     IF KEYWORD_SET(lKappa_thresh) THEN BEGIN
        IF tmpKappa LT lKappa_thresh THEN BEGIN
           nExcluded_lKappa++
           exclude_i   = [exclude_i,k]
           excluded    = 1
           ;; CONTINUE
        ENDIF
     ENDIF

     IF KEYWORD_SET(hKappa_thresh) AND ~excluded THEN BEGIN
        IF tmpKappa GT hKappa_thresh THEN BEGIN
           nExcluded_hKappa++
           exclude_i   = [exclude_i,k]
           excluded    = 1
           ;; CONTINUE
        ENDIF
     ENDIF

     ;; IF KEYWORD_SET(highDens_thresh) AND ~excluded THEN BEGIN
     ;;    IF tmpDens GT highDens_thresh THEN BEGIN
     ;;       nExcluded_highDens++
     ;;       exclude_i   = [exclude_i,k]
     ;;       excluded    = 1
     ;;       ;; CONTINUE
     ;;    ENDIF
     ;; ENDIF

     fitStrs       = [fitStrs     ,fit2D_inf_list[k].fitStr                           ]

     fitParams.Add,fit2D_inf_list[k].fitParams

  ENDFOR

  IF N_ELEMENTS(exclude_i) GT 0 THEN BEGIN
     include_i         = CGSETDIFFERENCE(INDGEN(nBestFits),exclude_i,COUNT=nKept)
     include_t         = (fitStrs.time)[include_i]

     exclude_t         = (fitStrs.time)[exclude_i]
  ENDIF ELSE BEGIN
     include_i         = INDGEN(nBestFits)
     include_t         = fitStrs.time
     nKept             = nBestFits

     exclude_t         = !NULL
  ENDELSE

  IF KEYWORD_SET(highDens_thresh) THEN BEGIN
     PRINT,'Excluded ' + STRCOMPRESS(nExcluded_highDens,/REMOVE_ALL) + $
           " fits on the basis of density threshold (dens LE " + STRCOMPRESS(highDens_thresh,/REMOVE_ALL) + ")"
  ENDIF

  IF KEYWORD_SET(lKappa_thresh) THEN BEGIN
     PRINT,'Excluded ' + STRCOMPRESS(nExcluded_lKappa,/REMOVE_ALL) + $
           " fits on the basis of low kappa threshold (kappa GE " + STRCOMPRESS(lKappa_thresh,/REMOVE_ALL) + ")"
  ENDIF

  IF KEYWORD_SET(hKappa_thresh) THEN BEGIN
     PRINT,'Excluded ' + STRCOMPRESS(nExcluded_hKappa,/REMOVE_ALL) + $
           " fits on the basis of high kappa threshold (kappa LE " + STRCOMPRESS(hKappa_thresh,/REMOVE_ALL) + ")"
  ENDIF

  PRINT,"N Kept: " + STRCOMPRESS(nKept,/REMOVE_ALL)

  best2DFit            = {SDT: fitStrs, $
                          params1D:fitParams}

  IF KEYWORD_SET(destroy) THEN BEGIN
     fit2D_inf_list       = !NULL  
  ENDIF

  RETURN,best2DFit

END