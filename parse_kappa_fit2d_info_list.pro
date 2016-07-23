;2016/07/22
FUNCTION PARSE_KAPPA_FIT2D_INFO_LIST,fit2D_inf_list, $
                                     ;; FITS1D_LIST=fits1D_list, $
                                     HIGHDENSITY_THRESHOLD=highDens_thresh, $
                                     KAPPA_LOWTHRESHOLD=lKappa_thresh, $
                                     KAPPA_HIGHTHRESHOLD=hKappa_thresh, $
                                     DESTROY_INFO_LIST=destroy
  
                                ;; BESTFIT_SDTSTRUCTS=bestFitStrs, $
                                ;; BESTFIT_1DPARAMS=bestFit_1DParams, $
                                ;; BESTFIT_DENS=bestFit_dens, $
                                ;; BESTFIT_ANGLE=bestFit_angle, $
                                ;; BESTFIT_CHI=bestFit_chi, $
                                ;; BESTFIT_2DCHI=bestFit_2DChi

  COMPILE_OPT idl2

  IF ~KEYWORD_SET(highDens_thresh) THEN BEGIN
     highDens_thresh = 10        ;We want nothing to do with it if it's above 5 cm^-3
  ENDIF

  IF ~KEYWORD_SET(lKappa_thresh) THEN BEGIN
     lKappa_thresh = 1.5D       ;We want nothing to do with it if kappa GT 1.5
  ENDIF


  nBestFits            = N_ELEMENTS(fit2D_inf_list)

  IF nBestFits EQ 0 THEN RETURN,-1

  bestFitStrs          = !NULL
  bestFit_1DParams     = LIST()
  bestFit_dens         = !NULL
  bestFit_angle        = !NULL
  bestFit_chi          = !NULL
  bestFit_2Dchi        = !NULL

  nExcluded_highDens   = 0
  nExcluded_lKappa     = 0
  nExcluded_hKappa     = 0
  FOR k=0,nBestFits-1 DO BEGIN
     tmpDens           = fit2D_inf_list[k].bestDens
     tmpKappa          = fit2D_inf_list[k].bestFit1DParams.A[2]

     IF KEYWORD_SET(lKappa_thresh) THEN BEGIN
        IF tmpKappa LT lKappa_thresh THEN BEGIN
           nExcluded_lKappa++
           CONTINUE
        ENDIF
     ENDIF

     IF KEYWORD_SET(hKappa_thresh) THEN BEGIN
        IF tmpKappa GT hKappa_thresh THEN BEGIN
           nExcluded_hKappa++
           CONTINUE
        ENDIF
     ENDIF

     IF KEYWORD_SET(highDens_thresh) THEN BEGIN
        IF tmpDens GT highDens_thresh THEN BEGIN
           nExcluded_highDens++
           CONTINUE
        ENDIF
     ENDIF

     bestFitStrs       = [bestFitStrs     ,fit2D_inf_list[k].bestFitStr                           ]

     bestFit_1DParams.Add,fit2D_inf_list[k].bestFit1DParams

     bestFit_dens      = [bestFit_dens    ,tmpDens                                                ]
     bestFit_angle     = [bestFit_angle   ,fit2D_inf_list[k].bestfit1dparams.bulkangleinf.sdtangle]
     bestFit_chi       = [bestFit_chi     ,fit2D_inf_list[k].bestfit1dparams.chi2                 ]
     bestFit_2Dchi     = [bestFit_2Dchi   ,MIN(fit2D_inf_list[k].chiArray)                        ]
  ENDFOR

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

  best2DFit            = {SDT: bestFitStrs, $
                          params1D:bestFit_1DParams, $
                          dens:bestFit_dens, $
                          angle:bestFit_angle, $
                          chi:bestFit_chi, $
                          chi2D:bestFit_2DChi}

  IF KEYWORD_SET(destroy) THEN BEGIN
     fit2D_inf_list       = !NULL  
  ENDIF

  RETURN,best2DFit

END