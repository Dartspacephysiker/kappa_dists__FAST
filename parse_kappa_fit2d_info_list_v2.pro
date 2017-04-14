;2016/07/22
FUNCTION PARSE_KAPPA_FIT2D_INFO_LIST_V2,fit2D_inf_list, $
                                        ;; FITS1D_LIST=fits1D_list, $
                                        SOUTH=south, $
                                        HIGHDENSITY_THRESHOLD=highDens_thresh, $
                                        LOWDENSITY_THRESHOLD=lowDens_thresh, $
                                        KAPPA_LOWTHRESHOLD=lKappa_thresh, $
                                        KAPPA_HIGHTHRESHOLD=hKappa_thresh, $
                                        CHI2_THRESHOLD=chi2_thresh, $
                                        CHI2_OVER_DOF_THRESHOLD=chi2_over_dof_thresh, $
                                        DIFFEFLUX_THRESHOLD=diffEflux_thresh, $
                                        N_PEAKS_ABOVE_DEF_THRESHOLD=nPkAbove_dEF_thresh, $
                                        ATM_LC_ANGLERANGE=atm_lc_angleRange, $
                                        DONT_SHRINK_PARSED_STRUCT=dont_shrink, $
                                        DESTROY_INFO_LIST=destroy, $
                                        IN_GOOD_I=in_good_i, $
                                        OUT_FITPARAM_STRUCT=fitParam_struct, $
                                        OUT_GOOD_I=include_i, $
                                        OUT_GOOD_T=include_t, $
                                        OUT_BAD_I=exclude_i, $
                                        OUT_BAD_T=exclude_t, $                                        
                                        FIT_TYPE=fit_type

  COMPILE_OPT IDL2,STRICTARRSUBS

  exclString = 'Excluded '
  IF KEYWORD_SET(dont_shrink_list) THEN exclString = 'Should have excluded '

  ;; IF ~KEYWORD_SET(highDens_thresh) THEN BEGIN
  ;;    highDens_thresh = 10        ;We want nothing to do with it if it's above 5 cm^-3
  ;; ENDIF

  IF ~KEYWORD_SET(lKappa_thresh) THEN BEGIN
     lKappa_thresh = 1.5D       ;We want nothing to do with it if kappa LT 1.5
  ENDIF


  nFits               = N_ELEMENTS(fit2D_inf_list)
  IF nFits EQ 0 THEN RETURN,-1

  nFitParams          = N_ELEMENTS(fit2D_inf_list[0].fitParams)
  nPFree_index        = N_ELEMENTS(fit2D_inf_list[0].pFree_index)
  nFitPoints          = N_ELEMENTS(fit2D_inf_list[0].best_fJac[*,0])


  SDTStr              = REPLICATE(fit2D_inf_list[0].SDT,nFits)
  fitParams           = MAKE_ARRAY(nFitParams,nFits,             VALUE=0.0,/FLOAT    )
  obs_scDens          = MAKE_ARRAY(nFits,                        VALUE=0.0,/FLOAT    )
  obs_scTemp          = MAKE_ARRAY(nFits,                        VALUE=0.0,/FLOAT    )
  obs_scFAConduct     = MAKE_ARRAY(nFits,                        VALUE=0.0,/FLOAT    )
  estimated_sc        = MAKE_ARRAY(2,nFits,                      VALUE=0.0,/FLOAT    )
  fit_scDens          = MAKE_ARRAY(nFits,                        VALUE=0.0,/FLOAT    )
  fit_scTemp          = MAKE_ARRAY(nFits,                        VALUE=0.0,/FLOAT    )
  fit_scFAConduct     = MAKE_ARRAY(nFits,                        VALUE=0.0,/FLOAT    )
  errMsg              = MAKE_ARRAY(nFits,                                  /STRING   )
  chi2                = MAKE_ARRAY(nFits,                        VALUE=0.0D,/FLOAT   )
  status              = MAKE_ARRAY(nFits,                        VALUE=0.0D,/INTEGER )
  nfEv                = MAKE_ARRAY(nFits,                        VALUE=0.0D,/INTEGER )
  ;; best_resid          = MAKE_ARRAY(nFitPoints,nFits,             VALUE=0.0D,/FLOAT   )
  pFree_index         = MAKE_ARRAY(nPFree_index,nFits,           VALUE=0.0D,/INTEGER )
  ;; best_fJac           = MAKE_ARRAY(nFitPoints,nPFree_index,nFits,VALUE=0.0D,/FLOAT   )
  nPegged             = MAKE_ARRAY(nFits,                        VALUE=0.0D,/BYTE    )
  nFree               = MAKE_ARRAY(nFits,                        VALUE=0.0D,/BYTE    )
  dof                 = MAKE_ARRAY(nFits,                        VALUE=0.0D,/INTEGER )
  covar               = MAKE_ARRAY(nFitParams,nFitParams,nFits,  VALUE=0.0D,/FLOAT   )
  pError              = MAKE_ARRAY(nFitParams,nFits,             VALUE=0.0D,/FLOAT   )
  nIter               = MAKE_ARRAY(nFits,                        VALUE=0.0D,/INTEGER )

  nExcluded_highDens  = 0
  nExcluded_lowDens   = 0
  nExcluded_lKappa    = 0
  nExcluded_hKappa    = 0
  nExcluded_chi2      = 0
  nExcluded_dEf       = 0
  exclude_i           = !NULL
  FOR k=0,nFits-1 DO BEGIN
     excluded         = 0
     tmpKappa         = fit2D_inf_list[k].fitParams[2]

     IF KEYWORD_SET(highDens_thresh) AND ~excluded THEN BEGIN
        IF fit2D_inf_list[k].obs_scDens GT highDens_thresh THEN BEGIN
           nExcluded_highDens++
           exclude_i   = [exclude_i,k]
           excluded    = 1
           ;; CONTINUE
        ENDIF
     ENDIF

     IF KEYWORD_SET(lowDens_thresh) AND ~excluded THEN BEGIN
        IF fit2D_inf_list[k].obs_scDens LT lowDens_thresh THEN BEGIN
           nExcluded_lowDens++
           exclude_i   = [exclude_i,k]
           excluded    = 1
           ;; CONTINUE
        ENDIF
     ENDIF

     IF KEYWORD_SET(lKappa_thresh) AND ~excluded THEN BEGIN
        IF tmpKappa LT lKappa_thresh THEN BEGIN
           nExcluded_lKappa++
           exclude_i  = [exclude_i,k]
           excluded   = 1
           ;; CONTINUE
        ENDIF
     ENDIF

     IF KEYWORD_SET(hKappa_thresh) AND ~excluded THEN BEGIN
        IF tmpKappa GT hKappa_thresh THEN BEGIN
           nExcluded_hKappa++
           exclude_i  = [exclude_i,k]
           excluded   = 1
           ;; CONTINUE
        ENDIF
     ENDIF

     IF KEYWORD_SET(chi2_thresh) AND ~excluded THEN BEGIN
        IF fit2D_inf_list[k].chi2 GT chi2_thresh THEN BEGIN
           nExcluded_chi2++
           exclude_i  = [exclude_i,k]
           excluded   = 1
           ;; CONTINUE
        ENDIF
     ENDIF

     IF KEYWORD_SET(chi2_over_dof_thresh) AND ~excluded THEN BEGIN
        IF (fit2D_inf_list[k].chi2/fit2D_inf_list[k].dof) GT chi2_over_dof_thresh THEN BEGIN
           nExcluded_chi2++
           exclude_i  = [exclude_i,k]
           excluded   = 1
           ;; CONTINUE
        ENDIF
     ENDIF

     IF KEYWORD_SET(nPkAbove_dEF_thresh) AND ~excluded THEN BEGIN
        ;;First get all the bins nearest to bulk energy
        junk = MIN(ABS(fit2D_inf_list[k].SDT.energy[*,0]-fit2D_inf_list[k].fitParams[0]),bulk_e__ind)
        tmpDat = fit2D_inf_list[k].SDT.data[bulk_e__ind,*]
        
        IF KEYWORD_SET(atm_lc_angleRange) THEN BEGIN
           CASE 1 OF
              KEYWORD_SET(south): BEGIN
                 angle_i = WHERE((fit2D_inf_list[k].SDT.theta[bulk_e__ind,*] GE atm_lc_angleRange[0]) OR $
                                 (fit2D_inf_list[k].SDT.theta[bulk_e__ind,*] LE atm_lc_angleRange[1]),nAnKeep)
              END
              ELSE: BEGIN
                 angle_i = WHERE((fit2D_inf_list[k].SDT.theta[bulk_e__ind,*] GE atm_lc_angleRange[0]) AND $
                                 (fit2D_inf_list[k].SDT.theta[bulk_e__ind,*] LE atm_lc_angleRange[1]),nAnKeep)
              END
           ENDCASE
           IF nAnKeep LT 2 THEN STOP
           tmpDat  = tmpDat[*,angle_i]
        ENDIF
        IF N_ELEMENTS(WHERE(tmpDat GE diffEflux_thresh,/NULL)) LT nPkAbove_dEF_thresh THEN BEGIN
           nExcluded_dEf++
           exclude_i   = [exclude_i,k]
           excluded    = 1
           ;; CONTINUE
        ENDIF
     ENDIF

  SDTStr[k]         = fit2D_inf_list[k].SDT
  fitParams[*,k]    = fit2D_inf_list[k].fitParams
  obs_scDens[k]     = fit2D_inf_list[k].obs_scDens
  obs_scTemp[k]     = fit2D_inf_list[k].obs_scTemp
  obs_scFAConduct[k]= fit2D_inf_list[k].obs_scFAConduct
  fit_scDens[k]     = fit2D_inf_list[k].fit_scDens
  fit_scTemp[k]     = fit2D_inf_list[k].fit_scTemp
  fit_scFAConduct[k]= fit2D_inf_list[k].fit_scFAConduct
  estimated_sc[*,k] = fit2D_inf_list[k].estimated_sc
  errMsg[k]         = fit2D_inf_list[k].errMsg
  chi2[k]           = fit2D_inf_list[k].chi2
  status[k]         = fit2D_inf_list[k].status
  nfEv[k]           = fit2D_inf_list[k].nfEv
  ;; best_resid[*,k]   = fit2D_inf_list[k].best_resid
  pFree_index[*,k]  = fit2D_inf_list[k].pFree_index
  ;; best_fJac[*,*,k]  = fit2D_inf_list[k].best_fJac
  nPegged[k]        = fit2D_inf_list[k].nPegged
  nFree[k]          = fit2D_inf_list[k].nFree
  dof[k]            = fit2D_inf_list[k].dof
  covar[*,*,k]      = fit2D_inf_list[k].covar
  pError[*,k]       = fit2D_inf_list[k].pError
  nIter[k]          = fit2D_inf_list[k].nIter

  ENDFOR

  IF N_ELEMENTS(exclude_i) GT 0 THEN BEGIN
     include_i      = CGSETDIFFERENCE(INDGEN(nFits),exclude_i,COUNT=nKept)
     include_t      = (SDTStr.time)[include_i]

     exclude_t      = (SDTStr.time)[exclude_i]
  ENDIF ELSE BEGIN
     include_i      = INDGEN(nFits)
     include_t      = SDTStr.time
     nKept          = nFits

     exclude_t      = !NULL
  ENDELSE

  IF KEYWORD_SET(highDens_thresh) THEN BEGIN
     PRINT,exclString + STRCOMPRESS(nExcluded_highDens,/REMOVE_ALL) + $
           " fits on the basis of density threshold (dens must be LE " + $
           STRCOMPRESS(highDens_thresh,/REMOVE_ALL) + ")"
  ENDIF

  IF KEYWORD_SET(lowDens_thresh) THEN BEGIN
     PRINT,exclString + STRCOMPRESS(nExcluded_lowDens,/REMOVE_ALL) + $
           " fits on the basis of low density threshold (dens must be GE " + $
           STRCOMPRESS(lowDens_thresh,/REMOVE_ALL) + ")"
  ENDIF

  IF KEYWORD_SET(lKappa_thresh) THEN BEGIN
     PRINT,exclString + STRCOMPRESS(nExcluded_lKappa,/REMOVE_ALL) + $
           " fits on the basis of low kappa threshold (kappa GE " + $
           STRCOMPRESS(lKappa_thresh,/REMOVE_ALL) + ")"
  ENDIF

  IF KEYWORD_SET(hKappa_thresh) THEN BEGIN
     PRINT,exclString + STRCOMPRESS(nExcluded_hKappa,/REMOVE_ALL) + $
           " fits on the basis of high kappa threshold (kappa LE " + STRCOMPRESS(hKappa_thresh,/REMOVE_ALL) + ")"
  ENDIF

  IF KEYWORD_SET(chi2_thresh) THEN BEGIN
     PRINT,exclString + STRCOMPRESS(nExcluded_chi2,/REMOVE_ALL) + $
           " fits on the basis of chi^2 threshold ( chi^2 GT " + STRCOMPRESS(chi2_thresh,/REMOVE_ALL) + ")"
  ENDIF

  IF KEYWORD_SET(chi2_over_dof_thresh) THEN BEGIN
     PRINT,exclString + STRCOMPRESS(nExcluded_chi2,/REMOVE_ALL) + $
           " fits on the basis of chi^2/dof threshold ( chi^2/dof GT " + STRCOMPRESS(chi2_over_dof_thresh,/REMOVE_ALL) + ")"
  ENDIF

  IF KEYWORD_SET(nPkAbove_dEF_thresh) THEN BEGIN
     PRINT,exclString + STRCOMPRESS(nExcluded_dEf,/REMOVE_ALL) + $
           " fits on the basis of low differential energy flux threshold at peak (peak must be GE " + $
           STRCOMPRESS(diffEFlux_thresh,/REMOVE_ALL) + ' for at least ' + $
           STRCOMPRESS(nPkAbove_dEF_thresh,/REMOVE_ALL) + " angles)"
  ENDIF

  IF KEYWORD_SET(in_good_i) THEN BEGIN
     nBef       = N_ELEMENTS(include_i)
     include2_i = CGSETINTERSECTION(include_i,in_good_i,COUNT=nAft,INDICES_A=include_ii)
     exclude2_i = CGSETDIFFERENCE(include_i,in_good_i,COUNT=nExclude2,POSITIONS=exclude_ii,NORESULT=-1)

     include_t = include_t[include_ii]

     IF exclude2_i[0] NE -1 THEN BEGIN

        exclude_i = [exclude_i,exclude2_i]
        exclude_i = exclude_i[SORT(exclude_i)]
        exclude_t = (SDTStr.time)[exclude_i]

        PRINT,exclString + " an additional " + STRCOMPRESS(nBef-nAft,/REMOVE_ALL) + ' inds based on user-provided (presumably kappa-i) input'
     ENDIF

     nKept = nAft

  ENDIF

  IF KEYWORD_SET(dont_shrink) THEN BEGIN
     keep_i = INDGEN(nFits)
     nKept  = nFits
     PRINT,"N should have kept: " + STRCOMPRESS(N_ELEMENTS(include_i),/REMOVE_ALL)
  ENDIF ELSE BEGIN
     keep_i = include_i
  ENDELSE
  PRINT,"N Kept: " + STRCOMPRESS(nKept,/REMOVE_ALL)

  ;; best2DFit            = {SDT: SDTStr, $
  ;;                         params1D:fitParams}

  fit2D             = {SDT          : SDTStr     [keep_i], $
                       fitParams    : fitParams[*,keep_i], $
                       obs_scDens   : obs_scDens    [keep_i], $
                       obs_scTemp   : obs_scTemp    [keep_i], $
                       obs_scFAConduct : obs_scFAConduct[keep_i], $
                       fit_scDens   : fit_scDens    [keep_i], $
                       fit_scTemp   : fit_scTemp    [keep_i], $
                       fit_scFAConduct : fit_scFAConduct[keep_i], $
                       estimated_sc : estimated_sc[*,keep_i], $
                       chi2         : chi2       [keep_i], $
                       errMsg       : errMsg     [keep_i], $
                       status       : status     [keep_i], $
                       nfEv         : nfEv       [keep_i], $
                       ;; best_resid   : best_resid [keep_i], $
                       pFree_index  : pFree_index[*,keep_i], $
                       ;; best_fJac    : best_fJac  [keep_i], $
                       nPegged      : nPegged    [keep_i], $
                       nFree        : nFree      [keep_i], $
                       dof          : dof        [keep_i], $
                       covar        : covar  [*,*,keep_i], $
                       pError       : pError   [*,keep_i], $
                       nIter        : nIter      [keep_i]}


  IF KEYWORD_SET(fit_type) THEN BEGIN
     fit2D          = CREATE_STRUCT(fit2d,'FIT_TYPE',fit_type)
  ENDIF

  IF KEYWORD_SET(destroy) THEN BEGIN
     fit2D_inf_list       = !NULL  
  ENDIF

  IF KEYWORD_SET(fitParam_struct) THEN BEGIN
     fitParam_struct = {bulk_energy : REFORM(fit2D.fitParams[0,*]), $
                        temperature : REFORM(fit2D.fitParams[1,*]), $
                        kappa       : REFORM(fit2D.fitParams[2,*]), $
                        N           : REFORM(fit2D.fitParams[3,*])}
  ENDIF

  RETURN,fit2D

END