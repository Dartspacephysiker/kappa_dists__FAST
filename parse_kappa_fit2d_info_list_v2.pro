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
                                        OUT_BAD_T=exclude_t, $
                                        FIT_TYPE=fit_type

  COMPILE_OPT idl2

  ;; IF ~KEYWORD_SET(highDens_thresh) THEN BEGIN
  ;;    highDens_thresh = 10        ;We want nothing to do with it if it's above 5 cm^-3
  ;; ENDIF

  IF ~KEYWORD_SET(lKappa_thresh) THEN BEGIN
     lKappa_thresh = 1.5D       ;We want nothing to do with it if kappa GT 1.5
  ENDIF


  nFits               = N_ELEMENTS(fit2D_inf_list)
  IF nFits EQ 0 THEN RETURN,-1

  nFitParams          = N_ELEMENTS(fit2D_inf_list[0].fitParams)
  nPFree_index        = N_ELEMENTS(fit2D_inf_list[0].pFree_index)
  nFitPoints          = N_ELEMENTS(fit2D_inf_list[0].best_fJac[*,0])


  SDTStr              = REPLICATE(fit2D_inf_list[0].SDT,nFits)
  fitParams           = MAKE_ARRAY(nFitParams,nFits,             VALUE=0.0,/FLOAT    )
  fitDens             = MAKE_ARRAY(nFits,                        VALUE=0.0,/FLOAT    )
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
  nExcluded_lKappa    = 0
  nExcluded_hKappa    = 0
  exclude_i           = !NULL
  FOR k=0,nFits-1 DO BEGIN
     excluded         = 0
     tmpKappa         = fit2D_inf_list[k].fitParams[2]

     IF KEYWORD_SET(lKappa_thresh) THEN BEGIN
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

     IF KEYWORD_SET(highDens_thresh) AND ~excluded THEN BEGIN
        IF fit2D_inf_list[k].fitDens GT highDens_thresh THEN BEGIN
           nExcluded_highDens++
           exclude_i   = [exclude_i,k]
           excluded    = 1
           ;; CONTINUE
        ENDIF
     ENDIF

  SDTStr[k]         = fit2D_inf_list[k].SDT
  fitParams[*,k]    = fit2D_inf_list[k].fitParams
  fitDens[k]        = fit2D_inf_list[k].fitDens
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
     PRINT,'Excluded ' + STRCOMPRESS(nExcluded_highDens,/REMOVE_ALL) + $
           " fits on the basis of density threshold (dens LE " + $
           STRCOMPRESS(highDens_thresh,/REMOVE_ALL) + ")"
  ENDIF

  IF KEYWORD_SET(lKappa_thresh) THEN BEGIN
     PRINT,'Excluded ' + STRCOMPRESS(nExcluded_lKappa,/REMOVE_ALL) + $
           " fits on the basis of low kappa threshold (kappa GE " + $
           STRCOMPRESS(lKappa_thresh,/REMOVE_ALL) + ")"
  ENDIF

  IF KEYWORD_SET(hKappa_thresh) THEN BEGIN
     PRINT,'Excluded ' + STRCOMPRESS(nExcluded_hKappa,/REMOVE_ALL) + $
           " fits on the basis of high kappa threshold (kappa LE " + STRCOMPRESS(hKappa_thresh,/REMOVE_ALL) + ")"
  ENDIF

  PRINT,"N Kept: " + STRCOMPRESS(nKept,/REMOVE_ALL)

  ;; best2DFit            = {SDT: SDTStr, $
  ;;                         params1D:fitParams}

  fit2D             = {SDT          : SDTStr     , $
                       fitParams    : fitParams  , $
                       fitDens      : fitDens    , $
                       chi2         : chi2       , $
                       errMsg       : errMsg     , $
                       status       : status     , $
                       nfEv         : nfEv       , $
                       ;; best_resid   : best_resid , $
                       pFree_index  : pFree_index, $
                       ;; best_fJac    : best_fJac  , $
                       nPegged      : nPegged    , $
                       nFree        : nFree      , $
                       dof          : dof        , $
                       covar        : covar      , $
                       pError       : pError     , $
                       nIter        : nIter      }


  IF KEYWORD_SET(fit_type) THEN BEGIN
     fit2D          = CREATE_STRUCT(fit2d,'FIT_TYPE',fit_type)
  ENDIF

  IF KEYWORD_SET(destroy) THEN BEGIN
     fit2D_inf_list       = !NULL  
  ENDIF

  RETURN,fit2D

END