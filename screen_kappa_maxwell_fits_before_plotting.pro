;2016/07/13
PRO SCREEN_KAPPA_MAXWELL_FITS_BEFORE_PLOTTING,kappa_current,gauss_current,obs_current,potential,potential2, $
   AStruct,AGaussStruct, $
   fitStatus,gaussFitStatus, $
   MAX_KAPPA=max_kappa, $
   EXCLUDE_BAD_FITS=exclude_bad_fits

  COMPILE_OPT IDL2,STRICTARRSUBS

  IF KEYWORD_SET(exclude_bad_fits) THEN BEGIN
     badFits_i = WHERE(fitStatus GE 1,nBadFits,COMPLEMENT=goodFits_i,NCOMPLEMENT=nGoodFits)
     IF nBadFits GT 0 THEN BEGIN
        PRINT,'Excluding ' + STRCOMPRESS(nBadFits,/REMOVE_ALL) + " current vals because they're poor fits ..."
        haveBadFits = 1
     ENDIF ELSE BEGIN
        haveBadFits = 0
     ENDELSE
  ENDIF

  IF KEYWORD_SET(kappa_max) THEN BEGIN
     testKMax_i = WHERE(AStruct.kappa GT kappa_max,nBadKMax,COMPLEMENT=goodKMax_i,NCOMPLEMENT=nGoodKMax)

     IF nBadKMax GT 0 THEN BEGIN
        PRINT,'Excluding ' + STRCOMPRESS(nBad,/REMOVE_ALL) + ' current vals due to kappa max ...'
        haveBadKMax = 1
     ENDIF ELSE BEGIN
        haveBadKMax = 0
     ENDELSE

  ENDIF ELSE BEGIN
     haveBadKMax = 0
  ENDELSE

  IF haveBadFits OR haveBadKMax THEN BEGIN
     IF haveBadFits THEN BEGIN
        good_i = goodFits_i
        CASE haveBadKMax OF
           1: BEGIN
              good_i = CGSETINTERSECTION(good_i,goodKMax_i)
           END
           ELSE:
        ENDCASE

     ENDIF ELSE BEGIN
        good_i = goodKMax_i
     ENDELSE
  ENDIF

  IF N_ELEMENTS(good_i) NE 0 THEN BEGIN
     kappa_current = kappa_current[good_i]
     gauss_current = gauss_current[good_i]
     obs_current   = obs_current[good_i]
     potential      = potential[good_i]
     IF N_ELEMENTS(potential2) GT 0 THEN BEGIN
        potential2  = potential2[good_i]
     ENDIF
  ENDIF

END