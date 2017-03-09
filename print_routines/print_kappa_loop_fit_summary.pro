PRO PRINT_KAPPA_LOOP_FIT_SUMMARY,fitStatus,gaussfitStatus


  COMPILE_OPT IDL2,STRICTARRSUBS

  badFits_i                    = WHERE(fitStatus GT 0,nBadFits)  
  
  IF N_ELEMENTS(gaussFitStatus) GT 0 THEN BEGIN
     badGaussFits_i            = WHERE(gaussFitStatus GT 0,nBadGaussFits)  
  ENDIF

  PRINT,""
  PRINT,"****************************************"
  PRINT,'NTotalFits    : ',N_ELEMENTS(fitStatus)
  PRINT,''
  PRINT,"NbadFits      : ",nBadFits

  IF N_ELEMENTS(gaussFitStatus) GT 0 THEN BEGIN
     PRINT,"NbadGaussFits : ",nBadGaussFits
     PRINT,"NBothBad      : ",N_ELEMENTS(CGSETINTERSECTION(badFits_i,badGaussFits_i))
  ENDIF


END