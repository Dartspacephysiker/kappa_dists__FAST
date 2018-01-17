PRO PRINT_KAPPA_LOOP_FIT_SUMMARY,fitStatus,gaussfitStatus,IS2D=is2D


  COMPILE_OPT IDL2,STRICTARRSUBS

  CASE 1 OF
     KEYWORD_SET(is2D): BEGIN
        OKStatus   = [1,2,3,4]  ;These are all the acceptable outcomes of fitting with MPFIT2DFUN

        nKappaHere = N_ELEMENTS(fitStatus)
        nGaussHere = N_ELEMENTS(gaussFitStatus)

        badFits_i  = MAKE_ARRAY(nKappaHere,/LONG)
        FOR k=0,nKappaHere-1 DO BEGIN
           badFits_i[k] = (WHERE(fitStatus[k] EQ OKStatus))[0] EQ -1
        ENDFOR

        nBadFits = N_ELEMENTS(WHERE(badFits_i,/NULL))
        
        IF nGaussHere GT 0 THEN BEGIN

           badGaussFits_i  = MAKE_ARRAY(nGaussHere,/LONG)

           FOR k=0,nGaussHere-1 DO BEGIN
              badGaussFits_i[k] = (WHERE(gaussFitStatus[k] EQ OKStatus))[0] EQ -1
           ENDFOR

           nBadGaussFits = N_ELEMENTS(WHERE(badGaussFits_i,/NULL))

        ENDIF

     END
     ELSE: BEGIN

        badFits_i                    = WHERE(fitStatus GT 0,nBadFits)  
        
        IF N_ELEMENTS(gaussFitStatus) GT 0 THEN BEGIN
           badGaussFits_i            = WHERE(gaussFitStatus GT 0,nBadGaussFits)  
        ENDIF

     END
  ENDCASE

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