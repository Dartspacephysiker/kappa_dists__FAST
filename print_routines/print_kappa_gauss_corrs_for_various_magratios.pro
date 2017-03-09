PRO PRINT_KAPPA_GAUSS_CORRS_FOR_VARIOUS_MAGRATIOS,AStruct,AStructGauss, $
   obs_current, $
   kappaPot,gaussPot, $
   EXCLUDE_KAPPA_I=excludeK_i, $
   EXCLUDE_GAUSS_I=excludeG_i, $
   DENSITY_KAPPA2D=kappaDens, $
   DENSITY_GAUSS2D=gaussDens, $
   RBCORRLUN=RBCorrLun

  COMPILE_OPT IDL2,STRICTARRSUBS

  RBVals = [1,2,3,4,5,6,7,8,9,10,20,30,40,50,60, $
            70,80,90,100,300,500,700,1000,3000,5000,7000,10000,30000,50000]
  nRBVals = N_ELEMENTS(RBVals)
  
  kCorrArr = MAKE_ARRAY(nRBVals,/FLOAT)
  gCorrArr = MAKE_ARRAY(nRBVals,/FLOAT)

  kLSArr      = !NULL
  gLSArr      = !NULL

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;And, some cleaning
  IF N_ELEMENTS(excludeK_i) GT 0 THEN BEGIN
     includeK_i = CGSETDIFFERENCE(INDGEN(N_ELEMENTS(AStruct.N)),excludeK_i)
  ENDIF ELSE BEGIN
     includeK_i = INDGEN(N_ELEMENTS(AStruct.N))
  ENDELSE

  IF N_ELEMENTS(excludeG_i) GT 0 THEN BEGIN
     includeG_i = CGSETDIFFERENCE(INDGEN(N_ELEMENTS(AStructGauss.N)),excludeG_i)
  ENDIF ELSE BEGIN
     includeG_i = INDGEN(N_ELEMENTS(AStructGauss.N))
  ENDELSE

  obsCK        = obs_current[includeK_i]
  obsCG        = obs_current[includeG_i]

  FOR r=0,nRBVals-1 DO BEGIN

     GET_KAPPA_AND_MAXWELLIAN_CURRENT,AStruct,AStructGauss, $
                                      kappaPot,gaussPot,RBVals[r], $
                                      kappa_current,gauss_current,obs_current, $
                                      DENSITY_KAPPA2D=kappaDens, $
                                      DENSITY_GAUSS2D=gaussDens, $
                                      /QUIET

     KAPPA_FLIP_CURRENT,kappa_current,gauss_current,obs_current

     kc           = kappa_current[includeK_i]
     gc           = gauss_current[includeG_i]

     sortCK_i  = SORT(obsCK)
     sortObsCK = obsCK[sortCK_i]
     sortKapC  = kc[sortCK_i]
     kappaFit     = LINFIT(sortObsCK,sortKapC, $
                           PROB=kappaProb, $
                           CHISQR=kappaChi)

     sortCG_i  = SORT(obsCG)
     sortObsCG = obsCG[sortCG_i]
     sortGauC  = gc[sortCG_i]

     gaussFit     = LINFIT(sortObsCG,sortGauC, $
                           PROB=gaussProb, $
                           CHISQR=gaussChi)

     kLineString  = STRING(FORMAT='("y=",F0.2,"x+",F0.2)',kappaFit[0],kappaFit[1])
     gLineString  = STRING(FORMAT='("y=",F0.2,"x+",F0.2)',gaussFit[0],gaussFit[1])

     kLSArr      = [kLSArr,kLineString]
     gLSArr      = [gLSArr,gLineString]

     kCorrArr[r] = CORRELATE(sortObsCK,sortKapC)
     gCorrArr[r] = CORRELATE(sortObsCG,sortGauC)
  ENDFOR

  IF N_ELEMENTS(RBCorrLun) EQ 0 THEN RBCorrLun = -1 ;stdout

  PRINTF,RBCorrLun,''
  PRINTF,RBCorrLun,FORMAT='("R_B",T15,"kappaCorr",T30,"gaussCorr",T45,"kappaFit",T65,"gaussFit")'
  FOR r=0,nRBVals-1 DO BEGIN
     PRINTF,RBCorrLun,FORMAT='(G0,T15,F0.3,T30,F0.3,T45,A0,T65,A0)', $
            RBVals[r], $
            kCorrArr[r], $
            gCorrArr[r], $
            kLSArr[r], $
            gLSArr[r]
  ENDFOR
  
END