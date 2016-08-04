PRO PRINT_KAPPA_GAUSS_CORRS_FOR_VARIOUS_MAGRATIOS,AStruct,AStructGauss, $
   obs_current, $
   kappaPot,gaussPot, $
   DENSITY_KAPPA2D=kappaDens, $
   DENSITY_GAUSS2D=gaussDens, $
   RBCORRLUN=RBCorrLun

  COMPILE_OPT idl2

  RBVals = [1,2,3,4,5,6,7,8,9,10,30,50,70,100,300,500,700]
  nRBVals = N_ELEMENTS(RBVals)
  
  kCorrArr = MAKE_ARRAY(nRBVals,/FLOAT)
  gCorrArr = MAKE_ARRAY(nRBVals,/FLOAT)

  kLSArr      = !NULL
  gLSArr      = !NULL
  FOR r=0,nRBVals-1 DO BEGIN

     GET_KAPPA_AND_MAXWELLIAN_CURRENT,AStruct,AStructGauss, $
                                      kappaPot,gaussPot,RBVals[r], $
                                      kappa_current,gauss_current,obs_current, $
                                      DENSITY_KAPPA2D=kappaDens, $
                                      DENSITY_GAUSS2D=gaussDens, $
                                      /QUIET

     KAPPA_FLIP_CURRENT,kappa_current,gauss_current,obs_current

     sortC_i  = SORT(obs_current)
     sortObsC = obs_current[sortC_i]
     sortKapC = kappa_current[sortC_i]
     sortGauC = gauss_current[sortC_i]

     kappaFit     = LINFIT(sortObsC,sortKapC, $
                           PROB=kappaProb, $
                           CHISQR=kappaChi)
     gaussFit     = LINFIT(sortObsC,sortGauC, $
                           PROB=gaussProb, $
                           CHISQR=gaussChi)

     kLineString  = STRING(FORMAT='("y=",F0.2,"x+",F0.2)',kappaFit[0],kappaFit[1])
     gLineString  = STRING(FORMAT='("y=",F0.2,"x+",F0.2)',gaussFit[0],gaussFit[1])

     kLSArr      = [kLSArr,kLineString]
     gLSArr      = [gLSArr,gLineString]

     kCorrArr[r] = CORRELATE(sortObsC,sortKapC)
     gCorrArr[r] = CORRELATE(sortObsC,sortGauC)
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