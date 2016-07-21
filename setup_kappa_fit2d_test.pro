PRO SETUP_KAPPA_FIT2D_TEST,good_angleBin_i,good_kappaFits_i,iWin, $
                           nEnergies,nTotAngles, $
                           curKappaStr,kappaFits,curDataStr, $
                           iAngle,iKappa,testKappa,testKappaFit,testArray, $
                           craptest, $
                           wts,X2D,Y2D,dataToFit, $
                           fa,dens_param
  
  COMPILE_OPT idl2

  iAngle             = good_angleBin_i[iWin]
  iKappa             = good_kappaFits_i[iWin]

  testKappa          = curKappaStr
  testKappa          = CONV_UNITS(testKappa,'counts')
  testKappa.ddata    = (testKappa.data)^.5
  testKappa          = CONV_UNITS(testKappa,units)

  testKappaFit       = kappaFits[iKappa]
  testData           = testKappa.data[*,iAngle]

  testArray          = MAKE_ARRAY(nEnergies,nTotAngles)
  FOR k=0,nTotAngles-1 DO BEGIN
     testArray[*,k]  = testData
  ENDFOR

  craptest           = testKappa
  craptest.data      = testArray

  ;;wts, calcked from fit
  wts                = 1.D/(testKappa.ddata)^2

  fixMe              = WHERE(~FINITE(wts),nFixMe)
  IF nFixMe GT 0 THEN BEGIN
     wts[fixMe]      = 0.0
  ENDIF

  X2D                = testKappa.energy
  Y2D                = testKappa.theta
  dataToFit          = curDataStr.data

  fa                 = {kappa_1d_fitparams:testKappaFit.A}
  dens_param         = testKappaFit.A[3]


END