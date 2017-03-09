PRO GET_2DFIT_KAPPA_AND_MAXWELLIAN_CURRENT,kappa2D,gauss2D, $
   kappa_current,gauss_current, $
   ENERGY_ELECTRONS=energy_electrons, $
   ANGLE=angle


  COMPILE_OPT IDL2,STRICTARRSUBS

  kappa_current = MAKE_ARRAY(N_ELEMENTS(kappa2D.SDT),/FLOAT)
  gauss_current = MAKE_ARRAY(N_ELEMENTS(gauss2D.SDT),/FLOAT)
  
  FOR m=0,N_ELEMENTS(kappa_current)-1 DO BEGIN
     kappa_current[m] = J_2D_FS(kappa2D.SDT[m], $
                                ENERGY=energy_electrons, $
                                ANGLE=angle)
     gauss_current[m] = J_2D_FS(gauss2D.SDT[m], $
                                ENERGY=energy_electrons, $
                                ANGLE=angle)
     
  ENDFOR
  kappa_current       = kappa_current*(-1.6e-9)
  gauss_current       = gauss_current*(-1.6e-9)

  kappaTime           = kappa2D.SDT.time
  gaussTime           = gauss2D.SDT.time
  gaussInd            = VALUE_LOCATE(gaussTime,kappaTime)
  gauss_current       = gauss_current[gaussInd]

END