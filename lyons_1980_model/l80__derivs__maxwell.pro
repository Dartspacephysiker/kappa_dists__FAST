;;10/29/16
;;Specify the values for derivatives of the ionospheric potential
;;iPotD[0] gives the value of, well, the ionospheric potential at x
;;iPotD[1] gives the value of phi'[x] at x
FUNCTION L80__DERIVS__MAXWELL,x,iPotD

  COMPILE_OPT IDL2,STRICTARRSUBS

  @~/idl/lib/hatch_idl_utils/knight_relation_funcs/common__dk_ode18.pro
  @common__l80_model.pro

  ;; dx = (x - l80xi[l80k-1])*1000.
  dx = x - l80xi[l80k-1]
  ;; IF l80k EQ 1 THEN print,dx

  ;;These guys are in kV, so swap to V
  ;; dPot     = iPotD[0]-( l80mPot[l80k-1] + ( (l80mPot[l80k]-l80mPot[l80k-1])/l80dxm ) * dx * SQRT(DK18__R_B__m) )
  dPot     = l80iPot[l80k-1] + iPotD[1] * dx - ( l80mPot[l80k-1] + ( (l80mPot[l80k]-l80mPot[l80k-1])/l80dxm ) * dx * SQRT(DK18__R_B__m) )
  ;; dPot     = l80iPot[l80k-1]-( l80mPot[l80k-1] + ( (l80mPot[l80k]-l80mPot[l80k-1])/l80dxm ) * dx * SQRT(DK18__R_B__m) )
  ;; IF (l80k/l80nSteps) LE 0.05 THEN PRINT,dPot
  IF l80k EQ 1 THEN PRINT,iPotD[0]
  ;; IF l80k EQ 1 THEN PRINT,dPot
  ;; dPot     = l80dPot
  ;; dPot     = l80mPot[l80k-1]
  ;; dPot     = iPotD[0]

  ;;Get field-aligned current density in A/m^2
  je       = KNIGHT_RELATION__DORS_KLETZING_4(DK18__T_m__m,DK18__dens_m__m, $
                                              dPot,DK18__R_B__m, $
                                              OUT_POTBAR=potBar, $
                                              /NO_MULT_BY_CHARGE)
  ;;Get Pedersen conductance in mhos
  ;;Also return electron energy flux in W/m^2
  Sigma_P  = PEDERSEN_HAREL1981(JEe,dPot, $
                                /MAXWELL, $
                                OUT_POTBAR=potBar, $
                                POT_IN_JOULES=pot_in_joules)

  ;; IF dPot LT 30 THEN BEGIN
  ;;    je = DOUBLE(1e-10)
  ;;    ;; jEe = DOUBLE(1e-10)
  ;;    ;; Sigma_P = PEDERSEN_HAREL1981(JEe,dPot, $
  ;;    ;;                            OUT_POTBAR=potBar, $
  ;;    ;;                            POT_IN_JOULES=pot_in_joules)
  ;; ENDIF

  dSigma_P_dJEe = PEDERSEN_HAREL1981(JEe,dPot, $
                                     ;; /MAXWELL, $
                                     /DERIVATIVE, $
                                     OUT_POTBAR=potBar, $
                                     POT_IN_JOULES=pot_in_joules)

  ;; Sigma_P  = PEDERSEN_ROBINSON1987(JEe,dPot, $
  ;;                                  IN_JE=je, $
  ;;                                  OUT_POTBAR=potBar, $
  ;;                                  POT_IN_JOULES=pot_in_joules)


  ;; dJEe_dPot     = KAPPA_1__DORS_KLETZING_EQ_14__D_EFLUX_D_POT__MAXWELL( $
  ;;                 ;; DK18__T_m__m,DK18__dens_m__m,dPot,DK18__R_B__m, $
  ;;                 DK18__T_m__m,DK18__dens_m__m,dPot,DK18__R_B__m, $
  ;;                 OUT_POTBAR=potBar, $
  ;;                 POT_IN_JOULES=pot_in_joules)


  ;;d (iPot) /dx
  ;;Units: ( kV / km ) = V / m
  ;; d1  = ( iPotD[0] - l80lastiPot ) / l80dx
  ;; d1  = ( iPotD[0] - l80iPot[l80k-1] ) / dx
  d1 = iPotD[1]

  ;; IF l80k GE 1930 THEN STOP

  ;;d (iPot') / dx
  CASE 1 OF
     ;; (KEYWORD_SET(l80use_meters) AND KEYWORD_SET(l80use_volts)): BEGIN
     ;;    ;;Units: ( V / m ) / m  = (V / m ) / m
     ;;    d2  = ( $
     ;;          je - $
     ;;          dSigma_P_dJEe * dJEe_dPot * iPotD[1] * iPotD[1] $
     ;;          ) / Sigma_P 
     ;; END
     ;; KEYWORD_SET(l80use_meters): BEGIN
     ;;    STOP
     ;; END
     ;; KEYWORD_SET(l80use_volts): BEGIN
     ;;    STOP
     ;; END
     ELSE: BEGIN
     ;;Units: ( kV / km ) / km  = (V / m ) / km

        ;;This version overshoots, but it's stable
        ;; d2  = ( $
        ;;       je * 1000. - $
        ;;       dSigma_P_dJEe * dJEe_dPot * iPotD[1] * iPotD[1] $
        ;;       ) / Sigma_P 

        ;; d2  = ( $
        ;;       je *1000. - $
        ;;       dSigma_P_dJEe * dJEe_dPot * ABS(iPotD[1]) * iPotD[1] $
        ;;       ) / Sigma_P


        ;; PRINT,'t1: ',je / Sigma_P * 1000.
        ;; PRINT,'t2: ',(-1.D) * dSigma_P_dJEe * dJEe_dPot * ABS(iPotD[1]) * iPotD[1] / Sigma_P

        d2  = ( $
              je - $
              ;; dSigma_P_dJEe * dJEe_dPot * ABS(iPotD[1]) * iPotD[1] $
              dSigma_P_dJEe * (jEe-l80jEe[l80k-1]) / dx * iPotD[1] $
              ;; (Sigma_P - l80lastGoodSigma_P) / dx * iPotD[1] $
              ) / Sigma_P

        ;; PRINT,'t1   : ',je / Sigma_P * 1000.
        ;; PRINT,'t2   : ',(-1.D) * (Sigma_P - l80lastGoodSigma_P) / dx * iPotD[1] / Sigma_P
        ;; PRINT,'t2alt: ',(-1.D) * dSigma_P_dJEe * dJEe_dPot * ABS(iPotD[1]) * iPotD[1] / Sigma_P

        IF ~(FINITE(d1) AND FINITE(d2)) THEN BEGIN
           PRINT,'Restarting!!!'
           l80RESTART = 1
        ENDIF
     END
  ENDCASE

  ;; PRINT,[d1,d2]
  RETURN,[d1,d2]
END
