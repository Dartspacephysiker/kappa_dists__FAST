;;10/29/16
;;Specify the values for derivatives of the ionospheric potential
;;iPotD[0] gives the value of, well, the ionospheric potential at x
;;iPotD[1] gives the value of phi'[x] at x
FUNCTION L80__DERIVS__MAXWELL,x,iPotD

  COMPILE_OPT IDL2

  @~/idl/lib/hatch_idl_utils/knight_relation_funcs/common__dk_ode18.pro
  @common__l80_model.pro

  ;;The Dors and Kletzing values
  ;; DK18__T_m__m    = 500
  ;; DK18__dens_m__m = 1
  ;; DK18__R_B__m    = 10

  ;;Use this as my last iPot??
  ;; l80lastiPot = l80iPot[l80k-1]

  ;;These guys are in kV, so swap to V
  ;; dPot     = (iPotD[0]-l80mPot[l80k])
  dPot     = l80dPot
  IF ~KEYWORD_SET(l80use_volts) THEN BEGIN
     dPot *= 1000.D
  ENDIF 

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

  dSigma_P_dJEe = PEDERSEN_HAREL1981(JEe,dPot, $
                                     ;; /MAXWELL, $
                                     /DERIVATIVE, $
                                     OUT_POTBAR=potBar, $
                                     POT_IN_JOULES=pot_in_joules)

  ;; Sigma_P  = PEDERSEN_ROBINSON1987(JEe,dPot, $
  ;;                                  IN_JE=je, $
  ;;                                  OUT_POTBAR=potBar, $
  ;;                                  POT_IN_JOULES=pot_in_joules)


  dJEe_dPot     = KAPPA_1__DORS_KLETZING_EQ_14__D_EFLUX_D_POT__MAXWELL( $
                  DK18__T_m__m,DK18__dens_m__m,dPot,DK18__R_B__m, $
                  OUT_POTBAR=potBar, $
                  POT_IN_JOULES=pot_in_joules)


  ;;d (iPot) /dx
  ;;Units: ( kV / km ) = V / m
  ;; d1  = ( iPotD[0] - l80lastiPot ) / l80dx
  ;; d1  = ( iPotD[0] - l80iPot[l80k-1] ) / dx
  d1 = iPotD[1]

  ;; IF l80k GE 1930 THEN STOP

  ;;d (iPot') / dx
  CASE 1 OF
     (KEYWORD_SET(l80use_meters) AND KEYWORD_SET(l80use_volts)): BEGIN
        ;;Units: ( V / m ) / m  = (V / m ) / m
        d2  = ( $
              je - $
              dSigma_P_dJEe * dJEe_dPot * iPotD[1] * iPotD[1] $
              ) / Sigma_P 
     END
     KEYWORD_SET(l80use_meters): BEGIN
        STOP
     END
     KEYWORD_SET(l80use_volts): BEGIN
        STOP
     END
     ELSE: BEGIN
     ;;Units: ( kV / km ) / km  = (V / m ) / km

        ;;This version overshoots, but it's stable
        ;; d2  = ( $
        ;;       je * 1000. - $
        ;;       dSigma_P_dJEe * dJEe_dPot * iPotD[1] * iPotD[1] $
        ;;       ) / Sigma_P 

        d2  = ( $
              je * 3300. - $
              dSigma_P_dJEe * dJEe_dPot * iPotD[1] * iPotD[1] $
              ) / Sigma_P 


        PRINT,'t1: ',je*1000. / Sigma_P
        PRINT,'t2: ',dSigma_P_dJEe * dJEe_dPot * iPotD[1] * iPotD[1] / Sigma_P
     END
  ENDCASE

  ;; PRINT,[d1,d2]
  RETURN,[d1,d2]
END
