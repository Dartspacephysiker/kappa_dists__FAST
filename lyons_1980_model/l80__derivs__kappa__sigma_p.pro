;;10/29/16
;;Specify the values for derivatives of the ionospheric potential
;;iPotD[0] gives the value of, well, the ionospheric potential at x
;;iPotD[1] gives the value of phi'[x] at x
FUNCTION L80__DERIVS__KAPPA__SIGMA_P,x,iPotD

  COMPILE_OPT IDL2

  @~/idl/lib/hatch_idl_utils/knight_relation_funcs/common__dk_ode18.pro
  @common__l80_model.pro


  ;;Prize-winning combo in the past
  dx = x - l80xi[l80k-1]
  dPot     = l80iPot[l80k-1] + iPotD[1] * dx - ( l80mPot[l80k-1] + ( (l80mPot[l80k]-l80mPot[l80k-1])/l80dxm ) * dx * SQRT(DK18__R_B__k) )
  ;; IF l80k EQ 1 THEN print,dx

  ;; dPot     = l80dPot

  ;; dPot     = iPotD[0]-( l80mPot[l80k-1] + ( (l80mPot[l80k]-l80mPot[l80k-1])/l80dxm ) * dx * SQRT(DK18__R_B__k) )
  ;; dPot     = l80dPot
  ;; dPot     = l80mPot[l80k-1]
  ;; dPot     = iPotD[0]

  ;;Get field-aligned current density in A/m^2
  je       = KNIGHT_RELATION__DORS_KLETZING_11(DK18__kappa,DK18__T_m__k,DK18__dens_m__k, $
                                              dPot,DK18__R_B__k, $
                                              OUT_POTBAR=potBar, $
                                              /NO_MULT_BY_CHARGE)
  ;;Get Pedersen conductance in mhos
  ;;Also return electron energy flux in W/m^2
  Sigma_P  = PEDERSEN_HAREL1981(JEe,dPot, $
                                /KAPPA, $
                                OUT_POTBAR=potBar, $
                                POT_IN_JOULES=pot_in_joules)

  ;; IF dPot LE ( 30 > l80dPotThresh ) THEN BEGIN
  kappaS = DK18__kappa 
  IF DK18__kappa EQ 2.0 OR DK18__kappa EQ 1.5 THEN kappaS += 0.0001
  chiKappa = dPot / DK18__T_m__k / ( kappaS - 1.5D )
  ;; IF l80k LT 20 THEN PRINT,chiKappa
  ;; IF DK18__kappa LT 2.25 THEN min_dPot = l80upperLinBndonPot ELSE min_dPot = 30 

  ;;Calc the upper limit
  IF DK18__kappa LT 2.25 THEN min_dPot = l80upperLinBndonPot ELSE BEGIN
     min_dPot = 30 * DK18__T_m__k / 500 * ( kappaS - 1.5D )
     IF (dPot LT min_dPot) AND (l80k LT 2) THEN PRINT,"Min dPot based on chiKappa: ",min_dPot
  ENDELSE

  IF dPot LE min_dPot THEN BEGIN
     je    = ( dPot > l80dPotThresh ) / min_dPot * KNIGHT_RELATION__DORS_KLETZING_11(DK18__kappa,DK18__T_m__k,DK18__dens_m__k, $
                                                                                                    0.0D,DK18__R_B__k, $
                                                                                                    OUT_POTBAR=potBar, $
                                                                                                    /NO_MULT_BY_CHARGE)
  ENDIF

  dSigma_P_dJEe = PEDERSEN_HAREL1981(JEe,dPot, $
                                     ;; /KAPPA, $
                                     /DERIVATIVE, $
                                     OUT_POTBAR=potBar, $
                                     POT_IN_JOULES=pot_in_joules)

  ;; Sigma_P  = PEDERSEN_ROBINSON1987(JEe,dPot, $
  ;;                                  IN_JE=je, $
  ;;                                  OUT_POTBAR=potBar, $
  ;;                                  POT_IN_JOULES=pot_in_joules)


  ;; dJEe_dPot     = KAPPA_1__DORS_KLETZING_EQ_15__D_EFLUX_D_POT__KAPPA( $
  ;;                 ;; DK18__T_m__k,DK18__dens_m__k,dPot,DK18__R_B__k, $
  ;;                 DK18__T_m__k,DK18__dens_m__k,dPot,DK18__R_B__k, $
  ;;                 OUT_POTBAR=potBar, $
  ;;                 POT_IN_JOULES=pot_in_joules)


  ;;d (iPot) /dx
  ;;Units: ( kV / km ) = V / m
  ;; d1  = ( iPotD[0] - l80lastiPot ) / l80dx
  ;; d1  = ( iPotD[0] - l80iPot[l80k-1] ) / dx
  d1 = iPotD[1]

  ;; IF l80k GE 1930 THEN STOP

  ;;d (iPot') / dx
  d2  = ( $
        je - $
        ;; dSigma_P_dJEe * dJEe_dPot * ABS(iPotD[1]) * iPotD[1] $
        ;; dSigma_P_dJEe * (jEe-l80jEe[l80k-1]) / dx * iPotD[1] $
        ;; dSigma_P_dJEe * (jEe - iPotD[2]) / dx * iPotD[1] $
        dSigma_P_dJEe * (jEe - iPotD[2]) / dx * iPotD[1] $
        ;; (Sigma_P - l80lastGoodSigma_P) / dx * iPotD[1] $
        ) / Sigma_P

  IF ~(FINITE(d1) AND FINITE(d2)) THEN BEGIN
     PRINT,'Restarting!!!'
     l80RESTART = 1
  ENDIF

  ;;d (sigma_p) / dx
  ;;Units: ( coulomb^2 / joule-second ) / meter = coulomb - second / kg - m^3

  ;;d jEe / dx
  ;;Units: 
  d3 = (jEe - iPotD[2]) / dx

  ;; PRINT,[d1,d2]
  RETURN,[d1,d2,d3]
END
