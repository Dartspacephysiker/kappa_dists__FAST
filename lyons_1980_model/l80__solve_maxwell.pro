;;10/29/16
PRO L80__SOLVE_MAXWELL,V1,V2,kmWid,nSteps, $
                       T_m,dens_m,R_B, $
                       SET_DK_DEFAULTS=set_DK_defaults, $
                       SHOW=show, $
                       RTOL=rTol, $
                       ATOL=aTol
  COMPILE_OPT IDL2

  @~/idl/lib/hatch_idl_utils/knight_relation_funcs/common__dk_ode18.pro

  @common__l80_model.pro

  ;;The Dors and Kletzing values
  ;; DK18__T_m__m    = 500
  ;; DK18__dens_m__m = 1
  ;; DK18__R_B__m    = 10
  IF KEYWORD_SET(set_DK_defaults) THEN BEGIN
     T_m    = 500
     dens_m = 1
     R_B    = 10

     eps1   =  0.06D
     eps2   =  0.06D
     minXi  = -200.D 
     maxXi  =  200.D 

     V1     = eps1 * minXi
     V2     = eps2 * maxXi

     kmWidm = (maxXi - minXi) * SQRT(R_B)

     nSteps = 240
  ENDIF

  L80__INIT_MODEL_PARAMS__MAXWELL,T_m,dens_m,R_B

  ;; use_meters = 0
  ;; use_volts  = 0
  L80__INIT_MSPHERE_POTENTIAL,V1,V2,kmWidm,R_B, $
                              NSTEPS=nSteps, $
                              USE_METERS=use_meters, $
                              USE_VOLTS=use_volts, $
                              SHOW=show

  l80iPot    = MAKE_ARRAY(l80nSteps,/DOUBLE)

  ;;Initialize ionospheric potential with magnetospheric values
  ;; prem_diPot   = (l80mpot[1]-l80mpot[0]) * 0.1  ;Le prémier delta_iPot, kV
  ;; l80iPot[0]   = l80mPot[0]
  ;; l80iPot[1]   = l80iPot[0] + prem_diPot

  ;;No, initialize with the assumption that Lyons [1980] make
  l80iPot[0]    = l80mPot[0]
  l80iPot[-1]   = l80mPot[-1]
  
  prem_diPot    = (l80mpot[1]-l80mpot[0]) ;Le prémier delta_iPot, kV
  ;; l80iPot[1]    = l80mPot[0] + prem_diPot/SQRT(R_B)/l80dxi

  ;;Initialize Pedersen conductivity, in mhos
  ;; l80lastGoodSigma_P = PEDERSEN_HAREL1981(JEe,(-1.D)*prem_diPot*1000.D, $
  ;;                                         /MAXWELL, $
  ;;                                         OUT_POTBAR=potBar, $
  ;;                                         POT_IN_JOULES=pot_in_joules)
  ;; l80lastGoodSigma_P = 0.D


  iPotD      = [l80iPot[0],prem_diPot / l80dxi]

  ;;Solution vector
  l80dPot    = l80iPot[0]-l80mPot[0]
  l80soln    = FLTARR(2,l80nSteps)
  FOR l80k=1,l80nSteps - 1 DO BEGIN

     l80soln[*,l80k] = LSODE(iPotD,l80xi[l80k],l80dxi,'l80__derivs__maxwell', $
                             l80status, $
                             RTOL=rTol, $
                             ATOL=aTol)

     l80iPot[l80k] = l80soln[0,l80k]
     iPotD   = l80soln[*,l80k]
     l80dPot = l80iPot[l80k] - l80mPot[l80k]
  ENDFOR

  STOP

END
