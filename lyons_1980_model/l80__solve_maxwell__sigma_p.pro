;;10/29/16
PRO L80__SOLVE_MAXWELL__SIGMA_P,V1,V2,kmWid,nSteps, $
                       T_m,dens_m,R_B, $
                       SET_DK_DEFAULTS=set_DK_defaults, $
                       SHOW=show, $
                       RTOL=rTol, $
                       ATOL=aTol
  COMPILE_OPT IDL2,STRICTARRSUBS

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
     minXi  = -200.D * 1000.D
     maxXi  =  200.D * 1000.D

     V1     = eps1 * minXi
     V2     = eps2 * maxXi

     mWidm = (maxXi - minXi) * SQRT(R_B)

     nSteps = 4000
  ENDIF

  L80__INIT_MODEL_PARAMS__MAXWELL,T_m,dens_m,R_B

  ;; use_meters = 0
  ;; use_volts  = 0
  L80__INIT_MSPHERE_POTENTIAL,V1,V2,mWidm,R_B, $
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
  
  prem_diPot    = (l80mpot[1]-l80mpot[0]) ;Le prémier delta_iPot, kV

  ;;Initialize Pedersen conductivity, in mhos
  l80lastGoodSigma_P = PEDERSEN_HAREL1981(JEe,0.D, $
                                          /MAXWELL, $
                                          OUT_POTBAR=potBar, $
                                          POT_IN_JOULES=pot_in_joules)

  PRINT,"Starting eField: ",prem_diPot/l80dxi
  iPotD         = [l80iPot[0],prem_diPot / l80dxi,jEe]

  nRestarts     = 0
  startK        = 1

  ;;Solution vector
  l80soln       = MAKE_ARRAY(3,l80nSteps,/DOUBLE)

  ;;Others
  l80dPot       = l80iPot[0]-l80mPot[0]
  l80jEe        = MAKE_ARRAY(l80nSteps  ,/DOUBLE)
  l80je         = MAKE_ARRAY(l80nSteps  ,/DOUBLE)
  l80Sigma_P    = MAKE_ARRAY(l80nSteps  ,/DOUBLE)
  l80soln[*,0]  = iPotD

  ;;junk for plots
  l802deriv     = MAKE_ARRAY(l80nSteps,/DOUBLE)

  FOR l80k=startK,l80nSteps - 1 DO BEGIN

     l80soln[*,l80k] = LSODE(iPotD,l80xi[l80k],l80dxi,'l80__derivs__maxwell__sigma_p', $
                             l80status, $
                             RTOL=rTol, $
                             ATOL=aTol)

     IF KEYWORD_SET(l80RESTART) THEN BEGIN
        startEField   = (INTERPOL(REFORM(l80soln[1,*]),l80nsteps))[10]
        ;; startEField   = INTERPOL(REFORM(l80soln[1,*]),1,/LSQUADRATIC)
        PRINT,"REStarting eField: ",startEField
        iPotD         = [l80iPot[0],startEField]
        l80k          = startK

        l80dPot       = l80iPot[l80k] - l80mPot[l80k]
        l80RESTART    = 0
        nRestarts++

        IF nRestarts EQ 1000 THEN STOP

        L80__CALC_2DERIV
        L80__PLOT_VARS
        STOP
     ENDIF ELSE BEGIN

        l80iPot[l80k] = l80soln[0,l80k]
        iPotD   = l80soln[*,l80k]
        l80dPot = l80iPot[l80k] - l80mPot[l80k]

        l80lastGoodSigma_P = PEDERSEN_HAREL1981(JEe,l80dPot, $
                                                /MAXWELL, $
                                                OUT_POTBAR=potBar, $
                                                POT_IN_JOULES=pot_in_joules)

        l80jEe[l80k]     = JEe
        l80Sigma_P[l80k] = l80lastGoodSigma_P

        l80je[l80k]       = KNIGHT_RELATION__DORS_KLETZING_4(DK18__T_m__m,DK18__dens_m__m, $
                                                             l80dPot,DK18__R_B__m, $
                                                             OUT_POTBAR=potBar, $
                                                             /NO_MULT_BY_CHARGE)

        dSigma_P_dJEe = PEDERSEN_HAREL1981(JEe,dPot, $
                                           ;; /MAXWELL, $
                                           /DERIVATIVE, $
                                           OUT_POTBAR=potBar, $
                                           POT_IN_JOULES=pot_in_joules)

     ENDELSE
  ENDFOR

  L80__CALC_2DERIV
  L80__PLOT_VARS
  STOP

  ;;Twice
  PRINT,'Soln 2 ...'

  l80lastGoodSigma_P = PEDERSEN_HAREL1981(JEe,l80dPot, $
                                          /MAXWELL, $
                                          OUT_POTBAR=potBar, $
                                          POT_IN_JOULES=pot_in_joules)

  ;; startEField   = (l80ipot[1]-l80ipot[0])/l80dxi
  startEField   = INTERPOL(REFORM(l80soln[1,*]),1,/LSQUADRATIC)
  PRINT,"Starting eField: ",startEField
  iPotD         = [l80iPot[0],startEField,JEe]

  ;;Solution vector
  l80dPot       = l80iPot[0]-l80mPot[0]
  ;; l80soln       = MAKE_ARRAY(2,l80nSteps,/DOUBLE)
  ;; l80soln       = FLTARR(2,l80nSteps)
  l80soln[*,0]  = iPotD
  FOR l80k=startK,l80nSteps - 1 DO BEGIN

     l80soln[*,l80k] = LSODE(iPotD,l80xi[l80k],l80dxi,'l80__derivs__maxwell__sigma_p', $
                             l80status, $
                             RTOL=rTol, $
                             ATOL=aTol)

     IF KEYWORD_SET(l80RESTART) THEN BEGIN
        startEField   = (INTERPOL(REFORM(l80soln[1,*]),l80nsteps))[10]
        ;; startEField   = INTERPOL(REFORM(l80soln[1,*]),1,/LSQUADRATIC)
        PRINT,"REStarting eField: ",startEField
        iPotD         = [l80iPot[0],startEField]
        l80k          = startK

        l80dPot       = l80iPot[l80k] - l80mPot[l80k]
        l80RESTART    = 0
        nRestarts++

        IF nRestarts EQ 1000 THEN STOP

        L80__CALC_2DERIV
        L80__PLOT_VARS
        STOP
     ENDIF ELSE BEGIN

        l80iPot[l80k] = l80soln[0,l80k]
        iPotD   = l80soln[*,l80k]
        l80dPot = l80iPot[l80k] - l80mPot[l80k]

        l80lastGoodSigma_P = PEDERSEN_HAREL1981(JEe,l80dPot, $
                                                /MAXWELL, $
                                                OUT_POTBAR=potBar, $
                                                POT_IN_JOULES=pot_in_joules)

        l80jEe[l80k]     = JEe
        l80Sigma_P[l80k] = l80lastGoodSigma_P

        l80je[l80k]       = KNIGHT_RELATION__DORS_KLETZING_4(DK18__T_m__m,DK18__dens_m__m, $
                                                             l80dPot,DK18__R_B__m, $
                                                             OUT_POTBAR=potBar, $
                                                             /NO_MULT_BY_CHARGE)

        dSigma_P_dJEe = PEDERSEN_HAREL1981(JEe,dPot, $
                                           ;; /MAXWELL, $
                                           /DERIVATIVE, $
                                           OUT_POTBAR=potBar, $
                                           POT_IN_JOULES=pot_in_joules)

     ENDELSE
  ENDFOR

  L80__CALC_2DERIV
  L80__PLOT_VARS
  STOP

  STOP

END
