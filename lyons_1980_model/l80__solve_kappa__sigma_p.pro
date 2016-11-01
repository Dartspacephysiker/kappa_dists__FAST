;;10/29/16
PRO L80__SOLVE_KAPPA__SIGMA_P,V1,V2,kmWid,nSteps, $
                              T_m,dens_m,R_B, $
                              SET_DK_DEFAULTS=set_DK_defaults, $
                              SHOW=show, $
                              RTOL=rTol, $
                              ATOL=aTol, $
                              EXPERIMENTAL=experimental, $
                              SAVE_CONVERGED_PLOT=save_converged_plot
  COMPILE_OPT IDL2

  @~/idl/lib/hatch_idl_utils/knight_relation_funcs/common__dk_ode18.pro
  @common__l80_model.pro

  ;;The Dors and Kletzing values
  ;; DK18__T_m__k    = 500
  ;; DK18__dens_m__k = 1
  ;; DK18__R_B__k    = 10
  IF KEYWORD_SET(set_DK_defaults) THEN BEGIN
     T_k    = 500
     dens_k = 1
     R_B    = 10
     kappa  = 2.3

     eps1   =  0.06D
     eps2   =  0.06D
     minXi  = -200.D * 1000.D
     maxXi  =  200.D * 1000.D

     V1     = eps1 * minXi
     V2     = eps2 * maxXi

     mWidm = (maxXi - minXi) * SQRT(R_B)

     lastDiff  = DOUBLE(V2)
     maxIncrem = 0.15
     miPotThresh = 1.5D

     nSteps = 6000

     IF (kappa GT 4.5) AND (kappa LE 5.5 ) THEN l80dPotThresh =  0.00047470516D
     IF (kappa GT 3.5) AND (kappa LE 4.5 ) THEN l80dPotThresh =  0.00165950371D
     IF (kappa GT 2.8) AND (kappa LE 3.5 ) THEN l80dPotThresh =  0.02921860578D
     IF (kappa GT 2.5) AND (kappa LE 2.8 ) THEN l80dPotThresh =  0.08296157481D
     IF (kappa GT 2.3) AND (kappa LE 2.5 ) THEN l80dPotThresh =  2.1719854D       ;with kappa eq 2.4
     IF (kappa GT 2.2) AND (kappa LE 2.3 ) THEN l80dPotThresh =  7.8338657D      ;with kappa eq 2.3
     IF (kappa GT 2.1) AND (kappa LE 2.2 ) THEN l80dPotThresh = 39.168273D       ;with kappa eq 2.2
     IF (kappa GT 2.1) AND (kappa LE 2.18) THEN l80dPotThresh = 30.319169
     IF (kappa GT 1.9) AND (kappa LE 2.1 ) THEN BEGIN

        T_k    = 3e3
        dens_k = 1.2

        l80dPotThresh = 39.168273D ;with kappa eq 2.2

        eps1   =  0.06D
        eps2   =  0.06D
        minXi  = -200.D * 1000.D
        maxXi  =  200.D * 1000.D

        V1     = eps1 * minXi
        V2     = eps2 * maxXi

        mWidm = (maxXi - minXi) * SQRT(R_B)

        lastDiff  = DOUBLE(V2)
        maxIncrem = 0.15
        miPotThresh = 1.5D

        nSteps = 6000

     ENDIF

  ENDIF

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;EXPERIMENTAL
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  IF KEYWORD_SET(experimental) THEN BEGIN
     T_k    = 1500
     dens_k = 0.5
     R_B    = 10
     kappa  = 1.91

     eps1   =  0.06D
     eps2   =  0.06D
     minXi  = -200.D * 1000.D
     maxXi  =  200.D * 1000.D

     V1     = eps1 * minXi
     V2     = eps2 * maxXi

     mWidm = (maxXi - minXi) * SQRT(R_B)

     lastDiff    = DOUBLE(V2)
     maxIncrem   = 0.15
     miPotThresh = 1.5D

     nSteps = 6000

     IF (kappa GT 4.5) AND (kappa LE 5.5 ) THEN l80dPotThresh =  0.00047470516D
     IF (kappa GT 3.5) AND (kappa LE 4.5 ) THEN l80dPotThresh =  0.00165950371D
     IF (kappa GT 2.8) AND (kappa LE 3.5 ) THEN l80dPotThresh =  0.02921860578D
     IF (kappa GT 2.5) AND (kappa LE 2.8 ) THEN l80dPotThresh =  0.08296157481D
     IF (kappa GT 2.3) AND (kappa LE 2.5 ) THEN l80dPotThresh =  2.1719854D       ;with kappa eq 2.4
     IF (kappa GT 2.2) AND (kappa LE 2.3 ) THEN l80dPotThresh =  22.990318        ;with kappa eq 2.3, T_k =600
     ;; IF (kappa GT 2.2) AND (kappa LE 2.3 ) THEN l80dPotThresh =  21.977891        ;with kappa eq 2.3, T_k =600
     ;; IF (kappa GT 2.2) AND (kappa LE 2.3 ) THEN l80dPotThresh =  125.11066       ;with kappa eq 2.3, T_k =800
     IF (kappa GT 2.2) AND (kappa LE 2.3 ) THEN l80dPotThresh =  386.68450       ;with kappa eq 2.3, T_k =1000
     IF (kappa GT 2.2) AND (kappa LE 2.3 ) THEN BEGIN
        l80dPotThresh =  100.7    ;with kappa eq 2.3, T_k =1500, width = 300 km
        l80dPotThresh =  109.42740     ;with kappa eq 2.3, T_k =1500, width = 300 km, dens = 0.9
        l80dPotThresh =   51.036079    ;with kappa eq 2.3, T_k =1500, width = 400 km, dens = 0.6
        l80dPotThresh =   46.865462    ;with kappa eq 2.3, T_k =1500, width = 400 km, dens = 0.5
        eps1   =  0.06D
        eps2   =  0.06D
        minXi  = -600.D * 1000.D
        maxXi  =  600.D * 1000.D

        V1     = eps1 * minXi
        V2     = eps2 * maxXi

        mWidm = (maxXi - minXi) * SQRT(R_B)

        lastDiff  = DOUBLE(V2)
        maxIncrem = 0.15
        miPotThresh = 1.5D
     ENDIF
     IF (kappa GT 2.1) AND (kappa LE 2.2 ) THEN l80dPotThresh = 39.168273D       ;with kappa eq 2.2
     IF (kappa GT 2.1) AND (kappa LE 2.18) THEN l80dPotThresh = 30.319169
     IF (kappa GT 1.9) AND (kappa LE 2.1 ) THEN BEGIN

        T_k    = 1800
        dens_k = 0.1
        R_B    = 30

        ;;Each of the following was done with nSteps = 6000
        l80dPotThresh = 10.168273D ;with kappa eq 2.2
        l80dPotThresh = 0.10074314 ;with kappa eq 2.1, dens = 0.6, T = 1500, wid = 600
        l80dPotThresh = 0.16518600 ;with kappa eq 2.1, dens = 0.4, T = 1500, wid = 800
        l80dPotThresh = 0.69783372 ;with kappa eq 2.1, dens = 0.1, T = 1500, wid = 800
        l80dPotThresh = 1.3878581e-11 ;with kappa eq 2.1, dens = 0.1, T = 1500, wid = 1000, R_B = 30
        
        ;;Each of the following was done with nSteps = 9000
        l80dPotThresh = 35.699277D ;with kappa eq 2.01, dens = 0.1, T = 1800, wid = 1500, R_B = 30
        ;; l80dPotThresh = 42.05D ;with kappa eq 2.01, dens = 0.1, T = 1800, wid = 1500, R_B = 30

        eps1   =  0.06D
        eps2   =  0.06D
        minXi  = -5000.D * 1000.D
        maxXi  =  5000.D * 1000.D

        V1     = eps1 * minXi
        V2     = eps2 * maxXi

        mWidm = (maxXi - minXi) * SQRT(R_B)

        lastDiff  = DOUBLE(V2)
        maxIncrem = 0.15
        miPotThresh = 1.5D

        ;; nSteps = 6000 ;;For every
        nSteps = 12000

     ENDIF
     IF (kappa GT 1.7) AND (kappa LE 1.9 ) THEN BEGIN
     
        T_k    = 3e3
        dens_k = 0.2
        R_B    = 30

        l80dPotThresh = 1e4 ;with kappa eq 2.2

        eps1   =  0.06D
        eps2   =  0.06D
        minXi  = -1500.D * 1000.D
        maxXi  =  1500.D * 1000.D

        V1     = eps1 * minXi
        V2     = eps2 * maxXi

        mWidm = (maxXi - minXi) * SQRT(R_B)

        lastDiff  = DOUBLE(V2)
        maxIncrem = 0.15
        miPotThresh = 1.5D

        nSteps = 6000

     ENDIF
  ENDIF



  l80upperLinBndonPot = l80dPotThresh

  L80__INIT_MODEL_PARAMS__KAPPA,T_k,dens_k,R_B,kappa

  ;; use_meters = 0
  ;; use_volts  = 0
  L80__INIT_MSPHERE_POTENTIAL,V1,V2,mWidm,R_B, $
                              NSTEPS=nSteps, $
                              USE_METERS=use_meters, $
                              USE_VOLTS=use_volts, $
                              SHOW=show

  L80__SETUP_SAVESTRING,/KAPPA

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
                                          /KAPPA, $
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
  satisfied     = 0
  WHILE ~satisfied DO BEGIN

     l80dPot       = l80iPot[0]-l80mPot[0]

     FOR l80k=startK,l80nSteps - 1 DO BEGIN

        l80soln[*,l80k] = LSODE(iPotD,l80xi[l80k],l80dxi,'l80__derivs__kappa__sigma_p', $
                                l80status, $
                                RTOL=rTol, $
                                ATOL=aTol)

        IF KEYWORD_SET(l80RESTART) THEN BEGIN
           startEField   = (INTERPOL(REFORM(l80soln[1,*]),l80nsteps,/LSQUADRATIC))[1]
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
                                                   /KAPPA, $
                                                   OUT_POTBAR=potBar, $
                                                   POT_IN_JOULES=pot_in_joules)

           l80jEe[l80k]     = JEe
           l80Sigma_P[l80k] = l80lastGoodSigma_P

           l80je[l80k]       = KNIGHT_RELATION__DORS_KLETZING_11(DK18__kappa,DK18__T_m__k,DK18__dens_m__k, $
                                                                 l80dPot,DK18__R_B__k, $
                                                                 OUT_POTBAR=potBar, $
                                                                 /NO_MULT_BY_CHARGE)

           dSigma_P_dJEe = PEDERSEN_HAREL1981(JEe,dPot, $
                                              ;; /KAPPA, $
                                              /DERIVATIVE, $
                                              OUT_POTBAR=potBar, $
                                              POT_IN_JOULES=pot_in_joules)

        ENDELSE
     ENDFOR

     L80__CALC_2DERIV
     L80__PLOT_VARS

     CASE 1 OF 
        ( (l80iPot[-1] - l80mPot[-1]) LT ( (-1.D) * miPotThresh ) ): BEGIN
           isPos          = 0
        END
        ( (l80iPot[-1] - l80mPot[-1]) GT (          miPotThresh ) ): BEGIN
           isPos          = 1
        END
        ELSE: satisfied = 1
     ENDCASE

     IF ~satisfied THEN BEGIN

        IF ABS(l80iPot[-1] - l80mPot[-1]) GT lastDiff THEN BEGIN
           maxIncrem = lastIncrem * 0.9
        ENDIF

        increm = ( 10.^(ALOG(ABS(l80iPot[-1] - l80mPot[-1]))-3.5D) ) < maxIncrem

        lastIncrem = increm
        lastDiff   = l80iPot[-1] - l80mPot[-1]
        ;; IF isPos THEN BEGIN
        ;;    l80dPotThresh *= 1.005
        ;; ENDIF ELSE BEGIN
        ;;    l80dPotThresh *= 0.9995
        ;; ENDELSE
        PRINT,"Increment: ",increm
        l80dPotThresh *= (isPos ? 1.D - increm : 1.D + increm )

        PRINT,"l80iPot[-1] - l80mPot[-1]: ",l80iPot[-1] - l80mPot[-1]
        PRINT,'Set l80dPotThresh to ' + STRCOMPRESS(l80dPotThresh,/REMOVE_ALL) + ' ...'
        l80iPot[0]    = l80mPot[0]
        
        prem_diPot    = (l80mpot[1]-l80mpot[0]) ;Le prémier delta_iPot, kV

        ;;Initialize Pedersen conductivity, in mhos
        l80lastGoodSigma_P = PEDERSEN_HAREL1981(JEe,0.D, $
                                                /KAPPA, $
                                                OUT_POTBAR=potBar, $
                                                POT_IN_JOULES=pot_in_joules)

        PRINT,"Starting eField: ",prem_diPot/l80dxi
        iPotD         = [l80iPot[0],prem_diPot / l80dxi,jEe]

     ENDIF
  ENDWHILE
  STOP

  IF KEYWORD_SET(save_converged_plot) THEN BEGIN
     l80File = '/SPENCEdata/Research/Satellites/FAST/kappa_dists/txtOutput/l80File.txt'
     plotDir = '/SPENCEdata/Research/Satellites/FAST/kappa_dists/plots/lyons_1980_model/' 

     ;;First the picture
     PRINT,'Saving to ' + l80savStr + ' ...'
     l80Window.Save,plotDir + l80savStr + '.png'

     ;;Now to text file
     OPENW,daLun,l80File,/GET_LUN,/APPEND
     PRINTF,daLun,l80savStr,l80dPotThresh
     CLOSE,daLun
     FREE_LUN,daLun
  ENDIF

  STOP

END
