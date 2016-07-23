;2016/07/13
;To be used as part of plotting kappa model- and Maxwellian model-derived currents versus observed currents (cf. PLOT_KAPPA_MAXWELL_AND_OBSERVED_CURRENT)
PRO GET_KAPPA_AND_MAXWELLIAN_CURRENT,AStruct,AStructGauss, $
                                     kappaPot,gaussPot,magRatio, $
                                     kappa_current,gauss_current,obs_current, $
                                     DENSITY_KAPPA2D=dens_kappa2D, $
                                     DENSITY_GAUSS2D=dens_gauss2D, $
                                     MAKE_CURRENTS_POSITIVE=make_currents_positive

  COMPILE_OPT idl2

  CASE 1 OF
     KEYWORD_SET(dens_kappa2D): BEGIN
        IF N_ELEMENTS(dens_kappa2D) LT N_ELEMENTS(AStruct.N) THEN BEGIN
           PRINT,"Number of 2D density estimates does not match number of 1D estimates!"
           STOP
        ENDIF
        PRINT,'2D density estimate for kappa ...'
        densK                    = dens_kappa2D
     END
     ELSE: BEGIN
        densK                    = AStruct.N
     END
  ENDCASE

  CASE 1 OF
     KEYWORD_SET(dens_gauss2D): BEGIN
        IF N_ELEMENTS(dens_gauss2D) LT N_ELEMENTS(AStructGauss.N) THEN BEGIN
           PRINT,"Number of 2D density estimates does not match number of 1D estimates!"
           STOP
        ENDIF
        PRINT,'2D density estimate for gauss ...'
        densG                    = dens_gauss2D
     END
     ELSE: BEGIN
        densG                    = AStructGauss.N
     END
  ENDCASE

  ;;Get currents based on R value
  kappa_current                  = DOUBLE(1.0e6) * KNIGHT_RELATION__DORS_KLETZING_11(AStruct.kappa,AStruct.temp,densK, $
                                                                                     kappaPot/DOUBLE(1.6e-19),magRatio) ;, $
  gauss_current                  = DOUBLE(1.0e6) * KNIGHT_RELATION__DORS_KLETZING_4(AStructGauss.temp,densG, $
                                                                                    gaussPot/DOUBLE(1.6e-19),magRatio)

  IF KEYWORD_SET(make_currents_positive) THEN BEGIN
     ;;Flip current signs if they're negative
     flipMeK                     = WHERE(kappa_current LT 0,nFlipK,COMPLEMENT=posK_i,NCOMPLEMENT=nPosK)
     flipMeG                     = WHERE(gauss_current LT 0,nFlipG,COMPLEMENT=posG_i,NCOMPLEMENT=nPosG)

     IF nFlipK GT 0 THEN BEGIN
        IF nPosK GT 0 THEN BEGIN
           PRINT,"What to do? The sign is different ..."
           STOP
        ENDIF
        kappa_current[flipMeK]   = (-1.D)*kappa_current[flipMeK]
     ENDIF
     IF nFlipG GT 0 THEN BEGIN
        IF nPosG GT 0 THEN BEGIN
           PRINT,"What to do? The sign is different ..."
           STOP
        ENDIF
        gauss_current[flipMeG]   = (-1.D)*gauss_current[flipMeG]
     ENDIF

     IF KEYWORD_SET(obs_current) THEN BEGIN
        flipMeO                  = WHERE(obs_current LT 0,nFlipO,COMPLEMENT=posO_i,NCOMPLEMENT=nPosO)

        IF nFlipO GT 0 THEN BEGIN
           IF nPosO GT 0 THEN BEGIN
              PRINT,"What to do? The sign is different ..."
              STOP
           ENDIF
           obs_current[flipMeO]  = (-1.D)*obs_current[flipMeO]
        ENDIF
     ENDIF
  ENDIF

END