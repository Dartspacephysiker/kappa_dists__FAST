;2016/07/13
;To be used as part of plotting kappa model- and Maxwellian model-derived currents versus observed currents (cf. PLOT_KAPPA_MAXWELL_AND_OBSERVED_CURRENT)
PRO GET_KAPPA_AND_MAXWELLIAN_CURRENT,kappa2D,Gauss2D, $
                                     kappaPot,gaussPot,magRatio, $
                                     kappa_current,gauss_current,obs_current, $
                                     DENSITY_KAPPA2D=dens_kappa2D, $
                                     DENSITY_GAUSS2D=dens_gauss2D, $
                                     MAKE_CURRENTS_POSITIVE=make_currents_positive, $
                                     QUIET=quiet

  COMPILE_OPT idl2

  CASE 1 OF
     KEYWORD_SET(dens_kappa2D): BEGIN
        IF N_ELEMENTS(dens_kappa2D) LT N_ELEMENTS(kappa2D.fitParams[2,*]) THEN BEGIN
           PRINT,"Number of 2D density estimates does not match number of 1D estimates!"
           STOP
        ENDIF
        IF ~KEYWORD_SET(quiet) THEN PRINT,'2D density estimate for kappa ...'
        densK                    = dens_kappa2D
     END
     ELSE: BEGIN
        ;; densK                    = AStruct.N
        densK                    = REFORM(kappa2D.fitParams[3,*])
     END
  ENDCASE

  CASE 1 OF
     KEYWORD_SET(dens_gauss2D): BEGIN
        IF N_ELEMENTS(dens_gauss2D) LT N_ELEMENTS(gauss2D.fitParams[2,*]) THEN BEGIN
           PRINT,"Number of 2D density estimates does not match number of 1D estimates!"
           STOP
        ENDIF
        IF ~KEYWORD_SET(quiet) THEN PRINT,'2D density estimate for gauss ...'
        densG                    = dens_gauss2D
     END
     ELSE: BEGIN
        ;; densG                    = gauss2D.N
        densG                    = gauss2D.fitParams[3,*]
     END
  ENDCASE

  ;;Get currents based on R value
  kappa_current                  = DOUBLE(1.0e6) * KNIGHT_RELATION__DORS_KLETZING_11( $
                                   REFORM(kappa2D.fitParams[2,*]), $
                                   REFORM(kappa2D.fitParams[1,*]),densK, $
                                   kappaPot/DOUBLE(1.6e-19), $
                                   magRatio) ;, $

  gauss_current                  = DOUBLE(1.0e6) * KNIGHT_RELATION__DORS_KLETZING_4( $
                                   gauss2D.fitParams[1,*], $
                                   densG, $
                                   gaussPot/DOUBLE(1.6e-19), $
                                   magRatio)

  IF KEYWORD_SET(make_currents_positive) THEN BEGIN

     KAPPA_FLIP_CURRENT,kappa_current,gauss_current,obs_current

  ENDIF

END