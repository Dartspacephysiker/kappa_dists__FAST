PRO   KAPPA_FIT2D_DEFAULTS, $
   BOUNDS=bounds
   ;; FIT_EACH_ANGLE=fit_each_angle, $

  COMPILE_OPT IDL2,STRICTARRSUBS

  IF ~KEYWORD_SET(bounds) THEN BEGIN
     bounds                    = 0 ;just do one
  ENDIF

  ;; IF ~KEYWORD_SET(fit_each_angle) THEN BEGIN
  ;;    fit_each_angle            = 1
  ;; ENDIF

END