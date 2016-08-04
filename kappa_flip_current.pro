PRO KAPPA_FLIP_CURRENT,kappa_current,gauss_current,obs_current

  COMPILE_OPT idl2

  ;;Flip current signs if they're negative
  flipMeK                     = WHERE(kappa_current LT 0,nFlipK,COMPLEMENT=posK_i,NCOMPLEMENT=nPosK)
  flipMeG                     = WHERE(gauss_current LT 0,nFlipG,COMPLEMENT=posG_i,NCOMPLEMENT=nPosG)

  CASE (nFlipK GT nPosK) OF
     1: BEGIN
        kappa_current = (-1.) * kappa_current
     END
     ELSE:
  ENDCASE

  CASE (nFlipG GT nPosG) OF
     1: BEGIN
        gauss_current = (-1.) * gauss_current
     END
     ELSE:
  ENDCASE

  ;; IF nFlipK GT 0 THEN BEGIN
  ;;    IF nPosK GT 0 THEN BEGIN
  ;;       PRINT,"What to do? The sign is different ..."
  ;;       STOP
  ;;    ENDIF
  ;;    kappa_current[flipMeK]   = (-1.D)*kappa_current[flipMeK]
  ;; ENDIF
  ;; IF nFlipG GT 0 THEN BEGIN
  ;;    IF nPosG GT 0 THEN BEGIN
  ;;       PRINT,"What to do? The sign is different ..."
  ;;       STOP
  ;;    ENDIF
  ;;    gauss_current[flipMeG]   = (-1.D)*gauss_current[flipMeG]
  ;; ENDIF

  IF KEYWORD_SET(obs_current) THEN BEGIN
     flipMeO     = WHERE(obs_current LT 0,nFlipO,COMPLEMENT=posO_i,NCOMPLEMENT=nPosO)

     CASE (nFlipO GT nPosO) OF
        1: BEGIN
           obs_current = (-1.) * obs_current
        END
        ELSE:
     ENDCASE

     ;; IF nFlipO GT 0 THEN BEGIN
     ;;    IF nPosO GT 0 THEN BEGIN
     ;;       PRINT,"What to do? The sign is different ..."
     ;;       STOP
     ;;    ENDIF
     ;;    obs_current[flipMeO]  = (-1.D)*obs_current[flipMeO]
     ;; ENDIF
  ENDIF


END