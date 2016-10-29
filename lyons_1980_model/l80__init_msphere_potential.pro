;;10/29/16
;;kmWid : Width of the magnetospheric potential (at the ionosphere)               ( kilometers )
;;V1    : Magnetospheric potential in kV at LH boundary                           ( kilovolts  )
;;V2    : Magnetospheric potential in kV at RH boundary                           ( kilovolts  )
;;x     : Transverse points along arc at which to calculate ionospheric potential ( kilometers )
PRO L80__INIT_MSPHERE_POTENTIAL,V1,V2,kmWidm,R_B, $
                                NSTEPS=nSteps, $
                                USE_METERS=use_meters, $
                                USE_VOLTS=use_volts, $
                                SHOW=show

  COMPILE_OPT IDL2

  @common__l80_model.pro

  IF ( N_ELEMENTS(V1) EQ 0 ) OR ( N_ELEMENTS(V2) EQ 0 ) OR ( N_ELEMENTS(nSteps) EQ 0 ) THEN BEGIN
     PRINT,'MUST PROVIDE V1, V2, AND NSTEPS TO INITIALIZE LYONS [1980] MODEL'
  ENDIF

  IF ( nSteps MOD 2 ) EQ 0 THEN BEGIN
     PRINT,'Adjusting nSteps for you ... Be wise, and let there be an uneven number.'
     nSteps++
  ENDIF

  kmWidi                  = kmWidm / SQRT(R_B)

  nPerSide                = ( nSteps - 1 ) / 2

  l80xi                   = LINDGEN(nPerSide)+1

  l80xi                   = DOUBLE([ (-1)*REVERSE(l80xi), 0, l80xi ] ) * kmWidi / nPerSide / 2
  l80dxi                  = l80xi[1] - l80xi[0]
  l80dxi__meter           = l80dxi * 1000.D
  
  ;;magnetosphere variables
  l80R_B                  = R_B
  l80xm                   = l80xi * SQRT(l80R_B)
  l80dxm                  = l80xm[1] - l80xm[0]

  IF KEYWORD_SET(use_meters) THEN BEGIN
     l80use_meters  = 1
     l80xi         *= 1000.D
     l80dxi        *= 1000.D
     l80dxi__meter /= 1000.D

     l80xm         *= 1000.D
     l80dxm        *= 1000.D
     ;; l80dxm__meter /= 1000.D
  ENDIF

  IF KEYWORD_SET(use_volts) THEN BEGIN
     l80use_volts  = 1
     v1    *= 1000.D
     v2    *= 1000.D
  ENDIF

  l80dV1dx  = V1 / (l80xm[nPerSide]-l80xm[0])
  l80dV2dx  = V2 / (l80xm[-1]-l80xm[nPerSide])

  l80mPot                 = MAKE_ARRAY(nSteps,/DOUBLE)
  l80mPot[0:nPerSide-1]   = l80dV1dx * l80xm[0:nPerSide-1]
  l80mPot[nPerSide+1:-1]  = l80dV2dx * l80xm[nPerSide+1:-1]

  ;;Now set up COMMON vars
  l80V1     = TEMPORARY(V1)
  l80V2     = TEMPORARY(V2)
  l80kmWidm = TEMPORARY(kmWidm)
  l80nSteps = TEMPORARY(nSteps)

  ;;Wanna see?
  IF KEYWORD_SET(show) THEN BEGIN
     IF KEYWORD_SET(use_meters) THEN BEGIN
        xVar = l80xi / 1000.
     ENDIF ELSE BEGIN
        xVar = l80xi
     ENDELSE
     mPlot = PLOT(xVar,l80mPot, $
                  XTITLE='Transverse distance (km)', $
                  YTITLE='Potential (kV)')
  ENDIF

  ;; RETURN,l80mPot

END
