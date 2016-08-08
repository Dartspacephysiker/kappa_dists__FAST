;2016/08/05
;A      = vector of function params:

; A[0]: E_b,       Plasma bulk energy (eV)
; A[1]: T,         Plasma kinetic temperature (eV)
; A[2]: kappa,     Kappa (of course!)--or more specifically 3D kappa index, so that kappa = kappa_0 + 3/2
; A[3]: n,         Plasma density (cm^-3)
; A[4]: bulkAngle, Angle between bulk velocity, u_b, and velocity in direction for which we're interested in the distribution

;2016/08/08 Requiring no_fixA and no_eRange_peak to be explicitly requested so that nothing happens inadvertently. They should be
;updated, except in very particular circumstances such as those found in KAPPA_FIT2D__1DFIT_EACH_ANGLE
PRO UPDATE_KAPPA_FITPARAM_INFO,paramInfo,A,fixA,eRange_peak, $
                               NO_FIXA=no_fixA, $
                               NO_ERANGE_PEAK=no_eRange_peak

  COMPILE_OPT idl2

  ;;Make 'em play nice
  FOR k=0,N_ELEMENTS(A)-1 DO BEGIN
     IF A[k] LT paramInfo[k].limits[0] THEN A[k] = paramInfo[k].limits[0]
     IF A[k] GT paramInfo[k].limits[1] THEN A[k] = paramInfo[k].limits[1]
  ENDFOR

  ;;Starting values
  paramInfo[*].value = A

  ;;Which ones are fixed?
  IF ~KEYWORD_SET(no_fixA) THEN BEGIN
     paramInfo[*].fixed = fixA
  ENDIF

  IF ~KEYWORD_SET(no_eRange_peak) THEN BEGIN
     paramInfo[0].limits = eRange_peak
  END

END