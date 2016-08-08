;2016/08/05
;A      = vector of function params:

; A[0]: E_b,       Plasma bulk energy (eV)
; A[1]: T,         Plasma kinetic temperature (eV)
; A[2]: kappa,     Kappa (of course!)--or more specifically 3D kappa index, so that kappa = kappa_0 + 3/2
; A[3]: n,         Plasma density (cm^-3)
; A[4]: bulkAngle, Angle between bulk velocity, u_b, and velocity in direction for which we're interested in the distribution

FUNCTION INIT_KAPPA_FITPARAM_INFO,A,fixA;; , $
                                  ;; ERANGE_PEAK=eRange_peak

  COMPILE_OPT idl2

  ;; AMaxStep        = REPLICATE(0.D,N_ELEMENTS(A))
  ;; ;;Don't let bulk energy get out of hand
  ;; AMaxStep[0]     = 50.

  ;; ;;And don't let temperature get out of hand
  ;; AMaxStep[1]     = 30.

  ;; ;;And don't let kappa get out of hand
  ;; AMaxStep[2]     = 1.0

  ;; ;;And don't let DENSITY get out of hand!
  ;; AMaxStep[3]     = 0.5
  AMaxStep        = DOUBLE([50.,30.,1.0,0.5,0.])

  Alimited         = [[1,1], $
                      [1,1], $
                      [1,1], $
                      [1,1], $
                      ;; [0,0], $
                      ;; [0,0], $
                      [0,0]]
  
  Alimits         = [[100.,1e4]    , $ ;E_b
                     [10,3.5e4]     , $ ;Temp
                     [1.5001D,100]  , $ ;kappa 
                     [1e-5,100]     , $ ;N
                     [-180,180]]       ;Bulk Angle
                     ;; [1,0], $
                     ;; [0,0], $
                     ;; [0,0], $

  ;;Make 'em play nice
  ;; FOR k=0,N_ELEMENTS(A)-1 DO BEGIN
  ;;    IF A[k] LT Alimits[0,k] THEN A[k] = Alimits[0,k]
  ;;    IF A[k] GT Alimits[1,k] THEN A[k] = Alimits[1,k]
  ;; ENDFOR

  Alimited        = TRANSPOSE(Alimited)
  Alimits         = TRANSPOSE(Alimits)

  paramInfo = REPLICATE({value:0.D       , $
                       fixed:0         , $
                       parname:''      , $
                       ;; relstep:0.D     , $
                       mpmaxstep:0.D   , $
                       limited:[0,0]   , $
                       limits:[0.D,0]} , $
                      ;; 7)
                      5)

  ;;Starting values
  paramInfo[*].value = A

  ;;Which ones are fixednnn?
  paramInfo[*].fixed = fixA

  ;;And their name?
  ;; paramInfo[*].parName = ["E_b","T","kappa","N","inDT","m","bulkAngle"]
  paramInfo[*].parName = ["E_b","T","kappa","N","bulkAngle"]

  ;;Got it. What about anything like, you know, a max step size?
  paramInfo[*].mpmaxstep  = AMaxStep

  ;;So certain values can't be exceeded?
  paramInfo[*].limited[0] = Alimited[*,0]
  paramInfo[*].limited[1] = Alimited[*,1]

  ;;What are the limits, then?
  paramInfo[*].limits[0] = Alimits[*,0]
  paramInfo[*].limits[1] = Alimits[*,1]

  RETURN,paramInfo

END