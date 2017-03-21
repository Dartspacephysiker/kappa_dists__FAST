;2017/03/21
;2017/03/17
;A      = vector of function params:

; A[0]: E_b,       Plasma bulk energy (eV)
; A[1]: T,         Plasma kinetic temperature (eV)
; A[2]: kappa,     Kappa (of course!)--or more specifically 3D kappa index, so that kappa = kappa_0 + 3/2
; A[3]: n,         Plasma density (cm^-3)
; A[4]: bulkAngle, Angle between bulk velocity, u_b, and velocity in direction for which we're interested in the distribution

FUNCTION INIT_JV_FITPARAM_INFO,A,fixA;; , $
                                  ;; ERANGE_PEAK=eRange_peak

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; ;;And don't let kappa get out of hand
  ;; AMaxStep[0]     = 1.0

  ;; ;;And don't let temperature get out of hand
  ;; AMaxStep[1]     = 30.

  ;; ;;And don't let DENSITY get out of hand!
  ;; AMaxStep[2]     = 0.5
  AMaxStep        = DOUBLE([1.0, $
                            30., $
                            0.5, $
                            1000])

  Alimited         = [[1,1], $
                      [1,1], $
                      [1,1], $
                      [1,1]]
  
  Alimits         = [[1.501D   ,100.0   ] , $ ;kappa 
                     [10      ,3.0e4 ] , $ ;Temp
                     [1e-4    ,100   ] , $ ;N
                     [1       ,1e4 ]]    ;R_B

  ;;Make 'em play nice
  ;; FOR k=0,N_ELEMENTS(A)-1 DO BEGIN
  ;;    IF A[k] LT Alimits[0,k] THEN A[k] = Alimits[0,k]
  ;;    IF A[k] GT Alimits[1,k] THEN A[k] = Alimits[1,k]
  ;; ENDFOR

  Alimited        = TRANSPOSE(Alimited)
  Alimits         = TRANSPOSE(Alimits)

  paramInfo = REPLICATE({value:0.D       , $
                       fixed:0B        , $
                       parname:''      , $
                       ;; relstep:0.D     , $
                       ;; mpmaxstep:0.D   , $
                       limited:[0B,0]   , $
                       limits:[0.D,0]} , $
                      ;; 7)
                      4)

  ;;Starting values
  paramInfo[*].value = A

  ;;Which ones are fixednnn?
  paramInfo[*].fixed = fixA

  ;;And their names?
  paramInfo[*].parName = ["kappa","T","N","R_B"]

  ;;Got it. What about anything like, you know, a max step size?
  ;; paramInfo[*].mpmaxstep  = AMaxStep

  ;;So certain values can't be exceeded?
  paramInfo[*].limited[0] = Alimited[*,0]
  paramInfo[*].limited[1] = Alimited[*,1]

  ;;What are the limits, then?
  paramInfo[*].limits[0] = Alimits[*,0]
  paramInfo[*].limits[1] = Alimits[*,1]

  RETURN,paramInfo

END
