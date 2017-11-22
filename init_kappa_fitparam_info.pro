;2016/08/05
;A      = vector of function params:

; A[0]: E_b,       Plasma bulk energy (eV)
; A[1]: T,         Plasma kinetic temperature (eV)
; A[2]: kappa,     Kappa (of course!)--or more specifically 3D kappa index, so that kappa = kappa_0 + 3/2
; A[3]: n,         Plasma density (cm^-3)
; A[4]: bulkAngle, Angle between bulk velocity, u_b, and velocity in direction for which we're interested in the distribution

FUNCTION INIT_KAPPA_FITPARAM_INFO,A,fixA;; , $
                                  ;; ERANGE_PEAK=eRange_peak

  COMPILE_OPT IDL2,STRICTARRSUBS

  @common__kappa_mpfit_paraminfo.pro
  ;; These should be initialized elsewhere, like in the journal file

  @kappa_mpfit_param_defaults.pro

  kFit__parmInfo = REPLICATE(kFit__parmTmplt, $
                        ;; 7)
                        5)

  ;;Starting values
  kFit__parmInfo[*].value = A

  ;;Which ones are fixednnn?
  kFit__parmInfo[*].fixed = fixA

  ;;And their name?
  ;; kFit__parmInfo[*].parName = ["E_b","T","kappa","N","inDT","m","bulkAngle"]
  kFit__parmInfo[*].parName = kFit__parmNames

  ;;Got it. What about anything like, you know, a max step size?
  kFit__parmInfo[*].mpmaxstep  = kFit__MaxStep

  ;;So certain values can't be exceeded?
  kFit__parmInfo[*].limited[0] = (TRANSPOSE(kFit__limited))[*,0]
  kFit__parmInfo[*].limited[1] = (TRANSPOSE(kFit__limited))[*,1]

  ;;What are the limits, then?
  kFit__parmInfo[*].limits[0] = (TRANSPOSE(kFit__limits))[*,0]
  kFit__parmInfo[*].limits[1] = (TRANSPOSE(kFit__limits))[*,1]

  ;; kFit__parmInfo[1].relstep   = 0.05D

  ;;Modify some kappaness
  ;; kFit__parmInfo[1].mpmaxstep = 50.D ;T
  ;; kFit__parmInfo[2].mpmaxstep = 0.1D ;kappa
  ;; kFit__parmInfo[3].mpmaxstep = 0.5D ;n

  RETURN,kFit__parmInfo

END
