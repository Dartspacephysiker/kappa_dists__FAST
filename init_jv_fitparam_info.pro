;2017/03/21
;A      = vector of function params:
;
; A[0]:       Kappa (of course!)--or more specifically 3D kappa index, so that kappa = kappa_0 + 3/2
; A[1]:       T,         Plasma kinetic temperature (eV)
; A[2]:       n,         Plasma density (cm^-3)
; A[3]:       R_B        Mag ratio
FUNCTION INIT_JV_FITPARAM_INFO,A,fixA, $
                               KAPPALIMS=kappaLims, $   
                               TEMPLIMS=TempLims, $    
                               DENSLIMS=DensLims, $    
                               MAGRATIOLIMS=magRatioLims
                                  ;; ERANGE_PEAK=eRange_peak

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; ;;And don't let kappa get out of hand
  ;; AMaxStep[0]     = 1.0

  ;; ;;And don't let temperature get out of hand
  ;; AMaxStep[1]     = 30.

  ;; ;;And don't let DENSITY get out of hand!
  ;; AMaxStep[2]     = 0.5
  AMaxStep        = DOUBLE([0.5, $
                            100., $
                            0.5, $
                            250])

  Alimited         = [[1,1], $
                      [1,1], $
                      [1,1], $
                      [1,1]]
  
  kappaLim        = KEYWORD_SET(kappaLims    ) ? kappaLims    : [1.505D   ,100.0   ]
  TempLim         = KEYWORD_SET(TempLims     ) ? TempLims     : [10      ,1.0D4   ]
  DensLim         = KEYWORD_SET(DensLims     ) ? DensLims     : [1E-3    ,100     ] 
  magRatioLim     = KEYWORD_SET(magRatioLims ) ? magRatioLims : [1       ,1D4     ]

  Alimits         = [[kappaLim] , $ ;kappa 
                     [TempLim ] , $ ;Temp
                     [DensLim ], $ ;N
                     [magRatioLim]]    ;R_B   

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
                         mpmaxstep:0.D   , $ ;;Do it this way, chump! Previously you were providing FOUR (identical) mpmaxstep values for EACH param
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
  IF N_ELEMENTS(AMaxStep) GT 0 THEN BEGIN
     paramInfo[*].mpmaxstep  = AMaxStep
  ENDIF

  ;;So certain values can't be exceeded?
  paramInfo[*].limited[0] = Alimited[*,0]
  paramInfo[*].limited[1] = Alimited[*,1]

  ;;What are the limits, then?
  paramInfo[*].limits[0] = Alimits[*,0]
  paramInfo[*].limits[1] = Alimits[*,1]

  RETURN,paramInfo

END
