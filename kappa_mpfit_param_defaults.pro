;; 2017/11/22
;; Used by, e.g., INIT_KAPPA_FITPARAM_INFO and JOURNAL__20161011__THE_CLASSICS__OUTRIGHT_2DFIT

;; kFit__MaxStep     = REPLICATE(0.D,N_ELEMENTS(A))
;; ;;Don't let bulk energy get out of hand
;; kFit__MaxStep[0]  = 50.

;; ;;And don't let temperature get out of hand
;; kFit__MaxStep[1]  = 30.

;; ;;And don't let kappa get out of hand
;; kFit__MaxStep[2]  = 1.0

;; ;;And don't let DENSITY get out of hand!
;; kFit__MaxStep[3]  = 0.5

IF ~KEYWORD_SET(kFit__DEFSINITIALIZED) THEN BEGIN

   kFit__parmTmplt = {value     :  0.D      , $
                      fixed     :  0        , $
                      parname   :  ''       , $
                      ;; step   :  0.D      , $ ;step size for numerical derivatives
                      relstep   :  0.D      , $
                      mpmaxstep :  0.D      , $ ;max step in going from one fit to another
                      limited   :  [0,0]    , $
                      limits    :  [0.D,0]}

   kFit__parmNames      = ["E_b","T","kappa","N","bulkAngle"]

   kFit__MaxStep        = DOUBLE([50., $ ; A[0]: E_b,       
                                  ;; 50., $   ; A[1]: T,         
                                  0., $ ; A[1]: T,         
                                  0.5, $ ; A[2]: kappa,     
                                  0.30, $ ; A[3]: n,         
                                  0.])    ; A[4]: bulkAngle, 

   kFit__limited        = [[1,1], $
                           [1,1], $
                           [1,1], $
                           [1,1], $
                           ;; [0,0], $
                           ;; [0,0], $
                           [0,0]]

   kFit__limits         = [[100.,3.0D4]    , $ ;E_b
                           [10,2D4]     , $    ;Temp
                           [1.501D,100]  , $   ;kappa 
                           [1D-4,100]     , $  ;N
                           [-180,180]]         ;Bulk Angle
;; [1,0], $
;; [0,0], $
;; [0,0], $

;;Make 'em play nice
;; FOR k=0,N_ELEMENTS(A)-1 DO BEGIN
;;    IF A[k] LT kFit__limits[0,k] THEN A[k] = kFit__limits[0,k]
;;    IF A[k] GT kFit__limits[1,k] THEN A[k] = kFit__limits[1,k]
;; ENDFOR

;; kFit__limited        = TRANSPOSE(kFit__limited)
;; kFit__limits         = TRANSPOSE(kFit__limits)

   kFit__DEFSINITIALIZED = 1

ENDIF