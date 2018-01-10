;+
; NAME:
;
;
;
; PURPOSE:
;
;
;
; CATEGORY:
;
;
;
; CALLING SEQUENCE:
;
;
;
; INPUTS:
;
;
;
; OPTIONAL INPUTS:
;
;
;
; KEYWORD PARAMETERS:
;
;                     E_ANGLE            : This wants to be the range of angles (i.e., two elements) over which
;                                          we're estimating temperature, density, etc.                  
;
;
; OUTPUTS:
;
;
;
; OPTIONAL OUTPUTS:
;
;
;
; COMMON BLOCKS:
;
;
;
; SIDE EFFECTS:
;
;
;
; RESTRICTIONS:
;
;
;
; PROCEDURE:
;
;
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;
;-
;;

PRO KAPPA__GET_A_ESTIMATES,dat,Xorig,Yorig, $
                           minEInd,maxEInd,nEnergies, $
                           peak_ind,peak_energy,eRange_peak, $
                           ANGLES=angles, $
                           N_ANGLES_IN_RANGE=n_angles_in_range, $
                           BULKANGLE_STRUCT=angleStr, $
                           DONT_TAKE_STOCK_OF_BULKANGLE=dont_take_stock_of_bulkangle, $
                           KAPPA_EST=kappa, $
                           E_ANGLE=e_angle_range, $
                           ADD_GAUSSIAN_ESTIMATE=add_gaussian_estimate, $
                           USE_SDT_GAUSSIAN_FIT=use_SDT_Gaussian_fit, $
                           ESTFACS=estFacs, $
                           PHI__USE_ENERGY_BEFORE_PEAK=phi__use_energy_before_peak, $
                           A_OUT=A, $
                           AGAUSS_OUT=AGauss, $
                           DONT_PRINT_ESTIMATES=dont_print_estimates, $
                           TEST_NOREV=test_noRev, $
                           UNITS=units


  COMPILE_OPT idl2,STRICTARRSUBS

  COMMON FIT_MASS,mass

  IF N_ELEMENTS(units) EQ 0 THEN BEGIN
     units = 'eFlux'
  ENDIF

  ;; McFadden says: "dat.mass is a scaler [sic] with units of eV/(km/s)^2."
  ;; Use 1.6e-12erg/eV * (km/1.e5cm)^2 to convert
  mass = dat.mass*1.6e-22

  IF SIZE(estFacs,/TYPE) NE 8 THEN BEGIN
     estFacs      = {T:1.0, $
                     N:10., $
                     B_E:1.0, $
                     TGauss:1.0, $
                     NGauss:5.0, $
                     B_EGauss:1.0}
  ENDIF

  IF KEYWORD_SET(test_noRev) THEN BEGIN
     ;; max_energy =  Xorig[(minEInd - 2) > 0]
     ;; min_energy =  Xorig[(maxEInd + 2) < (nEnergies - 1)]
     max_energy =  Xorig[minEInd > 0]
     min_energy =  Xorig[maxEInd < (nEnergies - 1)]
  ENDIF ELSE BEGIN
     ;; min_energy =  Xorig[(minEInd - 2) > 0]
     ;; max_energy =  Xorig[(maxEInd + 2) < (nEnergies - 1)]
     min_energy = !NULL
     max_energy = !NULL
  ENDELSE

  eRange_peak     = [min_energy,max_energy]

  IF KEYWORD_SET(phi__use_energy_before_peak) THEN BEGIN

     bulk_energy = (Xorig[peak_ind+1] LT peak_energy ? $
                    Xorig[peak_ind+1] : $
                    Xorig[peak_ind-1]) * estFacs.B_E

  ENDIF ELSE BEGIN

     bulk_energy = peak_energy*estFacs.B_E

  ENDELSE

  ;;So we estimate the temperature and density based on the full range of angles being considered 
  T               = (T_2D_FS(dat,ENERGY=eRange_peak, $
                             ANGLE=KEYWORD_SET(dont_take_stock_of_bulkangle) ? angles : e_angle_range))[3]*estFacs.T ;T_avg
  n_est           = N_2D_FS(dat,ENERGY=eRange_peak, $
                            ANGLE=KEYWORD_SET(dont_take_stock_of_bulkangle) ? angles : e_angle_range)*estFacs.N

  ;; IF KEYWORD_SET(dont_take_stock_of_bulkangle) THEN BEGIN
  ;;    n_est        /= n_angles_in_range
  ;; ENDIF

  ;;Decide on angle range
  IF N_ELEMENTS(angleStr) GT 0 THEN BEGIN
     IF angleStr.useMe THEN BEGIN
        bulkAngle = angleStr.bulkAngle
        bulkAngleProvided = 1
     ENDIF ELSE BEGIN
        bulkAngleProvided = 0
     ENDELSE
  ENDIF ELSE BEGIN
     bulkAngleProvided    = 0
  ENDELSE

  CASE bulkAngleProvided OF
     0: BEGIN
        angleOffset = 0         ;Just assume stuff is field aligned
     END
     1: BEGIN
        angleOffset  = MEAN(angles)-bulkAngle
     END
  ENDCASE

  A               = DOUBLE([bulk_energy,T,kappa,n_est,dat.integ_t,dat.mass,angleOffset]) 
  
  IF ~KEYWORD_SET(dont_print_estimates) THEN BEGIN
     PRINT,"Here's my initial estimate based on spectral properties: "
     PRINT_KAPPA_FLUX_FIT_PARAMS,A
     PRINT,''
  ENDIF

  IF KEYWORD_SET(add_gaussian_estimate) THEN BEGIN

     IF KEYWORD_SET(use_SDT_Gaussian_fit) THEN BEGIN
        MAXWELLIAN_1,Xorig,AGauss,Yorig,pder,INDEX=[peak_ind,maxEInd],UNITS=units

        T  = -1./Agauss[1]
        f0 = AGauss[0]
        density = AGauss[0]*exp(AGauss[1]*Xorig[peak_ind])*1.e-15 * (-1.*2.*3.1416*1.6e-12/(AGauss[1]*(mass)))^1.5 

        ;;Get AGauss in the form we like it
        IF ~KEYWORD_SET(dont_print_estimates) THEN BEGIN
           PRINT,"Here's my initial Gaussian estimate based on spectral properties: "
           PRINT_KAPPA_FLUX_FIT_PARAMS,[Xorig[peak_ind],T,999,density,A[4],A[5]]
        ENDIF

     ENDIF ELSE BEGIN

        bulk_EGauss  = peak_energy*estFacs.B_EGauss

        TGauss       = (T_2D_FS(dat,ENERGY=eRange_peak,ANGLE=e_angle_range))[3]*estFacs.TGauss
        n_estGauss   = N_2D_FS(dat,ENERGY=eRange_peak,ANGLE=e_angle_range)*estFacs.NGauss
        
        kappaGauss   = 100

        AGauss       = DOUBLE([bulk_EGauss,TGauss,kappaGauss,n_estGauss, $
                               dat.integ_t,dat.mass,angleOffset])

        IF ~KEYWORD_SET(dont_print_estimates) THEN BEGIN
           PRINT,"Here's my initial Gaussian estimate based on spectral properties: "
           PRINT_KAPPA_FLUX_FIT_PARAMS,AGauss
           PRINT,''
        ENDIF
     ENDELSE
  ENDIF

END
