;2017/04/21
FUNCTION JV_CURVE_FIT__TIE_R_B_AND_DENS__GET_DENS,pot,T_m,R_B

  COMPILE_OPT IDL2,STRICTARRSUBS

     @common__jv_curve_fit__tie_r_b_and_dens.pro

     ;;ionosphere R_B values - ind 1
     ;;FAST R_B values       - ind 1

     ;; R_B_ionos  = tRB_RBpairs[1,VALUE_CLOSEST2(tRB_RBpairs[1,*],R_B,/CONSTRAINED)]
     ;; IF ABS(R_B_ionos - R_B)/R_B_ionos GT 0.25 THEN STOP

     ;; R_B_FAST   = tRB_RBpairs[0,VALUE_CLOSEST2(tRB_RBpairs[1,*],R_B,/CONSTRAINED)]

     R_B_FAST  = INTERPOL(REFORM(tRB_RBpairs[0,*]),REFORM(tRB_RBpairs[1,*]),R_B)

     dens_m     = DENSITY_FACTOR__BARBOSA_1977(10.D^(MEAN(ALOG10(pot))), $
     ;; dens_m     = DENSITY_FACTOR__BARBOSA_1977(MAX(pot), $
     ;; tRB_nFLine = DENSITY_FACTOR__BARBOSA_1977(10.D^(MEAN(ALOG10(pot))), $
                                               T_m, $
                                               0, $
                                               tRB_nFAST, $
                                               R_B_FAST)
                                               ;; tRB_RBpairs[1,*])

     RETURN,dens_m
     
END
