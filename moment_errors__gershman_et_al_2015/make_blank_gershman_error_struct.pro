;;2017/03/04
FUNCTION MAKE_BLANK_GERSHMAN_ERROR_STRUCT,max,N_R, $
                                          PRESSURE_COVAR_CALC=pressure_covar_calc, $
                                          HEATFLUX_COVAR_CALC=heatFlux_covar_calc

  COMPILE_OPT IDL2,STRICTARRSUBS

  IF ~KEYWORD_SET(N_R) THEN BEGIN
     N_R = 4 + (KEYWORD_SET(pressure_covar_calc) ? 6 : 0) + (KEYWORD_SET(heatFlux_covar_calc) ? 3 : 0)
  ENDIF

  CASE 1 OF
     KEYWORD_SET(max): BEGIN

        errThingArr = {N   : MAKE_ARRAY(max,/DOUBLE), $
                       Ux  : MAKE_ARRAY(max,/DOUBLE), $
                       Uy  : MAKE_ARRAY(max,/DOUBLE), $
                       Uz  : MAKE_ARRAY(max,/DOUBLE), $
                       Pxx : MAKE_ARRAY(max,/DOUBLE), $
                       Pyy : MAKE_ARRAY(max,/DOUBLE), $
                       Pzz : MAKE_ARRAY(max,/DOUBLE), $
                       Pxy : MAKE_ARRAY(max,/DOUBLE), $
                       Pxz : MAKE_ARRAY(max,/DOUBLE), $
                       Pyz : MAKE_ARRAY(max,/DOUBLE), $
                       Hx  : MAKE_ARRAY(max,/DOUBLE), $
                       Hy  : MAKE_ARRAY(max,/DOUBLE), $
                       Hz  : MAKE_ARRAY(max,/DOUBLE), $
                       R   : MAKE_ARRAY(max,N_R,N_R,/DOUBLE)}

     END
     ELSE: BEGIN

        errThingArr = {N   : 0.D, $
                       Ux  : 0.D, $
                       Uy  : 0.D, $
                       Uz  : 0.D, $
                       Pxx : 0.D, $
                       Pyy : 0.D, $
                       Pzz : 0.D, $
                       Pxy : 0.D, $
                       Pxz : 0.D, $
                       Pyz : 0.D, $
                       Hx  : 0.D, $
                       Hy  : 0.D, $
                       Hz  : 0.D, $
                       R   : MAKE_ARRAY(N_R,N_R,/DOUBLE)}

     END
  ENDCASE

  RETURN,errThingArr
  
END
