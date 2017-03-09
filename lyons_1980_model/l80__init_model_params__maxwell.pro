;10/29/16

;T_m    : Temperature of magnetospheric plasma   ( eV    )
;dens_m : Density of magnetospheric plasma       ( cm^-3 )
;R_B    : Mirror ratio
PRO L80__INIT_MODEL_PARAMS__MAXWELL,T_m,dens_m,R_B

  COMPILE_OPT IDL2,STRICTARRSUBS

  @~/idl/lib/hatch_idl_utils/knight_relation_funcs/common__dk_ode18.pro


  DK18__T_m__m     = T_m
  DK18__dens_m__m  = dens_m
  DK18__R_B__m     = R_B

END
