;10/29/16
;kappa  ; kappa index of magnetospheric plasma
;T_m    : Temperature of magnetospheric plasma   ( eV    )
;dens_m : Density of magnetospheric plasma       ( cm^-3 )
;R_B    : Mirror ratio
PRO L80__INIT_MODEL_PARAMS__KAPPA,T_m,dens_m,R_B,kappa

  COMPILE_OPT IDL2

  @~/idl/lib/hatch_idl_utils/knight_relation_funcs/common__dk_ode18.pro

  DK18__kappa      = kappa
  DK18__T_m__k     = T_m
  DK18__dens_m__k  = dens_m
  DK18__R_B__k     = R_B

END
