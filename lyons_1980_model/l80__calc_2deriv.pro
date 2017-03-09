;;10/31/16
PRO L80__CALC_2DERIV

  COMPILE_OPT IDL2,STRICTARRSUBS

  @~/idl/lib/hatch_idl_utils/knight_relation_funcs/common__dk_ode18.pro
  @common__l80_model.pro

  l802deriv[1:-2] = (l80ipot[2:-1] - 2.D * l80ipot[1:-2] + l80ipot[0:-1]) / l80dxi^2

  
END
