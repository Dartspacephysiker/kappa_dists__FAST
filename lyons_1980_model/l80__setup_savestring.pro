;;11/01/16
PRO L80__SETUP_SAVESTRING,KAPPA=kappa, $
                          MAXWELL=Maxwell

  COMPILE_OPT IDL2

  @~/idl/lib/hatch_idl_utils/knight_relation_funcs/common__dk_ode18.pro
  @common__l80_model.pro

  CASE 1 OF
     KEYWORD_SET(kappa): BEGIN
        
        DKsavStr = STRING(FORMAT='(A0,G0.3,A0,G0.2,A0,I0,A0,F0.2)', $
                          'KEM--T_', $
                          DK18__T_m__k,'_eV--dens_', $
                          DK18__dens_m__k,'_cm-3--RB_', $
                          DK18__R_B__k,'--kappa_', $
                          DK18__kappa)                           
     END
     KEYWORD_SET(Maxwell): BEGIN
        DKsavStr = STRING(FORMAT='(A0,G0.3,A0,G0.2,A0,I0)', $
                          'Lyons1980--', $
                          DK18__T_m__m,'_eV--dens_', $
                          DK18__dens_m__m,'_cm-3--RB_', $
                          DK18__R_B__m)

     END
  ENDCASE

        l80savStr = STRING(FORMAT='(A0,I0,A0,F0.2,A0,F0.3,A0,F0.3)', $
                           '--nSteps_',l80nSteps, $
                           '--',l80mWidi/1000.,'km--eps1_', $
                           l80eps1,'--eps2_',l80eps2)

        l80savStr = DKsavStr + l80savStr

END
