PRO PARSE_KAPPA_2DFITS,fitParams, $
                       TINDEX=tIndex, $
                       STRUCT_A=Astruct, $
                       NAMES_A=A_names

  COMPILE_OPT idl2

  NStructs = N_ELEMENTS(fits)

  ;;Get A back in top shape

  A_names = ['bulk_energy', $
             'Temp', $
             'Kappa', $
             'N']

  Astruct = CREATE_STRUCT([A_names[0:3], $
                          REFORM(A[0,*]),REFORM(A[1,*]),REFORM(A[2,*]),REFORM(A[3,*]))
END