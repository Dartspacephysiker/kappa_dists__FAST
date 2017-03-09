PRO PARSE_KAPPA_2DFITS,fitParams, $
                       TINDEX=tIndex, $
                       STRUCT_A=Astruct, $
                       NAMES_A=A_names

  COMPILE_OPT IDL2,STRICTARRSUBS

  NStructs = N_ELEMENTS(fits)

  ;;Get A back in top shape

  A_names = ['bulk_energy', $
             'Temp', $
             'Kappa', $
             'N']

  Astruct = CREATE_STRUCT([A_names[0:3]], $
                           REFORM(fitParams[0,*]), $
                           REFORM(fitParams[1,*]), $
                           REFORM(fitParams[2,*]), $
                           REFORM(fitParams[3,*]))
END