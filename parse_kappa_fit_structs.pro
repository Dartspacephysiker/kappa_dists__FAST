PRO PARSE_KAPPA_FIT_STRUCTS,fits, $
                            A=a, $
                            TIME=time, $
                            MATCH_TIMES=match_times, $
                            TINDEX=tIndex, $
                            BULKANGLEINF=bulkAngleInf, $
                            STRUCT_A=Astruct, $
                            NAMES_A=A_names, $
                            CHI2=chi2, $
                            PVAL=pVal, $
                            FITSTATUS=fitStatus, $
                            USE_MPFIT1D=use_mpFit1D

  COMPILE_OPT idl2

  NStructs = N_ELEMENTS(fits)

  STR_ELEMENT_FROM_LIST_OF_STRUCTS,fits,'A',VALUE=A
  STR_ELEMENT_FROM_LIST_OF_STRUCTS,fits,'chi2',VALUE=chi2
  STR_ELEMENT_FROM_LIST_OF_STRUCTS,fits,'pVal',VALUE=pVal
  STR_ELEMENT_FROM_LIST_OF_STRUCTS,fits,'fitStatus',VALUE=fitStatus
  STR_ELEMENT_FROM_LIST_OF_STRUCTS,fits,'time',VALUE=time
  STR_ELEMENT_FROM_LIST_OF_STRUCTS,fits,'bulkAngleInf',VALUE=bulkAngleInf
  IF ARG_PRESENT(tIndex) THEN BEGIN
     STR_ELEMENT_FROM_LIST_OF_STRUCTS,fits,'time_index',VALUE=tIndex
  ENDIF

  ;;Get A back in top shape
  A = REFORM(A,N_ELEMENTS(A)/NStructs,NStructs)

  A_names = ['bulk_energy', $
             'temperature', $
             'Kappa', $
             'N', $
             'delta_t', $       ;bogus
             'mass', $          ;(eV/(km/s)^2)
             'Bulk_angle']

  IF KEYWORD_SET(match_times) THEN BEGIN
     match_i = VALUE_CLOSEST2(time,match_times)
  ENDIF ELSE BEGIN
     match_i = INDGEN(N_ELEMENTS(A[0,*]))
  ENDELSE



  CASE KEYWORD_SET(use_mpFit1D) OF
     1: BEGIN
        Astruct = CREATE_STRUCT([A_names[0:3],A_names[6]], $
                                REFORM(A[0,match_i]),REFORM(A[1,match_i]),REFORM(A[2,match_i]),REFORM(A[3,match_i]), $
                                REFORM(A[4,match_i]))
     END
     ELSE: BEGIN
        Astruct = CREATE_STRUCT(A_names, $
                                REFORM(A[0,match_i]),REFORM(A[1,match_i]),REFORM(A[2,match_i]),REFORM(A[3,match_i]), $
                                REFORM(A[4,match_i]),REFORM(A[5,match_i]),REFORM(A[6,match_i]))
     END
  ENDCASE

END