; A[0]: E_b,       Plasma bulk energy (eV)
; A[1]: T,         Plasma kinetic temperature (eV)
; A[2]: kappa,     Kappa (of course!)--or more specifically 3D kappa index, so that kappa = kappa_0 + 3/2
; A[3]: n,         Plasma density (cm^-3)
PRO PRINT_KAPPA_FLUX_FIT_PARAMS,A,chi2, $
                                IS_MAXWELLIAN_FIT=is_Maxwellian_fit
  
  IF N_ELEMENTS(A) GE 7 THEN BEGIN
     PRINT,FORMAT='("Bulk energy (eV)",T20,"Plasma temp. (eV)",T50,"Kappa",T60,"Density (cm^-3)",T75,"Angle offset (deg)",A0)',''
     PRINT,FORMAT='(F-15.2,T20,F-15.2,T40,F-7.3,T50,F-8.4,T65,F-8.4)', $
           A[0], $
           A[1], $
           A[2], $
           A[3], $
           A[6]
  ENDIF ELSE BEGIN

     CASE 1 OF
        KEYWORD_SET(is_Maxwellian_fit): BEGIN

           PRINT,FORMAT='("Bulk energy (eV)",T20,"Plasma temp. (eV)",T50,"Kappa",T60,"Density (cm^-3)"'  + (KEYWORD_SET(chi2) ? ',T75,"Chi^2"' : '') + ')'
           CASE KEYWORD_SET(chi2) OF
              1: BEGIN
                 PRINT,FORMAT='(F-15.2,T20,F-15.2,T40,A8,T50,F-7.3,T60,F-8.4,T75,F11.3)', $
                       A[0], $
                       A[1], $
                       '', $
                       A[2], $
                       A[3], $
                       chi2
              END
              ELSE: BEGIN
                 PRINT,FORMAT='(F-15.2,T20,F-15.2,T40,A8,T50,F-7.3,T60,F-8.4)', $
                       A[0], $
                       A[1], $
                       '', $
                       A[2], $
                       A[3]
              END
           ENDCASE

        END
        ELSE: BEGIN

           PRINT,FORMAT='("Bulk energy (eV)",T20,"Plasma temp. (core) (eV)",T50,"Kappa",T60,"Density (cm^-3)"'  + (KEYWORD_SET(chi2) ? ',T75,"Chi^2"' : '') + ')'
           CASE KEYWORD_SET(chi2) OF
              1: BEGIN
                 PRINT,FORMAT='(F-15.2,T20,F-15.2,T40,F-8.2,T50,F-7.3,T60,F-8.4,T75,F11.3)', $
                       A[0], $
                       A[1], $
                       A[1]*(A[2]-1.5D)/A[2], $
                       A[2], $
                       A[3], $
                       chi2
              END
              ELSE: BEGIN
                 PRINT,FORMAT='(F-15.2,T20,F-15.2,T40,F-8.2,T50,F-7.3,T60,F-8.4)', $
                       A[0], $
                       A[1], $
                       A[1]*(A[2]-1.5D)/A[2], $
                       A[2], $
                       A[3]
              END
           ENDCASE


        END
     ENDCASE

  ENDELSE


END