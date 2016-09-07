; A[0]: E_b,       Plasma bulk energy (eV)
; A[1]: T,         Plasma kinetic temperature (eV)
; A[2]: kappa,     Kappa (of course!)--or more specifically 3D kappa index, so that kappa = kappa_0 + 3/2
; A[3]: n,         Plasma density (cm^-3)
PRO PRINT_KAPPA_FLUX2D_HORSESHOE_PARAMS,A,chi2
  
  ;; PRINT,FORMAT='("Bulk energy (eV)",T20,"Plasma temp. (eV)",T40,"Kappa",T50,"Density (cm^-3)",T65,"Angle offset (deg)",A0)',''
  PRINT,FORMAT='(A0,T20,A0,T40,A0,T50,A0' + (KEYWORD_SET(chi2) ? ',T65,"Chi^2 (norm)"' : '') + ')', $
        "Bulk energy (eV)", $
        "Plasma temp. (eV)", $
        "Kappa", $
        "Density (cm^-3)"

  CASE KEYWORD_SET(chi2) OF
     1: BEGIN
        PRINT,FORMAT='(F-15.2,T20,F-15.2,T40,F-7.3,T50,F-8.4,T65,F11.3)', $
              A[0], $
              A[1], $
              A[2], $
              A[3], $
              chi2
     END
     ELSE: BEGIN
        PRINT,FORMAT='(F-15.2,T20,F-15.2,T40,F-7.3,T50,F-8.4)', $
              A[0], $
              A[1], $
              A[2], $
              A[3]
     END
  ENDCASE

END