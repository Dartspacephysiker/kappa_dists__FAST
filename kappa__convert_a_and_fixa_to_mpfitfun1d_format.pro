PRO KAPPA__CONVERT_A_AND_FIXA_TO_MPFITFUN1D_FORMAT,A,fixA

  COMPILE_OPT idl2

  A      = [A[0],A[1],A[2],A[3],A[6]]

  IF N_ELEMENTS(fixA) GE 7 THEN BEGIN
     ;;Invert sense of fixA here, since what MPFITFUN1D wants is opposite from what CURVEFIT wants
     fixA = ~[fixA[0],fixA[1], $
                   fixA[2],fixA[3], $
                   fixA[6]]
  ENDIF


END