;2017/03/21
PRO PRINT_JV_FIT_PARAMS,A
  
  PRINT,FORMAT='("Kappa",T10,"Plasma temp. (eV)",T30,"Density (cm^-3)",T45,"R_B")'
  PRINT,FORMAT='(F-7.3,T10,F-15.3,T30,F-8.4,T45,G-10.2)', $
        A[0], $
        A[1], $
        A[2], $
        A[3]

END


