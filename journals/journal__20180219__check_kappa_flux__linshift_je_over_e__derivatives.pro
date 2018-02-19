;2018/02/19
PRO JOURNAL__20180219__CHECK_KAPPA_FLUX__LINSHIFT_JE_OVER_E__DERIVATIVES

  COMPILE_OPT IDL2,STRICTARRSUBS

  energies = [815.4, 940.8, 1129., 1380., 1631., 1882., 2258., 2760., 3261., 3763., $
              4516., 5519., 6523., 7526., 9032., 1.104D4, 1.305D4,1.505D4,2.208D4, 3.011D4]
  P        = [energies[1],222,3.9,0.6,0.D]
  derivs   = [          1,  1,  1,  1,  0]
  modelFluxOverE = KAPPA_FLUX__LINEAR_SHIFT_IN_ENERGY__JE_OVER_E(energies,P,derivs)

  nHer     = N_ELEMENTS(modelFluxOverE)

  parNames = ['dPhi','T','kappa','n']
  ;; FOR k=0,3 DO BEGIN
  ;;    PRINT,parNames[k]
  ;;    PRINT,'--------'
  ;;    FOR j=0,nHer-1 DO BEGIN
  ;;       PRINT,FORMAT='(F8.1,TR4,G12.3,TR4,G12.3)',energies[j],modelFluxOverE[j],derivs[j,k]
  ;;    ENDFOR
  ;;    PRINT,''
  ;; ENDFOR

     PRINT,FORMAT='(A3,TR2,A-8,TR4,A-12,TR4,A-12,TR4,A-12,TR4,A-12,TR4,A-12)'," ","Energy","J(E)/E",parNames[0],parNames[1],parNames[2],parNames[3]
     PRINT,'--------------------------------------------------------------------------------'
     FOR j=0,nHer-1 DO BEGIN
        PRINT,FORMAT='(I3,TR2,F8.1,TR4,G12.4,TR4,G12.4,TR4,G12.4,TR4,G12.4,TR4,G12.4)',j+1,energies[j],modelFluxOverE[j],derivs[j,0],derivs[j,1],derivs[j,2],derivs[j,3]
     ENDFOR
     PRINT,''

  STOP
  
END
