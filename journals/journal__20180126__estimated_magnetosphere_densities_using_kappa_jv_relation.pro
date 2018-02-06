;2018/01/26
PRO JOURNAL__20180126__ESTIMATED_MAGNETOSPHERE_DENSITIES_USING_KAPPA_JV_RELATION

  COMPILE_OPT IDL2,STRICTARRSUBS

  Kest = 9.81D-10               ; mho/m^2, est. from J-V relationship inferred from FAST orb 1773 measurements
  preFac = 2.681D-8             ; e^2/ SQRT(2 !DPI massElec)
  T    = 110                    ;eV
  ;; T    = 75                     ;low end
  ;; T    = 155                    ;high end

  kappas = [1.55D,1.75D,2,2.45,5,100,1000]

  nm = Kest / preFac * kappas^(1.5D) / KAPPA_GAMMARAT(kappas) $
       * SQRT(T * ( DOUBLE(1)- ( 3.D / ( 2.D * kappas) ) ) )

  ;; nm = Kest / preFac * kappas^(1.5D) * GAMMA(kappas-0.5D) / GAMMA(kappas+1.D) $
  ;;      * SQRT(T * ( DOUBLE(1)- ( 3.D / ( 2.D * kappas) ) ) )

  FOR k=0,6 DO PRINT,FORMAT='(I0,", ",F0.2,", ",G0.3)',k,kappas[k],nm[k]

END
