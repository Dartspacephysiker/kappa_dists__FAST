;2018/02/01
;using data from Orbit 1773, our submitted GRL that probably should not be published
PRO JOURNAL__20180201__PLOT_LIEMOHN_AND_KHAZANOV_DENSITY_FACTOR

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; n = 1.88D                     ;cm^-3
  n = 1.D
  T = [13.75,27.5D,55.D,110D,220.D,440.D]                     ;eV
  E_b = 941.D                   ;eV

  minR_B  = 1.001D
  maxR_B  = 1D4
  dR_B    = 1.05D
  R_B = POWGEN(minR_B,maxR_B,dR_B)

  msph_to_fast = 1

  savePlot     = 1

  CASE 1 OF
     KEYWORD_SET(msph_to_FAST): BEGIN
        n      = 1.D
        yTitle = 'n!DFAST!N/n!Dm!N'
        legPos = [10,4.6]
        legData = 1
     END
     ELSE: BEGIN
        yTitle = 'n!Dm!N/n!DFAST!N'
        yLog   = 1
        legPos = [7000,70]
        legData = 1
     END
  ENDCASE

  nT = N_ELEMENTS(T)
  plotArr = MAKE_ARRAY(nT,/OBJ)

  lineStyles=['-','--','-:',"-.",':','__']

  window = WINDOW(DIMENSIONS=[900,600])

  FOR k=0,nT-1 DO BEGIN

     Ttemp = T[k]
     lineStyle = lineStyles[k]


     mapDens = NFACTOR_MAXWELLIAN_L_AND_K(E_b,Ttemp,kappa,n,R_B, $
                                          MAGNETOSPHERE_TO_FAST__NOT_FAST_TO_MSPH=msph_to_fast)

     ;; pTitle = STRING(FORMAT='(A0,A0,F0.2,A0)', $
     ;;                 "Density factor for Maxwellian + monotonic potential,!Cderived using Liemohn and Khazanov [1998] method", $
     ;;                 " ($\phi$!Dbar!N =", $
     ;;                 E_b/T, $
     ;;                ')')

     pTitle = STRING(FORMAT='(A0)', $
                     "Density factor for Maxwellian + monotonic potential,!Cderived using Liemohn and Khazanov [1998] method")
     pName  = STRING(FORMAT='(A0,F0.2)', $
                     "$\phi$!Dbar!N =", $
                     E_b/Ttemp)

     plotArr[k] = PLOT(R_B,mapDens, $
                       NAME=pName ,$
                       /XLOG, $
                       YLOG=yLog, $
                       XTITLE='R!DB!N', $
                       YTITLE=yTitle, $
                       TITLE=pTitle, $
                       LINESTYLE=lineStyle, $
                       /CURRENT, $
                      OVERPLOT=k GT 0)

  ENDFOR

  leg = LEGEND(TARGET=KEYWORD_SET(msph_to_FAST) ? plotArr : REVERSE(plotArr), $ ;Reversed so that plot labels are in same order as lines
               POSITION=legPos,DATA=legData)



  IF KEYWORD_SET(savePlot) THEN BEGIN
     SET_PLOT_DIR,plotDir,/FOR_KAPPA_DB,/ADD_TODAY
     ;; pName = 'densFac_Maxwellian__L_and_K_1998__potBar_eq_'+(STRING(FORMAT='(F0.2)',E_b/T)).Replace('.','_')+'.png'
     ;; pName = 'densFac_Maxwellian__L_and_K_1998.png'
     pName = 'densFac_Maxwellian__L_and_K_1998__potBar_eq_'$
             + ((STRING(FORMAT='(F0.2)',E_b/T[0])).Replace('.','_')) $
             + ((STRING(FORMAT='("-",F0.2)',E_b/T[-1])).Replace('.','_')) $
             + (KEYWORD_SET(msph_to_FAST) ? '_msph_to_FAST' : '_FAST_to_msph' ) + '.png'

     PRINT,'Saving to ' + pName + ' ...'
     window.Save,plotDir+pName

     ;; window.Close
     ;; window = !NULL
  ENDIF

  STOP

END
