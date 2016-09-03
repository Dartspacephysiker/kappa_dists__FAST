;;09/02/16
FUNCTION KAPPA_EFLUX__ANISOTROPY_DIST,X,Y,Z, $
                                      bestAngle_i, $
                                      BULK_ENERGY=bulk_energy, $
                                      MIN_ENERGY=min_energy, $
                                      REDUCENEGFAC=reduceNegFac, $
                                      LOGSCALE_REDUCENEGFAC=logScale, $
                                      PLOT_BULKE_MODEL=plot_bulke_model, $
                                      PLOT_BULKE_FACTOR=plot_bulke_factor, $
                                      POLARPLOT_BULKE_FACTOR=polarPlot_bulke_factor, $
                                      PLOT_BULKE_MODEL_V_DATA_COMPARISON=plot_comparison, $
                                      PLOT_FLUX_PEAKS=plot_flux_peaks, $
                                      PLOTDIR=plotDir, $
                                      ORBIT=orbit, $
                                      SAVE_PLOTS=save_plots, $
                                      OUT_PEAK_ENERGIES=peak_en, $
                                      OUT_PEAK_FLUXES=peak_flux, $
                                      OUT_ANGLES=peak_angle, $
                                      PRINT=print

  COMPILE_OPT IDL2

  IF KEYWORD_SET(save_plots) THEN BEGIN
     SET_PLOT_DIR,plotDir,/FOR_KAPPA_DB,/ADD_TODAY

     pSuff              = '.png'

     orbStr             = KEYWORD_SET(orbit)  ? STRING(FORMAT='("--",I0)',orbit) : ''
     timeStr            = KEYWORD_SET(time)   ? STRING(FORMAT='("--",A0)',TIME_TO_STR(time,/MS))  : ''
     minEnStr           = KEYWORD_SET(minEn)  ? STRING(FORMAT='("--minEn_",I0)',minEn)  : ''

     bFuncSPName        = 'kappa_anisotropy--bFunc'      +orbStr+timeStr+minEnStr+pSuff
     bFuncPolarSPName   = 'kappa_anisotropy--bFunc_polar'+orbStr+timeStr+minEnStr+pSuff
     gFuncSPName        = 'kappa_anisotropy--gFunc'      +orbStr+timeStr+minEnStr+pSuff

  ENDIF

  ;;Initialize
  angles                = Y[0,*]
  nAngles               = N_ELEMENTS(angles)

  peak_en               = MAKE_ARRAY(nAngles,/DOUBLE)
  peak_flux             = MAKE_ARRAY(nAngles,/DOUBLE)
  peak_angle            = MAKE_ARRAY(nAngles,/DOUBLE)

  allEn_i               = INDGEN(N_ELEMENTS(X[*,0]))
  energyMin             = MIN(ABS(X[*,bestAngle_i]-bulk_energy),energy_i)

  peak_en[bestAngle_i]  = X[energy_i,bestAngle_i]

  ;;Now loop
  FOR k=0,nAngles-1 DO BEGIN
     tmpAngle_i  = (bestAngle_i + k    ) MOD nAngles
     prevAngle_i = (bestAngle_i + k - 1) MOD nAngles

     IF KEYWORD_SET(min_energy) THEN BEGIN
        tmpEn    = X[WHERE(X[*,tmpAngle_i] GE min_energy),tmpAngle_i]
        tmpAngle = Y[WHERE(X[*,tmpAngle_i] GE min_energy),tmpAngle_i]
        tmpData  = Z[WHERE(X[*,tmpAngle_i] GE min_energy),tmpAngle_i]
     ENDIF ELSE BEGIN
        tmpEn    = X[*,tmpAngle_i]
        tmpAngle = Y[*,tmpAngle_i]
        tmpData  = Z[*,tmpAngle_i]
     ENDELSE

     ;; tmpMax    = GET_N_MAXIMA_IN_ARRAY(tmpData, $
     ;;                                N=3, $
     ;;                                OUT_I=tmpMax_ii)

     ;; energyMin     = MIN(ABS(tmpEn[tmpMax_ii]-peak_en[prevAngle_i]), $
     ;;                     energy_iii)
     ;; peak_en[tmpAngle_i] = tmpEn[tmpMax_ii[energy_iii]]
     ;; peak_angle[tmpAngle_i] = tmpAngle[tmpMax_ii[energy_iii]]

     tmpMax                   = MAX(tmpData,tmpMax_ii)

     ;; energyMin     = MIN(ABS(tmpEn[tmpMax_ii]-peak_en[prevAngle_i]), $
     ;;                     energy_iii)
     peak_en[tmpAngle_i]      = tmpEn[tmpMax_ii]
     peak_flux[tmpAngle_i]    = tmpMax
     peak_angle[tmpAngle_i]   = tmpAngle[tmpMax_ii]

  ENDFOR

  posAngle           = ABS(Y[energy_i,*]) LE 90
  posAngle_i         = WHERE(posAngle,nPos, $
                             COMPLEMENT=negAngle_i,NCOMPLEMENT=nNeg)

  factor             = MAKE_ARRAY(nAngles,VALUE=0.0)

  ;; minRatio           = 0.5

  CASE 1 OF
     KEYWORD_SET(logScale): BEGIN
        minRatio           = ALOG10(MIN(peak_En/peak_En[0]))
     END
     ELSE: BEGIN
        minRatio           = MIN(peak_En/peak_En[0])
     END
  ENDCASE

  ;; ;;Original
  ;; factor[posAngle_i] = 1. - (      ABS(angles[posAngle_i]) / 90. * (1 - minRatio) )
  ;; factor[negAngle_i] = 1. -        ABS(angles[negAngle_i]))/180. * (1 - minRatio) 

  ;; ;;Another try
  ;; factor[posAngle_i] = 1. - (      ABS(angles[posAngle_i]) / 90. * (1 - minRatio) )
  ;; factor[negAngle_i] = 1. - ( (180-ABS(angles[negAngle_i]))/ 90. * (1 - minRatio) ) * 0.5

  ;;And another try
  ;; factor[posAngle_i] = minRatio + (1-minRatio) * ( (  90 - ABS(angles[posAngle_i]) ) / 90.)
  ;; factor[negAngle_i] = minRatio + (1-minRatio) * ( ABS( 90 - ABS(angles[negAngle_i])) / 90.) $
  ;;                      * 0.5
  ;;And with COS
  ;; factor[posAngle_i] = minRatio + (1-minRatio) * COS(angles[posAngle_i]/180*!PI)
  ;; factor[negAngle_i] = minRatio + (1-minRatio) * COS(180-ABS(angles[negAngle_i]))/180*!PI)
  
  reduceNegFac        = KEYWORD_SET(reduceNegFac) ? reduceNegFac : 0.3

  CASE 1 OF
     KEYWORD_SET(logScale): BEGIN
        factor        = COS(angles/90*!PI) * ALOG10(1.0D / ( 1.0D + reduceNegFac)) * ALOG10(1-minRatio)
     END
     ELSE: BEGIN
        factor        = COS(angles/90*!PI) * (1.0D / ( 1.0D + reduceNegFac)) * (1-minRatio)
     END
  ENDCASE


  posAngle_i          = WHERE(factor GE 0 AND posAngle,nPos, $
                             COMPLEMENT=negAngle_i,NCOMPLEMENT=nNeg)

  ;; posFactor_i         = WHERE(factor GE 0,nPos, $
  ;;                            COMPLEMENT=negFactor_i,NCOMPLEMENT=nNeg)

  factor[negAngle_i] *= reduceNegFac
  factor              = factor + (minRatio - MIN(factor))
  ;; factor[negAngle_i] *= 0.5
  ;; factor[negAngle_i] = minRatio + (1-minRatio) * COS(180-ABS(angles[negAngle_i]))/180*!PI)


  IF KEYWORD_SET(plot_bulke_model) THEN BEGIN

     IF ~ISA(window) THEN window = WINDOW(DIMENSIONS=[1200,800])

     those = PLOT(peak_angle,factor, $
                  NAME='Model function', $
                  SYMBOL='*', $
                  SYM_SIZE=2.0, $
                  FONT_SIZE=18, $
                  LINESTYLE='', $
                  COLOR='red', $
                  ;; /OVERPLOT, $
                  CURRENT=window)

     IF KEYWORD_SET(save_plots) THEN BEGIN
        PRINT,'Saving bFunc plot: ' + bFuncSPName
        window.Save,plotDir+bFuncSPName

        window.Close
        window = !NULL
     ENDIF ELSE BEGIN
        STOP
     ENDELSE
  ENDIF

  IF KEYWORD_SET(plot_bulke_factor) THEN BEGIN

     IF ~ISA(window) THEN window = WINDOW(DIMENSIONS=[1200,800])

     sort_i = SORT(peak_angle)

     xTickValues = [-180,-90,0,90,180]
     xTickName   = STRING(FORMAT='(I0)',xTickValues)

     those = PLOT(peak_angle[sort_i],peak_en[sort_i]/peak_en[0], $
                  NAME='Model function', $
                  XTITLE='$\theta$ (deg)', $
                  YTITLE='b($\theta$)!C!C(E$_b$($\theta$)/E$_{b,||}$)', $
                  YRANGE=[0.1,1.02], $
                  XTICKVALUES=xTickValues, $
                  XTICKNAME=xTickName, $
                  SYMBOL='*', $
                  SYM_SIZE=2.0, $
                  FONT_SIZE=18, $
                  LINESTYLE='-', $
                  ;; COLOR='red', $
                  ;; /OVERPLOT, $
                  CURRENT=window)

     IF KEYWORD_SET(save_plots) THEN BEGIN
        PRINT,'Saving bFunc plot: ' + bFuncSPName
        window.Save,plotDir+bFuncSPName

        window.Close
        window = !NULL
     ENDIF ELSE BEGIN
        STOP
     ENDELSE
  ENDIF

  IF KEYWORD_SET(polarPlot_bulke_factor) THEN BEGIN

     IF ~ISA(window) THEN window = WINDOW(DIMENSIONS=[1200,800])

     those = POLARPLOT(ALOG10(peak_en),peak_angle*!PI/180., $
                  NAME='Model function', $
                  SYMBOL='*', $
                  SYM_SIZE=2.0, $
                  FONT_SIZE=18, $
                  LINESTYLE='', $
                  COLOR='black', $
                  ;; /OVERPLOT, $
                  CURRENT=window)

     IF KEYWORD_SET(save_plots) THEN BEGIN
        PRINT,'Saving bFunc polarPlot: ' + bFuncPolarSPName
        window.Save,plotDir+bFuncPolarSPName

        window.Close
        window = !NULL
     ENDIF ELSE BEGIN
        STOP
     ENDELSE
  ENDIF

  IF KEYWORD_SET(plot_comparison) THEN BEGIN

     IF ~ISA(window) THEN window = WINDOW(DIMENSIONS=[1200,800])

     sort_i = SORT(peak_angle)

     that = PLOT(peak_angle,peak_en/peak_en[0], $
                 NAME='Data', $
                 ;; TITLE='Variation in E$_{bulk}$ with pitch angle', $
                 YRANGE=[0.19,1.02], $
                 XTITLE='$\theta$ (deg)', $
                 YTITLE='b($\theta$)!C!C(E$_b$($\theta$)/E$_{b,||}$)', $
                 ;; XTITLE='Pitch angle (deg)', $
                 ;; YTITLE='E$_{peak}$/E$_{peak,field-aligned}$', $
                 SYMBOL='*', $
                 SYM_SIZE=2.0, $
                 FONT_SIZE=18, $
                 LINESTYLE='', $
                 CURRENT=window)

     those = PLOT(peak_angle[sort_i],factor[sort_i], $
                  NAME='Model function', $
                  ;; SYMBOL='*', $
                  ;; SYM_SIZE=2.0, $
                  FONT_SIZE=18, $
                  ;; LINESTYLE='', $
                  ;; LINESTYLE='', $
                  COLOR='red', $
                  /OVERPLOT, $
                  CURRENT=window)

     legend = LEGEND( $
              ;; TARGET=[that,those], $
              TARGET=[that,those], $
              POSITION=[0.35,0.8], $
              /NORMAL)


  ENDIF

  IF KEYWORD_SET(plot_flux_peaks) THEN BEGIN

     IF ~ISA(window) THEN window = WINDOW(DIMENSIONS=[1200,800])

     sort_i       = SORT(peak_angle)
     yLog         = ALOG10(MIN(peak_flux)/MAX(peak_flux)) LT 2 ? 1 : 0
     ;; pkFlux_title = STRING(FORMAT='(A0,A0)', $
     ;;                          "Variation in peak diff. eFlux with pitch angle", $
     ;;                          (KEYWORD_SET(min_energy) ? $
     ;;                           STRING(FORMAT='("!C(Min energy: ",G0.2," eV)")',min_energy) : $
     ;;                           ''))

     ;; that = PLOT(peak_angle[sort_i],peak_flux[sort_i]/peak_flux[0], $
     ;;             NAME='peak_flux', $
     ;;             TITLE=pkFlux_title, $
     ;;             XTITLE='$\theta$ (deg)', $
     ;;             YTITLE='g($\theta$)!C!C(Eflux$_{peak}$/Eflux$_{peak,||}$)', $
     ;;             YLOG=yLog, $
     ;;             SYMBOL='*', $
     ;;             SYM_SIZE=2.0, $
     ;;             FONT_SIZE=18, $
     ;;             LINESTYLE='-', $
     ;;             CURRENT=window)

     xTickValues = [-180,-90,0,90,180]
     xTickName   = STRING(FORMAT='(I0)',xTickValues)

     that = PLOT(peak_angle[sort_i],peak_flux[sort_i]/peak_flux[0], $
                 NAME='peak_flux', $
                 TITLE=pkFlux_title, $
                 XTITLE='$\theta$ (deg)', $
                 ;; YTITLE='g($\theta$)!C!C(Eflux$_{peak}$/Eflux$_{peak,||}$)', $
                 YTITLE='g($\theta$)!C!CMAX($dJ_E/dE)$/MAX($dJ_{E,||}/dE$)', $
                 YLOG=yLog, $
                 SYMBOL='*', $
                 XTICKVALUES=xTickValues, $
                 XTICKNAME=xTickName, $
                 SYM_SIZE=2.0, $
                 FONT_SIZE=18, $
                 LINESTYLE='-', $
                 CURRENT=window)

     IF KEYWORD_SET(save_plots) THEN BEGIN
        PRINT,'Saving gFunc plot: ' + gFuncSPName
        window.Save,plotDir+gFuncSPName

        window.Close
        window = !NULL
     ENDIF ELSE BEGIN
        STOP
     ENDELSE
  ENDIF

  ;; print = 1
  IF KEYWORD_SET(print) THEN BEGIN
     diff = (peak_en/peak_en[0])-factor

     PRINT,FORMAT='(A0,T12,A0,T24,A0,T36,A0)', $
           "Angle", $
           "Actual", $
           "Model", $
           "(Act-Mod)" 
     FOR k=0,nAngles-1 DO PRINT,FORMAT='(F0.2,T12,F0.2,T24,F0.2,T36,F0.2)', $
                                peak_angle[k], $
                                peak_en[k]/peak_en[0], $
                                factor[k], $
                                diff[k]
  ENDIF

  RETURN,factor

END
