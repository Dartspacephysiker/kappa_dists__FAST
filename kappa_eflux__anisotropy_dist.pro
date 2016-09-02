;;09/02/16
FUNCTION KAPPA_EFLUX__ANISOTROPY_DIST,X,Y,Z, $
                                      bestAngle_i, $
                                      BULK_ENERGY=bulk_energy, $
                                      MIN_ENERGY=min_energy, $
                                      REDUCENEGFAC=reduceNegFac, $
                                      LOGSCALE_REDUCENEGFAC=logScale, $
                                      OUT_PEAK_ENERGIES=peakEn__en, $
                                      OUT_ANGLES=peakEn__angle, $
                                      PLOT_FACTOR=plot_factor, $
                                      PLOT_COMPARISON=plot_comparison, $
                                      PRINT=print

  COMPILE_OPT IDL2

  angles  = Y[0,*]
  nAngles = N_ELEMENTS(angles)

  peakEn__en    = MAKE_ARRAY(nAngles,/DOUBLE)
  peakEn__angle = MAKE_ARRAY(nAngles,/DOUBLE)

  ;;Initialize
  allEn_i       = INDGEN(N_ELEMENTS(X[*,0]))
  ;; bestAngle_i   = FIX(fit2dstruct.bestAngle_i)
  energyMin     = MIN(ABS(X[*,bestAngle_i]-bulk_energy),energy_i)

  peakEn__en[bestAngle_i] = X[energy_i,bestAngle_i]

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

     ;; energyMin     = MIN(ABS(tmpEn[tmpMax_ii]-peakEn__en[prevAngle_i]), $
     ;;                     energy_iii)
     ;; peakEn__en[tmpAngle_i] = tmpEn[tmpMax_ii[energy_iii]]
     ;; peakEn__angle[tmpAngle_i] = tmpAngle[tmpMax_ii[energy_iii]]

     tmpMax    = MAX(tmpData,tmpMax_ii)

     ;; energyMin     = MIN(ABS(tmpEn[tmpMax_ii]-peakEn__en[prevAngle_i]), $
     ;;                     energy_iii)
     peakEn__en[tmpAngle_i] = tmpEn[tmpMax_ii]
     peakEn__angle[tmpAngle_i] = tmpAngle[tmpMax_ii]

  ENDFOR

  posAngle           = ABS(Y[energy_i,*]) LE 90
  posAngle_i         = WHERE(posAngle,nPos, $
                             COMPLEMENT=negAngle_i,NCOMPLEMENT=nNeg)

  factor             = MAKE_ARRAY(nAngles,VALUE=0.0)

  ;; minRatio           = 0.5

  CASE 1 OF
     KEYWORD_SET(logScale): BEGIN
        minRatio           = ALOG10(MIN(peakEn__En/peakEn__En[0]))
     END
     ELSE: BEGIN
        minRatio           = MIN(peakEn__En/peakEn__En[0])
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


  IF KEYWORD_SET(plot_factor) THEN BEGIN
     those = PLOT(peakEn__angle,factor, $
                  NAME='Model function', $
                  SYMBOL='*', $
                  SYM_SIZE=2.0, $
                  FONT_SIZE=18, $
                  LINESTYLE='', $
                  COLOR='red', $
                  ;; /OVERPLOT, $
                  CURRENT=window)
  ENDIF

  IF KEYWORD_SET(plot_comparison) THEN BEGIN
     that = PLOT(peakEn__angle,peakEn__en/peakEn__en[0], $
                 NAME='Data', $
                 TITLE='Variation in E$_{bulk}$ with pitch angle', $
                 YRANGE=[0.19,1.02], $
                 XTITLE='Pitch angle (deg)', $
                 YTITLE='E$_{peak}$/E$_{peak,field-aligned}$', $
                 SYMBOL='*', $
                 SYM_SIZE=2.0, $
                 FONT_SIZE=18, $
                 LINESTYLE='', $
                 CURRENT=window)

     those = PLOT(peakEn__angle,factor, $
                  NAME='Model function', $
                  SYMBOL='*', $
                  SYM_SIZE=2.0, $
                  FONT_SIZE=18, $
                  LINESTYLE='', $
                  COLOR='red', $
                  /OVERPLOT, $
                  CURRENT=window)

     legend = LEGEND( $
              ;; TARGET=[that,those], $
              TARGET=[that,those], $
              POSITION=[0.35,0.8], $
              /NORMAL)


  ENDIF

  ;; print = 1
  IF KEYWORD_SET(print) THEN BEGIN
     diff = (peakEn__en/peakEn__en[0])-factor

     PRINT,FORMAT='(A0,T12,A0,T24,A0,T36,A0)', $
           "Angle", $
           "Actual", $
           "Model", $
           "(Act-Mod)" 
     FOR k=0,nAngles-1 DO PRINT,FORMAT='(F0.2,T12,F0.2,T24,F0.2,T36,F0.2)', $
                                peakEn__angle[k], $
                                peakEn__en[k]/peakEn__en[0], $
                                factor[k], $
                                diff[k]
  ENDIF

  RETURN,factor

END
