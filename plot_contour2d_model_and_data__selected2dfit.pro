PRO PLOT_CONTOUR2D_MODEL_AND_DATA__SELECTED2DFIT,fit2DStruct,dataSDT, $
   LIMITS=limits, $
   ADD_FITPARAMS_TEXT=add_fitParams_text, $
   FITSTRING=fitString

  COMPILE_OPT idl2

  CONTOUR2D,fit2DStruct.bestFitStr, $
            /POLAR, $
            /FILL, $
            /MSEC, $
            LIMITS=limits, $
            /LABEL
  CONTOUR2D,dataSDT, $
            /POLAR, $
            /OVERPLOT, $
            /MSEC, $
            LIMITS=limits, $
            /LABEL
  IF KEYWORD_SET(add_fitParams_text) THEN BEGIN
     
     tmpA        = fit2DStruct.bestFit1DParams.A
     fitTitle    = ["Bulk energy  (eV)","Plasma temp. (eV)", $
                    "Kappa","Density (cm^-3)", $
                    CGGREEK('chi',/PS)+'!11^2']
                    ;; "Angle (deg)"]
     fitInfoStr  = [STRING(FORMAT='(F-15.2)',tmpA[0]), $
                    STRING(FORMAT='(F-15.2)',tmpA[1]), $
                    STRING(FORMAT='(F-7.3)',tmpA[2]), $
                    STRING(FORMAT='(F-8.4)',fit2DStruct.bestDens), $
                    STRING(FORMAT='(G-9.4)',fit2DStruct.bestChi2)]

     theString   = STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[0],fitInfoStr[0]) + '!C' + $
                   STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[1],fitInfoStr[1]) + '!C' + $
                   STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[2],fitInfoStr[2]) + '!C' + $
                   STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[3],fitInfoStr[3]) + '!C' + $
                   STRING(FORMAT='(A0,T27,": ",A0)',fitTitle[4],fitInfoStr[4]) + '!C'
     ;; STRING(FORMAT='("Fit success",T20,": ",A0)',(fit2DStruct.bestFit1DParams.fitStatus EQ 0 ? 'Y' : 'N')), $

     IF N_ELEMENTS(fitString) GT 0 THEN BEGIN
        XYOUTS,0.13,0.26,fitString, $
               CHARSIZE=1.0, $
               FONT=0, $
               /NORMAL, $
               /NOCLIP
     ENDIF

     XYOUTS,0.13,0.24,'!11' + theString + '!X', $ ;+ $
     ;; XYOUTS,0.12,0.28,theString, $ ;+ $
            CHARSIZE=1.0, $
            FONT=0, $
            /NORMAL, $
            /NOCLIP
     ;; FONT_NAME='Courier', $
     ;; /NORMAL)
  ENDIF

END