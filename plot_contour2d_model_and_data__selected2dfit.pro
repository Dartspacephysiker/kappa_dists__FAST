PRO PLOT_CONTOUR2D_MODEL_AND_DATA__SELECTED2DFIT,fit2DStruct,dataSDT, $
   FOR_HORSESHOE_FIT=for_horseshoe_fit, $
   LIMITS=limits, $
   ADD_FITPARAMS_TEXT=add_fitParams_text, $
   FITSTRING=fitString

  COMPILE_OPT idl2

  @common__kappa_flux2d__horseshoe__eanisotropy.pro
  @common__kappa_fit2d_structs.pro

  ;; IF N_ELEMENTS(KF2D__SDTData_opt) GT 0 THEN BEGIN
  ;;    angle = KF2D__SDTData_opt.fit2D_dens_aRange
  ;; ENDIF ELSE BEGIN
  ;;    angle = !NULL
  ;; ENDELSE

  ;;For boundary plots
  ;; boundaryColor = '0000FF'x ;'red'
  boundaryColor = 250 ;'red'
  boundarySym   = 1 ;plus sign
  bFuncSym      = 2 ;asterisk
  
  symSize       = 0.8

  CONTOUR2D,fit2DStruct.bestFitStr, $
            ;; ANGLE=angle, $
            /POLAR, $
            /FILL, $
            /MSEC, $
            LIMITS=limits, $
            /LABEL
  CONTOUR2D,dataSDT, $
            ;; ANGLE=angle, $
            /POLAR, $
            /OVERPLOT, $
            /MSEC, $
            LIMITS=limits, $
            /LABEL

  IF KEYWORD_SET(KF2D__plot_opt.fit2D__add_boundaries) THEN BEGIN
     CASE 1 OF
        KEYWORD_SET(KF2D__curveFit_opt.fit2D_fit_above_minE): BEGIN
           ;;plot min peak energy
           plotme      = MAKE_ARRAY(2,N_ELEMENTS(K_EA__angle_i))
           plotme[0,*] = COS(K_EA__angles*!PI/180.)*ALOG10(KF2D__curveFit_opt.min_peak_energy) ;*K_EA__bFunc)
           plotme[1,*] = SIN(K_EA__angles*!PI/180.)*ALOG10(KF2D__curveFit_opt.min_peak_energy) ;*K_EA__bFunc)
           PLOTS,plotme,/DATA,PSYM=boundarySym,COLOR=boundaryColor, $
                 SYMSIZE=symSize
        END
        KEYWORD_SET(KF2D__curveFit_opt.fit2D_just_eRange_peak): BEGIN
           plotme1      = MAKE_ARRAY(2,N_ELEMENTS(K_EA__angle_i))
           plotme1[0,*] = COS(K_EA__angles*!PI/180.)*ALOG10(fit2DStruct.eRange_peak[0]) ;*K_EA__bFunc)
           plotme1[1,*] = SIN(K_EA__angles*!PI/180.)*ALOG10(fit2DStruct.eRange_peak[0]) ;*K_EA__bFunc)
           plotme2      = MAKE_ARRAY(2,N_ELEMENTS(K_EA__angle_i))
           plotme2[0,*] = COS(K_EA__angles*!PI/180.)*ALOG10(fit2DStruct.eRange_peak[1]) ;*K_EA__bFunc)
           plotme2[1,*] = SIN(K_EA__angles*!PI/180.)*ALOG10(fit2DStruct.eRange_peak[1]) ;*K_EA__bFunc)
           PLOTS,plotme1,/DATA,PSYM=boundarySym,COLOR=boundaryColor, $
                 SYMSIZE=symSize
           PLOTS,plotme2,/DATA,PSYM=boundarySym,COLOR=boundaryColor, $
                 SYMSIZE=symSize
        END
     ENDCASE
  ENDIF

  ;;    CONTOUR2D,curDataStr,/POLAR
     junk        = MAX(fit2DStruct.bestFitStr.data[*,fit2DStruct.fitAngle_i],edgery_i)
     peak_energy = (fit2DStruct.bestFitStr.energy[edgery_i,fit2DStruct.fitAngle_i])[0]
     plotme      = MAKE_ARRAY(2,N_ELEMENTS(K_EA__angle_i))
     plotme[0,*] = COS(K_EA__angles*!PI/180.)*ALOG10(peak_energy*K_EA__bFunc)
     plotme[1,*] = SIN(K_EA__angles*!PI/180.)*ALOG10(peak_energy*K_EA__bFunc)
     PLOTS,plotme,/DATA,PSYM=bFuncSym, $
           SYMSIZE=symSize

  ;;    STOP

  IF KEYWORD_SET(add_fitParams_text) THEN BEGIN
     
     tmpA        = KEYWORD_SET(for_horseshoe_fit) ? fit2DStruct.bestFit1DParams : $
                   fit2DStruct.bestFit1DParams.A
     fitTitle    = ["Bulk energy  (eV)","Plasma temp. (eV)", $
                    "Kappa","Density (cm!U-3!N)", $
                    CGGREEK('chi',/PS)+'!11!U2!N!Dred!N']
                    ;; "Angle (deg)"]
     fitInfoStr  = [STRING(FORMAT='(F-15.2)',tmpA[0]), $
                    STRING(FORMAT='(F-15.2)',tmpA[1]), $
                    STRING(FORMAT='(F-7.3)',tmpA[2]), $
                    STRING(FORMAT='(F-8.4)',fit2DStruct.bestDens), $
                    STRING(FORMAT='(G-9.4)',fit2DStruct.bestChi2)]

     theString   = STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[0],fitInfoStr[0]) + '!C' + $
                   STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[1],fitInfoStr[1]) + '!C' + $
                   STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[2],fitInfoStr[2]) + '!C' + $
                   STRING(FORMAT='(A0,T24,": ",A0)',fitTitle[3],fitInfoStr[3]) + '!C' + $
                   STRING(FORMAT='(A0,T36,": ",A0)',fitTitle[4],fitInfoStr[4]) + '!C'
     ;; STRING(FORMAT='("Fit success",T20,": ",A0)',(fit2DStruct.bestFit1DParams.fitStatus EQ 0 ? 'Y' : 'N')), $

     IF N_ELEMENTS(fitString) GT 0 THEN BEGIN
        XYOUTS,0.12,0.27,fitString, $
               CHARSIZE=1.0, $
               FONT=0, $
               /NORMAL, $
               /NOCLIP
     ENDIF

     XYOUTS,0.12,0.25,'!11' + theString + '!X', $ ;+ $
     ;; XYOUTS,0.12,0.28,theString, $ ;+ $
            CHARSIZE=1.0, $
            FONT=0, $
            /NORMAL, $
            /NOCLIP
     ;; FONT_NAME='Courier', $
     ;; /NORMAL)
  ENDIF

END