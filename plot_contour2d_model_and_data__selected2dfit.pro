PRO PLOT_CONTOUR2D_MODEL_AND_DATA__SELECTED2DFIT,fit2DStruct,dataSDT, $
   ONLY_DATA=only_data, $ 
   FOR_HORSESHOE_FIT=for_horseshoe_fit, $
   LIMITS=limits, $
   ADD_FITPARAMS_TEXT=add_fitParams_text, $
   FITSTRING=fitString

  COMPILE_OPT IDL2,STRICTARRSUBS

  @common__kappa_flux2d__horseshoe__eanisotropy.pro
  @common__kappa_fit2d_structs.pro

  
  IF N_ELEMENTS(fitString) EQ 0 THEN BEGIN
     is_kappa = 0
  ENDIF ELSE BEGIN
     is_kappa = STRMATCH(STRUPCASE(fitString),'KAPPA')
  ENDELSE
     

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

  CASE 1 OF
     KEYWORD_SET(only_data): BEGIN
        CONTOUR2D,dataSDT, $
                  ;; ANGLE=angle, $
                  /POLAR, $
                  /FILL, $
                  ;; /OVERPLOT, $
                  /MSEC, $
                  LIMITS=limits, $
                  /LABEL
     END
     ELSE: BEGIN
        CONTOUR2D,fit2DStruct.SDT, $
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

           ;; Do the angles
           CASE 1 OF
              KEYWORD_SET(KF2D__curveFit_opt.fit2D_fit_above_minE): BEGIN

                 ;; Need this later
                 eBounds = [fit2DStruct.extra_info.eRange_peak[0], $
                            MAX(fit2DStruct.SDT.energy)]

                 ;;plot min peak energy

                 ;;This chunk does ALL angles
                 ;; plotme      = MAKE_ARRAY(2,N_ELEMENTS(K_EA__angle_i))
                 ;; plotme[0,*] = COS(K_EA__angles*!PI/180.)*ALOG10(KF2D__curveFit_opt.min_peak_energy) ;*K_EA__bFunc)
                 ;; plotme[1,*] = SIN(K_EA__angles*!PI/180.)*ALOG10(KF2D__curveFit_opt.min_peak_energy) ;*K_EA__bFunc)

                 ;; This chunk does only angles that we fit
                 plotme      = MAKE_ARRAY(2,N_ELEMENTS(K_EA__fitAngle_i))
                 plotme[0,*] = COS(K_EA__fitAngles*!PI/180.)*ALOG10(KF2D__curveFit_opt.min_peak_energy) ;*K_EA__bFunc)
                 plotme[1,*] = SIN(K_EA__fitAngles*!PI/180.)*ALOG10(KF2D__curveFit_opt.min_peak_energy) ;*K_EA__bFunc)

                 PLOTS,plotme,/DATA,PSYM=boundarySym,COLOR=boundaryColor, $
                       SYMSIZE=symSize
              END
              KEYWORD_SET(KF2D__curveFit_opt.fit2D_just_eRange_peak): BEGIN

                 eBounds  = fit2DStruct.extra_info.eRange_peak

                 ;; This chunk does ALL angles
                 plotme1      = MAKE_ARRAY(2,N_ELEMENTS(K_EA__angle_i))
                 plotme1[0,*] = COS(K_EA__angles*!PI/180.)*ALOG10(fit2DStruct.extra_info.eRange_peak[0]) ;*K_EA__bFunc)
                 plotme1[1,*] = SIN(K_EA__angles*!PI/180.)*ALOG10(fit2DStruct.extra_info.eRange_peak[0]) ;*K_EA__bFunc)
                 plotme2      = MAKE_ARRAY(2,N_ELEMENTS(K_EA__angle_i))
                 plotme2[0,*] = COS(K_EA__angles*!PI/180.)*ALOG10(fit2DStruct.extra_info.eRange_peak[1]) ;*K_EA__bFunc)
                 plotme2[1,*] = SIN(K_EA__angles*!PI/180.)*ALOG10(fit2DStruct.extra_info.eRange_peak[1]) ;*K_EA__bFunc)

                 ;; This chunk does only angles that we fit
                 plotme1      = MAKE_ARRAY(2,N_ELEMENTS(K_EA__fitAngle_i))
                 plotme1[0,*] = COS(K_EA__fitAngles*!PI/180.)*ALOG10(fit2DStruct.extra_info.eRange_peak[0]) ;*K_EA__bFunc)
                 plotme1[1,*] = SIN(K_EA__fitAngles*!PI/180.)*ALOG10(fit2DStruct.extra_info.eRange_peak[0]) ;*K_EA__bFunc)
                 plotme2      = MAKE_ARRAY(2,N_ELEMENTS(K_EA__fitAngle_i))
                 plotme2[0,*] = COS(K_EA__fitAngles*!PI/180.)*ALOG10(fit2DStruct.extra_info.eRange_peak[1]) ;*K_EA__bFunc)
                 plotme2[1,*] = SIN(K_EA__fitAngles*!PI/180.)*ALOG10(fit2DStruct.extra_info.eRange_peak[1]) ;*K_EA__bFunc)

                 PLOTS,plotme1,/DATA,PSYM=boundarySym,COLOR=boundaryColor, $
                       SYMSIZE=symSize
                 PLOTS,plotme2,/DATA,PSYM=boundarySym,COLOR=boundaryColor, $
                       SYMSIZE=symSize
              END
           ENDCASE

           ;; Now do the energies at the boundary
           minAngle  = MIN(K_EA__fitAngles)
           maxAngle  = MAX(K_EA__fitAngles)
           minCosSin = [COS(minAngle*!PI/180.),SIN(minAngle*!PI/180.)]
           maxCosSin = [COS(maxAngle*!PI/180.),SIN(maxAngle*!PI/180.)]

           energy_i = WHERE(fit2DStruct.SDT.energy[*,0] GE eBounds[0] AND $
                            fit2DStruct.SDT.energy[*,0] LE eBounds[1],nEnergy)
           energies = fit2DStruct.SDT.energy[energy_i,0]
           plotme1 = MAKE_ARRAY(2,nEnergy)
           plotme2 = MAKE_ARRAY(2,nEnergy)

           plotme1[0,*] = minCosSin[0]*ALOG10(energies) ;*K_EA__bFunc)
           plotme1[1,*] = minCosSin[1]*ALOG10(energies) ;*K_EA__bFunc)
           plotme2[0,*] = maxCosSin[0]*ALOG10(energies) ;*K_EA__bFunc)
           plotme2[1,*] = maxCosSin[1]*ALOG10(energies) ;*K_EA__bFunc)

           PLOTS,plotme1,/DATA,PSYM=boundarySym,COLOR=boundaryColor, $
                 SYMSIZE=symSize
           PLOTS,plotme2,/DATA,PSYM=boundarySym,COLOR=boundaryColor, $
                 SYMSIZE=symSize

        ENDIF

        ;;    CONTOUR2D,curDataStr,/POLAR
        junk        = MAX(fit2DStruct.SDT.data[*,fit2DStruct.extra_info.fitAngle_i],edgery_i)
        peak_energy = (fit2DStruct.SDT.energy[edgery_i,fit2DStruct.extra_info.fitAngle_i])[0]

        ;; This chunk does ALL angles
        ;; plotme      = MAKE_ARRAY(2,N_ELEMENTS(K_EA__angle_i))
        ;; plotme[0,*] = COS(K_EA__angles*!PI/180.)*ALOG10(peak_energy*K_EA__bFunc)
        ;; plotme[1,*] = SIN(K_EA__angles*!PI/180.)*ALOG10(peak_energy*K_EA__bFunc)

        ;; This chunk does only angles that we fit
        plotme      = MAKE_ARRAY(2,N_ELEMENTS(K_EA__fitAngle_i))
        plotme[0,*] = COS(K_EA__fitAngles*!PI/180.)* $
                      ALOG10(peak_energy*K_EA__bFunc[K_EA__fitAngle_i])
        plotme[1,*] = SIN(K_EA__fitAngles*!PI/180.)* $
                      ALOG10(peak_energy*K_EA__bFunc[K_EA__fitAngle_i])
        PLOTS,plotme,/DATA,PSYM=bFuncSym, $
              SYMSIZE=symSize

        ;;    STOP

        IF KEYWORD_SET(add_fitParams_text) THEN BEGIN
           
           tmpA        = KEYWORD_SET(for_horseshoe_fit) ? fit2DStruct.fit1D.A : $
                         fit2DStruct.fitParams
           fitTitle    = ["Bulk energy  (eV)", $
                          (is_kappa ? $
                           "Plasma temp. (core) (eV)" : $
                           "Plasma temp. (eV)"), $
                          "Kappa", $
                          "Density (cm!U-3!N)", $
                          CGGREEK('chi',/PS)+'!11!U2!N!Dred!N']
           ;; "Angle (deg)"]
           fitInfoStr  = [STRING(FORMAT='(F-15.2)',tmpA[0]), $
                          (is_kappa ? $
                           STRING(FORMAT='(F-10.2,T11,"(",F-7.2,")")',tmpA[1],tmpA[1]*(tmpA[2]-1.5D)/tmpA[2]) : $
                           STRING(FORMAT='(F-15.2)',tmpA[1])), $
                          STRING(FORMAT='(F-7.3)',tmpA[2]), $
                          STRING(FORMAT='(F-8.4)',fit2DStruct.fitMoms.scDens), $
                          ;; STRING(FORMAT='(F-8.4)',tmpA[3]), $
                          STRING(FORMAT='(G-9.4)',fit2DStruct.chi2/(fit2DStruct.dof-fit2DStruct.nPegged))]

           theString   = STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[0],fitInfoStr[0]) + '!C' + $
                         STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[1],fitInfoStr[1]) + '!C' + $
                         STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[2],fitInfoStr[2]) + '!C' + $
                         STRING(FORMAT='(A0,T24,": ",A0)',fitTitle[3],fitInfoStr[3]) + '!C' + $
                         STRING(FORMAT='(A0,T36,": ",A0)',fitTitle[4],fitInfoStr[4]) + '!C'
           ;; STRING(FORMAT='("Fit success",T20,": ",A0)',(fit2DStruct.bestFit1DParams.fitStatus EQ 0 ? 'Y' : 'N')), $

           IF N_ELEMENTS(fitString) GT 0 THEN BEGIN
              XYOUTS,0.19,0.27,fitString, $
                     CHARSIZE=1.0, $
                     FONT=0, $
                     /NORMAL, $
                     /NOCLIP
           ENDIF

           XYOUTS,0.19,0.25,'!11' + theString + '!X', $ ;+ $
                  ;; XYOUTS,0.12,0.28,theString, $ ;+ $
                  CHARSIZE=1.0, $
                  FONT=0, $
                  /NORMAL, $
                  /NOCLIP
           ;; FONT_NAME='Courier', $
           ;; /NORMAL)
        ENDIF


     END
  ENDCASE

END