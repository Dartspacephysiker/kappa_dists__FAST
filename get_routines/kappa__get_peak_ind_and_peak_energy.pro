PRO KAPPA__GET_PEAK_IND_AND_PEAK_ENERGY, $
   Xorig,Yorig,peak_ind,peak_energy, $
   BULK_OFFSET=bulk_offset, $
   CHECK_FOR_HIGHER_FLUX_PEAKS=check_for_higher_flux_peaks, $
   MIN_PEAK_ENERGY=min_peak_energy, $
   MAX_PEAK_ENERGY=max_peak_energy, $
   PEAK_ENERGY__START_AT_HIGHE=peak_energy__start_at_highE, $
   PHI__USE_ENERGY_BEFORE_PEAK=phi__use_energy_before_peak, $
   CONTINUE_IF_NOMATCH=its_OK__everyone_has_feelings, $
   ONECOUNT_STR=oneCurve

  COMPILE_OPT IDL2,STRICTARRSUBS

  b_offset              = KEYWORD_SET(bulk_offset) ? bulk_offset : 0 ;bulk offset

  minE                  = KEYWORD_SET(min_peak_energy) ? min_peak_energy : MIN(Xorig)
  maxE                  = KEYWORD_SET(max_peak_energy) ? max_peak_energy : MAX(Xorig)
  IF KEYWORD_SET(oneCurve) THEN BEGIN
     IF N_ELEMENTS(oneCurve.y) EQ N_ELEMENTS(Yorig) AND ARRAY_EQUAL(oneCurve.x,Xorig) THEN BEGIN
        minFlux         = oneCurve.y
     ENDIF ELSE BEGIN
        PRINT,"rpbolems (as you see)"
        STOP
     ENDELSE
  ENDIF ELSE BEGIN
     minFlux            = yOrig
  ENDELSE
  
  inds                  = WHERE((Xorig GE minE) AND (Xorig LE maxE) AND (Yorig-minFlux) GE 0,nInds)
  whichWy               = FIX(ABS((Xorig[inds[0]]-Xorig[inds[-1]]))/(Xorig[inds[0]]-Xorig[inds[-1]]))

  CASE 1 OF
     KEYWORD_SET(check_for_higher_flux_peaks): BEGIN

        ;;Figure out where most energetic maximum is
        max_ys             = GET_N_MAXIMA_IN_ARRAY(Yorig,N=3,OUT_I=maxima_i)
        peak_y             = MAX(max_ys,max_y_ii)
        peak_ind           = maxima_i[max_y_ii]
        peak_energy        = Xorig[peak_ind]
        peak_tol_percent   = .25
        
        FOR i=0,N_ELEMENTS(maxima_i)-1 DO BEGIN
           testMax_X       = Xorig[maxima_i[i]]
           testMax         = max_ys[i]
           curMax_i        = maxima_i[i]
           PRINT,'testval:',STRCOMPRESS((ABS(testMax-peak_y)/peak_y),/REMOVE_ALL)
           IF testMax_X GT Xorig[peak_ind] AND $
              (ABS(testMax-peak_y)/peak_y) LT peak_tol_percent THEN BEGIN
              peak_ind     = curMax_i
              peak_energy  = testMax_X
           ENDIF
        ENDFOR
        PRINT,peak_ind

     END
     KEYWORD_SET(peak_energy__start_at_highE): BEGIN
        ;; inds            = WHERE((Xorig GE minE) AND (Xorig LE maxE) AND (Yorig-minFlux) GE 0,nInds)
        IF (inds[0] EQ -1) THEN BEGIN
           peak_ind     = -1
           peak_energy  = -1
           RETURN
        ENDIF
        IF nInds EQ 1 THEN BEGIN
           peak_ind     = inds[0]
           peak_energy  = Xorig[peak_ind]
           RETURN
        ENDIF

        whichWy         = FIX(ABS((Xorig[inds[0]]-Xorig[inds[-1]]))/(Xorig[inds[0]]-Xorig[inds[-1]]))
        junk            = MIN(ABS(Xorig[inds]-minE),minEInd_ii)
        junk            = MIN(ABS(Xorig[inds]-maxE),maxEInd_ii)
        minEInd         = inds[minEInd_ii]
        maxEInd         = inds[maxEInd_ii]
        ;; start           = maxEInd
        ;; stop            = minEInd
        k               = maxEInd
        haveIt          = 0
        candidate       = 0
        ;; nCandidates     = 0

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;Check first on the solo tip

        ;;Less strict
        ;; candidate       = (Yorig[k] GT Yorig[k+whichWy]) + (Yorig[k] GT Yorig[k+whichWy+whichWy])
        ;;Stricter
        facDecrease     = 0.05
        CASE (minEInd-k) OF
           2: BEGIN
              candidate       = ((Yorig[k        ] - Yorig[k+whichWy  ])/Yorig[k        ] GT facDecrease) + $
                                ((Yorig[k+whichWy] - Yorig[k+whichWy*2])/Yorig[k+whichWy] GT facDecrease)
           END
           1: BEGIN
              candidate       = ((Yorig[k-1] - Yorig[k        ])/Yorig[k-1] GT facDecrease) + $
                                ((Yorig[k  ] - Yorig[k+whichWy])/Yorig[k  ] GT facDecrease)
           END
           ELSE:
        ENDCASE
        ;; IF ~(candidate EQ 2) THEN BEGIN
        ;;    k           += whichWy
        ;;    candidate    = ((Yorig[k]         - Yorig[k+whichWy])/Yorig[k] GT facDecrease) + $
        ;;                   ((Yorig[k+whichWy] - Yorig[k-whichWy])/Yorig[k] GT facDecrease)
        ;; ENDIF
        WHILE ((k+1) NE minEInd) AND ~(candidate EQ 2) DO BEGIN
           ;;Less strict
           ;; candidate    = (Yorig[k] GT Yorig[k+whichWy]) + (Yorig[k] GT Yorig[k-whichWy])

           ;;Stricter
           ;; candidate    = ((Yorig[k] - Yorig[k+whichWy])/Yorig[k] GT facDecrease) + ((Yorig[k] - Yorig[k-whichWy])/Yorig[k] GT facDecrease)
           candidate    = ((Yorig[k        ] - Yorig[k+whichWy  ])/Yorig[k        ] GT facDecrease) + $
                          ((Yorig[k+whichWy] - Yorig[k+whichWy*2])/Yorig[k+whichWy] GT facDecrease)
           
           IF (candidate EQ 2) THEN BREAK
           k += whichWy
        ENDWHILE
        ;; IF (candidate NE 2) THEN STOP
        IF (candidate NE 2) THEN BEGIN
           peak_ind     = -1
           peak_energy  = -1
        ENDIF ELSE BEGIN
           peak_ind     = k
           peak_energy  = Xorig[peak_ind]
        ENDELSE
     END
     ELSE: BEGIN
           inds         = WHERE((Xorig GE minE) AND (Xorig LE maxE) AND (Yorig-minFlux) GE 0)
           IF inds[0] EQ -1 AND ~KEYWORD_SET(its_OK__everyone_has_feelings) THEN BEGIN
              PRINT,"Can't find any good energy inds! Maybe lower your energy requirement."
              STOP
           ENDIF
           max_y        = MAX(Yorig[inds],peak_ind)
           peak_ind     = inds[peak_ind]
           peak_ind    -= b_offset
           peak_energy  = Xorig[peak_ind]
     END
  ENDCASE

  IF KEYWORD_SET(phi__use_energy_before_peak) THEN BEGIN

     peak_energy = Xorig[peak_ind+whichWy]
     peak_ind   += whichWy

  ENDIF

  ;; IF KEYWORD_SET(check_for_higher_flux_peaks) THEN BEGIN
  ;;    ;;Figure out where most energetic maximum is
  ;;    max_ys             = GET_N_MAXIMA_IN_ARRAY(Yorig,N=3,OUT_I=maxima_i)
  ;;    peak_y             = MAX(max_ys,max_y_ii)
  ;;    peak_ind           = maxima_i[max_y_ii]
  ;;    peak_energy        = Xorig[peak_ind]
  ;;    peak_tol_percent   = .25
     
  ;;    FOR i=0,N_ELEMENTS(maxima_i)-1 DO BEGIN
  ;;       testMax_X       = Xorig[maxima_i[i]]
  ;;       testMax         = max_ys[i]
  ;;       curMax_i        = maxima_i[i]
  ;;       PRINT,'testval:',STRCOMPRESS((ABS(testMax-peak_y)/peak_y),/REMOVE_ALL)
  ;;       IF testMax_X GT Xorig[peak_ind] AND $
  ;;          (ABS(testMax-peak_y)/peak_y) LT peak_tol_percent THEN BEGIN
  ;;          peak_ind     = curMax_i
  ;;          peak_energy  = testMax_X
  ;;       ENDIF
  ;;    ENDFOR
  ;;    PRINT,peak_ind
  ;; ENDIF ELSE BEGIN
     ;; CASE 1 OF
        ;; KEYWORD_SET(max_peak_energy): BEGIN
           

        ;; END
        ;; KEYWORD_SET(min_peak_energy): BEGIN
        ;;    inds         = WHERE(Xorig GE min_peak_energy)
        ;;    IF inds[0] EQ -1 THEN BEGIN
        ;;       PRINT,"Can't find any good energy inds! Maybe lower your energy requirement."
        ;;       STOP
        ;;    ENDIF
        ;;    max_y        = MAX(Yorig[inds],peak_ind)
        ;;    peak_ind     = inds[peak_ind]
        ;;    peak_ind    -= b_offset
        ;;    peak_energy  = Xorig[peak_ind]
        ;; END
     ;;    ELSE: BEGIN
     ;;       max_y        = MAX(Yorig,peak_ind)
     ;;       peak_ind    -= b_offset
     ;;       peak_energy  = Xorig[peak_ind]
     ;;    END
     ;; ENDCASE
     ;; PRINT,peak_ind
  ;; ENDELSE

END
