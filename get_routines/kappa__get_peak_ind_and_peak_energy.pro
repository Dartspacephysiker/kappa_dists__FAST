PRO KAPPA__GET_PEAK_IND_AND_PEAK_ENERGY, $
   Xorig,Yorig,peak_ind,peak_energy, $
   NENERGIES=nEnergies, $
   MAXEIND=maxEInd, $
   MINEIND=minEInd, $
   ENERGY_INDS=energy_inds, $
   ERANGE_FIT=eRange_fit, $
   N_BELOW_PEAK=n_below_peak, $
   N_ABOVE_PEAK=n_above_peak, $
   BULK_OFFSET=bulk_offset, $
   CHECK_FOR_HIGHER_FLUX_PEAKS=check_for_higher_flux_peaks, $
   MIN_PEAK_ENERGY=min_peak_energy, $
   MAX_PEAK_ENERGY=max_peak_energy, $
   PEAK_ENERGY__START_AT_HIGHE=peak_energy__start_at_highE, $
   CONTINUE_IF_NOMATCH=its_OK__everyone_has_feelings, $
   TEST_NOREV=test_noRev, $
   FOR_DMSP=for_DMSP, $
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
        facDecrease     = 0.025
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

  IF KEYWORD_SET(for_DMSP) THEN BEGIN

     minEInd        = (peak_ind - n_below_peak) > 0
     maxEInd        = (peak_ind + n_above_peak) < nEnergies-1

  ENDIF ELSE BEGIN
     ;;Note that while these are called maxE and minE, suggesting they refer to the max energy and min energy, they do NOT. 
     ;;Rather, they refer to the lowest and highest indices falling within the user-specified parameters 
     ;;  for fitting—namely, n_below_peak and n_above_peak
     maxEInd           = (peak_ind + n_below_peak) < (nEnergies-1)
     minEInd           = (peak_ind - n_above_peak) > 0
  ENDELSE

  ;; IF KEYWORD_SET(KF2D__Curvefit_opt.dont_fit_below_thresh_value) THEN BEGIN
  
  ;;    nAbove      = nEnergies-1-maxEInd
  ;;    killIt      = WHERE( (Xorig GE peak_energy) AND (Yorig LE 1e5),nStink)
  ;;    IF (nAbove GE 4) AND nStink NE 0 THEN BEGIN
  ;;       maxEInd  = maxEInd < MIN(killIt)
  ;;    ENDIF
  ;; ENDIF

  IF KEYWORD_SET(test_noRev) THEN BEGIN
     ;; max_energy =  Xorig[(minEInd - 2) > 0]
     ;; min_energy =  Xorig[(maxEInd + 2) < (nEnergies - 1)]
     max_energy =  Xorig[minEInd > 0]
     min_energy =  Xorig[maxEInd < (nEnergies - 1)]
  ENDIF ELSE BEGIN
     ;; min_energy =  Xorig[(minEInd - 2) > 0]
     ;; max_energy =  Xorig[(maxEInd + 2) < (nEnergies - 1)]
     IF KEYWORD_SET(for_DMSP) THEN BEGIN
        min_energy =  Xorig[minEInd > 0]
        max_energy =  Xorig[maxEInd < (nEnergies - 1)]
     ENDIF ELSE BEGIN
        min_energy = !NULL
        max_energy = !NULL
     ENDELSE
  ENDELSE

  eRange_fit      = [min_energy,max_energy]

  energy_inds     = [minEInd,maxEInd]

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
