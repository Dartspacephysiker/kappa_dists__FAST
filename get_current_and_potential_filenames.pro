;2017/03/22
PRO GET_CURRENT_AND_POTENTIAL_FILENAMES, $
   ORBTIMES=orbTimes, $
   ARANGE__MOMENTS_E_DOWN=aRange__moments_e_down, $
   ARANGE__MOMENTS_I_UP=aRange__moments_i_up, $
   USE_SC_POT_FOR_LOWERBOUND=use_sc_pot_for_lowerbound, $
   ADD_ONECOUNT_STATS=add_oneCount_stats, $
   SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
   ENFORCE_DIFF_EFLUX_SRATE=enforce_diff_eFlux_sRate, $
   MASTERFILE=masterFile, $
   SAVECURPOTFILE=saveCurPotFile

  COMPILE_OPT IDL2,STRICTARRSUBS

  t1Str = 'unspecified'
  t2Str = ''
  
  IF N_ELEMENTS(orbTimes) GT 0 THEN BEGIN
     CASE SIZE(orbTimes,/TYPE) OF
        5: BEGIN
           t1Str = T2S(orbTimes[0],/MS)
        END
        7: BEGIN
           t1Str = orbTimes[0]
        END
        ELSE: BEGIN
           STOP
        END
     ENDCASE

     IF N_ELEMENTS(orbTimes) EQ 2 THEN BEGIN
        CASE SIZE(orbTimes[1],/TYPE) OF
           5: BEGIN
              t2Str = T2S(orbTimes[1],/MS)
           END
           7: BEGIN
              t2Str = orbTimes[1]
           END
           ELSE: BEGIN
              STOP
           END
        ENDCASE
     ENDIF

     t1Str = '-' + (STRSPLIT(t1Str,'/',/EXTRACT))[1]
     t1Str = STRJOIN(STRSPLIT(t1Str,':',/EXTRACT),'_')
     t1Str = STRJOIN(STRSPLIT(t1Str,'.',/EXTRACT),'__')

     t2Str = '-' + (STRSPLIT(t2Str,'/',/EXTRACT))[1]
     t2Str = STRJOIN(STRSPLIT(t2Str,':',/EXTRACT),'_')
     t2Str = STRJOIN(STRSPLIT(t2Str,'.',/EXTRACT),'__')

     IF KEYWORD_SET(masterFile) THEN BEGIN
        ADD_FNAME_SUFF,masterFile,t1Str+t2Str
     ENDIF

     IF KEYWORD_SET(saveCurPotFile) THEN BEGIN
        ADD_FNAME_SUFF,saveCurPotFile,t1Str+t2Str
     ENDIF

  ENDIF

  ;; IF KEYWORD_SET(aRange__moments_e_down) THEN BEGIN

  ;;    CASE SIZE(aRange__moments_e_down,/TYPE) OF
  ;;       7: BEGIN

  ;;          CASE 1 OF
  ;;             STRMATCH(STRUPCASE(aRange__moments_e_down[0]),'*LC'): BEGIN
  ;;                LCStr = 'LC'
  ;;                IF STRLEN(aRange__moments_e_down[0]) GT 2 THEN BEGIN
  ;;                   factor = FLOAT(STRSPLIT(STRUPCASE(aRange__moments_e_down[0]),'LC',/EXTRACT))
  ;;                   LCStr = STRING(FORMAT='(F0.1)',factor) + LCStr
  ;;                ENDIF
  ;;                fSuff = "-aR_mom_eD_" + LCStr.Replace('.','_')
  ;;             END
  ;;             STRMATCH(STRUPCASE(aRange__moments_e_down[0]),'LC__EXCL_ATM'): BEGIN
  ;;                LCStr = 'LC__excl_atm'
  ;;                IF STRLEN(aRange__moments_e_down[0]) GT 12 THEN BEGIN
  ;;                   factor = FLOAT(STRSPLIT(STRUPCASE(aRange__moments_e_down[0]),'LC__EXCL_ATM',/EXTRACT))
  ;;                   LCStr = STRING(FORMAT='(F0.1)',factor) + LCStr
  ;;                ENDIF

  ;;                fSuff = "-aR_mom_eD_" + LCStr.Replace('.','_')
  ;;             END
  ;;             ELSE: BEGIN
  ;;                STOP
  ;;             END
  ;;          ENDCASE
  ;;       END
  ;;       ELSE: BEGIN
  ;;          fSuff = STRING(FORMAT='("-aR_mom_eD_",I0,"-",I0)',aRange__moments_e_down[0],aRange__moments_e_down[1])
  ;;       END
  ;;    ENDCASE

  ;;    IF KEYWORD_SET(masterFile) THEN BEGIN
  ;;       ADD_FNAME_SUFF,masterFile,fSuff
  ;;    ENDIF

  ;;    IF KEYWORD_SET(saveCurPotFile) THEN BEGIN
  ;;       ADD_FNAME_SUFF,saveCurPotFile,fSuff
  ;;    ENDIF

  ;; ENDIF

  ;; IF KEYWORD_SET(aRange__moments_i_up) THEN BEGIN

  ;;    CASE SIZE(aRange__moments_i_up,/TYPE) OF
  ;;       7: BEGIN
  ;;          IF STRMATCH(STRUPCASE(aRange__moments_i_up[0]),'*LC') THEN BEGIN
  ;;             LCStr = 'LC'
  ;;             IF STRLEN(aRange__moments_i_up[0]) GT 2 THEN BEGIN
  ;;                factor = FLOAT(STRSPLIT(STRUPCASE(aRange__moments_i_up[0]),'LC',/EXTRACT))
  ;;                LCStr = STRING(FORMAT='(F0.1)',factor) + LCStr
  ;;             ENDIF
  ;;             fSuff = "-aR_mom_iU_" + LCStr.Replace('.','_')
  ;;          ENDIF ELSE BEGIN
  ;;             STOP
  ;;          ENDELSE
  ;;       END
  ;;       ELSE: BEGIN

  ;;          IF ~(MIN(aRange__moments_i_up) EQ 0.) AND (MAX(aRange__moments_i_up) EQ 360.) THEN BEGIN

  ;;             fSuff = STRING(FORMAT='("-aR_mom_iU_",I0,"-",I0)',aRange__moments_i_up[0],aRange__moments_i_up[1])
  ;;             IF KEYWORD_SET(masterFile) THEN BEGIN
  ;;                ADD_FNAME_SUFF,masterFile,fSuff
  ;;             ENDIF

  ;;             IF KEYWORD_SET(saveCurPotFile) THEN BEGIN
  ;;                ADD_FNAME_SUFF,saveCurPotFile,fSuff
  ;;             ENDIF

  ;;          ENDIF

  ;;       END
  ;;    ENDCASE

  ;; ENDIF

  IF KEYWORD_SET(use_sc_pot_for_lowerbound) THEN BEGIN

     ;; IF ~(MIN(aRange__moments_i_up) EQ 0.) AND (MAX(aRange__moments_i_up) EQ 360.) THEN BEGIN

     fSuff = '-sc_pot'
     IF KEYWORD_SET(masterFile) THEN BEGIN
        ADD_FNAME_SUFF,masterFile,fSuff
     ENDIF

     IF KEYWORD_SET(saveCurPotFile) THEN BEGIN
        ADD_FNAME_SUFF,saveCurPotFile,fSuff
     ENDIF

     ;; ENDIF

  ENDIF

  IF KEYWORD_SET(add_oneCount_stats) THEN BEGIN
     ;;Whatever masterFile is, tack one '-oneCount' before the prefix

     fSuff = '-w_1Count'
     IF KEYWORD_SET(masterFile) THEN BEGIN
        ADD_FNAME_SUFF,masterFile,fSuff
     ENDIF

     IF KEYWORD_SET(saveCurPotFile) THEN BEGIN
        ADD_FNAME_SUFF,saveCurPotFile,fSuff
     ENDIF

  ENDIF

  CASE 1 OF
     KEYWORD_SET(enforce_diff_eFlux_sRate): BEGIN
        fSuff = '-sRate' + (STRING(FORMAT='(F0.2)',enforce_diff_eFlux_sRate)).Replace('.','_')

        IF KEYWORD_SET(masterFile) THEN BEGIN
           ADD_FNAME_SUFF,masterFile,fSuff
        ENDIF

        IF KEYWORD_SET(saveCurPotFile) THEN BEGIN
           ADD_FNAME_SUFF,saveCurPotFile,fSuff
        ENDIF
     END
     KEYWORD_SET(spectra_average_interval): BEGIN
        fSuff = '-avg_itvl' + STRING(FORMAT='(I0)',spectra_average_interval)

        IF KEYWORD_SET(masterFile) THEN BEGIN
           ADD_FNAME_SUFF,masterFile,fSuff
        ENDIF

        IF KEYWORD_SET(saveCurPotFile) THEN BEGIN
           ADD_FNAME_SUFF,saveCurPotFile,fSuff
        ENDIF
     END
  ENDCASE

  IF KEYWORD_SET(spectra_average_interval) THEN BEGIN
     ;;Whatever masterFile is, tack one '-oneCount' before the prefix

  ENDIF

  IF KEYWORD_SET(eeb_or_eesArr) THEN BEGIN
     ;;Who be dat
     CASE N_ELEMENTS(eeb_or_eesArr) OF
        1: BEGIN
           fSuff = eeb_or_eesArr[0]
        END
        2: BEGIN
           fSuff = eeb_or_eesArr[0] + '-' + eeb_or_eesArr[1]
        END
     ENDCASE

     IF KEYWORD_SET(masterFile) THEN BEGIN
        ADD_FNAME_SUFF,masterFile,fSuff
     ENDIF

     IF KEYWORD_SET(saveCurPotFile) THEN BEGIN
        ADD_FNAME_SUFF,saveCurPotFile,fSuff
     ENDIF

  ENDIF

END
