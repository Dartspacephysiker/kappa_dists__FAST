;;2017/02/22
PRO CURRENT_AND_POTENTIAL_ANALYSIS, $
   ORBIT=orbit, $
   ORBTIMES=orbTimes, $
   ORBBURSTTIMES=orbBurstTimes, $
   BONUSPREF=bonusPref, $
   DOWNTIMESSTR=downTimesStr, $
   UPTIMESSTR=upTimesStr, $
   TIMESLIST=timesList, $
   UNITS=units, $
   OUTDIR=outDir, $
   MASTERFILE=masterFile, $
   REMAKE_MASTERFILE=remake_masterFile, $
   SAVE_DIFF_EFLUX_FILE=save_diff_eFlux_file, $
   LOAD_DIFF_EFLUX_FILE=load_diff_eFlux_file, $
   EEB_OR_EESARR=eeb_or_eesArr, $
   ORDER=order, $
   LABEL=label, $
   ADD_ONECOUNT_STATS=add_oneCount_stats, $
   ARANGE__DENS_E_DOWN=aRange__dens_e_down, $
   ALSO_MSPH_SOURCECONE=also_msph_sourcecone, $
   ARANGE__MOMENTS_E_DOWN=aRange__moments_e_down, $
   ARANGE__MOMENTS_E_UP=aRange__moments_e_up, $
   ARANGE__MOMENTS_I_UP=aRange__moments_i_up, $
   ARANGE__PEAKEN_E_DOWN=aRange__peakEn_e_down, $
   ARANGE__PEAKEN_E_UP=aRange__peakEn_e_up, $
   ARANGE__PEAKEN_I_UP=aRange__peakEn_i_up, $
   ARANGE__CHARE_E_DOWN=aRange__charE_e_down, $
   ARANGE__CHARE_E_UP=aRange__charE_e_up, $
   ARANGE__CHARE_I_UP=aRange__charE_i_up, $
   WHICH_EEB__LABEL=label__which_eeb, $
   WHICH_TIMES__LABEL=label__which_times, $
   MOMENT_ENERGYARR=moment_energyArr, $
   USE_PEAKE_BOUNDS_FOR_MOMENT_CALC=use_peakE_bounds_for_moments, $
   USE_SC_POT_FOR_LOWERBOUND=use_sc_pot_for_lowerbound, $
   POT__FROM_FA_POTENTIAL=pot__from_fa_potential, $
   POT__ALL=pot__all, $
   POT__CHASTON_STYLE=pot__Chaston_style, $
   POT__FROM_FILE=pot__from_file, $
   ;; POT__FNAME=pot__fName, $
   POT__SAVE_FILE=pot__save_file, $
   ARANGE__MOMENTS_LIST=aRange__moments_list, $
   ARANGE__PEAKEN_LIST=aRange__peakEn_list, $
   ARANGE__CHARE_LIST=aRange__charE_list, $
   ELPHIC1998_DEFAULTS=Elphic1998_defaults, $
   MIN_PEAK_ENERGYARR=min_peak_energyArr, $
   MAX_PEAK_ENERGYARR=max_peak_energyArr, $
   PEAK_ENERGY__START_AT_HIGHEARR=peak_energy__start_at_highEArr, $
   UPGOINGARR=upgoingArr, $
   ERROR_ESTIMATES=error_estimates, $
   SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
   MAP_TO_100KM=map_to_100km, $
   SAVECURPOTFILE=saveCurPotFile, $
   OUT_CURPOTLIST=curPotList, $
   OUT_MAGCURRENT=magCurrent, $
   OUT_SC_POT=out_sc_pot, $
   OUT_DIFF_EFLUX_FILES=diff_eFlux_files, $
   OUT_SOURCECONE=out_sourcecone, $
   OUT_LOSSCONE=out_losscone, $
   _EXTRA=e

  COMPILE_OPT IDL2,STRICTARRSUBS

  IF ~KEYWORD_SET(timesList) THEN BEGIN
     PRINT,"Making timesList from downTimesStr and upTimesStr ..."
     downTimes               = REFORM(STR_TO_TIME(downTimesStr),SIZE(downTimesStr,/DIMENSIONS))
     upTimes                 = REFORM(STR_TO_TIME(upTimesStr  ),SIZE(upTimesStr  ,/DIMENSIONS))
     timesList               = LIST(downTimes,upTimes)
  ENDIF

  IF KEYWORD_SET(elphic1998_defaults) THEN BEGIN
     eeb_or_eesArr           = KEYWORD_SET(eeb_or_eesArr) ? eeb_or_eesArr : ['ees','ies']

     ;; order                   = [0,2,1]
     order                   = [0,1,2]
     ;; order                   = [2,1,0]
     label                   = ['downgoing_e','upgoing_e','upgoing_i']

     label__which_eeb        = [0,0,1]
     label__which_times      = [0,0,0]
     ;; moment_energyArr               = [[3e1,3.0e4],[3e1,3.0e4],[1e2,2.4e4]]
     ;; moment_energyArr               = [[4,3.0e4],[4,3.0e4],[4,2.4e4]]
     IF ~KEYWORD_SET(moment_energyArr) THEN BEGIN
        moment_energyArr            = [[50,3.0e4],[50,3.0e4],[4,2.4e4]]
     ENDIF

     ;;Remember, !NULL means that the program will use the loss-cone angle range by default!
     ;; aRange__dens_e_down     = KEYWORD_SET(aRange__dens_e_down) ? aRange__dens_e_down : [0.,360.]

     aRange__moments_e_down  = KEYWORD_SET(aRange__moments_e_down) ? aRange__moments_e_down : [0.,360.]
     aRange__moments_i_up    = KEYWORD_SET(aRange__moments_i_up  ) ? aRange__moments_i_up   : [0.,360.]
     aRange__moments_e_up    = KEYWORD_SET(aRange__moments_e_up  ) ? aRange__moments_e_up   : !NULL

     aRange__peakEn_e_down   = KEYWORD_SET(aRange__peakEn_e_down ) ? aRange__peakEn_e_down  : !NULL
     aRange__peakEn_e_up     = KEYWORD_SET(aRange__peakEn_e_up   ) ? aRange__peakEn_e_up    : !NULL
     aRange__peakEn_i_up     = KEYWORD_SET(aRange__peakEn_i_up   ) ? aRange__peakEn_i_up    : !NULL

     aRange__charE_e_down    = KEYWORD_SET(aRange__charE_e_down  ) ? aRange__charE_e_down   : !NULL
     aRange__charE_e_up      = KEYWORD_SET(aRange__charE_e_up    ) ? aRange__charE_e_up     : !NULL
     aRange__charE_i_up      = KEYWORD_SET(aRange__charE_i_up    ) ? aRange__charE_i_up     : !NULL

     ;; aRange__moments_e_down  = [330.,30.]
     ;; aRange__moments_i_up    = [150.,210.]
     ;; aRange__moments_e_up    = [150.,210.]

     aRange__moments_list    = LIST(aRange__moments_e_down,aRange__moments_e_up,aRange__moments_i_up)
     ;; aRange__peakEn_list     = LIST(!NULL,!NULL,[150,210])
     aRange__peakEn_list     = LIST(aRange__peakEn_e_down,aRange__peakEn_e_up,aRange__peakEn_i_up)
     aRange__charE_list      = LIST(aRange__charE_e_down,aRange__charE_e_up,aRange__charE_i_up)

     ;; min_peak_energy      = KEYWORD_SET(upgoing) ? 100 : 500
     ;; max_peak_energy      = KEYWORD_SET(upgoing) ? 3e4 : !NULL
     min_peak_energyArr      = KEYWORD_SET(min_peak_energyArr) ? min_peak_energyArr : [4,4,4]
     max_peak_energyArr      = KEYWORD_SET(max_peak_energyArr) ? max_peak_energyArr : [3e4,3e4,2.4e4]

     ;;If doing upgoing electrons
     peak_energy__start_at_highEArr  = [0,1,1]
     upgoingArr                      = [0,1,1]

  ENDIF

  GET_CURRENT_AND_POTENTIAL_FILENAMES, $
     ARANGE__MOMENTS_E_DOWN=aRange__moments_e_down, $
     ARANGE__MOMENTS_I_UP=aRange__moments_i_up, $
     USE_SC_POT_FOR_LOWERBOUND=use_sc_pot_for_lowerbound, $
     ADD_ONECOUNT_STATS=add_oneCount_stats, $
     SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
     MASTERFILE=masterFile, $
     SAVECURPOTFILE=saveCurPotFile

  IF KEYWORD_SET(outDir) THEN BEGIN
     diffEfluxDir = outDir.Replace('cur_and_pot_analysis/','diff_eFlux/')
  ENDIF

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;MEGA
  nCalcLoop              = N_ELEMENTS(label)
  preString = masterFile + ' ...'
  IF FILE_TEST(outDir+masterFile) AND ~KEYWORD_SET(remake_masterFile) THEN BEGIN
     RESTORE,outDir+masterFile
     afterString           = "Restored "

     also_oneCount         = ISA(n1_list) AND KEYWORD_SET(add_oneCount_stats)

  ENDIF ELSE BEGIN

     nDEFLoop              = N_ELEMENTS(eeb_or_eesArr)
     dEF_list              = LIST()
     dEF_1c_list           = LIST()
     north_southArr_list   = LIST()

     err_list              = LIST()
     err1_list             = LIST()

     time_list             = LIST()
     time1_list            = LIST()

     n_list                = LIST()
     nerr_list             = LIST()

     T_list                = LIST()
     Terr_list             = LIST()

     source_list           = LIST()

     j_list                = LIST()
     je_list               = LIST()
     cur_list              = LIST()
     chare_list            = LIST()

     jerr_list             = LIST()
     jeErr_list            = LIST()
     curErr_list           = LIST()
     charEErr_list         = LIST()

     IF KEYWORD_SET(add_oneCount_stats) THEN BEGIN

        n1_list            = LIST()
        n1err_list         = LIST()

        T1_list            = LIST()
        T1err_list         = LIST()

        j1_list            = LIST()
        je1_list           = LIST()
        cur1_list          = LIST()
        chare1_list        = LIST()

        j1err_list         = LIST()
        je1Err_list        = LIST()
        cur1Err_list       = LIST()
        charE1Err_list     = LIST()

     ENDIF

     IF KEYWORD_SET(use_sc_pot_for_lowerbound) THEN BEGIN
        sc_pot_list        = LIST()
     ENDIF

     diff_eFlux_files      = !NULL

     peak_ind_list         = LIST()
     peak_energy_list      = LIST()
     peak_dE_list          = LIST()
     aRange_oMoments_list  = LIST()
     aRange_oPeakEn_list   = LIST()
     aRange_oCharE_list    = LIST()

     FOR k=0,nDEFLoop-1 DO BEGIN

        eeb_or_ees        = eeb_or_eesArr[k]

        ;;String setup
        IF (STRUPCASE(eeb_or_ees) EQ 'EEB') OR (STRUPCASE(eeb_or_ees) EQ 'IEB') THEN BEGIN
           t1Str             = orbBurstTimes[0]
           t2Str             = orbBurstTimes[1]
           spectra_avg_itvl  = KEYWORD_SET(spectra_average_interval) ? spectra_average_interval : !NULL
        ENDIF ELSE BEGIN
           t1Str             = orbTimes[0]
           t2Str             = orbTimes[1]
           spectra_avg_itvl  = KEYWORD_SET(spectra_average_interval) ? spectra_average_interval : !NULL
        ENDELSE
        t1                   = STR_TO_TIME(t1Str)
        t2                   = STR_TO_TIME(t2Str)

        ;;... And strings!!!!
        KAPPA_FITTER__FSTRINGS, $
           ORBIT=orbit, $
           EEB_OR_EES=eeb_or_ees, $
           ELECTRON_ANGLERANGE=electron_angleRange ,$
           BONUSPREF=bonusPref ,$
           ;; ADD_ONECOUNT_STATS=add_oneCount_stats, $
           SAVE_DIFF_EFLUX_TO_FILE=save_diff_eFlux_to_file ,$
           SAVE_DIFF_EFLUX_FILE=save_diff_eFlux_file,$
           LOAD_DIFF_EFLUX_FILE=load_diff_eFlux_file,$
           OUT_DIFF_EFLUX_FILE=diff_eFlux_file, $
           FIT2D__ONLY_FIT_PEAK_ERANGE=fit2D__only_fit_peak_eRange ,$
           FIT2D__ONLY_FIT_ABOVEMIN=fit2D__only_fit_aboveMin ,$
           FIT2D__DISABLE_BFUNC=fit2D__disable_bFunc ,$
           ;; FIT2D__EXCLUDE_LCA_FROM_DENSCALC=fit2D__exclude_lca_from_densCalc ,$
           SPECTRA_AVERAGE_INTERVAL=spectra_avg_itvl, $
           ;; FITFILE=fitFile, $
           LOADDIR=diffEfluxDir

        pot__fName = diff_eflux_file.Replace('diff_eflux','sc_pot')
        preString = diff_eFlux_file + ' ...'

        ;;Can we get pot?
        IF KEYWORD_SET(use_sc_pot_for_lowerbound) THEN BEGIN
           ;; GET_SC_POTENTIAL,T1=diff_eFlux.time[0],T2=diff_eFlux.time[-1], $
           GET_SC_POTENTIAL,T1=t1,T2=t2, $
                            DATA=sc_pot, $
                            FROM_FA_POTENTIAL=pot__from_fa_potential, $
                            ALL=pot__all, $
                            /REPAIR, $
                            CHASTON_STYLE=pot__Chaston_style, $
                            FILENAME=pot__fName, $
                            FROM_FILE=pot__from_file, $
                            ORBIT=orbit, $
                            SAVE_FILE=pot__save_file
           out_sc_pot = sc_pot
           sc_pot_list.Add,sc_pot
        ENDIF

        gotIt = 0B
        IF (FILE_TEST(diff_eFlux_file) OR FILE_TEST(diffEfluxDir+diff_eFlux_file)) AND $
           KEYWORD_SET(load_diff_eFlux_file) $
        THEN BEGIN
           afterString = "Restored "
           realFile    = (FILE_TEST(diff_eFlux_file) ? '' : diffEfluxDir ) + diff_eFlux_file
           RESTORE,realFile
           gotIt = SIZE(diff_eFlux,/TYPE) EQ 8
        ENDIF

        IF gotIt THEN BEGIN
           afterString = "Restored "
        ENDIF ELSE BEGIN
           afterString = "Made "

           GET_DIFF_EFLUX,T1=t1,T2=t2, $
                          EEB_OR_EES=eeb_or_ees, $
                          NAME__DIFF_EFLUX=name__diff_eFlux, $
                          /CALC_GEOM_FACTORS, $
                          ;; UNITS=eSpecUnits, $
                          FIT_EACH_ANGLE=fit_each_angle, $
                          SPECTRA_AVERAGE_INTERVAL=spectra_avg_itvl, $
                          SC_POT=sc_pot, $
                          OUT_DIFF_EFLUX=diff_eflux, $
                          SAVE_DIFF_EFLUX_TO_FILE=save_diff_eFlux_to_file, $
                          LOAD_DAT_FROM_FILE=load_diff_eFlux_file, $
                          DIFF_EFLUX_FILE=diff_eFlux_file, $
                          LOAD_DIR=diffEfluxDir
        ENDELSE
        PRINT,preString + afterString

        IF KEYWORD_SET(add_oneCount_stats) THEN BEGIN
           
           PRINT,"Getting oneCount curve ..."
           save_dEF_oneCount_to_file = KEYWORD_SET(save_diff_eFlux_to_file)
           GET_ONECOUNT_DIFF_EFLUX,t1,t2, $
                                   EEB_OR_EES=eeb_or_ees, $
                                   SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                                   SC_POT=sc_pot, $
                                   IN_PROTOSTRUCT=diff_eFlux, $
                                   SDT_NAME=dEF_oneCount_name, $
                                   ANGLE=e_angle, $
                                   ;; ESPECUNITS=units, $
                                   ;; ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
                                   FIT_EACH_ANGLE=fit_each_angle, $ ;Perma-set because we do all angles for 2D fitting
                                   OUT_ONEDAT=out_oneDat, $
                                   DEF_ONECOUNT=dEF_oneCount, $
                                   SAVE_DEF_ONECOUNT_TO_FILE=save_dEF_oneCount_to_file, $
                                   LOAD_DAT_FROM_FILE=load_diff_eFlux_file, $
                                   DIFF_EFLUX_FILE=diff_eFlux_file, $
                                   LOAD_DIR=diffEfluxDir, $
                                   QUIET=quiet

           also_oneCount = N_ELEMENTS(dEF_oneCount) GT 0

           dEF_1c_list.Add,TEMPORARY(dEF_oneCount)

        ENDIF
        
        dEF_list.Add,TEMPORARY(diff_eFlux)

        diff_eFlux_files = [diff_eFlux_files,diffEfluxDir+diff_eFlux_file]

     ENDFOR

     ;;Now the calculations
     PRINT,"Beginning calculations"
     FOR fakeK=0,nCalcLoop-1 DO BEGIN

        k     = order[fakeK]
        t_k   = label__which_times[k]
        eeb_k = label__which_eeb[k]

        diff_eFlux                   = dEF_list[eeb_k]
        IF KEYWORD_SET(also_oneCount) THEN BEGIN
           dEF_oneCount              = dEF_1c_list[eeb_k]
        ENDIF
        IF KEYWORD_SET(use_sc_pot_for_lowerbound) THEN BEGIN
           sc_pot                    = sc_pot_list[eeb_k]
        ENDIF

        eeb_or_ees                   = eeb_or_eesArr[eeb_k]

        ;; energy                       = moment_energyArr[*,k]
        energy                       = MAKE_ENERGY_ARRAYS__FOR_DIFF_EFLUX(diff_eFlux, $
                                                                          ENERGY=moment_energyArr[*,k], $
                                                                          SC_POT=sc_pot, $
                                                                          EEB_OR_EES=eeb_or_ees)

        min_peak_energy              = min_peak_energyArr[k]
        max_peak_energy              = max_peak_energyArr[k]
        peak_energy__start_at_highE  = peak_energy__start_at_highEArr[k]
        upgoing                      = upgoingArr[k]

        ;;Set up, uh, n_below_peak and n_above_peak
        @kappa_fitter__defaults.pro

        tmpTimes                     = LIST_TO_1DARRAY(timesList[t_k])
        GET_FA_ORBIT,tmpTimes,/TIME_ARRAY,/NO_STORE,STRUC=struc
        ;; GET_DATA,'ILAT',DATA=ilat
        ;; north_southArr               = ABS(ilat.y)/ilat.y
        north_southArr               = ABS(struc.ilat)/struc.ilat

        north_southArr_list.Add,TEMPORARY(north_southArr)

        GET_LOSS_CONE_AND_ANGLE_RANGES_FOR_HEMI, $
           t1,t2, $
           lc_angleRange, $
           i_angle,i_angle_up, $
           north_south, $
           OUT_LCW=lcw, $
           ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
           CUSTOM_E_ANGLERANGE=custom_e_angleRange, $
           OUT_E_ANGLE=e_angle, $
           ANGLESTR=angleStr, $
           /JUST_ONE

        CASE upgoing OF
           0: BEGIN
              angleRange = lc_angleRange
           END
           1: BEGIN
              ;; CASE STRMATCH(STRUPCASE(eeb_or_ees),'EE*') OF
              ;;    0: BEGIN
              ;; angleRange = (360.*(angleRange/360.-FLOOR(angleRange/360.)))
              ;; angleRange = lc_angleRange - 180.
              angleRange = (360.*((lc_angleRange-180)/360.-FLOOR((lc_angleRange-180)/360.)))
              ;;    END
              ;; ENDCASE
           END
        ENDCASE

        aRange__dens = !NULL
        IF N_ELEMENTS(also_msph_sourcecone) GT 0 THEN BEGIN
           aRange__dens = also_msph_sourcecone[k] ? 'source' : !NULL
        ENDIF
        aRange__moments = N_ELEMENTS(aRange__moments_list[k]) GT 0 ? aRange__moments_list[k] : angleRange
        aRange__peakEn  = N_ELEMENTS(aRange__peakEn_list[k] ) GT 0 ? aRange__peakEn_list[k]  : angleRange
        aRange__charE   = N_ELEMENTS(aRange__charE_list[k]  ) GT 0 ? aRange__charE_list[k]   : angleRange

        IF SIZE(aRange__dens,/TYPE) EQ 7 THEN BEGIN

           CASE 1 OF
              STRMATCH(STRUPCASE(aRange__dens[0]),'SOURCE'): BEGIN

                 scw = 150

                 IF north_south[0] EQ -1 THEN BEGIN
                    aRange__dens = [180.-scw,180+scw]
                 ENDIF ELSE BEGIN
                    aRange__dens = [360.-scw,scw]
                 ENDELSE

              END
              ELSE: STOP
           ENDCASE

        ENDIF
        
        IF SIZE(aRange__moments[0],/TYPE) EQ 7 THEN BEGIN

           CASE 1 OF
              STRMATCH(STRUPCASE(aRange__moments[0]),'*LC'): BEGIN

                 IF STRLEN(aRange__moments[0]) GT 2 THEN BEGIN
                    factor = FLOAT(STRSPLIT(STRUPCASE(aRange__moments[0]),'LC',/EXTRACT))
                    IF north_south[0] EQ -1 THEN BEGIN
                       aRange__moments = [180.-factor*lcw,180+factor*lcw]
                    ENDIF ELSE BEGIN
                       aRange__moments = [360.-factor*lcw,factor*lcw]
                    ENDELSE
                 ENDIF ELSE BEGIN
                    aRange__moments = lc_angleRange
                 ENDELSE

                 IF upgoing THEN aRange__moments = (360.*((aRange__moments-180)/360.-FLOOR((aRange__moments-180)/360.)))

              END
              ELSE: BEGIN
                 PRINT,"Huh?"
                 STOP
              END
           ENDCASE

        ENDIF

        IF SIZE(aRange__peakEn[0],/TYPE) EQ 7 THEN BEGIN
           CASE 1 OF
              STRMATCH(STRUPCASE(aRange__peakEn[0]),'*LC'): BEGIN
                 IF STRLEN(aRange__peakEn[0]) GT 2 THEN BEGIN
                    factor = FLOAT(STRSPLIT(STRUPCASE(aRange__peakEn[0]),'LC',/EXTRACT))
                    IF north_south[0] EQ -1 THEN BEGIN
                       aRange__peakEn = [180.-factor*lcw,180+factor*lcw]
                    ENDIF ELSE BEGIN
                       aRange__peakEn = [360.-factor*lcw,factor*lcw]
                    ENDELSE
                 ENDIF ELSE BEGIN
                    aRange__peakEn  = lc_angleRange
                 ENDELSE

                 IF upgoing THEN aRange__peakEn = (360.*((aRange__peakEn-180)/360.-FLOOR((aRange__peakEn-180)/360.)))

              END
              ELSE: BEGIN
                 PRINT,"Huh?"
                 STOP
              END
           ENDCASE

        ENDIF

        IF SIZE(aRange__charE[0],/TYPE) EQ 7 THEN BEGIN

           CASE 1 OF
              STRMATCH(STRUPCASE(aRange__charE[0]),'*LC'): BEGIN
                 IF STRLEN(aRange__charE[0]) GT 2 THEN BEGIN
                    factor = FLOAT(STRSPLIT(STRUPCASE(aRange__charE[0]),'LC',/EXTRACT))
                    IF north_south[0] EQ -1 THEN BEGIN
                       aRange__charE = [180.-factor*lcw,180+factor*lcw]
                    ENDIF ELSE BEGIN
                       aRange__charE = [360.-factor*lcw,factor*lcw]
                    ENDELSE
                 ENDIF ELSE BEGIN
                    aRange__charE   = lc_angleRange
                 ENDELSE

                 IF upgoing THEN aRange__charE = (360.*((aRange__charE-180)/360.-FLOOR((aRange__charE-180)/360.)))

              END
              ELSE: BEGIN
                 PRINT,"Huh?"
                 STOP
              END
           ENDCASE

        ENDIF

        aRange_oMoments_list.Add,aRange__moments
        aRange_oPeakEn_list.Add,aRange__peakEn
        aRange_oCharE_list.Add,aRange__charE

        ;;Summary kind
        PRINT,FORMAT='("*****",A0,"*****")',STRUPCASE(label[k])
        IF KEYWORD_SET(aRange__dens) THEN BEGIN
           PRINT,FORMAT='(A0,T30,":",T35,2(F0.2,:,","))',"aRange__dens",aRange__dens
        ENDIF
        PRINT,FORMAT='(A0,T30,":",T35,2(F0.2,:,","))',"aRange__moments",aRange__moments
        PRINT,FORMAT='(A0,T30,":",T35,2(F0.2,:,","))',"aRange__peakEn",aRange__peakEn
        PRINT,FORMAT='(A0,T30,":",T35,2(F0.2,:,","))',"aRange__charE",aRange__charE
        ;; PRINT,FORMAT='(A0,T30,":",T35,2(F0.2,:,","))',"angleRange",angleRange
        PRINT,FORMAT='(A0,T30,":",T35,2(F0.2,:,","))',"energy",MEAN(energy,DIMENSION=2)
        PRINT,FORMAT='(A0,T30,":",T35,2(F0.2,:,","))',"min_peak_energy",min_peak_energy
        PRINT,FORMAT='(A0,T30,":",T35,2(F0.2,:,","))',"max_peak_energy",max_peak_energy
        PRINT,FORMAT='(A0,T30,":",T35,2(F0.2,:,","))',"peak_energy__start_at_highE",peak_energy__start_at_highE
        PRINT,FORMAT='(A0,T30,":",T35,2(F0.2,:,","))',"upgoing",upgoing
        PRINT,""

        PRINT,"Getting eSpecs ..."
        eSpec                 = GET_EN_SPEC__FROM_DIFF_EFLUX( $
                                diff_eFlux, $
                                /RETRACE, $
                                ANGLE=aRange__peakEn, $
                                UNITS=units)

        IF KEYWORD_SET(also_oneCount) THEN BEGIN
           oneCount_eSpec     = GET_EN_SPEC__FROM_DIFF_EFLUX( $
                                dEF_oneCount, $
                                /RETRACE, $
                                ANGLE=aRange__peakEn, $
                                UNITS=units)
        ENDIF
        
        angles                = TRANSPOSE(diff_eFlux.theta,[1,0,2])
        energies              = TRANSPOSE(diff_eFlux.energy,[1,0,2])
        nEnergies             = N_ELEMENTS(eSpec.v[0,*])

        iAngle                = 0
        nHere                 = N_ELEMENTS(eSpec.x)
        peak_indArr           = MAKE_ARRAY(nHere,VALUE=-999,/LONG)
        peak_energyArr        = MAKE_ARRAY(nHere,VALUE=-999,/FLOAT)
        peak_dEArr            = MAKE_ARRAY(nHere,VALUE=-999,/FLOAT)
        peak_EBoundsArr       = MAKE_ARRAY(2,nHere,VALUE=-999,/FLOAT)
        FOR iTime=0,nHere-1 DO BEGIN

           XorigArr           = energies[*,*,iTime]
           ;; YorigArr        = data[*,*,iTime]
           ;; worigArr        = ddata[*,*,iTime]
           ;; IF KEYWORD_SET(KF2D__Plot_opt.add_oneCount_curve) THEN BEGIN
           ;;    oneCountArr  = oneCount_data[*,*,iTime]
           ;; END
           AorigArr           = angles[*,*,iTime]

           ;;And now the order becomes [angle,energy] for each of these arrays
           Xorig              = REFORM(eSpec.v[iTime,*])
           Yorig              = REFORM(eSpec.y[iTime,*])
           worig              = REFORM(eSpec.yerr[iTime,*])
           Aorig              = REFORM(AorigArr[iAngle,*])

           IF KEYWORD_SET(also_oneCount) THEN BEGIN
              oneCurve        = {x:Xorig, $
                                 y:REFORM(oneCount_eSpec.y[iTime,*]), $
                                 NAME:"One Count"}
           ENDIF
           
           IF KEYWORD_SET(min_peak_energy) THEN BEGIN
              ;;Try taking it from the top
              min_peak_ind    = MAX(WHERE(REFORM(XorigArr[0,*]) GE min_peak_energy))
              IF min_peak_ind EQ -1 THEN BEGIN
                 STOP
              ENDIF
           ENDIF ELSE BEGIN
              min_peak_ind    = nEnergies-1
           ENDELSE

           KAPPA__GET_PEAK_IND_AND_PEAK_ENERGY, $
              Xorig,Yorig, $
              peak_ind,peak_energy, $
              BULK_OFFSET=bulk_offset, $
              CHECK_FOR_HIGHER_FLUX_PEAKS=check_higher_peaks_set_peakEn, $
              MIN_PEAK_ENERGY=min_peak_energy, $
              MAX_PEAK_ENERGY=max_peak_energy, $
              PEAK_ENERGY__START_AT_HIGHE=peak_energy__start_at_highE, $
              /CONTINUE_IF_NOMATCH, $
              ONECOUNT_STR=oneCurve

           ;;Note that while these are called maxE and minE, suggesting they refer to the max energy and min energy, they do NOT. 
           ;;Rather, they refer to the lowest and highest indices falling within the user-specified parameters 
           ;;  for fitting—namely, n_below_peak and n_above_peak
           maxEInd                   = (peak_ind + n_below_peak) < nEnergies-1
           minEInd                   = (peak_ind - n_above_peak) > 0

           peak_dEArr[iTime]         = eSpec.vErr[iTime,peak_ind]
           peak_energyArr[iTime]     = TEMPORARY(peak_energy)
           peak_indArr[iTime]        = TEMPORARY(peak_ind)
           peak_EBoundsArr[*,iTime]  = [Xorig[TEMPORARY(minEInd)],Xorig[TEMPORARY(maxEInd)]]
        ENDFOR

        peak_ind_list.Add,TEMPORARY(peak_indArr)
        peak_energy_list.Add,TEMPORARY(peak_energyArr)
        peak_dE_list.Add,TEMPORARY(peak_dEArr)

        IF KEYWORD_SET(use_peakE_bounds_for_moments) THEN BEGIN
           energy[0,*]               = peak_EBoundsArr[0,*] > energy[0,*]
           energy[1,*]               = peak_EBoundsArr[1,*] < energy[1,*]
        ENDIF

        ;; en_arr   = MAKE_ENERGY_ARRAYS__FOR_DIFF_EFLUX(diff_eFlux, $
        ;;                                               ENERGY=energy, $
        ;;                                               SC_POT=sc_pot, $
        ;;                                               EEB_OR_EES=eeb_or_ees)
        ;; energy   = TEMPORARY(en_arr)

        MOMENT_SUITE_2D,diff_eFlux, $
                        ENERGY=energy, $
                        ARANGE__DENS=aRange__dens, $
                        ARANGE__MOMENTS=aRange__moments, $
                        ARANGE__CHARE=aRange__charE, $
                        SC_POT=sc_pot, $
                        EEB_OR_EES=eeb_OR_ees, $
                        ERROR_ESTIMATES=error_estimates, $
                        MAP_TO_100KM=map_to_100km, $ 
                        ORBIT=orbit, $
                        /NEW_MOMENT_ROUTINE, $
                        QUIET=quiet, $
                        OUTTIME=time, $
                        OUT_N=n, $
                        OUT_J_=j, $
                        OUT_JE=je, $
                        OUT_T=T, $
                        OUT_CHARE=charE, $
                        OUT_CURRENT=cur, $
                        OUT_JJE_COVAR=jje_coVar, $
                        OUT_ERRORS=errors, $
                        OUT_ERR_N=nErr, $
                        OUT_ERR_J_=jErr, $
                        OUT_ERR_JE=jeErr, $
                        OUT_ERR_T=TErr, $
                        OUT_ERR_CURRENT=curErr, $
                        OUT_ERR_CHARE=charEErr, $
                        OUT_MAPRATIO=mapRatio, $
                        OUT_STRUCT=momStruct

        IF KEYWORD_SET(also_oneCount) THEN BEGIN

           MOMENT_SUITE_2D,dEF_oneCount, $
                           ENERGY=energy, $
                           ARANGE__DENS=aRange__dens, $
                           ARANGE__MOMENTS=aRange__moments, $
                           ARANGE__CHARE=aRange__charE, $
                           SC_POT=sc_pot, $
                           EEB_OR_EES=eeb_OR_ees, $
                           ERROR_ESTIMATES=error_estimates, $
                           MAP_TO_100KM=map_to_100km, $ 
                           ORBIT=orbit, $
                           /NEW_MOMENT_ROUTINE, $
                           QUIET=quiet, $
                           OUTTIME=time1, $
                           OUT_N=n1, $
                           OUT_J_=j1, $
                           OUT_JE=je1, $
                           OUT_T=T1Count, $
                           OUT_CHARE=charE1, $
                           OUT_CURRENT=cur1, $
                           OUT_JJE_COVAR=jje1_coVar, $
                           OUT_ERRORS=errors1, $
                           OUT_ERR_N=n1Err, $
                           OUT_ERR_J_=j1Err, $
                           OUT_ERR_JE=je1Err, $
                           OUT_ERR_T=T1Err, $
                           OUT_ERR_CURRENT=cur1Err, $
                           OUT_ERR_CHARE=charE1Err, $
                           OUT_MAPRATIO=mapRatio;; , $
                           ;; OUT_STRUCT=oneStruct

        ENDIF

        ;;Update lists
        ;; err_list.Add,TEMPORARY(errors)
        ;; time_list.Add,TEMPORARY(time)
        ;; n_list.Add,TEMPORARY(n)
        ;; T_list.Add,TEMPORARY(T)
        ;; j_list.Add,TEMPORARY(j)
        ;; je_list.Add,TEMPORARY(je)
        ;; cur_list.Add,TEMPORARY(cur)
        ;; chare_list.Add,charE
        ;; nerr_list.Add,TEMPORARY(nerr)
        ;; jerr_list.Add,TEMPORARY(jerr)
        ;; jeErr_list.Add,TEMPORARY(jeErr)
        ;; curErr_list.Add,TEMPORARY(curErr)
        ;; charEErr_list.Add,TEMPORARY(charEErr)
        ;; Terr_list.Add,TEMPORARY(Terr)

        err_list.Add,momStruct.errors
        time_list.Add,momStruct.time
        n_list.Add,momStruct.n
        T_list.Add,momStruct.T
        j_list.Add,momStruct.j
        je_list.Add,momStruct.je
        cur_list.Add,momStruct.cur
        chare_list.Add,momStruct.charE
        nerr_list.Add,momStruct.nerr
        jerr_list.Add,momStruct.jerr
        jeErr_list.Add,momStruct.jeErr
        curErr_list.Add,momStruct.curErr
        charEErr_list.Add,momStruct.charEErr
        Terr_list.Add,momStruct.Terr

        sourceStruct = !NULL
        STR_ELEMENT,momStruct,'source',sourceStruct
        IF SIZE(sourceStruct,/TYPE) EQ 8 THEN BEGIN
           ;;If k isn't 0, this isn't curPotList[edind]
           IF k NE 0 THEN STOP ELSE BEGIN
              source_list.Add,TEMPORARY(sourceStruct)
              source_ind = k
           ENDELSE

        ENDIF

        IF KEYWORD_SET(also_oneCount) THEN BEGIN
           err1_list.Add,TEMPORARY(errors1)
           time1_list.Add,TEMPORARY(time1)
           n1_list.Add,TEMPORARY(n1)
           T1_list.Add,TEMPORARY(T1Count)
           j1_list.Add,TEMPORARY(j1)
           je1_list.Add,TEMPORARY(je1)
           cur1_list.Add,cur1
           chare1_list.Add,charE1
           n1err_list.Add,TEMPORARY(n1err)
           j1err_list.Add,TEMPORARY(j1err)
           je1Err_list.Add,TEMPORARY(je1Err)
           cur1Err_list.Add,TEMPORARY(cur1Err)
           charE1Err_list.Add,TEMPORARY(charE1Err)
           T1err_list.Add,TEMPORARY(T1err)
        ENDIF

     ENDFOR


     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;The following loop ONLY exists so that times can be gotten for hacking at the mag current
     itvlTime         = !NULL
     tmpT             = timesList[t_k]
     nSegs            = N_ELEMENTS(tmpT[0,*])
     FOR segIter=0,nSegs-1 DO BEGIN

        tmpT1         = tmpT[0,segIter]
        tmpT2         = tmpT[1,segIter]

        theseInds     = WHERE( ( time_list[k] GE tmpT1 ) AND $
                               ( time_list[k] LE tmpT2 ), $
                               nThese)

        IF nThese EQ 0 THEN STOP

        theseInds     = CGSETINTERSECTION(theseInds,UNIQ((time_list[k]),SORT((time_list[k]))),COUNT=nThese)
        CHECK_SORTED,(time_list[k])[theseInds],is_sorted,/QUIET
        IF ~is_sorted THEN STOP
        IF nThese EQ 0 THEN STOP

        itvlTime      = [itvlTime,(time_list[k])[theseInds]]

     ENDFOR

     ;; ENDIF

     magCurrent = GET_CURRENT_FROM_FLUXMAG(t1,t2, $
                                           magStr,velocityStr, $
                                           /USE_DESPUN, $
                                           SDTNAME__JMAG=jMagName, $
                                           INFERRED_E_NUMFLUX=inferred_e_numFlux, $
                                           SDTNAME__INFERRED_E_NUMFLUX=e_numFluxName, $
                                           /STRANGEWAY_DECIMATE, $
                                           QUIET=quiet)

     magCurrent = DATA_CUT(magCurrent,itvlTime)*mapRatio

     afterString = "Made "

     SAVE,t1,t2, $
          north_southArr_list, $
          lc_angleRange, $
          mapRatio, $
          err_list, $
          err1_list, $
          time_list, $
          n_list, $
          j_list, $
          je_list, $
          cur_list, $
          chare_list, $
          T_list, $
          nerr_list, $
          jerr_list, $
          jeErr_list, $
          curErr_list, $
          charEErr_list, $
          source_list, $
          source_ind, $
          Terr_list, $
          time1_list, $
          n1_list, $
          j1_list, $
          je1_list, $
          cur1_list, $
          chare1_list, $
          T1_list, $
          n1err_list, $
          j1err_list, $
          je1Err_list, $
          cur1Err_list, $
          charE1Err_list, $
          T1err_list, $
          peak_ind_list, $
          peak_energy_list,peak_dE_list, $
          aRange_oMoments_list, $
          aRange_oCharE_list, $
          aRange_oPeakEn_list, $
          diff_eFlux_files, $
          magCurrent, $
          FILENAME=outDir+masterFile

  ENDELSE
  PRINT,preString + afterString

  out_sourcecone = lc_angleRange
  out_losscone   = (360.*((lc_angleRange-180)/360.-FLOOR((lc_angleRange-180)/360.)))

  ;;Now the calculations
  PRINT,"Now whittling to selected times"
  curPotList = LIST()
  FOR fakeK=0,nCalcLoop-1 DO BEGIN

     k     = order[fakeK]
     t_k   = label__which_times[k]
     eeb_k = label__which_eeb[k]

     PRINT,label[k] + ' ...'

     ;;electrons or ions?
     ions  = STRMATCH(STRUPCASE(eeb_or_eesArr[eeb_k]),'IE*')

     ;;North or south?
     tmpNS = north_southArr_list[t_k]

     ;;Get times
     tmpT  = timesList[t_k]
     nSegs = N_ELEMENTS(tmpT[0,*])

     IF (N_ELEMENTS((n_list[k])) NE N_ELEMENTS((j_list[k])        )) OR $
        (N_ELEMENTS((n_list[k])) NE N_ELEMENTS((je_list[k])       )) OR $
        (N_ELEMENTS((n_list[k])) NE N_ELEMENTS(peak_ind_list[k]   )) OR $
        (N_ELEMENTS((n_list[k])) NE N_ELEMENTS(peak_energy_list[k]))    $
     THEN BEGIN
        PRINT,"It's unequal everywhere"
        STOP
     ENDIF

     ;; IF ~ARRAY_EQUAL((n_list[k]).x,(j_list[k]).x      ) OR $
     ;;    ~ARRAY_EQUAL((n_list[k]).x,(je_list[k]).x     )    $
     ;; THEN BEGIN
     ;;    PRINT,"It's unequal everywhere"
     ;;    STOP
     ;; ENDIF

     itvlTime       = !NULL
     itvlN          = !NULL
     itvlJ          = !NULL
     itvlJe         = !NULL
     itvlCur        = !NULL
     itvlcharE      = !NULL
     itvlPeakE      = !NULL
     itvlT          = !NULL
     itvlCur        = !NULL
     itvlNerr       = !NULL
     itvlJerr       = !NULL
     itvlJeErr      = !NULL
     itvlcharEErr   = !NULL
     itvlPeakdE     = !NULL
     itvlTerr       = !NULL
     itvlCurErr     = !NULL
     itvlErrors     = !NULL

     itvlN1         = !NULL
     itvlJ1         = !NULL
     itvlJe1        = !NULL
     itvlcharE1     = !NULL
     itvlT1         = !NULL
     itvlCur1       = !NULL
     itvlN1err      = !NULL
     itvlJ1err      = !NULL
     itvlJe1Err     = !NULL
     itvlCharE1Err  = !NULL
     itvlT1err      = !NULL
     itvlCur1Err    = !NULL
     itvlErrors1    = !NULL
     itvlInds       = !NULL

     FOR segIter=0,nSegs-1 DO BEGIN

        tmpT1               = tmpT[0,segIter]
        tmpT2               = tmpT[1,segIter]

        ;; theseInds           = WHERE( ( (j_list[k]).x GE tmpT1 ) AND $
        ;;                        ( (j_list[k]).x LE tmpT2 ), $
        ;;                        nThese)
        theseInds           = WHERE( ( time_list[k] GE tmpT1 ) AND $
                                     ( time_list[k] LE tmpT2 ), $
                                     nThese)

        IF nThese EQ 0 THEN STOP

        ;; theseInds           = CGSETINTERSECTION(theseInds,UNIQ((j_list[k].x),SORT((j_list[k].x))),COUNT=nThese)
        theseInds           = CGSETINTERSECTION(theseInds,UNIQ((time_list[k]),SORT((time_list[k]))),COUNT=nThese)
        ;; CHECK_SORTED,(j_list[k].x)[theseInds],is_sorted,/QUIET
        CHECK_SORTED,(time_list[k])[theseInds],is_sorted,/QUIET
        IF ~is_sorted THEN STOP
        IF nThese EQ 0 THEN STOP

        ;; tmpTimes            = (j_list[k]).x[theseInds]
        tmpTimes            = (time_list[k])[theseInds]

        ;;Pick up temps
        ;; tmpN                = (N_list[k]).y[theseInds]
        ;; tmpJ                = (j_list[k]).y[theseInds]
        ;; tmpJe               = (je_list[k]).y[theseInds]
        ;; tmpCur              = (cur_list[k])[theseInds]
        ;; tmpCharE            = (chare_list[k])[theseInds]
        ;; tmpPeakE            = (peak_energy_list[k])[theseInds]
        ;; tmpPeakdE           = (peak_dE_list[k])[theseInds]
        ;; tmpT                = (T_list[k]).y[*,theseInds]
        tmpN                = (N_list[k])[theseInds]
        tmpJ                = (j_list[k])[theseInds]
        tmpJe               = (je_list[k])[theseInds]
        tmpCur              = (cur_list[k])[theseInds]
        tmpCharE            = (chare_list[k])[theseInds]
        tmpPeakE            = (peak_energy_list[k])[theseInds]
        tmpPeakdE           = (peak_dE_list[k])[theseInds]
        tmpT                = (T_list[k])[*,theseInds]

        IF KEYWORD_SET(also_oneCount) THEN BEGIN
           
           ;; tmpN1            = (N1_list[k]).y[theseInds]
           ;; tmpJ1            = (j1_list[k]).y[theseInds]
           ;; tmpJe1           = (je1_list[k]).y[theseInds]
           ;; tmpCur1          = (cur1_list[k])[theseInds]
           ;; tmpCharE1        = (chare1_list[k])[theseInds]
           ;; tmpT1            = (T1_list[k]).y[*,theseInds]
           tmpTime1         = (time1_list[k])[theseInds]
           tmpN1            = (N1_list[k])[theseInds]
           tmpJ1            = (j1_list[k])[theseInds]
           tmpJe1           = (je1_list[k])[theseInds]
           tmpCur1          = (cur1_list[k])[theseInds]
           tmpCharE1        = (chare1_list[k])[theseInds]
           tmpT1            = (T1_list[k])[*,theseInds]

        ENDIF

        IF N_ELEMENTS(source_ind) GT 0 THEN BEGIN
           IF k EQ source_ind THEN BEGIN
              itvlInds = [itvlInds,theseInds]
           ENDIF
        ENDIF
        
        ;; IF KEYWORD_SET(error_estimates) AND KEYWORD_SET(dens_errors) THEN BEGIN
        IF KEYWORD_SET(error_estimates) THEN BEGIN

           tmpNerr          = (nerr_list[k])[theseInds]
           tmpJerr          = (jerr_list[k])[theseInds]
           tmpJeErr         = (jeErr_list[k])[theseInds]
           tmpCurErr        = (curErr_list[k])[theseInds]
           tmpcharEErr      = (charEErr_list[k])[theseInds]
           tmpTerr          = (Terr_list[k])[theseInds]
           ;; SHRINK_GERSHMAN_ERROR_STRUCT,err_list[k],theseInds,tmpErrors

           IF KEYWORD_SET(also_oneCount) THEN BEGIN
              tmpN1err      = (n1err_list[k])[theseInds]
              tmpJ1err      = (j1err_list[k])[theseInds]
              tmpJe1Err     = (je1Err_list[k])[theseInds]
              tmpCur1Err    = (cur1Err_list[k])[theseInds]
              tmpcharE1Err  = (charE1Err_list[k])[theseInds]
              tmpT1err      = (T1err_list[k])[theseInds]
              ;; SHRINK_GERSHMAN_ERROR_STRUCT,err1_list[k],theseInds,tmpErrors1
           ENDIF
        ENDIF

        ;;Get current (flip sign of current for electrons)
        ;; tmpCur            = tmpJ  * 1.6e-9 * (ions ? 1. : (-1.))
        ;; IF KEYWORD_SET(also_oneCount) THEN BEGIN
        ;;    tmpCur1        = tmpJ1 * 1.6e-9 * (ions ? 1. : (-1.))
        ;; ENDIF
        
        ;; IF KEYWORD_SET(error_estimates) AND KEYWORD_SET(dens_errors) THEN BEGIN
        ;; IF KEYWORD_SET(error_estimates) THEN BEGIN
        ;;    ;;Don't flip sign, children
        ;;    tmpCurErr      = tmpJerr  * 1.6e-9 ;* (ions ? 1. : (-1.))

        ;;    IF KEYWORD_SET(also_oneCount) THEN BEGIN
        ;;       tmpCur1Err  = tmpJ1err * 1.6e-9 ;* (ions ? 1. : (-1.))
        ;;    ENDIF
        ;; ENDIF

        ;;Make outward current positive in both hemis
        ;; ;;(You know, field lines going in at the NH, going out at the SH, yadda yadda)
        ;;I take it all back, Elphic et al. [1998] make upward current (downgoing electrons) negative

        ;; CASE N_ELEMENTS(WHERE(tmpNS GT 0,/NULL)) OF
        ;; CASE N_ELEMENTS(WHERE(tmpNS LT 0,/NULL)) OF
        ;;    N_ELEMENTS(tmpNS): BEGIN
        ;;       tmpCur  *= (-1.)

        ;;       IF KEYWORD_SET(also_oneCount) THEN BEGIN
        ;;          tmpCur1 *= (-1.)
        ;;       ENDIF

        ;;    END
        ;;    0: BEGIN
        ;;    END
        ;;    ELSE: BEGIN
        ;;       STOP
        ;;    END
        ;; ENDCASE

        ;; tmpFile = 'TMP_'+label[k]+'-'+STRCOMPRESS(segIter,/REMOVE_ALL)+'.sav'
        ;; PRINT,"Saving " + tmpFile
        ;; SAVE,tmpJ,tmpJe,tmpCharE,tmpPeakE,tmpCur,FILENAME=outDir+tmpFile

        itvlTime             = [itvlTime      ,tmpTimes     ]
        itvlN                = [itvlN         ,tmpN         ]
        itvlJ                = [itvlJ         ,tmpJ         ]
        itvlJe               = [itvlJe        ,tmpJe        ]
        itvlcharE            = [itvlcharE     ,tmpChare     ]
        itvlT                = [itvlT         ,tmpT         ]
        itvlCur              = [itvlCur       ,tmpCur       ]

        IF KEYWORD_SET(also_oneCount) THEN BEGIN

           itvlN1            = [itvlN1        ,tmpN1        ]
           itvlJ1            = [itvlJ1        ,tmpJ1        ]
           itvlJe1           = [itvlJe1       ,tmpJe1       ]
           itvlcharE1        = [itvlcharE1    ,tmpChare1    ]
           itvlT1            = [itvlT1        ,tmpT1        ]
           itvlCur1          = [itvlCur1      ,tmpCur1      ]

        ENDIF
        
        ;; IF KEYWORD_SET(error_estimates) AND KEYWORD_SET(dens_errors) THEN BEGIN
        IF KEYWORD_SET(error_estimates) THEN BEGIN

           itvlNerr          = [itvlNerr      ,tmpNerr      ]
           itvlJerr          = [itvlJerr      ,tmpJerr      ]
           itvlJeErr         = [itvlJeErr     ,tmpJeErr     ]
           itvlcharEErr      = [itvlcharEErr  ,tmpcharEErr  ]
           itvlTerr          = [itvlTerr      ,tmpTerr      ]
           itvlCurErr        = [itvlCurErr    ,tmpCurErr    ]

           SHRINK_GERSHMAN_ERROR_STRUCT       ,err_list[k   ],theseInds,itvlErrors, $
                                               /ADD_TO_ERROROUT

           IF KEYWORD_SET(also_oneCount) THEN BEGIN

              itvlN1err      = [itvlN1err     ,tmpN1err     ]
              itvlJ1err      = [itvlJ1err     ,tmpJ1err     ]
              itvlJe1Err     = [itvlJe1Err    ,tmpJe1Err    ]
              itvlcharE1Err  = [itvlcharE1Err ,tmpcharE1Err ]
              itvlT1err      = [itvlT1err     ,tmpT1err     ]
              itvlCur1Err    = [itvlCur1Err   ,tmpCur1Err   ]
              SHRINK_GERSHMAN_ERROR_STRUCT    ,err1_list[k  ],theseInds,itvlErrors1, $
                                               /ADD_TO_ERROROUT
           ENDIF
        ENDIF

        itvlPeakE            = [itvlPeakE     ,tmpPeakE     ]
        itvlPeakdE           = [itvlPeakdE    ,tmpPeakdE    ]

     ENDFOR

     CASE 1 OF
        KEYWORD_SET(error_estimates): BEGIN

           tmpStruct      = {label    : label[k],$
                             time     : TEMPORARY(itvlTime)   , $
                             N        : TEMPORARY(itvlN)      , $
                             j        : TEMPORARY(itvlJ)      , $
                             je       : TEMPORARY(itvlJe)     , $
                             chare    : TEMPORARY(itvlcharE)  , $
                             T        : TEMPORARY(itvlT)      , $
                             cur      : TEMPORARY(itvlCur)    , $
                             Nerr     : TEMPORARY(itvlNerr)   , $
                             Jerr     : TEMPORARY(itvlJerr)   , $
                             JeErr    : TEMPORARY(itvlJeErr)   , $
                             charEErr : TEMPORARY(itvlcharEErr)   , $
                             Terr     : TEMPORARY(itvlTerr)   , $
                             CurErr   : TEMPORARY(itvlCurErr) , $
                             errors   : TEMPORARY(itvlErrors)}

           IF KEYWORD_SET(also_oneCount) THEN BEGIN
              tmp1Struct  = {N1       : TEMPORARY(itvlN1)     , $
                             j1       : TEMPORARY(itvlJ1)     , $
                             je1      : TEMPORARY(itvlJe1)    , $
                             cur1     : TEMPORARY(itvlCur1)   , $
                             chare1   : TEMPORARY(itvlcharE1) , $
                             T1       : TEMPORARY(itvlT1)     , $
                             N1err    : TEMPORARY(itvlN1err)  , $
                             J1Err    : TEMPORARY(itvlJ1Err)  , $
                             Je1Err    : TEMPORARY(itvlJe1Err)   , $
                             charE1Err : TEMPORARY(itvlcharE1Err)   , $
                             T1err    : TEMPORARY(itvlT1err)  , $
                             Cur1Err  : TEMPORARY(itvlCur1Err) , $
                             errors1  : TEMPORARY(itvlErrors1)}
              tmpStruct   = CREATE_STRUCT(TEMPORARY(tmpStruct),TEMPORARY(tmp1Struct))
           ENDIF

           bonus          = {peakE    : TEMPORARY(itvlPeakE)               , $
                             peakErr  : TEMPORARY(itvlPeakdE)              , $
                             energy   : moment_energyArr[*,k]              , $
                             angles   : {charE   : aRange_oCharE_list[k]   , $
                                         moments : aRange_oMoments_list[k] , $
                                         peakEn  : aRange_oPeakEn_list[k]} $
                            }

           tmpStruct      = CREATE_STRUCT(TEMPORARY(tmpStruct),TEMPORARY(bonus))
           
           IF N_ELEMENTS(source_ind) GT 0 THEN BEGIN
              IF k EQ source_ind THEN BEGIN
                 source_struct = {source : source_list[0], $
                                  inds   : TEMPORARY(itvlInds)}

                 tmpStruct   = CREATE_STRUCT(TEMPORARY(tmpStruct),TEMPORARY(source_struct))
              ENDIF
           ENDIF

        END
        ELSE: BEGIN
           tmpStruct      = {label    : label[k],$
                             time     : TEMPORARY(itvlTime)   , $
                             N        : TEMPORARY(itvlN)      , $
                             j        : TEMPORARY(itvlJ)      , $
                             je       : TEMPORARY(itvlJe)     , $
                             chare    : TEMPORARY(itvlcharE)  , $
                             T        : TEMPORARY(itvlT)      , $
                             cur      : TEMPORARY(itvlCur)    }

           IF KEYWORD_SET(also_oneCount) THEN BEGIN
              tmp1Struct  = {j1       : TEMPORARY(itvlJ1)     , $
                             je1      : TEMPORARY(itvlJe1)    , $
                             cur1     : TEMPORARY(itvlCur1)   , $
                             chare1   : TEMPORARY(itvlcharE1) , $
                             N1       : TEMPORARY(itvlN1)     , $
                             T1       : TEMPORARY(itvlT1)     }
              tmpStruct   = CREATE_STRUCT(TEMPORARY(tmpStruct),TEMPORARY(tmp1Struct))
           ENDIF

           bonus          = {peakE    : TEMPORARY(itvlPeakE)               , $
                             peakErr  : TEMPORARY(itvlPeakdE)              , $
                             energy   : moment_energyArr[*,k]              , $
                             angles   : {charE   : aRange_oCharE_list[k]   , $
                                         moments : aRange_oMoments_list[k] , $
                                         peakEn  : aRange_oPeakEn_list[k]} $
                            }

           tmpStruct      = CREATE_STRUCT(TEMPORARY(tmpStruct),TEMPORARY(bonus))
           
        END
     ENDCASE

     curPotList.Add,TEMPORARY(tmpStruct)
  ENDFOR

  IF KEYWORD_SET(saveCurPotFile) THEN BEGIN
     
     PRINT,"Saving it all to " + saveCurPotFile
     SAVE,curPotList,magCurrent,FILENAME=outDir+saveCurPotFile
  ENDIF

END
