;;2017/02/22
;2018/08/13 Set "ARRAY_OF_STRUCTS_INSTEAD" keyword
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
   USE_MSPH_SOURCECONE_FOR_DENS=use_msph_sourcecone_for_dens, $
   USE_MSPH_SOURCECONE_FOR_TEMP=use_msph_sourcecone_for_temp, $
   USE_ENERGIES_ABOVE_PEAK_FOR_TEMP=use_energies_above_peak_for_temp, $
   MSPH_SOURCECONE_HALFWIDTH=msph_sourcecone_halfWidth, $
   TEMPERATURE_TYPE_INDEX=tTypeInd, $
   ARANGE__DENS_E_DOWN=aRange__dens_e_down, $
   ARANGE__DENS_E_UP=aRange__dens_e_up, $
   ARANGE__DENS_I_UP=aRange__dens_i_up, $
   ARANGE__TEMP_E_DOWN=aRange__temp_e_down, $
   ;; ARANGE__TEMP_E_UP=aRange__temp_e_up, $
   ;; ARANGE__TEMP_I_UP=aRange__temp_i_up, $
   ERANGE__TEMP_E_DOWN=eRange__temp_e_down, $
   ;; ERANGE__TEMP_E_UP=eRange__temp_e_up, $
   ;; ERANGE__TEMP_I_UP=eRange__temp_i_up, $
   ;; ALSO_MSPH_SOURCECONE=also_msph_sourcecone, $
   ARANGE__MOMENTS_E_DOWN=aRange__moments_e_down, $
   ARANGE__MOMENTS_E_UP=aRange__moments_e_up, $
   ARANGE__MOMENTS_I_UP=aRange__moments_i_up, $
   ARANGE__PEAKEN_E_DOWN=aRange__peakEn_e_down, $
   ARANGE__PEAKEN_E_UP=aRange__peakEn_e_up, $
   ARANGE__PEAKEN_I_UP=aRange__peakEn_i_up, $
   ARANGE__CHARE_E_DOWN=aRange__charE_e_down, $
   ARANGE__CHARE_E_UP=aRange__charE_e_up, $
   ARANGE__CHARE_I_UP=aRange__charE_i_up, $
   MANUAL_ANGLE_CORRECTION=manual_angle_correction, $
   WHICH_EEB__LABEL=label__which_eeb, $
   WHICH_TIMES__LABEL=label__which_times, $
   MOMENT_ENERGYARR=moment_energyArr, $
   USE_PEAKE_BOUNDS_FOR_MOMENT_CALC=use_peakE_bounds_for_moment_calc, $
   PEAKE_BOUNDS_INDSHIFT=peakE_bounds_indShift, $
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
   ARANGE__DENS_LIST=aRange__dens_list, $
   ARANGE__TEMP_LIST=aRange__temp_list, $
   ERANGE__TEMP_LIST=eRange__temp_list, $
   ELPHIC1998_DEFAULTS=Elphic1998_defaults, $
   MIN_PEAK_ENERGYARR=min_peak_energyArr, $
   MAX_PEAK_ENERGYARR=max_peak_energyArr, $
   MIN_PEAK_ENERGY_TSTRUCT=min_peak_energy_tStruct, $
   MAX_PEAK_ENERGY_TSTRUCT=max_peak_energy_tStruct, $
   PEAK_ENERGY__START_AT_HIGHEARR=peak_energy__start_at_highEArr, $
   UPGOINGARR=upgoingArr, $
   ERROR_ESTIMATES=error_estimates, $
   SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
   ENFORCE_DIFF_EFLUX_SRATE=enforce_diff_eFlux_sRate, $
   MAP_TO_100KM=map_to_100km, $
   JV_THEOR__INITIAL_SOURCE_R_E=jv_theor__initial_source_R_E, $
   JV_THEOR__INITIAL_SOURCE__POLARSAT=jv_theor__initial_source__Polar, $
   JV_THEOR__INITIAL_SOURCE__EQUATOR=jv_theor__initial_source__equator, $
   SAVECURPOTFILE=saveCurPotFile, $
   OUT_CURPOTLIST=curPotList, $
   OUT_MAGCURRENT=magCurrent, $
   SC_POT=sc_pot, $
   OUT_DIFF_EFLUX_FILES=diff_eFlux_files, $
   OUT_SOURCECONE=out_sourcecone, $
   OUT_LOSSCONE=out_losscone, $
   OUT_MRATIO=mRatio, $
   BATCH_MODE=batch_mode, $
   _EXTRA=e

  COMPILE_OPT IDL2,STRICTARRSUBS

  IF ~KEYWORD_SET(timesList) THEN BEGIN
     PRINT,"Making timesList from downTimesStr and upTimesStr ..."
     downTimes               = REFORM(STR_TO_TIME(downTimesStr),SIZE(downTimesStr,/DIMENSIONS))
     upTimes                 = REFORM(STR_TO_TIME(upTimesStr  ),SIZE(upTimesStr  ,/DIMENSIONS))
     timesList               = LIST(downTimes,upTimes)
  ENDIF

  IF N_ELEMENTS(use_msph_sourcecone_for_dens) GT 0 THEN BEGIN
     CASE N_ELEMENTS(use_msph_sourcecone_for_dens) OF
        1: BEGIN
           msph_sc_dens = [use_msph_sourcecone_for_dens,0,0]
        END
        2: BEGIN
           msph_sc_dens = [use_msph_sourcecone_for_dens,0]
        END
        3: BEGIN
           msph_sc_dens = use_msph_sourcecone_for_dens
        END
     ENDCASE        
  ENDIF ELSE BEGIN
     msph_sc_dens       = [0,0,0]
  ENDELSE

  IF N_ELEMENTS(use_msph_sourcecone_for_temp) GT 0 THEN BEGIN
     CASE N_ELEMENTS(use_msph_sourcecone_for_temp) OF
        1: BEGIN
           msph_sc_temp = [use_msph_sourcecone_for_temp,0,0]
        END
        2: BEGIN
           msph_sc_temp = [use_msph_sourcecone_for_temp,0]
        END
        3: BEGIN
           msph_sc_temp = use_msph_sourcecone_for_temp
        END
     ENDCASE        
  ENDIF ELSE BEGIN
     msph_sc_temp       = [0,0,0]
  ENDELSE
  
  IF N_ELEMENTS(use_energies_above_peak_for_temp) GT 0 THEN BEGIN
     CASE N_ELEMENTS(use_energies_above_peak_for_temp) OF
        1: BEGIN
           e_above_peak_temp = [use_energies_above_peak_for_temp,0,0]
        END
        2: BEGIN
           e_above_peak_temp = [use_energies_above_peak_for_temp,0]
        END
        3: BEGIN
           e_above_peak_temp = use_energies_above_peak_for_temp
        END
     ENDCASE        
  ENDIF ELSE BEGIN
     e_above_peak_temp       = [0,0,0]
  ENDELSE
  
  peakE_bounds_for_moment_calc = [0,0,0]
  IF KEYWORD_SET(use_peakE_bounds_for_moment_calc) THEN BEGIN
     CASE N_ELEMENTS(use_peakE_bounds_for_moment_calc) OF
        1: BEGIN
           peakE_bounds_for_moment_calc = [1,1,1] * use_peakE_bounds_for_moment_calc
        END
        2: BEGIN
           peakE_bounds_for_moment_calc = [use_peakE_bounds_for_moment_calc,0]
        END
        3: BEGIN
           peakE_bounds_for_moment_calc = use_peakE_bounds_for_moment_calc
        END
     ENDCASE
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

     ;; Set defaults
     aRange__moments_e_down  = KEYWORD_SET(aRange__moments_e_down) ? aRange__moments_e_down : [0.,360.]
     aRange__moments_i_up    = KEYWORD_SET(aRange__moments_i_up  ) ? aRange__moments_i_up   : [0.,360.]
     aRange__moments_e_up    = KEYWORD_SET(aRange__moments_e_up  ) ? aRange__moments_e_up   : !NULL

     aRange__peakEn_e_down   = KEYWORD_SET(aRange__peakEn_e_down ) ? aRange__peakEn_e_down  : !NULL
     aRange__peakEn_e_up     = KEYWORD_SET(aRange__peakEn_e_up   ) ? aRange__peakEn_e_up    : !NULL
     aRange__peakEn_i_up     = KEYWORD_SET(aRange__peakEn_i_up   ) ? aRange__peakEn_i_up    : !NULL

     aRange__charE_e_down    = KEYWORD_SET(aRange__charE_e_down  ) ? aRange__charE_e_down   : !NULL
     aRange__charE_e_up      = KEYWORD_SET(aRange__charE_e_up    ) ? aRange__charE_e_up     : !NULL
     aRange__charE_i_up      = KEYWORD_SET(aRange__charE_i_up    ) ? aRange__charE_i_up     : !NULL

     ;; These can all be overridden by msph_sc_dens! They might be meaningless!
     aRange__dens_e_down    = KEYWORD_SET(aRange__dens_e_down  ) ? aRange__dens_e_down   : !NULL
     aRange__dens_e_up      = KEYWORD_SET(aRange__dens_e_up    ) ? aRange__dens_e_up     : !NULL
     aRange__dens_i_up      = KEYWORD_SET(aRange__dens_i_up    ) ? aRange__dens_i_up     : !NULL

     aRange__temp_e_down    = KEYWORD_SET(aRange__temp_e_down  ) ? aRange__temp_e_down   : !NULL
     aRange__temp_e_up      = KEYWORD_SET(aRange__temp_e_up    ) ? aRange__temp_e_up     : !NULL
     aRange__temp_i_up      = KEYWORD_SET(aRange__temp_i_up    ) ? aRange__temp_i_up     : !NULL

     eRange__temp_e_down    = KEYWORD_SET(eRange__temp_e_down  ) ? eRange__temp_e_down   : moment_energyArr[*,0]
     eRange__temp_e_up      = KEYWORD_SET(eRange__temp_e_up    ) ? eRange__temp_e_up     : !NULL ;moment_energyArr[*,1]
     eRange__temp_i_up      = KEYWORD_SET(eRange__temp_i_up    ) ? eRange__temp_i_up     : !NULL ;moment_energyArr[*,2]

     ;; aRange__moments_e_down  = [330.,30.]
     ;; aRange__moments_i_up    = [150.,210.]
     ;; aRange__moments_e_up    = [150.,210.]

     aRange__moments_list    = LIST(aRange__moments_e_down,aRange__moments_e_up,aRange__moments_i_up)
     ;; aRange__peakEn_list     = LIST(!NULL,!NULL,[150,210])
     aRange__peakEn_list     = LIST(aRange__peakEn_e_down,aRange__peakEn_e_up,aRange__peakEn_i_up)
     aRange__charE_list      = LIST(aRange__charE_e_down,aRange__charE_e_up,aRange__charE_i_up)
     aRange__dens_list       = LIST(aRange__dens_e_down,aRange__dens_e_up,aRange__dens_i_up)
     aRange__temp_list       = LIST(aRange__temp_e_down,aRange__temp_e_up,aRange__temp_i_up)
     eRange__temp_list       = LIST(eRange__temp_e_down,eRange__temp_e_up,eRange__temp_i_up)

     ;; min_peak_energy      = KEYWORD_SET(upgoing) ? 100 : 500
     ;; max_peak_energy      = KEYWORD_SET(upgoing) ? 3e4 : !NULL
     min_peak_energyArr      = KEYWORD_SET(min_peak_energyArr) ? min_peak_energyArr : [4,4,4]
     max_peak_energyArr      = KEYWORD_SET(max_peak_energyArr) ? max_peak_energyArr : [3.1D4,3.1D4,2.4e4]

     ;;If doing upgoing electrons
     peak_energy__start_at_highEArr  = [0,1,0]
     upgoingArr                      = [0,1,1]

  ENDIF

  array_of_structs_instead = 1  ;2018/08/13

  GET_CURRENT_AND_POTENTIAL_FILENAMES, $
     ORBTIMES=orbTimes, $
     ARANGE__MOMENTS_E_DOWN=aRange__moments_list[0], $
     ARANGE__MOMENTS_I_UP=aRange__moments_list[2], $
     USE_SC_POT_FOR_LOWERBOUND=use_sc_pot_for_lowerbound, $
     ADD_ONECOUNT_STATS=add_oneCount_stats, $
     SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
     ENFORCE_DIFF_EFLUX_SRATE=enforce_diff_eFlux_sRate, $
     MASTERFILE=masterFile, $
     SAVECURPOTFILE=saveCurPotFile

  IF KEYWORD_SET(outDir) THEN BEGIN
     loadDir = outDir.Replace('cur_and_pot_analysis/','')
     diffEFluxSuffDir = 'diff_eFlux/'
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
     blacklist_list        = MAKE_ARRAY(nCalcLoop,/LONG,VALUE=0)

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
     momsforT_list         = LIST()

     j_list                = LIST()
     je_list               = LIST()
     cur_list              = LIST()
     mapRatio_list         = LIST()
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

        IF SIZE(sc_pot,/TYPE) NE 8 THEN BEGIN
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
        ENDIF
     ENDIF

     diff_eFlux_files      = !NULL

     eSpec_list            = LIST()
     peak_ind_list         = LIST()
     peak_energy_list      = LIST()
     peak_dE_list          = LIST()
     peak_eBounds_list     = LIST()
     aRange_oMoments_list  = LIST()
     aRange_oPeakEn_list   = LIST()
     aRange_oCharE_list    = LIST()
     aRange_oTemp_list     = LIST()

     FOR k=0,nDEFLoop-1 DO BEGIN

        eeb_or_ees        = eeb_or_eesArr[k]

        ;;String setup
        IF (STRUPCASE(eeb_or_ees) EQ 'EEB') OR (STRUPCASE(eeb_or_ees) EQ 'IEB') THEN BEGIN
           t1Str             = orbBurstTimes[0]
           t2Str             = orbBurstTimes[1]
           spectra_avg_itvl  = KEYWORD_SET(spectra_average_interval) ? spectra_average_interval : !NULL
           enforce_sRate     = KEYWORD_SET(enforce_diff_eFlux_sRate) ? enforce_diff_eFlux_sRate : !NULL
        ENDIF ELSE BEGIN
           t1Str             = orbTimes[0]
           t2Str             = orbTimes[1]
           spectra_avg_itvl  = KEYWORD_SET(spectra_average_interval) ? spectra_average_interval : !NULL
           enforce_sRate     = KEYWORD_SET(enforce_diff_eFlux_sRate) ? enforce_diff_eFlux_sRate : !NULL
        ENDELSE
        t1                   = STR_TO_TIME(t1Str)
        t2                   = STR_TO_TIME(t2Str)

        ;;... And strings!!!!
        KAPPA_FITTER__FSTRINGS, $
           T1=t1, $
           T2=t2, $
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
           ENFORCE_DIFF_EFLUX_SRATE=enforce_diff_eFlux_sRate, $
           ;; FITFILE=fitFile, $
           LOADDIR=loadDir

        pot__fName = diff_eflux_file.Replace('diff_eflux','sc_pot')
        preString = diff_eFlux_file + ' ...'

        ;;Can we get pot?
        IF KEYWORD_SET(use_sc_pot_for_lowerbound) THEN BEGIN

           tmpscinds = WHERE((sc_pot.x GE t1) AND (sc_pot.x LE t2))
           IF tmpscinds[0] EQ -1 THEN IF ~KEYWORD_SET(batch_mode) THEN STOP
           tmpSc_pot = sc_pot
           STR_ELEMENT,tmpSc_pot,'x',sc_pot.x[tmpscinds],/ADD_REPLACE
           STR_ELEMENT,tmpSc_pot,'y',sc_pot.y[tmpscinds],/ADD_REPLACE

           sc_pot_list.Add,tmpSc_pot
        ENDIF

        gotIt = 0B
        IF (FILE_TEST(diff_eFlux_file) OR FILE_TEST(loadDir+diffEFluxSuffDir+diff_eFlux_file)) AND $
           KEYWORD_SET(load_diff_eFlux_file) $
        THEN BEGIN
           afterString = "Restored "
           realFile    = (FILE_TEST(diff_eFlux_file) ? '' : loadDir+diffEFluxSuffDir ) + diff_eFlux_file
           RESTORE,realFile
           gotIt = SIZE(diff_eFlux,/TYPE) EQ 8
        ENDIF

        IF gotIt THEN BEGIN
           afterString = "Restored "

           IF KEYWORD_SET(manual_angle_correction) THEN BEGIN
              ;; STOP
              ;; cLimit = 0
              ;; COMMON cc,counter
              ;; IF N_ELEMENTS(counter) EQ 0 THEN counter = 1 ELSE counter += 1
              ;; IF counter GE cLimit THEN STOP
              MANUALLY_CORRECT_DIFF_EFLUX_ANGLE,diff_eFlux,manual_angle_correction
           ENDIF
        ENDIF ELSE BEGIN
           afterString = "Made "

           GET_DIFF_EFLUX,T1=t1,T2=t2, $
                          EEB_OR_EES=eeb_or_ees, $
                          NAME__DIFF_EFLUX=name__diff_eFlux, $
                          /CALC_GEOM_FACTORS, $
                          ARRAY_OF_STRUCTS_INSTEAD=array_of_structs_instead, $
                          ;; UNITS=eSpecUnits, $
                          FIT_EACH_ANGLE=fit_each_angle, $
                          SPECTRA_AVERAGE_INTERVAL=spectra_avg_itvl, $
                          ENFORCE_DIFF_EFLUX_SRATE=enforce_diff_eFlux_sRate, $
                          MANUAL_ANGLE_CORRECTION=manual_angle_correction, $
                          SC_POT=tmpSc_pot, $
                          OUT_DIFF_EFLUX=diff_eflux, $
                          SAVE_DIFF_EFLUX_TO_FILE=save_diff_eFlux_to_file, $
                          LOAD_DAT_FROM_FILE=load_diff_eFlux_file, $
                          DIFF_EFLUX_FILE=diff_eFlux_file, $
                          LOAD_DIR=loadDir
        ENDELSE
        PRINT,preString + afterString

        IF KEYWORD_SET(add_oneCount_stats) THEN BEGIN
           
           PRINT,"Getting oneCount curve ..."
           save_dEF_oneCount_to_file = KEYWORD_SET(save_diff_eFlux_to_file)
           GET_ONECOUNT_DIFF_EFLUX,t1,t2, $
                                   EEB_OR_EES=eeb_or_ees, $
                                   SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                                   ENFORCE_DIFF_EFLUX_SRATE=enforce_diff_eFlux_sRate, $
                                   SC_POT=tmpSc_pot, $
                                   IN_PROTOSTRUCT=diff_eFlux, $
                                   SDT_NAME=dEF_oneCount_name, $
                                   ANGLE=e_angle, $
                                   ARRAY_OF_STRUCTS_INSTEAD=array_of_structs_instead, $
                                   ;; ESPECUNITS=units, $
                                   ;; ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
                                   FIT_EACH_ANGLE=fit_each_angle, $ ;Perma-set because we do all angles for 2D fitting
                                   OUT_ONEDAT=out_oneDat, $
                                   DEF_ONECOUNT=dEF_oneCount, $
                                   SAVE_DEF_ONECOUNT_TO_FILE=save_dEF_oneCount_to_file, $
                                   LOAD_DAT_FROM_FILE=load_diff_eFlux_file, $
                                   DIFF_EFLUX_FILE=diff_eFlux_file, $
                                   LOAD_DIR=loadDir, $
                                   QUIET=quiet

           also_oneCount = N_ELEMENTS(dEF_oneCount) GT 0

           dEF_1c_list.Add,TEMPORARY(dEF_oneCount)

        ENDIF
        
        dEF_list.Add,TEMPORARY(diff_eFlux)

        diff_eFlux_files = [diff_eFlux_files,loadDir+diffEFluxSuffDir+diff_eFlux_file]

     ENDFOR

     ;;Now the calculations
     PRINT,"Beginning calculations"
     FOR fakeK=0,nCalcLoop-1 DO BEGIN

        k     = order[fakeK]
        t_k   = label__which_times[k]
        eeb_k = label__which_eeb[k]

        diff_eFlux                   = dEF_list[eeb_k]

        IF SIZE(diff_eFlux,/TYPE) NE 8 THEN BEGIN
           PRINT,"Sorry, can't do this'n."
           blacklist_list[k] = 1
           CONTINUE
        ENDIF

        IF KEYWORD_SET(also_oneCount) THEN BEGIN
           dEF_oneCount              = dEF_1c_list[eeb_k]
        ENDIF
        IF KEYWORD_SET(use_sc_pot_for_lowerbound) THEN BEGIN
           tmpSc_pot                 = sc_pot_list[eeb_k]
        ENDIF ELSE BEGIN
           
        ENDELSE

        eeb_or_ees                   = eeb_or_eesArr[eeb_k]

        use_peakE_bounds_for_moments = peakE_bounds_for_moment_calc[k]
        ;; energy                       = moment_energyArr[*,k]
        energy                       = MAKE_ENERGY_ARRAYS__FOR_DIFF_EFLUX(diff_eFlux, $
                                                                          ENERGY=moment_energyArr[*,k], $
                                                                          SC_POT=tmpSc_pot, $
                                                                          EEB_OR_EES=eeb_or_ees)
        IF KEYWORD_SET(e_above_peak_temp[k]) THEN BEGIN
           energyArr_forTemp         = MAKE_ENERGY_ARRAYS__FOR_DIFF_EFLUX(diff_eFlux, $
                                                                          ENERGY=moment_energyArr[*,k], $
                                                                          SC_POT=tmpSc_pot, $
                                                                          EEB_OR_EES=eeb_or_ees)
        ENDIF

        minpeType                   = 'F0.2'
        min_peak_energy           = min_peak_energyArr[k]
        has_minpe_struct            = KEYWORD_SET(min_peak_energy_tStruct)
        IF has_minpe_struct THEN BEGIN

           IF (WHERE(min_peak_energy_tStruct.forWhom EQ k))[0] NE -1 THEN BEGIN

              min_peak_energy        = 'tStruct'
              minpeType                = 'A0'

           ENDIF ELSE BEGIN

              min_peak_energy        = min_peak_energyArr[k]
              has_minpe_struct       = 0
              
           ENDELSE

        ENDIF

        maxpeType                   = 'F0.2'
        max_peak_energy           = max_peak_energyArr[k]
        has_maxpe_struct            = KEYWORD_SET(max_peak_energy_tStruct)
        IF has_maxpe_struct THEN BEGIN

           IF (WHERE(max_peak_energy_tStruct.forWhom EQ k))[0] NE -1 THEN BEGIN

              max_peak_energy        = 'tStruct'
              maxpeType                = 'A0'

           ENDIF ELSE BEGIN

              max_peak_energy        = max_peak_energyArr[k]
              has_maxpe_struct       = 0
              
           ENDELSE

        ENDIF
        ;; max_peak_energy              = max_peak_energyArr[k]
        peak_energy__start_at_highE  = peak_energy__start_at_highEArr[k]
        upgoing                      = upgoingArr[k]

        ;;Set up, uh, n_below_peak and n_above_peak
        @kappa_fitter__defaults.pro

        tmpTimes                     = LIST_TO_1DARRAY(timesList[t_k])
        GET_FA_ORBIT,tmpTimes,/TIME_ARRAY,/ALL,/NO_STORE,STRUC=struc
        ;; GET_DATA,'ILAT',DATA=ilat
        ;; north_southArr               = ABS(ilat.y)/ilat.y
        north_southArr               = ABS(struc.ilat)/struc.ilat

        north_southArr_list.Add,TEMPORARY(north_southArr)

        GET_LOSS_CONE_AND_ANGLE_RANGES_FOR_HEMI, $
           t1,t2, $
           lc_angleRange, $
           i_angle,i_angle_up, $
           north_south, $
           ALLEXCLATM_ARANGE=allExclAtm_aRange, $
           OUT_LCW=lcw, $
           ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
           CUSTOM_E_ANGLERANGE=custom_e_angleRange, $
           OUT_E_ANGLE=e_angle, $
           ANGLESTR=angleStr, $
           SDTSTRUCT=struc, $
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
        aRange__temp = !NULL
        eRange__temp = !NULL
        ;; IF N_ELEMENTS(also_msph_sourcecone) GT 0 THEN BEGIN
        ;; IF KEYWORD_SET(msph_sc_dens[k]) OR KEYWORD_SET(msph_sc_temp[k]) THEN BEGIN
        ;;    aRange__dens = also_msph_sourcecone[k] ? 'source' : !NULL
        ;; ENDIF
        IF KEYWORD_SET(msph_sc_dens[k]) OR KEYWORD_SET(msph_sc_temp[k]) THEN BEGIN
           ;; Make sure there isn't a conflict
           IF N_ELEMENTS(aRange__dens_list) GT 0 THEN $
              IF N_ELEMENTS(aRange__dens_list[k]) EQ 2 THEN BEGIN
              PRINT,"You realize there's a probable conflict, eh?"
              PRINT,"You're using the msphere sc to get dens, but you have also defined these ranges:"
              PRINT,aRange__dens_list[k]
              STOP
           ENDIF

           aRange__dens    = 'source'
        ENDIF ELSE IF N_ELEMENTS(aRange__dens_list[k]) GT 0 THEN BEGIN
           aRange__dens = aRange__dens_list[k]
        ENDIF
        
        IF KEYWORD_SET(msph_sc_temp[k]) THEN BEGIN
           aRange__temp = 'source'
        ENDIF ELSE IF N_ELEMENTS(aRange__temp_list[k]) GT 0 THEN BEGIN
           aRange__temp = aRange__temp_list[k]
        ENDIF

        aRange__moments = N_ELEMENTS(aRange__moments_list[k]) GT 0 ? aRange__moments_list[k] : angleRange
        aRange__peakEn  = N_ELEMENTS(aRange__peakEn_list[k] ) GT 0 ? aRange__peakEn_list[k]  : angleRange
        aRange__charE   = N_ELEMENTS(aRange__charE_list[k]  ) GT 0 ? aRange__charE_list[k]   : angleRange

        IF SIZE(aRange__dens,/TYPE) EQ 7 THEN BEGIN

           CASE 1 OF
              STRMATCH(STRUPCASE(aRange__dens[0]),'SOURCE'): BEGIN

                 ;; scw = 150
                 scw = N_ELEMENTS(msph_sourcecone_halfWidth) GT 0 ? msph_sourcecone_halfWidth : 150

                 IF north_south[0] EQ -1 THEN BEGIN
                    aRange__dens = [180.-scw,180+scw]
                 ENDIF ELSE BEGIN
                    aRange__dens = [360.-scw,scw]
                 ENDELSE

              END
              ELSE: STOP
           ENDCASE

        ENDIF
        
        IF SIZE(aRange__temp,/TYPE) EQ 7 THEN BEGIN

           CASE 1 OF
              STRMATCH(STRUPCASE(aRange__temp[0]),'*LC'): BEGIN

                 IF STRLEN(aRange__temp[0]) GT 2 THEN BEGIN
                    factor = FLOAT(STRSPLIT(STRUPCASE(aRange__temp[0]),'LC',/EXTRACT))
                    IF north_south[0] EQ -1 THEN BEGIN
                       aRange__temp = [180.-factor*lcw,180+factor*lcw]
                    ENDIF ELSE BEGIN
                       aRange__temp = [360.-factor*lcw,factor*lcw]
                    ENDELSE
                 ENDIF ELSE BEGIN
                    aRange__temp = lc_angleRange
                 ENDELSE

                 IF upgoing THEN aRange__temp = (360.*((aRange__temp-180)/360.-FLOOR((aRange__temp-180)/360.)))

              END
              STRMATCH(STRUPCASE(aRange__temp[0]),'*ALL__EXCL_ATM'): BEGIN

                 IF STRLEN(aRange__temp[0]) GT 12 THEN BEGIN
                    factor = FLOAT(STRSPLIT(STRUPCASE(aRange__temp[0]),'ALL__EXCL_ATM',/EXTRACT))
                 ENDIF ELSE BEGIN
                    factor = 1
                 ENDELSE
                 
                 ;; OLD 2018/01/12
                 ;; IF north_south[0] EQ -1 THEN BEGIN
                 ;;    ;; aRange__temp = [180.-factor*lcw,180+factor*lcw]
                 ;;    aRange__temp = [factor*lcw,360.-factor*lcw]
                 ;; ENDIF ELSE BEGIN
                 ;;    ;; aRange__temp = [(-180.)+factor*lcw,180.-factor*lcw]
                 ;;    aRange__temp = [180.+factor*lcw,180.-factor*lcw]
                 ;; ENDELSE

                 ;;NEW 2018/01/12
                 aRange__temp = allExclAtm_aRange

                 ;; ENDIF ELSE BEGIN
                 ;;    ;;you're getting the loss cone, not the whole thing excluding the loss cone
                 ;;    STOP
                 ;;    aRange__temp = lc_angleRange
                 ;; ENDELSE

              END
              STRMATCH(STRUPCASE(aRange__temp[0]),'SOURCE'): BEGIN

                 ;; scw = 150
                 scw = N_ELEMENTS(msph_sourcecone_halfWidth) GT 0 ? msph_sourcecone_halfWidth : 150

                 IF north_south[0] EQ -1 THEN BEGIN
                    aRange__temp = [180.-scw,180+scw]
                 ENDIF ELSE BEGIN
                    aRange__temp = [360.-scw,scw]
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
              STRMATCH(STRUPCASE(aRange__moments[0]),'*ALL__EXCL_ATM'): BEGIN

                 IF STRLEN(aRange__moments[0]) GT 12 THEN BEGIN
                    factor = FLOAT(STRSPLIT(STRUPCASE(aRange__moments[0]),'ALL__EXCL_ATM',/EXTRACT))
                 ENDIF ELSE BEGIN
                    factor = 1
                 ENDELSE
                 
                 ;; OLD 2018/01/12
                 ;; IF north_south[0] EQ -1 THEN BEGIN
                 ;;    ;; aRange__moments = [180.-factor*lcw,180+factor*lcw]
                 ;;    aRange__moments = [factor*lcw,360.-factor*lcw]
                 ;; ENDIF ELSE BEGIN
                 ;;    ;; aRange__moments = [(-180.)+factor*lcw,180.-factor*lcw]
                 ;;    aRange__moments = [180.+factor*lcw,180.-factor*lcw]
                 ;; ENDELSE

                 ;;NEW 2018/01/12
                 aRange__moments = allExclAtm_aRange

                 ;; ENDIF ELSE BEGIN
                 ;;    ;;you're getting the loss cone, not the whole thing excluding the loss cone
                 ;;    STOP
                 ;;    aRange__moments = lc_angleRange
                 ;; ENDELSE

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
        aRange_oTemp_list.Add,aRange__temp

        ;;Summary kind
        PRINT,FORMAT='("*****",A0,"*****")',STRUPCASE(label[k])
        IF KEYWORD_SET(aRange__dens) THEN BEGIN
           PRINT,FORMAT='(A0,T30,":",T35,2(F0.2,:,","))',"aRange__dens",aRange__dens
        ENDIF
        PRINT,FORMAT='(A0,T30,":",T35,2(F0.2,:,","))',"aRange__moments",aRange__moments
        PRINT,FORMAT='(A0,T30,":",T35,2(F0.2,:,","))',"aRange__peakEn",aRange__peakEn
        PRINT,FORMAT='(A0,T30,":",T35,2(F0.2,:,","))',"aRange__charE",aRange__charE
        IF KEYWORD_SET(aRange__temp) THEN BEGIN
           PRINT,FORMAT='(A0,T30,":",T35,2(F0.2,:,","))',"aRange__temp",aRange__temp
        ENDIF
        IF KEYWORD_SET(eRange__temp) THEN BEGIN
           PRINT,FORMAT='(A0,T30,":",T35,2(F0.2,:,","))',"eRange__temp",eRange__temp
        ENDIF
        ;; PRINT,FORMAT='(A0,T30,":",T35,2(F0.2,:,","))',"angleRange",angleRange
        PRINT,FORMAT='(A0,T30,":",T35,2(F0.2,:,","))',"energy",MEAN(energy,DIMENSION=2)
        PRINT,FORMAT='(A0,T30,":",T35,2('+minpeType+',:,","))',"min_peak_energy",min_peak_energy
        PRINT,FORMAT='(A0,T30,":",T35,2('+maxpeType+',:,","))',"max_peak_energy",max_peak_energy
        ;; PRINT,FORMAT='(A0,T30,":",T35,2(F0.2,:,","))',"max_peak_energy",max_peak_energy
        PRINT,FORMAT='(A0,T30,":",T35,2(F0.2,:,","))',"peak_energy__start_at_highE",peak_energy__start_at_highE
        PRINT,FORMAT='(A0,T30,":",T35,2(F0.2,:,","))',"upgoing",upgoing
        PRINT,""

        PRINT,"Getting eSpecs ..."
        eSpecUnits            = 'eflux'
        eSpec                 = GET_EN_SPEC__FROM_DIFF_EFLUX( $
                                diff_eFlux, $
                                /RETRACE, $
                                IS_MCFADDEN_DIFF_EFLUX=array_of_structs_instead, $
                                ANGLE=aRange__peakEn, $
                                UNITS=eSpecUnits, $
                                OUT_TIME=out_time)
        eSpec_list.Add,eSpec

        IF KEYWORD_SET(also_oneCount) THEN BEGIN
           oneCount_eSpec     = GET_EN_SPEC__FROM_DIFF_EFLUX( $
                                dEF_oneCount, $
                                /RETRACE, $
                                IS_MCFADDEN_DIFF_EFLUX=array_of_structs_instead, $
                                ANGLE=aRange__peakEn, $
                                UNITS=eSpecUnits)
        ENDIF
        
        angles                = TRANSPOSE(diff_eFlux.theta,[1,0,2])
        energies              = TRANSPOSE(diff_eFlux.energy,[1,0,2])
        nEnergies             = N_ELEMENTS(eSpec.v[0,*])

        iAngle                = 0
        nHere                 = N_ELEMENTS(diff_eFlux.time)
        peak_indArr           = MAKE_ARRAY(nHere,VALUE=-999,/LONG)
        peak_energyArr        = MAKE_ARRAY(nHere,VALUE=-999,/FLOAT)
        peak_dEArr            = MAKE_ARRAY(nHere,VALUE=-999,/FLOAT)
        peak_EBoundsArr       = MAKE_ARRAY(2,nHere,VALUE=-999,/FLOAT)
        peakE_indShift        = KEYWORD_SET(peakE_bounds_indShift) ? peakE_bounds_indShift : [0,0]

        IF N_ELEMENTS(eSpec.x) NE nHere THEN STOP

        IF has_minpe_struct THEN BEGIN

           minpe_arr          = KAPPA__MAKE_ENERGY_ARR_FOR_SPECIFIED_TBOUNDS( $
                                eSpec.x, $
                                min_peak_energy_tStruct, $
                                k)

        ENDIF

        IF has_maxpe_struct THEN BEGIN

           maxpe_arr          = KAPPA__MAKE_ENERGY_ARR_FOR_SPECIFIED_TBOUNDS( $
                                eSpec.x, $
                                max_peak_energy_tStruct, $
                                k)

        ENDIF

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
           
           CASE 1 OF
              has_minpe_struct: BEGIN
                 minpe = minpe_arr[iTime]
                 ;; PRINT,"MINPE : ",minpe
              END
              ELSE: BEGIN
                 minpe = KEYWORD_SET(min_peak_energy) ? min_peak_energy : !NULL
              END
           ENDCASE

           IF KEYWORD_SET(minpe) THEN BEGIN
              ;;Try taking it from the top
              min_peak_ind    = MAX(WHERE(REFORM(XorigArr[0,*]) GE minpe))
              IF min_peak_ind EQ -1 THEN BEGIN
                 STOP
              ENDIF
           ENDIF ELSE BEGIN
              min_peak_ind    = nEnergies-1
           ENDELSE

           CASE 1 OF
              has_maxpe_struct: BEGIN
                 maxpe = maxpe_arr[iTime]
                 ;; PRINT,"MAXPE : ",maxpe
              END
              ELSE: BEGIN
                 maxpe = KEYWORD_SET(max_peak_energy) ? max_peak_energy : !NULL
              END
           ENDCASE


           KAPPA__GET_PEAK_IND_AND_PEAK_ENERGY, $
              Xorig,Yorig, $
              peak_ind,peak_energy, $
              NENERGIES=nEnergies, $
              MAXEIND=maxEInd, $
              MINEIND=minEInd, $
              ENERGY_INDS=energy_inds, $
              ERANGE_FIT=eRange_fit, $
              N_BELOW_PEAK=n_below_peak, $
              N_ABOVE_PEAK=n_above_peak, $
              BULK_OFFSET=bulk_offset, $
              CHECK_FOR_HIGHER_FLUX_PEAKS=check_higher_peaks_set_peakEn, $
              MIN_PEAK_ENERGY=minpe, $
              MAX_PEAK_ENERGY=maxpe, $
              PEAK_ENERGY__START_AT_HIGHE=peak_energy__start_at_highE, $
              /CONTINUE_IF_NOMATCH, $
              /TEST_NOREV, $
              ONECOUNT_STR=oneCurve, $
              WHICHWY=whichWy

           ;;Note that while these are called maxE and minE, suggesting they refer to the max energy and min energy, they do NOT. 
           ;;Rather, they refer to the lowest and highest indices falling within the user-specified parameters 
           ;;  for fitting—namely, n_below_peak and n_above_peak
           
           ;; OLD 2018/01/12
           ;; maxEInd                   = (peak_ind + n_below_peak) < (nEnergies-1)
           ;; minEInd                   = (peak_ind - n_above_peak) > 0

           ;; NEW 2018/01/12; use as many as possible above peak
           maxEInd                   = (peak_ind + n_below_peak) < (nEnergies-1)
           minEInd                   = (peak_ind - 47          ) > 0

           peak_dEArr[iTime]         = eSpec.vErr[iTime,peak_ind]
           peak_energyArr[iTime]     = TEMPORARY(peak_energy)
           peak_indArr[iTime]        = TEMPORARY(peak_ind)
           peak_EBoundsArr[*,iTime]  = [Xorig[(TEMPORARY(maxEInd)-peakE_indShift[0]*whichWy) < (nEnergies-1)], $
                                        Xorig[(TEMPORARY(minEInd)-peakE_indShift[1]*whichWy) > 0]]
        ENDFOR

        ;; 2018/03/19
        ;; Trying to figure out why identification of peaks in ion data is so bad
        ;; iTime = 89 & xOrig = reform(espec.v[itime,*]) & yOrig = reform(espec.y[itime,*]) & tmpInds = WHERE(xOrig GE minpe AND xOrig LE maxpe) & maxFlux = MAX(yOrig[tmpInds],maxInd_ii) & PRINT,FORMAT='(A0,G0.2," (",G0.2,")")',"Max flux (edgery): ",maxFlux,xOrig[tmpInds[maxInd_ii]] & junkPlot = PLOT(xOrig,yOrig,/xlog,/ylog,title=T2S(eSpec.x[iTime],/mS),YRANGE=MINMAX(yOrig) > 1E4) & linePlot = PLOT(REPLICATE(xOrig[tmpInds[maxInd_ii]],12),10.^(LINDGEN(12)),LINESTYLE='--',COLOR='RED',/OVERPLOT,YRANGE=MINMAX(yOrig) > 1E4)

        kill = WHERE(peak_energyArr LT -0.5,/NULL)
        peak_energyarr[kill] = 0.
        peak_dEArr[kill]     = 0.
        peak_ind_list.Add,TEMPORARY(peak_indArr)
        peak_energy_list.Add,TEMPORARY(peak_energyArr)
        peak_dE_list.Add,TEMPORARY(peak_dEArr)
        peak_eBounds_list.Add,peak_EBoundsArr

        IF KEYWORD_SET(use_peakE_bounds_for_moments) THEN BEGIN
           energy[0,*]               = peak_EBoundsArr[0,*] > energy[0,*]
           energy[1,*]               = peak_EBoundsArr[1,*] < energy[1,*]
        ENDIF

        IF KEYWORD_SET(e_above_peak_temp[k]) THEN BEGIN
           energyArr_forTemp[0,*]    = peak_EBoundsArr[0,*] > energy[0,*]
           energyArr_forTemp[1,*]    = peak_EBoundsArr[1,*] < energy[1,*]
           eRange__temp              = TEMPORARY(energyArr_forTemp)
        ENDIF ELSE IF N_ELEMENTS(eRange__temp_list[k]) GT 0 THEN BEGIN
           eRange__temp              = eRange__temp_list[k]
        ENDIF

        MOMENT_SUITE_2D,diff_eFlux, $
                        ENERGY=energy, $
                        ARANGE__DENS=aRange__dens, $
                        ARANGE__MOMENTS=aRange__moments, $
                        ARANGE__CHARE=aRange__charE, $
                        ARANGE__TEMP=aRange__temp, $
                        ERANGE__TEMP=eRange__temp, $
                        SC_POT=tmpSc_pot, $
                        EEB_OR_EES=eeb_or_ees, $
                        ERROR_ESTIMATES=error_estimates, $
                        ;; MAP_TO_100KM=map_to_100km, $ 
                        ORBIT=orbit, $
                        /NEW_MOMENT_ROUTINE, $
                        QUIET=quiet, $
                        MCFADDEN_STYLE_DIFF_EFLUX=array_of_structs_instead, $
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
                        INOUT_MAPRATIO=mapRatio, $
                        OUT_STRUCT=momStruct, $
                        BATCH_MODE=batch_mode

        IF KEYWORD_SET(also_oneCount) THEN BEGIN

           MOMENT_SUITE_2D,dEF_oneCount, $
                           ENERGY=energy, $
                           ARANGE__DENS=aRange__dens, $
                           ARANGE__MOMENTS=aRange__moments, $
                           ARANGE__CHARE=aRange__charE, $
                           ARANGE__TEMP=aRange__temp, $
                           ERANGE__TEMP=eRange__temp, $
                           SC_POT=tmpSc_pot, $
                           EEB_OR_EES=eeb_or_ees, $
                           ERROR_ESTIMATES=error_estimates, $
                           ;; MAP_TO_100KM=map_to_100km, $ 
                           ORBIT=orbit, $
                           /NEW_MOMENT_ROUTINE, $
                           QUIET=quiet, $
                           MCFADDEN_STYLE_DIFF_EFLUX=array_of_structs_instead, $
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
                           INOUT_MAPRATIO=mapRatio, $
                           BATCH_MODE=batch_mode ;; , $
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
        mapRatio_list.Add,mapRatio
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

        momsforTStruct = !NULL
        STR_ELEMENT,momStruct,'moments_forT',momsforTStruct
        IF SIZE(momsforTStruct,/TYPE) EQ 8 THEN BEGIN
           ;;If k isn't 0, this isn't curPotList[edind]
           IF k NE 0 THEN STOP ELSE BEGIN
              momsforT_list.Add,TEMPORARY(momsforTStruct)
              momsforT_ind = k
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

        theseInds     = WHERE( ( time_list[0] GE tmpT1 ) AND $
                               ( time_list[0] LE tmpT2 ), $
                               nThese)

        IF nThese EQ 0 THEN STOP

        theseInds     = CGSETINTERSECTION(theseInds,UNIQ((time_list[0]),SORT((time_list[0]))),COUNT=nThese)
        CHECK_SORTED,(time_list[0])[theseInds],is_sorted,/QUIET
        IF ~is_sorted THEN STOP
        IF nThese EQ 0 THEN STOP

        itvlTime      = [itvlTime,(time_list[0])[theseInds]]

     ENDFOR

     ;; ENDIF

     magCurrent = GET_CURRENT_FROM_FLUXMAG(t1,t2, $
                                           magStr,velocityStr, $
                                           /USE_DESPUN, $
                                           SDTNAME__JMAG=jMagName, $
                                           INFERRED_E_NUMFLUX=inferred_e_numFlux, $
                                           SDTNAME__INFERRED_E_NUMFLUX=e_numFluxName, $
                                           /STRANGEWAY_DECIMATE, $
                                           /EXIT_ON_PROBLEMS, $
                                           /QUIET)

     magCurrent = DATA_CUT(magCurrent,itvlTime)

     IF ~(KEYWORD_SET(jv_theor__initial_source__equator) OR KEYWORD_SET(jv_theor__initial_source__Polar) OR $
          KEYWORD_SET(jv_theor__initial_source_R_E) OR KEYWORD_SET(to_km)) $
     THEN jv_theor__initial_source__Polar = 1

     
     mRatio = GET_FA_MIRROR_RATIO__UTC(time_list[0], $
                                       /TIME_ARRAY, $
                                       ;; USE_FAST_AS_IONOS=~KEYWORD_SET(map_to_100km), $
                                       TO_EQUATOR=jv_theor__initial_source__equator, $
                                       TO_POLAR_SATELLITE=jv_theor__initial_source__Polar, $
                                       TO_THIS_RE=jv_theor__initial_source_R_E, $
                                       TO_THIS_KM=to_km)


     afterString = "Made "

     SAVE,t1,t2, $
          blacklist_list, $
          north_southArr_list, $
          lc_angleRange, $
          err_list, $
          err1_list, $
          time_list, $
          n_list, $
          j_list, $
          je_list, $
          cur_list, $
          mapRatio_list, $
          chare_list, $
          T_list, $
          nerr_list, $
          jerr_list, $
          jeErr_list, $
          curErr_list, $
          charEErr_list, $
          source_list, $
          source_ind, $
          momsforT_list, $
          momsforT_ind, $
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
          eSpec_list, $
          peak_ind_list, $
          peak_energy_list,peak_dE_list,peak_eBounds_list, $
          aRange_oMoments_list, $
          aRange_oCharE_list, $
          aRange_oPeakEn_list, $
          diff_eFlux_files, $
          magCurrent, $
          mRatio, $
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

     IF KEYWORD_SET(blacklist_list[k]) THEN BEGIN
        PRINT,"Can't do this one, you know ..."
        curPotList.Add,{time:-1,label:label[k]}
        CONTINUE
     ENDIF

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
     itvlESpecX     = !NULL
     itvlESpecY     = !NULL
     itvlESpecYErr  = !NULL
     itvlESpecV     = !NULL
     itvlESpecVErr  = !NULL
     itvlcharE      = !NULL
     itvlPeakE      = !NULL
     itvlT          = !NULL
     itvlCur        = !NULL
     itvlNerr       = !NULL
     itvlJerr       = !NULL
     itvlJeErr      = !NULL
     itvlcharEErr   = !NULL
     itvlPeakdE     = !NULL
     itvlPeakEBounds = !NULL
     itvlTerr       = !NULL
     itvlCurErr     = !NULL
     itvlErrors     = !NULL
     itvlMapRatio   = !NULL

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
        tmpMapRatio         = (mapRatio_list[k])[theseInds]
        tmpCharE            = (chare_list[k])[theseInds]
        tmpPeakE            = (peak_energy_list[k])[theseInds]
        tmpPeakdE           = (peak_dE_list[k])[theseInds]
        tmpPeakEBounds      = (peak_eBounds_list[k])[*,theseInds]
        tmpT                = (T_list[k])[*,theseInds]
        ;; tmpeSpecX           = {x: (eSpec_list[k]).x[theseInds], $
        ;;                        y: (eSpec_list[k]).y[theseInds,*], $
        ;;                        v: (eSpec_list[k]).v[theseInds,*]}
        tmpESpecX           = (eSpec_list[k]).x[theseInds]
        tmpESpecY           = (eSpec_list[k]).y[theseInds,*]
        tmpESpecYErr        = (eSpec_list[k]).yErr[theseInds,*]
        tmpESpecV           = (eSpec_list[k]).v[theseInds,*]
        tmpESpecVErr        = (eSpec_list[k]).vErr[theseInds,*]
        
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
        

        IF N_ELEMENTS(momsforT_ind) GT 0 THEN BEGIN
           IF k EQ momsforT_ind THEN BEGIN
              momsForT_inds = theseInds
           ENDIF
        ENDIF
        
        ;; IF KEYWORD_SET(error_estimates) AND KEYWORD_SET(dens_errors) THEN BEGIN
        IF KEYWORD_SET(error_estimates) THEN BEGIN

           tmpNerr          = (nerr_list[k])[theseInds]
           tmpJerr          = (jerr_list[k])[theseInds]
           tmpJeErr         = (jeErr_list[k])[theseInds]
           tmpCurErr        = (curErr_list[k])[theseInds]
           tmpcharEErr      = (charEErr_list[k])[theseInds]
           tmpTerr          = (Terr_list[k])[*,theseInds]
           ;; SHRINK_GERSHMAN_ERROR_STRUCT,err_list[k],theseInds,tmpErrors

           IF KEYWORD_SET(also_oneCount) THEN BEGIN
              tmpN1err      = (n1err_list[k])[theseInds]
              tmpJ1err      = (j1err_list[k])[theseInds]
              tmpJe1Err     = (je1Err_list[k])[theseInds]
              tmpCur1Err    = (cur1Err_list[k])[theseInds]
              tmpcharE1Err  = (charE1Err_list[k])[theseInds]
              tmpT1err      = (T1err_list[k])[*,theseInds]
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
        itvlMapRatio         = [itvlMapRatio  , tmpMapRatio ]

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

        ;; itvlESpec            = [itvlESpec     ,tmpESpec     ]
        itvlESpecX           = [itvlESpecX    ,tmpESpecX    ]
        itvlESpecY           = [itvlESpecY    ,tmpESpecY    ]
        itvlESpecYErr        = [itvlESpecYErr ,tmpESpecYErr ]
        itvlESpecV           = [itvlESpecV    ,tmpESpecV    ]
        itvlESpecVErr        = [itvlESpecVErr ,tmpESpecVErr ]
        itvlPeakE            = [itvlPeakE     ,tmpPeakE     ]
        itvlPeakdE           = [itvlPeakdE    ,tmpPeakdE    ]
        itvlPeakEBounds      = [itvlPeakEBounds,tmpPeakEBounds    ]

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
                             mapRatio : TEMPORARY(itvlMapRatio), $
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
                             ;; mapRatio : tmpStruct.mapRatio    , $
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

           bonus          = {eSpec    : {x:TEMPORARY(itvlESpecX), $
                                         y:TEMPORARY(itvlESpecY), $
                                         v:TEMPORARY(itvlESpecV), $
                                         yErr:TEMPORARY(itvlESpecYErr), $
                                         vErr:TEMPORARY(itvlESpecVErr)}, $
                             ;; TEMPORARY(itvlESpec)               , $
                             peakE    : TEMPORARY(itvlPeakE)               , $
                             peakErr  : TEMPORARY(itvlPeakdE)              , $
                             peakEBounds : TEMPORARY(itvlPeakEBounds)      , $
                             energy   : moment_energyArr[*,k]              , $
                             angles   : {charE   : aRange_oCharE_list[k]   , $
                                         moments : aRange_oMoments_list[k] , $
                                         peakEn  : aRange_oPeakEn_list[k]} $
                            }

           tmpStruct      = CREATE_STRUCT(TEMPORARY(tmpStruct),TEMPORARY(bonus))
           
           IF N_ELEMENTS(source_ind) GT 0 THEN BEGIN
              IF k EQ source_ind THEN BEGIN
                 source_struct = {source      : source_list[0], $
                                  source_inds : TEMPORARY(itvlInds)}

                 tmpStruct   = CREATE_STRUCT(TEMPORARY(tmpStruct),TEMPORARY(source_struct))
              ENDIF
           ENDIF

           IF N_ELEMENTS(momsforT_ind) GT 0 THEN BEGIN
              IF k EQ momsforT_ind THEN BEGIN
                 momsforT_struct = {momsForT : momsforT_list[0], $
                                    momsForT_inds : TEMPORARY(momsForT_inds)}

                 tmpStruct   = CREATE_STRUCT(TEMPORARY(tmpStruct),TEMPORARY(momsforT_struct))
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
                             cur      : TEMPORARY(itvlCur)    , $
                             mapRatio : TEMPORARY(itvlMapRatio)}

           IF KEYWORD_SET(also_oneCount) THEN BEGIN
              tmp1Struct  = {j1       : TEMPORARY(itvlJ1)     , $
                             je1      : TEMPORARY(itvlJe1)    , $
                             cur1     : TEMPORARY(itvlCur1)   , $
                             mapRatio : tmpStruct.mapRatio    , $
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
