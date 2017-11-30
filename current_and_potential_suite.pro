;2017/03/22
PRO PRINT_CURRENT_AND_POTENTIAL_SUMMARY,jvPlotData,useInds

  nUsers       = N_ELEMENTS(useInds)
  useInds      = useInds[SORT(jvplotdata.time[useInds])]

  CURANDPOT__SELECT_T_AND_N,jvPlotData, $ ;avgs_JVfit, $
                            TEMPERATURE=Temperature, $
                            DENSITY=Density, $
                            ERR_TEMPERATURE=TemperatureErr, $
                            ERR_DENSITY=DensityErr, $
                            /SKIP_USEINDS, $
                            ;; THESE_USEINDS=useInds, $
                            /ARRAYS
  ;; CASE 1 OF
  ;;    KEYWORD_SET(jvPlotData.use_source_avgs): BEGIN

  ;;       Temperature    = JVPlotData.source.TDown
  ;;       Density        = JVPlotData.source.NDown
  ;;       TemperatureErr = JVPlotData.source.TDownErr
  ;;       DensityErr     = JVPlotData.source.NDownErr

  ;;    END
  ;;    ELSE: BEGIN

  ;;       Temperature    = JVPlotData.TDown
  ;;       Density        = JVPlotData.NDown
  ;;       TemperatureErr = JVPlotData.TDownErr
  ;;       DensityErr     = JVPlotData.NDownErr

  ;;    END
  ;; ENDCASE

  

  PRINT,FORMAT='(A0,T5,A0,T30,A0,T40,A0,T50,A0,T60,A0,T70,A0,T80,A0,T90,A0)', $
        'i','Time','Temp','N','Pot','Current','TFracErr','NFracErr','JFracErr'
  FOR k=0,nUsers-1 DO BEGIN

     PRINT,FORMAT='(I0,T5,A0,T30,F-8.1,T40,F-8.3,T50,F-8.1,T60,F-8.3,T70,F-8.1,T80,F-8.3,T90,F-8.3)', $
           k, $
           TIME_TO_STR(JVPlotData.time[useInds[k]],/MS), $
           Temperature[useInds[k]], $
           Density[useInds[k]], $
           JVPlotData.pot[useInds[k]], $
           JVPlotData.cur[useInds[k]], $
           TemperatureErr[useInds[k]]/Temperature[useInds[k]], $
           DensityErr[useInds[k]]/Density[useInds[k]], $
           ABS(JVPlotData.curErr[useInds[k]]/JVPlotData.cur[useInds[k]])
     
  ENDFOR
  PRINT,FORMAT='(A0,T30,F-8.3,T40,F-8.3,T50,F-8.3,T60,G-8.3)', $
        "Avg", $
        MEAN(Temperature[useInds]), $
        MEAN(Density[useInds]), $
        MEAN(JVPlotData.pot[useInds]), $
        MEAN(JVPlotData.cur[useInds])
END
PRO CURRENT_AND_POTENTIAL_SUITE, $
   PLOT_T1=plot_t1, $
   PLOT_T2=plot_t2, $
   USE_ALL_CURRENTS=use_all_currents, $
   USE_DOWNGOING_ELECTRON_CURRENT=use_ed_current, $
   USE_UPGOING_ION_CURRENT=use_iu_current, $
   USE_UPGOING_ELECTRON_CURRENT=use_eu_current, $
   USE_MAGNETOMETER_CURRENT=use_mag_current, $
   USE_CHAR_EN_FOR_DOWNPOT=use_charE_for_downPot, $
   USE_PEAK_EN_FOR_DOWNPOT=use_peakE_for_downPot, $
   ADD_UPGOING_ION_POT=add_iu_pot, $
   T_PLUSMINUSFAC_FOR_POT=T_plusMinusFac_for_pot, $
   ERROR_BAR_FACTOR=errorBarFac, $
   USEI__RELCHANGE=useInds__relChange, $
   USEI__TWOLUMPS=useInds__twoLumps, $
   EN_SPEC__NAMES=en_spec__names, $
   EN_SPEC__EEB_OR_EES=en_spec__eeb_or_ees, $
   EN_SPEC__ANGLE_RANGES=en_spec__angle_ranges, $
   EN_SPEC__ENERGY_RANGES=en_spec__energy_ranges, $
   EN_SPEC__UPGOING=en_spec__upgoing, $
   JV_A_LA_ELPHIC__UPCURRENT_CONDUCTIVITIES=upCurrent_conductivities, $
   JV_THEOR__MINPOT=jv_theor__minPot, $
   JV_THEOR__MAXPOT=jv_theor__maxPot, $
   JV_THEOR__MINCUR=jv_theor__minCur, $
   JV_THEOR__MAXCUR=jv_theor__maxCur, $
   JV_THEOR__PLOT_J_RATIOS=plot_j_ratios, $
   JV_THEOR__PLOT_ION_ELEC_RATIOS=plot_ion_elec_ratios, $
   JV_THEOR__FIT_TIME_SERIES=JV_theor__fit_time_series, $
   JV_THEOR__R_B_INIT=jv_theor__R_B_init, $
   JV_THEOR__KAPPA_INIT=jv_theor__kappa_init, $
   JV_THEOR__KAPPALIMS=kappaLims, $   
   JV_THEOR__TEMPLIMS=TempLims, $    
   JV_THEOR__DENSLIMS=DensLims, $    
   JV_THEOR__MAGRATIOLIMS=magRatioLims, $
   JV_THEOR__FIT_JE=jv_theor__fit_je, $
   JV_THEOR__FIT_BOTH=jv_theor__fit_both, $
   JV_THEOR__INITIAL_SOURCE_R_E=jv_theor__initial_source_R_E, $
   JV_THEOR__INITIAL_SOURCE__POLARSAT=jv_theor__initial_source__Polar, $
   JV_THEOR__INITIAL_SOURCE__EQUATOR=jv_theor__initial_source__equator, $
   ;; JV_THEOR__ITERATIVE_DENSITY_AND_R_B_GAME=jv_theor__iterative_game, $
   JV_THEOR__ITERATIVE_RBDENS_GAME=jv_theor__iterative_RBDens_game, $
   JV_THEOR__ITERGAME__DENSFAC=jv_theor__itergame_densFac, $
   JV_THEOR__ITERGAME_TIE_R_B_AND_DENS=jv_theor__itergame_tie_R_B_and_dens, $
   ;; JV_THEOR__ADD_DENTON_ET_AL_2006_MODEL_COEFFS=add_Denton2006, $
   JVPOTBAR__J_ON_YAXIS=jvPotBar__j_on_yAxis, $
   JVPOTBAR__INTERACTIVE_OVERPLOT=interactive_overplot, $
   MAP__MULTI_MAGRATIO_ARRAY=map__multi_magRatio_array, $
   MAP__MULTI_KAPPA_ARRAY=map__multi_kappa_array, $
   MAP__2D=map__2D, $
   TN_YLOG_NDOWN=TN_yLog_nDown, $
   PLOT_J_V_POTBAR=plot_j_v_potBar, $
   PLOT_JV_A_LA_ELPHIC=plot_jv_a_la_Elphic, $
   PLOT_T_AND_N=plot_T_and_N, $
   PLOT_J_V_AND_THEORY=plot_j_v_and_theory, $
   PLOT_J_V__FIXED_T_AND_N=plot_j_v__fixed_t_and_n, $
   PLOT_J_V_MAP__R_B_AND_KAPPA__FIXED_T_AND_N=plot_j_v_map__r_b_and_kappa__fixed_t_and_n, $
   PLOT_MAGCURRENT_VS_CURRENT=plot_magCurrent_vs_current, $
   PLOT_EN_SPECS=plot_en_specs, $
   EN_SPECS__MOVIE=en_specs__movie, $
   A_LA_ELPHIC_SPNAME=a_la_Elphic_spName, $
   JVPOTBAR_SPNAME=jvpotBar_spName, $
   TN_SPNAME=TN_spName, $
   JV_THEOR_SPNAME=JV_theor_spName, $
   J_V__FIXTANDN__SPNAME=j_v__fixTandN__spName, $
   J_V__FIXTANDN__SAVEPLOTDATA=j_v__fixTandN__savePlotData, $
   J_V__FIXTANDN__DATAFILENAME=j_v__fixTandN__dataFilename, $
   EN_SPEC__SPNAME=en_spec__spName, $
   OUT_CURPOTLIST=curPotList, $
   OUT_JVPLOTDATA=jvPlotData, $
   OUT_AVGS_FOR_FITTING=avgs_JVfit, $
   OUT_SC_POT=out_sc_pot, $
   PLOTDIR=plotDir, $
   _REF_EXTRA=e

  COMPILE_OPT IDL2,STRICTARRSUBS

  CURRENT_AND_POTENTIAL_ANALYSIS, $
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
     ;; ALSO_MSPH_SOURCECONE=also_msph_sourcecone, $
     ARANGE__DENS_E_DOWN=aRange__dens_e_down, $
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
     USE_SC_POT_FOR_LOWERBOUND=use_sc_pot_for_lowerbound, $
     POT__FROM_FA_POTENTIAL=pot__from_fa_potential, $
     POT__CHASTON_STYLE=pot__Chaston_style, $
     POT__FROM_FILE=pot__from_file, $
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

  CURRENT_AND_POTENTIAL_PLOTDATA_PREP,curPotList,jvPlotData, $
                                      T1=plot_t1, $
                                      T2=plot_t2, $
                                      USE_ALL_CURRENTS=use_all_currents, $
                                      USE_DOWNGOING_ELECTRON_CURRENT=use_ed_current, $
                                      USE_UPGOING_ION_CURRENT=use_iu_current, $
                                      USE_UPGOING_ELECTRON_CURRENT=use_eu_current, $
                                      USE_MAGNETOMETER_CURRENT=use_mag_current, $
                                      USE_CHAR_EN_FOR_DOWNPOT=use_charE_for_downPot, $
                                      USE_PEAK_EN_FOR_DOWNPOT=use_peakE_for_downPot, $
                                      ADD_UPGOING_ION_POT=add_iu_pot, $
                                      T_PLUSMINUSFAC_FOR_POT=T_plusMinusFac_for_pot, $
                                      ;; ALSO_MSPH_SOURCECONE=also_msph_sourcecone, $
                                      ;; USE_MSPH_SOURCE=jv_theor__use_msph_source, $
                                      USE_MSPH_SOURCECONE_FOR_DENS=use_msph_sourcecone_for_dens, $
                                      USE_MSPH_SOURCECONE_FOR_TEMP=use_msph_sourcecone_for_temp, $
                                      ERROR_BAR_FACTOR=errorBarFac, $
                                      USEI__RELCHANGE=useInds__relChange, $
                                      FRACCHANGE_NDOWN=fracChange_NDown, $
                                      FRACCHANGE_JDOWN=fracChange_JDown, $
                                      FRACCHANGE_TDOWN=fracChange_TDown, $
                                      FRACERROR_NDOWN=fracError_NDown, $
                                      FRACERROR_JDOWN=fracError_JDown, $
                                      FRACERROR_TDOWN=fracError_TDown, $
                                      USE_FRACERROR_NDOWN=use_fracError_NDown, $
                                      USE_FRACERROR_JDOWN=use_fracError_JDown, $
                                      USE_FRACERROR_TDOWN=use_fracError_TDown, $
                                      USEI__TWOLUMPS=useInds__twoLumps, $
                                      MAX_TDOWN=max_TDown, $
                                      MIN_TDOWN=min_TDown, $
                                      MAX_NDOWN=max_NDown, $
                                      MIN_NDOWN=min_NDown, $
                                      TRANGES=tRanges, $
                                      MINPOT=minPot, $
                                      MAXPOT=maxPot, $
                                      MINCUR=minCur, $
                                      MAXCUR=maxCur, $
                                      USEINDS=useInds, $
                                      PLOT_J_RATIOS=plot_j_ratios, $
                                      JV_THEOR__INITIAL_SOURCE_R_E=jv_theor__initial_source_R_E, $
                                      JV_THEOR__INITIAL_SOURCE__POLARSAT=jv_theor__initial_source__Polar, $
                                      JV_THEOR__INITIAL_SOURCE__EQUATOR=jv_theor__initial_source__equator, $
                                      MAP_TO_100KM=map_to_100km, $
                                      IN_MAGCURRENT=magCurrent, $
                                      OUT_AVGS_FOR_FITTING=avgs_JVfit, $
                                      _EXTRA=e

  ;; dir         = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/cur_and_pot_analysis/'

  ;; doSAVE = 1
  ;; IF doSAVE THEN BEGIN
  ;;    ;; plotDatFile = 'minusDat.dat'
  ;;    plotDatFile = 'plusDat.dat'

  ;;    PRINT,'Saving ' + plotDatFile + ' ...'
  ;;    SAVE,curpotlist,jvplotdata,avgs_JVfit, $
  ;;         plot_t1, $
  ;;         plot_t2, $
  ;;         use_all_currents, $
  ;;         use_ed_current, $
  ;;         use_iu_current, $
  ;;         use_eu_current, $
  ;;         use_mag_current, $
  ;;         use_charE_for_downPot, $
  ;;         use_peakE_for_downPot, $
  ;;         add_iu_pot, $
  ;;         T_plusMinusFac_for_pot, $
  ;;         errorBarFac, $
  ;;         useInds__relChange, $
  ;;         useInds__twoLumps, $
  ;;         en_spec__names, $
  ;;         en_spec__eeb_or_ees, $
  ;;         en_spec__angle_ranges, $
  ;;         en_spec__energy_ranges, $
  ;;         en_spec__upgoing, $
  ;;         upCurrent_conductivities, $
  ;;         jv_theor__minPot, $
  ;;         jv_theor__maxPot, $
  ;;         jv_theor__minCur, $
  ;;         jv_theor__maxCur, $
  ;;         plot_j_ratios, $
  ;;         plot_ion_elec_ratios, $
  ;;         JV_theor__fit_time_series, $
  ;;         jv_theor__R_B_init, $
  ;;         jv_theor__kappa_init, $
  ;;         kappaLims, $   
  ;;         TempLims, $    
  ;;         DensLims, $    
  ;;         magRatioLims, $
  ;;         jv_theor__fit_je, $
  ;;         jv_theor__fit_both, $
  ;;         jv_theor__initial_source_R_E, $
  ;;         jv_theor__initial_source__Polar, $
  ;;         jv_theor__initial_source__equator, $
  ;;         jv_theor__iterative_game, $
  ;;         jv_theor__itergame_NFac, $
  ;;         jv_theor__itergame_tie_R_B_and_dens, $
  ;;         ;; add_Denton2006, $
  ;;         jvPotBar__j_on_yAxis, $
  ;;         interactive_overplot, $
  ;;         map__multi_magRatio_array, $
  ;;         map__multi_kappa_array, $
  ;;         map__2D, $
  ;;         TN_yLog_nDown, $
  ;;         plot_j_v_potBar, $
  ;;         plot_jv_a_la_Elphic, $
  ;;         plot_T_and_N, $
  ;;         plot_j_v_and_theory, $
  ;;         plot_j_v__fixed_t_and_n, $
  ;;         plot_j_v_map__r_b_and_kappa__fixed_t_and_n, $
  ;;         plot_magCurrent_vs_current, $
  ;;         plot_en_specs, $
  ;;         en_specs__movie, $
  ;;         a_la_Elphic_spName, $
  ;;         jvpotBar_spName, $
  ;;         TN_spName, $
  ;;         JV_theor_spName, $
  ;;         j_v__fixTandN__spName, $
  ;;         en_spec__spName, $
  ;;         curPotList, $
  ;;         jvPlotData, $
  ;;         avgs_JVfit, $
  ;;         out_sc_pot, $
  ;;         plotDir, $
  ;;         orbit, $
  ;;         orbTimes, $
  ;;         orbBurstTimes, $
  ;;         bonusPref, $
  ;;         downTimesStr, $
  ;;         upTimesStr, $
  ;;         timesList, $
  ;;         units, $
  ;;         outDir, $
  ;;         masterFile, $
  ;;         remake_masterFile, $
  ;;         save_diff_eFlux_file, $
  ;;         load_diff_eFlux_file, $
  ;;         eeb_or_eesArr, $
  ;;         order, $
  ;;         label, $
  ;;         add_oneCount_stats, $
  ;;         use_msph_sourcecone_for_dens, $
  ;;         use_msph_sourcecone_for_temp, $
  ;;         ;; also_msph_sourcecone, $
  ;;         aRange__dens_e_down, $
  ;;         aRange__moments_e_down, $
  ;;         aRange__moments_e_up, $
  ;;         aRange__moments_i_up, $
  ;;         aRange__peakEn_e_down, $
  ;;         aRange__peakEn_e_up, $
  ;;         aRange__peakEn_i_up, $
  ;;         aRange__charE_e_down, $
  ;;         aRange__charE_e_up, $
  ;;         aRange__charE_i_up, $
  ;;         manual_angle_correction, $
  ;;         label__which_eeb, $
  ;;         label__which_times, $
  ;;         moment_energyArr, $
  ;;         use_sc_pot_for_lowerbound, $
  ;;         pot__from_fa_potential, $
  ;;         pot__Chaston_style, $
  ;;         pot__from_file, $
  ;;         pot__save_file, $
  ;;         aRange__moments_list, $
  ;;         aRange__peakEn_list, $
  ;;         aRange__charE_list, $
  ;;         Elphic1998_defaults, $
  ;;         min_peak_energyArr, $
  ;;         max_peak_energyArr, $
  ;;         peak_energy__start_at_highEArr, $
  ;;         upgoingArr, $
  ;;         error_estimates, $
  ;;         spectra_average_interval, $
  ;;         map_to_100km, $
  ;;         saveCurPotFile, $
  ;;         curPotList, $
  ;;         magCurrent, $
  ;;         out_sc_pot, $
  ;;         diff_eFlux_files, $
  ;;         out_sourcecone, $
  ;;         out_losscone, $
  ;;         FILENAME=dir+plotDatFile

  ;;    STOP

  ;; ENDIF

  ;; suppress_magRat_sum = ~(KEYWORD_SET(jv_theor__initial_source__equator) OR KEYWORD_SET(jv_theor__initial_source__Polar) OR $
  ;;                         KEYWORD_SET(jv_theor__initial_source_R_E) OR KEYWORD_SET(to_km))
  ;; ;; to_polar            = 0
  ;; ;; " Within its polar orbit with geocentric perigee and apogee of 1.8 and 9.0 RE, respectively … " Laakso et al. [2003]
  ;; ;;"… 9 RE (56,500 km) and a perigee of 2 RE (11,500 km) …" --https://directory.eoportal.org/web/eoportal/satellite-missions/p/polar
  ;; ;; to_RE               = 3
  ;; IF ~KEYWORD_SET(suppress_magRat_sum) THEN BEGIN

  ;;    junk = GET_FA_MIRROR_RATIO__UTC(JVPlotData.time, $
  ;;                                    /TIME_ARRAY, $
  ;;                                    TO_EQUATOR=jv_theor__initial_source__equator, $
  ;;                                    TO_POLAR_SATELLITE=jv_theor__initial_source__Polar, $
  ;;                                    TO_THIS_RE=jv_theor__initial_source_R_E, $
  ;;                                    TO_THIS_KM=to_km)

  ;;    add_Denton2006 = 0
  ;;    IF KEYWORD_SET(add_Denton2006) THEN BEGIN

  ;;       ne_F   = MEAN(jvPlotData.source.NDown[avgs_JVfit.useInds])
  ;;       mlt    = MEAN(junk.mlt[avgs_JVfit.useInds])
  ;;       RE_F   = MEAN(junk.R_E.fast[avgs_JVfit.useInds])
  ;;       Lshell = MEAN(junk.lshell.T[avgs_JVfit.useInds])

  ;;       dentonParams = GET_DENTON_ET_AL_2006__EQUATORIAL_DENSITY(ne_F,mlt,RE_F,Lshell)

  ;;    ENDIF

  ;;    STR_ELEMENT,jvPlotData,'mRatio',junk,/ADD_REPLACE

  ;; ENDIF

  IF KEYWORD_SET(plot_jv_a_la_Elphic) THEN BEGIN
     PLOT_THREEPANEL_ANALOG_TO_FIG2_ELPHIC_ETAL_1998,jvPlotData, $
        ORIGINAL_PLOTIDEE=orig_plotIdee, $
        SAVEPLOT=savePlot, $
        SPNAME=a_la_Elphic_spName, $
        UPCURRENT_CONDUCTIVITIES=upCurrent_conductivities, $
        ORIGINATING_ROUTINE=routName, $
        PLOTDIR=plotDir, $
        _EXTRA=e
  ENDIF

  IF KEYWORD_SET(plot_j_v_potBar) THEN BEGIN
     PLOT_J_VS_POTBAR,jvPlotData,avgs_jvFit, $
                      J_ON_YAXIS=jvPotBar__j_on_yAxis, $
                      SAVEPLOT=savePlot, $
                      SPNAME=jvpotBar_spName, $
                      INTERACTIVE_OVERPLOT=interactive_overplot, $
                      ORIGINATING_ROUTINE=routName, $
                      PLOTDIR=plotDir, $
                      _EXTRA=e
  ENDIF

  IF KEYWORD_SET(plot_T_and_N) THEN BEGIN
     PLOT_TEMPERATURE_AND_DENSITY_TSERIES, $
        jvPlotData, $
        ORIGINAL_PLOTIDEE=orig_plotIdee, $
        YLOG_NDOWN=TN_yLog_nDown, $
        USEI__TWOLUMPS=useInds__twoLumps, $
        USEINDS=useInds, $
        SAVEPLOT=savePlot, $
        SPNAME=TN_spName, $
        ORIGINATING_ROUTINE=routName, $
        PLOTDIR=plotDir, $
        OUT_WINDOW=window1, $
        OVERPLOTALL=overplotAll, $
        OVERPLOT_WINDOW=overplot_window, $
        _EXTRA=e
  ENDIF

  CURANDPOT__SELECT_T_AND_N,jvPlotData,avgs_JVfit, $
                            TEMPERATURE=Temperature, $
                            DENSITY=Density ;, $
                            ;;ARRAYS=arrays

                             ;;            kappa,       Temp,   Dens, R_B

  kappa_init = KEYWORD_SET(jv_theor__kappa_init) ? jv_theor__kappa_init : 10
  R_B_init   = KEYWORD_SET(jv_theor__R_B_init  ) ? jv_theor__R_B_init   : 1D3
  A_in       = KEYWORD_SET(A_in) ? A_in : [kappa_init, $     ;kappa
                                           Temperature, $ ;Temp
                                           Density, $ ;Dens
                                           R_B_init]           ;R_B
  IF KEYWORD_SET(plot_j_v_and_theory) THEN BEGIN

     PLOT_JV_DATA_AND_THEORETICAL_CURVES,jvPlotData, $
                                         CURPOTLIST=curPotList, $
                                         MINPOT=jv_theor__minPot, $
                                         MAXPOT=jv_theor__maxPot, $
                                         MINCUR=jv_theor__minCur, $
                                         MAXCUR=jv_theor__maxCur, $
                                         USEINDS=useInds, $
                                         FIT_JE=jv_theor__fit_je, $
                                         FIT_BOTH=jv_theor__fit_both, $
                                         PLOT_J_RATIOS=plot_j_ratios, $
                                         PLOT_ION_ELEC_RATIOS=plot_ion_elec_ratios, $
                                         ORIGINATING_ROUTINE=routName, $
                                         PLOTDIR=plotDir, $
                                         SAVEPLOT=savePlot, $
                                         SPNAME=JV_theor_spName, $
                                         AVGS_FOR_FITTING=avgs_JVfit, $
                                         FIT_TIME_SERIES=JV_theor__fit_time_series, $
                                         FIT_TSERIES__A_IN=A_in, $
                                         KAPPALIMS=kappaLims, $   
                                         TEMPLIMS=TempLims, $    
                                         DENSLIMS=DensLims, $    
                                         MAGRATIOLIMS=magRatioLims, $
                                         _EXTRA=e


  ENDIF

  IF KEYWORD_SET(plot_j_v__fixed_t_and_n) THEN BEGIN

     avgs_JVfit.useInds = avgs_JVfit.useInds[SORT(jvplotdata.pot[avgs_JVfit.useInds])]

     ESTIMATE_JV_CURVE_FROM_AVERAGE_PARAMS,jvPlotData,avgs_JVfit, $
                                           A_IN=A_in, $
                                           KAPPALIMS=kappaLims, $   
                                           TEMPLIMS=TempLims, $    
                                           DENSLIMS=DensLims, $    
                                           MAGRATIOLIMS=magRatioLims, $
                                           ORIGINATING_ROUTINE=routName, $
                                           OUT_KAPPA_A=A, $
                                           OUT_GAUSS_A=AGauss, $
                                           OUT_PLOTDATA=pData, $
                                           _EXTRA=e

     PLOT_J_VS_POT__FIXED_T_AND_N,jvPlotData,avgs_JVfit,pData, $
                                  KAPPA_A=A, $
                                  GAUSS_A=AGauss, $
                                  ORIGINATING_ROUTINE=routName, $
                                  ORBIT=orbit, $
                                  SAVEPLOT=savePlot, $
                                  SPNAME=j_v__fixTandN__spName, $
                                 J_V__FIXTANDN__SAVEPLOTDATA=j_v__fixTandN__savePlotData, $
                                 J_V__FIXTANDN__DATAFILENAME=j_v__fixTandN__dataFilename, $
                                  ;; SAVEDATA=j_v__fixTandN__savePlotData, $
                                  ;; DATAFILENAME=j_v__fixTandN__dataFilename, $
                                  _EXTRA=e



     ;; avgs_JVfit.useInds = avgs_JVfit.useInds[SORT(jvplotdata.pot[useInds])]

  ENDIF

  IF KEYWORD_SET(jv_theor__iterative_RBDens_game) THEN BEGIN

     ;; STOP
     ;; jv_theor__itergame_tie_R_B_and_dens = 1
     IF KEYWORD_SET(jv_theor__itergame_tie_R_B_and_dens) THEN BEGIN
        GET_FA_FIELD_LINE,jvPlotData.time, $
                          USEINDS=avgs_JVFit.useInds, $
                          /TIME_ARRAY, $
                          MRATIO=jvPlotData.mRatio
     ENDIF

     ESTIMATE_JV_CURVE_FROM_AVERAGE_PARAMS,jvPlotData,avgs_JVfit, $
                                           A_IN=A_in, $
                                           KAPPALIMS=kappaLims, $   
                                           TEMPLIMS=TempLims, $    
                                           DENSLIMS=DensLims, $    
                                           MAGRATIOLIMS=magRatioLims, $
                                           /ITERATIVE_GAME_MODE, $
                                           JV_THEOR__ITERGAME_DENSFAC=jv_theor__itergame_densFac, $
                                           JV_THEOR__ITERGAME_TIE_R_B_AND_DENS=jv_theor__itergame_tie_R_B_and_dens, $
                                           MAP_TO_100KM=map_to_100km, $
                                           ORIGINATING_ROUTINE=routName, $
                                           OUT_KAPPA_A=A, $
                                           OUT_GAUSS_A=AGauss, $
                                           OUT_PLOTDATA=pData, $
                                           _EXTRA=e

     PLOT_J_VS_POT__FIXED_T_AND_N,jvPlotData,avgs_JVfit,pData, $
                                  KAPPA_A=A, $
                                  GAUSS_A=AGauss, $
                                  ORIGINATING_ROUTINE=routName, $
                                  ORBIT=orbit, $
                                  SAVEPLOT=savePlot, $
                                  SPNAME=j_v__fixTandN__spName, $
                                  _EXTRA=e


  ENDIF

  IF KEYWORD_SET(plot_j_v_map__r_b_and_kappa__fixed_t_and_n) THEN BEGIN

     ;;Options for R_B map
     map__2D = 1B

     IF KEYWORD_SET(jv_theor__itergame_tie_R_B_and_dens) THEN BEGIN
        GET_FA_FIELD_LINE,jvPlotData.time, $
                          USEINDS=avgs_JVFit.useInds, $
                          /TIME_ARRAY, $
                          MRATIO=jvPlotData.mRatio
     ENDIF

     ;; minR_B  = 1
     ;; maxR_B  = 1D3
     ;; dR_B    = KEYWORD_SET(map__2D) ? 5 : 1
     ;; nR_B    = CEIL(FLOAT(maxR_B-minR_B)/dR_B)
     ;; map__multi_magRatio_array = INDGEN(nR_B)*dR_B+minR_B

     minR_B  = 5
     maxR_B  = 1D3
     dR_B    = 1.03D
     ;; nR_B    = ALOG10(maxR_B/minR_B)/ALOG10(dR_B)
     ;; map__multi_magRatio_array = minR_B*dR_B^(INDGEN(nR_B+1))
     map__multi_magRatio_array = POWGEN(minR_B,maxR_B,dR_B)
     IF map__multi_magRatio_array[-1] LT maxR_B THEN map__multi_magRatio_array = [map__multi_magRatio_array,maxR_B]
     ;; map__multi_kappa_array    = [1.501,1.55,1.6,1.65,1.7,1.75,1.8,1.85,1.9,1.95,2.00,2.05,2.10,2.2,2.3,2.4,2.5,2.6,2.7,2.8,2.9,3.0,3.25,3.50,3.75,4.0,4.5,5.0,5.5,6.0,6.5,7.0,7.5,8.0,9.0,10.0,20.0,30.0,40.0,50.0,60.0,70.0,80.0,90.0,100.0]
     ;; map__multi_kappa_array    = [1.501,1.51,1.52,1.53,1.54,1.55,1.56,1.57,1.58,1.59, $

     ;; helper                    = [0.0D,0.025D,0.05D,0.075D]
     ;; helper                    = [0.0D,0.01D,0.02D,0.03D,0.04D,0.05D,0.06D,0.07D,0.08D,0.09D]
     helper                    = LINDGEN(20)*0.005D
     helper2                   = [0.0D,0.25D,0.50D,0.75D]
     ;; map__multi_kappa_array    = [1.505,1.5075,1.51,1.515,1.52,1.525,1.53,1.535,1.54,1.55,1.56,1.57,1.58,1.59, $
     map__multi_kappa_array    = [1.5 + helper[1:-1], $
                                  1.6 + helper, $
                                  1.7 + helper, $
                                  1.8 + helper, $
                                  1.9 + helper, $
                                  2.00,2.05,2.10,2.2,2.3,2.4,2.5,2.6,2.7,2.8,2.9, $
                                  3.0D + helper2, $
                                  4.0D + helper2, $
                                  5.0D + helper2, $
                                  6.0D + helper2, $
                                  7.0D + helper2, $
                                  8.0]

     map2D__log_kappa          = 0
     IF KEYWORD_SET(map2D__logkappa) THEN BEGIN
        map__multi_kappa_array = [map__multi_kappa_array,LINDGEN(100) + 9]
     ENDIF
     
     ESTIMATE_JV_CURVE_FROM_AVERAGE_PARAMS,jvPlotData,avgs_JVfit, $
                                           A_IN=A_in, $
                                           KAPPALIMS=kappaLims, $   
                                           TEMPLIMS=TempLims, $    
                                           DENSLIMS=DensLims, $    
                                           MAGRATIOLIMS=magRatioLims, $
                                           /MULTI_MAGRATIO_MODE, $
                                           MAP__MULTI_MAGRATIO_ARRAY=map__multi_magRatio_array, $
                                           MAP__MULTI_KAPPA_ARRAY=map__multi_kappa_array, $
                                           MAP__2D=map__2D, $
                                           JV_THEOR__ITERGAME_TIE_R_B_AND_DENS=jv_theor__itergame_tie_R_B_and_dens, $
                                           ORIGINATING_ROUTINE=routName, $
                                           OUT_KAPPA_A=A, $
                                           OUT_GAUSS_A=AGauss, $
                                           OUT_PLOTDATA=pData, $
                                           OUT_MULTI_MAGRATIO=mMagDat, $
                                           _EXTRA=e

     PLOT_J_V_MAP__R_B_AND_KAPPA__FIXED_T_AND_N,mMagDat,jvPlotData,avgs_JVFit, $
        MAP__2D=map__2D, $
        MAP2D__LOG_KAPPA=map2D__log_kappa, $
        ORBIT=orbit, $
        IN_KAPPA_A=A, $
        IN_GAUSS_A=AGauss, $
        SAVEPLOT=savePlot, $
        _EXTRA=e

     PLOT_J_VS_POT__FIXED_T_AND_N,jvPlotData,avgs_JVfit,pData, $
                                  KAPPA_A=A, $
                                  GAUSS_A=AGauss, $
                                  ORIGINATING_ROUTINE=routName, $
                                  ORBIT=orbit, $
                                  SAVEPLOT=savePlot, $
                                  SPNAME=j_v__fixTandN__spName, $
                                  J_V__FIXTANDN__SAVEPLOTDATA=j_v__fixTandN__savePlotData, $
                                  J_V__FIXTANDN__DATAFILENAME=j_v__fixTandN__dataFilename, $
                                  ;; SAVEDATA=j_v__fixTandN__savePlotData, $
                                  ;; SDNAME=j_v__fixTandN__dataFilename, $
                                  /NO_TITLE, $
                                  IN_MMAGDAT=mMagDat, $
                                  _EXTRA=e

     STOP
     
  ENDIF

  ;; plot_magCurrent_vs_current = 1
  IF KEYWORD_SET(plot_magCurrent_vs_current) THEN BEGIN

     ;; tmpCur = 0.D * curPotList[0].cur
     ;; ;; IF KEYWORD_SET(use_ed_current) THEN BEGIN
     ;; tmpCur += curPotList[0].cur
     ;; ;; ENDIF
     ;; ;; IF KEYWORD_SET(use_eu_current) THEN BEGIN
     ;; ;;    tmpCur += curPotList[1]
     ;; ;; ENDIF
     ;; ;; IF KEYWORD_SET(use_iu_current) THEN BEGIN
     ;; tmpCur += curPotList[2].cur
     ;; ENDIF
     ;; tmpCur = curPotList.cur[0] + curPotList.cur[1] + curPotList.cur[2]

     ;; plot     = PLOT(jvPlotData.cur[avgs_JVfit.useInds], $
     ;;                 jvPlotData.magCur[avgs_JVfit.useInds], $
     ;;                 LINESTYLE='', $
     ;;                 SYMBOL='*', $
     ;;                 XTITLE='j$_{ESA}$', $
     ;;                 YTITLE='j$_{Mag}$')

     winder   = WINDOW(DIMENSIONS=[800,600])

     plot2_1  = PLOT(jvPlotData.time-jvPlotData.time[0], $
                     jvPlotData.magCur, $
                     NAME='Mag', $
                     LINESTYLE='', $
                     SYMBOL='*', $
                     XTITLE='t since ' + TIME_TO_STR(jvPlotData.time[0],/MS), $
                     YTITLE='j$_{||}$', $
                     /CURRENT)

     plot2_2  = PLOT(jvPlotData.time-jvPlotData.time[0], $
                     jvPlotData.cur, $
                     NAME='ESA', $
                     LINESTYLE='', $
                     SYMBOL='*', $
                     COLOR='red', $
                     /OVERPLOT)

     IF KEYWORD_SET(useInds__twoLumps) AND KEYWORD_SET(useInds) THEN BEGIN
        
        minI      = MIN(useInds)
        maxI      = MAX(useInds)
        line1X    = [jvPlotData.tDiff[minI],jvPlotData.tDiff[minI]]
        line2X    = [jvPlotData.tDiff[maxI],jvPlotData.tDiff[maxI]]

        line1Y    = MINMAX([jvPlotData.cur,jvPlotData.magCur])
        line2Y    = line1Y

        linePlot1 = PLOT(line1X, $
                         line1Y, $
                         ;; RGB_TABLE=hammerCT, $
                         ;; VERT_COLORS=jvPlotData.tMag[[minI,minI]], $
                         /OVERPLOT)
        
        linePlot2 = PLOT(line2X, $
                         line2Y, $
                         ;; RGB_TABLE=hammerCT, $
                         ;; VERT_COLORS=jvPlotData.tMag[[maxI,maxI]], $
                         /OVERPLOT)

     ENDIF

     legend = LEGEND(TARGET=[plot2_1,plot2_2])

  ENDIF

  IF KEYWORD_SET(plot_en_specs) THEN BEGIN

     plotDir = '~/Desktop/test/'

     PLOT_EN_SPECS__DIFF_EFLUX,diff_eFlux_files, $
                               PLOT_T1=plot_t1, $
                               PLOT_T2=plot_t2, $
                               NAMES=en_spec__names, $
                               EEB_OR_EES=en_spec__eeb_or_ees, $
                               ANGLE_RANGES=en_spec__angle_ranges, $
                               ENERGY_RANGES=en_spec__energy_ranges, $
                               UNITS=units, $
                               SAVEPLOT=savePlot, $
                               SPNAME=en_spec__spName, $
                               MOVIE=en_specs__movie, $
                               PLOTDIR=plotDir, $
                               UPGOING=en_spec__upgoing, $
                               IN_SOURCECONE=out_sourcecone, $
                               IN_LOSSCONE=out_losscone, $
                               _EXTRA=e
                               

                               

  ENDIF

  PRINT_CURRENT_AND_POTENTIAL_SUMMARY,jvPlotData,useInds

END
