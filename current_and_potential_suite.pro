;2017/03/22
PRO CURRENT_AND_POTENTIAL_SUITE, $
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
     DATFILE=datFile, $
     REMAKE_MASTERFILE=remake_masterFile, $
     SAVE_DIFF_EFLUX_FILE=save_diff_eFlux_file, $
     LOAD_DIFF_EFLUX_FILE=load_diff_eFlux_file, $
     EEB_OR_EESARR=eeb_or_eesArr, $
     ORDER=order, $
     LABEL=label, $
     ADD_ONECOUNT_STATS=add_oneCount_stats, $
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
     ENERGYARR=energyArr, $
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
     ;; DENS_ERRORS=dens_errors, $
     SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
     MAP_TO_100KM=map_to_100km, $
     SAVECURPOTFILE=saveCurPotFile, $
     OUT_CURPOTLIST=curPotList, $
     _EXTRA=e


  CURRENT_AND_POTENTIAL_PLOTDATA_PREP,curPotList,jvPlotData, $
                                      T1=plot_t1, $
                                      T2=plot_t2, $
                                      USE_ALL_CURRENTS=use_all_currents, $
                                      USE_DOWNGOING_ELECTRON_CURRENT=use_ed_current, $
                                      USE_UPGOING_ION_CURRENT=use_iu_current, $
                                      USE_UPGOING_ELECTRON_CURRENT=use_eu_current, $
                                      USE_CHAR_EN_FOR_DOWNPOT=use_charE_for_downPot, $
                                      USE_PEAK_EN_FOR_DOWNPOT=use_peakE_for_downPot, $
                                      ADD_UPGOING_ION_POT=add_iu_pot, $
                                      ERROR_BAR_FACTOR=errorBarFac, $
                                     USEI__RELCHANGE=useInds__relChange, $
                                     FRACCHANGE_TDOWN=fracChange_TDown, $
                                     FRACCHANGE_NDOWN=fracChange_NDown, $
                                     FRACERROR_TDOWN=fracError_TDown, $
                                     FRACERROR_NDOWN=fracError_NDown, $
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
                                      OUT_AVGS_FOR_FITTING=avgs_JVfit, $
                                      _EXTRA=e


  IF KEYWORD_SET(plot_jv_a_la_Elphic) THEN BEGIN
     PLOT_THREEPANEL_ANALOG_TO_FIG2_ELPHIC_ETAL_1998,jvPlotData, $
        ORIGINAL_PLOTIDEE=orig_plotIdee, $
        SAVEPLOT=savePlot, $
        SPNAME=a_la_Elphic_spName, $
        ORIGINATING_ROUTINE=routName, $
        PLOTDIR=plotDir, $
        _EXTRA=e
  ENDIF

  IF KEYWORD_SET(plot_j_v_potBar) THEN BEGIN
     PLOT_J_VS_POTBAR,jvPlotData, $
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

  ;;             kappa,            Temp,            Dens,  R_B
  A_in         = [  10,avgs_JVfit.T.avg,avgs_JVfit.N.avg, 1D3]
  IF KEYWORD_SET(plot_j_v_and_theory) THEN BEGIN

     PLOT_JV_DATA_AND_THEORETICAL_CURVES,jvPlotData, $
                                         CURPOTLIST=curPotList, $
                                         MINPOT=jv_theor__minPot, $
                                         MAXPOT=jv_theor__maxPot, $
                                         MINCUR=jv_theor__minCur, $
                                         MAXCUR=jv_theor__maxCur, $
                                         USEINDS=useInds, $
                                         PLOT_J_RATIOS=plot_j_ratios, $
                                         PLOT_ION_ELEC_RATIOS=plot_ion_elec_ratios, $
                                         ORIGINATING_ROUTINE=routName, $
                                         PLOTDIR=plotDir, $
                                         SAVEPLOT=savePlot, $
                                         SPNAME=JV_theor_spName, $
                                         AVGS_FOR_FITTING=avgs_JVfit, $
                                         FIT_TIME_SERIES=JV_theor__fit_time_series, $
                                         FIT_TSERIES__A_IN=A_in, $
                                         _EXTRA=e


  ENDIF

  IF KEYWORD_SET(plot_j_v__fixed_t_and_n) THEN BEGIN

     ESTIMATE_JV_CURVE_FROM_AVERAGE_PARAMS,jvPlotData,avgs_JVfit, $
                                           ORBIT=orbit, $
                                           A_IN=A_in, $
                                           ORIGINATING_ROUTINE=routName, $
                                           SAVEPLOT=savePlot, $
                                           SPNAME=j_v__fixTandN__spName, $
                                           _EXTRA=e

     nUsers       = N_ELEMENTS(useInds)
     useInds      = useInds[SORT(jvplotdata.time[useInds])]

     PRINT,FORMAT='(I0,T5,A0,T35,A0,T45,A0,T55,A0,T65,A0,T75,A0,T85,A0,T95,A0)', $
           'i','Time','Temp','N','Pot','Current','TFracErr','NFracErr','JFracErr'
     FOR k=0,nUsers-1 DO BEGIN
        PRINT,FORMAT='(I0,T5,A0,T35,F-8.3,T45,F-8.3,T55,F-8.3,T65,F-8.3,T75,F-8.3,T85,F-8.3,T95,F-8.3)', $
              k, $
              TIME_TO_STR(JVPlotData.time[useInds[k]]), $
              JVPlotData.TDown[useInds[k]], $
              JVPlotData.NDown[useInds[k]], $
              JVPlotData.pot[useInds[k]], $
              JVPlotData.cur[useInds[k]], $
              JVPlotData.TDownErr[useInds[k]]/JVPlotData.TDown[useInds[k]], $
              JVPlotData.NDownErr[useInds[k]]/JVPlotData.NDown[useInds[k]], $
              ABS(JVPlotData.curErr[useInds[k]]/JVPlotData.cur[useInds[k]])
        
     ENDFOR
     PRINT,FORMAT='(A0,T35,F-8.3,T45,F-8.3,T55,F-8.3,T65,G-8.3)', $
           "Avg", $
           MEAN(JVPlotData.TDown[useInds]), $
           MEAN(JVPlotData.NDown[useInds]), $
           MEAN(JVPlotData.pot[useInds]), $
           MEAN(JVPlotData.cur[useInds])

  ENDIF

END
