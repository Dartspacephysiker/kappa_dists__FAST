;2018/01/27
PRO JOURNAL__20180127__PLOT_KAPPA_AURORA_FIGS1A__ONLY_GAUSSIAN

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;;For adding an R_E axis, if that's wassup
  @common__jv_curve_fit__tie_r_b_and_dens.pro

  only_Gaussian = 1

  savePlot = 1
  j_v_map__winDim = [800,300]
  j_v_map__gauss_spPref = 'Kappa_aurora__Fig3c__only_Gaussian'
  suff   = '.png'

  j_v__withEstCond__spName = 'Kappa_aurora_Fig_3a_estCond_bestFitMax' + suff
  estCond__add_gaussian    = 1

  j_v_map__gauss_spName = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + '-' + $
           j_v_map__gauss_spPref + suff
  SET_PLOT_DIR,plotDir,/FOR_KAPPA_DB,/ADD_TODAY

  dir = '/SPENCEdata/software/sdt/batch_jobs/kappas_Maxwellians_for_inverted_Vs/'
  file = 'orb1773_jvplotdata_and_avgs_jvfit_for_GRLRESPONSE2_using_peak_en.sav'

  RESTORE,dir+file

  ;; PLOT_J_VS_POT__WITH_ESTIMATED_CONDUCTIVITY,jvPlotData,avgs_JVfit,pData, $
  ;;    KAPPA_A=A, $
  ;;    GAUSS_A=AGauss, $
  ;;    ORIGINATING_ROUTINE=routName, $
  ;;    ADD_GAUSSIAN=estCond__add_gaussian, $
  ;;    ORBIT=orbit, $
  ;;    SAVEPLOT=savePlot, $
  ;;    SPNAME=j_v__withEstCond__spName, $
  ;;    PLOTDIR=plotDir, $
  ;;    J_V__WITHESTCOND__SAVEPLOTDATA=j_v__withEstCond__savePlotData, $
  ;;    J_V__WITHESTCOND__DATAFILENAME=j_v__withEstCond__dataFilename, $
  ;;    ;; SAVEDATA=j_v__withEstCond__savePlotData, $
  ;;    ;; SDNAME=j_v__withEstCond__dataFilename, $
  ;;    /NO_TITLE, $
  ;;    IN_MMAGDAT=mMagDat, $
  ;;    IS_EFLUX=jv_theor__only_eFlux, $
  ;;    _EXTRA=e


  PLOT_J_V_MAP__R_B_AND_KAPPA__FIXED_T_AND_N,mMagDat,jvPlotData,avgs_JVFit, $
     MAP__2D=0, $
     ONLY_GAUSSIAN=only_Gaussian, $
     MAP2D__LOG_KAPPA=map2D__log_kappa, $
     ORBIT=orbit, $
     IN_KAPPA_A=A, $
     IN_GAUSS_A=AGauss, $
     SAVEPLOT=savePlot, $
     SPNAME=j_v_map__gauss_SPName, $
     WINDIM=j_v_map__winDim, $
     PLOTDIR=plotDir, $
     IS_EFLUX=jv_theor__only_eFlux, $
     _EXTRA=e

END
