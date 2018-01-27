;2018/01/27
PRO JOURNAL__20180127__PLOT_KAPPA_AURORA_FIGS1A__ONLY_GAUSSIAN

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;;For adding an R_E axis, if that's wassup
  @common__jv_curve_fit__tie_r_b_and_dens.pro

  only_Gaussian = 1

  savePlot = 1
  spPref = 'Kappa_aurora__FigS1a__only_Gaussian'
  suff   = '.eps'

  spName = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + '-' + spPref + suff
  SET_PLOT_DIR,plotDir,/FOR_KAPPA_DB,/ADD_TODAY

  dir = '/SPENCEdata/software/sdt/batch_jobs/kappas_Maxwellians_for_inverted_Vs/'
  file = 'orb1773_jvplotdata_and_avgs_jvfit_for_GRLRESPONSE2_using_peak_en.sav'

  RESTORE,dir+file

  PLOT_J_V_MAP__R_B_AND_KAPPA__FIXED_T_AND_N,mMagDat,jvPlotData,avgs_JVFit, $
     MAP__2D=0, $
     ONLY_GAUSSIAN=only_Gaussian, $
     MAP2D__LOG_KAPPA=map2D__log_kappa, $
     ORBIT=orbit, $
     IN_KAPPA_A=A, $
     IN_GAUSS_A=AGauss, $
     SAVEPLOT=savePlot, $
     SPNAME=SPName, $
     PLOTDIR=plotDir, $
     IS_EFLUX=jv_theor__only_eFlux, $
     _EXTRA=e

END
