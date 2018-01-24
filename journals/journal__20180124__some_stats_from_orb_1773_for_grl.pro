;2018/01/24
PRO JOURNAL__20180124__SOME_STATS_FROM_ORB_1773_FOR_GRL

  COMPILE_OPT IDL2,STRICTARRSUBS

  dir  = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/cur_and_pot_analysis/'
  file = 'Orbit_1773-apres_Elph98--classics-3-Elphic_et_al_1998-0BelowPk-GRLRESPONSEFINAL2blkBox-Fig2__meal-aR_mom_eD_-28-28-sc_pot-w_1Count-avg_itvl2.sav'

  fileIngred = 'Orbit_1773-apres_Elph98--classics-3-Elphic_et_al_1998-0BelowPk-GRLRESPONSEFINAL2blkBox-Fig2_ingredients-aR_mom_eD_-28-28-sc_pot-w_1Count-avg_itvl2.sav'

  realDir  = '/SPENCEdata/Research/Satellites/FAST/kappa_dists/saves_output_etc/'
  realFile = '20180120--jvplotdata_and_avgs_jvfit_for_orb1773_analysis_using_peak_en_for_downpot.sav'

  RESTORE,realDir+realFile

  STOP

END
