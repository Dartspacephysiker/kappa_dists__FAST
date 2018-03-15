;2018/03/14
PRO JOURNAL__20180314__GET_PLOTS_AT_EACH_ANGLE_FOR_A_HANDFUL_OF_ORBITS

  COMPILE_OPT IDL2,STRICTARRSUBS

  orbit = 3458

  eFlux_units_instead = 1
  use_2D_fit_info     = 1

  spec_avg_itvl  = 1
  avgItvl        = STRING(FORMAT='(I0)',spec_avg_itvl)

  diffEFluxFile = 'orb_3458-diff_eflux-ees-avg_itvl'+avgItvl+'-05_53_55__000-05_55_48__000.sav'
  fitFile       = '20180314-orb_3458-Kappa_fits_and_Gauss_fits-ees-horseshoe2d-TESTRUN-20180302-only_fit_peak_eRange-avg_itvl'+avgItvl+'.sav'
  times         = ['1997-07-07/05:55:15.49', $
                   '1997-07-07/05:55:17.39', $
                   '1997-07-07/05:55:21.18']

  KAPPA_FITS__PLOT_EACH_ANGLE,orbit, $
                              DIFFEFLUXFILE=diffEFluxFile, $
                              FITFILE=fitFile, $
                              TIMESTRINGARR=times, $
                              SPEC_AVG_ITVL=spec_avg_itvl, $
                              USE_2D_FIT_INFO=use_2D_fit_info, $
                              EFLUX_UNITS_INSTEAD=eFlux_units_instead, $
                              JUST_LOSSCONE_ANGLES=just_losscone_angles

END
