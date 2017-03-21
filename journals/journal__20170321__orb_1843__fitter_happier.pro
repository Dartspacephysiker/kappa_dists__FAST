;;2017/03/21
PRO JOURNAL__20170321__ORB_1843__FITTER_HAPPIER

  COMPILE_OPT IDL2,STRICTARRSUBS

  orbs                               = [1843]

  orbTimes                           = '1997-02-07/' + $
                                       [['20:49:30','20:50:10']]

  kStats_startStops__eeb             = LIST(LIST('1997-02-01/' + [['20:49:30','20:50:10']]))


  bonusPrefs                         = [ $
               '--oDoyleRules--0--Ergun_et_al_1998' $
               ]

  show_post_plots                    = 0
  save_kappa_plot                    = 0
  close_kp_after_save                = 0

  outfil = 'dimlessPot_vs_time__20170321__orb_1843__data_1DKappafit_1DMaxwfit__from_JOURNAL__20170321__ORB_1843__FITTER_HAPPIER.png'
  
  ;; debug__skip_to_this_time        = STR_TO_TIME('97-02-01/09:26:31')
  ;; debug__break_on_this_time       = STR_TO_TIME('97-02-01/09:26:31')

  only_1D_fits                       = 1
  fit1D__sourceCone_energy_spectrum  = 1
  fit1D__nFlux                       = 1
  fit1D__weighting                   = 1 ;1 = lin 2 = square
  
  add_oneCount_curve                 = 1

  fit1D__save_plotSlices             = 0
  fit2D__save_all_candidate_plots    = 0
  fit2D__show_each_candidate         = 0
  fit2D__weighting                   = 1 ;1 = lin 2 = square

  show_Strangeway_summary            = 0
  sway__save_ps                      = 0
  sway__add_kappa_panel              = 0
  sway__add_chare_panel              = 1
  sway__add_Newell_panel             = 0
  sway__log_kappaPlot                = 0

  show_kappa_summary                 = 1
  kSum__save_ps                      = 0
  kSum__convert_to_Newell_interp     = 1
  kSum__add_chi2_line                = 1

  kStats__save_stuff                 = 1

  save_diff_eFlux_file = 1
  load_diff_eFlux_file = 1
  ;; restore_fitFile      = 1

  evtNum               = 0

  ;;survey window
  eeb_or_ees           = 'eeb'
  spectra_average_interval = 6
  burstItvl            = 0

  ;;String setup
  orbit                = orbs      [evtNum]
  t1Str                = orbTimes[0,evtNum]
  t2Str                = orbTimes[1,evtNum]
  bonusPref            = bonusPrefs[evtNum]

  IF STRUPCASE(eeb_or_ees) EQ 'EEB' THEN BEGIN
     ;; t1Str             = (orbBurstTimes[evtNum])[0,burstItvl]
     ;; t2Str             = (orbBurstTimes[evtNum])[1,burstItvl]
     bonusPref        += '--burstItvl_' + STRCOMPRESS(burstItvl,/REMOVE_ALL)
     kStats__include_these_startstops = (kStats_startStops__eeb[evtNum])[burstItvl]
  ENDIF ELSE BEGIN
     ;; kStats__include_these_startstops = kStats_startStops__ees[evtNum]
  ENDELSE

  ;;Thresholds for inclusion
  ;; chi2_thresh          = 1.5e4
  chi2_over_dof_thresh = 25
  lowDens_thresh       = 0.01
  diffEflux_thresh     = 5e7
  nPkAbove_dEF_thresh  = 5

  ;; electron_angleRange  = [-24,24]
  electron_angleRange  = 'lc'
  energy_electrons     = [3e1,3.0e4]
  electron_lca         = [150,-150]
  min_peak_energy      = 500

  KAPPA_FITTER_BLACKBOX,orbit, $
                        ELECTRON_SOURCECONEANGLE=electron_angleRange, $
                        ELECTRON_LOSSCONEANGLE=electron_lca, $
                        ENERGY_ELECTRONS=energy_electrons, $
                        MIN_PEAK_ENERGY=min_peak_energy, $
                        EEB_OR_EES=eeb_or_ees, $
                        SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                        CHI2_THRESHOLD=chi2_thresh, $
                        CHI2_OVER_DOF_THRESHOLD=chi2_over_dof_thresh, $
                        HIGHDENSITY_THRESHOLD=highDens_thresh, $
                        LOWDENSITY_THRESHOLD=lowDens_thresh, $
                        DIFFEFLUX_THRESHOLD=diffEflux_thresh, $
                        N_PEAKS_ABOVE_DEF_THRESHOLD=nPkAbove_dEF_thresh, $
                        RESTORE_FITFILE=restore_fitFile, $
                        T1STR=t1Str, $
                        T2STR=t2Str, $
                        SHOW_POST_PLOTS=show_post_plots, $
                        ONLY_1D_FITS=only_1D_fits, $
                        FIT1D__SOURCECONE_ENERGY_SPECTRUM=fit1D__sourceCone_energy_spectrum, $
                        FIT1D__NFLUX=fit1D__nFlux, $
                        FIT1D__WEIGHTING=fit1D__weighting, $
                        FIT1D__SAVE_PLOTSLICES=fit1D__save_plotSlices, $
                        FIT2D__SHOW_EACH_CANDIDATE=fit2D__show_each_candidate, $
                        FIT2D__SAVE_ALL_CANDIDATE_PLOTS=fit2D__save_all_candidate_plots, $
                        FIT2D__WEIGHTING=fit2D__weighting, $
                        ADD_ONECOUNT_CURVE=add_oneCount_curve, $
                        SAVE_KAPPA_PLOTS=save_kappa_plot, $
                        SAVEKAPPA_BONUSPREF=bonusPref, $
                        CLOSE_KAPPAPLOTS_AFTER_SAVE=close_kp_after_save, $
                        PLOTDIR=plotDir, $
                        SHOW_STRANGEWAY_SUMMARY=show_Strangeway_summary, $
                        SWAY__SAVE_PS=sway__save_ps, $
                        SWAY__SAVE_PNG=sway__save_png, $
                        SWAY__ADD_KAPPA_PANEL=sway__add_kappa_panel, $
                        SWAY__ADD_CHARE_PANEL=sway__add_chare_panel, $
                        SWAY__ADD_NEWELL_PANEL=sway__add_Newell_panel, $
                        SWAY__LOG_KAPPAPLOT=sway__log_kappaPlot, $
                        SHOW_KAPPA_SUMMARY=show_kappa_summary, $
                        KSUM__SAVE_PS=kSum__save_ps, $
                        KSUM__SAVE_PNG=kSum__save_png, $
                        KSUM__CONV_DESPECS_TO_NEWELL_INTERP=kSum__convert_to_Newell_interp, $
                        KSUM__ADD_CHI2_LINE=kSum__add_chi2_line, $
                        OUT_FIT2DK=fit2DK, $
                        OUT_FIT2DGAUSS=fit2DG, $
                        OUT_KAPPA_FIT_STRUCTS=kappaFits, $
                        OUT_GAUSS_FIT_STRUCTS=gaussFits, $
                        FIT2D_KAPPA_INF_LIST=fit2DKappa_inf_list, $
                        FIT2D_GAUSS_INF_LIST=fit2DGauss_inf_list, $
                        SAVE_DIFF_EFLUX_FILE=save_diff_eFlux_file, $
                        LOAD_DIFF_EFLUX_FILE=load_diff_eFlux_file, $
                        KAPPA_STATS__SAVE_STUFF=kStats__save_stuff, $
                        KAPPA_STATS__INCLUDE_THESE_STARTSTOPS=kStats__include_these_startstops,$
                        DEBUG__SKIP_TO_THIS_TIME=debug__skip_to_this_time, $
                        DEBUG__BREAK_ON_THIS_TIME=debug__break_on_this_time

  SDTEsts  = EXTRACT_MEMBER_FROM_LIST_OF_STRUCTS(kappaFits,'A_SDT',/INDEX_INSTANCES_AT_REAR)
  fitK     = EXTRACT_MEMBER_FROM_LIST_OF_STRUCTS(kappaFits,'A',/INDEX_INSTANCES_AT_REAR)
  fitG     = EXTRACT_MEMBER_FROM_LIST_OF_STRUCTS(gaussFits,'A',/INDEX_INSTANCES_AT_REAR)
  time     = EXTRACT_MEMBER_FROM_LIST_OF_STRUCTS(kappaFits,'TIME',/INDEX_INSTANCES_AT_REAR)
  bulkEst  = SDTEsts[0,*]
  tempEst  = SDTEsts[1,*]
  symDat   = '*'

  bulkK    = fitK[0,*]
  tempK    = fitK[1,*]
  symK     = 'x'
  colK     = 'Blue'

  timeG    = EXTRACT_MEMBER_FROM_LIST_OF_STRUCTS(gaussFits,'TIME')
  bulkG    = fitG[0,*]
  tempG    = fitG[1,*]
  symG     = '+'
  colG     = 'Red'

  rangeCand = [bulkEst/tempEst,bulkK/tempK,bulkG/tempG]
  yRange   = MINMAX(rangeCand[WHERE(FINITE(rangeCand))])
  yRange   = [0.09,100]
  win      = WINDOW(DIMENSIONS=[800,600])
  plot     = PLOT(time-time[0], $
                  bulkEst/tempEst,  $
                  NAME='Data', $
                  LINESTYLE='', $
                  /YLOG, $
                  YRANGE=yRange, $
                  SYMBOL=symDat, $
                  XTITLE='Seconds since ' + TIME_TO_STR(time[0],/MS), $
                  YTITLE="Dimensionless potential", $
                  /CURRENT)

  plotK    = PLOT(time-time[0], $
                  bulkK/tempK, $
                  NAME='Kappa', $
                  LINESTYLE='', $
                  SYMBOL=symK, $
                  COLOR=colK, $
                  /OVERPLOT)

  plotG    = PLOT(timeG-timeG[0], $
                  bulkG/tempG, $
                  NAME='Maxw', $
                  LINESTYLE='', $
                  SYMBOL=symG, $
                  COLOR=colG, $
                  /OVERPLOT)

  legend = LEGEND(TARGET=[plot,plotK,plotG], $
                  POSITION=[50,0.4],/DATA)
  outdir = '~/software/sdt/batch_jobs/plots/'
  PRINT,"Saving " + outfil
  win.Save,outdir+outFil
  STOP

END


