FUNCTION INIT_KAPPA_PLOT_OPTIONS,NO_PLOTS=no_plots, $
                                 SAVE_FITPLOTS=save_fitplots, $
                                 PLOT_FULL_FIT=plot_full_fit, $
                                 PLOTDIR=plotDir, $
                                 ADD_ONECOUNT_CURVE=add_oneCount_curve, $
                                 ADD_FITPARAMS_TEXT=add_fitParams_text, $
                                 ADD_ANGLE_LABEL=add_angle_label
  
  COMPILE_OPT idl2


  defAdd_oneCount_curve            = 1

  kPlot_opt                        = {no_plots:0, $
                                      save_fitplots:0, $
                                      plot_full_fit:0, $
                                      plotDir:'', $
                                      add_angle_label:0, $
                                      add_fitParams_text:0, $
                                      add_oneCount_curve:defAdd_oneCount_curve}

  IF N_ELEMENTS(no_plots) GT 0 THEN BEGIN
     kPlot_opt.no_plots            = no_plots
  ENDIF

  IF N_ELEMENTS(save_fitplots) GT 0 THEN BEGIN
     kPlot_opt.save_fitplots       = save_fitplots
  ENDIF

  IF N_ELEMENTS(plot_full_fit) GT 0 THEN BEGIN
     kPlot_opt.plot_full_fit       = plot_full_fit
  ENDIF

  IF N_ELEMENTS(plotDir) GT 0 THEN BEGIN
     kPlot_opt.plotDir             = plotDir
  ENDIF ELSE BEGIN
     SET_PLOT_DIR,plotDir,/FOR_SDT,/ADD_TODAY,/VERBOSE
     kPlot_opt.plotDir             = plotDir
  ENDELSE


  IF N_ELEMENTS(add_oneCount_curve) GT 0 THEN BEGIN
     kPlot_opt.add_oneCount_curve  = add_oneCount_curve
  ENDIF

  IF N_ELEMENTS(add_fitParams_text) GT 0 THEN BEGIN
     kPlot_opt.add_fitParams_text  = add_fitParams_text
  ENDIF

  IF N_ELEMENTS(add_angle_label) GT 0 THEN BEGIN
     kPlot_opt.add_angle_label     = add_angle_label
  ENDIF
  

  RETURN,kPlot_opt

END