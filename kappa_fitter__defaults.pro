  kappa_est                     = 10

  ;; T_est_fac                     = 1.3
  ;; N_est_fac                     = 7.0
  ;; bulkE_est_fac                 = 1.0

  ;; TGauss_est_fac                = 0.3
  ;; NGauss_est_fac                = 1.0
  ;; bulkEGauss_est_fac            = 1.0

  T_est_fac                     = 1.0
  N_est_fac                     = 1.0
  bulkE_est_fac                 = 1.0

  TGauss_est_fac                = 1.0
  NGauss_est_fac                = 1.0
  bulkEGauss_est_fac            = 1.0

  estFacs                       = {T        :    T_est_fac      , $
                                   N        :    N_est_fac      , $
                                   B_E      :    bulkE_est_fac  , $
                                   TGauss   :    TGauss_est_fac , $
                                   NGauss   :    NGauss_est_fac , $
                                   B_EGauss :    bulkEGauss_est_fac}

  do_all_times                  = 1
  add_full_fits                 = 1
  fit2D__only_fit_peak_eRange   = 1
  fit2D__only_fit_aboveMin      = 1
  fit2D__only_fit_eAngles       = 1
  fit2D__keep_wholeFit          = 1
  fit2D__bulk_e_anisotropy      = 1
  fit2D__exclude_lca_from_densCalc = 1
  fit2D__disable_bFunc          = 1
  ;; fit2D__bulk_e_anis_factor  = 0.3
  IF N_ELEMENTS(fit2D__density_angleRange) EQ 0 THEN BEGIN
     ;; fit2D__density_angleRange  = [-32,32]
     ;; fit2D__density_angleRange  = 'lc'
  ENDIF

  IF KEYWORD_SET(upgoing) THEN BEGIN

     CASE SIZE(fit2D__density_angleRange,/TYPE) OF
        7: BEGIN
           ;; STOP
        END
        0:
        ELSE: BEGIN
           IF KEYWORD_SET(fit2D__density_angleRange) THEN BEGIN
              fit2D__density_angleRange += 180
           ENDIF
        END
     ENDCASE
     CASE SIZE(electron_angleRange,/TYPE) OF
        7: BEGIN
           ;; STOP
        END
        0:
        ELSE: BEGIN
           IF KEYWORD_SET(electron_angleRange) THEN BEGIN
              electron_angleRange    += 180
           ENDIF
        END
     ENDCASE

     CASE SIZE(electron_lca,/TYPE) OF
        7: BEGIN
           STOP
        END
        0:
        ELSE: BEGIN
           IF KEYWORD_SET(electron_lca) THEN BEGIN
              electron_lca    += 180
           ENDIF
        END
     ENDCASE
     
     n_below_peak               = 2
     n_above_peak               = 20
     n_below_peak2D             = 2
     n_above_peak2D             = 20

  ENDIF ELSE BEGIN

     n_below_peak               = 3
     n_above_peak               = 20
     n_below_peak2D             = 3
     n_above_peak2D             = 20

  ENDELSE
  
  synthPackage                  = 1
  average_over_angleRange       = 1

  estimate_A_from_data          = 1
  dont_print_estimates          = 1
  dont_print_fitinfo            = 1
  print_2DFitInfo               = 1

  use_mpFit1D                   = 1

  fit1D__skip_bad_fits          = 1
  fit1D__show_and_prompt        = 0
  IF ~KEYWORD_SET(fit2D__show_each_candidate) THEN BEGIN
     fit2D__show_each_candidate = 0
  ENDIF
  fit2D__add_boundaries         = 1
  fit_fail__user_prompt         = 0

  dont_fit_below_thresh_value   = 0
  bulk_offset                   = 0

  add_gaussian_estimate         = 1
  ;; add_oneCount_curve            = 0

  no_plots                      = 1
  save_fitPlots                 = 1
  saveData                      = 1
  plot_full_fit                 = 1
  add_fitParams_text            = 1
  add_angle_label               = 1

  max_iter                      = 10000
  fit2D_max_iter                = 10000

  fit_tol                       = 1D-4
  fit2D_tol                     = 1D-6

  IF STRUPCASE(eeb_or_ees) EQ 'EEB' THEN BEGIN
     IF N_ELEMENTS(spectra_average_interval) EQ 0 THEN spectra_average_interval = 4
  ENDIF

