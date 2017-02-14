; A[0]: E_b,       Plasma bulk energy (eV)
; A[1]: T,         Plasma kinetic temperature (eV)
; A[2]: kappa,     Kappa (of course!)--or more specifically 3D kappa index, so that kappa = kappa_0 + 3/2
; A[3]: n,         Plasma density (cm^-3)
; A[6]: bulkAngle, Angle between bulk velocity, u_b, and velocity in direction for which we're interested in the distribution

;;We don't use the ones below...
; A[4]: inDT,      The "delta_t" for integration of electron counts (UNUSED)
; A[5]: m,         Particle mass (in this case electron mass), in eV/c^2
;2016/07/19
FUNCTION INIT_KAPPA_CURVEFIT_OPTIONS,FIT1D__TOLERANCE=fit_tol, $
                                     FIT1D__MAX_ITERATIONS=max_iter, $
                                     FIT2D__TOLERANCE=fit2D_tol, $
                                     FIT2D__MAX_ITERATIONS=fit2D_max_iter, $
                                     FIT2D__ONLY_FIT_DENSANGLES=fit2D__only_fit_densAngles, $
                                     FIT2D__ONLY_FIT_ELECTRON_ANGLES=fit2D__only_fit_eAngles, $
                                     FIT2D__ONLY_FIT_ERANGE_AROUND_PEAK=fit2D__only_fit_peak_eRange, $
                                     FIT2D__ONLY_FIT_ERANGE_ABOVE_MIN=fit2D__only_fit_aboveMin, $
                                     FIT2D__USE_BULK_E_ANISOTROPY=fit2D__bulk_e_anisotropy, $
                                     FIT2D__BULK_E_ANISO_FACTOR=fit2D__bulk_e_anis_factor, $
                                     FIT2D__EXCLUDE_LCA_FROM_DENSCALC=fit2D__exclude_lca_from_densCalc, $
                                     FIT2D__DISABLE_BFUNC=fit2D__disable_bFunc, $
                                     FIT2D__KEEP_WHOLEFIT=fit2D__keep_wholeFit, $
                                     FIT_EACH__1DFIT_TO_DENSITY_AT_EACH_ANGLE=fit1D_to_density_at_each_angle, $
                                     N_ENERGIES_BELOW_PEAK=n_below_peak, $
                                     N_ENERGIES_ABOVE_PEAK=n_above_peak, $
                                     N_BELOW_PEAK2D=n_below_peak2D, $
                                     N_ABOVE_PEAK2D=n_above_peak2D, $
                                     TRIM_ENERGIES_BELOW_PEAK=trim_energies_below_peak, $
                                     MIN_PEAK_ENERGY=min_peak_energy, $
                                     DONT_FIT_BELOW_THRESH_VALUE=dont_fit_below_thresh_value, $
                                     ESTIMATE_A_FROM_DATA=estimate_A_from_data, $
                                     ADD_GAUSSIAN_ESTIMATE=add_gaussian_estimate, $
                                     USE_SDT_GAUSSIAN_FIT=use_SDT_Gaussian_fit, $
                                     USE_MPFIT1D=use_mpFit1D, $
                                     DENSITY_EST=n_est, $
                                     TEMPERATURE_EST=T, $
                                     KAPPA_EST=kappa, $
                                     BULK_OFFSET=bulk_offset, $
                                     _EXTRA=e


  COMPILE_OPT idl2

  defFit_tol                                 = 1.e-3
  defFit2D_tol                               = 1.e-3
  defMax_iter                                = 1000
  defFit2D_max_iter                          = 1000
  defNEn_below_peak                          = 2
  defNEn_above_peak                          = 7
  defMin_peak_energy                         = 300
  defAdd_gaussian_estimate                   = 1

  ;;default mass given in eV/(km/s)^2, per SDT
  defKappaFitA                               = [1000,300,3.0,0.1,0.0,5.6856602e-06,0.0]

  kCurvefit_opt                              = {fit_tol                     : defFit_tol, $
                                                fit2D_tol                   : defFit2D_tol, $
                                                fit2D_only_dens_angles      :  0, $
                                                fit2D_only_eAngles          :  0, $
                                                fit2D_just_eRange_peak      :  0, $
                                                fit2D_fit_above_minE        :  0, $
                                                fit2D__bulk_e_anisotropy    :  0, $
                                                fit2D__bulk_e_anis_factor   :  0., $
                                                fit2D__exclude_lca_from_densCalc :  0, $
                                                fit2D__disable_bFunc        :  0, $
                                                fit2D__keep_wholeFit        :  0, $
                                                max_iter                    : defMax_iter, $
                                                fit2D_max_iter              : defFit2D_max_iter, $
                                                fita                        : defKappaFitA, $
                                                fit1D_dens__each_angle      :  0, $
                                                n_below_peak                : defNEn_below_peak, $
                                                n_above_peak                : defNEn_above_peak, $
                                                n_below_peak2D              : defNEn_below_peak, $
                                                n_above_peak2D              : defNEn_above_peak, $
                                                trim_energies_below_peak    : 1, $
                                                min_peak_energy             : defMin_peak_energy, $
                                                thresh_eFlux                : 0.0D, $
                                                estimate_A_from_data        : 1, $
                                                add_gaussian_estimate       : defAdd_gaussian_estimate, $
                                                use_SDT_Gaussian_fit        :  0, $
                                                use_mpFit1D                 :  0, $
                                                bulk_offset                 :  0.0}

  IF N_ELEMENTS(fit_tol) GT 0 THEN BEGIN
     kCurvefit_opt.fit_tol                   = fit_tol

     PRINT,FORMAT='("kCurvefit_opt.fit_tol",T45,":",T48,F0.2)', $
     kCurvefit_opt.fit_tol
  ENDIF

  IF N_ELEMENTS(max_iter) GT 0 THEN BEGIN
     kCurvefit_opt.max_iter                  = max_iter

     PRINT,FORMAT='("kCurvefit_opt.max_iter",T45,":",T48,I0)', $
     kCurvefit_opt.max_iter
  ENDIF

  IF N_ELEMENTS(fit2D_max_iter) GT 0 THEN BEGIN
     kCurvefit_opt.fit2D_max_iter            = fit2D_max_iter

     PRINT,FORMAT='("kCurvefit_opt.fit2D_max_iter",T45,":",T48,I0)', $
     kCurvefit_opt.fit2D_max_iter
  ENDIF

  IF N_ELEMENTS(fit2D_tol) GT 0 THEN BEGIN
     kCurvefit_opt.fit2D_tol                 = fit2D_tol

     PRINT,FORMAT='("kCurvefit_opt.fit2D_tol",T45,":",T48,F0.2)', $
     kCurvefit_opt.fit2D_tol
  ENDIF

  IF N_ELEMENTS(n_below_peak) GT 0 THEN BEGIN
     kCurvefit_opt.n_below_peak              = n_below_peak

     PRINT,FORMAT='("kCurvefit_opt.n_below_peak",T45,":",T48,I0)', $
     kCurvefit_opt.n_below_peak
  ENDIF

  IF N_ELEMENTS(n_above_peak) GT 0 THEN BEGIN
     kCurvefit_opt.n_above_peak              = n_above_peak

     PRINT,FORMAT='("kCurvefit_opt.n_above_peak",T45,":",T48,I0)', $
     kCurvefit_opt.n_above_peak
  ENDIF

  IF N_ELEMENTS(n_below_peak2d) GT 0 THEN BEGIN
     kCurvefit_opt.n_below_peak2d              = n_below_peak2d

     PRINT,FORMAT='("kCurvefit_opt.n_below_peak2d",T45,":",T48,I0)', $
     kCurvefit_opt.n_below_peak2d
  ENDIF

  IF N_ELEMENTS(n_above_peak2d) GT 0 THEN BEGIN
     kCurvefit_opt.n_above_peak2d              = n_above_peak2d

     PRINT,FORMAT='("kCurvefit_opt.n_above_peak2d",T45,":",T48,I0)', $
     kCurvefit_opt.n_above_peak2d
  ENDIF

  IF N_ELEMENTS(trim_energies_below_peak) GT 0 THEN BEGIN
     kCurvefit_opt.trim_energies_below_peak  = trim_energies_below_peak

     PRINT,FORMAT='("kCurvefit_opt.trim_energies_below_peak",T45,":",T48,I0)', $
     kCurvefit_opt.trim_energies_below_peak
  ENDIF

  IF N_ELEMENTS(min_peak_energy) GT 0 THEN BEGIN
     kCurvefit_opt.min_peak_energy           = min_peak_energy

     PRINT,FORMAT='("kCurvefit_opt.min_peak_energy",T45,":",T48,I0)', $
     kCurvefit_opt.min_peak_energy
  ENDIF

  IF N_ELEMENTS(dont_fit_below_thresh_value) GT 0 THEN BEGIN
     ;; ADD_STR_ELEMENT,kCurvefit_opt,'thresh_eFlux',dont_fit_below_thresh_value
     kCurvefit_opt.thresh_eFlux              = dont_fit_below_thresh_value

     PRINT,FORMAT='("kCurvefit_opt.thresh_eFlux",T45,":",T48,I0)', $
     kCurvefit_opt.thresh_eFlux

  ENDIF

  IF N_ELEMENTS(estimate_A_from_data) GT 0 THEN BEGIN
     kCurvefit_opt.estimate_A_from_data      = estimate_A_from_data
  ENDIF

  IF N_ELEMENTS(add_gaussian_estimate) GT 0 THEN BEGIN
     kCurvefit_opt.add_gaussian_estimate     = add_gaussian_estimate

     PRINT,FORMAT='("kCurvefit_opt.add_gaussian_estimate",T45,":",T48,I0)', $
     kCurvefit_opt.add_gaussian_estimate
  ENDIF

  IF N_ELEMENTS(use_SDT_Gaussian_fit) GT 0 THEN BEGIN
     kCurvefit_opt.use_SDT_Gaussian_fit      = use_SDT_Gaussian_fit
  ENDIF

  IF N_ELEMENTS(use_mpFit1D) GT 0 THEN BEGIN
     kCurvefit_opt.use_mpFit1D               = use_mpFit1D

     PRINT,FORMAT='("kCurvefit_opt.use_mpFit1D",T45,":",T48,I0)', $
     kCurvefit_opt.use_mpFit1D
  ENDIF

  IF KEYWORD_SET(fit1D_to_density_at_each_angle) THEN BEGIN
     kCurvefit_opt.fit1D_dens__each_angle    = fit1D_to_density_at_each_angle

     PRINT,FORMAT='("kCurvefit_opt.fit1D_to_density_at_each_angle",T45,":",T48,I0)', $
     kCurvefit_opt.fit1D_to_density_at_each_angle
  ENDIF

  IF N_ELEMENTS(n_est) GT 0 THEN BEGIN
     kCurvefit_opt.fitA[3]                   = n_est

     PRINT,FORMAT='("kCurvefit_opt.fitA[3] (N)",T45,":",T48,I0)', $
     kCurvefit_opt.fitA[3]
  ENDIF

  IF N_ELEMENTS(T) GT 0 THEN BEGIN
     kcurvefit_opt.fitA[1]                   = T

     PRINT,FORMAT='("kCurvefit_opt.fitA[1] (T)",T45,":",T48,I0)', $
     kCurvefit_opt.fitA[1]
  ENDIF

  IF N_ELEMENTS(kappa) GT 0 THEN BEGIN
     kCurvefit_opt.fitA[2]                   = kappa

     PRINT,FORMAT='("kCurvefit_opt.fitA[2] (kappa)",T45,":",T48,I0)', $
     kCurvefit_opt.fitA[2]
  ENDIF

  IF N_ELEMENTS(bulk_offset) GT 0 THEN BEGIN
     kCurvefit_opt.bulk_offset               = bulk_offset

     PRINT,FORMAT='("kCurvefit_opt.bulk_offset",T45,":",T48,I0)', $
     kCurvefit_opt.bulk_offset
  ENDIF

  IF N_ELEMENTS(fit2D__only_fit_densAngles) GT 0 THEN BEGIN
     kCurvefit_opt.fit2D_only_dens_angles    = fit2D__only_fit_densAngles

     PRINT,FORMAT='("kCurvefit_opt.only_dens_angles",T45,":",T48,I0)', $
     kCurvefit_opt.only_dens_angles
  ENDIF

  IF N_ELEMENTS(fit2D__only_fit_eAngles) GT 0 THEN BEGIN
     kCurvefit_opt.fit2D_only_eAngles    = fit2D__only_fit_eAngles

     PRINT,FORMAT='("kCurvefit_opt.fit2D_only_eAngles",T45,":",T48,I0)', $
     kCurvefit_opt.fit2D_only_eAngles
  ENDIF

  IF N_ELEMENTS(fit2D__only_fit_peak_eRange) GT 0 THEN BEGIN
     kCurvefit_opt.fit2D_just_eRange_peak = fit2D__only_fit_peak_eRange

     PRINT,FORMAT='("kCurvefit_opt.fit2D_just_eRange_peak",T45,":",T48,I0)', $
     kCurvefit_opt.fit2D_just_eRange_peak
  ENDIF

  IF N_ELEMENTS(fit2D__only_fit_aboveMin) GT 0 THEN BEGIN
     kCurvefit_opt.fit2D_fit_above_minE = fit2D__only_fit_aboveMin

     PRINT,FORMAT='("kCurvefit_opt.fit2D_fit_above_minE",T45,":",T48,I0)', $
     kCurvefit_opt.fit2D_fit_above_minE
  ENDIF

  IF N_ELEMENTS(fit2D__bulk_e_anisotropy) GT 0 THEN BEGIN
     kCurvefit_opt.fit2D__bulk_e_anisotropy = fit2D__bulk_e_anisotropy

     PRINT,"Assuming anisotropic bulk energy ..."
  ENDIF

  IF KEYWORD_SET(kCurvefit_opt.fit2D__bulk_e_anisotropy) THEN BEGIN
     kCurvefit_opt.fit2D__bulk_e_anis_factor = (KEYWORD_SET(fit2D__bulk_e_anis_factor) ? $
                                               fit2D__bulk_e_anis_factor : 0.3)

     PRINT,FORMAT='("Bulk_E Anistropy Factor : ",F0.2)', $
           kCurvefit_opt.fit2D__bulk_e_anis_factor
  ENDIF

  IF N_ELEMENTS(fit2D__exclude_lca_from_densCalc) GT 0 THEN BEGIN
     kCurvefit_opt.fit2D__exclude_lca_from_densCalc = fit2D__exclude_lca_from_densCalc

     PRINT,FORMAT='("kCurvefit_opt.fit2D__exclude_lca_from_densCalc",T45,":",T48,I0)', $
     kCurvefit_opt.fit2D__exclude_lca_from_densCalc
  ENDIF

  IF N_ELEMENTS(fit2D__disable_bFunc) GT 0 THEN BEGIN
     kCurvefit_opt.fit2D__disable_bFunc = fit2D__disable_bFunc

     PRINT,FORMAT='("kCurvefit_opt.fit2D__disable_bFunc",T45,":",T48,I0)', $
     kCurvefit_opt.fit2D__disable_bFunc
  ENDIF

  IF N_ELEMENTS(fit2D__keep_wholeFit) GT 0 THEN BEGIN
     kCurvefit_opt.fit2D__keep_wholeFit = fit2D__keep_wholeFit

     PRINT,FORMAT='("kCurvefit_opt.fit2D__keep_wholeFit",T45,":",T48,I0)', $
     kCurvefit_opt.fit2D__keep_wholeFit
  ENDIF

  PRINT,''

  RETURN,kCurvefit_opt

END