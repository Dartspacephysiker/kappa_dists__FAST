FUNCTION INIT_KAPPA_SDTDATA_OPTIONS,EEB_OR_EES=eeb_or_ees, $
                                    SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                                    DO_ALL_TIMES=do_all_times, $
                                    ENERGY_ELECTRONS=energy_electrons, $
                                    ELECTRON_ANGLERANGE=electron_angleRange, $
                                    MANUAL_ANGLE_CORRECTION=manual_angle_correction, $
                                    ;; ELECTRON_LOSSCONE_ANGLE=electron_lca, $
                                    FIT2D__DENSITY_ANGLERANGE=fit2D__density_angleRange, $
                                    FIT2D__TEMPERATURE_ANGLERANGE=fit2D__temperature_angleRange, $
                                    FIT2D__FACONDUCTANCE_ANGLERANGE=fit2D__faConductance_angleRange, $
                                    FIT2D__ESTIMATE_DENS_ARANGE_FROM_DIST=fit2D__estimate_sourceCone_from_dist, $
                                    _EXTRA=e

  COMPILE_OPT IDL2,STRICTARRSUBS

  defEEB_or_EES                = 'eeb'
  defSpectra_average_interval  = 0
  defEnergy_electrons          = [4,3.5e4]
  ;; defElectron_angleRange       = [-32,32]
  ;; defElectron_angleRange       = [150,-150]
  defElectron_angleRange       = 'lc'
  defFit2D_dens_angleRange     = [-150,150]
  defFit2D_faCond_angleRange   = [-150,150]
  defFit2D_temp_angleRange     = 'lc'

  kSDTData_opt  = {eeb_or_ees          :defEEB_or_EES, $
                   spec_avg_intvl      :defSpectra_average_interval, $
                   do_all_times        :0, $
                   energy_electrons    :[4,3.5e4], $
                   routine             :'get_fa_' + defEEB_or_EES, $
                   electron_anglerange :defElectron_angleRange, $
                   manual_angle_correction : 0, $
                   ;; electron_lca        :[-180.,180.], $
                   fit2D_dens_aRange   :defFit2D_dens_angleRange, $
                   fit2D_temp_aRange   :defFit2D_temp_angleRange, $ 
                   fit2D_faCond_aRange :defFit2D_faCond_angleRange, $ 
                   estimate_sourceCone_from_dist : 0B, $
                   densFunc            :'N_2D_FS'} ;the best choice for both EES and EEB—look at documentation


  IF N_ELEMENTS(eeb_or_ees) GT 0 THEN BEGIN
     kSDTData_opt.eeb_or_ees                = eeb_or_ees
     kSDTData_opt.routine                   = 'get_fa' + eeb_or_ees
  ENDIF
  
  IF N_ELEMENTS(spectra_average_interval) GT 0 THEN BEGIN
     kSDTData_opt.spec_avg_intvl  = spectra_average_interval

     ;;Note, only if spectra_average_interval is GT 0 do we do something about it 
     IF KEYWORD_SET(spectra_average_interval) THEN BEGIN
        kSDTData_opt.routine += '_ts'
     ENDIF

  ENDIF

  IF N_ELEMENTS(do_all_times) GT 0 THEN BEGIN
     kSDTData_opt.do_all_times              = do_all_times
     PRINT,FORMAT='(A0)', $
           'Doing all times ...'
  ENDIF

  IF N_ELEMENTS(energy_electrons) GT 0 THEN BEGIN
     kSDTData_opt.energy_electrons          = energy_electrons
     ;; PRINT,FORMAT='("SDT electron energy range",T45,":",T48,2(F0.2))', $
     PRINT,FORMAT='("kSDTData_opt.energy_electrons",T45,":",T48,2(F0.2,:,", "))', $
           kSDTData_opt.energy_electrons
  ENDIF

  IF N_ELEMENTS(electron_angleRange) GT 0 THEN BEGIN
     ;; kSDTData_opt.electron_angleRange      = electron_angleRange
     STR_ELEMENT,kSDTData_opt,'electron_angleRange',electron_angleRange,/ADD_REPLACE
     ;; PRINT,FORMAT='("SDT electron angle range",T45,":",T48,2(F0.2))', $
     CASE SIZE(electron_angleRange,/TYPE) OF
        7: BEGIN
           PRINT,FORMAT='("kSDTData_opt.electron_angleRange",T45,":",T48,A0)', $
                 kSDTData_opt.electron_angleRange
        END
        ELSE: BEGIN
           PRINT,FORMAT='("kSDTData_opt.electron_angleRange",T45,":",T48,2(F0.2,:,", "))', $
                 kSDTData_opt.electron_angleRange
        END
     ENDCASE
  ENDIF
  ;; kSDTData_opt.fit2D_dens_aRange           = kSDTData_opt.electron_angleRange
  
  ;; IF N_ELEMENTS(electron_lca) GT 0 THEN BEGIN
  ;;    CASE N_ELEMENTS(electron_lca) OF
  ;;       1: BEGIN
  ;;          kSDTData_opt.electron_lca       = [ABS(electron_lca),-ABS(electron_lca)]
  ;;       END
  ;;       2: BEGIN
  ;;          kSDTData_opt.electron_lca       = electron_lca
  ;;         END
  ;;       ELSE: BEGIN
  ;;          PRINT,'Bogus lca provided! Has to be one or two elements.'
  ;;          STOP
  ;;       END
  ;;    ENDCASE
  ;;    PRINT,FORMAT='("kSDTData_opt.electron_lca",T45,":",T48,2(F0.2,:,", "))', $
  ;;          kSDTData_opt.electron_lca
  ;;    PRINT,"     (i.e., not fitting angles outside the above range)"
  ;;    ;; PRINT,FORMAT='("kSDTData_opt.electron_lca",T45,":",T48,F0.2)', $
  ;;    ;;       kSDTData_opt.electron_lca
  ;;    ;; PRINT,FORMAT='("kSDTData_opt.fit2D_dens_aRange",T45,":",T48,2(F0.2,:,", "))', $
  ;;    ;;       kSDTData_opt.fit2D_dens_aRange
  ;; ENDIF
  
  IF N_ELEMENTS(manual_angle_correction) GT 0 THEN BEGIN
     STR_ELEMENT,kSDTData_opt,'manual_angle_correction',manual_angle_correction,/ADD_REPLACE
     CASE SIZE(manual_angle_correction,/TYPE) OF
        7: BEGIN
           PRINT,FORMAT='("kSDTData_opt.manual_angle_correction",T45,":",T48,A0)', $
                 kSDTData_opt.manual_angle_correction
        END
        ELSE: BEGIN
           PRINT,FORMAT='("kSDTData_opt.manual_angle_correction",T45,":",T48,2(F0.2,:,", "))', $
                 kSDTData_opt.manual_angle_correction
        END
     ENDCASE
  ENDIF


  IF N_ELEMENTS(fit2D__density_angleRange) GT 0 THEN BEGIN
     ;; kSDTData_opt.fit2D_dens_aRange      = fit2D__density_angleRange
     STR_ELEMENT,kSDTData_opt,'fit2D_dens_aRange',fit2D__density_angleRange,/ADD_REPLACE
     ;; PRINT,FORMAT='("SDT electron angle range",T45,":",T48,2(F0.2))', $
     CASE SIZE(fit2D__density_angleRange,/TYPE) OF
        7: BEGIN
           PRINT,FORMAT='("kSDTData_opt.fit2D_dens_aRange",T45,":",T48,A0)', $
                 kSDTData_opt.fit2D_dens_aRange
        END
        ELSE: BEGIN
           PRINT,FORMAT='("kSDTData_opt.fit2D_dens_aRange",T45,":",T48,2(F0.2,:,", "))', $
                 kSDTData_opt.fit2D_dens_aRange
        END
     ENDCASE
  ENDIF

  IF N_ELEMENTS(fit2D__temperature_angleRange) GT 0 THEN BEGIN
     ;; kSDTData_opt.fit2D_temp_aRange      = fit2D__temperature_angleRange
     STR_ELEMENT,kSDTData_opt,'fit2D_temp_aRange',fit2D__temperature_angleRange,/ADD_REPLACE
     ;; PRINT,FORMAT='("SDT electron angle range",T45,":",T48,2(F0.2))', $
     CASE SIZE(fit2D__temperature_angleRange,/TYPE) OF
        7: BEGIN
           PRINT,FORMAT='("kSDTData_opt.fit2D_temp_aRange",T45,":",T48,A0)', $
                 kSDTData_opt.fit2D_temp_aRange
        END
        ELSE: BEGIN
           PRINT,FORMAT='("kSDTData_opt.fit2D_temp_aRange",T45,":",T48,2(F0.2,:,", "))', $
                 kSDTData_opt.fit2D_temp_aRange
        END
     ENDCASE
  ENDIF

  IF N_ELEMENTS(fit2D__faConductance_angleRange) GT 0 THEN BEGIN
     ;; kSDTData_opt.fit2D_faCond_aRange      = fit2D__faConductance_angleRange
     STR_ELEMENT,kSDTData_opt,'fit2D_faCond_aRange',fit2D__faConductance_angleRange,/ADD_REPLACE
     ;; PRINT,FORMAT='("SDT electron angle range",T45,":",T48,2(F0.2))', $
     CASE SIZE(fit2D__faConductance_angleRange,/TYPE) OF
        7: BEGIN
           PRINT,FORMAT='("kSDTData_opt.fit2D_faCond_aRange",T45,":",T48,A0)', $
                 kSDTData_opt.fit2D_faCond_aRange
        END
        ELSE: BEGIN
           PRINT,FORMAT='("kSDTData_opt.fit2D_faCond_aRange",T45,":",T48,2(F0.2,:,", "))', $
                 kSDTData_opt.fit2D_faCond_aRange
        END
     ENDCASE
  ENDIF

  IF N_ELEMENTS(fit2D__estimate_sourceCone_from_dist) GT 0 THEN BEGIN
     kSDTData_opt.estimate_sourceCone_from_dist  = fit2D__estimate_sourceCone_from_dist

  ENDIF

  PRINT,''

  RETURN,kSDTData_opt

END