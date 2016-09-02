FUNCTION INIT_KAPPA_SDTDATA_OPTIONS,EEB_OR_EES=eeb_or_ees, $
                                    SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                                    DO_ALL_TIMES=do_all_times, $
                                    ENERGY_ELECTRONS=energy_electrons, $
                                    ELECTRON_ANGLERANGE=electron_angleRange, $
                                    ELECTRON_LOSSCONE_ANGLE=electron_lca, $
                                    FIT2D_DENSITY_ANGLERANGE=fit2D_density_angleRange

  COMPILE_OPT idl2

  defEEB_or_EES                             = 'eeb'
  defSpectra_average_interval               = 0
  defEnergy_electrons                       = [4,3.5e4]
  defElectron_angleRange                    = [-20,20]
  defFit2D_dens_angleRange                  = [-20,20]

  kSDTData_opt                              = {eeb_or_ees          :defEEB_or_EES, $
                                               spec_avg_intvl      :defSpectra_average_interval, $
                                               do_all_times        :0, $
                                               energy_electrons    :[4,3.5e4], $
                                               routine             :'get_fa_' + defEEB_or_EES, $
                                               electron_anglerange :defElectron_angleRange, $
                                               electron_lca        :180., $
                                               fit2D_dens_aRange   :defFit2D_dens_angleRange, $
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
     kSDTData_opt.electron_angleRange       = electron_angleRange
     ;; PRINT,FORMAT='("SDT electron angle range",T45,":",T48,2(F0.2))', $
     PRINT,FORMAT='("kSDTData_opt.electron_angleRange",T45,":",T48,2(F0.2,:,", "))', $
           kSDTData_opt.electron_angleRange
  ENDIF
  
  IF N_ELEMENTS(electron_lca) GT 0 THEN BEGIN
     kSDTData_opt.electron_lca              = electron_lca

     ;; PRINT,FORMAT='("Loss-cone angle (not fitting angles outside this range): ",F0.2)', $
     PRINT,FORMAT='("kSDTData_opt.electron_lca",T45,":",T48,F0.2)', $
           kSDTData_opt.electron_lca
  ENDIF
  
  IF N_ELEMENTS(fit2D_density_angleRange) GT 0 THEN BEGIN
     kSDTData_opt.fit2D_dens_aRange         = fit2D_density_angleRange

     ;; PRINT,FORMAT='("SDT density angle range",T45,":",T48,2(F0.2))', $
     PRINT,FORMAT='("kSDTData_opt.fit2D_dens_aRange",T45,":",T48,2(F0.2,:,", "))', $
           kSDTData_opt.fit2D_dens_aRange
  ENDIF

  PRINT,''

  RETURN,kSDTData_opt

END