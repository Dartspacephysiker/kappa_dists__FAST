;2016/07/13
;To be used as part of plotting kappa model- and Maxwellian model-derived currents versus observed currents (cf. PLOT_KAPPA_MAXWELL_AND_OBSERVED_CURRENT)
PRO SETUP_POTENTIAL_AND_CURRENT,setup, $ 
                                obs_current,obsName,obsSuff, $
                                kappaPot,gaussPot, $
                                potName,potTitleStr, $
                                USE_JE_CURRENT=use_je_current, $
                                USE_JMAG_CURRENT=use_jMag_current, $
                                ;; USE_CHARE_POT=use_charE_pot, $
                                USE_BULKENERGY_POT=use_bulkEnergy_pot, $
                                BOTH_USE_KAPPA_BULKENERGY=both_use_kappa_bulkenergy, $
                                BOTH_USE_MAXWELL_BULKENERGY=both_use_maxwell_bulkenergy, $
                                NO_CHARI_FOR_POT=no_charI_for_pot, $
                                USE_CALCKED_JDIFF_EFLUX=use_calcked_Jdiff_eFlux

  COMPILE_OPT idl2

  ;; setup = {kappaS:Astruct, $
  ;;          gaussS:AStructGauss, $
  ;;          charE:charE_kappa_interp, $
  ;;          charI:charI_kappa_interp, $
  ;;          charTot:charTot_kappa_interp, $
  ;;          Jtot:Jtot_kappa_interp, $
  ;;          JMag:jMag_kappa_interp, $
  ;;          Je:Je_kappa_interp, $
  ;;          Ji:Ji_kappa_interp}

  ;;Get potential, suffixes for plot name
  potTitleStr        = "Potential: "
  CASE 1 OF
     KEYWORD_SET(both_use_maxwell_bulkenergy): BEGIN
        gaussPot     = setup.GaussS.bulk_energy
        kappaPot     = setup.GaussS.bulk_energy
        potName      = 'Gauss_bulkE'
        potTitleStr += 'Maxwell fit ' + (KEYWORD_SET(no_charI_for_pot) ? '' : ' + Char. i!U+!N energy')
     END
     KEYWORD_SET(both_use_kappa_bulkenergy): BEGIN
        gaussPot     = setup.KappaS.bulk_energy
        kappaPot     = setup.KappaS.bulk_energy
        potName      = 'kappa_bulkE'
        potTitleStr += 'Kappa fit ' + (KEYWORD_SET(no_charI_for_pot) ? '' : ' + Char. i!U+!N energy')
     END
     KEYWORD_SET(use_bulkEnergy_pot): BEGIN
        gaussPot     = setup.GaussS.bulk_energy
        kappaPot     = setup.KappaS.bulk_energy
        potName      = 'bulkE'
        potTitleStr += 'Fit-derived' + (KEYWORD_SET(no_charI_for_pot) ? '' : ' + Char. i!U+!N energy')
     END
     ELSE: BEGIN
        gaussPot     = setup.charE
        kappaPot     = setup.charE
        potName      = 'charE'
        potTitleStr += 'Char. e!U-!N' + (KEYWORD_SET(no_charI_for_pot) ? '' : ' + i!U+!N') + ' energy'
     END
  ENDCASE

  IF ~KEYWORD_SET(no_charI_for_pot) THEN BEGIN
     gaussPot       += setup.charI
     kappaPot       += setup.charI
     potName        += '_charI'
  ENDIF

  ;;Get current, suffixes for plot name
  CASE 1 OF
     KEYWORD_SET(use_je_current): BEGIN
        obs_current     = setup.Je
        obsName         = 'e- ESA'
        obsSuff         = 'e_ESA'
     END
     KEYWORD_SET(use_jMag_current): BEGIN
        obs_current  = setup.jMag
        obsName      = 'Fluxgate Mag-derived'
        obsSuff      = 'fluxgate'
     END
     KEYWORD_SET(use_calcked_Jdiff_eFlux): BEGIN
        obs_current  = setup.Jdiff_eFlux
        obsName      = 'e- ESA'
        obsSuff      = 'JDiff_e_ESA'
     END
     ELSE: BEGIN
        obs_current  = setup.Jtot
        obsName      = 'e- and i+ ESA'
        obsSuff      = 'both_ESAs'
     END
  ENDCASE


END