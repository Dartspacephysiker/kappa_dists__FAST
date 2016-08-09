FUNCTION KAPPA__SELECT_2DFIT_DENS,fit2D, $
                                  USE_DATA_DENS=use_data_dens, $
                                  CALC_FITDENS_OVER_ELECTRON_ARANGE=calc_fitDens__aRange, $
                                  ELECTRON_ANGLERANGE=e_aRange, $
                                  FITTYPE__STRING=fitType__string, $
                                  QUIET=quiet

  COMPILE_OPT idl2

  IF N_ELEMENTS(fitType__string) GT 0 THEN BEGIN
     ft_str = fitType__string
  ENDIF ELSE BEGIN
     ft_str = '2DFit'
  ENDELSE

  CASE 1 OF
     KEYWORD_SET(use_data_dens): BEGIN
        IF ~KEYWORD_SET(quiet) THEN PRINT,'Using data dens for ' + ft_str + ' ...'
        fitDens = fit2D.dataDens
     END
     KEYWORD_SET(calc_fitDens__aRange): BEGIN

        IF ~KEYWORD_SET(quiet) THEN PRINT,'Using SDT-calculated density for ' + ft_str + ' ...'
        IF ~KEYWORD_SET(quiet) THEN PRINT,FORMAT='("Angle range: [",F0.1,",",F0.1,"]")',e_aRange
        nSDT    = N_ELEMENTS(fit2D.SDT)
        fitDens = MAKE_ARRAY(nSDT,VALUE=0.,/FLOAT)
        FOR j=0,nSDT-1 DO BEGIN
           fitDens[j] = N_2D_FS(fit2D.SDT[j],ANGLE=e_aRange)
        ENDFOR
     END
     ELSE: BEGIN
        fitDens = fit2D.dens
     END
  ENDCASE

  RETURN,fitDens

END
