;2018/12/21
;NWO!! Steiner, Hollywood—all of 'em
PRO JOURNAL__20180117__BOOTSTRAP_ORB_1773_2D_DISTS_TO_GET_BESTFIT_PARAM_ERRORS

  COMPILE_OPT IDL2,STRICTARRSUBS

  @common__kappa_fit2d_structs.pro

  dir = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/'
  fil = '20180117-orb_1773-Kappa_fits_and_Gauss_fits-ees-horseshoe2d-classics-3-Elphic_et_al_1998-0BelowPk-GRLRESPONSEFINAL2-only_fit_peak_eRange-avg_itvl2.sav'

  diff_eFlux_dir = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/diff_eFlux/'
  diff_eFlux_fil = 'orb_1773-diff_eflux-ees-avg_itvl2-09_26_10__000-09_27_15__000.sav'

  nRolls         = 1000

  make_fit2D_info     = 0       ;Much more info than we need
  make_fit2DParamArrs = 1       ;Juuust right
  add_gaussian_estimate = 1

  observed_dist  = 0

  saveSuff = 'orb1773_2DMCarlo_ests__'
  saveDir = '/SPENCEdata/Research/Satellites/FAST/kappa_dists/saves_output_etc/' + $
            GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + '/'

  ;; from JOURNAL__20171222__THE_CLASSICS__RESPOND_TO_REFEREE__NOFIT_BELOW_PEAK_ENERGY
  chi2_over_dof_thresh = 25
  lowDens_thresh       = 0.05
  diffEflux_thresh     = 3e7
  nPkAbove_dEF_thresh  = 5

  make_bFunc_gFunc_plots = 0
  save_bFunc_gFunc_plots = 0

  print_2DFitInfo = 0
  ;; print_2DWinInfo = 1
  ;; carloTime = '09:27:01.57'     ;Time shown in Figure 2a title
  ;; carloTime = '09:27:01.261'    ;The correct time, since the average of this time and the next time (09:27:01.893) gives 09:27:01.57
  ;; carloTimeStart = '09:26:56'
  ;; carloTimeStop  = '09:27:03'

  ;; carloTimeStart = '09:26:11'
  ;; carloTimeStop  = '09:27:11'

  ;; Some are already done
  carloTimeStart = '09:26:11'
  carloTimeStop  = '09:26:55'

  RESTORE,dir+fil
  RESTORE,diff_eFlux_dir+diff_eFlux_fil

  ;; PARSE_KAPPA_FIT_STRUCTS,kappaFit1Ds, $
  ;;                         A=a, $
  ;;                         STRUCT_A=Astruct, $
  ;;                         TIME=time, $
  ;;                         NAMES_A=A_names, $
  ;;                         CHI2=chi2, $
  ;;                         PVAL=pVal, $
  ;;                         FITSTATUS=fitStatus, $
  ;;                         /USE_MPFIT1D

  kFit2DParam_struct = 1
  gFit2DParam_struct = 1

  fit2DK = PARSE_KAPPA_FIT2D_INFO_LIST_V2(fit2DKappa_inf_list, $
                                          SOUTH=south, $
                                          FIT_TYPE='Kappa', $
                                          HIGHDENSITY_THRESHOLD=highDens_thresh, $
                                          LOWDENSITY_THRESHOLD=lowDens_thresh, $
                                          CHI2_OVER_DOF_THRESHOLD=chi2_over_dof_thresh, $
                                          CHI2_THRESHOLD=chi2_thresh, $
                                          DIFFEFLUX_THRESHOLD=diffEflux_thresh, $
                                          N_PEAKS_ABOVE_DEF_THRESHOLD=nPkAbove_dEF_thresh, $
                                          OUT_GOOD_I=includeK_i, $
                                          OUT_FITPARAM_STRUCT=kFit2DParam_struct, $
                                          /DONT_SHRINK_PARSED_STRUCT)

  fit2DG = PARSE_KAPPA_FIT2D_INFO_LIST_V2(fit2DGauss_inf_list, $
                                          SOUTH=south, $
                                          FIT_TYPE='Maxwellian', $
                                          HIGHDENSITY_THRESHOLD=highDens_thresh, $
                                          LOWDENSITY_THRESHOLD=lowDens_thresh, $
                                          CHI2_OVER_DOF_THRESHOLD=chi2_over_dof_thresh, $
                                          CHI2_THRESHOLD=chi2_thresh, $
                                          DIFFEFLUX_THRESHOLD=diffEflux_thresh, $
                                          N_PEAKS_ABOVE_DEF_THRESHOLD=nPkAbove_dEF_thresh, $
                                          IN_GOOD_I=includeK_i, $
                                          OUT_GOOD_I=includeG_i, $
                                          OUT_FITPARAM_STRUCT=gFit2DParam_struct, $
                                          /DONT_SHRINK_PARSED_STRUCT)

  ;;Now shrink everyone
  IF ~( ARRAY_EQUAL(includeK_i,includeG_i)                          AND $
        (N_ELEMENTS(kappaFit1Ds)  EQ N_ELEMENTS(gaussFit1Ds)           ) AND $
        (N_ELEMENTS(kappaFit1Ds)  EQ N_ELEMENTS(fit2DKappa_inf_list) ) AND $
        (N_ELEMENTS(includeK_i) EQ N_ELEMENTS(fit2DKappa_inf_list) ) ) THEN BEGIN

     IF N_ELEMENTS(kappaFit1Ds) NE N_ELEMENTS(gaussFit1Ds) THEN STOP
     IF N_ELEMENTS(fit2DKappa_inf_list) NE N_ELEMENTS(fit2DGauss_inf_list) THEN STOP
     IF N_ELEMENTS(kappaFit1Ds) NE N_ELEMENTS(fit2DKappa_inf_list) THEN STOP

     include_i = CGSETINTERSECTION(includeK_i,includeG_i)

     fit2DK            = {SDT          : fit2DK.SDT         [include_i], $
                          fitParams    : fit2DK.fitParams   [*,include_i], $
                          obsMoms      : fit2DK.obsMoms     [include_i], $
                          fitMoms      : fit2DK.fitMoms     [include_i], $
                          moment_info  : fit2DK.moment_info [include_i], $
                          ;; fitDens      : fit2DK.fitDens     [include_i], $
                          chi2         : fit2DK.chi2        [include_i], $
                          errMsg       : fit2DK.errMsg      [include_i], $
                          status       : fit2DK.status      [include_i], $
                          nfEv         : fit2DK.nfEv        [include_i], $
                          ;; best_resid   : best_resid      [include_i], $
                          pFree_index  : fit2DK.pFree_index [*,include_i], $
                          ;; best_fJac    : best_fJac       [include_i], $
                          nPegged      : fit2DK.nPegged     [include_i], $
                          nFree        : fit2DK.nFree       [include_i], $
                          dof          : fit2DK.dof         [include_i], $
                          covar        : fit2DK.covar       [*,*,include_i], $
                          pError       : fit2DK.pError      [*,include_i], $
                          nIter        : fit2DK.nIter       [include_i]}

     fit2DG            = {SDT          : fit2DG.SDT         [include_i], $
                          fitParams    : fit2DG.fitParams   [*,include_i], $
                          obsMoms      : fit2DG.obsMoms     [include_i], $
                          fitMoms      : fit2DG.fitMoms     [include_i], $
                          moment_info  : fit2DG.moment_info [include_i], $
                          chi2         : fit2DG.chi2        [include_i], $
                          errMsg       : fit2DG.errMsg      [include_i], $
                          status       : fit2DG.status      [include_i], $
                          nfEv         : fit2DG.nfEv        [include_i], $
                          ;; best_resid   : best_resid      [include_i], $
                          pFree_index  : fit2DG.pFree_index [*,include_i], $
                          ;; best_fJac    : best_fJac       [include_i], $
                          nPegged      : fit2DG.nPegged     [include_i], $
                          nFree        : fit2DG.nFree       [include_i], $
                          dof          : fit2DG.dof         [include_i], $
                          covar        : fit2DG.covar       [*,*,include_i], $
                          pError       : fit2DG.pError      [*,include_i], $
                          nIter        : fit2DG.nIter       [include_i]}

     fit2DKappa_inf_list = fit2DKappa_inf_list[include_i]
     fit2DGauss_inf_list = fit2DGauss_inf_list[include_i]

     kFit2DParam_struct = {bulk_energy  : kFit2DParam_struct.bulk_energy[include_i], $
                           temperature  : kFit2DParam_struct.temperature[include_i], $
                           kappa        : kFit2DParam_struct.kappa      [include_i], $
                           n            : kFit2DParam_struct.n          [include_i]}

     gFit2DParam_struct = {bulk_energy  : gFit2DParam_struct.bulk_energy[include_i], $
                           temperature  : gFit2DParam_struct.temperature[include_i], $
                           kappa        : gFit2DParam_struct.kappa      [include_i], $
                           n            : gFit2DParam_struct.n          [include_i]}

  ENDIF

  nFits = N_ELEMENTS(fit2DKappa_inf_list)
  tid   = MAKE_ARRAY(nFits,/DOUBLE)
  tidString = MAKE_ARRAY(nFits,/STRING)
  FOR k=0,nFits-1 DO BEGIN
     tid[k]       = (fit2DKappa_inf_list[k]).sdt.time
     tidString[k] = T2S(tid[k],/MS)
  ENDFOR

  ;; Now need to match diff_eFlux structs with majic fitStructs
  diff_eFlux_inds = VALUE_CLOSEST2(diff_eFlux.time,tid,/CONSTRAINED)
  badObsSynth_i   = WHERE(ABS(diff_eFlux.time[diff_eFlux_inds]-tid) GT 0.05,nBadObsSynth)
  IF nBadObsSynth GT 0 THEN STOP

  ;; Now get obs structs
  obsData    = MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(diff_eFlux,diff_eFlux_inds[0])
  obsStructArr = REPLICATE(TEMPORARY(obsData),nFits)
  FOR k=1,nFits-1 DO BEGIN
     obsStructArr[k] = MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(diff_eFlux,diff_eFlux_inds[k])
  ENDFOR

  ;; match_i      = WHERE(STRMATCH(tid, '*' + carloTime      + '*', /FOLD_CASE))
  match_iStart = WHERE(STRMATCH(tidString, '*' + carloTimeStart + '*', /FOLD_CASE))
  match_iStop  = WHERE(STRMATCH(tidString, '*' + carloTimeStop  + '*', /FOLD_CASE))

  nRollStr    = STRING(FORMAT='("-",I0,"Rolls")',nRolls)

  gaussString = KEYWORD_SET(add_gaussian_estimate) ? '_wGauss'      : ''
  obsString   = observed_dist                      ? '_obs'         : '_synthetic'
  saveInfStr  = KEYWORD_SET(make_fit2D_info      ) ? '-info_lists'  : ''
  saveParmStr = KEYWORD_SET(make_fit2DParamArrs  ) ? '-fit2DParams' : ''

  inds = [match_iStart[0]:match_iStop[0]]
  nHjar = N_ELEMENTS(inds)
  FOR k=0,nHjar-1 DO BEGIN

     match_i = (inds[k])[0]

     ;; Now get the data
     IF observed_dist THEN BEGIN
        carlo = obsStructArr[match_i]

     ENDIF ELSE BEGIN
        carlo = fit2DKappa_inf_list[match_i].sdt
        ;; carlo = synthPackage[1,match_i]

        ;; shiftTheta = 0
        ;; carlo.data = KAPPA_FLUX2D__HORSESHOE__ENERGY_ANISOTROPY__COMMON( $
        ;;                 carlo.energy, $
        ;;                 SHIFT(carlo.theta,0,shiftTheta), $
        ;;                 fit2DKappa_inf_list[match_i].fitParams, $
        ;;                 UNITS=units, $
        ;;                 MASS=carlo.mass)

        ;; WAIT! Use best-fit param data, but experimental error!
        ;; tmpStr          = CONV_UNITS(carlo,'counts')
        ;; tmpStr.ddata    = (tmp3d.data)^.5
        ;; carlo            = CONV_UNITS(TEMPORARY(tmpStr),units)

     ENDELSE

     ;; energy_inds = WHERE(carlo.energy[*,0] LE 34120.)

     ;; nEnergies = N_ELEMENTS(energy_inds)
     ;; nAngles = carlo.nBins

     ;; carlo = { $
     ;;        data_name        : carlo.data_name, $
     ;;        valid            : carlo.valid, $
     ;;        project_name     : carlo.project_name, $
     ;;        units_name       : carlo.units_name, $
     ;;        units_procedure  : carlo.units_procedure, $
     ;;        time             : carlo.time, $
     ;;        end_time         : carlo.end_time, $
     ;;        integ_t          : carlo.integ_t, $
     ;;        nbins            : carlo.nbins, $
     ;;        nenergy          : nEnergies, $
     ;;        data             : carlo.data[energy_inds, *], $
     ;;        ddata            : carlo.ddata[energy_inds, *], $
     ;;        energy           : carlo.energy[energy_inds, *], $
     ;;        theta            : carlo.theta[energy_inds, *], $
     ;;        geom             : carlo.geom[energy_inds, *], $
     ;;        denergy          : carlo.denergy[energy_inds, *], $
     ;;        dtheta           : carlo.dtheta, $
     ;;        eff              : carlo.eff[energy_inds], $
     ;;        mass             : carlo.mass, $
     ;;        geomfactor       : carlo.geomfactor, $
     ;;        header_bytes     : carlo.header_bytes, $
     ;;        st_index         : carlo.st_index, $
     ;;        en_index         : carlo.en_index, $
     ;;        npts             : carlo.npts, $
     ;;        index            : carlo.index}

     ;; Whatever the case, use observed uncertainties
     carlo.ddata = (obsStructArr[match_i]).ddata

     ;; carlo = {x      : kappaFits[match_i[0]].x, $
     ;;         y      : kappaFits[match_i[0]].y, $
     ;;         yerror : kappaFits[match_i[0]].orig.yerror[kappaFits[match_i[0]].orig.energy_inds[0]:kappaFits[match_i[0]].orig.energy_inds[1]]}

     ;; Params
     Pkappa         = fit2DKappa_inf_list[match_i].fitParams
     Pgauss         = fit2DGauss_inf_list[match_i].fitParams

     mass           = fit2DKappa_inf_list[match_i].sdt.mass

     ;; PRINT,carlo.x[carlo.energy_inds[0]:carlo.energy_inds[1]] ;Range of energies used

     tmpTidString = STRSPLIT(tidString[match_i],'/',/EXTRACT)
     IF N_ELEMENTS(tmpTidString) GT 0 THEN tmpTidString = tmpTidString[1] ELSE tmpTidString = tmpTidString[0]
     PRINT,FORMAT='(I05,T10,A0)',k,tmpTidString

     tidFNStr = STRJOIN(STRSPLIT(tmpTidString,':',/EXTRACT),'_')
     tidFNStr = STRJOIN(STRSPLIT(tidFNStr,'.',/EXTRACT),'__')

     utFil = saveSuff + tidFNStr +obsString+gaussString+nRollStr+saveInfStr+saveParmStr
     utFil = utFil+'.sav'

     fit2DKappa_info = fit2DKappa_inf_list[match_i]
     fit2DGauss_info = fit2DGauss_inf_list[match_i]

     IF fit2DKappa_info.sdt.time NE fit2DGauss_info.sdt.time THEN STOP

     KAPPA_FIT2D__MONTECARLO_UNCERTAINTY,carlo,Pkappa,Pgauss, $
                                         TIDFNSTR=tidFNStr, $
                                         NROLLS=nRolls, $
                                         NOT_MPFIT1D=not_mpFit1D, $
                                         KCURVEFIT_OPT=KF2D__CURVEFIT_OPT, $
                                         OBSERVED_DIST=observed_dist, $
                                         BOOTSTRAP_MODE=bootstrap, $
                                         ADD_GAUSSIAN_ESTIMATE=add_gaussian_estimate, $
                                         MASS=mass, $
                                         FIT2DKAPPA_INFO=fit2DKappa_info, $
                                         FIT2DGAUSS_INFO=fit2DGauss_info, $
                                         MAKE_FIT2D_INFO=make_fit2D_info, $
                                         MAKE_FIT2DPARAMARRS=make_fit2DParamArrs, $
                                         MAKE_BFUNC_GFUNC_PLOTS=make_bFunc_gFunc_plots, $
                                         SAVE_BFUNC_GFUNC_PLOTS=save_bFunc_gFunc_plots, $
                                         SAVEFILE=utFil, $
                                         SAVEDIR=saveDir, $
                                         PRINT_2DFITINFO=print_2DFitInfo, $
                                         PRINT_2DWININFO=print_2DWinInfo

  ENDFOR


END
