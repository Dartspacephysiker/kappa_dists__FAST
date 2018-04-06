;2018/04/06
;NWO!! Steiner, Hollywood—all of 'em

;; JOURNAL__20180117__BOOTSTRAP_ORB_1773_2D_DISTS_TO_GET_BESTFIT_PARAM_ERRORS,CARLOTIMESTART='09:26:41.669', $
;;    CARLOTIMESTOP='09:26:42.301'
PRO JOURNAL__20180406__BOOTSTRAP_ORB_1612_2D_DISTS_TO_GET_BESTFIT_PARAM_ERRORS,CARLOTIMESTART=carloTimeStart, $
   CARLOTIMESTOP=carloTimeStop

  COMPILE_OPT IDL2,STRICTARRSUBS

  @common__kappa_fit2d_structs.pro

  dir = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/'
  fil = '20180405-orb_1612-Kappa_fits_and_Gauss_fits-ees-horseshoe2d-AUTO-only_fit_peak_eRange-avg_itvl2.sav'

  diff_eFlux_dir = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/diff_eFlux/'
  diff_eFlux_fil = 'orb_1612-diff_eflux-ees-avg_itvl2-12_00_04__000-12_02_07__000.sav'

  orbit = 1612
  orbString = STRING(FORMAT='(I0)',orbit)

  nRolls         = 100
  ;; nRolls         = 10000
  ;; nRolls         = 10000

  make_fit2D_info     = 0       ;Much more info than we need
  make_fit2DParamArrs = 1       ;Juuust right
  add_gaussian_estimate = 1

  observed_dist  = 0

  saveSuff = 'orb'+orbString+'_2DMCarlo_ests__'
  saveDir = '/SPENCEdata/Research/Satellites/FAST/kappa_dists/saves_output_etc/' + $
            GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + '/'

  ;; from JOURNAL__20171222__THE_CLASSICS__RESPOND_TO_REFEREE__NOFIT_BELOW_PEAK_ENERGY
  chi2_over_dof_thresh = 100
  lowDens_thresh       = 0.05
  diffEflux_thresh     = 3D6
  nPkAbove_dEF_thresh  = 3

  make_bFunc_gFunc_plots = 0
  save_bFunc_gFunc_plots = 0

  print_2DFitInfo = 0
  ;; print_2DWinInfo = 1
  ;; carloTime = '09:27:01.57'     ;Time shown in Figure 2a title
  ;; carloTime = '09:27:01.261'    ;The correct time, since the average of this time and the next time (09:27:01.893) gives 09:27:01.57
  ;; carloTimeStart = '09:26:55'
  ;; carloTimeStop  = '09:27:05'

  ;; 'chine 1
  carloTimeStart = KEYWORD_SET(carloTimeStart) ? carloTimeStart : '12:00:29.79'
  carloTimeStop  = KEYWORD_SET(carloTimeStop ) ? carloTimeStop  : '12:00:48.7'

  ;; Some are already done
  ;; carloTimeStart = '09:26:11'
  ;; carloTimeStop  = '09:26:55'

  RESTORE,dir+fil
  RESTORE,diff_eFlux_dir+diff_eFlux_fil

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

  ;;2018/04/06 I think, because a few a months I shifted the times in fit2D{Gauss,Kappa}_inf_list to
  ;; represent the start of the period over which ESA obs are integrated, I here need to
  ;; artificially set back the diff_eFlux times by half the sample period
  offset = (tid[1]-tid[0])/2.
  diff_eFlux_inds = VALUE_CLOSEST2((diff_eFlux.time-offset),tid,/CONSTRAINED)
  badObsSynth_i   = WHERE(ABS((diff_eFlux.time[diff_eFlux_inds]-offset)-tid) GT 0.05,nBadObsSynth)
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
        carloK = obsStructArr[match_i]

        IF KEYWORD_SET(add_gaussian_estimate) THEN BEGIN
           carloG = obsStructArr[match_i]
        ENDIF

     ENDIF ELSE BEGIN
        carloK = fit2DKappa_inf_list[match_i].sdt

        IF KEYWORD_SET(add_gaussian_estimate) THEN BEGIN
           carloG = fit2DGauss_inf_list[match_i].sdt
        ENDIF

     ENDELSE

     ;; Whatever the case, use observed uncertainties
     carloK.ddata = (obsStructArr[match_i]).ddata
     IF KEYWORD_SET(add_gaussian_estimate) THEN BEGIN
        carloG.ddata = (obsStructArr[match_i]).ddata
     ENDIF

     ;; Params
     Pkappa         = fit2DKappa_inf_list[match_i].fitParams
     Pgauss         = fit2DGauss_inf_list[match_i].fitParams

     mass           = fit2DKappa_inf_list[match_i].sdt.mass

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

     KAPPA_FIT2D__MONTECARLO_UNCERTAINTY,carloK,carloG,Pkappa,Pgauss, $
                                         CURDATASTR=obsStructArr[match_i], $
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
