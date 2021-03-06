;2018/04/06
;NWO!! Steiner, Hollywood—all of 'em

;; JOURNAL__20180117__BOOTSTRAP_ORB_1773_2D_DISTS_TO_GET_BESTFIT_PARAM_ERRORS,CARLOTIMESTART='09:26:41.669', $
;;    CARLOTIMESTOP='09:26:42.301'
PRO JOURNAL__20180406__BOOTSTRAP_ORB_1612_2D_DISTS_TO_GET_BESTFIT_PARAM_ERRORS, $
   CARLOTIMESTART=carloTimeStart, $
   CARLOTIMESTOP=carloTimeStop, $
   CHECK_FOR_AND_ONLY_DO_BADDIES=check_for_and_only_do_baddies, $
   DIR_TO_CHECK=dir_to_check

  COMPILE_OPT IDL2,STRICTARRSUBS

  @common__kappa_fit2d_structs.pro

  IF KEYWORD_SET(check_for_and_only_do_baddies) THEN BEGIN

     IF ~KEYWORD_SET(dir_to_check) THEN BEGIN
        PRINT,"If you want me to check, gotta provide a directory to check, fool!"
        RETURN
     ENDIF

     extraFactor = 5.0

     PRINT,FORMAT='("Check for baddies dir : ",A0  )',dir_to_check
     PRINT,FORMAT='("Extra factor          : ",F0.2)',extraFactor
  ENDIF

  spectra_average_interval = 2
  avgItvlStr = STRING(FORMAT='("-avg_itvl",I0)',spectra_average_interval)

  dir = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/'
  fil = '20180409-orb_1612-Kappa_fits_and_Gauss_fits-ees-horseshoe2d-AUTO-only_fit_peak_eRange' + $
        avgItvlStr + '.sav'

  diff_eFlux_dir = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/diff_eFlux/'
  diff_eFlux_fil = 'orb_1612-diff_eflux-ees' + $
                   avgItvlStr + (spectra_average_interval EQ 3      ? $
                                '-12_00_24__000-12_01_47__000.sav' : $
                                '-12_00_04__000-12_02_07__000.sav')

  orbit = 1612
  orbString = STRING(FORMAT='(I0)',orbit)

  ;; nRolls         = 1000
  nRolls         = 10000
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

  ;; 'chine 1
  ;; carloTimeStart = KEYWORD_SET(carloTimeStart) ? carloTimeStart : '1997-01-17/12:00:29.79'
  ;; carloTimeStop  = KEYWORD_SET(carloTimeStop ) ? carloTimeStop  : '1997-01-17/12:00:48.7'
  ;; carloTimeStart = KEYWORD_SET(carloTimeStart) ? carloTimeStart : '1997-01-17/12:01:12.044'
  ;; carloTimeStop  = KEYWORD_SET(carloTimeStop ) ? carloTimeStop  : '1997-01-17/12:01:12.999'

  ;; Testing CHECK_FOR_BADDIES stuff
  carloTimeStart = KEYWORD_SET(carloTimeStart) ? carloTimeStart : '1997-01-17/12:00:24'
  carloTimeStop  = KEYWORD_SET(carloTimeStop ) ? carloTimeStop  : '1997-01-17/12:00:30'

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
  ;; offset = 0.
  diff_eFlux_inds = VALUE_CLOSEST2((diff_eFlux.time-offset),tid,/CONSTRAINED)
  badObsSynth_i   = WHERE(ABS((diff_eFlux.time[diff_eFlux_inds]-offset)-tid) GT 0.05,nBadObsSynth)
  IF nBadObsSynth GT 0 THEN STOP

  ;; Now get obs structs
  obsData    = MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(diff_eFlux,diff_eFlux_inds[0])
  obsStructArr = REPLICATE(TEMPORARY(obsData),nFits)
  FOR k=1,nFits-1 DO BEGIN
     obsStructArr[k] = MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(diff_eFlux,diff_eFlux_inds[k])
  ENDFOR

  ;; Old old way
  ;; match_i      = WHERE(STRMATCH(tid, '*' + carloTime      + '*', /FOLD_CASE))
  ;; match_iStart = WHERE(STRMATCH(tidString, '*' + carloTimeStart + '*', /FOLD_CASE))
  ;; match_iStop  = WHERE(STRMATCH(tidString, '*' + carloTimeStop  + '*', /FOLD_CASE))

  ;; Old way
  ;; match_iStart = VALUE_CLOSEST2(tid,S2T(carloTimeStart),/CONSTRAINED)
  ;; match_iStop  = VALUE_CLOSEST2(tid,S2T(carloTimeStop),/CONSTRAINED)

  ;; new way
  inds = WHERE((tid GE S2T(carloTimeStart)) AND $
               (tid LT S2T(carloTimeStop )), $
               nInds)
  IF nInds EQ 0 THEN BEGIN
     PRINT,"Nussing!"
     RETURN
  ENDIF

  nRollStr    = STRING(FORMAT='("-",I0,"Rolls")',nRolls)

  gaussString = KEYWORD_SET(add_gaussian_estimate) ? '_wGauss'      : ''
  obsString   = observed_dist                      ? '_obs'         : '_synthetic'
  saveInfStr  = KEYWORD_SET(make_fit2D_info      ) ? '-info_lists'  : ''
  saveParmStr = KEYWORD_SET(make_fit2DParamArrs  ) ? '-fit2DParams' : ''

  ;; inds = [match_iStart[0]:match_iStop[0]]
  nHjar = N_ELEMENTS(inds)
  FOR k=0,nHjar-1 DO BEGIN

     match_i = (inds[k])[0]

     tmpTidString = STRSPLIT(tidString[match_i],'/',/EXTRACT)
     IF N_ELEMENTS(tmpTidString) GT 0 THEN tmpTidString = tmpTidString[1] ELSE tmpTidString = tmpTidString[0]
     PRINT,FORMAT='(I05,T10,A0)',k,tmpTidString

     tidFNStr = STRJOIN(STRSPLIT(tmpTidString,':',/EXTRACT),'_')
     tidFNStr = STRJOIN(STRSPLIT(tidFNStr,'.',/EXTRACT),'__')

     utFil = saveSuff + tidFNStr +obsString+gaussString+nRollStr+saveInfStr+saveParmStr + avgItvlStr
     utFil = utFil+'.sav'


     IF KEYWORD_SET(check_for_and_only_do_baddies) THEN BEGIN

        checkFil = utFil.Replace('.sav','__BAD.sav')
        IF ~FILE_TEST(dir_to_check+checkFil) THEN BEGIN
           PRINT,FORMAT='("Nowhere in sight: ",A0)',checkFil
           PRINT,"Continuing!"
           CONTINUE
        ENDIF

        PRINT,FORMAT='("Got '+"'"+'im: ",A0)',checkFil

     ENDIF

     fit2DKappa_info = fit2DKappa_inf_list[match_i]
     fit2DGauss_info = fit2DGauss_inf_list[match_i]

     ;; Params
     Pkappa         = fit2DKappa_info.fitParams
     Pgauss         = fit2DGauss_info.fitParams

     ;; extraDensDivFac = 1.
     ;; Pkappa[3] = Pkappa[3] / fit2DKappa_info.nAngle / extraDensDivFac
     ;; Pgauss[3] = Pgauss[3] / fit2DGauss_info.nAngle / extraDensDivFac
     Pkappa[3] = fit2DKappa_info.fit1D.A[3] / fit2DKappa_info.nAngle
     Pgauss[3] = fit2DKappa_info.fit1D.A[3] / fit2DKappa_info.nAngle

     ;; Now get the data
     IF observed_dist THEN BEGIN
        carloK = obsStructArr[match_i]

        IF KEYWORD_SET(add_gaussian_estimate) THEN BEGIN
           carloG = obsStructArr[match_i]
        ENDIF

     ENDIF ELSE BEGIN
        carloK = fit2DKappa_info.sdt

        IF KEYWORD_SET(add_gaussian_estimate) THEN BEGIN
           carloG = fit2DGauss_info.sdt
        ENDIF

     ENDELSE

     curDataStr = obsStructArr[match_i]

     ;; Whatever the case, use observed uncertainties
     carloK.ddata = (curDataStr).ddata
     IF KEYWORD_SET(add_gaussian_estimate) THEN BEGIN
        carloG.ddata = (curDataStr).ddata
     ENDIF

     mass           = fit2DKappa_info.sdt.mass

     ;;2018/04/60 DIAGNOSTIC PLOT
     ;;s=curdatastr & junk = MIN(ABS(s.theta[0,*]),ind) & wind = WINDOW(DIMENSIONS=[800,800]) & this = ERRORPLOT(s.energy[*,ind],s.data[*,ind],s.ddata[*,ind],/XLOG,/YLOG,LINESTYLE='',SYMBOL='+',YRANGE=[1e5,1e10],XRANGE=[5e0,3e4],XTITLE='$\Delta \Phi$ (V)',YTITLE='Diff. Energy Flux',CURRENT=wind) & s = carlok & that = ERRORPLOT(s.energy[*,ind],s.data[*,ind],s.ddata[*,ind],/XLOG,/YLOG,YRANGE=[1e5,1e10],XRANGE=[5e0,3e4],COLOR='red',ERRORBAR_COLOR='RED',/OVERPLOT,CURRENT=wind) & vert = PLOT(REPLICATE(fit2dkappa_info.extra_info.eRange_fit[0],10),10.D^(DINDGEN(10)+1.D),LINESTYLE='--',COLOR='GREEN',/OVERPLOT,CURRENT=wind)

     IF fit2DKappa_info.sdt.time NE fit2DGauss_info.sdt.time THEN STOP

     ;; Just checking to see what we get back
     ;; energy = (carloK.energy[*,42])[WHERE(carloK.energy[*,42] GE fit2dkappa_info.extra_info.erange_fit[0])]
     ;; this = KAPPA_FLUX__LINEAR_SHIFT_IN_ENERGY( $
     ;;        energy, $
     ;;        Pkappa, $
     ;;        UNITS='Flux', $
     ;;        MASS=carloK.mass)
     ;; thisPlot = PLOT(energy, $
     ;;                 this, $
     ;;                 /XLOG, $
     ;;                 /YLOG, $
     ;;                 YRANGE=[MIN(this)>1E1,max(this)*1.2], $
     ;;                 XRANGE=MINMAX(energy))     
     ;; tmpCarloK = carloK
     ;; CONVERT_ESA_UNITS2,tmpCarloK,'FLUX'
     ;; thatPlot = ERRORPLOT(tmpCarloK.energy[*,42], $
     ;;                      tmpCarloK.data[*,42], $
     ;;                      tmpCarloK.ddata[*,42], $
     ;;                      /XLOG, $
     ;;                      /YLOG, $
     ;;                      /OVERPLOT, $
     ;;                      LINESTYLE='--', $
     ;;                      COLOR='RED')

     KAPPA_FIT2D__MONTECARLO_UNCERTAINTY,carloK,carloG,Pkappa,Pgauss, $
                                         CURDATASTR=curDataStr, $
                                         TIDFNSTR=tidFNStr, $
                                         NROLLS=nRolls, $
                                         FACTOR_BY_WHICH_TO_INCREASE_UNCERT_ARRAY=extraFactor, $
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
