;2018/04/19
PRO KAPPA_RAWSAVE_PARSER, $
   KAPPAFIT1DS=kappaFit1Ds, $
   GAUSSFIT1DS=gaussFit1Ds, $
   KAPPA_INF_LIST=fit2DKappa_inf_list, $
   GAUSS_INF_LIST=fit2DGauss_inf_list, $
   KFIT2DPARAM_STRUCT=kFit2DParam_struct, $
   GFIT2DPARAM_STRUCT=gFit2DParam_struct, $
   FIT2DK=fit2DK, $
   FIT2DG=fit2DG, $
   USE_MPFIT1D=use_mpFit1D, $
   BATCH_MODE=batch_mode

  ;;Proof that it's better with Papa John's

  ;; Shrink these fools?
  STR_ELEMENT_FROM_LIST_OF_STRUCTS,kappaFit1Ds,'time_index',VALUE=k1DFitsTimeInd
  STR_ELEMENT_FROM_LIST_OF_STRUCTS,gaussFit1Ds,'time_index',VALUE=g1DFitsTimeInd
  junk1DTimeInds = CGSETINTERSECTION(k1DFitsTimeInd,g1DFitsTimeInd, $
                                     POSITIONS=k1DTInds)
  junk1DTimeInds = CGSETINTERSECTION(g1DFitsTimeInd,k1DFitsTimeInd, $
                                     POSITIONS=g1DTInds)

  IF ~ARRAY_EQUAL(k1DTInds,g1DTInds) THEN IF ~KEYWORD_SET(batch_mode) THEN STOP
  
  kappaFit1Ds = kappaFit1Ds[k1DTInds]
  gaussFit1Ds = gaussFit1Ds[g1DTInds]

  PARSE_KAPPA_FIT_STRUCTS,kappaFit1Ds, $
                          A=a, $
                          STRUCT_A=Astruct, $
                          TIME=kappaTime, $
                          NAMES_A=A_names, $
                          CHI2=chi2, $
                          PVAL=pVal, $
                          FITSTATUS=fitStatus, $
                          USE_MPFIT1D=use_mpFit1D

  PARSE_KAPPA_FIT_STRUCTS,gaussFit1Ds, $
                          A=AGauss, $
                          STRUCT_A=AStructGauss, $
                          TIME=gaussTime, $
                          NAMES_A=AGauss_names, $
                          CHI2=chi2Gauss, $
                          PVAL=pValGauss, $
                          FITSTATUS=gaussFit1DStatus, $
                          USE_MPFIT1D=use_mpFit1D

  STR_ELEMENT_FROM_LIST_OF_STRUCTS,kappaFit1Ds,'A',/PRESERVE_DIMENSIONALITY,VALUE=As
  STR_ELEMENT_FROM_LIST_OF_STRUCTS,GaussFit1Ds,'A',/PRESERVE_DIMENSIONALITY,VALUE=GaussAs
  ;; STR_ELEMENT_FROM_LIST_OF_STRUCTS,fit2dkappa_inf_list,'fitdens',VALUE=fitDens
  ;; STR_ELEMENT_FROM_LIST_OF_STRUCTS,fit2dkappa_inf_list,'SDT',VALUE=sdt
  ;; tid = SDT[*].time
  ;; these = VALUE_CLOSEST2(jvPlotdata.time,tid)
  ;; PRINT,jvplotdata.ndown[these]/fitDens
  ;; PRINT,jvPlotData.nDown[these]/As[*,3]

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
                                          /DONT_SHRINK_PARSED_STRUCT, $
                                          /QUIET)

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
                                          /DONT_SHRINK_PARSED_STRUCT, $
                                          /QUIET) 
  
  ;; PRINT_KAPPA_FIT2D_STATS_FOR_CURANDPOT_TRANGES,fit2DK,fit2DG,cAP_struct,jvPlotData, $
  ;;    /SOK_IF_INDS_DONT_MATCH, $
  ;;    BATCH_MODE=batch_mode
  ;; /ALSO_PARAM_STRUCTS, $
  ;; KFIT2DPARAM_STRUCT=kFit2DParam_struct, $
  ;; GFIT2DPARAM_STRUCT=gFit2DParam_struct

  ;;Now shrink everyone
  IF ~( ARRAY_EQUAL(includeK_i,includeG_i)                          AND $
        (N_ELEMENTS(kappaFit1Ds)  EQ N_ELEMENTS(gaussFit1Ds)           ) AND $
        (N_ELEMENTS(kappaFit1Ds)  EQ N_ELEMENTS(fit2DKappa_inf_list) ) AND $
        (N_ELEMENTS(includeK_i) EQ N_ELEMENTS(fit2DKappa_inf_list) ) ) THEN BEGIN

     IF N_ELEMENTS(kappaFit1Ds) NE N_ELEMENTS(gaussFit1Ds) THEN IF ~KEYWORD_SET(batch_mode) THEN STOP
     IF N_ELEMENTS(fit2DKappa_inf_list) NE N_ELEMENTS(fit2DGauss_inf_list) THEN IF ~KEYWORD_SET(batch_mode) THEN STOP
     IF N_ELEMENTS(kappaFit1Ds) NE N_ELEMENTS(fit2DKappa_inf_list) THEN IF ~KEYWORD_SET(batch_mode) THEN STOP

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

     AStruct      = {bulk_energy : AStruct.bulk_energy[include_i], $
                     temperature : AStruct.temperature[include_i], $
                     kappa       : AStruct.kappa[include_i], $
                     N           : AStruct.N[include_i], $
                     bulk_angle  : AStruct.bulk_angle[include_i]}

     AStructGauss = {bulk_energy : AStructGauss.bulk_energy[include_i], $
                     temperature : AStructGauss.temperature[include_i], $
                     kappa       : AStructGauss.kappa[include_i], $
                     N           : AStructGauss.N[include_i], $
                     bulk_angle  : AStructGauss.bulk_angle[include_i]}

     kFit2DParam_struct = {bulk_energy  : kFit2DParam_struct.bulk_energy[include_i], $
                           temperature  : kFit2DParam_struct.temperature[include_i], $
                           kappa        : kFit2DParam_struct.kappa      [include_i], $
                           n            : kFit2DParam_struct.n          [include_i]}

     gFit2DParam_struct = {bulk_energy  : gFit2DParam_struct.bulk_energy[include_i], $
                           temperature  : gFit2DParam_struct.temperature[include_i], $
                           kappa        : gFit2DParam_struct.kappa      [include_i], $
                           n            : gFit2DParam_struct.n          [include_i]}

  ENDIF



END
PRO JOURNAL__20180419__ESSAYE_AVEC_DES_KAPPA_FIT_FILES

  COMPILE_OPT IDL2,STRICTARRSUBS

  inDir = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/'
  CAPDir = inDir + 'cur_and_pot_analysis/'
  
  CAPpref = 'Orbit_'
  CAPmidM = '-apres_Elph98--GETKLOWBOUNDblkBox-Fig2__meal-aR_mom_eD'
  CAPmidI = '-apres_Elph98--GETKLOWBOUNDblkBox-Fig2_ingredients-aR_mom_eD'
  CAPsuff = '-sc_pot-sRate1_25.sav'

;; Orbit_1450-apres_Elph98--GETKLOWBOUNDblkBox-Fig2__meal-aR_mom_eD_-32-32-sc_pot-sRate1_25.sav
;;   Orbit_1450-apres_Elph98--GETKLOWBOUNDblkBox-Fig2_ingredients-aR_mom_eD_-32-32-sc_pot-sRate1_25.sav

  dato  = '20180419'
  pref  = dato + '-orb_'
  suff  = '-KandGfits-ees-GETKLOWBOUND-only_fit_peak_eRange-sRate1_25.sav'
  SPAWN,'cd ' + inDir + '; ls ' + pref + '*' + suff,liste

  timeagoStr  = '-mmin -600'
  findIString = "find . " + timeagoStr + " -iname '*" $
                + CAPpref + "*" + CAPmidI + "*" + CAPsuff $
                + "' -print0 | xargs -0 ls -1"
  findMString = "find . " + timeagoStr + " -iname '*" $
                + CAPpref + "*" + CAPmidM + "*" + CAPsuff $
                + "' -print0 | xargs -0 ls -1"

  ;; SPAWN,'cd ' + CAPDir + '; ls ' + CAPpref + '*' + CAPmidI + '*' + CAPsuff,CAPIliste
  ;; SPAWN,'cd ' + CAPDir + '; ls ' + CAPpref + '*' + CAPmidM + '*' + CAPsuff,CAPMliste
  SPAWN,'cd ' + CAPDir + '; ' + findIString,CAPIliste
  SPAWN,'cd ' + CAPDir + '; ' + findMString,CAPMliste

  nFil  = N_ELEMENTS(liste)
  nCAPI = N_ELEMENTS(CAPIliste)
  nCAPM = N_ELEMENTS(CAPMliste)
  
  ;; IF nFil NE nCAPI OR nFil NE nCAPM OR nCAPI NE nCAPM THEN BEGIN

  ;;    STOP
  ;; ENDIF

  use_mpFit1D = 1


  bArr      = MAKE_ARRAY(30000,/FLOAT,VALUE=0.)
  KF2DParms = {bulk_energy : bArr, $
               temperature : bArr, $
               kappa       : bArr, $
               N           : bArr, $
               chi2red     : bArr}
  GF2DParms = KF2DParms
  ephem     = {time        : DOUBLE(bArr), $
               MLT         : bArr, $
               ILAT        : bArr, $
               ALT         : bArr}

  bArr      = !NULL
  
  curInd    = 0
  ;; KF2DParms = !NULL
  ;; GF2DParms = !NULL
  FOR k=0,nFil-1 DO BEGIN

     kappaFit1Ds         = !NULL
     gaussFit1Ds         = !NULL
     fit2DKappa_inf_list = !NULL
     fit2DGauss_inf_list = !NULL
     kFit2DParam_struct  = !NULL
     gFit2DParam_struct  = !NULL
     fit2DK              = !NULL
     fit2DG              = !NULL

     orbit = LONG(STRMID(liste[k],STRLEN(pref),4))
     PRINT,orbit

     ingInd  = WHERE(STRMATCH(CAPIliste,'*' + STRING(FORMAT='(I4)',orbit) + '*'),nIng)
     mealInd = WHERE(STRMATCH(CAPMliste,'*' + STRING(FORMAT='(I4)',orbit) + '*'),nMeal)

     IF nIng NE 1 OR nMeal NE 1 THEN BEGIN
        PRINT,CAPIliste[ingInd]
        PRINT,CAPMliste[mealInd]
        STOP
     ENDIF

     RESTORE,inDir+liste[k]
     ;; RESTORE,CAPDir+CAPIliste[ingInd]
     RESTORE,CAPDir+CAPMliste[mealInd]

     KAPPA_RAWSAVE_PARSER, $
        KAPPAFIT1DS=kappaFit1Ds, $
        GAUSSFIT1DS=gaussFit1Ds, $
        KAPPA_INF_LIST=fit2DKappa_inf_list, $
        GAUSS_INF_LIST=fit2DGauss_inf_list, $
        KFIT2DPARAM_STRUCT=kFit2DParam_struct, $
        GFIT2DPARAM_STRUCT=gFit2DParam_struct, $
        FIT2DK=fit2DK, $
        FIT2DG=fit2DG, $
        USE_MPFIT1D=use_mpFit1D, $
        /BATCH_MODE

     nHere = N_ELEMENTS(kFit2DParam_struct.bulk_energy)

     curInds = [curInd:(curInd+nHere-1)]
     KF2DParms.bulk_energy[curInds] = kFit2DParam_struct.bulk_energy
     KF2DParms.temperature[curInds] = kFit2DParam_struct.temperature
     KF2DParms.kappa[curInds]       = kFit2DParam_struct.kappa
     KF2DParms.N[curInds]           = kFit2DParam_struct.N
     KF2DParms.chi2Red[curInds]     = fit2DK.chi2/(fit2DK.dof-fit2DK.nFree)

     GF2DParms.bulk_energy[curInds] = gFit2DParam_struct.bulk_energy
     GF2DParms.temperature[curInds] = gFit2DParam_struct.temperature
     GF2DParms.kappa[curInds]       = gFit2DParam_struct.kappa
     GF2DParms.N[curInds]           = gFit2DParam_struct.N
     GF2DParms.chi2Red[curInds]     = fit2DG.chi2/(fit2DG.dof-fit2DG.nFree)

     kTid = fit2DK.sdt.time
     gTid = fit2DG.sdt.time
     IF N_ELEMENTS(kTid) NE N_ELEMENTS(gTid) THEN BEGIN
        PRINT,"Unequal number of K and G tider!"
        CONTINUE
     ENDIF
     IF (WHERE(ABS(kTid-gTid) GT MEDIAN(kTid[1:-1]-kTid[0:-2])))[0] NE -1 THEN BEGIN
        PRINT,"K and G tider not equal!" 
        CONTINUE
     ENDIF
     
     ephem.time[curInds]            = kTid

     ;; fit2DK.chi2/(fit2DK.dof-fit2DK.nFree)
     ;; fit2DG.chi2/(fit2DG.dof-fit2DG.nFree)

     ;; KF2DParms = [KF2DParms,kFit2DParam_struct]
     ;; GF2DParms = [GF2DParms,gFit2DParam_struct]

     curInd += nHere

  ENDFOR

END
