
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
PRO JOURNAL__20180419__ESSAYE_AVEC_DES_KAPPA_FIT_FILES, $
   EXCLUDE_IONS=exclude_ions, $
   REQUIRE_IONS=require_ions, $
   COMBINED_HISTOS=combined_histos, $
   KHIST_BINSIZE=kHist_binSize, $
   KHIST_MIN=kHist_min, $
   MHIST_BINSIZE=mHist_binSize, $
   MHIST_MIN=mHist_min, $
   NORMED=normed, $
   GOVERK=GoverK, $
   MAXKCHI2=maxKChi2

  COMPILE_OPT IDL2,STRICTARRSUBS

  restoreFile = 1
  requireIons = N_ELEMENTS(require_ions) GT 0 ? require_ions : 0
  excludeIons = N_ELEMENTS(exclude_ions) GT 0 ? exclude_ions : 0

  makeMetaStabPlot = 1
  makeMLTILATplot = 0
  makeILATKappaplot = 1
  makeMLTKappaplot = 1
  bufferPlots = 1

  GoverKReq = KEYWORD_SET(GoverK)   ? GoverK   : 1.5
  KChi2Max  = KEYWORD_SET(maxKChi2) ? maxKChi2 : 5

  minM  = -5
  maxM  = 5
  notMLT = 0
  minI  = 55
  maxI  = 90
  hemi  = 'BOTH'

  kHBinSize = N_ELEMENTS(kHist_binSize) GT 0 ? kHist_binSize : 0.5
  kHistMin  = N_ELEMENTS(kHist_min    ) GT 0 ? kHist_min     : 1.4

  mHBinSize = N_ELEMENTS(mHist_binSize) GT 0 ? mHist_binSize : 0.05
  mHistMin  = N_ELEMENTS(mHist_min    ) GT 0 ? mHist_min     : 0.

  outDir   = '/SPENCEdata/Research/Satellites/FAST/kappa_dists/saves_output_etc/'
  ;; restoreD = GET_TODAY_STRING(/DO_YYYYMMDD_FMT)
  ;; restoreD = '20180420' ;2018/04/23 Making a new one, calling it 20180424 for kicks
  ;; restoreD = '20180426'
  ;; restoreD = '20180427TRY3' ;Later, with orbs reaching into 7000+
  restoreD = '20180427TRY4' 
  restoreD = '20180427TRY5' 
  restoreD = '20180501' 
  bonusPlotSuff = '-WITH7000NUP'
  outFName = restoreD+'-parsedKappa.sav'

  ;; newellDates   = restoreD
  ;; KandGFitDates = restoreD
  newellDates   = ['20180425','20180426','20180427','20180430','20180501']
  KandGFitDates = newellDates
  CAPtimeagoStr = '-mtime -6'

  IF KEYWORD_SET(restoreFile) THEN BEGIN
     makeNewFile = ~FILE_TEST(outDir+outFName)
     IF makeNewFile THEN BEGIN
        PRINT,"Couldn't find " + outFName + '! Remaking ...'
        STOP
     ENDIF
  ENDIF ELSE makeNewFile = 1

  IF KEYWORD_SET(makeNewFile) THEN BEGIN

     ;; Only longest interval of 4377 is bad, but I don't know how to screen for that
     badOrbs = [1257,1635,1693,1875,1945,3123,3268,3353,4377,4424,4479,7871]

     inDir = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/'
     CAPDir = inDir + 'cur_and_pot_analysis/'

     CAPpref = 'Orbit_'
     CAPmidM = '--GETKLOWBOUND-meal'
     CAPmidI = '--GETKLOWBOUND-ingredients'
     CAPsuff = '-sc_pot-sRate1_25.sav'

     nNewellDates = N_ELEMENTS(newellDates)
     newellListe  = !NULL
     FOR k=0,nNewellDates-1 DO BEGIN
        Newelldir = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/kappa_Newell_data/' $
                    + newellDates[k] + '/'
        Newellpref = 'NewellData-'
        Newellsuff = '-GETKLOWBOUND-'

        tempNewellListe = FILE_SEARCH(Newelldir+Newellpref+'*'+Newellsuff+'*'+'.sav')
        newellListe = [newellListe,tempNewellListe]
     ENDFOR
     
;; Orbit_1450-apres_Elph98--GETKLOWBOUNDblkBox-Fig2__meal-aR_mom_eD_-32-32-sc_pot-sRate1_25.sav
;;   Orbit_1450-apres_Elph98--GETKLOWBOUNDblkBox-Fig2_ingredients-aR_mom_eD_-32-32-sc_pot-sRate1_25.sav

     ;; KandGFitDate  = '20180419'
     nKAndGFitDates = N_ELEMENTS(KandGFitDates)
     KandGliste = !NULL
     FOR k=0,nKAndGFitDates-1 DO BEGIN
        KandGPref  = KandGFitDates[k] + '-orb_'
        KandGMid   = '-KandGfits-ees-GETKLOWBOUND-only_fit_peak_eRange-sRate1_25'
        KandGSuff  = '.sav'
        SPAWN,'cd ' + inDir + '; ls ' + KandGPref + '*' + KandGMid + '*' + KandGSuff,tempKandGliste
        KandGliste = [KandGliste,tempKandGliste]
     ENDFOR
     
     findIString = "find . " + CAPtimeagoStr + " -iname '*" $
                   + CAPpref + "*" + CAPmidI + "*" + CAPsuff $
                   + "' -print0 | xargs -0 ls -1"
     findMString = "find . " + CAPtimeagoStr + " -iname '*" $
                   + CAPpref + "*" + CAPmidM + "*" + CAPsuff $
                   + "' -print0 | xargs -0 ls -1"

     ;; SPAWN,'cd ' + CAPDir + '; ls ' + CAPpref + '*' + CAPmidI + '*' + CAPsuff,CAPIliste
     ;; SPAWN,'cd ' + CAPDir + '; ls ' + CAPpref + '*' + CAPmidM + '*' + CAPsuff,CAPMliste
     SPAWN,'cd ' + CAPDir + '; ' + findIString,CAPIliste
     SPAWN,'cd ' + CAPDir + '; ' + findMString,CAPMliste

     nKandGFil = N_ELEMENTS(KandGliste)
     nCAPI     = N_ELEMENTS(CAPIliste)
     nCAPM     = N_ELEMENTS(CAPMliste)
     nNewell   = N_ELEMENTS(newellListe)

     ;; Check for duplicates?

     toOrbLen  = STRLEN(NewellDir+Newellpref)
     toTidLen  = STRLEN(NewellDir+Newellpref+Newellsuff)+4
     toDateLen = STRLEN(NewellDir)-9
     rmIndArr  = !NULL
     FOR k=0,nNewell-1 DO BEGIN
        orbStr = STRMID(newellListe[k],toOrbLen,4)        
        tidStr = STRMID(newellListe[k],toTidLen,27)

        match  = WHERE(STRMATCH(newellListe,'*'+NewellPref+orbStr+'*'+tidStr+'*'),nMatch)

        ;; Now pick newest
        CASE nMatch OF
           0: BEGIN
              PRINT,"Huh??? Should at least match self ..."
              STOP
           END
           1:                   ;Just keep moving
           ELSE: BEGIN
              tmpDates = !NULL
              FOR kk=0,nMatch-1 DO tmpDates = [tmpDates,LONG(STRMID(newellListe[match[kk]],toDateLen,8))]
              winna = MAX(tmpDates,winInd)
              REMOVE,winInd,match ;Keep the latest file (by removing it from the list of files to be removed)
              rmIndArr = [rmIndArr,match]
           END
        ENDCASE

     ENDFOR

     rmIndArr = rmIndArr[UNIQ(rmIndArr,SORT(rmIndArr))]
     REMOVE,rmIndArr,newellListe

     ;; Now remove from KandG
     toOrbLen  = 13
     toTidLen  = STRLEN(KandGPref+KandGMid)+5
     toDateLen = 0
     rmIndArr  = !NULL
     FOR k=0,nKandGFil-1 DO BEGIN
        orbStr = STRMID(KandGliste[k],toOrbLen,4)        
        tidStr = STRMID(KandGliste[k],toTidLen,27)

        match  = WHERE(STRMATCH(KandGliste,'*'+orbStr+KandGMid+'*'+tidStr+'*'),nMatch)

        ;; Now pick newest
        CASE nMatch OF
           0: BEGIN
              PRINT,"Huh??? Should at least match self ..."
              STOP
           END
           1:                   ;Just keep moving
           ELSE: BEGIN
              tmpDates = !NULL
              FOR kk=0,nMatch-1 DO tmpDates = [tmpDates,LONG(STRMID(KandGliste[match[kk]],toDateLen,8))]
              winna = MAX(tmpDates,winInd)
              REMOVE,winInd,match ;Keep the latest file (by removing it from the list of files to be removed)
              rmIndArr = [rmIndArr,match]
           END
        ENDCASE

     ENDFOR

     rmIndArr = rmIndArr[UNIQ(rmIndArr,SORT(rmIndArr))]
     REMOVE,rmIndArr,KandGliste

     ;; Recalc
     nKandGFil = N_ELEMENTS(KandGliste)
     nCAPI     = N_ELEMENTS(CAPIliste)
     nCAPM     = N_ELEMENTS(CAPMliste)
     nNewell   = N_ELEMENTS(newellListe)

     ;; IF nKandGFil NE nCAPI OR nKandGFil NE nCAPM OR nCAPI NE nCAPM THEN BEGIN

     ;;    STOP
     ;; ENDIF

     use_mpFit1D = 1

     maks      = 90000
     bArr      = MAKE_ARRAY(maks,/FLOAT,VALUE=0.)
     KF2DParms = {time              : DOUBLE(bArr), $
                  bulk_energy       : bArr, $
                  temperature       : bArr, $
                  kappa             : bArr, $
                  N                 : bArr, $
                  chi2red           : bArr}
     GF2DParms = KF2DParms
     andre     = {time              : DOUBLE(bArr), $
                  orbit             : LONG(bArr), $
                  MLT               : bArr, $
                  ILAT              : bArr, $
                  ALT               : bArr, $
                  mono              : BYTE(bArr), $
                  broad             : BYTE(bArr), $
                  diffuse           : BYTE(bArr), $
                  ionBeam           : BYTE(bArr), $
                  newell_tMismatch  : bArr}

     bArr      = !NULL

     curInd    = 0
     ;; KF2DParms = !NULL
     ;; GF2DParms = !NULL
     nIkkeHa   = 0
     totT      = 0.D
     orbArr    = !NULL
     FOR k=0,nKandGFil-1 DO BEGIN

        kappaFit1Ds         = !NULL
        gaussFit1Ds         = !NULL
        fit2DKappa_inf_list = !NULL
        fit2DGauss_inf_list = !NULL
        kFit2DParam_struct  = !NULL
        gFit2DParam_struct  = !NULL
        fit2DK              = !NULL
        fit2DG              = !NULL

        orbStr = STRMID(KandGliste[k],STRLEN(KandGPref),4)
        orbit = LONG(orbStr)
        PRINT,orbit

        IF (WHERE(orbit EQ badOrbs))[0] NE -1 THEN BEGIN
           PRINT,FORMAT='("Bad orbit: ",I0,". Skipping ...")',orbit
           CONTINUE
        ENDIF

        ;; ingInd  = WHERE(STRMATCH(CAPIliste,'*' + orbStr + '*'),nIng)
        mealInd = WHERE(STRMATCH(CAPMliste,'./'+CAPpref+'*' + orbStr + '*'),nMeal)
        newellInd = WHERE(STRMATCH(newellListe,'*'+Newellpref+'*' + orbStr + '*'),nNewell)

        ;; IF (nIng      NE  1) OR (nMeal      NE  1) THEN BEGIN
        ;;    PRINT,CAPIliste[ingInd]
        ;;    PRINT,CAPMliste[mealInd]
        ;;    STOP
        ;; ENDIF

        ;; IF (ingInd[0] EQ -1) THEN BEGIN
        ;;    PRINT,"Couldn't find ingredients for orb " + orbStr + "!"
        ;; nIkkeHa++
        ;;    CONTINUE
        ;; ENDIF

        ;; IF nMeal NE 1 THEN BEGIN
        ;;    IF mealInd[0] EQ -1 THEN PRINT,"No Meal!" $
        ;;    ELSE BEGIN
        ;;       PRINT,"Too many meals: ",CAPMliste[mealInd]
        ;;       STOP
        ;;    ENDELSE
        ;;    nIkkeHa++
        ;;    CONTINUE
        ;; ENDIF

        ;; IF (mealInd[0] EQ -1) THEN BEGIN
        ;;    PRINT,"Couldn't find meal for orb " + orbStr + "!"
        ;;    nIkkeHa++
        ;;    CONTINUE
        ;; ENDIF

        IF nNewell GT 5 THEN STOP

        IF (newellInd[0] EQ -1) THEN BEGIN
           PRINT,"Couldn't find Newellfile for orb " + orbStr + "!"
           nIkkeHa++
           CONTINUE
        ENDIF

        RESTORE,inDir+KandGliste[k]

        IF nNewell GT 1 THEN BEGIN
           x           = !NULL
           mlt         = !NULL
           ilat        = !NULL
           mono        = !NULL
           broad       = !NULL
           diffuse     = !NULL
           je          = !NULL
           jee         = !NULL
           ionBeam     = !NULL
           nbad_espec  = !NULL
           info        = !NULL

           nHere = N_ELEMENTS(events.x)
           FOR bro=0,nNewell-1 DO BEGIN
              events = !NULL
              RESTORE,newellListe[newellInd[bro]]

              ;; Har vi ions?
              junk = !NULL
              STR_ELEMENT,events,'ionBeam',VALUE=junk
              tmpIonBeam  = N_ELEMENTS(junk) GT 0 ? events.ionBeam : BYTE(events.ilat)*0B

              x           = [x         ,events.x            ]
              mlt         = [mlt       ,events.mlt          ]
              ilat        = [ilat      ,events.ilat         ]
              mono        = [mono      ,events.mono         ]
              broad       = [broad     ,events.broad        ]
              diffuse     = [diffuse   ,events.diffuse      ]
              ionBeam     = [ionBeam   ,tmpIonBeam          ] 
              je          = [je        ,events.je           ]
              jee         = [jee       ,events.jee          ]
              nbad_espec  = [nbad_espec,events.nbad_espec   ]
              info        = [info      ,events.info         ]

           ENDFOR

           sorti  = SORT(x)
           events = {x         : (TEMPORARY(x         ))[sorti], $
                    mlt        : (TEMPORARY(mlt       ))[sorti], $
                    ilat       : (TEMPORARY(ilat      ))[sorti], $
                    mono       : (TEMPORARY(mono      ))[sorti], $
                    broad      : (TEMPORARY(broad     ))[sorti], $
                    diffuse    : (TEMPORARY(diffuse   ))[sorti], $
                    ionBeam    : (TEMPORARY(ionBeam   ))[sorti], $
                    je         : (TEMPORARY(je        ))[sorti], $
                    jee        : (TEMPORARY(jee       ))[sorti], $
                    nbad_espec : (TEMPORARY(nbad_espec))[sorti], $
                    info       : TEMPORARY(info      )}
        ENDIF ELSE BEGIN
           RESTORE,newellListe[newellInd]
        ENDELSE
        ;; RESTORE,CAPDir+CAPIliste[ingInd]
        ;; RESTORE,CAPDir+CAPMliste[mealInd]

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

        kTid                            = fit2DK.sdt.time
        gTid                            = fit2DG.sdt.time
        IF N_ELEMENTS(kTid) NE N_ELEMENTS(gTid) THEN BEGIN
           PRINT,"Unequal number of K and G tider!"
           nIkkeHa++
           CONTINUE
        ENDIF

        IF (WHERE(ABS(kTid-gTid) GT MEDIAN(kTid[1:-1]-kTid[0:-2])))[0] NE -1 THEN BEGIN
           PRINT,"K and G tider not equal!"
           nIkkeHa++
           CONTINUE
        ENDIF

        nHere                           = N_ELEMENTS(kFit2DParam_struct.bulk_energy)

        curInds                         = [curInd:((curInd+nHere-1) < (maks-1))]
        KF2DParms.bulk_energy[curInds]  = kFit2DParam_struct.bulk_energy
        KF2DParms.temperature[curInds]  = kFit2DParam_struct.temperature
        KF2DParms.kappa[curInds]        = kFit2DParam_struct.kappa
        KF2DParms.N[curInds]            = kFit2DParam_struct.N
        KF2DParms.chi2Red[curInds]      = fit2DK.chi2/(fit2DK.dof-fit2DK.nFree)

        GF2DParms.time[curInds]         = gTid
        GF2DParms.bulk_energy[curInds]  = gFit2DParam_struct.bulk_energy
        GF2DParms.temperature[curInds]  = gFit2DParam_struct.temperature
        GF2DParms.kappa[curInds]        = gFit2DParam_struct.kappa
        GF2DParms.N[curInds]            = gFit2DParam_struct.N
        GF2DParms.chi2Red[curInds]      = fit2DG.chi2/(fit2DG.dof-fit2DG.nFree)

        ;; Match 'em up with events struct
        evtInds = VALUE_CLOSEST2(events.x,kTid,/CONSTRAINED)

        orbArr  = [orbArr,orbit]
        totT   += events.x[-1]-events.x[0]

        tDiffs  = events.x[evtInds]-kTid
        baddies = WHERE(ABS(tDiffs)/MEDIAN(events.x[1:-1]-events.x[0:-2]) GT 1,nBaddies)
        IF nBaddies GT 0 THEN BEGIN
           ;; STOP
           andre.newell_tMismatch[curInds[baddies]] = tDiffs[baddies]
        ENDIF

        andre.orbit[curInds]   = orbit
        andre.mlt[curInds]     = events.mlt[evtInds]
        andre.ilat[curInds]    = events.ilat[evtInds]
        andre.mono[curInds]    = events.mono[evtInds]
        andre.broad[curInds]   = events.broad[evtInds]
        andre.diffuse[curInds] = events.diffuse[evtInds]
        andre.ionBeam[curInds] = events.ionBeam[evtInds]

        KF2DParms.time[curInds]         = kTid
        GF2DParms.time[curInds]         = gTid
        andre.time[curInds]             = events.x[evtInds]

        IF curInds[-1] EQ (maks-1) THEN BEGIN
           PRINT,"Cutting out early! Reached maks ind ..."
           BREAK
        ENDIF

        curInd += nHere

     ENDFOR

     PRINT,"Nlost: ",nIkkeHa

     STOP

     ;; Now shrink 'em
     finalInds = [0:(curInd-1)]

     mismatches = WHERE(ABS(andre.newell_tMismatch) GT 0.01,nMismatch)
     PRINT,FORMAT='(I5," total mismatches")',nMismatch

     finalInds = CGSETDIFFERENCE(finalInds,mismatches,COUNT=nFinal)

     CHECK_SORTED,andre.time[finalInds],is_sorted,/QUIET
     IF ~is_sorted THEN BEGIN
        PRINT,"Not sorted! Figure it, son"
        STOP
     ENDIF

     uniq_final = UNIQ(andre.time[finalInds],SORT(andre.time[finalInds]))
     nUniq      = N_ELEMENTS(uniq_final)
     IF nUniq NE nFinal THEN BEGIN
        PRINT,FORMAT='("You' + "'" + 're dupin' + "'" + ' wif ",I0," inds")',nFinal-nUniq
        STOP
        finalInds = finalInds[uniq_final]
        nFinal    = N_ELEMENTS(finalInds)
     ENDIF

     PRINT,"NFinal: ",nFinal

     GET_FA_ORBIT,andre.time[finalInds],/TIME_ARRAY,/NO_STORE,STRUC=struc

     andre.alt[finalInds] = struc.alt

     LOAD_DST_AE_DBS,Dst,AE
     match_ae_i = VALUE_CLOSEST2(ae.time,andre.time[finalInds],/CONSTRAINED)
     match_dst_i = VALUE_CLOSEST2(dst.time,andre.time[finalInds],/CONSTRAINED)

     STOP

     KF2DParms = {time              : KF2DParms.time[finalInds]        , $
                  bulk_energy       : KF2DParms.bulk_energy[finalInds] , $
                  temperature       : KF2DParms.temperature[finalInds] , $
                  kappa             : KF2DParms.kappa[finalInds]       , $
                  N                 : KF2DParms.N[FINALINDS]           , $
                  chi2red           : KF2DParms.chi2red[finalInds]}
     GF2DParms = {time              : GF2DParms.time[finalInds]        , $
                  bulk_energy       : GF2DParms.bulk_energy[finalInds] , $
                  temperature       : GF2DParms.temperature[finalInds] , $
                  kappa             : GF2DParms.kappa[finalInds]       , $
                  N                 : GF2DParms.N[FINALINDS]           , $
                  chi2red           : GF2DParms.chi2red[finalInds]}
     andre     = {time              : andre.time[finalInds]            , $
                  orbit             : andre.orbit[finalInds]           , $
                  MLT               : andre.MLT[finalInds]             , $
                  ILAT              : andre.ILAT[finalInds]            , $
                  ALT               : andre.ALT[finalInds]             , $
                  mono              : andre.mono[finalInds]            , $
                  broad             : andre.broad[finalInds]           , $
                  diffuse           : andre.diffuse[finalInds]         , $
                  ionBeam           : andre.ionBeam[finalInds]         , $
                  AE                : AE.AE[match_ae_i]                , $
                  DST               : DST.DST[match_dst_i]             , $
                  newell_tMismatch  : andre.newell_tMismatch[finalInds]}

     PRINT,"Saving " + outFName + ' ...'
     SAVE,andre,KF2DParms,GF2DParms,totT,orbArr,FILENAME=outDir+outFName

  ENDIF ELSE BEGIN

     PRINT,"Restoring " + outFName + ' ...'
     RESTORE,outDir+outFName

  ENDELSE

  SET_DEFAULT_MLT_ILAT_AND_MAGC,MINMLT=minM, $
                                MAXMLT=maxM, $
                                BINMLT=binM, $
                                SHIFTMLT=shiftM, $
                                USE_LNG=use_lng, $
                                MINLNG=minLng, $
                                MAXLNG=maxLng, $
                                BINLNG=binLng, $
                                SHIFTLNG=shiftLng, $
                                MINILAT=minI, $
                                MAXILAT=maxI, $
                                BINILAT=binI, $
                                SHIFTILAT=shiftI, $
                                DONT_CORRECT_ILATS=dont_correct_ilats, $
                                DO_LSHELL=do_lShell, $
                                MINLSHELL=minL, $
                                MAXLSHELL=maxL, $
                                BINLSHELL=binL, $
                                REVERSE_LSHELL=reverse_lShell, $
                                COORDINATE_SYSTEM=coordinate_system, $
                                USE_AACGM_COORDS=use_AACGM, $
                                USE_GEI_COORDS=use_GEI, $
                                USE_GEO_COORDS=use_GEO, $
                                USE_MAG_COORDS=use_MAG, $
                                USE_SDT_COORDS=use_SDT, $
                                MIN_MAGCURRENT=minMC, $
                                MAX_NEGMAGCURRENT=maxNegMC, $
                                HEMI=hemi, $
                                NORTH=north, $
                                SOUTH=south, $
                                BOTH_HEMIS=both_hemis, $
                                GLOBE=globe, $
                                DAYSIDE=dayside, $
                                NIGHTSIDE=nightside, $
                                MIMC_STRUCT=MIMC_struct, $
                                MAP_PROJECTION=map_projection, $
                                _EXTRA=e, $
                                LUN=lun

  mlt_i = GET_MLT_INDS(andre,minM,maxM, $
                       DAWNSECTOR=dawnSector, $
                       DUSKSECTOR=duskSector, $
                       DAYSIDE=dayside, $
                       NIGHTSIDE=nightside, $
                       N_MLT=n_mlt, $
                       N_OUTSIDE_MLT=n_outside_MLT, $
                       DIRECT_MLTS=direct_mlts, $
                       /GET_COMPLEMENT_INDS, $
                       NOTMLT_I=notMlt_i, $
                       NNOTMLT=nNotMlt)

  mltSuff = 'MLT'
  IF KEYWORD_SET(notMLT) THEN BEGIN
     mlt_i = notMlt_i
     mltSuff = 'notMLT'
  ENDIF

  ilat_i            = GET_ILAT_INDS(andre, $
                                    minI, $
                                    maxI, $
                                    hemi, $
                                    N_ILAT=n_ilat, $
                                    N_NOT_ILAT=n_not_ilat, $
                                    LUN=lun)
  region_i          = CGSETINTERSECTION(ilat_i,mlt_i)

  mono_i            = WHERE(andre.mono  EQ 1 OR andre.mono  EQ 2, $
                            nMono, $
                            NCOMPLEMENT=nNotMono)
  broad_i           = WHERE(andre.broad EQ 1 OR andre.broad EQ 2, $
                            nBroad, $
                            NCOMPLEMENT=nNotBroad)

  chi2_i            = WHERE((GF2DParms.chi2red/KF2DParms.chi2red GE GoverKReq) AND $
                            (KF2DParms.chi2red LE KChi2Max), $
                            nChi2, $
                            NCOMPLEMENT=nNotChi2)
  final_i           = CGSETINTERSECTION(region_i,mono_i, $
                                        COUNT=count)
  IF count EQ 0 THEN STOP
  final_i           = CGSETINTERSECTION(final_i,chi2_i, $
                                        COUNT=count)
  IF count EQ 0 THEN STOP

  GoverKStr         = (STRING(FORMAT='("-GoverK",F0.1)',GoverKReq)).Replace('.','_')
  KChi2MaxStr       = (STRING(FORMAT='("-Kchi2Max",F0.1)',KChi2Max)).Replace('.','_')
  kHBinSizeStr        = (STRING(FORMAT='("-binSz",F0.1)',kHBinSize)).Replace('.','_')
  mHBinSizeStr        = (STRING(FORMAT='("-binSz",F0.3)',mHBinSize)).Replace('.','_')

  ionSuff           = '-allObs'

  IF KEYWORD_SET(requireIons) AND KEYWORD_SET(excludeIons) THEN BEGIN
     PRINT,"Now you're on my turf."
     STOP
  ENDIF

  IF KEYWORD_SET(requireIons) THEN BEGIN
     ion_i          = WHERE(andre.ionBeam EQ 1 OR andre.ionBeam EQ 2,nIonBeam,NCOMPLEMENT=nNotIonBeam)
     final_i        = CGSETINTERSECTION(final_i,ion_i,COUNT=count)
     ionSuff        = '-onlyIonBeams'
  ENDIF ELSE IF KEYWORD_SET(excludeIons) THEN BEGIN
     ion_i          = WHERE(andre.ionBeam EQ 1 OR andre.ionBeam EQ 2,nIonBeam,NCOMPLEMENT=nNotIonBeam)
     final_i        = CGSETDIFFERENCE(final_i,ion_i,COUNT=count)
     ionSuff        = '-excludeIons'
  ENDIF ELSE IF KEYWORD_SET(combined_histos) THEN BEGIN
     req_i          = WHERE(andre.ionBeam EQ 1 OR andre.ionBeam EQ 2,nReq)
     exc_i          = WHERE(andre.ionBeam EQ 1 OR andre.ionBeam EQ 2,nExc)

     req_i          = CGSETINTERSECTION(final_i,req_i,COUNT=nReq)
     exc_i          = CGSETDIFFERENCE(final_i,exc_i,COUNT=nExc)

     ionSuff        = '-onlyAndExcl'
  ENDIF

  parmStr           = GoverKStr + KChi2MaxStr + ionSuff

  ;; What about a metastability measure?
  qVals = 1.+1./KF2DParms.kappa
  MVals = 4.*(qVals-1.)/(qVals+1.)

  magicQVal = 1.+1./2.45
  magicMVal = 4.*(magicQVal-1.)/(magicQVal+1.)
  checkM = magicMVal-(mHBinSize)*(DOUBLE(ROUND(magicMVal/mHBinSize)))

  IF KEYWORD_SET(makeMLTILATplot) OR KEYWORD_SET(makeMLTKappaplot) THEN BEGIN
     MLTs = andre.mlt
     MLTs[WHERE(MLTs GT 18)] = MLTs[WHERE(MLTs GT 18)] - 24.
  ENDIF


  IF ~KEYWORD_SET(combined_histos) THEN BEGIN

     PRINT,FORMAT='("Working with ",I0, " inds")',count

     ;; Unique low kappa-ers
     ;; lowKappa_i        = WHERE(KF2DParms.kappa LE 2,nLowKappa,COMPLEMENT=notLowKappa_i,NCOMPLEMENT=nNotLowKappa)
     ;; lowKappa_i        = CGSETINTERSECTION(lowKappa_i,final_i,COUNT=nLowKappa)
     ;; lowKappaOrbs      = andre.orbit[lowkappa_i[UNIQ(andre.orbit[lowkappa_i],SORT(andre.orbit[lowkappa_i]))]]

     plot_i            = final_i

     ;; nNorth            = FLOAT(N_ELEMENTS(WHERE(andre.ilat[final_i] GE 0)))
     ;; nSouth            = FLOAT(N_ELEMENTS(WHERE(andre.ilat[final_i] LT 0)))
     ;; pctNorth          = nNorth/count
     ;; pctSouth          = nSouth/count

     ;; !P.multi = [0,1,1,0,0]
     ;; WINDOW,1,XSIZE=600,YSIZE=600
     ;; CGHISTOPLOT,KF2DParms.kappa[plot_i],BINSIZE=0.15,MININPUT=1.5,MAXINPUT=10

     ;; !P.multi = [0,2,2,0,0]
     ;; WINDOW,0,XSIZE=600,YSIZE=600
     ;; CGHISTOPLOT,ALOG10(GF2DParms.chi2red[plot_i]),TITLE="Maxwellian"
     ;; CGHISTOPLOT,ALOG10(KF2DParms.chi2red[plot_i]),TITLE='Kappa'
     ;; CGHISTOPLOT,GF2DParms.chi2red[plot_i]/KF2DParms.chi2red[plot_i],TITLE='Ratio G/K',MAXINPUT=10,binsize=0.1

     kHist = HISTOGRAM(KF2DParms.kappa[plot_i], $
                       BINSIZE=kHBinSize, $
                       MIN=kHistMin, $
                       LOCATIONS=kBins, $
                       REVERSE_INDICES=rKInds)

     mHist = HISTOGRAM(MVals[plot_i], $
                       BINSIZE=mHBinSize, $
                       MIN=MIN([mHistMin,checkM]), $
                       LOCATIONS=mBins, $
                       REVERSE_INDICES=rMInds)

     IF KEYWORD_SET(normed) THEN BEGIN
        kTot = TOTAL(kHist)
        mTot = TOTAL(mHist)

        kHist = FLOAT(kHist)/FLOAT(kTot)
        mHist = FLOAT(mHist)/FLOAT(mTot)

        parmStr += '-normed'
     ENDIF

     ;; Where are they less than .245?
     lt245inds = !NULL
     bin245 = VALUE_CLOSEST2(kBins,2.45,/CONSTRAINED)
     FOR k=0,bin245-1 DO BEGIN
        IF rKInds[k] NE rKInds[k+1] THEN $
           lt245inds = [lt245inds,rKInds[rKInds[k] : rKInds[k+1]-1]]
     ENDFOR

     ;; Kappa plot
     titleStr = STRING(FORMAT='(I02,"-",I02," ' + mltSuff + ', ",I0,"$^\circ$ < |ILAT| < ",I0,"$^\circ$!C!COrbits ",I0,"-",I0," (",I0,"/",I0," considered)")', $
                       minM+24,maxM, $
                       minI,maxI, $
                       MIN(orbArr),MAX(orbArr), $
                       N_ELEMENTS(orbArr),MAX(orbArr)-MIN(orbArr))
     winder   = WINDOW(DIMENSIONS=[800,800],BUFFER=bufferPlots)

     xRange   = [1.5,15]
     yRange   = [0,MAX(kHist)*1.1]
     kHistPlot = PLOT(kBins,kHist,/HISTOGRAM, $
                      XRANGE=xRange, $
                      YRANGE=yRange, $
                      XTITLE='$\kappa$',YTITLE='Count',TITLE=titleStr, $
                      FONT_SIZE=16,THICK=2.5, $
                      CURRENT=winder)

     kLinePlot = PLOT(REPLICATE(2.45,11),FINDGEN(11)/10.*MAX(kHist)*2, $
                      YRANGE=yRange, $
                      THICK=2.,COLOR='GREEN',/OVERPLOT, $
                      CURRENT=winder)

     kText     = TEXT(0.25,0.8,"$\kappa_t$ = 2.45",/NORMAL, $
                      FONT_SIZE=16,FONT_COLOR='GREEN', $
                      TARGET=winder)

     plotDir      = '/SPENCEdata/Research/Satellites/FAST/kappa_dists/plots/'
     outPlotName = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) $
                   + STRING(FORMAT='("-kappaStats_",I02,"-",I02,"' + mltSuff + '","-",A0,A0,A0,A0,".png")', $
                            (minM LT 0 ? minM + 24 : minM),maxM,hemi,parmStr,kHBinSizeStr,bonusPlotSuff)
     PRINT,"Saving to " + outPlotName
     winder.Save,plotDir+outPlotName

     IF KEYWORD_SET(makeMetaStabPlot) THEN BEGIN

        ;; Kappa plot
        titleStr = STRING(FORMAT='(I02,"-",I02," ' + mltSuff + ', ",I0,"$^\circ$ < |ILAT| < ",I0,"$^\circ$!C!COrbits ",I0,"-",I0," (",I0,"/",I0," considered)")', $
                          minM+24,maxM, $
                          minI,maxI, $
                          MIN(orbArr),MAX(orbArr), $
                          N_ELEMENTS(orbArr),MAX(orbArr)-MIN(orbArr))
        winderM  = WINDOW(DIMENSIONS=[800,800],BUFFER=bufferPlots)

        xRange   = [0.,1.]
        yRange   = [0,MAX(mHist)*1.1]
        mPosition = [0.1,0.1,0.9,0.8]
        mHistPlot = PLOT(mBins,mHist,/HISTOGRAM, $
                         XRANGE=xRange, $
                         YRANGE=yRange, $
                         XTITLE='$M_q$',YTITLE='Count',TITLE=titleStr, $
                         FONT_SIZE=16,THICK=2.5, $
                         CURRENT=winderM, $
                         POSITION=mPosition)
        mLinePlot = PLOT(REPLICATE(magicMVal,11), $
                         FINDGEN(11)/10.*MAX(mHist)*2, $
                         YRANGE=yRange, $
                         THICK=2., $
                         COLOR='GREEN', $
                         /OVERPLOT, $
                         CURRENT=winderM)
        mText     = TEXT(0.7,0.8,"$\kappa_t$ = 2.45", $
                         /NORMAL, $
                         FONT_SIZE=16, $
                         FONT_COLOR='GREEN', $
                         TARGET=winderM)

        MPlotName = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) $
                    + STRING(FORMAT='("-MetaStab_",I02,"-",I02,"' + mltSuff + '","-",A0,A0,A0,A0,".png")', $
                             (minM LT 0 ? minM + 24 : minM),maxM, $
                             hemi, $
                             parmStr, $
                             mHBinSizeStr, $
                             bonusPlotSuff)

        PRINT,"Saving to " + MPlotName
        winderM.Save,plotDir+MPlotName

     ENDIF

     IF KEYWORD_SET(makeMLTILATplot) THEN BEGIN

        winder2 = WINDOW(DIMENSIONS=[800,800],BUFFER=bufferPlots)

        MLTILATplot = SCATTERPLOT(MLTs[final_i], $
                                  ABS(andre.ilat[final_i]), $
                                  XTITLE='mlt', $
                                  YTITLE='ilat', $
                                  TRANSP=50, $
                                  CURRENT=winder2)

        scatPlotName = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) $
                       + "-kS-" + mltSuff + "_ILAT_coverage" + "-" + hemi + parmStr + bonusPlotSuff + ".png"

        PRINT,"Saving to " + scatPlotName
        winder2.Save,plotDir+scatPlotName

     ENDIF  

     IF KEYWORD_SET(makeILATKappaplot) THEN BEGIN

        winder3 = WINDOW(DIMENSIONS=[800,800],BUFFER=bufferPlots)

        ILATkappaplot = SCATTERPLOT(ABS(andre.ilat[final_i]), $
                                    KF2DParms.kappa[plot_i], $
                                    XTITLE='ILAT (deg)', $
                                    YTITLE='Kappa', $
                                    YRANGE=[1.5,15], $
                                    TRANSP=50, $
                                    CURRENT=winder3)

        ilatPlotName = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) $
                       + "-kS-ILATkappa" + "-" + hemi + parmStr + bonusPlotSuff + ".png"

        PRINT,"Saving to " + ilatPlotName
        winder3.Save,plotDir+ilatPlotName

     ENDIF  

  ENDIF ELSE BEGIN

     PRINT,FORMAT='("Working with ",I0, " RequInds")',nReq
     PRINT,FORMAT='("Working with ",I0, " ExclInds")',nExc

     AARName    = 'AAR' + STRING(9B) + STRING(9B) + STRING(9B) + STRING(9B) + STRING(FORMAT='("(N = ",I4,")")',nReq)
     belAARName = 'Below AAR'                                  + STRING(9B) + STRING(FORMAT='("(N = ",I4,")")',nExc)

     ;; req_i          = WHERE(andre.ionBeam EQ 1 OR andre.ionBeam EQ 2,nReq)
     ;; exc_i          = WHERE(andre.ionBeam EQ 1 OR andre.ionBeam EQ 2,nExc)

     ;; Unique low kappa-ers
     ;; lowKappa_i        = WHERE(KF2DParms.kappa LE 2,nLowKappa,COMPLEMENT=notLowKappa_i,NCOMPLEMENT=nNotLowKappa)
     ;; lowKappa_i        = CGSETINTERSECTION(lowKappa_i,final_i,COUNT=nLowKappa)
     ;; lowKappaOrbs      = andre.orbit[lowkappa_i[UNIQ(andre.orbit[lowkappa_i],SORT(andre.orbit[lowkappa_i]))]]

     ;; plot_i            = final_i

     kHistReq = HISTOGRAM(KF2DParms.kappa[req_i], $
                       BINSIZE=kHBinSize, $
                       MIN=kHistMin, $
                       LOCATIONS=kBinsReq, $
                       REVERSE_INDICES=rKIndsReq)

     mHistReq = HISTOGRAM(MVals[req_i], $
                       BINSIZE=mHBinSize, $
                       MIN=MIN([mHistMin,checkM]), $
                       LOCATIONS=mBinsReq, $
                       REVERSE_INDICES=rMIndsReq)

     kHistExc = HISTOGRAM(KF2DParms.kappa[exc_i], $
                       BINSIZE=kHBinSize, $
                       MIN=kHistMin, $
                       LOCATIONS=kBinsExc, $
                       REVERSE_INDICES=rKIndsExc)

     mHistExc = HISTOGRAM(MVals[exc_i], $
                       BINSIZE=mHBinSize, $
                       MIN=MIN([mHistMin,checkM]), $
                       LOCATIONS=mBinsExc, $
                       REVERSE_INDICES=rMIndsExc)

     IF KEYWORD_SET(normed) THEN BEGIN
        kTotReq = TOTAL(kHistReq)
        mTotReq = TOTAL(mHistReq)

        kHistReq = FLOAT(kHistReq)/FLOAT(kTotReq)
        mHistReq = FLOAT(mHistReq)/FLOAT(mTotReq)

        kTotExc = TOTAL(kHistExc)
        mTotExc = TOTAL(mHistExc)

        kHistExc = FLOAT(kHistExc)/FLOAT(kTotExc)
        mHistExc = FLOAT(mHistExc)/FLOAT(mTotExc)

        parmStr += '-normed'
     ENDIF

     ;; Where are they less than .245?
     ;; lt245inds = !NULL
     ;; bin245 = VALUE_CLOSEST2(kBins,2.45,/CONSTRAINED)
     ;; FOR k=0,bin245-1 DO BEGIN
     ;;    IF rKInds[k] NE rKInds[k+1] THEN $
     ;;       lt245inds = [lt245inds,rKInds[rKInds[k] : rKInds[k+1]-1]]
     ;; ENDFOR

     ;; Kappa plot
     titleStr = STRING(FORMAT='(I02,"-",I02," ' + mltSuff + ', ",I0,"$^\circ$ < |ILAT| < ",I0,"$^\circ$!C!COrbits ",I0,"-",I0," (",I0,"/",I0," considered)")', $
                       minM+24,maxM, $
                       minI,maxI, $
                       MIN(orbArr),MAX(orbArr), $
                       N_ELEMENTS(orbArr),MAX(orbArr)-MIN(orbArr))
     winder   = WINDOW(DIMENSIONS=[800,800],BUFFER=bufferPlots)

     xRange   = [1.5,15]
     yRange   = [0,MAX(kHistReq)*1.1]
     kHistPlotReq = PLOT(kBinsReq,kHistReq,/HISTOGRAM, $
                         XRANGE=xRange, $
                         YRANGE=yRange, $
                         NAME=AARName, $
                         ;; LINESTYLE='
                         XTITLE='$\kappa$',YTITLE='Count',TITLE=titleStr, $
                         FONT_SIZE=16,THICK=2.5, $
                         CURRENT=winder)

     kHistPlotExc = PLOT(kBinsExc,kHistExc,/HISTOGRAM, $
                         XRANGE=xRange, $
                         YRANGE=yRange, $
                         LINESTYLE='--', $
                         NAME=belAARName, $
                         COLOR='GRAY', $
                         THICK=2.5, $
                         /OVERPLOT, $
                         CURRENT=winder)

     histLegend  = LEGEND(TARGET=[kHistPlotReq,kHistPlotExc], $
                          /NORMAL, $
                          POSITION=[0.85,0.7])

     k245LineCol = 'GREEN'
     kLinePlot = PLOT(REPLICATE(2.45,11),FINDGEN(11)/10.*MAX(kHistReq)*2, $
                      YRANGE=yRange, $
                      THICK=2., $
                      COLOR=k245LineCol, $
                      /OVERPLOT, $
                      CURRENT=winder)

     kText     = TEXT(0.25,0.85,"$\kappa_t$ = 2.45", $
                      /NORMAL, $
                      FONT_SIZE=16, $
                      FONT_COLOR=k245LineCol, $
                      TARGET=winder)

     plotDir      = '/SPENCEdata/Research/Satellites/FAST/kappa_dists/plots/'
     outPlotName = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) $
                   + STRING(FORMAT='("-kappaStats_",I02,"-",I02,"' + mltSuff + '","-",A0,A0,A0,A0,".png")', $
                            (minM LT 0 ? minM + 24 : minM),maxM,hemi,parmStr,kHBinSizeStr,bonusPlotSuff)
     PRINT,"Saving to " + outPlotName
     winder.Save,plotDir+outPlotName

     IF KEYWORD_SET(makeMetaStabPlot) THEN BEGIN

        ;; Kappa plot
        titleStr = STRING(FORMAT='(I02,"-",I02," ' + mltSuff + ', ",I0,"$^\circ$ < |ILAT| < ",I0,"$^\circ$!C!COrbits ",I0,"-",I0," (",I0,"/",I0," considered)")', $
                          minM+24,maxM, $
                          minI,maxI, $
                          MIN(orbArr),MAX(orbArr), $
                          N_ELEMENTS(orbArr),MAX(orbArr)-MIN(orbArr))
        winderM  = WINDOW(DIMENSIONS=[800,800],BUFFER=bufferPlots)

        xRange   = [0.,1.]
        yRange   = [0,MAX(mHistReq)*1.1]
        mPosition = [0.1,0.1,0.9,0.8]
        mHistPlotReq = PLOT(mBinsReq,mHistReq, $
                            /HISTOGRAM, $
                            XRANGE=xRange, $
                            YRANGE=yRange, $
                            NAME=AARName, $
                            XTITLE='$M_q$',YTITLE='Count',TITLE=titleStr, $
                            FONT_SIZE=16,THICK=2.5, $
                            CURRENT=winderM, $
                            POSITION=mPosition)

        mHistPlotExc = PLOT(mBinsExc,mHistExc,$
                            /HISTOGRAM, $
                            XRANGE=xRange, $
                            YRANGE=yRange, $
                            LINESTYLE='--', $
                            NAME=belAARName, $
                            COLOR='GRAY', $
                            THICK=2.5, $
                            CURRENT=winderM, $
                            /OVERPLOT, $
                            POSITION=mPosition)

        metaHistLegend  = LEGEND(TARGET=[mHistPlotReq,mHistPlotExc], $
                                 /NORMAL, $
                                 POSITION=[0.85,0.7])

        mLinePlot = PLOT(REPLICATE(magicMVal,11), $
                         FINDGEN(11)/10.*MAX(mHistReq)*2, $
                         YRANGE=yRange, $
                         THICK=2., $
                         COLOR=k245LineCol, $
                         /OVERPLOT, $
                         CURRENT=winderM)
        mText     = TEXT(0.7,0.72,"$\kappa_t$ = 2.45", $
                         /NORMAL, $
                         FONT_SIZE=16, $
                         FONT_COLOR=k245LineCol, $
                         TARGET=winderM)

        MPlotName = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) $
                    + STRING(FORMAT='("-MetaStab_",I02,"-",I02,"' + mltSuff + '","-",A0,A0,A0,A0,".png")', $
                             (minM LT 0 ? minM + 24 : minM),maxM, $
                             hemi, $
                             parmStr, $
                             mHBinSizeStr, $
                             bonusPlotSuff)

        PRINT,"Saving to " + MPlotName
        winderM.Save,plotDir+MPlotName

     ENDIF

     IF KEYWORD_SET(makeMLTILATplot) THEN BEGIN

        winder2 = WINDOW(DIMENSIONS=[800,800],BUFFER=bufferPlots)

        MLTILATplot1 = SCATTERPLOT(MLTs[exc_i], $
                                  ABS(andre.ilat[exc_i]), $
                                  XTITLE='mlt', $
                                  YTITLE='ilat', $
                                     SYM_COLOR='BLUE', $
                                     SYMBOL='x', $
                                     NAME=belAARName, $
                                  TRANSP=85, $
                                  CURRENT=winder2)

        MLTILATplot2 = SCATTERPLOT(MLTs[req_i], $
                                  ABS(andre.ilat[req_i]), $
                                   SYM_COLOR='RED', $
                                   SYMBOL='+', $
                                   NAME=AARName, $
                                  TRANSP=65, $
                                   CURRENT=winder2, $
                                  /OVERPLOT)

        scatPlotName = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) $
                       + "-kS-" + mltSuff + "_ILAT_coverage" + "-" + hemi + parmStr + bonusPlotSuff + ".png"

        PRINT,"Saving to " + scatPlotName
        winder2.Save,plotDir+scatPlotName

     ENDIF  

     IF KEYWORD_SET(makeILATKappaplot) THEN BEGIN

        winder3 = WINDOW(DIMENSIONS=[800,800],BUFFER=bufferPlots)

        ILATkappaplot1 = SCATTERPLOT(KF2DParms.kappa[exc_i], $
                                     ABS(andre.ilat[exc_i]), $
                                     XTITLE='Kappa', $
                                     YTITLE='ILAT (deg)', $
                                     SYM_COLOR='BLUE', $
                                     SYMBOL='x', $
                                     NAME=belAARName, $
                                     XRANGE=[1.5,15], $
                                     TRANSP=85, $
                                     CURRENT=winder3)

        ILATkappaplot2 = SCATTERPLOT(KF2DParms.kappa[req_i], $
                                     ABS(andre.ilat[req_i]), $
                                     SYM_COLOR='RED', $
                                     SYMBOL='+', $
                                     NAME=AARName, $
                                     XRANGE=[1.5,15], $
                                     TRANSP=60, $
                                     /OVERPLOT, $
                                     CURRENT=winder3)

        ilatPlotName = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) $
                       + "-kS-ILATkappa" + "-" + hemi + parmStr + bonusPlotSuff + ".png"

        PRINT,"Saving to " + ilatPlotName
        winder3.Save,plotDir+ilatPlotName

     ENDIF  

     IF KEYWORD_SET(makeMLTKappaplot) THEN BEGIN

        winder4 = WINDOW(DIMENSIONS=[800,800],BUFFER=bufferPlots)

        MLTkappaplot1 = SCATTERPLOT(KF2DParms.kappa[exc_i], $
                                     MLTs[exc_i], $
                                     XTITLE='Kappa', $
                                     YTITLE='MLT', $
                                     SYM_COLOR='BLUE', $
                                     SYMBOL='x', $
                                     NAME=belAARName, $
                                     XRANGE=[1.5,15], $
                                     TRANSP=85, $
                                     CURRENT=winder4)

        MLTkappaplot2 = SCATTERPLOT(KF2DParms.kappa[req_i], $
                                     MLTs[req_i], $
                                     SYM_COLOR='RED', $
                                     SYMBOL='+', $
                                     NAME=AARName, $
                                     XRANGE=[1.5,15], $
                                     TRANSP=60, $
                                     /OVERPLOT, $
                                     CURRENT=winder4)

        mltPlotName = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) $
                       + "-kS-MLTkappa" + "-" + hemi + parmStr + bonusPlotSuff + ".png"

        PRINT,"Saving to " + mltPlotName
        winder4.Save,plotDir+mltPlotName

     ENDIF  

  ENDELSE

  IF ~KEYWORD_SET(bufferPlots) THEN STOP

  winder.Close
  IF KEYWORD_SET(makeMLTILATplot) THEN BEGIN
     winder2.Close
  ENDIF
  IF KEYWORD_SET(makeILATkappaplot) THEN BEGIN
     winder3.Close
  ENDIF

  IF KEYWORD_SET(makeMLTkappaplot) THEN BEGIN
     winder4.Close
  ENDIF


END
