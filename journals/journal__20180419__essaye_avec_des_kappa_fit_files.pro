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

  restoreFile = 1

  outDir   = '/SPENCEdata/Research/Satellites/FAST/kappa_dists/saves_output_etc/'
  ;; restoreD = GET_TODAY_STRING(/DO_YYYYMMDD_FMT)
  ;; restoreD = '20180420' ;2018/04/23 Making a new one, calling it 20180424 for kicks
  restoreD = '20180424'
  outFName = restoreD+'-parsedKappa.sav'

  IF KEYWORD_SET(restoreFile) THEN BEGIN
     makeNewFile = ~FILE_TEST(outDir+outFName)
     IF makeNewFile THEN BEGIN
        PRINT,"Couldn't find " + outFName + '! Remaking ...'
        STOP
     ENDIF
  ENDIF ELSE makeNewFile = 1

  IF KEYWORD_SET(makeNewFile) THEN BEGIN

     badOrbs = [1257,1635,1693,1875,1945,3123,3268,3353]

     inDir = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/'
     CAPDir = inDir + 'cur_and_pot_analysis/'

     CAPpref = 'Orbit_'
     CAPmidM = '-apres_Elph98--GETKLOWBOUNDblkBox-Fig2__meal-aR_mom_eD'
     CAPmidI = '-apres_Elph98--GETKLOWBOUNDblkBox-Fig2_ingredients-aR_mom_eD'
     CAPsuff = '-sc_pot-sRate1_25.sav'

     Newelldir = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/kappa_Newell_data/20180424/'
     Newellpref = 'NewellData-'
     Newellsuff = '-GETKLOWBOUND-'

     newellListe = FILE_SEARCH(Newelldir+Newellpref+'*'+Newellsuff+'*'+'.sav')

;; Orbit_1450-apres_Elph98--GETKLOWBOUNDblkBox-Fig2__meal-aR_mom_eD_-32-32-sc_pot-sRate1_25.sav
;;   Orbit_1450-apres_Elph98--GETKLOWBOUNDblkBox-Fig2_ingredients-aR_mom_eD_-32-32-sc_pot-sRate1_25.sav

     ;; dato  = '20180419'
     dato  = '20180424'
     pref  = dato + '-orb_'
     suff  = '-KandGfits-ees-GETKLOWBOUND-only_fit_peak_eRange-sRate1_25.sav'
     SPAWN,'cd ' + inDir + '; ls ' + pref + '*' + suff,liste

     timeagoStr  = '-mtime -5'
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

     maks      = 30000
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
                  newell_tMismatch  : bArr}

     bArr      = !NULL

     curInd    = 0
     ;; KF2DParms = !NULL
     ;; GF2DParms = !NULL
     nIkkeHa   = 0
     totT      = 0.D
     orbArr    = !NULL
     FOR k=0,nFil-1 DO BEGIN

        kappaFit1Ds         = !NULL
        gaussFit1Ds         = !NULL
        fit2DKappa_inf_list = !NULL
        fit2DGauss_inf_list = !NULL
        kFit2DParam_struct  = !NULL
        gFit2DParam_struct  = !NULL
        fit2DK              = !NULL
        fit2DG              = !NULL

        orbStr = STRMID(liste[k],STRLEN(pref),4)
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

        RESTORE,inDir+liste[k]

        IF nNewell GT 1 THEN BEGIN
           x           = !NULL
           mlt         = !NULL
           ilat        = !NULL
           mono        = !NULL
           broad       = !NULL
           diffuse     = !NULL
           je          = !NULL
           jee         = !NULL
           nbad_espec  = !NULL
           info        = !NULL

           nHere = N_ELEMENTS(events.x)
           FOR bro=0,nNewell-1 DO BEGIN
              events = !NULL
              RESTORE,newellListe[newellInd[bro]]

              x           = [x         ,events.x            ]
              mlt         = [mlt       ,events.mlt          ]
              ilat        = [ilat      ,events.ilat         ]
              mono        = [mono      ,events.mono         ]
              broad       = [broad     ,events.broad        ]
              diffuse     = [diffuse   ,events.diffuse    ]
              je          = [je        ,events.je           ]
              jee         = [jee       ,events.jee          ]
              nbad_espec  = [nbad_espec,events.nbad_espec ]
              info        = [info      ,events.info       ]

           ENDFOR

           sorti  = SORT(x)
           events = {x         : (TEMPORARY(x         ))[sorti], $
                    mlt        : (TEMPORARY(mlt       ))[sorti], $
                    ilat       : (TEMPORARY(ilat      ))[sorti], $
                    mono       : (TEMPORARY(mono      ))[sorti], $
                    broad      : (TEMPORARY(broad     ))[sorti], $
                    diffuse    : (TEMPORARY(diffuse   ))[sorti], $
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
        PRINT,"You're dupin'"
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
                  AE                : AE.AE[match_ae_i]                , $
                  DST               : DST.DST[match_dst_i]             , $
                  newell_tMismatch  : andre.newell_tMismatch[finalInds]}

     PRINT,"Saving " + outFName + ' ...'
     SAVE,andre,KF2DParms,GF2DParms,totT,orbArr,FILENAME=outDir+outFName

  ENDIF ELSE BEGIN

     PRINT,"Restoring " + outFName + ' ...'
     RESTORE,outDir+outFName

  ENDELSE

  minM  = -5
  maxM  = 5
  minI  = 60
  maxI  = 89
  hemi  = 'BOTH'

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

  ;; mlt_i = notMlt_i

  ilat_i            = GET_ILAT_INDS(andre, $
                                    minI, $
                                    maxI, $
                                    hemi, $
                                    N_ILAT=n_ilat, $
                                    N_NOT_ILAT=n_not_ilat, $
                                    LUN=lun)
  region_i          = CGSETINTERSECTION(ilat_i,mlt_i)

  mono_i            = WHERE(andre.mono  EQ 1 OR andre.mono  EQ 2,nMono,NCOMPLEMENT=nNotMono)
  broad_i           = WHERE(andre.broad EQ 1 OR andre.broad EQ 2,nBroad,NCOMPLEMENT=nNotBroad)

  chi2_i            = WHERE((GF2DParms.chi2red/KF2DParms.chi2red GE 2.0) AND $
                            (KF2DParms.chi2red LT 5),nChi2,NCOMPLEMENT=nNotChi2)
  final_i           = CGSETINTERSECTION(region_i,mono_i,COUNT=count)
  IF count EQ 0 THEN STOP
  final_i           = CGSETINTERSECTION(final_i,chi2_i,COUNT=count)
  IF count EQ 0 THEN STOP
  STOP

  ;; Unique low kappa-ers

  lowKappa_i        = WHERE(KF2DParms.kappa LE 2,nLowKappa,COMPLEMENT=notLowKappa_i,NCOMPLEMENT=nNotLowKappa)
  lowKappa_i        = CGSETINTERSECTION(lowKappa_i,final_i,COUNT=nLowKappa)
  lowKappaOrbs      = andre.orbit[lowkappa_i[UNIQ(andre.orbit[lowkappa_i],SORT(andre.orbit[lowkappa_i]))]]

  plot_i            = final_i

  nNorth            = FLOAT(N_ELEMENTS(WHERE(andre.ilat[final_i] GE 0)))
  nSouth            = FLOAT(N_ELEMENTS(WHERE(andre.ilat[final_i] LT 0)))
  pctNorth          = nNorth/count
  pctSouth          = nSouth/count

  !P.multi = [0,1,1,0,0]
  WINDOW,1,XSIZE=600,YSIZE=600
  CGHISTOPLOT,KF2DParms.kappa[plot_i],BINSIZE=0.15,MININPUT=1.5,MAXINPUT=10

  !P.multi = [0,2,2,0,0]
  WINDOW,0,XSIZE=600,YSIZE=600
  CGHISTOPLOT,ALOG10(GF2DParms.chi2red[plot_i]),TITLE="Maxwellian"
  CGHISTOPLOT,ALOG10(KF2DParms.chi2red[plot_i]),TITLE='Kappa'
  CGHISTOPLOT,GF2DParms.chi2red[plot_i]/KF2DParms.chi2red[plot_i],TITLE='Ratio G/K',MAXINPUT=10,binsize=0.1

  binSize = 0.5
  histMin = 1.45
  kHist = HISTOGRAM(KF2DParms.kappa[plot_i], $
                    BINSIZE=binSize, $
                    MIN=histMin, $
                    LOCATIONS=kBins, $
                    REVERSE_INDICES=rInds)

  ;; Where are they less than .245?
  lt245inds = !NULL
  bin245 = VALUE_CLOSEST2(kBins,2.45,/CONSTRAINED)
  FOR k=0,bin245-1 DO BEGIN
     IF rInds[k] NE rInds[k+1] THEN $
        lt245inds = [lt245inds,rInds[rInds[k] : rInds[k+1]-1]]
  ENDFOR

  STOP

  titleStr = STRING(FORMAT='(I02,"-",I02," MLT, ",I0,"$^\circ$ < |ILAT| < ",I0,"$^\circ$!C!COrbits ",I0,"-",I0," (",I0,"/",I0," considered)")', $
                    minM+24,maxM, $
                    minI,maxI, $
                    MIN(orbArr),MAX(orbArr), $
                    N_ELEMENTS(orbArr),MAX(orbArr)-MIN(orbArr))
  winder   = WINDOW(DIMENSIONS=[800,800])

  xRange   = [1.5,15]
  histPlot = PLOT(kBins,kHist,/HISTOGRAM, $
                  XRANGE=xRange,YRANGE=[0,MAX(kHist)*1.2], $
                  XTITLE='$\kappa$',YTITLE='Count',TITLE=titleStr, $
                  FONT_SIZE=16,THICK=2.5, $
                 CURRENT=winder)
  linePlot = PLOT(REPLICATE(2.45,11),FINDGEN(11)/10.*MAX(kHist)*2, $
                  YRANGE=[0,MAX(kHist)*1.2], $
                  THICK=2.,COLOR='GREEN',/OVERPLOT, $
                  CURRENT=winder)
  text     = TEXT(0.25,0.8,"$\kappa_t$ = 2.45",/NORMAL, $
                  FONT_SIZE=16,FONT_COLOR='GREEN', $
                  TARGET=winder)

  outDir      = '/SPENCEdata/Research/Satellites/FAST/kappa_dists/plots/'
  outPlotName = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) $
                + STRING(FORMAT='("-kappaStats_",I02,"-",I02,"MLT.png")', $
                         (minM LT 0 ? minM + 24 : minM),maxM)
  PRINT,"Saving to " + outPlotName
  winder.Save,outDir+outPlotName
  winder.Close

END
