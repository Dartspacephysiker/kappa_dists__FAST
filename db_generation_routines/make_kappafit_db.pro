;2018/05/02
PRO MAKE_KAPPAFIT_DB

  COMPILE_OPT IDL2,STRICTARRSUBS

  restoreFile = 0
  outDir   = '/SPENCEdata/Research/Satellites/FAST/kappa_dists/saves_output_etc/'
  ;; restoreD = GET_TODAY_STRING(/DO_YYYYMMDD_FMT)
  ;; restoreD = '20180420' ;2018/04/23 Making a new one, calling it 20180424 for kicks
  ;; restoreD = '20180426'
  ;; restoreD = '20180427TRY3' ;Later, with orbs reaching into 7000+
  ;; restoreD = '20180427TRY4' 
  ;; restoreD = '20180427TRY5' 
  restoreD = '20180501' 
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
     badOrbs = [1257,1635,1693,1875,1945,3123,3268,3353,4377,4424,4479,7871,8281]

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

        ;; IF orbit EQ 1727 THEN STOP

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


END
