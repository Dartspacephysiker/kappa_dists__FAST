;2018/05/02
FUNCTION GOVERK_CHI2FUNC,kappa, $
                         DECILE=GoverKReq

  CASE 1 OF
     STRMATCH(GoverKReq,'ventile*'): BEGIN
        ventile = LONG(STRMID(GoverKReq,8,(STRLEN(GoverKReq) EQ 10 ? 2 : 1)))

        CASE ventile OF
           1: BEGIN
              a = 0.0321162
              b = 1.05356
              c = -9.99884
           END
           2: BEGIN
              a = 0.0287779
              b = 1.0044
              c = -9.99912
           END
           3: BEGIN
              a = 0.0261157
              b = 0.980735
              c = -9.9993
           END
           4: BEGIN
              a = 0.0259515
              b = 0.954985
              c = -9.99822
           END
           5: BEGIN
              a = 0.0328899
              b = 0.916171
              c = -7.97266
           END
           6: BEGIN
              a = 0.038353
              b = 0.881016
              c = -6.7753
           END
           7: BEGIN
              a = 0.038521
              b = 0.860842
              c = -6.56118
           END
           8: BEGIN
              a = 0.0393816
              b = 0.839244
              c = -6.28238
           END
           9: BEGIN
              a = 0.0461834
              b = 0.791554
              c = -5.35093
           END
           10: BEGIN
              a = 0.0489726
              b = 0.757539
              c = -4.9623
           END
           11: BEGIN
              a = 0.0505127
              b = 0.730309
              c = -4.72134
           END
           12: BEGIN
              a = 0.0490058
              b = 0.718652
              c = -4.74476
           END
           13: BEGIN
              a = 0.0500843
              b = 0.691966
              c = -4.58643
           END
           14: BEGIN
              a = 0.0471004
              b = 0.690131
              c = -4.72254
           END
           15: BEGIN
              a = 0.0492251
              b = 0.652176
              c = -4.40406
           END
           16: BEGIN
              a = 0.0518239
              b = 0.609359
              c = -4.0809
           END
           17: BEGIN
              a = 0.0551121
              b = 0.554346
              c = -3.71785
           END
           18: BEGIN
              a = 0.0530357
              b = 0.529952
              c = -3.66168
           END
           19: BEGIN
              a = 0.0491673
              b = 0.504251
              c = -3.64071
           END
        ENDCASE

     END
     ELSE: BEGIN
        decile = N_ELEMENTS(GoverKReq) GT 0 ? LONG(STRMID(GoverKReq,7,1)) : 1

        ;; haveDeciles = [1,2,3]
        haveDeciles = [1,2,3,4,5,6,7,8,9]
        IF (WHERE(decile EQ haveDeciles))[0] EQ -1 THEN BEGIN
           PRINT,"Don't have decile " + STRING(FORMAT='(I0)',decile) + "!"
           STOP
        ENDIF
        

        CASE decile OF
           ;; 1: BEGIN
           ;;    ;; first decile
           ;;    a = 0.071777
           ;;    b = 0.979759
           ;;    c = -5
           ;; END
           ;; 2: BEGIN
           ;;    ;; second decile
           ;;    a = 0.0595617
           ;;    b = 0.894554
           ;;    c = -5
           ;; END
           ;; 3: BEGIN
           ;;    ;; third decile
           ;;    a = 0.0550905
           ;;    b = 0.83265
           ;;    c = -5
           ;; END
           1: BEGIN
              a = 0.0298931
              b = 1.00168
              c = -9.99969
           END
           2: BEGIN
              a = 0.0263136
              b = 0.955473
              c = -9.99969
           END
           3: BEGIN
              a = 0.0284356
              b = 0.911727
              c = -8.97227
           END
           4: BEGIN
              a = 0.0362803
              b = 0.850323
              c = -6.87573
           END
           5: BEGIN
              a = 0.0462379
              b = 0.769292
              c = -5.29457
           END
           6: BEGIN
              a = 0.0481355
              b = 0.721586
              c = -4.85215
           END
           7: BEGIN
              a = 0.0454183
              b = 0.697242
              c = -4.92433
           END
           8: BEGIN
              a = 0.0473201
              b = 0.635967
              c = -4.43713
           END
           9: BEGIN
              a = 0.0399212
              b = 0.631185
              c = -4.88113
           END
        ENDCASE

     END
  ENDCASE

  RETURN,1.+(a * kappa + b)^c
END
FUNCTION KCHI2MAX_CHI2FUNC,kappa, $
                         DECILE=KChi2Max

  decile = N_ELEMENTS(KChi2Max) GT 0 ? LONG(STRMID(KChi2Max,7,1)) : 1
  CASE decile OF
     7: BEGIN
        ;; seventh decile
        PRINT,"Needs to be done"
        STOP
        a = 0.071777
        b = 0.979759
        c = -5
     END
     8: BEGIN
        ;; eigth decile
        a = 0.174054
        b = 0.384697
        c = -7.61408
        d = 3.65279
     END
     9: BEGIN
        ;; ninth decile
        a = 0.200464
        b = -0.0548358
        c = -2.70258
        d = 6.17905
     END
  ENDCASE


  RETURN,d+(a * kappa + b)^c
END
FUNCTION GET_KAPPADB_INDS,andre, $
                          KF2DParms, $
                          GF2DParms, $
                          OUT_COUNT=count, $
                          GOVERKREQ=GoverKReq, $
                          KCHI2MAX=KChi2Max, $
                          MINMLT=minM, $
                          MAXMLT=maxM, $
                          BINMLT=binM, $
                          SHIFTMLT=shiftM, $
                          MINILAT=minI, $
                          MAXILAT=maxI, $
                          MINALT=minA, $
                          MAXALT=maxA, $
                          MLTSTR=mltStr, $
                          ALTSTR=altStr, $
                          DSTSTR=dstStr, $
                          HEMI=hemi, $
                          NORTH=north, $
                          SOUTH=south, $
                          BOTH_HEMIS=both_hemis, $
                          GLOBE=globe, $
                          DAYSIDE=dayside, $
                          NIGHTSIDE=nightside, $
                          DSTCUTOFF=dstCutoff
                          ;; USE_LNG=use_lng, $
                          ;; MINLNG=minLng, $
                          ;; MAXLNG=maxLng, $
                          ;; BINLNG=binLng, $
                          ;; SHIFTLNG=shiftLng, $
                          ;; BINILAT=binI, $
                          ;; SHIFTILAT=shiftI, $
                          ;; DONT_CORRECT_ILATS=dont_correct_ilats, $
                          ;; DO_LSHELL=do_lShell, $
                          ;; MINLSHELL=minL, $
                          ;; MAXLSHELL=maxL, $
                          ;; BINLSHELL=binL, $
                          ;; REVERSE_LSHELL=reverse_lShell, $
                          ;; COORDINATE_SYSTEM=coordinate_system, $
                          ;; USE_AACGM_COORDS=use_AACGM, $
                          ;; USE_GEI_COORDS=use_GEI, $
                          ;; USE_GEO_COORDS=use_GEO, $
                          ;; USE_MAG_COORDS=use_MAG, $
                          ;; USE_SDT_COORDS=use_SDT, $
                          ;; MIN_MAGCURRENT=minMC, $
                          ;; MAX_NEGMAGCURRENT=maxNegMC, $
                          ;; MIMC_STRUCT=MIMC_struct, $
                          ;; MAP_PROJECTION=map_projection, $

  COMPILE_OPT IDL2,STRICTARRSUBS

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

  checkMin  = ( ABS(ROUND(minM)-minM) GT 0.1 ) 
  checkMax  = ( ABS(ROUND(maxM)-maxM) GT 0.1 )

  IF checkMin THEN mltMinFmt = 'F0.2' ELSE mltMinFmt = 'I02'
  IF checkMax THEN mltMaxFmt = 'F0.2' ELSE mltMaxFmt = 'I02'

  mltStr = STRING(FORMAT='(' + mltMinFmt + ',"-",' + mltMaxFmt + ',A0)', $
                  (minM LT 0 ? minM + 24 : minM),maxM,mltSuff)

  IF (checkMin OR checkMax) THEN mltStr = mltStr.Replace('.','_')

  ilat_i            = GET_ILAT_INDS(andre, $
                                    minI, $
                                    maxI, $
                                    hemi, $
                                    N_ILAT=n_ilat, $
                                    N_NOT_ILAT=n_not_ilat, $
                                    LUN=lun)
  region_i          = CGSETINTERSECTION(ilat_i,mlt_i)

  altStr = ''
  IF KEYWORD_SET(minA) OR KEYWORD_SET(maxA) THEN BEGIN

     minA = KEYWORD_SET(minA) ? minA : 300
     maxA = KEYWORD_SET(maxA) ? maxA : 4300
     
     alt_i = GET_ALTITUDE_INDS(andre,minA,maxA)

     region_i = CGSETINTERSECTION(region_i,alt_i)
     
     IF minA EQ 300 AND maxA EQ 4300 THEN BEGIN
        altStr = '-allALT'
     ENDIF ELSE BEGIN
        altStr = STRING(FORMAT='("-",I0,"-",I0,"ALT")', $
                        minA,maxA)
     ENDELSE

  ENDIF

  mono_i            = WHERE(andre.mono  EQ 1 OR andre.mono  EQ 2, $
                            nMono, $
                            NCOMPLEMENT=nNotMono)
  broad_i           = WHERE(andre.broad EQ 1 OR andre.broad EQ 2, $
                            nBroad, $
                            NCOMPLEMENT=nNotBroad)

  final_i           = CGSETINTERSECTION(region_i,mono_i, $
                                        COUNT=count)
  IF count EQ 0 THEN STOP

  IF N_ELEMENTS(GoverKReq) GT 0 THEN BEGIN

     ratio = GF2DParms.chi2red/KF2DParms.chi2red
     CASE 1 OF
        SIZE(GoverKReq,/TYPE) EQ 7: BEGIN
           chi2_i = WHERE(ratio GE GOVERK_CHI2FUNC(KF2DParms.kappa, $
                                                  DECILE=GoverKReq), $
                          nChi2, $
                          NCOMPLEMENT=nNotChi2)
        END
        ELSE: BEGIN
           IF GoverKReq NE 0 THEN BEGIN
              chi2_i         = WHERE(ratio GE GoverKReq, $
                                     nChi2, $
                                     NCOMPLEMENT=nNotChi2)
           ENDIF ELSE BEGIN
              chi2_i = LINDGEN(N_ELEMENTS(ratio))
           ENDELSE
        END
     ENDCASE

     final_i        = CGSETINTERSECTION(final_i,chi2_i, $
                                        COUNT=count)
     IF count EQ 0 THEN STOP

  ENDIF

  IF N_ELEMENTS(KChi2Max) GT 0 THEN BEGIN

     CASE 1 OF
        SIZE(KChi2Max,/TYPE) EQ 7: BEGIN
           chi2_i = WHERE(KF2DParms.chi2red LE KCHI2MAX_CHI2FUNC(KF2DParms.kappa, $
                                                  DECILE=KChi2Max), $
                          nChi2, $
                          NCOMPLEMENT=nNotChi2)
        END
        ELSE: BEGIN
           IF KChi2Max NE 0 THEN BEGIN
              chi2_i         = WHERE(KF2DParms.chi2red LE KChi2Max, $
                                     nChi2, $
                                     NCOMPLEMENT=nNotChi2)
           ENDIF ELSE BEGIN
              chi2_i = LINDGEN(N_ELEMENTS(KF2DParms.chi2red))
           ENDELSE
        END
     ENDCASE

     final_i        = CGSETINTERSECTION(final_i,chi2_i, $
                                        COUNT=count)
     IF count EQ 0 THEN STOP

  ENDIF

  ;; IF N_ELEMENTS(KChi2Max) GT 0 THEN IF KChi2Max NE 0 THEN BEGIN
  ;;    chi2_i         = WHERE(KF2DParms.chi2red LE KChi2Max, $
  ;;                           nChi2, $
  ;;                           NCOMPLEMENT=nNotChi2)

  ;;    final_i        = CGSETINTERSECTION(final_i,chi2_i, $
  ;;                                       COUNT=count)
  ;;    IF count EQ 0 THEN STOP

  ;; ENDIF

  dstStr = ''
  IF KEYWORD_SET(dstCutoff) THEN BEGIN
     SET_ALFVENDB_PLOT_DEFAULTS, $
        DSTCUTOFF=dstCutoff, $
        /USE_STORM_STUFF, $
        ALFDB_PLOT_STRUCT=alfDB_plot_struct

     SET_IMF_PARAMS_AND_IND_DEFAULTS, $
        CLOCKSTR=clockStr, $
        EARLIEST_JULDAY=earliest_julDay, $
        LATEST_JULDAY=latest_julDay, $
        IMF_STRUCT=IMF_struct, $
        ALFDB_PLOT_STRUCT=alfDB_plot_struct, $
        LUN=lun

     GET_NONSTORM_MAINPHASE_AND_RECOVERYPHASE_FASTDB_INDICES, $
        ALFDB_PLOT_STRUCT=alfDB_plot_struct, $
        IMF_STRUCT=IMF_struct, $
        /GET_CUSTOM_I_NOT_ALFDB_I, $
        CUSTOMDB=andre, $
        CUSTOMTIMES=andre.time, $
        CUSTOMGOOD_I=final_i, $
        NONSTORM_I=ns_i, $
        MAINPHASE_I=mp_i, $
        RECOVERYPHASE_I=rp_i, $
        N_NONSTORM=n_ns, $
        N_STORM=n_s, $
        N_MAINPHASE=n_mp, $
        N_RECOVERYPHASE=n_rp

     final_i = CGSETINTERSECTION(final_i,ns_i)

     dstStr = STRING(FORMAT='(A0,I0)','-Dst',Dstcutoff)
     
  ENDIF

  RETURN,final_i

END
