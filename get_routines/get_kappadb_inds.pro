;2018/05/02
FUNCTION GOVERK_CHI2FUNC,kappa, $
                         DECILE=GoverKReq

  decile = N_ELEMENTS(GoverKReq) GT 0 ? LONG(STRMID(GoverKReq,7,1)) : 1
  CASE decile OF
     1: BEGIN
        ;; first decile
        a = 0.071777
        b = 0.979759
        c = -5
     END
     2: BEGIN
        ;; second decile
        a = 0.0595617
        b = 0.894554
        c = -5
     END
     3: BEGIN
        ;; third decile
        a = 0.0550905
        b = 0.83265
        c = -5
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
                          HEMI=hemi, $
                          NORTH=north, $
                          SOUTH=south, $
                          BOTH_HEMIS=both_hemis, $
                          GLOBE=globe, $
                          DAYSIDE=dayside, $
                          NIGHTSIDE=nightside
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

  IF checkMin THEN mltMinFmt = 'F0.1' ELSE mltMinFmt = 'I02'
  IF checkMax THEN mltMaxFmt = 'F0.1' ELSE mltMaxFmt = 'I02'

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
     
     altStr = STRING(FORMAT='("-",I0,"-",I0,"ALT")', $
                              minA,maxA)
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

  RETURN,final_i

END
