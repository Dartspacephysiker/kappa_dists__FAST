;2017/04/05
PRO JOURNAL__20170405__GET_MAG_FIELD_RATIO__EQUATORIAL_PLANE_IONOSPHERE

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; GEOPACK_CONV_COORD
  ;; Description: Convert between a variety of commonly used coordinate systems.
  ;; Calling Sequence: geopack_conv_coord(_08), s1, s2, s3, d1, d2, d3.
  ;; Inputs: s1, s2, s3: Coordinates in system of origin.
  ;; Outputs: d1, d2, d3: Coordinates in target system.
  ;; Keywords: FROM_GEO: Specify source in geopgraphic coordinates. 
  ;;  FROM_MAG: Specify source in geomagnetic coordinates.
  ;;  FROM_GEI: Specify source in geocentric equatorial inertial coordinates.
  ;;  FROM_SM: Specify source in solar magnetic coordinates.
  ;;  FROM_GSM: Specify source in geocentric solar magnetospheric
  ;;  coordinates.
  ;;  FROM_GSE: Specify source in geocentric solar ecliptic coordinates.
  ;;  TO_GEO: Specify destination in geopgraphic coordinates.
  ;;  TO_MAG: Specify destination in geomagnetic coordinates.
  ;;  TO_GEI: Specify destination in geocentric equatorial inertial coordinates.
  ;;  TO_SM: Specify destination in solar magnetic coordinates.
  ;;  TO_GSM: Specify destination in geocentric solar magnetospheric
  ;;  coordinates. 


  downTail_GSE  = [-30,0,0] ;in R_E

  PRINT,'Restoring culled OMNI data to get mag_utc ...'
  dataDir           = "/SPENCEdata/Research/database/OMNI/"
  tiltFile          = 'sw_data--dpTilt_for_culled_OMNI_magdata.dat'
  culledDataStr     = "culled_OMNI_magdata.dat"
  ;; culledSWDataStr   = "culled_OMNI_swdata.dat"
  culledPresDataStr = "culled_OMNI_densPressTempData.dat"


  ;;Solar winders
  ;; RESTORE,dataDir + culledSWDataStr
  RESTORE,dataDir + culledDataStr
  RESTORE,dataDir + culledPresDataStr
  LOAD_DST_AE_DBS,Dst

  t1Str         = '1997-02-01/09:26:00.0'
  t1            = STR_TO_TIME(t1Str)
  time_epoch    = UTC_TO_CDF_EPOCH(t1)

  tArr          = t1
  ;;PARMOD(0) SOLAR WIND RAM PRESSURE IN NANOPASCALS
  ;;PARMOD(1) DST 
  ;;PARMOD(2),PARMOD(3) IMF BY AND BZ

  magInds       = VALUE_CLOSEST2(mag_UTC,tArr,/CONSTRAIN)
  ;; swInds        = VALUE_CLOSEST2(mag_UTC_SW,tArr,/CONSTRAIN)
  presInds      = VALUE_CLOSEST2(mag_UTC_SWTP,tArr,/CONSTRAIN)
  dstInds       = VALUE_CLOSEST2(dst.time,tArr,/CONSTRAIN)

  maxTDiff      = 61.
  maxDstTDiff   = 3600.
  IF (WHERE(ABS(mag_UTC[magInds]-tArr      ) GT maxTDiff))[0] NE -1 OR $
     ;; (WHERE(ABS(mag_UTC_SW[swInds]-tArr    ) GT maxTDiff))[0] NE -1 OR $
     (WHERE(ABS(mag_UTC_SWTP[presInds]-tArr) GT maxTDiff))[0] NE -1 OR $
     (WHERE(ABS(dst.time[dstInds]-tArr     ) GT maxDstTDiff))[0] NE -1 $
  THEN BEGIN
     STOP
  ENDIF

  By            = By_GSM[magInds]
  Bz            = Bz_GSM[magInds]
  press         = pressure[presInds]
  dstVal        = dst.dst[dstInds]

  ;;
  parMod        = MAKE_ARRAY(10,/DOUBLE)
  parMod[0]     = press
  parMod[1]     = dstVal
  parMod[2]     = By
  parMod[3]     = Bz
  
  CONVERT_TIME_STRING_TO_YMDHMS_ARRAYS, $
     t1Str, $
     OUT_YEARARR=yearArr, $
     OUT_DOYARR=DOYArr, $
     OUT_MONTHARR=monthArr, $
     OUT_DAYARR=dayArr, $
     OUT_HOURARR=hourArr, $
     OUT_MINARR=minArr, $
     OUT_SECARR=secArr

  GET_FA_ORBIT,t1,/TIME_ARRAY,/NO_STORE,STRUC=struc

  i=0
  GEOPACK_RECALC_08,YearArr[i],MonthArr[i],DayArr[i], $
                    HourArr[i],MinArr[i],SecArr[i], $
                    /DATE, $
                    TILT=thisTilt

  CASE 1 OF
     KEYWORD_SET(start_at_equator): BEGIN
        ;;GSE to GSW
        GEOPACK_CONV_COORD_08,downTail_GSE[0],downTail_GSE[1],downTail_GSE[2], $
                              downTail_GSM_x,downTail_GSM_y,downTail_GSM_z, $
                              /FROM_GSE,/TO_GSW,EPOCH=time_epoch[0]

        downTail_GSM  = [downTail_GSM_x,downTail_GSM_y,downTail_GSM_z]

        GEOPACK_TRACE_08,downTail_GSM_x,downTail_GSM_y,downTail_GSM_z, $
                         -1,parMod, $
                         ionos_GSM_x,ionos_GSM_y,ionos_GSM_z, $
                         /REFINE, $
                         /IONOSPHERE, $
                         /TS04, $
                         TILT=thisTilt ;should be in degrees

     END
     ELSE: BEGIN
        ;;GEO to GEI
        struc.fa_pos /= 6370.D
        GEOPACK_CONV_COORD_08,struc.fa_pos[0],struc.fa_pos[1],struc.fa_pos[2], $
                              FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
                              /FROM_GSE,/TO_GSW,EPOCH=time_epoch[0]

        FAST_GSM      = [FAST_GSM_x,FAST_GSM_y,FAST_GSM_z]

        GEOPACK_TRACE_08,FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
                         1,parMod, $
                         downTail_GSM_x,downTail_GSM_y,downTail_GSM_z, $
                         /REFINE, $
                         /EQUATOR, $
                         ;; /TS04, $
                         /IGRF, $
                         TILT=thisTilt ;should be in degrees

        GEOPACK_TRACE_08,FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
                         -1,parMod, $
                         ionos_GSM_x,ionos_GSM_y,ionos_GSM_z, $
                         /REFINE, $
                         /IONOSPHERE, $
                         ;; /TS04, $
                         /IGRF, $
                         TILT=thisTilt ;should be in degrees

        downTail_GSM  = [downTail_GSM_x,downTail_GSM_y,downTail_GSM_z]
        ionos_GSM     = [ionos_GSM_x,ionos_GSM_y,ionos_GSM_z]


        ;;   IOPGEN - GENERAL OPTION FLAG:  IOPGEN=0 - CALCULATE TOTAL FIELD
        ;;                                  IOPGEN=1 - DIPOLE SHIELDING ONLY
        ;;                                  IOPGEN=2 - TAIL FIELD ONLY
        ;;                                  IOPGEN=3 - BIRKELAND FIELD ONLY
        ;;                                  IOPGEN=4 - RING CURRENT FIELD ONLY
        ;;                                  IOPGEN=5 - INTERCONNECTION FIELD ONLY
        ;;
        ;;   IOPT -  TAIL FIELD FLAG:       IOPT=0  -  BOTH MODES
        ;;                                  IOPT=1  -  MODE 1 ONLY
        ;;                                  IOPT=2  -  MODE 2 ONLY
        ;;
        ;;   IOPB -  BIRKELAND FIELD FLAG:  IOPB=0  -  ALL 4 TERMS
        ;;                                  IOPB=1  -  REGION 1, MODES 1 AND 2
        ;;                                  IOPB=2  -  REGION 2, MODES 1 AND 2
        ;;
        ;;   IOPR -  RING CURRENT FLAG:     IOPR=0  -  BOTH SRC AND PRC
        ;;                                  IOPR=1  -  SRC ONLY
        ;;                                  IOPR=2  -  PRC ONLY

        GEOPACK_TS04,parMod,downTail_GSM_x,downTail_GSM_y,downTail_GSM_z, $
                     downTail_Bx,downTail_By,downTail_Bz, $
                     TILT=thisTilt, $
                     IOPGEN=0

        GEOPACK_TS04,parMod,FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
                     FAST_Bx,FAST_By,FAST_Bz, $
                     TILT=thisTilt, $
                     IOPGEN=0

        GEOPACK_TS04,parMod,ionos_GSM_x,ionos_GSM_y,ionos_GSM_z, $
                     ionos_Bx,ionos_By,ionos_Bz, $
                     TILT=thisTilt, $
                     IOPGEN=0

        GEOPACK_IGRF_GSW_08,downTail_GSM_x,downTail_GSM_y,downTail_GSM_z, $
                     downTail_Bx_IGRF,downTail_By_IGRF,downTail_Bz_IGRF, $
                            EPOCH=time_epoch[0]

        GEOPACK_IGRF_GSW_08,FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
                     FAST_Bx_IGRF,FAST_By_IGRF,FAST_Bz_IGRF, $
                     EPOCH=time_epoch[0]

        GEOPACK_IGRF_GSW_08,ionos_GSM_x,ionos_GSM_y,ionos_GSM_z, $
                     ionos_Bx_IGRF,ionos_By_IGRF,ionos_Bz_IGRF, $
                     EPOCH=time_epoch[0]


        FAST_RE       = SQRT(TOTAL(FAST_GSM^2))
        downTail_RE   = SQRT(TOTAL(downTail_GSM^2))
        ionos_RE      = SQRT(TOTAL(ionos_GSM^2))

        RE_to_km      = 6370.D
        FAST_km       = (FAST_RE     - 1.D ) * RE_to_km 
        downTail_km   = (downTail_RE - 1.D ) * RE_to_km 
        ionos_km      = (ionos_RE    - 1.D ) * RE_to_km 

        FAST_B        = [FAST_Bx,FAST_By,FAST_Bz]
        downTail_B    = [downTail_Bx,downTail_By,downTail_Bz]
        ionos_B       = [ionos_Bx,ionos_By,ionos_Bz]

        FAST_B_IGRF        = [FAST_Bx_IGRF,FAST_By_IGRF,FAST_Bz_IGRF]
        downTail_B_IGRF    = [downTail_Bx_IGRF,downTail_By_IGRF,downTail_Bz_IGRF]
        ionos_B_IGRF       = [ionos_Bx_IGRF,ionos_By_IGRF,ionos_Bz_IGRF]

        FAST_BMag     = SQRT(TOTAL(FAST_B^2))
        downTail_BMag = SQRT(TOTAL(downTail_B^2))
        ionos_BMag    = SQRT(TOTAL(ionos_B^2))

        FAST_B_IGRFMag     = SQRT(TOTAL(FAST_B_IGRF^2))
        downTail_B_IGRFMag = SQRT(TOTAL(downTail_B_IGRF^2))
        ionos_B_IGRFMag    = SQRT(TOTAL(ionos_B_IGRF^2))

        R_B_FAST      = FAST_BMag/downTail_BMag
        R_B_ionos     = ionos_BMag/downTail_BMag

        R_B_IGRF_FAST      = FAST_B_IGRFMag/downTail_B_IGRFMag
        R_B_IGRF_ionos     = ionos_B_IGRFMag/downTail_B_IGRFMag

     END
  ENDCASE


  STOP

END
