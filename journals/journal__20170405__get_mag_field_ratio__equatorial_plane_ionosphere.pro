;2017/04/05
PRO JOURNAL__20170405__GET_MAG_FIELD_RATIO__EQUATORIAL_PLANE_IONOSPHERE, $
   TS04=TS04, $
   IGRF=IGRF

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

  ;; start_at_equator = 0
  ;; ;; downTail_GSE  = [-30,0,0] ;in R_E
  ;; downTail_GSE  = [-30,0,0] ;in R_E

  ;; RLim = 4

  times = ['1997-06-15/02:34:10','1997-06-15/02:36:50']

  tee1  = times[0]
  tee2  = times[1]

  ;;Which field model?
  ;; TS04          = 0
  ;; IGRF          = 1

  ;;For TS04
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
  IOPGen = 0
  ;; IOPT   = !NULL
  ;; IOPB   = !NULL
  ;; IOPR   = !NULL


  GET_FA_MIRROR_RATIO__UTC,tee1,tee2, $
                           TIME_ARRAY=time_array, $
                           START_AT_EQUATOR=start_at_equator, $
                           DOWNTAIL_GSE=downTail_GSE, $
                           IGRF=IGRF, $
                           TS04=TS04, $
                           IOPGEN=IOPGen, $
                           IOPT=IOPT, $
                           IOPB=IOPB, $
                           IOPR=IOPR, $
                           R0=R0, $
                           RLIM=RLim

  STOP

END
