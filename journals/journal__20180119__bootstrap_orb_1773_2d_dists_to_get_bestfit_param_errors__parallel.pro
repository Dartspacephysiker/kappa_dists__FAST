;; 2018/01/19
;; It woiks
;; 2018/01/20
;; Somehow we missed a bunch of files...
;; These'ns:
;; orb1773_2DMCarlo_ests__09_26_41__669_synthetic_wGauss-10000Rolls-fit2DParams.sav
;; orb1773_2DMCarlo_ests__09_26_42__301_synthetic_wGauss-10000Rolls-fit2DParams.sav
;; orb1773_2DMCarlo_ests__09_26_42__933_synthetic_wGauss-10000Rolls-fit2DParams.sav
;; orb1773_2DMCarlo_ests__09_26_43__565_synthetic_wGauss-10000Rolls-fit2DParams.sav
;; orb1773_2DMCarlo_ests__09_26_45__461_synthetic_wGauss-10000Rolls-fit2DParams.sav
;; orb1773_2DMCarlo_ests__09_26_47__357_synthetic_wGauss-10000Rolls-fit2DParams.sav
;; orb1773_2DMCarlo_ests__09_26_48__621_synthetic_wGauss-10000Rolls-fit2DParams.sav

PRO JOURNAL__20180119__BOOTSTRAP_ORB_1773_2D_DISTS_TO_GET_BESTFIT_PARAM_ERRORS__PARALLEL

  mainRoutine = '/SPENCEdata/Research/Satellites/FAST/kappa_dists/journals/journal__20180117__bootstrap_orb_1773_2d_dists_to_get_bestfit_param_errors.pro'

  proDir1     = '/SPENCEdata/Research/Satellites/FAST/kappa_dists/'
  routineArr1 = STRLOWCASE( $
                ['KAPPA_FIT2D__MONTECARLO_UNCERTAINTY', $
                 'KAPPA_FIT2D__MONTECARLO__1DINIT']) + '.pro'

  ;; proDir2     = '~/idl/lib/hatch_idl_utils/sdt_routines/'
  ;; routineArr2 = STRLOWCASE(["GET_EN_SPEC__SINGLE_STRUCT", $
  ;;                           ]$

  ;; routineArr = ['fastdb_coordinate_conversion__single.pro', $
  ;;               'create_fastdb_tstamps.pro', $
  ;;               'get_fast_gei_coords.pro', $
  ;;               'convert_gei_coords_to_geo_and_mag_coords.pro', $
  ;;               'convert_geo_to_aacgm.pro', $
  ;;               'get_dipoletilt_data.pro']

  ;; proDir     = '~/idl/lib/hatch_idl_utils/coordinate_conversions/'        

  nExec      = KEYWORD_SET(create_timeStamps     ) + KEYWORD_SET(get_GEI_coords      ) + $
               KEYWORD_SET(do_GEO_MAG_conversions) + KEYWORD_SET(do_AACGM_conversions) + $
               KEYWORD_SET(stitch_files)           + KEYWORD_SET(get_dipoleTilt_data )

  nCPUs    = !CPU.HW_NCPU-1
  
  oBridge        = OBJARR(nCPUs)
  IDLChildPref   = 'IDL_child_Carloers_'
  IDLChildDir    = '/SPENCEdata/Research/Satellites/FAST/kappa_dists/saves_output_etc/'
  IDLChildOutput = IDLChildDir + IDLChildPref + STRCOMPRESS(INDGEN(nCPUs),/REMOVE_ALL) + '.txt'


  ;; carloTimeArr = [['09:26:11','09:26:20'], $
  ;;                 ['09:26:21','09:26:30'], $
  ;;                 ['09:26:31','09:26:40'], $
  ;;                 ['09:26:41','09:26:50'], $
  ;;                 ['09:26:51','09:27:00'], $
  ;;                 ['09:27:01','09:27:11']]

  ;; Fixers from 2018/01/19 run
  ;; ;; carloTimeArr = [['09:26:41.669','09:26:41.669'], $
  ;; ;;                 ['09:26:42.301','09:26:42.301'], $
  ;; ;;                 ['09:26:42.933','09:26:42.933'], $
  ;; ;;                 ['09:26:43.565','09:26:43.565'], $
  ;; ;;                 ['09:26:45.461','09:26:45.461'], $
  ;; ;;                 ['09:26:47.357','09:26:47.357'], $
  ;; ;;                 ['09:26:48.621','09:26:48.621']]

  ;; carloTimeArr = [['09:26:42.301','09:26:42.301'], $
  ;;                 ['09:26:42.933','09:26:42.933'], $
  ;;                 ['09:26:45.461','09:26:45.461']]

  nCPUsToRun = N_ELEMENTS(carloTimeArr[0,*])

  IF nCPUsToRun GT nCPUs THEN STOP

  IF ~KEYWORD_SET(startCPU  ) THEN startCPU   = 0
  stopCPU = (N_ELEMENTS(stopCPU) GT 0 ? stopCPU : (startCPU + nCPUsToRun - 1) < (nCPUs - 1))

  ;;Show user before beginning
  PRINT,"Here's what I'm going to do: "
  FOR i=startCPU,stopCPU DO BEGIN
     PRINT,FORMAT='(A0,I0," : ",A0,"-",A0)',"CPU ",i, $
           carloTimeArr[0,i],carloTimeArr[1,i]
  ENDFOR

  PRINT,"Look OK?"
  response = ''
  cont     = 0
  WHILE ~cont DO BEGIN
     READ,response

     CASE 1 OF
        STRMATCH(STRUPCASE(response),'Y*'): BEGIN
           cont = 1
        END
        STRMATCH(STRUPCASE(response),'N*'): BEGIN
           cont = 1
           PRINT,"OK, leaving ..."
           RETURN
        END
        STRMATCH(STRUPCASE(response),'STOP*'): BEGIN
           cont = 1
           PRINT,"OK, stopping ..."
           STOP
        END
        ELSE: BEGIN
           PRINT,"No, you need to answer 'yes' or 'no' (or 'stop')"
        END
     ENDCASE
  ENDWHILE

  FOR i=startCPU,stopCPU DO BEGIN

     IF ~KEYWORD_SET(dry_run) THEN BEGIN

        oBridge[i] = OBJ_NEW('IDL_IDLBridge',OUTPUT=IDLChildOutput[i])

        ;;Set all the vars for this environment
        oBridge[i]->SetVar, '!PATH'             ,!PATH

        oBridge[i]->SetVar, 'carloTimeStart'    ,carloTimeArr[0,i]
        oBridge[i]->SetVar, 'carloTimeStop'     ,carloTimeArr[1,i]

        execStr = 'JOURNAL__20180117__BOOTSTRAP_ORB_1773_2D_DISTS_TO_GET_BESTFIT_PARAM_ERRORS,' + $
                  ;; 'CARLOTIMESTART="' +carloTimeArr[0,i]+'",' + $
                  ;; 'CARLOTIMESTOP="' +carloTimeArr[1,i]+'"'
                  'CARLOTIMESTART=carloTimeStart,' + $
                  'CARLOTIMESTOP=carloTimeStop'

           oBridge[i]->Execute,'.compile ' + mainRoutine

        FOR ll=0,N_ELEMENTS(routineArr1)-1 DO BEGIN
           oBridge[i]->Execute,'.compile ' + proDir1 + routineArr1[ll]
        ENDFOR

        oBridge[i]->Execute,execStr,/NOWAIT
        
        PRINT,'Started that homey'
     ENDIF

  ENDFOR

  IF ~KEYWORD_SET(dry_run) THEN BEGIN

     notdone    = 1
     count      = 0LL
     waiting    = MAKE_ARRAY(N_ELEMENTS(oBridge),VALUE=0B,/BYTE)
     retVal     = MAKE_ARRAY(N_ELEMENTS(oBridge),VALUE=0B,/BYTE)
     retString  = MAKE_ARRAY(N_ELEMENTS(oBridge),/STRING)

     waiting[startCPU:stopCPU] = 1B
     WHILE N_ELEMENTS(WHERE(waiting,/NULL)) GT 0 DO BEGIN
        ;; done = 0

        FOR i=startCPU,stopCPU DO BEGIN
           IF waiting[i] THEN BEGIN
              tmpStatus = oBridge[i]->Status(ERROR=retTmp)

              CASE tmpStatus OF
                 0: BEGIN
                    PRINT,"Processor " + STRCOMPRESS(i,/REMOVE_ALL) + " has finished"
                    waiting[i] = 0B
                 END
                 1: BEGIN
                    PRINT,"Processor " + STRCOMPRESS(i,/REMOVE_ALL) + " is executing:"
                    SPAWN,"tail -n 5 " + IDLChildOutput[i],tmpStat
                    PRINT,tmpStat
                 END
                 2: BEGIN
                    PRINT,"Processor " + STRCOMPRESS(i,/REMOVE_ALL) + " finished!"
                    retString[i] = 'Finished!'
                    waiting[i]   = 0B
                 END
                 3: BEGIN
                    PRINT,"Processor " + STRCOMPRESS(i,/REMOVE_ALL) + ": ERROR!"
                    retString[i] = retTmp
                    waiting[i]   = 0B
                 END
              ENDCASE

           ENDIF

           ;; done = done+tmpStatus
           
        ENDFOR

        WAIT,5.0

        count++
        IF (count MOD 120) EQ 0 THEN BEGIN
           PRINT,"Waiting .."
           PRINT,"nFinished = " + STRCOMPRESS(N_ELEMENTS(WHERE(~waiting,/NULL)),/REMOVE_ALL)
        ENDIF
     ENDWHILE

     FOR i=0,N_ELEMENTS(oBridge)-1 DO BEGIN
        OBJ_DESTROY,oBridge[N_ELEMENTS(oBridge)-1-i]
     ENDFOR

     PRINT,"DONE WITH EVERYTHANG "

  ENDIF

END