;; 2018/01/19
;; It woiks

PRO JOURNAL__20180406__BOOTSTRAP_ORB_1612_2D_DISTS_TO_GET_BESTFIT_PARAM_ERRORS__PARALLEL

  mainRoutine = '/SPENCEdata/Research/Satellites/FAST/kappa_dists/journals/journal__20180406__bootstrap_orb_1612_2d_dists_to_get_bestfit_param_errors.pro'

  routineName = 'JOURNAL__20180406__BOOTSTRAP_ORB_1612_2D_DISTS_TO_GET_BESTFIT_PARAM_ERRORS'

  proDir1     = '/SPENCEdata/Research/Satellites/FAST/kappa_dists/'
  routineArr1 = STRLOWCASE( $
                ['KAPPA_FIT2D__MONTECARLO_UNCERTAINTY', $
                 'KAPPA_FIT2D__MONTECARLO__1DINIT']) + '.pro'

  dir = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/'
  fil = '20180405-orb_1612-Kappa_fits_and_Gauss_fits-ees-horseshoe2d-AUTO-only_fit_peak_eRange-avg_itvl2.sav'

  diff_eFlux_dir = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/diff_eFlux/'
  diff_eFlux_fil = 'orb_1612-diff_eflux-ees-avg_itvl2-12_00_04__000-12_02_07__000.sav'

  orbit = 1612
  orbString = STRING(FORMAT='(I0)',orbit)

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

  ;; carloTimeArr  = [['12:00:24','12:00:33.999'], $
  ;;                  ['12:00:34','12:00:43.999'], $
  ;;                  ['12:00:44','12:00:53.999'], $
  ;;                  ['12:00:54','12:01:03.999'], $
  ;;                  ['12:01:04','12:01:13.999'], $
  ;;                  ['12:01:14','12:01:23.999'], $
  ;;                  ['12:01:24','12:01:33.999'], $
  ;;                  ['12:01:34','12:01:47']]

  ;; carloTimeArr  = '1997-01-17/' + [['12:00:24','12:00:35.999'], $
  ;;                                  ['12:00:36','12:00:47.999'], $
  ;;                                  ['12:00:48','12:00:59.999'], $
  ;;                                  ['12:01:00','12:01:11.999'], $
  ;;                                  ['12:01:12','12:01:23.999'], $
  ;;                                  ['12:01:24','12:01:35.999'], $
  ;;                                  ['12:01:36','12:01:48']]

  carloTimeArr  = '1997-01-17/' + [['12:00:24','12:00:36'], $
                                   ['12:00:36','12:00:48'], $
                                   ['12:00:48','12:01:00'], $
                                   ['12:01:00','12:01:12'], $
                                   ['12:01:12','12:01:24'], $
                                   ['12:01:24','12:01:36'], $
                                   ['12:01:36','12:01:48']]


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

        execStr = routineName + ',' + $
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