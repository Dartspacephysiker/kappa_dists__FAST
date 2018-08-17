;; 2018/08/08
;; 'Nother 'nother
PRO JOURNAL__20180808__BOOTSTRAP_ORB_1607_2D_DISTS_TO_GET_BESTFIT_PARAM_ERRORS__PARALLEL

  mainRoutine = '/SPENCEdata/Research/Satellites/FAST/kappa_dists/journals/journal__20180808__bootstrap_orb_1607_2d_dists_to_get_bestfit_param_errors.pro'

  routineName = 'JOURNAL__20180808__BOOTSTRAP_ORB_1607_2D_DISTS_TO_GET_BESTFIT_PARAM_ERRORS'

  check_for_and_only_do_baddies = 0
  skip_existing = 1
  dir_to_check = "/SPENCEdata/Research/Satellites/FAST/kappa_dists/saves_output_etc/20180817/"

  proDir1     = '/SPENCEdata/Research/Satellites/FAST/kappa_dists/'
  routineArr1 = STRLOWCASE( $
                ['KAPPA_FIT2D__MONTECARLO_UNCERTAINTY', $
                 'KAPPA_FIT2D__MONTECARLO__1DINIT']) + '.pro'

  ;; sRate = 0.63
  sRate = 1.89
  avgItvlStr = (STRING(FORMAT='("-sRate",F4.2)',sRate)).Replace(".","_")

  dir = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/'
  CASE sRate OF
     0.63: BEGIN
        fil = '20180817-orb_1607-KandGfits-ees-2NDKAPPA-only_fit_peak_eRange-sRate0_63-01_04_20__500-01_05_54__000.sav'
        diff_eFlux_fil = 'orb_1607-diff_eflux-ees-sRate0_63-01_04_20__500-01_05_54__000.sav'
     END
     1.89: BEGIN
        fil = '20180817-orb_1607-KandGfits-ees-2NDKAPPA-only_fit_peak_eRange-sRate1_89-01_03_53__988-01_06_15__000.sav'
        diff_eFlux_fil = 'orb_1607-diff_eflux-ees-sRate1_89-01_03_53__988-01_06_15__000.sav'
     END
  ENDCASE

  diff_eFlux_dir = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/diff_eFlux/'

  orbit = 1607
  orbString = STRING(FORMAT='(I0)',orbit)

  nCPUs    = !CPU.HW_NCPU-1
  
  oBridge        = OBJARR(nCPUs)
  IDLChildPref   = 'IDL_child_Carloers_'
  IDLChildDir    = '/SPENCEdata/Research/Satellites/FAST/kappa_dists/saves_output_etc/'
  IDLChildOutput = IDLChildDir + IDLChildPref + STRCOMPRESS(INDGEN(nCPUs),/REMOVE_ALL) + '.txt'

  ;; 7 processors to work with
  ;; an interval of 06m15s-03m50s = 2m25s = 145s
  ;; 20*2+21*5 = 145s
  ;; carloTimeArr  = '1997-01-17/' + [['01:03:50','01:04:10'], $
  ;;                                  ['01:04:10','01:04:30'], $
  ;;                                  ['01:04:30','01:04:51'], $
  ;;                                  ['01:04:51','01:05:12'], $
  ;;                                  ['01:05:12','01:05:33'], $
  ;;                                  ['01:05:33','01:05:54'], $
  ;;                                  ['01:05:54','01:06:15']]

  ;; carloTimeArr  = '1997-01-17/' + [['01:04:28','01:04:30'], $
  ;;                                  ['01:04:30','01:04:32'], $
  ;;                                  ['01:04:32','01:04:34'], $
  ;;                                  ['01:04:34','01:04:36'], $
  ;;                                  ['01:04:36','01:04:38'], $
  ;;                                  ['01:04:38','01:04:40'], $
  ;;                                  ['01:04:40','01:04:42']]

  ;; carloTimeArr  = '1997-01-17/' + [['01:04:38','01:04:39'], $
  ;;                                  ['01:04:39','01:04:40'], $
  ;;                                  ['01:04:40','01:04:41'], $
  ;;                                  ['01:04:41','01:04:42']]

  carloTimeArr  = '1997-01-17/' + [['01:04:20','01:04:46'], $
                                   ['01:04:46','01:04:57'], $
                                   ['01:04:57','01:05:08'], $
                                   ['01:05:08','01:05:19'], $
                                   ['01:05:19','01:05:30'], $
                                   ['01:05:30','01:05:43'], $
                                   ['01:05:43','01:05:54']]

  ;; Do some quick catchup
  ;; carloTimeArr  = '1997-01-17/' + [['01:04:06','01:04:07'], $
  ;;                                  ['01:04:08','01:04:09']]

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
        oBridge[i]->SetVar, 'sRate'             ,sRate

        execStr = routineName + ',' + $
                  ;; 'CARLOTIMESTART="' +carloTimeArr[0,i]+'",' + $
                  ;; 'CARLOTIMESTOP="' +carloTimeArr[1,i]+'"'
                  'CARLOTIMESTART=carloTimeStart,' + $
                  'CARLOTIMESTOP=carloTimeStop,' + $
                  'IN_SRATE=sRate'

        IF KEYWORD_SET(skip_existing) THEN BEGIN
           execStr += ',/SKIP_EXISTING'
        ENDIF

        IF KEYWORD_SET(check_for_and_only_do_baddies) THEN BEGIN
           oBridge[i]->SetVar,'dir_to_check', dir_to_check
           execStr += ',/CHECK_FOR_AND_ONLY_DO_BADDIES,' + $
                      'DIR_TO_CHECK=dir_to_check'
        ENDIF

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