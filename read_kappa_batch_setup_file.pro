;2018/03/02
PRO RKAPPA__PRINTSUMMARY,dir,file,orbit,count,tmpOrb,MLT,ILAT,ALT,t1Str,t2Str,t_streakLen,nPts,dt_avg,avg_current
           SPAWN,"wc -l " + dir+file + " | awk '{ print $1 }'",lineCount
           lineCount = LONG(lineCount)-1
           PRINT,'============================================================'
           PRINT,FORMAT='("INFO FOR ORBIT ",I0, " (entry #",I0,"/",I0,")")', $
                 orbit,count,lineCount
           PRINT,'============================================================'
           PRINT,FORMAT='(A5,T7,A4,T13,A4,T19,A4,T25,A19,T46,A8,T56,A8,T66,A5,T73,A6,T81,A8)', $
                 'Orbit', $
                 'MLT', $
                 'ILAT', $
                 'Alt', $
                 'Start T', $
                 'Stop T', $
                 'Len (s)', $
                 'N pts', $
                 'Avg dt', $
                 'Current'
           PRINT,FORMAT='(I05,T7,F04.1,T12,F05.1,T19,I4,T25,A19,T46,A8,T56,G-8.5,T66,I-5,T73,G-6.3,T81,G-8.4)', $
                 tmpOrb,MLT,ILAT,ALT,t1Str,STRMID(t2Str,11,8),t_streakLen,nPts,dt_avg,avg_current

END
PRO READ_KAPPA_BATCH_SETUP_FILE, $
   orbit,MLT,ILAT,ALT,t1Str,t2Str,t_streakLen,nPts,dt_avg,avg_current, $
   NTOSKIP=nToSkip, $
   DATE_OF_GENERATION=date, $
   MLTRANGE=mltRange, $
   ALTRANGE=altRange, $
   MIN_T_STREAKLEN=min_T_streakLen, $
   PRINT_SUMMARY=print_summary

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; Catch EOF errors
  ;; CATCH, Error_status
  ;; ;;This statement begins the error handler:
  ;; IF Error_status NE 0 THEN BEGIN
  ;;    PRINT, 'Error index: ', Error_status
  ;;    PRINT, 'Error message: ', !ERROR_STATE.MSG
  ;;                               ; Handle the error by extending A:
  ;;    ;; A=FLTARR(12)
  ;;    CATCH, /CANCEL
  ;; ENDIF

  skipRem = KEYWORD_SET(nToSkip)? nToSkip : 0

  IF N_ELEMENTS(orbit) EQ 0 THEN BEGIN
     PRINT,"Bogus!"
     RETURN
  ENDIF

  mltR = KEYWORD_SET(mltRange) ? mltRange : [-5,5]
  orbR = KEYWORD_SET(orbRange) ? orbRange : [1000,16000]
  altR = KEYWORD_SET(altRange) ? altRange : [300,4300]

  IF N_ELEMENTS(min_T_streakLen) EQ 0 THEN BEGIN
     min_T_streakLen = 60       ;in seconds
  ENDIF

  mltStr  = STRING(FORMAT='("__",I0,"-",I0,"MLT")',mltR[0],mltR[1])
  ;; orbStr   = STRING(FORMAT='("__",I0,"-",I0,"ORB")',orbR[0],orbR[1])
  altStr   = STRING(FORMAT='("__",I0,"-",I0,"ALT")',altR[0],altR[1])
  min_TStr = STRING(FORMAT='("__minTStreak_sec_",I0)',min_T_streakLen)


  filsDate = KEYWORD_SET(date) ? date : '20180302'

  dir = '/SPENCEdata/Research/Satellites/FAST/espec_identification/txtOutput/'
  fPref = filsDate + '--mono_aurora_streaks' + mltStr
  ;; Which orb range?
  startOrb = 1000L
  endOrb   = 3999L
  cont = 0
  WHILE 1 DO BEGIN
     cont = (orbit GE startOrb) AND (orbit LE endOrb)
     IF cont THEN BREAK
     startOrb += 3000
     endOrb   += 3000
  ENDWHILE
  orbStr       = STRING(FORMAT='("__",I0,"-",I0,"ORB")',startOrb,endOrb)
  fSuff = altStr + min_TStr + '.txt'

  file = fPref+orbStr+fSuff
  IF ~FILE_TEST(dir+file) THEN BEGIN
     PRINT,"NO"
     STOP
  ENDIF
  OPENR,lun,dir+file, /GET_LUN
  t1Str = ''
  t2Str = ''

  cont = 0
  junkLine = ''
  READF,lun,junkLine
  tmpOrb = 0L
  count  = 0
  WHILE ~cont AND ~EOF(lun) DO BEGIN
     READF,lun,FORMAT='(I05,T7,F04.1,T12,F05.1,T19,I4,T25,A19,T46,A8,T56,G-8.5,T66,I-5,T73,G-6.3,T81,G-0.5)', $
           tmpOrb,MLT,ILAT,ALT,t1Str,t2Str,t_streakLen,nPts,dt_avg,avg_current

     ;; PRINT,tmpOrb
     IF tmpOrb EQ orbit THEN BEGIN
        cont = (skipRem EQ 0)
        skipRem--

        lasttmpOrb       = tmpOrb
        LASTMLT          = MLT
        LASTILAT         = ILAT
        LASTALT          = ALT
        lastt1Str        = t1Str
        lastt2Str        = t2Str
        lastt_streakLen  = t_streakLen
        lastnPts         = nPts
        lastdt_avg       = dt_avg
        lastavg_current  = avg_current
        lastcount        = count

        IF KEYWORD_SET(print_summary) THEN BEGIN
           RKAPPA__PRINTSUMMARY, $
              dir,file,orbit,count,tmpOrb,MLT,ILAT,ALT,t1Str,t2Str,t_streakLen,nPts,dt_avg,avg_current
        ENDIF

     ENDIF

     count++
     
  ENDWHILE

  IF EOF(lun) THEN BEGIN

     tmpOrb       = lasttmpOrb
     MLT          = LASTMLT
     ILAT         = LASTILAT
     ALT          = LASTALT
     t1Str        = lastt1Str
     t2Str        = lastt2Str
     t_streakLen  = lastt_streakLen
     nPts         = lastnPts
     dt_avg       = lastdt_avg
     avg_current  = lastavg_current
     count        = lastcount

     PRINT,"Reached end of file, so using thesen's:"

     RKAPPA__PRINTSUMMARY, $
        dir,file,orbit,count,tmpOrb,MLT,ILAT,ALT,t1Str,t2Str,t_streakLen,nPts,dt_avg,avg_current
  ENDIF

  date = STRMID(t1Str,0,11)
  t2Str = date + t2Str

  CLOSE,lun

END
