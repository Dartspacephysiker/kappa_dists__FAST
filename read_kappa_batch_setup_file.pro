;2018/03/02
PRO READ_KAPPA_BATCH_SETUP_FILE,orbit,MLT,ALT,t1Str,t2Str,t_streakLen,nPts,dt_avg,avg_current

  COMPILE_OPT IDL2,STRICTARRSUBS

  IF N_ELEMENTS(orbit) EQ 0 THEN BEGIN
     PRINT,"Bogus!"
     RETURN
  ENDIF

  dir = '/SPENCEdata/Research/Satellites/FAST/espec_identification/txtOutput/'
  fPref = '20180302--mono_aurora_streaks__-5-5MLT'
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
  fSuff = '__300-4300ALT__minTStreak_sec_60.txt'

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
  READF,lun,junkLine
  tmpOrb = 0L
  WHILE ~cont DO BEGIN
     READF,lun,FORMAT='(I05,T7,F04.1,T12,F05.1,T19,I4,T25,A19,T46,A8,T56,G-8.5,T66,I-5,T73,G-6.3,T81,G-0.5)', $
           tmpOrb,MLT,ILAT,ALT,t1Str,t2Str,t_streakLen,nPts,dt_avg,avg_current

     PRINT,tmpOrb
     cont = tmpOrb EQ orbit
  ENDWHILE

  date = STRMID(t1Str,0,11)
  t2Str = date + t2Str

  CLOSE,lun

END
