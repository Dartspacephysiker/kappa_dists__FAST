;2018/05/01
PRO DAILY_KAPPA_REDO_FILE__READ,orbit,dateString, $
                                OUT_NTOSKIP=nToSkip

  COMPILE_OPT IDL2,STRICTARRSUBS

  dir = '/SPENCEdata/Research/Satellites/FAST/kappa_dists/txtOutput/'
  pref = 'dailyKappaRedoFile-'
  suff = '.txt'

  nToSkip = 0

  IF ~KEYWORD_SET(dateString) THEN dateString = GET_TODAY_STRING(/DO_YYYYMMDD_FMT)

  IF ~FILE_TEST(dir+pref+dateString+suff) THEN BEGIN
     ;; PRINT,"File doesn't exist! Will make"
     RETURN
  ENDIF

  OPENR,lun,dir+pref+dateString+suff,/GET_LUN

  WHILE ~EOF(lun) DO BEGIN
     READF,lun,FORMAT='(I05)', $
           tmpOrb

     IF tmpOrb EQ orbit THEN BEGIN
        nToSkip += 1
     ENDIF

  ENDWHILE

  ;; IF EOF(lun) THEN BEGIN

  ;; ENDIF

  CLOSE,lun

END
