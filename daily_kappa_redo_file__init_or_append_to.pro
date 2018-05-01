;2018/05/01
PRO DAILY_KAPPA_REDO_FILE__INIT_OR_APPEND_TO,orbit,datestring

  COMPILE_OPT IDL2,STRICTARRSUBS

  dir = '/SPENCEdata/Research/Satellites/FAST/kappa_dists/txtOutput/'
  pref = 'dailyKappaRedoFile-'
  suff = '.txt'

  IF ~KEYWORD_SET(datestring) THEN dateString = GET_TODAY_STRING(/DO_YYYYMMDD_FMT)

  OPENW,lun,dir+pref+dateString+suff, $
        APPEND=FILE_TEST(dir+pref+dateString+suff), $
        /GET_LUN

  PRINTF,lun,FORMAT='(I05)', $
         orbit

  CLOSE,lun
  FREE_LUN,lun

END
