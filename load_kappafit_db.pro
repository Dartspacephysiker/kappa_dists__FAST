;2018/05/02
PRO LOAD_KAPPAFIT_DB,andre, $
                     KF2DPARMS=KF2DParms, $
                     GF2DPARMS=GF2DParms, $
                     TOTT=totT, $
                     ORBARR=orbArr

  COMPILE_OPT IDL2,STRICTARRSUBS

  dir   = '/SPENCEdata/Research/Satellites/FAST/kappa_dists/saves_output_etc/'

  ;; restoreD = GET_TODAY_STRING(/DO_YYYYMMDD_FMT)
  ;; restoreD = '20180420' ;2018/04/23 Making a new one, calling it 20180424 for kicks
  ;; restoreD = '20180426'
  ;; restoreD = '20180427TRY3' ;Later, with orbs reaching into 7000+
  ;; restoreD = '20180427TRY4' 
  ;; restoreD = '20180427TRY5' 
  restoreD = '20180501' 

  fName = restoreD+'-parsedKappa.sav'

  IF ~FILE_TEST(dir+fName) THEN BEGIN
     PRINT,"Couldn't find " + fName + '!'
     STOP
  ENDIF

  PRINT,"Loading " + fName + ' ...'
  RESTORE,dir+fName

END

