;2018/02/09
PRO JOURNAL__20180209__READ_LIEMOHN_AND_KAPPA_DENSFAC_TXTS__FROM_MATHEMATICA

  COMPILE_OPT IDL2,STRICTARRSUBS

  dir = '/SPENCEdata/Research/Satellites/FAST/kappa_dists/saves_output_etc/'
  pref = 'kappaLandKFacs-TFAST_eq_110_0-nFAST_eq_1_88--'
  suff = '.txt'
  nTot = 131

  T      = 110
  potBar = 941

  outFile = 'kappaLandKFacs-TFAST_eq_110_0-nFAST_eq_1_88.sav'

  ASCIITmpltFil = 'kappaLandKFacs__ASCII_tmplt.sav'

  tmpFile   = STRING(FORMAT='(A0,I0,"_of_",I0,A0)',pref,1,nTot,suff)
  IF FILE_TEST(dir+ASCIITmpltFil) THEN BEGIN
     RESTORE,dir+ASCIITmpltFil
  ENDIF ELSE BEGIN

     tmplt = ASCII_TEMPLATE(dir+fil)

     PRINT,"Saving ASCII tmplt ..."
     SAVE,tmplt,FILENAME=dir+ASCIITmpltFil

  ENDELSE

  ;; Get some infos
  tmpStruct = READ_ASCII(dir+tmpFile,TEMPLATE=tmplt)

  nRB = N_ELEMENTS(tmpStruct.RB)

  ;; Make the final struct
  LK = {kappa : MAKE_ARRAY(nTot,/DOUBLE), $
                 rb    : tmpStruct.RB, $
        ratio : MAKE_ARRAY(nTot,nRB,/DOUBLE), $
        assumes : {T: T ,$
                   potAbove: potBar}}

  ;; Read all files
  FOR k=1,131 DO BEGIN

     tmpFile = STRING(FORMAT='(A0,I0,"_of_",I0,A0)',pref,k,nTot,suff)

     IF ~FILE_TEST(dir+tmpFile) THEN STOP

     PRINT,tmpFile

     tmpStruct = READ_ASCII(dir+tmpFile,TEMPLATE=tmplt)

     LK.kappa[k-1]   = tmpStruct.kappa[0]
     LK.ratio[k-1,*] = tmpStruct.ratio

  ENDFOR

  PRINT,"Done!"
  PRINT,"Saving to " + outFile + ' ...'
  
  SAVE,LK,FILENAME=dir+outFile

  STOP

END

