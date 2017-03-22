;2017/03/22
PRO ADD_FNAME_SUFF,fName,suff

  COMPILE_OPT IDL2,STRICTARRSUBS

        fNameTmp     = STRSPLIT(fName,'.',/EXTRACT)
        fNameTmp[0] += suff
        fName        = STRJOIN(TEMPORARY(fNameTmp),'.')

END

