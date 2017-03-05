;;2017/03/04
PRO SHRINK_GERSHMAN_ERROR_STRUCT,errors,inds,errorOut, $
                                 ADD_TO_ERROROUT=add

  COMPILE_OPT IDL2,STRICTARRSUBS

  IF N_ELEMENTS(inds) EQ 0 THEN inds = LINDGEN(N_ELEMENTS(errors.N))

  IF KEYWORD_SET(add) AND (N_ELEMENTS(errorOut) GT 0) THEN BEGIN

     errorOut = {N   : [errorOut.N,errors.N[inds]]   , $
                 Ux  : [errorOut.Ux,errors.Ux[inds]]  , $
                 Uy  : [errorOut.Uy,errors.Uy[inds]]  , $
                 Uz  : [errorOut.Uz,errors.Uz[inds]]  , $
                 Pxx : [errorOut.Pxx,errors.Pxx[inds]] , $
                 Pyy : [errorOut.Pyy,errors.Pyy[inds]] , $
                 Pzz : [errorOut.Pzz,errors.Pzz[inds]] , $
                 Pxy : [errorOut.Pxy,errors.Pxy[inds]] , $
                 Pxz : [errorOut.Pxz,errors.Pxz[inds]] , $
                 Pyz : [errorOut.Pyz,errors.Pyz[inds]] , $
                 Hx  : [errorOut.Hx,errors.Hx[inds]]  , $
                 Hy  : [errorOut.Hy,errors.Hy[inds]]  , $
                 Hz  : [errorOut.Hz,errors.Hz[inds]]  , $
                 R   : [[[errorOut.R]],[errors.R[inds,*,*]]]}
     RETURN

  ENDIF

  errorOut = {N   : errors.N[inds]   , $
              Ux  : errors.Ux[inds]  , $
              Uy  : errors.Uy[inds]  , $
              Uz  : errors.Uz[inds]  , $
              Pxx : errors.Pxx[inds] , $
              Pyy : errors.Pyy[inds] , $
              Pzz : errors.Pzz[inds] , $
              Pxy : errors.Pxy[inds] , $
              Pxz : errors.Pxz[inds] , $
              Pyz : errors.Pyz[inds] , $
              Hx  : errors.Hx[inds]  , $
              Hy  : errors.Hy[inds]  , $
              Hz  : errors.Hz[inds]  , $
              R   : errors.R[inds,*,*]}

END
