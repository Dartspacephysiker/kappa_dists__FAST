;2018/03/07
;; TPLOTs electron and ion data from DMSP F16 satellite file.
;;'Twas obtained from http://cedar.openmadrigal.org/showExperiment/?experiment_list=100125279
PRO JOURNAL__20180307__BADDELEY_ET_AL_2017__TPLOT, $
   ALSO_AACGM=also_AACGM, $
   PS=ps, $
   EPS=eps, $
   OUTPLOTNAME=outPlotName

  COMPILE_OPT IDL2,STRICTARRSUBS

  dir  = '/SPENCEdata/Research/database/DMSP/20071227/'
  file = 'dms_20071227_16e.001__Baddeley_et_al_2007.sav'

  ;; defOutPlotName = 'dmsp_f16_Baddeley_et_al_2017__fig7'
  ;; t1 = S2T('2007-12-27/16:00:00')
  ;; t2 = S2T('2007-12-27/16:10:00')

  defOutPlotName = 'dmsp_f16_Baddeley_et_al_2017__fig7__ZOOM'
  t1 = S2T('2007-12-27/16:04:00')
  t2 = S2T('2007-12-27/16:08:00')

  winXDim = 1200
  winYDim = 900

  el_dNFlux = 0
  el_dEFlux = 1
  ion_dNFlux = 0
  ion_dEFlux = 1

  IF FILE_TEST(dir+file) THEN BEGIN

     PRINT,"Restoring " + file + ' ...'
     RESTORE,dir+file

  ENDIF ELSE BEGIN

     PRINT,"USE JOURNAL__20180307__BADDELEY_ET_AL_2017__READ_HDF5_FILE__MAKE_IDL_SAV!"
     RETURN

  ENDELSE
  @/SPENCEdata/software/spedas/idl/general/tplot/tplot_com

  doBuff = KEYWORD_SET(ps) OR KEYWORD_SET(eps)
  IF KEYWORD_SET(outPlotName) THEN $
     IF ~doBuff THEN eps = 1

  IF (KEYWORD_SET(ps) OR KEYWORD_SET(eps)) AND ~KEYWORD_SET(outPlotName) THEN $
     outPlotName = defOutPlotName
  
  IF doBuff THEN BEGIN

     suff = KEYWORD_SET(eps) ? '.eps' : '.ps'

     filNavn = outPlotName + suff

     IF ~KEYWORD_SET(plotDir) THEN SET_PLOT_DIR,plotDir,/FOR_KAPPA_DB,/ADD_TODAY

     POPEN,plotDir+filNavn, $
           /LAND, $
           ;; /PORT, $
           ;; ASPECT=0.625, $
           FONT=-1, $
           ENCAPSULATED=eps

     DEVICE,/PALATINO,FONT_SIZE=13

  ENDIF

  STORE_DATA,'LAT',DATA={x: dmsp.time, y: dmsp.geo.lat}
  STORE_DATA,'LON',DATA={x: dmsp.time, y: dmsp.geo.lon}
  IF KEYWORD_SET(also_AACGM) THEN BEGIN
     STORE_DATA,'ALT',DATA={x: dmsp.time, y: dmsp.aacgm.alt}
     STORE_DATA,'MLT',DATA={x: dmsp.time, y: dmsp.aacgm.mlt}
     STORE_DATA,'MLAT',DATA={x: dmsp.time, y: dmsp.aacgm.lat}
  ENDIF ELSE BEGIN
     STORE_DATA,'ALT',DATA={x: dmsp.time, y: dmsp.alt}
     STORE_DATA,'MLT',DATA={x: dmsp.time, y: dmsp.mlt}
     STORE_DATA,'MLAT',DATA={x: dmsp.time, y: dmsp.mag.lat}
  ENDELSE

  IF KEYWORD_SET(el_dNFlux) THEN BEGIN

     data = {x: dmsp.time, y: dmsp.el_d_flux, v: dmsp.ch_energy}
     ;; data.y = ALOG10(data.y)

     var_name = 'el_d_flux'
     STORE_DATA,var_name,DATA=data

     OPTIONS,var_name,'spec',1	
     ;; zlim,var_name, $
     ;;      (MIN(data.y[WHERE(FINITE(data.y))]) > 1 ), $
     ;;      (MAX(data.y[WHERE(FINITE(data.y))]) < 8),0
     zlim,var_name, $
          (MIN(data.y[WHERE(FINITE(data.y))]) > 1e2 ), $
          (MAX(data.y[WHERE(FINITE(data.y))]) < 1e7),1
     ylim,var_name,30,30000,1
     OPTIONS,var_name,'ytitle','Electron!C!CEnergy (eV)'
     OPTIONS,var_name,'ztitle','Log #!C!C/cm!U2!N-s-sr-eV'
     OPTIONS,var_name,'x_no_interp',1
     OPTIONS,var_name,'y_no_interp',1
     OPTIONS,var_name,'panel_size',2
     IF (N_ELEMENTS(tPlt_vars) eq 0) THEN tPlt_vars=[var_name] else tPlt_vars=[tPlt_vars,var_name]

  ENDIF 
  IF KEYWORD_SET(el_dEFlux) THEN BEGIN

     ;; Energy flux

     data = {x: dmsp.time, y: dmsp.el_d_ener, v: dmsp.ch_energy}
     ;; data.y = ALOG10(data.y)

     var_name = 'el_d_ener'
     STORE_DATA,var_name,DATA=data

     OPTIONS,var_name,'spec',1	
     ;; zlim,var_name, $
     ;;      (MIN(data.y[WHERE(FINITE(data.y))]) > 1 ), $
     ;;      (MAX(data.y[WHERE(FINITE(data.y))]) < 8),0
     zlim,var_name, $
          (MIN(data.y[WHERE(FINITE(data.y))]) > 1e5 ), $
          (MAX(data.y[WHERE(FINITE(data.y))]) < 1e9),1
     ylim,var_name,30,30000,1
     OPTIONS,var_name,'ytitle','Electron!C!CEnergy (eV)'
     OPTIONS,var_name,'ztitle','Log eV!C!C/cm!U2!N-s-sr-eV'
     OPTIONS,var_name,'x_no_interp',1
     OPTIONS,var_name,'y_no_interp',1
     OPTIONS,var_name,'panel_size',2

     IF (N_ELEMENTS(tPlt_vars) eq 0) THEN tPlt_vars=[var_name] else tPlt_vars=[tPlt_vars,var_name]

  ENDIF 
  IF KEYWORD_SET(ion_dNFlux) THEN BEGIN

     data = {x: dmsp.time, y: dmsp.ion_d_flux, v: dmsp.ch_energy}
     ;; data.y = ALOG10(data.y)

     var_name = 'ion_d_flux'
     STORE_DATA,var_name,DATA=data

     OPTIONS,var_name,'spec',1	
     ;; zlim,var_name, $
     ;;      (MIN(data.y[WHERE(FINITE(data.y))]) > 1 ), $
     ;;      (MAX(data.y[WHERE(FINITE(data.y))]) < 8),0
     zlim,var_name, $
          (MIN(data.y[WHERE(FINITE(data.y))]) > 1e2 ), $
          (MAX(data.y[WHERE(FINITE(data.y))]) < 1e7),1
     ylim,var_name,30,30000,1
     OPTIONS,var_name,'ytitle','Ion!C!CEnergy (eV)'
     OPTIONS,var_name,'ztitle','Log #!C!C/cm!U2!N-s-sr-eV'
     OPTIONS,var_name,'x_no_interp',1
     OPTIONS,var_name,'y_no_interp',1
     OPTIONS,var_name,'panel_size',2
     IF (N_ELEMENTS(tPlt_vars) eq 0) THEN tPlt_vars=[var_name] else tPlt_vars=[tPlt_vars,var_name]

  ENDIF 
  IF KEYWORD_SET(ion_dEFlux) THEN BEGIN

     ;; Energy flux

     data = {x: dmsp.time, y: dmsp.ion_d_ener, v: dmsp.ch_energy}
     ;; data.y = ALOG10(data.y)

     var_name = 'ion_d_ener'
     STORE_DATA,var_name,DATA=data

     OPTIONS,var_name,'spec',1	
     ;; zlim,var_name, $
     ;;      (MIN(data.y[WHERE(FINITE(data.y))]) > 1 ), $
     ;;      (MAX(data.y[WHERE(FINITE(data.y))]) < 8),0
     zlim,var_name, $
          (MIN(data.y[WHERE(FINITE(data.y))]) > 1e5 ), $
          (MAX(data.y[WHERE(FINITE(data.y))]) < 1e9),1
     ylim,var_name,30,30000,1
     OPTIONS,var_name,'ytitle','Ion!C!CEnergy (eV)'
     OPTIONS,var_name,'ztitle','Log eV!C!C/cm!U2!N-s-sr-eV'
     OPTIONS,var_name,'x_no_interp',1
     OPTIONS,var_name,'y_no_interp',1
     OPTIONS,var_name,'panel_size',2

     IF (N_ELEMENTS(tPlt_vars) eq 0) THEN tPlt_vars=[var_name] else tPlt_vars=[tPlt_vars,var_name]

  ENDIF

  ;; STOP

  IF ~doBuff THEN BEGIN
     wInd = 0
     WINDOW,wInd,XSIZE=winXDim,YSIZE=winYDim
  ENDIF

  ;; tplot_OPTIONS,'region',[0.,0.5,1.0,1.0]
  loadct2,43
  TPLOT,REVERSE(tPlt_vars), $
        VAR=['MLT','MLAT','LON','LAT'], $
        WINDOW=doBuff ? !NULL : wInd, $
        TRANGE=[t1,t2]

  IF doBuff THEN BEGIN
     PCLOSE
  ENDIF

  STOP

END

