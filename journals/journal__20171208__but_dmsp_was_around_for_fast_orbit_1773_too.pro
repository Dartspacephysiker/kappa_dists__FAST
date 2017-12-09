;2017/12/08
;; Reads DMSP F12 satellite file obtained from http://cedar.openmadrigal.org/showExperiment/?experiment_list=100125279
;;timestamps:   DOUBLE(86280) = DOUBLE(timestamps)
;;   0  units: Unix seconds
;;   1  description: Number of seconds since UT midnight 1970-01-01
;;ch_energy:    DOUBLE(19) = DOUBLE(ch_energy)
;;   0  units: eV
;;   1  description: Channel central energy
;;el_i_ener:    DOUBLE(86280) = DOUBLE(timestamps)
;;   0  units: enFlux
;;   1  description: Integr elect energy flux (eV/cm2s*ster)
;;el_i_flux:    DOUBLE(86280) = DOUBLE(timestamps)
;;   0  units: numFlux
;;   1  description: Integrated elect num flux (1/cm2s*ster)
;;el_m_ener:    DOUBLE(86280) = DOUBLE(timestamps)
;;   0  units: eV
;;   1  description: Mean electron energy
;;gdalt:        DOUBLE(86280) = DOUBLE(timestamps)
;;   0  units: km
;;   1  description: Geodetic altitude (height)
;;gdlat:        DOUBLE(86280) = DOUBLE(timestamps)
;;   0  units: deg
;;   1  description: Geodetic latitude of measurement
;;glon:         DOUBLE(86280) = DOUBLE(timestamps)
;;   0  units: deg
;;   1  description: Geographic longitude of measurement
;;ion_i_ener:   DOUBLE(86280) = DOUBLE(timestamps)
;;   0  units: enFlux
;;   1  description: Integr ion energy flux (eV/cm2s*ster)
;;ion_i_flux:   DOUBLE(86280) = DOUBLE(timestamps)
;;   0  units: numFlux
;;   1  description: Integrated ion num flux (1/cm2s*ster)
;;ion_m_ener:   DOUBLE(86280) = DOUBLE(timestamps)
;;   0  units: eV
;;   1  description: Mean ion energy
;;mlat:         DOUBLE(86280) = DOUBLE(timestamps)
;;   0  units: deg
;;   1  description: Magnetic latitude
;;mlong:        DOUBLE(86280) = DOUBLE(timestamps)
;;   0  units: deg
;;   1  description: Magnetic Longitude
;;mlt:          DOUBLE(86280) = DOUBLE(timestamps)
;;   0  units: hour
;;   1  description: Magnetic local time
;;sat_id:       LONG64(86280) = LONG64(timestamps)
;;   0  units: N/A
;;   1  description: Satellite id
;;ch_ctrl_ener: DOUBLE(19,86280) = DOUBLE(ch_energy,timestamps)
;;   0  units: eV
;;   1  description: Channel spacing energy
;;el_d_ener:    DOUBLE(19,86280) = DOUBLE(ch_energy,timestamps)
;;   0  units: enFlux
;;   1  description: Diff electron energy flux (1/cm2s*ster)
;;el_d_flux:    DOUBLE(19,86280) = DOUBLE(ch_energy,timestamps)
;;   0  units: numFlux
;;   1  description: Diff electron num flux (1/cm2eVs*ster)
;;ion_d_ener:   DOUBLE(19,86280) = DOUBLE(ch_energy,timestamps)
;;   0  units: enFlux
;;   1  description: Diff ion energy flux (1/cm2s*ster)
;;ion_d_flux:   DOUBLE(19,86280) = DOUBLE(ch_energy,timestamps)
;;   0  units: numFlux
;;   1  description: Diff ion num flux (1/cm2eVs*ster)
PRO JOURNAL__20171208__BUT_DMSP_WAS_AROUND_FOR_FAST_ORBIT_1773_TOO

  COMPILE_OPT IDL2,STRICTARRSUBS

  dir  = '/SPENCEdata/Research/database/DMSP/19970201/'

  ;; file = 'dms_19970201_12e.001.nc'
  ;; outFile = 'dms_19970201_12e.001.sav'

  file = 'dms_19970201_13e.001.nc'
  outFile = 'dms_19970201_13e.001.sav'

  IF FILE_TEST(dir+outFile) THEN BEGIN

     PRINT,"Restoring " + outFile + ' ...'
     RESTORE,dir+outFile

  ENDIF ELSE BEGIN

     NCDF_LIST, dir+file,/VARIABLES,/DIMENSIONS,/GATT,/VATT

     NCDF_GET,dir+file,'mlt',result
     mlt = (result['mlt'])['value']

     NCDF_GET,dir+file,'timestamps',result
     timestamps = (result['timestamps'])['value']

     NCDF_GET,dir+file,'ch_energy',result
     ch_energy = (result['ch_energy'])['value']

     NCDF_GET,dir+file,'el_i_ener',result
     el_i_ener = (result['el_i_ener'])['value']

     NCDF_GET,dir+file,'el_i_flux',result
     el_i_flux = (result['el_i_flux'])['value']

     NCDF_GET,dir+file,'el_m_ener',result
     el_m_ener = (result['el_m_ener'])['value']

     NCDF_GET,dir+file,'gdalt',result
     gdalt = (result['gdalt'])['value']

     NCDF_GET,dir+file,'gdlat',result
     gdlat = (result['gdlat'])['value']

     NCDF_GET,dir+file,'glon',result
     glon = (result['glon'])['value']

     NCDF_GET,dir+file,'ion_i_ener',result
     ion_i_ener = (result['ion_i_ener'])['value']

     NCDF_GET,dir+file,'ion_i_flux',result
     ion_i_flux = (result['ion_i_flux'])['value']

     NCDF_GET,dir+file,'ion_m_ener',result
     ion_m_ener = (result['ion_m_ener'])['value']

     NCDF_GET,dir+file,'mlat',result
     mlat = (result['mlat'])['value']

     NCDF_GET,dir+file,'mlong',result
     mlong = (result['mlong'])['value']

     NCDF_GET,dir+file,'mlt',result
     mlt = (result['mlt'])['value']

     NCDF_GET,dir+file,'sat_id',result
     sat_id = (result['sat_id'])['value']

     NCDF_GET,dir+file,'ch_ctrl_ener',result
     ch_ctrl_ener = (result['ch_ctrl_ener'])['value']

     NCDF_GET,dir+file,'el_d_ener',result
     el_d_ener = (result['el_d_ener'])['value']

     NCDF_GET,dir+file,'el_d_flux',result
     el_d_flux = (result['el_d_flux'])['value']

     NCDF_GET,dir+file,'ion_d_ener',result
     ion_d_ener = (result['ion_d_ener'])['value']

     NCDF_GET,dir+file,'ion_d_flux',result
     ion_d_flux = (result['ion_d_flux'])['value']

     nTime = N_ELEMENTS(timestamps)
     ch_energy = ch_energy # MAKE_ARRAY(nTime,/FLOAT,VALUE=1.)
     dmsp = {time          : TEMPORARY(timestamps)   , $        ; Unix seconds, Number of seconds since UT midnight 1970-01-01
             mlt           : TEMPORARY(mlt)          , $        ; hour, Magnetic local time
             alt           : TEMPORARY(gdalt)        , $        ; km, Geodetic altitude (height)
             mag           : {lat          : TEMPORARY(mlat) , $ ; deg, Magnetic latitude
                              lon          : TEMPORARY(mlong) }, $ ; deg, Magnetic Longitude
             geo           : {lat         : TEMPORARY(gdlat) , $   ; deg, Geodetic latitude of measurement
                              lon         : TEMPORARY(glon)   }, $ ; deg, Geographic longitude of measurement
             el_i_ener     : TEMPORARY(el_i_ener)    , $           ; enFlux, Integr elect energy flux (eV/cm2s*ster)
             el_i_flux     : TEMPORARY(el_i_flux)    , $           ; numFlux, Integrated elect num flux (1/cm2s*ster)
             el_d_ener     : TEMPORARY(el_d_ener)    , $           ; enFlux, Diff electron energy flux (1/cm2s*ster)
             el_d_flux     : TEMPORARY(el_d_flux)    , $           ; numFlux, Diff electron num flux (1/cm2eVs*ster)
             el_m_ener     : TEMPORARY(el_m_ener)    , $           ; eV, Mean electron energy
             ion_i_ener    : TEMPORARY(ion_i_ener)   , $           ; enFlux, Integr ion energy flux (eV/cm2s*ster)
             ion_i_flux    : TEMPORARY(ion_i_flux)   , $           ; numFlux, Integrated ion num flux (1/cm2s*ster)
             ion_d_ener    : TEMPORARY(ion_d_ener)   , $           ; enFlux, Diff ion energy flux (1/cm2s*ster)
             ion_d_flux    : TEMPORARY(ion_d_flux)   , $           ; numFlux, Diff ion num flux (1/cm2eVs*ster)
             ion_m_ener    : TEMPORARY(ion_m_ener)   , $           ; eV, Mean ion energy
             ch_energy     : TEMPORARY(ch_energy)    , $           ; eV, Channel spacing energy
             ch_ctrl_ener  : TEMPORARY(ch_ctrl_ener) , $           ; eV, Channel central energy
             sat_id        : TEMPORARY(sat_id)       }             ; sat_id:

     PRINT,"Saving DMSP struct to " + outFile + ' ...'
     SAVE,dmsp,FILENAME=dir+outFile

  ENDELSE
  @tplot_com

  STORE_DATA,'ALT',DATA={x: dmsp.time, y: dmsp.alt}
  STORE_DATA,'MLT',DATA={x: dmsp.time, y: dmsp.mlt}
  STORE_DATA,'MLAT',DATA={x: dmsp.time, y: dmsp.mag.lat}

  data = {x: dmsp.time, y: TRANSPOSE(dmsp.el_d_flux), v: TRANSPOSE(dmsp.ch_energy)}
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


  ;; Energy flux

  data = {x: dmsp.time, y: TRANSPOSE(dmsp.el_d_ener), v: TRANSPOSE(dmsp.ch_energy)}
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

  t1 = S2T('1997-02-01/09:19:00')
  t2 = S2T('1997-02-01/09:32:00')

  ;; STOP

  IF (N_ELEMENTS(tPlt_vars) eq 0) THEN tPlt_vars=[var_name] else tPlt_vars=[tPlt_vars,var_name]

  wInd = 0
  WINDOW,wInd,XSIZE=1200,YSIZE=700
  ;; tplot_OPTIONS,'region',[0.,0.5,1.0,1.0]
  loadct2,43
  tplot,tPlt_vars,var=['ALT','MLAT','MLT'], $
        WINDOW=wInd, $
        TRANGE=[t1,t2]

  STOP

END
