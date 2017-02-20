;;02/11/17
PRO JOURNAL__20170210__ORB_1773__JUST_LOOK_AT_EN_SPEC_IN_ESPECDB_DETRITUS

  COMPILE_OPT IDL2

  ;; detritDir = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/do_the_Newell_2009/Newell_batch_output/ions_included/'
  ;; detritus  = 'Newell_et_al_identification_of_electron_spectra--ions_included--Orbit_1773_0.sav'

  diffEFDir = '~/software/sdt/batch_jobs/saves_output_etc/'
  diffEFFil = 'orb_1773-diff_eflux-ees-e_angle_-24.0-24.0--classics--3--Elphic_et_al_1998-fit_above_500_eV-No_bFunc-exc_LCA.sav'

  upgoing   = 1

  RESTORE,diffEFDir+diffEFFil

  ;; strtStr = '1997-02-01/09:25:30'
  ;; stopStr = '1997-02-01/09:28:01'
  ;; strtStr = '1997-02-01/09:26:00'
  ;; stopStr = '1997-02-01/09:27:30'

  no_strict_types = 1

  CASE upgoing OF
     0: BEGIN
        ;; diffEFFil = 'orb_1773-diff_eflux-ees-e_angle_156.0-204.0--classics--3--Elphic_et_al_1998-fit_above_500_eV-No_bFunc-exc_LCA.sav'
        strtStr      = '1997-02-01/09:26:00'
        stopStr      = '1997-02-01/09:27:30'
        directionStr = 'downgoing'
        angle        = [-30,30]
     END
     1: BEGIN
        strtStr      = '1997-02-01/09:25:50'
        stopStr      = '1997-02-01/09:28:00'
        directionStr = 'upgoing'
        angle        = [150,210]
     END
  ENDCASE
  ;; RESTORE,detritDir+detritus

  tmpespec_lc = GET_EN_SPEC__FROM_DIFF_EFLUX(diff_eFlux,ANGLE=angle, $
                                             /RETRACE)
  tmpspec_lc  = GET_EN_SPEC__FROM_DIFF_EFLUX(diff_eFlux,ANGLE=angle, $
                                             /RETRACE, $
                                             UNITS='flux')
  tmpJe_lc    = J_2D__FROM_DIFF_EFLUX(diff_eFlux,ANGLE=angle)
  tmpJee_lc   = JE_2D__FROM_DIFF_EFLUX(diff_eFlux,ANGLE=angle)
  charE       = CHAR_ENERGY(tmpJe_lc,tmpJee_lc)

  nHjar       = N_ELEMENTS(tmpeSpec_lc.x)
  nUniq       = N_ELEMENTS(UNIQ(tmpeSpec_lc.x,SORT(tmpeSpec_lc.x)))
  nJe         = N_ELEMENTS(tmpJe_lc.x)
  nJee        = N_ELEMENTS(tmpJee_lc.x)

  IF ~((nHjar EQ nUniq) AND (nHjar EQ nJe) AND (nHjar EQ nJee)) THEN STOP

  IF ~(ARRAY_EQUAL(tmpeSpec_lc.x,tmpJe_lc.x) AND ARRAY_EQUAL(tmpJee_lc.x,tmpJe_lc.x)) THEN BEGIN
     PRINT,"BRO"
     STOP
  ENDIF

  tInds   = VALUE_CLOSEST2(tmpeSpec_lc.x,STR_TO_TIME([strtStr,stopStr]))
  t1      = tmpeSpec_lc.x[tInds[0]]
  t2      = tmpeSpec_lc.x[tInds[1]]

  ;; GET_FA_ORBIT,t1,t2,/DEFINITIVE
  GET_FA_ORBIT,tmpeSpec_lc.x,/TIME_ARRAY,/DEFINITIVE
  GET_DATA,'ORBIT',DATA=orbit
  GET_DATA,'MLT',DATA=mlt
  GET_DATA,'ILAT',DATA=ilat
  GET_DATA,'ALT',DATA=alt
  mlt         = mlt.y
  ilat        = ilat.y
  alt         = alt.y
  orbit       = orbit.y[0]
  orbit_num   = STRCOMPRESS(STRING(orbit),/REMOVE_ALL)


  IDENTIFY_DIFF_EFLUXES_AND_CREATE_STRUCT,tmpeSpec_lc,tmpjee_lc,tmpje_lc, $
                                          mlt,ilat,alt,MAKE_ARRAY(nHjar,VALUE=orbit,/UINT), $
                                          eSpecs_parsed, $
                                          SC_POT=out_sc_pot, $ 
                                          /QUIET, $
                                          BATCH_MODE=batch_mode, $
                                          ORBSTR=orbStr, $
                                          ERRORLOGFILE=badFile


  PREPARE_IDENTIFIED_DIFF_EFLUXES_FOR_TPLOT,eSpecs_parsed, $
                                            TPLOT_NAME=tPlot_name, $
                                            NO_STRICT_TYPES=no_strict_types, $
                                            /CONVERT_TO_NEWELL_INTERP, $
                                            ;; FAVOR_BROADSTRICT_OVER_MONO=favor_broadStrict_over_mono, $
                                            SYMSIZE=symSize, $
                                            YTITLE=yTitle

  tPlt_vars = tPlot_name


  ;;Now eSpec itself
  tPlot_name = 'eSpec'
  tPlt_vars  = [tPlot_name,tPlt_vars]
  tmp        = tmpeSpec_lc
  STORE_DATA,tPlot_name,DATA=tmp

  tmp.y = tmp.y>1.e1 ; Remove zeros
  tmp.y = ALOG10(tmp.y)                                                 ; Pre-log
  STORE_DATA,'eSpec',DATA=tmp                                           ; store data structure
  OPTIONS,'eSpec','spec',1                                              ; set for spectrogram
  ZLIM,'eSpec',6,9,0                                                    ; set z limits
  YLIM,'eSpec',4,40000,1                                                ; set y limits
  OPTIONS,'eSpec','ytitle','e- ' + directionStr + ' !C!CEnergy (eV)'    ; y title
  OPTIONS,'eSpec','ztitle','Log Eflux!C!CeV/cm!U2!N-s-sr-eV'            ; z title
  OPTIONS,'eSpec','x_no_interp',1                                       ; don't interpolate
  OPTIONS,'eSpec','y_no_interp',1                                       ; don't interpolate
  OPTIONS,'eSpec','yticks',3                                            ; set y-axis labels
  OPTIONS,'eSpec','ytickname',['10!A1!N','10!A2!N','10!A3!N','10!A4!N'] ; set y-axis labels
  OPTIONS,'eSpec','ytickv',[10,100,1000,10000]                          ; set y-axis labels
  OPTIONS,'eSpec','panel_size',2                                        ; set panel size 

  ;;Now … spec itself
  tPlot_name = 'spec'
  tPlt_vars  = [tPlot_name,tPlt_vars]
  tmp        = tmpSpec_lc
  STORE_DATA,tPlot_name,DATA=tmp

  tmp.y = tmp.y>1.e1 ; Remove zeros
  tmp.y = ALOG10(tmp.y)                                                 ; Pre-log
  STORE_DATA,'spec',DATA=tmp                                           ; store data structure
  OPTIONS,'spec','spec',1                                              ; set for spectrogram
  ZLIM,'spec',4,7,0                                                    ; set z limits
  YLIM,'spec',4,40000,1                                                ; set y limits
  OPTIONS,'spec','ytitle','e- ' + directionStr + ' !C!CEnergy (eV)'               ; y title
  OPTIONS,'spec','ztitle','Log nFlux!C!C#/cm!U2!N-s-sr-eV'             ; z title
  OPTIONS,'spec','x_no_interp',1                                       ; don't interpolate
  OPTIONS,'spec','y_no_interp',1                                       ; don't interpolate
  OPTIONS,'spec','yticks',3                                            ; set y-axis labels
  OPTIONS,'spec','ytickname',['10!A1!N','10!A2!N','10!A3!N','10!A4!N'] ; set y-axis labels
  OPTIONS,'spec','ytickv',[10,100,1000,10000]                          ; set y-axis labels
  OPTIONS,'spec','panel_size',2                                        ; set panel size 

; Plot the data

  ;;Get a fresh take on the Newell interp

  LOADCT2,43
  TPLOT,tPlt_vars,$
        VAR_LABEL=['ALT','ILAT','MLT'], $
        TITLE='FAST ORBIT '+orbit_num, $
        TRANGE=[t1,t2]

  STOP


END
