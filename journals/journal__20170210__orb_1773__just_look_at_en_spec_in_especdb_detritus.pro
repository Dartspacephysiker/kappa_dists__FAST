;;02/11/17
PRO JOURNAL__20170210__ORB_1773__JUST_LOOK_AT_EN_SPEC_IN_ESPECDB_DETRITUS

  COMPILE_OPT IDL2

  detritDir = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/do_the_Newell_2009/Newell_batch_output/ions_included/'
  detritus  = 'Newell_et_al_identification_of_electron_spectra--ions_included--Orbit_1773_0.sav'

  diffEFDir = '~/software/sdt/batch_jobs/saves_output_etc/'
  diffEFFil = 'orb_1773--diff_eflux--ees--output_from_get_losscone_and_eflux_data.sav'

  RESTORE,diffEFDir+diffEFFil

  STOP
  
  ;; strtStr = '1997-02-01/09:25:30'
  ;; stopStr = '1997-02-01/09:28:01'
  strtStr = '1997-02-01/09:26:00'
  stopStr = '1997-02-01/09:27:30'

  no_strict_types = 1

  RESTORE,detritDir+detritus

  nHjar     = N_ELEMENTS(tmpeSpec_lc.x)
  nUniq     = N_ELEMENTS(UNIQ(tmpeSpec_lc.x,SORT(tmpeSpec_lc.x)))
  nJe       = N_ELEMENTS(tmpJe_lc.x)
  nJee      = N_ELEMENTS(tmpJee_lc.x)

  IF ~((nHjar EQ nUniq) AND (nHjar EQ nJe) AND (nHjar EQ nJee)) THEN STOP

  IF ~(ARRAY_EQUAL(tmpeSpec_lc.x,tmpJe_lc.x) AND ARRAY_EQUAL(tmpJee_lc.x,tmpJe_lc.x)) THEN STOP


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
  OPTIONS,'eSpec','ytitle','e- downgoing !C!CEnergy (eV)'               ; y title
  OPTIONS,'eSpec','ztitle','Log Eflux!C!CeV/cm!U2!N-s-sr-eV'            ; z title
  OPTIONS,'eSpec','x_no_interp',1                                       ; don't interpolate
  OPTIONS,'eSpec','y_no_interp',1                                       ; don't interpolate
  OPTIONS,'eSpec','yticks',3                                            ; set y-axis labels
  OPTIONS,'eSpec','ytickname',['10!A1!N','10!A2!N','10!A3!N','10!A4!N'] ; set y-axis labels
  OPTIONS,'eSpec','ytickv',[10,100,1000,10000]                          ; set y-axis labels
  OPTIONS,'eSpec','panel_size',2                                        ; set panel size 

; Plot the data

  ;;Get a fresh take on the Newell interp

  LOADCT2,43
  TPLOT,tPlt_vars,$
        VAR_LABEL=['ALT','ILAT','MLT'], $
        TITLE='FAST ORBIT '+orbit_num, $
        TRANGE=[t1,t2]


  getTime = '1997-02-01/09:26:20'
  tInd    = VALUE_CLOSEST2(tmpeSpec_lc.x,STR_TO_TIME(getTime))
  ;; ind   = 0
  Xorig       = REFORM(tmpeSpec_lc.v[tInd,*])
  Yorig       = REFORM(tmpeSpec_lc.y[tInd,*])
  junk        = MAX(Yorig,peak_ind)
  peak_energy = Xorig[peak_ind]
  KAPPA__GET_A_ESTIMATES,dat,Xorig,Yorig, $
                         minEInd,maxEInd,nEnergies, $
                         peak_ind,peak_energy,eRange_peak, $
                         ANGLES=angles, $
                         N_ANGLES_IN_RANGE=n_angles_in_range, $
                         BULKANGLE_STRUCT=angleStr, $
                         DONT_TAKE_STOCK_OF_BULKANGLE=dont_take_stock_of_bulkangle, $
                         KAPPA_EST=kappa, $
                         E_ANGLE=e_angle_range, $
                         ADD_GAUSSIAN_ESTIMATE=add_gaussian_estimate, $
                         USE_SDT_GAUSSIAN_FIT=use_SDT_Gaussian_fit, $
                         ESTFACS=estFacs, $
                         A_OUT=A, $
                         AGAUSS_OUT=AGauss, $
                         DONT_PRINT_ESTIMATES=dont_print_estimates, $
                         TEST_NOREV=test_noRev

  STOP

END
