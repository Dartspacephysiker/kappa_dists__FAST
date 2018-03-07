;2018/03/07
;; Reads DMSP F16 satellite file obtained from http://cedar.openmadrigal.org/showExperiment/?experiment_list=100125279
PRO JOURNAL__20180307__BADDELEY_ET_AL_2017__READ_1ST_HDF5_FILE__MAKE_IDL_SAV,ALSO_AACGM=also_AACGM

  COMPILE_OPT IDL2,STRICTARRSUBS

  dir  = '/SPENCEdata/Research/database/DMSP/20071227/'

  file = 'dms_20071227_16e.001__Baddeley_et_al_2007.hdf5'
  outFile = 'dms_20071227_16e.001__Baddeley_et_al_2007.sav'

  orbit = 21626

  remake  = 0

  IF FILE_TEST(dir+outFile) AND ~KEYWORD_SET(remake) THEN BEGIN

     PRINT,"Restoring " + outFile + ' ...'
     RESTORE,dir+outFile

  ENDIF ELSE BEGIN

     IF KEYWORD_SET(also_AACGM) THEN BEGIN
        aacgm__saveFileName = 'dmsp_f16_orb21626__aacgm.sav'

        PRINT,'remember to run to run @/home/spencerh/idl/lib/aacgm/compile_aacgm.pro before going!'
        WAIT,1
     ENDIF

     result = H5F_OPEN(dir+file)

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;OPEN GROUPS
     data1DGroup = H5G_OPEN(result,"Data/Array Layout/1D Parameters")
     data2DGroup = H5G_OPEN(result,"Data/Array Layout/2D Parameters")
     dataLayoutGroup = H5G_OPEN(result,"Data/Array Layout")

     ;; el_i_enerID = H5D_OPEN(data1DGroup,var)
     ;; el_i_ener = H5D_READ(el_i_enerID)
     
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;GROUP 1D

     data_parametersID = H5D_OPEN(data1DGroup,"Data Parameters")
     data_parameters = H5D_READ(data_parametersID)
     H5D_CLOSE,data_parametersID

     ion_i_fluxID = H5D_OPEN(data1DGroup,"ion_i_flux")
     ion_i_flux = H5D_READ(ion_i_fluxID)
     H5D_CLOSE,ion_i_fluxID

     gdlatID = H5D_OPEN(data1DGroup,"gdlat")
     gdlat = H5D_READ(gdlatID)
     H5D_CLOSE,gdlatID

     el_m_enerID = H5D_OPEN(data1DGroup,"el_m_ener")
     el_m_ener = H5D_READ(el_m_enerID)
     H5D_CLOSE,el_m_enerID

     el_i_enerID = H5D_OPEN(data1DGroup,"el_i_ener")
     el_i_ener = H5D_READ(el_i_enerID)
     H5D_CLOSE,el_i_enerID

     gdlatID = H5D_OPEN(data1DGroup,"gdlat")
     gdlat = H5D_READ(gdlatID)
     H5D_CLOSE,gdlatID

     el_i_fluxID = H5D_OPEN(data1DGroup,"el_i_flux")
     el_i_flux = H5D_READ(el_i_fluxID)
     H5D_CLOSE,el_i_fluxID

     gdaltID = H5D_OPEN(data1DGroup,"gdalt")
     gdalt = H5D_READ(gdaltID)
     H5D_CLOSE,gdaltID

     glonID = H5D_OPEN(data1DGroup,"glon")
     glon = H5D_READ(glonID)
     H5D_CLOSE,glonID

     ion_i_enerID = H5D_OPEN(data1DGroup,"ion_i_ener")
     ion_i_ener = H5D_READ(ion_i_enerID)
     H5D_CLOSE,ion_i_enerID

     ion_m_enerID = H5D_OPEN(data1DGroup,"ion_m_ener")
     ion_m_ener = H5D_READ(ion_m_enerID)
     H5D_CLOSE,ion_m_enerID

     mltID = H5D_OPEN(data1DGroup,"mlt")
     mlt = H5D_READ(mltID)
     H5D_CLOSE,mltID

     mlatID = H5D_OPEN(data1DGroup,"mlat")
     mlat = H5D_READ(mlatID)
     H5D_CLOSE,mlatID

     mlongID = H5D_OPEN(data1DGroup,"mlong")
     mlong = H5D_READ(mlongID)
     H5D_CLOSE,mlongID

     sat_idID = H5D_OPEN(data1DGroup,"sat_id")
     sat_id = H5D_READ(sat_idID)
     H5D_CLOSE,sat_idID

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;GROUP 2D
     data_parametersID = H5D_OPEN(data2DGroup,"Data Parameters")
     data_parameters = H5D_READ(data_parametersID)
     H5D_CLOSE,data_parametersID

     ch_ctrl_enerID = H5D_OPEN(data2DGroup,"ch_ctrl_ener")
     ch_ctrl_ener = H5D_READ(ch_ctrl_enerID)
     H5D_CLOSE,ch_ctrl_enerID

     el_d_enerID = H5D_OPEN(data2DGroup,"el_d_ener")
     el_d_ener = H5D_READ(el_d_enerID)
     H5D_CLOSE,el_d_enerID

     el_d_fluxID = H5D_OPEN(data2DGroup,"el_d_flux")
     el_d_flux = H5D_READ(el_d_fluxID)
     H5D_CLOSE,el_d_fluxID

     ion_d_enerID = H5D_OPEN(data2DGroup,"ion_d_ener")
     ion_d_ener = H5D_READ(ion_d_enerID)
     H5D_CLOSE,ion_d_enerID

     ion_d_fluxID = H5D_OPEN(data2DGroup,"ion_d_flux")
     ion_d_flux = H5D_READ(ion_d_fluxID)
     H5D_CLOSE,ion_d_fluxID

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;GROUP LAYOUT

     ch_energyID = H5D_OPEN(dataLayoutGroup,"ch_energy")
     ch_energy = H5D_READ(ch_energyID)
     H5D_CLOSE,ch_energyID

     timestampsID = H5D_OPEN(dataLayoutGroup,"timestamps")
     timestamps = H5D_READ(timestampsID)
     H5D_CLOSE,timestampsID

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; CLOSE GROUPS
     H5G_CLOSE,data1DGroup
     H5G_CLOSE,data2DGroup
     H5G_CLOSE,dataLayoutGroup

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; RANDOM THANGS

     ;; bro = H5_BROWSER(dir+file)

     ;; this = H5G_GET_MEMBER_NAME(result,"Data/Array Layout/1D Parameters",1)
     
     ;; pref = 'Data/Array layout/1D Parameters/'
     ;; var  = 'el_i_ener'

     ;; slashNMem = H5G_GET_NMEMBERS(result,'/')
     ;; slashGroup = H5G_OPEN(result,'/')
     
     ;; H5G_CLOSE(slashGroup)

     ;; FOR k=0,slashNMem-1 DO BEGIN
     ;;    this = H5G_GET_MEMBER_NAME(result,'Data',k)

     ;; Result = H5G_GET_NMEMBERS(result,Name)

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; NCDF STUFF

     ;; NCDF_LIST, dir+file,/VARIABLES,/DIMENSIONS,/GATT,/VATT

     ;; NCDF_GET,dir+file,'mlt',result
     ;; mlt = (result['mlt'])['value']

     ;; NCDF_GET,dir+file,'timestamps',result
     ;; timestamps = (result['timestamps'])['value']

     ;; NCDF_GET,dir+file,'ch_energy',result
     ;; ch_energy = (result['ch_energy'])['value']

     ;; NCDF_GET,dir+file,'el_i_ener',result
     ;; el_i_ener = (result['el_i_ener'])['value']

     ;; NCDF_GET,dir+file,'el_i_flux',result
     ;; el_i_flux = (result['el_i_flux'])['value']

     ;; NCDF_GET,dir+file,'el_m_ener',result
     ;; el_m_ener = (result['el_m_ener'])['value']

     ;; NCDF_GET,dir+file,'gdalt',result
     ;; gdalt = (result['gdalt'])['value']

     ;; NCDF_GET,dir+file,'gdlat',result
     ;; gdlat = (result['gdlat'])['value']

     ;; NCDF_GET,dir+file,'glon',result
     ;; glon = (result['glon'])['value']

     ;; NCDF_GET,dir+file,'ion_i_ener',result
     ;; ion_i_ener = (result['ion_i_ener'])['value']

     ;; NCDF_GET,dir+file,'ion_i_flux',result
     ;; ion_i_flux = (result['ion_i_flux'])['value']

     ;; NCDF_GET,dir+file,'ion_m_ener',result
     ;; ion_m_ener = (result['ion_m_ener'])['value']

     ;; NCDF_GET,dir+file,'mlat',result
     ;; mlat = (result['mlat'])['value']

     ;; NCDF_GET,dir+file,'mlong',result
     ;; mlong = (result['mlong'])['value']

     ;; NCDF_GET,dir+file,'mlt',result
     ;; mlt = (result['mlt'])['value']

     ;; NCDF_GET,dir+file,'sat_id',result
     ;; sat_id = (result['sat_id'])['value']

     ;; NCDF_GET,dir+file,'ch_ctrl_ener',result
     ;; ch_ctrl_ener = (result['ch_ctrl_ener'])['value']

     ;; NCDF_GET,dir+file,'el_d_ener',result
     ;; el_d_ener = (result['el_d_ener'])['value']

     ;; NCDF_GET,dir+file,'el_d_flux',result
     ;; el_d_flux = (result['el_d_flux'])['value']

     ;; NCDF_GET,dir+file,'ion_d_ener',result
     ;; ion_d_ener = (result['ion_d_ener'])['value']

     ;; NCDF_GET,dir+file,'ion_d_flux',result
     ;; ion_d_flux = (result['ion_d_flux'])['value']

     nTime = N_ELEMENTS(timestamps)
     ch_energy = TRANSPOSE(ch_energy # MAKE_ARRAY(nTime,/FLOAT,VALUE=1.))
     dmsp = {time          : TEMPORARY(timestamps)   , $ ; Unix seconds, Number of seconds since UT midnight 1970-01-01
             orbit         : REPLICATE(orbit,nTime)  , $
             mlt           : TEMPORARY(mlt)          , $ ; hour, Magnetic local time
             alt           : TEMPORARY(gdalt)        , $ ; km, Geodetic altitude (height)
             aacgm         : 'N/A'                   , $
             mag           : {lat         : TEMPORARY(mlat), $   ; deg, Magnetic latitude
                              lon         : TEMPORARY(mlong)}, $ ; deg, Magnetic Longitude
             geo           : {lat         : TEMPORARY(gdlat), $   ; deg, Geodetic latitude of measurement
                              lon         : TEMPORARY(glon) }, $ ; deg, Geographic longitude of measurement
             el_i_ener     : TEMPORARY(el_i_ener)    , $ ; enFlux, Integr elect energy flux (eV/cm2s*ster)
             el_i_flux     : TEMPORARY(el_i_flux)    , $ ; numFlux, Integrated elect num flux (1/cm2s*ster)
             el_d_ener     : TEMPORARY(el_d_ener)    , $ ; enFlux, Diff electron energy flux (1/cm2s*ster)
             el_d_flux     : TEMPORARY(el_d_flux)    , $ ; numFlux, Diff electron num flux (1/cm2eVs*ster)
             el_m_ener     : TEMPORARY(el_m_ener)    , $ ; eV, Mean electron energy
             ion_i_ener    : TEMPORARY(ion_i_ener)   , $ ; enFlux, Integr ion energy flux (eV/cm2s*ster)
             ion_i_flux    : TEMPORARY(ion_i_flux)   , $ ; numFlux, Integrated ion num flux (1/cm2s*ster)
             ion_d_ener    : TEMPORARY(ion_d_ener)   , $ ; enFlux, Diff ion energy flux (1/cm2s*ster)
             ion_d_flux    : TEMPORARY(ion_d_flux)   , $ ; numFlux, Diff ion num flux (1/cm2eVs*ster)
             ion_m_ener    : TEMPORARY(ion_m_ener)   , $ ; eV, Mean ion energy
             ch_energy     : TEMPORARY(ch_energy)    , $ ; eV, Channel spacing energy
             ch_ctrl_ener  : TEMPORARY(ch_ctrl_ener) , $ ; eV, Channel central energy
             sat_id        : TEMPORARY(sat_id)       }   ; sat_id:

     PRINT,"Saving DMSP struct to " + outFile + ' ...'
     SAVE,dmsp,FILENAME=dir+outFile

     IF KEYWORD_SET(also_AACGM) THEN BEGIN

        geoStruct = {lat: dmsp.geo.lat, lon: dmsp.geo.lon, alt: dmsp.alt}

        outDir = '/SPENCEdata/Research/Satellites/FAST/kappa_dists/saves_output_etc/'

        AACGM = CONVERT_GEO_TO_AACGM__SINGLE_SET(dmsp.time,geoStruct, $
                                                 OUTPUT__SAVEFILENAME=aacgm__saveFileName, $
                                                 OUTDIR=outDir, $
                                                 /RESTORE_LASTCONV)

        ;; dmspTmp = CREATE_STRUCT('AACGM',aacgm,dmsp)
        ;; dmsp = TEMPORARY(dmspTmp)

        STR_ELEMENT,dmsp,'AACGM',AACGM,/ADD_REPLACE
        SAVE,dmsp,FILENAME=dir+outFile
     ENDIF

  ENDELSE
  STOP

END


