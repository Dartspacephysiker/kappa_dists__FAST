;2018/03/07
;; Reads DMSP F16 satellite file obtained from http://cedar.openmadrigal.org/showExperiment/?experiment_list=100125279
PRO JOURNAL__20180307__BADDELEY_ET_AL_2017__READ_2ND_HDF5_FILE__MAKE_IDL_SAV

  COMPILE_OPT IDL2,STRICTARRSUBS

  dir  = '/SPENCEdata/Research/database/DMSP/20071227/'

  thirdFile = 0

  pref = 'dms_20071227_16s1.001__Baddeley_et_al_2007'
  IF KEYWORD_SET(thirdFile) THEN pref = 'dms_20071227_16s4.001__Baddeley_et_al_2007'
  file = pref+'.hdf5'
  outFile = pref + '.sav'
  infoFile = pref + '.info'

  orbit = 21626

  remake  = 1

  IF FILE_TEST(dir+outFile) AND ~KEYWORD_SET(remake) THEN BEGIN

     PRINT,"Restoring " + outFile + ' ...'
     RESTORE,dir+outFile

  ENDIF ELSE BEGIN

     IF KEYWORD_SET(also_AACGM) THEN BEGIN
        aacgm__saveFileName = 'dmsp_f16_orb21626__aacgm__file02.sav'

        PRINT,'remember to run to run @/home/spencerh/idl/lib/aacgm/compile_aacgm.pro before going!'
        WAIT,1
     ENDIF

     result = H5F_OPEN(dir+file)

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;OPEN GROUP AND DATASET (AND SPACE?)
     dataGroup = H5G_OPEN(result,"Data")
     metaDGroup = H5G_OPEN(result,"Metadata")
     tableID = H5D_OPEN(dataGroup,"Table Layout")
     ;; tabSpaceID = H5D_GET_SPACE(tableID)

     ;; this = H5G_GET_OBJ_NAME_BY_IDX(dataGroup,0)
     ;; that = H5T_GET_FIELDS

     structs = H5D_READ(tableID)

     paramID = H5D_OPEN(metaDGroup,"Data Parameters")
     params  = H5D_READ(paramID)
     expNID  = H5D_OPEN(metaDGroup,"Experiment Notes")
     expN    = H5D_READ(expNID)
     expPID  = H5D_OPEN(metaDGroup,"Experiment Parameters")
     expP    = H5D_READ(expPID)
     ;;empty
     ;; indSPID = H5D_OPEN(metaDGroup,"Independent Spatial Parameters")
     ;; indSP   = H5D_READ(indSPID)

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; CLOSE EVERYONE
     ;; H5S_CLOSE,tabSpaceID
     H5D_CLOSE,paramID
     H5D_CLOSE,expNID
     H5D_CLOSE,expPID

     H5D_CLOSE,tableID
     H5G_CLOSE,dataGroup
     H5F_CLOSE,result

     OPENW,outLun,dir+infoFile,/GET_LUN

     PRINTF,outLun,'"Parameters"'
     PRINTF,outLun,FORMAT='(A15,T17,A40,T60,A7,T68,A8,T78,A30)','MNEMONIC','DESCRIPTION','ISERROR','UNITS','CATEGORY'
     FOR k=0,N_ELEMENTS(params)-1 DO PRINTF,outLun,FORMAT='(A15,T17,A40,T60,I6,T68,A8,T78,A30)', $
               params[k].mnemonic, $
               params[k].description, $
               params[k].iserror, $
               params[k].units, $
               params[k].category
     PRINTF,outLun,''
     PRINTF,outLun,'"Experiment Notes"'
     FOR k=0,N_ELEMENTS(expN)-1 DO PRINTF,outLun,expN[k].file_notes
     PRINTF,outLun,''
     PRINTF,outLun,'"Experiment Parameters"'
     FOR k=0,N_ELEMENTS(expP)-1 DO PRINTF,outLun,FORMAT='(A20," : ",A0)',expP[k].name,expP[k].value

     PRINT,"Closing " + infoFile + ' ...'
     CLOSE,outLun

     ;; tabSpaceIsSimple = H5S_IS_SIMPLE(tabSpaceID)
     ;; tabSpaceDims  = H5S_GET_SIMPLE_EXTENT_DIMS(tabSpaceID,MAX_DIMENSIONS=tabSpaceMaxDims)
     ;; tabSpaceNDims = H5S_GET_SIMPLE_EXTENT_NDIMS(tabSpaceID)
     ;; tabSpaceNPoints = H5S_GET_SIMPLE_EXTENT_NPOINTS(tabSpaceID)
     ;; tabSpaceType = H5S_GET_SIMPLE_EXTENT_TYPE(tabSpaceID)

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;REARRANGE STRUCTS

     IF KEYWORD_SET(thirdFile) THEN BEGIN

        dmsp3  = {year         : structs[*].year       , $
                  month        : structs[*].month      , $
                  day          : structs[*].day        , $
                  hour         : structs[*].hour       , $
                  min          : structs[*].min        , $
                  sec          : structs[*].sec        , $
                  recno        : structs[*].recno      , $
                  kindat       : structs[*].kindat     , $
                  kinst        : structs[*].kinst      , $
                  ut1_unix     : structs[*].ut1_unix   , $
                  ut2_unix     : structs[*].ut2_unix   , $
                  gdlat        : structs[*].gdlat      , $
                  glon         : structs[*].glon       , $
                  gdalt        : structs[*].gdalt      , $
                  sat_id       : structs[*].sat_id     , $
                  mlt          : structs[*].mlt        , $
                  mlat         : structs[*].mlat       , $
                  mlong        : structs[*].mlong      , $
                  ti           : structs[*].ti         , $
                  te           : structs[*].te         , $
                  p_oplus      : structs[*].po_        , $
                  elepot       : structs[*].elepot}

        all_good_i = WHERE(FINITE(dmsp3.year) AND $       
                           FINITE(dmsp3.month) AND $      
                           FINITE(dmsp3.day) AND $        
                           FINITE(dmsp3.hour) AND $       
                           FINITE(dmsp3.min) AND $        
                           FINITE(dmsp3.sec) AND $        
                           FINITE(dmsp3.recno) AND $      
                           FINITE(dmsp3.kindat) AND $     
                           FINITE(dmsp3.kinst) AND $      
                           FINITE(dmsp3.ut1_unix) AND $   
                           FINITE(dmsp3.ut2_unix) AND $   
                           FINITE(dmsp3.gdlat) AND $      
                           FINITE(dmsp3.glon) AND $       
                           FINITE(dmsp3.gdalt) AND $      
                           FINITE(dmsp3.sat_id) AND $     
                           FINITE(dmsp3.mlt) AND $        
                           FINITE(dmsp3.mlat) AND $       
                           FINITE(dmsp3.mlong) AND $      
                           FINITE(dmsp3.ti) AND $        
                           ;; FINITE(dmsp3.te) AND $  
                           FINITE(dmsp3.p_oplus) AND $  
                           FINITE(dmsp3.elepot))

        STR_ELEMENT,dmsp3,"all_good_i",TEMPORARY(all_good_i),/ADD_REPLACE

        PRINT,"Saving DMSP3 struct to " + outFile + ' ...'
        SAVE,dmsp3,FILENAME=dir+outFile

     ENDIF ELSE BEGIN

        dmsp2  = {year         : structs[*].year       , $
                  month        : structs[*].month      , $
                  day          : structs[*].day        , $
                  hour         : structs[*].hour       , $
                  min          : structs[*].min        , $
                  sec          : structs[*].sec        , $
                  recno        : structs[*].recno      , $
                  kindat       : structs[*].kindat     , $
                  kinst        : structs[*].kinst      , $
                  ut1_unix     : structs[*].ut1_unix   , $
                  ut2_unix     : structs[*].ut2_unix   , $
                  gdlat        : structs[*].gdlat      , $
                  glon         : structs[*].glon       , $
                  gdalt        : structs[*].gdalt      , $
                  sat_id       : structs[*].sat_id     , $
                  mlt          : structs[*].mlt        , $
                  mlat         : structs[*].mlat       , $
                  mlong        : structs[*].mlong      , $
                  n_e          : structs[*]._ne        , $
                  hor_ion_v    : structs[*].hor_ion_v  , $
                  vert_ion_v   : structs[*].vert_ion_v , $
                  bd           : structs[*].bd         , $
                  b_forward    : structs[*].b_forward  , $
                  b_perp       : structs[*].b_perp     , $
                  diff_bd      : structs[*].diff_bd    , $
                  diff_b_for   : structs[*].diff_b_for , $
                  diff_b_perp  : structs[*].diff_b_perp}

        all_good_i = WHERE(FINITE(dmsp2.year) AND $       
                           FINITE(dmsp2.month) AND $      
                           FINITE(dmsp2.day) AND $        
                           FINITE(dmsp2.hour) AND $       
                           FINITE(dmsp2.min) AND $        
                           FINITE(dmsp2.sec) AND $        
                           FINITE(dmsp2.recno) AND $      
                           FINITE(dmsp2.kindat) AND $     
                           FINITE(dmsp2.kinst) AND $      
                           FINITE(dmsp2.ut1_unix) AND $   
                           FINITE(dmsp2.ut2_unix) AND $   
                           FINITE(dmsp2.gdlat) AND $      
                           FINITE(dmsp2.glon) AND $       
                           FINITE(dmsp2.gdalt) AND $      
                           FINITE(dmsp2.sat_id) AND $     
                           FINITE(dmsp2.mlt) AND $        
                           FINITE(dmsp2.mlat) AND $       
                           FINITE(dmsp2.mlong) AND $      
                           FINITE(dmsp2.n_e) AND $        
                           FINITE(dmsp2.hor_ion_v) AND $  
                           FINITE(dmsp2.vert_ion_v) AND $ 
                           FINITE(dmsp2.bd) AND $         
                           FINITE(dmsp2.b_forward) AND $  
                           FINITE(dmsp2.b_perp) AND $     
                           FINITE(dmsp2.diff_bd) AND $    
                           FINITE(dmsp2.diff_b_for) AND $ 
                           FINITE(dmsp2.diff_b_perp))

        STR_ELEMENT,dmsp2,"all_good_i",TEMPORARY(all_good_i),/ADD_REPLACE

        PRINT,'Making tStamps ...'

        nHere = N_ELEMENTS(dmsp2.year)
        timeStr = MAKE_ARRAY(nHere,/STRING)
        times = MAKE_ARRAY(nHere,/DOUBLE)
        FOR k=0,nHere-1 DO $
           timeStr[k] = STRING(FORMAT='(I4,"-",I02,"-",I02,"/",I02,":",I02,":",I02)', $
                               dmsp2.year[k], $
                               dmsp2.month[k], $
                               dmsp2.day[k], $
                               dmsp2.hour[k], $
                               dmsp2.min[k], $
                               dmsp2.sec[k])

        divFactor     = 10000L  ;No more than 10000 at once
        FOR kk=0L,(nHere/divFactor) DO BEGIN
           ind1       = kk*divFactor
           ind2       = ( ((kk+1)*divFactor) < (nHere - 1) )
           PRINT,'Inds: ' + STRCOMPRESS(ind1,/REMOVE_ALL) + ', ' + STRCOMPRESS(ind2,/REMOVE_ALL)
           tempI      = [ind1:ind2]
           times[tempI] = S2T(timeStr[tempI])
        ENDFOR

        dmsp2Tmp = CREATE_STRUCT('time',times,dmsp2)
        dmsp2 = TEMPORARY(dmsp2Tmp)
        
        PRINT,"Saving DMSP2 struct to " + outFile + ' ...'
        SAVE,dmsp2,FILENAME=dir+outFile

     ENDELSE

     ;; IF KEYWORD_SET(also_AACGM) THEN BEGIN

     ;;    geoStruct = {lat: dmsp.geo.lat, lon: dmsp.geo.lon, alt: dmsp.alt}

     ;;    outDir = '/SPENCEdata/Research/Satellites/FAST/kappa_dists/saves_output_etc/'

     ;;    AACGM = CONVERT_GEO_TO_AACGM__SINGLE_SET(dmsp.time,geoStruct, $
     ;;                                             OUTPUT__SAVEFILENAME=aacgm__saveFileName, $
     ;;                                             OUTDIR=outDir, $
     ;;                                             /RESTORE_LASTCONV)

     ;;    ;; dmspTmp = CREATE_STRUCT('AACGM',aacgm,dmsp)
     ;;    ;; dmsp = TEMPORARY(dmspTmp)

     ;;    STR_ELEMENT,dmsp,'AACGM',AACGM,/ADD_REPLACE
     ;;    SAVE,dmsp,FILENAME=dir+outFile
     ;; ENDIF

  ENDELSE
  STOP

END
