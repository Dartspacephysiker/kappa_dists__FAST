;2016/07/14 Now get the thing back
;2017/12/05 As a result of the newest version of GET_DIFF_EFLUX, which relies on McFadden-style cleansing (as discovered in
;MAKE_ESA_CDF.pro)
FUNCTION MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX__MCFADDEN_STYLE,diff_eFlux,ind, $
   HAS_SC_POT=has_sc_pot, $
   UNITS=units

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; this = ndimen(data3d[ind].geomfactor)

  data3d = diff_eFlux[ind]

  ;; 20181006 Added while working on ELECTRON_MOMENTS_AND_SPECTRAL_IDENTIFICATION_V0
  nUniqEdgery = N_ELEMENTS(UNIQ(FLOOR(data3d.energy),SORT(FLOOR(data3d.energy))))
  xtraString = ''
  IF nUniqEdgery LT 10 AND data3d.project_name EQ 'FAST' THEN BEGIN
     ;; PRINT,FORMAT='("MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX__MCFADDEN_STYLE: Only have ",I0," unique energies; Making diff_eFlux member invalid ...")',nUniqEdgery
     xtraString = STRING(FORMAT='("(Only have ",I0," unique energies)")',nUniqEdgery)

     diff_eFlux[ind].valid = 0
     data3d.valid = 0
     RETURN, data3d
  ENDIF

  IF ~data3d.valid OR ((WHERE(FINITE(data3d.data)))[0] EQ -1) THEN BEGIN
     ;; PRINT,"Bogus diff eFlux struct"
     PRINT,FORMAT='("MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX__MCFADDEN_STYLE: invalid at ",A0," ",A0)',T2S(data3d.time,/MS),xtraString
     RETURN,data3d
  ENDIF

  STR_ELEMENT,data3d,'ddata',VALUE=ddata

  IF NOT KEYWORD_SET(ddata) THEN BEGIN
     data3d                   = CONV_UNITS(data3d,'counts')
     ADD_STR_ELEMENT,data3d,'ddata',(data3d.data)^.5
     data3d                   = CONV_UNITS(data3d,units)
  ENDIF ELSE data3d           = CONV_UNITS(data3d,units)

  twoD_geom_and_dTheta = (N_ELEMENTS(WHERE(ABS(data3d.dtheta) GT 0.00001))/data3d.nBins) EQ data3d.nenergy

  CASE NDIMEN(data3d.geom) OF
     2: BEGIN
        struct           = {data_name       :data3d.data_name     , $
                            valid           :data3d.valid         , $
                            project_name    :data3d.project_name  , $
                            units_name      :data3d.units_name           , $
                            units_procedure :data3d.units_procedure      , $
                            time            :data3d.time          , $
                            end_time        :data3d.end_time      , $
                            integ_t         :data3d.integ_t       , $
                            nbins           :data3d.nbins         , $
                            nenergy         :data3d.nenergy                 , $
                            ;; data            :data3d.data         [*,*] , $
                            ;; ddata           :data3d.ddata        [*,*] , $
                            ;; energy          :data3d.energy       [*,*] , $
                            ;; theta           :data3d.theta        [*,*] , $
                            data            :data3d.data         [0:data3d.nenergy-1,0:data3d.nbins-1] , $
                            ddata           :data3d.ddata        [0:data3d.nenergy-1,0:data3d.nbins-1] , $
                            energy          :data3d.energy       [0:data3d.nenergy-1,0:data3d.nbins-1] , $
                            theta           :data3d.theta        [0:data3d.nenergy-1,0:data3d.nbins-1] , $
                            geom            :(twoD_geom_and_dTheta                             ? $
                                              data3d.geom[0:data3d.nenergy-1,0:data3d.nbins-1] : $
                                              data3d.geom[0:data3d.nenergy-1]), $
                            denergy         :data3d.denergy      [0:data3d.nenergy-1,0:data3d.nbins-1] , $
                            dtheta          :(twoD_geom_and_dTheta                             ? $
                                              data3d.dtheta[0:data3d.nenergy-1,0:data3d.nbins-1] : $
                                              data3d.dtheta[0:data3d.nenergy-1]), $
                            eff             :data3d.eff          [0:data3d.nenergy-1] , $
                            mass            :data3d.mass             , $
                            geomfactor      :data3d.geomfactor       , $
                            header_bytes    :data3d.header_bytes [*] , $
                            ;; st_index        :data3d.st_index      , $
                            ;; en_index        :data3d.en_index      , $
                            ;; npts            :data3d.npts          , $
                            index           :data3d.index        }

        

     END
     3: BEGIN
        ;; N_ELEMENTS(WHERE(ABS(data3d.dtheta) GT 0.00001))/
        struct           = {data_name       :data3d.data_name     , $
                            valid           :data3d.valid         , $
                            project_name    :data3d.project_name  , $
                            units_name      :data3d.units_name    , $
                            units_procedure :data3d.units_procedure, $
                            time            :data3d.time          , $
                            end_time        :data3d.end_time      , $
                            integ_t         :data3d.integ_t       , $
                            nbins           :data3d.nbins         , $
                            nenergy         :data3d.nenergy       , $
                            data            :data3d.data         [0:data3d.nenergy-1,0:data3d.nbins-1] , $
                            ddata           :data3d.ddata        [0:data3d.nenergy-1,0:data3d.nbins-1] , $
                            energy          :data3d.energy       [0:data3d.nenergy-1,0:data3d.nbins-1] , $
                            theta           :data3d.theta        [0:data3d.nenergy-1,0:data3d.nbins-1] , $
                            geom            :data3d.geom         [0:data3d.nenergy-1,0:data3d.nbins-1] , $
                            denergy         :data3d.denergy      [0:data3d.nenergy-1,0:data3d.nbins-1] , $
                            dtheta          :data3d.dtheta       [0:data3d.nenergy-1,0:data3d.nbins-1], $
                            eff             :data3d.eff          [*], $
                            mass            :data3d.mass            , $
                            geomfactor      :data3d.geomfactor      , $
                            header_bytes    :data3d.header_bytes [*], $
                            ;; st_index        :data3d.st_index      , $
                            ;; en_index        :data3d.en_index      , $
                            ;; npts            :data3d.npts          , $
                            index           :data3d.index        }


     END
  ENDCASE

  IF KEYWORD_SET(has_sc_pot) THEN STR_ELEMENT,struct,'sc_pot',data3d.sc_pot,/ADD_REPLACE

  ;; IF KEYWORD_SET(units) THEN CALL_PROCEDURE,data3d.units_procedure,UNITS=units

  RETURN,struct
END
