;2016/07/14 Now get the thing back
FUNCTION MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX,data3d,ind, $
   HAS_SC_POT=has_sc_pot, $
   UNITS=units, $
   IS_MCFADDEN_DIFF_EFLUX=is_McFadden_diff_eFlux

  COMPILE_OPT IDL2,STRICTARRSUBS

  this = ndimen(data3d.geomfactor)

  IF KEYWORD_SET(is_McFadden_diff_eFlux) OR N_ELEMENTS(data3d) GT 1 THEN BEGIN

     struct = data3d[ind]

     IF struct.valid EQ 0 THEN RETURN,struct

     ;; we need to shrink struct if 2nd dim of struct.theta disagrees with struct.nbins
     IF N_ELEMENTS(struct.theta[0,*]) NE struct.nbins THEN BEGIN
        nbins  = struct.nbins
        nNRG   = struct.nenergy
        struct = {data_name       :struct.data_name     , $
                  valid           :struct.valid         , $
                  project_name    :struct.project_name  , $
                  units_name      :struct.units_name           , $
                  units_procedure :struct.units_procedure      , $
                  time            :struct.time          , $
                  end_time        :struct.end_time      , $
                  integ_t         :struct.integ_t       , $
                  nbins           :struct.nbins         , $
                  nenergy         :struct.nenergy            , $
                  data            :struct.data         [0:nNRG-1,0:nbins-1] , $
                  ddata           :struct.ddata        [0:nNRG-1,0:nbins-1] , $
                  energy          :struct.energy       [0:nNRG-1,0:nbins-1] , $
                  theta           :struct.theta        [0:nNRG-1,0:nbins-1] , $
                  geom            :struct.geom         [0:nNRG-1,0:nbins-1] , $
                  denergy         :struct.denergy      [0:nNRG-1,0:nbins-1] , $
                  ;; dtheta          :struct.dtheta       [0:nbins-1] , $
                  dtheta          :struct.dtheta       [0:nNRG-1,0:nbins-1] , $
                  eff             :struct.eff          [0:nNRG-1] , $
                  mass            :struct.mass             , $
                  geomfactor      :struct.geomfactor       , $
                  header_bytes    :struct.header_bytes [*] , $
                  index           :struct.index        }
     ENDIF

     ;; CASE NDIMEN(data3d.geom) OF
     ;;    2: BEGIN
     ;;       struct           = {data_name       :data3d[ind].data_name     , $
     ;;                           valid           :data3d[ind].valid         , $
     ;;                           project_name    :data3d[ind].project_name  , $
     ;;                           units_name      :data3d[ind].units_name           , $
     ;;                           units_procedure :data3d[ind].units_procedure      , $
     ;;                           time            :data3d[ind].time          , $
     ;;                           end_time        :data3d[ind].end_time      , $
     ;;                           integ_t         :data3d[ind].integ_t       , $
     ;;                           nbins           :data3d[ind].nbins         , $
     ;;                           nenergy         :data3d[ind].nenergy            , $
     ;;                           data            :data3d[ind].data         [*,*] , $
     ;;                           ddata           :data3d[ind].data         [*,*] , $
     ;;                           energy          :data3d[ind].energy       [*,*] , $
     ;;                           theta           :data3d[ind].theta        [*,*] , $
     ;;                           geom            :data3d[ind].geom         [*]   , $
     ;;                           denergy         :data3d[ind].denergy      [*,*] , $
     ;;                           dtheta          :data3d[ind].dtheta       [*] , $
     ;;                           eff             :data3d[ind].eff          [*] , $
     ;;                           mass            :data3d[ind].mass             , $
     ;;                           geomfactor      :data3d[ind].geomfactor       , $
     ;;                           header_bytes    :data3d[ind].header_bytes [*] , $
     ;;                           index           :data3d[ind].index        }

           

     ;;    END
     ;;    3: BEGIN
     ;;       struct           = {data_name       :data3d[ind].data_name     , $
     ;;                           valid           :data3d[ind].valid         , $
     ;;                           project_name    :data3d[ind].project_name  , $
     ;;                           units_name      :data3d[ind].units_name      , $
     ;;                           units_procedure :data3d[ind].units_procedure , $
     ;;                           time            :data3d[ind].time          , $
     ;;                           end_time        :data3d[ind].end_time      , $
     ;;                           integ_t         :data3d[ind].integ_t       , $
     ;;                           nbins           :data3d[ind].nbins         , $
     ;;                           nenergy         :data3d[ind].nenergy            , $
     ;;                           data            :data3d[ind].data         [*,*] , $
     ;;                           ddata           :data3d[ind].data         [*,*] , $
     ;;                           energy          :data3d[ind].energy       [*,*] , $
     ;;                           theta           :data3d[ind].theta        [*,*] , $
     ;;                           geom            :data3d[ind].geom         [*,*] , $
     ;;                           denergy         :data3d[ind].denergy      [*,*] , $
     ;;                           dtheta          :data3d[ind].dtheta       [*,*] , $
     ;;                           eff             :data3d[ind].eff          [*] , $
     ;;                           mass            :data3d[ind].mass             , $
     ;;                           geomfactor      :data3d[ind].geomfactor       , $
     ;;                           header_bytes    :data3d[ind].header_bytes [*] , $
     ;;                           index           :data3d[ind].index        }

     ;;       PRINT,"MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX WARNING: USING DATA AS DDATA"
     ;;       STOP
     ;;    END
     ;; ENDCASE

  ENDIF ELSE BEGIN
     
     CASE NDIMEN(data3d.geom) OF
        2: BEGIN
           struct           = {data_name       :data3d.data_name    [     ind] , $
                               valid           :data3d.valid        [     ind] , $
                               project_name    :data3d.project_name [     ind] , $
                               units_name      :data3d.units_name           , $
                               units_procedure :data3d.units_procedure      , $
                               time            :data3d.time         [     ind] , $
                               end_time        :data3d.end_time     [     ind] , $
                               integ_t         :data3d.integ_t      [     ind] , $
                               nbins           :data3d.nbins        [     ind] , $
                               nenergy         :data3d.nenergy                 , $
                               data            :data3d.data         [*,*, ind] , $
                               ddata           :data3d.ddata        [*,*, ind] , $
                               energy          :data3d.energy       [*,*, ind] , $
                               theta           :data3d.theta        [*,*, ind] , $
                               geom            :data3d.geom         [*,   ind] , $
                               denergy         :data3d.denergy      [*,*, ind] , $
                               dtheta          :data3d.dtheta       [*,   ind] , $
                               eff             :data3d.eff          [*,   ind] , $
                               mass            :data3d.mass                    , $
                               geomfactor      :data3d.geomfactor   [     ind] , $
                               header_bytes    :data3d.header_bytes [*,   ind] , $
                               st_index        :data3d.st_index     [     ind] , $
                               en_index        :data3d.en_index     [     ind] , $
                               npts            :data3d.npts         [     ind] , $
                               index           :data3d.index        [     ind]}

           

        END
        3: BEGIN
           struct           = {data_name       :data3d.data_name    [     ind] , $
                               valid           :data3d.valid        [     ind] , $
                               project_name    :data3d.project_name [     ind] , $
                               units_name      :data3d.units_name              , $
                               units_procedure :data3d.units_procedure         , $
                               time            :data3d.time         [     ind] , $
                               end_time        :data3d.end_time     [     ind] , $
                               integ_t         :data3d.integ_t      [     ind] , $
                               nbins           :data3d.nbins        [     ind] , $
                               nenergy         :data3d.nenergy                 , $
                               data            :data3d.data         [*,*, ind] , $
                               ddata           :data3d.ddata        [*,*, ind] , $
                               energy          :data3d.energy       [*,*, ind] , $
                               theta           :data3d.theta        [*,*, ind] , $
                               geom            :data3d.geom         [*,*, ind] , $
                               denergy         :data3d.denergy      [*,*, ind] , $
                               dtheta          :data3d.dtheta       [*,   ind] , $
                               eff             :data3d.eff          [*,   ind] , $
                               mass            :data3d.mass                    , $
                               geomfactor      :data3d.geomfactor   [     ind] , $
                               header_bytes    :data3d.header_bytes [*,   ind] , $
                               st_index        :data3d.st_index     [     ind] , $
                               en_index        :data3d.en_index     [     ind] , $
                               npts            :data3d.npts         [     ind] , $
                               index           :data3d.index        [     ind]}

        END
     ENDCASE

  ENDELSE

  IF KEYWORD_SET(has_sc_pot) THEN STR_ELEMENT,struct,'sc_pot',data3d.sc_pot[ind],/ADD_REPLACE

  IF KEYWORD_SET(units) THEN CALL_PROCEDURE,data3d.units_procedure,UNITS=units

  RETURN,struct
END