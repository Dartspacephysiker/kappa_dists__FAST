;2016/07/15 Now get 'em all back
FUNCTION MAKE_ARRAY_OF_SDT_STRUCTS_FROM_PREPPED_EFLUX,data3d, $
   TIME_INDS=time_inds, $
   RECALCULATE_DDATA=recalculate_ddata, $
   PREPEND_DATA_NAME=prepend_data_name, $
   APPEND_DATA_NAME=append_data_name, $
   REPLACE_DATA_NAME=replace_data_name

  COMPILE_OPT IDL2,STRICTARRSUBS

  nStructs = N_ELEMENTS(data3d.data_name)
  keep_i   = INDGEN(nStructs)
  
  IF KEYWORD_SET(time_inds) THEN BEGIN
     nStructs = N_ELEMENTS(time_inds)
     keep_i   = time_inds
  ENDIF

  structArr = !NULL

  FOR i=0,nStructs-1 DO BEGIN
     structArr = [structArr,MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(data3d,keep_i[i])]
  ENDFOR

  CASE 1 OF
     KEYWORD_SET(replace_data_name): BEGIN
        FOR i=0,nStructs-1 DO BEGIN
           structArr[i].data_name = replace_data_name
        ENDFOR        
     END
     KEYWORD_SET(prepend_data_name): BEGIN
        FOR i=0,nStructs-1 DO BEGIN
           structArr[i].data_name = prepend_data_name + structArr[i].data_name
        ENDFOR        
     END
     KEYWORD_SET(append_data_name): BEGIN
        FOR i=0,nStructs-1 DO BEGIN
           structArr[i].data_name += append_data_name
        ENDFOR        
     END
     ELSE:
  ENDCASE

  IF KEYWORD_SET(recalculate_ddata) THEN BEGIN
     units = structArr[0].units_name

     FOR i=0,nStructs-1 DO BEGIN
        tmp3d                   = CONV_UNITS(structArr[i],'counts')
        tmp3d.ddata             = (tmp3d.data)^.5
        structArr[i]            = CONV_UNITS(tmp3d,units)
     ENDFOR
  ENDIF

  RETURN,structArr
END