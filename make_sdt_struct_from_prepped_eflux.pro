;2016/07/14 Now get the thing back
FUNCTION MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX,data3d,ind

  COMPILE_OPT idl2

  struct           = {data_name:data3d.data_name[ind], $
                           valid:data3d.valid[ind], $
                           project_name:data3d.project_name[ind], $
                           units_name:data3d.units_name, $
                           units_procedure:data3d.units_procedure, $
                           time:data3d.time[ind], $
                           end_time:data3d.end_time[ind], $
                           integ_t:data3d.integ_t[ind], $
                           nbins:data3d.nbins[ind], $
                           nenergy:data3d.nenergy, $
                           data:data3d.data[*,*,ind], $
                           ddata:data3d.ddata[*,*,ind], $
                           energy:data3d.energy[*,*,ind], $
                           theta:data3d.theta[*,*,ind], $
                           geom:data3d.geom[*,ind], $
                           denergy:data3d.denergy[*,*,ind], $
                           dtheta:data3d.dtheta[*,ind], $
                           eff:data3d.eff[*,ind], $
                           mass:data3d.mass, $
                           geomfactor:data3d.geomfactor[ind], $
                           header_bytes:data3d.header_bytes[*,ind], $
                           st_index:data3d.st_index[ind], $
                           en_index:data3d.en_index[ind], $
                           npts:data3d.npts[ind], $
                           index:data3d.index[ind]}
  RETURN,struct
END