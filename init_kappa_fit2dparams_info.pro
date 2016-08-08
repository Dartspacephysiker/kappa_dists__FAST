PRO INIT_KAPPA_FIT2DPARAMS_INFO,dens2D_params

  densMaxStep                 = 0.5
  denslimited                 = [1,1]
  densLimits                  = [1e-4,100]  

  dens2D_params               = {value:0.D       , $
                                 fixed:0         , $
                                 parname:''      , $
                                 ;; relstep:0.D     , $
                                 mpmaxstep:0.D   , $
                                 limited:[0,0]   , $
                                 limits:[0.D,0]}
  
  dens2D_params[*].mpmaxstep  = densMaxStep

  dens2D_params.limited[0]    = densLimited[0]
  dens2D_params.limited[1]    = densLimited[1]

  ;;What are the limits, then?
  dens2D_params.limits[0]     = densLimits[0]
  dens2D_params.limits[1]     = densLimits[1]

END