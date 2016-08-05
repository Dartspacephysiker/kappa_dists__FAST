FUNCTION INIT_KAPPA_FITPARAM_INFO,A,

  COMPILE_OPT idl2

  parinfo = REPLICATE({value:0.D, $
                       fixed:0, $
                       parname:'', $
                       relstep:0.D, $
                       mpmaxstep:0.D, $
                       limited:[0,0], $
                       limits:[0.D,0]}, 7)

  Alimited         = [[1,1], $
                      [1,1], $
                      [1,0], $
                      [1,1], $
                      [0,0], $
                      [0,0], $
                      [1,1]]
  
  Alimits         = [[minE,maxE], $
                     [0,3.5e4], $
                     [1.5D,1e9], $
                     [0,100], $
                     [1,0], $
                     [0,0], $
                     [0,0], $
                     [-180,180]]

  RETURN,parInfo

END