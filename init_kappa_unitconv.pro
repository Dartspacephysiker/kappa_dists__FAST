; 2016/09/02
; 2018/04/06 Basically … it's deprecated. As far as I can tell, only KAPPA_FLUX2D__HORSESHOE
PRO INIT_KAPPA_UNITCONV,curDataStr

  COMPILE_OPT IDL2,STRICTARRSUBS


  ;; @kappa_unitconversion_common.pro

  U__init     = 1
  U__geom     = curDataStr.geom
  IF NDIMEN(U__geom) EQ 0 THEN U__geom = [U__geom]
  IF NDIMEN(U__geom) EQ 1 THEN U__geom = REPLICATE(1.,curDataStr.nEnergy) # U__geom

  U__gf       = curDataStr.geomfactor
  U__dt       = curDataStr.integ_t
  U__mass     = curDataStr.mass
  U__dead     = .11e-6

  U__energies = curDataStr.energy

END