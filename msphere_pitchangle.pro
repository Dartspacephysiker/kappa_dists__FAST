;+
; NAME:                 MSPHERE_PITCHANGLE
;
;
;
; PURPOSE:              I can see which angle constitutes the loss cone in FAST data. Assuming a dipole magnetic field, what should the
;                            pitch angle be in the magnetosphere?
;
;
;
; CATEGORY:
;
;
;
; INPUTS:              IONOS_PITCHANGLE       : Pitch angle in the ionosphere (in degrees!)
;                      B_RATIO                : Ratio of B-field in the ionosphere to B-field in the magnetosphere
;
;
; OPTIONAL INPUTS:
;
;
;
; KEYWORD PARAMETERS:
;
;
;
; OUTPUTS:
;
;
;
; OPTIONAL OUTPUTS:
;
;
;
; COMMON BLOCKS:
;
;
;
; SIDE EFFECTS:
;
;
;
; RESTRICTIONS:
;
;
;
; PROCEDURE:
;
;
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:   2016/07/23 Barnebarn
;
;-
;;07/23/16
;;En route to Frankfurt (fastest flight of my life!)

FUNCTION MSPHERE_PITCHANGLE,ionos_pitchAngle,B_ratio

  COMPILE_OPT IDL2,STRICTARRSUBS

  RETURN,ASIN(SQRT(1D/B_ratio)*ABS(SIN(ionos_pitchAngle/!RADEG)))*!RADEG


END
