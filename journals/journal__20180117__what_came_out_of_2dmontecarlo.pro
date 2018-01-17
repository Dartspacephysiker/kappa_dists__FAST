;2018/01/17
PRO JOURNAL__20180117__WHAT_CAME_OUT_OF_2DMONTECARLO

  COMPILE_OPT IDL2,STRICTARRSUBS

  saveDir = '/SPENCEdata/Research/Satellites/FAST/kappa_dists/saves_output_etc/'
  inFile  = 'orb1773_2DMCarlo_ests__09_26_56_synthetic.sav'

  RESTORE,saveDir+inFile

  STR_ELEMENT_FROM_LIST_OF_STRUCTS,fit2dkappa_inf_list,'fitparams',VALUE=kFit2DParms,/PRESERVE_DIMENSIONALITY

  CGHISTOPLOT,kFit2DParms[*,2]

  STOP


END
