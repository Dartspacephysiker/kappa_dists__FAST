;;2016/08/08 Called within KAPPA_FIT2D__TRY_EACH_1DFIT
PRO UPDATE_KAPPA_FIT2D_PARAM_INFO,kFit2DParamStruct,pre_densEst

  kFit2DParamStruct.value    = pre_densEst

  IF kFit2DParamStruct.value LT kFit2DParamStruct.limits[0] THEN BEGIN
     kFit2DParamStruct.value = kFit2DParamStruct.limits[0]
  END
  IF kFit2DParamStruct.value GT kFit2DParamStruct.limits[1] THEN BEGIN
     kFit2DParamStruct.value = kFit2DParamStruct.limits[1]
  END

END