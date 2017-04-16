;2017/04/15
PRO CURANDPOT__SELECT_T_AND_N,jvPlotData,avgs_JVfit, $
                              TEMPERATURE=Temperature, $
                              DENSITY=Density, $
                              ERR_TEMPERATURE=TemperatureErr, $
                              ERR_DENSITY=DensityErr, $
                              ARRAYS=arrays

  COMPILE_OPT IDL2,STRICTARRSUBS

  IF KEYWORD_SET(avgs_JVfit) THEN BEGIN
     useInds              = avgs_JVfit.useInds
  ENDIF ELSE BEGIN
     useInds              = LINDGEN(N_ELEMENTS(jvPlotData.time))
  ENDELSE

  IF KEYWORD_SET(arrays) THEN BEGIN

     CASE 1 OF
        KEYWORD_SET(jvPlotData.use_source_avgs): BEGIN
           Temperature    = jvPlotData.source.TDown[useInds]
           Density        = jvPlotData.source.NDown[useInds]/jvPlotData.mRatio.R_B_IGRF.FAST[useInds]
           TemperatureErr = jvPlotData.source.TDownErr[useInds]
           DensityErr     = jvPlotData.source.NDownErr[useInds]/jvPlotData.mRatio.R_B_IGRF.FAST[useInds]
        END
        ELSE: BEGIN
           Temperature    = jvPlotData.TDown[useInds]
           Density        = jvPlotData.NDown[useInds]
           TemperatureErr = jvPlotData.TDownErr[useInds]
           DensityErr     = jvPlotData.NDownErr[useInds]
        END
     ENDCASE

  ENDIF ELSE BEGIN

     IF KEYWORD_SET(avgs_JVfit.use_source_avgs) THEN BEGIN
        Temperature = avgs_JVfit.T_SC.avg
        Density     = avgs_JVfit.N_SC.avg/MEAN(jvPlotData.mRatio.R_B_IGRF.FAST[useInds])
     ENDIF ELSE BEGIN
        Temperature = avgs_JVfit.T.avg
        Density     = avgs_JVfit.N.avg
     ENDELSE

  ENDELSE



END
