;2017/04/15
PRO CURANDPOT__SELECT_T_AND_N,jvPlotData,avgs_JVfit, $
                              TEMPERATURE=Temperature, $
                              DENSITY=Density, $
                              ERR_TEMPERATURE=TemperatureErr, $
                              ERR_DENSITY=DensityErr, $
                              DONT_MAP_SOURCEDENS=dont_map_sourceDens, $
                              THESE_USEINDS=these_useInds, $
                              SKIP_USEINDS=skip_useInds, $
                              ARRAYS=arrays

  COMPILE_OPT IDL2,STRICTARRSUBS

  CASE 1 OF
     KEYWORD_SET(these_useInds): BEGIN
        useInds           = these_useInds
     END
     KEYWORD_SET(avgs_JVfit) AND ~KEYWORD_SET(skip_useInds): BEGIN
        useInds              = avgs_JVfit.useInds
     END
     ELSE: BEGIN
        useInds              = LINDGEN(N_ELEMENTS(jvPlotData.time))
     END
  ENDCASE

  ;; IF KEYWORD_SET(avgs_JVfit) AND ~KEYWORD_SET(skip_useInds) THEN BEGIN
  ;;    useInds              = avgs_JVfit.useInds
  ;; ENDIF ELSE BEGIN
  ;;    useInds              = LINDGEN(N_ELEMENTS(jvPlotData.time))
  ;; ENDELSE

  IF KEYWORD_SET(arrays) THEN BEGIN


     ;;Temperature
     CASE 1 OF
        KEYWORD_SET(jvPlotData.use_source_temp): BEGIN

           Temperature    = jvPlotData.source.TDown[useInds]
           TemperatureErr = jvPlotData.source.TDownErr[useInds]

        END
        ELSE: BEGIN

           Temperature    = jvPlotData.TDown[useInds]
           TemperatureErr = jvPlotData.TDownErr[useInds]

        END
     ENDCASE

     ;;Density
     CASE 1 OF
        KEYWORD_SET(jvPlotData.use_source_dens): BEGIN

           CASE 1 OF
              KEYWORD_SET(dont_map_sourceDens): BEGIN

                 Density        = jvPlotData.source.NDown[useInds]
                 DensityErr     = jvPlotData.source.NDownErr[useInds]

              END
              ELSE: BEGIN
                 Density        = DENSITY_FACTOR__BARBOSA_1977(jvPlotData.only_downE_pot[useInds], $
                                                               Temperature, $
                                                               0, $
                                                               jvPlotData.source.NDown[useInds], $
                                                               jvPlotData.mRatio.R_B.FAST[useInds])

                 DensityErr     = DENSITY_FACTOR__BARBOSA_1977(jvPlotData.only_downE_pot[useInds], $
                                                               Temperature, $
                                                               0, $
                                                               jvPlotData.source.NDownErr[useInds], $
                                                               jvPlotData.mRatio.R_B.FAST[useInds])

              END
           ENDCASE
        END
        ELSE: BEGIN
           Density        = jvPlotData.NDown[useInds]
           DensityErr     = jvPlotData.NDownErr[useInds]
        END
     ENDCASE

  ENDIF ELSE BEGIN

     ;;Temperature
     IF KEYWORD_SET(avgs_JVfit.use_source_temp) THEN BEGIN
        Temperature = avgs_JVfit.T_SC.avg
     ENDIF ELSE BEGIN
        Temperature = avgs_JVfit.T.avg
     ENDELSE

     ;;Density
     IF KEYWORD_SET(avgs_JVfit.use_source_dens) THEN BEGIN

        CASE 1 OF
           KEYWORD_SET(dont_map_sourceDens): BEGIN
              Density     = avgs_JVfit.N_SC.avg
           END
           ELSE: BEGIN
              
              ;; Density     = DENSITY_FACTOR__BARBOSA_1977(10.D^(MEAN(ALOG10(jvPlotData.pot[useInds]))), $
              Density     = DENSITY_FACTOR__BARBOSA_1977(10.D^(MEAN(ALOG10(jvPlotData.only_downE_pot[useInds]))), $
                                                         Temperature, $
                                                         0, $
                                                         avgs_JVfit.N_SC.avg, $
                                                         MEAN(jvPlotData.mRatio.R_B.FAST[useInds]))
           END
        ENDCASE
     ENDIF ELSE BEGIN
        Density     = avgs_JVfit.N.avg
     ENDELSE

  ENDELSE



END
