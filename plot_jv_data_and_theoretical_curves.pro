;2017/03/18
PRO PLOT_JV_DATA_AND_THEORETICAL_CURVES,jvPlotData, $
                                        CURPOTLIST=curPotList, $
                                        MINPOT=minPot, $
                                        MAXPOT=maxPot, $
                                        MINCUR=minCur, $
                                        MAXCUR=maxCur, $
                                        USEINDS=useInds, $
                                        FIT_JE=fit_je, $
                                        FIT_BOTH=fit_both, $
                                        PLOT_J_RATIOS=plot_j_ratios, $
                                        PLOT_ION_ELEC_RATIOS=plot_ion_elec_ratios, $
                                        ORIGINATING_ROUTINE=routName, $
                                        PLOTDIR=plotDir, $
                                        SAVEPLOT=savePlot, $
                                        SPNAME=spName, $
                                        AVGS_FOR_FITTING=avgs_JVfit, $
                                        FIT_TIME_SERIES=fit_time_series, $
                                        FIT_TSERIES__A_IN=A_in, $
                                        KAPPALIMS=kappaLims, $   
                                        TEMPLIMS=TempLims, $    
                                        DENSLIMS=DensLims, $    
                                        MAGRATIOLIMS=magRatioLims, $
                                        _EXTRA=e

  COMPILE_OPT IDL2,STRICTARRSUBS

  IF KEYWORD_SET(plot_ion_elec_ratios) THEN BEGIN
     nListMem        = N_ELEMENTS(curPotList)         
     nHere           = N_ELEMENTS(curPotList[0].time)
     errorBarFac     = KEYWORD_SET(errorBarFac) ? errorBarFac : 1.

     looking         = nListMem
     ind             = 0

     WHILE (looking GT 0) DO BEGIN
        IF STRMATCH(STRUPCASE(curPotList[ind].label),'*DOWN*E') THEN BEGIN
           looking--
           edind = ind
        ENDIF

        IF STRMATCH(STRUPCASE(curPotList[ind].label),'*UP*E') THEN BEGIN
           looking--
           euind = ind
        ENDIF

        IF STRMATCH(STRUPCASE(curPotList[ind].label),'*UP*I') THEN BEGIN
           looking--
           iuind = ind
        ENDIF
        ind++
     ENDWHILE

     ji_je_ratio    = curPotList[iuind].cur/curPotList[edind].cur

  ENDIF

  ;; IF KEYWORD_SET(jvPlotData.use_source_avgs) THEN BEGIN
  ;;    curDat         = jvPlotData.source.cur*(-1.D) / (KEYWORD_SET(plot_j_ratios) ? jvPlotData.source.cur*(-1.D) : 1.D)
  ;;    divDat         = jvPlotData.source.cur*(-1.D)
  ;; ENDIF ELSE BEGIN
     curDat         = jvPlotData.cur*(-1.D) / (KEYWORD_SET(plot_j_ratios) ? jvPlotData.cur*(-1.D) : 1.D)
     divDat         = jvPlotData.cur*(-1.D)
  ;; ENDELSE
  
  ;; SAVE,KnightRelat30,KnightRelat300,KnightRelat3000,jvPlotData,FILENAME=
  ;; RESTORE,'
  CASE 1 OF
     KEYWORD_SET(fit_time_series): BEGIN

        maxIter     = 150
        fTol        = 1D-15
        gTol        = 1D-15

        IF KEYWORD_SET(jvPlotData.use_source_avgs) THEN BEGIN
           Temperature = avgs_JVfit.T_SC.avg
           Density     = avgs_JVfit.N_SC.avg
        ENDIF ELSE BEGIN
           Temperature = avgs_JVfit.T.avg
           Density     = avgs_JVfit.N.avg
        ENDELSE

        kappa_A     = KEYWORD_SET(A_in) ? A_in : [10, $          ;kappa
                                                  Temperature, $ ;Temp
                                                  Density, $     ;Dens
                                                  1D3]           ;R_B

        ;;Keep the original guesses
        Aorig       = kappa_A
        AGaussOrig  = kappa_A

        kappa_fixA  = [0,1,1,0]
        gauss_fixA  = [1,1,1,0]

        TmultFac__kappa    = [1]
        TmultFac__Maxwell  = [1]

        ;; PRINT,"Kappa startGuess: "
        ;; PRINT_JV_FIT_PARAMS,A
        ;; PRINT,"Gauss startGuess: "
        ;; PRINT_JV_FIT_PARAMS,AGaussOrig

        pot     = jvPlotData.pot[avgs_JVfit.useInds]
        cur     = jvPlotData.cur[avgs_JVfit.useInds]*(-1.D)
        potErr  = jvPlotData.potErr[avgs_JVfit.useInds]
        curErr  = jvPlotData.curErr[avgs_JVfit.useInds]
        CASE 1 OF
           KEYWORD_SET(jvPlotData.use_source_avgs): BEGIN
              T       = jvPlotData.source.TDown[avgs_JVfit.useInds]
              N       = jvPlotData.source.NDown[avgs_JVfit.useInds]
           END
           ELSE: BEGIN
              T       = jvPlotData.TDown[avgs_JVfit.useInds]
              N       = jvPlotData.NDown[avgs_JVfit.useInds]
           END
        ENDCASE

        FIT_JV_TS_WITH_THEORETICAL_CURVES,pot,cur, $
                                          potErr,curErr, $
                                          T,N, $
                                          ;; USEINDS=avgs_JVfit.useInds, $
                                          KAPPA_FIXA=kappa_fixA, $
                                          GAUSS_FIXA=gauss_fixA, $
                                          KAPPA_A=kappa_A, $
                                          GAUSS_A=Gauss_A, $
                                          MAXITER=maxIter, $
                                          FTOL=fTol, $
                                          GTOL=gTol, $
                                          OUT_FITKAPPA_A=fitKappa_A, $
                                          OUT_FITGAUSS_A=fitGauss_A, $
                                          KAPPALIMS=kappaLims, $   
                                          TEMPLIMS=TempLims, $    
                                          DENSLIMS=DensLims, $    
                                          MAGRATIOLIMS=magRatioLims

        kappas             = fitKappa_A[0]
        R_Bs__K            = fitKappa_A[3]
        R_Bs__M            = fitGauss_A[3]

     END
     ELSE: BEGIN

        ;; R_Bs__M         = [30,300,3000]
        R_Bs__M            = [100,10000,100,10000]
        ;; R_Bs__K         = [30,300,3000]
        ;; kappas          = [2.0,2.0,2.0,1.6]
        TmultFac__Maxwell  = [1,1,10,10]
        R_Bs__K            = [100,100,10000,10000]
        kappas             = [2.0,1.8,2.0,1.8]
        TmultFac__kappa    = [1,1,1,1]

     END
  ENDCASE

  nR_Bs__M                 = N_ELEMENTS(R_Bs__M)
  nR_Bs__K                 = N_ELEMENTS(R_Bs__K)
  nDer                     = N_ELEMENTS(useInds)

  maxwellJVs                  = MAKE_ARRAY(nR_Bs__M,nDer,/DOUBLE)
  kappaJVs                    = MAKE_ARRAY(nR_Bs__K,nDer,/DOUBLE)

  MaxwellTransp               = 30
  MaxwellSym                  = '*'
  MaxwellColors               = ['Red','Brown','Dark Green','Dark Orange']
  MaxwellLinestyle            = ['']
  MaxwellNames                = MAKE_ARRAY(nR_Bs__M,/STRING)

  kappaTransp                 = 30
  kappaSym                    = ['x','tu','+','td']
  kappaColors                 = ['Purple','Brown','Gray',"Violet"]
  kappaLinestyle              = ['']
  kappaNames                  = MAKE_ARRAY(nR_Bs__K,/STRING)

  FOR k=0,nR_Bs__M-1 DO BEGIN
     CASE 1 OF
        KEYWORD_SET(fit_je): BEGIN
           maxwellJVs[k,*] = KAPPA_1__DORS_KLETZING_EQ_14__EFLUX__MAXWELL(T, $
                                                                          N, $
                                                                          pot, $
                                                                          R_Bs__M[k], $
                                                                          MASS=mass)
        END
        ELSE: BEGIN
           maxwellJVs[k,*] = KNIGHT_RELATION__DORS_KLETZING_4(T, $
                                                              N, $
                                                              pot, $
                                                              R_Bs__M[k], $
                                                              /NO_MULT_BY_CHARGE)*1D6
        END
     ENDCASE


     ;; MaxwellNames[k] = 'R!DB!N = ' + STRING(FORMAT='(G0.4)',R_Bs__M[k])
     ;; MaxwellNames[k] = STRING(FORMAT='("R!DB!N = ",G0.4," (T*=",I0,")")', $
                            ;; R_Bs__M[k],TmultFac__Maxwell[k])
     MaxwellNames[k] = STRING(FORMAT='("R!DB!N = ",G0.4," (Maxwellian)")', $
                            R_Bs__M[k])

     IF KEYWORD_SET(plot_j_ratios) THEN BEGIN
        maxwellJVs[k,*] /= divDat[useInds]
     ENDIF
  ENDFOR

  FOR k=0,nR_Bs__K-1 DO BEGIN
     CASE 1 OF
        KEYWORD_SET(fit_je): BEGIN
           kappaJVs[k,*] = KAPPA_1__DORS_KLETZING_EQ_15__EFLUX(kappas[k], $
                                                               T, $
                                                               N, $
                                                               pot, $
                                                               R_Bs__K[k], $
                                                               MASS=mass)
        END
        ELSE: BEGIN
           kappaJVs[k,*] = KNIGHT_RELATION__DORS_KLETZING_11(kappas[k], $
                                                             T, $
                                                             N, $
                                                             pot, $
                                                             R_Bs__K[k], $
                                                             /NO_MULT_BY_CHARGE, $
                                                             MASS=mass)*1D6
        END
     ENDCASE
     
     ;; kappaNames[k] = STRING(FORMAT='("R!DB!N = ",G0.4," ($\kappa$=",F0.2,",T*=",I0,")")', $
                            ;; R_Bs__K[k],kappas[k],TmultFac__kappa[k])
     kappaNames[k] = STRING(FORMAT='("R!DB!N = ",G0.4," ($\kappa$=",F0.3,")")', $
                            R_Bs__K[k],kappas[k])

     IF KEYWORD_SET(plot_j_ratios) THEN BEGIN
        kappaJVs[k,*] /= divDat[useInds]
     ENDIF

  ENDFOR

  MaxwellPlots = MAKE_ARRAY(nR_Bs__M,/OBJ)
  kappaPlots   = MAKE_ARRAY(nR_Bs__K,/OBJ)
  window1      = WINDOW(DIMENSIONS=[1000,800], $
                        BUFFER=savePlot)
  yLog         = 0
  dataLStyle   = ''
  dataSym      = 'o'
  dataName     = 'Data'
  title        = KEYWORD_SET(fit_time_series) ? '$\kappa$ and R!DB!N are fit parameters!CT, n for each data point set by observations' : ''
  xTitle       = 'Potential (V)'
  yTitle       = KEYWORD_SET(plot_j_ratios) ? "J!D||,obs!N / J!D||,theor!N" : $
                 'Current density ($\mu$A/m!U2!N), mapped to 100km'

  ji_je_lStyle = ''
  ji_je_sym    = 's'
  ji_je_name   = 'J!Di!N over J!De!N'

  ;; dataplot     = ERRORPLOT(pot, $
  ;;                          curDat[useInds], $
  ;;                          curErr*1D-6, $
  dataplot     = PLOT(pot, $
                      curDat[useInds], $
                      LINESTYLE=dataLStyle, $
                      SYMBOL=dataSym, $
                      TITLE=title, $
                      XTITLE=xTitle, $
                      YTITLE=yTitle, $
                      NAME=dataName, $
                      YLOG=yLog, $
                      /CURRENT)

  FOR k=0,nR_Bs__M-1 DO BEGIN
     MaxwellPlots[k] = PLOT(pot, $
                            MaxwellJVs[k,*], $
                            TRANSPARENCY=MaxwellTransp, $
                            LINESTYLE=MaxwellLinestyle, $
                            SYMBOL=MaxwellSym, $
                            COLOR=MaxwellColors[k], $
                            /OVERPLOT, $
                            NAME=MaxwellNames[k])
  ENDFOR

  FOR k=0,nR_Bs__K-1 DO BEGIN
     kappaPlots[k] = PLOT(pot, $
                        kappaJVs[k,*], $
                        TRANSPARENCY=kappaTransp, $
                        LINESTYLE='', $
                        SYMBOL=kappaSym[k], $
                        COLOR=kappaColors[k], $
                        /OVERPLOT, $
                        NAME=kappaNames[k])
  ENDFOR

  legArr = [dataplot, $
            kappaPlots, $
            MaxwellPlots]

  IF KEYWORD_SET(ji_je_ratio) THEN BEGIN

     ji_je_plot = PLOT(pot[useInds], $
                       ji_je_ratio[useInds], $
                       LINESTYLE=ji_je_lStyle, $
                       SYMBOL=ji_je_sym, $
                       ;; XTITLE=xTitle, $
                       ;; YTITLE=yTitle, $
                       NAME=ji_je_name, $
                       YLOG=yLog, $
                       /CURRENT, $
                       /OVERPLOT)

     legArr = [legArr,ji_je_plot]

  ENDIF
  leg = LEGEND(TARGET=legArr)

  IF KEYWORD_SET(savePlot) THEN BEGIN

     IF ~KEYWORD_SET(sPName) THEN BEGIN
        sPName = routName + '-JV_Theor.png'
     ENDIF

     IF ~KEYWORD_SET(plotDir) THEN BEGIN
        pDirSuff      = '/cur_and_pot_analysis'
        SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF=pDirSuff
     ENDIF

     PRINT,"Saving to " + sPName + ' ...'

     window1.Save,plotDir+sPName

  ENDIF

END
