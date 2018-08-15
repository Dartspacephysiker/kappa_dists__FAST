;2018/04/13
;; FRAC         : fraction of values needed
PRO JOURNAL__20180809__ORB1607__2D_MONTE_CARLOERS

  COMPILE_OPT IDL2,STRICTARRSUBS

  nRolls  = 10000               ;2018/04/13
  nRollStr = STRING(FORMAT='(I0,"Rolls")',nRolls)

  sRate = 0.95
  avgItvlStr = (STRING(FORMAT='("-sRate",F4.2)',sRate)).Replace(".","_")

  inDir   = '/SPENCEdata/Research/Satellites/FAST/kappa_dists/saves_output_etc/20180815/'
  filPref = 'orb1607_2DMCarlo_ests__'
  filSuff = '_synthetic_wGauss-' + nRollStr + '-fit2DParams' + avgItvlStr + '.sav'

  ;; The file that provides the original fit stuff
  dir = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/'
  fil = '20180815-orb_1607-KandGfits-ees-2NDKAPPA-only_fit_peak_eRange-sRate0_95-01_03_53__988-01_06_15__000.sav'

  calcUncertaintyBars = 1

  ;; CAPTURE_FRAC is for a mode that I decided not to use
  ;; it involves sweeping out from the most probable value until CAPTURE_FRAC of
  ;; param estimates are within the boundaries
  ;;
  capture_frac  = .90 

  make_2D_uncert = 1
  ;; make_1D_uncert = 1


  showPlots     = 0

  CASE 1 OF
     KEYWORD_SET(make_2D_uncert): BEGIN
        outSuff = '-2DPARMERRORS_TWOSIDED.sav'
     END
     ELSE: BEGIN
        outSuff = '-2DPARMERRORS.sav'
     END
  ENDCASE
  outFil = fil.Replace(".sav",outSuff)
  outFil = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + $
           STRMID(outFil,8,STRLEN(outFil)-8)
  outFil = outFil.Replace('.sav','-'+nRollStr+'.sav')

  SPAWN,'cd ' + inDir + '; ls ' + filPref + '*' + filSuff,fileList

  ;; Histogram bin sizes
  BulkEBinSize  = 5.
  TBinSize      = 5.
  NBinSize      = 0.02
  kBinSize      = 0.05

  ;; stepsizes for getting percentages
  BulkEStepSize = 2.5
  TStepSize     = 2.5
  NStepSize     = 0.01
  kStepSize     = 0.025

  ;; min set by physics
  kMin          = KEYWORD_SET(make_2D_uncert) ? 1.5 : 0.0
  TMin          = 0.
  bulkEMin      = 0.
  NMin          = 0.

  ;;physical limits of various quants
  lowLimK       = [bulkEMin,TMin,kMin,NMin,NMin]
  lowLimG       = [bulkEMin,TMin     ,NMin,NMin]

  nHere = N_ELEMENTS(fileList)

  ;; Assemble bin and step sizes
  binSizesK  = [bulkEBinSize ,TBinSize ,kBinSize ,NBinSize ,NBinSize ]
  stepSizesK = [bulkEStepSize,TStepSize,kStepSize,NStepSize,NStepSize]

  binSizesG  = [bulkEBinSize ,TBinSize ,NBinSize ,NBinSize ]
  stepSizesG = [bulkEStepSize,TStepSize,NStepSize,NStepSize]


  IF KEYWORD_SET(showPlots) THEN BEGIN

     FOR k=0,nHere-1 DO BEGIN
        
        RESTORE,inDir+fileList[k]

        STOP

        fKArr = TEMPORARY(kappaFit2DParamArr)
        ;; fGArr = TEMPORARY(gaussFit2DParamArr)

        ;; Get histograms

        ;; Plot histograms

        ;; Plot vertical line showing actual best-fit parms

     ENDFOR

  ENDIF

  IF KEYWORD_SET(calcUncertaintyBars) THEN BEGIN

     ;; Set up arrays with spreads
     fKSprArr = MAKE_ARRAY(2,5,nHere,/FLOAT)
     fGSprArr = MAKE_ARRAY(2,4,nHere,/FLOAT)

     nBadKArr = MAKE_ARRAY(nHere,/LONG  )
     nBadGArr = MAKE_ARRAY(nHere,/LONG  )
     tidKArr  = MAKE_ARRAY(nHere,/DOUBLE)
     tidGArr  = MAKE_ARRAY(nHere,/DOUBLE)

     IF KEYWORD_SET(make_2D_uncert) THEN BEGIN

        multFac = [-1.,1.]

        fKMostProb = MAKE_ARRAY(5,nHere,/FLOAT)
        fGMostProb = MAKE_ARRAY(4,nHere,/FLOAT)

        fKSprArr   = MAKE_ARRAY(2,5,nHere,/FLOAT)
        fGSprArr   = MAKE_ARRAY(2,4,nHere,/FLOAT)

     ENDIF ELSE BEGIN

        multFac = 1.

        fKSprArr = MAKE_ARRAY(5,nHere,/FLOAT)
        fGSprArr = MAKE_ARRAY(4,nHere,/FLOAT)

     ENDELSE
     
     IF KEYWORD_SET(make_2D_uncert) THEN $
        PRINT,FORMAT='(A8,TR3,A8,TR3,A7,TR3,A6,TR3,A6,TR5,A8,TR3,A8,TR3,A6,TR3,A6)', $
              "KBulkE","KTemp","kappa","K N","K N2","GBulkE","GTemp","G N","G N2"
     
     FOR k=0,nHere-1 DO BEGIN
        
        RESTORE,inDir+fileList[k]

        IF fit2DKappa_info.SDT.time NE fit2DGauss_info.SDT.time THEN STOP
        tidKArr[k] = fit2DKappa_info.SDT.time
        tidGArr[k] = fit2DGauss_info.SDT.time
           
        PRINT,FORMAT='(I03," : ",A0," (",F0.5,")")', $
              k, $
              STRMID(fileList[k],STRLEN(filPref),13), $
              tidKArr[k]-tidKArr[(k-1)>0]

        ;; Pick up original fitParams
        ;; K2Dparm = fit2DKappa_info.fitParams
        ;; g2Dparm = fit2DGauss_info.fitParams

        ;; K2Dparm[3] = fit2DKappa_info.fitmoms.scDens
        ;; g2Dparm[3] = fit2DGauss_info.fitmoms.scDens

        fKArr = TEMPORARY(kappaFit2DParamArr)
        fGArr = TEMPORARY(gaussFit2DParamArr)
        
        nfKHere  = N_ELEMENTS(fKArr[0,*])
        nfKParms = N_ELEMENTS(fKArr[*,0] )

        nfGHere  = N_ELEMENTS(fGArr[0,*])
        nfGParms = N_ELEMENTS(fGArr[*,0] )
        
        ;; Some checks
        ;; IF nfKHere  NE nRolls THEN nBadKArr[k] = nRolls - nfKHere
        IF nfKHere  NE nRolls THEN STOP
        IF nfKParms NE 6      THEN STOP

        ;; IF nfGHere  NE nRolls THEN nBadGArr[k] = nRolls - nfGHere
        IF nfGHere  NE nRolls THEN STOP
        IF nfGParms NE 6      THEN STOP

        ;; Shrink kappa fit2D array to drop bogus angle thing
        fKArr   = [fKArr[0,*],fKArr[1,*],fKArr[2,*],fKArr[3,*],fKArr[5,*]]

        ;; Shrink Gaussian fit2D array to drop bogus angle and kappa, which is always 100 for Gaussian fits
        fGArr   = [fGArr[0,*],fGArr[1,*],fGArr[3,*],fGArr[5,*]]
        ;; g2DParm = [g2DParm[0],g2DParm[1],g2DParm[3]]

        CASE 1 OF
           KEYWORD_SET(make_2D_uncert): BEGIN

              FOR kk=0,4 DO BEGIN
                 
                 fKMostProb[kk,k] = MOST_PROB_FOR_MC_PARMS(fKArr[kk,*],stepSizesK[kk],binSizesK[kk])
                 fKSprArr[*,kk,k] = GET_SPREAD_FOR_MC_PARMS(fKMostProb[kk,k], $
                                                            fKArr[kk,*], $
                                                            stepSizesK[kk], $
                                                            lowLimK[kk], $
                                                            binSizesK[kk], $
                                                            capture_frac, $
                                                            N=nRolls)
                 ;; fKSprArr[*,kk,k] = REPLICATE(STDDEV(fKArr[kk,*]),2)
                 ;; fKSprArr[*,kk,k] = k2DParm[kk] + STDDEV(fKArr[kk,*]) * multFac > lowLimK[kk]

              ENDFOR

              FOR kk=0,3 DO BEGIN
                 
                 fGMostProb[kk,k] = MOST_PROB_FOR_MC_PARMS(fGArr[kk,*],stepSizesG[kk],binSizesG[kk])
                 fGSprArr[*,kk,k] = GET_SPREAD_FOR_MC_PARMS(fGMostProb[kk,k], $
                                                            fGArr[kk,*], $
                                                            stepSizesG[kk], $
                                                            lowLimG[kk], $
                                                            binSizesG[kk], $
                                                            capture_frac, $
                                                            N=nRolls)
                 ;; fGSprArr[*,kk,k] = REPLICATE(STDDEV(fGArr[kk,*]),2)
                 ;; fGSprArr[*,kk,k] = g2DParm[kk] + STDDEV(fGArr[kk,*]) * multFac > lowLimG[kk]

              ENDFOR

              PRINT,FORMAT='(G8.4,TR3,G8.4,TR3,F7.3,TR3,F6.3,TR3,F6.3,TR5,G8.4,TR3,G8.4,TR3,F6.3,TR3,F6.3)', $
                    fKMostProb[0,k], $
                    fKMostProb[1,k], $
                    fKMostProb[2,k], $
                    fKMostProb[3,k], $
                    fKMostProb[4,k], $
                    fGMostProb[0,k], $
                    fGMostProb[1,k], $
                    fGMostProb[2,k], $
                    fGMostProb[3,k]

           END
           ELSE: BEGIN

              FOR kk=0,3 DO BEGIN
                 
                 ;; fKSprArr[kk,k] = GET_SPREAD_FOR_MC_PARMS(fKArr[kk,*],stepSizesK[kk],capture_frac,N=nRolls)
                 fKSprArr[kk,k] = STDDEV(fKArr[kk,*]) > lowLimK[kk]
                 ;; fKSprArr[kk,k] = (k2DParm[kk] + STDDEV(fKArr[kk,*])) > lowLimK[kk]

              ENDFOR

              FOR kk=0,2 DO BEGIN
                 
                 ;; fGSprArr[kk,k] = GET_SPREAD_FOR_MC_PARMS(fGArr[kk,*],stepSizesG[kk],capture_frac,N=nRolls)
                 fGSprArr[kk,k] = STDDEV(fGArr[kk,*]) > lowLimG[kk]
                 ;; fGSprArr[kk,k] = (g2DParm[kk] + STDDEV(fGArr[kk,*]))  > lowLimG[kk]

              ENDFOR

           END
        ENDCASE

     ENDFOR

     CASE 1 OF
        KEYWORD_SET(make_2D_uncert): BEGIN

           mostProbK   = {bulk_energy : REFORM(fKMostProb[0,*]), $
                          temperature : REFORM(fKMostProb[1,*]), $
                          kappa       : REFORM(fKMostProb[2,*]), $
                          N           : REFORM(fKMostProb[3,*]), $
                          N2          : REFORM(fKMostProb[4,*])}

           mostProbG   = {bulk_energy : REFORM(fGMostProb[0,*]), $
                          temperature : REFORM(fGMostProb[1,*]), $
                          N           : REFORM(fGMostProb[2,*]), $
                          N2          : REFORM(fGMostProb[3,*])}

           ;; k2DParmErr  = {time        : tidKArr, $
           ;;                bulk_energy : REFORM(fKSprArr[*,0,*],2,nHere), $
           ;;                temperature : REFORM(fKSprArr[*,1,*],2,nHere), $
           ;;                kappa       : REFORM(fKSprArr[*,2,*],2,nHere), $
           ;;                N           : REFORM(fKSprArr[*,3,*],2,nHere), $
           ;;                mostProb    : mostProbK}

           ;; g2DParmErr  = {time        : tidGArr, $
           ;;                bulk_energy : REFORM(fGSprArr[*,0,*],2,nHere), $
           ;;                temperature : REFORM(fGSprArr[*,1,*],2,nHere), $
           ;;                N           : REFORM(fGSprArr[*,2,*],2,nHere), $
           ;;                mostProb    : mostProbG}

           ;; Abs vals
           k2DParmErr  = {time        : tidKArr, $
                          bulk_energy : ABS(REFORM(fKSprArr[*,0,*],2,nHere)), $
                          temperature : ABS(REFORM(fKSprArr[*,1,*],2,nHere)), $
                          kappa       : ABS(REFORM(fKSprArr[*,2,*],2,nHere)), $
                          N           : ABS(REFORM(fKSprArr[*,3,*],2,nHere)), $
                          N2          : ABS(REFORM(fKSprArr[*,4,*],2,nHere)), $
                          mostProb    : mostProbK}

           g2DParmErr  = {time        : tidGArr, $
                          bulk_energy : ABS(REFORM(fGSprArr[*,0,*],2,nHere)), $
                          temperature : ABS(REFORM(fGSprArr[*,1,*],2,nHere)), $
                          N           : ABS(REFORM(fGSprArr[*,2,*],2,nHere)), $
                          N2          : ABS(REFORM(fGSprArr[*,3,*],2,nHere)), $
                          mostProb    : mostProbG}

        END
        ELSE: BEGIN


           k2DParmErr  = {time        : tidKArr, $
                          bulk_energy : REFORM(fKSprArr[0,*],nHere), $
                          temperature : REFORM(fKSprArr[1,*],nHere), $
                          kappa       : REFORM(fKSprArr[2,*],nHere), $
                          N           : REFORM(fKSprArr[3,*],nHere), $
                          N2          : REFORM(fKSprArr[4,*],nHere)}

           g2DParmErr  = {time        : tidGArr, $
                          bulk_energy : REFORM(fGSprArr[0,*],nHere), $
                          temperature : REFORM(fGSprArr[1,*],nHere), $
                          N           : REFORM(fGSprArr[2,*],nHere), $
                          N2          : REFORM(fGSprArr[3,*],nHere)}


        END
     ENDCASE

     PRINT,FORMAT='(A5,TR3,A8,TR3,A8,TR3,A8,TR3,A7,TR3,A6,TR3,A6,TR5,A8,TR3,A8,TR3,A6,TR3,A6)', $
           "Ind","Time","KBulkE","KTemp","kappa","K N","K N2","GBulkE","GTemp","G N","G N2"
     ;; PRINT,FORMAT='(A8,TR3,A8,TR3,A7,TR3,A6,TR3,A6,TR5,A8,TR3,A8,TR3,A6,TR3,A6)', $
     ;;       "KBulkE","KTemp","kappa","K N","K N2","GBulkE","GTemp","G N","G N2"
     FOR k=0,nHere-1 DO BEGIN
        PRINT,FORMAT='(I5,TR3,A8,TR3,G8.4,TR3,G8.4,TR3,F7.3,TR3,F6.3,TR3,F6.3,TR5,G8.4,TR3,G8.4,TR3,F6.3,TR3,F6.3)', $
              k, $
              STRMID(fileList[k],STRLEN(filPref),13), $
              k2DParmErr.mostProb.(0)[k], $
              k2DParmErr.mostProb.(1)[k], $
              k2DParmErr.mostProb.(2)[k], $
              k2DParmErr.mostProb.(3)[k], $
              k2DParmErr.mostProb.(4)[k], $
              g2DParmErr.mostProb.(0)[k], $
              g2DParmErr.mostProb.(1)[k], $
              g2DParmErr.mostProb.(2)[k], $
              g2DParmErr.mostProb.(3)[k]

     ENDFOR


     STOP
     PRINT,"Saving to " + outFil + " ..."
     SAVE,k2DParmErr,g2DParmErr,FILENAME=dir+outFil

  ENDIF

END

