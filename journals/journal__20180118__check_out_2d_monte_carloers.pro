;2018/01/18
;; FRAC         : fraction of values needed
FUNCTION MOST_PROB_FOR_MC_PARMS,parmArr,parmStep,binSize

  COMPILE_OPT IDL2,STRICTARRSUBS

  mostProbVal  = MEDIAN(parmArr)

  hist         = HISTOGRAM(parmArr,BINSIZE=binSize,LOCATIONS=bins)
  junk         = MAX(hist,mostProbInd)
  mostProbVal  = bins[mostProbInd]+binSize/2.

  RETURN,mostProbVal
  
END
FUNCTION GET_SPREAD_FOR_MC_PARMS,mostProbVal,parmArr,parmStep,parmMinVal,binSize,frac, $
                                 N=N

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; initialize
  boundStep    = [-parmStep,parmStep]
  tmpBound     = mostProbVal + boundStep

  captured     = N_ELEMENTS( WHERE((parmArr GE tmpBound[0]) AND (parmArr LE tmpBound[1]),/NULL) )/FLOAT(N)

  ;; Loop until we've got it
  nTry = 0
  WHILE captured LT frac DO BEGIN

     tmpBound  = (tmpBound + boundStep) > parmMinVal
     captured  = N_ELEMENTS( WHERE((parmArr GE tmpBound[0]) AND (parmArr LE tmpBound[1]),/NULL) )/FLOAT(N)

     nTry++

     ;; IF ~(nTry MOD 10) THEN PRINT,FORMAT='("try ",I04,": ",F0.3,"/",F0.2)', $
     ;;                             nTry, $
     ;;                             captured, $
     ;;                             frac

     IF nTry EQ  1000 THEN PRINT,"Hit 1000 ..."
     IF nTry EQ  2000 THEN PRINT,"Hit 2000 ..."
     IF nTry EQ 30000 THEN BEGIN
        PRINT,"Hit 30000! STOP"
        PRINT,"Parmbounds: ",tmpBound[0],", ",tmpBound[1]
        STOP
     ENDIF

  ENDWHILE

  tmpBound -= mostProbVal

  RETURN,tmpBound

END  
PRO JOURNAL__20180118__CHECK_OUT_2D_MONTE_CARLOERS

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; nRolls  = 1000
  nRolls  = 10000               ;2018/01/19
  nRollStr = STRING(FORMAT='(I0,"Rolls")',nRolls)

  inDir   = '/SPENCEdata/Research/Satellites/FAST/kappa_dists/saves_output_etc/20180119/'
  ;; inDir   = '/SPENCEdata/Research/Satellites/FAST/kappa_dists/saves_output_etc/20180118/' + nRollStr + '/'
  filPref = 'orb1773_2DMCarlo_ests__'
  filSuff = '_synthetic_wGauss-' + nRollStr + '-fit2DParams.sav'

  ;; The file that provides the original fit stuff
  dir = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/'
  fil = '20180117-orb_1773-Kappa_fits_and_Gauss_fits-ees-horseshoe2d-classics-3-Elphic_et_al_1998-0BelowPk-GRLRESPONSEFINAL2-only_fit_peak_eRange-avg_itvl2.sav'

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
  ;; outFil = outFil.Replace('20180117',GET_TODAY_STRING(/DO_YYYYMMDD_FMT))
  outFil = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + $
           STRMID(outFil,8,STRLEN(outFil)-8)
  outFil = outFil.Replace('.sav','-'+nRollStr+'.sav')

  SPAWN,'cd ' + inDir + '; ls ' + filPref + '*' + filSuff,fileList

  ;; Histogram bin sizes
  kBinSize      = 0.05
  TBinSize      = 10
  BulkEBinSize  = 10
  NBinSize      = 0.02

  ;; stepsizes for getting percentages
  kStepSize     = 0.01
  TStepSize     = 2.5
  BulkEStepSize = 2.5
  NStepSize     = 0.002

  ;; min set by physics
  kMin          = KEYWORD_SET(make_2D_uncert) ? 1.5 : 0.0
  TMin          = 0.
  bulkEMin      = 0.
  NMin          = 0.

  ;;physical limits of various quants
  lowLimK       = [bulkEMin,TMin,kMin,NMin]
  lowLimG       = [bulkEMin,TMin     ,NMin]

  nHere = N_ELEMENTS(fileList)

  ;; Assemble bin and step sizes
  binSizesK  = [bulkEBinSize ,TBinSize ,kBinSize ,NBinSize ]
  stepSizesK = [bulkEStepSize,TStepSize,kStepSize,NStepSize]

  binSizesG  = [bulkEBinSize ,TBinSize ,NBinSize ]
  stepSizesG = [bulkEStepSize,TStepSize,NStepSize]


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
     fKSprArr = MAKE_ARRAY(2,4,nHere,/FLOAT)
     fGSprArr = MAKE_ARRAY(2,3,nHere,/FLOAT)

     nBadKArr = MAKE_ARRAY(nHere,/LONG  )
     nBadGArr = MAKE_ARRAY(nHere,/LONG  )
     tidKArr  = MAKE_ARRAY(nHere,/DOUBLE)
     tidGArr  = MAKE_ARRAY(nHere,/DOUBLE)

     IF KEYWORD_SET(make_2D_uncert) THEN BEGIN

        multFac = [-1.,1.]

        fKMostProb = MAKE_ARRAY(4,nHere,/FLOAT)
        fGMostProb = MAKE_ARRAY(3,nHere,/FLOAT)

        fKSprArr   = MAKE_ARRAY(2,4,nHere,/FLOAT)
        fGSprArr   = MAKE_ARRAY(2,3,nHere,/FLOAT)

     ENDIF ELSE BEGIN

        multFac = 1.

        fKSprArr = MAKE_ARRAY(4,nHere,/FLOAT)
        fGSprArr = MAKE_ARRAY(3,nHere,/FLOAT)

     ENDELSE
     
     IF KEYWORD_SET(make_2D_uncert) THEN $
        PRINT,FORMAT='(A8,TR3,A8,TR3,A7,TR3,A6,TR5,A8,TR3,A8,TR3,A6)', $
              "KBulkE","KTemp","kappa","K N","GBulkE","GTemp","G N"
     
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
        K2Dparm = fit2DKappa_info.fitParams
        g2Dparm = fit2DGauss_info.fitParams

        K2Dparm[3] = fit2DKappa_info.fitmoms.scDens
        g2Dparm[3] = fit2DGauss_info.fitmoms.scDens

        fKArr = TEMPORARY(kappaFit2DParamArr)
        fGArr = TEMPORARY(gaussFit2DParamArr)
        
        nfKHere  = N_ELEMENTS(fKArr[0,*])
        nfKParms = N_ELEMENTS(fKArr[*,0] )

        nfGHere  = N_ELEMENTS(fGArr[0,*])
        nfGParms = N_ELEMENTS(fGArr[*,0] )
        
        ;; Some checks
        ;; IF nfKHere  NE nRolls THEN nBadKArr[k] = nRolls - nfKHere
        IF nfKHere  NE nRolls THEN STOP
        IF nfKParms NE 5      THEN STOP

        ;; IF nfGHere  NE nRolls THEN nBadGArr[k] = nRolls - nfGHere
        IF nfGHere  NE nRolls THEN STOP
        IF nfGParms NE 5      THEN STOP

        ;; Shrink Gaussian fit2D array to drop kappa, which is always 100 for Gaussian fits
        fGArr   = [fGArr[0,*],fGArr[1,*],fGArr[3,*]]
        g2DParm = [g2DParm[0],g2DParm[1],g2DParm[3]]

        CASE 1 OF
           KEYWORD_SET(make_2D_uncert): BEGIN

              FOR kk=0,3 DO BEGIN
                 
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

              FOR kk=0,2 DO BEGIN
                 
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

              PRINT,FORMAT='(G8.4,TR3,G8.4,TR3,F7.3,TR3,F6.3,TR5,G8.4,TR3,G8.4,TR3,F6.3)', $
                    fKMostProb[0,k], $
                    fKMostProb[1,k], $
                    fKMostProb[2,k], $
                    fKMostProb[3,k], $
                    fGMostProb[0,k], $
                    fGMostProb[1,k], $
                    fGMostProb[2,k]

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
                          N           : REFORM(fKMostProb[3,*])}

           mostProbG   = {bulk_energy : REFORM(fGMostProb[0,*]), $
                          temperature : REFORM(fGMostProb[1,*]), $
                          N           : REFORM(fGMostProb[2,*])}

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
                          mostProb    : mostProbK}

           g2DParmErr  = {time        : tidGArr, $
                          bulk_energy : ABS(REFORM(fGSprArr[*,0,*],2,nHere)), $
                          temperature : ABS(REFORM(fGSprArr[*,1,*],2,nHere)), $
                          N           : ABS(REFORM(fGSprArr[*,2,*],2,nHere)), $
                          mostProb    : mostProbG}

        END
        ELSE: BEGIN


           k2DParmErr  = {time        : tidKArr, $
                          bulk_energy : REFORM(fKSprArr[0,*],nHere), $
                          temperature : REFORM(fKSprArr[1,*],nHere), $
                          kappa       : REFORM(fKSprArr[2,*],nHere), $
                          N           : REFORM(fKSprArr[3,*],nHere)}

           g2DParmErr  = {time        : tidGArr, $
                          bulk_energy : REFORM(fGSprArr[0,*],nHere), $
                          temperature : REFORM(fGSprArr[1,*],nHere), $
                          N           : REFORM(fGSprArr[2,*],nHere)}


        END
     ENDCASE

     STOP
     PRINT,"Saving to " + outFil + " ..."
     SAVE,k2DParmErr,g2DParmErr,FILENAME=dir+outFil

  ENDIF

END
