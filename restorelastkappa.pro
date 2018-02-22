lastFile = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/lastFile.sav'

PRINT,"Restoring lastkappa stuff:"
PRINT,"A,AGauss,avgs_JVfit,jvPlotData,curPotList,cAP_struct,fitFile,KF2D__SDTData_opt,KF2D__Curvefit_opt,KF2D__strings,KF2D__plot_opt,electron_angleRange,energy_electrons,kappaFit1Ds,gaussFit1Ds,fit2DKappa_inf_list,fit2DGauss_inf_list,use_mpFit1D,nPkAbove_dEF_thresh,diffEflux_thresh,lowDens_thresh,highDens_thresh,chi2_over_dof_thresh,chi2_thresh"
PRINT,""
RESTORE,lastFile
PRINT,"fitFile: ",fitFile

canDo2D = ~KEYWORD_SET(only_1D_fits) AND $
          N_ELEMENTS(fit2DKappa_inf_list) GT 0
IF KEYWORD_SET(add_gaussian_estimate) THEN BEGIN & $
   canDo2D = canDo2D AND N_ELEMENTS(fit2DGauss_inf_list) GT 0 & $
ENDIF
  IF ~canDo2D THEN STOP

       ;;Proof that it's better with Papa John's

     ;; Shrink these fools?
     STR_ELEMENT_FROM_LIST_OF_STRUCTS,kappaFit1Ds,'time_index',VALUE=k1DFitsTimeInd
     STR_ELEMENT_FROM_LIST_OF_STRUCTS,gaussFit1Ds,'time_index',VALUE=g1DFitsTimeInd
     junk1DTimeInds = CGSETINTERSECTION(k1DFitsTimeInd,g1DFitsTimeInd, $
                                        POSITIONS=k1DTInds)
     junk1DTimeInds = CGSETINTERSECTION(g1DFitsTimeInd,k1DFitsTimeInd, $
                                        POSITIONS=g1DTInds)

     IF ~ARRAY_EQUAL(k1DTInds,g1DTInds) THEN STOP
     
     kappaFit1Ds = kappaFit1Ds[k1DTInds]
     gaussFit1Ds = gaussFit1Ds[g1DTInds]

     PARSE_KAPPA_FIT_STRUCTS,kappaFit1Ds, $
                             A=a, $
                             STRUCT_A=Astruct, $
                             TIME=kappaTime, $
                             NAMES_A=A_names, $
                             CHI2=chi2, $
                             PVAL=pVal, $
                             FITSTATUS=fitStatus, $
                             USE_MPFIT1D=use_mpFit1D

     PARSE_KAPPA_FIT_STRUCTS,gaussFit1Ds, $
                             A=AGauss, $
                             STRUCT_A=AStructGauss, $
                             TIME=gaussTime, $
                             NAMES_A=AGauss_names, $
                             CHI2=chi2Gauss, $
                             PVAL=pValGauss, $
                             FITSTATUS=gaussFit1DStatus, $
                             USE_MPFIT1D=use_mpFit1D

     STR_ELEMENT_FROM_LIST_OF_STRUCTS,kappaFit1Ds,'A',/PRESERVE_DIMENSIONALITY,VALUE=As
     STR_ELEMENT_FROM_LIST_OF_STRUCTS,GaussFit1Ds,'A',/PRESERVE_DIMENSIONALITY,VALUE=GaussAs
     ;; STR_ELEMENT_FROM_LIST_OF_STRUCTS,fit2dkappa_inf_list,'fitdens',VALUE=fitDens
     ;; STR_ELEMENT_FROM_LIST_OF_STRUCTS,fit2dkappa_inf_list,'SDT',VALUE=sdt
     ;; tid = SDT[*].time
     ;; these = VALUE_CLOSEST2(jvPlotdata.time,tid)
     ;; PRINT,jvplotdata.ndown[these]/fitDens
     ;; PRINT,jvPlotData.nDown[these]/As[*,3]

     kFit2DParam_struct = 1
     gFit2DParam_struct = 1

     fit2DK = PARSE_KAPPA_FIT2D_INFO_LIST_V2(fit2DKappa_inf_list, $
                                             SOUTH=south, $
                                             FIT_TYPE='Kappa', $
                                             HIGHDENSITY_THRESHOLD=highDens_thresh, $
                                             LOWDENSITY_THRESHOLD=lowDens_thresh, $
                                             CHI2_OVER_DOF_THRESHOLD=chi2_over_dof_thresh, $
                                             CHI2_THRESHOLD=chi2_thresh, $
                                             DIFFEFLUX_THRESHOLD=diffEflux_thresh, $
                                             N_PEAKS_ABOVE_DEF_THRESHOLD=nPkAbove_dEF_thresh, $
                                             OUT_GOOD_I=includeK_i, $
                                             OUT_FITPARAM_STRUCT=kFit2DParam_struct, $
                                             /DONT_SHRINK_PARSED_STRUCT)

     fit2DG = PARSE_KAPPA_FIT2D_INFO_LIST_V2(fit2DGauss_inf_list, $
                                             SOUTH=south, $
                                             FIT_TYPE='Maxwellian', $
                                             HIGHDENSITY_THRESHOLD=highDens_thresh, $
                                             LOWDENSITY_THRESHOLD=lowDens_thresh, $
                                             CHI2_OVER_DOF_THRESHOLD=chi2_over_dof_thresh, $
                                             CHI2_THRESHOLD=chi2_thresh, $
                                             DIFFEFLUX_THRESHOLD=diffEflux_thresh, $
                                             N_PEAKS_ABOVE_DEF_THRESHOLD=nPkAbove_dEF_thresh, $
                                             IN_GOOD_I=includeK_i, $
                                             OUT_GOOD_I=includeG_i, $
                                             OUT_FITPARAM_STRUCT=gFit2DParam_struct, $
                                             /DONT_SHRINK_PARSED_STRUCT) 
     
     PRINT_KAPPA_FIT2D_STATS_FOR_CURANDPOT_TRANGES,fit2DK,fit2DG,cAP_struct,jvPlotData;; , $
        ;; /ALSO_PARAM_STRUCTS, $
        ;; KFIT2DPARAM_STRUCT=kFit2DParam_struct, $
        ;; GFIT2DPARAM_STRUCT=gFit2DParam_struct

     IF ~( ARRAY_EQUAL(includeK_i,includeG_i)                          AND $
           (N_ELEMENTS(kappaFit1Ds)  EQ N_ELEMENTS(gaussFit1Ds)           ) AND $
           (N_ELEMENTS(kappaFit1Ds)  EQ N_ELEMENTS(fit2DKappa_inf_list) ) AND $
           (N_ELEMENTS(includeK_i) EQ N_ELEMENTS(fit2DKappa_inf_list) ) ) THEN BEGIN & $

        IF N_ELEMENTS(kappaFit1Ds) NE N_ELEMENTS(gaussFit1Ds) THEN STOP & $
        IF N_ELEMENTS(fit2DKappa_inf_list) NE N_ELEMENTS(fit2DGauss_inf_list) THEN STOP & $
        IF N_ELEMENTS(kappaFit1Ds) NE N_ELEMENTS(fit2DKappa_inf_list) THEN STOP & $

        include_i = CGSETINTERSECTION(includeK_i,includeG_i) & $

        fit2DK            = {SDT          : fit2DK.SDT         [include_i], $
                             fitParams    : fit2DK.fitParams   [*,include_i], $
                             obsMoms      : fit2DK.obsMoms     [include_i], $
                             fitMoms      : fit2DK.fitMoms     [include_i], $
                             moment_info  : fit2DK.moment_info [include_i], $
                             ;; fitDens      : fit2DK.fitDens     [include_i], $
                             chi2         : fit2DK.chi2        [include_i], $
                             errMsg       : fit2DK.errMsg      [include_i], $
                             status       : fit2DK.status      [include_i], $
                             nfEv         : fit2DK.nfEv        [include_i], $
                             ;; best_resid   : best_resid      [include_i], $
                             pFree_index  : fit2DK.pFree_index [*,include_i], $
                             ;; best_fJac    : best_fJac       [include_i], $
                             nPegged      : fit2DK.nPegged     [include_i], $
                             nFree        : fit2DK.nFree       [include_i], $
                             dof          : fit2DK.dof         [include_i], $
                             covar        : fit2DK.covar       [*,*,include_i], $
                             pError       : fit2DK.pError      [*,include_i], $
                             nIter        : fit2DK.nIter       [include_i]} & $
        
        fit2DG            = {SDT          : fit2DG.SDT         [include_i], $
                             fitParams    : fit2DG.fitParams   [*,include_i], $
                             obsMoms      : fit2DG.obsMoms     [include_i], $
                             fitMoms      : fit2DG.fitMoms     [include_i], $
                             moment_info  : fit2DG.moment_info [include_i], $
                             chi2         : fit2DG.chi2        [include_i], $
                             errMsg       : fit2DG.errMsg      [include_i], $
                             status       : fit2DG.status      [include_i], $
                             nfEv         : fit2DG.nfEv        [include_i], $
                             ;; best_resid   : best_resid      [include_i], $
                             pFree_index  : fit2DG.pFree_index [*,include_i], $
                             ;; best_fJac    : best_fJac       [include_i], $
                             nPegged      : fit2DG.nPegged     [include_i], $
                             nFree        : fit2DG.nFree       [include_i], $
                             dof          : fit2DG.dof         [include_i], $
                             covar        : fit2DG.covar       [*,*,include_i], $
                             pError       : fit2DG.pError      [*,include_i], $
                             nIter        : fit2DG.nIter       [include_i]} & $

        fit2DKappa_inf_list = fit2DKappa_inf_list[include_i] & $
        fit2DGauss_inf_list = fit2DGauss_inf_list[include_i] & $

        AStruct      = {bulk_energy : AStruct.bulk_energy[include_i], $
                        temperature : AStruct.temperature[include_i], $
                        kappa       : AStruct.kappa[include_i], $
                        N           : AStruct.N[include_i], $
                        bulk_angle  : AStruct.bulk_angle[include_i]} & $

        AStructGauss = {bulk_energy : AStructGauss.bulk_energy[include_i], $
                        temperature : AStructGauss.temperature[include_i], $
                        kappa       : AStructGauss.kappa[include_i], $
                        N           : AStructGauss.N[include_i], $
                        bulk_angle  : AStructGauss.bulk_angle[include_i]} & $

        kFit2DParam_struct = {bulk_energy  : kFit2DParam_struct.bulk_energy[include_i], $
                              temperature  : kFit2DParam_struct.temperature[include_i], $
                              kappa        : kFit2DParam_struct.kappa      [include_i], $
                              n            : kFit2DParam_struct.n          [include_i]} & $

        gFit2DParam_struct = {bulk_energy  : gFit2DParam_struct.bulk_energy[include_i], $
                              temperature  : gFit2DParam_struct.temperature[include_i], $
                              kappa        : gFit2DParam_struct.kappa      [include_i], $
                              n            : gFit2DParam_struct.n          [include_i]} & $

     ENDIF