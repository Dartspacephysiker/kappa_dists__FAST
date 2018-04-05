;2018/03/01
PRO PRINT_J_V_INFO_FOR_MATHEMATICA, $
   JVPLOTDATA=jvPlotData, $
   AVGS_JVFIT=avgs_jvFit, $
   KFIT2DPARAM_STRUCT=kFit2DParam_struct, $
   GFIT2DPARAM_STRUCT=gFit2DParam_struct, $
   FIT2DKAPPA_INF_LIST=fit2DKappa_inf_list, $
   FIT2DGAUSS_INF_LIST=fit2DGauss_inf_list, $
   KAPPA2D=fit2DK, $
   GAUSS2D=fit2DG, $
   CAP_STRUCT=cAP_struct

  COMPILE_OPT IDL2,STRICTARRSUBS

  nHer = N_ELEMENTS(jvPlotData.time)
  inds = LINDGEN(nHer)
  nHerStr = STRING(FORMAT='(I0)',nHer)

  ;; PRINT,FORMAT='("time={",' + nHerStr + '(A25,:,", "),"}")','"'+T2S(jvPlotData.time[inds],/MS)+'"'
  PRINT,STRING(FORMAT='("time={",' + nHerStr + '(A0,:,", "),"}")','"'+STRMID(T2S(jvPlotData.time[inds],/MS),11,23)+'"')+'};'
  PRINT,STRING(FORMAT='("pots={",' + nHerStr + '(F-0.2,:,", "),"}")',jvPlotData.pot[inds])+'};'
  PRINT,STRING(FORMAT='("curs={",' + nHerStr + '(F-0.3,:,", "),"}")',jvPlotData.cur[inds]*(-1.D))+'};'
  PRINT,STRING(FORMAT='("curErrs={",' + nHerStr + '(F-0.3,:,", "),"}")',jvPlotData.curErr[inds])+'};'
  PRINT,STRING(FORMAT='("jes={",' + nHerStr + '(F-0.3,:,", "),"}")',jvPlotData.je[inds])+'};'
  PRINT,STRING(FORMAT='("jeErrs={",' + nHerStr + '(F-0.3,:,", "),"}")',jvPlotData.jeerr[inds])+'};'
  PRINT,STRING(FORMAT='("dens={",' + nHerStr + '(F-0.3,:,", "),"}")',jvPlotData.source.nDown[inds])+'};'
  PRINT,STRING(FORMAT='("densErr={",' + nHerStr + '(F-0.3,:,", "),"}")',jvPlotData.source.nDownErr[inds])+'};'
  PRINT,STRING(FORMAT='("downepots={",' + nHerStr + '(F-0.3,:,", "),"}")',jvPlotData.only_downe_pot[inds])+'};'
  IF N_ELEMENTS(kFit2DParam_struct.bulk_energy) EQ nHer THEN BEGIN
     PRINT,STRING(FORMAT='("gaussbulkEs={",' + nHerStr + '(F-0.2,:,", "),"}")',gFit2DParam_struct.bulk_energy[inds])+'};'
     PRINT,STRING(FORMAT='("kappabulkEs={",' + nHerStr + '(F-0.2,:,", "),"}")',kFit2DParam_struct.bulk_energy[inds])+'};'
  ENDIF

  IF N_ELEMENTS(fit2DKappa_inf_list) GT 0 AND N_ELEMENTS(fit2DGauss_inf_list) GT 0 THEN BEGIN

     ;; kStart = 42
     ;; FOR k=0,kStart DO PRINT,k,",",T2S(jvplotdata.time[avgs_jvfit.useinds[k]])
     ;; inds=[0:29]
     STR_ELEMENT_FROM_LIST_OF_STRUCTS,fit2DKappa_inf_list,'obsmoms',VALUE=kObsMoms
     STR_ELEMENT_FROM_LIST_OF_STRUCTS,fit2DGauss_inf_list,'obsmoms',VALUE=gObsMoms
     PRINT,STRING(FORMAT='("kFAConduct={",' $
                  + STRING(N_ELEMENTS(kObsMoms)) + '(G-0.4,:,", "),"}")',kObsMoms.scfaconduct) $
           + '};'
     PRINT,STRING(FORMAT='("gFAConduct={",' $
                  + STRING(N_ELEMENTS(gObsMoms)) + '(G-0.4,:,", "),"}")',gObsMoms.scfaconduct) $
           + '};'
  ENDIF

  IF N_ELEMENTS(cAP_struct) GT 0 THEN BEGIN

     tRanges = !NULL
     STR_ELEMENT,cAP_struct,'tRanges',tRanges
     IF N_ELEMENTS(tRanges) GT 0 THEN IF SIZE(tRanges,/TYPE) EQ 7 THEN BEGIN

        PRINT,""

        PRINT,FORMAT='(A3,TR1,A22,TR5,A6,TR5,A6,TR5,A6)', $
              'k', $
              'Time', $
              'Tcalc', $
              'TKappa', $
              'TGauss'

        nTRanges = N_ELEMENTS(tRanges[0,*])
        useInds  = !NULL
        FOR j=0,nTRanges-1 DO BEGIN

           tmpInds = WHERE(JVPlotData.time[avgs_jvFit.useInds] GE STR_TO_TIME(tRanges[0,j]) AND $
                           JVPlotData.time[avgs_jvFit.useInds] LE STR_TO_TIME(tRanges[1,j]),nTmp)

           IF nTmp GT 0 AND N_ELEMENTS(avgs_jvFit) GT 0 THEN BEGIN

              FOR k=0,N_ELEMENTS(tmpInds)-1 DO $
                 PRINT,FORMAT='(I3,TR1,A22,TR5,F6.2,TR5,F6.2,TR5,F6.2)', $
                       k, $
                       T2S(jvPlotData.time[avgs_jvFit.useInds[tmpInds[k]]]), $
                       jvPlotData.tDown[avgs_jvFit.useInds[tmpInds[k]]], $
                       fit2DK.fitParams[1,avgs_jvFit.useInds[tmpInds[k]]], $
                       fit2DG.fitParams[1,avgs_jvFit.useInds[tmpInds[k]]]

              ;; Now temperatures
              PRINT,FORMAT='(A6,TR25,F6.2,TR5,F6.2,TR5,F6.2)', $
                    'MEAN', $
                    MEAN(jvPlotData.tDown[avgs_jvFit.useInds[tmpInds]]), $
                    MEAN(fit2DK.fitParams[1,avgs_jvFit.useInds[tmpInds]]), $
                    MEAN(fit2DG.fitParams[1,avgs_jvFit.useInds[tmpInds]])
              PRINT,FORMAT='(A6,TR25,F6.2,TR5,F6.2,TR5,F6.2)', $
                    'MEDIAN', $
                    MEDIAN(jvPlotData.tDown[avgs_jvFit.useInds[tmpInds]]), $
                    MEDIAN(fit2DK.fitParams[1,avgs_jvFit.useInds[tmpInds]]), $
                    MEDIAN(fit2DG.fitParams[1,avgs_jvFit.useInds[tmpInds]])

              PRINT,''
           ENDIF

        ENDFOR

     ENDIF

  ENDIF



END
