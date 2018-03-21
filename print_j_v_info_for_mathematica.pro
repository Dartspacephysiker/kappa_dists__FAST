;2018/03/01
PRO PRINT_J_V_INFO_FOR_MATHEMATICA,jvPlotData,kFit2DParam_struct,gFit2DParam_struct

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
        PRINT,""
        IF N_ELEMENTS(kFit2DParam_struct.bulk_energy) EQ nHer THEN BEGIN
           PRINT,STRING(FORMAT='("gaussbulkEs={",' + nHerStr + '(F-0.2,:,", "),"}")',gFit2DParam_struct.bulk_energy[inds])+'};'
           PRINT,""
           PRINT,STRING(FORMAT='("kappabulkEs={",' + nHerStr + '(F-0.2,:,", "),"}")',kFit2DParam_struct.bulk_energy[inds])+'};'
        ENDIF

END
