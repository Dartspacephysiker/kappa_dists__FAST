;2016/07/20
;Find out how many good fits are in a given run
PRO KAPPA__GET_GOOD_INDS_FROM_FITSTRUCTS,kappaFits,time_index,runIndex,curThetas, $
                                         nGoodFits, $
                                         goodAngles,good_angleBin_i, $
                                         good_kappaFits_i, $
                                         OUT_KAPPAFITS_LIST=kappaFits_list

  COMPILE_OPT IDL2,STRICTARRSUBS

     nGoodFits               = 0L
     goodAngles              = !NULL
     good_angleBin_i         = !NULL
     good_kappaFits_i        = !NULL


     ;;Find the first one
     STR_ELEMENT_FROM_LIST_OF_STRUCTS,kappaFits,'time_index',VALUE=time_indexArr
     runIndex                = MIN(WHERE(time_indexArr EQ time_index))
     nFits                   = N_ELEMENTS(kappaFits)

     WHILE kappaFits[runIndex].time_index EQ time_index DO BEGIN
        IF kappaFits[runIndex].fitStatus EQ 0 THEN BEGIN

           ;;Pick up the winning angles
           goodAngles        = [goodAngles,kappaFits[runIndex].bulkAngleInf.sdtAngle]
           tmpMin            = MIN(ABS(goodAngles[-1]-curThetas),min_i)
           good_angleBin_i   = [good_angleBin_i,min_i]
           good_kappaFits_i  = [good_kappaFits_i,runIndex]
           nGoodFits++
        ENDIF
        runIndex++
        IF runIndex EQ nFits THEN BREAK
     ENDWHILE

     ;;Apparently duplicates are possible?? Remove them
     CHECK_DUPES,good_angleBin_i, $
                 HAS_DUPES=has_dupe_angles, $
                 N_DUPES=n_dupe_angles, $
                 OUT_DUPE_I=dupe_angles_ii, $
                 OUT_UNIQ_I=uniq_angles_ii, $
                 /QUIET

     IF KEYWORD_SET(has_dupe_angles) THEN BEGIN
        PRINT,"Whoops, there are " + STRCOMPRESS(n_dupe_angles,/REMOVE_ALL) + " duplicate angles here. Étrange ..."
        good_angleBin_i  = good_angleBin_i[uniq_angles_ii]
        good_kappaFits_i = good_kappaFits_i[uniq_angles_ii]
        goodAngles       = goodAngles[uniq_angles_ii]
        nGoodFits       -= n_dupe_angles
     ENDIF

END