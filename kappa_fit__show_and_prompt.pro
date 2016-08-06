;2016/07/21
PRO KAPPA_FIT__SHOW_AND_PROMPT,diff_eFlux,synthKappa, $
                               iTime,nGoodFits_tempK, $
                               ;; kStrings,kPlot_opt, $
                               strings, $
                               nAngles, $
                               PLOTDIR=plotDir, $
                               PROMPT__CONT_TO_NEXT_FIT=prompt__cont_to_next_fit, $
                               PROMPT__CONT_UNTIL_FIT_EQ=prompt__cont_until_fit_eq, $
                               FINISH_AND_SAVE_ALL=finish_and_save_all, $
                               KAPPA_FIT__SHOW__QUIT=show__quit

  COMPILE_OPT idl2

  CASE 1 OF
     KEYWORD_SET(prompt__cont_to_next_fit): BEGIN
        IF nGoodFits_tempK GT 0 THEN BEGIN
           cont = 0
           prompt__cont_to_next_fit = 0
        ENDIF ELSE BEGIN
           cont = 1
        ENDELSE
     END
     KEYWORD_SET(prompt__cont_until_fit_eq): BEGIN
        IF nGoodFits_tempK GE prompt__cont_until_fit_eq THEN BEGIN
           cont = 0
        ENDIF ELSE BEGIN
           cont = 1
        ENDELSE
     END
     ELSE: BEGIN
        cont    = 0
     END
  ENDCASE

  input         = ''
  showFit       = nGoodFits_tempK GT 0

  IF ~KEYWORD_SET(finish_and_save_all) THEN BEGIN
     IF showFit THEN BEGIN
        CONTOUR2D,MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(synthKappa,iTime),/POLAR,/FILL
     ENDIF
     CONTOUR2D,MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(diff_eFlux,iTime),/POLAR,OVERPLOT=showFit
  ENDIF

  WHILE ~cont DO BEGIN
     PRINT,"(C)ontinue    / Cont. to (N)ext fit   / Cont. (U)ntil fitNum Eq  / S(T)op and inspect / "
     PRINT,"Sp(E)ctrum    / (FE) Fitted spectrum  / (P)itch angle / "
     PRINT,"(S)ave        / (F)inish and save all / (Q)uit ?"
     IF KEYWORD_SET(finish_and_save_all) THEN input = 's' ELSE READ,input
     CASE STRLOWCASE(input) OF
        "c": BEGIN
           cont = 1
        END
        "n": BEGIN
           prompt__cont_to_next_fit = 1
           cont = 1
        END
        "u": BEGIN
           cont2 = 0
           WHILE ~cont2 DO BEGIN
              PRINT,"Min fits for stopping: "
              READ,minNumFits
              IF minNumFits GT nAngles THEN BEGIN
                 PRINT,"Can't select higher number than there are angles being fit!"
              ENDIF ELSE BEGIN
                 prompt__cont_until_fit_eq = minNumFits
                 cont2 = 1
              ENDELSE
           ENDWHILE
           cont = 1
        END
        "t": BEGIN
           STOP
        END
        "e": BEGIN
           cont = 0
           ;; tempFN = STRING(FORMAT='("contour2d--data_and_kappa_fit--orb_",A0,"--",A0)',strings.orbStr,strings.timeFNStrs[iTime])
           ;; PRINT,"Saving to " + tempFN + ' ...'
           ;; POPEN,(KEYWORD_SET(plotDir) ? plotDir : './') + tempFN
           ;; IF showFit THEN CONTOUR2D,MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(synthKappa,iTime),/POLAR,/FILL
           SPEC2D,MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(diff_eFlux,iTime),/LABEL,LIMITS={yrange:[1e6,1e10]} ;,/POLAR,OVERPLOT=showFit
           ;; PCLOSE
        END
        "fe": BEGIN
           cont = 0
           ;; tempFN = STRING(FORMAT='("contour2d--data_and_kappa_fit--orb_",A0,"--",A0)',strings.orbStr,strings.timeFNStrs[iTime])
           ;; PRINT,"Saving to " + tempFN + ' ...'
           ;; POPEN,(KEYWORD_SET(plotDir) ? plotDir : './') + tempFN
           ;; IF showFit THEN CONTOUR2D,MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(synthKappa,iTime),/POLAR,/FILL
           IF showFit THEN BEGIN
              SPEC2D,MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(synthKappa,iTime),/LABEL,LIMITS={yrange:[1e6,1e10]} 
           ENDIF ELSE PRINT,"No good fits!"
           ;; PCLOSE
        END
        "p": BEGIN
           cont = 0
           IF showFit THEN CONTOUR2D,MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(synthKappa,iTime),/POLAR,/FILL
           CONTOUR2D,MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(diff_eFlux,iTime),/POLAR,OVERPLOT=showFit
        END
        "s": BEGIN
           cont = 1
           tempFN = STRING(FORMAT='("contour2d--data_and_kappa_fit--orb_",A0,"--",A0)',strings.orbStr,strings.timeFNStrs[iTime])
           PRINT,"Saving to " + tempFN + ' ...'
           POPEN,(KEYWORD_SET(plotDir) ? plotDir : './') + tempFN
           IF showFit THEN CONTOUR2D,MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(synthKappa,iTime),/POLAR,/FILL
           CONTOUR2D,MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(diff_eFlux,iTime),/POLAR,OVERPLOT=showFit
           PCLOSE
        END
        "f": BEGIN
           cont = 1
           finish_and_save_all = 1
           tempFN = STRING(FORMAT='("contour2d--data_and_kappa_fit--orb_",A0,"--",A0)',strings.orbStr,strings.timeFNStrs[iTime])
           PRINT,"Saving to " + tempFN + ' ...'
           POPEN,(KEYWORD_SET(plotDir) ? plotDir : './') + tempFN
           IF showFit THEN CONTOUR2D,MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(synthKappa,iTime),/POLAR,/FILL
           CONTOUR2D,MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(diff_eFlux,iTime),/POLAR,OVERPLOT=showFit
           PCLOSE
        END
        "q": BEGIN
           PRINT,"Returning ..."
           quit = 1
           cont = 1
           ;; RETURN
        END
        ELSE: BEGIN
           PRINT,"Invalid option: " + input
        END
     ENDCASE

  ENDWHILE


END