;2016/08/06
PRO KAPPA_FIT2D__SHOW_AND_PROMPT__EACH_CANDIDATE,curDataStr,fit2DStruct, $
   nTot2DFits, $
   iTime, $
   IS_MAXWELLIAN_FIT=is_maxwellian_fit, $
   KSTRINGS=kStrings, $
   KPLOT_OPT=kPlot_opt, $
   KCURVEFIT_OPT=kCurvefit_opt, $
   PROMPT__CONT_TO_NEXT_FIT=prompt__cont_to_next_fit, $
   PROMPT__CONT_UNTIL_FIT_EQ=prompt__cont_until_fit_eq, $
   FINISH_AND_SAVE_ALL=finish_and_save_all, $
   KAPPA_FIT__SHOW__QUIT=show__quit

  COMPILE_OPT idl2

  ;;Some stuff in case we decide to write a few of these chocolatiers
  upLim       = MAX(curDataStr.data) > MAX(fit2DStruct.bestFitStr.data)
  ;; cont2DLims  = {zrange:[10^(6.6),10^9]}
  ;; spec2DLims = {yrange:[1e6,1e10]}
  cont2DLims  = {zrange:[10^(6.6),upLim]}
  spec2DLims = {yrange:[1e6,upLim]}
  spec2DDatLims = CREATE_STRUCT(spec2DLims,'PSYM',1)

  IF KEYWORD_SET(is_maxwellian_fit) THEN BEGIN
     fitString  = 'Maxwell'
  ENDIF ELSE BEGIN
     fitString  = 'Kappa'
  ENDELSE

  CASE 1 OF
     KEYWORD_SET(prompt__cont_until_fit_eq): BEGIN
        IF nTot2DFits GE prompt__cont_until_fit_eq THEN BEGIN
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
  showFit       = 1

  IF KEYWORD_SET(finish_and_save_all) THEN BEGIN
     tempFN = STRING(FORMAT='(A0,"_fit--orb_",A0,"--",A0)', $
                     fitString, $
                     kStrings.orbStr, $
                     kStrings.timeFNStrs[iTime])

     POPEN,(KEYWORD_SET(kPlot_opt.plotDir) ? kPlot_opt.plotDir : './') + tempFN
     PLOT_CONTOUR2D_MODEL_AND_DATA__SELECTED2DFIT,fit2DStruct,curDataStr, $
        LIMITS=cont2DLims, $
        ADD_FITPARAMS_TEXT=kPlot_opt.add_fitParams_text, $
        FITSTRING=fitString
     PCLOSE

  ENDIF

  WHILE ~cont DO BEGIN
     PRINT,"(C)ontinue    / Cont. to (N)ext fit   / Cont. (U)ntil fitNum Eq  / S(T)op and inspect / "
     PRINT,"Sp(E)ctrum    / (FE) Fitted spectrum  / (BE) Both Spectra        / (SE) Save Spectrum / " 
     PRINT,"(P)itch angle / (S)ave Pitch angle    / "
     PRINT,"(F)inish and save all / (Q)uit ?"
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
              prompt__cont_until_fit_eq = minNumFits
              cont2 = 1
           ENDWHILE
           cont = 1
        END
        "t": BEGIN
           STOP
        END
        "e": BEGIN
           cont = 0
           SPEC2D,curDataStr,/LABEL,LIMITS=spec2DLims,/MSEC
        END
        "fe": BEGIN
           cont = 0
           SPEC2D,fit2DStruct.bestFitStr,/LABEL,LIMITS=spec2DLims,/MS
        END
        "be": BEGIN
           cont = 0
           SPEC2D,fit2DStruct.bestFitStr,/LABEL,/MS,LIMITS=spec2DLims
           SPEC2D,curDataStr,OVERPLOT=showFit,/LABEL,/MS,LIMITS=spec2DDatLims
        END
        "se": BEGIN
           cont = 0
           tempFN = STRING(FORMAT='("spec2d--data_and_",A0,"_fit--orb_",A0,"--",A0)', $
                           fitString,kStrings.orbStr,kStrings.timeFNStrs[iTime])
           POPEN,(KEYWORD_SET(kPlot_opt.plotDir) ? kPlot_opt.plotDir : './') + tempFN
           SPEC2D,fit2DStruct.bestFitStr,/LABEL,/MS,LIMITS=spec2DLims
           SPEC2D,curDataStr,OVERPLOT=showFit,/LABEL,/MS,LIMITS=spec2DDatLims
           PCLOSE
        END
        "p": BEGIN
           cont = 0
           PLOT_CONTOUR2D_MODEL_AND_DATA__SELECTED2DFIT,fit2DStruct,curDataStr, $
              LIMITS=cont2DLims, $
              ADD_FITPARAMS_TEXT=kPlot_opt.add_fitParams_text, $
              FITSTRING=fitString
        END
        "s": BEGIN
           cont = 0
           tempFN = STRING(FORMAT='("contour2d--data_and_",A0,"_fit--orb_",A0,"--",A0)', $
                           fitString,kStrings.orbStr,kStrings.timeFNStrs[iTime])
           POPEN,(KEYWORD_SET(kPlot_opt.plotDir) ? kPlot_opt.plotDir : './') + tempFN
           PLOT_CONTOUR2D_MODEL_AND_DATA__SELECTED2DFIT,fit2DStruct,curDataStr, $
              LIMITS=cont2DLims, $
              ADD_FITPARAMS_TEXT=kPlot_opt.add_fitParams_text, $
              FITSTRING=fitString
           PCLOSE
        END
        "f": BEGIN
           cont = 1
           finish_and_save_all = 1
           tempFN = STRING(FORMAT='("contour2d--data_and_",A0,"_fit--orb_",A0,"--",A0)', $
                           fitString,strings.orbStr,strings.timeFNStrs[iTime])
           POPEN,(KEYWORD_SET(kPlot_opt.plotDir) ? kPlot_opt.plotDir : './') + tempFN
           CONTOUR2D,fit2DStruct.bestFitStr,/POLAR,/FILL,/LABEL,/MS,LIMITS=cont2DLims
           CONTOUR2D,curDataStr,/POLAR,OVERPLOT=showFit,/LABEL,/MS,LIMITS=cont2DLims
           PCLOSE
        END
        "q": BEGIN
           PRINT,"Returning ..."
           quit = 1
           cont = 1
        END
        ELSE: BEGIN
           PRINT,"Invalid option: " + input
        END
     ENDCASE

  ENDWHILE

END