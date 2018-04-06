;2016/08/06
PRO KAPPA_FIT2D__SHOW_AND_PROMPT__EACH_CANDIDATE,curDataStr,fit2DStruct, $
   TIMEFNSTR=timeFNStr, $
   ONLY_DATA=only_data, $ 
   FOR_HORSESHOE_FIT=for_horseshoe_fit, $
   IS_MAXWELLIAN_FIT=is_maxwellian_fit, $
   PROMPT__CONT_TO_NEXT_FIT=prompt__cont_to_next_fit, $
   PROMPT__CONT_UNTIL_FIT_EQ=prompt__cont_until_fit_eq, $
   PROMPT__NTOT2DFITS=nTot2DFits, $
   FINISH_AND_SAVE_ALL=finish_and_save_all, $
   KAPPA_FIT__SHOW__QUIT=show__quit, $
   FIT2D__PA_ZRANGE=fit2D__PA_zRange, $
   EPS=eps

  COMPILE_OPT IDL2,STRICTARRSUBS

  @common__kappa_fit2d_structs.pro

  ;; xSize       = 8.
  ;; ySize       = 6.4
  xSize       = 9.5
  ySize       = 9.5
  land        = 1

  xWinSize    = 700
  yWinSize    = 700

  ;;Some stuff in case we decide to write a few of these chocolatiers
  ;; cont2DLims  = {zrange:[10^(6.6),10^9]}
  ;; spec2DLims = {yrange:[1e6,1e10]}

  ;; More fings?
  cont2DLims  = {zrange:[10.^(6.),10.^9], $
                 xrange:[-4.6,4.6], $
                 yrange:[-4.6,4.6], $
                 xstyle:1, $
                 ystyle:1}
  spec2DLims = {yrange:[1e6,1e10]}

  ;; upLim       = MAX(curDataStr.data) > (KEYWORD_SET(only_data) ? MAX(curDataStr.data) : MAX(fit2DStruct.bestFitStr.data))
  ;; cont2DLims  = {zrange:[10^(6.6),upLim]}
  ;; spec2DLims = {yrange:[1e6,upLim]}

  IF curDataStr.mass GT 5.7D-06 THEN BEGIN
     fit2D__PA_zRange = 10.D^([5.5,8.5])
  ENDIF ELSE BEGIN
     fit2D__PA_zRange = 10.D^([6.0,8.5])
  ENDELSE
  
  ;; IF KEYWORD_SET(fit2D__PA_zRange) THEN BEGIN
     cont2DLims.zRange = fit2D__PA_zRange
     spec2DLims.yRange = fit2D__PA_zRange
  ;; ENDIF

  spec2DDatLims = CREATE_STRUCT(spec2DLims,'PSYM',1)

  IF N_ELEMENTS(KF2D__SDTData_opt) GT 0 THEN BEGIN
     angleRange = KF2D__SDTData_opt.fit2D_dens_aRange
  ENDIF

  CASE 1 OF
     KEYWORD_SET(is_maxwellian_fit): BEGIN
        fitString  = 'Maxwell'
     END
     KEYWORD_SET(only_data): BEGIN
        fitString  = 'data'
     END
     ELSE: BEGIN
        fitString  = 'Kappa'
     END
  ENDCASE

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

  ;;Set up PA plot dir
  PAPlotDir = (KEYWORD_SET(KF2D__Plot_opt.plotDir) ? KF2D__Plot_opt.plotDir : './') + 'PA_plots/'
  plotDir_exists = FILE_TEST(PAPlotdir,/DIRECTORY)
  IF ~plotDir_exists THEN BEGIN
     IF KEYWORD_SET(verbose) THEN PRINTF,lun,"SET_PLOT_DIR: Making directory " + PAPlotdir
     SPAWN,'mkdir -p ' + PAPlotDir
  ENDIF
  plotDir_exists = FILE_TEST(PAPlotDir,/DIRECTORY)
  IF ~plotDir_exists THEN BEGIN
     PRINTF,lun,'Failed to make directory: ' + PAPlotDir
     PRINTF,lun,"Setting PAPlot dir to ./"
     PAPlotDir = './'
  ENDIF

  IF KEYWORD_SET(finish_and_save_all) THEN BEGIN
     CASE 1 OF
        KEYWORD_SET(only_data): BEGIN
           tempFN = STRING(FORMAT='("orb_",A0,"-",A0,A0,"-",A0,"-",A0)', $
                           KF2D__strings.orbStr, $
                           KF2D__strings.eeb_or_ees, $
                           KF2D__Plot_opt.plotNamePref EQ '' ? '' : '-' + KF2D__Plot_opt.plotNamePref, $
                           'Data', $
                           timeFNStr)
        END
        ELSE: BEGIN
           tempFN = STRING(FORMAT='("orb_",A0,A0,"-",A0,"_fit-",A0,A0)', $
                           KF2D__strings.orbStr, $
                           KF2D__Plot_opt.plotNamePref, $
                           fitString, $
                           timeFNStr, $
                           (KEYWORD_SET(KF2D__Curvefit_opt.fit2D__clampTemperature) ? '-clampT' : ''))
        END
     ENDCASE

     POPEN,(KEYWORD_SET(PAPlotDir) ? PAPlotDir : './') + tempFN, $
           XSIZE=xSize, $
           YSIZE=ySize, $
           LAND=land, $
           CTABLE=43, $
           ENCAPSULATED=eps, $
           OPTIONS={charsize:1.5}
     
     tmp2DStruct = fit2DStruct
     ;; these = WHERE((tmp2DStruct.SDT.energy LT KF2D__SDTData_opt.energy_electrons[0]) OR $
     ;;               (tmp2DStruct.SDT.energy GT KF2D__SDTData_opt.energy_electrons[1]),nThese)
     ;; these = WHERE((tmp2DStruct.SDT.energy LT tmp2DStruct.extra_info.eRange_fit[0]) OR $
     ;;               (tmp2DStruct.SDT.energy GT tmp2DStruct.extra_info.eRange_fit[1]),nThese)

     ;; The reason it looks like you're getting points that you're not supposed
     ;; to (I suppose), is that diff energy flux emphasizes higher energies over
     ;; lower ones.
     ;; zeroVal = 700087.25
     zeroVal = 1.0E3
     tmpEBounds = KEYWORD_SET(is_Maxwellian_fit) ? tmp2dstruct.extra_info.erange_fit : REVERSE(tmp2DStruct.sdt.energy[tmp2DStruct.fit1d.orig.energy_inds,0])
     ;; tmpEBounds = REVERSE(tmp2DStruct.sdt.energy[(tmp2DStruct.fit1d.orig.energy_inds-1) > 0,0])
     these = WHERE((tmp2DStruct.SDT.energy LT tmpEBounds[0]) OR $
                   (tmp2DStruct.SDT.energy GT tmpEBounds[1]),nThese)

     IF nThese GT 0 THEN tmp2DStruct.SDT.data[these] = zeroVal
     PRINT,FORMAT='("nEnergy: ",I0)',nThese
     these = WHERE((tmp2DStruct.SDT.theta[tmp2DStruct.SDT.nBins/2,*] LT $
                    KF2D__SDTData_opt.electron_angleRange[0]) OR $
                   (tmp2DStruct.SDT.theta[tmp2DStruct.SDT.nBins/2,*] GT $
                    KF2D__SDTData_opt.electron_angleRange[1]),nThese)
     IF nThese GT 0 THEN tmp2DStruct.SDT.data[*,these] = zeroVal
     ;; PRINT,FORMAT='("nangle: ",I0)',nThese
     nCont = 8
     PLOT_CONTOUR2D_MODEL_AND_DATA__SELECTED2DFIT,tmp2DStruct,curDataStr, $
        ONLY_DATA=only_data, $ 
        FOR_HORSESHOE_FIT=for_horseshoe_fit, $
        LIMITS=cont2DLims, $
        ADD_FITPARAMS_TEXT=KF2D__Plot_opt.add_fitParams_text, $
        ;; KF2D__SDTDATA_OPT=KF2D__SDTData_opt, $
        FITSTRING=fitString, $
        NCONT=nCont

     PCLOSE

     RETURN
  ENDIF

  WHILE ~cont DO BEGIN
     PRINT,"(C)ontinue    / Cont. to (N)ext fit   / Cont. (U)ntil fitNum Eq  / S(T)op and inspect / "
     PRINT,"Sp(E)ctrum    / (FE) Fitted spectrum  / (BE) Both Spectra        / (SE) Save Spectrum / " 
     PRINT,"(A)djust spec. angle range / "
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
           WINDOW,0,XSIZE=xWinSize,YSIZE=yWinSize
           SPEC2D,curDataStr,ANGLE=angleRange, $
                  /LABEL,LIMITS=spec2DLims,/MSEC

        END
        "fe": BEGIN

           cont = 0
           WINDOW,0,XSIZE=xWinSize,YSIZE=yWinSize
           SPEC2D,fit2DStruct.bestFitStr,ANGLE=angleRange, $
                  /LABEL,LIMITS=spec2DLims,/MS

        END
        "be": BEGIN

           cont = 0
           WINDOW,0,XSIZE=xWinSize,YSIZE=yWinSize
           SPEC2D,fit2DStruct.bestFitStr,ANGLE=angleRange, $
                  /LABEL,/MS,LIMITS=spec2DLims
           SPEC2D,curDataStr,OVERPLOT=showFit,ANGLE=angleRange, $
                  /LABEL,/MS,LIMITS=spec2DDatLims

        END
        "se": BEGIN

           cont   = 0
           tempFN = STRING(FORMAT='("spec2d--orb_",A0,A0,"--data_and_",A0,"_fit--",A0)', $
                           KF2D__strings.orbStr, $
                           KF2D__Plot_opt.plotNamePref, $
                           fitString,timeFNStr)
           POPEN,(KEYWORD_SET(PAPlotDir) ? PAPlotDir : './') + tempFN, $
                 XSIZE=xSize, $
                 YSIZE=ySize, $
                 LAND=land, $
                 ENCAPSULATED=eps

           SPEC2D,fit2DStruct.bestFitStr,ANGLE=angleRange, $
                  /LABEL,/MS,LIMITS=spec2DLims
           SPEC2D,curDataStr,ANGLE=angleRange, $
                  OVERPLOT=showFit, $
                  /LABEL,/MS,LIMITS=spec2DDatLims

           PCLOSE

        END
        "a": BEGIN
           cont = 0

           test_aRange = angleRange
           input2 = ''
           cont2  = 0

           WHILE ~cont2 DO BEGIN
              PRINT,FORMAT='("Current angle range : [",F0.1,",",F0.1,"]")',test_aRange
              ;; PRINT,FORMAT='("Current energy inds: [",I0,I0,"]")',test_eInds
              ;; PRINT,FORMAT='("Current energies   : [",G0.2," ,",G0.2,"]")',Xorig[test_eInds]
              PRINT,'(E)dit angle range     / (P)rint current angle range info /'
              PRINT,'(S)ave and finish editing / (F)inish editing, no save        '
              READ,input2
              CASE STRLOWCASE(input2) OF
                 "e": BEGIN
                    READ,newAngle,PROMPT="Enter new angle 0: "
                    test_aRange[0] = newAngle
                    READ,newAngle,PROMPT="Enter new angle 1: "
                    test_aRange[1] = newAngle
                 END
                 "p": BEGIN
                    PRINT,FORMAT='("Current angle range : [",F0.1,",",F0.1,"]")',test_aRange
                    ;; PRINT,FORMAT='("Current energies   : [",G0.2," ,",G0.2,"]")',Xorig[test_eInds]
                 END
                 "s": BEGIN
                    angleRange = test_aRange
                    cont2       = 1
                 END
                 "f": BEGIN
                    cont2       = 1
                 END
              ENDCASE
           ENDWHILE
        END
        "p": BEGIN
           cont = 0
           WINDOW,0,XSIZE=xWinSize,YSIZE=yWinSize
           PLOT_CONTOUR2D_MODEL_AND_DATA__SELECTED2DFIT,fit2DStruct,curDataStr, $
              FOR_HORSESHOE_FIT=for_horseshoe_fit, $
              LIMITS=cont2DLims, $
              ADD_FITPARAMS_TEXT=KF2D__Plot_opt.add_fitParams_text, $
              FITSTRING=fitString
        END
        "s": BEGIN
           cont = 0
           tempFN = STRING(FORMAT='("contour2d--orb_",A0,A0,"--data_and_",A0,"_fit--",A0)', $
                           KF2D__strings.orbStr, $
                           KF2D__Plot_opt.plotNamePref, $
                           fitString,timeFNStr)
           POPEN,(KEYWORD_SET(PAPlotDir) ? PAPlotDir : './') + tempFN, $
                 XSIZE=xSize, $
                 YSIZE=ySize, $
                 LAND=land, $
                 ENCAPSULATED=eps

           PLOT_CONTOUR2D_MODEL_AND_DATA__SELECTED2DFIT,fit2DStruct,curDataStr, $
              FOR_HORSESHOE_FIT=for_horseshoe_fit, $
              LIMITS=cont2DLims, $
              ADD_FITPARAMS_TEXT=KF2D__Plot_opt.add_fitParams_text, $
              FITSTRING=fitString

           PCLOSE

        END
        "f": BEGIN
           cont = 1
           finish_and_save_all = 1
           tempFN = STRING(FORMAT='("contour2d--orb_",A0,A0,"--data_and_",A0,"_fit--",A0)', $
                           strings.orbStr, $
                           KF2D__Plot_opt.plotNamePref, $
                           fitString,timeFNStr)
           POPEN,(KEYWORD_SET(PAPlotDir) ? PAPlotDir : './') + tempFN, $
                 XSIZE=xSize, $
                 YSIZE=ySize, $
                 LAND=land, $
                 ENCAPSULATED=eps

           CONTOUR2D,fit2DStruct.bestFitStr,/POLAR, $
                     /FILL,/LABEL,/MS,LIMITS=cont2DLims
           CONTOUR2D,curDataStr,/POLAR, $
                     OVERPLOT=showFit,/LABEL,/MS,LIMITS=cont2DLims

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

;; FUNCTION GET_PA_PLOT_DIR

;;   @common__kappa_fit2d_structs.pro


;;   RETURN,PAPlotDir

;; END
