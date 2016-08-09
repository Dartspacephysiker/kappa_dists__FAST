;;2016/08/05 Use it to see what's happening
PRO KAPPA__FIT_FAIL_USER_PROMPT,A,fixA,energy_inds, $
                                Xorig,X,Yorig,Y, $
                                STRINGS=strings, $
                                BOUNDS_I=bounds_i

  COMPILE_OPT idl2
   
  ;; failedlasttime = 1

  input = ''

  cont       = 0

  origA      = A
  orig_fixA  = fixA
  orig_eInds = energy_inds

  WHILE ~cont DO BEGIN
     PRINT,"(A)djust A  / Adjust (E) inds  / Adjust (F)ixed fit params        / (P)lot sitiation / (PF) Plot full fit"
     PRINT,"(R)estore original settings    /(T)ry again / (S)top and inspect  / (C)ontinue to next fit?"
     READ,input
     CASE STRLOWCASE(input) OF
        "a": BEGIN
           PRINT,"Here are your failed fit params"
           PRINT_KAPPA_FLUX_FIT_PARAMS__MPFITFUN,A
           PRINT,""
           
           testA  = A
           input2 = ''
           cont2  = 0
           WHILE ~cont2 DO BEGIN
              PRINT,'Edit (B)ulk energy  / Plasma (T)emp / (K)appa   / (D)ensity / (A)ngle offset / '
              PRINT,'(P)rint A estimates / (S)ave and finish editing / (F)inish editing, no save '
              READ,input2
              CASE STRLOWCASE(input2) OF
                 "b": BEGIN
                    READ,newBulk,PROMPT="Enter new bulk energy estimate: "
                    testA[0] = newBulk
                 END
                 "t": BEGIN
                    READ,newTemp,PROMPT="Enter new temperature estimate: "
                    testA[1] = newTemp
                 END
                 "k": BEGIN
                    READ,newKappa,PROMPT="Enter new kappa estimate: "
                    testA[2] = newKappa
                 END
                 "d": BEGIN
                    READ,newDens,PROMPT="Enter new density estimate: "
                    testA[3] = newDens
                 END
                 "a": BEGIN
                    ;; READ,newAngle,PROMPT=STRING(FORMAT='(%"Enter new angle estimate: \r")','')
                    READ,newAngle,PROMPT="Enter new angle estimate: "
                    testA[6] = newAngle
                 END
                 "p": BEGIN
                    PRINT,"Current fit params"
                    PRINT_KAPPA_FLUX_FIT_PARAMS,testA
                    ;; PRINT,STRING(FORMAT='(%"\r")','')
                    PRINT,''
                 END
                 "s": BEGIN
                    A     = testA
                    cont2 = 1
                 END
                 "f": BEGIN
                    cont2  = 1
                 END
                 ELSE: BEGIN
                 END
              ENDCASE
           ENDWHILE
        END
        "e": BEGIN
           test_eInds = energy_inds
           input2 = ''
           cont2  = 0

           PRINT,FORMAT='("Current energy inds: [",I0,I0,"]")',test_eInds
           PRINT,FORMAT='("Current energies   : [",G0.2," ,",G0.2,"]")',Xorig[test_eInds]
           PRINT,'Edit (E)nergy inds        / (P)rint Current energy ind info /'
           PRINT,'(S)ave and finish editing / (F)inish editing, no save        '
           READ,input2
           CASE STRLOWCASE(input2) OF
              "e": BEGIN
                 READ,newEnergyInd,PROMPT="Enter new energy ind 0: "
                 test_eInds[0] = newEnergyInd
                 READ,newEnergyInd,PROMPT="Enter new energy ind 1: "
                 test_eInds[1] = newEnergyInd
              END
              "p": BEGIN
                 PRINT,FORMAT='("Current energy inds: [",I0,I0,"]")',test_eInds
                 PRINT,FORMAT='("Current energies   : [",G0.2," ,",G0.2,"]")',Xorig[test_eInds]
              END
              "s": BEGIN
                 energy_inds = test_eInds
              END
              "f": BEGIN
                 cont2       = 1
              END
           ENDCASE
        END
        "f": BEGIN
           PRINT,"Here are your currently variable params"
           PRINT_KAPPA_FLUX_FIT_PARAMS,~kappa_fixA
           PRINT,""

           test_fixA = kappa_fixA
           input2 = ''
           cont2  = 0
           WHILE ~cont2 DO BEGIN
              PRINT,'Vary (B)ulk energy   / Plasma (T)emp / (K)appa   / (D)ensity / (A)ngle offset / '
              PRINT,'(P)rint Fixed params / (S)ave and finish editing / (F)inish editing, no save '
              READ,input2
              CASE STRLOWCASE(input2) OF
                 "b": BEGIN
                    READ,newBulk,PROMPT="Bulk energy can vary (1/0)?: "
                    test_fixA[0] = ~newBulk
                 END
                 "t": BEGIN
                    READ,newTemp,PROMPT="Temperature can vary (1/0)?: "
                    test_fixA[1] = ~newTemp
                 END
                 "k": BEGIN
                    READ,newKappa,PROMPT="Kappa can vary (1/0)?: "
                    test_fixA[2] = ~newKappa
                 END
                 "d": BEGIN
                    READ,newDens,PROMPT="Density can vary (1/0)? : "
                    test_fixA[3] = ~newDens
                 END
                 "a": BEGIN
                    READ,newAngle,PROMPT="Angle can vary (1/0)?: "
                    test_fixA[6] = ~newAngle
                 END
                 "p": BEGIN
                    PRINT,"Current variable vals"
                    PRINT_KAPPA_FLUX_FIT_PARAMS,~test_fixA
                    PRINT,''
                 END
                 "s": BEGIN
                    PRINT,"Updating kappa_fixA ..."
                    kappa_fixA = test_fixA
                    cont2  = 1
                 END
                 "f": BEGIN
                    cont2  = 1
                 END
                 ELSE: BEGIN
                 END
              ENDCASE
           ENDWHILE
        END
        "p": BEGIN
           cont = 0
           IF N_ELEMENTS(tempWindow) EQ 0 THEN BEGIN
              tempWindow = WINDOW(DIMENSIONS=[900,700])
           ENDIF ELSE BEGIN
              tempWindow.Erase
           ENDELSE

           yTemp     = KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__CONV_TO_F__FUNC(X,A)

           tempPlot  = PLOT(Xorig,Yorig,/XLOG,/YLOG, $
                            TITLE=strings.plotTimes[bounds_i], $
                            XTITLE='Energy (eV)',YTITLE='eFlux (eV/cm!U2!N-s-sr-eV)', $
                            NAME="eFlux Spectrum", $
                            COLOR='RED', $
                            CURRENT=tempWindow)
           tempPlot2 = PLOT(X,yTemp, $
                            /XLOG,/YLOG, $
                            /OVERPLOT, $
                            NAME="Test Kappa dist", $
                            COLOR='BLACK', $
                            LINESTYLE='--', $
                            CURRENT=tempWindow)
           legend    = LEGEND(TARGET=[tempPlot,tempPlot2], $
                              /NORMAL, $
                              POSITION=[0.5,0.8])
        END
        "pf": BEGIN
           cont = 0
           IF N_ELEMENTS(tempWindow) EQ 0 THEN BEGIN
              tempWindow = WINDOW(DIMENSIONS=[400,400])
           ENDIF ELSE BEGIN
              tempWindow.Erase
           ENDELSE

           yTemp     = KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__CONV_TO_F__FUNC(Xorig,A)

           tempPlot  = PLOT(Xorig,Yorig,/XLOG,/YLOG, $
                            TITLE=strings.plotTimes[bounds_i], $
                            XTITLE='Energy (eV)',YTITLE='eFlux (eV/cm!U2!N-s-sr-eV)', $
                            NAME="eFlux Spectrum", $
                            COLOR='RED', $
                            CURRENT=tempWindow)
           tempPlot2 = PLOT(Xorig,yTemp, $
                            /XLOG,/YLOG, $
                            /OVERPLOT, $
                            NAME="Test Kappa dist", $
                            COLOR='BLACK', $
                            LINESTYLE='--', $
                            CURRENT=tempWindow)
           legend    = LEGEND(TARGET=[tempPlot,tempPlot2], $
                              /NORMAL, $
                              POSITION=[0.5,0.8])
        END
        "r": BEGIN
           PRINT,'Restoring original settings ...'
           A           = origA
           kappa_fixA  = orig_fixA 
           energy_inds = orig_eInds
        END
        "t": BEGIN
           cont = 1
           chi2 = 0.0
        END
        "s": BEGIN
           STOP
        END
        "c": BEGIN
           cont      = 1
           contKappa = 1
        END
        ELSE: BEGIN
           PRINT,"Invalid option: " + input
        END
     ENDCASE
  ENDWHILE

END