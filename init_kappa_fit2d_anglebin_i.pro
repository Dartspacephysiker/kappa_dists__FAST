PRO INIT_KAPPA_FIT2D_ANGLEBIN_I,tempRange,alleyOop,nEnergies, $
                                AORIGARR=AorigArr, $
                                KSDTDATA_OPT=kSDTData_opt, $
                                OUT_NREQ_ANGLES=nReqAngles, $
                                OUT_USETHESEANGLESINDEX=useTheseAnglesIndex

  CASE N_ELEMENTS(kSDTData_opt.electron_angleRange) OF
     0: BEGIN
        tempRange    = [-180,180]
     END
     1: BEGIN
        tempRange    = [(-1.)*ABS(kSDTData_opt.electron_angleRange),ABS(kSDTData_opt.electron_angleRange)]
     END
     2: BEGIN
        tempRange    = kSDTData_opt.electron_angleRange

        CASE 1 OF
           tempRange[0] LT tempRange[1]: BEGIN
              alleyOop = 0
           END
           ELSE: alleyOop = 1
        ENDCASE
        IF tempRange[0] LT -179.99999 AND tempRange[1] GT 179.99999 THEN BEGIN
           tempRange = [0,360]
        ENDIF ELSE BEGIN
           IF tempRange[0] LT -180. THEN tempRange[0] = tempRange[0] + 360.
           IF tempRange[1] LT -180. THEN tempRange[1] = tempRange[1] + 360.
        ENDELSE
     END
  ENDCASE


  nAnglesPerEnergy = MAKE_ARRAY(nEnergies,/INTEGER)
  FOR k=0,nEnergies-1 DO BEGIN
     nAnglesPerEnergy[k]                = N_ELEMENTS(WHERE((AorigArr[*,k] GE tempRange[0]) AND (AorigArr[*,k] GE tempRange[1])))
  ENDFOR
  junk                                  = MAX(nAnglesPerEnergy,useTheseAnglesIndex)
  tempAllAngles                         = AorigArr[*,useTheseAnglesIndex]

     ;; CASE alleyOop OF
     ;;    0: BEGIN
     ;;       angleBins                       = tempAllAngles GE tempRange[0] AND tempAllAngles LE tempRange[1]
     ;;    END
     ;;    1: BEGIN
     ;;       angleBins                       = tempAllAngles GE tempRange[0] OR tempAllAngles LE tempRange[1]
     ;;    END
        
     ;; ENDCASE

     ;; angleBin_i                            = WHERE(angleBins,nReqAngles)

     CASE alleyOop OF
        0: BEGIN
           nReqAngles                      = N_ELEMENTS(WHERE(tempAllAngles GE tempRange[0] AND tempAllAngles LE tempRange[1],/NULL))
        END
        1: BEGIN
           nReqAngles                      = N_ELEMENTS(WHERE(tempAllAngles GE tempRange[0] OR tempAllAngles LE tempRange[1],/NULL))
        END
        
     ENDCASE

     ;; angleBin_i                            = WHERE(angleBins,nReqAngles)


END
