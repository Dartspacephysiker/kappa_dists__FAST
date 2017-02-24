;;2017/02/22
PRO CURRENT_AND_POTENTIAL_ANALYSIS, $
   ORBIT=orbit, $
   ORBTIMES=orbTimes, $
   ORBBURSTTIMES=orbBurstTimes, $
   BONUSPREF=bonusPref, $
   DOWNTIMESSTR=downTimesStr, $
   UPTIMESSTR=upTimesStr, $
   TIMESLIST=timesList, $
   UNITS=units, $
   OUTDIR=outDir, $
   DATFILE=datFile, $
   REMAKE_MASTERFILE=remake_masterFile, $
   SAVE_DIFF_EFLUX_FILE=save_diff_eFlux_file, $
   LOAD_DIFF_EFLUX_FILE=load_diff_eFlux_file, $
   EEB_OR_EESARR=eeb_or_eesArr, $
   ORDER=order, $
   LABEL=label, $
   ARANGE__MOMENTS_E_DOWN=aRange__moments_e_down, $
   ARANGE__MOMENTS_I_UP=aRange__moments_i_up, $
   WHICH_EEB__LABEL=label__which_eeb, $
   WHICH_TIMES__LABEL=label__which_times, $
   ENERGYARR=energyArr, $
   ARANGE__MOMENTS_LIST=aRange__moments_list, $
   ARANGE__PEAKEN_LIST=aRange__peakEn_list, $
   ARANGE__CHARE_LIST=aRange__charE_list, $
   ELPHIC1998_DEFAULTS=Elphic1998_defaults, $
   MIN_PEAK_ENERGYARR=min_peak_energyArr, $
   MAX_PEAK_ENERGYARR=max_peak_energyArr, $
   PEAK_ENERGY__START_AT_HIGHEARR=peak_energy__start_at_highEArr, $
   UPGOINGARR=upgoingArr, $
   ERROR_ESTIMATES=error_estimates, $
   ;; DENS_ERRORS=dens_errors, $
   MAP_TO_100KM=map_to_100km, $
   SAVECURPOTFILE=saveCurPotFile, $
   OUT_CURPOTLIST=curPotList

  COMPILE_OPT IDL2

  IF ~KEYWORD_SET(timesList) THEN BEGIN
     PRINT,"Making timesList from downTimesStr and upTimesStr ..."
     downTimes               = REFORM(STR_TO_TIME(downTimesStr),SIZE(downTimesStr,/DIMENSIONS))
     upTimes                 = REFORM(STR_TO_TIME(upTimesStr  ),SIZE(upTimesStr  ,/DIMENSIONS))
     timesList               = LIST(downTimes,upTimes)
  ENDIF

  IF KEYWORD_SET(elphic1998_defaults) THEN BEGIN
     eeb_or_eesArr           = ['ees','ies']

     ;; order                   = [0,2,1]
     order                   = [0,1,2]
     ;; order                   = [2,1,0]
     label                   = ['downgoing_e','upgoing_e','upgoing_i']

     label__which_eeb        = [0,0,1]
     label__which_times      = [0,0,0]
     ;; energyArr               = [[3e1,3.0e4],[3e1,3.0e4],[1e2,2.4e4]]
     ;; energyArr               = [[4,3.0e4],[4,3.0e4],[4,2.4e4]]
     energyArr               = [[50,3.0e4],[50,3.0e4],[4,2.4e4]]

     ;;Remember, !NULL means that the program will use the loss-cone angle range by default!
     aRange__moments_e_down  = [0.,360.]
     aRange__moments_i_up    = [0.,360.]
     aRange__moments_e_up    = !NULL

     ;; aRange__moments_e_down  = [330.,30.]
     ;; aRange__moments_i_up    = [150.,210.]
     ;; aRange__moments_e_up    = [150.,210.]

     aRange__moments_list    = LIST(aRange__moments_e_down,aRange__moments_e_up,aRange__moments_i_up)
     aRange__moments_list    = LIST(aRange__moments_e_down,aRange__moments_e_up,aRange__moments_i_up)
     ;; aRange__peakEn_list     = LIST(!NULL,!NULL,[150,210])
     aRange__peakEn_list     = LIST(!NULL,!NULL,!NULL)
     aRange__charE_list      = LIST(!NULL,!NULL,!NULL)

     ;; min_peak_energy      = KEYWORD_SET(upgoing) ? 100 : 500
     ;; max_peak_energy      = KEYWORD_SET(upgoing) ? 3e4 : !NULL
     min_peak_energyArr      = [4,4,4]
     max_peak_energyArr      = [3e4,3e4,2.4e4]

     ;;If doing upgoing electrons
     peak_energy__start_at_highEArr  = [0,1,1]
     upgoingArr                      = [0,1,1]

  ENDIF

  nCalcLoop              = N_ELEMENTS(label)
  preString = datFile + ' ...'
  IF FILE_TEST(outDir+datFile) AND ~KEYWORD_SET(remake_masterFile) THEN BEGIN
     RESTORE,outDir+datFile
     afterString = "Restored "

  ENDIF ELSE BEGIN

     nDEFLoop             = N_ELEMENTS(eeb_or_eesArr)
     dEF_list             = LIST()
     dEF_1c_list          = LIST()
     north_southArr_list  = LIST()

     err_list             = LIST()
     err1_list            = LIST()
     n_list               = LIST()
     nerr_list            = LIST()
     ;; fracN_list           = LIST() ;;Foolish, this is what Gershman's routine does!!
     j_list               = LIST()
     je_list              = LIST()
     chare_list           = LIST()
     jerr_list            = LIST()
     n1_list              = LIST()
     n1err_list           = LIST()
     ;; fracN1_list          = LIST()
     j1_list              = LIST()
     je1_list             = LIST()
     chare1_list          = LIST()
     j1err_list           = LIST()
     peak_ind_list        = LIST()
     peak_energy_list     = LIST()
     aRange_oMoments_list = LIST()
     aRange_oPeakEn_list  = LIST()
     aRange_oCharE_list   = LIST()

     FOR k=0,nDEFLoop-1 DO BEGIN

        eeb_or_ees        = eeb_or_eesArr[k]

        ;;String setup
        IF (STRUPCASE(eeb_or_ees) EQ 'EEB') OR (STRUPCASE(eeb_or_ees) EQ 'IEB') THEN BEGIN
           t1Str             = orbBurstTimes[0]
           t2Str             = orbBurstTimes[1]
           spectra_avg_itvl  = KEYWORD_SET(spectra_average_interval) ? spectra_average_interval : 4
        ENDIF ELSE BEGIN
           t1Str             = orbTimes[0]
           t2Str             = orbTimes[1]
           spectra_avg_itvl  = !NULL
        ENDELSE
        t1                   = STR_TO_TIME(t1Str)
        t2                   = STR_TO_TIME(t2Str)

        ;;... And strings!!!!
        KAPPA_FITTER__FSTRINGS, $
           ORBIT=orbit, $
           EEB_OR_EES=eeb_or_ees, $
           ELECTRON_ANGLERANGE=electron_angleRange ,$
           BONUSPREF=bonusPref ,$
           SAVE_DIFF_EFLUX_TO_FILE=save_diff_eFlux_to_file ,$
           SAVE_DIFF_EFLUX_FILE=save_diff_eFlux_file,$
           LOAD_DIFF_EFLUX_FILE=load_diff_eFlux_file,$
           OUT_DIFF_EFLUX_FILE=diff_eFlux_file, $
           FIT2D__ONLY_FIT_PEAK_ERANGE=fit2D__only_fit_peak_eRange ,$
           FIT2D__ONLY_FIT_ABOVEMIN=fit2D__only_fit_aboveMin ,$
           FIT2D__DISABLE_BFUNC=fit2D__disable_bFunc ,$
           FIT2D__EXCLUDE_LCA_FROM_DENSCALC=fit2D__exclude_lca_from_densCalc ,$
           FITFILE=fitFile, $
           LOADDIR=outDir

        preString = diff_eFlux_file + ' ...'
        IF (FILE_TEST(diff_eFlux_file) OR FILE_TEST(outDir+diff_eFlux_file)) AND $
           KEYWORD_SET(load_diff_eFlux_file) $
        THEN BEGIN
           afterString = "Restored "
           realFile    = (FILE_TEST(diff_eFlux_file) ? '' : outDir ) + diff_eFlux_file
           RESTORE,realFile
        ENDIF ELSE BEGIN
           afterString = "Made "

           GET_DIFF_EFLUX,T1=t1,T2=t2, $
                          EEB_OR_EES=eeb_or_ees, $
                          NAME__DIFF_EFLUX=name__diff_eFlux, $
                          /CALC_GEOM_FACTORS, $
                          UNITS=eSpecUnits, $
                          FIT_EACH_ANGLE=fit_each_angle, $
                          SPECTRA_AVERAGE_INTERVAL=spectra_avg_itvl, $
                          OUT_DIFF_EFLUX=diff_eflux, $
                          SAVE_DIFF_EFLUX_TO_FILE=save_diff_eFlux_to_file, $
                          LOAD_DAT_FROM_FILE=load_diff_eFlux_file, $
                          DIFF_EFLUX_FILE=diff_eFlux_file, $
                          LOAD_DIR=outDir
        ENDELSE
        PRINT,preString + afterString

        PRINT,"Getting oneCount curve ..."
        GET_ONECOUNT_DIFF_EFLUX_CURVE,t1,t2, $
                                      ;; LOAD_DAT_FROM_FILE=loadFile, $ ;;handled through proto
                                      EEB_OR_EES=eeb_or_ees, $
                                      SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                                      IN_PROTOSTRUCT=diff_eFlux, $
                                      SDT_NAME=dEF_oneCount_name, $
                                      ANGLE=e_angle, $
                                      ESPECUNITS=units, $
                                      ;; ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
                                      FIT_EACH_ANGLE=fit_each_angle, $ ;Perma-set because we do all angles for 2D fitting
                                      OUT_ONEDAT=out_oneDat, $
                                      DEF_ONECOUNT=dEF_oneCount, $
                                      QUIET=quiet


        dEF_list.Add,TEMPORARY(diff_eFlux)
        dEF_1c_list.Add,TEMPORARY(dEF_oneCount)

     ENDFOR

     ;;Now the calculations
     PRINT,"Beginning calculations"
     FOR fakeK=0,nCalcLoop-1 DO BEGIN

        k     = order[fakeK]
        t_k   = label__which_times[k]
        eeb_k = label__which_eeb[k]

        diff_eFlux                   = dEF_list[eeb_k]
        dEF_oneCount                 = dEF_1c_list[eeb_k]

        energy                       = energyArr[*,k]
        min_peak_energy              = min_peak_energyArr[k]
        max_peak_energy              = max_peak_energyArr[k]
        peak_energy__start_at_highE  = peak_energy__start_at_highEArr[k]
        upgoing                      = upgoingArr[k]

        GET_FA_ORBIT,LIST_TO_1DARRAY(timesList[t_k]),/TIME_ARRAY
        GET_DATA,'ILAT',DATA=ilat
        north_southArr       = ABS(ilat.y)/ilat.y

        north_southArr_list.Add,TEMPORARY(north_southArr)

        GET_LOSS_CONE_AND_ANGLE_RANGES_FOR_HEMI, $
           t1,t2, $
           lc_angleRange, $
           i_angle,i_angle_up, $
           north_south, $
           ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
           CUSTOM_E_ANGLERANGE=custom_e_angleRange, $
           OUT_E_ANGLE=e_angle, $
           ANGLESTR=angleStr, $
           /JUST_ONE

        CASE upgoing OF
           0: BEGIN
              angleRange = lc_angleRange
           END
           1: BEGIN
              ;; CASE STRMATCH(STRUPCASE(eeb_or_ees),'EE*') OF
              ;;    0: BEGIN
              ;; angleRange = (360.*(angleRange/360.-FLOOR(angleRange/360.)))
              ;; angleRange = lc_angleRange - 180.
              angleRange = (360.*((lc_angleRange-180)/360.-FLOOR((lc_angleRange-180)/360.)))
              ;;    END
              ;; ENDCASE
           END
        ENDCASE

        aRange__moments = N_ELEMENTS(aRange__moments_list[k]) GT 0 ? aRange__moments_list[k] : angleRange
        aRange__peakEn  = N_ELEMENTS(aRange__peakEn_list[k] ) GT 0 ? aRange__peakEn_list[k]  : angleRange
        aRange__charE   = N_ELEMENTS(aRange__charE_list[k]  ) GT 0 ? aRange__charE_list[k]   : angleRange

        aRange_oMoments_list.Add,aRange__moments
        aRange_oPeakEn_list.Add,aRange__peakEn
        aRange_oCharE_list.Add,aRange__charE

        ;;Summary kind
        PRINT,FORMAT='("*****",A0,"*****")',STRUPCASE(label[k])
        PRINT,FORMAT='(A0,T30,":",T35,2(F0.2,:,","))',"aRange__moments",aRange__moments
        PRINT,FORMAT='(A0,T30,":",T35,2(F0.2,:,","))',"aRange__peakEn",aRange__peakEn
        PRINT,FORMAT='(A0,T30,":",T35,2(F0.2,:,","))',"aRange__charE",aRange__charE
        ;; PRINT,FORMAT='(A0,T30,":",T35,2(F0.2,:,","))',"angleRange",angleRange
        PRINT,FORMAT='(A0,T30,":",T35,2(F0.2,:,","))',"energy",energy
        PRINT,FORMAT='(A0,T30,":",T35,2(F0.2,:,","))',"min_peak_energy",min_peak_energy
        PRINT,FORMAT='(A0,T30,":",T35,2(F0.2,:,","))',"max_peak_energy",max_peak_energy
        PRINT,FORMAT='(A0,T30,":",T35,2(F0.2,:,","))',"peak_energy__start_at_highE",peak_energy__start_at_highE
        PRINT,FORMAT='(A0,T30,":",T35,2(F0.2,:,","))',"upgoing",upgoing
        PRINT,""

        PRINT,"Getting eSpecs ..."
        eSpec          = GET_EN_SPEC__FROM_DIFF_EFLUX( $
                         diff_eFlux, $
                         /RETRACE, $
                         ANGLE=aRange__peakEn, $
                         UNITS=units)
        oneCount_eSpec = GET_EN_SPEC__FROM_DIFF_EFLUX( $
                         dEF_oneCount, $
                         /RETRACE, $
                         ANGLE=aRange__peakEn, $
                         UNITS=units)

        angles         = TRANSPOSE(diff_eFlux.theta,[1,0,2])
        energies       = TRANSPOSE(diff_eFlux.energy,[1,0,2])
        nEnergies      = N_ELEMENTS(eSpec.v[*,0])

        iAngle         = 0
        nHere          = N_ELEMENTS(eSpec.x)
        peak_indArr    = MAKE_ARRAY(nHere,VALUE=-999,/LONG)
        peak_energyArr = MAKE_ARRAY(nHere,VALUE=-999,/FLOAT)
        FOR iTime=0,nHere-1 DO BEGIN

           XorigArr         = energies[*,*,iTime]
           ;; YorigArr         = data[*,*,iTime]
           ;; worigArr         = ddata[*,*,iTime]
           ;; IF KEYWORD_SET(KF2D__Plot_opt.add_oneCount_curve) THEN BEGIN
           ;;    oneCountArr   = oneCount_data[*,*,iTime]
           ;; END
           AorigArr = angles[*,*,iTime]

           ;;And now the order becomes [angle,energy] for each of these arrays
           Xorig    = REFORM(eSpec.v[iTime,*])
           Yorig    = REFORM(eSpec.y[iTime,*])
           worig    = REFORM(eSpec.yerr[iTime,*])
           Aorig    = REFORM(AorigArr[iAngle,*])

           oneCurve = {x:Xorig, $
                       y:REFORM(oneCount_eSpec.y[iTime,*]), $
                       NAME:"One Count"}

           IF KEYWORD_SET(min_peak_energy) THEN BEGIN
              ;;Try taking it from the top
              min_peak_ind  = MAX(WHERE(REFORM(XorigArr[0,*]) GE min_peak_energy))
              IF min_peak_ind EQ -1 THEN BEGIN
                 STOP
              ENDIF
           ENDIF ELSE BEGIN
              min_peak_ind  = nEnergies-1
           ENDELSE

           KAPPA__GET_PEAK_IND_AND_PEAK_ENERGY, $
              Xorig,Yorig, $
              peak_ind,peak_energy, $
              BULK_OFFSET=bulk_offset, $
              CHECK_FOR_HIGHER_FLUX_PEAKS=check_higher_peaks_set_peakEn, $
              MIN_PEAK_ENERGY=min_peak_energy, $
              MAX_PEAK_ENERGY=max_peak_energy, $
              PEAK_ENERGY__START_AT_HIGHE=peak_energy__start_at_highE, $
              /CONTINUE_IF_NOMATCH, $
              ONECOUNT_STR=oneCurve

           peak_indArr[iTime]    = TEMPORARY(peak_ind)
           peak_energyArr[iTime] = TEMPORARY(peak_energy)
        ENDFOR

        peak_ind_list.Add,TEMPORARY(peak_indArr)
        peak_energy_list.Add,TEMPORARY(peak_energyArr)

        n        = N_2D__FROM_DIFF_EFLUX(diff_eFlux,ENERGY=energy,ANGLE=aRange__moments,QUIET=quiet)
        n1       = N_2D__FROM_DIFF_EFLUX(def_onecount,ENERGY=energy,ANGLE=aRange__moments,QUIET=quiet)

        ;;Number and energy flux
        j    = J_2D__FROM_DIFF_EFLUX(diff_eFlux,ENERGY=energy,ANGLE=aRange__moments,QUIET=quiet)
        je   = JE_2D__FROM_DIFF_EFLUX(diff_eFlux,ENERGY=energy,ANGLE=aRange__moments,QUIET=quiet)

        j1   = J_2D__FROM_DIFF_EFLUX(dEF_oneCount,ENERGY=energy,ANGLE=aRange__moments,QUIET=quiet)
        je1  = JE_2D__FROM_DIFF_EFLUX(dEF_oneCount,ENERGY=energy,ANGLE=aRange__moments,QUIET=quiet)

        jC   = J_2D__FROM_DIFF_EFLUX(diff_eFlux,ENERGY=energy,ANGLE=aRange__charE,QUIET=quiet)
        jeC  = JE_2D__FROM_DIFF_EFLUX(diff_eFlux,ENERGY=energy,ANGLE=aRange__charE,QUIET=quiet)

        j1C  = J_2D__FROM_DIFF_EFLUX(dEF_oneCount,ENERGY=energy,ANGLE=aRange__charE,QUIET=quiet)
        je1C = JE_2D__FROM_DIFF_EFLUX(dEF_oneCount,ENERGY=energy,ANGLE=aRange__charE,QUIET=quiet)

        ;;Error everything
        IF KEYWORD_SET(error_estimates) THEN BEGIN

           errors      = MOMENTERRORS_2D__FROM_DIFF_EFLUX(diff_eFlux,ENERGY=energy,ANGLE=aRange__moments,QUIET=quiet)
           errors1     = MOMENTERRORS_2D__FROM_DIFF_EFLUX(dEF_oneCount,ENERGY=energy,QUIET=quiet)

           ;; IF KEYWORD_SET(dens_errors) THEN BEGIN
           nerr        = MAKE_ARRAY(nHere,/FLOAT)
           n1err       = MAKE_ARRAY(nHere,/FLOAT)

           jerr        = MAKE_ARRAY(nHere,/FLOAT)
           j1err       = MAKE_ARRAY(nHere,/FLOAT)

           FOR l=0,N_ELEMENTS(diff_eFlux.time)-1 DO BEGIN
              nerr[l]  = n.y[l]  * errors[l].n
              n1err[l] = n1.y[l] * errors1[l].n

              jerr[l]  = SQRT((j.y[l])^(2.D) * $
                              ( (errors[l].n)^(2.D) + (errors[l].Uz)^(2.D) + errors[l].n*errors[l].Uz*errors[l].R[0,3] ) )
              j1err[l] = SQRT((j1.y[l])^(2.D) * $
                              ( (errors1[l].n)^(2.D) + (errors1[l].Uz)^(2.D) + errors1[l].n*errors1[l].Uz*errors1[l].R[0,3] ) )

           ENDFOR

        ENDIF

        ;;Update lists
        err_list.Add,TEMPORARY(errors)
        n_list.Add,TEMPORARY(n)
        j_list.Add,TEMPORARY(j)
        je_list.Add,TEMPORARY(je)
        chare_list.Add,CHAR_ENERGY((TEMPORARY(jC)).y,(TEMPORARY(jeC)).y)

        err1_list.Add,TEMPORARY(errors1)
        n1_list.Add,TEMPORARY(n1)
        j1_list.Add,TEMPORARY(j1)
        je1_list.Add,TEMPORARY(je1)
        chare1_list.Add,CHAR_ENERGY((TEMPORARY(j1C)).y,(TEMPORARY(je1C)).y)

        ;; IF KEYWORD_SET(dens_errors) THEN BEGIN
        nerr_list.Add,TEMPORARY(nerr)
        n1err_list.Add,TEMPORARY(n1err)

        jerr_list.Add,TEMPORARY(jerr)
        j1err_list.Add,TEMPORARY(j1err)
        ;; ENDIF

     ENDFOR

     afterString = "Made "
     SAVE,north_southArr_list, $
          err_list, $
          err1_list, $
          n_list, $
          nerr_list, $
          ;; fracN_list, $
          j_list, $
          je_list, $
          chare_list, $
          jerr_list, $
          n1_list, $
          n1err_list, $
          ;; fracN1_list, $
          j1_list, $
          je1_list, $
          chare1_list, $
          j1err_list, $
          peak_ind_list, $
          peak_energy_list, $
          aRange_oMoments_list, $
          aRange_oCharE_list, $
          aRange_oPeakEn_list, $
          FILENAME=outDir+datFile
  ENDELSE
  PRINT,preString + afterString


  ;;Now the calculations
  PRINT,"Now whittling to selected times"
  curPotList = LIST()
  FOR fakeK=0,nCalcLoop-1 DO BEGIN

     k     = order[fakeK]
     t_k   = label__which_times[k]
     eeb_k = label__which_eeb[k]

     PRINT,label[k] + ' ...'

     ;;electrons or ions?
     ions  = STRMATCH(STRUPCASE(eeb_or_eesArr[eeb_k]),'IE*')
     ;;Get times
     tmpT  = timesList[t_k]
     tmpNS = north_southArr_list[t_k]
     nSegs = N_ELEMENTS(tmpT[0,*])

     IF (N_ELEMENTS((n_list[k]).y) NE N_ELEMENTS((j_list[k]).y      )) OR $
        (N_ELEMENTS((n_list[k]).y) NE N_ELEMENTS((je_list[k]).y     )) OR $
        (N_ELEMENTS((n_list[k]).y) NE N_ELEMENTS(peak_ind_list[k]   )) OR $
        (N_ELEMENTS((n_list[k]).y) NE N_ELEMENTS(peak_energy_list[k]))    $
     THEN BEGIN
        PRINT,"It's unequal everywhere"
        STOP
     ENDIF

     IF ~ARRAY_EQUAL((n_list[k]).x,(j_list[k]).x      ) OR $
        ~ARRAY_EQUAL((n_list[k]).x,(je_list[k]).x     )    $
     THEN BEGIN
        PRINT,"It's unequal everywhere"
        STOP
     ENDIF

     itvlTime    = !NULL
     itvlJ       = !NULL
     itvlJe      = !NULL
     itvlCur     = !NULL
     itvlcharE   = !NULL
     itvlJ1      = !NULL
     itvlJe1     = !NULL
     itvlCur1    = !NULL
     itvlcharE1  = !NULL
     itvlPeakE   = !NULL
     itvlN       = !NULL
     itvlNerr    = !NULL
     itvlJerr    = !NULL
     itvlCurErr  = !NULL
     ;; itvlFracN   = !NULL
     itvlN1      = !NULL
     itvlN1err   = !NULL
     itvlJ1err   = !NULL
     itvlCur1Err = !NULL
     ;; itvlFracN1  = !NULL
     FOR realK=0,nSegs-1 DO BEGIN

        tmpT1 = tmpT[0,realK]
        tmpT2 = tmpT[1,realK]

        theseInds = WHERE( ( (j_list[k]).x GE tmpT1 ) AND $
                           ( (j_list[k]).x LE tmpT2 ), $
                           nThese)

        IF nThese EQ 0 THEN STOP

        theseInds = CGSETINTERSECTION(theseInds,UNIQ((j_list[k].x),SORT((j_list[k].x))),COUNT=nThese)
        CHECK_SORTED,(j_list[k].x)[theseInds],is_sorted,/QUIET & IF ~is_sorted THEN STOP
        IF nThese EQ 0 THEN STOP

        tmpTimes  = (j_list[k]).x[theseInds]

        ;;Pick up temps
        tmpJ       = (j_list[k]).y[theseInds]
        tmpJe      = (je_list[k]).y[theseInds]
        ;; tmpCharE = CHAR_ENERGY(tmpJ,tmpJe)
        tmpCharE   = (chare_list[k])[theseInds]
        tmpPeakE   = (peak_energy_list[k])[theseInds]
        tmpN       = (N_list[k]).y[theseInds]

        tmpJ1      = (j1_list[k]).y[theseInds]
        tmpJe1     = (je1_list[k]).y[theseInds]
        tmpCharE1  = (chare1_list[k])[theseInds]
        tmpN1      = (N1_list[k]).y[theseInds]

        ;; IF KEYWORD_SET(error_estimates) AND KEYWORD_SET(dens_errors) THEN BEGIN
        IF KEYWORD_SET(error_estimates) THEN BEGIN
           tmpNerr    = (nerr_list[k])[theseInds]
           tmpN1err   = (n1err_list[k])[theseInds]

           tmpJerr    = (jerr_list[k])[theseInds]
           tmpJ1err   = (j1err_list[k])[theseInds]
        ENDIF

        IF KEYWORD_SET(map_to_100km) THEN BEGIN
           GET_ALT_MLT_ILAT_FROM_FAST_EPHEM,orbit,tmpTimes, $
                                            OUT_TSORTED_I=tSort_i, $
                                            OUT_ALT=alt, $
                                            OUT_MLT=mlt, $
                                            OUT_ILAT=ilat, $
                                            OUT_MAPRATIO=mapRatio, $
                                            OUT_NEVENTS=nEvents, $
                                            LOGLUN=logLun
           IF N_ELEMENTS(tSort_i) GT 0 THEN STOP

           tmpJ        *= mapRatio
           tmpJe       *= mapRatio

           tmpJ1       *= mapRatio
           tmpJe1      *= mapRatio

           ;; IF KEYWORD_SET(error_estimates) AND KEYWORD_SET(dens_errors) THEN BEGIN
           IF KEYWORD_SET(error_estimates) THEN BEGIN
              tmpJerr  *= mapRatio
              tmpJ1err *= mapRatio
           ENDIF

        ENDIF

        ;;Get current (flip sign of current for electrons)
        tmpCur     = tmpJ  * 1.6e-9 * (ions ? 1. : (-1.))
        tmpCur1    = tmpJ1 * 1.6e-9 * (ions ? 1. : (-1.))

        ;; IF KEYWORD_SET(error_estimates) AND KEYWORD_SET(dens_errors) THEN BEGIN
        IF KEYWORD_SET(error_estimates) THEN BEGIN
           tmpCurErr  = tmpJerr  * 1.6e-9 * (ions ? 1. : (-1.))
           tmpCur1Err = tmpJ1err * 1.6e-9 * (ions ? 1. : (-1.))
        ENDIF

        ;;Make outward current positive in both hemis
        ;; ;;(You know, field lines going in at the NH, going out at the SH, yadda yadda)
        ;;I take it all back, Elphic et al. [1998] make upward current (downgoing electrons) negative

        ;; CASE N_ELEMENTS(WHERE(tmpNS GT 0,/NULL)) OF
        CASE N_ELEMENTS(WHERE(tmpNS LT 0,/NULL)) OF
           N_ELEMENTS(tmpNS): BEGIN
              tmpCur  *= (-1.)
              tmpCur1 *= (-1.)

              ;; IF KEYWORD_SET(error_estimates) AND KEYWORD_SET(dens_errors) THEN BEGIN
              IF KEYWORD_SET(error_estimates) THEN BEGIN
                 tmpCurErr  *= (-1.)
                 tmpCur1Err *= (-1.)
              ENDIF

           END
           0: BEGIN
           END
           ELSE: BEGIN
              STOP
           END
        ENDCASE

        ;; tmpFile = 'TMP_'+label[k]+'-'+STRCOMPRESS(realK,/REMOVE_ALL)+'.sav'
        ;; PRINT,"Saving " + tmpFile
        ;; SAVE,tmpJ,tmpJe,tmpCharE,tmpPeakE,tmpCur,FILENAME=outDir+tmpFile

        itvlTime       = [itvlTime  ,tmpTimes ]
        itvlJ          = [itvlJ     ,tmpJ     ]
        itvlJe         = [itvlJe    ,tmpJe    ]
        itvlCur        = [itvlCur   ,tmpCur   ]
        itvlcharE      = [itvlcharE ,tmpChare ]
        itvlN          = [itvlN     ,tmpN     ]
        itvlJ1         = [itvlJ1    ,tmpJ1    ]
        itvlJe1        = [itvlJe1   ,tmpJe1   ]
        itvlCur1       = [itvlCur1  ,tmpCur1  ]
        itvlcharE1     = [itvlcharE1,tmpChare1]
        itvlN1         = [itvlN1    ,tmpN1    ]
        ;; IF KEYWORD_SET(error_estimates) AND KEYWORD_SET(dens_errors) THEN BEGIN
        IF KEYWORD_SET(error_estimates) THEN BEGIN
           itvlNerr    = [itvlNerr  ,tmpNerr  ]
           itvlN1err   = [itvlN1err ,tmpN1err ]

           itvlJerr    = [itvlJerr  ,tmpJerr  ]
           itvlJ1err   = [itvlJ1err ,tmpJ1err ]

           itvlCurErr  = [itvlCurErr  ,tmpCurErr  ]
           itvlCur1Err = [itvlCur1Err ,tmpCur1Err ]
        ENDIF
        itvlPeakE      = [itvlPeakE ,tmpPeakE ]

     ENDFOR

     CASE 1 OF
        ;; (KEYWORD_SET(error_estimates) AND KEYWORD_SET(dens_errors)): BEGIN
        KEYWORD_SET(error_estimates): BEGIN
           tmpStruct     = {label    : label[k],$
                            time     : TEMPORARY(itvlTime)                , $
                            j        : TEMPORARY(itvlJ)                   , $
                            je       : TEMPORARY(itvlJe)                  , $
                            cur      : TEMPORARY(itvlCur)                 , $
                            chare    : TEMPORARY(itvlcharE)               , $
                            N        : TEMPORARY(itvlN)                   , $
                            Nerr     : TEMPORARY(itvlNerr)                , $
                            Jerr     : TEMPORARY(itvlJerr)                , $
                            CurErr   : TEMPORARY(itvlCurErr)              , $
                            j1       : TEMPORARY(itvlJ1)                  , $
                            je1      : TEMPORARY(itvlJe1)                 , $
                            cur1     : TEMPORARY(itvlCur1)                , $
                            chare1   : TEMPORARY(itvlcharE1)              , $
                            N1       : TEMPORARY(itvlN1)                  , $
                            N1err    : TEMPORARY(itvlN1err)               , $
                            J1Err    : TEMPORARY(itvlJ1Err)               , $
                            Cur1Err  : TEMPORARY(itvlCur1Err)             , $
                            peakE    : TEMPORARY(itvlPeakE)               , $
                            energy   : energyArr[*,k]                     , $
                            angles   : {charE   : aRange_oCharE_list[k]   , $
                                        moments : aRange_oMoments_list[k] , $
                                        peakEn  : aRange_oPeakEn_list[k]} $
                           }
        END
        ELSE: BEGIN
           tmpStruct     = {label  : label[k],$
                            time   : TEMPORARY(itvlTime)                , $
                            j      : TEMPORARY(itvlJ)                   , $
                            je     : TEMPORARY(itvlJe)                  , $
                            cur    : TEMPORARY(itvlCur)                 , $
                            chare  : TEMPORARY(itvlcharE)               , $
                            N      : TEMPORARY(itvlN)                   , $
                            j1     : TEMPORARY(itvlJ1)                  , $
                            je1    : TEMPORARY(itvlJe1)                 , $
                            cur1   : TEMPORARY(itvlCur1)                , $
                            chare1 : TEMPORARY(itvlcharE1)              , $
                            N1     : TEMPORARY(itvlN1)                  , $
                            peakE  : TEMPORARY(itvlPeakE)               , $
                            energy : energyArr[*,k]                     , $
                            angles : {charE   : aRange_oCharE_list[k]   , $
                                      moments : aRange_oMoments_list[k] , $
                                      peakEn  : aRange_oPeakEn_list[k]} $
                           }
        END
     ENDCASE

     curPotList.Add,tmpStruct
  ENDFOR

  IF KEYWORD_SET(saveCurPotFile) THEN BEGIN
     PRINT,"Saving it all to " + saveCurPotFile
     SAVE,curPotList,FILENAME=outDir+saveCurPotFile
  ENDIF

END
