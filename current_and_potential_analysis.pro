;;2017/02/22
PRO ADD_FNAME_SUFF,fName,suff
        fNameTmp     = STRSPLIT(fName,'.',/EXTRACT)
        fNameTmp[0] += suff
        fName        = STRJOIN(TEMPORARY(fNameTmp),'.')
END

PRO ERROR_N,n,errors,nerr

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; FOR l=0,N_ELEMENTS(n.x)-1 DO BEGIN
     ;; nerr[l] = n.y[l]  * errors[l].n
     ;; nerr[l] = n.y[l]  * errors.n[l]
  ;; ENDFOR
     nerr = n.y  * errors.n
  
END

PRO ERROR_J,j,errors,jerr

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;;In #/cm^2-s
  jerr = SQRT((j.y)^(2.D) * $
                 ( (errors.n)^(2.D) + (errors.Uz)^(2.D) + errors.n*errors.Uz*errors.R[*,0,3] ) )  

END

PRO ERROR_JE,n,j,je,T,errors,jeErr

  COMPILE_OPT IDL2,STRICTARRSUBS

  eV_cm2sec_to_mW_m2 = 1.60218D-12
  eV_cm3_to_J_m3     = 1.60218D-13

  ;;I guess we'll SI-a-tize everything here
  vPar              = j.y/n.y/1D2                             ;j.y in #/cm^2-sec and n.y in cm^-3, so mult. by 1e-2 to get m/s
  PPar              = REFORM(T.y[2,*])*n.y*3.D*eV_cm3_to_J_m3 ;T.y in eV, so P in eV/cm^3
  PPrp              = REFORM(T.y[0,*])*n.y*3.D*eV_cm3_to_J_m3
  ;; jePar          = je.y*1D-3                               ;W/m^3

  ;;Parallel heat flux, from Hvec = Qvec - vVec dot P_tensor - 0.5 * vVec * Trace(P_tensor)
  ;; HPar_mW_m2        = je.y - 1.5D * eV_cm2sec_to_mW_m2 * (vPar * PPar - vPar * PPrp) * 1D2 /eV_cm3_to_J_m3 ;in mW/m2
  HPar              = (je.y*1D-3) - 1.5D * (vPar * PPar - vPar * PPrp)                                     ;in W/m2

  ;;sigmas
  sigmaVParSquared  = vPar * vPar * errors.Uz  * errors.Uz
  sigmaPParSquared  = PPar * PPar * errors.Pzz * errors.Pzz
  sigmaPPrpSquared  = PPrp * PPrp * errors.Pxx * errors.Pxx
  sigmaHParSquared  = errors.Hz * errors.Hz * HPar * HPar

  ;;covars
  covarVParPPar     = errors.R[*,3, 6] * (vPar * errors.Uz ) * (PPar * errors.Pzz)
  covarVParPPrp     = errors.R[*,3, 4] * (vPar * errors.Uz ) * (PPar * errors.Pxx)
  covarVParHPar     = errors.R[*,3,12] * (vPar * errors.Uz ) * (HPar * errors.Hz )
  covarPParPPrp     = errors.R[*,6, 4] * (PPar * errors.Pzz) * (PPrp * errors.Pxx)
  covarPParHPar     = errors.R[*,6,12] * (PPar * errors.Pzz) * (HPar * errors.Hz)
  covarPPrpHPar     = errors.R[*,4,12] * (PPrp * errors.Pxx) * (HPar * errors.Hz)

  jeErr = SQRT(sigmaHParSquared + $
               (1.5D*PPar + PPrp) * ( 2.D * TEMPORARY(covarVParHPar) + (1.5D*PPar + PPrp) * TEMPORARY(sigmaVParSquared) ) + $
               vPar * (3.D * TEMPORARY(covarPParHPar) + 2.D * TEMPORARY(covarPPrpHPar) + $
                       (1.5D * PPar + PPrp) * (3.D * TEMPORARY(covarVParPPar) + 2.D * TEMPORARY(covarVParPprp) )        ) + $
               vPar * vPar * (2.25D * TEMPORARY(sigmaPParSquared) + 3.D * covarPParPPrp + TEMPORARY(sigmaPPrpSquared)   )   ) $
          * 1D3 ;Convert back to mW/m^2

END

PRO ERROR_CHARE,j,je,jerr,jeErr,jje_coVar,errors,charEErr

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; PRINT,"MISSING TERM: COVAR__JE_PAR__J_PAR (IT'S BEEN SET TO ZERO IN THE MEANTIME)!!!!"
  ;; PRINT,"MISSING TERM: COVAR__JE_PAR__J_PAR (IT'S BEEN SET TO ZERO IN THE MEANTIME)!!!!"
  ;; PRINT,"MISSING TERM: COVAR__JE_PAR__J_PAR (IT'S BEEN SET TO ZERO IN THE MEANTIME)!!!!"

  const          = 6.242D*1.0D11

  ;; covarJParJePar = 0.D * jerr * jeErr

  ;; charEErr       = const * SQRT( (jeErr / j.y)^2.D + ( je.y * jErr / j.y^2.D )^2.D - 2.D * je.y / j.y^3.D * covarJParJePar )
  charEErr       = const * SQRT( (jeErr / j.y)^2.D + ( je.y * jErr / j.y^2.D )^2.D - 2.D * je.y / j.y^3.D * jje_coVar )

END

PRO ERROR_T,T,n,errors,Terr

  COMPILE_OPT IDL2,STRICTARRSUBS

  Tavg             = REFORM(T.y[3,*])
  PPar             = REFORM(T.y[2,*])*n.y*3.D
  PPrp             = REFORM(T.y[0,*])*n.y*3.D

  sigma_N_PPar     = errors.R[*,0,6]*(errors.N*N.y)*(errors.Pzz*PPar)
  sigma_N_PPrp     = errors.R[*,0,4]*(errors.N*N.y)*(errors.Pxx*PPrp)
  sigma_PPar_PPrp  = errors.R[*,6,4]*(errors.Pzz*PPar)*(errors.Pxx*PPrp)

  ;;pi=1/3N, and is for convenience
  piSq             = 1.D/(9.D*n.y*n.y)

  ;;other          = 2*pi*Tavg/N, and is also for convenience
  other            = 2.D*Tavg/(3.D*n.y*n.y)

  Terr             = SQRT( piSq*((errors.Pzz*PPar)^2.D                                        + $
                                 4.D*errors.R[*,6,4]*(errors.Pzz*PPar)*(errors.Pxx*PPrp)      + $
                                 4.D*(errors.Pxx*PPrp)^2.D                                    ) $
                           +                                                                    $
                           (-1.D)*other*(sigma_N_PPar     + $
                                         2.D*sigma_N_PPrp ) $
                           + $
                           (Tavg*errors.n)^(2.D)                                                )
                           ;; See? the n.y terms cancel each other. Hence the simplification above
                           ;; (Tavg/n.y*errors.n*n.y)^(2.D)

END

PRO ERROR_CALC,diff_eFlux,errors,n,j,je,T,nerr,jerr,jeErr,charEErr,Terr,jje_coVar;; , $
               ;; ENERGY_ERROR=enErr

  ;Raise
  ERROR_N ,n,errors,nerr

  ;The
  ERROR_J ,j,errors,jerr
  ERROR_JE,n,j,je,T,errors,jeErr
  ERROR_CHARE,j,je,jerr,jeErr,jje_coVar,errors,charEErr

  ;Stakes
  ;; IF KEYWORD_SET(enErr) THEN BEGIN
     ;; ERROR_T,T,n,enErr,errors,Terr
  ERROR_T,T,n,errors,Terr
  ;; ENDIF

END

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
   ADD_ONECOUNT_STATS=add_oneCount_stats, $
   ARANGE__MOMENTS_E_DOWN=aRange__moments_e_down, $
   ARANGE__MOMENTS_I_UP=aRange__moments_i_up, $
   WHICH_EEB__LABEL=label__which_eeb, $
   WHICH_TIMES__LABEL=label__which_times, $
   ENERGYARR=energyArr, $
   USE_PEAKE_BOUNDS_FOR_MOMENT_CALC=use_peakE_bounds_for_moments, $
   USE_SC_POT_FOR_LOWERBOUND=use_sc_pot_for_lowerbound, $
   POT__FROM_FA_POTENTIAL=pot__from_fa_potential, $
   POT__CHASTON_STYLE=pot__Chaston_style, $
   POT__FROM_FILE=pot__from_file, $
   POT__SAVE_FILE=pot__save_file, $
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
   SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
   MAP_TO_100KM=map_to_100km, $
   SAVECURPOTFILE=saveCurPotFile, $
   OUT_CURPOTLIST=curPotList

  COMPILE_OPT IDL2,STRICTARRSUBS

  IF ~KEYWORD_SET(timesList) THEN BEGIN
     PRINT,"Making timesList from downTimesStr and upTimesStr ..."
     downTimes               = REFORM(STR_TO_TIME(downTimesStr),SIZE(downTimesStr,/DIMENSIONS))
     upTimes                 = REFORM(STR_TO_TIME(upTimesStr  ),SIZE(upTimesStr  ,/DIMENSIONS))
     timesList               = LIST(downTimes,upTimes)
  ENDIF

  IF KEYWORD_SET(aRange__moments_e_down) THEN BEGIN

     CASE SIZE(aRange__moments_e_down,/TYPE) OF
        7: BEGIN
           IF STRMATCH(STRUPCASE(aRange__moments_e_down[0]),'*LC') THEN BEGIN
              fSuff = "-aR_mom_eD_LC"
           ENDIF ELSE BEGIN
              STOP
           ENDELSE
        END
        ELSE: BEGIN
           fSuff = STRING(FORMAT='("-aR_mom_eD_",I0,"-",I0)',aRange__moments_e_down[0],aRange__moments_e_down[1])
        END
     ENDCASE

     IF KEYWORD_SET(datFile) THEN BEGIN
        ADD_FNAME_SUFF,datFile,fSuff
     ENDIF

     IF KEYWORD_SET(saveCurPotFile) THEN BEGIN
        ADD_FNAME_SUFF,saveCurPotFile,fSuff
     ENDIF

  ENDIF

  IF KEYWORD_SET(aRange__moments_i_up) THEN BEGIN

     CASE SIZE(aRange__moments_i_up,/TYPE) OF
        7: BEGIN
           IF STRMATCH(STRUPCASE(aRange__moments_i_up[0]),'*LC') THEN BEGIN
              fSuff = "-aR_mom_iU_" + STRUPCASE(aRange__moments_i_up[0])
           ENDIF ELSE BEGIN
              STOP
           ENDELSE
        END
        ELSE: BEGIN

           IF ~(MIN(aRange__moments_i_up) EQ 0.) AND (MAX(aRange__moments_i_up) EQ 360.) THEN BEGIN

              fSuff = STRING(FORMAT='("-aR_mom_iU_",I0,"-",I0)',aRange__moments_i_up[0],aRange__moments_i_up[1])
              IF KEYWORD_SET(datFile) THEN BEGIN
                 ADD_FNAME_SUFF,datFile,fSuff
              ENDIF

              IF KEYWORD_SET(saveCurPotFile) THEN BEGIN
                 ADD_FNAME_SUFF,saveCurPotFile,fSuff
              ENDIF

           ENDIF

        END
     ENDCASE

  ENDIF

  IF KEYWORD_SET(use_sc_pot_for_lowerbound) THEN BEGIN

     ;; IF ~(MIN(aRange__moments_i_up) EQ 0.) AND (MAX(aRange__moments_i_up) EQ 360.) THEN BEGIN

        fSuff = '-sc_pot'
        IF KEYWORD_SET(datFile) THEN BEGIN
           ADD_FNAME_SUFF,datFile,fSuff
        ENDIF

        IF KEYWORD_SET(saveCurPotFile) THEN BEGIN
           ADD_FNAME_SUFF,saveCurPotFile,fSuff
        ENDIF

     ;; ENDIF

  ENDIF

  IF KEYWORD_SET(add_oneCount_stats) THEN BEGIN
     ;;Whatever datFile is, tack one '-oneCount' before the prefix

     fSuff = '-w_1Count'
     IF KEYWORD_SET(datFile) THEN BEGIN
        ADD_FNAME_SUFF,datFile,fSuff
     ENDIF

     IF KEYWORD_SET(saveCurPotFile) THEN BEGIN
        ADD_FNAME_SUFF,saveCurPotFile,fSuff
     ENDIF

  ENDIF

  IF KEYWORD_SET(elphic1998_defaults) THEN BEGIN
     eeb_or_eesArr           = KEYWORD_SET(eeb_or_eesArr) ? eeb_or_eesArr : ['ees','ies']

     ;; order                   = [0,2,1]
     order                   = [0,1,2]
     ;; order                   = [2,1,0]
     label                   = ['downgoing_e','upgoing_e','upgoing_i']

     label__which_eeb        = [0,0,1]
     label__which_times      = [0,0,0]
     ;; energyArr               = [[3e1,3.0e4],[3e1,3.0e4],[1e2,2.4e4]]
     ;; energyArr               = [[4,3.0e4],[4,3.0e4],[4,2.4e4]]
     IF ~KEYWORD_SET(energyArr) THEN BEGIN
        energyArr            = [[50,3.0e4],[50,3.0e4],[4,2.4e4]]
     ENDIF

     ;;Remember, !NULL means that the program will use the loss-cone angle range by default!
     aRange__moments_e_down  = KEYWORD_SET(aRange__moments_e_down) ? aRange__moments_e_down : [0.,360.]
     aRange__moments_i_up    = KEYWORD_SET(aRange__moments_i_up  ) ? aRange__moments_i_up   : [0.,360.]
     aRange__moments_e_up    = !NULL

     ;; aRange__moments_e_down  = [330.,30.]
     ;; aRange__moments_i_up    = [150.,210.]
     ;; aRange__moments_e_up    = [150.,210.]

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

  IF KEYWORD_SET(outDir) THEN BEGIN
     diffEfluxDir = outDir.Replace('cur_and_pot_analysis/','diff_eFlux/')
  ENDIF

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;MEGA
  nCalcLoop              = N_ELEMENTS(label)
  preString = datFile + ' ...'
  IF FILE_TEST(outDir+datFile) AND ~KEYWORD_SET(remake_masterFile) THEN BEGIN
     RESTORE,outDir+datFile
     afterString           = "Restored "

     also_oneCount         = ISA(n1_list) AND KEYWORD_SET(add_oneCount_stats)

  ENDIF ELSE BEGIN

     nDEFLoop              = N_ELEMENTS(eeb_or_eesArr)
     dEF_list              = LIST()
     dEF_1c_list           = LIST()
     north_southArr_list   = LIST()

     err_list              = LIST()
     err1_list             = LIST()

     n_list                = LIST()
     nerr_list             = LIST()

     T_list                = LIST()
     Terr_list             = LIST()

     j_list                = LIST()
     je_list               = LIST()
     chare_list            = LIST()

     jerr_list             = LIST()
     jeErr_list            = LIST()
     charEErr_list         = LIST()

     IF KEYWORD_SET(add_oneCount_stats) THEN BEGIN
        n1_list            = LIST()
        n1err_list         = LIST()

        T1_list            = LIST()
        T1err_list         = LIST()

        j1_list            = LIST()
        je1_list           = LIST()
        chare1_list        = LIST()
        j1err_list         = LIST()
        je1Err_list        = LIST()
        charE1Err_list     = LIST()

     ENDIF

     IF KEYWORD_SET(use_sc_pot_for_lowerbound) THEN BEGIN
        sc_pot_list        = LIST()
     ENDIF

     peak_ind_list         = LIST()
     peak_energy_list      = LIST()
     peak_dE_list          = LIST()
     aRange_oMoments_list  = LIST()
     aRange_oPeakEn_list   = LIST()
     aRange_oCharE_list    = LIST()

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
           ;; ADD_ONECOUNT_STATS=add_oneCount_stats, $
           SAVE_DIFF_EFLUX_TO_FILE=save_diff_eFlux_to_file ,$
           SAVE_DIFF_EFLUX_FILE=save_diff_eFlux_file,$
           LOAD_DIFF_EFLUX_FILE=load_diff_eFlux_file,$
           OUT_DIFF_EFLUX_FILE=diff_eFlux_file, $
           FIT2D__ONLY_FIT_PEAK_ERANGE=fit2D__only_fit_peak_eRange ,$
           FIT2D__ONLY_FIT_ABOVEMIN=fit2D__only_fit_aboveMin ,$
           FIT2D__DISABLE_BFUNC=fit2D__disable_bFunc ,$
           FIT2D__EXCLUDE_LCA_FROM_DENSCALC=fit2D__exclude_lca_from_densCalc ,$
           ;; FITFILE=fitFile, $
           LOADDIR=diffEfluxDir

        preString = diff_eFlux_file + ' ...'
        IF (FILE_TEST(diff_eFlux_file) OR FILE_TEST(diffEfluxDir+diff_eFlux_file)) AND $
           KEYWORD_SET(load_diff_eFlux_file) $
        THEN BEGIN
           afterString = "Restored "
           realFile    = (FILE_TEST(diff_eFlux_file) ? '' : diffEfluxDir ) + diff_eFlux_file
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
                          LOAD_DIR=diffEfluxDir
        ENDELSE
        PRINT,preString + afterString

        IF KEYWORD_SET(add_oneCount_stats) THEN BEGIN
           
           PRINT,"Getting oneCount curve ..."
           save_dEF_oneCount_to_file = KEYWORD_SET(save_diff_eFlux_to_file)
           GET_ONECOUNT_DIFF_EFLUX_CURVE,t1,t2, $
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
                                         SAVE_DEF_ONECOUNT_TO_FILE=save_dEF_oneCount_to_file, $
                                         LOAD_DAT_FROM_FILE=load_diff_eFlux_file, $
                                         DIFF_EFLUX_FILE=diff_eFlux_file, $
                                         LOAD_DIR=diffEfluxDir, $
                                         QUIET=quiet

           also_oneCount = N_ELEMENTS(dEF_oneCount) GT 0

           dEF_1c_list.Add,TEMPORARY(dEF_oneCount)

        ENDIF
        
        dEF_list.Add,TEMPORARY(diff_eFlux)

        IF KEYWORD_SET(use_sc_pot_for_lowerbound) THEN BEGIN
           GET_SC_POTENTIAL,T1=dEF_list[0].time[0],T2=dEF_list[0].time[-1], $
                            DATA=sc_pot, $
                            FROM_FA_POTENTIAL=pot__from_fa_potential, $
                            /REPAIR, $
                            CHASTON_STYLE=pot__Chaston_style, $
                            FROM_FILE=pot__from_file, $
                            ORBIT=orbit, $
                            SAVE_FILE=pot__save_file
           sc_pot_list.Add,sc_pot
        ENDIF

     ENDFOR

     ;;Now the calculations
     PRINT,"Beginning calculations"
     FOR fakeK=0,nCalcLoop-1 DO BEGIN

        k     = order[fakeK]
        t_k   = label__which_times[k]
        eeb_k = label__which_eeb[k]

        diff_eFlux                   = dEF_list[eeb_k]
        IF KEYWORD_SET(also_oneCount) THEN BEGIN
           dEF_oneCount              = dEF_1c_list[eeb_k]
        ENDIF
        IF KEYWORD_SET(use_sc_pot_for_lowerbound) THEN BEGIN
           sc_pot                    = sc_pot_list[eeb_k]
        ENDIF

        eeb_or_ees                   = eeb_or_eesArr[eeb_k]

        ;; energy                       = energyArr[*,k]
        energy                       = MAKE_ENERGY_ARRAYS__FOR_DIFF_EFLUX(diff_eFlux, $
                                                                          ENERGY=energyArr[*,k], $
                                                                          SC_POT=sc_pot, $
                                                                          EEB_OR_EES=eeb_or_ees)

        min_peak_energy              = min_peak_energyArr[k]
        max_peak_energy              = max_peak_energyArr[k]
        peak_energy__start_at_highE  = peak_energy__start_at_highEArr[k]
        upgoing                      = upgoingArr[k]

        ;;Set up, uh, n_below_peak and n_above_peak
        @kappa_fitter__defaults.pro

        tmpTimes                     = LIST_TO_1DARRAY(timesList[t_k])
        GET_FA_ORBIT,tmpTimes,/TIME_ARRAY
        GET_DATA,'ILAT',DATA=ilat
        north_southArr               = ABS(ilat.y)/ilat.y

        north_southArr_list.Add,TEMPORARY(north_southArr)

        GET_LOSS_CONE_AND_ANGLE_RANGES_FOR_HEMI, $
           t1,t2, $
           lc_angleRange, $
           i_angle,i_angle_up, $
           north_south, $
           OUT_LCW=lcw, $
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

        IF SIZE(aRange__moments[0],/TYPE) EQ 7 THEN BEGIN
           IF STRMATCH(STRUPCASE(aRange__moments[0]),'*LC') THEN BEGIN
              IF STRLEN(aRange__moments[0]) GT 2 THEN BEGIN
                 factor = FLOAT(STRSPLIT(STRUPCASE(aRange__moments[0]),'LC',/EXTRACT))
                 IF north_south[0] EQ -1 THEN BEGIN
                    aRange__moments = [180.-factor*lcw,180+factor*lcw]
                 ENDIF ELSE BEGIN
                    aRange__moments = [360.-factor*lcw,factor*lcw]
                 ENDELSE
              ENDIF ELSE BEGIN
                 aRange__moments = lc_angleRange
              ENDELSE

              IF upgoing THEN aRange__moments = (360.*((aRange__moments-180)/360.-FLOOR((aRange__moments-180)/360.)))

           ENDIF ELSE BEGIN
              PRINT,"Huh?"
              STOP
           ENDELSE
        ENDIF

        IF SIZE(aRange__peakEn[0],/TYPE) EQ 7 THEN BEGIN
           IF STRMATCH(STRUPCASE(aRange__peakEn[0]),'*LC') THEN BEGIN
              IF STRLEN(aRange__peakEn[0]) GT 2 THEN BEGIN
                 factor = FLOAT(STRSPLIT(STRUPCASE(aRange__peakEn[0]),'LC',/EXTRACT))
                 IF north_south[0] EQ -1 THEN BEGIN
                    aRange__peakEn = [180.-factor*lcw,180+factor*lcw]
                 ENDIF ELSE BEGIN
                    aRange__peakEn = [360.-factor*lcw,factor*lcw]
                 ENDELSE
              ENDIF ELSE BEGIN
                 aRange__peakEn  = lc_angleRange
              ENDELSE

              IF upgoing THEN aRange__peakEn = (360.*((aRange__peakEn-180)/360.-FLOOR((aRange__peakEn-180)/360.)))

           ENDIF ELSE BEGIN
              PRINT,"Huh?"
              STOP
           ENDELSE
        ENDIF

        IF SIZE(aRange__charE[0],/TYPE) EQ 7 THEN BEGIN
           IF STRMATCH(STRUPCASE(aRange__charE[0]),'*LC') THEN BEGIN
              IF STRLEN(aRange__charE[0]) GT 2 THEN BEGIN
                 factor = FLOAT(STRSPLIT(STRUPCASE(aRange__charE[0]),'LC',/EXTRACT))
                 IF north_south[0] EQ -1 THEN BEGIN
                    aRange__charE = [180.-factor*lcw,180+factor*lcw]
                 ENDIF ELSE BEGIN
                    aRange__charE = [360.-factor*lcw,factor*lcw]
                 ENDELSE
              ENDIF ELSE BEGIN
                 aRange__charE   = lc_angleRange
              ENDELSE

              IF upgoing THEN aRange__charE = (360.*((aRange__charE-180)/360.-FLOOR((aRange__charE-180)/360.)))

           ENDIF ELSE BEGIN
              PRINT,"Huh?"
              STOP
           ENDELSE
        ENDIF

        aRange_oMoments_list.Add,aRange__moments
        aRange_oPeakEn_list.Add,aRange__peakEn
        aRange_oCharE_list.Add,aRange__charE

        ;;Summary kind
        PRINT,FORMAT='("*****",A0,"*****")',STRUPCASE(label[k])
        PRINT,FORMAT='(A0,T30,":",T35,2(F0.2,:,","))',"aRange__moments",aRange__moments
        PRINT,FORMAT='(A0,T30,":",T35,2(F0.2,:,","))',"aRange__peakEn",aRange__peakEn
        PRINT,FORMAT='(A0,T30,":",T35,2(F0.2,:,","))',"aRange__charE",aRange__charE
        ;; PRINT,FORMAT='(A0,T30,":",T35,2(F0.2,:,","))',"angleRange",angleRange
        PRINT,FORMAT='(A0,T30,":",T35,2(F0.2,:,","))',"energy",MEAN(energy,DIMENSION=2)
        PRINT,FORMAT='(A0,T30,":",T35,2(F0.2,:,","))',"min_peak_energy",min_peak_energy
        PRINT,FORMAT='(A0,T30,":",T35,2(F0.2,:,","))',"max_peak_energy",max_peak_energy
        PRINT,FORMAT='(A0,T30,":",T35,2(F0.2,:,","))',"peak_energy__start_at_highE",peak_energy__start_at_highE
        PRINT,FORMAT='(A0,T30,":",T35,2(F0.2,:,","))',"upgoing",upgoing
        PRINT,""

        PRINT,"Getting eSpecs ..."
        eSpec                 = GET_EN_SPEC__FROM_DIFF_EFLUX( $
                                diff_eFlux, $
                                /RETRACE, $
                                ANGLE=aRange__peakEn, $
                                UNITS=units)

        IF KEYWORD_SET(also_oneCount) THEN BEGIN
           oneCount_eSpec     = GET_EN_SPEC__FROM_DIFF_EFLUX( $
                                dEF_oneCount, $
                                /RETRACE, $
                                ANGLE=aRange__peakEn, $
                                UNITS=units)
        ENDIF
        
        angles                = TRANSPOSE(diff_eFlux.theta,[1,0,2])
        energies              = TRANSPOSE(diff_eFlux.energy,[1,0,2])
        nEnergies             = N_ELEMENTS(eSpec.v[0,*])

        iAngle                = 0
        nHere                 = N_ELEMENTS(eSpec.x)
        peak_indArr           = MAKE_ARRAY(nHere,VALUE=-999,/LONG)
        peak_energyArr        = MAKE_ARRAY(nHere,VALUE=-999,/FLOAT)
        peak_dEArr            = MAKE_ARRAY(nHere,VALUE=-999,/FLOAT)
        peak_EBoundsArr       = MAKE_ARRAY(2,nHere,VALUE=-999,/FLOAT)
        FOR iTime=0,nHere-1 DO BEGIN

           XorigArr           = energies[*,*,iTime]
           ;; YorigArr        = data[*,*,iTime]
           ;; worigArr        = ddata[*,*,iTime]
           ;; IF KEYWORD_SET(KF2D__Plot_opt.add_oneCount_curve) THEN BEGIN
           ;;    oneCountArr  = oneCount_data[*,*,iTime]
           ;; END
           AorigArr           = angles[*,*,iTime]

           ;;And now the order becomes [angle,energy] for each of these arrays
           Xorig              = REFORM(eSpec.v[iTime,*])
           Yorig              = REFORM(eSpec.y[iTime,*])
           worig              = REFORM(eSpec.yerr[iTime,*])
           Aorig              = REFORM(AorigArr[iAngle,*])

           IF KEYWORD_SET(also_oneCount) THEN BEGIN
              oneCurve        = {x:Xorig, $
                                 y:REFORM(oneCount_eSpec.y[iTime,*]), $
                                 NAME:"One Count"}
           ENDIF
           
           IF KEYWORD_SET(min_peak_energy) THEN BEGIN
              ;;Try taking it from the top
              min_peak_ind    = MAX(WHERE(REFORM(XorigArr[0,*]) GE min_peak_energy))
              IF min_peak_ind EQ -1 THEN BEGIN
                 STOP
              ENDIF
           ENDIF ELSE BEGIN
              min_peak_ind    = nEnergies-1
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

           ;;Note that while these are called maxE and minE, suggesting they refer to the max energy and min energy, they do NOT. 
           ;;Rather, they refer to the lowest and highest indices falling within the user-specified parameters 
           ;;  for fitting—namely, n_below_peak and n_above_peak
           maxEInd                   = (peak_ind + n_below_peak) < nEnergies-1
           minEInd                   = (peak_ind - n_above_peak) > 0

           peak_dEArr[iTime]         = eSpec.vErr[iTime,peak_ind]
           peak_energyArr[iTime]     = TEMPORARY(peak_energy)
           peak_indArr[iTime]        = TEMPORARY(peak_ind)
           peak_EBoundsArr[*,iTime]  = [Xorig[TEMPORARY(minEInd)],Xorig[TEMPORARY(maxEInd)]]
        ENDFOR

        peak_ind_list.Add,TEMPORARY(peak_indArr)
        peak_energy_list.Add,TEMPORARY(peak_energyArr)
        peak_dE_list.Add,TEMPORARY(peak_dEArr)

        IF KEYWORD_SET(use_peakE_bounds_for_moments) THEN BEGIN
           energy[0,*]               = peak_EBoundsArr[0,*] > energy[0,*]
           energy[1,*]               = peak_EBoundsArr[1,*] < energy[1,*]
        ENDIF

        ;; en_arr   = MAKE_ENERGY_ARRAYS__FOR_DIFF_EFLUX(diff_eFlux, $
        ;;                                               ENERGY=energy, $
        ;;                                               SC_POT=sc_pot, $
        ;;                                               EEB_OR_EES=eeb_or_ees)
        ;; energy   = TEMPORARY(en_arr)

        n        = N_2D__FROM_DIFF_EFLUX(diff_eFlux, $
                                         ENERGY=energy, $
                                         ANGLE=aRange__moments, $
                                         SC_POT=sc_pot, $
                                         EEB_OR_EES=eeb_or_ees, $
                                         QUIET=quiet)
        T        = T_2D__FROM_DIFF_EFLUX(diff_eFlux, $
                                         ENERGY=energy, $
                                         ANGLE=aRange__moments, $
                                         SC_POT=sc_pot, $
                                         EEB_OR_EES=eeb_or_ees, $
                                         QUIET=quiet)
        j        = J_2D__FROM_DIFF_EFLUX(diff_eFlux, $
                                         ENERGY=energy, $
                                         ANGLE=aRange__moments, $
                                         SC_POT=sc_pot, $
                                         EEB_OR_EES=eeb_or_ees, $
                                         QUIET=quiet)
        je       = JE_2D__FROM_DIFF_EFLUX(diff_eFlux, $
                                          ENERGY=energy, $
                                          ANGLE=aRange__moments, $
                                          SC_POT=sc_pot, $
                                          EEB_OR_EES=eeb_or_ees, $
                                          QUIET=quiet)
        jC       = J_2D__FROM_DIFF_EFLUX(diff_eFlux, $
                                         ENERGY=energy, $
                                         ANGLE=aRange__charE, $
                                         SC_POT=sc_pot, $
                                         EEB_OR_EES=eeb_or_ees, $
                                         QUIET=quiet)
        jeC      = JE_2D__FROM_DIFF_EFLUX(diff_eFlux, $
                                          ENERGY=energy, $
                                          ANGLE=aRange__charE, $
                                          SC_POT=sc_pot, $
                                          EEB_OR_EES=eeb_or_ees, $
                                          QUIET=quiet)
        jje_coVar  = (TEMPORARY(JE_2D__FROM_DIFF_EFLUX(diff_eFlux, $
                                                       ENERGY=energy, $
                                                       ANGLE=aRange__charE, $
                                                       SC_POT=sc_pot, $
                                                       EEB_OR_EES=eeb_or_ees, $
                                                       /JJE, $
                                                       QUIET=quiet))).y - jeC.y*jC.y


        IF KEYWORD_SET(also_oneCount) THEN BEGIN

           n1    = N_2D__FROM_DIFF_EFLUX(def_onecount, $
                                         ENERGY=energy, $
                                         ANGLE=aRange__moments, $
                                         SC_POT=sc_pot, $
                                         EEB_OR_EES=eeb_or_ees, $
                                         QUIET=quiet)
           T1    = T_2D__FROM_DIFF_EFLUX(def_onecount, $
                                         ENERGY=energy, $
                                         ANGLE=aRange__moments, $
                                         SC_POT=sc_pot, $
                                         EEB_OR_EES=eeb_or_ees, $
                                         QUIET=quiet)
           j1    = J_2D__FROM_DIFF_EFLUX(dEF_oneCount, $
                                         ENERGY=energy, $
                                         ANGLE=aRange__moments, $
                                         SC_POT=sc_pot, $
                                         EEB_OR_EES=eeb_or_ees, $
                                         QUIET=quiet)
           je1   = JE_2D__FROM_DIFF_EFLUX(dEF_oneCount, $
                                          ENERGY=energy, $
                                          ANGLE=aRange__moments, $
                                          SC_POT=sc_pot, $
                                          EEB_OR_EES=eeb_or_ees, $
                                          QUIET=quiet)
           j1C   = J_2D__FROM_DIFF_EFLUX(dEF_oneCount, $
                                         ENERGY=energy, $
                                         ANGLE=aRange__charE, $
                                         SC_POT=sc_pot, $
                                         EEB_OR_EES=eeb_or_ees, $
                                         QUIET=quiet)
           je1C  = JE_2D__FROM_DIFF_EFLUX(dEF_oneCount, $
                                          ENERGY=energy, $
                                          ANGLE=aRange__charE, $
                                          SC_POT=sc_pot, $
                                          EEB_OR_EES=eeb_or_ees, $
                                          QUIET=quiet)
           jje1_coVar = (TEMPORARY(JE_2D__FROM_DIFF_EFLUX(dEF_oneCount, $
                                          ENERGY=energy, $
                                          ANGLE=aRange__charE, $
                                          SC_POT=sc_pot, $
                                          /JJE, $
                                          EEB_OR_EES=eeb_or_ees, $
                                          QUIET=quiet))).y - je1C.y * j1C.y

        ENDIF

        ;;Error everything
        IF KEYWORD_SET(error_estimates) THEN BEGIN

           errors          = MOMENTERRORS_2D__FROM_DIFF_EFLUX(diff_eFlux, $
                                                              ENERGY=energy, $
                                                              ANGLE=aRange__moments, $
                                                              SC_POT=sc_pot, $
                                                              EEB_OR_EES=eeb_or_ees, $
                                                              ;; PRESSURE_COVAR_CALC=pressure_covar_calc, $
                                                              /PRESSURE_COVAR_CALC, $
                                                              ;; HEATFLUX_COVAR_CALC=heatFlux_covar_calc, $
                                                              /HEATFLUX_COVAR_CALC, $
                                                              QUIET=quiet)

           ;; IF KEYWORD_SET(dens_errors) THEN BEGIN
           ;; nerr            = MAKE_ARRAY(nHere,/FLOAT)
           ;; jerr            = MAKE_ARRAY(nHere,/FLOAT)
           ;; Terr            = MAKE_ARRAY(nHere,/FLOAT)

           ERROR_CALC,diff_eFlux,errors,n,j,je,T,nerr,jerr,jeErr,charEErr,Terr,jje_coVar
           ;; ERROR_CALC,diff_eFlux,errors,j,je,n,T,jerr,jeErr,nerr,Terr

           IF KEYWORD_SET(also_oneCount) THEN BEGIN
              errors1      = MOMENTERRORS_2D__FROM_DIFF_EFLUX(dEF_oneCount, $
                                                              ENERGY=energy, $
                                                              ANGLE=aRange__moments, $
                                                              SC_POT=sc_pot, $
                                                              EEB_OR_EES=eeb_or_ees, $
                                                              ;; PRESSURE_COVAR_CALC=pressure_covar_calc, $
                                                              /PRESSURE_COVAR_CALC, $
                                                              ;; HEATFLUX_COVAR_CALC=heatFlux_covar_calc, $
                                                              /HEATFLUX_COVAR_CALC, $
                                                              QUIET=quiet)
              ;; n1err        = MAKE_ARRAY(nHere,/FLOAT)
              ;; j1err        = MAKE_ARRAY(nHere,/FLOAT)
              ;; T1err        = MAKE_ARRAY(nHere,/FLOAT)

              ERROR_CALC,dEF_oneCount,errors1,n1,j1,je1,T1,n1err,j1err,je1Err,charE1Err,T1err,jje1_coVar
              ;; ERROR_CALC,dEF_oneCount,errors1,j1,je1,n1,T1,j1err,je1Err,n1err,T1err

           ENDIF

           ;; FOR l=0,N_ELEMENTS(diff_eFlux.time)-1 DO BEGIN
           ;;    jerr[l]      = SQRT((j.y[l])^(2.D) * $
           ;;                    ( (errors[l].n)^(2.D) + (errors[l].Uz)^(2.D) + errors[l].n*errors[l].Uz*errors[l].R[0,3] ) )
           ;;    nerr[l]      = n.y[l]  * errors[l].n
           ;;    ;; Terr[l]      = 
           ;; ENDFOR
           ;; IF KEYWORD_SET(also_oneCount) THEN BEGIN
           ;;    ;; FOR l=0,N_ELEMENTS(dEF_oneCount.time)-1 DO BEGIN
           ;;    ;;    j1err[l]  = SQRT((j1.y[l])^(2.D) * $
           ;;    ;;                    ( (errors1[l].n)^(2.D) + (errors1[l].Uz)^(2.D) + errors1[l].n*errors1[l].Uz*errors1[l].R[0,3] ) )
           ;;    ;;    n1err[l]  = n1.y[l] * errors1[l].n
           ;;    ;;    ;; T1err[l]  = 
           ;;    ;; ENDFOR
           ;; ENDIF

        ENDIF

        ;;Update lists
        err_list.Add,TEMPORARY(errors)
        n_list.Add,TEMPORARY(n)
        T_list.Add,TEMPORARY(T)
        j_list.Add,TEMPORARY(j)
        je_list.Add,TEMPORARY(je)
        chare_list.Add,CHAR_ENERGY((TEMPORARY(jC)).y,(TEMPORARY(jeC)).y)
        nerr_list.Add,TEMPORARY(nerr)
        jerr_list.Add,TEMPORARY(jerr)
        jeErr_list.Add,TEMPORARY(jeErr)
        charEErr_list.Add,TEMPORARY(charEErr)
        Terr_list.Add,TEMPORARY(Terr)

        IF KEYWORD_SET(also_oneCount) THEN BEGIN
           err1_list.Add,TEMPORARY(errors1)
           n1_list.Add,TEMPORARY(n1)
           T1_list.Add,TEMPORARY(T1)
           j1_list.Add,TEMPORARY(j1)
           je1_list.Add,TEMPORARY(je1)
           chare1_list.Add,CHAR_ENERGY((TEMPORARY(j1C)).y,(TEMPORARY(je1C)).y)
           n1err_list.Add,TEMPORARY(n1err)
           j1err_list.Add,TEMPORARY(j1err)
           je1Err_list.Add,TEMPORARY(je1Err)
           charE1Err_list.Add,TEMPORARY(charE1Err)
           T1err_list.Add,TEMPORARY(T1err)
        ENDIF

     ENDFOR

     afterString = "Made "
     SAVE,north_southArr_list, $
          err_list, $
          err1_list, $
          n_list, $
          j_list, $
          je_list, $
          chare_list, $
          T_list, $
          nerr_list, $
          jerr_list, $
          jeErr_list, $
          charEErr_list, $
          Terr_list, $
          n1_list, $
          j1_list, $
          je1_list, $
          chare1_list, $
          T1_list, $
          n1err_list, $
          j1err_list, $
          je1Err_list, $
          charE1Err_list, $
          T1err_list, $
          peak_ind_list, $
          peak_energy_list,peak_dE_list, $
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

     itvlTime       = !NULL
     itvlN          = !NULL
     itvlJ          = !NULL
     itvlJe         = !NULL
     itvlcharE      = !NULL
     itvlPeakE      = !NULL
     itvlT          = !NULL
     itvlCur        = !NULL
     itvlNerr       = !NULL
     itvlJerr       = !NULL
     itvlJeErr      = !NULL
     itvlcharEErr   = !NULL
     itvlPeakdE     = !NULL
     itvlTerr       = !NULL
     itvlCurErr     = !NULL
     itvlErrors     = !NULL

     itvlN1         = !NULL
     itvlJ1         = !NULL
     itvlJe1        = !NULL
     itvlcharE1     = !NULL
     itvlT1         = !NULL
     itvlCur1       = !NULL
     itvlN1err      = !NULL
     itvlJ1err      = !NULL
     itvlJe1Err     = !NULL
     itvlCharE1Err  = !NULL
     itvlT1err      = !NULL
     itvlCur1Err    = !NULL
     itvlErrors1    = !NULL

     ;; itvlErrors  = MAKE_BLANK_GERSHMAN_ERROR_STRUCT(2000, $
     ;;                                                /PRESSURE_COVAR_CALC, $
     ;;                                                HEATFLUX_COVAR_CALC=heatFlux_covar_calc)

     FOR realK=0,nSegs-1 DO BEGIN

        tmpT1               = tmpT[0,realK]
        tmpT2               = tmpT[1,realK]

        theseInds           = WHERE( ( (j_list[k]).x GE tmpT1 ) AND $
                               ( (j_list[k]).x LE tmpT2 ), $
                               nThese)

        IF nThese EQ 0 THEN STOP

        theseInds           = CGSETINTERSECTION(theseInds,UNIQ((j_list[k].x),SORT((j_list[k].x))),COUNT=nThese)
        CHECK_SORTED,(j_list[k].x)[theseInds],is_sorted,/QUIET
        IF ~is_sorted THEN STOP
        IF nThese EQ 0 THEN STOP

        tmpTimes            = (j_list[k]).x[theseInds]

        ;;Pick up temps
        tmpN                = (N_list[k]).y[theseInds]
        tmpJ                = (j_list[k]).y[theseInds]
        tmpJe               = (je_list[k]).y[theseInds]
        ;; tmpCharE         = CHAR_ENERGY(tmpJ,tmpJe)
        tmpCharE            = (chare_list[k])[theseInds]
        tmpPeakE            = (peak_energy_list[k])[theseInds]
        tmpPeakdE           = (peak_dE_list[k])[theseInds]
        tmpT                = (T_list[k]).y[*,theseInds]

        IF KEYWORD_SET(also_oneCount) THEN BEGIN
           
           tmpN1            = (N1_list[k]).y[theseInds]
           tmpJ1            = (j1_list[k]).y[theseInds]
           tmpJe1           = (je1_list[k]).y[theseInds]
           tmpCharE1        = (chare1_list[k])[theseInds]
           tmpT1            = (T1_list[k]).y[*,theseInds]

        ENDIF

        ;; IF KEYWORD_SET(error_estimates) AND KEYWORD_SET(dens_errors) THEN BEGIN
        IF KEYWORD_SET(error_estimates) THEN BEGIN

           tmpNerr          = (nerr_list[k])[theseInds]
           tmpJerr          = (jerr_list[k])[theseInds]
           tmpJeErr         = (jeErr_list[k])[theseInds]
           tmpcharEErr      = (charEErr_list[k])[theseInds]
           tmpTerr          = (Terr_list[k])[theseInds]
           ;; SHRINK_GERSHMAN_ERROR_STRUCT,err_list[k],theseInds,tmpErrors

           IF KEYWORD_SET(also_oneCount) THEN BEGIN
              tmpN1err      = (n1err_list[k])[theseInds]
              tmpJ1err      = (j1err_list[k])[theseInds]
              tmpJe1Err     = (je1Err_list[k])[theseInds]
              tmpcharE1Err  = (charE1Err_list[k])[theseInds]
              tmpT1err      = (T1err_list[k])[theseInds]
              ;; SHRINK_GERSHMAN_ERROR_STRUCT,err1_list[k],theseInds,tmpErrors1
           ENDIF
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

           tmpJ         *= mapRatio
           tmpJe        *= mapRatio

           IF KEYWORD_SET(also_oneCount) THEN BEGIN
              tmpJ1     *= mapRatio
              tmpJe1    *= mapRatio
           ENDIF

           ;; IF KEYWORD_SET(error_estimates) AND KEYWORD_SET(dens_errors) THEN BEGIN
           IF KEYWORD_SET(error_estimates) THEN BEGIN
              tmpJerr   *= mapRatio

              IF KEYWORD_SET(also_oneCount) THEN BEGIN
                 tmpJ1err *= mapRatio
              ENDIF

           ENDIF

        ENDIF

        ;;Get current (flip sign of current for electrons)
        tmpCur            = tmpJ  * 1.6e-9 * (ions ? 1. : (-1.))
        IF KEYWORD_SET(also_oneCount) THEN BEGIN
           tmpCur1        = tmpJ1 * 1.6e-9 * (ions ? 1. : (-1.))
        ENDIF
        
        ;; IF KEYWORD_SET(error_estimates) AND KEYWORD_SET(dens_errors) THEN BEGIN
        IF KEYWORD_SET(error_estimates) THEN BEGIN
           ;;Don't flip sign, children
           tmpCurErr      = tmpJerr  * 1.6e-9 ;* (ions ? 1. : (-1.))

           IF KEYWORD_SET(also_oneCount) THEN BEGIN
              tmpCur1Err  = tmpJ1err * 1.6e-9 ;* (ions ? 1. : (-1.))
           ENDIF
        ENDIF

        ;;Make outward current positive in both hemis
        ;; ;;(You know, field lines going in at the NH, going out at the SH, yadda yadda)
        ;;I take it all back, Elphic et al. [1998] make upward current (downgoing electrons) negative

        ;; CASE N_ELEMENTS(WHERE(tmpNS GT 0,/NULL)) OF
        CASE N_ELEMENTS(WHERE(tmpNS LT 0,/NULL)) OF
           N_ELEMENTS(tmpNS): BEGIN
              tmpCur  *= (-1.)

              IF KEYWORD_SET(also_oneCount) THEN BEGIN
                 tmpCur1 *= (-1.)
              ENDIF
              ;;No, don't flip sign on error
              ;; IF KEYWORD_SET(error_estimates) AND KEYWORD_SET(dens_errors) THEN BEGIN
              ;; IF KEYWORD_SET(error_estimates) THEN BEGIN
              ;;    tmpCurErr  *= (-1.)
              ;;    tmpCur1Err *= (-1.)
              ;; ENDIF

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

        itvlTime             = [itvlTime      ,tmpTimes     ]
        itvlN                = [itvlN         ,tmpN         ]
        itvlJ                = [itvlJ         ,tmpJ         ]
        itvlJe               = [itvlJe        ,tmpJe        ]
        itvlcharE            = [itvlcharE     ,tmpChare     ]
        itvlT                = [itvlT         ,tmpT         ]
        itvlCur              = [itvlCur       ,tmpCur       ]

        IF KEYWORD_SET(also_oneCount) THEN BEGIN

           itvlN1            = [itvlN1        ,tmpN1        ]
           itvlJ1            = [itvlJ1        ,tmpJ1        ]
           itvlJe1           = [itvlJe1       ,tmpJe1       ]
           itvlcharE1        = [itvlcharE1    ,tmpChare1    ]
           itvlT1            = [itvlT1        ,tmpT1        ]
           itvlCur1          = [itvlCur1      ,tmpCur1      ]

        ENDIF
        
        ;; IF KEYWORD_SET(error_estimates) AND KEYWORD_SET(dens_errors) THEN BEGIN
        IF KEYWORD_SET(error_estimates) THEN BEGIN

           itvlNerr          = [itvlNerr      ,tmpNerr      ]
           itvlJerr          = [itvlJerr      ,tmpJerr      ]
           itvlJeErr         = [itvlJeErr     ,tmpJeErr     ]
           itvlcharEErr      = [itvlcharEErr  ,tmpcharEErr  ]
           itvlTerr          = [itvlTerr      ,tmpTerr      ]
           itvlCurErr        = [itvlCurErr    ,tmpCurErr    ]

           SHRINK_GERSHMAN_ERROR_STRUCT       ,err_list[k   ],theseInds,itvlErrors, $
                                               /ADD_TO_ERROROUT

           IF KEYWORD_SET(also_oneCount) THEN BEGIN

              itvlN1err      = [itvlN1err     ,tmpN1err     ]
              itvlJ1err      = [itvlJ1err     ,tmpJ1err     ]
              itvlJe1Err     = [itvlJe1Err    ,tmpJe1Err    ]
              itvlcharE1Err  = [itvlcharE1Err ,tmpcharE1Err ]
              itvlT1err      = [itvlT1err     ,tmpT1err     ]
              itvlCur1Err    = [itvlCur1Err   ,tmpCur1Err   ]
              SHRINK_GERSHMAN_ERROR_STRUCT    ,err1_list[k  ],theseInds,itvlErrors1, $
                                               /ADD_TO_ERROROUT
           ENDIF
        ENDIF

        itvlPeakE            = [itvlPeakE     ,tmpPeakE     ]
        itvlPeakdE           = [itvlPeakdE    ,tmpPeakdE    ]

     ENDFOR

     CASE 1 OF
        KEYWORD_SET(error_estimates): BEGIN

           tmpStruct      = {label    : label[k],$
                             time     : TEMPORARY(itvlTime)   , $
                             N        : TEMPORARY(itvlN)      , $
                             j        : TEMPORARY(itvlJ)      , $
                             je       : TEMPORARY(itvlJe)     , $
                             chare    : TEMPORARY(itvlcharE)  , $
                             T        : TEMPORARY(itvlT)      , $
                             cur      : TEMPORARY(itvlCur)    , $
                             Nerr     : TEMPORARY(itvlNerr)   , $
                             Jerr     : TEMPORARY(itvlJerr)   , $
                             JeErr    : TEMPORARY(itvlJeErr)   , $
                             charEErr : TEMPORARY(itvlcharEErr)   , $
                             Terr     : TEMPORARY(itvlTerr)   , $
                             CurErr   : TEMPORARY(itvlCurErr) , $
                             errors   : TEMPORARY(itvlErrors)}

           IF KEYWORD_SET(also_oneCount) THEN BEGIN
              tmp1Struct  = {N1       : TEMPORARY(itvlN1)     , $
                             j1       : TEMPORARY(itvlJ1)     , $
                             je1      : TEMPORARY(itvlJe1)    , $
                             cur1     : TEMPORARY(itvlCur1)   , $
                             chare1   : TEMPORARY(itvlcharE1) , $
                             T1       : TEMPORARY(itvlT1)     , $
                             N1err    : TEMPORARY(itvlN1err)  , $
                             J1Err    : TEMPORARY(itvlJ1Err)  , $
                             Je1Err    : TEMPORARY(itvlJe1Err)   , $
                             charE1Err : TEMPORARY(itvlcharE1Err)   , $
                             T1err    : TEMPORARY(itvlT1err)  , $
                             Cur1Err  : TEMPORARY(itvlCur1Err) , $
                             errors1  : TEMPORARY(itvlErrors1)}
              tmpStruct   = CREATE_STRUCT(TEMPORARY(tmpStruct),TEMPORARY(tmp1Struct))
           ENDIF

           bonus          = {peakE    : TEMPORARY(itvlPeakE)               , $
                             peakErr  : TEMPORARY(itvlPeakdE)              , $
                             energy   : energyArr[*,k]                     , $
                             angles   : {charE   : aRange_oCharE_list[k]   , $
                                         moments : aRange_oMoments_list[k] , $
                                         peakEn  : aRange_oPeakEn_list[k]} $
                            }

           tmpStruct      = CREATE_STRUCT(TEMPORARY(tmpStruct),TEMPORARY(bonus))
           
        END
        ELSE: BEGIN
           tmpStruct      = {label    : label[k],$
                             time     : TEMPORARY(itvlTime)   , $
                             N        : TEMPORARY(itvlN)      , $
                             j        : TEMPORARY(itvlJ)      , $
                             je       : TEMPORARY(itvlJe)     , $
                             chare    : TEMPORARY(itvlcharE)  , $
                             T        : TEMPORARY(itvlT)      , $
                             cur      : TEMPORARY(itvlCur)    }

           IF KEYWORD_SET(also_oneCount) THEN BEGIN
              tmp1Struct  = {j1       : TEMPORARY(itvlJ1)     , $
                             je1      : TEMPORARY(itvlJe1)    , $
                             cur1     : TEMPORARY(itvlCur1)   , $
                             chare1   : TEMPORARY(itvlcharE1) , $
                             N1       : TEMPORARY(itvlN1)     , $
                             T1       : TEMPORARY(itvlT1)     }
              tmpStruct   = CREATE_STRUCT(TEMPORARY(tmpStruct),TEMPORARY(tmp1Struct))
           ENDIF

           bonus          = {peakE    : TEMPORARY(itvlPeakE)               , $
                             peakErr  : TEMPORARY(itvlPeakdE)              , $
                             energy   : energyArr[*,k]                     , $
                             angles   : {charE   : aRange_oCharE_list[k]   , $
                                         moments : aRange_oMoments_list[k] , $
                                         peakEn  : aRange_oPeakEn_list[k]} $
                            }

           tmpStruct      = CREATE_STRUCT(TEMPORARY(tmpStruct),TEMPORARY(bonus))
           
        END
     ENDCASE

     curPotList.Add,tmpStruct
  ENDFOR

  ;; looking         = 3B
  ;; ind             = 0
  ;; WHILE (looking GT 0) DO BEGIN
  ;;    IF STRMATCH(STRUPCASE(curPotList[ind].label),'*DOWN*E') THEN BEGIN
  ;;       looking--
  ;;       edind = ind
  ;;    ENDIF

  ;;    IF STRMATCH(STRUPCASE(curPotList[ind].label),'*UP*E') THEN BEGIN
  ;;       looking--
  ;;       euind = ind
  ;;    ENDIF

  ;;    IF STRMATCH(STRUPCASE(curPotList[ind].label),'*UP*I') THEN BEGIN
  ;;       looking--
  ;;       iuind = ind
  ;;    ENDIF
  ;;    ind++
  ;; ENDWHILE


  IF KEYWORD_SET(saveCurPotFile) THEN BEGIN
     PRINT,"Saving it all to " + saveCurPotFile
     SAVE,curPotList,FILENAME=outDir+saveCurPotFile
  ENDIF

END
