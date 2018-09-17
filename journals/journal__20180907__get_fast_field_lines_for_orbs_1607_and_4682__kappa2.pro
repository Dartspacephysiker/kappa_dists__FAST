;2018/09/07
PRO JOURNAL__20180907__GET_FAST_FIELD_LINES_FOR_ORBS_1607_AND_4682__KAPPA2

  COMPILE_OPT IDL2,STRICTARRSUBS

  @common__jv_curve_fit__tie_r_b_and_dens.pro
  ;; Which contains COMMON tieRB,tRB_RBpairs,tRB_fLine,tRB_nFAST,tRB_nFLine,tRB_fLineRE

  dir = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/cur_and_pot_analysis/'
  orbit = 1607

  ;; From JOURNAL__20180807__KAPPA_PAPER2_ORBITS_1607_AND_1612
  jv_theor__initial_source__Polar = 1

  CASE orbit OF
     1607: BEGIN
        dato = '1997-01-17/'

        ;; cAP_tRanges = dato + [['01:04:24.276','01:04:49.521']]
        cAP_tRanges = dato + [['01:04:28.0','01:04:41.3']] ;2018/06/11

        ;; eff sample rate we use is 0.63 s
        f1 = 'Orbit_1607--2NDKAPPA-ingredients-01_04_20__500-01_05_54__000-sc_pot-sRate0_63.sav'
        f2 = 'Orbit_1607--2NDKAPPA-meal-01_04_20__500-01_05_54__000-sc_pot-sRate0_63.sav'
     END
     4682: BEGIN
        dato = '1997-10-28/'

        cAP_tRanges = dato + [['09:06:31','09:06:51.5']] ;2018/08/13

        ;; eff sample rate is actually 2.5 s, not 1.25 s
        f1 = 'Orbit_4682--2NDKAPPA-ingredients-09_05_40__000-09_06_55__000-sc_pot-sRate1_25.sav'
        f2 = 'Orbit_4682--2NDKAPPA-meal-09_05_40__000-09_06_55__000-sc_pot-sRate1_25.sav'
     END
  ENDCASE

  f = {ingred : f1, $
       meal   : f2}

  RESTORE,dir+f.ingred
  RESTORE,dir+f.meal


  t1 = S2T(cAP_tRanges[0])
  t2 = S2T(cAP_tRanges[1])

  ;; mRatio comes from f.meal file
  ;; mRatio = GET_FA_MIRROR_RATIO__UTC(time_list[0], $
  ;;                                   /TIME_ARRAY, $
  ;;                                   ;; USE_FAST_AS_IONOS=~KEYWORD_SET(map_to_100km), $
  ;;                                   TO_EQUATOR=jv_theor__initial_source__equator, $
  ;;                                   TO_POLAR_SATELLITE=jv_theor__initial_source__Polar, $
  ;;                                   TO_THIS_RE=jv_theor__initial_source_R_E, $
  ;;                                   TO_THIS_KM=to_km)


  useInds = WHERE(time_list[0] GE t1 AND time_list[0] LE t2,nUseInds)

  IF nUseInds EQ 0 THEN STOP

  GET_FA_FIELD_LINE,time_list[0], $
                    USEINDS=useInds, $
                    /TIME_ARRAY, $
                    MRATIO=mRatio

  FOR k=0,N_ELEMENTS(trb_fline[*,0])-1 DO PRINT,REFORM(trb_fline[k,*])

  IF N_ELEMENTS(tRB_fLineRE) GT 0 THEN BEGIN
     make_R_E_axis    = 1
     R_B_axis_vals    = [10^.5,10]

     maxRBVal = 20000

     WHILE MAX(R_B_axis_vals) LT maxRBVal DO BEGIN
        R_B_axis_vals = [R_B_axis_vals,(R_B_axis_vals[-1]*10^.5) < maxRBVal]
     ENDWHILE

     R_B_axis_names   = STRING(FORMAT='(I0)',R_B_axis_vals)
     ;; R_B_axis_names   = ['5','10','100','10!U3!N','10!U4!N']

     R_E_axis_vals    = INTERPOL(tRB_fLineRE,REFORM(tRB_RBpairs[1,*]),R_B_axis_vals)
     nVals            = N_ELEMENTS(R_E_axis_vals)

     R_E_axis_names   = STRING(FORMAT='(F0.2)',R_E_axis_vals)

  ENDIF

  nVals = N_ELEMENTS(R_B_axis_names)

  PRINT,"Orbit : ",orbit
  PRINT,'axVals={'
  FOR k=0,nVals-1 DO BEGIN
     PRINT,FORMAT='("{",I0,",",A0,"}")',R_B_axis_vals[k],R_E_axis_names[k]
  ENDFOR
  PRINT,'}'

  ;; tRB_RBpairs: Indexed as R_B_Fast,R_B_ionos
  ;; tRB_fLine : 
  ;; tRB_nFAST :
  ;; tRB_nFLine :
  ;; tRB_fLineRE :

END
