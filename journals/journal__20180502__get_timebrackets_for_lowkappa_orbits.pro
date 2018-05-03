;2018/05/02
PRO JOURNAL__20180502__GET_TIMEBRACKETS_FOR_LOWKAPPA_ORBITS

  COMPILE_OPT IDL2,STRICTARRSUBS

  minM  = 21
  maxM  = 24
  notMLT = 0
  minI  = 60
  maxI  = 90
  hemi  = 'BOTH'

  GoverKReq = 5
  KChi2Max = 5.0
  minA = 3000

  LOAD_KAPPAFIT_DB,andre, $
                   KF2DPARMS=KF2DParms, $
                   GF2DPARMS=GF2DParms, $
                   TOTT=totT, $
                   ORBARR=orbArr

  final_i = GET_KAPPADB_INDS(andre, $
                             KF2DParms, $
                             GF2DParms, $
                             OUT_COUNT=count, $
                             GOVERKREQ=GoverKReq, $
                             KCHI2MAX=KChi2Max, $
                             MINMLT=minM, $
                             MAXMLT=maxM, $
                             BINMLT=binM, $
                             SHIFTMLT=shiftM, $
                             MINILAT=minI, $
                             MAXILAT=maxI, $
                             MINALT=minA, $
                             MAXALT=maxA, $
                             MLTSTR=mltStr, $
                             ALTSTR=altStr, $
                             HEMI=hemi, $
                             NORTH=north, $
                             SOUTH=south, $
                             BOTH_HEMIS=both_hemis, $
                             GLOBE=globe, $
                             DAYSIDE=dayside, $
                             NIGHTSIDE=nightside)

  ;; Unique low kappa-ers
  lowKappa_i        = WHERE(KF2DParms.kappa LE 2.0,nLowKappa, $
                            COMPLEMENT=notLowKappa_i, $
                            NCOMPLEMENT=nNotLowKappa)
  lowKappa_i        = CGSETINTERSECTION(lowKappa_i,final_i, $
                                        COUNT=nLowKappa)
  lowKappaOrbs      = andre.orbit[lowkappa_i[UNIQ(andre.orbit[lowkappa_i], $
                                                  SORT(andre.orbit[lowkappa_i]))]]
  allOrbs           = andre.orbit[UNIQ(andre.orbit, $
                                       SORT(andre.orbit))]
  
  
  FOREACH bro,lowKappaOrbs,ind DO BEGIN

     theseInds = WHERE(andre.orbit EQ bro,nThese)

     tmpTimes = andre.time[theseInds]
     t1       = MIN(tmpTimes)
     t2       = MAX(tmpTimes)

     t1Str    = T2S(t1,/MS)
     t2Str    = T2S(t2,/MS)
     date     = STRMID(t1Str,0,10)
     t1Str    = STRMID(t1Str,11)
     t2Str    = STRMID(t2Str,11)

     PRINT,FORMAT='(I3,TR2,I04," : ",A0,"/ ",A0," - ",A0)', $
           ind+1, $
           bro, $
           date, $
           t1Str, $
           t2Str

  ENDFOREACH

  PRINT,''
  PRINT,''
  FOREACH bro,lowKappaOrbs,ind DO BEGIN

     ;; theseInds = WHERE(andre.orbit EQ bro,nThese)
     theseInds = lowKappa_i[WHERE(andre.orbit[lowKappa_i] EQ bro,nThese)]

     tmpTimes = andre.time[theseInds]
     t1       = MIN(tmpTimes)
     t2       = MAX(tmpTimes)

     t1Str    = T2S(t1,/MS)
     t2Str    = T2S(t2,/MS)
     date     = STRMID(t1Str,0,10)
     t1Str    = STRMID(t1Str,11)
     t2Str    = STRMID(t2Str,11)

     PRINT,'****************************************'
     PRINT,FORMAT='(I3,TR2,I04," : ",A0,"/ ",A0," - ",A0)', $
           ind+1, $
           bro, $
           date, $
           t1Str, $
           t2Str
     PRINT,'****************************************'
     PRINT,''
     tmpTimes = STRMID(T2S(tmpTimes,/MS),11)
     FOREACH this,theseInds,thisInd DO $
        PRINT,FORMAT='(A5,TR5,I3,TR5,A12,TR5,F6.2,TR5,F6.2,TR5,F6.2,TR5,F6.2)', $
              (KF2DParms.kappa[this] LE 2 ? '*' : ''), $
              thisInd,tmpTimes[thisInd], $
              KF2DParms.kappa[this], $
              KF2DParms.chi2red[this], $
              GF2DParms.chi2red[this], $
              GF2DParms.chi2red[this]/KF2DParms.chi2red[this]
              
     ;; ENDFOREACH
     PRINT,''

  ENDFOREACH


END
