	;2017/03/13
PRO JOURNAL__20170313__FIND_DAYSIDE_MONO_AURORA

  COMPILE_OPT IDL2,STRICTARRSUBS

  LOAD_NEWELL_ESPEC_DB,eSpec,/NO_MEMORY_LOAD
  
  ;; minMLT          = 6.0
  ;; maxMLT          = 17.0

  ;;nightside
  minMLT          = 20.5
  maxMLT          = 2.5

  ;;time streak stuff
  acceptable_dt   = 3.0
  min_streakLen_t = 25
  min_nPts        = 20
  
  m_i = WHERE(eSpec.mono EQ 1 OR eSpec.mono EQ 2,nM)

  CASE 1 OF
     minMLT GT maxMLT: BEGIN
        day_i = WHERE(eSpec.mlt GE minMLT OR eSpec.mlt LE maxMLT,nDay)
     END
     ELSE: BEGIN
        day_i = WHERE(eSpec.mlt GE minMLT AND eSpec.mlt LE maxMLT,nDay)
     END
  ENDCASE

  mday_i = CGSETINTERSECTION(day_i,m_i,COUNT=nMDay)

  IF nMDay EQ 0 THEN BEGIN
     PRINT,"Bro"
     RETURN
  ENDIF

  GET_DOUBLE_STREAKS__NTH_DECIMAL_PLACE,eSpec.x[m_i],0, $
                                        GAP_TIME=acceptable_dt, $
                                        NPTS=min_nPts, $
                                        MIN_T_STREAKLEN=min_streakLen_t, $
                                        START_I=strt_ii, $
                                        STOP_I=stop_ii, $
                                        STREAKLENS=streakLens, $
                                        T_STREAKLENS=streakLens_t, $
                                        NSTREAKS=nStreaks

  
  tStrStreaks = TRANSPOSE([[TIME_TO_STR(eSpec.x[m_i[strt_ii]])],[TIME_TO_STR(eSpec.x[m_i[stop_ii]])]])

  sortMe = SORT(streakLens_t)
  FOR k=0,nStreaks-1 DO BEGIN

     ind = sortMe[k]
     eSpecInd = m_i[strt_ii[ind]]
     eSpecIndE = m_i[stop_ii[ind]]

     PRINT,FORMAT='(A0,T25,A0,T50,F0.3,T60,I0,T70,F0.3,T80,F0.3,T90,F0.3)', $
           tStrStreaks[0,ind], $
           tStrStreaks[1,ind], $
           streakLens_t[ind], $
           eSpec.orbit[eSpecInd], $
           eSpec.mlt[eSpecInd], $
           eSpec.ilat[eSpecInd], $
           MEDIAN(eSpec.je[eSpecInd:eSpecIndE])*1.6D-9

  ENDFOR

  ;; GET_FA_ORBIT,eSpec.x[m_i[strt_ii]],/TIME_ARRAY,/NO_STORE,STRUC=struc

  STOP

END
