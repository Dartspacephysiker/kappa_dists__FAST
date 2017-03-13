;2017/03/13
PRO JOURNAL__20170313__FIND_DAYSIDE_MONO_AURORA

  COMPILE_OPT IDL2,STRICTARRSUBS

  LOAD_NEWELL_ESPEC_DB,eSpec,/NO_MEMORY_LOAD
  
  minMLT          = 8.0
  maxMLT          = 17.0

  ;;time streak stuff
  acceptable_dt   = 2.0
  min_streakLen_t = 5
  min_nPts        = 10
  
  m_i = WHERE(eSpec.mono EQ 1 OR eSpec.mono EQ 2,nM)

  day_i = WHERE(eSpec.mlt GE minMLT AND eSpec.mlt LE maxMLT,nDay)

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

  

  STOP

END
