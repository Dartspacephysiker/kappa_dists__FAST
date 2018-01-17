;2016/07/19
FUNCTION INIT_KAPPA_STRING_STRUCT,diff_eFlux, $
                                  orbStr, $
                                  angleStr, $
                                  kSDTData_opt
  
  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; times                              = (diff_eFlux.time+diff_eFlux.end_time)/2.D
  times                              = diff_eFlux.time

  kStrings                           = {today:GET_TODAY_STRING(/DO_YYYYMMDD_FMT), $
                                        eeb_or_ees:kSDTData_opt.eeb_or_ees, $
                                        avgStr:KEYWORD_SET(kSDTData_opt.spec_avg_intvl) ? $
                                        STRING(FORMAT='("--",I0,"_avgs")',kSDTData_opt.spec_avg_intvl) : '' , $
                                        orbStr:orbStr, $
                                        orbDate:STRMID(TIME_TO_STR(diff_eFlux.time),0,10), $
                                        yearStr:STRMID(TIME_TO_STR(times[0],/MSEC),0,10), $
                                        timeStrs:STRMID(TIME_TO_STR(times,/MSEC),11,11), $
                                        timeFNStrs:STRMID(TIME_TO_STR(times,/MSEC),11,11), $
                                        plotTimes:STRMID(TIME_TO_STR(diff_eFlux.time,/MSEC),11,12), $
                                        angleStr:angleStr}

  ;;Make the strings nice for filenames down the line
  kStrings.timeFNStrs                = kStrings.timeFNStrs.REPLACE(':', '_')
  kStrings.timeFNStrs                = kStrings.timeFNStrs.REPLACE('.', '__')

  RETURN,kStrings

END