;2017/04/17
PRO PRINT_ITERATIVE_GAME_MODE_FITHISTORY,gameFitInfo

  COMPILE_OPT IDL2,STRICTARRSUBS

  PRINT,FORMAT='("dRB_arr  : ",20(F0.2,:,", "))',gameFitInfo.history.dRB
  PRINT,FORMAT='("RB_arr   : ",20(F0.2,:,", "))',gameFitInfo.history.RB
  PRINT,FORMAT='("RLim_arr : ",20(F0.2,:,", "))',gameFitInfo.history.RLim
  PRINT,FORMAT='("RE_arr   : ",20(F0.2,:,", "))',gameFitInfo.history.RE
  PRINT,FORMAT='("dens_arr : ",20(G0.4,:,", "))',gameFitInfo.history.dens


END
