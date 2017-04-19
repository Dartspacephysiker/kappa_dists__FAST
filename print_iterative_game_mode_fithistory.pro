;2017/04/17
PRO PRINT_ITERATIVE_GAME_MODE_FITHISTORY,gameFitInfo

  COMPILE_OPT IDL2,STRICTARRSUBS

  PRINT,FORMAT='("dR_B_arr  : ",20(F0.2,:,", "))',gameFitInfo.history.dR_B
  PRINT,FORMAT='("R_B_arr   : ",20(F0.2,:,", "))',gameFitInfo.history.R_B
  PRINT,FORMAT='("RLim_arr  : ",20(F0.2,:,", "))',gameFitInfo.history.RLim
  PRINT,FORMAT='("R_E_arr   : ",20(F0.2,:,", "))',gameFitInfo.history.R_E
  PRINT,FORMAT='("dens_arr  : ",20(G0.4,:,", "))',gameFitInfo.history.dens


END
