;2017/12/21
;Time to Monte Carlo it up so that we can respond to the referee reports on our kappa paper
PRO JOURNAL__20171221__BOOTSTRAP_ORB_1773_DISTS_TO_GET_BESTFIT_PARAM_ERRORS

  COMPILE_OPT IDL2,STRICTARRSUBS

  dir = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/'
  fil = '20171124-orb_1773-Kappa_fits_and_Gauss_fits-ees-horseshoe2d-classics-3-Elphic_et_al_1998-only_fit_peak_eRange-avg_itvl2.sav'

  ;; use_mpFit1D = 1               ;Alltid. It's all I use nowadays
  observed_dist  = 0

  saveSuff = 'orb1773_1DMCarlo_ests__'
  saveDir = '/SPENCEdata/Research/Satellites/FAST/kappa_dists/saves_output_etc/'

  ;; carloTime = '09:27:01.57'     ;Time shown in Figure 2a title
  carloTime = '09:27:01.261'    ;The correct time, since the average of this time and the next time (09:27:01.893) gives 09:27:01.57
  carloTimeStart = '09:26:53'
  carloTimeStop  = '09:27:03'

  RESTORE,dir+fil

  PARSE_KAPPA_FIT_STRUCTS,kappaFits, $
                          A=a, $
                          STRUCT_A=Astruct, $
                          TIME=time, $
                          NAMES_A=A_names, $
                          CHI2=chi2, $
                          PVAL=pVal, $
                          FITSTATUS=fitStatus, $
                          /USE_MPFIT1D

  nFits = N_ELEMENTS(kappaFits)
  tid = MAKE_ARRAY(nFits,/STRING)
  FOR k=0,nFits-1 DO BEGIN
     tid[k] = kappafits[k].orig.name
  ENDFOR
  
  match_i      = WHERE(STRMATCH(tid, '*' + carloTime      + '*', /FOLD_CASE))
  match_iStart = WHERE(STRMATCH(tid, '*' + carloTimeStart + '*', /FOLD_CASE))
  match_iStop  = WHERE(STRMATCH(tid, '*' + carloTimeStop  + '*', /FOLD_CASE))

  gaussString = KEYWORD_SET(add_gaussian_estimate) ? '_wGauss' : ''
  obsString   = observed_dist ? '_obs' : '_synthetic'

  inds = [match_iStart[0]:match_iStop[0]]
  nHjar = N_ELEMENTS(inds)
  FOR k=0,nHjar-1 DO BEGIN

     match_i = inds[k]

     ;; Now get the data
     IF observed_dist THEN BEGIN
        data           = kappaFits[match_i[0]].orig
     ENDIF ELSE BEGIN
        ;; WAIT! Use best-fit param data, but experimental error!
        data = {x      : kappaFits[match_i[0]].x, $
                y      : kappaFits[match_i[0]].y, $
                yerror : kappaFits[match_i[0]].orig.yerror[kappaFits[match_i[0]].orig.energy_inds[0]:kappaFits[match_i[0]].orig.energy_inds[1]]}
     ENDELSE

     ;; Params
     Pkappa         = kappaFits[match_i[0]].a     ;Best-fit bulk E, T, kappa [meaningless], density
     Pgauss         = gaussFits[match_i[0]].a     ;Best-fit bulk E, T, kappa [meaningless], density
     Pobs           = kappaFits[match_i[0]].a_sdt ;initial (or estimated?) bulk E, T, kappa [meaningless], density

     mass           = fit2DGauss_inf_list[match_i[0]].sdt.mass

     ;; PRINT,data.x[data.energy_inds[0]:data.energy_inds[1]] ;Range of energies used

     PRINT,FORMAT='(I05,T10,A0)',k,tid[match_i]

     utFil = saveSuff + STRJOIN(STRSPLIT(tid[match_i],':',/EXTRACT),'_')+obsString+gaussString
     utFil = STRJOIN(STRSPLIT(utFil,'.',/EXTRACT),'__')+'.sav'

     KAPPA_FIT1D__MONTECARLO_UNCERTAINTY,data,Pkappa,Pgauss,Pobs, $
                                         NROLLS=nRolls, $
                                         NOT_MPFIT1D=not_mpFit1D, $
                                         KCURVEFIT_OPT=KF2D__CURVEFIT_OPT, $
                                         OBSERVED_DIST=observed_dist, $
                                         BOOTSTRAP_MODE=bootstrap, $
                                         ADD_GAUSSIAN_ESTIMATE=add_gaussian_estimate, $
                                         MASS=mass, $
                                         SAVEFILE=utFil, $
                                         SAVEDIR=saveDir

  ENDFOR


END
