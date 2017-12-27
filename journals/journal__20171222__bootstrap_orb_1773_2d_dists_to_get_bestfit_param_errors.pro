;2017/12/21
;Time to Monte Carlo it up so that we can respond to the referee reports on our kappa paper
PRO JOURNAL__20171222__BOOTSTRAP_ORB_1773_2D_DISTS_TO_GET_BESTFIT_PARAM_ERRORS

  COMPILE_OPT IDL2,STRICTARRSUBS

  @common__kappa_fit2d_structs.pro

  dir = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/'
  fil = '20171124-orb_1773-Kappa_fits_and_Gauss_fits-ees-horseshoe2d-classics-3-Elphic_et_al_1998-only_fit_peak_eRange-avg_itvl2.sav'

  observed_dist  = 0

  saveSuff = 'orb1773_2DMCarlo_ests__'
  saveDir = '/SPENCEdata/Research/Satellites/FAST/kappa_dists/saves_output_etc/'

  make_bFunc_gFunc_plots = 0
  save_bFunc_gFunc_plots = 0

  print_2DFitInfo = 1
  ;; print_2DWinInfo = 1
  ;; carloTime = '09:27:01.57'     ;Time shown in Figure 2a title
  ;; carloTime = '09:27:01.261'    ;The correct time, since the average of this time and the next time (09:27:01.893) gives 09:27:01.57
  carloTimeStart = '09:26:56'
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
  
  ;; match_i      = WHERE(STRMATCH(tid, '*' + carloTime      + '*', /FOLD_CASE))
  match_iStart = WHERE(STRMATCH(tid, '*' + carloTimeStart + '*', /FOLD_CASE))
  match_iStop  = WHERE(STRMATCH(tid, '*' + carloTimeStop  + '*', /FOLD_CASE))

  gaussString = KEYWORD_SET(add_gaussian_estimate) ? '_wGauss' : ''
  obsString   = observed_dist ? '_obs' : '_synthetic'

  inds = [match_iStart[0]:match_iStop[0]]
  nHjar = N_ELEMENTS(inds)
  FOR k=0,nHjar-1 DO BEGIN

     match_i = (inds[k])[0]

     ;; Now get the data
     IF observed_dist THEN BEGIN
        data = synthPackage[0,match_i]
     ENDIF ELSE BEGIN
        data = synthPackage[1,match_i]

        shiftTheta = 0
        data.data = KAPPA_FLUX2D__HORSESHOE__ENERGY_ANISOTROPY__COMMON( $
                        data.energy, $
                        SHIFT(data.theta,0,shiftTheta), $
                        fit2DKappa_inf_list[match_i].fitParams, $
                        UNITS=units, $
                        MASS=data.mass)

        ;; WAIT! Use best-fit param data, but experimental error!
        ;; tmpStr          = CONV_UNITS(data,'counts')
        ;; tmpStr.ddata    = (tmp3d.data)^.5
        ;; data            = CONV_UNITS(TEMPORARY(tmpStr),units)

     ENDELSE

     energy_inds = WHERE(data.energy[*,0] LE 34120.)

     nEnergies = N_ELEMENTS(energy_inds)
     nAngles = data.nBins

     data = { $
            data_name        : data.data_name, $
            valid            : data.valid, $
            project_name     : data.project_name, $
            units_name       : data.units_name, $
            units_procedure  : data.units_procedure, $
            time             : data.time, $
            end_time         : data.end_time, $
            integ_t          : data.integ_t, $
            nbins            : data.nbins, $
            nenergy          : nEnergies, $
            data             : data.data[energy_inds, *], $
            ddata            : data.ddata[energy_inds, *], $
            energy           : data.energy[energy_inds, *], $
            theta            : data.theta[energy_inds, *], $
            geom             : data.geom[energy_inds, *], $
            denergy          : data.denergy[energy_inds, *], $
            dtheta           : data.dtheta, $
            eff              : data.eff[energy_inds], $
            mass             : data.mass, $
            geomfactor       : data.geomfactor, $
            header_bytes     : data.header_bytes, $
            st_index         : data.st_index, $
            en_index         : data.en_index, $
            npts             : data.npts, $
            index            : data.index}

     ;; Whatever the case, use observed uncertainties
     data.ddata = synthPackage[0,match_i].ddata

     ;; data = {x      : kappaFits[match_i[0]].x, $
     ;;         y      : kappaFits[match_i[0]].y, $
     ;;         yerror : kappaFits[match_i[0]].orig.yerror[kappaFits[match_i[0]].orig.energy_inds[0]:kappaFits[match_i[0]].orig.energy_inds[1]]}

     ;; Params
     Pkappa         = fit2DKappa_inf_list[match_i].fitParams
     Pgauss         = fit2DGauss_inf_list[match_i].fitParams
     Pobs           = kappaFits[match_i].A ;initial (from 1D fit) bulk E, T, kappa [meaningless], density

     mass           = fit2DKappa_inf_list[match_i].sdt.mass

     ;; PRINT,data.x[data.energy_inds[0]:data.energy_inds[1]] ;Range of energies used

     PRINT,FORMAT='(I05,T10,A0)',k,tid[match_i]

     tidFNStr = STRJOIN(STRSPLIT(tid[match_i],':',/EXTRACT),'_')
     tidFNStr = STRJOIN(STRSPLIT(tidFNStr,'.',/EXTRACT),'__')

     utFil = saveSuff + tidFNStr +obsString+gaussString
     utFil = utFil+'.sav'

     KAPPA_FIT2D__MONTECARLO_UNCERTAINTY,data,Pkappa,Pgauss,Pobs, $
                                         TIDFNSTR=tidFNStr, $
                                         NROLLS=nRolls, $
                                         NOT_MPFIT1D=not_mpFit1D, $
                                         KCURVEFIT_OPT=KF2D__CURVEFIT_OPT, $
                                         OBSERVED_DIST=observed_dist, $
                                         BOOTSTRAP_MODE=bootstrap, $
                                         ADD_GAUSSIAN_ESTIMATE=add_gaussian_estimate, $
                                         MASS=mass, $
                                         FIT2DKAPPA_INFO=fit2DKappa_inf_list[match_i], $
                                         MAKE_BFUNC_GFUNC_PLOTS=make_bFunc_gFunc_plots, $
                                         SAVE_BFUNC_GFUNC_PLOTS=save_bFunc_gFunc_plots, $
                                         SAVEFILE=utFil, $
                                         SAVEDIR=saveDir, $
                                         PRINT_2DFITINFO=print_2DFitInfo, $
                                         PRINT_2DWININFO=print_2DWinInfo

  ENDFOR


END

