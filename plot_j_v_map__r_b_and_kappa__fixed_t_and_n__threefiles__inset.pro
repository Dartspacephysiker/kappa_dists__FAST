;2017/08/15
PRO PLOT_J_V_MAP__R_B_AND_KAPPA__FIXED_T_AND_N__THREEFILES__INSET

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;;For adding an R_E axis, if that's wassup
  @common__jv_curve_fit__tie_r_b_and_dens.pro

  date             = GET_TODAY_STRING(/DO_YYYYMMDD_FMT)
  dir              = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/cur_and_pot_analysis/'
  fErs             = ['reg','plus','minus']
  files            = 'fixTandN__' + fErs + 'Dat.sav'
  showCase_i       = WHERE(STRUPCASE(fErs) EQ 'REG')

  jvPlotDataList   = LIST()
  avgs_JVfitList   = LIST()
  pDataList        = LIST()
  AList            = LIST()
  AGaussList       = LIST()
  orbitList        = LIST()
  routNameList     = LIST()
  mMagDatList      = LIST()
  tRB_RBpairsList  = LIST()
  tRB_fLineList    = LIST()
  tRB_nFASTList    = LIST()
  tRB_nFLineList   = LIST()
  tRB_fLineREList  = LIST()

  nFiles          = N_ELEMENTS(fErs)
  FOREACH file,files,file_i DO BEGIN
     IF FILE_TEST(dir+file) THEN BEGIN
        PRINT,FORMAT='(A0,I0,"/",I0,": ",A0)', $
              (file_i EQ 0 ? "Getting " : "Now "), $
              file_i+1, $
              nFiles, $
              file
        
;; tRB_RBpairsList
;; tRB_fLineList  
;; tRB_nFASTList  
;; tRB_nFLineList 
;; tRB_fLineREList

        RESTORE,dir+file
        
        PLOT_J_V_MAP__R_B_AND_KAPPA__FIXED_T_AND_N__OVERPLOT_VERSION,mMagDat,jvPlotData,avgs_JVFit, $
           MAP__2D=map__2D, $
           MAP2D__LOG_KAPPA=map2D__log_kappa, $
           ORBIT=orbit, $
           IN_KAPPA_A=A, $
           IN_GAUSS_A=AGauss, $
           OUT_YBEST=out_yBest, $
           SAVEPLOT=savePlot, $
           ZOOM_ON_EXTREME_KAPPA=zoom_on_extreme_kappa, $
           _EXTRA=e

        ;;Make lists for any reason at all?
        IF KEYWORD_SET(makeLists) THEN BEGIN 
           jvPlotDataList.Add,TEMPORARY(jvPlotData)
           avgs_JVfitList.Add,TEMPORARY(avgs_JVfit)
           pDataList.Add,TEMPORARY(pData)     
           AList.Add,TEMPORARY(A)         
           AGaussList.Add,TEMPORARY(AGauss)    
           orbitList.Add,TEMPORARY(orbit)     
           routNameList.Add,TEMPORARY(routName)  
           mMagDatList.Add,TEMPORARY(mMagDat)   
           IF N_ELEMENTS(tRB_RBpairs) GT 0 THEN BEGIN
              tRB_RBpairsList.Add,TEMPORARY(tRB_RBpairs)
           ENDIF
           IF N_ELEMENTS(tRB_fLine) GT 0 THEN BEGIN
              tRB_fLineList.Add,TEMPORARY(tRB_fLine)
           ENDIF
           IF N_ELEMENTS(tRB_nFAST) GT 0 THEN BEGIN
              tRB_nFASTList.Add,TEMPORARY(tRB_nFAST)
           ENDIF
           IF N_ELEMENTS(tRB_nFLine) GT 0 THEN BEGIN
              tRB_nFLineList.Add,TEMPORARY(tRB_nFLine)
           ENDIF
           IF N_ELEMENTS(tRB_fLineRE) GT 0 THEN BEGIN
              tRB_fLineREList.Add,TEMPORARY(tRB_fLineRE)
           ENDIF
        ENDIF
        
     ENDIF ELSE BEGIN
        STOP
     ENDELSE
  ENDFOREACH

END
