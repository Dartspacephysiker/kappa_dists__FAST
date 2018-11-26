;2018/11/25
;; Viviane is correct, solid angle is what is wanted
PRO JOURNAL__20181126__KAPPA2__WHAHAPPUN_IF_WE_INTEGRATE_AN_ARTIFICIAL_DIST

  COMPILE_OPT IDL2,STRICTARRSUBS

  file = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/diff_eFlux/orb_1607-diff_eflux-ees-sRate0_63-01_04_20__500-01_05_54__000.sav'

  RESTORE,file

  FOR k=0,N_ELEMENTS(diff_eFlux)-1 DO PRINT,k,"   ",T2S(diff_eflux[k].time,/MS)

  ;; Our guy is 22

  dat = diff_eFlux[22]

  ;; See?
  ;; WINDOW,0,XSIZE=800,YSIZE=800
  ;; CONTOUR2D,dat,/POLAR

  ;; Pitch angle spacing?
  PRINT,SHIFT(REFORM(dat.theta[0,*]),1)-REFORM(dat.theta[0,*])
  ;; It's 5.625 degrees 

  ;; Pick a slice that is going to be ma'ter
  lowAngle = MIN(ABS(dat.theta[dat.nEnergy/2,*]-45),lowAngleI)

  makeEm = dat.data[*,lowAngleI]

  FOR jj=0,dat.nBins-1 DO dat.data[*,jj] = makeEm


  eRange = [1D2,2D4]
  startA = 5.
  dPA = 6
  nPA = LONG((180.-startA)/dPA)
  pitchAngles=startA+FINDGEN(nPA)*dPA

  densities = MAKE_ARRAY(nPA,VALUE=0.)
  FOR k=0,nPA-1 DO BEGIN
     tmpAngles = [-pitchAngles[k],pitchAngles[k]]
     densities[k] = N_2D_FS(dat,ENERGY=eRange,ANGLE=tmpAngles)
  ENDFOR

  densities = densities / MAX(densities) * 2.

  plot = PLOT(pitchAngles,densities, $
              XTITLE="Pitch angle (deg)", $
              YTITLE="Density (cm!U-3!N)", $
              TITLE="Orbit 1773: " +T2S(dat.time))

  plot = PLOT(pitchAngles,1-COS(pitchAngles*!DTOR), $
              COLOR='RED', $
              LINESTYLE='--', $
              NAME="ARTIFICE", $
              /OVERPLOT)
              ;; XTITLE="Pitch angle (deg)", $
              ;; YTITLE="Density (cm!U-3!N)", $
              ;; TITLE="Orbit 1773: " +T2S(dat.time))


  STOP

  xSize       = 9.5
  ySize       = 9.5
  land        = 1

  xWinSize    = 700
  yWinSize    = 700

  outdir = '/SPENCEdata/software/sdt/batch_jobs/kappas_Maxwellians_for_inverted_Vs/'
  outPS = 'Orb1773-contour'
  POPEN,outDir+outPS, $
        XSIZE=xSize, $
        YSIZE=ySize, $
        LAND=land, $
        CTABLE=43, $
        ENCAPSULATED=eps, $
        OPTIONS={charsize:1.5}

  CONTOUR2D,dat, $
            ;; ANGLE=angle, $
            /POLAR, $
            /FILL, $
            ;; /OVERPLOT, $
            /MSEC, $
            LIMITS=limits, $
            /LABEL, $
            THICK=thick

  PCLOSE

  STOP

END
