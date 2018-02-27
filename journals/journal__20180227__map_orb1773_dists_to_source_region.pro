;2018/02/27
PRO KILLER,curDataStr,killed,careInds, $
           MINE=minE, $
           MAXE=maxE

  COMPILE_OPT IDL2,STRICTARRSUBS



  killed = WHERE(((curDataStr.theta GE 90.D) OR $
                  (curDataStr.theta LE -90.D)) $
                 OR (curDataStr.energy LT (KEYWORD_SET(minE) ? minE : 0.)) $
                 OR (curDataStr.energy GT (KEYWORD_SET(maxE) ? maxE : 3.1E4)), $
                 /NULL,  $
                 COMPLEMENT=careInds)

END

PRO JOURNAL__20180227__MAP_ORB1773_DISTS_TO_SOURCE_REGION, $
   KILL=kill, $
   FIX_ANGLE=fickAngle, $
   MAP=map, $
   ONLY_MAPPED=only_mapped, $
   RBFAST=RBFAST, $
   POTABOVEFAST=potAboveFAST, $
   SMOOTH_EN=smooth_en, $
   SMWIN_EN=smWin_en, $
   SMOOTH_AN=smooth_an, $
   SMWIN_AN=smWin_an, $
   MIN_CURVE_SURF=min_curve_surf

  COMPILE_OPT IDL2,STRICTARRSUBS

  dir = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/'
  
  fil = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/diff_eFlux/orb_1773-diff_eflux-ees-avg_itvl2-09_26_10__000-09_27_15__000.sav'
  iTime = 80 ;1997-02-01/09:27:00.629

  IF N_ELEMENTS(kill      ) EQ 0 THEN kill       = 0 ;Kill stuff at |pitch angles| GT 90
  IF N_ELEMENTS(fickAngle ) EQ 0 THEN fickAngle  = 0 ;Average the angles so they're not all weird and rude
  IF N_ELEMENTS(map       ) EQ 0 THEN map        = 0
  IF N_ELEMENTS(RBFAST    ) EQ 0 THEN RBFAST     = 1
  IF N_ELEMENTS(smooth_en ) EQ 0 THEN smooth_en  = 1
  IF N_ELEMENTS(smWin_en  ) EQ 0 THEN smWin_en   = 5
  IF N_ELEMENTS(smooth_an ) EQ 0 THEN smooth_an  = 1
  IF N_ELEMENTS(smWin_an  ) EQ 0 THEN smWin_an   = 5
  IF N_ELEMENTS(min_curve_surf  ) EQ 0 THEN min_curve_surf = 0

  IF N_ELEMENTS(fil) EQ 0 THEN fil = DIALOG_PICKFILE(/READ,PATH=dir)
  
  RESTORE,fil

  IF N_ELEMENTS(iTime) EQ 0 THEN BEGIN
     PRINT,FORMAT='(A5,TR5,A25)',"Index","Time"
     nHere = N_ELEMENTS(diff_eFlux.time)
     FOR k=0,nHere-1 DO BEGIN
        PRINT,FORMAT='(I03,TR5,A25)',k,T2S(diff_eFlux.time[k],/MS)
     ENDFOR

     cont = 0
     WHILE ~cont DO BEGIN

        READ,FORMAT='(I0)',PROMPT=STRING(FORMAT='("Pick a num twixt ",I0," and ",I0," : ")',0,nHere-1),iTime

        IF iTime LE nHere THEN cont = 1 ELSE PRINT,"NO!"

     ENDWHILE
  ENDIF

  tString    = T2S(diff_eFlux.time[iTime],/MS)
  curDataStr = MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(diff_eFlux,iTime)

  ;; Plot window stuff
  
  ;; !D.X_SIZE = newSize[0]
  ;; !D.Y_SIZE = newSize[1]

  ;; !D.X_VSIZE = newVSize[0]
  ;; !D.Y_VSIZE = newVSize[1]

  origsize = [!D.X_SIZE,!D.Y_SIZE]
  origvsize = [!D.X_VSIZE,!D.Y_VSIZE]
  
  newSize = [1000,1000]
  ;; newVsize = newSize

  charSize = 10.
  DEVICE,SET_CHARACTER_SIZE=REPLICATE(charSize,2)

  WINDOW,kill+fickAngle*2+map*4+smooth_en*8+smooth_an*16+min_curve_surf,XSIZE=newSize[0],YSIZE=newSize[1]

  nAngle = N_ELEMENTS(curDataStr.theta[*,0])
  nEnergy = N_ELEMENTS(curDataStr.energy[*,0])

  IF KEYWORD_SET(fickAngle) THEN BEGIN

     midAngles = MEDIAN(curDataStr.theta,DIMENSION=1)
     FOR k=0,nAngle-1 DO BEGIN
        curDataStr.theta[*,k] = midAngles[k]
     ENDFOR

  ENDIF

  ;; Kill everyone with |pitch angle| gt 90°
  KILLER,curDataStr,killed,careInds, $
         MINE=potAboveFAST, $
         MAXE=maxE
  IF kill THEN BEGIN
     curDataStr.data[killed] = 0.
  ENDIF

  ;; Now map
  IF map THEN BEGIN
     mapDataStr                 = curDataStr

     haveEdgery                 = KEYWORD_SET(potAboveFAST)
     haveRB                     = KEYWORD_SET(RBFAST)

     CASE 1 OF
        haveEdgery AND haveRB: BEGIN

           mapDataStr.energy[careInds] = mapDataStr.energy[careInds] - potAboveFAST

           ;; canDo = WHERE(mapDataStr.energy GT potAboveFAST,/NULL,COMPLEMENT=cantDo)

           mapDataStr.theta[careInds] = ASIN(SIN(curDataStr.theta[careInds]*!PI/180.) $
                                             * SQRT( curDataStr.energy[careInds] / (curDataStr.energy[careInds]-potAboveFAST) / FLOAT(RBFAST)))*180./!PI

        END
        haveRB: BEGIN

           mapDataStr.theta[careInds] = ASIN(SIN(curDataStr.theta[careInds]*!PI/180.)/SQRT(FLOAT(RBFAST)))*180./!PI

        END
     ENDCASE

     minMaxMapAngle             = MAX(mapDataStr.theta[careInds])
     
  ENDIF

  ;; Smooth_En?
  IF smooth_en THEN BEGIN

     nSmooths = 5
     FOR k=0,nEnergy-1 DO BEGIN
        smoothCount = nSmooths
        WHILE smoothCount GT 0 DO BEGIN
           curDataStr.data[k,*] = SMOOTH(curDataStr.data[k,*],((smWin_en-2*(nSmooths-smoothCount))>3),/EDGE_TRUNCATE,/NAN)
           smoothCount--
        ENDWHILE

     ENDFOR
     
  ENDIF

  ;; Smooth_an?
  IF smooth_an THEN BEGIN

     nSmooths = 2
     FOR k=0,nAngle-1 DO BEGIN
        smoothCount = nSmooths
        WHILE smoothCount GT 0 DO BEGIN
           curDataStr.data[*,k] = SMOOTH(curDataStr.data[*,k],((smWin_an-2*(nSmooths-smoothCount))>3),/EDGE_TRUNCATE,/NAN)
           smoothCount--
        ENDWHILE

     ENDFOR
     
  ENDIF

  IF KEYWORD_SET(min_curve_surf) THEN BEGIN
     ;; Epar  = mapDataStr.energy * (COS(mapDataStr.theta*!DPI/180.D))^2.D
     ;; Eperp = mapDataStr.energy * (SIN(mapDataStr.theta*!DPI/180.D))^2.D
     ;; ;; mapDataStr.data[careInds] = MIN_CURVE_SURF(mapDataStr.data[careInds],Epar[careInds],Eperp[careInds])

     ;; Epar  = (mapDataStr.energy * (COS(mapDataStr.theta*!PI/180.))^2.D)[careInds]
     ;; Eperp = (mapDataStr.energy * (SIN(mapDataStr.theta*!PI/180.))^2.D)[careInds]

     ;; outEpar   = REVERSE(mapDataStr.energy[1:-1,0])
     ;; outEperp  = REVERSE(mapDataStr.energy[1:-1,0])

     ;; outEpar   = 10.^REBIN(ALOG10(outEpar ),N_ELEMENTS(outepar)*2)
     ;; outEperp  = 10.^REBIN(ALOG10(outEperp),N_ELEMENTS(outEperp)*2)

     ;; Speed in units of c (speed of light)
     speed     = SQRT(2. * mapDataStr.energy / 5.1099894E5)
     vPar      = speed * COS(mapDataStr.theta*!PI/180.)
     vPerp     = speed * SIN(mapDataStr.theta*!PI/180.)

     posVPar_i = WHERE(vPar GT 0,/NULL)

     ;;OLD WAY
     ;;;;;;;;;;;;;;;;;;;
     ;; outVPar   = [-1.*speed[1:-1,0],REVERSE(speed[1:-1,0])]
     ;; nNewAngle = N_ELEMENTS(outVPar)
     ;; nNewSpeed = N_ELEMENTS(outVPar)
              
     ;; outVPar   = outVPar # REPLICATE(1.,nNew)
     ;; outVPerp  = TRANSPOSE(outVPar)
              
     ;;NEW WAY
     ;;;;;;;;;;;;;;;;;;;
     nNewAngleHalfm1 = 10
     tmper     = FINDGEN(nNewAngleHalfm1-1)+1.
     outAngles = [-1. * REVERSE(tmper),0.,tmper] $
                 / (nNewAngleHalfm1-1.) $
                 * minMaxMapAngle 
     outSpeed  = REVERSE(speed[*,0])

     nNewAngle = N_ELEMENTS(outAngles)
     nNewSpeed = N_ELEMENTS(outSpeed)

     outVPar   = outSpeed # COS(outAngles*!PI/180.)
     outVPerp  = outSpeed # SIN(outAngles*!PI/180.)

     newE      = (outVPar^2. + outVPerp^2.)/2.*5.109989E5
     newThet   = ATAN(outVPerp,outVPar)*180./!PI

     ;; outVPerp = [-1.*speed[1:-1,0],REVERSE(speed[1:-1,0])]
     
     zoData = MIN_CURVE_SURF(mapDataStr.data[posVPar_i],vPar[posVPar_i],vPerp[posVPar_i],XPOUT=outVPar,YPOUT=outVPerp)

     ;;The min and max angles
     zoData[*,-1] = 0.
     zoData[*,0]  = 0.

     tmpTheta = REFORM(mapDataStr.theta[0,*])
     sortMe = SORT(tmpTheta)
     sortTheta = tmpTheta[sortMe]
     sortGeom = (mapDataStr.geom[0,*])[sortMe]
     geomInds = VALUE_CLOSEST2(sortTheta,newThet,/CONSTRAINED)
     newGeom = sortGeom[TEMPORARY(geomInds)]

     mapDataStr.NBins   = nNewAngle
     mapDataStr.NEnergy = nNewSpeed
     STR_ELEMENT,mapDataStr,'data',zoData,/ADD_REPLACE
     STR_ELEMENT,mapDataStr,'ddata',zoData*1.,/ADD_REPLACE
     STR_ELEMENT,mapDataStr,'energy',TEMPORARY(newE),/ADD_REPLACE
     STR_ELEMENT,mapDataStr,'theta',TEMPORARY(newThet),/ADD_REPLACE
     STR_ELEMENT,mapDataStr,'geom',TEMPORARY(newGeom),/ADD_REPLACE
     STR_ELEMENT,mapDataStr,'eff',REPLICATE(1.,nNewSpeed),/ADD_REPLACE
     ;; STR_ELEMENT,mapDataStr,'geom',/DELETE

     ;; curDataStr = TEMPORARY(mapDataStr)

     ;; A re-killing is necessary sometimes
     IF kill THEN BEGIN

        KILLER,mapDataStr,killed,careInds, $
               MINE=potAboveFAST, $
               MAXE=maxE
        mapDataStr.data[killed] = 0.
        
     ENDIF

     killed = WHERE((mapDataStr.theta GT minMaxMapAngle) OR (mapDataStr.theta LT (-1.)*minMaxMapAngle),/NULL)
     mapDataStr.data[killed] = 0.

  ENDIF

  IF KEYWORD_SET(only_mapped) THEN BEGIN

     curDataStr = TEMPORARY(mapDataStr)

  ENDIF ELSE IF map THEN BEGIN
     mapDataStr = {SDT : TEMPORARY(mapDataStr)}

     @common__kappa_fit2d_structs.pro
     KF2D__plot_opt = {fit2D__add_boundaries : 0}

  ENDIF

  PLOT_CONTOUR2D_MODEL_AND_DATA__SELECTED2DFIT,mapDataStr,curDataStr, $
     ONLY_DATA=(~map) OR KEYWORD_SET(only_mapped), $ 
     FOR_HORSESHOE_FIT=for_horseshoe_fit, $
     LIMITS=cont2DLims, $
     ;; ADD_FITPARAMS_TEXT=KF2D__Plot_opt.add_fitParams_text, $
     ;; KF2D__SDTDATA_OPT=KF2D__SDTData_opt, $
     FITSTRING=fitString, $
     /SKIP_ORNAMENTATION

  STOP
END
