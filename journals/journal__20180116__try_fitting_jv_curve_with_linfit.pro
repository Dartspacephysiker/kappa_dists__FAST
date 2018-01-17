;;2018/01/16


;; SAVE,jvplotdata,avgs_jvfit, $
;;FILENAME='/SPENCEdata/Research/Satellites/FAST/kappa_dists/saves_output_etc/20180116_jvplotdata_and_avgs_jv_fit__orb_1773.sav'

filDir  = '/SPENCEdata/Research/Satellites/FAST/kappa_dists/saves_output_etc/'
filNavn = '20180116_jvplotdata_and_avgs_jv_fit__orb_1773.sav'

RESTORE,filDir+filNavn

;; Stopped at line 893 in CURRENT_AND_POTENTIAL_SUITE
pot = jvplotdata.pot[avgs_jvfit.useinds]
cur = jvplotdata.cur[avgs_jvfit.useinds]
curerr = jvplotdata.curerr[avgs_jvfit.useinds]
FOR k=0,12 DO PRINT,FORMAT='(F0.2,TR10,F0.2,TR10,F0.2)',pot[k],cur[k],curerr[k]


;; Fit 'em all! ('Tallica)

;; Use current density
coeffs = LINFIT(pot,cur*(-1.),MEASURE_ERRORS=curerr,SIGMA=sig,PROB=prob)

conductivity = coeffs[1] / 1D6 ;division by 1D6 because current density is in microA/m^2
condErr = sig[1] / 1D6
normCondErr = condErr / conductivity

;; Use number flux
;; cur = cur*(-1.)/1.6D-9
;; curerr = curerr/1.6D-9
;; coeffs = LINFIT(pot,cur*(-1.),MEASURE_ERRORS=curerr,SIGMA=sig,PROB=prob)


charge = 1.6021766D-19          ;C
mass   = 9.10938356D-31         ; kg
prefactor = charge^2.D / SQRT(2.D * !DPI * mass)
prefactor = charge^2.D / SQRT(2.D * !DPI * mass * charge) * 1D6

;; Here's an assay
T_m = avgs_JVFit.T.Avg
n_m = avgs_jvfit.n_sc.avg
;; T_m = 141                         ;eV
predicted_n_m = conductivity/prefactor*SQRT(T_m)

;; Also, thermal mirror ratio
E_p = MEAN(jvplotdata.only_downe_pot[avgs_jvfit.useinds])

R_B_Thermal = 4 * (E_p / T_m )^2.D +1

;; PROB
;; Set this keyword to a named variable that will contain the probability that
;; the computed fit would have a value of CHISQ or greater. If PROB is greater
;; than 0.1, the model parameters are “believable”. If PROB is less than 0.1,
;; the accuracy of the model parameters is questionable.

;; SIGMA
;; Set this keyword to a named variable that will contain the 1-sigma
;; uncertainty estimates for the returned parameters

orbit = 1773
orbPref  = 'Orbit ' + STRCOMPRESS(orbit,/REMOVE_ALL)

;; titleStr         = STRING(FORMAT='(A0," ($\langle$T!DFAST!N$\rangle$=",F0.1," eV, ' + $
;;                           'n!DFAST!N=",G0.3," cm!U-3!N)")', $
;;                           orbPref,T_m,n_m)
;; titleStr         = STRING(FORMAT='(A0," $\langle$T!DFAST!N$\rangle$=",F0.1," eV, ' + $
;;                           '$\langle$n!DFAST!N$\rangle$=",G0.3," cm!U-3!N")', $
;;                           orbPref,T_m,n_m)
titleStr         = STRING(FORMAT='("$\langle$T!DFAST!N$\rangle$=",F0.1," eV;  ' + $
                          '$\langle$n!DFAST!N$\rangle$=",G0.3," cm!U-3!N")', $
                          T_m,n_m)
msec             = 1
t1Str            = (STRSPLIT(TIME_TO_STR(MIN(jvPlotData.time[avgs_JVFit.useInds]),MSEC=msec),'/',/EXTRACT))[1]
t2Str            = (STRSPLIT(TIME_TO_STR(MAX(jvPlotData.time[avgs_JVFit.useInds]),MSEC=msec),'/',/EXTRACT))[1]
dataName         = STRING(FORMAT='(A0,"–",A0)',t1Str,t2Str)
;; kappaName        = STRING(FORMAT='("j!D$\parallel$!N = ",G0.2," + ",G0.3," $\Delta \Phi$ $\mu$A m!U-2!N")',coeffs[0],coeffs[1])
kappaName        = STRING(FORMAT='("j!D$\parallel$!N = ",G0.2," + ",G0.3," $\Delta \Phi$")',coeffs[0],coeffs[1])

window1          = WINDOW(DIMENSION=[1000,800],BUFFER=savePlot)

symbol      = '+'
sym_thick   = 2.0
thick       = 2.2
font_size   = 18
legFont_size = 16

;; xRange      = MINMAX(pot)
;; yRange      = MINMAX(cur*(-1))
;; yRange      = [1.5,3]

xFrac            = 0.1
yFrac            = 0.1
xRange           = MINMAX(pot)
xRange           = [xRange[0]*((1.D)-xFrac),xRange[1]*((1.D)+xFrac)]

potFit         = [1000:2800:10]          

yFit           = coeffs[0]+coeffs[1]*potFit

yRFitInds        = WHERE((potFit GE xRange[0]) AND (potFit LE xRange[1]))
yRange           = [MIN([cur*(-1),yFit[yRFitInds],yFit[yRFitInds]]),MAX(cur*(-1)+curErr)]
yRange           = [yRange[0]*((1.D)-yFrac),yRange[1]*((1.D)+yFrac)]

that             = ERRORPLOT(pot,cur*(-1),curerr, $
                             SYMBOL=symbol, $
                             LINESTYLE='', $
                             NAME=dataName, $
                             TITLE=titleStr, $
                             XTITLE='$\Phi$ (V)', $
                             YTITLE='j!D$\parallel$,i!N ($\mu$A/m!U2!N)', $
                             ;; YTITLE=yTitle, $
                             XRANGE=xRange, $
                             YRANGE=yRange, $
                             XSTYLE=2, $
                             SYM_THICK=sym_thick, $
                             ;; YRANGE=MINMAX(pData.Y+pData.YError), $
                             FONT_SIZE=font_size, $
                             /CURRENT)

this             = PLOT(potFit,yFit, $
                        NAME=kappaName, $
                        XRANGE=xRange, $
                        YRANGE=yRange, $
                        LINESTYLE='--', $
                        THICK=thick, $
                        FONT_SIZE=font_size, $
                        COLOR='gray', $
                        /OVERPLOT)

  legPos           = [0.47,0.83]
  ;; leg           = LEGEND(TARGET=[that,this,those],POSITION=legPos__data,/DATA)
  leg              = LEGEND(TARGET=[that,this], $
                            POSITION=legPos, $
                            FONT_SIZE=legFont_size)


;; this1 = errorplot(pot,cur*(-1.D),curerr,LINESTYLE='')
;; this2 = plot(pot,coeffs[0]+coeffs[1]*pot,/OVERPLOT)

