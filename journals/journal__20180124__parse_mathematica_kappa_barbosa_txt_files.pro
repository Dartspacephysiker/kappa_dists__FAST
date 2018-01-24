;2018/01/24
PRO JOURNAL__20180124__PARSE_MATHEMATICA_KAPPA_BARBOSA_TXT_FILES

  COMPILE_OPT IDL2,STRICTARRSUBS

  doPlots = 1
  overwrite_plots = 1
  plotsPerWindow  = 5

  overwrite_saveFile = 0


  dir     = '/SPENCEdata/Research/Satellites/FAST/kappa_dists/saves_output_etc/'
  filPref = 'kappaBarbosaFacs-TFAST_eq_110_2-nFAST_eq_1_88--'

  starter = 'kappaBarbosaFacs-TFAST_eq_110_2-nFAST_eq_1_88--1_of_131.txt'

  outFil  = 'kappaBarbosaFacs-TFAST_eq_110_2-nFAST_eq_1_88.sav'

  tmpltFil = 'kappaBarbosaFacs-ASCII_TEMPLATE.sav'
  IF FILE_TEST(dir+tmpltFil) THEN RESTORE,dir+tmpltFil ELSE BEGIN
     STOP
     tmplt = ASCII_TEMPLATE(dir+starter)
     ;; SAVE,tmplt,FILENAME=dir+tmpltFil
  ENDELSE

  nFiles  = 131
  dat1    = READ_ASCII(dir+starter,TEMPLATE=tmplt)
  kapBarb = REPLICATE(dat1,nFiles)

  IF KEYWORD_SET(doPlots) THEN BEGIN
     SET_PLOT_DIR,plotDir,/FOR_KAPPA_DB,/ADD_TODAY

     Temperature = 110.2
     density     = 1.88
     orbPref     = "Orbit 1773"
     
     xTitle      = 'R!DB!N'
     yTitle      = 'Q$_\kappa$!N'
     title       = STRING(FORMAT='(A0," (T!DF!N=",I0," eV, n!DF!N=",G0.3," cm!U-3!N)")', $
                          orbPref,Temperature,Density)

     defFontSize      = 14
     defBigFontSize   = 18
     defMidFontSize   = 16
     lineStyle        = ['-','__',"--","-.",":","-",'__']

     color            = ['orange','red','green','blue','black','purple','pink']


  ENDIF

  IF ~FILE_TEST(dir+outFil) OR (FILE_TEST(dir+outFil) AND KEYWORD_SET(overwrite_saveFile)) THEN BEGIN

     FOR k=1,nFiles DO BEGIN

        pref = STRING(FORMAT='(I0,"_of_",I0,".txt")',k,nFiles)
        ;; PRINT,pref

        fil = filPref + pref
        ;; PRINT,fil

        IF ~FILE_TEST(dir+fil) THEN STOP

        kapBarb[k-1] = READ_ASCII(dir+fil,TEMPLATE=tmplt)


     ENDFOR

     PRINT,"Saving kapBarbs to " + outFil + ' ...'
     SAVE,kapBarb,FILENAME=dir+outFil

  ENDIF ELSE BEGIN

     RESTORE,dir+outFil

  ENDELSE

  IF KEYWORD_SET(doPlots) THEN BEGIN

     k = 1
     plotCount = 0
     WHILE k LT nFiles DO BEGIN

        IF ((k-1) MOD plotsPerWindow) EQ 0 THEN BEGIN
           window = WINDOW(DIMENSION=[800,600],/BUFFER)
           plotArr = MAKE_ARRAY(plotsPerWindow,/OBJ)
           startKappa = kapBarb[k-1].kappa[0]
        ENDIF

        ;; plotInd = (k-1) MOD plotsPerWindow

        plotArr[plotCount] = PLOT(kapBarb[k-1].RB, $
                                  kapBarb[k-1].factor, $
                                  NAME=STRING(FORMAT='("$\kappa$ = ",G0.4)',kapBarb[k-1].kappa[0]), $
                                  XRANGE=[5,10000], $
                                  ;; YRANGE=[1,1000], $
                                  /XLOG, $
                                  /YLOG, $
                                  TITLE=title, $
                                  XTITLE=xTitle, $
                                  YTITLE=yTitle, $
                                  LINESTYLE=lineStyle[plotCount], $
                                  COLOR=color[plotCount], $
                                  FONT_SIZE=defBigFontSize, $
                                  XTICKFONT_SIZE=defMidFontSize, $
                                  YTICKFONT_SIZE=defMidFontSize, $
                                  CURRENT=window)

        ;; IF (k MOD plotsPerWindow) EQ 0 OR (k EQ nPlots) THEN BEGIN
        IF plotCount EQ (plotsPerWindow-1) OR (k EQ nPlots) THEN BEGIN

           plotCount = 0
           stopKappa = kapBarb[k-1].kappa[0]

           plotFil = filPref + (STRING(FORMAT='("kappa_",F0.3,"-",F0.3)',startKappa,stopKappa)).Replace(".","_") + '.png'

           IF FILE_TEST(plotDir+plotFil) AND ~KEYWORD_SET(overwrite_plots) THEN BEGIN
              PRINT,plotFil + ' already exists!'
              CONTINUE

           ENDIF

           legend = LEGEND(TARGET=plotArr[*],FONT_SIZE=defMidFontSize)

           PRINT,'Saving ' + plotFil + ' ...'
           window.Save,plotDir+plotFil
           window.Close

        ENDIF
        
        k++
        
     ENDWHILE
     
  ENDIF


  STOP
END
