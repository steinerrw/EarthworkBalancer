C****************************************************************
      PROGRAM MAIN
C*****************************************************************
C      EARTHWORK BALANCING SYSTEM
C
C                                     RICK STEINER
C****************************************************************
C
      CHARACTER OPT*1, EW_FILE_ID*40,EW_PRINT_FILE*40
      CALL CLS
   10 WRITE(*,900)
      CALL GET_INP(OPT)
      IF (OPT .EQ. '1') THEN
         CALL EBS1(EW_FILE_ID)                    ! INPUT .CNF
      ELSEIF (OPT .EQ. '2') THEN
         CALL EBS2(EW_FILE_ID)                    ! EDIT & BROWSE
      ELSEIF (OPT .EQ. '3') THEN
         CALL EBS3(EW_FILE_ID,EW_PRINT_FILE)      ! BALANCE PROCESS
      ELSEIF (OPT .EQ. '4') THEN
         CALL EBS4(EW_PRINT_FILE)                 ! PRINT 
      ELSEIF ((OPT .EQ. 'Q') .or. (OPT .EQ. 'q')) THEN                 
         GO TO 1000                               ! QUIT
      ENDIF
      CALL CLS
      GO TO 10
 1000 CONTINUE 
  900 FORMAT(//,T20,40('*'),
     >        /,T20,'*  EARTHWORK BALANCING SYSTEM  (V 1.3) *',
     >        /,T20,40('*'),
     >        /,T20,'*   1.   DATA ENTRY                    *',
     >        /,T20,'*   2.   EDIT & BROWSE                 *',
     >        /,T20,'*   3.   RUN                           *',
     >        /,T20,'*   4.   PRINT                         *',
     >        /,T20,'*   Q.   QUIT                          *',
     >        /,T20,40('*'))
      STOP 02
      END
C**********************************************************************
C    EBS1       V 1.3            RICK STEINER                10/27/89 *
C**********************************************************************
      SUBROUTINE EBS1(EW_FILE_ID)
      INTEGER   CUT, CUT_SIGN, FILL, FILL_SIGN, WIN_IR1, WIN_IR2,
     >          NO_CHAR, NO_CHAR1
      CHARACTER ZINPUT*30, OPT, EW_FILE_NAME*40, CNTY*15, ROUTE*10,
     >          COMMENT*30, SF1*4, APPEND*1, INBUF*80, REC_HEAD*2,
     >          EW_FILE_ID*40, FB*1,AST*1
      LOGICAL*1 TST
      EQUIVALENCE (REC_HEAD, INBUF(11:12))
      DATA WIN_IR1, WIN_IR2/1,11/
      ZINPUT = ' '
      APPEND = 'N'
      CALL CLS
      CALL MOVE_CURSOR(10,1)
  100 WRITE(*,905)
      CALL GET_INP(EW_FILE_NAME)
      CALL CLS
      NO_CHAR=INDEX(EW_FILE_NAME,' ')-1
      EW_FILE_ID=EW_FILE_NAME(1:NO_CHAR)//'.CNF'
      WRITE(*,*)' FILE ID=',EW_FILE_ID
      READ(*,*)
      INQUIRE(FILE=EW_FILE_ID,EXIST=TST)
      IF (.NOT. TST) THEN
         OPEN(4,FILE=EW_FILE_ID,STATUS='NEW',FORM='FORMATTED')
      ELSE
C         EW_FILE_ID=EW_FILE_NAME
         OPEN(4,FILE=EW_FILE_ID,STATUS='OLD',FORM='FORMATTED')
         READ(4,902,END=206)INBUF
         IF (REC_HEAD .EQ. 'EW') THEN
            WRITE(*,902)INBUF
         ELSE
            WRITE(*,903)
            READ(*,*)
            CALL CLS
            GO TO 100            
         ENDIF
  206    CLOSE(4)
  208    CONTINUE
         WRITE(*,915) EW_FILE_ID
         CALL GET_INP(OPT)
         IF (OPT .EQ. '1') THEN
C
C***   OVERWRITE TO THE OLD EW FILE
C
            OPEN (4,FILE=EW_FILE_ID,STATUS='OLD',FORM='FORMATTED')
         ELSEIF (OPT .EQ. '2') THEN
C
C***   APPEND TO THE END OF EW FILE
C
            OPEN (4,FILE=EW_FILE_ID,STATUS='OLD',FORM='FORMATTED',
     >      ACCESS='APPEND')
            APPEND = 'Y'
         ELSEIF (OPT .EQ. '0') THEN
            RETURN
         ELSE 
            GO TO 208
         ENDIF
      ENDIF
      CALL CLS
      CALL DEF_WINDOW(WIN_IR1,WIN_IR2)
      CALL DOUBLE_LINE(WIN_IR2+1,1)
C
C***  ENTER FILE DESCRIPTION
C
      IF (APPEND .NE. 'Y') THEN
         CALL SET_WINDOW(0)
  305    CALL ERASE_BLOCK(WIN_IR2+2)
         CALL MOVE_CURSOR(WIN_IR2+2,1)
         WRITE(*,920)
         CALL GET_INP(SF1)
         READ(SF1,*,ERR=310)SF
         GO TO 315
  310    CALL ERR_PROC(*305)
  315    CONTINUE
         CALL ERASE_BLOCK(WIN_IR2+2)
         CALL MOVE_CURSOR(WIN_IR2+2,1)
         WRITE(*,922)
         CALL GET_INP(CNTY)
         WRITE(*,924)
         CALL GET_INP(ROUTE)
         WRITE(*,926)
         CALL GET_INP(COMMENT)
         AST='*'
         EW='EW'
         WRITE(4,900)AST,EW,SF,CNTY,ROUTE,COMMENT
         CALL SET_WINDOW(1)
         CALL MOVE_CURSOR(WIN_IR2-1,1)
         WRITE(*,900)AST,EW,SF,CNTY,ROUTE,COMMENT
      ENDIF
C
C***  ENTER STATION
C
      CURR_CUT=' '
      CURR_FILL=' '
  400 CALL SET_WINDOW(0)
      CALL ERASE_BLOCK(WIN_IR2+2)
      CALL MOVE_CURSOR(WIN_IR2+2,1)
      WRITE(*,930)
      CALL GET_INP(ZINPUT)
      READ(ZINPUT,*,ERR=410)STA
  410 IF ((ZINPUT(1:3) .EQ. 'END') .OR. (ZINPUT(1:3) .EQ. 'end')) THEN
         GOTO 800
      ELSE
         READ(ZINPUT,*,ERR=400)STA
      ENDIF
C
C***  ENTER CUT
C
  411 CALL MOVE_CURSOR(WIN_IR2+5,1)
      WRITE(*,932)
      CALL GET_INP(ZINPUT)
      PREV_CUT = CURR_CUT
      IF (ZINPUT .EQ. '-') THEN
         CUT_SIGN = 1
         CUT = 0
         CURR_CUT = '-'
      ELSE
         CUT_SIGN = 0
         READ(ZINPUT,*,ERR=411)CUT
         CURR_CUT = ' '
      ENDIF
C
C***  ENTER FILL
C
  421 CALL MOVE_CURSOR(WIN_IR2+8,1)
      WRITE(*,934)
      CALL GET_INP(ZINPUT)
      PREV_FILL = CURR_FILL
      IF (ZINPUT .EQ. '-') THEN
         FILL_SIGN = 1
         FILL = 0
         CURR_FILL = '-'
      ELSE
         FILL_SIGN = 0
         READ(ZINPUT,*,ERR=411)FILL
         CURR_FILL = ' '
      ENDIF
      WRITE (4,910)STA,CUT,FILL,CUT_SIGN,FILL_SIGN
      CALL SET_WINDOW(1)
      CALL MOVE_CURSOR(WIN_IR2-1,1)
      WRITE (*,910)STA,CUT,FILL,CUT_SIGN,FILL_SIGN
      GO TO 400
  800 CONTINUE
      CALL SET_WINDOW(0)
      CALL DEF_WINDOW(1,25)
  802 CALL CLS
      CLOSE (4)
  806 CONTINUE
      CALL CLS
  850 WRITE(*,940)EW_FILE_ID
      READ(*,*)
      CLOSE(4)
  940 FORMAT(/,T5,70('*'),/,T5,'* THE FOLLOWING .CNF FILE WAS CREATED ', 
     >         A30,' *',/,T5,70('*'),//,T2,'PRESS <ENTER> TO CONTINUE')
  900 FORMAT(A1,9X,A2,1X,F4.1,1X,A15,1X,A10,1X,A30)
  902 FORMAT(A80)
  903 FORMAT(10X,'*** ERROR CUT & FILL FILE READ IN ***',
     >      /10X,'***    HIT <ENTER> TO CONTINUE    ***')
  905 FORMAT(//,T20,'PLEASE ENTER CUT & FILL (.CNF) FILE NAME',
     >        /,T20,'    WITHOUT EXTENTION OR HIT CNTL-Y TO EXIT',/)
  910 FORMAT(1X,F9.3,2X,I5,5X,I5,10X,2I1)
  915 FORMAT(//,T10,'*** CUT & FILL (.CNF) FILE EXISTS - ',A40,
     >         //,T25,'DO YOU WANT TO',
     >          /,T25,'   0    = EXIT',
     >          /,T25,'   1    = OVERWRITE',
     >          /,T25,'   2    = APPEND--TO NEXT PROMPT',/)
  920 FORMAT(/,T21,'ENTER SHRINKAGE FACTOR:')
  922 FORMAT(/,T21,'ENTER COUNTY NAME :')
  924 FORMAT(/,T21,'ENTER ROUTE ID:')
  926 FORMAT(/,T21,'ENTER COMMENTS (30 CHARACTERS):')
  930 FORMAT(/,T21,'ENTER STATION NUMBER:')
  932 FORMAT(/,T21,'ENTER CUT:')
  934 FORMAT(/,T21,'ENTER FILL:')
      RETURN
      END
C******************************************************************
      SUBROUTINE ERASE_BLOCK(IROW)
C******************************************************************
      CALL MOVE_CURSOR(IROW,1)
      WRITE (*, 900) CHAR(27)
  900 FORMAT (1X, A1,'[0J',$)
      RETURN
      END
C******************************************************************
      SUBROUTINE CLS
C******************************************************************
      WRITE(*,*)CHAR(27),'[2J'
      WRITE(*,*)CHAR(27),'[H'
      RETURN
      END
C*****************************************************************
      SUBROUTINE GET_INP(INCHAR)
C*****************************************************************
      CHARACTER*(*) INCHAR
      WRITE(*,900)
  900 FORMAT(20X,'===>',$)
      READ(*,'(A)')INCHAR
      RETURN
      END
C*****************************************************************
      SUBROUTINE DEF_WINDOW(IR1,IR2)
C*****************************************************************
      WRITE(*,900)CHAR(27),IR1,IR2
  900 FORMAT(1X,A1,'[',I2.2,';',I2.2,'r')   ! SET SCROLL 
      RETURN
      END
C**************************************************
      SUBROUTINE SET_WINDOW(IW)
C**************************************************
      IF (IW .EQ. 1)THEN
        WRITE(*,900)CHAR(27),CHAR(27)       ! BOLD, SCROLL  
      ELSE
        WRITE(*,905)CHAR(27),CHAR(27)       ! NORMAL
      END IF
  900 FORMAT(1X,A1,'[?6h',1X,A1,'[1m',$)
  905 FORMAT(1X,A1,'[?6l',1X,A1,'[0m',$)
      RETURN
      END
C*****************************************************
      SUBROUTINE DOUBLE_LINE(IR,IC1)
C*****************************************************
      CALL MOVE_CURSOR(IR,IC1)
  200 WRITE(*,900)
  900 FORMAT(1X,80('='))
      RETURN
      END          
C********************************************
      SUBROUTINE ERASE_LINE(IR)
C********************************************
      CALL MOVE_CURSOR(IR,1)
      WRITE(*,900)CHAR(27)
  900 FORMAT(1X,A1,'[0K',$)
      RETURN
      END      	
C**********************************************
      SUBROUTINE MOVE_CURSOR(IR,IC)
C**********************************************
      WRITE(*,900)CHAR(27),IR,IC
  900 FORMAT(1X,A1,'[',I2.2,';',I2.2,'H',$)      
      RETURN
      END      	
C**********************************************
      SUBROUTINE ERR_PROC(*)
C**********************************************
      WRITE(*,900)
      READ(*,*) 
  900 FORMAT(12X,'ILLEGAL DATA INPUT !  HIT <ENTER> TO CONTINUE')
      CALL ERASE_BLOCK(14)
      RETURN 1
      END
C*******************************************************
      SUBROUTINE EBS2(DEF_FILE)
C*******************************************************
C      INVOKES EDT EDITOR          
C*******************************************************
      CHARACTER*40 INFILE, COM_ID1, DEF_FILE, COM_ID2
      CHARACTER EXT*4
      COM_ID1 = 'ZFA1:[110100]EDT_STARTUP1.COM'
      COM_ID2 = 'ZFA1:[110100]EDT_STARTUP2.COM'
      CALL CLS
      CALL MOVE_CURSOR(3,1)
      WRITE(*,905)
C     
C***    QUERY INPUT FILE
C
      EXT='.CNF'
      CALL IN_FILE_PROC(DEF_FILE,INFILE,EXT)
      IF ((EXT .EQ. '.EWK') .or. (EXT .EQ. '.ewk')) THEN
         CALL EDT$EDIT(INFILE,,COM_ID2)
      ELSE
         CALL EDT$EDIT(INFILE,,COM_ID1)
      ENDIF
905   FORMAT(28X,'--- EDIT / BROWSE ---') 
      RETURN
      END
C********************************************************
      SUBROUTINE IN_FILE_PROC(DEF_FILE,INFILE,EXT)
C********************************************************
      CHARACTER*40 INFILE, DEF_FILE
      CHARACTER EXT*4
C
C***    LOOP UNTIL INFILE EXISTS
C
100   CALL MOVE_CURSOR(05,1)
      WRITE(*,900)DEF_FILE
      CALL GET_INP(INFILE)
      IF(INFILE .EQ. ' ') INFILE = DEF_FILE
      CALL FILE_CHECK(INFILE,EXT,*100)
C
C***   END OF LOOP
C
      DEF_FILE = INFILE
      EXT = DEF_FILE(INDEX(DEF_FILE,'.'):INDEX(DEF_FILE,' '))
900   FORMAT(15X,'INPUT FILE NAME OR HIT <ENTER> TO USE DEFAULT :',
     >      /15X,' - ',A40,/)
      RETURN
      END
C********************************************************
      SUBROUTINE EBS3(DEF_FILE,EW_PRINT_FILE)
C********************************************************
      DIMENSION STA(1000), CUT(1000), FILL(1000), XSIGN(1000)
      CHARACTER XSIGN*4, CNTY*15, ROUTE*10, COMMENT*30,
     >          REC_HEAD*2, DDATE*9, SORT_FILE*40
      CHARACTER INFILE*40,   EW_PRINT_FILE*40, DUMMY*1,
     >          DEF_FILE*40, EXT*4, OPT*1, BAL_DIR*1
      INTEGER CUT, FILL
C
C**** FILE STATUS AND SORT .CNF FILE
C
      CALL CLS
      EXT='.CNF'
      CALL IN_FILE_PROC(DEF_FILE,INFILE,EXT)
      NC=INDEX(INFILE,'.')-1
      OPEN (4,FILE=INFILE,STATUS='OLD')
      EW_PRINT_FILE=INFILE(1:NC)//'.EWK'
      OPEN (7,FILE=EW_PRINT_FILE,STATUS='NEW')
C
C****  BUILD ARRAY
C
      READ(4,905,ERR=800)REC_HEAD, SF, CNTY, ROUTE, COMMENT      
      CALL DATE(DDATE)
      WRITE(7,920)DDATE,CNTY,ROUTE
      IP = 1
      NEQU = 0
  200 CONTINUE
      READ(4,910,END=250)STA(IP), CUT(IP), FILL(IP),
     >                           XSIGN(IP)(1:3),XSIGN(IP)(4:4)      
C      READ(4,910,END=250,ERR=810)STA(IP), CUT(IP), FILL(IP),
C     >                           XSIGN(IP)(1:3),XSIGN(IP)(4:4)      
      IP = IP + 1
      GO TO 200
  250 CONTINUE
      CLOSE(4)
      NP  = IP - 1 
      IERR = 0
C
C***     CHECK IF STATION NUMBERS ARE IN ASCENDING ORDER
C
      DO 300 IP =2,NP
         IF((STA(IP).LT.STA(IP-1)) .AND. (XSIGN(IP)(4:4).NE.'E')) THEN       
           IERR = IERR + 1
           WRITE(*,960)STA(IP-1),STA(IP)
         END IF
  300 CONTINUE            
      IF (IERR .NE. 0) THEN
         WRITE(*,965)IERR
         READ(*,*)
         RETURN
      END IF
C
C***   INQUIRE BALANCING TYPE (1=FORWARD/2=BACKWARD)
C
      OPT = ' '
      BAL_DIR = ' '
      DO WHILE ((OPT .NE. 'Q') .AND. (OPT .NE. 'q'))
        CALL CLS
        WRITE(*,900)
        CALL GET_INP(OPT)
        IF (OPT .EQ. '1') THEN
          BAL_DIR = 'F'
          CALL EBS3A(STA,CUT,FILL,XSIGN,NP,SF,BAL_DIR)    ! FORWARD PROCESS
        ELSE IF(OPT .EQ. '2') THEN
          BAL_DIR = 'B'
          CALL EBS3A(STA,CUT,FILL,XSIGN,NP,SF,BAL_DIR)    ! BACKWARD PROCESS
        END IF
      END DO
      GO TO 850
C
C***    ERROR HANDLING FOR ILLEGAL RECORD I READ
C
  800 CONTINUE
      CALL CLS
      WRITE(*,940)REC_HEAD,SF,CNTY,ROUTE,COMMENT
      READ(*,*)
      CLOSE(7)
      RETURN
C
C***    ERROR HANDLING FOR ILLEGAL RECORD 2 READ
C
  810 CONTINUE
      CALL CLS
      WRITE(*,950)STA(IP),CUT(IP),FILL(IP),XSIGN(IP)
      READ(*,*)
      CLOSE(7)
      CLOSE(4)
      RETURN
C
C***    DISPLAY THE .EWK FILE ID. SHOW THE LOCATION OF FINAL RESULT. 
C
  850 WRITE(*,930)EW_PRINT_FILE
      READ(*,*)
      CLOSE(7)   
      CLOSE(4)
C
C***    FORMAT STATEMENTS
C
  900 FORMAT(////,10X,'SELECT THE BALANCING TYPE :',
     >         //,10X,'  1  -  FORWARD PROCESS.  '
     >          /,10X,'  2  -  BACKWARD PROCESS. '
     >         //,10X,'  Q  -  QUIT              ',//)
  905 FORMAT(T11,A2,T14,F4.1,T19,A15,T35,A10,T46,A30)
  910 FORMAT(1X,F9.2,T13,I5,T23,I5,T38,A3,T11,A1)
  920 FORMAT(1H1,/,T30,52('*'),
     >  /,T30,'*                 SOUTH CAROLINA                   *', 
     >  /,T30,'* DEPARTMENT OF HIGHWAYS AND PUBLIC TRANSPORTATION *',
     >  /,T30,'*                                                  *',
     >  /,T30,'*          EARTHWORK BALANCING SYSTEM              *',
     >  /,T30,'*                                                  *',
     >  /,T30,'* VERSION 1.3                    DATE:',A9,'    *',
     >  /,T30,'*                                                  *',
     >  /,T30,'* COUNTY: ',A15,8X,'ROUTE: ',A10,' *',
     >  /,T30,52('*'))
  930 FORMAT(//,T5,'THE FOLLOWING .EWK FILE WAS CREATED :',A30,
     >       //,T5,'PRESS <ENTER> TO CONTINUE')
  940 FORMAT(/,T2,'ERROR IN READING .CNF HEADER RECORD',
     >       /,T11,A2,T13,F4.1,T19,A15,T35,A10,T46,A30,
     >      //,T5,'PRESS <ENTER> TO CONTINUE')
  950 FORMAT(/,T2,'ERROR IN READING .CNF FILE - PROCESSING STOPPED',
     >       /,T2,F9.2,T13,I5,T23,I5,T38,A3,
     >      //,T5,'PRESS <ENTER> TO CONTINUE')
  960 FORMAT(/T5,'*** ERROR.  STAION NUMBERS NOT IN ASCENDING ORDER.',
     >           ' ***',/,10X,'(STA. ',F8.2,'/',F8.2,' )')  
  965 FORMAT(/T10,'TOTAL NO. OF ERRORS = ',I3,' - PROCESSING STOPPED',
     >      //,T10,'PRESS <ENTER> TO CONTINUE')
      RETURN
      END          
C********************************************************
      SUBROUTINE FWD_BAL (STA, NP, JBP, JEP, INCR)
C********************************************************
      DIMENSION STA(NP), STA_IN(2), IP(2)
      CHARACTER*8 XSTA, TDESC(2), OPT*1
      DATA TDESC/'BEG STA.','END STA.'/ 
      INCR = 1
      CALL CLS
30    CONTINUE
      WRITE(*,900)
      CALL GET_INP(OPT)
      IF (OPT .NE. '2') THEN
        JBP = 1
        JEP = NP
        RETURN
      END IF         
C
C***   INQUIRE BEG. STA. & END STA.
C
      DO 100 I = 1,2
50      WRITE(*,905)TDESC(I)
        CALL GET_INP(XSTA)
        READ(XSTA,*)STA_IN(I)
        CALL FIND_STA(STA,NP,STA_IN(I),IP(I),*50)
100   CONTINUE           
      JBP = IP(1)
      JEP = IP(2)
900   FORMAT(////,20X,'FORKWORD BALANCING. ',
     >         //,20X,'  1. TOP --> BOTTOM ',
     >          /,20X,'  2. ENTER BEG. & END STA. ',/)
905   FORMAT(//,10X,'ENTER ',A8)
910   FORMAT(10X,'INPUT ERROR.  END STA. SHOULD BE GREATER THAN'
     >           ' BEG STA. ') 
      RETURN
      END
C********************************************************
      SUBROUTINE REV_BAL (STA, NP, JBP, JEP, INCR)
C********************************************************
      DIMENSION STA(NP), STA_IN(2), IP(2)
      CHARACTER*8 XSTA, TDESC(2), OPT*1
      DATA TDESC/'BEG STA.','END STA.'/ 
      INCR = -1
      CALL CLS
30    CONTINUE
      WRITE(*,900)
      CALL GET_INP(OPT)
      IF (OPT .NE. '2') THEN
        JBP = NP
        JEP = 1
        RETURN
      END IF         
C
C***   INQUIRE BEG. STA. & END STA.
C
      DO 100 I = 1,2
50      WRITE(*,905)TDESC(I)
        CALL GET_INP(XSTA)
        READ(XSTA,*)STA_IN(I)
        CALL FIND_STA(STA,NP,STA_IN(I),IP(I),*50)
100   CONTINUE           
      JBP = IP(1)
      JEP = IP(2)
900   FORMAT(////,20X,'BACKWORD BALANCING. ',
     >         //,20X,'  1. BOTTOM --> TOP ',
     >          /,20X,'  2. ENTER BEG. & END STA. ',/)
905   FORMAT(//,10X,'ENTER ',A8)
910   FORMAT(10X,'INPUT ERROR.  BEG STA. SHOULD BE GREATER THAN'
     >           ' END STA. ') 
      RETURN
      END
C********************************************************
       SUBROUTINE EBS3A(STA,CUT,FILL,XSIGN,NP,SF,BAL_DIR)
C********************************************************
      DIMENSION STA(NP), CUT(NP), FILL(NP), XSIGN(NP)
      CHARACTER INFILE*40, XSIGN*4, REC_HEAD*2, DDATE*9, 
     >          EW_FILE_NAME*40, EW_FILE_ID*40,
     >          EW_PRINT_FILE*40, DEF_FILE*40, EXT*4, BAL_DIR*1
      INTEGER CUT, FILL, CUT_VOL, FILL_VOL,
     >        D_CUT, D_FILL, DBC, DBF, PTC, PTF,
     >        TOT_CUT, TOT_FILL,
     >        BAL_CUT, BAL_FILL, F_BAL_FILL,
     >        PG_TOT_CUT, PG_TOT_FILL, RN_TOT_CUT, RN_TOT_FILL,
     >        G_TOT_CUT, G_TOT_FILL, LINE, PAGE
      REAL    STA, CUT_DIST, FILL_DIST, TOT_DIRT 
      INTEGER*2 BAL_SW
C
C***     INITIALIZATION  
C
      D_CUT(I)    = CUT(I) + CUT(I-INCR)
      D_FILL(I)   = FILL(I) + FILL(I-INCR)
      CUT_VOL(K)  = INT(D_CUT(K)  * CUT_DIST  / 54.+0.5)
      FILL_VOL(K) = INT(D_FILL(K) * FILL_DIST / 54.+0.5)
      TOT_CUT = 0
      TOT_FILL = 0
      G_TOT_CUT = 0
      G_TOT_FILL = 0
      TOT_DIST = 0.0
      TOT_DIRT = 0.0
      PG_TOT_CUT = 0
      PG_TOT_FILL = 0
      SFF = 1. + SF /100.
      NEQU = 0
      PAGE=1
      WRITE(7,900)PAGE,SF
      LINE=5
C
C***    FORWARD & BACKWARD PROCESS.   RANDOM SATION NUMBERS SELECTION.
C
      IF (BAL_DIR .EQ. 'F') THEN 
         CALL FWD_BAL(STA, NP, JBP, JEP, INCR)
      ELSE
         CALL REV_BAL(STA, NP, JBP, JEP, INCR)
      END IF
      WRITE(*,910)STA(JBP),STA(JEP)
C
C***    1ST STATION PROCESS
C
      WRITE(7,935) STA(JBP),CUT(JBP),FILL(JBP)
      IBP = JBP + INCR
C
C***    TURN ON/OFF THE BALANCE SWITH
C
      BAL_SW = 1
      IF (CUT_VOL(IBP) .LT. FILL_VOL(IBP)*SFF) BAL_SW = -1
C****************************************************************
C
C       LOOP FROM 2ND STA. THRU LAST STA.
C       
C****************************************************************
      DO 400 IP = IBP, JEP, INCR
         STA_DIST  = ABS(STA(IP) - STA(IP-INCR))
C
C***       EQUALITY PROCESS.    'E' CODE PROCESS
C
         IF (XSIGN(IP)(4:4) .EQ. 'E') THEN 
            NEQU = NEQU + 1
            IF (JMOD(NEQU,2) .EQ. 0) THEN
              STA_DIST = 0.0
              WRITE (7,955) STA(IP-INCR),STA(IP)
            END IF         
         END IF
C
C***       COMPUTE DISTANCE BTW 2 STATION NUMBERS.
C          INCLUDE '-' CODE PROCESS (1/2 OF THE DISTANCE).
C
         CUT_DIST  = STA_DIST
         FILL_DIST = STA_DIST
         TOT_DIST  = TOT_DIST + STA_DIST
         IF ((XSIGN(IP)  (1:1) .EQ. '1') .OR. 
     >       (XSIGN(IP-INCR)(1:1) .EQ. '1'))
     >        CUT_DIST = 0.5 * STA_DIST
         IF ((XSIGN(IP)  (2:2) .EQ. '1') .OR. 
     >       (XSIGN(IP-INCR)(2:2) .EQ. '1'))
     >        FILL_DIST = 0.5 * STA_DIST
C
C***       SUM TOTAL VOLUMN
C
         TOT_CUT  = TOT_CUT  + CUT_VOL(IP)      
         TOT_FILL = TOT_FILL + FILL_VOL(IP)      
         G_TOT_CUT  = G_TOT_CUT  + CUT_VOL(IP)      
         G_TOT_FILL = G_TOT_FILL + FILL_VOL(IP)      
         TOT_DIRT = TOT_DIRT + CUT_VOL(IP) - FILL_VOL(IP) * SFF
C
C***      BALANCING PROCESS
C
         IF ((BAL_SW .GT. 0  .AND. TOT_DIRT .LT. 0.0) .OR.
     >       (BAL_SW .LT. 0  .AND. TOT_DIRT .GT. 0.0)) THEN
              BAL_SW = -1 * BAL_SW
C
C***       SKIP BALANCING PROCESS IF TOTAL DISTANCE < 200 FT.
C
            IF (TOT_DIST .GT. 200.) THEN
                PTC = TOT_CUT - CUT_VOL(IP)
                PTF = TOT_FILL - FILL_VOL(IP)
                DSTA = STA_DIST*(PTC-PTF*SFF)
     >                 /(FILL_VOL(IP)*SFF - CUT_VOL(IP))
                BAL_STA = STA(IP-1) + DSTA
                IF(BAL_DIR .EQ. 'B')BAL_STA = STA(IP+1) - DSTA
                DBC = INT(DSTA/STA_DIST  * CUT_VOL(IP) + 0.5)
                DBF2= DSTA/STA_DIST * FILL_VOL(IP) 
                DBF = INT(DBF2 + 0.5)
                BAL_CUT  = PTC + DBC
                BAL_FILL = PTF + DBF
                F_BAL_FILL = INT((PTF + DBF2)*SFF+0.5)
                TOT_CUT  = CUT_VOL(IP)  - DBC
                TOT_FILL = FILL_VOL(IP) - DBF
                TOT_DIST = 0.0
                WRITE(7,940)DBC, DBF, BAL_STA,  BAL_CUT,   
     >                      BAL_FILL, F_BAL_FILL,
     >                      TOT_CUT, TOT_FILL
                LINE=LINE+2
            END IF
         END IF      
C
C***     OUTPUT THE RESULTS TO .EWK FILE
C
  350 WRITE(7,930) STA(IP),  CUT(IP),  D_CUT(IP),
     >             CUT_DIST,  CUT_VOL(IP), TOT_CUT,
     >             FILL(IP),  D_FILL(IP),
     >             FILL_DIST, FILL_VOL(IP),TOT_FILL,
     >             INT(TOT_FILL*SFF+0.5) 
      IF (XSIGN(IP)(1:1) .EQ. '1') THEN
         WRITE(7,902)
      ENDIF
      IF (XSIGN(IP)(1:2) .EQ. '1') THEN
         WRITE(7,904)
      ENDIF
      PG_TOT_CUT  = PG_TOT_CUT  + CUT_VOL(IP)
      PG_TOT_FILL = PG_TOT_FILL + FILL_VOL(IP)
C
C***    FORCED BALANCE.   'B' CODE PROCESS. 
C
      IF (XSIGN(IP)(3:3) .EQ. 'B')THEN
         IF (TOT_DIRT .LT. 0) THEN 
             BORROW = -1*TOT_DIRT
             WRITE(7,945)'BORROW :',BORROW
             LINE=LINE+2
         ELSE           
             WASTE  = TOT_DIRT
             WRITE(7,945)'WASTE : ',WASTE
             LINE=LINE+2
         END IF
         TOT_CUT  = 0.0
         TOT_FILL = 0.0
         TOT_DIRT = 0.0
         TOT_DIST = 0.0
         BAL_SW = 1
         IF (IP .LT. NP) THEN
            WRITE(7,950)
            IF (CUT_VOL(IP-INCR) .LT. FILL_VOL(IP-INCR)*SFF)BAL_SW = -1
         END IF
      END IF         
C
C***   LINE & PAGE CONTROL
C
      CALL LINE_CHECK(LINE,PAGE,SF,IP,JEP,PG_TOT_CUT,
     >                PG_TOT_FILL, G_TOT_CUT, G_TOT_FILL,TOT_DIRT)
  400 CONTINUE
C
C***   END OF LOOP. 
C
      WRITE(*,920)
      READ(*,*)
C
C***   FORMAT STATEMENTS
C
  900 FORMAT(1H1,/,
     >        T100,'PAGE:',I2,/,
     >        T22,'DOUBLE',T49,'BALANCE',T67,'DOUBLE',T98,'BALANCE',/,
     >        T5,'STATION',T17,'CUT',T23,'AREA',T30,'DISTANCE',
     >        T41,'VOLUME',
     >        T51,'CUT',T61,'FILL',T68,'AREA',T75,'DISTANCE',
     >        T86,'VOLUME',
     >        T97,'FILL',T103,'F+',F4.1,T109,'%',/,T2,110('='))  
  902 FORMAT('+',T19,'-')
  904 FORMAT('+',T72,'-')
  910 FORMAT(//,20X,'  BALANCING PROCESS',
     >        /,20X,'FROM STA ',F8.2,'  TO STA ',F8.2) 
  920 FORMAT(//,20X,'PROCESS COMPLETED.  HIT <ENTER> TO CONTINUE.') 
  930 FORMAT(T2,F10.2,T14,I6,T22,I6,T31,F6.2,T40,I7,T49,I7,
     >      T59,I6,T67,I6,T75,F6.2,T85,I7,T94,I7,T103,I7)
  935 FORMAT(T2,F10.2,T14,I6,T59,I6)
  940 FORMAT(T39,'(',I7,')',T84,'(',I7,')', 
     >      /T2,F10.2,'**',T49,I7,T94,I7,T103,I7,
     >      /T39,'(',I7,')',T84,'(',I7,')')
  945 FORMAT(/T5,'** FORCED BALANCE **',T28,A8,F7.0,'  C.Y.')
  950 FORMAT(T55,'0',T100,'0')
  955 FORMAT(/T5,'** EQUALITY **',5X,'(STA.',F8.2,' = STA.',F8.2,')'/)    
      RETURN
      END 
C************************************************************************
      SUBROUTINE FIND_STA (STA, NP, FSTA, IPN, *)
C*****************************************************************
C
C     SEARCH THE TARGET STATION NUMBER FROM SATION ARRAY
C      AND RETURN THE INDEX .
C     IF MORE THAN ONE STA. FOUND.  QUERRY THE SEQUENCE NUMBER.
C
C*****************************************************************
      DIMENSION STA(NP)
      INTEGER*2 JP(8)
      IE = 0
      DO 100 I=1,NP
        IF (STA(I) .EQ. FSTA)THEN
           IE = IE + 1
           JP(IE) = I
        END IF
100   CONTINUE
      IPASS = 0
      IF (IE .EQ. 0)THEN
         PRINT *,'    *****  NON-EXISTANT STATION *****'
         RETURN 1
      ELSE IF (IE .EQ. 1) THEN
         IPN = JP(1)
      ELSE
        DO WHILE (IPASS .EQ. 0)
           WRITE(*,900)IE,FSTA
           DO 200 J=1,IE
200        WRITE(*,910)J,JP(J)+1,STA(JP(J))
           WRITE(*,920)
           READ (*,*) INNO
           IF ( INNO .LE. NP) THEN
             IPN = JP(INNO)
             IPASS = 1
           END IF
        END DO
      END IF
900   FORMAT(/,T20,'*** ',I3,' STA.',F8.2,' FOUND ***',
     >       //,T20,'SEQ',T25,'LINE-NO',T35,'STA.')
910   FORMAT(T20,I3,T25,I3,T35,F8.2)
920   FORMAT(//,T20,'PLEASE KEY IN ONE SEQUENCE NO.===>',$)
      RETURN
      END
C***********************************************************************
	SUBROUTINE FILE_CHECK ( FILE_NAME, EXT, * )
C
C       ADD THE DEFAULT EXT. AND DETERMINE IF FILE EXIST
C***********************************************************************
      CHARACTER*40 FILE_NAME, EXT*4
      LOGICAL*1 TST
       J1 = INDEX ( FILE_NAME, '.' )
       J2 = INDEX ( FILE_NAME, ' ' ) - 1
       IF ( J1 .EQ. 0 ) FILE_NAME = FILE_NAME(1:J2) // EXT
       INQUIRE ( FILE = FILE_NAME, EXIST=TST )
       IF ( .NOT. TST) THEN
         PRINT *,' '
         PRINT *,'                     *****  NON-EXISTANT FILE  *****'
         RETURN 1
      END IF
      RETURN
      END
C**********************************************
      SUBROUTINE LINE_CHECK(LINE,PAGE,SF,IP,JEP,
     >                      PG_TOT_CUT,PG_TOT_FILL,
     >                      G_TOT_CUT,G_TOT_FILL,TOT_DIRT)
C**********************************************
      INTEGER PAGE,LINE,PG_TOT_CUT,PG_TOT_FILL,G_TOT_CUT,G_TOT_FILL,
     >        IP, JEP
      LINE = LINE + 1
      IF (IP .EQ. JEP) THEN
         IF (TOT_DIRT .LT. 0) THEN 
             BORROW = -1*TOT_DIRT
             WRITE(7,945)'BORROW :',BORROW
         ELSE           
             WASTE  = TOT_DIRT
             WRITE(7,945)'WASTE : ',WASTE
         END IF
         WRITE(7,900)PG_TOT_CUT,PG_TOT_FILL,G_TOT_CUT,G_TOT_FILL
      ELSEIF ((LINE .LE. 45) .AND. (IP .EQ. JEP)) THEN
         WRITE(7,900)PG_TOT_CUT,PG_TOT_FILL,G_TOT_CUT,G_TOT_FILL
      ELSEIF (LINE .GT. 45) THEN
         WRITE(7,905)PG_TOT_CUT,PG_TOT_FILL,G_TOT_CUT,G_TOT_FILL
         PG_TOT_CUT = 0
         PG_TOT_FILL= 0
         PAGE = PAGE + 1
         WRITE(7,910)PAGE,SF
         LINE = 5
      ENDIF
  900 FORMAT(T2,110('='),
     >        /,T2,'PAGE TOTAL',T38,I9,T83,I9,
     >        /,T2,'GRAND TOTAL',T38,I9,T83,I9) 
  905 FORMAT(T2,110('='),
     >        /,T2,'PAGE TOTAL',T38,I9,T83,I9,
     >        /,T2,'RUNNING TOTAL',T38,I9,T83,I9) 
  910 FORMAT(1H1,/,T100,'PAGE:',I2,/,
     >        T22,'DOUBLE',T49,'BALANCE',T67,'DOUBLE',T98,'BALANCE',/,
     >        T5,'STATION',T17,'CUT',T23,'AREA',T30,'DISTANCE',
     >        T41,'VOLUME',
     >        T51,'CUT',T61,'FILL',T68,'AREA',T75,'DISTANCE',
     >        T86,'VOLUME',
     >        T97,'FILL',T103,'F+',F4.1,T109,'%',/,T2,110('='))  
  945 FORMAT(/T4,'** FINISHED BALANCE **',T28,A8,F7.0,'  C.Y.')
      RETURN
      END
C**********************************************
      SUBROUTINE CNF_SORT(FILE_IN, FILE_OUT)
C**********************************************
      INTEGER STATUS, FN_SIZE_IN, FN_SIZE_OUT,
     >        LUN_IN, LUN_OUT,
     >        LIB$GET_INPUT, LIB$GET_LUN,
     >        SOR$PASS_FILES, SOR$BEGIN_SORT,
     >        SOR$SORT_MERGE, SOR$END_SORT
      CHARACTER*20 FILE_IN, FILE_OUT
      EXTERNAL DSC$K_DTYPE_T              ! CHARACTER DATA TYPE DEFINITION
      INTEGER*2 KEY_BUFFER(5)
      KEY_BUFFER(1) = 1                   ! NUMBER OF KEYS
      KEY_BUFFER(2) = %LOC(DSC$K_DTYPE_T) ! CHARACTER DATA
      KEY_BUFFER(3) = 0                   ! ASCENDING ORDER
      KEY_BUFFER(4) = 1                   ! STARTING POSITION
      KEY_BUFFER(5) = 9                   ! KEY LENGTH
      STATUS = SOR$PASS_FILES(FILE_IN, FILE_OUT)
      IF ( .NOT. STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
      STATUS = SOR$BEGIN_SORT(KEY_BUFFER)
      IF ( .NOT. STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
      STATUS = SOR$SORT_MERGE()
      IF ( .NOT. STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
      STATUS = SOR$END_SORT()
      IF ( .NOT. STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
      RETURN
      END
C***********************************************************************
C
      SUBROUTINE EBS4(DEF_FILE)
C
C***********************************************************************
      CHARACTER OPT*1,
     >          INFILE*40,INBUFF*132,
     >          DEF_FILE*40,EXT*4
      CALL CLS
      EXT='.EWK'
      CALL IN_FILE_PROC(DEF_FILE,INFILE,EXT)
      OPEN(7,FILE=INFILE,STATUS='OLD')
      CALL CLS
   10 WRITE(*,900)
      CALL GET_INP(OPT)
      IF (OPT .EQ. '1') THEN
         CLOSE (7,DISPOSE='PRINT')
      ELSEIF (OPT .EQ. '2') THEN
         CALL PC_PRINT(EXT)
         CLOSE(7)
      ELSE
         CLOSE(7)
         GO TO 1000
      ENDIF
 1000 CONTINUE
  900 FORMAT(/////,T20,'SELECT PRINTER DESTINATION',
     >           /,T20,' 1 - VAX PRINTER',
     >           /,T20,' 2 - PC  PRINTER',
     >           /,T20,' Q - QUIT')
  910 FORMAT(A132)
      RETURN
      END
C********************************************************************
C
      SUBROUTINE PC_PRINT(EXT)
C
C********************************************************************
      CHARACTER OPT*1, INFILE*40, INBUFF*132, EXT*4
      CALL CLS
      REWIND 7
   10 WRITE(*,900)
      CALL GET_INP(OPT)
C
C*****SETS SCREEN TO 132 COLUMNS WIDTH ***
C
      STATUS=LIB$SPAWN('SET TERM/WID=132')
C
C*****[5i  STARTS THE PRINTER PASS ***
C
      WRITE(*,*)CHAR(27),'[5i'
      ILINE = 1
      IF (OPT .EQ. '1') THEN
         IF ((EXT .EQ. '.EWK') .or. (EXT .EQ. 'ewk')) THEN
C
C********   CHAR(15) SETS PITCH TO 17 CPI ***
C
            WRITE(*,*)CHAR(27),CHAR(15)
         ELSE
C
C*******    CHAR(18) SETS PITCH TO 10 CPI ***
C
            WRITE(*,*)CHAR(27),CHAR(18)
         ENDIF   
  100    READ(7,910,END=175)INBUFF
         IF (INBUFF(1:1) .EQ. '1') THEN
            INBUFF(1:1) = ' '
            DO 150 IL = ILINE+1, 66
              WRITE(*,*)
  150       CONTINUE
            WRITE(*,920)INBUFF
            ILINE = 1
         ELSEIF (INBUFF(1:1) .EQ. '+') THEN
            WRITE(*,930)INBUFF(1:1),INBUFF(2:132)
         ELSE
            WRITE(*,920)INBUFF
            ILINE = ILINE + 1 
         END IF
         GOTO 100
C
C***         CHAR(18) SETS PITCH BACK TO 10 CPI ***
C
  175    WRITE(*,*)CHAR(27),CHAR(18)
         GO TO 1000
      ELSEIF (OPT .EQ. '2') THEN
         WRITE(*,*)' PRINTER OPTION 2 IS NOT INSTALLED'
         RETURN
      ELSE
         GO TO 1000
      ENDIF
 1000 CONTINUE
C
C***    [4i  END PRINTER PASS ***
C
      WRITE(*,*)CHAR(27),'[4i'
C
C***     SETS SCREEN BACK TO 80 COL WIDTH ***
C
      STATUS=LIB$SPAWN('SET TERM/WID=80')
  900 FORMAT(/////,T20,'SELECT PC PRINTER TYPE',
     >           /,T20,' 1 - OKIDATA   PRINTER',
     >           /,T20,' 2 - HP LASER  PRINTER',
     >           /,T20,' Q - QUIT')
  910 FORMAT(A132)
  920 FORMAT(1X,A131)
  930 FORMAT(A1,1X,A131)
      RETURN
      END