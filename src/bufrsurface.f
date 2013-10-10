C ##############################################################################
C #     PROGRAM BUFRSFC                                                        #
C ##############################################################################
C
C       A BUFR DATA FILE CONTAINS A SERIES OF "MESSAGES" (WHICH ARE VARIABLE
C         LENGTH RECORDS), EACH CONTAINING AT LEAST ONE BUFR "SUB-MESSAGE"
C         (REPORT).  THIS PROGRAM BREAKS THESE OPEN AND PRINTS OUT THE
C         REPORTS, ACCORDING TO THE USER'S CONFIGURATION FILE.
C
        CHARACTER*1 DEFAULT, DODIAG
C
        PARAMETER  ( IDUNIT=7, ICUNIT=8,IXUNIT=9)
        CHARACTER*8  DIRIN
        CHARACTER*3  PARLEL
        CHARACTER*8  DIROUT
        DATA  DIRIN  /'bufrobs/'/   ! EXECUTES IN THE UNTAR'D ../bufrobs DIRECTORY
        DATA  PARLEL /'../'/        !
        DATA  DIROUT /'textobs/'/   ! RESULTS ARE LEFT IN THE ../textobs DIRECTORY
        CHARACTER*32 DIGFILE, CONFILE
        DATA DIGFILE /'bufrsurface_diagnostics         '/
        DATA CONFILE /'bufrsurface_config              '/
C
        PARAMETER  (IIUNIT=11,IPUNIT=21)
        CHARACTER*30 INFILE, INFILES(1500), NOFILE
        DATA  NOFILE /'                              '/
        CHARACTER*6  NAMETAG, NAMTAGL
        CHARACTER*10 DATETAG
        CHARACTER*100 PRTFILE
C
        PARAMETER (MXMN=16)
        PARAMETER (MXREPL=120)
C
        PARAMETER (MXBF=16000 )
C
        PARAMETER (NEMLIM=100)
C
        COMMON /BITBUF/ MAXBYT,IBIT,IBAY(5000),MBYT(32),MBAY(5000,32)
        INTEGER INDEX, IDX
C
        CHARACTER*6    ASURF(10)
        DATA ASURF(1) /'ADPSFC'/        ! LAND SURFACE RECORD NAME
        DATA ASURF(2) /'SFCSHP'/        ! MARINE SURFACE RECORD NAME
        CHARACTER*6    RECGET(20)       ! FOR LIST OF RECORDS TO GET
        CHARACTER*1    IRECDO
C
        INTEGER      RECORDS, RECSREJ, RECSACC, RECREPS, REPORTS
        INTEGER      REPSACC, REPSREJ
C
        CHARACTER*100  CSTRING
        CHARACTER*1    IFILTER
        CHARACTER*8    OBSGET(50)
        CHARACTER*1    IOBSDO
C
        CHARACTER*1    DATEDO
        CHARACTER*1    DATEOK
        CHARACTER*6    RRLEV
C
        CHARACTER*8    NEMLIST(NEMLIM)
        CHARACTER*1    NMDO
        CHARACTER*10   XN
C
        CHARACTER*1    LLDO
        CHARACTER*1    LLWRAP
C
        CHARACTER*1    LLRDO
C
        CHARACTER*1    WMODO
        CHARACTER*5    WMOLIST(100)
        CHARACTER*1    WBBDO
        CHARACTER*2    WBBLIST(100)
C
        CHARACTER*1    IELEVDO
C
        CHARACTER*1    ACK
C
C       NEXT TWO STRING SIZES LIMITED BY BUFR LIBRARY TO 80 CHARACTERS.
C         SEE ROUTINE string.f
C
C       MAXIMUM NUMBER OF PARAMETERS RETURNED BY UFBINT FOR A MNEMONIC REQUEST
C         (OF UP TO 80 CHARACTERS) IS 80 / 5 = 16 (MXMN).  COMMON MNEMONICS ARE
C         USUALLY 4 CHARACTERS, OTHERS CAN BE AS LONG AS 8.  SEE:
C    http://www.emc.ncep.noaa.gov/mmb/data_processing/bufrtab_tableb.htm
C         ONE OR MORE OF THE PARAMETERS MAY BE "REPLICATED"
C
        CHARACTER*80 QIDENT               ! REPORT IDENTIFICATION TABLE B MNEMONICS
        CHARACTER*80 QBPARM               ! BASIC PARAMETER TABLE B MNEMONICS
        REAL*8       R8IDENT(MXMN,MXREPL) ! ARRAY TO RECEIVE DATA REQUESTED IN QIDENT
        REAL*8       R8BPARM(MXMN,MXREPL) ! ARRAY TO RECEIVE DATA REQUESTED IN QBPARM
        REAL*8       R8XPARM(MXMN,MXREPL) ! ARRAY TO RECEIVE DATA REQUESTED IN NEMLIST
C
C       A STRING OF MNEMONICS PROVIDED TO UFBINT CAN NOT INVOLVE MORE THAN ONE
C         "REPLICATION GROUP."  FROM "GUIDE TO WMO TABLE DRIVEN CODE FORMS:"
C           REPLICATION IS THE REPEATING OF A SINGLE PARAMETER OR A GROUP OF
C           PARAMETERS SOME NUMBER OF TIMES, AS IN A TEMP OR PILOT REPORT
C           WITH MANY LEVELS.
C
C       THE BUFRLIB ROUTINE PARUSER WILL COMPLAIN ABOUT INPUT STRING STORE
C         NODES (MNEMONICS), WHEN REPLICATION GETS BROKEN SOMEHOW.
C
        REAL*8 R8CLAT, R8CLON
        REAL*8 R8PRES, R8PMSL, R8WDIR, R8WSPD
        REAL*8 R8TMDB, R8TMDP, R8REHU
C
        REAL*8 R8BIG
        DATA R8BIG / 9999999999.0 /
C
        CHARACTER*8  CSUBSET    ! HAS THE CODED RECORD TYPE AND OBSERVATION TYPE
        CHARACTER*6  RECTYPE    ! UNCODED RECORD TYPE
        CHARACTER*8  OBSTYPE    ! UNCODED OBSERVATION TYPE
        CHARACTER*6  A8RPID
C
        REAL*8       EXTRA(100)         ! FOR EXTRA PARAMETERS (BEYOND THE BASICS)
C
        CHARACTER    ARGV*300, MINUTE*2
C
        REAL LATR(100), LONR(100)
        INTEGER RADR
        INTEGER*4 RECDATE, I8DATE, IBEGDAT, IENDDAT
C
C       DO NOT CHANGE THESE NEXT TWO DECLARATIONS (THE INTEGER AND THE
C         EQUIVALENCE)
C
        INTEGER IBFMSG(MXBF/4), LN, CODE, Y, Z, JJ, IARGC, N
        CHARACTER    CBFMSG*(MXBF)
C
        EQUIVALENCE (CBFMSG(1:4),IBFMSG(1))     ! NEITHER ARE USED ANYWHERE?
C
        CHARACTER*1200 DUMPHED(3)
C
        CHARACTER*1 DOHEAD
C
C ##############################################################################
C #     END OF DECLARATIONS  ############## BEGIN INSTRUCTIONS #################
C ##############################################################################
C
C       THESE TWO STRING SIZES ARE LIMITED BY THE BUFR
C         LIBRARY - SEE ROUTINE string.f
C
        QIDENT(001:025) = 'WMOB WMOS RPID CLAT CLON '
        QIDENT(026:050) = 'SELV YEAR MNTH DAYS HOUR '
C       QIDENT(051:075) = 'MINU CORN                '
        QIDENT(051:075) = 'MINU                     '
        QIDENT(076:080) = '     '
C
        QBPARM(001:025) = '     PRES PMSL ALSE WDIR '
        QBPARM(026:050) = 'WSPD TMDB TMDP REHU HOVI '
        QBPARM(051:075) = 'TP03 TP24                '
        QBPARM(076:080) = '     '
C
C       SET THE HEADER STRINGS (FOR THE DEFAULT MODE - LATER RESET IF
C         NOT IN DEFAULT MODE)
C       2010.07.01 - LET'S NOT SHOW THE RECORD DATE, NOR USE IT TO
C                      SELECT RECORDS.  DO NOT SHOW THE CORRECTION FLAG
C       ================================================================
C       DUMPHED(1)(001:040) = ' REC     RECORD TIME  OBS       REPORT T'
C       DUMPHED(1)(041:080) = 'IME   STATION   LATI-   LONGI-   ELE-   '
C       DUMPHED(1)(081:120) = '     STATION           AIR    DEW-   REL'
C       DUMPHED(1)(121:160) = '    WIND    WIND      HOR     3H PR   24'
C       DUMPHED(1)(161:200) = 'H PR                                    '
C       IHDEND = 167
C       ----------------------------------------------------------------
        DUMPHED(1)(001:028) = ' REC      OBS       REPORT T'
        DUMPHED(1)(029:068) = 'IME   STATION   LATI-   LONGI-   ELE-   '
        DUMPHED(1)(069:108) = '     STATION      ALTIM    AIR    DEW-  '
        DUMPHED(1)(109:148) = ' REL    WIND    WIND      HOR     3H PR '
        DUMPHED(1)(149:188) = '  24H PR                                '
        IHDEND = 155
        DUMPHED(1)(189:200) = '            '
C
        DUMPHED(1)(201:240) = '                                        '
        DUMPHED(1)(241:280) = DUMPHED(1)(201:240)
        DUMPHED(1)(281:360) = DUMPHED(1)(201:280)
        DUMPHED(1)(361:520) = DUMPHED(1)(201:360)
        DUMPHED(1)(521:840) = DUMPHED(1)(201:520)
        DUMPHED(1)(841:1200)= DUMPHED(1)(201:560)
C
        DUMPHED(2)(001:028) = ' TYPE     TYPE      YYYYMMDD'
        DUMPHED(2)(029:068) = 'HHMM  BBSSS     TUDE     TUDE   VATION  '
        DUMPHED(2)(069:108) = '  PR     DSLP      SET     TEMP  POINT  '
        DUMPHED(2)(109:148) = ' HUM    DIR   SPD(M/S)   VIS(M)  (KG/M2)'
        DUMPHED(2)(149:188) = '  (KG/M2)                               '    
        IHDEND = 155
        DUMPHED(2)(189:200) = '            '
C
        DUMPHED(2)(201:240) = '                                        '
        DUMPHED(2)(241:280) = DUMPHED(2)(201:240)
        DUMPHED(2)(281:360) = DUMPHED(2)(201:280)
        DUMPHED(2)(361:520) = DUMPHED(2)(201:360)
        DUMPHED(2)(521:840) = DUMPHED(2)(201:520)
        DUMPHED(2)(841:1200)= DUMPHED(2)(201:560)
C       ================================================================
C
        FILL9  = 999.9
        FILL99 = 999.99
        XN = '          '
C
C       N = IARGC()     ! GNU FORTRAN: NUMBER OF ARGUMENTS PASSED ON THE
C                       !   COMMAND LINE
C
        DO N = 1, 1500
          INFILES(N) = NOFILE
        ENDDO
        PRTFILE(001:040) = '                                        '
        PRTFILE(041:080) = PRTFILE(001:040)
        PRTFILE(081:100) = PRTFILE(001:020)
        INK = 0
C
C ##############################################################################
C #     INITIALIZE SELECTION CONFIGURATION AND SET DEFAULTS                    #
C ##############################################################################
C
        DODIAG = 'n'
        DEFAULT = 'y'           ! WHEN SET TO 'n', PROGRAM WILL GET ONLY THE
C                               !   PARAMETERS SPECIFIED BY THE USER. E.G. JUST
C                               !   ONE, LIKE TEMPERATURE
C-------
        IRECBEG = 1
        IRECEND = 10
        DO N = 1, 20
          RECGET(N) = '        '
        ENDDO
        NREC = 0
        IRECDO = 'n'
C
        OBSGET(1) = '        '  ! INDEX 026   n/a                 BUFR type to get
        IOBSBEG = 1
        IOBSEND = 8
        DO N = 2, 50
          OBSGET(N) = '        '
        ENDDO
        NOBS = 0
        IOBSDO = 'n'
C-------
        IBEGDAT = 99            ! INDEX 031   YEAR_MNTH_DAY_HOUR  beginning date
        IENDDAT = 99            ! INDEX 031   YEAR_MNTH_DAY_HOUR  ending date
        DATEDO = 'n'
C-------
        DO N = 1, NEMLIM
          NEMLIST(N) = '        ' ! INDEX 041   e.g. PMSL           list of extra mnemonics (parameters)
C                       see  http://www.emc.ncep.noaa.gov/mmb/data_processing/bufrtab_tableb.htm
        ENDDO
        NML = 0
        INBEG = 1
        INEND = INBEG + 14
        NMDO = 'n'
C-------
        LATS = -90              ! INDEX 051   LAT                 southern latitude of a lat-lon box
        LATN = 90               ! INDEX 051   LAT                 northern latitude of a lat-lon box
        LONW = -180             ! INDEX 051   LON                 western longitude of a lat-lon box
        LONE = 180              ! INDEX 051   LON                 eastern longitude of a lat-lon box
        LLDO = 'n'
C-------
        RADR = 0                ! INDEX 061   n/a                 radius of all circles, kilometers
        DO N = 1, 100
          LATR(N) = 99.99       ! INDEX 061   LAT                 latitude  for a circle locus
          LONR(N) = 999.99      ! INDEX 061   LON                 longitude for a circle locus
        ENDDO
        NIR = 0
        IRBEG = 1
        IREND = IRBEG + 3
        LLRDO = 'n'
C-------
        DO N = 1, 100
          WMOLIST(N) = '     '  ! INDEX 071   RPID                list of stations (WMO numbers)
          WBBLIST(N) = '  '     ! INDEX 076   RPID                list of stations (WMO numbers)
        ENDDO
        NWMO = 0
        NWBB = 0
        IWBEG = 1
        IWEND = IWBEG + 9
        WMODO = 'n'
        WBBDO = 'n'
C-------
        IELEVL = -1000          ! INDEX 081   SELV                lowest  station elevation, meters
        IELEVH = 12000          ! INDEX 081   SELV                highest station elevation, meters
        IELEVDO = 'n'
C-------
C
C ##############################################################################
C #     OPEN AND READ THE CONFIGURATION FILE                                   #
C ##############################################################################
C
        OPEN (ICUNIT, FILE=CONFILE)
C
C        WRITE (*,*) 'opening configuration file'
C
        IFILTER = 'n'
C
        DO IC = 1, 10000        ! BEGIN  CONFIGURATION FILE READS
C 
C          WRITE (*,*)  'read line ',ic,' from the configuration file'
C
          READ (ICUNIT,8020,IOSTAT=IOS)  CSTRING  ! READ NEXT ENTRY IN CONFIGURATION FILE
8020      FORMAT (A)
          IF (IOSTAT.NE.0)  EXIT
          READ (CSTRING,'(I3)')  INDEX
C
C          WRITE (*,*)  '  just read the index ',index,' off of the line'
C 
          IF (INDEX.EQ.999)  THEN
            EXIT
          ENDIF
          IDX = INDEX / 10
          IF (IDX.EQ. 0)  THEN
            READ (CSTRING,'(4X,A1)')  DODIAG
            CYCLE
          ENDIF
          IF (IDX.EQ. 1)  THEN
C
C           123456789012345678901234567890
C           gdas.adpsfc.t00z.20100323.bufr  < INPUT FILENAME "FORM"
C           gdas.sfcshp.t00z.20100323.bufr  < INPUT FILENAME "FORM"
C
            INK = INK + 1
            IF (INK.LE.1500)  THEN
              READ (CSTRING,'(4X,A30)')  INFILES(INK)
              DO N = 1, 30
                IF (INFILES(INK)(N:N).EQ.' ')  THEN
                  INK = INK - 1
                  CYCLE
                ENDIF
              ENDDO
            ENDIF
            CYCLE
          ENDIF
          IF (IDX.EQ. 2)  THEN
            IF (INDEX.EQ.21)  THEN
              READ (CSTRING,'(4X,10(A6,1X))')
     +        (RECGET(N),N=IRECBEG,IRECEND)
              IF (IRECEND.LT.20)  THEN
                IRECBEG = IRECBEG + 10
                IRECEND = IRECBEG + 9
                IRECDO = 'y'
                IFILTER = 'y'
              ENDIF
            ENDIF
            IF (INDEX.EQ.26)  THEN
              READ (CSTRING,'(4X,8(A8,1X))')
     +        (OBSGET(N),N=IOBSBEG,IOBSEND)
              IF (IOBSEND.LT.48)  THEN
                IOBSBEG = IOBSBEG + 8
                IOBSEND = IOBSBEG + 7
                IOBSDO = 'y'
                IFILTER = 'y'
              ELSE
                IOBSBEG = 49
                IOBSEND = 50
              ENDIF
            ENDIF
            CYCLE
          ENDIF
          IF (IDX.EQ. 3)  THEN
            READ (CSTRING,'(4X,I10,2X,I10)',IOSTAT=IOS) IBEGDAT, IENDDAT  ! YYYYMMDDHH
            IF (IOSTAT.NE.0)  CYCLE
            IF (IBEGDAT.GE.1970010100.AND.IENDDAT.GE.IBEGDAT)  THEN
              DATEDO = 'y'
              IFILTER = 'y'
            ENDIF
            CYCLE
          ENDIF
          IF (IDX.EQ. 4)  THEN
            IF (INDEX.EQ.41.AND.CSTRING(5:5).EQ.'n')  THEN
              DEFAULT = 'n'
C
C             MUST RESET THE HEADER STRINGS
C
        DUMPHED(1)(001:028) = ' REC      OBS       REPORT T'
        DUMPHED(1)(029:068) = 'IME   STATION   LATI-   LONGI-   ELE-   '
        DUMPHED(1)(069:103) = '                                   '
        DUMPHED(1)(104:143) = '                                        '
        DUMPHED(1)(144:183) = '                                        '
        DUMPHED(1)(184:200) = '                 '
C
        DUMPHED(2)(001:028) = ' TYPE     TYPE      YYYYMMDD'
        DUMPHED(2)(029:068) = 'HHMM  BBSSS     TUDE     TUDE   VATION  '
        DUMPHED(2)(069:103) = '                                   '
        DUMPHED(2)(104:143) = '                                        '
        DUMPHED(2)(144:183) = '                                        '
        DUMPHED(2)(184:200) = '                 '
C
              IHDEND =  67
              CYCLE
            ENDIF
            IF (INDEX.EQ.41.AND.CSTRING(5:5).EQ.'y')  THEN
              CYCLE
            ENDIF
            NPT = 0
            DO LK = 5, 79       ! LOOK (SCAN) FOR MNEMONICS -
              IF (CSTRING(LK:LK).NE.' ')  THEN
                NPT = NPT + 1           ! ANOTHER CHARACTER IN THIS MNEMONIC
                IF (NPT.EQ.1)  NML = NML + 1    ! ANOTHER MNEMONIC FOR OUR LIST
                IF (NML.GT.NEMLIM)  THEN
                  CYCLE
                ENDIF
                IF (NPT.LE.8)  THEN     ! ADD THIS CHARACTER TO THE NMLth MNEMONIC
                  NEMLIST(NML)(NPT:NPT) = CSTRING(LK:LK)
                ENDIF
              ELSE
                IF (NPT.GT.0)  THEN
                  I = IHDEND
                  M = NML
                  N = NPT
                  IF (N.LE.9)  THEN       ! PUT THIS MNEMONIC IN THE HEADER
                    DUMPHED(2)(I+1:I+10) = XN(1:10-N)//NEMLIST(M)(1:N)
                  ELSE
                    DUMPHED(2)(I+1:I+10) =             NEMLIST(M)(1:N)
                  ENDIF
                  IHDEND = IHDEND + 12
                  NPT = 0
                ENDIF
              ENDIF
            ENDDO
C
C           IDEALLY WE SHOULD VERIFY THE USER'S MNEMONICS WITH THE
C             NCEP BUFR TABLE
C
            IF (NML.GT.0)  THEN
              NMDO = 'y'
              IFILTER = 'y'
            ENDIF
            CYCLE
          ENDIF
          IF (IDX.EQ. 5)  THEN
            IF (CSTRING(05:10).EQ.'      '.OR.
     +          CSTRING(11:16).EQ.'      '.OR.
     +          CSTRING(17:22).EQ.'      '.OR.
     +          CSTRING(23:28).EQ.'      ')  THEN
              CYCLE
            ENDIF
            READ (CSTRING,'(4X,4I6)',IOSTAT=IOS)  LATS, LATN, LONW, LONE
            IF (IOSTAT.NE.0)  CYCLE
            IF (LATS.NE.-90.OR.LATN.NE.90.
     +       OR.LONW.NE.-180.OR.LONE.NE.180)  THEN
              LLDO = 'y'
              IFILTER = 'y'
            ENDIF
C
C           WHEN THE "WINDOW" INCLUDES THE D.L., WE NEED TO DEAL WITH THE
C             DATE LINE, WHERE, SCANNING EASTWARD, THE SIGN OF THE LONGITUDE
C             FLIPS FROM POSITIVE TO NEGATIVE, I.E. FROM 179.9 TO -179.9
C           FIRST WE NEED TO RECOGNIZE THAT WE ARE DEALING WITH SUCH A FLIP,
C             OR WRAP-AROUND, WHICH OCCURS WHEN LONE < LONW
C
            IF (LONW.LT.LONE)  THEN
              LLWRAP = 'n'
            ELSE
              LLWRAP = 'y'
            ENDIF
            CYCLE
          ENDIF
          IF (IDX.EQ. 6)  THEN
            IF (INDEX.EQ.61.AND.CSTRING(1:4).NE.'   ')  THEN
              READ (CSTRING,'(4X,I3)',IOSTAT=IOS)  RADR
              IF (IOSTAT.NE.0)  CYCLE
              IF (RADR.LT.5.OR.RADR.GT.999)  THEN     ! TOO SMALL OR TOO BIG
                RADR = 0
                CYCLE
              ENDIF
            ENDIF
            IF (INDEX.EQ.62.AND.RADR.NE.0)  THEN
              DO  K = 5, 53, 16
                IF (CSTRING(K  :K+ 7).EQ.'        '.OR.
     +              CSTRING(K+8:K+15).EQ.'        ')  THEN
                  CYCLE
                ENDIF
                READ (CSTRING(K:K+15),'(2F8.2)',IOSTAT=IOS)
     +            XLATR, XLONR
                IF (IOSTAT.NE.0)  CYCLE
                IF (XLATR.GE. -90.AND.XLATR.LE. 90.AND.
     +              XLONR.GE.-180.AND.XLONR.LE.180)  THEN
                  NIR = NIR + 1
                  LATR(NIR) = XLATR
                  LONR(NIR) = XLONR
                  LLRDO = 'y'
                  IFILTER = 'y'
                ENDIF
              ENDDO
              CYCLE
            ENDIF
          ENDIF
          IF (IDX.EQ. 7)  THEN
            IF (INDEX.EQ.71)  THEN
              READ (CSTRING,'(4X,10(A5,1X))')
     +          (WMOLIST(N),N=IWBEG,IWEND)
              IF (IWEND.LE.90)  THEN
                IWBEG = IWBEG + 10
                IWEND = IWBEG + 9
                WMODO = 'y'
                IFILTER = 'y'
              ENDIF
              CYCLE
            ENDIF
            IF (INDEX.EQ.76)  THEN
              READ (CSTRING,'(4X,10(A2,1X))')
     +          (WBBLIST(N),N=IWBEG,IWEND)
              IF (IWEND.LE.90)  THEN
                IWBEG = IWBEG + 10
                IWEND = IWBEG + 9
                WBBDO = 'y'
                IFILTER = 'y'
              ENDIF
              CYCLE
            ENDIF
          ENDIF
          IF (IDX.EQ. 8)  THEN
            IF (CSTRING(05:10).NE.'      '.AND.
     +          CSTRING(11:16).NE.'      ')  THEN
              READ (CSTRING,'(4X,2I6)',IOSTAT=IOS)  IELEVL, IELEVH
              IF (IOSTAT.NE.0)  CYCLE
              IF (IELEVL.GE.-1000.AND.IELEVH.LE.12000)  THEN
                IELEVDO = 'y'
                IFILTER = 'y'
              ENDIF
            ENDIF
            CYCLE
          ENDIF
C
C         IF (IDX.EQ. 9)  THEN
C           CYCLE
C         ENDIF
C
C         IF (IDX.EQ.10)  THEN
C           CYCLE
C         ENDIF
C
        ENDDO                   ! END OF CONFIGURATION FILE READS
C
        CLOSE (ICUNIT )
C ##############################################################################
C
        IF (IRECDO.EQ.'y')  THEN
          DO N = 1, 20
            IF (RECGET(N).EQ.'      ')  THEN
              NREC = N - 1
              EXIT
            ENDIF
          ENDDO
        ELSE
          NREC = 0
        ENDIF
        IF (IOBSDO.EQ.'y')  THEN
          DO N = 1, 50
            IF (OBSGET(N).EQ.'        ')  THEN
              NOBS = N - 1
              EXIT
            ENDIF
          ENDDO
        ELSE
          NOBS = 0
        ENDIF
        IF (WMODO.EQ.'y')  THEN
          DO N = 1, 100
            IF (WMOLIST(N).EQ.'     ')  THEN
              NWMO = N - 1
              EXIT
            ENDIF
          ENDDO
        ELSE
          NWMO = 0
        ENDIF
        IF (WBBDO.EQ.'y')  THEN
          DO N = 1, 100
            IF (WBBLIST(N).EQ.'  ')  THEN
              NWBB = N - 1
              EXIT
            ENDIF
          ENDDO
        ELSE
          NWBB = 0
        ENDIF
C
        IF (WMODO.EQ.'y'.OR.WBBDO.EQ.'y')  THEN
          IF (IOBSDO.EQ.'n')  THEN
            IOBSDO = 'y'
            NOBS = 1
            OBSGET(1) = 'SYNOP   '
C
C           STUFF BEING PUT IN OBSGET(2) AND OBSGET(3) IS JUST
C             FOR THE CONFIGURATION PRINTOUT, AND NOTHING ELSE.
C             I.E., WITH NOBS SET TO 1, FILTERING FOR WMO NUMBERS
C             WORKS ONLY FOR SYNOP OBS TYPE
C
            OBSGET(2) = '  forced'
            IF (WMODO.EQ.'y')  OBSGET(3) = 'by WMODO'
            IF (WBBDO.EQ.'y')  OBSGET(3) = 'by WBBDO'
            OBSGET(4) = '        '
          ENDIF
        ENDIF
C
        WRITE (*,*)
        WRITE (*,*)  'CONFIGURATION FILE HAS BEEN ACCEPTED:'
C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C +     PRINT THE CONFIGURATION TO THE USER'S SCREEN                           +
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
        WRITE (*,9070)
     +      IRECDO, NREC, RECGET(1),  RECGET(2),  RECGET(3),  RECGET(4),
     +      IOBSDO, NOBS, OBSGET(1),  OBSGET(2),  OBSGET(3),  OBSGET(4),
     +      DATEDO, IBEGDAT, IENDDAT,
     +      NMDO,  NML, (NEMLIST(NM),NM=1,20),
     +      LLDO, LATS, LATN, LONW, LONE,
     +      LLRDO, RADR,
     +      NIR, LATR(1), LONR(1), LATR(2), LONR(2), LATR(3), LONR(3),
     +      IELEVDO, IELEVL, IELEVH,
     +      WMODO, NWMO, (WMOLIST(NZ),NZ=1,20),
     +      WBBDO, NWBB, (WBBLIST(NZ),NZ=1,10)
C
        IF (DODIAG.EQ.'y'.OR.DODIAG.EQ.'x')  THEN
C
C ##############################################################################
C #       OPEN THE DIAGNOSTIC FILE, WHEN NEEDED                                #
C ##############################################################################
C
          OPEN (IDUNIT, FILE=DIGFILE)
          WRITE (IDUNIT,9064)  DIGFILE
9064      FORMAT (/,1X,'DIAGNOSTIC FILE ',A32,' OPENED')
C
          WRITE (IDUNIT,9065)  CONFILE
9065      FORMAT (/,1X,'CONFIGURATION FILE ',A32,' OPENED',
     +            /,3X,'CHOICES FOLLOW')
C
          WRITE (*,*)
          WRITE (*,*)  'DIAGNOSTIC FILE HAS BEEN OPENED'
C
        ENDIF
C
C ##############################################################################
C
        IF (DODIAG.EQ.'y')  THEN
C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C +       PRINT THE CONFIGURATION IN THE DIAGNOSTIC FILE                       +
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
          WRITE (IDUNIT,9070)
     +      IRECDO, NREC, RECGET(1),  RECGET(2),  RECGET(3),  RECGET(4),
     +      IOBSDO, NOBS, OBSGET(1),  OBSGET(2),  OBSGET(3),  OBSGET(4),
     +      DATEDO, IBEGDAT, IENDDAT,
     +      NMDO,  NML, (NEMLIST(NM),NM=1,20),
     +      LLDO, LATS, LATN, LONW, LONE,
     +      LLRDO, RADR,
     +      NIR, LATR(1), LONR(1), LATR(2), LONR(2), LATR(3), LONR(3),
     +      IELEVDO, IELEVL, IELEVH,
     +      WMODO, NWMO, (WMOLIST(NZ),NZ=1,20),
     +      WBBDO, NWBB, (WBBLIST(NZ),NZ=1,10)
9070      FORMAT (/,
     +      ' -------------------------------------------------------',/
     +      '    FILTER   USE  CRITERIA',/
     +      ' RECORD TYPE  ',A1,'   RECGET  (1- 4 OF',
     +            I3,')', 4(2X,A6),/
     +      '    OBS TYPE  ',A1,'   OBSGET  (1- 4 OF',
     +            I3,')',4(2X,A8),/
     +      '   DATE/TIME  ',A1,'   IBEGDAT ',I10,'  IENDDAT ',I10,/
     +      '   MNEMONICS  ',A1,'   NEMLIST (1-20 OF',
     +            I3,')',10(2X,A8),/35X,10(2X,A8),/
     +      '   LAT-LON    ',A1,'   LATS',I4,'  LATN',I4,
     +                          '  LONW',I5,'  LONE',I5,/
     +      '   CIRCLES    ',A1,'   RADR ',I6,/
     +      '                  LATR, LONR (1- 3 OF',
     +                          I3,') ',3(1X,F6.2,', ',F7.2,1X),/
     +      '  ELEVATION   ',A1,'   IELEVL ',I5,'  IELEVH ',I5,/
     +      'WMO STATIONS  ',A1,'   WMOLIST (1-20 OF',I3,') ',
     +                          10(A5,1x),/36X,10(A5,1x),/
     +      'WMO BLOCKS    ',A1,'   WBBLIST (1-10 OF',I3,') ',
     +                          10(A2,1x)/,
     +      ' -------------------------------------------------------')
        ENDIF
C
C ##############################################################################
C                                                                              #
C       TOP OF MAIN LOOP ON INPUT FILES                                        #
C                                                                              #
        KNK = 0                                                                #
        DOFILS: DO              ! LOOP TO READ BUFR DATA FILES                 #
C                                                                              #
C ##############################################################################
C
        KNK = KNK + 1
        IF (KNK.GT.INK)  EXIT DOFILS
C
        INFILE = INFILES(KNK)
C
C       123456789012345678901234567890
C       gdas.adpsfc.t00z.20100323.bufr
C       gdas.sfcshp.t00z.20100323.bufr
C
C ##############################################################################
C #     OPEN THE BUFR DATA FILE, WHICH IS PACKED BINARY                        #
C ##############################################################################
C
        OPEN (IIUNIT, FILE=INFILE, FORM='UNFORMATTED' )
        IF (DODIAG.EQ.'y')  THEN
          WRITE (IDUNIT,9090)  KNK, INK, INFILE
9090      FORMAT (/,1X,'BUFR DATA INPUT FILE ',I5,' OF ',I5,
     +            /,A30,' OPENED')
        ENDIF
        WRITE (*,*)
        WRITE (*,*)  'BUFR DATA FILE ',KNK,' OF ',INK
        WRITE (*,*)  '    ',INFILE,' OPENED'
C
C ????? ASSOCIATE THE TABLES FILE WITH THE MESSAGES FILE, AND IDENTIFY
C ????? THE LATTER TO THE BUFRLIB SOFTWARE.
C ?????
        CALL OPENBF  ( IIUNIT, 'IN', 11 )
C ?????
C
        NAMTAGL(01:06) = INFILE(06:11)
        CALL LOW2UP (NAMTAGL,NAMETAG,6)
        DATETAG(01:08) = INFILE(18:25)
        DATETAG(09:10) = INFILE(14:15)
        PRTFILE = PARLEL//DIROUT//NAMETAG//'.'//DATETAG//'print'
C
C              123456789012345678901234567890
C       DIROUT/ADPSFC.2010032300print
C       DIROUT/SFCSHP.2010032300print
C
C ##############################################################################
C #     OPEN BUFR PRINTOUT ("DUMP") FILE                                       #
C ##############################################################################
C
        OPEN(IPUNIT,FILE=PRTFILE,STATUS='UNKNOWN',FORM='FORMATTED')
        IF (DODIAG.EQ.'y')  THEN
          WRITE (IDUNIT,9100)  PRTFILE
9100      FORMAT (/,1X,'BUFR REPORT PRINTOUT FILE',
     +      /,1X,A100,'OPENED')
        ENDIF
        WRITE (*,*)
        WRITE (*,*)  'BUFR REPORT PRINTOUT FILE'
        WRITE (*,*)  PRTFILE,' OPENED'
C
C       SPECIFY THAT WE WOULD LIKE ROUTINE READNS TO RETURN RECDATE VALUES WITH
C         10 DIGITS (I.E. YYYYMMDDHH ), WHICH IS THE MAXIMUM BECAUSE MINUTES ARE
C         NOT AVAILABLE.
C
        CALL DATELEN (10)   ! IS THIS IN THE BUFR LIBRARY??
C
C       OPEN YOUR OWN ("EXTERNAL") BUFR TABLES FILE, AS AN ALTERNATIVE TO THE
C         TABLE IN THE DATA FILES.
C
C       OPEN (IXUNIT, FILE='BUFRTAB.EXAMPLE' )
C
        LN = 0          ! NEVER REDEFINED, UNLESS IT'S THROUGH THE EQIVALENCE
C
C ##### INITIALIZE BUFFER RECORD AND BUFR REPORT COUNTERS
C
        RECORDS = 0     ! TOTAL BUFR RECORDS FOUND (READNS CALLS)
        RECSREJ = 0     ! TOTAL BUFR RECORDS REJECTED
C
        RECREPS = 0     ! TOTAL BUFR REPORTS IN CURRENT RECORD
        REPORTS = 0     ! TOTAL BUFR REPORTS OPENED
        REPSACC = 0     ! TOTAL BUFR REPORTS ACCEPTED
        REPSREJ = 0     ! TOTAL BUFR REPORTS REJECTED
C
C ##############################################################################
C #     LOOP TO READ BUFR RECORDS FROM THE DATA FILE                           #
C ##############################################################################
C
        DORECS: DO                      ! LOOP TO READ BUFR RECORDS (MESSAGES)
C
C         READ THE NEXT BUFR MESSAGE ("RECORD") FROM THE FILE
C
          CALL READNS(IIUNIT,CSUBSET,RECDATE,ISTATUS)
C
          IF  (ISTATUS.NE. 0 )  THEN    ! END OF DATA FILE
            CALL CLOSBF (IIUNIT)
            CLOSE (IIUNIT)
            EXIT DORECS         ! GO PRINT STATISTICS FOR THIS FILE'S PROCESSING
          ENDIF
C
          CODE = IUPBS1(MBAY,33)
C
          RECORDS = RECORDS + 1
          MODREC = MOD(RECORDS,500)
          IF (DODIAG.EQ.'y')  THEN
            IF (RECORDS.LE.50.OR.MODREC.EQ.1)  THEN
              WRITE (IDUNIT,9120)  RECORDS, RECDATE, CSUBSET, CODE
9120          FORMAT (/,1X,111('#'),
     +          /,1X,'BUFR RECORD ',I8,' WITH RECDATE ',I10,
     +          ' OPENED,  SUBSET ',A8,'  CODE',I8)
            ENDIF
          ENDIF
C
C         THE RETURNED RECDATE (DAY/TIME) AND MAYBE CSUBSET APPLY TO
C           ALL BUFR REPORTS IN THE RECORD?
C
          CALL GETRO (CSUBSET,RECTYPE,OBSTYPE)
C
          IF (IRECDO.EQ.'y')  THEN
            CALL CKREC (RECORDS,RECTYPE,RECGET,NREC,
     +        IDUNIT,DODIAG,ACK)
            IF (ACK.EQ.'n')  THEN
              RECSREJ = RECSREJ + 1
              CYCLE DORECS              ! REJECT UNINTERESTING RECORD
            ENDIF                       !          (RECORD TYPE)
          ENDIF
C
C ---------
          IF (IOBSDO.EQ.'y')  THEN
            CALL CKOBS (RECORDS,OBSTYPE,OBSGET,NOBS,
     +        IDUNIT,DODIAG,ACK)
            IF (ACK.EQ.'n')  THEN
              RECSREJ = RECSREJ + 1
              CYCLE DORECS              ! REJECT UNINTERESTING RECORD
            ENDIF                       !          (OBSERVATION TYPE)
          ENDIF
C
C ---------
          IF (DATEDO.EQ.'y')  THEN
            RRLEV = 'RECORD'
            CALL CKDATE (RRLEV,RECORDS,RECDATE,IBEGDAT,IENDDAT,
     +        IDUNIT,DODIAG,ACK)
            IF (ACK.EQ.'n')  THEN
              RECSREJ = RECSREJ + 1
              CYCLE DORECS              ! REJECT UNINTERESTING RECORD
            ENDIF                       !          (DATE/TIME)
          ENDIF
C
C ---------
          IF (DODIAG.EQ.'y')  THEN
            WRITE (IDUNIT,9977)  RECORDS
9977        FORMAT (9X,'RECORD ',I9,', START MAIN UFBINT LOOP')
          ENDIF
C
C ##############################################################################
C #       LOOP TO READ BUFR REPORTS FROM THE RECORD, AND DO THE FILTERING      #
C ##############################################################################
C
          DOREPS: DO                            ! LOOP TO READ BUFR REPORTS (SUB-MESSAGES)
C
C           AT THIS POINT, WE HAVE A BUFR MESSAGE ("RECORD") OPEN WITHIN THE
C             INTERNAL ARRAYS OF BUFRLIB.  NEXT GO THROUGH ITS CONTENTS, WHICH
C             WILL INCLUDE ONE OR MORE SUB-MESSAGES ("REPORTS")
C
C           SUBROUTINE UFBINT EXTRACTS THE DESIRED PARAMETERS FROM A REPORT, AS
C             REQUESTED BY THE MNEMONICS GIVEN IN THE QIDENT, QBPARM AND
C             NEMLIST STRINGS.  THE "PATTERN" OF MNEMONICS MUST RESPECT SOME
C             GROUPING OR "REPLICATION" RULES.  THESE RULES GET COMPLICATED
C             BEYOND THE BASICS, SO THE OPTIONAL EXTRA (NEMLIST) PARAMETERS
C             ARE EXTRACTED ONE AT A TIME.  THAT GETS SLOW.
C
C               CALL UFBINT (IIUNIT,R8IDENT,MXMN,MXREPL,NREPL,QIDENT)
C               CALL UFBINT (IIUNIT,R8BPARM,MXMN,MXREPL,NREPL,QBPARM)
C               CALL UFBINT (IIUNIT,R8XPARM,MXMN,MXREPL,NREPL,QXPARM)
C
C           THE DESIRED PARAMETERS ARE RETURNED IN THE R8IDENT, R8BPARM AND
C             R8XPARM ARRAYS.  THESE WILL BE MOVED TO VARIABLES WHOSE NAMES HAVE
C             THE FORM X8NEMO, WHERE X CAN BE I (INTEGER), R (REAL), OR A
C             (CHARACTER).  THE 'NEMO' IS THE MNEMONIC (TRUNCATED TO FOUR
C             CHARACTERS WHEN NECESSARY) THAT WAS USED IN THE QIDENT, QBPARM,
C             OR NEMLIST STRINGS TO TELL UFBINT WHAT VARIABLES TO GET.
C
C           UFBINT RETURNS NREPL REPLICATIONS (PERHAPS LEVELS) OF A MAXIMUM OF MXREPL
C           UFBINT RETURNS UP TO MXMN PARAMETERS (AT EACH NREPL), CORRESPONDING TO
C             THE MAXIMUM NUMBER OF MNEMONICS IN QIDENT
C
C           THE MAXIMUM NUMBER OF PARAMETERS RETURNED BY UFBINT FOR A MNEMONIC
C             REQUEST STRING OF UP TO 80 CHARACTERS IS  80 / 5 = 16 (MXMN)
C             THERE CAN BE UP TO MXREPL REPLICATIONS
C
C ----------
C           (RE)INITIALIZE PARAMETER ARRAYS FOR THIS NEXT REPORT
C
            DO  MZ = 1, MXMN
                R8IDENT(MZ,1)  = FILL9
              DO  LZ = 1, MXREPL
                R8BPARM(MZ,LZ) = FILL9
                R8XPARM(MZ,LZ) = FILL9
              ENDDO
            ENDDO
C
C ++++++++++
C           THE FOLLOWING UFBINT CALL GETS A REPORT'S IDENTIFICATION.  THIS
C             INCLUDES STATION NUMBER OR CALL SIGN, LOCATION, ELEVATION, DATE
C             AND TIME.
C           MXREPL HAS BEEN SET TO ONE BECAUSE THERE SHOULD NOT BE ANY
C             REPLICATION OF ID PARAMETERS.
C ++++++++++
C
            CALL UFBINT (IIUNIT,R8IDENT,MXMN,MXREPL,NREPL,QIDENT)
C
            REPORTS = REPORTS + 1
            RECREPS = RECREPS + 1
            IDUMP = MOD(REPORTS,500)
C
            Z = 1
C
C           TOSS, AS SOON AS POSSIBLE, RECORDS WHICH ARE MISSING ABSOLUTELY
C             ESSENTIAL INFORMATION.  THIS AVOIDS UNNECESSARY PROCESSING
C             OF SUBSEQUENT INFORMATION IN A REPORT
C
            DO  ITOSS = 4, 10
              IF (ITOSS.EQ.6)  CYCLE
C
C             BAD LATITUDE, LONGITUDE, YEAR, MONTH, DAY, HOUR
C                 4         5          7     8      9    10
C
              IF (R8IDENT(ITOSS,Z).GT.R8BIG)  GO TO 290 ! MISSING VALUE
            ENDDO
C
            R8CLAT = R8IDENT(4,Z)
            R8CLON = R8IDENT(5,Z)
C
            IF (LLDO.EQ.'y')  THEN
              WRITE (IDUNIT,9987)  R8CLAT,R8CLON,LATS,LATN,LONW,LONE
9987          FORMAT (1X,'CALLING R8CLAT,R8CLON,LATS,LATN,LONW,LONE ',/
     +                1X,F7.2,F7.2,4I5)
              CALL CKLL (RECORDS,RECREPS,R8CLAT,R8CLON,LATS,LATN,
     +                   LONW,LONE,LLWRAP,IDUNIT,DODIAG,ACK)
              IF (ACK.EQ.'n')  GO TO 290        ! REJECT UNINTERESTING REPORT
            ENDIF
            IF (LLRDO.EQ.'y')  THEN
              CALL CKRAD (RECORDS,RECREPS,R8CLAT,R8CLON,RADR,LATR,LONR,
     +                   NIR,IDUNIT,DODIAG,ACK)
              IF (ACK.EQ.'n')  GO TO 290        ! REJECT UNINTERESTING REPORT
            ENDIF
C
C           IF (R8IDENT(1,Z).GT.R8BIG)  R8IDENT(1,Z) = 99.
C           I8WMOB = R8IDENT(1,Z)               ! DO NOT NEED THIS
C
C           IF (R8IDENT(2).GT.R8BIG)  R8IDENT(2,Z) = 999.
C           I8WMOS = R8IDENT(2,Z)               ! DO NOT NEED THIS
C
            WRITE (A8RPID,9125)  R8IDENT(3,Z)     ! R8IDENT(3,Z) IS CHARACTER
9125        FORMAT (A6)
C
            IF (WMODO.EQ.'y')  THEN
              CALL CKWMO (RECORDS,RECREPS,A8RPID,WMOLIST,NWMO,
     +                  IDUNIT,DODIAG,ACK)
              IF (ACK.EQ.'n')  THEN
                GO TO 290                       ! REJECT UNINTERESTING REPORT
              ENDIF
            ENDIF
            IF (WBBDO.EQ.'y')  THEN
              CALL CKWBB (RECORDS,RECREPS,A8RPID,WBBLIST,NWBB,
     +                  IDUNIT,DODIAG,ACK)
              IF (ACK.EQ.'n')  GO TO 290        ! REJECT UNINTERESTING REPORT
            ENDIF
C
            IF (R8IDENT(6,Z).GT.R8BIG)  R8IDENT(6,Z)   = 99999.
            I8SELV = R8IDENT(6,Z)
C
            IF (IELEVDO.EQ.'y')  THEN
              CALL CKELEV (RECORDS,RECREPS,I8SELV,IELEVL,IELEVH,
     +          IDUNIT,DODIAG,ACK)
              IF (ACK.EQ.'n')  GO TO 290        ! REJECT UNINTERESTING REPORT
            ENDIF
C
            I8YEAR = R8IDENT(7,Z)
            I8MNTH = R8IDENT(8,Z)
            I8DAYS = R8IDENT(9,Z)
            I8HOUR = R8IDENT(10,Z)
            I8DATE = I8YEAR*1000000 + I8MNTH*10000 + I8DAYS*100 + I8HOUR
C
C           IF (DATEDO.EQ.'y')  THEN
C             RRLEV = 'REPORT'
C             CALL CKDATE (RRLEV,REPORTS,I8DATE,IBEGDAT,IENDDAT,
C    +          IDUNIT,DODIAG,ACK)
C             IF (ACK.EQ.'n')  GO TO 290        ! REJECT UNINTERESTING REPORT
C           ENDIF
C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C +         DONE FILTERING                                                     +
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
            REPSACC = REPSACC + 1
C
            IF (R8IDENT(11,Z).GT.R8BIG)  R8IDENT(11,Z)   = 99.
            I8MINU = R8IDENT(11,Z)
C
C       2010.07.01 - LET'S NOT RETRIEVE OR SHOW THE CORRECTION FLAG
C
C           IF (R8IDENT(12,Z).GT.R8BIG)  R8IDENT(12,Z)   = 99.
C           I8CORN = R8IDENT(12,Z)
C
C ++++++++++
C           THE FOLLOWING UFBINT CALL GETS A REPORT'S BASIC METEOROLOGICAL
C             PARAMETERS - WHEN A USER HAS NOT TURNED THIS DEFAULT OFF.
C
C           MXREPL IS THE MAXIMUM (ESTIMATED) NUMBER OF REPLICATIONS WHICH
C             MIGHT BE RETURNED, WHILE NREPL IS THE NUMBER RETURNED.
C ++++++++++
C
            IF (DEFAULT.EQ.'y')  THEN
              CALL UFBINT (IIUNIT,R8BPARM,MXMN,MXREPL,NREPL,QBPARM)
C
              IF (NREPL.GT.1)  STOP 99991
              IF (R8BPARM(1,NREPL).GT.R8BIG)   R8BPARM(1,NREPL) = 999990.0
              R8PRES = R8BPARM(1,NREPL)
C
              IF (R8BPARM(2,NREPL).GT.R8BIG)   R8BPARM(2,NREPL) = 999990.0
              R8PMSL = R8BPARM(2,NREPL)
C
              IF (R8BPARM(3,NREPL).GT.R8BIG)   R8BPARM(3,NREPL) = 999990.0
              R8ALSE = R8BPARM(3,NREPL)
C
              IF (R8BPARM(4,NREPL).GT.R8BIG)   R8BPARM(4,NREPL)  = FILL9
              R8WDIR = R8BPARM(4,NREPL)
C
              IF (R8BPARM(5,NREPL).GT.R8BIG)   R8BPARM(5,NREPL)  = FILL9
              R8WSPD = R8BPARM(5,NREPL)
C
              IF (R8BPARM(6,NREPL).GT.R8BIG)   R8BPARM(6,NREPL)  = FILL9
              R8TMDB = R8BPARM(6,NREPL)
C
              IF (R8BPARM(7,NREPL).GT.R8BIG)   R8BPARM(7,NREPL)  = FILL9
              R8TMDP = R8BPARM(7,NREPL)
C
              IF (R8BPARM(8,NREPL).GT.R8BIG)   R8BPARM(8,NREPL)  = FILL9
              R8REHU = R8BPARM(8,NREPL)
C
              IF (R8BPARM(9,NREPL).GT.R8BIG)   R8BPARM(9,NREPL)  = FILL9
              R8HOVI = R8BPARM(9,NREPL)
C
              IF (R8BPARM(10,NREPL).GT.R8BIG)  R8BPARM(10,NREPL) = FILL9
              R8TP03 = R8BPARM(10,NREPL)
C
              IF (R8BPARM(11,NREPL).GT.R8BIG)  R8BPARM(11,NREPL) = FILL9
              R8TP24 = R8BPARM(11,NREPL)
            ENDIF
C
C ++++++++++
C           THE FOLLOWING LOOP ON UFBINT CALLS GETS A SELECTION OF EXTRA
C             METEOROLOGICAL PARAMETERS, WHEN SPECIFIED BY THE USER (WHEN
C             NML .NE. 0)
C           WHEN DEFAULT = 'n' THIS IS THE ONLY WAY TO GET ANY PARAMETERS.
C           MXREPL IS THE MAXIMUM (ESTIMATED) NUMBER OF REPLICATIONS WHICH
C             MIGHT BE RETURNED, WHILE NREPL IS THE NUMBER RETURNED.
C ++++++++++
C
            IF (NML.GT.0)  THEN
              DO NX = 1, NML
                CALL UFBINT (IIUNIT,R8XPARM,MXMN,MXREPL,NREPL,
     +            NEMLIST(NX))
                IF (NREPL.GT.1)  STOP 99992
C
C               NEMLIST(NX) HAS THE MNEMONICS - NML OF THEM
C
C               NREPL > 1  WHEN MORE THAN ONE LEVEL - FOR UPPER AIR DATA
C               NREPL = 1  SHOULD BE ONLY ONE LEVEL FOR SURFACE DATA
C
                IF (R8XPARM(1,NREPL).GT.R8BIG)  R8XPARM(1,NREPL)= FILL99
                EXTRA(NX) = R8XPARM(1,NREPL)
                IF (DODIAG.EQ.'y')  THEN
                  WRITE (IDUNIT,2222)  NX, NML,
     +              R8XPARM(1,NREPL), EXTRA(NX)
2222              FORMAT (1X,'NX, NML, R8XPARM(1,NREPL), EXTRA(NX)',2I4,
     +              2F15.6)
                ENDIF
              ENDDO
            ENDIF
C
            IRECDAT = RECDATE
            WRITE (MINUTE,FMT='(I2)') I8MINU
            DO K = 1, 2
              IF (MINUTE(K:K).EQ.' ') THEN
                MINUTE(K:K) = '0'
              ENDIF
            ENDDO
            IF (DEFAULT.EQ.'y')  THEN
              R8PRES = R8PRES / 100.
              R8PMSL = R8PMSL / 100.
              R8ALSE = R8ALSE / 100.
              IF (R8TMDB.NE.FILL9) THEN
                R8TMDB = R8TMDB - 273.16
              ENDIF
              IF (R8TMDP.NE.FILL9) THEN
                R8TMDP = R8TMDP - 273.16
              ENDIF
            ENDIF
C
            IF (MOD(REPSACC,30).EQ.1)  THEN
C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C +           PRINT A HEADER EVERY 30 REPORTS                                  +
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
              WRITE (IPUNIT,9270)  DUMPHED(1)(001:IHDEND),
     +                             DUMPHED(2)(001:IHDEND)
9270          FORMAT (/a/a)
            ENDIF
C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C +         PRINT THIS REPORT                                                  +
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C           2010.07.01 - LET'S NOT RETRIEVE OR SHOW THE RECORD DATE
C
            IF (DEFAULT.EQ.'y')  THEN
              WRITE (IPUNIT,9280)
C    &        RECTYPE, IRECDAT, OBSTYPE,
     &        RECTYPE,          OBSTYPE,
     &        I8DATE, MINUTE, A8RPID, R8CLAT, R8CLON, I8SELV,
C    &        I8CORN, R8PRES, R8PMSL,
     &                R8PRES, R8PMSL, R8ALSE,
     &        R8TMDB, R8TMDP, R8REHU, R8WDIR, R8WSPD, R8HOVI,
     &        R8TP03, R8TP24,
     &        (EXTRA(NX),NX=1,NML)
9280          FORMAT (
C    +        1X,A6,2X,I10,3X,A8,
     +        1X,A6,1X,    2X,A8,
     +        2X,I10,A2,2X,A6,2X,F7.2,2X,F7.2,2X,I5,
C    +        2X,I3,2X,F7.1,2X,F7.1,
     +              2X,F7.1,2X,F7.1,2X,F7.1,
     +        2X,F5.1,2X,F5.1,2X,F5.1,2X,F6.1,2X,F6.1,2X,F7.1,
     +        2X,F7.1,2X,F7.1,2X,
     +        100F12.2)
            ENDIF
C
            IF (DEFAULT.EQ.'n')  THEN
              WRITE (IPUNIT,9285)
C    &        RECTYPE, IRECDAT, OBSTYPE,
     &        RECTYPE,          OBSTYPE,
     &        I8DATE, MINUTE, A8RPID, R8CLAT, R8CLON, I8SELV,
C    &        I8CORN, R8PRES, R8PMSL,
C    &                R8PRES, R8PMSL,
C    &        R8TMDB, R8TMDP, R8REHU, R8WDIR, R8WSPD, R8HOVI,
C    &        R8TP03, R8TP24,
     &        (EXTRA(NX),NX=1,NML)
9285          FORMAT (
C    +        1X,A6,2X,I10,3X,A8,
     +        1X,A6,1X,    2X,A8,
     +        2X,I10,A2,2X,A6,2X,F7.2,2X,F7.2,2X,I5,
C    +        2X,I3,2X,F7.1,2X,F7.1,
C    +              2X,F7.1,2X,F7.1,
C    +        2X,F5.1,2X,F5.1,2X,F5.1,2X,F6.1,2X,F6.1,2X,F7.1,
C    +        2X,F7.1,2X,F7.1,2X,
     +        100F12.2)
            ENDIF
C
            GO TO 300
 290        CONTINUE
C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C +         WE HAVE SKIPPED DOWN TO HERE TO REJECT A REPORT                    +
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
            REPSREJ = REPSREJ + 1
C
 300        CONTINUE
C
C           SEE IF THERE IS ANOTHER SUB-MESSAGE ("REPORT") IN THIS MESSAGE ("RECORD")
C             NOTE THAT IN THE BUFR LIBRARY COMMENTS, NCEP USES THE TERM "SUBSET"
C             INSTEAD OF SUB-MESSAGE - CLEAR AS MUD, EH?
C
            CALL READSB  ( IIUNIT, IERRSB )
C
            IF  (IERRSB.NE.0)  THEN
              EXIT DOREPS
            ENDIF
          ENDDO DOREPS          ! END OF READING A BUFR REPORT (SUB-MESSAGE)
          RECREPS = 0
C
        ENDDO DORECS            ! END OF READING A BUFR RECORD (MESSAGE)
C
        IF (DODIAG.EQ.'y')  THEN
          WRITE(IDUNIT,9800)
9800      FORMAT (//,' ************* INPUT FILE DONE ')
        ENDIF
        RECSACC = RECORDS - RECSREJ
C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C +     PRINT THE CONFIGURATION AT THE END OF THE PRINTOUT ("DUMP") FILE       +
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
        WRITE (IPUNIT,9880)  CONFILE
9880    FORMAT (/,1X,'CONFIGURATION FILE ',A32,
     +          /,3X,'CHOICES FOLLOW')
        WRITE (IPUNIT,9070)
     +      IRECDO, NREC, RECGET(1),  RECGET(2),  RECGET(3),  RECGET(4),
     +      IOBSDO, NOBS, OBSGET(1),  OBSGET(2),  OBSGET(3),  OBSGET(4),
     +      DATEDO, IBEGDAT, IENDDAT,
     +      NMDO,  NML, (NEMLIST(NM),NM=1,20),
     +      LLDO, LATS, LATN, LONW, LONE,
     +      LLRDO, RADR,
     +      NIR, LATR(1), LONR(1), LATR(2), LONR(2), LATR(3), LONR(3),
     +      IELEVDO, IELEVL, IELEVH,
     +      WMODO, NWMO, (WMOLIST(NZ),NZ=1,20),
     +      WBBDO, NWBB, (WBBLIST(NZ),NZ=1,10)
C
        WRITE (IDUNIT,9890)  INFILE, RECORDS, RECSREJ, RECSACC,
     +     REPORTS, REPSACC, REPSREJ
9890    FORMAT (/,1X,'BUFR EXTRACTION STATISTICS',
     +            1X,'FOR FILE INFILE  ',A30,/,
     +            5X,'RECORDS    FOUND ',I12,/,
     +            5X,'RECORDS REJECTED ',I12,/,
     +            5X,'RECORDS ACCEPTED ',I12,//,
     +            5X,'REPORTS   OPENED ',I12,
     +                  ' (FROM ACCEPTED RECORDS)',/
     +           ,5X,'REPORTS ACCEPTED ',I12,/,
     +            5X,'REPORTS REJECTED ',I12)
C
        WRITE (IPUNIT,9890)  INFILE, RECORDS, RECSREJ, RECSACC,
     +     REPORTS, REPSACC, REPSREJ
        CLOSE   ( IPUNIT )
C
C ##############################################################################
C
        ENDDO DOFILS            ! END OF READING A BUFR DATA FILE
C
C ##############################################################################
C
        WRITE (*,*)
        WRITE (*,*)  'ALL ',INK,' BUFR DATA FILES PROCESSED'
        WRITE (*,*)
C
        WRITE(IDUNIT,9900)  INK
9900    FORMAT (//,' **** ALL ',I5,' BUFR DATA FILES PROCESSED *****',
     +          //,' ************ DONE ************')
        STOP 99999
        END
C
C       ########################################################################
C
        SUBROUTINE GETRO (CSUBSET,RECTYPE,OBSTYPE)
C
C       GET AND MAP RECORD TYPE AND OBSERVATION (REPORT) TYPE
C
C       CSUBSET--   see  http://www.emc.ncep.noaa.gov/mmb/data_processing/data_dumping.doc/table_1.htm
C
        CHARACTER*8  CSUBSET    ! HAS THE CODED RECORD TYPE AND OBSERVATION TYPE
        CHARACTER*6  RECTYPE    ! UNCODED RECORD TYPE
        CHARACTER*8  OBSTYPE    ! UNCODED OBSERVATION TYPE
C
        CHARACTER*6    ASURF(10)
        DATA ASURF(1)  /'ADPSFC'/    ! SURFACE LAND
        DATA ASURF(2)  /'SFCSHP'/    ! SURFACE SEA
C       DATA ASURF(3)  /'      '/    ! AUPAF(3) IS 'ADPUPA'
C       DATA ASURF(4)  /'      '/    ! AUPAF(4) IS 'SATSDG'
C       DATA ASURF(5)  /'      '/    ! AUPAF(5) IS 'AIRCFT'
C       DATA ASURF(6)  /'      '/    ! AUPAF(6) IS 'SATSGL'
C       DATA ASURF(7)  /'      '/
C       DATA ASURF(8)  /'      '/
C       DATA ASURF(9)  /'      '/
C       DATA ASURF(10) /'      '/
C
        CHARACTER*8 OBSTYP(10,200)
        DATA OBSTYP /2000*'        '/
C
        SAVE
C
C       OBSTYP(1,000) = 'SYNOPR  '  !  SYNOPTIC RESTRICTED (WMO40)
        OBSTYP(1,001) = 'SYNOP   '  !  FIXED MANUAL AND AUTOMATIC
        OBSTYP(1,002) = 'SYNOPM  '  !  MOBILE MANUAL AND AUTOMATIC
        OBSTYP(1,003) = '        '  !
        OBSTYP(1,004) = '        '  !
        OBSTYP(1,005) = '        '  !
        OBSTYP(1,006) = '        '  !
        OBSTYP(1,007) = 'METAR   '  !  AVIATION METAR
        OBSTYP(1,008) = 'PRFLRS  '  !  PROFILERS NOAA (NPN) AND MULTI-AGENCY (MAP)
        OBSTYP(1,009) = '        '  !
        OBSTYP(1,010) = 'SHEFCM  '  !  MISCELLANEOUS (ORIGINALLY IN SHEF) 
        OBSTYP(1,011) = 'SHEFF   '  !  AFOS - PRECIP (ORIGINALLY IN SHEF)
        OBSTYP(1,012) = 'SCD     '  !  AVIATION SUPPL AND CLIMAT (SCD)
C
        OBSTYP(2,001) = 'SHIPS   '  !  SHIP MANUAL AND AUTOMATIC, RESTRICTED (WMO40)
        OBSTYP(2,002) = 'DBUOY   '  !  MOORED OR DRIFTING BUOY, F-18
        OBSTYP(2,003) = 'MBUOY   '  !  MOORED BUOY. F-13
        OBSTYP(2,004) = 'LCMAN   '  !  CMAN STATION
        OBSTYP(2,005) = 'TIDEG   '  !  TIDE GAUGE REPORTS (ORIGINALLY IN CREX)
        OBSTYP(2,006) = 'SLPBG   '  !  SEA LEVEL PRESSURE BOGUS
        OBSTYP(2,007) = 'CSTGD   '  !  U.S. COAST GUARD REPORTS
        OBSTYP(2,008) = 'TIGDCM  '  !  TIDE GAUGE REPORTS (ORIGINALLY IN CMAN)
        OBSTYP(2,009) = 'RIVER   '  !  USGS RIVER / STREAM
        OBSTYP(2,010) = 'CMANSH  '  !  CMAN (ORIGINALLY IN SHEF)
        OBSTYP(2,011) = 'BUOYSH  '  !  MOORED OR DRIFTING BUOY (ORIGINALLY IN SHEF)
        OBSTYP(2,012) = 'TIDGSH  '  !  TIDE GAUGE REPORTS (ORIGINALLY IN SHEF)
        OBSTYP(2,013) = 'SHIPSU  '  !  SHIP MANUAL AND AUTOMATIC (UNRESTRICTED)
C
        READ (CSUBSET(3:5),'(I3)')  J
        J = J + 1                              ! BUFR RECORD TYPE POINTER
        RECTYPE = ASURF(J)
C
        READ (CSUBSET(6:8),'(I3)')  K
C
        IF (OBSTYP(J,K).NE.'        ')  THEN
          OBSTYPE = OBSTYP(J,K)                ! GRAB IT
        ELSE
          OBSTYPE(1:8) = CSUBSET(1:8)          ! DEFAULT IS THE CODE (WHEN UNRECOGNIZED)
        ENDIF
C
C       FOLLOWING ARE IN OTHER BUFR FILES:
C          CSUBSET(1:8).EQ.'NC255nnn')         ! MESONET MADIS RECORDS FOR VARIOUS (NNN) REGIONS
C                                              !   http://madis.noaa.gov/
C          CSUBSET(1:8).EQ.'NC006nnn')         ! NEXRAD LEVEL II RADAR DATA SEQUENCES AT HOURS NNN
C
        RETURN
        END
C
C       ########################################################################
C
        SUBROUTINE CKREC (RECS,RECTYP,RECGET,NREC,
     +    IDUNIT,DODIAG,ACK)
C
C       CHECK RECORD TYPE, E.G. ADPSFC, SFCSHP ...
C
        INTEGER*4 RECS
        CHARACTER*6 RECTYP, RECGET(20)
        CHARACTER*1 DODIAG, ACK
        SAVE
        ACK = 'n'
        DO NR = 1, NREC
          IF (RECTYP.EQ.RECGET(NR))  THEN
            ACK = 'y'
            EXIT
          ENDIF
        ENDDO
        IF (DODIAG.EQ.'y')  THEN
          IF (ACK.EQ.'y')  THEN
            WRITE (IDUNIT,7706) RECS, RECTYP
7706        FORMAT (1X,'CKREC:  RECORD ',I9,', ACCEPTED, RECTYP ',
     +       A6)
          ELSE
            WRITE (IDUNIT,7705) RECS, RECTYP
7705        FORMAT (1X,'CKREC:  RECORD ',I9,', REJECTED, RECTYP ',
     +       A6)
          ENDIF
        ENDIF
        RETURN
        END
C
C       ########################################################################
C
        SUBROUTINE CKOBS (RECS,OBSTYP,OBSGET,NOBS,
     +    IDUNIT,DODIAG,ACK)
C
C       CHECK OBSERVATION (REPORT) TYPE, E.G. SYNOP, METAR ...
C
        INTEGER*4 RECS
        CHARACTER*8 OBSTYP, OBSGET(50)
        CHARACTER*1 DODIAG, ACK
        SAVE
        ACK = 'n'
        DO NO = 1, NOBS
          IF (OBSTYP.EQ.OBSGET(NO))  THEN
            ACK = 'y'
            EXIT
          ENDIF
        ENDDO
        IF (DODIAG.EQ.'y')  THEN
          IF (ACK.EQ.'y')  THEN
            WRITE (IDUNIT,7706) RECS, OBSTYP
7706        FORMAT (1X,'CKOBS:  RECORD ',I9,
     +       ', ACCEPTED, OBSTYP ',A6)
          ELSE
            WRITE (IDUNIT,7705) RECS, OBSTYP
7705        FORMAT (1X,'CKOBS:  RECORD ',I9,
     +       ', REJECTED, OBSTYP ',A6)
          ENDIF
        ENDIF
        RETURN
        END
C
C       ########################################################################
C
        SUBROUTINE CKDATE (RRLEV,RR,IDATE,IBEG,IEND,
     +    IDUNIT,DODIAG,ACK)
C
C       CHECK RECORD OR REPORT DATE
C
        CHARACTER*6 RRLEV       ! WILL CONTAIN EITHER 'RECORD' OR 'REPORT'
        INTEGER*4 RR, IDATE, IBEG, IEND
        CHARACTER*1 DODIAG, ACK
        SAVE
        ACK = 'y'
        IF (IDATE.LT.IBEG.OR.IDATE.GT.IEND)  ACK = 'n'
        IF (DODIAG.EQ.'y')  THEN
          IF (ACK.EQ.'n')  THEN
            WRITE (IDUNIT,7730)  RRLEV, RR, IDATE, IBEG, IEND
7730        FORMAT (/1X,'CKDATE: ',A6,1X,I9,', REJECTED, DATE ',I10,
     +       ' BEGIN ',I10,', END ',I10)
          ELSE
            WRITE (IDUNIT,7731)  RRLEV, RR, IDATE, IBEG, IEND
7731        FORMAT (/1X,'CKDATE: ',A6,1X,I9,', ACCEPTED, DATE ',I10,
     +       ' BEGIN ',I10,', END ',I10)
          ENDIF
        ENDIF
        RETURN
        END
C
C       ########################################################################
C
        SUBROUTINE CKLL(RECS,RR,LAT,LON,LATS,LATN,LONW,LONE,LLWRAP,
     +    IDUNIT,DODIAG,ACK)
C
C       CHECK GEOGRAPHIC WINDOW (LATITUDE - LONGITUDE)
C
        INTEGER*4 RECS, RR
        REAL*8 LAT, LON
        CHARACTER*1 LLWRAP
        CHARACTER*1 DODIAG, ACK
        SAVE
        ACK = 'y'
        IF (LAT.LT.LATS.OR.LAT.GT.LATN)  THEN
          IF (DODIAG.EQ.'y')  THEN
            WRITE (IDUNIT,7750)  RECS, RR, LAT, LON,
     &      LATS, LATN, LONW, LONE
7750        FORMAT ( 1X,'CKLL:   RECORD ',I9,', REPORT ',I9,
     +      ', REJECTED,  ',F7.1,F8.1,',  LATS',I6,' LATN',I6,
     +      ' LONW',I6,' LONE',I6)
          ENDIF
          GO TO 90   ! TO REJECT REPORT
        ENDIF
        IF (LLWRAP.EQ.'n')  THEN
          IF (LON.LT.LONW.OR.LON.GT.LONE)  THEN
            IF (DODIAG.EQ.'y')  THEN
              WRITE (IDUNIT,7750)  RECS, RR, LAT, LON,
     &        LATS, LATN, LONW, LONE
            ENDIF
            GO TO 90   ! TO REJECT REPORT
          ENDIF
        ENDIF
        IF (LLWRAP.EQ.'y')  THEN
          IF (LON.GE.0.0)  THEN
            IF (LON.LT.LONW)  THEN
              IF (DODIAG.EQ.'y')  THEN
                WRITE (IDUNIT,7750)  RECS, LAT, LON,
     &          LATS, LATN, LONW, LONE
              ENDIF
              GO TO 90   ! TO REJECT REPORT
            ENDIF
          ELSE
            IF (LON.GT.LONE)  THEN
              IF (DODIAG.EQ.'y')  THEN
                WRITE (IDUNIT,7750)  RECS, LAT, LON,
     &          LATS, LATN, LONW, LONE
              ENDIF
              GO TO 90   ! TO REJECT REPORT
            ENDIF
          ENDIF
        ENDIF
        IF (DODIAG.EQ.'y')  THEN
          WRITE (IDUNIT,7751)  RECS, LAT, LON,
     &    LATS, LATN, LONW, LONE
7751      FORMAT ( 1X,'CKLL:   RECORD ',I9,', REPORT ',I9,
     +    ', ACCEPTED,  ',F7.1,F8.1,',  LATS',I6,' LATN',I6,
     +    ' LONW',I6,' LONE',I6)

        ENDIF
        RETURN
  90    CONTINUE
        ACK = 'n'
        RETURN
        END
C
C       ########################################################################
C
        SUBROUTINE CKRAD (RECS,RR,XLAT,XLON,RADR,YLAT,YLON,NIR,
     +    IDUNIT,DODIAG,ACK)
C
C       CHECK GEOGRAPHIC CIRCLE (LATITUDE, LONGITUDE, RADIUS)
C
        INTEGER RECS, RR, RADR
        REAL*8 XLAT, XLON
        REAL YLAT(100), YLON(100)
        DATA R, DRAD / 6371.2277, 0.0174533/    ! EARTH'S RADIUS IN KM
        CHARACTER*1 DODIAG, ACK
        SAVE
        ACK = 'n'
        DO  K = 1, NIR
C         DLON = ABS(XLON - YLON(K))  ! ORIGINAL (ON29), FOR 0W TO 359W
          XL = XLON + 180.0           ! BUFR IS -180 TO +180 (DO LIKE LATITUDE)
          YL = YLON(K) + 180.0        ! BUFR IS -180 TO +180 (DO LIKE LATITUDE)
          DLON = ABS(XL - YL)         ! BUFR IS -180 TO +180 (DO LIKE LATITUDE)
          IF (DLON.GT.180.)  DLON = 360. - DLON
          DL = DLON * DRAD
C
          XL = (XLAT + 90.) * DRAD
          YL = (YLAT(K) + 90.) * DRAD
          DIST = R * ABS(ACOS(COS(XL)*COS(YL)
     +                 + SIN(XL)*SIN(YL)*COS(DL)))
          IF (DIST.LE.RADR)  ACK = 'y'
          IF (DODIAG.EQ.'y')  THEN
            IF (ACK.EQ.'n')  THEN
              WRITE (IDUNIT,7760) RECS, RR, XLAT, XLON,
     +          YLAT(K), YLON(K), DIST, RADR
7760          FORMAT (1X,'CKRAD:  RECORD ',I9,', REPORT ',I9,
     +          ', REJECTED, ',' XLAT ',F6.0,' XLON ',F6.0,' YLAT ',
     +          F8.2,' YLON ',F8.2,' DIST ',F6.0,' RADR ',I6)
            ELSE
              WRITE (IDUNIT,7761) RECS, RR, XLAT, XLON,
     +          YLAT(K), YLON(K), DIST, RADR
7761          FORMAT (1X,'CKRAD:  RECORD ',I9,', REPORT ',I9,
     +          ', ACCEPTED, ',' XLAT ',F6.0,' XLON ',F6.0,' YLAT ',
     +          F8.2,' YLON ',F8.2,' DIST ',F6.0,' RADR ',I6)
            ENDIF
          ENDIF
          EXIT
        ENDDO
        RETURN
        END
C
C       ########################################################################
C
        SUBROUTINE CKWMO(RECS,RR,A8RPID,WMOLIST,NWMO,
     +    IDUNIT,DODIAG,ACK)
C
C       CHECK WMO STATION NUMBER (bbsss)
C
        INTEGER*4 RECS, RR
        CHARACTER*6 A8RPID
        CHARACTER*5 WMOLIST(100)
        CHARACTER*1 DODIAG, ACK
        SAVE
        ACK = 'n'
        DO N = 1, NWMO
          IF (A8RPID(1:5).EQ.WMOLIST(N)(1:5))  THEN
            ACK = 'y'
            EXIT
          ENDIF
        ENDDO
        IF (DODIAG.EQ.'y')  THEN
          IF (ACK.EQ.'n')  THEN
            WRITE (IDUNIT,7770) RECS, RR, A8RPID
7770        FORMAT (1X,'CKWMO:  RECORD ',I9,', REPORT ',I9,
     +       ', REJECTED, STATION ',A6)
          ELSE
            WRITE (IDUNIT,7771) RECS, RR, A8RPID
7771        FORMAT (1X,'CKWMO:  RECORD ',I9,', REPORT ',I9,
     +       ', ACCEPTED, STATION ',A6)
          ENDIF
        ENDIF
        RETURN
        END
C
C       ########################################################################
C
        SUBROUTINE CKWBB(RECS,RR,A8RPID,WBBLIST,NWBB,
     +    IDUNIT,DODIAG,ACK)
C
C       CHECK WMO STATION BLOCK NUMBER (bb)
C
        INTEGER*4 RECS, RR
        CHARACTER*5 A8RPID
        CHARACTER*2 WBBLIST(100)
        CHARACTER*1 DODIAG, ACK
        SAVE
        ACK = 'n'
        DO N = 1, NWBB
          IF (A8RPID(1:2).EQ.WBBLIST(N)(1:2))  THEN
            ACK = 'y'
            EXIT
          ENDIF
        ENDDO
        IF (DODIAG.EQ.'y')  THEN
          IF (ACK.EQ.'n')  THEN
            WRITE (IDUNIT,7775) RECS, RR, A8RPID
7775        FORMAT (1X,'CKWBB:  RECORD ',I9,', REPORT ',I9,
     +       ', REJECTED, STATION ',A6)
          ELSE
            WRITE (IDUNIT,7776) RECS, RR, A8RPID
7776        FORMAT (1X,'CKWBB:  RECORD ',I9,', REPORT ',I9,
     +       ', ACCEPTED, STATION ',A6)
          ENDIF
        ENDIF
        RETURN
        END
C
C       ########################################################################
C
        SUBROUTINE CKELEV (RECS,RR,IEL,IELO,IEHI,
     +    IDUNIT,DODIAG,ACK)
C
C       CHECK STATION ELEVATION
C
        INTEGER*4 RECS, RR
        CHARACTER*1 DODIAG, ACK
        SAVE
        ACK = 'y'
        IF (IEL.LT.IELO.OR.IEL.GT.IEHI)  ACK = 'n'
        IF (DODIAG.EQ.'y')  THEN
          IF (ACK.EQ.'n')  THEN
            WRITE (IDUNIT,7780) RECS, RR, IEL, IELO, IEHI
7780        FORMAT (1X,'CKELEV: RECORD ',I9,', REPORT ',I9,
     +       ', REJECTED, ELEV ',I6,' BOTTOM ',I6,', TOP ',I6)
          ELSE
            WRITE (IDUNIT,7781) RECS, RR, IEL, IELO, IEHI
7781        FORMAT (1X,'CKELEV: RECORD ',I9,', REPORT ',I9,
     +       ', ACCEPTED, ELEV ',I6,' BOTTOM ',I6,', TOP ',I6)
          ENDIF
        ENDIF
        RETURN
        END
C
C       ########################################################################
C
        SUBROUTINE UP2LOW (WORD1,WORD2,NCH)
C
C       MAP UPPER CASE CHARACTERS TO LOWER CASE
C
        CHARACTER*100 WORD1, WORD2
        CHARACTER*1 CHU(26), CHL(26)
        DATA CHU /
     +  'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
     +  'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'/
        DATA CHL /
     +  'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
     +  'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'/
        SAVE
        DO  N = 1, NCH
          K = 13        ! (M)
          KSTEP = 7
          IDO = 0
          DO
            IDO = IDO + 1
            IF (IDO.GT.6)  EXIT
            IF (WORD1(N:N).EQ.CHU(K))  THEN
              WORD2(N:N) = CHL(K)
              GO TO 9
            ENDIF
            IF (WORD1(N:N).LT.CHU(K))  THEN
              K = K - KSTEP
            ELSE
              K = K + KSTEP
            ENDIF
            KSTEP = KSTEP * .5
            IF (KSTEP.LT.1)  KSTEP = 1
          ENDDO
   9      CONTINUE
        ENDDO
        RETURN
        END
C
C       ########################################################################
C
        SUBROUTINE LOW2UP (WORD1,WORD2,NCH)
C
C       MAP LOWER CASE CHARACTERS TO UPPER CASE
C
        CHARACTER*100 WORD1, WORD2
        CHARACTER*1 CHU(26), CHL(26)
        DATA CHU /
     +  'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
     +  'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'/
        DATA CHL /
     +  'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
     +  'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'/
        SAVE
        DO  N = 1, NCH
          K = 13        ! (M)
          KSTEP = 7
          IDO = 0
          DO 
            IDO = IDO + 1
            IF (IDO.GT.6)  EXIT
            IF (WORD1(N:N).EQ.CHL(K))  THEN
              WORD2(N:N) = CHU(K)
              GO TO 9
            ENDIF
            IF (WORD1(N:N).LT.CHL(K))  THEN
              K = K - KSTEP
            ELSE
              K = K + KSTEP
            ENDIF
            KSTEP = KSTEP * .5
            IF (KSTEP.LT.1)  KSTEP = 1
          ENDDO
   9      CONTINUE
        ENDDO
        RETURN
        END
C ***
C ***   When modifying bufradpsfc.f, after editing, a reinstall must
C ***     be done by doing
C ***          cd .../bufrdecode/src
C ***          [edit] bufradpsfc.f
C ***          cd ../install
C ***          cat install.sh | sed s/CPLAT=linux/CPLAT=sun/ >! mk_exec.sh
C ***          chmod 700 mk_sun_exec.sh
C ***          mk_sun_exec.sh >>&! /dev/null
C ***     Note that the install expects to be run in the .../install
C ***     directory, to find the source code in a file named
C ***     .../src/bufradpsfc.f , and a library of supporting code in
C ***     .../lib , and will write the executable in
C ***     .../exe/bufradpsfc.x .  It's default platform is "linux".  You
C ***     you may need to change that to "sun" or something.
C ***
C ***   The install creates a script named convert.csh in directory .../exe,
C ***     which runs every file found in directory .../bufrobs through
C ***     both surface data decoders - .../exe/bufradpsfc.x and
C ***     .../exe/bufrsfcship.x , leaving outputs in directory .../textobs.
C ***
C ***   Note that the published data files are in:
C ***     /datazone/dsszone/ds461.0/
C ***     A single typical ds461.0 tar file has a name like:
C ***     gdassfcobs.20100410.tar.gz
C ***
