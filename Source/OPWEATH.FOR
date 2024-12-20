C=======================================================================
C  OpWeath, Subroutine, C.H.Porter from weather portions of OPDAY
C  Generates output for daily weather data.
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  02/08/2002 CHP Written
C  06/06/2002 GH  Modified for crop rotations
C  08/20/2002 GH  Modified for Y2K
!  07/16/2012 CHP Modified for stand-alone weather generator.
C-----------------------------------------------------------------------
C  Called from:   WEATHR
C  Calls:         None
C=======================================================================
      SUBROUTINE OpWeath(DYNAMIC, YRDOY, FILEW,
     &    INSI, XLAT, XLONG, XELEV, TAV, TAMP, REFHT, WiNDHT,           !Station
     &    CO2, PAR, RAIN, RHCOLUMNNAME, RHHOURSOVER, SRAD, TMAX, TMIN)  !Daily values

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      SAVE

      CHARACTER*4   INSI
      CHARACTER*6   RHCOLUMNNAME
      CHARACTER*6,  PARAMETER :: ERRKEY = 'OPWTHR'
      CHARACTER*48  DAILYFORMAT 
      CHARACTER*80  FILEW

      INTEGER DYNAMIC, ERRNUM, LUN, YRDOY, YYDDD, RHHOURSOVER

      REAL CO2, PAR, RAIN, SRAD, TMAX, TMIN
      REAL XLAT, XLONG, XELEV, TAV, TAMP, REFHT, WiNDHT

      LOGICAL FOPEN, FEXIST

!C-----------------------------------------------------------------------
!C     Define constructed variable types based on definitions in
!C     ModuleDefs.for.
!C-----------------------------------------------------------------------
!      TYPE (ControlType) CONTROL
!C-----------------------------------------------------------------------
!C     The variable "ISWITCH" is of type "SwitchType".
!C-----------------------------------------------------------------------
!      TYPE (SwitchType) ISWITCH
!
!      IDETG   = ISWITCH % IDETG
!      IDETL   = ISWITCH % IDETL
!      IF (IDETG .NE. 'Y' .OR. IDETL == '0') RETURN

!      DAS     = CONTROL % DAS 
!      DYNAMIC = CONTROL % DYNAMIC 
!      FROP    = CONTROL % FROP  
!      RNMODE  = CONTROL % RNMODE
!      REPNO   = CONTROL % REPNO
!      RUN     = CONTROL % RUN    
!      YRDOY   = CONTROL % YRDOY   

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
!        CALL GETLUN('OUTWTH', LUN)
!        INQUIRE (FILE = OUTWTH, EXIST = FEXIST)
!        IF (FEXIST) THEN
!          OPEN (UNIT = LUN, FILE = OUTWTH, STATUS = 'OLD',
!     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
!        ELSE
          LUN = 200
          INQUIRE(FILE=FILEW, EXIST=FEXIST)
          IF (FEXIST) THEN
            OPEN (UNIT = LUN, FILE = FILEW, STATUS = 'REPLACE',
     &      IOSTAT = ERRNUM)
          ELSE
            OPEN (UNIT = LUN, FILE = FILEW, STATUS = 'NEW',
     &      IOSTAT = ERRNUM)
          ENDIF


!          WRITE(LUN,'("*WEATHER MODULE DAILY OUTPUT FILE")')
!        ENDIF
!
!        IF (RNMODE .NE. 'Q' .OR. RUN .EQ. 1) THEN
!          !For first run of a sequenced run, use replicate
!          ! number instead of run number in header.
!          IF (RNMODE .EQ. 'Q') THEN
!            CALL HEADER(SEASINIT, LUN, REPNO)
!          ELSE
!            CALL HEADER(SEASINIT, LUN, RUN)
!          ENDIF

          WRITE (LUN,120) INSI, XLAT, XLONG,XELEV, TAV,TAMP,REFHT,WiNDHT
  120     FORMAT('*WEATHER DATA : ',//,
     &      '@ INSI      LAT     LONG  ELEV   TAV   AMP REFHT WNDHT',/,
     &      2X, A4, 2F9.3, F6.0, 2F6.1, 2F6.2, /)
     
          DAILYFORMAT = '@DATE  SRAD  TMAX  TMIN  RAIN   PAR  CO2D ' // 
     &      RHCOLUMNNAME
          WRITE (LUN, '(A)') DAILYFORMAT
!        ENDIF

!***********************************************************************
!***********************************************************************
!     Daily Output - OUTPUT and SEASEND calls
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT .OR. DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------
      INQUIRE(FILE=FILEW,OPENED=FOPEN)
      INQUIRE(UNIT=LUN,OPENED=FOPEN)
C       Generate output for file Weather.OUT
!        IF ((DYNAMIC .EQ. OUTPUT .AND. MOD(DAS,FROP) .EQ. 0) .OR.
!     &      (DYNAMIC .EQ. SEASEND  .AND. MOD(DAS,FROP) .NE. 0) .OR. 
!     &       DAS == 1) THEN 

!!         These can be modified by ETPHOT during hourly energy balance
!          TGROAV = WEATHER % TGROAV
!          TGRODY = WEATHER % TGRODY
      
!          CALL YR_DOY(YRDOY, YEAR, DOY)
          !Daily printout
          YYDDD = MOD(YRDOY,100000)
          WRITE (LUN,('(I5, 6F6.1, I6)')) YYDDD, SRAD, TMAX, TMIN, RAIN, 
     &          PAR, CO2, RHHOURSOVER
!        ENDIF

        IF (DYNAMIC .EQ. SEASEND) THEN
          !Close daily output files.
          CLOSE (LUN)
        ENDIF

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE OpWeath
!***********************************************************************
