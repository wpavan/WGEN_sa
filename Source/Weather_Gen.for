!=======================================================================
!  COPYRIGHT 1998-2011 DSSAT Foundation
!                      University of Florida, Gainesville, Florida
!                      International Fertilizer Development Center
!                      Washington State University
!  ALL RIGHTS RESERVED
!=======================================================================
!=======================================================================
!  CROPGRO Weather Module consists of the following files:
!     WGEN_SIM.FOR  - Main routine for weather module
!     HMET.FOR    - Generates hourly meteorological values from daily data
!     IPWTH.FOR   - Reads daily weather data from FILEW
!     SOLAR.FOR   - Computes day length (DAYLEN) and solar parameters (SOLAR)
!     WGEN.FOR    - Generates daily weather data
!     WTHMOD.FOR  - Modification of daily data based on user-supplied
!                     parameters
!
!=======================================================================
!  WGEN_SIM, Program, C.H.Porter
!  Main program for weather generator
!  Modified WEATHR.FOR to run as a stand-alone - outside crop models.
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  07/16/2012 CHP Written based on WEATHR.FOR from DSSAT CSM v4.6.1.0
!-----------------------------------------------------------------------
!  Called by: Main
!  Calls:     DAYLEN, ERROR, HMET, IPWTH, SOLAR, WGEN, WTHMDB, WTHMOD
!=======================================================================

      Program Weather_Gen !(CONTROL, ISWITCH, WEATHER, YREND)

!-----------------------------------------------------------------------
      USE ModuleDefs   
      IMPLICIT NONE
      EXTERNAL CALC_TDEW, HMET, SOLAR, YR_DOY
      SAVE

      CHARACTER*1  MEWTH    !, RNMODE
      CHARACTER*4  INSI
      CHARACTER*6  RHCOLUMNNAME
!      CHARACTER*6  ERRKEY
      CHARACTER*80 FILEW, CLIFILE, Text
!      CHARACTER*78 MESSAGE(10)
!      CHARACTER*80 PATHWT
!      CHARACTER*92 FILEWW

      INTEGER YR, YRDOY, YRSIM
      INTEGER RSEED1, RSEED(4)
      INTEGER DYNAMIC
      INTEGER INCDAT, LENGTH, LenString
      INTEGER RHTHRESHOLD, RHHOURSOVER, HR
      INTEGER StartDate, EndDate

      INTEGER DOY

      REAL
     &  CCO2, CLOUDS, CO2, DAYL, DEC, ISINB, PAR, 
     &  PI, RAD, RAIN, REFHT, RHUM, 
     &  S0N, SNDN, SNUP, SRAD, 
     &  TAMP, TAV, TAVG, TDAY, TDEW, TGROAV, TGRODY,
     &  TMAX, TMIN, WINDHT, WINDSP,
     &  XELEV, XLAT, XLONG
     
      LOGICAL NOTDEW
      REAL CALC_TDEW

      REAL, DIMENSION(TS) :: AMTRH, AZZON, BETA, FRDIFP, FRDIFR, PARHR
      REAL, DIMENSION(TS) :: RADHR, RHUMHR, TAIRHR, TGRO, WINDHR

!      PARAMETER (ERRKEY = 'WEATHR')
      PARAMETER (PI=3.14159, RAD=2.0*PI/365.0)

!!     The variable "CONTROL" is of constructed type "ControlType" as 
!!     defined in ModuleDefs.for, and contains the following variables.
!!     The components are copied into local variables for use here.
!      TYPE (ControlType) CONTROL
!      TYPE (SwitchType) ISWITCH
!      TYPE (WeatherType) WEATHER
!
!      DYNAMIC = CONTROL % DYNAMIC 
!      MULTI   = CONTROL % MULTI   
!      RUN     = CONTROL % RUN    
!      RNMODE  = CONTROL % RNMODE  
!      REPNO   = CONTROL % REPNO  
!      YRDOY   = CONTROL % YRDOY   
!      YRSIM   = CONTROL % YRSIM   

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
        DYNAMIC = 2

        CALL GETARG(1, Text)
        READ(Text, '(I7)') StartDate
        CALL GETARG(2, Text)
        READ(Text, '(I7)') EndDate
        CALL GETARG(3, Text)
        READ(Text, '(I7)') RSEED1
        CALL GETARG(4, MEWTH)
        CALL GETARG(5, CLIFILE)
        CALL GETARG(6, Text)
        READ(Text, '(I7)') RHTHRESHOLD

        LENGTH = LENSTRING(Text)
        RHCOLUMNNAME = "   "(1:(3-LENGTH)) // "RH" // Text

        LENGTH = LENSTRING(CLIFILE)
        FILEW = CLIFILE(1:LENGTH-4) // ".WTG"
!-----------------------------------------------------------------------
C       Set default values FOR REFHT AND WINDHT
          REFHT  = 1.5
          WINDHT = 2.0
          CCO2 = -99.
          SRAD = -99.0
          TMAX = -99.0
          TMIN = -99.0
          RAIN = -99.0
          PAR  = -99.0
          RHUM = -99.0

          YRDOY = StartDate

          CALL WGEN(DYNAMIC,
     &      CLIFILE, MEWTH, RSEED1, YRDOY, YRSIM,         !Input
     &      INSI, RSEED, TAMP, TAV, XELEV, XLAT, XLONG,   !Output
     &      PAR, RAIN, SRAD, TMAX, TMIN)                  !Output
     &             

          IF (TAV  .LE. 0.0) THEN       
            TAV = 20.0
          ENDIF

          IF (TAMP .LE. 0.0) THEN
            TAMP = 5.0
          ENDIF

!     Subroutine to determine daily CO2
!      CALL CO2VAL(DYNAMIC, YRSIM, YRDOY, CO2)

C     Calculate daily solar parameters.
      CALL SOLAR(
     &    DAYL, DEC, SRAD, XLAT,                          !Input
     &    CLOUDS, ISINB, S0N)                             !Output

!C     Adjust wind speed from reference height to 2m height.
!      IF (WINDSP > 0.0) THEN
!        WINDSP = WINDSP * (2.0 / WINDHT) ** 2.0
!      ELSE
!        WINDSP = 86.4
!      ENDIF
!
      CALL OpWeath(DYNAMIC, YRDOY, FILEW,
     &    INSI, XLAT, XLONG, XELEV, TAV, TAMP, REFHT, WINDHT,           !Station
     &    CO2, PAR, RAIN, RHCOLUMNNAME, RHHOURSOVER, SRAD, TMAX, TMIN)  !Daily values

      YRDOY = StartDate
      YRSIM = StartDate
      DO WHILE (YRDOY <= EndDate)

!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATIONS - Read or generate daily weather data
!         (also run for initialization to get first day of weather data
!         for use by soil nitrogen and soil temperature initialization
!         routines.)
!***********************************************************************
      DYNAMIC = RATE

!-----------------------------------------------------------------------
C     Read new weather record.
        SRAD = -99.0
        TMAX = -99.0
        TMIN = -99.0
        RAIN = -99.0
        PAR  = -99.0
        RHHOURSOVER = 0

      CALL WGEN(DYNAMIC,
     &      CLIFILE, MEWTH, RSEED1, YRDOY, YRSIM,         !Input
     &      INSI, RSEED, TAMP, TAV, XELEV, XLAT, XLONG,   !Output
     &      PAR, RAIN, SRAD, TMAX, TMIN)                  !Output

!     Update YR and DOY for DAYL calculations
      CALL YR_DOY(YRDOY, YR, DOY)

C     Calculate day length, sunrise and sunset.
      CALL DAYLEN(
     &    DOY, XLAT,                                      !Input
     &    DAYL, DEC, SNDN, SNUP)                          !Output

!     Subroutine to determine daily CO2
      CALL CO2VAL(DYNAMIC, YRSIM, YRDOY, CO2)

C     Calculate daily solar parameters.
      CALL SOLAR(
     &    DAYL, DEC, SRAD, XLAT,                          !Input
     &    CLOUDS, ISINB, S0N)                             !Output

C     Adjust wind speed from reference height to 2m height.
      IF (WINDSP > 0.0) THEN
        WINDSP = WINDSP * (2.0 / WINDHT) ** 2.0
      ELSE
        WINDSP = 86.4
      ENDIF

!     Substitute default values if TDEW is missing.
      IF (TDEW <= -90.)  THEN 
c               MJ, 2007-04-05: set TDEW to TMIN if TDEW not otherwise available.  This is not
c               appropriate to South African (and presumably other) conditions
c               --> suggest replacing with a better calculation based on relative humidity, if
c                   available.
          NOTDEW = .TRUE.
          IF (RHUM .GT. 0.01) THEN
              TDEW = CALC_TDEW(TMIN, RHUM)
          ELSE
             TDEW = TMIN
          ENDIF
      ELSE
          NOTDEW = .FALSE.
      ENDIF  

C     Calculate hourly weather data.
      CALL HMET(
     &    CLOUDS, DAYL, DEC, ISINB, PAR, REFHT,           !Input
     &    SNDN, SNUP, S0N, SRAD, TDEW, TMAX,              !Input
     &    TMIN, WINDHT, WINDSP, XLAT,                     !Input
     &    AMTRH, AZZON, BETA, FRDIFP, FRDIFR, PARHR,      !Output
     &    RADHR, RHUMHR, TAIRHR, TAVG, TDAY, TGRO,        !Output
     &    TGROAV, TGRODY, WINDHR)                         !Output

      DO HR = 1, TS
          IF (RHUMHR(HR) .GE. RHTHRESHOLD) THEN
              RHHOURSOVER = RHHOURSOVER + 1
          ENDIF
      ENDDO

!***********************************************************************
!***********************************************************************
!     Daily Output
!***********************************************************************
      DYNAMIC = 5 
C-----------------------------------------------------------------------
      CALL OpWeath(DYNAMIC, YRDOY, FILEW,
     &    INSI, XLAT, XLONG, XELEV, TAV, TAMP, REFHT, WiNDHT,           !Station
     &    CO2, PAR, RAIN, RHCOLUMNNAME, RHHOURSOVER, SRAD, TMAX, TMIN)  !Daily values

      YRDOY = INCDAT(YRDOY, 1)

!***********************************************************************
      ENDDO

      DYNAMIC = 6

      CALL OpWeath(DYNAMIC, YRDOY, FILEW,
     &    INSI, XLAT, XLONG, XELEV, TAV, TAMP, REFHT, WiNDHT,           !Station
     &    CO2, PAR, RAIN, RHCOLUMNNAME, RHHOURSOVER, SRAD, TMAX, TMIN)  !Daily values

      STOP
      END PROGRAM Weather_Gen

!***********************************************************************
!***********************************************************************
! WEATHR Variables
!-----------------------------------------------------------------------
! CO2        Atmospheric carbon dioxide concentration (ppm)
! ERRKEY     Subroutine name for error file 
! FILEW      Weather data file 
! MEWTH      Switch for method of obtaining weather data-- 'G' or 'M'- read 
!              weather data file 'S'- read SIMMETEO inputs and generate 
!              weather data 'W'- read WGEN inputs and generate weather data 
! PAR        Daily photosynthetically active radiation or photon flux 
!              density (moles[quanta]/m2-d)
! RAIN       Precipitation depth for current day (mm)
! REFHT      Reference height for wind speed (m)
! RHUM       Relative humidity (%)
! RSEED(4)   Random number generator seeds 
! RSEED1     Random number generator seed- user input 
! SRAD       Solar radiation (MJ/m2-d)
! TAMP       Amplitude of temperature function used to calculate soil 
!              temperatures (�C)
! TAV        Average annual soil temperature, used with TAMP to calculate 
!              soil temperature. (�C)
! TMAX       Maximum daily temperature (�C)
! TMIN       Minimum daily temperature (�C)
! WINDHT     Reference height for wind speed (m)
! WINDSP     Wind speed (km/d)
! XELEV      Field elevation (not used) (m)
! XLAT       Latitude (deg.)
! XLONG      Longitude (deg.)
! YR_DOY     Function subroutoine converts date in YYYYDDD format to integer 
!              year (YY) and day (DDD). 
! YRDOY      Current day of simulation (YYYYDDD)
! YRSIM      Start of simulation date (YYYYDDD)
!***********************************************************************
!***********************************************************************


   