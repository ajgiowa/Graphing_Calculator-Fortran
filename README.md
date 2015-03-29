# Graphing_Calculator-Fortran
*This is a Fortran77 graphing calculator that I started.  It does not quite work exactly how I want it but it is pretty good.  Feel free to try it out and add to it. 
* Graphing Program
* Andrew Glover
* 

* DECLERATIONS OF VARIABLES
      IMPLICIT NONE
      INTEGER        :: RUN, INTVAL, END, I, J, K, YPOS
      CHARACTER * 20 :: IFILEN, OFILEN
      CHARACTER      :: YLINE(71), XLINE(100)
      CHARACTER * 5  :: CHOICE
      LOGICAL        :: LEXIST, LOPEN, SORTED

      REAL 	     :: X(100), Y(100), DX(100), YVAL(6)
      REAL	     :: XMIN, XMAX, XSTEP, YMIN, YMAX, YSTEP
      REAL	     :: STEP=5, TEMP, A, N

*CLEAR THE ARRAYS
	DO I=1, 100
	X(I)=0
	Y(I)=0
	DX(I)=0
	YVAL(I)=0
	YLINE(I)=' '
	END DO

* DO WHILE TO LOOP PROGRAM
	RUN=1
      DO WHILE(RUN.EQ.1)

* PROMPT USER FOR INPUT FILE
      WRITE (*,*) 'Enter and INPUT file name: '
      READ (*,*) IFILEN

      INQUIRE (FILE= IFILEN, EXIST= LEXIST)
* DOES FILE EXIST
        IF ( .NOT. LEXIST ) THEN
	DO WHILE(.NOT. LEXIST.AND.RUN.EQ.1)
	WRITE (*,*) 'File does not exist'
        WRITE (*,*) 'Enter an INPUT file name or "QUIT" to quit'
        READ (*,*) IFILEN
*IF THEY QUIT
	IF (IFILEN .EQ. 'QUIT') THEN
        RUN=2
	END IF
*CHECK TO SEE IF NEW FILE EXISTS
	INQUIRE (FILE= IFILEN, EXIST= LEXIST)
	
	END DO
	END IF
*OPEN FILE
      OPEN (UNIT=9, FILE=IFILEN, STATUS='OLD')
      LOPEN = .TRUE.


* OUTPUT FILE
* PROMPT USER FOR OUTPUT FILE
      WRITE (*,*) 'Enter and OUTPUT file name: '
      READ (*,*) OFILEN

      INQUIRE (FILE= OFILEN, EXIST= LEXIST)

      IF ( .NOT. LEXIST ) THEN
	OPEN (UNIT=10, FILE=OFILEN, STATUS='NEW')
      	LOPEN = .TRUE.
        
*IF FILE EXISTS
	ELSE
	WRITE (*,*) 'File name already exists..'
	WRITE (*,*) 'Would you like to '
	WRITE (*,*) '1-Overwrite file'
	WRITE (*,*) '2-Choose new file name'
	WRITE (*,*) 'QUIT'
	READ (*,*) CHOICE

	SELECT CASE(CHOICE)
*OVERWRITE
		CASE('1')
		 OPEN (UNIT=10, FILE=OFILEN, STATUS='REPLACE')
      		 LOPEN = .TRUE.
*REPROMPT
		CASE('2')
		 DO WHILE(LEXIST)
		 WRITE (*,*) 'Already exists, Enter a new OUTPUT file name: '
		 READ (*,*) OFILEN
		 INQUIRE (FILE= OFILEN, EXIST= LEXIST)
		 END DO
		 OPEN (UNIT=10, FILE=OFILEN, STATUS='NEW')
      		 LOPEN = .TRUE.
*QUIT
		CASE('QUIT')
		OFILEN = 'QUIT'
		RUN = 2
	END SELECT

*CHECK TO SEE IF NEW FILE EXISTS
*	INQUIRE (FILE= OFILEN, EXIST= LEXIST)
	
	END IF

*READ IN NUMBER OF DATA POINTS
	READ(9,*) A

*ERROR CHECK ON NUM OF DATA POINTS
      IF(A.LT.0.OR.A.GT.100) THEN
	RUN=2
      END IF

*READ IN DATA POINTS
	I=1
	DO WHILE(I.LE.A)	
	  READ(9, *, IOSTAT=INTVAL) X(I), Y(I)
	  IF(INTVAL .EQ. 0) THEN
	  ELSE
	  PRINT*, 'Read error'
	  RUN=2
	  END IF
	  I=I+1
      	END DO

*WRITE DATA POINTS TO OUTPUT FILE
3		FORMAT(' ', 2(F10.4))
	WRITE (10, *) ' '
	I=1
	DO WHILE(I.LE.A)	
		WRITE (10, 3) X(I), Y(I)
		I=I+1
	END DO

*CALCULATE XMIN
	I=1
	XMIN=X(I)
	I=2
	DO WHILE(I.LE.A)
	 IF(X(I).LT.XMIN) THEN
	   XMIN=X(I)
	 END IF
	 I=I+1
	END DO
	WRITE(10,*) 'Xmin= ', XMIN

*CALCULATE XMAX
	I=1
	XMAX=X(I)
	I=2
	DO WHILE(I.LE.A)
	 IF(X(I).GT.XMAX) THEN
	   XMAX=X(I)
	 END IF
	I=I+1
	END DO
	WRITE(10,*) 'Xmax= ', XMAX

*CALCULATE XSTEP
	XSTEP=(XMAX-XMIN)/(A/5)
	WRITE(10,*) 'Xstep= ', XSTEP

*CALCULATE YMIN
	I=1
	YMIN=Y(I)
	I=2
	DO WHILE(I.LE.A)
	 IF(Y(I).LT.YMIN) THEN
	   YMIN=Y(I)
	 END IF
	I=I+1
	END DO
	WRITE(10,*) 'Ymin= ', YMIN

*CALCULATE YMAX
	I=1
	YMAX=Y(I)
	I=2
	DO WHILE(I.LE.A)
	 IF(Y(I).GT.YMAX) THEN
	   YMAX=Y(I)
	 END IF
	I=I+1
	END DO
	WRITE(10,*) 'Ymax= ', YMAX

*CALCULATE YSTEP
	YSTEP=(YMAX-YMIN)/6
	WRITE(10,*) 'Ystep= ', YSTEP

*WRITE Y SCALE
5		FORMAT(' ', (F10.4))
	I=YMIN
	J=1
	WRITE (10, *) ' '
	DO WHILE(I.LE.A)	
		WRITE (10, 5) REAL(I)
		YVAL(J)=I
		J=J+1
		I=I+YSTEP
	END DO

*BUBBLE SORT
	END=A-1
	SORTED=.FALSE.
	DO WHILE(.NOT. SORTED)
	SORTED=.TRUE.
	DO I=1, END
	  IF(X(I).GT.X(I+1)) THEN
	    TEMP=X(I)
	    X(I)=X(I+1)
	    X(I+1)=TEMP
	    TEMP=Y(I)
	    Y(I)=Y(I+1)
	    Y(I+1)=TEMP
	    SORTED=.FALSE.
	  END IF
	END DO
	END=END-1
	END DO

*DELTA X
	STEP=100
	I=2
	DO WHILE(I.LE.A)
	  DX(I)=X(I)-X(I-1)
	    IF(DX(I).LT.STEP.AND.DX(I).NE.0) THEN
		STEP=DX(I)
	    END IF
	   I=I+1
	END DO

*---------------------------------------------
*PLOT Y AXIS
	I=1
	DO WHILE(I.LE.71)
	YLINE(I)='-'
	I=I+1
	ENDDO
	I=1
	DO WHILE(I.LE.71)
	YLINE(I)='+'
	I=I+10
	ENDDO
	WRITE(10,110) YLINE

110     FORMAT(' ', X9, 71A)

	IF(XSTEP.LT.20) THEN
	  XSTEP=20
	  DX=(XMAX-XMIN)/(XSTEP-1)
	ENDIF
	IF(XSTEP.GT.100) THEN
	  XSTEP=100
	  DX=(XMAX-XMIN)/(XSTEP-1)
	ENDIF

*---------------------------------------
		J=2
		DO WHILE(J.LE.XSTEP)
		I=1
		DO WHILE(I.LE.71)
		   YLINE(I)= ' '
		   I=I+1
		ENDDO
		IF(MOD(REAL(J), STEP).EQ.0) THEN
		   YLINE(I)='+'
		ELSE
		   YLINE(I)='|'
		ENDIF

		IF(MOD(REAL(J), STEP).EQ.0.OR.J.EQ.1) THEN
		   WRITE(10,12) XMIN+(J-1)*DX, YLINE
		ELSE
		   WRITE(10,110) YLINE
		END IF
		J=J+1
		END DO

12		FORMAT(' ', F9.3, A62)

	RUN=2
	END DO

	CLOSE(UNIT=9)
        CLOSE(UNIT=10)
	
	WRITE(*,*) 'Have a nice day'
      END PROGRAM
