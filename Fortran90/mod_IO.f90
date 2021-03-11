MODULE IO
USE PARAMS
USE datetime_module
IMPLICIT NONE

!!--- SPECIAL CHARACTERS --!!
CHARACTER (LEN=1) :: TAB = char(9) 
CHARACTER (LEN=1) :: NL = NEW_LINE(NL)


!!--- DERIVIED TYPES ---!!
TYPE, PUBLIC :: TIME_SERIES
	type(datetime), DIMENSION(:), ALLOCATABLE	:: idx	
	REAL, DIMENSION(:), ALLOCATABLE		:: value
CONTAINS
	PROCEDURE :: load => load_data
	PROCEDURE :: display => print_data
END TYPE TIME_SERIES



!!--- SUBROUTINES & FUNCTIONS ---!!	
CONTAINS

	!!---- Load time series data from a formatted .txt file composed by 3 columns (date, time, value)  ---------!!
	!!---- and allocate space for the time series derived data type based on the file's length (limited by N) --!!
	SUBROUTINE load_data(self,file_name)
		class(TIME_SERIES), INTENT(INOUT)	:: self
		CHARACTER(LEN=:), ALLOCATABLE		:: file_name
		CHARACTER(LEN=8)			:: date
		CHARACTER(LEN=6)			:: time
		INTEGER				:: i,res,M
		
		!-Open the formatted file to read from 
		OPEN(UNIT=1, FILE=file_name, FORM="FORMATTED", IOSTAT=res)
			IF (res /= 0) THEN
				print*,"Error in opening file:",file_name
				print*,"Err status:",res
				STOP
			END IF
			
			!-Count the total lines in the .txt file
			i = 0
			DO WHILE(.true.)
				READ(1,'(A)',END=99) LINE
				i = i + 1
			END DO
			99 CONTINUE
			CLOSE(1)
		print*,"Reading file:",file_name
		print*,"Allocated space for time series of length:",i
		
		!-Set the lenght of the time series (truncate to N if longer)
		IF (i .gt. N) THEN
			ALLOCATE(self % idx(N))
			ALLOCATE(self % value(N))
			M = N
		ELSE
			ALLOCATE(self % idx(i))
			ALLOCATE(self % value(i))
			M = i
		END IF
		
		!-Read datetime index and the corresponding real value from file		
		OPEN(UNIT=1, FILE=file_name, FORM="FORMATTED", IOSTAT=res)
			DO i=1,M
				READ(UNIT=1, FMT=*, IOSTAT=res, END=100) date, time, self % value(i)
				IF (res /= 0) THEN
					print*,"Error in reading file:",file_name
					print*,"Err status:",res
					CLOSE(1)
					STOP
				END IF

				!-Fill the TIME_SERIES datetime index by converting the date_time string in input
				self % idx(i) = strptime(date//" "//time,datetime_fmt)
			END DO		
			100 CONTINUE
			CLOSE(1)
	END SUBROUTINE load_data



	!!--- Print to screen the time series ---!!
	SUBROUTINE print_data(self)
		class(TIME_SERIES), INTENT(IN) :: self
		LOGICAL			:: compact
		INTEGER			:: i

		compact = .False.
		print*,"------------------------------------"
		print*,"  DATE",TAB,TAB,"TIME",TAB,"  VALUE"
		DO i=1,SIZE(self % idx)

			!-Try to print a compact view of the time series
			IF (i .gt. 5 .and. i .lt. SIZE(self % idx) - 5) THEN
				IF (compact .eqv. .False.) THEN
					print*,TAB,TAB,TAB,"  ..."
					IF (i .eq. 6) THEN 
						compact = .True.
					END IF
				END IF
			ELSE
				print*,self % idx(i) % strftime('%Y-%m-%d %H:%M:%S'), self % value(i)
			END IF			
		END DO
		print*,"Length:",SIZE(self % idx)
		print*,"------------------------------------"
		print*,NL
	END SUBROUTINE print_data

	

END MODULE IO

