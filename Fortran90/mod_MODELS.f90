MODULE MODELS
USE IO
IMPLICIT NONE
PRIVATE


! ARMA model ranks data type
TYPE, PUBLIC :: ARMA_RANK
	INTEGER				:: S=0, R=0
	REAL, DIMENSION(:,:), ALLOCATABLE	:: a 
	REAL, DIMENSION(:,:), ALLOCATABLE	:: b
CONTAINS
	PROCEDURE :: init_ARMA => random_init_ARMA
	PROCEDURE :: residuals => ARMA_residuals
END TYPE ARMA_RANK



! GARCH model ranks data type
TYPE, PUBLIC, EXTENDS(ARMA_RANK) :: GARCH_RANK
	INTEGER				:: Q=0, P=0
	REAL,DIMENSION(:), ALLOCATABLE	:: delta_0
	REAL, DIMENSION(:,:), ALLOCATABLE	:: delta
	REAL, DIMENSION(:,:), ALLOCATABLE	:: beta
CONTAINS
	PROCEDURE :: init_GARCH => random_init_GARCH
	PROCEDURE :: sigma => GARCH_sigma
END TYPE GARCH_RANK



! Gaussian Mixture ARMA-GARCH model with K components 
TYPE, PUBLIC, EXTENDS(GARCH_RANK) :: GMIX_MODEL
	INTEGER			:: K = 0
	CHARACTER(LEN=:), ALLOCATABLE	:: name
CONTAINS
	PROCEDURE :: init => random_init
END TYPE GMIX_MODEL


!!--- SUBROUTINES & FUNCTIONS ---!!
CONTAINS

	!-Return the stability value for the GARCH model parameters (delta + beta < 1)
	!-Compute the sum of delta(d) and beta(b) parameters for every K component
	FUNCTION check_2stability(d,b) RESULT(sum_check)
		REAL, DIMENSION(:,:), INTENT(IN)	:: d,b
		REAL, DIMENSION(:), ALLOCATABLE	:: db_sum
		LOGICAL				:: sum_check
		INTEGER				:: K,Q,P
		INTEGER				:: i,j
		
		!-Retrieve parameters dimensions
		K = SIZE(d,DIM=1)
		Q = SIZE(d,DIM=2)
		P = SIZE(b,DIM=2)
		
		!-Result must be an array with K components dimension
		ALLOCATE(db_sum(K))
		db_sum = 0
		DO i=1,K
			DO j=1,Q
				db_sum(i) = db_sum(i) + d(i,j)
			END DO
			DO j=1,P
				db_sum(i) = db_sum(i) + b(i,j)
			END DO
		END DO
		
		!-Check if all sum elements are < 1
		sum_check = ALL(db_sum < 1)
		
	END FUNCTION check_2stability
	
	
	!-ARMA model parameters random initialization
	SUBROUTINE random_init_ARMA(self,S,R,K)
		class(ARMA_RANK), INTENT(INOUT)	:: self
		INTEGER				:: S, R, K

		!-Fix parameter dimensions
		self%S = S
		self%R = R
		
		!-Allocate memory and generate random number for parameter initialization
		ALLOCATE(self % a(K,S))
		ALLOCATE(self % b(K,R))
		CALL RANDOM_NUMBER(self % a)
		CALL RANDOM_NUMBER(self % b)

	END SUBROUTINE random_init_ARMA


	!-GARCH model parameters random initialization
	SUBROUTINE random_init_GARCH(self,Q,P,K)
		class(GARCH_RANK), INTENT(INOUT)	:: self
		INTEGER				:: Q, P, K
		LOGICAL				:: sum_check = .False.

		self % Q = Q
		self % P = P
		ALLOCATE(self % delta_0(K))
		ALLOCATE(self % delta(K,Q))
		ALLOCATE(self % beta(K,P))
		CALL RANDOM_NUMBER(self % delta_0)
		
		!-Call RANDOM_NUMBER until match for stability condition (delta + beta < 1) 
		DO WHILE(sum_check .eqv..False.)
			CALL RANDOM_NUMBER(self % delta)
			CALL RANDOM_NUMBER(self % beta)		
			sum_check = check_2stability(self % delta,self % beta)
		END DO
	END SUBROUTINE random_init_GARCH


	!-GAUSSIAN MIXTURE model parameters initialization
	!-call subroutines for ARMA-GARCH random init 
	SUBROUTINE random_init(self,K,S,R,Q,P)
		class(GMIX_MODEL), INTENT(INOUT)	:: self
		INTEGER				:: K,S,R,Q,P

		!-Initialize the number of mixture components and it sub-models parameters
		self%K = K
		CALL self % init_ARMA(S,R,K)
		CALL self % init_GARCH(Q,P,K)
	END SUBROUTINE random_init


	FUNCTION ARMA_residuals(params,series,K) RESULT(residuals)
		INTEGER				:: i,t,M,K
		class(ARMA_RANK)			:: params
		type(TIME_SERIES)			:: series
		type(TIME_SERIES), DIMENSION(K)	:: residuals
				
		!-Start two nasted loops: over K components and along M size
		M = SIZE(series % idx)
		DO i=1,K
			!-Allocate space for the output series of rank K
			ALLOCATE(residuals(i) % idx(M - params % R))
			ALLOCATE(residuals(i) % value(M - params % R))
						
			!-Iterate until the end of the input series
			DO t = params % R, M-1
			
				!-AR part of the model
				residuals(i) % idx(t-params % R+1) = series % idx(t+1)
				residuals(i) % value(t-params % R+1) = series % value(t+1) - DOT_PRODUCT(params % b(i,:), series % value(t:t-params % R+1:-1))
				
				!-MA part of the model
				IF (t .ge. (params%R + params%S)) THEN		
					residuals(i) % value(t-params % R+1) =  residuals(i) % value(t-params % R+1) & 
										&- DOT_PRODUCT(params % a(i,:), residuals(i) % value(t-params % R:t-params % R-params % S+1:-1))
				END IF
			END DO 
		END DO

	END FUNCTION ARMA_residuals


	FUNCTION GARCH_sigma(params,series,K) RESULT(sigma)
		INTEGER				:: i,t,M,K
		class(GARCH_RANK)			:: params
		type(TIME_SERIES), DIMENSION(K)	:: series
		type(TIME_SERIES), DIMENSION(K)	:: sigma
				
		!-Start two nasted loops: over K components and along M size
		M = SIZE(series(1) % idx)
		DO i=1,K
			!-Allocate space for the output series of rank K
			ALLOCATE(sigma(i) % idx(M - params % Q))
			ALLOCATE(sigma(i) % value(M - params % Q))
						
			!-Iterate until the end of the input series
			DO t = params % Q, M-1
			
				!-GAR part of the model
				sigma(i) % idx(t-params % Q+1) = series(i) % idx(t+1)
				sigma(i) % value(t-params % Q+1) = params % delta_0(i) &
								   &+ series(i) % value(t+1) &
								   &- DOT_PRODUCT(params % delta(i,:), series(i) % value(t:t-params % Q+1:-1))
				
				!-CH part of the model
				IF (t .ge. (params%Q + params%P)) THEN
					sigma(i) % value(t-params % Q+1) =  sigma(i) % value(t-params % Q+1) &
									    &- DOT_PRODUCT(params % beta(i,:), sigma(i) % value(t-params % Q:t-params % Q-params % P+1:-1))
				END IF
			END DO 
		END DO

	END FUNCTION GARCH_sigma		
	
END MODULE MODELS










