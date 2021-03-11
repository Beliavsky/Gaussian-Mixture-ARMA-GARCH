
!!! START PROGRAM !!!
PROGRAM MAGEM_ALTUM

!--Modules used--!
USE MODELS
USE PARAMS
USE IO

IMPLICIT NONE

! Input file name for reading the series to be analyzed
CHARACTER(LEN=:), ALLOCATABLE	:: file_name

!-Time series declaration
type(TIME_SERIES)			:: price_returns
type(TIME_SERIES), DIMENSION(K)	:: arma_res, garch_sigma

! Model variables declaration
type(ARMA_RANK)	:: arma_model
type(GARCH_RANK)	:: garch_model	
type(GMIX_MODEL) 	:: mix_model

!-Read data from file
file_name = "./RUNEUSDT_4H_conv.txt"
CALL price_returns % load(file_name)

!-Initialize the Mixture model name and number of component
print*,NL,"Initialize the Gaussian Mixture ARMA-GARCH model:"
mix_model = GMIX_MODEL(name="Model 1") 
CALL mix_model % init(K,S,R,Q,P)


print*,"================================================================================="
print*,"Model NAME |",TAB,TAB,"K",TAB,"    S",TAB,TAB,"R",TAB,"    Q",TAB,TAB,"P"
print*,"---------------------------------------------------------------------------------"
print*,"   ",mix_model % name," |",mix_model % K,mix_model % S,mix_model % R,mix_model % Q,mix_model % P
print*,NL,"INITIALIZED PARAMETER VALUES:"
print*,"a:",mix_model % a
print*,"b:",mix_model % b
print*,"delta_0:",mix_model % delta_0
print*,"delta:",mix_model % delta
print*,"beta:",mix_model % beta
print*,"================================================================================="

!-First compute the residuals from the ARMA model (given the time series to be analized) ...
print*,NL,"Initial random ARMA model series:"
arma_model = mix_model % arma_rank
arma_res = arma_model % residuals(price_returns,K)
CALL arma_res(1) % display()

!...then compute the variance from the GARCH module (given the residuals) 
print*,NL,"Initial random GARCH model series:"
garch_model = mix_model % garch_rank
garch_sigma = garch_model % sigma(arma_res,K)
CALL garch_sigma(1) % display()

print*,NL,"Everything OK!"



END PROGRAM MAGEM_ALTUM
