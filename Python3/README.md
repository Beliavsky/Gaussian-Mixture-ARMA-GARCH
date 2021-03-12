# How it works:

The main program is privided with EM.py file, which contains the begin entry for the execution of the package.
It has to be launched on a command shell with the Python3 interpreter, such as "python3 EM.py".

The EM.py script starts to read a dataframe from a .csv file given in input (a test file has been provided
with the name "RUNEUSDT_4H.csv" which contains all the relevant information for the RUNE/USDT digital asset 
pair obtained from the Binance exchange for a certain period of time ranging from 2020/09/04 to 2021/03/12.
Each report in the dataframe file gives information about the digital asset price packed into a 4 hours window,
such as open time, open price, close price, ecc.

As default parameter, the main program take into acount a 500 close price observations (from the last and backward)
and compute the price returns time series to be modelled by an ARMA-GARCH mixture fitting.
The fitting strategy is obtained by implementing the Expectation Maximization algorithm, which will find the MLE 
for the K gaussian compoenents underlying the observations; the number of iterations to reach a maximum depends on 
the random initial parameters values for the ARMA-GARCH models, so a tollerance parameters needs to be set in order to 
stop the fit when the likelihood doesn't change anymore above the tollerance percentage.

At the state of the art the K parameter can be arbitrary choosen (usually 2 or 3 is a good choice), while the ranks 
of the ARMA and GARCH models (R,S and Q,P respectively) needs to be set to 2 (second order models).
Hopefully future implementations of the package will permit larger values for these orders.

Finally, the "Utils" folder contains all the modules for the arithmetical computation subroutines and functions that 
operates during the fitting procedure, and which are invoked by the main program at each iteration.

