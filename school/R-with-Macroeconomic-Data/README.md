# R-with-Macroeconomic-Data
Analyzing growth rates and running regressions on Penn World Table data (examples from a problem set in Macro theory)

(a)	Descriptive Statistics

-	GDP per worker in 1985 and 2005, by country
-	Average Savings Rate from 1985 to 2005 by Country
-	Average Growth Rate of the Labor Force (n) from 1985 to 2005 by Country. 
- Average Growth Rate of GDP per worker from 1985 to 2005, gY/L. 
-	Number of Countries
-	Mean of log(Y/L1985) across all countries at t=1985
-	Mean of log(Y/L2005) across all countries at t=2005
-	Mean of “s” across all countries across all time in the data set
-	Mean of “n” (avg growth rate of labor force) across all countries across all time in the data set
-	Mean of gY/L (avg growth rate of GDP/worker) across all countries across all time in the data set 
-	Standard Deviation across countries for Y/L1985 and Y/L2005, respectively
-	Standard Deviation across countries & all time for “s”, the average savings rate
-	Standard Deviation across countries & all time for “n”, the average growth rate of the labor force
-	Standard Deviation across countries & all time for “gY/L”, the average growth rate of Y/L

(b) Unconditional Convergence

The Solow model predicts that those with lower initial GDP per capita (ie. developing countries) would grow at a faster GDP/capita growth rate and vice versa. These countries would only converge to the same steady state growth rate if they have the same characteristics(paramters) that determine their steady state (such as savings, population growth). This regression is showing that since the countries do not have the same parameters, there is a very weak correlation between initial GDP per capita and GDP/capita growth rate, which shows "unconditional convergence" because the countries are not converging to the same steady state.
- R Squared very low

(c) MRW Model (Mankiw, Romer, Weil) 

The MRW model incorporates human capital accumulation to the Solow Model in order to improve the model's explanation of variation of GDP/capita across countries.
- Coefficient of 1.95, showing a 1% increase in savings, predicting a 1.95% increase in Y/L (2005)
- R squared of around 50%

(d) HJ Model (Hall and Jones)
The HJ model emphasizes the role that technology plays in the long run growth of GDP/capita
- In this log-log model, B1 estimator is 1.1315, which is interpreted as a 1% increase in A(2005) predicts a 1.13% increase in Y/L (2005)
- R squared 90%


