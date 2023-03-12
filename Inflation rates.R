##################### Problem Set 3 #####################
### Question 4 ###
#install and load libraries
install.packages("dplyr")
library(dplyr)
library(ggplot2)
install.packages("tseries")
library(tseries)

#import data
pcepi=read.csv("PCEPI.csv")
pcepi$DATE <- as.Date(pcepi$DATE, format = "%Y-%m-%d") #convert DATE column to date class

### i) Compute the inflation rate, infl = 400 * (lnPCEPIt - lnPCEPIt-1)
pcepi$lnpcepi <- log(pcepi$PCEPI) #take natural logs
pcepi$lnpcepi_1 <- lag(pcepi$lnpcepi, 1) #get values at lag 1
pcepi$infl <- 400*(pcepi$lnpcepi - pcepi$lnpcepi_1)
pcepi$infl

# The units of infl are precentage per year 
# because lnPCEPIt - lnPCEPIt-1 = ln(PCEPIt/PCEPIt-1) = ln(1+inflation) = inflation for small number of inflation
# since time period is in quarter, timing it by 4 gives the inflation per year, and by 100 gives the percentage

### ii) Plot the value of infl
ggplot(data=pcepi[-c(1,2,3),], aes(x=DATE, y=infl))+geom_line()

# I think infl looks like to have a stochastic trend because there is a lot of variation 
# over time instead of centering around a linear trend.

### iii) Compute the first four autocorrelations of delta(infl)
pcepi$infl_1 <- lag(pcepi$infl, 1) ##infl at lag 1
pcepi$delta_infl <- pcepi$infl - pcepi$infl_1 ##compute delta(infl)

acf(pcepi$delta_infl[-c(1,2,3)], lag.max = 4, pl=FALSE) ##compute autocorrelations
acf(pcepi$delta_infl[-c(1,2,3)], lag.max = 4) ##plot

### iv) Plot the value of delta_infl
ggplot(data=pcepi[-c(1,2,3),], aes(x=DATE, y=delta_infl))+geom_line()

# There seems to be little autocorrelation left after taking first differences of infl from part iii),
# so the plot should look jagged.

### v) OLS regression of delta_infl_t on delta_infl_t-1
pcepi$delta_infl_1 <- lag(pcepi$delta_infl, 1)
ols = lm(delta_infl~delta_infl_1, pcepi[-c(1,2,3),])
summary(ols)

#Yes, the coefficient of delta_infl_t-1 is estimated to be -0.25 and 
#is statistically significant at 0.1% confidence level.









