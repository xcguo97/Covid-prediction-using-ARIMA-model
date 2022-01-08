
# Install and Load Packages.
library("forecast")
library("tseries") 		# reqired for adf.test of stationarity
library("readxl")

# Clear Data.
rm(list=ls(all=TRUE)) 	

# Read Excel file.
data <- read.csv("United_States_COVID_Cases_and_Deaths.csv")
head(data)



# Subset data by state, and create variables for cases and deaths.
data_ok <- subset(data, data$state=='OK')

# Cases / Deaths per thousand

cases <- data_ok[,3]/1000
deaths <- data_ok[,4]/1000


### CASES ### 


# Create time series object for cases, frequency is 365 days & start date is the 
# 22nd day of 2020. 
yy.cases = ts(cases, frequency = 365,start = c(2020, 22))		
yy.deaths = ts(deaths,frequency = 365, start = c(2020, 22))

# Plot to view trends over time.
plot.ts(yy.cases)		
plot.ts(yy.deaths)

# We observe a clear updward pattern, with a few small bumps. 

# Use Augmented Dickey-Fuller Test to test stationarity == > large p-value means nonstationary
adf.test(yy.cases)	

# P-Value of 0.99, we conclude that our data is not stationary.

# Plot differences (d) in ARIMA
yd = diff(yy.cases,differences = 1)			
plot.ts(yd)								# looks stationary visually
adf.test(yd)							# estimated p = 0.01 => small p-value (< 0.10) => so yd is stationary ==> fix d = 1 in ARIMA models to be fitted

# We conclude that YD is stationary (P-Value of 0.01) We'll thus fit d = 1 in 
# the ARIMA model. 

# Next, test Pacf to fit (p) value in ARIMA. 
Pacf(yd, lag.max = 10)					# Pacf suggests p = 1 or 2 (2 is close).


# To decide MA, plot Acf. For MA(q) => Acf becomes zero at some lag q
Acf(yd, lag.max = 10)				# Acf suggests q = 2


### TEST ARIMA MODELS ###

### PREDICT VALUES FROM ARIMA MODELS ### 

# Proceed with Auto ARIMA.
m0 = auto.arima(yy.cases)
summary(m0)

aicc0 <- m0$aicc
bic0 <- m0$bic

# prediction
h = 30																# forecast horizon
m0.predict = forecast:::forecast.Arima(m0, h = 30, level = c(95))
plot(m0.predict)													# check forecasts
summary(m0.predict)


m1 = Arima(yy.cases, order = c(4,4,3))			# d has to be increased to fix nonstationaity
aicc1 <- m1$aicc
bic1 <- m1$bic

m2 = Arima(yy.cases, order = c(2,2,3))			
aicc2 <- m2$aicc
bic2 <- m2$bic

m3 = Arima(yy.cases, order = c(3,3,4))			# d has to be increased to fix nonstationaity
aicc3 <- m3$aicc
bic3 <- m3$bic

m4 = Arima(yy.cases, order = c(3,3,2))			# d increased to fix nonstationaity
aicc4 <- m4$aicc
bic4 <- m4$bic


## compare scores on informatin criteria to find competitive models

aicc.out = cbind(aicc0, aicc1, aicc2, aicc3, aicc4)
aicc.diff = aicc.out - min(aicc.out)			# m2 is competitive with m0 according to AIC_c

bic.out = cbind(bic0, bic1, bic2, bic3, bic4)
bic.diff = bic.out - min(bic.out)				# m2 is competitive with m0 according to BIC as well

## Now check the plot and summary of m2

m2.predict = forecast:::forecast.Arima(m2, h = 30, level = c(95))
plot(m2.predict)								# forecast shows that cases likely to reduce 

summary(m2.predict)								# MAPE 1.85%. Better than m0

## Now check the plot and summary of m4


m4.predict = forecast:::forecast.Arima(m4, h = 30, level = c(95))
plot(m4.predict)								# forecast shows that cases likely to reduce 

summary(m4.predict)								# MAPE 1.85%. Better than m0



# Step 5.  Consensus Forecast (aka forecasts combination).

ybar0 <- m0.predict$mean						# auto.arima forecast
ybar2 <- m2.predict$mean						# m1 based forecat

ybar.avg = (ybar0 + ybar2)/2					# consensus forecast

# Also need to find prediction intervals for consensus forecasts. First find variances and then average the variances.

low0 <- m0.predict$lower
var0 <- ((ybar0 - low0[1:h])/1.96)^2 			# b/c yhat - (yhat - 1.96 x se) gives 1.96 x se

low1 <- m1.predict$lower
var1 <- ((ybar2 - low1[1:h])/1.96)^2 

var.avg = (var0[1:h] + var1[1:h])/2				# averaged variance




# Step 6. Provide Prediction Intervals for Consensus Forecasts

lo68 = ybar.avg - 1 * sqrt(var.avg)				# 1 SD (68% confidence)
hi68 = ybar.avg + 1 * sqrt(var.avg)


lo95 = ybar.avg - 1.96 * sqrt(var.avg)			# 2 SD (68% confidence)
hi95 = ybar.avg + 1.96 * sqrt(var.avg)

final.out = cbind(ybar.avg, lo68, hi68, lo95, hi95)

### DEATHS ### 

m0 = auto.arima(yy.deaths)
summary(m0)

aicc0 <- m0$aicc
bic0 <- m0$bic

# prediction
h = 30																# forecast horizon
m0.predict = forecast:::forecast.Arima(m0, h = 30, level = c(95))
plot(m0.predict)													# check forecasts
summary(m0.predict)


m1 = Arima(yy.deaths, order = c(4,4,3))			# d has to be increased to fix nonstationaity
aicc1 <- m1$aicc
bic1 <- m1$bic

m2 = Arima(yy.deaths, order = c(2,2,3))			
aicc2 <- m2$aicc
bic2 <- m2$bic

m3 = Arima(yy.deaths, order = c(3,3,4))			# d has to be increased to fix nonstationaity
aicc3 <- m3$aicc
bic3 <- m3$bic

m4 = Arima(yy.deaths, order = c(3,3,2))			# d increased to fix nonstationaity
aicc4 <- m4$aicc
bic4 <- m4$bic


## compare scores on informatin criteria to find competitive models

aicc.out = cbind(aicc0, aicc1, aicc2, aicc3, aicc4)
aicc.diff = aicc.out - min(aicc.out)			# m2 is competitive with m0 according to AIC_c

bic.out = cbind(bic0, bic1, bic2, bic3, bic4)
bic.diff = bic.out - min(bic.out)				# m2 is competitive with m0 according to BIC as well

## Now check the plot and summary of m2

m2.predict = forecast:::forecast.Arima(m2, h = 30, level = c(95))
plot(m2.predict)								# forecast shows that cases likely to reduce 

summary(m2.predict)								# MAPE 1.85%. Better than m0

## Now check the plot and summary of m4


m4.predict = forecast:::forecast.Arima(m4, h = 30, level = c(95))
plot(m4.predict)								# forecast shows that cases likely to reduce 

summary(m4.predict)								# MAPE 1.85%. Better than m0



# Step 5.  Consensus Forecast (aka forecasts combination).

ybar0 <- m0.predict$mean						# auto.arima forecast
ybar2 <- m2.predict$mean						# m1 based forecat

ybar.avg = (ybar0 + ybar2)/2					# consensus forecast

# Also need to find prediction intervals for consensus forecasts. First find variances and then average the variances.

low0 <- m0.predict$lower
var0 <- ((ybar0 - low0[1:h])/1.96)^2 			# b/c yhat - (yhat - 1.96 x se) gives 1.96 x se

low1 <- m1.predict$lower
var1 <- ((ybar2 - low1[1:h])/1.96)^2 

var.avg = (var0[1:h] + var1[1:h])/2				# averaged variance




# Step 6. Provide Prediction Intervals for Consensus Forecasts

lo68 = ybar.avg - 1 * sqrt(var.avg)				# 1 SD (68% confidence)
hi68 = ybar.avg + 1 * sqrt(var.avg)


lo95 = ybar.avg - 1.96 * sqrt(var.avg)			# 2 SD (68% confidence)
hi95 = ybar.avg + 1.96 * sqrt(var.avg)

final.out = cbind(ybar.avg, lo68, hi68, lo95, hi95)

