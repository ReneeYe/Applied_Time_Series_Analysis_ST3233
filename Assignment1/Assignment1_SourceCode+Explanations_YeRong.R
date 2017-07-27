#### ---R Source Code Info---
#### Module: ST3233 Applied Time Series Analysis 
#### Title: Assignment 1
#### Last Editing: 2016-10-01 18:30:53
#### Status: AC/1Qs/3NSs/2BDis

# Exerise1(Electricity forecast)

#1. load data and convert into a Time Series
electri_load <- read.table("D:/@复旦u/5.大三第一学期@NUS/1.学习/ST3233 Applied time Series Analysis/Assignments/Assignment1/electricity_load.dat")
electricity <- ts(electri_load$V1,frequency= 7*24)

#2. display the Time Series
plot(electricity,main = "The Hourly Electricity Demand in Poland",ylab="Electricity Demand(Hourly)")
## From the plot, we can see that the time series has a seasonal period = 1 week, since it is a weekly electricity demand.

#3. decompose the time series into trend + seasonal + remainder
library(fpp)
fit <- stl(electricity, s.window = "periodic", robust = TRUE)
plot(fit)

#4. use a seaonalplot to display the seasonal pattern
library(forecast)
#detrending by using the variable fit$time.series[,"trend"] and plot seasonplot
seasonplot(electricity-fit$time.series[,"trend"], s = 24*7, col = 1:24, main = "Seasonplot of electricity demand")
#explain why the time series is not perfectly seasonal:
## Form the decomposition plot of the time series, we find that some remainders are large

#5. TES to make a forecast for the next 3 weeks
electricity_hw <- hw(electricity, initial = "simple",seasonal = "additive", h = 24*7*3)
plot(electricity,main = "The Predicted Hourly Electricity Demand",ylab="Electricity Demand(Hourly)",xlim = c(0,27),lwd = 2)
lines(electricity_hw$mean, col = "blue", type ="l", lwd = 1)

# Exercise2(Bootstrap estimate)

#Is the MA(2): Xk = Wk - (5/6)Wk-1 + (1/6)Wk-2 invertible?
## consider the characteristic polynominal
library(shape)
plot(polyroot(c(1, -5/6, 1/6)),pch = 20, col = "red",xlim = c(-3,3), ylim = c(-3,3))
plotcircle(r=1, mid = c(0,0),lw = 3)
## From the plot, we can clearly see that there's no root inside the circle, which means all the roots of the characteristic polynomial are strictly larger than 1 in absoulte value, so the MA(2) model is invertible.

#2. stimulate a trajectory of length T = 1000 
MA2 <- arima.sim(n=1000, list(ma=c(-5/6,1/6)), sd=1)
plot(MA2[1:500],lwd = 3, type = "l", main = "MA(2) Trajectory")
MA2_fit <- arima(MA2, order = c(0,0,2))
MA2_fit
##From the estimates of the coefficients, MA1 = -0.8244, MA2 = 0.1735, which is close to -5/6 and 1/6.

autocor_coeff_exact <- ARMAacf(ma = c(-5/6,1/6),lag.max = 20)
acf(MA2, lag.max = 20, lwd = 3, main = "MA(2)::ACF(Theory vs. Empirical estim) = 1000")
points(0.3+0:20, autocor_coeff_exact , type = "h", lwd =3, xlab = "lag", ylab = "Autocorrelation", main = "AR(2)::ACF",col = "red")
acf(MA2_fit, lag.max = 20, lwd = 3)
## Question on this problem, what does DOUBLE CHECK mean?

#3. estimate the accuracy of the estimation procedure for short time series
## simulate 1000 time series with T = 40, and get the estimation of Betas.
beta <- c()
for (i in 1:1000){
  MA2_short <- arima.sim(n=40, list(ma=c(-5/6,1/6)), sd=1)
  MA2_short_fit <- arima(MA2_short,order = c(0,0,2))
  beta[i] <- MA2_short_fit$coef[2]
}
beta
mean(beta)
# the mean of the estimation of Beta = {beta1, beta2, ... beta1000} is equal to 0.150296, which is close to 1/6

#4. plot a histogram of all these eatimates, and compute the mean and variance
hist(beta, freq = T, nclass = 100 , main = "Histogram of all the estimations of beta")
mean(beta)
# mean(beta) = 0.1502962
var(beta)
# variance of beta = 0.04429112

#5. simulate a time series MA(1), with T = 1000 and alpha = 3
MA1 <- arima.sim(n = 10000, list(ma = c(1,3)),sd = 1)
MA1_fit <- arima(MA1, order = c(0,0,1))
MA1_fit
# From the estimation of coefficients, the estimation of alpha = 0.2583, which is different from alpha = 3
## Explanation: let's consider the invertibility of the MA(1) model with alpha = 3
plot(polyroot(c(1, 3)),pch = 20, col = "red",xlim = c(-3,3), ylim = c(-3,3))
plotcircle(r=1, mid = c(0,0),lw = 3)
## From the plot, we can clearly see that there is a root inside the circle, which means the root of the characteristic polynomial is less than 1 in absoulte value, so the MA(1) model is not invertible.
## However, consider another MA(1) model with alpha = 1/3: Xk = Wk + 1/3Wk-1, which is invertible, and that's why when we use arima() to estimate the coefficient, the result is close to 1/3


# Exercise3(Sale forecast)
## get time series from Google Trends @SG
aircon_data <- read.csv("D:/@复旦u/5.大三第一学期@NUS/1.学习/ST3233 Applied time Series Analysis/Assignments/Assignment1/aircon_GTrends_SG.csv",header= FALSE,sep=",")
aircon_GTrend <- ts(aircon_data$V2,frequency = 12,start = c(2004,1))
# plot it
plot.ts(aircon_GTrend, main = "The number of queries containing 'aircon'", xlab = "Time", ylab = "Queries")
# decompoes it
library(fpp)
fit <- stl(aircon_GTrend, s.window = "periodic", robust = TRUE)
plot(fit)

#using TES to estimate the queries containing "aricon" in Jun/2017
library(forecast)
aircon_hw <- hw(aircon_GTrend, initial = "optim", seasonal = "additive", h = 20)
plot(aircon_GTrend,main = "The Predicted queries of aircon",ylab="Queries",xlim = c(2004, 2018),lwd = 3)
lines(fitted(aircon_hw),col = "blue", type = "l", lwd = 2)
lines(aircon_hw$mean, col = "red", type ="l", lwd = 3)
plot(aircon_hw,lw = 3)
aircon_hw_Jun17 <- aircon_hw$mean[9]
aircon_hw
# Get following result:
#         Point Forecast    Lo 80     Hi 80    Lo 95     Hi 95 #
#Jun 2017       90.39623 81.53079  99.26167 76.83771 103.95475 #

# using purchase_rate to predict the sale of aircon in Jun/2017, given the sale in Jun2016 is 51.
aircon_GTrend_Jun16 <- aircon_GTrend[12*12+6]
aircon_sold_Jun16 <- 51
purchase_rate <- aircon_sold_Jun16 / aircon_GTrend_Jun16

aircon_sold_Jun17.est <- aircon_hw_Jun17 * purchase_rate
aircon_sold_Jun17.est

aircon_sold_Jun17.lower <- 76.83771 * purchase_rate
aircon_sold_Jun17.upper <- 103.95475 * purchase_rate
aircon_sold_Jun17.lower
aircon_sold_Jun17.upper

# The predicted sale in Jun2017 is 65, with confidence interval [55.98176,75.73846] at 95% level.


# Exercise4(Yule-Walker)
## 这个应该是AR model,不是MA model

# 1. estimate the first three autocorrelation coefficients
AR3_data <- read.table("D:/@复旦u/5.大三第一学期@NUS/1.学习/ST3233 Applied time Series Analysis/Assignments/Assignment1/asg_1_MA3.dat")
AR3 <- ts(AR3_data)
acf(AR3,lag.max = 20,lwd = 3, main ="AR(3)::ACF = 10000")
AR3_acf <- acf(AR3,lag.max=20)
AR3_acf$acf[1:4]
#result is: 1.0000000 0.4198046 0.3719330 0.1622123
# so the first three autocorrelation coefficients are: 0.4198046, 0.3719330, 0.1622123

# 2. (Non-programming) Yule-Walker equations:
# Yule-Walker equations for estimating alpha, beta, gamma:
## p(k) = alpha * p(k-1) + beta * p(k-2) + gamma * p(k-3)

# 3: find the estimation of alpha, beta, gamma by solving the equations:
## equations (Note: p(k) = p(-k)):
##    p(0) = 1
##    p(1) = alpha * p(0) + beta * p(1) + gamma * p(2)
##    p(2) = alpha * p(1) + beta * p(0) + gamma * p(1)
##    p(3) = alpha * p(2) + beta * p(1) + gamma * p(0)

library(rootSolve)
p0 <- 1
p1 <- AR3_acf$acf[2]
p2 <- AR3_acf$acf[3]
p3 <- AR3_acf$acf[4]
model <- function(x){
  f1 <- x[1] * p0 + x[2] * p1 + x[3] * p2 - p1
  f2 <- x[1] * p1 + x[2] * p0 + x[3] * p1 - p2
  f3 <- x[1] * p2 + x[2] * p1 + x[3] * p0 - p3
  c(f1=f1,f2=f2,f3=f3)
}
solution <- multiroot(f = model, start = c(0,0,0))
solution$root
#result: 0.33736221  0.26085723 -0.07277295
# So, the estimations are : alpha = 0.33736221, beta = 0.26085723, gamma = -0.07277295
