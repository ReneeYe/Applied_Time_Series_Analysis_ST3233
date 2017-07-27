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
