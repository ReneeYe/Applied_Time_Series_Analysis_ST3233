x <- arima.sim(n = 100000, list(ar=c(0.5),ma=(0.5)),sd = 1)
plot.ts(x[1:100],lwd = 3)
x_acf <- acf(x, lwd = 3, main = "ACF")
pacf(x, lwd = 3, main = "PACF")

# estimate sigma^2
est <- arima(x, order=c(1,0,1))
est$sigma2
rou1 <- x_acf$acf[2]/est$sigma2
rou2 <- x_acf$acf[3]/est$sigma2