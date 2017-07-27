ts1_data <- read.table("D:/@复旦u/5.大三第一学期@NUS/1.学习/ST3233 Applied time Series Analysis/Tutorial/Tutorial5/tut5_ts1.dat")
ts1 <- ts(ts1_data)
ts2_data <- read.table("D:/@复旦u/5.大三第一学期@NUS/1.学习/ST3233 Applied time Series Analysis/Tutorial/Tutorial5/tut5_ts2.dat")
ts2 <- ts(ts2_data)
plot(ts1,main= "Time Series 1", ylab= "TS1",type = "l")
plot(ts2,main= "Time Series 2", ylab= "TS2",type = "l")

#First plot autocorrelation function to find whether they are MA model
acf(ts1[1:50],lag.max = 20,lwd = 3, main = "TS1::ACF")
acf(ts2[1:50],lag.max = 20,lwd = 3, main = "TS2::ACF")

#TS1 is a MA(1) model and TS2 is a AR(p) model
#determine p, plot PACF
pacf(ts2[1:50],lag.max = 20,lwd = 3, main ="TS2::PACF")
# p = 2, so ts2 is a AR(2) model.

# Fit AR(p) / MA(q)
# Fit ts1 by using MA(1) model
ts1_fit <- arima(ts1, order = c(0,0,1))
ts1_fit
# Xk = Wk + 0.6253 Wk-1

# Fit ts2 by using AR(2) model
ts2_fit <- arima(ts2,order = c(2,0,0))
ts2_fit
# Xk = 0.1852 Xk-1 +0.1395 Xk-2 + Wk