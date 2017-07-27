SP_data <- read.csv("D:/@复旦u/5.大三第一学期@NUS/1.学习/ST3233 Applied time Series Analysis/Tutorial/Tutorial5/tut5_SP500.csv",sep = ",")
x <- ts(SP_data$Open,frequency = 356)
plot.ts(x)

y <- log(x/lag(x,k=1))
plot(y)

n <- length(x)
y <- c()
for (i in 1:n-1){
  y[i] = log(x[i]/x[i-1]) 
}
y <- ts(y[2:n])
plot(y)

acf(x[1:50],lag.max = 20,main = "X::ACF")
pacf(x[1:50],lag.max = 20,main = "X::PACF")
#x is a AR(1) model
acf(y[2:50],lag.max = 20,main = "Y::ACF")
pacf(y[2:50],lag.max = 20,main = "Y::PACF")
#Y is a MA(2) model

y_fit <- arima(y,order = c(0,0,2))
y_fit
# yk = Wk + 0.0455 Wk-1 - 0.0372 Wk-2

y_sim <- arima.sim(n = 15000, list(ma = c(0.0455,-0.0372)),sd = 1)
plot(y_sim)
plot(y)
# No, the simulated time series doesn't look similar to yk

# 6,7>>