#[exercise1]: load csv and plot it
##using function ts()
temp_data <- read.csv("D:/@复旦u/5.大三第一学期@NUS/ST3233 Applied time Series Analysis/Tutorial/Tutorial1/temperature_in_singapore.csv",header = T)
temp_in_sg <- ts(temp_data$mean_temp,frequency = 12,start = c(1982,1))
plot(temp_in_sg,type = "l")

#[exercise2]: decompose the time series into a trend,seasonal and reminder TS
library(fpp)
fit <- stl(temp_in_sg,s.window = "periodic",robust = T) #stl() is used in decomposition
plot(fit) # plot decomposition: TS=seasonal + trend + remainder

#Plot trend- the red line
plot(temp_in_sg)
lines(fit$time.series[,"trend"],col ="red",lwd = 3)

#season adjust
plot(temp_in_sg)
lines(temp_in_sg-fit$time.series[,"seasonal"],col = "blue")

#[exercise3]: plot seasonal pattern
#trend estimate
weights <- c(0.5,rep(1,11),0.5)/12
temp.trend.est = filter(temp_in_sg,filter = weights, sides = 2)
plot(temp_in_sg)
lines(temp.trend.est, col = "red", lwd = 3)

#difference between temp_in_sg and its estimate trend
library(forecast)
plot(temp_in_sg - temp.trend.est)
seasonplot(temp_in_sg-temp.trend.est, s = 12, col = 1:12)

#exercise4: forecast  the monthly temperature
#Naive forecast 0_0
fcast <- forecast(fit,method = "naive")
plot(fcast)

