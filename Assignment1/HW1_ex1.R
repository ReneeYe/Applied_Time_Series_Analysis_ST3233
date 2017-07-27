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
seasonplot(electricity-fit$time.series[,"trend"], s = 24*7, col = 1:24, main = "Seasonplot of electricity demand",type = "l")
#explain why the time series is not perfectly seasonal:
## Form the decomposition plot of the time series, we find that some remainders are large

#5. TES to make a forecast for the next 3 weeks
electricity_hw <- hw(electricity, initial = "simple",seasonal = "additive", h = 24*7*3)
plot(electricity,main = "The Predicted Hourly Electricity Demand",ylab="Electricity Demand(Hourly)",xlim = c(0,27),lwd = 2)
lines(electricity_hw$mean, col = "blue", type ="l", lwd = 1)
