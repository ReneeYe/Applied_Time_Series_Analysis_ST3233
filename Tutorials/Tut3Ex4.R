#exercise4
#read csv data and form time series
tempData <- read.csv("D:/@复旦u/5.大三第一学期@NUS/ST3233 Applied time Series Analysis/Tutorial/Tutorial3/durian.csv",header= TRUE,sep=",")
durian <- ts(tempData$query,frequency = 12,start = c(2004,1))

#plot the time series
plot.ts(durian,type="l",main = "Holt-Winters: queries of durian",xlim= c(2004,2018))

#triple exponential smoothing: Holt-winter
library(fpp)
fit_hw <- hw(durian,initial = "optimal",seasonal = "additive",h=50)
#plot fit model
lines(fitted(fit_hw),col = "blue")
#plot the estimation
lines(fit_hw$mean,col = "red",type = "l",lwd = 2)
#predict
plot(fit_hw, lw = 3,xlim = c(2004,2018))
plot(fit_hw$model)
