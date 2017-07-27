#exercise5
#read csv data and form time series
tempData <- read.csv("D:/@复旦u/5.大三第一学期@NUS/ST3233 Applied time Series Analysis/Tutorial/Tutorial3/Vivocity.csv",header= TRUE,sep=",")
vivo <- ts(tempData$vivo,frequency = 12,start = c(2006,1))

#plot the time series
plot.ts(vivo,type="l",main = "Holt-Winters: queries of Vivocity",
        ylim = c(0,120),xlim= c(2006,2017))

#triple exponential smoothing: Holt-winter
library(fpp)
fit_hw <- hw(vivo,initial = "optimal",seasonal = "additive",h=50)
#plot fit model
lines(fitted(fit_hw),col = "blue")
#plot the estimation
lines(fit_hw$mean,col = "red",type = "l",lwd = 2)
#predict
plot(fit_hw, lw = 3,xlim = c(2006,2018))
plot(fit_hw$model)
