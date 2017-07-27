#exercise 3
#1. generate drifted random walk: yk = yk-1 + delta + wk
Tlength <- 200
white_noise <- rnorm(Tlength,mean = 0,sd=1) #wk
delta = 1
RW = cumsum(white_noise + delta) #yk
plot.ts(RW, type="o",main="Drifted Random Walk")

#2. DES :Holt's Algorithm
library(fpp)
fit_optimal <- holt(RW[1:150],initial = "optimal",h=50)
print(fit_optimal$model$par)
plot.ts(RW,main = "fit_optimal")
lines(fitted(fit_optimal),col = "red")

#3. make some forecast Fk for time 150<k<200 and 
##  compare it to the truth
## the HOLT estimate is a line
plot.ts(RW,main = "Forcast:random walk[151:200]")
lines(fit_optimal$mean,col = "red",type = "l",lwd =2)