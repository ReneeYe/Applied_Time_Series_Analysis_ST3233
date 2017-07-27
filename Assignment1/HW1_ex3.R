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
fit2 <- decompose(aircon_GTrend, type = "additive", filter = NULL)
plot(fit2)

#using TES to estimate the queries containing "aricon" in Jun/2017
library(forecast)
aircon_hw <- hw(aircon_GTrend, initial = "optim", seasonal = "additive", h = 20)
plot(aircon_GTrend,main = "The Predicted queries of aircon",ylab="Queries",xlim = c(2004, 2018),lwd = 3)
lines(fitted(aircon_hw),col = "blue", type = "l", lwd = 2)
lines(aircon_hw$mean, col = "red", type ="l", lwd = 3)
plot(aircon_hw,lw = 3)

aircon_hw
# Get following result:
#         Point Forecast    Lo 80     Hi 80    Lo 95     Hi 95 #
#Jun 2017       90.08184 81.25155  98.91213 76.57707 103.58660 #

aircon_hw_Jun17 <- aircon_hw$mean[8]

# using purchase_rate to predict the sale of aircon in Jun/2017, given the sale in Jun2016 is 51.
aircon_GTrend_Jun16 <- aircon_GTrend[12*12+6]
aircon_GTrend
aircon_sold_Jun16 <- 51
purchase_rate <- aircon_sold_Jun16 / aircon_GTrend_Jun16

purchase_rate

aircon_sold_Jun17.est <- aircon_hw_Jun17 * purchase_rate
aircon_sold_Jun17.est

aircon_sold_Jun17.lower <- 76.57707 * purchase_rate
aircon_sold_Jun17.upper <- 103.58660 * purchase_rate
aircon_sold_Jun17.lower
aircon_sold_Jun17.upper

# The predicted sale in Jun2017 is 65, with confidence interval [57.4328,77.68995] at 95% level.
