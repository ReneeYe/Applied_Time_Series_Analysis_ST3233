#generate an AR(1) model
## using arima.sim function
length_T = 50
AR1 <-  arima.sim(n=length_T,list(ar=c(1/2)),sd =1)
plot(AR1)

estimate_alpha <- function(time_series){
  return(acf(time_series),plot=False)$
}