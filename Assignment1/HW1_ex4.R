# Exercise4(Yule-Walker)
## 这个应该是AR model,不是MA model

# 1. estimate the first three autocorrelation coefficients
AR3_data <- read.table("D:/@复旦u/5.大三第一学期@NUS/1.学习/ST3233 Applied time Series Analysis/Assignments/Assignment1/asg_1_MA3.dat")
AR3 <- ts(AR3_data)
acf(AR3,lag.max = 20,lwd = 3, main ="AR(3)::ACF = 10000")
AR3_acf <- acf(AR3,lag.max=20)
AR3_acf$acf[1:4]
#result is: 1.0000000 0.4198046 0.3719330 0.1622123
# so the first three autocorrelation coefficients are: 0.4198046, 0.3719330, 0.1622123

# 2. (Non-programming) Yule-Walker equations:
# Yule-Walker equations for estimating alpha, beta, gamma:
## p(k) = alpha * p(k-1) + beta * p(k-2) + gamma * p(k-3)

# 3: find the estimation of alpha, beta, gamma by solving the equations:
## equations (Note: p(k) = p(-k)):
##    p(0) = 1
##    p(1) = alpha * p(0) + beta * p(1) + gamma * p(2)
##    p(2) = alpha * p(1) + beta * p(0) + gamma * p(1)
##    p(3) = alpha * p(2) + beta * p(1) + gamma * p(0)

install.packages("rootSolve")
library(rootSolve)
p0 <- 1
p1 <- AR3_acf$acf[2]
p2 <- AR3_acf$acf[3]
p3 <- AR3_acf$acf[4]
model <- function(x){
  f1 <- x[1] * p0 + x[2] * p1 + x[3] * p2 - p1
  f2 <- x[1] * p1 + x[2] * p0 + x[3] * p1 - p2
  f3 <- x[1] * p2 + x[2] * p1 + x[3] * p0 - p3
  c(f1=f1,f2=f2,f3=f3)
}
solution <- multiroot(f = model, start = c(0,0,0))
solution$root
#result: 0.33736221  0.26085723 -0.07277295
# So, the estimations are : alpha = 0.33736221, beta = 0.26085723, gamma = -0.07277295

