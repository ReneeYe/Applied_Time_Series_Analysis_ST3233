tlength <- 500
white_noise <- rnorm(tlength, mean = 0, sd = 1)
plot.ts(white_noise)

#xk = sin(K)+wK
xk <- sin(1:tlength) + rnorm(tlength, mean = 0, sd = 1)
plot.ts(xk)

acf(xk, 20)
