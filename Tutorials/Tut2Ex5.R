#exercise 5 Oil and Gas

#1. load the dataset and plot two time series- oil and gas- on the same plot
load("D:/@复旦u/5.大三第一学期@NUS/ST3233 Applied time Series Analysis/Datasets/tsa3.rda")
par(mfrow = c(2,1))
plot.ts(oil/oil[1],col = "blue",main = "Oil and Gas")
lines(gas/gas[1],col="red")
#from the graph, we can see that gas and oil are highly related.


#2. log(xk/xk-1) = (xk-xk-1)/xk-1
# the first way to get poil and pgas - taught by XTN using vector and the function
poil <- (oil[-1]+oil[-length(oil)])/oil[-length(oil)]
plot.ts(poil)
pgas <- (gas[-1]+gas[-length(gas)])/gas[-length(gas)]
plot.ts(pgas)

# the second way to get poil and pgas - using diff() introduced in class
poil <- diff(log(oil))
plot.ts(poil,main ="Poil")
pgas <- diff(log(gas))
plot.ts(pgas,main = "Pgas")

#3. autocorrelation
acf(poil,30)
acf(pgas,30)

#4. correlation
cor(oil,gas)

#5. scatter plot: poil(k) and pgas(k-l) //l = 1,2,3
pgas<-ts(pgas) 
par(mfrow = c(2,2))
plot(poil,lag(pgas,1))
plot(poil,lag(pgas,2))
plot(poil,lag(pgas,3))