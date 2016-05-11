rm(list=ls())

thetaprior = 0.6
x = c(125, 18, 20, 34)
n = 197
z = thetaprior*x[1]/(2 + thetaprior)

thetastar = (z + x[4])/(z + x[2] + x[3] + x[4])


MLE = rep(0, 400)


for(i in 1:400) {
  
  sample = rmultinom(n, n, x)
  z = thetastar*sample[1]/(2 + thetastar)
  
  MLE[i] = (z + sample[4])/(z + sample[2] + sample[3] + sample[4])
  

}

hist(MLE, breaks=10, main="Histogram of Simulated MLEs",
      xlab=paste(" Mean: ", round(mean(MLE),4), ", Standard deviation: ",round(sd(MLE),6)))