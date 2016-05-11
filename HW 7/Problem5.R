rm(list=ls())

thetaprior = 0.6
x = c(125, 18, 20, 34)
n = 197
z = thetaprior*x[1]/(2 + thetaprior)
thetastar = (z + x[4])/(z + x[2] + x[3] + x[4])

F = dmultinom(n, n, x)
G = dnorm(thetastar)
F = dnorm(X)

# 
# Y = rep(0, 1000)
# weights = rep(0, 1000)
# 
# for(i in 1:100) {
#   
#     
#     H1 = ifelse(abs(X) > 2, 1, 0)
#     F*H/G
#     Y[i] = sum(F*H/G)/2
# }
# 
# #print(paste("Mean: ",mean(Y/100),"Variance: ",var(Y/100)))
# hist(Y/100, breaks=10, main="Rejection Sampling P(z > 2) With Standard Normal",
#      xlab=paste("P(z > 2): Mean: ",round(mean(Y/100),4),", Variance: ",round(var(Y),4),"* 10^-4" ))