rm(list=ls())

Y = rep(0, 100)
vars = rep(0, 100)
for(i in 1:100) {
#   X = rnorm(100)
#   F = dnorm(X)
#   H = ifelse(abs(X) > 2, 1, 0)
#   
#   Y[i] = sum(H)/2
    
  
  X = rcauchy(100)
  G = dcauchy(X)
  F = dnorm(X)
  W = F/G
  H = ifelse(abs(X) > 2, 1, 0)
  Y[i] = sum(W*H)/(2*sum(W))
  vars[i] = sum(W^2*(H - Y[i])^2)/sum(W)
  
  #F*H/G
  #Y[i] = sum(F*H/G)/2
}

#print(paste("Mean: ",mean(Y/100),"Variance: ",var(Y/100)))
hist(Y, breaks=10, main="Importance Sampling P(z > 2) With Cauchy",
     xlab=paste("P(z > 2): Mean: ",round(mean(Y),5),", Variance: ",round(var(Y),8) ))