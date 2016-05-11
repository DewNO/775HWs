library(ggplot2)
rm(list=ls())

x = seq(from = 0, 1, length.out=1000)

betaprior = 0.5*x^(9)*(1-x)^19*gamma(30)/(gamma(10)*gamma(20)) +
  0.2*x^(14)*(1-x)^14*gamma(30)/(gamma(15)*gamma(15)) +
  0.3*x^(19)*(1-x)^9*gamma(30)/(gamma(20)*gamma(10)) 
  
betaposterior = 0.5*x^(23)*(1-x)^55*gamma(80)/(gamma(24)*gamma(56)) +
  0.2*x^(28)*(1-x)^50*gamma(80)/(gamma(29)*gamma(51)) +
  0.3*x^(33)*(1-x)^45*gamma(80)/(gamma(34)*gamma(46)) 

df = data.frame(x=c(x,x), y=c(betaprior, betaposterior), z=c(rep("prior",1000), rep("posterior",1000)))

ggplot(df, aes(x=x, y=y, color=z)) + geom_point() + facet_grid(.~z)
