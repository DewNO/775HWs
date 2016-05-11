library(ggplot2)
library(grid)
library(gridExtra)
rm(list=ls())

mu = seq(length.out= 100, from=0, to=10)
lambda = seq(length.out = 100, from=0, to=10)

likelihood = function(x, y, W, B) {
  lh = (1/(factorial(W)*factorial(B)))*((x + y)^W)*((y)^B)*exp(-x - 2*y)
  return(lh)
}
  
df.mu = rep(mu, 100)
df.lambda = rep(lambda, each=100)
df.lh = likelihood(df.mu, df.lambda, W=5, B=2)
df.lh2 = likelihood(df.mu, df.lambda, W=3, B=4)
df = data.frame(mu=df.mu, lambda=df.lambda, lh=df.lh, lh2=df.lh2)

p1 = (ggplot(df, aes(x=mu, y=lambda, z=lh)) 
 +  stat_contour()
 + labs(title="W = 5, B = 2")
)

p2 = (ggplot(df, aes(x=mu, y=lambda, z=lh2)) 
      +  stat_contour()
      + labs(title="W = 3, B = 4")
)

grid.arrange(p1, p2, ncol=2)
