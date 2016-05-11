rm(list=ls())

library(ggplot2)
library(gridExtra)
library(grid)

prior = 0.01
data = c(43, 44, 45, 46.5, 47.5)
m = 100
grid_x = seq(0, 100, length = 100*m + 1)

likelihood_unnorm = rep(prior, 100*m + 1)
for(i in 1:5)
{
  likelihood_unnorm = likelihood_unnorm * 1/(1 + (data[i] - grid_x)^2)
}

df.posterior = data.frame(x=grid_x, y=likelihood_unnorm)

likelihood_norm = likelihood_unnorm/sum(likelihood_unnorm)

sample = sample(x=grid_x, size = 1000, replace = TRUE, prob=likelihood_norm)
sampley = sapply(sample, FUN=function(x) {rcauchy(1, location = x, scale = 1)})

df.sample = data.frame(y = sampley, x = sample)

#likelihood_unnorm[]


p1 = ggplot(data=df.posterior, aes(x=x)) +
  
  geom_line(data=df.posterior, color="blue", aes(x=x, y=y)) +
  labs(title="Unnormalized Posterior Density", x="theta", y="") +
  theme_grey(base_size = 16, base_family="serif")


p2 = ggplot(data=df.posterior, aes(x=x)) +
  xlim(0, 100) +
  geom_histogram(data=df.sample, color="black", binwidth=0.01, aes(x=x)) + #, ..density..)) +
  labs(title="n = 1000 draws from Posterior", x="theta", y="") +
  theme_grey(base_size = 16, base_family="serif")

p3 = ggplot(data=df.posterior, aes(x=x)) +
  #xlim(0, 100) +
  geom_histogram(data=df.sample, color="black", binwidth=0.1, aes(x=y)) + #, ..density..)) +
  labs(title="n = 1000 draws from Posterior", x="theta", y="") +
  theme_grey(base_size = 16, base_family="serif")

print(p3)

p1 <- ggplot_gtable(ggplot_build(p1))
p2 <- ggplot_gtable(ggplot_build(p2))
p3 <- ggplot_gtable(ggplot_build(p3))

maxWidth = unit.pmax(p1$widths[2:3], p2$widths[2:3], p3$widths[2:3])
 
 p1$widths[2:3] <- maxWidth
 p2$widths[2:3] <- maxWidth
  p3$widths[2:3] <- maxWidth


p = grid.arrange(p1, p2, p3)


#print(p)