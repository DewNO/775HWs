rm(list=ls())

library(ggplot2)
library(gridExtra)
library(grid)

year = seq(1976, 1985, 1)  
accidents = c(24, 25, 31, 31, 22, 21, 26, 20, 16, 22)
deaths = c(734, 516, 754, 877, 814, 362, 764, 809, 223, 1066)
rate = c(0.19, 0.12, 0.15, 0.16, 0.14, 0.06, 0.13, 0.13, 0.03, 0.15)
miles = 100*deaths/rate


posterior.sample.a = rnbinom(1000, sum(accidents) + 60, 10 + 3)
a.quantiles = quantile(posterior.sample.a, probs = c(0.05, 0.95))
print(a.quantiles)
posterior.sample.b = rgamma(1000, sum(accidents) + 20, sum(miles) + 500000)
b.quantiles = quantile(posterior.sample.b, probs = c(0.05, 0.95))
print(b.quantiles)
#posterior.sample.a = rgamma(1000, sum(accidents) + 60, 10 + 3)


df.sample = data.frame(x=posterior.sample.a)

#likelihood_unnorm[]


# p1 = ggplot(data=df.posterior, aes(x=x)) +
#   
#   geom_line(data=df.posterior, color="blue", aes(x=x, y=y)) +
#   labs(title="Unnormalized Posterior Density", x="theta", y="") +
#   theme_grey(base_size = 16, base_family="serif")
# 
# 
# p2 = ggplot(data=df.posterior, aes(x=x)) +
#   xlim(0, 100) +
#   geom_histogram(data=df.sample, color="black", binwidth=0.01, aes(x=x)) + #, ..density..)) +
#   labs(title="n = 1000 draws from Posterior", x="theta", y="") +
#   theme_grey(base_size = 16, base_family="serif")

p3 = ggplot(data=df.sample, aes(x=x)) +
  geom_histogram(data=df.sample, color="black", binwidth=0.1, aes(x=x)) + #, ..density..)) +
  labs(title="n = 1000 draws from Posterior", x="theta", y="") +
  theme_grey(base_size = 16, base_family="serif")

print(p3)

# p1 <- ggplot_gtable(ggplot_build(p1))
# p2 <- ggplot_gtable(ggplot_build(p2))
# p3 <- ggplot_gtable(ggplot_build(p3))
# 
# maxWidth = unit.pmax(p1$widths[2:3], p2$widths[2:3], p3$widths[2:3])
#  
#  p1$widths[2:3] <- maxWidth
#  p2$widths[2:3] <- maxWidth
#   p3$widths[2:3] <- maxWidth
# 
# 
# p = grid.arrange(p1, p2, p3)


#print(p)