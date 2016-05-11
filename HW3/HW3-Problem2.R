rm(list=ls())

library(ggplot2)
library(gridExtra)

u1 = runif(1000)
u2 = runif(1000)
u3 = runif(1000)
u4 = runif(1000)

x1 = -log(u1)
x2 = (-log(u2))^(1/3)
x3 = tan(pi*(u2 - 1/2))


CDF4_root = function(param) { 

  seed_u = 20
  u = seed_u
  value_u = param*u^4 - 4*u + 3
  deriv_u = 4*param*u^3 - 4
  while( abs(value_u)> 10^(-10) )
  {
    u = u - value_u/deriv_u
    value_u = param*u^4 - 4*u + 3
    deriv_u = 4*param*u^3 - 4
  }
  return(u)
}

x4 = sapply(u4, FUN=CDF4_root)



values4 = u4*(x4^4) - 4*x4 + 3

x4 = (x4 - 1)*(3/2)

xexp = seq(0, max(x1), length=1000)
xweibull = seq(0, max(x2), length=1000)
xcauchy = seq(quantile(x3, 0.025), quantile(x3, 0.975), length=1000)
xF = seq(0, max(x4), length=1000)

yexp = dexp(xexp)
yweibull = dweibull(xweibull, 3)
ycauchy = dcauchy(xcauchy)
yF = df(xF, 4, 6)

df.exp = data.frame(x = xexp, y = yexp)
df.weibull = data.frame(x = xweibull, y = yweibull)
df.cauchy = data.frame(x = xcauchy, y = ycauchy)
df.F = data.frame(x = xF, y = yF)

df.x1 = data.frame(x=x1)
df.x2 = data.frame(x=x2)
df.x3 = data.frame(x=x3)
df.x4 = data.frame(x=x4)

p1 = ggplot(data=df.exp) +
    geom_freqpoly(data=df.x1, color="black", binwidth=0.05, aes(x=x, ..density..)) +
    geom_line(data=df.exp, color="blue", aes(x=x, y=y)) +
    labs(title="Exponential (mean 1)") +
    theme_grey(base_size = 16, base_family="serif")

p2 = ggplot(data=df.weibull) +
  geom_freqpoly(data=df.x2, color="black", binwidth=0.05, aes(x=x, ..density..)) +
  geom_line(data=df.weibull, color="blue", aes(x=x, y=y)) +
  labs(title="Weibull (shape = 3)") +
  theme_grey(base_size = 16, base_family="serif")

p3 = ggplot(data=df.cauchy) +
  geom_freqpoly(data=df.x3, color="black", binwidth=0.05, aes(x=x, ..density..)) +
  geom_line(data=df.cauchy, color="blue", aes(x=x, y=y)) +
  labs(title="Standard Cauchy") +
  xlim(quantile(x3, 0.05), quantile(x3, 0.95)) +
  theme_grey(base_size = 16, base_family="serif")

p4 = ggplot(data=df.F) +
  geom_freqpoly(data=df.x4, color="black", binwidth=0.05, aes(x=x, ..density..)) +
  geom_line(data=df.F, color="blue", aes(x=x, y=y)) +
  labs(title="F(4,6)") +
  theme_grey(base_size = 16, base_family="serif")


p = grid.arrange(p1, p2, p3, p4, ncol=2)


print(p)

