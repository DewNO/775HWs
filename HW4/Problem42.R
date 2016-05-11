rm(list=ls())

xi = c(-0.86, -0.30, -0.05, 0.73)
ni = c(5, 5, 5, 5) 
yi = c(0, 1, 3, 5)
ylogit = yi/ni


out = glm(ylogit ~ xi, family = binomial, weights=ni)
print(summary(out))

pi = out$fitted
qi = 1 - pi

alpha = out$coefficients[1]
beta = out$coefficients[2]

I11 = sum(ni*pi*qi)
I12 = sum(xi*ni*pi*qi)
I21 = I12
I22 = sum(xi^2*ni*pi*qi)

I = matrix(data = c(I11, I12, I21, I22), nrow = 2)
theta = matrix(data = c(alpha, beta), nrow = 1)