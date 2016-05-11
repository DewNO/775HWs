rm(list=ls())

n = seq(203, 5000000)
#prior_lh = (1/n)*(99/100)^n
prior_lh = rep(10, length(n))*(1/n)

normal = sum(prior_lh)

posterior = prior_lh/normal
mean = sum(posterior*n)
var = sum( posterior*(n^2)) - mean^2

sd = sqrt(var)