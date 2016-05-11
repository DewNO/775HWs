rm(list=ls())

mu_0 = 180
tau_0 = 40
sigma = 20
ybar = 150


tau_10 = ((1 / tau_0^2) + (10 / sigma^2))^(-.5)
mu_10 = ((1 / tau_0^2)*mu_0 + (10 / sigma^2)*ybar)/tau_10^(-2)

confint_theta10L = mu_10 - 1.960*tau_10
confint_theta10R = mu_10 + 1.960*tau_10

confint_ytilde10L = mu_10 - 1.960*( sqrt(tau_10^2 + sigma^2))
confint_ytilde10R = mu_10 + 1.960*( sqrt(tau_10^2 + sigma^2))

tau_100 = ((1 / tau_0^2) + (100 / sigma^2))^(-.5)
mu_100 = ((1 / tau_0^2)*mu_0 + (100 / sigma^2)*ybar)/tau_100^(-2)

confint_theta100L = mu_100 - 1.960*tau_100
confint_theta100R = mu_100 + 1.960*tau_100

confint_ytilde100L = mu_100 - 1.960*( sqrt(tau_100^2 + sigma^2))
confint_ytilde100R = mu_100 + 1.960*( sqrt(tau_100^2 + sigma^2))
