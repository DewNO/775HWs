rm(list=ls())

# theta_prior = c(0,1,2,3,4,5,6,7,8,9,10)*(1/55)

theta_prior = rep(.01, times=11)
theta_prior[6] = .90

theta = c(0,1,2,3,4,5,6,7,8,9,10)*.1
theta_lh = (theta**18)*((1 - theta)**3)

theta_marginal = sum(theta_prior*theta_lh)

theta_posterior = (theta_prior*theta_lh)/theta_marginal

theta_exp = theta_posterior*theta

theta_hat = sum(theta_exp)

# report_lh = 0.90*LVH_lh + 0.05*(1 - LVH_lh)
# 
# report_plh = report_lh * d_prior
# 
# report_marginal = sum(report_plh)
# 
# report_posterior = report_plh/report_marginal