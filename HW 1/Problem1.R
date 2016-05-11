rm(list=ls())

d_prior = c(0.45, 0.14, 0.03, 0.06, 0.12, 0.20)

LVH_lh = c(0.10, 0.15, 0.12, 0.90, 0.05, 0.10)

LVH_marginal = sum(d_prior*LVH_lh)
#print(d_prior*LVH_lh)
LVH_posterior = (d_prior*LVH_lh)/LVH_marginal


report_lh = 0.90*LVH_lh + 0.05*(1 - LVH_lh)

report_plh = report_lh * d_prior

report_marginal = sum(report_plh)

report_posterior = report_plh/report_marginal