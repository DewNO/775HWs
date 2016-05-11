rm(list=ls())

library(ggplot2)
library(gridExtra)
library(grid)

res_br_bikes = c(16, 9, 10, 13, 19, 20, 18, 17, 35, 55)
res_br_other = c(58, 90, 48, 57, 103, 57, 86, 112, 273, 64)
res_br_total = res_br_bikes + res_br_other
y1 = res_br_bikes/res_br_total
br_mean_num = mean(res_br_total)
br_mean_proportion = mean(y1)
prior_br = c(4, 16)
posterior_br = prior_br + c(sum(res_br_bikes), sum(res_br_other))

posterior_sample_br = rbeta(1000, posterior_br[1], posterior_br[2])
df.br = data.frame(br=posterior_sample_br)

res_nr_bikes = c(12, 1, 2, 4, 9, 7, 9, 8)
res_nr_other = c(113, 18, 14, 44, 208, 67, 29, 154)
res_nr_total = res_nr_bikes + res_nr_other
z1 = res_nr_bikes/res_nr_total
nr_mean_num = mean(res_nr_total)
nr_proportion = mean(z1)

prior_nr = c(2, 18)
posterior_nr = prior_nr + c(sum(res_nr_bikes), sum(res_nr_other))

posterior_sample_nr = rbeta(1000, posterior_nr[1], posterior_nr[2])
df.nr = data.frame(nr=posterior_sample_nr)

df.diff = data.frame(diff = (posterior_sample_br - posterior_sample_nr))

p1 = ggplot(data=df.diff, aes(x=diff)) +
  
  geom_histogram(data=df.diff, color="black", bins = 100, aes(x=diff)) +
  labs(title="Difference (bike route vs. no bike route)", x="Differential in proportions", y="Count") +
  theme_grey(base_size = 24, base_family="serif")

print(p1)