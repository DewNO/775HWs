rm(list=ls())

library(ggplot2)
library(gridExtra)
library(grid)

sampleControl = rbeta(10000, 40, 636)
sampleTreatment = rbeta(10000, 23, 659)
sampleOdds = (sampleTreatment/(1 - sampleTreatment))/(sampleControl/(1 - sampleControl))

df.sample = data.frame(x=sampleOdds)
#df.sample.censored = data.frame(x=df.sample[sampleOdds > 0,])
#posProb = dim(df.sample.censored)[1]/10000

p1 = ggplot(data=df.sample, aes(x=x)) +
  #xlim(0, 100) +
  geom_histogram(data=df.sample, color="black", bins = 100, aes(x=x)) +
 # geom_histogram(data=df.sample.censored, color="white", bins = 100, aes(x=x)) +
  labs(title="Odds Ratio in Favor of Control", x="Odds Ratio", y="") +
  theme_grey(base_size = 16, base_family="serif")

print(p1)