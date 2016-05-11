rm(list=ls())

library(ggplot2)
library(gridExtra)
library(grid)

samplePre = rbeta(10000, 295, 308)
samplePost = rbeta(10000, 289, 333)
sampleDiff = samplePost - samplePre

df.sample = data.frame(x=sampleDiff)
df.sample.censored = data.frame(x=df.sample[sampleDiff > 0,])
posProb = dim(df.sample.censored)[1]/10000

p1 = ggplot(data=df.sample, aes(x=x)) +
  #xlim(0, 100) +
  geom_histogram(data=df.sample, color="black", bins = 100, aes(x=x)) +
  geom_histogram(data=df.sample.censored, color="white", bins = 100, aes(x=x)) +
  labs(title=paste("Drift towards Bush is positive with probability", posProb), x=expression(alpha[post] - alpha[pre]), y="") +
  theme_grey(base_size = 16, base_family="serif")

print(p1)