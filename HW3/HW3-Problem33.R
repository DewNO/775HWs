rm(list=ls())

library(ggplot2)
library(gridExtra)
library(grid)


sampleControl_t = rt(n = 10000, df = 31)
sampleTreatment_t = rt(n = 10000, df = 35)
meanControl=1.013
meanTreatment=1.173
sControl = 0.24
sTreatment = 0.20
nControl = 32
nTreatment= 36

sampleControl = sampleControl_t*sControl/sqrt(nControl) + meanControl
sampleTreatment = sampleTreatment_t*sTreatment/sqrt(nTreatment) + meanTreatment

sampleDiff = sampleTreatment - sampleControl

df.sample = data.frame(x=sampleDiff)


#quantiles = quantile(sampleDiff, probs = c(0.25, 0.5, 0.75))
quantiles = quantile(sampleDiff, probs = c(0.05, 0.95))

df.sample.censoredLeft = data.frame(x=df.sample[which(sampleDiff < min(quantiles)),])
df.sample.censoredRight = data.frame(x=df.sample[which(sampleDiff > max(quantiles)),])

p1 = ggplot(data=df.sample, aes(x=x)) +
  #xlim(0, 100) +
  geom_histogram(data=df.sample, color="black", bins = 100, aes(x=x)) +
  geom_histogram(data=df.sample.censoredLeft, color="white", bins = 100, aes(x=x)) +
  geom_histogram(data=df.sample.censoredRight, color="white", bins = 100, aes(x=x)) +
  #geom_vline(xintercept = quantiles, color="white") +
  geom_vline(xintercept = quantiles, color="black") +
  labs(title="Calcium Flow Increase In Treatment Group", x=expression(mu[treatment] - mu[control]), y="") +
  theme_grey(base_size = 16, base_family="serif")

print(p1)