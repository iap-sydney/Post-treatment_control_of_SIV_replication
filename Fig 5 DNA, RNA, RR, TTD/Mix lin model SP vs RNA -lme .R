rm(list=ls(all.names =  TRUE))
gc()

#Compare model with random slopes and without random slope.
#Intercepts are always random.


library(nlme)
library(readxl)
library(lattice)
library(tidyverse)


data0 <-read_excel("Input SP_vs_RNA.xlsx", sheet=1)
data1=na.omit(data0)

#--------------------------------------------------
print("=======Model with random group effect===========")

ctrl <- lmeControl(opt='optim');
m1a=lme(sp~1+rna, random = list(cohort = pdDiag(~ rna)), data=data1,control=ctrl,method="ML")
summary(m1a)
anova(m1a)
qqnorm(resid(m1a))
qqline(resid(m1a), col = "steelblue", lwd = 2)
plot(m1a)

print("Model without random group effect")
ctrl <- lmeControl(opt='optim');
m1b=lme(sp~1+rna, random =~ 1|cohort, data=data1,control=ctrl,method="ML")
summary(m1b)
anova(m1b)
qqnorm(resid(m1b))
qqline(resid(m1b), col = "steelblue", lwd = 2)
plot(m1b)
print("comparison of models with no group effect and with the group effect" )
anova(m1b,m1a)

print("fixed effect and coef. of model WITH random group effect"  )
fixed.effects(m1a)
coef(m1a)

print("fixed effectand coef. of model WITHOUT random group effec"  )
fixed.effects(m1b)
coef(m1b)
