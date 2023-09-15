rm(list=ls(all.names =  TRUE))
gc()

#Compare model with random slopes and without random slope.
#Intercepts are always random.

library(nlme)
library(readxl)
library(lattice)
library(tidyverse)

Data0 <-read_excel("Input SP vs time on ART .xlsx", sheet=1)

Data1=Data0
Data1=filter(Data0,(Group != "d60")) #without group d60 

#fitting without day 60 group -------------------- 

print("Model with random group effect;no d60"  )

ctrl <- lmeControl(opt='optim');
m1a=lme(SP_VL~1+Time_on_Rx, random = list(Group = pdDiag(~ Time_on_Rx)), data=Data1,control=ctrl,method="ML")
summary(m1a)
anova(m1a)
qqnorm(resid(m1a))
qqline(resid(m1a), col = "steelblue", lwd = 2)
plot(m1a)

print("=====Model without random no group effect; no d60========" )

ctrl <- lmeControl(opt='optim');
m1b=lme(SP_VL~1+Time_on_Rx, random =~ 1|Group, data=Data1,control=ctrl,method="ML")
summary(m1b)
anova(m1b)
qqnorm(resid(m1b))
qqline(resid(m1b), col = "steelblue", lwd = 2)
plot(m1b)

print("comparison of models with no group effect and with the group effect; no d60" )
anova(m1b,m1a)
#--------------------------------------------------
print("=======Model with random group effect;with d60==========="  )

ctrl <- lmeControl(opt='optim');
m2a=lme(SP_VL~1+Time_on_Rx, random = list(Group = pdDiag(~ Time_on_Rx)), data=Data0,control=ctrl,method="ML")
summary(m2a)
anova(m2a)
qqnorm(resid(m2a))
qqline(resid(m2a), col = "steelblue", lwd = 2)
plot(m2a)

print("Model withot random group effect;with d60"  )
ctrl <- lmeControl(opt='optim');
m2b=lme(SP_VL~1+Time_on_Rx, random =~ 1|Group, data=Data0,control=ctrl,method="ML")
summary(m2b)
anova(m2b)
qqnorm(resid(m2b))
qqline(resid(m2b), col = "steelblue", lwd = 2)
plot(m2b)
print("comparison of models with no group effect and with the group effect; with d60" )
anova(m2b,m2a)



print("fixed effect and coef. of model WITH random group effect;no d60"  )
fixed.effects(m1a)
coef(m1a)

print("fixed effect and coef. of model WITH random group effect;with d60"  )
fixed.effects(m2a)
coef(m2a)

print("fixed effect and coef. of model WITHOUT random group effect; no d60"  )
fixed.effects(m1b)
coef(m1b)

print("fixed effectand coef. of model WITHOUT random group effect; with d60"  )
fixed.effects(m2b)
coef(m2b)
