# Dependence of transaction time on two counts of types of transactions.
#Reformulation part un.
library(alr4)
data(Transact)
help(Transact)
lmtransact1 = lm(time~t1+t2,data=Transact)
summary(lmtransact1)
# How do we test if both types have same effect?
# Test if Î²1 = Î²2
# What does the hypothesis Î²1 = Î²2 imply?
library(lmreg)
tr = Transact$t1+Transact$t2
lmtransact0 = lm(time~tr,data=Transact)  # simpler model
hanova(lmtransact0,lmtransact1)


#Reformulation part deux.
data("worldrecord")
plot(worldrecord$Distance, worldrecord$MenRecord)
plot(worldrecord$Distance, worldrecord$WomenRecord)

logDist = log(worldrecord$Distance)
logMen = log(worldrecord$MenRecord)
logWomen = log(worldrecord$WomenRecord)

#Check the plot again.
plot(logDist, logMen)
plot(logDist, logWomen)

#Those look more uniformly spaced. Bon.

summary(lm(logDist ~ logMen))$coef
summary(lm(logDist ~ logWomen))$coef

#Construct a combined model with diff intercepts
#But same slope for Men and Women

n = length(logDist)
logT <- c(logMen, logWomen)
logCombined <- c(logDist, logDist)
gender <- c(rep(1, n), rep(0, n))
cbind(logT, gender, logCombined)

#Creating a simple model
lmsimple <- lm(logT ~ gender + logCombined)
summary(lmsimple)$coef