#Basic Anova table
library(MASS)

data(birthwt)
library(lmreg)
races = as.data.frame(binaries(birthwt$race))
colnames(races) = c("Noir", "Autre", "Blanche")

#Constant model.
lmbwt <- lm(bwt ~ smoke + races$Blanche, data = birthwt)
summary(lmbwt)


#Rudimentary linear analysis.
lmbwt1 <- lm(bwt ~ 1, data = birthwt)
anova(lmbwt1, lmbwt3)

#For better eyeballing use Hanova.
hanova(lmbwt1, lmbwt3)