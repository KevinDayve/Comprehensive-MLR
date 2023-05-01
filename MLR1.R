#Multiple linear regression to Birth weight of babies
library(lmreg)
library(MASS)
data("birthwt")


#Checking the data
head(birthwt)

#Fitting a linear model

linmod <- lm(bwt ~ lwt+smoke+race, data = birthwt)
#Cannot interpret "race", we need to convert it into
#interpretable binaries.

races = as.data.frame(binaries(birthwt$race))
colnames(races) = c("Black", "Other", "White")

linmod_i <- lm(bwt ~ smoke+races$White + races$Black + lwt, data = birthwt)
summary(linmod_i)
