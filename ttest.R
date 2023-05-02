library(lmreg)
data("girlgrowth")
head(girlgrowth)
lmgirl1 = lm(Height~Age,data=girlgrowth)
summary(lmgirl1)$coeff


lmgirl2 = lm(Height~poly(Age,2,raw=T),data=girlgrowth)
summary(lmgirl2)


# Birth weight of babies
library(MASS)
data("birthwt")
head(birthwt)
races = as.data.frame(binaries(birthwt$race)) 
colnames(races) = c("Black","Other","White")
head(races)
lmbwt2 = lm(bwt~smoke+races$White+races$Black+lwt,data=birthwt)
summary(lmbwt2)

lmbwt3 = lm(bwt~smoke+races$White+lwt,data=birthwt)
summary(lmbwt3)