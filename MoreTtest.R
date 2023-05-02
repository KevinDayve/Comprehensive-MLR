#More complex question answered with the help of
#T test.

# Galton Families data 
library(HistData)
# select cases of first born sons
data(GaltonFamilies)
head(GaltonFamilies)
help(GaltonFamilies)
y <- GaltonFamilies$childHeight[GaltonFamilies$gender=="male" &
                                  GaltonFamilies$childNum==1]
x1 <- GaltonFamilies$father[GaltonFamilies$gender=="male" &
                              GaltonFamilies$childNum==1]
x2 <- GaltonFamilies$mother[GaltonFamilies$gender=="male" &
                              GaltonFamilies$childNum==1]
lmGF1 <- lm(y~x1+x2)
lmGF1
# Test whether father's height has a stronger effect, 
# in relation to mother's height, on height of first born son 
library(lmreg)
help(hyptest)

p <- c(0,1,-1)
hyptest(lmGF1, p, type = "upper")
