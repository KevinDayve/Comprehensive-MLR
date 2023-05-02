# Galton Families data 
library(HistData)
# select cases of first born sons
data(GaltonFamilies)
head(GaltonFamilies)
y <- GaltonFamilies$childHeight[GaltonFamilies$gender=="male" &
                                  GaltonFamilies$childNum==1]
# In regression of first born son's height on midparentHeight,
# test whether regression coefficient is greater than one 
help(GaltonFamilies)
x <- GaltonFamilies$midparentHeight[GaltonFamilies$gender=="male" &
                                      GaltonFamilies$childNum==1]
lmGF2 <- lm(y~x)
lmGF2
p <- c(0,1)
hyptest(lmGF2, p, xi = 1, type = "upper")
hyptest(lmGF2, p, xi = 1, type = "lower")