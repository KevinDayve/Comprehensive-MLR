#Quadratic model fitting;
library(lmreg)
data("girlgrowth")

lmgirl = lm(Height ~ poly(Age, 2, raw = T), data = girlgrowth)
summary(lmgirl)

#Fitted equation is 56.78 + 10.45 * Age^2
plot(girlgrowth, cex = 0.4,
     xlab = "Age",
     ylab = "Height")
xgrid = seq(7, 12, 0.1)
yfitgrid = lmgirl$coef[1] + lmgirl$coef[2] * xgrid + 
  lmgirl$coef[3] * xgrid^2
lines(xgrid, yfitgrid, col = "blue")

condmeans = NULL
for (i in 7:12) {
  condmeans <- c(condmeans, mean(girlgrowth$Height[girlgrowth$Age == i]))
}
Ages = c(7:12)
points(Ages, condmeans, col = "dodgerblue")