#Fitting a simple model to data.
library(lmreg)
data("girlgrowth")
plot(girlgrowth, cex = 0.4,
     xlab = "Age (years)",
     ylab = "Height (cm)")

model <- lm(Height ~ Age, data = girlgrowth)
#Evaluating the fitted model.
#Fitted model: 77.994 + 5.836 * Age
summary(model)

#Visualising a straight line.
abline(model)
#Correlation^2 is similar to R^2 in case of simple 
#linear regression.
cor(girlgrowth$Age, girlgrowth$Height)^2

#Conditional means
condmeans = NULL
for (i in 7:12) {
  condmeans = c(condmeans,
                mean(girlgrowth$Height[girlgrowth$Age==i]))
}
points(7:12,condmeans,col="red",pch=16)
lines(7:12,condmeans,col="red")
