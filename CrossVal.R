# Prediction error specification through Cross validation
# Velocity of Cosmic objects.

library(lmreg)
data(stars1)

lmstars <- lm(Velocity ~ Distance, data = stars1)

#Sample size and setting number of regressors.
n <- length(stars1$Distance); k = 1

#Response variable.
y <- stars1$Velocity
#Explanatory variable
x <- stars1$Distance

lm(y ~ x)

#Computing root mean square of prediction through Cross Validation

CrossValSumsq <- 0
for(i in 1:n) {
  b0 <-  lm(y[-i] ~ x[-i])$coef[1]
  b1 <- lm(y[-i] ~ x[-i])$coef[2]
  CrossValSumsq <- CrossValSumsq + (y[i] - (b0 + b1 * x[i]))
}

RMSEPCrossVal <- sqrt(CrossValSumsq / n)

#Model based prediction error variance for an observation
#is sigma^2 * (1 + leverage)

RMSEPModel <- sqrt((1 + (k+1)/n) * (summary(lmstars)$sig^2))

#Plotting to have a visual grasp of the data.
plot(stars1)
abline(lmstars)


#Error would be much more for distant stars.

data(stars2)
newdata <- data.frame(Distance = stars2$Distance)
predstars <- predict(lmstars, newdata, interval = "prediction", level = 0.95)
prederror <- stars2$Velocity - predstars[, 1]

#RMSEP
sqrt(mean(prederror)^2)

#This little practical experiment shows us both the wins and Ls of 
#the Cross Validation technique.
#So it is quite successful when you're using it for 
#data that looked like the observed data. 
#And may not be as useful when 
#it is used to extrapolate in a far different area of the space.