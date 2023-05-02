# Six sets of synthetic data with similar outcome 
# Of routine regression analysis.

library(lmreg)
data("anscombeplus")
head(anscombeplus)

#The same old drill. Remember it is always a good idea to look at 
#Plots 

a <- lm(y1 ~ x1, data = anscombeplus)
b <- lm(y2 ~ x1, data = anscombeplus)
c <- lm(y3 ~ x1, data = anscombeplus)
d <- lm(y4 ~ x1, data = anscombeplus)
e <- lm(y5 ~ x1, data = anscombeplus)
f <- lm(y6 ~ x2, data = anscombeplus)

summary(a)$r.sq; summary(b)$r.sq
summary(c)$r.sq; summary(d)$r.sq
summary(e)$r.sq; summary(f)$r.sq

#As far as routine analysis is concerned, the results are identical.
#Lets eye ball the data through a plot.
par(mfrow = c(3, 2))
plot(anscombeplus$x1, anscombeplus$y1); abline(a, col = "red")
plot(anscombeplus$x1, anscombeplus$y2); abline(a, col = "red")
plot(anscombeplus$x1, anscombeplus$y3); abline(a, col = "red")
plot(anscombeplus$x1, anscombeplus$y4); abline(a, col = "red")
plot(anscombeplus$x1, anscombeplus$y5); abline(a, col = "red")
plot(anscombeplus$x2, anscombeplus$y6); abline(a, col = "red")
