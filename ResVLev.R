# Standardised resids vs Leverage

head(anscombeplus)

lma <- lm(y1 ~ x1, data = anscombeplus)
lme <- lm(y5 ~ x1, data = anscombeplus)
lmf <- lm(y6 ~ x2, data = anscombeplus)

library(MASS) # To compute leverages
cbind(hatvalues(lma), hatvalues(lme), hatvalues(lmf))
plot(anscombeplus$x2, anscombeplus$y6); abline(lmf, col = "dodgerblue")

#To compute studentised residuals.
stres <- cbind(studres(lma), studres(lme), studres(lmf))
which(stres >= abs(2))
