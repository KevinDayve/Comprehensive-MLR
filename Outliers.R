# Standardised vs residual plots
library(lmreg)
data("anscombeplus")
head(anscombeplus)

#Three linear models
lma <- lm(y1 ~ x1, data = anscombeplus)
lme <- lm(y5 ~ x1, data = anscombeplus)
lmf <- lm(y6 ~ x2, data = anscombeplus)

library(MASS)
plot(hatvalues(lme), stdres(lme))
plot(lme, which = 5, cook.levels = c(),
     add.smooth = F, id.n = 0)
abline(h = 2, lty = 3); abline(h = -2, lty = 3)
n <- length(anscombeplus$x1); k = 1
abline(v = 2*(k+1)/n, lty = 3)

cases <- 1:n
hihi <- which(hatvalues(lme) > 2*(k+1)/n)
hiri <- which(abs(stdres(lme))>2)
mark <- unique(c(hihi, hiri))
text(hatvalues(lme)[mark], stdres(lme)[mark],
     cases[mark], pos = 2)