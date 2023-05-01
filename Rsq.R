#Rsquare intuition.
n <- 100
set.seed(12345)

mu_x <- 4; mu_y <- 20;
sig_x <- 1;
sig_y <- 2; rho = -0.9

b1 <- rho * (sig_y/sig_x)
b0 <- mu_y - rho*(sig_y/sig_x) * mu_x

sig_e <- sig_y * sqrt(1 - rho^2)
x <- rnorm(n, mean = mu_x, sd = sig_x)
y <- b0 + b1 * x + rnorm(n, sd = sig_e)

plot(x, y)
abline(lm(y ~ x), col = "blue")

#Generating observed vs fitted.
lm1 <- lm(y ~ x)
fity <- lm1$fit
Rsq <- summary(lm1)$r.squared
Rsq <- round(Rsq, digits = 4)

plot(fity, y, xlab = "Fitted",
     ylab = "Observed")
lm2 <- lm(y ~ fity)
abline(lm2, col = "red")

text(quantile(fity, 0.9), quantile(y, 0.1),
     paste("Rsquare = ", Rsq), col = "blue")