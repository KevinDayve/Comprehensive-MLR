#Dealing with ordinal predictors
library(ordPens)
library(lmreg)
data(ICFCoreSetCWP)


#Model fitting
lmphcs1 <- lm(phcs ~ d450, data = ICFCoreSetCWP)
walking_difficulty <- binaries(ICFCoreSetCWP$d450)

head(walking_difficulty)
diff_morethan0 <- walking_difficulty[, 3] + walking_difficulty[, 4] +
  walking_difficulty[, 2]
diff_morethan1 <- walking_difficulty[, 4] + walking_difficulty[, 2]
diff_morethan2 <- walking_difficulty[, 2]

health <- ICFCoreSetCWP$phcs
lmphcs2 <- lm(health ~ diff_morethan0 + diff_morethan1 +
                diff_morethan2)
plot(lmphcs2$fit, health, 
     xlab = "Fitted values", ylab = "Observed values")

abline(lm(health ~ lmphcs2$fit))
