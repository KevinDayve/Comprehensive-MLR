#Significance of a factor, namely race in this case.]
#Does race even matter?
library(lmreg)
library(birthwt)

lmbwt1 <- lm(bwt ~ smoke + factor(race),
             data = birthwt)
result <- summary(lmbwt1)
result

#We want a singular answer. We have to consider minimised
#Sum of squares, here.
races <- as.data.frame(binaries(birthwt$race))

colnames(races) = c("Black", "Other",
                    "White")

Model3 <- lm(bwt ~ smoke + races$Noir,
             races$Autre, data = birthwt)
result <- summary(Model3)
result

#Simple model
lmbwt <- lm(bwt ~ smoke + lwt, data = birthwt)
hanova(lmbwt, lmbwt1)
