# First define a function to compute Pearson chi-square 
# and deviance tests and p-values

goftests <- function(x, m1, m2, df) 
{
  # Compute Pearson chi-squared 
  # and deviance tests and p-values
  # In this function 
  #    x = observed counts
  #   m1 = expected counts under H0
  #   m2 = expected counts under HA
  #   df = degrees of freedom
  
  k <- length(x)
  m1 <- m1 + .00000000000000001
  m2 <- m2 + .00000000000000001
  x2p <- sum(((m1-m2)^2)/m1)
  pvalp <- 1 - pchisq(x2p, df)
  
  # Compute the G^2 statistic
  
  g2 <- 2*sum(x*(log(m2/m1)))
  pvalg <- 1-pchisq(g2, df)
  
  cat("\n", "       Pearson test = ", round(x2p,2))
  cat("\n", " Degrees of freedom = ", df)
  cat("\n", "            p-value = ", round(pvalp,5))
  cat("\n", "      Deviance test = ", round(g2,2))
  cat("\n", "                 df = ", df)
  cat("\n", "            p-value = " , round(pvalg,5),"\n")
}


# Enter the data

danishd <- read.csv("./danish.csv", header = T)

danishd$A <- as.factor(danishd$A)
danishd$B <- as.factor(danishd$B)
danishd$C <- as.factor(danishd$C)
danishd$D <- as.factor(danishd$D)


# Print the data

danishd

# complete independent model
options(contrasts=c("contr.treatment", "contr.poly"))

danish1 <- glm(Y ~ A + C + B + D, family=poisson, data=danishd, x=T, trace=T)
summary(danish1)
goftests(danishd$Y, danish1$fit, danishd$Y, danish1$df.residual)
anova(danish1, test = "Chisq")

# using stepwise to find a model. start with complete indepedent model.
danish.step <- step(danish1, list(lower=formula(danish1), upper=~ .^4), scale=1, trace=F)
summary(danish.step)
goftests(danishd$Y, danish.step$fit, danishd$Y, danish.step$df.residual)
anova(danish.step, test = "Chisq")

# fit model without 4-interaction
danish2 <- glm(Y ~ .^4 - A:C:B:D, family=poisson, data=danishd, x=T, trace=T)
summary(danish2)
goftests(danishd$Y, danish2$fit, danishd$Y, danish2$df.residual)
anova(danish2, test = "Chisq")

# fit model without 4-interaction and A:C:D
danish3 <- glm(Y ~ .^4 - A:C:B:D - A:C:D, family=poisson, data=danishd, x=T, trace=T)
summary(danish3)
goftests(danishd$Y, danish3$fit, danishd$Y, danish3$df.residual)
anova(danish3, test = "Chisq")


# logistic regression
library(reshape2)
library(VGAM)
danishd1 <- dcast(formula = C + B + D ~ A, value = Y , data = danishd)
colnames(danishd1) <- c("C", "B", "D", "A1", "A2", "A3")  
danishd1

options(contrasts = c("contr.treatment", "contr.poly"))
danishlogit1 <- vglm(cbind(A1, A2, A3) ~ C + B + D + C:B, family=multinomial, data=danishd1)
summary(danishlogit1)
