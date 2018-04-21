## Homewrk 5
#Problem 1
colleges <- read.table("Colleges.txt", head = T, sep = "\t")

phi <- function(omega, lambda){
  if(lambda == 0)
    return(log(omega))
  else
    return((omega^lambda - 1)/lambda)
}

Phi <- function(Omega, Lambda){
  p <- length(Omega)
  res <- rep(0, p)
  for(i in 1:p)
    res[i] <- phi(Omega[i], Lambda[i])
  return(res)
}

X <- as.matrix(colleges[colleges$School_Type == "Lib Arts", -c(1,2)])
Y <- as.matrix(colleges[colleges$School_Type == "Univ", -c(1,2)])

Phi_X <- apply(X, MARGIN = 1, Phi, Lambda = lambdas.grid[400,])
mu <- apply(Phi_X, MARGIN = 1, mean)
Phi_Y <- apply(Y, MARGIN = 1, Phi, Lambda = lambdas.grid[400,])
nu <- apply(Phi_Y, MARGIN = 1, mean)
Sigma <- array(rep(0, length(mu)^2), dim = c(length(mu), length(mu)))
for (i in 1:ncol(Phi_X)){
  Sigma <- Sigma + (Phi_X[,i] - mu) %*% t(Phi_X[,i] - mu)
}
for (i in 1:ncol(Phi_Y)){
  Sigma <- Sigma + (Phi_Y[,i] - nu) %*% t(Phi_Y[,i] - nu)
}
Sigma <- Sigma/(ncol(Phi_X) + ncol(Phi_Y))

logXsum <- apply(log(X), MARGIN = 2, sum)
logYsum <- apply(log(Y), MARGIN = 2, sum)

mloglik <- -(1/2) * (ncol(Phi_X) + ncol(Phi_Y)) * log(abs(det(Sigma))) + t(lambdas.grid[400,]-1)%*%(logXsum + logYsum)

maxloglik <- function(lambda, X, Y){
  Phi_X <- apply(X, MARGIN = 1, Phi, Lambda = lambda)
  mu <- apply(Phi_X, MARGIN = 1, mean)
  Phi_Y <- apply(Y, MARGIN = 1, Phi, Lambda = lambda)
  nu <- apply(Phi_Y, MARGIN = 1, mean)
  Sigma <- array(rep(0, length(mu)^2), dim = c(length(mu), length(mu)))
  for (i in 1:ncol(Phi_X)){
    Sigma <- Sigma + (Phi_X[,i] - mu) %*% t(Phi_X[,i] - mu)
  }
  for (i in 1:ncol(Phi_Y)){
    Sigma <- Sigma + (Phi_Y[,i] - nu) %*% t(Phi_Y[,i] - nu)
  }
  Sigma <- Sigma/(ncol(Phi_X) + ncol(Phi_Y))
  
  logXsum <- apply(log(X), MARGIN = 2, sum)
  logYsum <- apply(log(Y), MARGIN = 2, sum)
  
  mloglik <- -(1/2) * (ncol(Phi_X) + ncol(Phi_Y)) * log(abs(det(Sigma))) + t(lambda - 1)%*%(logXsum + logYsum)
  
  return(mloglik)
}

library(gtools)
lambdas <- c(0, 1/4, 1/3, 1/2, 1, 2, 3, 4) 
lambdas.grid <- permutations(n = length(lambdas), r = 6, v = lambdas, repeats.allowed = T)

maxlogliks <- apply(X = lambdas.grid, MARGIN = 1, FUN = function(lambda) maxloglik(lambda, X = X, Y = Y))

max.lambda <- lambdas.grid[which.max(maxlogliks), ]

U <- apply(X, MARGIN = 1, Phi, Lambda = max.lambda)
V <- apply(Y, MARGIN = 1, Phi, Lambda = max.lambda)

colleges.tranformed <- cbind(colleges$School_Type, as.data.frame(rbind(t(U), t(V))))
names(colleges.tranformed) <- names(colleges)[-1]

source("testnormality.R")
testnormality(X = colleges.tranformed[colleges.tranformed$School_Type == "Lib Arts", -1])
testnormality(X = colleges.tranformed[colleges.tranformed$School_Type == "Univ", -1])

tp_value<-function(X, cl){
  class <- levels(cl)
  return(t.test(X[cl == class[1]], X[cl == class[2]], var.equal = T)$p.value)
}

p_vals <- sapply(colleges.tranformed[,-1], tp_value, cl = as.factor(colleges.tranformed$School_Type))

p.adjust(p_vals[order(p_vals)], method = "fdr")




# Problem 2
library(sas7bdat)
psych <- read.sas7bdat("psych.sas7bdat")

#a) Fit the linear model
library(car)
psych$PROG <-as.factor(psych$PROG)
levels(psych$PROG)
fit.lm <- lm(cbind(LOCUS_OF_CONTROL,SELF_CONCEPT,MOTIVATION) ~ READ + WRITE + SCIENCE + PROG, data = psych)
summary(fit.lm)
fit.manova <- Manova(fit.lm)
summary(fit.manova)

#Comment. From the result for locus of control, w
# Therefore, we reject the null hypothesis and conclude that there is a significant effect of on locus of control.


#b) Refit the model without test scores of writing and science.
fit.lm1 <- lm(cbind(LOCUS_OF_CONTROL,SELF_CONCEPT,MOTIVATION)~ READ + PROG, data = psych)
summary(fit.lm1)
fit.manova1 <- Manova(fit.lm1)
summary(fit.manova1)

#c) 
anova(fit.lm, fit.lm1, test = "Wilks")
#d)Test simultaneously 
p_values <- c(0,0)

fit.lm.P12 <- lm(cbind(LOCUS_OF_CONTROL,SELF_CONCEPT,MOTIVATION) ~ READ + WRITE + SCIENCE + PROG, data = psych[psych$PROG %in% c(1,2),])
fit.lm.P12_reduced <- lm(cbind(LOCUS_OF_CONTROL,SELF_CONCEPT,MOTIVATION) ~ READ + WRITE + SCIENCE, data = psych[psych$PROG %in% c(1,2),])
test_P12 <- anova(fit.lm.P12, fit.lm.P12_reduced, test = "Wilks")
p_vals[1] <- test_P12$`Pr(>F)`[2]

fit.lm.P23 <- lm(cbind(LOCUS_OF_CONTROL,SELF_CONCEPT,MOTIVATION) ~ READ + WRITE + SCIENCE + PROG, data = psych[psych$PROG %in% c(2,3),])
fit.lm.P23_reduced <- lm(cbind(LOCUS_OF_CONTROL,SELF_CONCEPT,MOTIVATION) ~ READ + WRITE + SCIENCE, data = psych[psych$PROG %in% c(2,3),])
test_P23 <- anova(fit.lm.P23, fit.lm.P23_reduced, test = "Wilks")
p_values[2] <- test_P23$`Pr(>F)`[2]

bon_adj<-p.adjust(p_values,method="bonferroni")
bon_adj
fdr_adj<-p.adjust(p_values,method="fdr")
fdr_adj

#e)
C <- matrix(c(0,0,1,0,0,0), nrow = 1)
M <- matrix(c(1,-1,0), ncol = 1)
newfit <- linearHypothesis(fit.lm, hypothesis.matrix = C, P = M)
print(newfit)


#f)
C1 <- matrix(c(0,0,1,0,0,0,0,0,0,1,0,0), nrow = 2, byrow = T)
M <- matrix(c(1,-1,0), ncol = 1)
newfit1 <- linearHypothesis(fit.lm, hypothesis.matrix = C1, P = M)
print(newfit1)


#g)
fit.lm2<- lm(cbind(LOCUS_OF_CONTROL,SELF_CONCEPT,MOTIVATION)~ .^2, data = psych)
summary(fit.lm2)
fit.manova12 <- Manova(fit.lm2)
summary(fit.manova12)

