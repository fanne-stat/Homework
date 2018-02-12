
 
# Use the glm function in R to fit loglinear 
# models to the analyze associations among incidence
# of trafiic violations, cardiovascular disease,
# and gender.

# This file is posted as  drivers1.R


# First define a function to compute Pearson chi-square 
# and deviance tests and p-values
combine<-function(cc, x, ep, nc, mb)
{
  ptr <- nc
  I <- c()
  k <- 0
  cl<-cc
  cu<-cc
  while(ptr > 1) {
    ptrm1 <- ptr - 1
    if(ep[ptr] < mb) {
      ep[ptrm1] <- ep[ptrm1] + ep[ptr]
      x[ptrm1] <- x[ptrm1] + x[ptr]
      cu[ptrm1]<-cu[ptr]
    }
    else {
      k <- k + 1
      I[k] <- ptr
    }
    ptr <- ptrm1
  }
  if(ep[1] < mb) {
    Ik <- I[k]
    ep[Ik] <- ep[Ik] + ep[1]
    x[Ik] <- x[Ik] + x[1]
    cl[Ik]<-cl[1]
  }
  else {
    k <- k + 1
    I[k] <- 1
  }
  II <- I[k:1]
  list(k=k, cl = cl[II], cu=cu[II], xt = x[II], ept = ep[II])   
}


goftests <- function(x, m1, df) 
{
  
  # Compute the number of categories.
  
  nc<-length(x)
  
  # Enter the category levels.
  
  cc<-0:(nc-1)
  ep <- m1
  mb<-5
  comb<-combine(cc, x, ep, nc, mb)
  xt<-comb$xt
  ept<-comb$ept
  k<-comb$k
  
  
 # Compute Pearson chi-squared 
 # and deviance tests and p-values
 # In this function 
 #    x = observed counts
 #   m1 = expected counts under H0
 #   m2 = expected counts under HA
 #   df = degrees of freedom
  x2p <- sum(((xt-ept)^2)/ept)
  pvalp <- 1 - pchisq(x2p, df - nc + k)

# Compute the G^2 statistic
  g2<-0
  for(i in 1:k) {
    at<-0
    if(xt[i] > 0) {at<-2*xt[i]*log(xt[i]/ept[i])}
    g2 <- g2+at}
  pvalg <- 1-pchisq(g2, df - nc + k)

  cat("\n", "       Pearson test = ", round(x2p,2))
  cat("\n", " Degrees of freedom = ", df - nc + k)
  cat("\n", "            p-value = ", round(pvalp,5))
  cat("\n", "      Deviance test = ", round(g2,2))
  cat("\n", "                 df = ", df - nc + k)
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

# Use the  glm  function to fit the complete 
# independence model. Use the family=poisson option 
# when a fiiting a log-linear model even when 
# you have multinomial data

  options(contrasts=c("contr.treatment", "contr.poly"))
  danish1 <- glm(Y ~ A + C + B + D, family=poisson, 
              data=danishd, maxit=20, epsilon=.000001, x=T, trace=T)

# Print some results
  summary(danish1, correlation=F)

# Print the estimated means for the complete
# independence model

  danish1$fit

# Test the fit of the model against the general
# alternative

  goftests(danishd$Y, danish1$fit, danishd$Y, danish1$df.residual)

#  The estimates of the parameters are
#  stored in driver1$coef

   driver1$coef

#  Compute the covariance matrix for the
#  large sample normal approximation to
#  the distribution of the parameter estimates

   danish1$cov <- solve(t(danish1$x)%*%diag(danish1$weight)%*%danish1$x)
   danish1$cov

#  Produce an analysis of deviance table

   anova(danish1, test="Chisq")


#  Now fit a joint independence model
#  and print some results

  driver2 <- glm(Fr ~ violations + disease + sex + disease*sex,
               family=poisson,  data=driverd, maxit=20, 
               epsilon=.000001, x=T, trace=T)

  summary(driver2, correlation=F)
  goftests(driverd$Fr, driver2$fit, driverd$Fr, driver2$df.residual)

  driver2$cov <- solve(t(driver2$x)%*%diag(driver2$weight)%*%driver2$x)
  driver2$cov

#  Now fit a conditional independence model
#  and print some results

  driver3 <- glm(Fr ~ violations + disease + sex + violations*sex + 
              disease*sex, family=poisson, data=driverd, 
              maxit=20, epsilon=.000001, x=T, trace=T)

  summary(driver3, correlation=F)
  goftests(driverd$Fr, driver3$fit, driverd$Fr, driver3$df.residual)

  driver3$cov <- solve(t(driver3$x)%*%diag(driver3$weight)%*%driver3$x)
  driver3$cov


#  Now fit the no three factor interaction model
#  and print some results

  danish4 <- glm(Y ~ .^4,  family=poisson, data=danishd, x=T, trace=T)

  summary(danish4, correlation=F)
  goftests(danishd$Y, danish4$fit, danishd$Y, danish4$df.residual)

  driver4$cov <- solve(t(driver4$x)%*%diag(driver4$weight)%*%driver4$x)
  driver4$cov



#  Use the  step  function to search for a good model.  
#  Start with the results for the complete independence model 

  danish.step <- step(danish5, list(upper=~ .^4), trace=F)
  
  summary(danish.step, correlation=F)
  goftests(danishd$Y, danish.step$fit, danishd$Y, danish.step$df.residual)

#  Print a summary of the results for the search.  

  danish.step$anova
  			   




danish5 <- glm(Y ~ .^4 - A:C:B:D - A:C:D , family=poisson, dat=danishd, x=T, trace=T)
  
  summary(danish5, correlation=F)
  goftests(danishd$Y, danish5$fit, danishd$Y, danish5$df.residual)
  
  
library(VGAM)
danishd1 <- dcast(formula = C + B + D ~ A, value = Y , data = danishd)
colnames(danishd1) <- c("C", "B", "D", "A1", "A2", "A3")  
danishd1

options(contrasts = c("contr.sum", "contr.poly"))
danishlogit1 <- vglm(cbind(A1, A2, A3) ~ C + B + D + C:B + C:D + B:D, family=multinomial, data=danishd1)
summary(danishlogit1)
# coef(mlogit)
# predict(mlogit, ddat, type="response")
# predict(mlogit, ddat, type="link")

danishlogit2 <- vglm(cbind(A1, A2, A3) ~ C + B + C:B , family=multinomial, data=danishd1)
summary(danishlogit2)
