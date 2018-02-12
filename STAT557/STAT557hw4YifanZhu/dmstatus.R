

# This is R code for fitting
# polychotomous logistic regression
 

#  Enter the data into a data frame


 ddat <- read.table("dmstatus.dat", 
  col.names=c("x","single","married","divorced","total"))

  ddat
 
#  Compute sample proportions and subtract
#  16 from the age values

  ddat$ps <- ddat$single/ddat$total
  ddat$pm <- ddat$married/ddat$total
  ddat$pd <- ddat$divorced/ddat$total
  ddat$z <- ddat$x-16  
  ddat$lz <- log(ddat$z)
  ddat


# Fit a polychotomous logistic regression
# model using married as the baseline


options(contrasts = c("contr.treatment", "contr.poly"))
library(VGAM)
mlogit <- vglm(cbind(single, divorced, married) ~ z, family=multinomial, data=ddat)
summary(mlogit)
coef(mlogit)
predict(mlogit, ddat, type="response")
predict(mlogit, ddat, type="link")



#  plot the estimated curves

a <- seq(0,59,1)
ax <- a+16
ay <- a/59

pp <- predict(mlogit, data.frame(z=a),type="response")

par(fin=c(5.5,5.5),pch=18,mkh=.1,mex=1.5)
plot(ax, ay, type="n", 
       xlab="Age",
       ylab="Probability",
       main="Danish Marital Status")
lines(ax, pp[ , 1], lty=1, lwd=3)
lines(ax, pp[ , 2], lty=3, lwd=3)
lines(ax, pp[ , 3], lty=2, lwd=3)
points(ddat$x, ddat$ps, pch=15, mkh=.2)
points(ddat$x, ddat$pd, pch=17, mkh=.2)
points(ddat$x, ddat$pm, pch=6, mkh=.2)
legend(50, 1.04,c("Single", "Divorced",
                 "Married"),lty=c(1,3,2),bty="n")



ddat$z2 <- (ddat$z)^2
ddat$lz2 <- (ddat$lz)^2

#------------
mlogit2 <- vglm(cbind(single, divorced, married) ~ z + z2, family=multinomial, data=ddat)
summary(mlogit2)
a <- seq(0,59,1)
ax <- a+16
ay <- a/59

pp <- predict(mlogit2, data.frame(z=a, z2 = a),type="response")

par(fin=c(5.5,5.5),pch=18,mkh=.1,mex=1.5)
plot(ax, ay, type="n", 
     xlab="Age",
     ylab="Probability",
     main="Danish Marital Status")
lines(ax, pp[ , 1], lty=1, lwd=3)
lines(ax, pp[ , 2], lty=3, lwd=3)
lines(ax, pp[ , 3], lty=2, lwd=3)
points(ddat$x, ddat$ps, pch=15, mkh=.2)
points(ddat$x, ddat$pd, pch=17, mkh=.2)
points(ddat$x, ddat$pm, pch=6, mkh=.2)
legend(50, 1.04,c("Single", "Divorced",
                  "Married"),lty=c(1,3,2),bty="n")

#--------------------------------------------------------
mlogit3 <- vglm(cbind(single, divorced, married) ~ lz, family=multinomial, data=ddat)
summary(mlogit3)
a <- seq(1,59,1)
ax <- a+16
ay <- a/59

pp <- predict(mlogit3, data.frame(lz = log(a)),type="response")

par(fin=c(5.5,5.5),pch=18,mkh=.1,mex=1.5)
plot(ax, ay, type="n", 
     xlab="Age",
     ylab="Probability",
     main="Danish Marital Status")
lines(ax, pp[ , 1], lty=1, lwd=3)
lines(ax, pp[ , 2], lty=3, lwd=3)
lines(ax, pp[ , 3], lty=2, lwd=3)
points(ddat$x, ddat$ps, pch=15, mkh=.2)
points(ddat$x, ddat$pd, pch=17, mkh=.2)
points(ddat$x, ddat$pm, pch=6, mkh=.2)
legend(50, 1.04,c("Single", "Divorced",
                  "Married"),lty=c(1,3,2),bty="n")
#------------------------------------------------
mlogit4 <- vglm(cbind(single, divorced, married) ~ lz + lz2, family=multinomial, data=ddat)
summary(mlogit4)
a <- seq(1,59,1)
ax <- a+16
ay <- a/59

pp <- predict(mlogit4, data.frame(lz = log(a), lz2 = (log(a))^2),type="response")

par(fin=c(5.5,5.5),pch=18,mkh=.1,mex=1.5)
plot(ax, ay, type="n", 
     xlab="Age",
     ylab="Probability",
     main="Danish Marital Status")
lines(ax, pp[ , 1], lty=1, lwd=3)
lines(ax, pp[ , 2], lty=3, lwd=3)
lines(ax, pp[ , 3], lty=2, lwd=3)
points(ddat$x, ddat$ps, pch=15, mkh=.2)
points(ddat$x, ddat$pd, pch=17, mkh=.2)
points(ddat$x, ddat$pm, pch=6, mkh=.2)
legend(50, 1.04,c("Single", "Divorced",
                  "Married"),lty=c(1,3,2),bty="n")
#-------------------------------------------------


# Fit a model with cummulative logits


  dsing <- ddat[ , c(1,2,5,6,9,10)]
    colnames(dsing) <- c("x", "count", "total", "p", "z", "logz")
    dsing$y <- 1
  ddiv <-  ddat[ , c(1,3,5,7,9,10)]
    colnames(ddiv) <- c("x", "count", "total", "p", "z", "logz")
    ddiv$y <- 2
  dmar <- ddat[ , c(1,4,5,8,9,10)]
     colnames(dmar) <- c("x", "count", "total", "p", "z", "logz")
     dmar$y <- 3

  ddats <- rbind(dsing, ddiv, dmar)
  ddats$y <- as.factor(ddats$y)

  library(MASS)
  clogit <- polr(y ~ z, weights=count, data=ddats, Hess=TRUE)
  summary(clogit)
  coef(clogit)
  predict(clogit, ddats, type="probs")
  
 

#  plot the estimated curves

a <- seq(0,59,1)
ax <- a+16
ay <- a/59

pp <- predict(clogit, data.frame(z=a),type="probs")

par(fin=c(5.5,5.5),pch=18,mkh=.1,mex=1.5)
plot(ax, ay, type="n", 
       xlab="Age",
       ylab="Probability",
       main="Danish Marital Status")
lines(ax, pp[ , 1], lty=1, lwd=3)
lines(ax, pp[ , 2], lty=3, lwd=3)
lines(ax, pp[ , 3], lty=2, lwd=3)
points(ddat$x, ddat$ps, pch=15, mkh=.2)
points(ddat$x, ddat$pd, pch=17, mkh=.2)
points(ddat$x, ddat$pm, pch=6, mkh=.2)
legend(50, 1.04,c("Single", "Divorced",
                 "Married"),lty=c(1,3,2),bty="n")


               
etable <- function(x){
  return(c(nparam(x), deviance(x), AIC(x),BIC(x)))
}
