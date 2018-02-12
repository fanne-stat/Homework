

#  This R code is posted as milkzip.R

#  Enter the data on milk consumption (cups) 
#  from problem 5 on assignment 3.

 x <- seq(0, 6, 1)
 count <- c(767, 557, 333, 142, 62, 23, 16)
 milk <- data.frame(x = x, count = count)
 milk$one <- 1
 milk
 nc <- nrow(milk)


#  Compute the total count, mean, and variance

   n <- sum(milk$count)
   n
   meanx <- crossprod(milk$x, milk$count)/n
   meanx
   varx <- (crossprod(milk$count, milk$x^2) - n*meanx^2)/n
   varx

# You can also get the mean of the poisson distribution using the 
# the glm function.  Here the model matrix is a column of ones.

    modelp <- glm( x ~ one , data=milk, weights=count, family=poisson)
    
    meanx <- exp(modelp$coef[1])
    meanx

#  Compute expected counts for the Poisson model and 
#  include them as a cxolumn in the data frame

    prob <- dpois(milk$x, meanx)
    prob[nc] <- 1-sum(prob[1:(nc-1)])
    prob
    milk$epoisson <- n*prob
    milk

# Compute the Pearson goodness-of-fit test and p-value

   x2p <- sum((milk$count-milk$epoisson)^2/milk$epoisson)
   x2p
   dfp <- nc - 2
   pvalp <- 1 - pchisq(x2p, dfp)
   pvalp

# Compute the G^2 goodness-of-fit test.

   g2<-0
   for(i in 1:nc) {
     at<-0
     if(milk$epoisson[i] > 0) {at<-2*milk$count[i]*log(milk$count[i]/milk$epoisson[i])}
     g2 <- g2+at }
   g2
   pvalg <- 1 - pchisq(g2, dfp)
   pvalg

#  Fit a Poisson ZIP model.  First attach the pscl library.

   library(pscl)
  

 # In the following, dist= is used to indicate the distribution of the Poisson
 # counts for tose who drink milk.  The log-link is always used for this distribution.
 # The link= option is used is used for the binomial probability corresponding to the
 # proportion of the population that never drink milk.  In this example, the log-link
 # is used.  The logit link is another choice but it caused numerical problems in  
 # this particular case.
  
   modelzip <- zeroinfl( x ~ 1 | 1, data=milk, dist="poisson", link="log",
                 weights=count)
   summary(modelzip)

   modelzip$coef

 # Compute mean for the Poisson distribution of cups of milk 
 # consumed for those who drink milk and and the probability
 # that a respondent who never drinks milk is selected.
  
   meanzip <- exp(modelzip$coef$count)
   meanzip

   pzip <- exp(modelzip$coef$zero)
   pzip

 # Compute expecte counts for the zip model and
 # apply goodness of fit tests

    prob <- dpois(milk$x, meanzip)
    prob[nc] <- 1-sum(prob[1:(nc-1)])
    prob <- prob*(1-pzip)
    milk$epoisson <- n*prob
    prob[1] <- prob[1]+pzip
    milk$ezip <- n*prob
    milk

# Compute the Pearson goodness-of-fit test and p-value

   x2p <- sum((milk$count-milk$ezip)^2/milk$ezip)
   x2p
   dfp <- nc - 2
   pvalp <- 1 - pchisq(x2p, dfp)
   pvalp

# Compute the G^2 goodness-of-fit test.

   g2<-0
   for(i in 1:nc) {
     at<-0
     if(milk$ezip[i] > 0) {at<-2*milk$count[i]*log(milk$count[i]/milk$ezip[i])}
     g2 <- g2+at }
   g2
   pvalg <- 1 - pchisq(g2, dfp)
   pvalg


#  Fit a Negative binomial ZIP model.  

  
   modelzinb <- zeroinfl( x ~ 1 | 1, data=milk, dist="negbin", link="log",
                 weights=count)
   summary(modelzinb)

   modelzinb$coef

 
# Fit a Poisson Hurdle model.  A slightly different 
# paramterization than the SAS parameterization is used.

  modelhurdlep <- hurdle( x ~ 1 | 1, data=milk, dist="poisson", 
                 zerodist="binomial", link="log", weights=count)
  summary(modelhurdlep)

   modelhurdlep$coef
   
   modelhurdlep1 <- hurdle( x ~ 1 | 1, data=milk, dist="negbin", 
                           zerodist="binomial", link="log", weights=count)
   
   meanhurdlenegbin <- exp(modelhurdlep1$coefficients$count)
   thetanb <- modelhurdlep1$theta 
   knb <- 1/thetanb
   pihurdle <- 1 - exp(modelhurdlep1$coefficients$zero)
   
   prob1 <- dnbinom(milk$x, mu = meanhurdlenegbin, size = thetanb)
   prob1[nc] <- 1-sum(prob1[1:(nc-1)])
   prob1 <- prob1*(1-pihurdle)/(1 - dnbinom(0, mu = meanhurdlenegbin, size = thetanb))
   prob1[1] <- pihurdle
   milk$ehurdlenb <- n*prob1
   milk
