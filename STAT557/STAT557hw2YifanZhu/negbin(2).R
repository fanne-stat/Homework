

#  This R code is posted as negbin.R

#  This code computes maximum likelihood 
#  estimates for Poisson and negative 
#  binomial distributions and tests the 
#  fit of these distirbutions.

#  The code includes a function for a 
#  modified Newton-Raphson algorithm that 
#  is used to maximize the log-likelihoods.  
#  It requires specification of functions 
#  to evaluate the log-likelihood and
#  evaluate first and second partial 
#  derivatives of the log-likelihood.

#  First create a number of useful functions

#  This function is used to combine 
#  categories so all expected counts exceed 
#  some constant mb. Here mb=2 by default if 
#  you do not use the fourth argument of the 
#  function.  Start at the bottom of the array.
#  It may incur some roundoff error in 
#  cummulating expected counts.  


#---FUNCTION FOR COMBINIG CATEGORIES------
#---KEEP EXPECTED COUNTS FOR COMBINED-----
#---CATEGORIES ABOVE A SET LOWER BOUND: mb

# Start at the bottom of the array and 
# combine categories until the expected 
# count for the combined category is 
# at least mb.

#   input:
#   ======	
#   cc -- vector of levels of a 
#          catogorical variable.
#   x -- vector of counts.
#  ep -- vector of expected counts.
#  nc -- the original number of 
#           categories (levels) . 			
#  mb -- the minimum expected count that
#           each combined category should 
#           have.	 		
		

#   output:
#   =======		
#	 k -- the number of categories after 
#          combining.	
#   cl (cu) -- the smallest (largest) value 
#                of a combined category.
#   xt -- the observed count for a combined 
#             category.
#  ept -- the expected count for a combined 
#             category.		 	
	
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
   list(k=k, cl = cl[II], cu=cu[II], 
           xt = x[II], ept = ep[II])
}


#  Function for evaluating the 
#  log-likelihood function

fun <- function(b, x.dat, x.cat)
 {
   if(b[2,1] > 40) {stop(paste("beta > 40"))} 
  else 
   {f <- crossprod(x.dat, lgamma(x.cat+b[2,1])-lgamma(x.cat+1))   
   f <- f +  sum(x.dat)*(b[2,1]* log(b[1,1])- lgamma(b[2,1]))
   f <- f + crossprod(x.dat, x.cat)*log(1-b[1,1])}
   f
 }


#  Function for evaluating negative 
#  binomial probabilities

prob <- function(b, x.dat, x.cat)
 {
  pp <- exp(lgamma(x.cat+b[2,1])
          -lgamma(x.cat+1)-lgamma(b[2,1])
          +(b[2,1]*log(b[1,1])) 
          +x.cat*log(1-b[1,1]))
  kt <- length(pp)
  pp[kt] <- 1 - sum(pp) + pp[kt]
  pp
 }



#  Function for computing first 
#  partial derivatives and the
#  negative of the matrix of the 
#  matrix of second partial derivatives
#  of the negative binomial log-likelihood 

dfun <- function(b, x.dat, x.cat)
 {
  xcum <- sum(x.dat)-cumsum(x.dat)+x.dat
  q <- matrix(c(1,1), 2, 1)
  h <- matrix(c(1, 1, 1, 1), 2, 2)
  q[1,1] <- sum(x.dat)*b[2,1]/b[1,1] - 
          crossprod(x.dat, x.cat)/(1-b[1,1])
  q[2,1] <- sum(xcum/(x.cat+(b[2,1]-1)))-
               (xcum[1]/(b[2,1]-1)) + 
               (sum(x.dat)*log(b[1,1]))
  
  h[1,1] <- sum(x.dat)*b[2,1]/(b[1,1]^2) + 
       crossprod(x.dat, x.cat)/((1-b[1,1])^2)
  h[1,2] <- -sum(x.dat)/b[1,1]
  h[2,1] <- h[1,2]
  h[2,2] <- sum(xcum/((x.cat+(b[2,1]-1))^2))
  h[2,2] <- h[2,2]-(xcum[1]/((b[2,1]-1)^2))
  list(q=q, h=h)
 }



# Modified Newton-Raphson Algorithm

mnr <- function(b, x.dat, x.cat, 
          maxit = 50, halving = 16, 
          conv = .000001)
{
  check <- 1
  iter <- 1
  while(check > conv && iter < maxit+1) {
    fold <- fun(b, x.dat, x.cat)
    bold <- b
    aa <- dfun(b, x.dat, x.cat)
    hi <- solve(aa$h)
   
    b <- bold + hi%*%aa$q
   
    fnew <- fun(b, x.dat, x.cat)
    hiter <- 1
      while(fold-fnew > 0 && hiter < halving+1) {
         b <- bold + 2.*((.5)^hiter)*hi%*%aa$q
         fnew <- fun(b, x.dat, x.cat)
         hiter <- hiter + 1
      }
   cat("\n", "Iteration = ", iter, " pi =", b[1,1], 
           " beta = ", b[2,1], "Log-likelihood", fnew)
    iter <- iter + 1
    check <- crossprod(bold-b,bold-b)/crossprod(bold,bold)
    }  
       aa <- dfun(b, x.dat, x.cat)
       hi <- solve(aa$h) 
       list(b = b, hi = hi, grad = aa$q)
  }


#  The data in this example are 
#  the smooth surface cavity data 
#  considered in STAT 557.

#  Enter a list of counts for each 
#  of the categories from zero 
#  through K, the maximum number of 
#  cavities seen in any one child. 
#  All categories preceeding K must 
#  be included, even if the observed 
#  count is zero. There should be K+1 
#  entries in the list of counts.

x.dat <- c(15,32,26,29,22,19,9,8,3,1,0,0,1,0,0,1)
mb<-2

#  Enter the category values
 
nc <- length(x.dat)
x.cat <- 0:(nc-1)


#  Compute the total count, mean, 
#  and variance

n <- sum(x.dat)
m.x <- crossprod(x.dat, x.cat)/n
v.x <- (crossprod(x.dat, x.cat^2) - n*m.x^2)/n


#  Compute expected counts for 
#  the Poisson model

m.p <- (n/m.x)*exp(-m.x)*
      cumprod(m.x*(c(1,1:(length(x.dat)-1))^(-1)))

# Combine categories to keep expected 
# counts above a lower bound
 
xcc <- combine(x.cat, x.dat, m.p, nc, mb)

# Compute the Pearson chi-squared 
# test and p-value

  x2p <- sum((xcc$xt-xcc$ept)^2/xcc$ept)
  dfp <- length(xcc$ept) - 2
  pvalp <- 1 - pchisq(x2p, dfp)

# Compute the G^2 statistic

 g2<-0
 for(i in 1:length(xcc$ept)) {
 at<-0
 if(xcc$xt[i] > 0) 
   {at<-2*xcc$xt[i]*log(xcc$xt[i]/xcc$ept[i])}
 g2 <- g2+at}
 pvalg <- 1 - pchisq(g2, dfp)



# Compute the Fisher Index of Dispersion test statistic
# The one-sided alternative is that the variance is 
# larger than the mean.

  fisherd <- n*v.x/m.x
  dff <- n -1
  pvalf <- 1 - pchisq(fisherd, dff)


# Print results

  kk <- length(xcc$ept)
  new2 <- matrix(c(xcc$cl, xcc$cu, xcc$xt, xcc$ept), 
 kk, 4)
  cat("\n", "  Results for fitting the 
 Poisson distribution", "\n")
  cat("\n", "Category Bounds  Count  Expected", "\n")
  print(new2)   
  cat("\n", "      Pearson test = ", x2p)
  cat("\n", "Degrees of freedom = ", dfp)
  cat("\n", "           p-value = ", pvalp, "\n")
  cat("   Likelihood ratio test =", g2, "\n")
  cat("                      df =", dfp, "\n")
  cat("                  p-value =", pvalg, "\n")
  cat("\n","Fisher Index of Dispersion = ", fisherd)
  cat("\n", "    Degrees of freedom = ", dff)
  cat("\n", "               p-value = ", pvalf )




#  Begin computation for fitting the 
#  negative binomial distribution
 
  pi <- .95
  beta <- 20

  if(v.x > m.x) {pi <- m.x/v.x
              beta <- m.x^2/(v.x-m.x) }
  
  cat("\n", "Results for fitting the 
 negative binomial distribution", "\n")
  cat("\n", "  Method of moment estimators ")
  cat("\n", "     pi = ", pi)
  cat("\n", "   beta = ", beta, "\n")


#  Compute maximum likelihood estimates

  b <- matrix(c(pi, beta), nrow=2, ncol=1)
    
  nbr <- mnr(b, x.dat, x.cat)

cat("\n", "  Maximum likelihood estimators")
cat("\n", "     pi = ", nbr$b[1])
cat("\n", "   beta = ", nbr$b[2], "\n")
cat("\n", "  Covariance matrix =", nbr$hi )


#  Compute expected counts for the 
#  negative binomial model

m.nb <- sum(x.dat)*prob(nbr$b, x.dat, x.cat)

#  Combine categories in the right 
#  tail of the distribution
 
nc <- length(x.dat)  
xcc <- combine(x.cat, x.dat, m.nb, nc, mb)

# Compute the Pearson chi-squared 
# test and p-value

  x2p <- sum(((xcc$xt-xcc$ept)^2)/xcc$ept)
  dfp <- length(xcc$ept) - 3
  pvalp <- 1 - pchisq(x2p, dfp)

# Compute the G^2 statistic

g2<-0
for(i in 1:length(xcc$ept)) {
 at<-0
 if(xcc$xt[i] > 0) 
   {at<-2*xcc$xt[i]*log(xcc$xt[i]/xcc$ept[i])}
 g2 <- g2+at}
 pvalg <- 1-pchisq(g2, dfp)

#  print results

 kk <- length(xcc$ept) 
 new3 <- matrix(c(xcc$cl, xcc$cu, xcc$xt, xcc$ept), kk, 4)
  cat("\n", "  Results for fitting the Negative Binomial distribution", "\n")
  cat("\n", "Category Bounds   Count   Expected", "\n")
  print(new3)   
  cat("\n", "         Pearson test = ", x2p)
  cat("\n", "   Degrees of freedom = ", dfp)
  cat("\n", "              p-value = ", pvalp)
  cat("\n", "Likelihood ratio test = ", g2)
  cat("\n", "                   df = ", dfp)
  cat("\n", "              p-value = " , pvalg)


