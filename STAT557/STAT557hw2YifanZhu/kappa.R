


#   This file contains R code for computing a 
#   Kappa statistic, or weighted Kappa statistic, 
#   standard errors and confidence intervals. 
#   It is applied to the student teacher data.

#   The file is posted as  kappa.R

# First create a function to compute kappa

kappa.f <- function(x, w=NULL)
{

#  Compute expected counts for random agreement

  n <- sum(x)
  xr <- apply(x, 1, sum)
  xc <- apply(x, 2, sum)
  one <- rep(1, length(xr))
  e <- outer(xr, xc)/n
  
#  Compute the unweighted Kappa

  k1 <- sum(diag(x))/n
  k2 <- sum(diag(e))/n
  k3 <- sum(diag(x)*diag(xr+xc))/(n*n)
  k4 <- sum(((outer(xc, one)+
          outer(one, xr))**2)*x)/(n**3)
  kappa <- (k1-k2)/(1-k2)

#  Compute standard errors:  
#     s1 does not assume random agreement
#     s2 assumes only random agreement

  s11 <- (k1*(1-k1)/((1-k2)**2)+2*(1-k1)*
         (2*k1*k2-k3)/((1-k2)**3)+
      ((1-k1)**2)*(k4-4*k2*k2)/((1-k2)**4))/n
  s1 <- s11**.5

  s22 <- (k2+k2*k2-(sum(diag(e)*diag(xr+xc))
          /(n**2)))/(n*(1-k2)**2)
  s2 <- s22**.5

# Compute default weights if no weights are provided

k <- dim(x)[2]
if (is.null(w)) {
        w <- matrix(0, ncol = k, nrow = k)
        for (i in 1:k) {
            for (j in 1:k) {
                w[i, j] <- 1 - (abs(i - j))^2/(k-1)^2
            }
        }
    }


#  Compute the weighted Kappa

  xw <- x*w
  ew <- e*w
  wr <- apply(w*xc, 2, sum)/n
  wc <- apply(w*xr, 2, sum)/n

  kw1 <- sum(xw)/n
  kw2 <- sum(ew)/n
  tt2 <- outer(wr, one)+outer(one, wc)
  tt3 <- ((w*(1-kw2))-(tt2*(1-kw1)))**2
  kappaw <- (kw1-kw2)/(1-kw2)

#  Compute standard errors: 
#     sw11 does not assume random agreement
#     sw22  assumes only random agreement

  sw11 <- sum(x*tt3)/n
  sw11 <- (sw11-(kw1*kw2-2*kw2+kw1)**2)/
          (n*(1-kw2)**4)
  sw1 <- sw11**.5
  sw22 <- (w-tt2)**2
  sw22 <- ((sum(e*sw22)/n)-(kw2**2))/
          (n*(1-kw2)**2)
  sw2 <- sw22**.5

#  Construct 95% confidence intervals 
#  and tests for random agreement

  tk <- kappa/s2
  tkw <- kappaw/sw2
  tt4 <- tk**2
  pk <- (1-pchisq(tt4, 1))
  tt4 <- tkw**2
  pkw <-(1-pchisq(tt4, 1))
  ckl <- kappa-(1.96)*s1
  cku <- kappa+(1.96)*s1
  ckwl <- kappaw-(1.96)*sw1
  ckwu <- kappaw+(1.96)*sw1
  ww <- matrix( w, ncol=k, nrow=k)

#print results

  cat("\n", "       Unweighted Kappa = ", signif(kappa,5))
  cat("\n", "         Standard error = ", signif(s1,5))
  cat("\n", "95% confidence interval:  ", signif(ckl,5), signif(cku,5))
  cat("\n", "p-value for test of random agreement = ", signif(pk,5), "\n", "\n")

  cat("\n", "         Weighted Kappa = ", signif(kappaw,5))
  cat("\n", "         Standard error = ", signif(sw1,5))
  cat("\n", "95% confidence interval:  ", signif(ckwl,5), signif(ckwu,5))
  cat("\n", "p-value for test of random agreement = ", signif(pkw,5),"\n")
}



#   Enter the observed counts into a matrix, reading
#   across the rows of the 2x2 table

  xw <- matrix(c(9, 10, 5, 26, 6, 13, 
        10, 17, 10), 3, 3, byrow=T)
  xb <- matrix(c(10, 5, 10, 31, 10, 4, 
                 22, 18, 9), 3, 3, byrow=T)
  
  
  
#   Enter the weights in the same order

  w<-matrix(c(1.0, 0.5, 0.0, 0.5, 1.0, 
       0.5, 0.0, 0.5, 1.0), 3, 3, byrow=T)  

#  Execute the function defined above

  kappa.f(x, w)

# Execute the kappa.f  function without specifying 
#  weights  (use the default weights)

  kappa.f(x)
