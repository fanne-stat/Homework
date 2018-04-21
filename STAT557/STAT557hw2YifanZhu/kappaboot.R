


#   This file contains R code for computing the Kappa statistic, 
#   or weighted Kappa statistic, and bootstrpped standard errors 
#   and confidence intervals. It is applied to ratings of student 
#   teachers by two supervisors.

#   The file is posted as  kappaboot.R

# First create a function to compute kappa. Here 
# xlist has one row for each subject, and exactly
# two columns to indicate the categories reported 
# by the two raters.  w is a square matrix containing
# weights.  inds is a list of indicies used by the
# boostrap function

kappaf2 <- function(xlist, inds, w=NULL)
{

#  Compute the two-way table of counts

x=table(xlist[inds ,1], xlist[inds ,2])

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

  kappaout <- cbind(kappa, kappaw)
  kappaout
}



#   Enter the observed counts into a matrix, reading
#   across the rows of the 2x2 table

  x <- xb
  
#   Enter the weights in the same order

  w<-matrix(c(1.0, 0.5, 0.0, 0.5, 1.0, 
       0.5, 0.0, 0.5, 1.0), 3, 3, byrow=T)  

#  Create a matrix with one row for each item
#  that the raters evaluate.  The result for 
#  the first rater is in the first column and 
#  the result for the second rater is in the 
#  second column.


  ntotal <-  sum(x)
  xnew <- matrix (0, nrow=ntotal, ncol=2)
  nn1 <- 1
  nr <- nrow(x)
  nc <- ncol(x)
     for(i in 1:nr) { for(j in 1:nc) {
       if(x[i,j]>0) {
       nn2 <-x[i, j]+nn1-1
       xnew[nn1:nn2, ] <- matrix( c(i,j), nrow=x[i,j], ncol=2, byrow=T) 
      nn1 <- nn2+1 } } }
     
 

# Execute the kappa.f  function

  inds <- 1:ntotal
  kappaf2(xnew, inds, w)


# Calculate bootstrap value of the standard error 
# and confidence intervals for kappa

library(boot)
results <- boot(xnew, statistic=kappaf2, R=2000) 

# view results
results

hist(results$t[ , 1], nclass=100)
hist(results$t[ , 2], nclass=100)
plot(density(results$t[ ,1]))
plot(density(results$t[ ,2]))


# Get 95% confidence intervals for the unweighted 
# kappa  (index=1 because results are in first
# column of results$t

boot.ci(results, type="all", index=1)


# Get 95% confidence intervals for the weighted 
# kappa  (index=2 because results are in first
# column of results$t

boot.ci(results, type="all", index=2)

