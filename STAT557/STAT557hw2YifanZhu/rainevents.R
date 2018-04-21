# This code is posted as rainevents.R

# R code for fitting a Poisson
# distribution to count data.  This
# file is stored  as  poisson.ssc

# First create the function "combine" 
# for combining categories to keep all
# expected counts abobe a specified
# lower bound,  mb.  
# Start at the bottom of the array and 
# combine categories until the expected 
# count for the combined category 
# exceeds mb.

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


# Enter the counts.  Do not skip any
# categoies.  Enter a zero if the
# observed count is zero.

x<-c(15,32,26,29,22,19,9,8,3,1,0,0,1,0,0,1)

# Compute the number of categories.

nc<-length(x)

# Enter the category levels.

cc<-0:(nc-1)

# Compute the total sample size.
n<-sum(x)


# Compute mle's for the sample mean 
# and sample variance.

mean<-sum(cc*x)/n
var<-(sum(x*cc^2)-n*mean^2)/n

cat(" *** Sample mean is: ", mean, "\n")
cat(" *** Sample variance is: ", var, "\n") 


# Compute expected counts for the 
# iid Poisson model.

ep<-n*dpois(cc, mean)
ep[nc]<-ep[nc]+n-sum(ep)

# Combine categories to make each expected 
# count at least mb.

mb<-2
comb<-combine(cc, x, ep, nc, mb)
xt<-comb$xt
ept<-comb$ept
k<-comb$k
cl<-comb$cl
cu<-comb$cu

#Compute Pearson statistic.

PEARSON<-sum((xt-ept)^2/ept)

# Compute the G^2 statistic

g2<-0
for(i in 1:k) {
 at<-0
 if(xt[i] > 0) {at<-2*xt[i]*log(xt[i]/ept[i])}
 g2 <- g2+at}

#Compute Fisher Deviance.

FISHERD<-n*var/mean

dff<-n-1
dfp<-k-2
 adiff<-abs(FISHERD-dff)
 al<-0
 if(adiff<dff) {al<-dff-adiff}
PVALF<- 1- pchisq(adiff+dff, dff)+pchisq(al,dff)
PVALP<-1-pchisq(PEARSON, dfp)
PVALG<-1-pchisq(g2,dfp)
#Print the results.
M<-cbind(cl, cu, xt, ept)
dimnames(M)<-
  list(NULL, c("cl", "cu", "observed", "expected")) 
M

cat(" ***  Results for fitting the Poisson distribution ***\n")

cat("Pearson goodness-of-fit statistic =", PEARSON, "\n")
cat("                               df =", dfp, "\n")
cat("                          p-value =", PVALP, "\n")
cat("    Likelihood ratio G2 statistic =", g2, "\n")
cat("                               df =", dfp, "\n")
cat("                          p-value =", PVALG, "\n")
cat("                  Fisher deviance =", FISHERD, "\n")
cat("                               df =", dff, "\n")
cat("                          p-value =", PVALF, "\n")



# Compute confidence interval for mean

a<-0.05
clevel<-1-a

#Use large sample normal distribution.

za<-qnorm(1-a/2)
mlower<-mean-za*sqrt(mean/n)
mupper<-mean+za*sqrt(mean/n)

cat(" *** Large sample confidence interval ***\n")
cat(100*clevel,"% confidence interval: 
 (", mlower, ",", mupper, ")\n") 

#More accurate confidence limits.

mlower<-qchisq(a/2, 2*n*mean)/(2*n)
mupper<-qchisq(1-a/2, 2+2*n*mean)/(2*n)

cat(" *** Exact confidence interval ***\n")
cat(100*clevel," % confidence interval: 
  (", mlower, ",", mupper, ")\n") 











