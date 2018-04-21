steel <- read.table(file =
                    "http://www.public.iastate.edu/~maitra/stat501/datasets/steel.dat",
                    header=F, col.names=c("temperature", "yield", "strength"))

# test for univariate normality of the variables


apply(steel[steel$temperature == 1, -1], 2, shapiro.test)

# The above function performs Shapiro-Wilks' test to test for univariate normality of the observations in the first group.

# We get the following output: 
    $yield

            Shapiro-Wilk normality test

    data:  newX[, i] 
    W = 0.99, p-value = 0.9796


    $strength

            Shapiro-Wilk normality test

    data:  newX[, i] 
    W = 0.9524, p-value = 0.754

# clearly, there is support for both distributions being assumed to be
# univariate normal in the first group

# let us try the same for the second group

apply(steel[steel$temperature == 2, -1], 2, shapiro.test)

# here the results for the second group:

      $yield

        	Shapiro-Wilk normality test

      data:  newX[, i] 
      W = 0.9587, p-value = 0.8071


      $strength

                Shapiro-Wilk normality test

      data:  newX[, i] 
      W = 0.9183, p-value = 0.4564

as.numeric(matrix(unlist(apply(steel[steel$temperature == 1, -1], 2, shapiro.test)),ncol=4, by = T)[,2])

##
## test for multivariate normality
##


source("testnormality.R")



system.time(pvals <- testnormality(steel[steel$temperature == 1, -1]))
#   user  system elapsed 
#101.225   0.062 101.820 


sum(sort(pvals) < 1:length(pvals) * 0.05/length(pvals))

#
# By Benjamini and Hochberg False Discovery Rate controls, we get none are  
# significant
#



system.time(pvals <- testnormality(steel[steel$temperature == 2, -1]))
#   user  system elapsed 
#102.838   0.090 103.264 

sum(sort(pvals) < 1:length(pvals) * 0.05/length(pvals))

# we come to the same conclusion in this case also.



library(energy)

mvnorm.etest(x = steel[steel$temperature == 1, -1])

#	Energy test of multivariate normality: estimated parameters

# data:  x, sample size 5, dimension 2, replicates 999 
# E-statistic = 0.5402, p-value = 0.6066

mvnorm.etest(x = steel[steel$temperature == 2, -1])

#	Energy test of multivariate normality: estimated parameters

# data:  x, sample size 7, dimension 2, replicates 999 
# E-statistic = 0.5866, p-value = 0.5095

# let us check now for homogeneity of variance-covariance matrix assumptions.


cov(steel[steel$temperature == 1, -1])
         yield strength
   yield      7.3      4.2
   strength   4.2      4.3

cov(steel[steel$temperature == 2, -1])
            yield strength
   yield    8.333333 6.666667
   strength 6.666667 7.619048

# under the rule of thumb established, it would be ok to assume homogeneity of
# variances and covariances.

# now we perform the two-sample Hotelling T^2-test

library(ICSNP)

HotellingsT2(steel[steel$temperature == 1, -1], steel[steel$temperature == 2, -1])
	Hotelling's two sample T2-test

data:  steel[steel$temperature == 1, -1] and steel[steel$temperature == 2, -1] 
T.2 = 10.7603, df1 = 2, df2 = 9, p-value = 0.004106
alternative hypothesis: true location difference is not equal to c(0,0) 

#
# So there is evidence that temperature has an effect on yield or strength.
#
# actually, both of them individually have are not significantly affected.


