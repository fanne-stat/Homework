# Generate X = RY

library(Rlab)
x <- rbern(n = 100, p = 0.3) * rpois(n = 100, lambda = 2)


# parameter used for Gibbs Sampling
a <- 1
b <- 1

dyn.load("~/Desktop/Homework/STAT580/STAT580hw5YifanZhu/Codes/Gibbs.so")

samples <- .Call("Gibbs", a, b, x)

# CI for lambda 
quantile(samples[[1]], c(0.025, 0.975))

# CI  for p
quantile(samples[[2]], c(0.025, 0.975))
