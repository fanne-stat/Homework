# (a) Generate X = RY

library(Rlab)
x <- rbern(n = 100, p = 0.3) * rpois(n = 100, lambda = 2)

# (c) Simulation using Gibbs Sampler

# Initialize lambda and p
lambda <- 1
p <- 0.5

# vector to store simulated lambda's and p's
lambda_chain <- array(dim = 5000)
p_chain <- array(dim = 5000)

# Gibbs Sampling
a <- 1
b <- 1
n <- length(x)
Ix <- ifelse(x==0,1,0)
rx <- 1 - Ix
Sx <- sum(x)
for (i in 1:5000){
  r <- rbern(n = 100, p = (p*exp(-lambda))/(p*exp(-lambda) + (1 - p))) * Ix + rx  
  lambda <- rgamma(1, shape = a + Sx, rate = b + sum(r))
  lambda_chain[i] <- lambda
  p <- rbeta(1, shape1 = 1 + sum(r), shape2 = n + 1 - sum(r))
  p_chain[i] <- p
}

# drop the first 1000 lambda's and p's
lambda_chain <- lambda_chain[1001:5000]
p_chain <- p_chain[1001:5000]

quantile(lambda_chain, c(0.025, 0.975))
quantile(p_chain, c(0.025,0.975))