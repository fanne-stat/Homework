# Creat vector for storing Z's
Z_chain <- array(dim = 1500)

# Initialize Z0
Z <- 1

# parameters for gamma distribution
a <- 1
b <- 1

# parameters for Z
theta1 <- 1.5
theta2 <- 2

for (i in 1:1500){
  Y <- rgamma(1, shape = a, rate = b)
  U <- runif(1)
  r <- (Z/Y)^(3/2)*exp(theta1*(Z - Y) + theta2*(1/Z - 1/Y)) * (Z/Y)^(a - 1)*exp(b*(Y - Z))
  if (U <= r){
    Z <- Y
  }
  Z_chain[i] <- Z
}

# drop the first 500 simulations
Z_chain <- Z_chain[501:1500]

# true values
EZ <- sqrt(theta2/theta1)
EZinverse <- sqrt(theta1/theta2) + 1/(2*theta2)

# estimates
EZ_sim <- mean(Z_chain)
EZinverse_sim <- mean(1/Z_chain)

cbind(EZ, EZinverse, EZ_sim, EZinverse_sim)
