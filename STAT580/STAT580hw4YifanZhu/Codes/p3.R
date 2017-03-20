mcint <- c(0,0,0); 
i <- 1;

for (v in c(0.1, 1, 10)){
  n <- 150000;
  X <- rnorm(n, mean = 1.5, sd = v)
  mcint[i] <- mean(v * exp(-X^2/2 + (X - 1.5)^2/(2*v^2)) * ifelse(X > 1 & X < 2, 1, 0))
  i <- i+1
}
mcint
