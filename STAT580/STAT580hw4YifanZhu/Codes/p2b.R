n <- 1500;
X <- rnorm(n, mean = 0, sd = 1/2);
Y <- runif(n);
mean(sqrt(pi)*cos(X*Y))