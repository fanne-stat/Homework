n <- 1500;
X <- rweibull(n, shape = 3, scale = 4^(1/3));
mean(X^2)