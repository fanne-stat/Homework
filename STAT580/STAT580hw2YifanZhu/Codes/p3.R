
require(ggplot2)

sample1 <- function (n)
{
  size <- 0;
  O <- rep(0,5000);
  while (size < 5000)
  {
    U <- runif(1);
    X <- rexp(1);
    if (U <= 1/(1 + X^2))
    {
      size <- size + 1;
      O[size] <- X;
    }
  }
  
  return(O)
}

sample2 <- function (n)
{
  size <- 0;
  O <- rep(0,5000);
  while (size < 5000)
  {
    U <- runif(1);
    X <- abs(rcauchy(1));
    if (U <= exp(-X))
    {
      size <- size + 1;
      O[size] <- X;
    }
  }
  
  return(O)
}

d1 <- data.frame(x = sample1(5000));
ggplot(d1, aes(x = x)) + geom_histogram(aes(y = ..density..), binwidth = 0.1, color = "blue", fill = "white") + geom_density(alpha = .2 , colour = "red",fill = "#FF6666") + xlim(0,5)

d2 <- data.frame(x = sample2(5000));
ggplot(d2, aes(x = x)) + geom_histogram(aes(y = ..density..), binwidth = 0.1, color = "blue", fill = "white") + geom_density(alpha = .2 , colour = "red",fill = "#FF6666") + xlim(0,5)

system.time(sample1(5000))

system.time(sample2(5000))


