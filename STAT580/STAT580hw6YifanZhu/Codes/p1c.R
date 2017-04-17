library(ggplot2)



n <- length(x)

l <- function(theta){
  x <- c(-13.87, -2.53, -2.44, -2.40, -1.75, -1.34, -1.05, -0.23, -0.07, 0.27, 1.77, 2.76, 3.29, 3.47, 3.71, 3.80, 4.24, 4.53, 43.21, 56.75)
  
  return(- n * log(pi) - sum(log(1 + (theta - x)^2)))
}

theta <- seq(-100, 100, 0.2)

ltheta <- sapply(theta, l)


qplot(x = theta, y = ltheta, geom = "line")


