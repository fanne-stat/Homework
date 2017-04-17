library(MASS)

y <- c(47, 76, 97, 107, 123, 139, 152, 159, 191, 201, 200, 207)
x <- rep(c(0.02, 0.06, 0.11, 0.22, 0.56, 1.10), each = 2)

g <- function(theta, y, x){
  return(-sum((y - theta[1]*x/(x + theta[2]))^2))
}

dg <- function(theta, y, x){
  dg1 <- 2* sum((y - (theta[1]*x/(x + theta[2]))) * (x / (x + theta[2])))
  dg2 <- -2 * sum((y - (theta[1] * x/ (x + theta[2]))) * (theta[1] * x / (x + theta[2])^2))
  return(c(dg1, dg2))
}

SA <- function(thetat, y, x, alpha){
  thetat_new <- thetat + alpha * dg(thetat, y, x)
  condition <- (sqrt(sum((thetat_new - thetat)^2)/(sum((thetat)^2) + 0.00005)) > 0.000001)
  while (condition){
    thetat <- thetat_new
    thetat_new <- thetat + alpha * dg(thetat, y, x)
    while (g(thetat_new, y, x) < g(thetat, y, x)){
      alpha <- alpha/2
      thetat_new <- thetat + alpha * dg(thetat, y, x)
    }
    condition <- (sqrt(sum((thetat_new - thetat)^2)/(sum((thetat)^2) + 0.00005)) > 0.000001)
    if (is.na(condition))
      return("Not Converge")
  }
  return(thetat)
}