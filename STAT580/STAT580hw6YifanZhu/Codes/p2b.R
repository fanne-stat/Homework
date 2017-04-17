library(MASS)

y <- c(47, 76, 97, 107, 123, 139, 152, 159, 191, 201, 200, 207)
x <- rep(c(0.02, 0.06, 0.11, 0.22, 0.56, 1.10), each = 2)

dg <- function(theta, y, x){
  dg1 <- 2* sum((y - (theta[1]*x/(x + theta[2]))) * (x / (x + theta[2])))
  dg2 <- -2 * sum((y - (theta[1] * x/ (x + theta[2]))) * (theta[1] * x / (x + theta[2])^2))
  return(c(dg1, dg2))
}

ddg <- function(theta, y, x){
  ddg11 <- -2 * sum(x^2/(x + theta[2])^2)
  ddg22 <- 2 * sum(2*theta[1]*x*y/(x + theta[2])^3 - 3 * (theta[1] * x)^2/(x + theta[2])^4)
  ddg12 <- 2 * sum(2*theta[1]*x^2/(x + theta[2])^3 - x*y/(x + theta[2])^2)
  return(matrix(c(ddg11, ddg12, ddg12, ddg22), nrow = 2))
}

Newton <- function(thetat, y, x){
  thetat_new <- thetat - ginv(ddg(thetat,y, x))%*%dg(thetat,y,x)
  condition <- (sqrt(sum((thetat_new - thetat)^2)/(sum((thetat)^2) + 0.00005)) > 0.000001)
  while (condition){
    thetat <- thetat_new
    thetat_new <- thetat - ginv(ddg(thetat,y, x))%*%dg(thetat,y,x)
    condition <- (sqrt(sum((thetat_new - thetat)^2)/(sum((thetat)^2) + 0.00005)) > 0.000001)
    if (is.na(condition))
      return("Not Converge")
  }
  return(thetat)
}




