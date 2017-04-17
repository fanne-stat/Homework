library(MASS)

y <- c(47, 76, 97, 107, 123, 139, 152, 159, 191, 201, 200, 207)
x <- rep(c(0.02, 0.06, 0.11, 0.22, 0.56, 1.10), each = 2)

f <- function(theta,x){
  return(theta[1]*x/(x + theta[2]))
}

A <- function(theta, x){
  df1 <- x/(x + theta[2])
  df2 <- - theta[1]*x/(x + theta[2])^2
  return(matrix(c(df1, df2), ncol = 2))
}

Z <- function(theta, y, x){
  return(y - f(theta,x))
}

GN <- function(thetat, y, x){
  At <- A(thetat, x)
  Zt <- Z(thetat, y, x)
  thetat_new <- thetat + ginv(t(At)%*%At)%*%t(At)%*%Zt
  condition <- (sqrt(sum((thetat_new - thetat)^2)/(sum((thetat)^2) + 0.00005)) > 0.000001)
  while (condition){
    thetat <- thetat_new
    At <- A(thetat, x)
    Zt <- Z(thetat, y, x)
    thetat_new <- thetat + ginv(t(At)%*%At)%*%t(At)%*%Zt
    condition <- (sqrt(sum((thetat_new - thetat)^2)/(sum((thetat)^2) + 0.00005)) > 0.000001)
    if (is.na(condition))
      return("Not Converge")
  }
  return(thetat)
}



