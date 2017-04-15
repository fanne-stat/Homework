dltheta <- function(theta){
  x <- c(-13.87, -2.53, -2.44, -2.40, -1.75, -1.34, -1.05, -0.23, -0.07, 0.27, 1.77, 2.76, 3.29, 3.47, 3.71, 3.80, 4.24, 4.53, 43.21, 56.75)
  return(-2 * sum((theta - x)/(1 + (theta - x)^2)))
}

ddltheta <- function(theta){
  x <- c(-13.87, -2.53, -2.44, -2.40, -1.75, -1.34, -1.05, -0.23, -0.07, 0.27, 1.77, 2.76, 3.29, 3.47, 3.71, 3.80, 4.24, 4.53, 43.21, 56.75)
  return(-2 * sum((1 - (theta - x)^2)/(1 + (theta - x)^2)^2))
}


Newton <- function(thetat){
  thetat_new <- thetat - dltheta(thetat)/ddltheta(thetat)
  condition <- (abs((thetat_new - thetat)/(thetat + 0.00005)) > 0.0001)
  while (condition){
    thetat <- thetat_new
    thetat_new <- thetat - dltheta(thetat)/ddltheta(thetat)
    condition <- (abs((thetat_new - thetat)/(thetat + 0.00005)) > 0.0001)
    if (is.na(condition))
      return("Not Converge")
  }
  return(thetat)
}

FisherS <- function(thetat){
  x <- c(-13.87, -2.53, -2.44, -2.40, -1.75, -1.34, -1.05, -0.23, -0.07, 0.27, 1.77, 2.76, 3.29, 3.47, 3.71, 3.80, 4.24, 4.53, 43.21, 56.75)
  I <- length(x)/2
  thetat_new <- thetat + dltheta(thetat)/I
  while (abs((thetat_new - thetat)/(thetat + 0.00005)) > 0.0001){
    thetat <- thetat_new
    thetat_new <- thetat + dltheta(thetat)/I
  }
  return(thetat)
}

start <- c(-11, -1, 0, 1.4, 4.1, 4.8, 7, 8, 38)

# Newton Method
for (thetat in start){
  thetahat <- Newton(thetat)
  print(thetahat)
}

# First use Fisher Scoring and then refinr using Newton
for (thetat in start){
  thetahat <- FisherS(thetat)
  thetahat <- Newton(thetahat)
  print(thetahat)
}
