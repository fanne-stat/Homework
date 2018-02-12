library(statmod)

n = 100
lambda = 4
mu = 5

source("./functions.R")

interval_wald <- function(y){
  thetahat <- thetamle(y)
  vtheta <- vthetamle(thetahat)
  thetamme <- thetamme(y)
  muwald <- muwaldint(thetahat[1], vtheta[1])
  lambdawald <- lambdawaldint(thetahat[2], vtheta[2])
  return(c(muwald, lambdawald))
}

M <- c(500, 1000*1:10, 20000, 50000, 100000, 200000)



mucovered <- rep(0, length(M))
widthmu <- mucovered
lambdacovered <- mucovered
widthlambda <- mucovered
for(i in 1:length(M)){
  m <- M[i]
  y <- matrix(rinvgauss(n*m, mean = mu, shape = lambda), nrow = n)
  ints <- apply(y, 2, interval_wald)
  dim(ints) <- c(2,2,m)
  muiscover <- apply(ints[,1,], 2, muiscovered)
  mucoverrate <- sum(muiscover)/m
  lambdaiscover <- apply(ints[,2,], 2, lambdaiscovered)
  lambdacoverrate <- sum(lambdaiscover)/m
  mucovered[i] <- mucoverrate
  lambdacovered[i] <- lambdacoverrate
  widthmu[i] <- 2*1.96*sqrt(sum((muiscover - mucoverrate)^2)/(m*(m-1)))
  widthlambda[i] <- 2*1.96*sqrt(sum((lambdaiscover - lambdacoverrate)^2)/(m*(m-1)))
}

d <- data.frame(M = M, mucovered = mucovered, widthmu = widthmu, lambdacovered = lambdacovered, widthlambda = widthlambda)
d <- round(d, digits = 3)
out <- paste("./n=", n, "_lambda=",lambda, ".txt", sep="")
write.table(d, out, quote = F,row.names = F)










