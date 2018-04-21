library(statmod)

thetamle <- function(y){
  muhat <- sum(y)/n
  lambdahat <- 1/(sum(1/y)/n - 1/muhat)
  return(c(muhat, lambdahat))
}

vthetamle <- function(thetahat){
  vmu <- thetahat[1]^3/(thetahat[2]*n)
  vlambda <- 2*thetahat[2]^2/n
  return(c(vmu, vlambda))
}

muwaldint <- function(muhat, vmu){
  c(muhat - 1.96*sqrt(vmu), muhat + 1.96*sqrt(vmu))
}

lambdawaldint <- function(lambdahat, vlambda){
  c(lambda - 1.96*sqrt(vlambda), lambdahat + 1.96*sqrt(vlambda))
}


#---------------------------------------------------------------
loglike <- function(mu, lambda, y){
  n*log(lambda)/2 - sum(y)*lambda/(2*mu^2) + lambda*n/mu - sum(1/y)*lambda/2
}

mulrt <- function(mu, thetahat, y){
  lambdatil <- 1/(sum(y)/(n*mu^2) + sum(1/y)/n - 2/mu)
  return(loglike(thetahat[1], thetahat[2], y) - loglike(mu, lambdatil, y) - 1.92)
}

lambdalrt <- function(lambda, thetahat, y){
  mutil <- sum(y)/n
  return(loglike(thetahat[1], thetahat[2], y) - loglike(mutil, lambda, y) - 1.92)
}


muinvlikint <- function(y, thetahat, muwald){
  lower <- uniroot(function(mu) mulrt(mu, thetahat, y), c(0.01, thetahat[1]))$root
  upper <- tryCatch({uniroot(function(mu) mulrt(mu, thetahat, y), c(thetahat[1], 10*muwald[2]))$root}, error = function(err) Inf)
  return(c(lower, upper))
}

lambdainvlikint <- function(y, thetahat, lambdawald){
  lower <- uniroot(function(lambda) lambdalrt(lambda, y = y, thetahat = thetahat), c(0.01, thetahat[2]))$root
  upper <- tryCatch({uniroot(function(lambda) lambdalrt(lambda, y = y, thetahat = thetahat), c(thetahat[2], 10*lambdawald[2]))$root}, error = function(err) Inf)
  return(c(lower, upper))
}

bootstatmle <- function(thetahat){
  yboot <- matrix(rinvgauss(n*M1, mean = thetahat[1], shape = thetahat[2]), nrow = n)
  return(apply(yboot, 2, thetamle))
}

bootintmle12 <- function(thetastar, thetahat){
  thetastarlu <- apply(thetastar, 1, function(x) quantile(x, c(0.025,0.975)))
  thetal1 <- 2*thetahat - thetastarlu[2,]
  thetau1 <- 2*thetahat - thetastarlu[1,]
  thetal2 <- thetahat^2/thetastarlu[2,]
  thetau2 <- thetahat^2/thetastarlu[1,]
  return(matrix(c(thetal1, thetal2, thetau1, thetau2), byrow = T, nrow = 2))
}

bootintmle3 <- function(thetastar, thetahat, vthetahat){
  vthetastar <- apply(thetastar, 2, vthetamle)
  z <- (thetastar - thetahat)/sqrt(vthetastar)
  zlu <- apply(z,1,function(x) quantile(x,c(0.025, 0.975)))
  thetal3 <- thetahat -sqrt(vthetahat)*zlu[2,]
  thetau3 <- thetahat -sqrt(vthetahat)*zlu[1,]
  return(matrix(c(thetal3, thetau3), byrow = T, nrow = 2))
}

thetamme <- function(y){
  muhat <- sum(y)/n
  lambdahat <- 1/(sum(y^2)/(muhat^3*n) - 1/muhat)
  return(c(muhat, lambdahat))
}

bootstatmme <- function(thetahat){
  yboot <- matrix(rinvgauss(n*M1, mean = thetahat[1], shape = thetahat[2]), nrow = n)
  return(apply(yboot, 2, thetamme))
}

bootintmme <- function(thetastar, thetahat){
  thetastarlu <- apply(thetastar, 1, function(x) quantile(x, c(0.025,0.975)))
  thetal <- 2*thetahat - thetastarlu[2,]
  thetau <- 2*thetahat - thetastarlu[1,]
  return(matrix(c(thetal, thetau), byrow = T, nrow = 2))
}

bootmleints <- function(thetahat, vthetahat){
  thetastar <- bootstatmle(thetahat)
  return(c(bootintmle12(thetastar, thetahat), bootintmle3(thetastar, thetahat, vthetahat)))
}

bootmmeints <- function(thetahat){
  thetastar <- bootstatmme(thetahat)
  return(bootintmme(thetastar, thetahat))
}


muiscovered <- function(x){
  (x[1] < mu)*(x[2] > mu)
}

lambdaiscovered <- function(x){
  (x[1] < lambda)*(x[2] > lambda)
}

isout <- function(x){
  (x[1]<0)
}

intwidth <- function(x){
  x[2] - x[1]
}



intervals <- function(y){
  thetahat <- thetamle(y)
  vtheta <- vthetamle(thetahat)
  thetamme <- thetamme(y)
  muwald <- muwaldint(thetahat[1], vtheta[1])
  lambdawald <- lambdawaldint(thetahat[2], vtheta[2])
  muinvlik <- muinvlikint(y, thetahat, muwald)
  lambdainvlik <- lambdainvlikint(y, thetahat, lambdawald)
  bootmle <- bootmleints(thetahat, vtheta)
  bootmme <- bootmmeints(thetamme)
  return(c(muwald, lambdawald, muinvlik, lambdainvlik, bootmle, bootmme))
}
