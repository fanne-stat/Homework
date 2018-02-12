series1 <- data.frame(n = c(29, 30, 28, 27, 30, 31, 30, 29),
                      dose = c(49.06, 52.99, 56.91, 60.84, 64.76, 68.69, 72.61, 76.54),
                      y = c(0.069, 0.233, 0.329, 0.519, 0.767, 0.936, 0.967, 1 ))
series2 <- data.frame(n = c(30, 30, 34, 29, 33, 28, 32),
                     dose = c(49.06, 52.99, 56.91, 60.84, 64.76, 68.69, 72.61),
                     y = c(0.133, 0.2, 0.265, 0.483, 0.879, 0.857, 1))

total <- rbind(series1, serie2)


series1$logdose <- log(series1$dose)
series2$logdose <- log(series2$dose)
total$logdose <- log(total$dose)

source("./GLM.R")


# basic glm for series 1 data

xmatseries1 <- as.matrix(cbind(rep(1,8),series1[,4]))
nsseries1 <- series1$n

series1bascglm <- basicglm(xmat = xmatseries1, y = series1$y, link = 3, random = 2, ns = nsseries1)

# series 2 basic glm
xmatseries2 <- as.matrix(cbind(rep(1,7),series2[,4]))
nsseries2 <- series2$n

series2bascglm <- basicglm(xmat = xmatseries2, y = series2$y, link = 3, random = 2, ns = nsseries2)


BlissFisher <- function(data, start){
  #browser()
  n <- data$n
  y <- data$y
  y[y==1] <- 1 - 0.00001
  logd <- data$logdose
  xmat <- matrix(c(rep(1, length(logd)), logd), ncol = 2)
  epsilon_n <- start
  repeat {
    epsilon <- epsilon_n 
    beta <- epsilon[1:2]
    lambda <- epsilon[3]
    eta <- xmat%*%beta
    p <- 1 - (1 + lambda*exp(eta))^(-1/lambda)
    etap <- lambda/((1 - p)*(1 - (1 - p)^lambda))
    vp <- p*(1 - p)
    w <- as.vector(n/(etap^2*vp))
    z <- (y - p)*etap
    etalambda <- -log(1 - p)/(1 - (1 - p)^lambda) - 1/lambda
    
    
    xmata <- cbind(xmat, -etalambda)
    epsilon_n <- solve(t(xmata)%*%diag(w)%*%xmata)%*%t(xmata)%*%diag(w)%*%z + epsilon
    if(sqrt(sum((epsilon_n - epsilon)^2)) < 1e-08)
      break
  }
    inv_inf <- solve(t(xmata)%*%diag(w)%*%xmata)
    loglik <- sum(n*(y*log(p) + (1 - y)*log(1 - p)))
    res <- list(epsilon = epsilon_n, inv_inf = inv_inf, estp = p, loglik = loglik)
    return(res)
}


total$response <- round(total$n*total$y)
u <- rep(0, nrow(total))
for(i in 1:nrow(total)){
  u[i] <- pbinom(total$response[i], size = total$n[i], prob = seriestotalbliss$estp[i])
}










