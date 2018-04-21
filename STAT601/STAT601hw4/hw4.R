data <- read.table("./bivnormdat.txt", head = T)
z <- data[!(is.na(data["x"])|is.na(data["y"])),]
u <- data[is.na(data["x"]),]
v <- data[is.na(data["y"]),]

z <- t(z)

yj <- u[,"y"]
xk <- v[,"x"]

#2 MLE with only zi
mu_2 <- apply(z, MARGIN = 1, mean)

quardra <- function(x){
  return(x%*%t(x))
}

quadra_sum <- function(X){
  s <- apply(apply(X, MARGIN = 2, quardra), MARGIN = 1, sum)
  return(matrix(s, nrow = 2))
}

zminusmu <- z - matrix(rep(mu_2, ncol(z)), nrow = 2)

Sigma_2 <- quadra_sum(zminusmu)/ncol(zminusmu)

## the estimate with this method is
theta_2 <- list(mu = mu_2, Sigma = Sigma_2)

theta_2

#1 Using EM Algorithm
Eu <- function(y_j, mu_p, Sigma_p){
  Exjm <- mu_p[1] + (Sigma_p[1,2]/Sigma_p[2,2])*(y_j - mu_p[2])
  return(c(Exjm, y_j))
}

Ev <- function(x_k, mu_p, Sigma_p){
  Eykm <- mu_p[2] + (Sigma_p[2,1]/Sigma_p[1,1])*(x_k - mu_p[1])
  return(c(x_k, Eykm))
}

Vu <- function(mu_p, Sigma_p){
  return(matrix(c(Sigma_p[1,1] - Sigma_p[1,2]^2/Sigma_p[2,2], 0, 0, 0), nrow = 2))
}

Vv <- function(mu_p, Sigma_p){
  return(matrix(c(0, 0, 0, Sigma_p[2,2] - Sigma_p[2,1]^2/Sigma_p[1,1]), nrow = 2))
}

theta_update <- function(z, yj, xk, mu_p, Sigma_p){
  n <- nrow(data)
  
  zsum <- apply(z, MARGIN = 1, sum)
  Eus <- sapply(yj, Eu, mu_p = mu_p, Sigma_p = Sigma_p)
  Eusum <- apply(Eus, MARGIN = 1, sum)
  Evs <- sapply(xk, Ev, mu_p = mu_p, Sigma_p = Sigma_p)
  Evsum <- apply(Evs, MARGIN = 1, sum)
  mu_new <- (zsum + Eusum + Evsum)/n
  
  zminusmu <- z - matrix(rep(mu_new, ncol(z)), nrow = 2)
  Euminusmu <- Eus - matrix(rep(mu_new, ncol(Eus)), nrow = 2)
  Evminusmu <- Evs - matrix(rep(mu_new, ncol(Evs)), nrow = 2)
  
  Sigma_new <- (1/n)*(quadra_sum(zminusmu) + quadra_sum(Euminusmu) + quadra_sum(Evminusmu) + Vu(mu_p, Sigma_p)*length(yj) + Vv(mu_p, Sigma_p)*length(xk))
  
  return(list(mu = mu_new, Sigma = Sigma_new))
}


# initialize with result from question 2
theta_old <- list(mu = mu_2, Sigma = Sigma_2)
iter <- 0
print(noquote(paste(c("iteration", "mu_x", "mu_y", "sigma_xx", "sigma_xy", "sigma_yy"), collapse = '&')))
repeat{
    iter <- iter + 1  
    theta_new <- theta_update(z, yj, xk, theta_old$mu, theta_old$Sigma)
    print(noquote(paste(c(iter, c(theta_new$mu, theta_new$Sigma)[-4]), collapse='&')))
    diff <- (c(theta_new$mu, theta_new$Sigma) - c(theta_old$mu, theta_old$Sigma))[-4]
    diffnorm <- sqrt(sum(diff^2))
    if(diffnorm < 1e-8)
      break
    theta_old <- theta_new
}

theta_new

# 3 using conditional mean to replace missing data
theta_update_3 <- function(z, yj, xk, mu_p, Sigma_p){
  n <- nrow(data)
  
  zsum <- apply(z, MARGIN = 1, sum)
  Eus <- sapply(yj, Eu, mu_p = mu_p, Sigma_p = Sigma_p)
  Eusum <- apply(Eus, MARGIN = 1, sum)
  Evs <- sapply(xk, Ev, mu_p = mu_p, Sigma_p = Sigma_p)
  Evsum <- apply(Evs, MARGIN = 1, sum)
  mu_new <- (zsum + Eusum + Evsum)/n
  
  zminusmu <- z - matrix(rep(mu_new, ncol(z)), nrow = 2)
  Euminusmu <- Eus - matrix(rep(mu_new, ncol(Eus)), nrow = 2)
  Evminusmu <- Evs - matrix(rep(mu_new, ncol(Evs)), nrow = 2)
  
  Sigma_new <- (1/n)*(quadra_sum(zminusmu) + quadra_sum(Euminusmu) + quadra_sum(Evminusmu))
  
  return(list(mu = mu_new, Sigma = Sigma_new))
}

theta_old <- list(mu = mu_2, Sigma = Sigma_2)
iter <- 0
print(noquote(paste(c("iteration", "mu_x", "mu_y", "sigma_xx", "sigma_xy", "sigma_yy"), collapse = '&')))
repeat{
  iter <- iter + 1  
  theta_new <- theta_update_3(z, yj, xk, theta_old$mu, theta_old$Sigma)
  print(noquote(paste(c(iter, c(theta_new$mu, theta_new$Sigma)[-4]), collapse='&')))
  diff <- (c(theta_new$mu, theta_new$Sigma) - c(theta_old$mu, theta_old$Sigma))[-4]
  diffnorm <- sqrt(sum(diff^2))
  if(diffnorm < 1e-8)
    break
  theta_old <- theta_new
}

theta_new