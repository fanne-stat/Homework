model1gen <- function(lambda, gamma, sigma, tau, Time){
  Y <- rep(0, Time)
  mu0 <- rnorm(mean = lambda, sd = tau, n = 1)
  Y[1] <- mu0
  mutm1 <- mu0
  for(i in 2:Time){
    mut <- lambda + gamma * (mutm1 - lambda)
    wt <- rnorm(0, sigma, n = 1)
    Y[i] <- mut + wt
    mutm1 <- mut
  }
  return(Y)
}

model2gen <- function(lambda, gamma, sigma, tau, Time){
  Y <- rep(0, Time)
  mu0 <- rnorm(mean = lambda, sd = sqrt(tau^2/(1 - gamma^2)), n = 1)
  Y[1] <- mu0
  mutm1 <- mu0
  for(i in 2:Time){
    vt <- rnorm(n = 1, mean = 0, sd = tau)
    mut <- lambda + gamma * (mutm1 - lambda) + vt
    wt <- rnorm(0, sigma, n = 1)
    Y[i] <- mut + wt
    mutm1 <- mut
  }
  return(Y)
}

model3gen <- function(lambda, gamma, sigma, tau, Time){
  Y <- rep(0, Time)
  mu0 <- rnorm(mean = lambda, sd = tau, n = 1)
  Y[1] <- mu0
  mutm1 <- mu0
  for(i in 2:Time){
    vt <- rnorm(n = 1, mean = 0, sd = tau)
    mut <- mutm1 + vt
    wt <- rnorm(0, sigma, n = 1)
    Y[i] <- mut + wt
    mutm1 <- mut
  }
  return(Y)
}

model4gen <- function(mu, gamma, sigma, Time){
  Y <- rep(0, Time)
  W0 <- rnorm(mean = 0, sd = sqrt(sigma^2/(1 - gamma^2)), n = 1)
  Y[1] <- mu + W0
  Wtm1 <- W0
  for(i in 2:Time){
    epsilont <- rnorm(n = 1, mean = 0, sd = sigma)
    Wt <- gamma * Wtm1 + epsilont
    Y[i] <- mu + Wt
    Wtm1 <- Wt
  }
  return(Y)
}

plotseries <- function(Time, n, func){
  # browser()
  gen <- match.fun(func)
  M <- NULL
  for(k in 1:n){
    series <- gen(Time)
    M <- cbind(M, series)
  }
  plot(x = 1:Time, y = 1:Time, 'n', ylim = 1.1*c(min(M), max(M)), xlab = "t", ylab = "Y")
  for(k in 1:n){
    lines(x = 1:Time, y = M[,k], col = k)
  }
}

plotautocov <- function(Time, n, func){
  # browser()
  gen <- match.fun(func)
  M <- NULL
  for(k in 1:n){
    series <- gen(Time)
    M <- cbind(M, acf(series, type = "covariance", plot = F)$acf)
  }
  plot(x = 1:nrow(M), y = 1:nrow(M), 'n', ylim = 1.1*c(min(M), max(M)), xlab = "time lag", ylab = "autocovariance")
  for(k in 1:n){
    lines(x = 1:nrow(M), y = M[,k], col = k)
  }
}