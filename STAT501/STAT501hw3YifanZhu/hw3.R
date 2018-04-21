# 1(b) generate random vector X given X-bar.
library(MASS)
X.gen <- function(xbar, n, sigma){
  mu <- rep(xbar, n-1)
  Sigma <- diag(rep(sigma^2, n-1)) - matrix(rep((sigma^2/n), (n-1)*(n-1)), nrow = n-1)
  Y1 <- mvrnorm(mu = mu, Sigma = Sigma)
  X <- c(Y1, n*xbar - sum(Y1))
  return(X)
}

# 1(c) 
## i. & iii.
## read the image
library(rtiff)
owlet <- readTiff(fn = "Indian_spotted_owlet.tiff")
plot(owlet)



## function super resolution one pixel to 4x4, and truncate at 0 and 1
supres <- function(x){
  y <- X.gen(x, 16, 0.4)
  y1 <- ifelse(y < 0, 0, y)
  y2 <- ifelse(y1 > 1, 1, y1)
  return(matrix(y2, nrow = 4))
}

## high resolution matrix for red channel
size.low <- owlet@size
owletsupr <- apply(X = owlet@red, MARGIN = c(1,2), FUN = supres)
owletsupr1 <- array(dim = c(4,4)*size.low)
for(i in 1:size.low[1]){
  for(j in 1:size.low[2]){
    for(k in 1:4){
      for(l in 1:4){
        owletsupr1[4*(i - 1) + k, 4*(j-1) + l] <- owletsupr[4*(k-1) + l, i, j]
      }
    }
  }
}

## ii.
## high resolution matrix for green and blue

### green channel
owletsupg <- apply(X = owlet@green, MARGIN = c(1,2), FUN = supres)
owletsupg1 <- array(dim = c(4,4)*size.low)
for(i in 1:size.low[1]){
  for(j in 1:size.low[2]){
    for(k in 1:4){
      for(l in 1:4){
        owletsupg1[4*(i - 1) + k, 4*(j-1) + l] <- owletsupg[4*(k-1) + l, i, j]
      }
    }
  }
}

### blue channel
owletsupb <- apply(X = owlet@blue, MARGIN = c(1,2), FUN = supres)
owletsupb1 <- array(dim = c(4,4)*size.low)
for(i in 1:size.low[1]){
  for(j in 1:size.low[2]){
    for(k in 1:4){
      for(l in 1:4){
        owletsupb1[4*(i - 1) + k, 4*(j-1) + l] <- owletsupb[4*(k-1) + l, i, j]
      }
    }
  }
}



owlet.high <- array(dim  = c(owlet@size*c(4,4), 3))
owlet.high[,,1] <- owletsupr1
owlet.high[,,2] <- owletsupg1
owlet.high[,,3] <- owletsupb1

## iv.
### create pixmap object
owlet.highres <- pixmapRGB(owlet.high)

## v.
### display the high resolution image, save
par(mar=c(0,0,0,0))
plot(owlet.highres)
writeTiff(owlet.highres, fn = "./owlet_high.tiff")



# 2.
## 2 (b) i.
library(SMPracticals)
summary(danish)
hist(danish)

## remove the potential outlier
danish1 <- danish[!danish %in% boxplot.stats(danish)$out]
hist(danish1)

## 2 (b) ii.
### define g function
g <- function(x, theta){
  if(theta == 0)
    return(x)
  else
    return(asinh(theta*x)/theta)
}

### define the maximized log likelihood
ell <- function(x, theta){
  n <- length(x)
  y <- g(x, theta)
  muhat <- sum(y)/n
  sigmahat2 <- sum((y - muhat)^2)/n
  return(-(n/2)*log(2*pi* sigmahat2) - (n/2) - sum(log(1 + theta^2 * x^2))/2)
}

### find maxmized log likelihood for each theta and plot the curve
thetas <- seq(0, 4, 0.1)
ells <- sapply(thetas, ell, x = danish)
plot(x = thetas, y = ells, 'n',xlab = "theta", ylab = "maximized logLik")
lines(x = thetas, y = ells)

### plot between theta from 3 to 4
thetas1 <- thetas[thetas >= 3]
ells1 <- ells[thetas >= 3]
plot(x = thetas1, y = ells1, 'n',xlab = "theta", ylab = "maximized logLik")
lines(x = thetas1, y = ells1)

### transform the data with theta = 4
transformed_danish <- g(danish, 4)

hist(transformed_danish, main = "")

qqnorm(transformed_danish, main = "")

### shapiro-wilk's test
shapiro.test(transformed_danish)
