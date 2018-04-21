setwd("c:/Users/fanne/Dropbox/Homework/STAT557/STAT557hw2YifanZhu/")

#1
X <- matrix(c(784,311,236,66), ncol = 2)
o <- chisq.test(X, correct = F)
o
o$expected

G2 <- 2*sum(X*log(X/o$expected))
G2

#2
y <- c(784,311,236,66)
n <- sum(y)
ya <- rep(0,4)
ya[1] <- (2*y[1] + y[2] + y[3])^2/(4*n)
ya[2] <- (2*y[1] + y[2] + y[3])*(2*y[4] + y[2] + y[3])/(4*n)
ya[3] <- ya[2]
ya[4] <- (2*y[4] + y[2] + y[3])^2/(4*n)

yb <- rep(0,4)
yb[1] <- y[1]
yb[2] <- (y[2] + y[3])/2
yb[3] <- yb[2]
yb[4] <- y[4]

GA <- 2*sum(y*log(y/ya))
GB <- 2*sum(y*log(y/yb))
1 - pchisq(GA,2)
1 - pchisq(GB,1)

GA - GB
1 - pchisq(GA - GB, 1)
