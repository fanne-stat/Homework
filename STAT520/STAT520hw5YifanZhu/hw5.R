setwd("c:/Users/fanne/Dropbox/Homework/STAT520/STAT520hw5YifanZhu/")
d <- read.table("gompertzdat.txt", header = T)
attach(d)

source("nonlin.r")

#---------------------------------------------------------
gompfctn<-function(xs,ps){
  #Gompertz response curve
  #ps is (b1,b2,b3)
  b1<-ps[1]; b2<-ps[2]; b3<-ps[3]
  fs<-b1*exp(-exp(b2-b3*xs))
  return(fs)
}
#--------------------------------------------------------
gompVmat<-function(xs,ps){
  #compute matrix of derivatives for Gompertz model for use with nonlin
  #
  n<-length(xs)
  b1<-ps[1]; b2<-ps[2]; b3<-ps[3]
  t1<-exp(b2-b3*xs)
  t2<-exp((-1)*t1)
  db1<-t2
  db2<-(-1)*b1*t2*t1
  db3<-b1*t2*t1*xs
  V<-matrix(c(db1,db2,db3),n,3,byrow=F)
  return(V)
}
#------------------------------------------------------------
gompwts<-function(xs,ps){
  4
  #weights for gompertz model with power of the mean variances
  #power (thet) must be changed within this function
  #
  thet<-0.50
  mus<-gompfctn(xs,ps)
  ws<-1/(mus^(2*thet))
  W<-diag(ws)
  return(W)
}
#--------------------------------------------------------------

o <- nonlin(xmat = x, ys = y, ps = c(20,3,0.5), fctn = gompfctn, ders = gompVmat, wts = gompwts)
o$sshat
o$bs
o$covb
#--95% ci lower bound---
o$bs - 1.96 * sqrt(diag(o$covb))
#--95% ci upper bound---
o$bs + 1.96 * sqrt(diag(o$covb))

#---ci for beta2/beta3------------------------------------------

D <- function(beta){
  Dbeta <- rep(0,3)
  Dbeta[1] <- 0
  Dbeta[2] <- 1/beta[3]
  Dbeta[3] <- -beta[2]/(beta[3])^2
  return(Dbeta)
}




beta2overbeta3ci(o)

#-------------------------------------------------------------------
library(stats4)

minuslogl <- function(beta1, beta2, beta3, sigma2){
  mu <- beta1*exp(-exp(beta2 - beta3*x))
  v <- sigma2 * mu
  l <- sum(log(2*pi*v)+(y - mu)^2/v)
  return(l)
}

o1 <- mle(minuslogl = minuslogl, start = list(beta1 = 20, beta2 = 3, beta3 = 0.5, sigma2 = 0.2))
