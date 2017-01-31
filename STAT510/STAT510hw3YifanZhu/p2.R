library(curl)

fabric <- read.delim(curl("http://dnett.github.io/S510/FabricLoss.txt"))

fabric$surface <- factor(fabric$surface)
fabric$filler <- factor(fabric$filler)
fabric$p <- factor(fabric$p)

o=lm(y~0+surface:filler:p,data=fabric)



estimate=function(lmout,C,a=0.05)
{
  b=coef(lmout)
  V=vcov(lmout)
  df=lmout$df
  Cb=C%*%b
  se=sqrt(diag(C%*%V%*%t(C)))
  tval=qt(1-a/2,df)
  low=Cb-tval*se
  up=Cb+tval*se
  m=cbind(C,Cb,se,low,up)
  dimnames(m)[[2]]=c(paste("c",1:ncol(C),sep=""),
                     "estimate","se",
                     paste(100*(1-a),"% Conf.",sep=""),
                     "limits")
  m
}

C <- matrix(c(0,0,0,0,0,0,1,0,0,0,0,0,
              1/6,1/6,0,0,1/6,1/6,0,0,1/6,1/6,0,0,
              0,0,1/6,1/6,0,0,1/6,1/6,0,0,1/6,1/6,
              0,1/2,0,1/2,0,0,0,0,0,0,0,0), nrow = 4, byrow = T)

estimate(o,C)

test=function(lmout,C,d=0){
  b=coef(lmout)
  V=vcov(lmout)
  dfn=nrow(C)
  dfd=lmout$df
  Cb.d=C%*%b-d
  Fstat=drop(t(Cb.d)%*%solve(C%*%V%*%t(C))%*%Cb.d/dfn)
  pvalue=1-pf(Fstat,dfn,dfd)
  list(Fstat=Fstat,pvalue=pvalue)
}

Ctest1 <- matrix(c(1/6,1/6,-1/6,-1/6,1/6,1/6,-1/6,-1/6,1/6,1/6,-1/6,-1/6), nrow = 1)

test(o,Ctest1)

Ctest2 <- matrix(c(1,-1,-1,1,-1,1,1,-1,0,0,0,0,
                   1,-1,-1,1,0,0,0,0,-1,1,1,-1),nrow = 2, byrow = T)

test(o,Ctest2,d = c(0,0))

Ctest3 <- matrix(c(1/2,1/2,-1/2,-1/2,-1/2,-1/2,1/2,1/2,0,0,0,0,
                   1/2,1/2,-1/2,-1/2,0,0,0,0,-1/2,-1/2,1/2,1/2),nrow = 2, byrow = T)
test(o, Ctest3, d = c(0,0))
