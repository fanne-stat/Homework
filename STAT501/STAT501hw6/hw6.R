
library(MASS)
crabs.data <- crabs[,-c(1,2,3)]

#1(a)
crabs.pc <- prcomp(crabs.data)



#  compute proportion of total variance explained by 
#  each component
   
   s <- crabs.pc$sdev^2                                

   pvar<-s/sum(s)
   cat("proportion of variance: ", pvar, fill=T) 

#  cumulative proportion of total variance explained 
#  by each component

   cpvar <- cumsum(s)/sum(s)
   cat("cumulative proportion of variance: ", cpvar, fill=T)
   
# test 2 is enough
#   source("PCs.proportion.variation.enuff.R")
# PCs.proportion.variation.enuff(lambda = s, q = 2, nobs = 200, propn = 0.99)


   crabs.type <-as.factor(paste(crabs[,1], crabs[,2], sep = ""))
    plot(crabs.pc$x[,1],crabs.pc$x[,2],
          xlab="PC1",
          ylab="PC2",type="n")
    text(crabs.pc$x[,1],crabs.pc$x[,2],labels=crabs.type, col = rainbow(4)[as.numeric(crabs.type)])  


#is there a significant difference?

fit.lm <- lm(crabs.pc$x[,1:2]~as.factor(crabs.type))

library(car)
fit.manova <- Manova(fit.lm)
summary(fit.manova)

#1(b)
library(kernlab)
for(sigma in c(0.2, 0.4, 0.8, 1, 1.5, 3)){
  crabs.kpc <- kpca(x = as.matrix(crabs.data),kernel = "rbfdot", kpar = list(sigma = sigma), features = 2)
  plot(crabs.kpc@rotated[,1],crabs.kpc@rotated[,2],
       xlab="PC1",
       ylab="PC2",type="n")
  text(crabs.kpc@rotated[,1],crabs.kpc@rotated[,2],labels=crabs.type, col = rainbow(4)[as.numeric(crabs.type)])  
}


#2(a)
#i
ziptrain <- read.table("ziptrain.dat")
zipdigit <- as.factor(read.table("zipdigit.dat")[,1])
pval_equal_mean<- function(x, cl){
  fit <- lm(x~cl)
  return(anova(fit)[[5]][1])
}
pvals <- sapply(ziptrain, FUN = pval_equal_mean, cl = zipdigit)
pval.bonf <- p.adjust(pvals, "bonferroni")
which(pval.bonf > 0.05)
pval.fdr <- p.adjust(pvals, "fdr")
which(pval.fdr > 0.05)

id <- order(pvals, decreasing = F)
id100 <- id[1:100]

ziptrain100 <- ziptrain[,id100]

source("testnormality.R")
testnormality(X = ziptrain100[zipdigit == 0,])

source("BoxMTest-2.R")
BoxMTest(X = ziptrain100, cl = zipdigit)

#ii
n <- as.vector(table(zipdigit))
means <- list()
for(i in 1:10){
  means[[i]] <- apply(ziptrain100[zipdigit == i-1,], MARGIN = 2, FUN = mean)
}
vars <- list()
for(i in 1:10){
  vars[[i]] <- cov(ziptrain100[zipdigit == i-1,])
}

quard <- function(x, A){
  x <- as.vector(x)
  return(t(x)%*%A%*%x)
}

lambda <- 0


r <- 0
l <- 0
for(i in 1:10){
  r <- r + n[i]*ginv(vars[[i]]) %*% means[[i]]
  l <- l + n[i]*ginv(vars[[i]])
}
muhat <- as.vector(ginv(l)%*%r)

for(i in 1:10){
  Xi <- ziptrain100[zipdigit == i-1,]
  Xicentered <- Xi - matrix(rep(means[[i]], n[i]), ncol = 100, byrow = T)
  Xim <- Xi - matrix(rep(muhat, n[i]), ncol = 100, byrow = T)
  fulli <- sum(apply(Xicentered, MARGIN = 1, FUN = quard, A = ginv(vars[[i]])))
  reducedi <-  sum(apply(Xim, MARGIN = 1, FUN = quard, A = ginv(vars[[i]])))
  lambda <- reducedi - fulli
}

pchisq(lambda, df = 9, lower.tail = F)

#(b)
ziptrain.centered <- NULL
for(i in 1:10){
  mean <- apply(ziptrain[zipdigit == i-1,], MARGIN = 2, FUN = mean)
  ziptrain.centered <- rbind(ziptrain.centered, ziptrain[zipdigit == i-1,] - matrix(rep(mean, n[i]), ncol = 256, byrow = T))
}

ziptrain.pc <- prcomp(ziptrain.centered)
source("PCs.proportion.variation.enuff.R")
p <- rep(0, 256)
for(i in 1:256){
  p[i] <- PCs.proportion.variation.enuff(lambda = ziptrain.pc$sdev^2, q = i, propn = 0.8, nobs = nrow(ziptrain.centered))
}
min(which(p > 0.05))

zipmean <- NULL
for(i in 1:10){
  mean <- apply(ziptrain[zipdigit == i-1,], MARGIN = 2, FUN = mean)
  zipmean <- rbind(zipmean,  matrix(rep(mean, n[i]), ncol = 256, byrow = T))
}

zipproj_full <- ziptrain.pc$x + zipmean%*%ziptrain.pc$rotation
zipproj <- ziptrain.pc$x[,1:40] + zipmean%*%ziptrain.pc$rotation[,1:40]

source("radviz2d.R")
class <- NULL
for(i in 0:9){
  class <- c(class, rep(i, n[i+1]))
}

class <- as.factor(class)
library(dprep)
radviz2d(dataset = cbind(zipproj_full, class), name = "Full dimension")
radviz2d(dataset = cbind(zipproj, class), name = "Reduced dimension")

source("starcoord.R")
starcoord(data = cbind(zipproj_full,class), class = T, main = "Full dimension")
starcoord(data = cbind(zipproj,class), class = T, main = "Reduced dimension")

source("ggandrews.R")
ggandrews(df = cbind(ziptrain.pc$x,class), clr = ncol(ziptrain.pc$x) + 1, type = 2)
