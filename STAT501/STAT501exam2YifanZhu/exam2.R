# read data
wine <- read.table("wine.dat", head = F, sep = ",",
                   col.names = c("Cultivar","Alcohol", "Malic acid", "Ash", "Alkalinity of ash",
                                 "Magnesium", "Total phenols", "Flavanoids", "Nonflavanoid phenols",
                                 "Proanthocyanins", "Color intensity", "Hue", "OD280/OD315", "Proline"))
wine$Cultivar <- as.factor(wine$Cultivar)
# check multivariate normality for each cultivar
source("testnormality.R")
library(energy)
library(dplyr)
wine %>% group_by(Cultivar) %>% do(data.frame(testnormality = testnormality(.[,-1]), 
                                          energytest = mvnorm.etest(.[,-1], R = 999)$p.value))



# cultivar information
cultivar <- wine$Cultivar

# standardize data
wine.sc <- scale(wine[,-1])

source("ggandrews.R")
# Hierarchical clustering with average linkage
hc <- hclust(dist(wine.sc), method = "average")
plot(hc, main = "")

# display using ggandrews
ggandrews(data.frame(cutree(hc, k = 3), wine.sc), clr = 1, return_value = F)

# k-means initialized with hc
kmnsinithcl <- function(x.data, nclus, ncut = nclus, hcl.tree)
{
  x.hcl <- hcl.tree
  x.cl <- cutree(x.hcl, k = ncut)
  data.x <- data.frame(x.data, cl = x.cl)
  means <- aggregate(. ~ cl, data = data.x, FUN = mean)
  return(kmeans(x.data,centers= means[, -1]))
}

km <- kmnsinithcl(wine.sc, nclus = 3, ncut = 3, hcl.tree = hc)

# display using ggandrews
ggandrews(data.frame(km$cluster, wine.sc), clr = 1, return_value = F)

# k-means with random initialization
km.r <- kmeans(wine.sc, centers = 3, nstart = 10000)

# display using ggandrews
ggandrews(data.frame(km.r$cluster, wine.sc), clr = 1, return_value = F)


# model based clustering
library(mclust)
mcl <- Mclust(wine.sc)
plot(mcl$BIC)
plot.Mclust(mcl, what = "classification")

# display using ggandrews
ggandrews(data.frame(mcl$classification, wine.sc), clr = 1, return_value = F)


# compare the results with cultivar information
# hierarchical clustering with average linkage
ftable(table(cultivar, cutree(hc, k = 3)))
# k-means innitialized with hc
ftable(table(cultivar, mapvalues(km$cluster, from = c(2,3,1), c(1,2,3))))
# k-means with random initialization
ftable(table(cultivar, km.r$cluster))
# model based clustering
ftable(table(cultivar, mcl$classification))

# PCA
wine.pc <- prcomp(wine.sc)
#  compute proportion of total variance explained by 
#  each component

s <- wine.pc$sdev^2                                

pvar<-s/sum(s)
cat("proportion of variance: ", pvar, fill=T) 

#  cumulative proportion of total variance explained 
#  by each component

cpvar <- cumsum(s)/sum(s)
cat("cumulative proportion of variance: ", cpvar, fill=T)

# test 5 is enough while 4 is not
source("PCs.proportion.variation.enuff.R")
PCs.proportion.variation.enuff(lambda = s, q = 4, nobs = nrow(wine.sc), propn = 0.8)
PCs.proportion.variation.enuff(lambda = s, q = 5, nobs = nrow(wine.sc), propn = 0.8)

# first 5 PCs
wine.pc$rotation[,1:5]

# split data
set.seed(8413)
train.idx <- sample(1:nrow(wine), size = 128, replace = F)
wine.train <- wine[train.idx,]
wine.test <- wine[-train.idx,]

# QDA
library(MASS)
wine.qda <- qda(Cultivar ~ ., data = wine.train, CV = F)

# AER
mean(wine.train$Cultivar!=predict(wine.qda)$class) 

# CV
wine.qda.cv <- qda(Cultivar ~ ., data = wine.train, CV = T)
mean(wine.train$Cultivar!=wine.qda.cv$class) 

# misclassification on test set
mean(wine.test$Cultivar!=predict(wine.qda, newdata = wine.test[,-1])$class)

#kNN
library(class)
# using cross-validation to pick k
# using scaled data
wine.train.sc <- scale(wine.train[,-1])
wine.test.sc <- scale(wine.test[,-1])
# try k = 1,..,10
knn.cv.err<-NULL
knn.cv.sd<-NULL
for (i in 1:10) { 
  temp<-NULL
  for (j in 1:10000)
    temp <- c(temp,mean(knn.cv(wine.train.sc,
                               cl = wine.train$Cultivar, k = i) != wine.train$Cultivar))
  knn.cv.err<-c(knn.cv.err,mean(temp))
  knn.cv.sd<-c(knn.cv.sd,sd(temp))
  cat("\n Done i= ",i)
}


plot(knn.cv.err, xlim = c(1, 10),
     ylim=c(min(knn.cv.err - 1.96 * knn.cv.sd),
            max(knn.cv.err + 1.96 * knn.cv.sd)), type = "n")
lines(knn.cv.err + 1.96 * knn.cv.sd, lty = 2, col = "blue")
lines(knn.cv.err - 1.96 * knn.cv.sd, lty = 2, col = "green")
lines(knn.cv.err, col = "red")

# use k = 5

wine.knn.train <- knn(train = wine.train.sc, test = wine.train.sc, cl = wine.train$Cultivar, k = 5)

#AER
mean(wine.knn.train != wine.train$Cultivar)

#CV
knn.cv.err[5]

# misclassification on test set
wine.knn <- knn(train = wine.train.sc, test = wine.test.sc, cl = wine.train$Cultivar, k = 5)
mean(wine.knn != wine.test$Cultivar)

# CART
library(tree)
# getting optimal tree using cross-validation
wine.tree <- tree(formula = Cultivar ~ ., data = wine.train)
wine.tree.cv <- cv.tree(wine.tree, K = nrow(wine.train))
plot(wine.tree.cv)
# the best one is 4. Plot the best one.
wine.tree.opt <- prune.tree(wine.tree, k = 4)
plot(wine.tree.opt)
text(wine.tree.opt)

#AER 
mean(apply(predict(wine.tree.opt), 1, which.max)!=wine.train$Cultivar)

#CV
mean(sapply(1:nrow(wine.train), function(x) mean(mean(apply(predict(tree(Cultivar ~ ., data = wine.train[-x,]), newdata = wine.train[,-1]), 1, which.max)!=wine.train$Cultivar))))

# misclassification on test set
mean(apply(predict(wine.tree.opt, newdata = wine.test[,-1]), 1, which.max)!=wine.test$Cultivar)


# display the first 5 PCs using 3d radviz
source("radviz3d-3.R")
radialvis3d(data = wine.pc$x[,1:5], cl = wine$Cultivar)
rgl.snapshot("radviz3d.jpg")
# using 2d radviz
source("radviz2d.R")
source("mmnorm.R")
source("circledraw.R")
radviz2d(dataset = cbind(wine.pc$x[,1:5], wine$Cultivar), name = "wine")
