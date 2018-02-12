n = 25
mu = 5
lambda = 2
M = 10000
M1 = 2000

source("./functions.R")

y <- matrix(rinvgauss(n*M, mean = mu, shape = lambda), nrow = n)

ints <- apply(y, 2, intervals)
dim(ints) <- c(2,12, M)

muints <- ints[,2*(1:6)-1,]
lambdaints <- ints[,2*(1:6),]

mucoverrate <- apply(apply(muints, c(2,3), muiscovered),1,sum)/M
lambdacoverrate <- apply(apply(lambdaints, c(2,3), lambdaiscovered),1,sum)/M

muoutp <- apply(apply(muints, c(2,3), isout), 1, sum)/M
lambdaoutp <- apply(apply(lambdaints, c(2,3), isout), 1, sum)/M

muwidth <- apply(apply(muints, c(2,3), intwidth), 1, median)
lambdawidth <- apply(apply(lambdaints, c(2,3), intwidth), 1, median)

dmu <- data.frame(coverage_rate = mucoverrate, median_width = muwidth, prob_out = muoutp, row.names = c("wald", "inv_lrt", "boot1", "boot2", "boot3", "boot4"))

dlambda <- data.frame(coverage_rate = lambdacoverrate, median_width = lambdawidth, prob_out = lambdaoutp, row.names = c("wald", "inv_lrt", "boot1", "boot2", "boot3", "boot4"))

out1 <- paste("./n=", n, "_lambda=",lambda, "_mu.csv", sep="")
write.csv(dmu, out1, quote = F)
out2 <- paste("./n=", n, "_lambda=",lambda, "_lambda.csv", sep="")
write.csv(dlambda, out2, quote = F)
