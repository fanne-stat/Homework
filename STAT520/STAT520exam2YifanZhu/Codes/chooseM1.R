n = 500
lambda = 2
mu = 5

M1 <- seq(50, 10000, 50)


library(statmod)
source("./functions.R")
y <- rinvgauss(n, mean = mu, shape = lambda)
thetahat <- thetamle(y)

boot1ciwidth <- function(M1){
  yboot <- matrix(rinvgauss(n*M1, mean = thetahat[1], shape = thetahat[2]), nrow = n)
  thetastar <- apply(yboot, 2, thetamle)
  thetastarlu <- apply(thetastar, 1, function(x) quantile(x, c(0.025,0.975)))
  return(thetastarlu[2,] - thetastarlu[1,])
}

widths <- matrix(rep(0, 2*length(M1)), nrow = 2)

for(i in 1:length(M1)){
  widths[,i] <- boot1ciwidth(M1[i])
}

out1 <- paste("mu_", n, "_", lambda, ".jpg", sep = "")
out2 <- paste("lambda_", n, "_", lambda, ".jpg", sep = "")

jpeg(out1, height = 540, width = 650, quality = 100)
plot(x = M1, y = widths[1,], xlab = "M1", ylab = "mu_width", 'n')
lines(x = M1, y = widths[1,])
dev.off()

jpeg(out2, height = 540, width = 650, quality = 100)
plot(x = M1, y = widths[2,], xlab = "M1", ylab = "lambda_width", 'n')
lines(x = M1, y = widths[2,])
dev.off()
