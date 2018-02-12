Ms <- c(100*1:10, 2000)
N <- 2000

P1Ms <- rep(0, N)
P2Ms <- P1Ms

P1M <- function(x,y){
  M <- length(x)
  return(sum(x<y)/M)
}

P2M <- function(x,y){
  M <- length(x)
  x1 <- rep(x, times = M)
  y1 <- rep(y, each = M)
  return(sum(x1 < y1)/M^2)
}
P1Mss <- NULL
P2Mss <- NULL
for (M in Ms){
  xs <- matrix(rnorm(M*N), ncol = N)
  ys <- matrix(rnorm(M*N), ncol = N)
  for (n in 1:N){
    P1Ms[n] <- P1M(xs[,n],ys[,n])
    P2Ms[n] <- P2M(xs[,n],ys[,n])
  }
  P1Mss <- cbind(P1Mss, P1Ms)
  P2Mss <- cbind(P2Mss, P2Ms)
}
ep1m <- apply(P1Mss, MARGIN = 2, FUN = mean)
ep2m <- apply(P2Mss, MARGIN = 2, FUN = mean)
varp1m <- apply(P1Mss, MARGIN = 2, FUN = var)
varp2m <- apply(P2Mss, MARGIN = 2, FUN = var)

result <- list(ep1m = ep1m, ep2m = ep2m, varp1m = varp1m, varp2m = varp2m)

save(P1Mss, P2Mss, result, file = "./results.Rda")


