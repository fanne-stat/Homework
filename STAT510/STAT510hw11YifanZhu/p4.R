library(MASS)
G <- diag(rep(2,3))
Zt <- matrix(c(1,1,0,0,
               0,0,1,0,
               0,0,0,1), byrow = T, nrow = 3)
Sigma <- matrix(c(4,2,0,0,
                  2,4,0,0,
                  0,0,4,0,
                  0,0,0,4),byrow = T, nrow = 4)

Sigma_inv <- ginv(Sigma)

y <- c(51,54,48,52)

X <- matrix(c(1,0,
              0,1,
              1,0,
              1,0),byrow = T, nrow = 4)

betahat <- ginv(t(X)%*%Sigma_inv%*%X)%*%t(X)%*%Sigma_inv%*%y

betahat

yhat <- X%*%betahat

y - yhat

betahat[2] + G%*%Zt%*%Sigma_inv%*%(y - yhat)
