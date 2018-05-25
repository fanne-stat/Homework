library(readxl)
senators<-read_xls("senate_voting_data.xls")

#b) Plot Andrews' curves

senators.names<-names(senators)[-c(1,2)]
rev.party.state.names<-lapply(X=strsplit(gsub(pattern="[.]",replacement="",x=senators.names),split=" "),FUN = rev)

senators.party <- lapply(X = rev.party.state.names, FUN = function(x)(unlist(x)[1]))
senators.party <- unlist(senators.party)

senators.last.names <- lapply(X = rev.party.state.names, FUN = function(x)(unlist(x)[4]))
senators.last.names <- unlist(senators.last.names)


#Create new data.frame for plotting
senators_new <- as.data.frame(t(senators[,-c(1,2)]))

colnames(senators_new) <- NULL
rownames(senators_new) <- NULL

senators_new <- data.frame(senators_new, party = senators.party)


# Use the codes from Canvas
source("ggandrews.R")

# Display the Andrews' curves
ggandrews(senators_new, type = 2, clr = 543, linecol = c("blue", "green", "red"), return_value = FALSE)


epmf <- function(x){
  n <- length(x)
  return(c(sum(x==-1), sum(x==0), sum(x==1))/n)
}

ecdf <- function(epmf){
  return(c(0, epmf[1], epmf[1]+epmf[2], 1))
}

senator_epmf <- sapply(senators_new[,-ncol(senators_new)], epmf)
senator_ecdf <- apply(senator_epmf, MARGIN = 2, ecdf)

library(plyr)

gdtrans <- function(i){
  u1 <- mapvalues(senators_new[,i], from = c(-1,0,1), to = senator_epmf[,i])
  u2 <- mapvalues(senators_new[,i], from = c(-1,0,1), to = senator_ecdf[1:3,i])
  return(u2 + u1*runif(n = length(u1)))
}

inv_gdtrans <- function(u, i){
  -1* (senator_ecdf[1,i] < u[,i] & senator_ecdf[2,i] >= u[,i]) + 1* (senator_ecdf[3,i] < u[,i] & senator_ecdf[4,i] >= u[,i])
}

source("testnormality.R")



senators_c <- sapply(1:542, gdtrans)

senators_normal <- qnorm(senators_c, mean = 0, sd = 1)

testnormality(senators_normal)

library(energy)
mvnorm.etest(x = senators_normal, R = 1000)

library(MASS)
library(Matrix)

# rho based estimation
spearmanrho <- cor(senators_c, method = "spearman")
sigmahat_rho <- 2*sin(pi/6*spearmanrho)
sigmahat_rho <- nearPD(sigmahat_rho, corr = T)
decomp_rho <- eigen(sigmahat_rho$mat, symmetric = T)

s <- decomp_rho$values

pvar<-s/sum(s)

#  cumulative proportion of total variance explained 
#  by each component

cpvar <- cumsum(s)/sum(s)
plot(x = 1:length(cpvar), y = cpvar, 'n', xlab = 'number of PCs', ylab = 'total variance')
lines(x = 1:length(cpvar), y = cpvar)

npc80 <- min(which(cpvar > 0.8))
npc90 <- min(which(cpvar > 0.9))

senators_normal_80 <- senators_normal%*%decomp_rho$vectors[,1:npc80]%*%t(decomp_rho$vectors[,1:npc80])

senators_u_80 <- pnorm(senators_normal_80, mean = 0, sd = 1)

senators_80 <- sapply(1:542, inv_gdtrans, u = senators_u_80)

sum(senators_80==senators_new[,-ncol(senators_new)])/(542*100)


senators_normal_90 <- senators_normal%*%decomp_rho$vectors[,1:npc90]%*%t(decomp_rho$vectors[,1:npc90])

senators_u_90 <- pnorm(senators_normal_90, mean = 0, sd = 1)

senators_90 <- sapply(1:542, inv_gdtrans, u = senators_u_90)

sum(senators_90==senators_new[,-ncol(senators_new)])/(542*100)

# tau based estimation
kendalltau <- cor(senators_c, method = "kendall")
sigmahat_tau <- sin(pi/2*kendalltau)
sigmahat_tau <- nearPD(sigmahat_tau, corr = T)
decomp_tau <- eigen(sigmahat_tau$mat, symmetric = T)

s <- decomp_tau$values

pvar<-s/sum(s)

#  cumulative proportion of total variance explained 
#  by each component

cpvar <- cumsum(s)/sum(s)
plot(x = 1:length(cpvar), y = cpvar, 'n', xlab = 'number of PCs', ylab = 'total variance')
lines(x = 1:length(cpvar), y = cpvar)
npc80 <- min(which(cpvar > 0.8))
npc90 <- min(which(cpvar > 0.9))

senators_normal_80 <- senators_normal%*%decomp_tau$vectors[,1:npc80]%*%t(decomp_tau$vectors[,1:npc80])

senators_u_80 <- pnorm(senators_normal_80, mean = 0, sd = 1)

senators_80 <- sapply(1:542, inv_gdtrans, u = senators_u_80)

sum(senators_80==senators_new[,-ncol(senators_new)])/(542*100)


senators_normal_90 <- senators_normal%*%decomp_tau$vectors[,1:npc90]%*%t(decomp_tau$vectors[,1:npc90])

senators_u_90 <- pnorm(senators_normal_90, mean = 0, sd = 1)

senators_90 <- sapply(1:542, inv_gdtrans, u = senators_u_90)

sum(senators_90==senators_new[,-ncol(senators_new)])/(542*100)

