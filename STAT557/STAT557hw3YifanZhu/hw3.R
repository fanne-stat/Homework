setwd("c:/Users/fanne/Dropbox/Homework/STAT557/STAT557hw3YifanZhu/")
#--1---------------
d <- matrix(c(926, 467, 693, 288, 151, 234, 293, 150, 219, 104, 47, 70), nrow = 3)
d
n1 <- sum(d[1,])
n2 <- sum(d[2,])
n3 <- sum(d[3,])
ns<- c(n1, n2, n3)

#--a--
p <- c(9/16, 3/16, 3/16, 1/16)
ed <- matrix(c(p*n1, p*n2, p*n3), byrow = T, nrow = 3)
ed

G2 <- 2*sum(d * log(d/ed))
G2

pvalue <- 1 - pchisq(G2, df = 9)
pvalue

#--b--
pa <- (d[,1] + d[,3])/(ns)
pb <- (d[,1] + d[,2])/(ns)

Pmat <- matrix(c(pa*pb, (1 - pa)*pb, pa*(1-pb), (1-pa)*(1-pb)), nrow = 3)

ed1 <- Pmat*matrix(rep(ns,times = 4), nrow = 3)
G2_1 <- 2*sum(d * log(d/ed1))
G2_1

pvalue1 <- 1 - pchisq(G2_1, df = 3)
pvalue1

#--c--
n <- sum(ns)
pa1 <- sum(d[,1] + d[,3])/n
pb1 <- sum(d[,1] + d[,2])/n

Pmat1 <- c(pa1*pb1, (1 - pa1)*pb1, pa1*(1 - pb1), (1 - pa1)*(1 - pb1))
ed2 <- matrix(c(Pmat1*n1, Pmat1*n2, Pmat1*n3), byrow = T, nrow =3)
G2_2 <- 2*sum(d * log(d/ed2))
G2_2

pvalue2 <- 1 - pchisq(G2_2, df = 7)
pvalue2

#--4--
betail <- c(-61.3183, 2.2110)
covbil <- matrix(c(144.5382,-5.17904,-5.17904,0.185637), nrow = 2)
etail <- betail[1] + 28*betail[2]
piil <- (exp(etail))/(1 + exp(etail))
Dil <- c(1, 28)*(exp(etail)/(1 + exp(etail))^2)
varpiil <- t(Dil)%*%covbil%*%Dil
sepiil <- sqrt(varpiil)
til <- -betail[1]/betail[2]
Dtil <- c(-1/betail[1],betail[1]/betail[2]^2)
vartil <- t(Dtil)%*%covbil%*%Dtil
setil <- sqrt(vartil)
citil <- c(til - 1.96*setil, til+1.96*setil)
