require(Sleuth3)
require(ggplot2)

ggplot(data = case0501, aes(x = Diet, y = Lifetime)) + geom_boxplot()

y <- case0501$Lifetime
xfull <- case0501$Diet
xreduced <- xfull
xreduced[xreduced == 'lopro'] <- 'N/R50'

outfull <- lm(y~0 +xfull)
outreduced <- lm(y~0 +xreduced)

anova(outreduced, outfull)

C <- matrix(c(0,0,1,0,0,-1),nrow = 1)
b <- coef(outfull)
V <- vcov(outfull)
d <- 0
dfn <- nrow(C)
dfd <- outfull$df.residual
Cb.d <- C %*% b - d

Fstat <- drop(t(Cb.d)%*%solve(C%*%V%*%t(C))%*%Cb.d/dfn )

pvalue <- 1-pf(Fstat,dfn,dfd)

list(Fstat=Fstat,pvalue=pvalue)
