#setwd(.)


# enter the data 

  biledat <- read.table("bile.txt",
             col.names=c("id","sex","pbb","weight","tier","age","y"))


# Convert tier into a factors

  biledat$tier<-as.factor(biledat$tier)

# Fit a logistic regression model

 bile1 <- glm(y ~ tier + sex + weight+ age +pbb, family=binomial, data=biledat)
summary(bile1)
library(boot)

library(ggplot2)

biledat$pr <- (biledat$y - bile1$fitted.values)/sqrt(bile1$fitted.values * (1 - bile1$fitted.values))


biledat$dr <- residuals(bile1, type = "deviance")

pz6 <- ggplot(data = biledat, aes(x = weight, y = pr)) + geom_smooth(se = F) + geom_point() + labs(y = "Pearson Residuals", x = "Z6(Weight)")

pz7 <- ggplot(data = biledat, aes(x = age, y = pr)) + geom_smooth(se = F) + geom_point() + labs(y = "Pearson Residuals", x = "Z7(Age)")

pz8 <- ggplot(data = biledat, aes(x = pbb, y = pr)) + geom_smooth(se = F) + geom_point() + labs(y = "Pearson Residuals", x = "Z8(PBB)")

dz6 <- ggplot(data = biledat, aes(x = weight, y = dr)) + geom_smooth(se = F) + geom_point() + labs(y = "Deviance Residuals", x = "Z6(Weight)")

dz6 <- ggplot(data = biledat, aes(x = weight, y = dr)) + geom_smooth(se = F) + geom_point() + labs(y = "Deviance Residuals", x = "Z6(Weight)")

dz7 <- ggplot(data = biledat, aes(x = age, y = dr)) + geom_smooth(se = F) + geom_point() + labs(y = "Deviance Residuals", x = "Z7(Age)")

dz8 <- ggplot(data = biledat, aes(x = pbb, y = dr)) + geom_smooth(se = F) + geom_point() + labs(y = "Deviance Residuals", x = "Z8(PBB)")

bile2 <- glm(y ~ tier + sex + weight+ age +pbb + sex:weight + sex:age + sex:pbb + weight:age + weight:pbb + age:pbb, family=binomial, data=biledat)

selectModel <- step(object = bile2, scope = list(upper = ~ tier + sex + weight+ age +pbb + sex:weight + sex:age + sex:pbb + weight:age + weight:pbb + age:pbb, lower = ~ tier + sex + weight+ age +pbb), direction = c("both"))

leverage <- ggplot(data = biledat, aes(x = id, y = h, color = as.factor(y))) + geom_point()

leverage + geom_hline(yintercept = 27/nrow(biledat))

incl_select <- influence.measures(selectModel)

biledat$influencec <- incl_select$infmat[,"cook.d"]

inf_plot <- ggplot(data = biledat, aes(x = id, y = influencec, color = as.factor(y))) + geom_point()



