# read the data
skulls <- read.table("./Egyptian-skulls.dat")
names(skulls) <- c("max breadth", "basibregmatic height", "basialveolar length", "nasal height", "period")
skulls$period <- as.factor(skulls$period)

# visualization
## radial visualization
library(dprep)
source("radviz2d.R")
radviz2d(skulls, name = "Skulls")
## star plot
source("starcoord.R")
starcoord(data = skulls, class = TRUE, main = "Star coordinate plot for Skulls")
## paired scatter plots
pairs(~., data = skulls[skulls$period == 1, -ncol(skulls)])
pairs(~., data = skulls[skulls$period == 2, -ncol(skulls)])
pairs(~., data = skulls[skulls$period == 3, -ncol(skulls)])
## Chi-squared Q-Q plot
library(MVN)
mvn(data = skulls[skulls$period == 1, -ncol(skulls)], multivariatePlot = "qq")
mvn(data = skulls[skulls$period == 2, -ncol(skulls)], multivariatePlot = "qq")
mvn(data = skulls[skulls$period == 3, -ncol(skulls)], multivariatePlot = "qq")


## test for multivariate normality
source("testnormality.R")
testnormality(X = skulls[skulls$period == 1, -ncol(skulls)])
testnormality(X = skulls[skulls$period == 2, -ncol(skulls)])
testnormality(X = skulls[skulls$period == 3, -ncol(skulls)])

## correlation plot
source("plotcorr.R")
plot.corr(xx = skulls[skulls$period == 1, - ncol(skulls)])
plot.corr(xx = skulls[skulls$period == 2, - ncol(skulls)])
plot.corr(xx = skulls[skulls$period == 3, - ncol(skulls)])

## test for homogeneity of dispersions among 3 periods
source("BoxMTest-2.R")
BoxMTest(X = skulls[, -ncol(skulls)], cl = skulls$period)

## star plots and cheroff faces
skull_means <- aggregate( .~ period, data = skulls, FUN = mean)
library(aplpack)
stars(skull_means[,-1], labels = c("period 1", "period 2", "period 3"), draw.segments = TRUE, key.loc = c(4.5,2))
faces(skull_means[,-1])

## test for difference in skull sizes between period 1 and period 3
library(ICSNP)
T2test <- HotellingsT2(skulls[skulls$period == 1, -ncol(skulls)], skulls[skulls$period == 3, -ncol(skulls)], test = 'f')
df1 <- T2test$parameter['df1']
df2 <- T2test$parameter['df2']
T2stat <- T2test$statistic/df2*(df1 + df2 - 1)*df1
T2stat

BoxMTest(X = skulls[which(skulls$period == c(1,3)), -ncol(skulls)], cl = skulls$period[which(skulls$period == c(1,3))])  

## one-way MANOVA of skulls
library(car)
fit.lm <- lm(cbind(`max breadth`, `basibregmatic height`, `basialveolar length`, `nasal height`) ~ period, data = skulls, contrasts = list(period = contr.SAS))
fit.manova <- Manova(fit.lm)

summary(fit.manova)

## test mean vector changes from period 1 to period 2
C12 <- matrix(c(0,1,-1),nrow = 1, byrow = T)
test12 <- linearHypothesis(model = fit.lm, hypothesis.matrix = C12)

## test mean vector changes from period 2 to period 3
C23 <- matrix(c(0,0,1),nrow = 1, byrow = T)
test23 <- linearHypothesis(model = fit.lm, hypothesis.matrix = C23)

## simutanous tests: 4 measurements and 1-2, 2-3 periods. (8 pairs)
n <- table(skulls$period)
S_pool <- fit.manova$SSPE/fit.manova$error.df
mean_diff_12 <- fit.lm$coefficients[2,] - fit.lm$coefficients[3,]
mean_diff_23 <- fit.lm$coefficients[3,]
S_pool_ii <- diag(S_pool)
t_12 <- abs(mean_diff_12)/sqrt(((1/n[1]) + (1/n[2]))*S_pool_ii)
t_23 <- abs(mean_diff_23)/sqrt(((1/n[2]) + (1/n[3]))*S_pool_ii)
Tstat <- rbind(t_12, t_23)
row.names(Tstat) <- c("12", "23")

p_vals <- 2*pt(Tstat, df = fit.manova$error.df, lower.tail = F)
p.adjust(p_vals, "bonferroni")

## simutanous tests confidence interval: period 1 to period 2 ((4 out of 8 simutaneous pairs))
alpha1 <- 0.05
m <- 8

margin12 <- qt(1 - alpha1/(2*m), df = fit.manova$error.df)*sqrt(((1/n[1]) + (1/n[2]))*S_pool_ii)
conf.int12 <- rbind(mean_diff_12 - margin12, mean_diff_12 + margin12)

## simutanous tests confidence interval: period 2 to period 3 (4 out of 8 simutaneous pairs)
margin23 <- qt(1 - alpha1/(2*m), df = fit.manova$error.df)*sqrt(((1/n[2]) + (1/n[3]))*S_pool_ii)
conf.int23 <- rbind(mean_diff_23 - margin23, mean_diff_23 + margin23)


########################
# tp_value<-function(X, cl){
#   cl <- droplevels(cl)
#   class <- levels(cl)
#   return(t.test(X[cl == class[1]], X[cl == class[2]])$p.value)
# }
# 
# p_vals_12 <- sapply(skulls[which(skulls$period == c(1,2)), -ncol(skulls)], tp_value, cl = skulls$period[which(skulls$period == c(1,2))])
# p_vals_23 <- sapply(skulls[which(skulls$period == c(2,3)), -ncol(skulls)], tp_value, cl = skulls$period[which(skulls$period == c(2,3))])
# 
# p_vals <- c(p_vals_12, p_vals_23)
# 
# p.adjust(p_vals, method = "bonferroni")
# p.adjust(p_vals[order(p_vals)], method = "fdr")
# order(p_vals)
# 
# tconf.int<-function(X, cl, conf.level){
#   cl <- droplevels(cl)
#   class <- levels(cl)
#   return(t.test(X[cl == class[1]], X[cl == class[2]], conf.level = conf.level)$conf.int)
# }
# conf.int_12 <- sapply(skulls[which(skulls$period == c(1,2)), -ncol(skulls)], tconf.int, cl = skulls$period[which(skulls$period == c(1,2))], conf.level = 1 - alpha1/m)
# conf.int_23 <- sapply(skulls[which(skulls$period == c(2,3)), -ncol(skulls)], tconf.int, cl = skulls$period[which(skulls$period == c(2,3))], conf.level = 1 - alpha1/m)
