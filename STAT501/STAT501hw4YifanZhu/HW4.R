#Multinomial 
##Problem 1
#a) Radial visualization
library(lattice)
require(dprep)
olive <- read.table("http://maitra.public.iastate.edu/stat501/datasets/olive.dat", header=T)
olive
colnames(olive) <- c("Regions", "CH1", "CH2", "CH3", "CH4", "CH5","CH6","CH7","CH8")
names(olive)
oil<- as.factor(olive$Regions)

# Use codes from Canvas
source("radviz2d.R")
# Display the radial visualization plot 
radviz2d(dataset = cbind(olive[,-1], oil), name = "Regions")

# sub-regions R1
olive_R1 <- olive[olive$Regions %in% 1:4,]
radviz2d(dataset = cbind(olive_R1[,-1], as.factor(olive_R1$Regions)), name = "R1")

# sub-region  R2
olive_R2 <- olive[olive$Regions %in% 5:6,]
radviz2d(dataset = cbind(olive_R2[,-1], as.factor(olive_R2$Regions)), name = "R2")

# sub-region  R3
olive_R3 <- olive[olive$Regions %in% 7:9,]
radviz2d(dataset = cbind(olive_R3[,-1], as.factor(olive_R3$Regions)), name = "R3")
 
#Comment. The plot that contains all nine regions does not reveal exact difference between chemical components
#of olive oil except component CH8. The second plot for sub-region 1 shows that all chemical components have quite same pattern.
# The third plot for sub-region 2 indicates much overlap of chemical components. The plot for   sub-region 3 reveals the exact differnce between 
#CH8 and CH7&CH9. If we compare three sub-regions, the chemical components behave differenly in each case. Among visualization methods, 
#radial visualisation  might be effective since some other methods show only overall view of the chemical components.

#b) Choose the R2
#i) Calculate the correlation matrix of the two sub-regions
cor(olive_R2[olive_R2$Regions == 5, -1])
cor(olive_R2[olive_R2$Regions == 6, -1])
source("plotcorr.R")
par(mfrow = c(1,2))
plot.corr(xx = olive_R2[olive_R2$Regions == 5, -1])
plot.corr(xx = olive_R2[olive_R2$Regions == 6, -1]) 

#Comment. According to the plots for Area 6, the correlation between CH4&CH1, CH5&CH4, CH4&CH7 and CH5&CH7 much strong than in Area 5.
#In addition, the realtionship between CH1 and CH5 in Area 6 has a positive sign, however, in Area 5 is a negtaive.
#Overall, we can conclude that chemical components in Area 6 hihgtly correlated than in Area 5.

#ii) Compare marginal standard deviations
require(ggplot2)
require(GGally)
require(RColorBrewer)
source("parcoordplot.R")

SD_5<-sapply(olive_R2[olive_R2$Regions == 5, -1], FUN = sd)
SD_6<-sapply(olive_R2[olive_R2$Regions == 6, -1], FUN = sd)

parcoordplot(xx =olive_R2[,-1] ,cl = as.factor(olive_R2$Regions),FUN=mean,alpha = 0.2)
 
#Comment. The standard deviation for Area 5 higher for CH1,CH2, CH7 and CH8 than in Area 6, however other 
# the standard deviations of other chemical components are lower.
#The paralell coordinate plot shows that standard deviation of both areas centered around zero.


#iii) Test for difference in dispersions among the two groups

source("BoxMtest-2.R")

BoxMTest(X = olive_R2[,-1], cl = as.factor(olive_R2$Regions))

#iv) Test for normality
source("testnormality.R")

 testnormality(X=olive_R2[olive_R2$Regions == 5, -1])
 testnormality(X=olive_R2[olive_R2$Regions == 6, -1])

# Comment. The test result shows that for the Area 5 we can conclude multivariate normality is reasonable, but for Area 6 not.
 
 #v) Report T2-Hotelling test
 library(ICSNP)
 T2test <- HotellingsT2(X = olive_R2[olive_R2$Regions == 5, -1], Y = olive_R2[olive_R2$Regions == 6, -1])
 T2test
 df1 <- T2test$parameter['df1']
 df2 <- T2test$parameter['df2']
 T2stat <- T2test$statistic/df2*(df1 + df2 - 1)*df1
 T2stat
 #vi) Provide individual t-tests
 
 tp_value<-function(X, cl){
   class <- levels(cl)
   return(t.test(X[cl == class[1]], X[cl == class[2]], var.equal = T)$p.value)
 }
 
 p_vals <- sapply(olive_R2[,-1], tp_value, cl = as.factor(olive_R2$Regions))
 
 p.adjust(p_vals, method = "bonferroni")
 p.adjust(p_vals[order(p_vals)], method = "fdr")
 
 
 #vii) Draw 95% Confidence ellipses
 
 library(car)
 dataEllipse(olive_R2$CH5, olive_R2$CH6, groups = as.factor(olive_R2$Regions), levels=0.95, xlab = "CH5", ylab = "CH6", col = c("red", "blue"), ylim = c(10, 45), group.labels = c("A5", "A6"))
 
#c) Three main regions
#i)Display the Chernoff faces 
 library(TeachingDemos)
 mean_R1 <- sapply(olive_R1[,-1], mean)
 mean_R2 <- sapply(olive_R2[,-1], mean)
 mean_R3 <- sapply(olive_R3[,-1], mean)
 mean_Rs <- cbind(rbind(mean_R1, mean_R2, mean_R3))
 
faces(mean_Rs, labels = c("R1", "R2", "R3"))

# Comment.According to Chernoff faces, we can see that Region 1 has distintive features than Region 2 and 3.
 
#ii) Provide a one-way multivariate analysis of variance.
BigRegions <- rep(0, nrow(olive))
BigRegions[olive$Regions %in% 1:4] <- 1
BigRegions[olive$Regions %in% 5:6] <- 2
BigRegions[olive$Regions %in% 7:9] <- 3
olive <- data.frame(olive, BigRegions = as.factor(BigRegions))
fit.lm <- lm(cbind(CH1, CH2, CH3, CH4, CH5, CH6, CH7, CH8) ~ BigRegions, data = olive)
fit.manova <- Manova(fit.lm)

summary(fit.manova)


