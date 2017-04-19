#Read the data and create factors.

d=read.delim("http://dnett.github.io/S510/HeartRate.txt")
head(d)

d$woman=as.factor(d$woman)
d$drug=as.factor(d$drug)
d$time=as.factor(d$time)
head(d)

# Compute sample means

means = tapply(d$y,
    list(d$time,d$drug),mean)
means

#  Make a profile plot of the means

x.axis = unique(d$time)
matplot(c(-1,16), c(70,85), type="n", 
     xlab="time", ylab="y",
     main= "Observed Mean Y")   
matlines(x.axis,means,type='l',lty=c(1,2,3),lwd=2)
matpoints(x.axis,means, pch=c(16,17,15),cex=1.5)    
legend(4.16, 74 ,legend=c("A",
    'B','C'),lty=c(1,2,3),
    lwd=2,col=1:3,bty='n')

# Use the lme function. This application
# assumes that each subject has a different
# identification value

library(nlme)
d.lme = lme(y ~ drug*time,
  random= ~ 1|woman, data=d,
  method="REML")
summary(d.lme)
anova(d.lme)
names(d.lme)


#  Use the gls( ) function to fit a
#  model where the errors have a 
#  compound symmetry covariance structure
#  within subjects. Random effects are
#  not used to induce correlation.

d.glscs = gls(y ~ drug*time,
  data=d,
  correlation = corCompSymm(form=~1|woman),
  method="REML")
summary(d.glscs)
anova(d.glscs)



# Try an auto regressive covariance 
# structures across time within 
# subjects

d.glsar = gls(y ~ drug*time,
  data=d,
  correlation = corAR1(form=~1|woman),
  method="REML")
summary(d.glsar)
anova(d.glsar)


#  Use an arbitray covariance matrix for
#  observations at different time 
#  points within subjects

d.gls = gls(y ~ drug*time,
  data=d,
  correlation = corSymm(form=~1|woman),
  weight = varIdent(form = ~ 1|time),
  method="REML")
summary(d.gls)
anova(d.gls)


#  Compare the fit of various covariance
#  structures.

anova(d.gls, d.glscs)
anova(d.gls, d.glsar)


# Treat time as a continuous variable and
# fit quadratic trends in strength
# over time

d.time = gls(Strength ~ Program+Time+
  Program*Time+I(Time^2)+Program*I(Time^2),
  data=d,
  correlation = corAR1(form=~1|Subj),
  method="REML")
summary(d.time)
anova(d.time)


# To compare the continuous time model to the 
# model where we fit a different mean at each 
# time point, we must compare likelihood values
# instead of REML likelihood values.


d.glsarmle = gls(Strength ~ Program*Timef,
  data=d,
  correlation = corAR1(form=~1|Subj),
  method="ML")


d.timemle = gls(Strength ~ Program+ Time+
     Program*Time+I(Time^2)+Program*I(Time^2),
  data=d,
  correlation = corAR1(form=~1|Subj),
  method="ML")

anova(d.glsarmle, d.timemle)


#  Do not fit different quadratic trends
#  for different programs

d.timemle = gls(Strength ~ Program+Time+
     Program*Time+I(Time^2), data=d,
  correlation = corAR1(form=~1|Subj),
  method="ML")

anova(d.glsarmle, d.timemle)


#  Fit a model with random regression coefficients
#  for individual subjects 

d.timer = lme(Strength ~ Program+Time+
     Program*Time+I(Time^2), 
  random = ~ Time + I(Time^2) | Subj,
  data=d,
  correlation = corAR1(form=~1|Subj),
  control=list(msMaxIter=100),
  method="REML")

d.timer

fixef(d.timer)

ranef(d.timer)

coef(d.timer)

prog=d$Program[seq(7,nrow(d),by=7)]

b=cbind(coef(d.timer)[,1]+
       (prog==2)*coef(d.timer)[,2]+
       (prog==3)*coef(d.timer)[,3],
        coef(d.timer)[,4]+
       (prog==2)*coef(d.timer)[,6]+
       (prog==3)*coef(d.timer)[,7],
        coef(d.timer)[,5])       
x=seq(2,14,by=.1)
plot(c(2,14),c(71,90),ylim=c(71,90),pch=" ",
     ylab="Strength",xlab="Day")
for(i in 1:57){
  lines(x,b[i,1]+b[i,2]*x+b[i,3]*x^2,col=prog[i],lwd=1.9)
}
legend(2.1,75,legend=c("RI program",
    'WI Program','Controls'),lty=1,col=1:3,bty='n',lwd=1.9)

# Do we need the AR(1) structure in the
# random coefficients model?

d.timeru = lme(Strength ~ Program+Time+
     Program*Time+I(Time^2), 
  random = ~ Time + I(Time^2) | Subj,
  data=d,
  method="REML")

anova(d.timer,d.timeru)

# The more complicated model is preferred.
# Keep the AR(1) structure.











