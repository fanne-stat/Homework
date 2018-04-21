library(readxl)
library(dplyr)
library(tidyr)
library(discretization)  
library(ggplot2)
source("parallelplot.R")
source("combinations.R")
source("plotcorr.R")
source("surveyplot.R")

#Q.N3.a
#Read the file
Tornado<-read.table(file ="https://www.nssl.noaa.gov//users/brooks//public_html//feda//datasets//tornf1p.txt",col.names = c("year","jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec") )
#Create correlation plot
plot.corr(Tornado[2:13])
# Generally it is observed that months next to each other or near to each other like Jan-Feb, Jan-March has
#positibe correlation while month far away from each other/ in oppite season o year like Jan-June, Jan-July
#has negative correlation althought it is not true for all the cases.
# From the correlation plot it is also observed that the variance
#is high for months during spring/early summer Apr-June compared to
#other months

#Q.N3b.i
#Create 3 group and a column of group in the dataframe
Tornado$Period<-cut(x = Tornado$year,breaks=c(1954,1974,1994,2014),labels = c("I","II","III"),include.lowest = F)

#Create parallel plot with colour by the group
# Compute the mean for three groups
Means<-aggregate(x = Tornado[-1,2:13],by=list(Tornado$Period[-1]),FUN = mean)

parallelplot(x = cbind(Tornado[-1,2:13], as.integer(as.factor(Tornado$Period[-1]))), name = "Tornado in the US")
legend(x=1,y=1,legend = c("I","II","III"),lwd = 5,col = c("red","green","blue"))

par(new=T)
parallelplot(x = cbind(Means[,2:13],as.integer(as.factor(Means$Group.1))),lwd=3,add=T)
#Fom the parallel plot along with superimposed mean it can be seen that during different period the frequency of 
#tornado varied largely across the mont for example during third period the frequency was very lod from jun-aug while it was high
#durign 1st and 2nd period. Any specific pattern however is hard to observe due to the variation.
#Q.N3b.ii Create surveyplot and order by each 12 months
for (i in 1:12){
surveyplot(cbind(Tornado[,2:13],as.numeric(as.factor(Tornado[,14]))),order = i)}

#From above survey plot distinct pattern were not visible and ordering by none of the month 
#provided a clear separation between the classes

#Q.N3c. Create the star and chernoff plot for the mean of three group
library(TeachingDemos)


#Create chernoff face for the means
#Scale the orginal data according to scaling part in function "faces"
Tornado_faces <- apply(Tornado[-1,2:13],2,function(x){
  x<-x-min(x); x<-if(max(x)>0) 2*x/max(x)-1 else x })
#Means calculated from scaled data to plot faces
Mean_faces <- aggregate(x = Tornado_faces,by=list(Tornado$Period[-1]),FUN = mean)
faces(xy = Mean_faces[,2:13], scale = F)


#Scale the original data according to the scaling part in function "stars"
Tornado_stars <- apply(Tornado[-1,2:13], 2L, function(x) (x - min(x, na.rm = TRUE))/diff(range(x, na.rm = TRUE)))
#Means calculated from scaled data to plot stars
Mean_stars <- aggregate(x = Tornado_stars,by=list(Tornado$Period[-1]),FUN = mean)
#Create stars for the group means
stars(x = Mean_stars[,2:13],labels = as.character(Means$Group.1),scale = F,full = T,radius = T)

#From both chernoff face and stars differences are observed between the group mean