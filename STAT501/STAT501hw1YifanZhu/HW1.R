##Gaussian group

##Problem 1

#a) Read the dataset
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
ggandrews(senators_new, type = 2, clr = 543, linecol = c("blue", "purple", "red"))

#Comment: From Andrews' curves we can see that both parties have the same pattern of party affiliation. For some type of bills, 
# senators from two parties against each other. The amount of senators who voted as independent represents the pick of the curves. 

##Problem 2


#a) Radial visualization and star coordinates
library(lattice)
require(dprep)
sclerosis <- read.table("sclerosis.dat", header=F)

p <- dim(sclerosis)[2]
sclerosis[, p] <- as.factor(ifelse(sclerosis[,p] == 0, "normal", "sclerosis"))

colnames(sclerosis) <- c("Age", "TS1", "DS1", "TS2", "DS2", "Disease")

# Use codes from Canvas
source("radviz2d.R")

# Display the radial visualization plot 
radviz2d(dataset = sclerosis, name = "Sclerosis")

# Use the codes from Canvas 
source("starcoord.R")

#Display the star coordinates
starcoord(data = sclerosis, class = TRUE)


#b) Calculate the mean
normal_mean<-colMeans(sclerosis[sclerosis[,p]=="normal", -p])
normal_mean


sclerosis_mean<-colMeans(sclerosis[sclerosis[,p]=="sclerosis",-p])
sclerosis_mean



#c) Display the Chernoff faces 
sclerosis_mean_data <- as.matrix(rbind(normal_mean, sclerosis_mean))
library(TeachingDemos)
faces(sclerosis_mean_data, labels = c("normal", "sclerosis"))


#d) Display the correlation matrix for each group
source("plotcorr.R")
# normal
plot.corr(xx = sclerosis[sclerosis[,p]=="normal", -p])

# sclerosis
plot.corr(xx = sclerosis[sclerosis[,p]=="sclerosis", -p])

## Problem 3


#a)
#Read the dataset
Tornado<-read.table(file ="https://www.nssl.noaa.gov//users/brooks//public_html//feda//datasets//tornf1p.txt",col.names = c("year","jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"))
#Create correlation plot
source("plotcorr.R")
plot.corr(Tornado[2:13])
# Generally it is observed that months next to each other or near to each other like Jan-Feb, Jan-March has
# positibe correlation while month far away from each other/ in oppite season o year like Jan-June, Jan-July
# has negative correlation althought it is not true for all the cases.
# From the correlation plot it is also observed that the variance
# is high for months during spring/early summer Apr-June compared to
# other months

#b) i.
#Create 3 group and a column of group in the dataframe
Tornado$Period<-cut(x = Tornado$year,breaks=c(1954,1974,1994,2014),labels = c("I","II","III"),include.lowest = F)

#Create parallel plot with colour by the group
source("parcoordplot.R")
parcoordplot(xx =Tornado[-1,2:13],cl = as.factor(Tornado$Period[-1]),FUN=mean,alpha = 0.2)

# Fom the parallel plot along with superimposed mean it can be seen that during different period the frequency of
# tornado varied largely across the mont for example during third period the frequency was very low from jun-aug while it was high
# durign 1st and 2nd period. Any specific pattern however is hard to observe due to the variation.
#b) ii. Create surveyplot and order by each 12 months
source("surveyplot.R")
for (i in 1:12){
  surveyplot(cbind(Tornado[,2:13],as.numeric(as.factor(Tornado[,14]))),order = i)}

# From above survey plot distinct pattern were not visible and ordering by none of the month
# provided a clear separation between the classes

# b) iii. Create the star and chernoff plot for the mean of three group
library(TeachingDemos)
# Compute the mean for three groups
Means<-aggregate(x = Tornado[-1,2:13],by=list(Tornado$Period[-1]),FUN = mean)

#Create chernoff face for the means
faces(xy = Means[,2:13])
#Create stars for the group means
stars(x = Means[,2:13],labels = as.character(Means$Group.1),scale = T,full = T,radius = T)

#From both chernoff face and stars differences are observed between the group means