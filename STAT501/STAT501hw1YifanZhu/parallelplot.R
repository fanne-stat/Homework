# Parallel Coordinate Plot
# Description

# Constructs a parallel coordinate plot for a data set with classes in last column.
# Usage

# parallelplot(x, name = "", comb = -1, class = 0, obs = rep(0, 0), col = 2, lty = 1, ...)

# Arguments
# x 	A matrix of numerical values with classes in last column
# name 	The name of data set as will appear in the graph title
# comb 	An integer that represents the number of one of the possible combinations for the columns of this matrix.
# class 	A value representing the class number to which the plot should be limited
# obs 	A list of one or more row numbers that are to be highlighted in the plot
# col 	A value that provides a choice of color for the plot (if plotting only one class)
# lty 	A value that provides a choice of line width for the plot (if plotting only one class)
# ... 	Additional arguments for the matplot function
# Details

# This plot is not recommended for a large number of features (say more than 50). If comb=0, all distinct combinations of columns are graphed. If comb=-1 (default), the attributes are plotted in their original order, else comb should be equal to an integer that represents the number of one of the possible combinations for the columns of this matrix.
# Value

# A parallel coordinate plot of the data is produced.
# Author(s)

# Caroline Rodriguez
# References

# Wegman, E. (1990), Hyperdimensional data analysis using parallel coordinates, Journal of the American Statistical Association,85,664-675

parallelplot <-
function (x, name="",comb=-1,class=0,obs=rep(0,0),col = 2, lty = 1, ...) 
{

#Function to create a parallel coordinate plot for a dataset.

#x: the dataset
#name: name of the dataset
#comb: integer that represents the order in which the variables will be graphed
#class: the number of the class to be graphed
#obs: a list of row numbers of the observations to be highlighted
#col: color to be used when graphing only one class
#lty: width of poly-lines
#...: other parameters accepted by the plot() function of R

#Calculate number and size of classes in matrix
 classes=x[,ncol(x)]
 len.class=table(classes)
 numclass=length(len.class)

#remove classes and calculate size of matrix
 x=x[,-ncol(x)]
 r=nrow(x)
 c=ncol(x)

#find the number of distinct permutations of attributes
 numgraphs=combinations(c)

 class.list=as.integer(rownames(len.class))
 
#normalize matrix
 xrnms=rownames(x)
 x <- apply(x, 2, function(x) (x - min(x))/(max(x) - min(x)))
 rownames(x)=xrnms
   
#if graph of only one class is wanted, construct submatrix
 if (class!=0)
    {
       same=(classes==class)
       x=x[same,]
       col=class+1
    }

graphtitle=paste("Parallel Coordinate Plot for ",name) 

# if more than one class, then obtain color vector for each class
      if (class==0)
       col=classes+1

#if comb equals 0, user wants to see ALL graphs of distinct permutations of variables
 if (comb==0) 
     {
                   
      j=1
      def.par<-par(mfrow=c(2,2),font.lab=2,font.sub=2,cex=.75,las=2)
   
#draw the different graphs, 4 to each screen
      for (k in 1:ncol(numgraphs))
         {
           if (k %% 4==1) {
#get(getOption("device"))()
par(mfrow=c(2,2))}          
           varorder=numgraphs[,j]
           subtitle=paste("Combination #",j)
           matplot(1:c, t(x[,varorder]), type = "l", col = col, lty = lty, xlab = "", ylab = "",main= 
graphtitle, sub=subtitle,axes = FALSE, ...)
           axis(1, at = 1:c, labels = colnames(x[,varorder]))      
           for (i in 1:c) lines(c(i, i), c(0, 1), col = "grey70")
           j=j+1
         }
      par(def.par)
       }    
     
# else if only one particular combination is desired
  else
    {
     if (comb==-1) varorder = 1:c
     else varorder=numgraphs[,comb]
     def.par<-par(font.lab=2,font.sub=2,cex=.75,las=2,bg=gray(.8))
     subtitle=("Original Attribute Order")
     matplot(1:c, t(x[,varorder]), type = "l", col = col, lty = lty, xlab = "", ylab = "",main= graphtitle,sub=subtitle, axes = FALSE, ...)
     axis(1, at = 1:c, labels = colnames(x[,varorder]))      
     for (i in 1:c) lines(c(i, i), c(0, 1), col = "grey70")
        
       # if the user desires to highlight a particular observation
       if (length(obs)!=0)
        {
          obsers=rep(0,0)
          for(i in 1:length(obs)) obsers=c(obsers,which(rownames(x)==obs[i]))
  colors=palette()[(numclass+2):8]
          if (length(obsers)==1) matlines(1:c,x[obsers,varorder],lty=1,lwd=3,col=colors)
  else  matlines(1:c,t(x[obsers,varorder]),lty=1,lwd=3,col=colors)
  par(cex=.75)
          text(1,x[obsers,varorder[1]],rownames(x[obsers,]),pos=3)
          text(c,x[obsers,varorder[c]],rownames(x[obsers,]),pos=3)
          palette("default")
        }
  }        
 invisible()
 par(def.par)
}

