################################################################################
##
## A modified Parallel Coordinate Plot with highlighted summary (eg. medians)
## Requires R packages RColorBrewer, ggplot2 and GGally 
## arguments
## xx = A data frame containing the data to be displayed.
## pcp.labels = The specific labels to be highlighted. (If any)
## alpha = The transparency parameter value for the parallel coordinates plot for the entire data.
##        Set at 0.05 by default
##        Note that for the highlighted coordinates solid lines are displayed using no transparency.
## legend = Optional for specification of labels. By default no legend is displayed. If legend=1 then legend is displayed to the right
##
## Written Souradeep Chattopadhyay, Kolkata, India and Ranjan Maitra, Ames IA 50011-1090, January 2, 2016.
##

parcoordplot<-function(xx=NULL, cl=NULL, columns = 1:ncol(xx), pcp.labels=names(xx),
                       alpha=0.05, legend=NULL, FUN, brewer.palette = "Dark2")
{
    require(ggplot2)
    require(GGally)
    require(RColorBrewer)
    if(is.null(xx))
        cat("No data supplied. Will do nothing.\n") else {
                                                        if(is.null(cl)) {
                                                            cat("Group information not provided.\n")
                                                            cl <- as.factor(rep(1,nrow(xx)))
                                                        }
                                                        xx.medians <- aggregate(x = xx, by = list(cl), FUN = match.fun(FUN))##Calculating the median of the groups.
                                                        dat1 <- cbind(Group.1 = cl, xx)
                                                        new.dat1<-as.matrix(rbind(xx.medians, dat1)) ####Stacking the medians of the groups over the data
                                                        
                                                        new_dat <- data.frame(new.dat1, alpha1 =  c(rep(1,nrow(xx.medians)),rep(alpha,nrow(xx))))
                                                        new_dat[,1] <- as.factor(new_dat[,1])
                                                        
                                                        ##The parallel coordinates plot
                                                        if(is.null(legend))
                                                            ggparcoord(data=new_dat, columns=columns+1, groupColumn=1,alpha="alpha1") + scale_x_discrete(labels=pcp.labels)+scale_colour_brewer(type="qual", palette  = brewer.palette) + theme_light() + theme(legend.position="none") else
                                                                                                                                                                                                                                                                                            ggparcoord(data=new_dat, columns=columns+1, groupColumn=1,alpha="alpha1") + scale_x_discrete(labels=pcp.labels)+scale_colour_brewer(type="qual", palette=brewer.palette)+theme_light()
                                                    }
}


