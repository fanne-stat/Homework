##--------------------------------------------------------------------------
##
## @file: ggandrews.R
##
## Compute Andrews curves (1972) from a data frame . It assume the last columns have
## the ids. The function is similar as the andrews package but this one return
## the curves as a list and also plot them using ggplot2.
##
## Authors:
## Israel Almodovar-Rivera and Ranjan Maitra
## Department of Statistics
## Iowa State University
## almodova@iastate.edu
##------------------------------------------------------------------------

library("ggplot2")
library("reshape2") 

numarray <- function(df){
    dfm <- 0
    m <- dim(df)[2]
    for (a in 1:m) {
        if (is.numeric(df[, a])) {
            if (length(dfm) > 1) 
                dfm <- cbind(dfm, df[, a])
            else dfm <- df[, a]
        }
    }
    dfm
}

normalize <- function(ar){
    if (is.numeric(ar)) {
        ar <- (ar - min(ar))/(max(ar) - min(ar))
    }
    ar
}

ggandrews<-function (df, type = 1, clr = NULL, step = 100,plot = TRUE, return_value = TRUE, linecol = NULL, ...) 
{
    coordlist<-list()
    
    dat <- df 
    if (step < 1) 
        step <- 100
    
    n <- dim(df)[1]
    m <- dim(df)[2]
    
    if ((type == 1) | (type == 2) | (type == 4)) {
        xmin <- (-pi)
        xmax <- pi
    } else if (type == 3) {
        xmin <- 0
        xmax <- 4 * pi
    }

    for(i in 1:m){df[,i] <- normalize(df[,i])}

    dfm <- numarray(df)
    m <- dim(dfm)[2]

    coorx <- 0:step
    for (p in 0:step) coorx[p + 1] <- (xmin + (xmax - xmin)/step * p)
    coory <- 0:step
    for (i in 1:n) {
        for (p in 0:step) {
            coory[p + 1] <- 0
            tt <- (xmin + (xmax - xmin)/step * p)
            for (a in 1:m) {
                if (type == 1) {
                    if (a == 1) {
                        coory[p + 1] <- dfm[i, a]/(2^0.5)
                    }
                    else {
                        cnst <- (a - 2)%/%2 + 1
                        if ((a - 2)%%2 == 0) 
                            coory[p + 1] <- coory[p + 1] + dfm[i, a] * 
                                sin(cnst * tt)
                        else coory[p + 1] <- coory[p + 1] + dfm[i, 
                                                                a] * cos(cnst * tt)
                    }
                }
                else if (type == 2) {
                    cnst <- (a - 1)%/%2 + 1
                    if ((a - 1)%%2 == 0) 
                        coory[p + 1] <- coory[p + 1] + dfm[i, a] * 
                            sin(cnst * tt)
                    else coory[p + 1] <- coory[p + 1] + dfm[i, a] * cos(cnst * tt)
                }
                else if (type == 3) {
                    coory[p + 1] <- coory[p + 1] + dfm[i, a] * 
                        cos((a * tt)^0.5)
                }
                else if (type == 4) {
                    if (a == 1) {
                        coory[p + 1] <- dfm[i, a]
                    }
                    else {
                        cnst <- (a - 2)%/%2 + 1
                        if ((a - 2)%%2 == 0) 
                            coory[p + 1] <- coory[p + 1] + dfm[i, a] * 
                                (sin(cnst * tt) + cos(cnst * tt))
                        else coory[p + 1] <- coory[p + 1] + dfm[i, a] * (sin(cnst * tt) - cos(cnst * tt))
                    }
                    coory[p + 1] <- coory[p + 1]/(2^0.5)
                }
            }
        }
        coordlist[[i]] <- data.frame(x=coorx, y=coory)
    }

    if(plot){
        ll <- coordlist
        d <- data.frame(ll[[1]],Ids = as.factor(dat[1,clr]),Obs = "1")
        n <- dim(df)[1]
        for(i in 2:n)
        {
            d2 <- data.frame(ll[[i]], Ids = as.factor(dat[i,clr]),Obs = sprintf("%d",i))
            d <- rbind.data.frame(d, d2 )
        }

        p <- ggplot(data = d) + geom_line(aes(x = x, y = y,col= Ids,group = Obs)) + xlab("t") + ylab("") + theme_bw() + theme(legend.position="bottom") 
        if (is.null(linecol))
            plot(p) else
                        plot(p + scale_colour_manual(values = linecol))
    }
   
    if(return_value){ 
        return(coordlist)
    }
}

## Example with iris data set
#andrews(df = iris,clr=dim(iris)[2],type=2,return_value = FALSE, linecol = c("blue", "purple", "red"))

## Example 2
##library("MASS")
##data(crabs)
##new_crabs <- crabs[,c(4:8,1)]
##andrews(df = new_crabs,clr=dim(new_crabs)[2],type = 3,return_value = FALSE)

##new_crabs <- crabs[,c(4:8,2)]
##andrews(df = new_crabs,clr=dim(new_crabs)[2],type = 1,return_value = FALSE)
