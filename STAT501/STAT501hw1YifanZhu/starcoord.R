# This function displays a star coordinates plot introduced by Kondogan (2001).
# Usage

#starcoord(data, main = NULL, class = FALSE, outliers, vars = 0, scale = 1, cex = 0.8, lwd = 0.25, lty = par("lty")) 

# Arguments
# data 	The dataset
# main 	The title of the plot
# class 	The class
# outliers 	instances to be highlighted as potential outliers
# vars 	variable to be scaled
# scale 	The scale factor
# cex 	A numerical value giving the amount by which plotting text and symbols should be scaled.
# lwd 	the width of the lines representing the axis
# lty 	The type of the lines representing the axis
#
# This plot is not recommended for a large number of features (say more than 50)
# Value

# Returns a star coordinates plot of the data matrix
# Author(s)

# Edgar Acuna and Shiyun Wen
# References

# E. Kandogan (2001). Visualizing multidimensional clusters, Trends, and Outliers, using star coordinates. Proceedings of KDD 2001. 


starcoord <-
    function (data, main = NULL, class = FALSE, outliers=NULL, vars = 0, scale = 1, cex = 0.8, lwd = 0.25, lty =par("lty"), palette.col = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928"))
{
    if (is.data.frame(data)) 
        data <- data.matrix(data)
    else if (!is.matrix(data)) 
        stop("data must be a matrix or a data frame !")
    if (!is.numeric(data)) 
        stop("data must be numeric !")
    if (class == TRUE) {
        classinf <- data[, dim(data)[2]]
        data <- data[, 1:((dim(data)[2] - 1))]
    }
    nbrow <- dim(data)[1]
    nbcol <- dim(data)[2]
    angles <- seq(0, 2 * pi, length = nbcol + 1)[-(nbcol + 1)]
    if (length(angles) != nbcol) 
        stop("length(angles) must be the same as ncol(data) !")
    drange <- max(data) - min(data)
    o.loc <- as.matrix(c(0, 0))
    axcord <- rbind(cos(angles), sin(angles))
    o.loc1 <- matrix(rep(o.loc, nbcol), 2, )
    axcord <- o.loc1 + drange * axcord
    if (vars != 0) 
        axcord[, vars] <- axcord[, vars] * scale
    colRangee <- apply(data, 2, range)
    colRangee <- colRangee[2, ] - colRangee[1, ]
    colRangee <- rbind(colRangee, colRangee)
    unitvect <- axcord/colRangee
    data <- apply(data, 2, function(data) (data - min(data)))
    o.loc2 <- matrix(rep(o.loc, nbrow), 2, )
    data <- o.loc2 + t(data %*% t(unitvect))
    xlim <- c(min(data, axcord), max(data, axcord))
    ylim <- c(min(data, axcord), max(data, axcord))
    xlim <- range(data)
    ylim <- range(data)   
    plot(0, type = "n", xlim = xlim, ylim = ylim, main = main, asp = 1, xlab = "",frame.plot=FALSE)
    lab <- c(1:nbcol)
    for (i in 1:nbcol) {
        arrows(o.loc[1], o.loc[2], axcord[1, i], axcord[2, i], length = 0.05, lty = 5)
        if (i < nbcol/2) 
            text(axcord[1, i], axcord[2, i], lab[i], pos = 3, cex = 0.5)
        else text(axcord[1, i], axcord[2, i], lab[i], pos = 1, cex = 0.5)
    }
    if (class == FALSE) 
        points(data[1, ], data[2, ], pch = 15, type = "p", col = palette.col[1], cex = cex)
    else {
        pointcolor <- palette.col
        palette(pointcolor)
        if (length(outliers)!=0)          {
            nombres <- outliers
            text(data[1, outliers], data[2, outliers], labels = nombres, cex = cex)
          }
        data <- rbind(data, classinf)
        legnames <- levels(factor(classinf))
        legnames <- paste("Class ",legnames)
        par(xpd = TRUE)  
        legend(x=xlim[1], y=ylim[2], legend=legnames,fill=pointcolor,cex=.7,bty="n",xjust=0,yjust=0,horiz=TRUE)
     par(xpd = FALSE) 
        for (i in as.numeric(levels(factor(classinf))))
            points(data[1, data[3, ] == i], data[2, data[3, ] == i], col = i, pch = 16+i, cex = cex)
    }
}

