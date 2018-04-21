library(rgl)
library(RColorBrewer)

#Add axes without the bounding box function

rgl_add_axes <- function(x, y, z, axis.col = "black",
                         xlab = "", ylab="", zlab="", show.plane = FALSE,
                         show.bbox = FALSE, bbox.col = c("#333377","black"))
{
  #Function to add limit to axis:
  lim <- function(x){c(min(0,min(x)), max(abs(x))) * 1.1} 
  # Add axes
  xlim <- lim(x); ylim <- lim(y); zlim <- lim(z)
  rgl.lines(xlim, c(0, 0), c(0, 0), color = axis.col)
  rgl.lines(c(0, 0), ylim, c(0, 0), color = axis.col)
  rgl.lines(c(0, 0), c(0, 0), zlim, color = axis.col)
  # Add a point at the end of each axes to specify the direction
  axes <- rbind(c(xlim[2], 0, 0), c(0, ylim[2], 0),
                c(0, 0, zlim[2]))
  rgl.points(axes, color = axis.col, size = 3)
  # Add axis labels
  rgl.texts(axes, text = c(xlab, ylab, zlab), color = axis.col,
            adj = c(0.5, -0.8), size = 2)
  # Add plane
  if(show.plane) {
    xlim <- xlim/1.1; zlim <- zlim /1.1
    rgl.quads( x = rep(xlim, each = 2), y = c(0, 0, 0, 0),
               z = c(zlim[1], zlim[2], zlim[2], zlim[1]))
  }
  # Add bounding box decoration
  if(show.bbox){
    rgl.bbox(color=c(bbox.col[1],bbox.col[2]), alpha = 0.5,
             emission=bbox.col[1], specular=bbox.col[1], shininess=5,
             xlen = 3, ylen = 3, zlen = 3)
  }
}



rgl_add_axes(A,B,C,show.plane = F,show.bbox = F)
axis3d('x', pos = c(NA, 0, 0),col="black")
axis3d('y', pos = c(0, NA, 0),col="black")
axis3d('z', pos = c(0, 0, NA),col="black")

title3d(main = "Plot for random points",sub = NULL, xlab = "A", ylab = "B",
        zlab = "C", cex = 3)



# anchor points coordinates
phi <- (sqrt(5) + 1)/2
anchor_coord <- list()
anchor_coord[[4]] <- matrix(c(1,1,1,
                              1,-1,-1,
                              -1,1,-1,
                              -1,-1,1), byrow = T, ncol = 3)/sqrt(3) 
anchor_coord[[6]] <- matrix(c(1,0,0,
                              -1,0,0,
                              0,1,0,
                              0,-1,0,
                              0,0,1,
                              0,0,-1), byrow = T, ncol = 3)
anchor_coord[[8]] <- matrix(c(1,1,1,
                              -1,1,1,
                              1,-1,1,
                              -1,-1,1,
                              1,1,-1,
                              -1,1,-1,
                              1,-1,-1,
                              -1,-1,-1), byrow = T, ncol = 3)/sqrt(3)
anchor_coord[[12]] <- matrix(c(0,1,phi,
                               0,1,-phi,
                               0,-1,phi,
                               0,-1,-phi,
                               1,phi,0,
                               -1, phi, 0,
                               1, -phi, 0,
                               -1, -phi, 0,
                               phi, 0, 1,
                               -phi, 0, 1,
                               phi,0,-1,
                               -phi, 0, -1), byrow = T, ncol = 3)/sqrt(1+phi^2)
anchor_coord[[20]] <- matrix(c(1,1,1,
                              -1,1,1,
                              1,-1,1,
                              -1,-1,1,
                              1,1,-1,
                              -1,1,-1,
                              1,-1,-1,
                              -1,-1,-1,
                              0,1/phi,phi,
                              0,1/phi,-phi,
                              0,-1/phi,phi,
                              0,-1/phi,-phi,
                              1/phi,phi,0,
                              -1/phi, phi, 0,
                              1/phi, -phi, 0,
                              -1/phi, -phi, 0,
                              phi, 0, 1/phi,
                              -phi, 0, 1/phi,
                              phi,0,-1/phi,
                              -phi, 0, -1/phi), byrow = T, ncol = 3)/sqrt(3)

radial_tranform <- function(data){
  data <- as.matrix(data)
  data <- apply(data, MARGIN = 2, FUN = function(x) (x - min(x))/(max(x) - min(x)))
  p <- ncol(data)
  S <- apply(data, MARGIN = 1, sum)
  nominator <- data %*% anchor_coord[[p]]
  denominator <- matrix(rep(S, each = 3), byrow = T, ncol = 3) 
  return(nominator/denominator)
}

olive_trans <- radial_tranform(olivet)

radialvis3d <- function(data, cl, color = NULL){
  cl <- as.factor(cl)
  data_trans <- radial_tranform(data)
  class <- levels(cl)
  color <- rainbow(length(class))
  
  #Create rgl plot
  rgl.open()
  for(i in 1:length(class)){
    rgl.points(data_trans[cl == class[i],], color = color[i])
  }
  rgl.bg(color="gray", alpha = 0.01)

  rgl_add_axes <- function(x, y, z, axis.col = "black",
                           xlab = "", ylab="", zlab="", show.plane = FALSE,
                           show.bbox = FALSE, bbox.col = c("#333377","black"))
  {
    #Function to add limit to axis:
    lim <- function(x){c(min(0,min(x)), max(abs(x))) * 1.1} 
    # Add axes
    xlim <- lim(x); ylim <- lim(y); zlim <- lim(z)
    rgl.lines(xlim, c(0,0), c(0,0), color = axis.col)
    rgl.lines(c(0, 0), ylim, c(0, 0), color = axis.col)
    rgl.lines(c(0, 0), c(0, 0), zlim, color = axis.col)
    # Add a point at the end of each axes to specify the direction
    axes <- rbind(c(xlim[2], 0, 0), c(0, ylim[2], 0),
                  c(0, 0, zlim[2]))
    rgl.points(axes, color = axis.col, size = 3)
    # Add axis labels
    rgl.texts(axes, text = c(xlab, ylab, zlab), color = axis.col,
              adj = c(0.5, -0.8), size = 2)
    # Add plane
    if(show.plane) {
      xlim <- xlim/1.1; zlim <- zlim /1.1
      rgl.quads( x = rep(xlim, each = 2), y = c(0, 0, 0, 0),
                 z = c(zlim[1], zlim[2], zlim[2], zlim[1]))
    }
    # Add bounding box decoration
    if(show.bbox){
      rgl.bbox(color=c(bbox.col[1],bbox.col[2]), alpha = 0.5,
               emission=bbox.col[1], specular=bbox.col[1], shininess=5,
               xlen = 3, ylen = 3, zlen = 3)
    }
  }
  
  
  
  rgl_add_axes(data_trans[,1], data_trans[,2], data_trans[,3] ,show.plane = F,show.bbox = F)
  axis3d('x', pos = c(NA, 0, 0),col="black")
  axis3d('y', pos = c(0, NA, 0),col="black")
  axis3d('z', pos = c(0, 0, NA),col="black")
  
  title3d(main = "Plot for 3d visualization",sub = NULL, cex = 3)
}

# radialvis3d(oliveoils[,-c(1,2)], cl = oliveoils[,2])

# data1<- read.table("~/Desktop/Egyptian-skulls.dat", header=T)
# colnames(data1) <- c("MB", "BH", "BL", "NL", "Time")
# names(data1)
# period<- as.factor(data1$Time)
# levels(period)

