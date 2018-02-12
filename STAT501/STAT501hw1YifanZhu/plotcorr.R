##
## Function to display correlations, along with varianes on the diagonal. 
## Requires corrplot package
##
## arguments:
## xx = matrix or dataframe containing the observations
## corrplot.labels = labels for the variables in the correlation display
## cor.xx = correlation matrix. If valid and provided, will be used. If
##          actually, a dispersion matrix, then will be converted. If not valid
##          correlation or covariance matrix, xx will be used to construct
##          the correlation plot. 
## var.xx = diagonal of the variance-covariance matrix (to be calculated from xx if not provided)
## labels.col = color of the correlation text labels.
## cor.col = color of the correlations potted in the upper triangle
## vars.col = color of the variances plotted in the diagonal
## digits.var = number of digits to be included of the variances
##
## output:
##    provides a correlation plot with correlations displayed in the lower triangle using appropriately-oriented ellipses, variances on the diagonal, and correlation values in the upper triangle
##
## Written: Ranjan Maitra, Ames, Iowa 50011-1090, January 1, 2017.
##

plot.corr <- function(xx = NULL, corrplot.labels = names(xx), cor.xx = NULL, var.xx = NULL, labels.col="black", cor.col="darkgreen", vars.col="#6A3D9A", digits.var = 2, ...)
{
    require(corrplot)
    if (is.null(cor.xx)) 
        cor.xx <- cor(as.matrix(xx)) else {
                              d <- diag(cor.xx)
                              if (any(d <= 0)) {
                                  if (is.null(xx)) {
                                      cat("Invalid correlation matrix provided, with no data to calculate from\nExiting:")
                                      cor.xx <- NULL
                                  }
                                      else
                                                                                                                           {
                                                                                                                               ##                                                                              ## ignore dispersion/correlation matrix
                                                                                                                               ##
                                                                                                                               cat("this is not a valid correlation matrix: calculating our own from the data\n")
                                                                                                                               cor.xx <- cor(xx)
                                                                                                                           }
                              }
                              else {
                                  if (!((length(unique(d)) == 1) & (sum((unique(d) == 1)) == 1))) {
                                      ##
                                      ## not a correlation but a dispersion 
                                      ##
                                      if (sum(var.xx != diag(cor.xx)) !=0) {
                                          cat("supplied variances do not match diagonals of the supplied dispersion matrix: variances will be recalculated from the dispersion matrix\n")
                                          var.xx <- d
                                      }
                                      ## not correlation matrix, but covariance matrix
                                      ## convert to correlation matrix

                                      browser()
                                      cor.xx <- diag(1/sqrt(d)) %*% cor.xx %*% diag(1/sqrt(d))
                                  }
                              }
                          }
    if (is.null(var.xx)) {
        if (is.null(xx)) {
            var.xx <- NULL
            cat("variances not provided, with no dataset\nExiting\n")
        }
        else 
            var.xx <- apply(X = as.matrix(xx), MAR = 2, FUN = var)
    }
    if (!all(c(is.null(var.xx), is.null(cor.xx), is.null(xx)))) {
        corrplot(cor.xx, method="ellipse", type="lower", tl.pos="n", tl.col=labels.col, cl.pos="r")
        rownames(cor.xx) <- as.character(round(var.xx, digits = digits.var)) ##Positioning variances on the diagonals
        colnames(cor.xx) <- rownames(cor.xx) ##Positioning variances on the diagonals
        corrplot(cor.xx, method="number", type="upper", col= cor.col, cl.pos="n", tl.pos="d", tl.col = vars.col, add=TRUE)
        axis(3, at = 1:ncol(cor.xx), labels = corrplot.labels, lty = 0, pos = ncol(cor.xx) + c(0.4, 0.7))
        axis(2, at = nrow(cor.xx):1, labels = corrplot.labels, lty = 0, pos = c(0.6, ncol(cor.xx) + 0.7))
    }
}


