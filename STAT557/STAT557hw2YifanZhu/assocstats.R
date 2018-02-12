
#  You can compute some measures of association with 
#  the assocstats function available in the vcd package
#  Install the vcd package before running the following
#  commands.  No standard errors are computed.

library(vcd)

x <- matrix(c(48, 26, 22, 8, 2, 4, 6, 4), nrow = 2, byrow=T)

assocstats(x)

# Here is a listing of the assocstats function

function (x) 
{
    if (!is.matrix(x)) 
        stop("Function only defined for 2-way tables.")
    tab <- summary(loglm(~1 + 2, x))$tests
    phi <- sqrt(tab[2, 1]/sum(x))
    cont <- sqrt(phi^2/(1 + phi^2))
    cramer <- sqrt(phi^2/min(dim(x) - 1))
    structure(list(table = x, chisq_tests = tab, phi = phi, 
        contingency = cont, 
        cramer = cramer), class = "assocstats")
}
