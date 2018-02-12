
# Create a function to convert a matrix or data frame of counts 
# to data frame of cases with one row for each case (or subject)
#  x is the name of the data frame of counts
#  countcol is the name of the column in x containing the counts

  countsToCases <- function(x, countcol = "Freq") {
    # Get the row indices to pull from x
    idx <- rep.int(seq_len(nrow(x)), x[[countcol]])

    # Drop count column
    x[[countcol]] <- NULL

    # Get the rows from x
    x[idx, ]
}


#  Make a list of counts for the 2-way table
#  of counts for the New Haven study.

  x<-c(30,48,19,121,18,102,23,80,344,11,280,20,75,382,141)

#  Create a table with row and column labels

newhaven.table<-cbind(expand.grid(list(
      Diagnosis=c("Affective", "Alcoholic", "Organic","Schizophrenic","Senile"),
      Treatment=c("<Psychotherapy","Organic Therapy", "Custodial Care"))),
      count=x)
newhaven.table


# Convert the table of counts to a matrix of cases
# with one row for each respondent

  nhlist <- countsToCases(newhaven.table, countcol="count")
  nhlist

#  Define a function to compute the Cramer V statistic
#  from the cases data.  The first argumane is the data
#  frame or matrix containing the cases.  The row and 
#  column categories for the responses must be in the 
#  first two coulmns. The second argument is a list of 
#  indicies defining the individual cases. 

   cramerv <- function(xlist, inds) 
     {
       # Create a two-way table of counts
       x<-table(xlist[inds, 1], xlist[inds, 2])

       x2 <- chisq.test(x)$statistic  #Pearson chi-sq
       phi<-sqrt(x2/sum(temp))         #Phi Coefficient
       cc<-sqrt(x2/(x2+sum(temp))) #Contingency coefficient
                                   #Cramer V
       v<-sqrt(x2/sum(temp)/min(nrow(temp)-1,ncol(temp)-1))
       v 
     }

# Calculate Cramer V for the data in nhlist

   cramerv(nhlist, row(nhlist))


   install.packages("boot")
   library(boot)

# Calculate a bootstrap value for the standard error 

   results <- boot(nhlist, statistic=cramerv, R=2000) 

# view results
   results
   plot(results)
 
# get 95% confidence interval
    boot.ci(results, type="all")

