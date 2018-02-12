# pairwise scatter plots

paper <- read.table(file = "../datasets/paper-quality.dat")
names(paper) <- c("density", "Machine direction", "Cross direction")
pairs(paper) #see other examples in the pairs plot


lizards <- read.table(file = "../datasets/lizards.dat")
names(lizards) <- c("Mass", "SVL", "HLS")
pairs(lizards)


# growth curves example

par(mfrow = c(1, 2))

bears <- read.table(file = "../datasets/bears.dat")
names(bears) <- c("Wt2", "Wt3", "Wt4", "Wt5","Length2", "Length3", "Length4", 
                  "Length5")

plot(x = 2:5, y = bears[1,c(1:4)], type = "n", xlab = "Year",
     ylab = "Weight", ylim = range(bears[, c(1:4)]))

for (i in 1:7)  lines(x = 2:5, y = bears[i,c(1:4)], lty = i + 1, col = i + 1) 


plot(x = 2:5, y = bears[1,-c(1:4)], type = "n", xlab = "Year",
     ylab = "Length", ylim = range(bears[, -c(1:4)]))

for (i in 1:7)  lines(x = 2:5, y = bears[i,-c(1:4)], lty = i + 1, col = i + 1) 





# stars

utilities <- read.table(file = "../datasets/public_utilities.dat")
names(utilities) <- c("Fixed-charge coverage", "Rate of return on capital",
                      "Cost per kW capacity in place", "Annual load factor",
                      "Peak kWh demand growth from 1974",
                      "Sales (kWh use per year)", "Percent nuclear",
                      "Total fuel cost (cents per kWh)", "Company")

par(mfrow = c(1, 1))

stars(utilities[,-9], labels = utilities[,9])

# Chernoff faces example

library(TeachingDemos)
faces(utilities[,-9], labels = utilities[,9])
faces2(utilities[,-9], labels = utilities[,9]) # another way


