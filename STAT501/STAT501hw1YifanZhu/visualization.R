# pairwise scatter plots

paper <- read.table(file = "../datasets/paper-quality.dat")
names(paper) <- c("density", "Machine direction", "Cross direction")
pairs(paper) #see other examples in the pairs plot

dev.copy2pdf(file = "Rplots/paper.pdf")


lizards <- read.table(file = "../datasets/lizards.dat")
names(lizards) <- c("Mass", "SVL", "HLS")
pairs(lizards)

dev.copy2pdf(file = "Rplots/lizards.pdf")

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

dev.copy2pdf(file = "Rplots/bears-grcrvs.pdf")




# star plots

utilities <- read.table(file = "../datasets/public_utilities.dat")
names(utilities) <- c("Fixed-charge coverage", "Rate of return on capital",
                      "Cost per kW capacity in place", "Annual load factor",
                      "Peak kWh demand growth from 1974",
                      "Sales (kWh use per year)", "Percent nuclear",
                      "Total fuel cost (cents per kWh)", "Company")

par(mfrow = c(1, 2))

stars(utilities[,-9], labels = as.character(utilities[,9]))

dev.copy2pdf(file = "Rplots/utilities-stars.pdf")

# Chernoff faces example

library(TeachingDemos)  # need package TeachingDemos installed
faces(utilities[,-9], labels = utilities[,9])

dev.copy2pdf(file = "Rplots/utilities-faces.pdf")

faces2(utilities[,-9], labels = utilities[,9]) # another function

dev.copy2pdf(file = "Rplots/utilities-faces2.pdf")



library(andrews)  # need package andrews installed

par(mfrow=c(2,2))

andrews(iris, type = 4, clr = 5, ymax = 2, main = "Type = 4")

andrews(iris, type = 3, clr = 5, ymax = 4, main = "Type = 3")

andrews(iris, type = 2, clr = 5, ymax = 3, main = "Type = 2")

andrews(iris, type = 1, clr = 5, ymax = 4, main = "Type = 1")

dev.copy2pdf(file = "Rplots/iris-andrews.pdf")


bulls <- read.table(file = "http://www.public.iastate.edu/~maitra/stat501/datasets/bulls.dat")
names(bulls) <- c("Breed", "Sale Price", "Yearling height", "Fat Free Body",
                  "Percent Fat-free", "Frame", "Back fat", "Sale height",
                  "Sale weight")




# Parallel coordinates plot

source("parallelplot.R")
source("combinations.R")

parallelplot(cbind(iris[,-5], as.numeric(iris[,5]))) #note that the last column
                                                     #has to contain the ids,
                                                     #as the function is written
                                                     #in numeric order
dev.copy2pdf(file = "Rplots/iris-ppplot.pdf")


# Star Coordinate Plots

source("starcoord.R")
starcoord(iris,class = T)
starcoord(cbind(bulls[,-1], as.factor(bulls[,1])),class = T)
dev.copy2pdf(file = "Rplots/bulls-starcoord.pdf")


source("surveyplot.R")

surveyplot(cbind(bulls[,-1], bulls[,1]),order = 5)
dev.copy2pdf(file = "Rplots/bulls-survey.pdf")


surveyplot(cbind(iris[,-5],as.numeric(as.factor(iris[,5]))),order = 1)
dev.copy2pdf(file = "Rplots/iris-survey.pdf")


# Radial visualization Plots

source("mmnorm.R") 
source("circledraw.R")
source("radviz2d.R")

radviz2d(iris)

dev.copy2pdf(file = "Rplots/iris-radviz.pdf")

radviz2d(cbind(bulls[,-1],as.factor(bulls[,1])))

dev.copy2pdf(file = "Rplots/bulls-radviz.pdf")

andrews(bulls, type = 4, clr = 1, ymax = 2)

dev.copy2pdf(file = "Rplots/bulls-andrews.pdf")

library(scatterplot3d)

x <- read.table(file = "/home/maitra/ramleri/research/sim.datasets/c_sep/p3/data.p3K5c1.0n5000s0.25_1.dat")
cols <-  scan(file = "/home/maitra/ramleri/research/sim.datasets/c_sep/p3/class.p3K5c1.0n5000s0.25_1.dat")

scatterplot3d(x, color = cols)


x <- read.table(file = "../datasets/data.p3K5n1000c1.5s0.25.dat")
cols <- scan(file = "../datasets/class.p3K5n1000c1.5s0.25.dat")

scatterplot3d(x, color = cols)

library(mvtnorm)

x <- rmvnorm(n = 1000, mean = rep(0, 10), sigma = var(rmvnorm(n = 11, mean = rep(0, 10))))

source("starcoord.R")
starcoord(x, class = F)
radviz2d(cbind(x, rep(1, as.factor(nrow(x)))))

