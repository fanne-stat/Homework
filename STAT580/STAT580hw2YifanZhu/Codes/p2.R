require(ggplot2)

U <- runif(5000, min = 0, max = 1);
X <- 10^U;

d <- data.frame(x = X);

ggplot(d, aes(x = X)) + geom_histogram(aes(y = ..density..), binwidth = 0.5, color = "blue", fill = "white") + geom_density(alpha = .2 , colour = "red",fill = "#FF6666")


