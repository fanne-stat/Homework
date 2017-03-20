n <- 1500;
U <- runif(n);
h <- 1/(1 + U);

IMC <- mean(h);

IMC

c <- 1 + U;
b <- 12 * cov(h,c);

ICV <- mean(h) - b * (mean(c) - 1.5);

ICV

VIMC <-  var(h)/n;

VIMC

VICV <- var(h - b*c)/n

VICV


c1 <- exp(-U);
b1 <- cov(h, c1)/var(c1);

ICV1 <- mean(h) - b1 * (mean(c1) - (1 - exp(-1)));

ICV1

VICV1 <- var(h - b1 * c1)/n;

VICV1
