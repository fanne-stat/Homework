#1
##(a)
olive <- read.table(file = "http://maitra.public.iastate.edu/stat501/datasets/olive.dat", header = T)



attach(airquality)
Month <- factor(Month, labels = month.abb[5:9])
pairwise.t.test(Ozone, Month)
pairwise.t.test(Ozone, Month, p.adj = "bonf")
pairwise.t.test(Ozone, Month, pool.sd = FALSE)
detach()