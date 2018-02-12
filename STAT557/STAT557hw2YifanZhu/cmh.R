
#  This code is posted as cmh.R

#  Enter the data for the thre 2x2 tables
#  for the Mantel-Haenszel (1958) data

 x.array <- array(c(81, 34, 24, 71, 118, 74, 69, 105, 82, 63, 52, 93),c(2,2,3))


#  Print the array

 x.array


#  Compute the Cochran-Mantel-Haenszel test

 mantelhaen.test(x.array, conf.level=0.95)











