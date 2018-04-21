

#  This code is posted as exact.R

#  R has a built in function for the Fisher exact 
#  test.  It seems to work for any two-way table, 
$  as long as the counts are not too large and there
#  are not too many rows or columns in the table. 
#  This function uses a combination of algorithms 
#  developed by Joe (1988), Cryus and Metha (1985).  
#  The p-value is always for a two-sided or
#  multi-sided test. 


fisher.test(matrix(c(21,2,15,3),ncol=2, byrow=T))

fisher.test(matrix(c(3, 6, 11, 8, 4, 8, 10, 5, 5),ncol=3,byrow=T))

fisher.test(matrix(c(3,4,5,6,7,8,9,1,2,3,4,5,11,12,13,
            14,15,16,17,1,2,3,4,5), ncol=12,byrow=T))

fisher.test(matrix(c(3,4,5,6,7,8,9,1,2,3,4,5,11,12,13,
            14,15,16,17,1,2,3,4,5), ncol=12,byrow=T), 
            workspace=20000000)

fisher.test(matrix(c(3,4,5,6,7,8,9,1,2,3,4,5,11,12,13,
            14,15,16,17,1,2,3,4,5), ncol=12,byrow=T),
            simulate.p.value="T", B=10000)
