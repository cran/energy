### Name: print.etest.mvnorm
### Title: Print E-test (Energy Test) of Multivariate Normality
### Aliases: print.etest.mvnorm
### Keywords: print

### ** Examples

 ## print E-test for 5-dimensional data
 x <- matrix(rnorm(100), nrow=20, ncol=5)
 e <- mvnorm.etest(x)
 print.etest.mvnorm(e)
 


