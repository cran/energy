### Name: mvnorm.etest
### Title: E-statistic (Energy) Test of Multivariate Normality
### Aliases: mvnorm.etest
### Keywords: multivariate htest

### ** Examples

 ## test if the iris Setosa data has multivariate normal distribution
 data(iris)
 mvnorm.etest(iris[1:50,1:4])
 
 ## test a univariate sample for normality
 x <- runif(50, 0, 10)
 mvnorm.etest(x)
 


