### Name: ksample.e
### Title: E-statistic (Energy Statistic) for Multivariate k-sample Test of
###   Equal Distributions
### Aliases: ksample.e
### Keywords: multivariate htest nonparametric

### ** Examples

## compute 3-sample E-statistic for 4-dimensional iris data
 data(iris)
 ksample.e(iris[,1:4], c(50,50,50))

## compute a 3-sample univariate E-statistic
 ksample.e(rnorm(150), c(25,75,50))



