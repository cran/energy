### Name: ksample.e
### Title: E-statistic (Energy Statistic) for Multivariate k-sample Test of
###   Equal Distributions
### Aliases: ksample.e
### Keywords: multivariate htest nonparametric

### ** Examples

## compute 3-sample E-statistic for 4-dimensional iris data
 data(iris)
 ksample.e(iris[,1:4], c(50,50,50))

## compute univariate two-sample incomplete E-statistic
 x1 <- rnorm(200)
 x2 <- rnorm(300, .5)
 x <- c(x1, x2)
 ksample.e(x, c(200, 300), incomplete=TRUE, N=100)
 



