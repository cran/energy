### Name: eqdist.etest
### Title: Multisample E-statistic (Energy) Test of Equal Distributions
### Aliases: eqdist.etest
### Keywords: multivariate htest nonparametric

### ** Examples

 ## test if the 3 varieties of iris data (d=4) have equal distributions
 data(iris)
 eqdist.etest(iris[,1:4], c(50,50,50))
 
 ## univariate two-sample test using incomplete E-statistics
 x1 <- rnorm(200)
 x2 <- rnorm(300, .5)
 eqdist.etest(c(x1, x2), c(200, 300), incomplete=TRUE, N=100)



