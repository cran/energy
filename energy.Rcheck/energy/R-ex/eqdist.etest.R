### Name: eqdist.etest
### Title: Multisample E-statistic (Energy) Test of Equal Distributions
### Aliases: eqdist.etest
### Keywords: multivariate htest nonparametric

### ** Examples

 data(iris)
 
 ## test if the 3 varieties of iris data (d=4) have equal distributions
 eqdist.etest(iris[,1:4], c(50,50,50))

 ## compare incomplete versions of two sample test
 x <- c(rpois(400, 2), rnbinom(600, size=1, mu=2))
 eqdist.etest(x, c(400, 600), incomplete=TRUE, N=100)
 eqdist.etest(x, c(400, 600), incomplete=TRUE, N=200)
  
## Don't show: 
  x <- matrix(rnorm(500), nrow=100)
  y <- matrix(rnorm(500, mean=5), nrow=100)
  x <- rbind(x, y)
  eqdist.etest(dist(x), sizes=c(100, 100), distance=TRUE)
  eqdist.etest(x, sizes=c(100, 100), incomplete=TRUE, N=50, R=100)
## End Don't show



