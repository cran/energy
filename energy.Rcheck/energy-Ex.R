### * <HEADER>
###
attach(NULL, name = "CheckExEnv")
assign(".CheckExEnv", as.environment(2), pos = length(search())) # base
## This plot.new() patch has no effect yet for persp();
## layout() & filled.contour() are now ok
assign("plot.new",
       function() {
	   .Internal(plot.new())
	   pp <- par(c("mfg","mfcol","oma","mar"))
	   if(all(pp$mfg[1:2] == c(1, pp$mfcol[2]))) {
               outer <- (oma4 <- pp$oma[4]) > 0; mar4 <- pp$mar[4]
               mtext(paste("help(", ..nameEx, ")"), side = 4,
                     line = if(outer)max(1, oma4 - 1) else min(1, mar4 - 1),
                     outer = outer, adj = 1, cex = .8, col = "orchid")
	   }
       },
       env = environment(plot))
assign("cleanEx",
       function(env = .GlobalEnv) {
	   rm(list = ls(envir = env, all.names = TRUE), envir = env)
           RNGkind("Wichmann-Hill", "default")
	   assign(".Random.seed", c(0, rep(7654, 3)), pos = 1)
	   assign("T", delay(stop("T used instead of TRUE")),
		  pos = .CheckExEnv)
	   assign("F", delay(stop("F used instead of FALSE")),
		  pos = .CheckExEnv)
       },
       env = .CheckExEnv)
assign("..nameEx", "__{must remake R-ex/*.R}__", env = .CheckExEnv) # for now
assign("ptime", proc.time(), env = .CheckExEnv)
postscript("energy-Examples.ps")
assign("par.postscript", par(no.readonly = TRUE), env = .CheckExEnv)
options(contrasts = c(unordered = "contr.treatment", ordered = "contr.poly"), pager="console")
library('energy')

cleanEx(); ..nameEx <- "edist"

### * edist

### Name: edist
### Title: E-distance
### Aliases: edist
### Keywords: multivariate cluster nonparametric

### ** Examples

 ## compute e-distances for 3 samples of iris data
 data(iris)
 edist(iris[,1:4], c(50,50,50))

## Don't show: 
     ## compute e-distances from a distance object
     data(iris)
     edist(dist(iris[,1:4]), c(50, 50, 50), distance=TRUE)
    
     ## compute e-distances from a distance matrix
     data(iris)
     d <- as.matrix(dist(iris[,1:4]))
     edist(d, c(50, 50, 50), distance=TRUE) 
     
## End Don't show
 ## compute e-distances from vector of group labels
 d <- dist(matrix(rnorm(100), nrow=50))
 g <- cutree(energy.hclust(d), k=4)
 edist(d, sizes=table(g), ix=rank(g, ties.method="first"))
 


cleanEx(); ..nameEx <- "energy.hclust"

### * energy.hclust

### Name: energy.hclust
### Title: Hierarchical Clustering by Minimum (Energy) E-distance
### Aliases: energy.hclust
### Keywords: multivariate cluster

### ** Examples

   ## Not run: 
##D    
##D    library(cluster)
##D    data(animals)
##D    plot(energy.hclust(dist(animals)))
##D    
## End(Not run)
   
   library(mva)
   data(USArrests)
   ecl <- energy.hclust(dist(USArrests))
   print(ecl)    
   plot(ecl)
   cutree(ecl, k=3)
   cutree(ecl, h=150)
   
   ## compare performance of e-clustering, Ward's method, group average method
   ## when sampled populations have equal means: n=200, d=5, two groups
   z <- rbind(matrix(rnorm(1000), nrow=200), matrix(rnorm(1000, 0, 5), nrow=200))
   g <- c(rep(1, 200), rep(2, 200))
   d <- dist(z)
   e <- energy.hclust(d)
   a <- hclust(d, method="average")
   w <- hclust(d^2, method="ward")
   list("E" = table(cutree(e, k=2) == g), "Ward" = table(cutree(w, k=2) == g),
        "Avg" = table(cutree(a, k=2) == g))
 


cleanEx(); ..nameEx <- "eqdist.etest"

### * eqdist.etest

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



cleanEx(); ..nameEx <- "ksample.e"

### * ksample.e

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



cleanEx(); ..nameEx <- "mvnorm.e"

### * mvnorm.e

### Name: mvnorm.e
### Title: E-statistic (Energy Statistic) for Testing Multivariate
###   Normality
### Aliases: mvnorm.e
### Keywords: multivariate htest

### ** Examples

 
 ## compute multivariate normality test statistic for iris Setosa data
 data(iris)
 mvnorm.e(iris[1:50, 1:4])
 


cleanEx(); ..nameEx <- "mvnorm.etest"

### * mvnorm.etest

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
 


cleanEx(); ..nameEx <- "normal.e"

### * normal.e

### Name: normal.e
### Title: E-statistic (Energy Statistic) for Testing Univariate Normality
### Aliases: normal.e
### Keywords: htest

### ** Examples

 x <- rnorm(30)
 normal.e(x)



cleanEx(); ..nameEx <- "poisson.m"

### * poisson.m

### Name: poisson.m
### Title: Mean Distance Statistic for Testing Poisson Distribution
### Aliases: poisson.m
### Keywords: htest

### ** Examples

 x <- rpois(20, 1)
poisson.m(x)
 


cleanEx(); ..nameEx <- "poisson.mtest"

### * poisson.mtest

### Name: poisson.mtest
### Title: Mean Distance Test for Poisson Distribution
### Aliases: poisson.mtest
### Keywords: htest

### ** Examples

 x <- rpois(20, 1)
 poisson.mtest(x)
 


cleanEx(); ..nameEx <- "print.etest.eqdist"

### * print.etest.eqdist

### Name: print.etest.eqdist
### Title: Print Multisample E-test (Energy Test) for Equal Distributions
### Aliases: print.etest.eqdist
### Keywords: print

### ** Examples

## print test if the 3 varieties of iris data (d=4) have equal distributions
 data(iris)
 e <- eqdist.etest(iris[,1:4], c(50,50,50))
 print.etest.eqdist(e)
 


cleanEx(); ..nameEx <- "print.etest.mvnorm"

### * print.etest.mvnorm

### Name: print.etest.mvnorm
### Title: Print E-test (Energy Test) of Multivariate Normality
### Aliases: print.etest.mvnorm
### Keywords: print

### ** Examples

 ## print E-test for 5-dimensional data
 x <- matrix(rnorm(100), nrow=20, ncol=5)
 e <- mvnorm.etest(x)
 print.etest.mvnorm(e)
 


cleanEx(); ..nameEx <- "print.etest.poisson"

### * print.etest.poisson

### Name: print.etest.poisson
### Title: Print Mean Distance Test for Poisson Distribution
### Aliases: print.etest.poisson
### Keywords: print

### ** Examples

 x <- rpois(20, 1)
 e <- poisson.mtest(x)
 print.etest.poisson(e)
 


### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", env = .CheckExEnv),"\n")
dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
