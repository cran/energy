### Name: print.etest.eqdist
### Title: Print Multisample E-test (Energy Test) for Equal Distributions
### Aliases: print.etest.eqdist
### Keywords: print

### ** Examples

## print test if the 3 varieties of iris data (d=4) have equal distributions
 data(iris)
 e <- eqdist.etest(iris[,1:4], c(50,50,50))
 print.etest.eqdist(e)
 


