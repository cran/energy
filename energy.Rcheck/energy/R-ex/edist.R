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
 


