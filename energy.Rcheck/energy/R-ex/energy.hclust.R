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
 


