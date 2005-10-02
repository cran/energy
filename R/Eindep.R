indep.e<- 
function(x, y) {
    # energy statistic for multivariate independence
    x <- as.matrix(x)
    y <- as.matrix(y)
    n <- nrow(x)
    m <- nrow(y)
    if (n != m || n < 2) stop("Sample sizes must agree")
    if (! (all(is.finite(c(x, y)))))
        stop("Data contains missing or infinite values")
    
    stat <- 0
    dims <- c(n, ncol(x), ncol(y))

    if (ncol(x) == 1 && ncol(y) == 1) {
    e <- .C("indep", 
            x = as.double(t(x)),
            y = as.double(t(y)),
            size = as.integer(n), 
            stat = as.double(stat), 
            PACKAGE = "energy")
    print(e$stat)
    }
    
    e <- .C("indepE", 
            x = as.double(t(x)),
            y = as.double(t(y)),
            byrow = as.integer(TRUE),
            dims = as.integer(dims), 
            stat = as.double(stat), 
            PACKAGE = "energy")
    sqrt(e$stat)
}
  
            
indep.etest<- 
function(x, y, R=199) {
    # energy test for multivariate independence
    x <- as.matrix(x)
    y <- as.matrix(y)
    n <- nrow(x)
    m <- nrow(y)
    if (n != m || n < 2) stop("Sample sizes must agree")
    if (! (all(is.finite(c(x, y))))) 
        stop("Data contains missing or infinite values")

    stat <- reps <- 0
    if (R > 0) reps <- rep(0, R)
    pval <- 1
    dims <- c(n, ncol(x), ncol(y), R)
    
    a <- .C("indepEtest", 
            x = as.double(t(x)),
            y = as.double(t(y)),
            byrow = as.integer(TRUE),
            dims = as.integer(dims), 
            stat = as.double(stat), 
            reps = as.double(reps),
            pval = as.double(pval),
            PACKAGE = "energy")
    stat <- sqrt(a$stat)
    names(stat) <- "I"
    dataname <- paste("x (",n," by ",ncol(x), "), y(",n," by ", ncol(y), "), replicates ", R, sep="")
    e <- list(
        method = paste("Energy test of independence", sep = ""),
        statistic = stat, 
        p.value = a$pval, 
        data.name = dataname)
    class(e) <- "htest"                   
    e
}
  
            