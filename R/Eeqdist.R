ksample.e <- 
function(x, sizes, distance = FALSE, ix = 1:sum(sizes)) {
    ## computes the k-sample E-statistic for equal distributions
    ##   x:          pooled sample or distance matrix
    ##   sizes:      vector of sample sizes
    ##   distance:   TRUE if x is a distance matrix, otherwise FALSE
    ##   ix:         a permutation of row indices of x
    ##   
    ## NOT much error checking here: for test use eqdist.etest
    ## This version can be sourced R, but may be slower than the 
    ## eqdist.e() version
    ## 
    k <- length(sizes)
    if (k == 1) return (0.0)
    if (k < 2) return (NA)
    e <- e0 <- 0
    if (!is.null(attr(x, "Size"))) distance <- TRUE
    x <- as.matrix(x)
    
    if (distance == TRUE) {
        ##  same as test with 0 replicates
        b <- .C("ksampleEtest", 
        x = as.double(t(x)), 
        byrow = as.integer(1),
        nsamples = as.integer(length(sizes)), 
        sizes = as.integer(sizes),
        dim = as.integer(0), 
        R = as.integer(0), 
        e0 = as.double(e),
        e = as.double(e), 
        pval = as.double(e), 
        PACKAGE = "energy")           
        return (b$e0)
    }

    ##  compute e directly, without storing distances
    d <- ncol(x)
    n <- cumsum(sizes)
    m <- 1 + c(0, n[1:(k-1)])
    for (i in 1:(k - 1)) {
        for (j in (i + 1):k) {
            n1 <- sizes[i]
            n2 <- sizes[j]
            ii <- ix[m[i]:n[i]]
            jj <- ix[m[j]:n[j]]
                if (d == 1) y <- as.matrix(c(x[ii], x[jj]))
                else y <- rbind(x[ii,], x[jj,])
                e <- e + .C("E2sample",
                        x = as.double(t(y)), 
                        sizes = as.integer(c(n1, n2)), 
                        dim = as.integer(d), 
                        e = as.double(e0),
                        PACKAGE = "energy")$e
            }
        }
    e
}


eqdist.e <- 
function(x, sizes, distance = FALSE) {
    ## multivariate E-statistic for testing equal distributions
    ##   x:          matrix of pooled sample or distance matrix
    ##   sizes:      vector of sample sizes
    ##   distance:   logical, TRUE if x is a distance matrix, otherwise false
    return(as.double(eqdist.etest(x, sizes, distance = FALSE, R = 0)$statistic))
}    

eqdist.etest <- 
function(x, sizes, distance = FALSE, R = 999) {
    ## multivariate E-test of the multisample hypothesis of equal distributions
    ##   x:          matrix of pooled sample or distance matrix
    ##   sizes:      vector of sample sizes
    ##   distance:   logical, TRUE if x is a distance matrix, otherwise false
    ##   R:          number of replicates
    ##   
    
    nsamples <- length(sizes)
    if (nsamples < 2) return (NA)
    if (min(sizes) < 1) return (NA)
    if (!is.null(attr(x, "Size"))) distance <- TRUE
           
    x <- as.matrix(x)
    if (nrow(x) != sum(sizes)) stop("nrow(x) should equal sum(sizes)")
    if (distance == FALSE && nrow(x) == ncol(x))
        warning("square data matrix with distance==FALSE")
    d <- ncol(x)
    if (distance == TRUE) d <- 0
    str <- "Multivariate "
    if (d == 1) str <- "Univariate "
    if (d == 0) str <- ""

    e0 <- 0.0
    repl <- rep(0, R)
    pval <- 1.0
    b <- .C("ksampleEtest", 
        x = as.double(t(x)), 
        byrow = as.integer(1),
        nsamples = as.integer(nsamples), 
        sizes = as.integer(sizes),
        dim = as.integer(d), 
        R = as.integer(R), 
        e0 = as.double(e0),
        e = as.double(repl), 
        pval = as.double(pval), 
        PACKAGE = "energy")       
    
    names(b$e0) <- "E-statistic"
    sz <- paste(sizes, collapse = " ", sep = "")
    methodname <- paste(str, length(sizes), 
                  "-sample E-test of equal distributions", sep = "")
    dataname <- paste("sample sizes ", sz, ", replicates ", R, sep="")
    e <- list(
        method = methodname,
        statistic = b$e0,
        p.value = b$pval,
        data.name = dataname)

    class(e) <- "htest"        
    e
}
 
