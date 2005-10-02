ksample.e <- 
function(x, sizes, distance = FALSE, ix = 1:sum(sizes), 
         incomplete = FALSE, N = 100) {
    ## computes the k-sample E-statistic for equal distributions
    ##   x:          pooled sample or distance matrix
    ##   sizes:      vector of sample sizes
    ##   distance:   TRUE if x is a distance matrix, otherwise FALSE
    ##   ix:         a permutation of row indices of x
    ##   incomplete: if TRUE compute incomplete E-statistic
    ##   N:          incomplete sample size
    ##   
    ##   NOT much error checking here: for test use eqdist.etest
    ## 
    k <- length(sizes)
    if (k == 1) return (0.0)
    if (k < 2) return (NA)
    e <- e0 <- 0
    if (!is.null(attr(x, "Size"))) distance <- TRUE
    x <- as.matrix(x)
    if (incomplete == TRUE && distance == FALSE && any(sizes > N))
        return(.incomplete.etest(x, sizes=sizes, R=0, N=N)$statistic)  
    
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

eqdist.etest <- 
function(x, sizes, distance = FALSE, incomplete = FALSE, N = 100, R = 999) {
    ## multivariate E-test of the multisample hypothesis of equal distributions
    ##   x:          matrix of pooled sample or distance matrix
    ##   sizes:      vector of sample sizes
    ##   distance:   logical, TRUE if x is a distance matrix, otherwise false
    ##   R:          number of replicates
    ##   incomplete: logical, TRUE if incomplete E statistics
    ##   N:          sample size for incomplete version
    ##   
    
    nsamples <- length(sizes)
    if (nsamples < 2) return (NA)
    if (min(sizes) < 1) return (NA)
    if (!is.null(attr(x, "Size"))) distance <- TRUE
    
    if (nsamples == 2) {
        if (incomplete == TRUE && distance == FALSE && any(sizes > N))
            return(.incomplete.etest(x, sizes=sizes, N=N, R=R))  
        }
        
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
 

.incomplete.etest <- 
function(x, sizes, N = 100, R = 999) {
    ##   intended to be called from eqdist.etest, not much error checking
    ## 
    ##   multivariate E-test of the multisample hypothesis of equal 
    ##   distributions, incomplete E-statistic
    ##   C library currently supports two sample test only
    ##   x:          matrix of pooled sample or distance matrix
    ##   sizes:      vector of sample sizes
    ##   N:          max sample size for estimation of pairwise E
    ##   R:          number of replicates
    ##   
    
    k <- length(sizes)
    if (k != 2) return (NA);
    n <- cumsum(sizes) 
    m <- 1 + c(0, n[1:(k-1)])
    x <- as.matrix(x)
    if (nrow(x) != sum(sizes)) return (NA)
    d <- ncol(x)
    r <- nrow(x)
    str <- "Multivariate "
    if (d == 1) str <- "Univariate "
    e0 <- 0
    pval <- 1
    repl <- rep(0, R)  
    b <- .C("twosampleIEtest", 
        x = as.double(t(x)), 
        byrow = as.integer(1),
        sizes = as.integer(sizes),
        dim = as.integer(d),
        iN = as.integer(N),
        R = as.integer(R), 
        e0 = as.double(e0),
        e = as.double(repl), 
        pval = as.double(pval),
        PACKAGE = "energy")           

    methodname <- paste(str, length(sizes), 
                    "-sample E-test of equal distributions", sep = "") 
    sz <- paste(sizes, collapse = " ", sep = "")
    dataname <- paste("sample sizes ", sz, ", replicates ", 
                  R, ", N ", N, sep="")
    names(b$e0) <- "(Incomplete) E-statistic"
    e <- list(
        method = methodname,
        statistic = b$e0,
        p.value = b$pval,
        data.name = dataname)

    class(e) <- "htest"        
    e
}

