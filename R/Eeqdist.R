eqdist.e <- 
function(x, sizes, distance = FALSE, method = c("original","disco")) 
{
    ## multivariate E-statistic for testing equal distributions
    ##   x:          matrix of pooled sample or distance matrix
    ##   sizes:      vector of sample sizes
    ##   distance:   logical, TRUE if x is a distance matrix, otherwise false
    ##   method:     original (default) or disco components
    
    method <-match.arg(method)
    if (method=="disco") {
        g <- as.factor(rep(1:length(sizes), sizes))
        return(sum(disco(x, factors=g, distance)$between)) 
        } else
    return(as.double(eqdist.etest(x, sizes, distance = FALSE,
           method = method, R=0)$statistic))
}    

eqdist.etest <- 
function(x, sizes, distance = FALSE, method = c("original","disco"), R = 999) 
{
    ## multivariate E-test of the multisample hypothesis of equal distributions
    ##   x:          matrix of pooled sample or distance matrix
    ##   sizes:      vector of sample sizes
    ##   distance:   logical, TRUE if x is a distance matrix, otherwise false
    ##   method:     original (default) or disco between components
    ##   R:          number of replicates
    ##   
    
    method <-match.arg(method)
    if (method=="disco") {
      g <- as.factor(rep(1:length(sizes), sizes))
      return(disco(x, factors=g, distance, R))
      }

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
 
ksample.e <- 
function(x, sizes, distance = FALSE, method = c("original","disco"),
         ix = 1:sum(sizes))
{
    ## computes k-sample E-statistics for equal distributions
    ## retained for backward compatibility or use with boot
    ## (this function simply passes arguments to eqdist.e)
    ## 
    ##   x:          pooled sample or distance matrix
    ##   sizes:      vector of sample sizes
    ##   distance:   TRUE if x is a distance matrix, otherwise FALSE
    ##   method:     default (original) or disco between components
    ##   ix:         a permutation of row indices of x
    ##   
    x <- as.matrix(x)
    method <- match.arg(method)
    eqdist.e(x[ix,], sizes, distance, method)
}

