ksample.e <- 
function(x, sizes, distance = FALSE, ix = 1:sum(sizes), incomplete = FALSE, N = 100) {
    #computes the k-sample E-statistic for equal distributions
    #  x:          pooled sample or distance matrix
    #  sizes:      vector of sample sizes
    #  distance:   TRUE if x is a distance matrix, otherwise FALSE
    #  ix:         a permutation of row indices of x
    #  incomplete: use incomplete E-statistics?
    #  N:          sample size if incomplete
    #    
    k <- length(sizes)
    if (k == 1) return (0.0)
    if (k < 2) return (NA)
    e <- 0
    n <- cumsum(sizes)
    m <- 1 + c(0, n[1:(k-1)])
    if (distance == FALSE) {
        if (is.vector(x)) x <- matrix(x, nrow = length(x), ncol = 1)
        dst <- as.matrix(dist(x))
        }
    else dst <- as.matrix(x)
    for (i in 1:(k - 1)) {
        for (j in (i + 1):k) {
            n1 <- sizes[i]
            n2 <- sizes[j]
            ii <- ix[m[i]:n[i]]
            jj <- ix[m[j]:n[j]]
            if (incomplete) {
                if (n1 > N) {
                    n1 <- N
                    ii <- ix[sample(m[i]:n[i], N, replace = FALSE)]
                }
                if (n2 > N) {
                    n2 <- N
                    jj <- ix[sample(m[j]:n[j], N, replace = FALSE)]
                }
            }
            w <- n1 * n2 / (n1 + n2)
            m11 <- sum(dst[ii, ii]) / (n1 * n1)
            m22 <- sum(dst[jj, jj]) / (n2 * n2)
            m12 <- sum(dst[ii, jj]) / (n1 * n2)
            e <- e + w * ((m12 + m12) - (m11 + m22))
        }
    }
    e
}

eqdist.etest <- 
function(x, sizes, distance = FALSE, incomplete = FALSE, N = 100, R = 999) {
    #multivariate E-test of the multisample hypothesis of equal distributions
    #  x:          matrix of pooled sample or distance matrix
    #  sizes:      vector of sample sizes
    #  distance:   logical, TRUE if x is a distance matrix, otherwise false
    #  incomplete: use incomplete E-statistics?
    #  N:          sample size if incomplete
    #  R:          number of replicates
    #  
    if (length(sizes) < 2) return (NA)
    if (min(sizes) < 1) return (NA)
    if (distance == FALSE) {
        if (is.vector(x)) x <- matrix(x, nrow = length(x), ncol = 1)
        dst <- as.matrix(dist(x))
    }
    else dst <- x
    bootobj <- boot(data = dst, statistic = function(dst, ix, sizes, incomplete, N) {
            ksample.e(dst, sizes, distance=TRUE, ix=ix, incomplete, N)}, 
            R = R, sim = "permutation", sizes = sizes, incomplete = incomplete, N = N)
    p <- 1 - mean(bootobj$t < bootobj$t0)
    e <- list(
        method = paste("Multivariate ", length(sizes), "-sample E-test of equal distributions", sep = ""),
        statistic = bootobj$t0,
        p.value = p,
        n = sizes,
        R = R,
        incomplete = incomplete,
        N = N,
        replicates = bootobj$t)
    class(e) <- "etest.eqdist"        
    e
    }

print.etest.eqdist <- 
function(x, ...) {
    cat("\n", x$method, "\n")
    cat("\tSample sizes:       ", x$n, "\n")
    cat("\tTest statistic:    ", format(x$statistic, digits = 4), "\n")
    cat("\tApprox. p-value:   ", format.pval(x$p.value), "\n")
    cat("\t", x$R, " replicates, resampling method = permutation\n", sep = "")
    if (x$incomplete==TRUE)
       cat("\tIncomplete E-statistic, max. sample size", x$N, "\n\n")
}
            
