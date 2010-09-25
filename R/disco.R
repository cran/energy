### disco tests - implementation of DIStance COmponents methods in:
###
### Rizzo, M.L. and Szekely, G.J. (2010) "DISCO Analysis: A Nonparametric
### Extension of Analysis of Variance, Annals of Applied Statistics
###  Vol. 4, No. 2, 1034-1055. 
###
### Sept 2010 parts of disco package merged into energy package
### this release supports one way models
### this version does not use the C library
###
### disco: computes the decomposition
### .disco1: internal computations for one factor
### .disco1stat: provided for use with boot function
### 


disco <- function(x, factors, distance=FALSE, index=1.0, R=0) {
    ## x is response or Euclidean distance matrix or dist() object
    ## factors is a matrix or data frame of group labels
    ## distance=TRUE if x is distance, otherwise FALSE
    ## index is the exponent on distance, in (0,2]

    factors <- data.frame(factors)
    nfactors <- NCOL(factors)
    N <- NROW(x)
    if (distance && NCOL(x)==N)
        dst <- as.matrix(x) else
        dst <- as.matrix(dist(x))
    if(!isTRUE(all.equal(index, 1)))
        dst <- dst^index

    stats <- matrix(0, nfactors, 6)
    colnames(stats) <- c("Trt","Within","df1","df2","Stat","p-value")

    for (j in 1:nfactors) {
        trt <- factors[,j]
        stats[j, 1:4] <- .disco1(trt=trt, dst=dst)
        if (R > 0) {
            b <- boot(data = dst, statistic = .disco1stat, sim = "permutation",
                R = R, trt = trt)
            stats[j, 5] <- b$t0
            r <- c(b$t0, b$t)
            stats[j, 6] <- mean(r >= b$t0)
        } else {
    		stats[j, 5] <- .disco1stat(dst, i=1:nrow(dst), trt=trt)
    		stats[j, 6] <- NA
    		}
    	}

    methodname <- "DISCO"
    dataname <- deparse(substitute(x))
    total <- sum(stats[1,1:2])
    within <- total - sum(stats[ ,1])
    Df.trt <- stats[, 3]
    factor.names <- names(factors)
    factor.levels <- sapply(factors, nlevels)
    sizes <- sapply(factors, tabulate)
    e <- list(
        call = match.call(),
        method = methodname,
        statistic = stats[ ,5],
        p.value = stats[ ,6],
        k = nfactors,
        N = N,
        between = stats[ ,1],
        withins = stats[ ,2],
        within = within,
        total = total,
        Df.trt = Df.trt,
        Df.e = nrow(dst) - sum(Df.trt) - 1,
        index = index,
        factor.names = factor.names,
        factor.levels = factor.levels,
        sample.sizes = sizes,
        stats = stats
        )
    class(e) <- "disco"
    e
}

.disco1 <- function(trt, dst) {
	## dst is Euclidean distance matrix or power of it
	## trt is the treatment, a factor

	trt <- factor(trt)
	k <- nlevels(trt)
	n <- tabulate(trt)
	N <- sum(n)
	total <- sum(dst) / (2*N)
	y <- as.vector(dst[,1])
	M <- model.matrix(y ~ 0 + trt)
	G <- t(M) %*% dst %*% M
	withins <- diag(G) / (2*n)
	W <- sum(withins)
	B <- total - W
	c(B, W, k-1, N-k)
}

.disco1stat <- function(dst, i, trt) {
	## i is permuation vector supplied by bootstrap
	## dst is Euclidean distance matrix or power of it
	## trt is the treatment, a factor
    idx <- 1:nrow(dst)
    d <- .disco1(trt=trt[idx[i]], dst=dst)
    statistic <- (d[1]/d[3]) / (d[2]/d[4])
}

	
print.disco <-
function(x, ...) {
    k <- x$k
    md1 <- x$between / x$Df.trt
    md2 <- x$within / x$Df.e
    f0 <- x$statistic
    print(x$call)
    cat(sprintf("\nDistance Components: index %5.2f\n", x$index))
    cat(sprintf("%-20s %4s %10s %10s %10s %10s\n", "Source",
        "Df","Sum Dist", "Mean Dist", "F-ratio", "p-value"))
    for (i in 1:k) {
        fname <- x$factor.names[i]
        cat(sprintf("%-20s %4d %10.5f %10.5f %10.3f %10s\n", fname,
            x$Df.trt[i], x$between[i],
            md1[i], f0[i], format.pval(x$p.value[i])))
    }
    cat(sprintf("%-20s %4d %10.5f %10.5f\n", "Within",
        x$Df.e, x$within, md2))
    cat(sprintf("%-20s %4d %10.5f\n", "Total", x$N-1, x$total))
}


