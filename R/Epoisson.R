poisson.mtest <- 
function(x, R = 999) {
    # parametric bootstrap mean distance test of Poisson distribution
    n <- length(x)
    lambda <- mean(x)
    bootobj <- boot(x, statistic = poisson.m, R = R, sim = "parametric", 
            ran.gen = function(x, y) {rpois(n, lambda)})
    p <- 1 - mean(bootobj$t < bootobj$t0)
    e <- list(
        method = paste("Mean distance test of Poisson distribution", sep = ""),
        statistic = bootobj$t0, 
        p.value = p, 
        n = n, 
        lambda = lambda,
        R = R, 
        replicates = bootobj$t)
    class(e) <- "etest.poisson"        
    e           
}

poisson.m<- 
function(x) {
    # mean distance statistic for Poissonity
    n <- length(x)
    stat <- 0
    e <- .C("poisMstat", 
            x = as.integer(x),
            nx = as.integer(n), 
            stat = as.double(stat), 
            PACKAGE = "energy")$stat
    e
}
  
print.etest.poisson <- 
function(x, ...) {
    cat("\n", x$method, "\n")
    cat("\tSample size:       ", x$n, "\n")
    cat("\tSample mean:       ", x$lambda, "\n")
    cat("\tTest statistic:    ", format(x$statistic, digits = 4), "\n")
    cat("\tApprox. p-value:   ", format.pval(x$p.value), "\n")
    cat("\t", x$R, " replicates\n", sep = "")
}
            
