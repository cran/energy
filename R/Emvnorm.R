mvnorm.etest <- 
function(x, R=999) 
{
    # parametric bootstrap E-test for multivariate normality
    e <- list(
        method = paste("E-test of multivariate normality", sep = ""),
        statistic = 0, 
        p.value = 0, 
        n = nrow(x), 
        d = ncol(x), 
        R = R, 
        replicates = NULL)
    if (is.vector(x)) {
        e$n <- length(x)
        e$d <- 1
        bootobj <- boot(x, statistic = normal.e, R = R, sim = "parametric", 
            ran.gen = function(x, y) {return(rnorm(e$n)) })
        }
        else {
        bootobj <- boot(x, statistic = mvnorm.e, R = R, sim = "parametric", 
            ran.gen = function(x, y) {
                return(matrix(rnorm(e$n * e$d), nrow=e$n, ncol=e$d)) })
        }
    p <- 1 - mean(bootobj$t < bootobj$t0)
    e$statistic = bootobj$t0
    e$p.value = p
    e$replicates <- bootobj$t
    class(e) <- "etest.mvnorm"        
    e                 
}

mvnorm.e <- 
function(x) 
{
    # E-statistic for multivariate normality
    if (is.vector(x)) return(E.norm(x))
    n <- nrow(x)
    d <- ncol(x)
    if (n < 2) return(E.norm(x))
    z <- scale(x, scale = FALSE)    #subtract column means and 
    ev <- eigen(var(x), symmetric = TRUE)    #compute S^(-1/2)
    P <- ev$vectors
    lambda <- ev$values    
    y <- z %*% (P %*% diag(1 / sqrt(lambda)) %*% t(P))
    if (any(!is.finite(y))) return (NA)
    stat <- 0
    e <- .C("mvnEstat", y = as.double(t(y)), byrow = as.integer(TRUE),
            nobs = as.integer(n), dim = as.integer(d), 
            stat = as.double(stat), PACKAGE = "energy")$stat
    e
}

normal.e <- 
function(x) 
{
   x <- as.vector(x)
   y <- sort(x)
   n <- length(y)
   if (y[1] == y[n]) return (NA)
   y <- scale(y) 
   K <- seq(1 - n, n - 1, 2)
   e <- 2 * (sum(2 * y * pnorm(y) + 2 * dnorm(y)) - n / sqrt(pi) - mean(K * y))
   e
}
   
print.etest.mvnorm <- 
function(x, ...) 
{
    cat("\n", x$method, "\n")
    cat("\tSample size:       ", x$n, "\n")
    cat("\tDimension:         ", x$d, "\n")
    cat("\tTest statistic:    ", format(x$statistic, digits = 4), "\n")
    cat("\tApprox. p-value:   ", format.pval(x$p.value), "\n")
    cat("\t", x$R, " replicates\n", sep="")
}
            
