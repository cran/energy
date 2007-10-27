mvnorm.etest <- 
function(x, mu = NULL, Sigma = NULL, R = 999) 
{
    # parametric bootstrap E-test for multivariate normality
    # matrix x is a multivariate sample with obs. in rows
    # by default mu and Sigma will be estimated if not specified
    if (is.vector(x)) {
        n <- length(x)
        d <- 1
        bootobj <- boot(x, statistic = normal.e, R = R, sim = "parametric", 
            ran.gen = function(x, y) {return(rnorm(n)) },
            mu = mu, sigma = Sigma)
        }
        else {
        n <- nrow(x)
        d <- ncol(x)
        bootobj <- boot(x, statistic = mvnorm.e, R = R, sim = "parametric", 
            ran.gen = function(x, y) {
                return(matrix(rnorm(n * d), nrow = n, ncol = d)) })
        }
    p <- 1 - mean(bootobj$t < bootobj$t0)
    method = "Energy test of multivariate normality"
    names(bootobj$t0) <- "E-statistic"
    e <- list(call = match.call(),
              statistic = bootobj$t0,
              p.value = p,
              method = method,
              data.name = paste("x, sample size ", n, ", dimension ", d, ", replicates ", R, sep = ""))
    class(e) <- "htest"        
    e                 
}

mvnorm.e <- 
function(x, mu = NULL, Sigma = NULL) 
{
    # E-statistic for multivariate normality    
    n <- NROW(x)
    d <- NCOL(x)
    if (d < 2 || n < 2) return(normal.e(as.vector(x)))

    x <- as.matrix(x)
    if (is.null(mu))     
        mu <- colMeans(x)
    z <- sweep(x, 2, mu)    #subtract mean vector
    if (is.null(Sigma)) 
        Sigma <- var(x)
    ev <- eigen(var(x), symmetric = TRUE)    #compute Sigma^(-1/2)
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
function(x, mu = NULL, sigma = NULL) 
{
   # compute E for univariate normal GOF test
   # univariate version is linearized for computational efficiency
   if(is.matrix(x) && ncol(x) > 1)
       stop("  normal.e() expects a univariate sample, 
           but x is a multivariate sample; use mvnorm.e")
   x <- as.vector(x)
   y <- sort(x)
   n <- length(y)
   if (y[1] == y[n]) return (NA)
   if (is.null(mu)) mu <- mean(y)
   if (is.null(sigma)) sigma <- sd(y)
   y <- (y - mu) / sigma
   K <- seq(1 - n, n - 1, 2)
   e <- 2 * (sum(2 * y * pnorm(y) + 2 * dnorm(y)) - n / sqrt(pi) - mean(K * y))
   e
}
   
