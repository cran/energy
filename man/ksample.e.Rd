\name{ksample.e}
\alias{ksample.e}
\title{E-statistic (Energy Statistic) for Multivariate k-sample Test of Equal Distributions}
\description{
 Returns the E-statistic (energy statistic)
 for the multivariate k-sample test of equal distributions. 
}
\usage{
 ksample.e(x, sizes, distance = FALSE, ix = 1:sum(sizes), 
           incomplete = FALSE, N = 100)
}
\arguments{
  \item{x}{ data matrix of pooled sample}
  \item{sizes}{ vector of sample sizes}
  \item{distance}{ logical: if TRUE, x is a distance matrix}
  \item{ix}{ a permutation of the row indices of x }
  \item{incomplete}{ logical: if TRUE, compute incomplete \eqn{\mathcal{E}}{E}-statistics}
  \item{N}{ incomplete sample size}
}
\details{
  The k-sample multivariate \eqn{\mathcal{E}}{E}-statistic for testing equal distributions
  is returned. The statistic is computed from the original pooled samples, stacked in 
  matrix \code{x} where each row is a multivariate observation, or from the distance 
  matrix \code{x} of the original data. The
  first \code{sizes[1]} rows of \code{x} are the first sample, the next
  \code{sizes[2]} rows of \code{x} are the second sample, etc.
 
  The two-sample \eqn{\mathcal{E}}{E}-statistic proposed by Szekely and Rizzo (2003)
  is the e-distance \eqn{e(S_i,S_j)}, defined for two samples \eqn{S_i, S_j}
  of size \eqn{n_i, n_j} by
  \deqn{e(S_i,S_j)=\frac{n_i n_j}{n_i+n_j}[2M_{ij}-M_{ii}-M_{jj}],
  }{e(S_i, S_j) = (n_i n_j)(n_i+n_j)[2M_(ij)-M_(ii)-M_(jj)],}
  where
  \deqn{M_{ij}=\frac{1}{n_i n_j}\sum_{p=1}^{n_i} \sum_{q=1}^{n_j}
     \|X_{ip}-X_{jq}\|,}{
     M_{ij} = 1/(n_i n_j) sum[1:n_i, 1:n_j] ||X_(ip) - X_(jq)||,}
     \eqn{\|\cdot\|}{|| ||} denotes Euclidean norm, and \eqn{X_{ip}}{
     X_(ip)} denotes the p-th observation in the i-th sample.  
  The k-sample  
  \eqn{\mathcal{E}}{E}-statistic is defined by summing the pairwise e-distances over 
  all \eqn{k(k-1)/2} pairs 
  of samples:
  \deqn{\mathcal{E}=\sum_{1 \leq i < j \leq k} e(S_i,S_j).
  }{\emph{E} = sum[i<j] e(S_i,S_j).}  
  Large values of \eqn{\mathcal{E}}{\emph{E}} are significant.

  If \code{incomplete==TRUE}, an incomplete \eqn{\mathcal{E}}{E}-statistic (which is an
  incomplete V-statistic) is computed. That is, at most
  \code{N} observations from each sample are used, 
  by sampling without replacement as needed. 
}
\value{
 The value of the multisample \eqn{\mathcal{E}}{E}-statistic corresponding to
 the permutation \code{ix} is returned.
}
\references{ 
 Szekely, G. J. and Rizzo, M. L. (2003) Testing for Equal
 Distributions in High Dimension, submitted.
 
 Szekely, G. J. (2000) \eqn{\mathcal{E}}{E}-statistics: Energy of 
 Statistical Samples, preprint.
} 
\author{ Maria Rizzo \email{rizzo@math.ohiou.edu}}
\note{  This function computes the \eqn{\mathcal{E}}{E}-statistic only. 
 For the test decision,
 a nonparametric bootstrap test (approximate permutation test)
 is provided by the function \code{\link{eqdist.etest}}.
 }
\seealso{
 \code{\link{eqdist.etest}}
 }
\examples{
## compute 3-sample E-statistic for 4-dimensional iris data
 data(iris)
 ksample.e(iris[,1:4], c(50,50,50))

## compute univariate two-sample incomplete E-statistic
 x1 <- rnorm(200)
 x2 <- rnorm(300, .5)
 x <- c(x1, x2)
 ksample.e(x, c(200, 300), incomplete=TRUE, N=100)
 
}
\keyword{ multivariate }
\keyword{ htest }
\keyword{ nonparametric }
