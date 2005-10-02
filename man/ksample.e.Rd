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
  \code{sizes[2]} rows of \code{x} are the second sample, etc. Incomplete statistics are 
  supported for the two-sample case. If
  \code{incomplete==TRUE}, at most \code{N} observations from each sample 
  (by sampling without replacement) are used in the calculation of the statistic.
  If \code{distance==TRUE} complete statistics are always computed.
 
  The two-sample \eqn{\mathcal{E}}{E}-statistic proposed by Szekely and Rizzo (2004)
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
}
\value{
 The value of the multisample \eqn{\mathcal{E}}{E}-statistic corresponding to
 the permutation \code{ix} is returned.
}
\note{
The pairwise e-distances between samples can be conveniently
computed by the \code{edist} function, which returns a \code{dist} object.
The function \code{ksample.e} computes the \eqn{\mathcal{E}}{E}-statistic only. 
 For the test decision, a nonparametric bootstrap test (approximate permutation test)
 is provided by the function \code{\link{eqdist.etest}}. With the default arguments,
 \code{ksample.e} computes the statistic without storing the distance matrix, which
 is more efficient than calling \code{eqdist.etest} with \code{R = 0}.
}
\references{ 
 Szekely, G. J. and Rizzo, M. L. (2004) Testing for Equal
 Distributions in High Dimension, \emph{InterStat}, November (5).
 
 Szekely, G. J. (2000) Technical Report 03-05:
 \eqn{\mathcal{E}}{E}-statistics: Energy of 
 Statistical Samples, Department of Mathematics and Statistics, Bowling
 Green State University.
} 
\author{ Maria L. Rizzo \email{rizzo@math.ohiou.edu} and
Gabor J. Szekely \email{gabors@bgnet.bgsu.edu}}
\seealso{ 
 \code{\link{eqdist.etest}}
 \code{\link{edist}}
 \code{\link{energy.hclust}} 
 }
\examples{
## compute 3-sample E-statistic for 4-dimensional iris data
 data(iris)
 ksample.e(iris[,1:4], c(50,50,50))

## compute a 3-sample univariate E-statistic
 ksample.e(rnorm(150), c(25,75,50))
}

\keyword{ multivariate }
\keyword{ htest }
\keyword{ nonparametric }
\concept{ energy statistics }
