\name{eqdist.etest}
\alias{eqdist.etest}
\title{Multisample E-statistic (Energy) Test of Equal Distributions}
\description{
 Performs the nonparametric multisample E-statistic (energy) test
 for equality of multivariate distributions. 
}
\usage{
 eqdist.etest(x, sizes, distance = FALSE, 
              incomplete = FALSE, N = 100, R = 999)
}
\arguments{
  \item{x}{ data matrix of pooled sample}
  \item{sizes}{ vector of sample sizes}
  \item{distance}{logical: if TRUE, first argument is a distance matrix}
  \item{incomplete}{ logical: if TRUE, compute incomplete \eqn{\mathcal{E}}{E}-statistics}
  \item{N}{ incomplete sample size}
  \item{R}{ number of bootstrap replicates }
}
\details{
  The k-sample multivariate \eqn{\mathcal{E}}{E}-test of equal distributions
  is performed. The statistic is computed from the original
  pooled samples, stacked in matrix \code{x} where each row
  is a multivariate observation, or the corresponding distance matrix. The
  first \code{sizes[1]} rows of \code{x} are the first sample, the next
  \code{sizes[2]} rows of \code{x} are the second sample, etc.
 
  The test is implemented by nonparametric bootstrap, an approximate 
  permutation test with \code{R} replicates.
  
  The definition of the multisample \eqn{\mathcal{E}}{E}-statistic is given in the 
  \code{\link{ksample.e}} documentation.

  If \code{incomplete==TRUE}, incomplete \eqn{\mathcal{E}}{E}-statistics (which are
  incomplete V-statistics) are computed. That is, at most
  \code{N} observations from each sample are used, by sampling without replacement 
  as needed. 
}
\value{
 A list with class \code{etest.eqdist} containing
 \item{method}{Description of test}
 \item{statistic}{Observed value of the test statistic}
 \item{p.value}{Approximate p-value of the test}
 \item{sizes}{Vector of sample sizes}
 \item{R}{Number of replicates}
 \item{incomplete}{Argument \code{incomplete}}
 \item{N}{Argument \code{N}}
 \item{replicates}{Vector of replicates of the statistic}
}

\references{ 
 Szekely, G. J. and Rizzo, M. L. (2003) Testing for Equal
 Distributions in High Dimension, submitted.
 
 Szekely, G. J. (2000) \eqn{\mathcal{E}}{E}-statistics: Energy of 
 Statistical Samples, preprint.

} 
\author{ Maria Rizzo \email{rizzo@math.ohiou.edu}}

\seealso{
 \code{\link{ksample.e}},
 \code{\link{print.etest.eqdist}}
 }
\examples{
 ## test if the 3 varieties of iris data (d=4) have equal distributions
 data(iris)
 eqdist.etest(iris[,1:4], c(50,50,50))
 
 ## univariate two-sample test using incomplete E-statistics
 x1 <- rnorm(200)
 x2 <- rnorm(300, .5)
 eqdist.etest(c(x1, x2), c(200, 300), incomplete=TRUE, N=100)
}
\keyword{ multivariate }
\keyword{ htest }
\keyword{ nonparametric }
