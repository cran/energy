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
  \item{incomplete}{logical: if TRUE, compute incomplete E-statistics}
  \item{N}{sample size for incomplete statistics}
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
  permutation test with \code{R} replicates. For large samples it is
  more efficient if \code{x} contains the data matrix rather than the
  distances. Incomplete statistics are supported for the two-sample test. If
  \code{incomplete==TRUE}, at most \code{N} observations from each sample 
  (by sampling without replacement) are used in the calculation of the statistic.
  If \code{distance==TRUE} complete statistics are always computed.
  
  The definition of the multisample \eqn{\mathcal{E}}{E}-statistic is given in the 
  \code{\link{ksample.e}} documentation.
}
\value{
 A list with class \code{htest} containing
 \item{method}{description of test}
 \item{statistic}{observed value of the test statistic}
 \item{p.value}{approximate p-value of the test}
 \item{data.name}{description of data}
}
\note{
The pairwise e-distances between samples can be conveniently
computed by the \code{edist} function, which returns a \code{dist} object.
The function \code{ksample.e} computes the test statistic without storing
the distances, which is more efficient than calling \code{eqdist.etest} 
with \code{R = 0}.
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
 \code{\link{ksample.e}},
 \code{\link{edist}}
 \code{\link{energy.hclust}}
 }
\examples{
 data(iris)
 
 ## test if the 3 varieties of iris data (d=4) have equal distributions
 eqdist.etest(iris[,1:4], c(50,50,50), R = 199)

 ## compare incomplete versions of two sample test
 x <- c(rpois(400, 2), rnbinom(600, size=1, mu=2))
 eqdist.etest(x, c(400, 600), incomplete=TRUE, N=100, R = 199)
 eqdist.etest(x, c(400, 600), incomplete=TRUE, N=200, R = 199)
  
\dontshow{
  x <- matrix(rnorm(200), nrow=40)
  y <- matrix(rnorm(250, mean=5), nrow=50)
  x <- rbind(x, y)
  eqdist.etest(dist(x), sizes=c(40, 50), distance=TRUE, R = 19)
  eqdist.etest(x, sizes=c(40, 50), incomplete=TRUE, N=50, R = 19)
}
}
\keyword{ multivariate }
\keyword{ htest }
\keyword{ nonparametric }
\concept{ energy statistics}
