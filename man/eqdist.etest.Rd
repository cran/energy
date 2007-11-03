\name{eqdist.etest}
\alias{eqdist.etest}
\alias{eqdist.e}
\title{Multisample E-statistic (Energy) Test of Equal Distributions}
\description{
 Performs the nonparametric multisample E-statistic (energy) test
 for equality of multivariate distributions. 
}
\usage{
 eqdist.etest(x, sizes, distance = FALSE, R = 999)
 eqdist.e(x, sizes, distance = FALSE)
}
\arguments{
  \item{x}{ data matrix of pooled sample}
  \item{sizes}{ vector of sample sizes}
  \item{distance}{logical: if TRUE, first argument is a distance matrix}
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
  distances. 
  
  The function \code{eqdist.e} returns the test statistic only; it simply 
  passes the arguments through to \code{eqdist.etest} with \code{R = 0}.
  For computing the statistic only (no test), \code{ksample.e} is usually
  faster.
  
  The definition of the multisample \eqn{\mathcal{E}}{E}-statistic is given in the 
  \code{\link{ksample.e}} documentation.
}
\value{
 A list with class \code{htest} containing
 \item{method}{description of test}
 \item{statistic}{observed value of the test statistic}
 \item{p.value}{approximate p-value of the test}
 \item{data.name}{description of data}
 
 \code{eqdist.e} returns test statistic only.
}
\note{
The pairwise e-distances between samples can be conveniently
computed by the \code{edist} function, which returns a \code{dist} object.
The function \code{ksample.e} computes the test statistic without storing
the distances.
}
\references{ 
 Szekely, G. J. and Rizzo, M. L. (2004) Testing for Equal
 Distributions in High Dimension, \emph{InterStat}, November (5).
 
 Szekely, G. J. (2000) Technical Report 03-05:
 \eqn{\mathcal{E}}{E}-statistics: Energy of  
 Statistical Samples, Department of Mathematics and Statistics, Bowling
 Green State University.
}
\author{ Maria L. Rizzo \email{mrizzo @ bgnet.bgsu.edu} and
Gabor J. Szekely \email{gabors @ bgnet.bgsu.edu}}
\seealso{
 \code{\link{ksample.e}},
 \code{\link{edist}}
 \code{\link{energy.hclust}}
 }
\examples{
 data(iris)
 
 ## test if the 3 varieties of iris data (d=4) have equal distributions
 eqdist.etest(iris[,1:4], c(50,50,50), R = 199)

\dontshow{
  eqdist.e(iris[,1:4], c(50,50,50))
  x <- matrix(rnorm(200), nrow=40)
  y <- matrix(rnorm(250, mean=5), nrow=50)
  x <- rbind(x, y)
  eqdist.etest(dist(x), sizes=c(40, 50), distance=TRUE, R = 19)
  eqdist.e(dist(x), sizes=c(40, 50), distance=TRUE)
}
}
\keyword{ multivariate }
\keyword{ htest }
\keyword{ nonparametric }
\concept{ energy statistics}
