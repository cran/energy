\name{mvnorm.e}
\alias{mvnorm.e}
\title{ E-statistic (Energy Statistic) for Testing Multivariate Normality}
\description{
 Computes the E-statistic (energy statistic) for testing multivariate 
 or univariate normality when parameters are estimated.
}
\usage{
mvnorm.e(x)
}
\arguments{
  \item{x}{ matrix or vector of sample data}
}
\details{
 If \code{x} is a matrix, each row is a multivariate observation. The
 data will be standardized to zero mean and identity covariance matrix
 using the sample mean vector and sample covariance matrix. If \code{x}
 is a vector, the univariate statistic \code{normal.e(x)} is returned. 
 If the data contains missing values or the sample covariance matrix is 
 singular, NA is returned.
 
 The \eqn{\mathcal{E}}{E}-test of multivariate normality was proposed
 and implemented by Szekely and Rizzo (2004). The test statistic for 
 d-variate normality is given by
 \deqn{\mathcal{E} = n (\frac{2}{n} \sum_{i=1}^n E\|y_i-Z\| - 
 E\|Z-Z'\| - \frac{1}{n^2} \sum_{i=1}^n \sum_{j=1}^n \|y_i-y_j\|),
 }{E = n((2/n) sum[1:n] E||y_i-Z|| - E||Z-Z'|| - (1/n^2) sum[1:n,1:n]
 ||y_i-y_j||),}
 where \eqn{y_1,\ldots,y_n} is the standardized sample, 
 \eqn{Z, Z'} are iid standard d-variate normal, and
 \eqn{\| \cdot \|}{|| ||} denotes Euclidean norm.
}
\value{
 The value of the \eqn{\mathcal{E}}{E}-statistic for multivariate
 (univariate) normality is returned.
}
\references{
 Szekely, G. J. and Rizzo, M. L. (2004) A New Test for 
 Multivariate Normality, \emph{Journal of Multivariate Analysis},
 \url{http://dx.doi.org/10.1016/j.jmva.2003.12.002}.

 Rizzo, M. L. (2002). A New Rotation Invariant Goodness-of-Fit Test,
 Ph.D. dissertation, Bowling Green State University.
 
 Szekely, G. J. (1989) Potential and Kinetic Energy in Statistics, 
 Lecture Notes, Budapest Institute of Technology (Technical University). 
 }
\author{ Maria L. Rizzo \email{rizzo@math.ohiou.edu} and
Gabor J. Szekely \email{gabors@bgnet.bgsu.edu}}

\seealso{ \code{\link{mvnorm.etest}}, \code{\link{normal.e}}}

\examples{
 
 ## compute multivariate normality test statistic for iris Setosa data
 data(iris)
 mvnorm.e(iris[1:50, 1:4])
 }
\keyword{ multivariate }
\keyword{ htest }
\concept{ energy statistics }
