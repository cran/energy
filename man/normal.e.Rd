\name{normal.e}
\alias{normal.e}
\title{ E-statistic (Energy Statistic) for Testing Univariate Normality}
\description{
 Computes the E-statistic for testing univariate normality 
 when parameters are estimated.
}
\usage{
normal.e(x)
}
\arguments{
  \item{x}{ vector of univariate sample data}
}
\details{
 The
 data will be standardized to zero mean and unit variance
 using the sample mean and sample variance. If the data contains
 missing values or the sample variance is zero, NA is
 returned.
 
 The 
 \eqn{\mathcal{E}}{E}-test of multivariate (and univariate)
  normality was proposed and implemented by Szekely and Rizzo 
  (2004). The univariate test statistic
 is given by
 \deqn{\mathcal{E} = n (\frac{2}{n} \sum_{i=1}^n E|y_i-Z| - E|Z-Z'| -
 \frac{1}{n^2} \sum_{i=1}^n \sum_{j=1}^n |y_i-y_j|),
 }{n((2/n) sum[1:n] E|y_i-Z| - E|Z-Z'| - (1/n^2) sum[1:n,1:n]
 |y_i-y_j|),}
 where \eqn{y_1,\ldots,y_n} is the standardized sample and
 \eqn{Z, Z'} are iid standard normal variables. See 
 \code{\link{mvnorm.e}} for the multivariate statistic.
}
\value{
 The value of the \eqn{\mathcal{E}}{E}-statistic for univariate
 normality is returned.
}
\references{
 Szekely, G. J. and Rizzo, M. L. (2004) A New Test for 
 Multivariate Normality, \emph{Journal of Multivariate Analysis},
 to appear.

 Rizzo, M. L. (2002). A New Rotation Invariant Goodness-of-Fit Test,
 Ph.D. dissertation, Bowling Green State University.
 
 Szekely, G. J. (1989) Potential and Kinetic Energy in Statistics, 
 Lecture Notes, Budapest Institute of Technology (Technical University). 
 }
\author{ Maria Rizzo \email{rizzo@math.ohiou.edu} }

\seealso{ \code{\link{mvnorm.e} \link{mvnorm.etest}}}
\examples{
 x <- rnorm(30)
 normal.e(x)
}
\keyword{ htest }
