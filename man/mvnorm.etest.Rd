\name{mvnorm.etest}
\alias{mvnorm.etest}
\title{E-statistic (Energy) Test of Multivariate Normality}
\description{
 Performs the E-statistic (energy) test of multivariate or univariate normality.
}
\usage{
 mvnorm.etest(x, R = 999)
}
\arguments{
  \item{x}{ data matrix of multivariate sample, or univariate data vector}
  \item{R}{ number of bootstrap replicates }
}
\details{
  The \eqn{\mathcal{E}}{E}-test of multivariate (univariate) normality
  is performed. The test is implemented by parametric bootstrap with 
  \code{R} replicates. 
  
  The definition of the \eqn{\mathcal{E}}{E}-statistic is given in the 
  \code{\link{mvnorm.e}} documentation.
}
\value{
 A list with class \code{etest.mvnorm} containing
 \item{method}{Description of test}
 \item{statistic}{Observed value of the test statistic}
 \item{p.value}{Approximate p-value of the test}
 \item{n}{Sample size}
 \item{R}{Number of replicates}
 \item{replicates}{Vector of replicates of the statistic}
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

\seealso{
 \code{\link{mvnorm.e}},
 \code{\link{print.etest.mvnorm}}
 }
\examples{
 ## test if the iris Setosa data has multivariate normal distribution
 data(iris)
 mvnorm.etest(iris[1:50,1:4])
 
 ## test a univariate sample for normality
 x <- runif(50, 0, 10)
 mvnorm.etest(x)
 }
\keyword{ multivariate }
\keyword{ htest }
\concept{ energy statistics }
