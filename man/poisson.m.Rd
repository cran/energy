\name{poisson.m}
\alias{poisson.m}
\title{ Mean Distance Statistic for Testing Poisson Distribution}
\description{
 Returns the mean distance statistic for a goodness-of-fit test of Poisson distribution with unknown parameter.
}
\usage{
poisson.m(x)
}
\arguments{
  \item{x}{ vector of nonnegative integers, the sample data }
}
\details{
The mean distance test of Poissonity was proposed and implemented by Szekely and Rizzo (2004). The test is based on the result that the sequence of expected values E|X-j|, j=0,1,2,... characterizes the distribution of the random variable X. As an application of this characterization one can get an estimator \eqn{\hat F(j)} of the CDF. The test statistic is a Cramer-von Mises type of distance, with M-estimates replacing the usual EDF estimates of the CDF: 
 \deqn{M_n = n\sum_{j=0}^\infty (\hat F(j) - F(j\;; \hat \lambda))^2
f(j\;; \hat \lambda).}{M_n = n sum [j>=0] (\hat F(j) - F(j; \hat \lambda))^2
f(j; \hat \lambda).} See Szekely and Rizzo (2004) for the computing formula. 

}
\value{
 The value of the \eqn{M}-statistic for testing Poisson distribution is returned.
}
\references{ 
Szekely, G. J. and Rizzo, M. L. (2004) Mean Distance Test of Poisson Distribution, \emph{Statistics and Probability Letters}, 
67/3, 241-247. \url{http://dx.doi.org/10.1016/j.spl.2004.01.005}.
}
\author{ Maria L. Rizzo \email{rizzo@math.ohiou.edu} and
Gabor J. Szekely \email{gabors@bgnet.bgsu.edu}}
\seealso{ \code{\link{poisson.mtest}}}
\examples{
 x <- rpois(20, 1)
poisson.m(x)
 }
\keyword{ htest }
