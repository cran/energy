\name{poisson.mtest}
\alias{poisson.mtest}
\title{ Mean Distance Test for Poisson Distribution}
\description{
 Performs the mean distance goodness-of-fit test of Poisson distribution
 with unknown parameter.
}
\usage{
poisson.mtest(x, R=999)
}
\arguments{
  \item{x}{ vector of nonnegative integers, the sample data }
  \item{R}{ number of bootstrap replicates }
}
\details{
 The mean distance test of Poissonity was proposed and implemented by Szekely and Rizzo (2004). The test is based on the result that the sequence of expected values E|X-j|, j=0,1,2,... characterizes the distribution of the random  variable X. As an application of this characterization one can get an estimator \eqn{\hat F(j)} of the CDF. The test statistic (see \code{\link{poisson.m}}) is a Cramer-von Mises type of distance, with M-estimates replacing the usual EDF estimates of the CDF:
  \deqn{M_n = n\sum_{j=0}^\infty (\hat F(j) - F(j\;; \hat \lambda))^2
 f(j\;; \hat \lambda).}{M_n = n sum [j>=0] (\hat F(j) - F(j; \hat \lambda))^2
 f(j; \hat \lambda).} The test is implemented by parametric bootstrap with \code{R} replicates. 
}
\value{
 A list with class \code{etest.poisson} containing
 \item{method}{Description of test}
 \item{statistic}{Observed value of the test statistic}
 \item{p.value}{Approximate p-value of the test}
 \item{n}{Sample size}
 \item{lambda}{Sample mean}
 \item{R}{Number of replicates}
 \item{replicates}{Vector of replicates of the statistic}
}
\references
{ Szekely, G. J. and Rizzo, M. L. (2004) Mean Distance Test of Poisson Distribution, \emph{Statistics and Probability Letters}, to appear.
}
\author{ Maria Rizzo \email{rizzo@math.ohiou.edu} }
\seealso{ \code{\link{poisson.m}}}
\examples{
 x <- rpois(20, 1)
 poisson.mtest(x)
 }
\keyword{ htest }
