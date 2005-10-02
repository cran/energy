\name{Independence E-test}
\alias{indep.e}
\alias{indep.etest}
\title{ Energy Statistic Test of Independence}
\description{
 Computes the multivariate nonparametric E-statistic and test of independence.}
\usage{
indep.e(x, y)
indep.etest(x, y, R=199)
}
\arguments{
  \item{x}{ matrix: first sample, observations in rows}
  \item{y}{ matrix: second sample, observations in rows}
  \item{R}{ number of replicates}
}
\details{
 Computes the coefficient \eqn{\mathcal I}{I_n} and performs a nonparametric 
 \eqn{\mathcal E}{E}-test of independence. The test decision is obtained via 
 bootstrap, with \code{R} replicates.
 The sample sizes (number of rows) of the two samples must agree, and
 samples must not contain missing values. The statistic 
 \eqn{\mathcal E = n \mathcal I^2}{E = I^2} is a ratio of V-statistics based 
 on interpoint distances \eqn{\|x_{i}-y_{j}\|}{||x_{i}-y_{j}||}. 
 See the reference below for details. 
}
\value{
 The value of the test statistic is returned by \code{indep.e}.
 The function \code{indep.etest} returns a list with class 
 \code{htest} containing
 \item{method}{description of test}
 \item{statistic}{observed value of the coefficient \eqn{\mathcal I}{I}}
 \item{p.value}{approximate p-value of the test}
 \item{data.name}{description of data}
}
\references{
 Bakirov, N.K., Rizzo, M.L., and Szekely, G.J. (2004), A Multivariate
 Nonparametric Test of Independence, \emph{Journal of Multivariate Analysis} 
 (submitted).
 }
\author{ Maria L. Rizzo \email{rizzo@math.ohiou.edu} and
Gabor J. Szekely \email{gabors@bgnet.bgsu.edu}
}
\examples{
 ## independent multivariate data
 x <- matrix(rnorm(60), nrow=20, ncol=3)
 y <- matrix(rnorm(40), nrow=20, ncol=2)
 indep.e(x, y)
 indep.etest(x, y, R = 99)
 
 ## dependent bivariate data
 library(MASS)
 Sigma <- matrix(c(1, .5, .5, 1), 2, 2)
 x <- mvrnorm(30, c(0, 0), Sigma)
 indep.etest(x[,1], x[,2], R = 99)
}
\keyword{ htest }
\concept{ energy statistics }

