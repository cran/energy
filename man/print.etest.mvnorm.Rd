\name{print.etest.mvnorm}
\alias{print.etest.mvnorm}
\title{ Print E-test (Energy Test) of Multivariate Normality }
\description{
 Print method for \code{etest.mvnorm} object returned by the 
 \code{mvnorm.etest} function. 
}
\usage{
print.etest.mvnorm(x, ...)
}
\arguments{
  \item{x}{ an object of class \code{etest.mvnorm} }
  \item{...}{ extra arguments passed to or from other methods }  
}
\author{ Maria Rizzo \email{rizzo@math.ohiou.edu} }
\seealso{ \code{\link{mvnorm.etest}} }
\examples{
 ## print E-test for 5-dimensional data
 x <- matrix(rnorm(100), nrow=20, ncol=5)
 e <- mvnorm.etest(x)
 print.etest.mvnorm(e)
 }
\keyword{ print }
